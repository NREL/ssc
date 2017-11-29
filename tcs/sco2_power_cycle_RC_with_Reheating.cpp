#include "stdafx.h"
#include "sco2_power_cycle_RC_with_Reheating.h"
//#include "co2props.h"
//#include "co2props_nn.h"
#include "CO2_properties.h"
#include <algorithm>
//#include "sam_csp_util.h"
#include <numeric>
#include <limits>
#include "nlopt.hpp"
#include "nlopt_callbacks.h"

#include "fmin.h"
#include "fmin_callbacks.h"

using namespace std;

double P_pseudocritical_RC_with_Reheating(double T_K)
{
	return (0.191448*T_K + 45.6661)*T_K - 24213.3;
}

double HeatExchanger_RC_with_Reheating::hxr_DP(int stream, double m_dot, bool scale_DP /*kPa*/)
{
	/*Returns the pressure drop(in kPa) of the specified stream(1 or 2) for the heat exchanger specified by 'hxr'.
		If scale_DP is .true., scales the design - point pressure drop with mass flow rate according to the Darcy friction factor and Blasius correlation. */

	if( scale_DP )
		return m_HX_des_par.m_DP_design[stream] * pow((m_dot / m_HX_des_par.m_m_dot_design[stream]), 1.75);
	else
		return m_HX_des_par.m_DP_design[stream];
}

double HeatExchanger_RC_with_Reheating::hxr_UA(double m_dot_0, double m_dot_1, bool scale_UA /*kW/K*/)
{
	/*Returns the UA(kW / K) of the heat exchanger specified by 'hxr'.
		If scale_UA is .true., scales the design - point UA with mass flow rate according to the Dittus - Boelter heat transfer correlation.*/

	if( scale_UA )
	{
		double m_dot_ratio = (m_dot_0 / m_HX_des_par.m_m_dot_design[0] + m_dot_1 / m_HX_des_par.m_m_dot_design[1]) / 2.0;
		return m_HX_des_par.m_UA_design*pow(m_dot_ratio, 0.8);
	}
	else
		return m_HX_des_par.m_UA_design;
	
}

void calculate_turbomachinery_outlet_RC_with_Reheating(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double eta /*-*/, bool is_comp, int & error_code, double & enth_in /*kJ/kg*/, double & entr_in /*kJ/kg-K*/,
	double & dens_in /*kg/m3*/, double & temp_out /*K*/, double & enth_out /*kJ/kg*/, double & entr_out /*kJ/kg-K*/, double & dens_out /*kg/m3*/, double & spec_work /*kJ/kg*/)
{
	/*Calculates the outlet state of a compressor or turbine using its isentropic efficiency.
		is_comp = .true.means the turbomachine is a compressor(w = w_s / eta)
		is_comp = .false.means the turbomachine is a turbine(w = w_s * eta) */

	CO2_state co2_props;

	error_code = 0;

	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);		// properties at the inlet conditions
	if(prop_error_code != 0)
	{
		error_code = 1;
		return;
	}
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;
	dens_in = co2_props.dens;

	prop_error_code = CO2_PS(P_out, s_in, &co2_props);			// outlet enthalpy if compression/expansion is isentropic
	if(prop_error_code != 0)
	{
		error_code = 2;
		return;
	}
	double h_s_out = co2_props.enth;

	double w_s = h_in - h_s_out;			// specific work if process is isentropic (negative for compression, positive for expansion)

	double w = 0.0;
	if( is_comp )
		w = w_s / eta;						// actual specific work of compressor (negative)
	else
		w = w_s * eta;						// actual specific work of turbine (positive)

	double h_out = h_in - w;

	prop_error_code = CO2_PH(P_out, h_out, &co2_props);
	if(prop_error_code != 0)
	{
		error_code = 3;
		return;
	}

	enth_in = h_in;
	entr_in = s_in;
	temp_out = co2_props.temp;
	enth_out = h_out;
	entr_out = co2_props.entr;
	dens_out = co2_props.dens;
	spec_work = w;

	return;
};

void calculate_hxr_UA_RC_with_Reheating(int N_hxrs, double Q_dot /*units?*/, double m_dot_c, double m_dot_h, double T_c_in, double T_h_in, double P_c_in, double P_c_out, double P_h_in, double P_h_out,
	int & error_code, double & UA, double & min_DT)
{
	/*Calculates the UA of a heat exchanger given its mass flow rates, inlet temperatures, and a heat transfer rate.
	Note: the heat transfer rate must be positive.*/
	
	// Check inputs
	if(Q_dot < 0.0)
	{
		error_code = 4;
		return;
	}
	if(T_h_in < T_c_in)
	{
		error_code = 5;
		return;
	}
	if(P_h_in < P_h_out)
	{
		error_code = 6;
		return;
	}
	if(P_c_in < P_c_out)
	{
		error_code = 7;
		return;
	}
	if(Q_dot <= 1.E-14)		// very low Q_dot; assume it is zero
	{
		UA = 0.0;
		min_DT = T_h_in - T_c_in;
		return;
	}

	// Calculate inlet enthalpies from known state points
	CO2_state co2_props;
	int prop_error_code = CO2_TP(T_c_in, P_c_in, &co2_props);
	if(prop_error_code != 0)
	{
		error_code = 8;
		return;
	}
	double h_c_in = co2_props.enth;

	prop_error_code = CO2_TP(T_h_in, P_h_in, &co2_props);
	if(prop_error_code != 0)
	{
		error_code = 9;
		return;
	}
	double h_h_in = co2_props.enth;

	// Calculate outlet enthalpies from energy balance
	double h_c_out = h_c_in + Q_dot/m_dot_c;
	double h_h_out = h_h_in - Q_dot/m_dot_h;

	int N_nodes = N_hxrs + 1;
	double h_h_prev = 0.0;
	double T_h_prev = 0.0;
	double h_c_prev = 0.0;
	double T_c_prev = 0.0;
	UA = 0.0;
	min_DT = T_h_in;
	// Loop through the sub-heat exchangers
	for( int i = 0; i < N_nodes; i++ )
	{
		// Assume pressure varies linearly through heat exchanger
		double P_c = P_c_out + i*(P_c_in - P_c_out) / (N_nodes - 1);
		double P_h = P_h_in - i*(P_h_in - P_h_out) / (N_nodes - 1);

		// Calculate the entahlpy at the node
		double h_c = h_c_out + i*(h_c_in - h_c_out) / (N_nodes - 1);
		double h_h = h_h_in - i*(h_h_in - h_h_out) / (N_nodes - 1);

		// Calculate the hot and cold temperatures at the node
		prop_error_code = CO2_PH(P_h, h_h, &co2_props);
		if(prop_error_code != 0)
		{
			error_code = 12;
			return;
		}
		double T_h = co2_props.temp;
		
		prop_error_code = CO2_PH(P_c, h_c, &co2_props);
		if(prop_error_code != 0)
		{
			error_code = 13;
			return;
		}
		double T_c = co2_props.temp;

		// Check that 2nd law was not violated
		if(T_c >= T_h)
		{
			error_code = 11;
			return;
		}

		// Track the minimum temperature difference in the heat exchanger
		min_DT = min(min_DT, T_h - T_c);

		// Perform effectiveness-NTU and UA calculations 
		if(i > 0)
		{
			double C_dot_h = m_dot_h*(h_h_prev - h_h)/(T_h_prev - T_h);			// [kW/K] hot stream capacitance rate
			double C_dot_c = m_dot_c*(h_c_prev - h_c)/(T_c_prev - T_c);			// [kW/K] cold stream capacitance rate
			double C_dot_min = min(C_dot_h, C_dot_c);				// [kW/K] Minimum capacitance stream
			double C_dot_max = max(C_dot_h, C_dot_c);				// [kW/K] Maximum capacitance stream
			double C_R = C_dot_min / C_dot_max;						// [-] Capacitance ratio of sub-heat exchanger
			double eff = (Q_dot/(double)N_hxrs)/(C_dot_min*(T_h_prev - T_c));	// [-] Effectiveness of each sub-heat exchanger
			double NTU = 0.0;
			if( C_R != 1.0 )
				NTU = log((1.0 - eff*C_R) / (1.0 - eff)) / (1.0 - C_R);		// [-] NTU if C_R does not equal 1
			else
				NTU = eff / (1.0 - eff);
			UA += NTU*C_dot_min;						// [kW/K] Sum UAs for each hx section			
		}
		h_h_prev = h_h;
		T_h_prev = T_h;
		h_c_prev = h_c;
		T_c_prev = T_c;
	}

	// Check for NaNs that arose
	if(UA != UA)
	{
		error_code = 14;
		return;
	}

	return;
};

void compressor_RC_with_Reheating::solve_compressor(double T_in, double P_in, double m_dot, double N_in, int & error_code, double & T_out, double & P_out)
{
	// Fully define the inlet state of the compressor
	m_surge = false;
	error_code = 0;
	CO2_state co2_props;

	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = 15;
		return;
	}
	double rho_in = co2_props.dens;
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;

	m_N = N_in;
	// Calculate the modified flow and head coefficients and efficiency for the SNL compressor
	double U_tip = m_D_rotor*0.5*m_N*0.104719755;			// tip speed in m/s
	double phi = m_dot/(rho_in*U_tip*pow(m_D_rotor,2));	// flow coefficient
	if( phi < m_phi_min )		// The compressor is operating in the surge region
	{
		m_surge = true;
		phi = m_phi_min;		// reset phi to its minimum value; this sets psi and eta to be fixed at the values at the surge limit
	}

	double phi_star = phi*pow(m_N / m_N_design, 0.2);		// modified flow coefficient
	double psi_star = compressor_psi_polynomial_fit(m_comp_des_par.m_type, phi_star);
	double eta_star = compressor_eta_polynomial_fit(m_comp_des_par.m_type, phi_star);
	double psi = psi_star / (pow(m_N_design / m_N, pow(20.0*phi_star, 3.0)));
	double eta_0 = eta_star * 1.47528 / (pow(m_N_design / m_N, pow(20.0*phi_star, 5.0)));		// Efficiency is normalized so it equals 1.0 at phi_star = 0.0297 (the compressor's design point)
	m_eta = max(eta_0*m_comp_des_par.m_eta_design, 0.0);		// The actual compressor efficiency, not allowed to go negative

	// Check that the specified mass flow rate is possible with the compressor's current shaft speed
	if( psi <= 0.0 )
	{
		error_code = 16;
		return;
	}

	// Calculate the compressor outlet state
	double dh_s = psi*pow(U_tip, 2)*0.001;				// ideal enthalpy rise in compressor, from definition of head coefficient (kJ/kg)
	double dh = dh_s / m_eta;								// actual enthalpy rise in compressor

	double h_s_out = h_in + dh_s;						// ideal enthalpy at compressor outlet
	double h_out = h_in + dh;							// actual enthalpy at compressor outlet
	prop_error_code = CO2_HS(h_s_out, s_in, &co2_props);	// determines compressor outlet pressure
	if( prop_error_code != 0 )		// most likely case is that the outlet pressure is above the high pressure limit of the property routine
	{
		error_code = 17;
		return;
	}
	P_out = co2_props.pres;

	prop_error_code = CO2_PH(P_out, h_out, &co2_props);		// determines compressor outlet temperature and speed of sound
	if( prop_error_code != 0 )		// most likely case is that the outlet pressure is above the high pressure limit of the property routine
	{
		error_code = 18;
		return;
	}
	T_out = co2_props.temp;
	double ssnd_out = co2_props.ssnd;

	m_w = -dh;	// compressor power (negative value)
	double w_tip_ratio = ssnd_out / U_tip;		// ratio of the local (comp outlet) speed of sound to the tip speed

	m_m_dot = m_dot;

	return;
};

void turbine_RC_with_Reheating::solve_turbine(double T_in, double P_in, double P_out, double N_in, int & error_code, double & T_out, double & m_dot_out)
{
	m_N = N_in;

	CO2_state co2_props;
	error_code = 0;
	
	// Fully define the inlet state of the turbine
	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
	if(prop_error_code != 0)
	{
		error_code = 19;
		return;
	}
	double rho_in = co2_props.dens;
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;
	double ssnd_in = co2_props.ssnd;

	// Calculate the enthalpy at the turbine outlet if the expansion is isentropic
	prop_error_code = CO2_PS(P_out, s_in, &co2_props);
	if(prop_error_code != 0)
	{
		error_code = 20;
		return;
	}
	double h_s_out = co2_props.enth;
	
	// Apply the radial turbine equations for efficiency
	double C_s = sqrt( (h_in - h_s_out)*2000.0 );					// Spouting velocity (m/s) [note: there is a mutiple of 2 in the equation that was folded into the *1000 unit conversion]
	double U_tip = m_D_rotor*0.5*m_N*0.104719755;						// Turbine tip speed (m/s)
	double nu = U_tip / C_s;				// Ratio of tip speed to spouting velocity

	double eta_0 = std::numeric_limits<double>::quiet_NaN();
	if( nu < 1.0 )
		eta_0 = 2.0*nu*sqrt(1.0 - pow(nu, 2));				// Efficiency from Baines (1.0 at design point)
	else
		eta_0 = 0.0;										// catches nu values just over 1, which leads to sqrt of negative number
	m_eta = eta_0 * m_turb_des_par.m_eta_design;			// Actual turbine efficiency
		
	// Calculate the outlet state.
	double h_out = h_in - m_eta * (h_in - h_s_out);                       // enthalpy at turbine outlet
	prop_error_code = CO2_PH(P_out, h_out, &co2_props);
	if(prop_error_code != 0)
	{
		error_code = 21;
		return;
	}
	T_out = co2_props.temp;
	double rho_out = co2_props.dens;

	// Determine the allowed mass flow rate through the turbine and the total power.
	m_dot_out = C_s * m_A_nozzle * rho_out;	// mass flow through turbine(kg / s)
	m_m_dot = m_dot_out;
	m_w = h_in - h_out;
	double w_tip_ratio = ssnd_in / U_tip;			// ratio of the local(turbine inlet) speed of sound to the tip speed

}

bool RecompCycle_with_Reheating::design()
{
	int max_iter = 100;

//	// Set RecompCycle member variable
//	W_dot_net   = I_W_dot_net;		
//	conv_tol    = tol;
//	recomp_frac = I_recomp_frac;

	// Set other variables that need to reported at end of this function
	double min_DT_LT = 0.0;
	double min_DT_HT = 0.0;

	double m_dot_t    = 0.0;
	double m_dot_mc   = 0.0;
	double m_dot_rc   = 0.0;
	double w_mc       = 0.0;
	double w_rc       = 0.0;
	double w_mt        = 0.0;
	double w_rt        = 0.0;
	double Q_dot_LT   = 0.0;
	double Q_dot_HT   = 0.0;
	double UA_LT_calc = 0.0;
	double UA_HT_calc = 0.0;

	int cpp_offset = 1;
	m_temp_last[1-cpp_offset] = m_cycle_des_par.m_T_mc_in;
	double P_mc_in = m_cycle_des_par.m_P_mc_out / m_cycle_des_par.m_PR_mc;
	m_pres_last[1-cpp_offset] = P_mc_in;
	m_pres_last[2-cpp_offset] = m_cycle_des_par.m_P_mc_out;
	m_temp_last[6-cpp_offset] = m_cycle_des_par.m_T_mt_in;
	m_temp_last[12-cpp_offset] = m_cycle_des_par.m_T_rt_in;
    m_pres_last[12-cpp_offset] = m_cycle_des_par.m_P_rt_in;
	
	// Apply pressure drops to heat exchangers, fully defining the pressures at all stages
	if( m_cycle_des_par.m_DP_LT[1 - cpp_offset] < 0.0 )
		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset] - m_pres_last[2 - cpp_offset] * abs(m_cycle_des_par.m_DP_LT[1 - cpp_offset]);		// Relative pressure drop specified for LT recuperator (cold stream)
	else
		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset] - m_cycle_des_par.m_DP_LT[1 - cpp_offset];									// Absolute pressure drop specified for LT recuperator (cold stream)
	
	double UA_LT = m_cycle_des_par.m_UA_rec_total*m_cycle_des_par.m_LT_frac;
	double UA_HT = m_cycle_des_par.m_UA_rec_total*(1 - m_cycle_des_par.m_LT_frac);

	if( UA_LT < 1.E-12 )
		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset];		// if there is no LT recuperator, there is no pressure drop

	m_pres_last[4 - cpp_offset] = m_pres_last[3 - cpp_offset];			// No pressure drop in mixing value
	m_pres_last[10 - cpp_offset] = m_pres_last[3 - cpp_offset];			// No pressure drop in mixing value

	if( m_cycle_des_par.m_DP_HT[1 - cpp_offset] < 0.0 )
		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset] - m_pres_last[4 - cpp_offset] * abs(m_cycle_des_par.m_DP_HT[1 - cpp_offset]);	// relative pressure drop specified for HT recuperator (cold stream)
	else
		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset] - m_cycle_des_par.m_DP_HT[1 - cpp_offset];								// absolute pressure drop specified for HT recuperator (cold stream)

	if( UA_HT < 1.E-12 )
		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset];		// if there is no HT recuperator, there is no pressure drop

	if( m_cycle_des_par.m_DP_PHX[1 - cpp_offset] < 0.0 )
		m_pres_last[6 - cpp_offset] = m_pres_last[5 - cpp_offset] - m_pres_last[5 - cpp_offset] * abs(m_cycle_des_par.m_DP_PHX[1 - cpp_offset]);	// relative pressure drop specified for PHX
	else
		m_pres_last[6 - cpp_offset] = m_pres_last[5 - cpp_offset] - m_cycle_des_par.m_DP_PHX[1 - cpp_offset];								// absolute pressure drop specified for PHX

	if( m_cycle_des_par.m_DP_RHX[1 - cpp_offset] < 0.0 )
		m_pres_last[11 - cpp_offset] = m_pres_last[12 - cpp_offset] + m_pres_last[12 - cpp_offset] * abs(m_cycle_des_par.m_DP_RHX[1 - cpp_offset]);	// relative pressure drop specified for PHX
	else
		m_pres_last[11 - cpp_offset] = m_pres_last[12 - cpp_offset] + m_cycle_des_par.m_DP_RHX[1 - cpp_offset];								// absolute pressure drop specified for PHX
	
	if( m_cycle_des_par.m_DP_PC[2 - cpp_offset] < 0.0 )
		m_pres_last[9 - cpp_offset] = m_pres_last[1 - cpp_offset] / (1.0 - abs(m_cycle_des_par.m_DP_PC[2 - cpp_offset]));			// relative pressure drop specified for precooler [P1 = P9 - P9*rel_DP => P1 = P9*(1-rel_DP)
	else
		m_pres_last[9 - cpp_offset] = m_pres_last[1 - cpp_offset] + m_cycle_des_par.m_DP_PC[2 - cpp_offset];										// absolute pressure drop specified for precooler

	if( m_cycle_des_par.m_DP_LT[2 - cpp_offset] < 0.0 )
		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset] / (1.0 - abs(m_cycle_des_par.m_DP_LT[2 - cpp_offset]));			// relative pressure drop specified for LT recuperator (hot stream)
	else
		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset] + m_cycle_des_par.m_DP_LT[2 - cpp_offset];						// absolute pressure drop specified for LT recuperator (hot stream)

	if( UA_LT < 1.E-12 )
		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset];		// if there is no LT recup, there is no pressure drop

	if( m_cycle_des_par.m_DP_HT[2 - cpp_offset] < 0.0 )
		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset] / (1.0 - abs(m_cycle_des_par.m_DP_HT[2 - cpp_offset]));			// relative pressure drop specified for HT recup
	else
		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset] + m_cycle_des_par.m_DP_HT[2 - cpp_offset];						// absolute pressure drop specified for HT recup

	if( UA_HT < 1.E-12 )
		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset];

	int sub_error_code = 0;
	// Determine the outlet states of the main compressor and turbine and their specific works
	calculate_turbomachinery_outlet_RC_with_Reheating(m_temp_last[1 - cpp_offset], m_pres_last[1 - cpp_offset], m_pres_last[2 - cpp_offset], m_cycle_des_par.m_eta_mc,
		true, sub_error_code, m_enth_last[1 - cpp_offset], m_entr_last[1 - cpp_offset], m_dens_last[1 - cpp_offset],
		m_temp_last[2 - cpp_offset], m_enth_last[2 - cpp_offset], m_entr_last[2 - cpp_offset], m_dens_last[2 - cpp_offset], w_mc);

	if(sub_error_code != 0)
	{
		m_errors.SetError(22);
		m_errors.SetError(sub_error_code);
		return false;
	}

	//Main Turbine
	calculate_turbomachinery_outlet_RC_with_Reheating(m_temp_last[6 - cpp_offset], m_pres_last[6 - cpp_offset], m_pres_last[11 - cpp_offset], m_cycle_des_par.m_eta_mt,
		false, sub_error_code, m_enth_last[6 - cpp_offset], m_entr_last[6 - cpp_offset], m_dens_last[6 - cpp_offset],
		m_temp_last[11 - cpp_offset], m_enth_last[11 - cpp_offset], m_entr_last[11 - cpp_offset], m_dens_last[11 - cpp_offset], w_mt);

	if(sub_error_code != 0)
	{
		m_errors.SetError(23);
		m_errors.SetError(sub_error_code);
		return false;
	}

	//Reheating Turbine
	calculate_turbomachinery_outlet_RC_with_Reheating(m_temp_last[12 - cpp_offset], m_pres_last[12 - cpp_offset], m_pres_last[7 - cpp_offset], m_cycle_des_par.m_eta_rt,
		false, sub_error_code, m_enth_last[12 - cpp_offset], m_entr_last[12 - cpp_offset], m_dens_last[12 - cpp_offset],
		m_temp_last[7 - cpp_offset], m_enth_last[7 - cpp_offset], m_entr_last[7 - cpp_offset], m_dens_last[7 - cpp_offset], w_rt);

	if (sub_error_code != 0)
	{
		m_errors.SetError(23);
		m_errors.SetError(sub_error_code);
		return false;
	}

	// Check to ensure this cycle can produce power under the best conditions(ie, temp(9) = temp(2) if there is a recompressing compressor).
	w_rc = 0.0;
	
	if( m_cycle_des_par.m_recomp_frac >= 1.E-12 )
	{
		double dummy[7];

		calculate_turbomachinery_outlet_RC_with_Reheating(m_temp_last[2 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], m_cycle_des_par.m_eta_rc,
			true, sub_error_code, dummy[0], dummy[1], dummy[2], dummy[3], dummy[4], dummy[5], dummy[6], w_rc);

		if( sub_error_code != 0 )
		{
			m_errors.SetError(24);
			m_errors.SetError(sub_error_code);
			return false;
		}
	}

	if( w_mc + w_rc + w_mt + w_rt <= 0.0 )
	{
		m_errors.SetError(25);
		return false;
	}

	// Outer iteration loop : temp(8), checking against UA_HT
	double T8_lower_bound = 0.0;
	double T8_upper_bound = 0.0;
	double last_HT_residual = 0.0;
	double last_T8_guess = 0.0;
	if( UA_HT < 1.0E-12 )			// No high-temp recuperator
	{
		T8_lower_bound = m_temp_last[7 - cpp_offset];		// No iteration necessary
		T8_upper_bound = m_temp_last[7 - cpp_offset];		// No iteration necessary
		m_temp_last[8 - cpp_offset] = m_temp_last[7 - cpp_offset];
		UA_HT_calc = 0.0;
		last_HT_residual = 0.0;
		last_T8_guess = m_temp_last[7 - cpp_offset];		
	}
	else
	{
		T8_lower_bound = m_temp_last[2 - cpp_offset];		// The lower possible value of temp(8)
		T8_upper_bound = m_temp_last[7 - cpp_offset];		// The highest possible value of temp(8)
		m_temp_last[8 - cpp_offset] = (T8_lower_bound + T8_upper_bound)*0.5;	// Bisect bounds for first guess
		UA_HT_calc = -1.0;
		last_HT_residual = UA_HT;					// know a priori that with T8 = T7, UA_calc = 0 therefore residual is UA_HT-0
		last_T8_guess = m_temp_last[7 - cpp_offset];
	}

	CO2_state co2_props;
	int property_error_code = 0;
	int T8_iter = 0;
	// T8_loop
	for( T8_iter = 1; T8_iter <= max_iter; T8_iter++ )
	{
		property_error_code = CO2_TP(m_temp_last[8 - cpp_offset], m_pres_last[8-cpp_offset], &co2_props);		// fully define state 8
		if( property_error_code != 0 )
		{
			m_errors.SetError(26);
			return false;
		}
		m_enth_last[8 - cpp_offset] = co2_props.enth; m_entr_last[8 - cpp_offset] = co2_props.entr; m_dens_last[8 - cpp_offset] = co2_props.dens;

		// Inner iteration loop: temp(9), checking against UA_LT
		double T9_lower_bound, T9_upper_bound, last_LT_residual, last_T9_guess;
		T9_lower_bound = T9_upper_bound = last_LT_residual = last_T9_guess = 0.0;
		if( UA_LT < 1.E-12 )	// no low-temp recuperator
		{
			T9_lower_bound = m_temp_last[8 - cpp_offset];			// no iteration necessary
			T9_upper_bound = m_temp_last[8 - cpp_offset];			// no iteration necessary
			m_temp_last[9 - cpp_offset] = m_temp_last[8 - cpp_offset];
			UA_LT_calc = 0.0;
			last_LT_residual = 0.0;
			last_T9_guess = m_temp_last[8 - cpp_offset];
		}
		else
		{
			T9_lower_bound = m_temp_last[2 - cpp_offset];		// the lower possible value for T9
			T9_upper_bound = m_temp_last[8 - cpp_offset];		// the highest possible value for T9
			m_temp_last[9 - cpp_offset] = (T9_lower_bound + T9_upper_bound)*0.5;	// bisect bounds for first guess
			UA_LT_calc = -1.0;
			last_LT_residual = UA_LT;		// know a priori that with T9=T8, UA_calc = 0 therefore residual is UA_LT - 0
			last_T9_guess = m_temp_last[8 - cpp_offset];
		}

		// T9_loop
		int T9_iter = 0;
		for( T9_iter = 1; T9_iter <= max_iter; T9_iter++ )
		{
			// Determine the outlet state of the recompressor and its specific work
			if( m_cycle_des_par.m_recomp_frac >= 1.E-12 )
			{
				calculate_turbomachinery_outlet_RC_with_Reheating(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], m_cycle_des_par.m_eta_rc,
					true, sub_error_code, m_enth_last[9 - cpp_offset], m_entr_last[9 - cpp_offset], m_dens_last[9 - cpp_offset], m_temp_last[10 - cpp_offset],
					m_enth_last[10 - cpp_offset], m_entr_last[10 - cpp_offset], m_dens_last[10 - cpp_offset], w_rc);

				if( sub_error_code != 0 )
				{
					m_errors.SetError(27);
					m_errors.SetError(sub_error_code);
					return false;
				}			
			}
			else
			{
				w_rc = 0.0;		// the recompressor does not exist
				property_error_code = CO2_TP(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], &co2_props);
				if( property_error_code != 0 )
				{
					m_errors.SetError(28);
					return false;
				}
				m_temp_last[10 - cpp_offset] = m_temp_last[9 - cpp_offset];					// Assume state(10) is the same as state(9)
				m_enth_last[9 - cpp_offset] = m_enth_last[10 - cpp_offset] = co2_props.enth;
				m_entr_last [9 - cpp_offset] = m_entr_last[10 - cpp_offset] = co2_props.entr;
				m_dens_last[9 - cpp_offset] = m_dens_last[10 - cpp_offset] = co2_props.dens;
			}

			// Knowing the specific work of the the recompressing compressor, the required mass flow rate can be determined.
			m_dot_t = m_cycle_des_par.m_W_dot_net / (w_mc * (1.0 - m_cycle_des_par.m_recomp_frac) + w_rc * m_cycle_des_par.m_recomp_frac + w_mt + w_rt);			// total mass flow rate(through turbine)
			if (m_dot_t < 0.0)				// positive power output is not possible with these inputs
			{
				m_errors.SetError(29);
				return false;
			}
			m_dot_rc = m_dot_t * m_cycle_des_par.m_recomp_frac;
			m_dot_mc = m_dot_t - m_dot_rc;
			
			// Calculate the UA value of the low-temperature recuperator.
			if( UA_LT < 1.E-12 )			// no low-temp recuperator (this check is necessary to prevent pressure drops with UA=0 from causing problems)
				Q_dot_LT = 0.0;
			else
				Q_dot_LT = m_dot_t * (m_enth_last[8 - cpp_offset] - m_enth_last[9 - cpp_offset]);
			
			calculate_hxr_UA_RC_with_Reheating(m_cycle_des_par.m_N_sub_hxrs, Q_dot_LT, m_dot_mc, m_dot_t, m_temp_last[2 - cpp_offset], m_temp_last[8 - cpp_offset],
				m_pres_last[2 - cpp_offset], m_pres_last[3 - cpp_offset], m_pres_last[8 - cpp_offset], m_pres_last[9 - cpp_offset], 
				sub_error_code, UA_LT_calc, min_DT_LT);

			if( sub_error_code > 0 )
			{
				if( sub_error_code == 11 )		// second - law violation in hxr, therefore temp(9) is too low
				{
					T9_lower_bound = m_temp_last[9 - cpp_offset];
					m_temp_last[9 - cpp_offset] = (T9_lower_bound + T9_upper_bound)*0.5;		// bisect bounds for next guess
					continue;		// cycle T9_loop
				}
				else
				{
					m_errors.SetError(30);
					m_errors.SetError(sub_error_code);
					return false;
				}
			}

			// Check for convergence and adjust T9 appropriately.
			double UA_LT_residual = UA_LT - UA_LT_calc;
			if( abs(UA_LT_residual) < 1.E-12 )
				break;		// 'exit T9_loop' catches no LT case

			double secant_guess = m_temp_last[9 - cpp_offset] - UA_LT_residual*(last_T9_guess - m_temp_last[9 - cpp_offset]) / (last_LT_residual - UA_LT_residual);	// next guess predicted using secant method

			if(UA_LT_residual < 0.0)			// UA_LT_calc is too big, temp(9) needs to be higher
			{
				if( abs(UA_LT_residual) / UA_LT < m_cycle_des_par.m_tol )
					break;	// 'exit T9_loop' UA_LT converged (residual is negative)
				T9_lower_bound = m_temp_last[9 - cpp_offset];
			}
			else			// UA_LT_calc is too small, temp(9) needs to be lower
			{
				if( UA_LT_residual / UA_LT < m_cycle_des_par.m_tol )
					break; // 'exit T9_loop' UA_LT converged
				T9_upper_bound = m_temp_last[9 - cpp_offset];
			}
			last_LT_residual = UA_LT_residual;				// reset last stored residual value
			last_T9_guess = m_temp_last[9 - cpp_offset];			// reset last stored guess value
			
			// Check if the secant method overshoots and fall back to bisection if it does.
			if( secant_guess <= T9_lower_bound || secant_guess >= T9_upper_bound || secant_guess != secant_guess )
				m_temp_last[9 - cpp_offset] = (T9_lower_bound + T9_upper_bound)*0.5;
			else
				m_temp_last[9 - cpp_offset] = secant_guess;

		}		// End iteration T9

		// Check that T9_loop converged.
		if( T9_iter >= max_iter )
		{
			m_errors.SetError(31);
			return false;
		}
		
		// State 3 can now be fully defined.
		m_enth_last[3 - cpp_offset] = m_enth_last[2 - cpp_offset] + Q_dot_LT / m_dot_mc;		// energy balance on cold stream of low-temp recuperator
		property_error_code = CO2_PH(m_pres_last[3 - cpp_offset], m_enth_last[3 - cpp_offset], &co2_props);
		if( property_error_code != 0 )
		{
			m_errors.SetError(32);
			return false;
		}
		m_temp_last[3 - cpp_offset] = co2_props.temp;   m_entr_last[3 - cpp_offset] = co2_props.entr;     m_dens_last[3 - cpp_offset] = co2_props.dens;

		// Go through mixing valve
		if( m_cycle_des_par.m_recomp_frac >= 1.E-12 )
		{
			m_enth_last[4 - cpp_offset] = (1.0 - m_cycle_des_par.m_recomp_frac)*m_enth_last[3 - cpp_offset] + m_cycle_des_par.m_recomp_frac*m_enth_last[10 - cpp_offset];		// conservation of energy (both sides divided by m_dot_t
			property_error_code = CO2_PH(m_pres_last[4 - cpp_offset], m_enth_last[4 - cpp_offset], &co2_props);
			if( property_error_code != 0 )
			{
				m_errors.SetError(33);
				return false;
			}
			m_temp_last[4 - cpp_offset] = co2_props.temp;    m_entr_last[4 - cpp_offset] = co2_props.entr;      m_dens_last[4 - cpp_offset] = co2_props.dens;
		}
		else		// no mixing value, therefore (4) is equal to (3)
		{
			m_temp_last[4 - cpp_offset] = m_temp_last[3 - cpp_offset];
			m_enth_last[4 - cpp_offset] = m_enth_last[3 - cpp_offset];
			m_entr_last[4 - cpp_offset] = m_entr_last[3 - cpp_offset];
			m_dens_last[4 - cpp_offset] = m_dens_last[3 - cpp_offset];
		}

		// Check for a second law violation at the outlet of the high-temp recuperator.
		if(m_temp_last[4 - cpp_offset] >= m_temp_last[8 - cpp_offset])		// temp(8) is not valid; it must be higher than it is
		{
			T8_lower_bound = m_temp_last[8 - cpp_offset];
			m_temp_last[8 - cpp_offset] = (T8_lower_bound + T8_upper_bound)*0.5;
			continue;		// cycle T8_loop
		}

		// Calculate the UA value of the high-temperature recuperator.
		if( UA_HT < 1.E-12 )		// no high-temp recuperator (this check is necessary to prevent pressure drops with UA=0 from causing problems)
			Q_dot_HT = 0.0;
		else
			Q_dot_HT = m_dot_t * (m_enth_last[7 - cpp_offset] - m_enth_last[8 - cpp_offset]);
		
		calculate_hxr_UA_RC_with_Reheating(m_cycle_des_par.m_N_sub_hxrs, Q_dot_HT, m_dot_t, m_dot_t, m_temp_last[4 - cpp_offset], m_temp_last[7 - cpp_offset],
			m_pres_last[4 - cpp_offset], m_pres_last[5 - cpp_offset], m_pres_last[7 - cpp_offset], m_pres_last[8 - cpp_offset], 
			sub_error_code, UA_HT_calc, min_DT_HT);

		if( sub_error_code > 0 )
		{
			if( sub_error_code == 1 )		// 2nd law violation in hxr, therefore temp(8) is too low
			{
				T8_lower_bound = m_temp_last[8 - cpp_offset];
				m_temp_last[8 - cpp_offset] = (T8_lower_bound + T8_upper_bound)*0.5;	// bisect bounds for next guess
				continue;	// cycle T8_loop
			}
			else
			{
				m_errors.SetError(34);
				m_errors.SetError(sub_error_code);
				return false;
			}
		}

		// Check for convergence and adjust T8 appropriately.
		double UA_HT_residual = UA_HT - UA_HT_calc;

		if( abs(UA_HT_residual) < 1.E-12 )
			break;			// exit T8_loop  !catches no HT case
		
		double secant_guess = m_temp_last[8 - cpp_offset] - UA_HT_residual*(last_T8_guess - m_temp_last[8 - cpp_offset])/(last_HT_residual - UA_HT_residual);		// next guess predicted using secant method

		if(UA_HT_residual < 0.0)			// UA_HT_calc is too big, temp(8) needs to be higher
		{
			if( abs(UA_HT_residual) / UA_HT < m_cycle_des_par.m_tol )
				break;		// exit T8_loop    UA_HT converged (residual is negative)
			T8_lower_bound = m_temp_last[8 - cpp_offset];
		}
		else								// UA_HT_calc is too small, temp(8) needs to be larger
		{
			if( UA_HT_residual / UA_HT < m_cycle_des_par.m_tol )
				break;		// exit T8_loop    UA_HT converged
			T8_upper_bound = m_temp_last[8 - cpp_offset];
		}
		last_HT_residual = UA_HT_residual;			// reset last stored residual value
		last_T8_guess = m_temp_last[8 - cpp_offset];		// reset last stored guess value

		// Check if the secant method overshoots and fall back to bisection if it does.
		if( secant_guess <= T8_lower_bound || secant_guess >= T8_upper_bound )		// secant method overshot, use bisection
			m_temp_last[8 - cpp_offset] = (T8_lower_bound + T8_upper_bound)*0.5;
		else
			m_temp_last[8 - cpp_offset] = secant_guess;

	}		// End iteration on T8

	// Check that T8_loop converged
	if( T8_iter >= max_iter )
	{
		m_errors.SetError(35);
		return false;
	}

	// State 5 can now be fully defined
	m_enth_last[5 - cpp_offset] = m_enth_last[4 - cpp_offset] + Q_dot_HT / m_dot_t;		// Energy balance on cold stream of high-temp recuperator
	property_error_code = CO2_PH(m_pres_last[5 - cpp_offset], m_enth_last[5 - cpp_offset], &co2_props);
	if( property_error_code != 0 )
	{
		m_errors.SetError(36);
		return false;
	}
	m_temp_last[5 - cpp_offset] = co2_props.temp;     m_entr_last[5 - cpp_offset] = co2_props.entr;       m_dens_last[5 - cpp_offset] = co2_props.dens;

	// ****************************************************************
	// Calculate final performance metrics and initialize components.
	// ****************************************************************
		// LT Recuperator
	property_error_code = CO2_TP(m_temp_last[2 - cpp_offset], m_pres_last[9 - cpp_offset], &co2_props);	// enthalpy of hot stream outlet if temp is cold stream inlet
	if(property_error_code != 0)
	{
		m_errors.SetError(37);
		return false;
	}
	double h_hot_ideal = co2_props.enth;
	double eff = Q_dot_LT / (m_dot_t * (m_enth_last[8 - cpp_offset] - h_hot_ideal));
	double C_dot_cold = m_dot_mc*(m_enth_last[3-cpp_offset]-m_enth_last[2-cpp_offset])/(m_temp_last[3-cpp_offset]-m_temp_last[2-cpp_offset]);
	double C_dot_hot = m_dot_t*(m_enth_last[8-cpp_offset]-m_enth_last[9-cpp_offset])/(m_temp_last[8-cpp_offset]-m_temp_last[9-cpp_offset]);

	HX_design_parameters_RC_with_Reheating LT_des_par;
	LT_des_par.m_N_sub = m_cycle_des_par.m_N_sub_hxrs;
	//LT_des_par.m_m_dot_design.resize(2);
	LT_des_par.m_m_dot_design[0] = m_dot_mc;
	LT_des_par.m_m_dot_design[1] = m_dot_t;
	//LT_des_par.m_DP_design.resize(2);
	LT_des_par.m_DP_design[0] = m_pres_last[2-cpp_offset] - m_pres_last[3-cpp_offset];
	LT_des_par.m_DP_design[1] = m_pres_last[8-cpp_offset] - m_pres_last[9-cpp_offset];
	LT_des_par.m_UA_design = UA_LT_calc;
	LT_des_par.m_Q_dot_design = Q_dot_LT;
	LT_des_par.m_min_DT = min_DT_LT;
	m_LT.initialize(LT_des_par);

	// HT recuperator
	property_error_code = CO2_TP(m_temp_last[4-cpp_offset], m_pres_last[4-cpp_offset], &co2_props);		// enthalpy of hot steram outlet if temp is cold stream inlet
	if(property_error_code != 0)
	{
		m_errors.SetError(38);
		return false;
	}
	h_hot_ideal = co2_props.enth;
	eff = Q_dot_HT / (m_dot_t*(m_enth_last[7-cpp_offset] - h_hot_ideal));
	C_dot_cold = m_dot_t * (m_enth_last[5-cpp_offset] - m_enth_last[4-cpp_offset])/(m_temp_last[5-cpp_offset]-m_temp_last[4-cpp_offset]);
	C_dot_hot = m_dot_t * (m_enth_last[7-cpp_offset] - m_enth_last[8-cpp_offset])/(m_temp_last[7-cpp_offset]-m_temp_last[8-cpp_offset]);

	HX_design_parameters_RC_with_Reheating HT_des_par;
	HT_des_par.m_N_sub = m_cycle_des_par.m_N_sub_hxrs;
	//HT_des_par.m_m_dot_design.resize(2);
	HT_des_par.m_m_dot_design[0] = m_dot_t;
	HT_des_par.m_m_dot_design[1] = m_dot_t;
	//HT_des_par.m_DP_design.resize(2);
	HT_des_par.m_DP_design[0] = m_pres_last[4-cpp_offset] - m_pres_last[5-cpp_offset];
	HT_des_par.m_DP_design[1] = m_pres_last[7-cpp_offset] - m_pres_last[8-cpp_offset];
	HT_des_par.m_UA_design = UA_HT_calc;
	HT_des_par.m_Q_dot_design = Q_dot_HT;
	HT_des_par.m_min_DT = min_DT_HT;
	m_HT.initialize(HT_des_par);

	// PHX 
	HX_design_parameters_RC_with_Reheating PHX_des_par;
	PHX_des_par.m_N_sub = m_cycle_des_par.m_N_sub_hxrs;
	// Resize?
	PHX_des_par.m_m_dot_design[0] = m_dot_t;
	PHX_des_par.m_m_dot_design[1] = 0.0;
	// Resize?
	PHX_des_par.m_DP_design[0] = m_pres_last[5-cpp_offset] - m_pres_last[6-cpp_offset];
	PHX_des_par.m_DP_design[1] = 0.0;
	// Leave UA set to NaN
	double Q_dot_PHX = m_dot_t*(m_enth_last[6 - cpp_offset] - m_enth_last[5 - cpp_offset]);
	PHX_des_par.m_Q_dot_design = Q_dot_PHX;
	m_PHX.initialize(PHX_des_par);
	
	// RHX 
	HX_design_parameters_RC_with_Reheating RHX_des_par;
	RHX_des_par.m_N_sub = m_cycle_des_par.m_N_sub_hxrs;
	// Resize?
	RHX_des_par.m_m_dot_design[0] = m_dot_t;
	RHX_des_par.m_m_dot_design[1] = 0.0;
	// Resize?
	RHX_des_par.m_DP_design[0] = m_pres_last[11 - cpp_offset] - m_pres_last[12 - cpp_offset];
	RHX_des_par.m_DP_design[1] = 0.0;
	// Leave UA set to NaN
	double Q_dot_RHX = m_dot_t*(m_enth_last[12 - cpp_offset] - m_enth_last[11 - cpp_offset]);
	RHX_des_par.m_Q_dot_design = Q_dot_RHX;
	m_PHX.initialize(RHX_des_par);

	// Precooler
	HX_design_parameters_RC_with_Reheating PC_des_par;
	PC_des_par.m_N_sub = m_cycle_des_par.m_N_sub_hxrs;
	// Resize?
	PC_des_par.m_m_dot_design[0] = 0.0;
	PC_des_par.m_m_dot_design[1] = m_dot_mc;
	// Resize?
	PC_des_par.m_DP_design[0] = 0.0;
	PC_des_par.m_DP_design[1] = m_pres_last[9-cpp_offset] - m_pres_last[1-cpp_offset];
	// Leave UA set to NaN?
	PC_des_par.m_Q_dot_design = m_dot_mc*(m_enth_last[9-cpp_offset] - m_enth_last[1-cpp_offset]);
	m_PC.initialize(PC_des_par);

	// *************************************************
	// Only need 'eta_thermal_last' for optimization routine:
	// Cut out initializing the heat exchangers and turbomachinery until optimized?
	// *************************************************

	// Recompression Cycle
	m_W_dot_net_last = w_mc*m_dot_mc + w_rc*m_dot_rc + w_mt*m_dot_t + w_rt*m_dot_t;
	m_eta_thermal_last = m_W_dot_net_last / (Q_dot_PHX + Q_dot_RHX);
	if( m_eta_thermal_last != m_eta_thermal_last )
	{
		m_errors.SetError(12341);		// Need to map error code
		return false;	
	}

		// Main Compressor
	compressor_design_parameters_RC_with_Reheating mc_des_par;
	mc_des_par.m_type = m_cycle_des_par.m_mc_type;
	mc_des_par.m_w_design = w_mc;
	mc_des_par.m_eta_design = m_cycle_des_par.m_eta_mc;
	mc_des_par.m_m_dot_design = m_dot_mc;
	mc_des_par.m_rho_in_design = m_dens_last[1 - cpp_offset];
	mc_des_par.m_recomp_frac_design = 1.0 - m_cycle_des_par.m_recomp_frac;
	if( !m_mc.initialize(mc_des_par) )
	{
		m_errors.SetError(12342);		// Need to map error code
		return false;
	}

	// Recompressor
	compressor_design_parameters_RC_with_Reheating rc_des_par;
	rc_des_par.m_type = m_cycle_des_par.m_rc_type;
	rc_des_par.m_w_design = w_rc;
	rc_des_par.m_eta_design = m_cycle_des_par.m_eta_rc;
	rc_des_par.m_m_dot_design = m_dot_rc;
	rc_des_par.m_rho_in_design = m_dens_last[9 - cpp_offset];
	rc_des_par.m_recomp_frac_design = m_cycle_des_par.m_recomp_frac;
	if( !m_rc.initialize(rc_des_par) )
	{
		m_errors.SetError(12343);		// Need to map error code
		return false;
	}

	// Main Turbine
	double N_mt_in;
	if( m_cycle_des_par.m_N_mt < 0.0 )
		N_mt_in = m_mc.get_N_design();
	else
		N_mt_in = m_cycle_des_par.m_N_mt;

	turbine_design_parameters_RC_with_Reheating turb_des_par;
	turb_des_par.m_w_design = w_mt;
	turb_des_par.m_eta_design = m_cycle_des_par.m_eta_mt;
	turb_des_par.m_m_dot_design = m_dot_t;
	turb_des_par.m_N_design = N_mt_in;
	turb_des_par.m_rho_out_design = m_dens_last[11-cpp_offset];
	if( !m_mt.initialize(turb_des_par) )
	{
		m_errors.SetError(12344);		// Need to map error code
		return false;
	}

	return true;
}

bool RecompCycle_with_Reheating::design_no_opt()
{
	if (design())
	{
		set_des_data();
		return true;
	}
	else
	{
		m_eta_thermal_des = 0.0;
		m_W_dot_net_des = 0.0;
		return false;
	}
}

bool RecompCycle_with_Reheating::optimal_design()
{
	// Use booleans to determine whether to optimize the design for any combination of: compressor outlet pressure, pressure ratio, recompression fraction, and UA distribution
	// These boolean parameters and associated fixed values (if boolean indicate no optimization of parameter) or guess values (if boolean indicates optimization) should
	// ... be set during the constructor call for the RecompCycle class. These and the constant cycle parameters are stored in 'cycle_design_parameters' structure named 'm_cycle_des_par'

	int index = 0;

	std::vector<double> x(0);
	std::vector<double> lb(0);
	std::vector<double> ub(0);
	std::vector<double> scale(0);

	if (!m_cycle_des_par.m_fixed_P_rt_in)
	{
		x.push_back(m_cycle_des_par.m_P_rt_in_guess);
		lb.push_back(7377*1.05);
		ub.push_back(m_cycle_des_par.m_P_high_limit/1.05);
		scale.push_back(500.0);

		index++;
	}

	if (!m_cycle_des_par.m_fixed_P_mc_out)
	{
		x.push_back(m_cycle_des_par.m_P_mc_out_guess);
		lb.push_back(100.0);
		ub.push_back(m_cycle_des_par.m_P_high_limit);
		scale.push_back(500.0);

		index++;
	}

	if (!m_cycle_des_par.m_fixed_PR_mc)
	{
		x.push_back(m_cycle_des_par.m_PR_mc_guess);
		lb.push_back(0.1);
		double PR_max = m_cycle_des_par.m_P_high_limit / 100.0;
		ub.push_back(PR_max);
		scale.push_back(0.2);

		index++;
	}

	if (!m_cycle_des_par.m_fixed_recomp_frac)
	{
		x.push_back(m_cycle_des_par.m_recomp_frac_guess);
		lb.push_back(0.0);
		ub.push_back(1.0);
		scale.push_back(0.05);

		index++;
	}

	if (!m_cycle_des_par.m_fixed_LT_frac)
	{
		x.push_back(m_cycle_des_par.m_LT_frac_guess);
		lb.push_back(0.0);
		ub.push_back(1.0);
		scale.push_back(0.05);

		index++;
	}

	if (index > 0)
	{
		// Set up instance of nlopt class and set optimization parameters
		nlopt::opt        opt_des_cycle(nlopt::LN_SBPLX, index);
		opt_des_cycle.set_lower_bounds(lb);
		opt_des_cycle.set_upper_bounds(ub);
		opt_des_cycle.set_initial_step(scale);
		opt_des_cycle.set_xtol_rel(m_cycle_des_par.m_opt_tol);

		// Set max objective function
		opt_des_cycle.set_max_objective(nlopt_callback_opt_des_RC_with_Reheating, this);		// Calls wrapper/callback that calls 'design_point_eta', which optimizes design point eta through repeated calls to 'design'
		double max_f = std::numeric_limits<double>::quiet_NaN();
		nlopt::result   result_des_cycle = opt_des_cycle.optimize(x, max_f);

		// After optimization solves, get back the parameters that result in the maximum efficiency
		index = 0;

		if (!m_cycle_des_par.m_fixed_P_rt_in)
		{
			m_cycle_des_par.m_P_rt_in = x[index];
			index++;
		}

		if (!m_cycle_des_par.m_fixed_P_mc_out)
		{
			m_cycle_des_par.m_P_mc_out = x[index];
			index++;
		}

		if (!m_cycle_des_par.m_fixed_PR_mc)
		{
			m_cycle_des_par.m_PR_mc = x[index];
			index++;
		}

		if (!m_cycle_des_par.m_fixed_recomp_frac)
		{
			m_cycle_des_par.m_recomp_frac = x[index];
			if (m_cycle_des_par.m_recomp_frac <= 1.E-4)
				m_cycle_des_par.m_recomp_frac = 0.0;

			index++;
		}

		if (!m_cycle_des_par.m_fixed_LT_frac)
		{
			m_cycle_des_par.m_LT_frac = x[index];
			index++;
		}
	}

	// Solve a final time with the design parameters to set all component design-point information and check that it solves
	if (design())
	{
		set_des_data();
		return true;
	}
	else
	{
		m_eta_thermal_des = 0.0;
		m_W_dot_net_des = 0.0;
		return false;
	}
}

double RecompCycle_with_Reheating::design_point_eta(const std::vector<double> &x)
{
	// Called by nlopt optimization routine, which varies the values in the 'x' vector to maximize the thermal efficiency of the design cycle by calling 'design' method
	// 
	int index = 0;

	if (!m_cycle_des_par.m_fixed_P_rt_in)
	{
		m_cycle_des_par.m_P_rt_in = x[index];
		if (m_cycle_des_par.m_P_rt_in_guess <100)
			return 0.0;
		index++;
	}
	//
	if (!m_cycle_des_par.m_fixed_P_mc_out)
	{
		m_cycle_des_par.m_P_mc_out = x[index];
		if (m_cycle_des_par.m_P_mc_out > m_cycle_des_par.m_P_high_limit)
			return 0.0;
		index++;
	}

	if (!m_cycle_des_par.m_fixed_PR_mc)
	{
		m_cycle_des_par.m_PR_mc = x[index];
		if (m_cycle_des_par.m_PR_mc > 15.0)
			return 0.0;
		double P_mc_in = m_cycle_des_par.m_P_mc_out / m_cycle_des_par.m_PR_mc;
		if (P_mc_in >= m_cycle_des_par.m_P_mc_out)
			return 0.0;
		if (P_mc_in <= 7377)
			return 0.0;
		index++;
	}

	if (!m_cycle_des_par.m_fixed_recomp_frac)
	{
		m_cycle_des_par.m_recomp_frac = x[index];
		if (m_cycle_des_par.m_recomp_frac < 0.0)
			return 0.0;
		index++;
	}

	if (!m_cycle_des_par.m_fixed_LT_frac)
	{
		m_cycle_des_par.m_LT_frac = x[index];
		if (m_cycle_des_par.m_LT_frac > 1.0 || m_cycle_des_par.m_LT_frac < 0.0)
			return 0.0;
		index++;
	}

	if (design())
		return m_eta_thermal_last;
	else
		return 0.0;
}

double nlopt_callback_opt_des_RC_with_Reheating(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	RecompCycle_with_Reheating *frame = static_cast<RecompCycle_with_Reheating*>(data);
	if (frame != NULL) return frame->design_point_eta(x);
}