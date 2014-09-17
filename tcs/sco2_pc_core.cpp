#include "sco2_pc_core.h"
#include "CO2_properties.h"
#include <limits>
#include <algorithm>

#include "nlopt.hpp"
#include "nlopt_callbacks.h"

#include "fmin.h"

using namespace std;


const double C_turbine::m_nu_design = 0.7476;
const double C_compressor::m_snl_phi_design = 0.02971;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
const double C_compressor::m_snl_phi_min = 0.02;				//[-] Approximate surge limit for SNL compressor
const double C_compressor::m_snl_phi_max = 0.05;				//[-] Approximate x-intercept for SNL compressor
const double C_recompressor::m_snl_phi_design = 0.02971;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
const double C_recompressor::m_snl_phi_min = 0.02;				//[-] Approximate surge limit for SNL compressor
const double C_recompressor::m_snl_phi_max = 0.05;				//[-] Approximate x-intercept for SNL compressor

void calculate_turbomachinery_outlet_1(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double eta /*-*/, bool is_comp, int & error_code, double & spec_work /*kJ/kg*/)
{
	double enth_in, entr_in, dens_in, temp_out, enth_out, entr_out, dens_out;

	calculate_turbomachinery_outlet_1(T_in, P_in, P_out, eta, is_comp, error_code, enth_in, entr_in, dens_in, temp_out, enth_out, entr_out, dens_out, spec_work);
}

void calculate_turbomachinery_outlet_1(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double eta /*-*/, bool is_comp, int & error_code, double & enth_in /*kJ/kg*/, double & entr_in /*kJ/kg-K*/,
	double & dens_in /*kg/m3*/, double & temp_out /*K*/, double & enth_out /*kJ/kg*/, double & entr_out /*kJ/kg-K*/, double & dens_out /*kg/m3*/, double & spec_work /*kJ/kg*/)
{
	/*Calculates the outlet state of a compressor or turbine using its isentropic efficiency.
	is_comp = .true.means the turbomachine is a compressor(w = w_s / eta)
	is_comp = .false.means the turbomachine is a turbine(w = w_s * eta) */

	CO2_state co2_props;

	error_code = 0;

	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);		// properties at the inlet conditions
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;
	dens_in = co2_props.dens;

	prop_error_code = CO2_PS(P_out, s_in, &co2_props);			// outlet enthalpy if compression/expansion is isentropic
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
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
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
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

void calculate_hxr_UA_1(int N_hxrs, double Q_dot /*units?*/, double m_dot_c, double m_dot_h, double T_c_in, double T_h_in, double P_c_in, double P_c_out, double P_h_in, double P_h_out,
	int & error_code, double & UA, double & min_DT)
{
	/*Calculates the UA of a heat exchanger given its mass flow rates, inlet temperatures, and a heat transfer rate.
	Note: the heat transfer rate must be positive.*/

	// Check inputs
	if( Q_dot < 0.0 )
	{
		error_code = 4;
		return;
	}
	if( T_h_in < T_c_in )
	{
		error_code = 5;
		return;
	}
	if( P_h_in < P_h_out )
	{
		error_code = 6;
		return;
	}
	if( P_c_in < P_c_out )
	{
		error_code = 7;
		return;
	}
	if( Q_dot <= 1.E-14 )		// very low Q_dot; assume it is zero
	{
		UA = 0.0;
		min_DT = T_h_in - T_c_in;
		return;
	}

	// Calculate inlet enthalpies from known state points
	CO2_state co2_props;
	int prop_error_code = CO2_TP(T_c_in, P_c_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double h_c_in = co2_props.enth;

	prop_error_code = CO2_TP(T_h_in, P_h_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = 9;
		return;
	}
	double h_h_in = co2_props.enth;

	// Calculate outlet enthalpies from energy balance
	double h_c_out = h_c_in + Q_dot / m_dot_c;
	double h_h_out = h_h_in - Q_dot / m_dot_h;

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
		if( prop_error_code != 0 )
		{
			error_code = 12;
			return;
		}
		double T_h = co2_props.temp;

		prop_error_code = CO2_PH(P_c, h_c, &co2_props);
		if( prop_error_code != 0 )
		{
			error_code = 13;
			return;
		}
		double T_c = co2_props.temp;

		// Check that 2nd law was not violated
		if( T_c >= T_h )
		{
			error_code = 11;
			return;
		}

		// Track the minimum temperature difference in the heat exchanger
		min_DT = min(min_DT, T_h - T_c);

		// Perform effectiveness-NTU and UA calculations 
		if( i > 0 )
		{
			double C_dot_h = m_dot_h*(h_h_prev - h_h) / (T_h_prev - T_h);			// [kW/K] hot stream capacitance rate
			double C_dot_c = m_dot_c*(h_c_prev - h_c) / (T_c_prev - T_c);			// [kW/K] cold stream capacitance rate
			double C_dot_min = min(C_dot_h, C_dot_c);				// [kW/K] Minimum capacitance stream
			double C_dot_max = max(C_dot_h, C_dot_c);				// [kW/K] Maximum capacitance stream
			double C_R = C_dot_min / C_dot_max;						// [-] Capacitance ratio of sub-heat exchanger
			double eff = (Q_dot / (double)N_hxrs) / (C_dot_min*(T_h_prev - T_c));	// [-] Effectiveness of each sub-heat exchanger
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
	if( UA != UA )
	{
		error_code = 14;
		return;
	}

	return;
};

void isen_eta_from_poly_eta(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double poly_eta /*-*/, bool is_comp, int & error_code, double & isen_eta)
{
	/* 9.3.14: code written by John Dyreby, translated to C++ by Ty Neises
	! Calculate the isentropic efficiency that corresponds to a given polytropic efficiency
	! for the expansion or compression from T_in and P_in to P_out.
	!
	! Inputs:
	!   T_in -- inlet temperature (K)
	!   P_in -- inlet pressure (kPa)
	!   P_out -- outlet pressure (kPa)
	!   poly_eta -- polytropic efficiency (-)
	!   is_comp -- if .true., model a compressor (w = w_s / eta); if .false., model a turbine (w = w_s * eta)
	!
	! Outputs:
	!   error_trace -- an ErrorTrace object
	!   isen_eta -- the equivalent isentropic efficiency (-)
	!
	! Notes:
	!   1) Integration of small DP is approximated numerically by using 200 stages.
	!   2) No error checking is performed on the inlet and outlet pressures; valid pressure ratios are assumed. */

	CO2_state co2_props;
	
	// Properties at the inlet conditions
	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
	if(prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;

	// Outlet enthalpy if compression/expansion is isentropic
	prop_error_code = CO2_PS(P_out, s_in, &co2_props);
	if(prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	double h_s_out = co2_props.enth;

	double stage_P_in = P_in;		// Initialize first stage inlet pressure
	double stage_h_in = h_in;		// Initialize first stage inlet enthalpy
	double stage_s_in = s_in;		// Initialize first stage inlet entropy

	int N_stages = 200;

	double stage_DP = (P_out - P_in) / (double)N_stages;

	double stage_P_out = -999.9;
	double stage_h_out = -999.9;

	for( int i = 1; i <= N_stages; i++ )
	{
		stage_P_out = stage_P_in + stage_DP;

		// Outlet enthalpy if compression/expansion is isentropic
		prop_error_code = CO2_PS(stage_P_out, stage_s_in, &co2_props);
		if( prop_error_code != 0 )
		{
			error_code = prop_error_code;
			return;
		}
		double stage_h_s_out = co2_props.enth;

		double w_s = stage_h_in - stage_h_s_out;		// specific work if process is isentropic
		double w = numeric_limits<double>::quiet_NaN();
		if( is_comp )
			w = w_s / poly_eta;
		else
			w = w_s * poly_eta;
		stage_h_out = stage_h_in - w;

		// Reset next stage inlet values
		stage_P_in = stage_P_out;
		stage_h_in = stage_h_out;

		prop_error_code = CO2_PH(stage_P_in, stage_h_in, &co2_props);
		if( prop_error_code != 0 )
		{
			error_code = prop_error_code;
			return;
		}
		stage_s_in = co2_props.entr;
	}

	// Note: last stage outlet enthalpy is equivalent to turbomachinery outlet enthalpy
	if( is_comp )
		isen_eta = (h_s_out - h_in) / (stage_h_out - h_in);
	else
		isen_eta = (stage_h_out - h_in) / (h_s_out - h_in);
}

void C_RecompCycle::design_core(int & error_code)
{
	CO2_state co2_props;

	int max_iter = 500;
	double temperature_tolerance = 1.E-6;		// Temp differences below this are considered zero

	int cpp_offset = 1;

	// Initialize a few variables
	double m_dot_t, m_dot_mc, m_dot_rc, Q_dot_LT, Q_dot_HT, UA_LT_calc, UA_HT_calc;
	m_dot_t = m_dot_mc = m_dot_rc = Q_dot_LT = Q_dot_HT = UA_LT_calc = UA_HT_calc = 0.0;

	m_temp_last[1 - cpp_offset] = ms_des_par.m_T_mc_in;
	m_pres_last[1 - cpp_offset] = ms_des_par.m_P_mc_in;
	m_pres_last[2 - cpp_offset] = ms_des_par.m_P_mc_out;
	m_temp_last[6 - cpp_offset] = ms_des_par.m_T_t_in;

	// Apply pressure drops to heat exchangers, fully defining the pressures at all states
	if(ms_des_par.m_DP_LT[1-cpp_offset] < 0.0)
		m_pres_last[3-cpp_offset] = m_pres_last[2-cpp_offset] - m_pres_last[2-cpp_offset]*fabs(ms_des_par.m_DP_LT[1-cpp_offset]);	// relative pressure drop specified for LT recuperator (cold stream)
	else
		m_pres_last[3-cpp_offset] = m_pres_last[2-cpp_offset] - ms_des_par.m_DP_LT[1-cpp_offset];									// absolute pressure drop specified for LT recuperator (cold stream)

	if(ms_des_par.m_UA_LT < 1.0E-12)
		m_pres_last[3-cpp_offset] = m_pres_last[2-cpp_offset];		// If there is no LT recuperator, there is no pressure drop

	m_pres_last[4-cpp_offset] = m_pres_last[3-cpp_offset];			// Assume no pressure drop in mixing valve
	m_pres_last[10-cpp_offset] = m_pres_last[3-cpp_offset];			// Assume no pressure drop in mixing valve

	if(ms_des_par.m_DP_HT[1-cpp_offset] < 0.0)
		m_pres_last[5-cpp_offset] = m_pres_last[4-cpp_offset] - m_pres_last[4-cpp_offset]*fabs(ms_des_par.m_DP_HT[1-cpp_offset]);	// relative pressure drop specified for HT recuperator (cold stream)
	else
		m_pres_last[5-cpp_offset] = m_pres_last[4-cpp_offset] - ms_des_par.m_DP_HT[1-cpp_offset];									// absolute pressure drop specified for HT recuperator (cold stream)

	if(ms_des_par.m_UA_HT < 1.0E-12)
		m_pres_last[5-cpp_offset] = m_pres_last[4-cpp_offset];		// If there is no HT recuperator, there is no pressure drop

	if(ms_des_par.m_DP_PHX[1-cpp_offset] < 0.0)
		m_pres_last[6-cpp_offset] = m_pres_last[5-cpp_offset] - m_pres_last[5-cpp_offset]*fabs(ms_des_par.m_DP_PHX[1-cpp_offset]);	// relative pressure drop specified for PHX
	else
		m_pres_last[6-cpp_offset] = m_pres_last[5-cpp_offset] - ms_des_par.m_DP_PHX[1-cpp_offset];									// absolute pressure drop specified for PHX

	if(ms_des_par.m_DP_PC[2-cpp_offset] < 0.0)
		m_pres_last[9-cpp_offset] = m_pres_last[1-cpp_offset]/(1.0 - fabs(ms_des_par.m_DP_PC[2-cpp_offset]));	// relative pressure drop specified for precooler: P1=P9-P9*rel_DP => P1=P9*(1-rel_DP)
	else
		m_pres_last[9-cpp_offset] = m_pres_last[1-cpp_offset] + ms_des_par.m_DP_PC[2-cpp_offset];

	if(ms_des_par.m_DP_LT[2-cpp_offset] < 0.0)
		m_pres_last[8-cpp_offset] = m_pres_last[9-cpp_offset]/(1.0 - fabs(ms_des_par.m_DP_LT[2-cpp_offset]));	// relative pressure drop specified for LT recuperator (hot stream)
	else
		m_pres_last[8-cpp_offset] = m_pres_last[9-cpp_offset] + ms_des_par.m_DP_LT[2-cpp_offset];				// absolute pressure drop specified for LT recuperator (hot stream)

	if(ms_des_par.m_UA_LT < 1.0E-12)
		m_pres_last[8-cpp_offset] = m_pres_last[9-cpp_offset];		// if there is no LT recuperator, there is no pressure drop

	if(ms_des_par.m_DP_HT[2-cpp_offset] < 0.0)
		m_pres_last[7-cpp_offset] = m_pres_last[8-cpp_offset]/(1.0 - fabs(ms_des_par.m_DP_HT[2-cpp_offset]));	// relative pressure drop specified for HT recuperator (hot stream)
	else
		m_pres_last[7-cpp_offset] = m_pres_last[8-cpp_offset] + ms_des_par.m_DP_HT[2-cpp_offset];				// absolute pressure drop specified for HT recuperator (hot stream)

	if(ms_des_par.m_UA_HT < 1.0E-12)
		m_pres_last[7-cpp_offset] = m_pres_last[8-cpp_offset];		// if there is no HT recuperator, there is no pressure drop

	// Determine equivalent isentropic efficiencies for main compressor and turbine, if necessary.
	double eta_mc_isen = std::numeric_limits<double>::quiet_NaN();
	double eta_t_isen = std::numeric_limits<double>::quiet_NaN();
	if(ms_des_par.m_eta_mc < 0.0)
	{
		int poly_error_code = 0;
		
		isen_eta_from_poly_eta(m_temp_last[1-cpp_offset], m_pres_last[1-cpp_offset], m_pres_last[2-cpp_offset], fabs(ms_des_par.m_eta_mc),
			true, poly_error_code, eta_mc_isen);

		if(poly_error_code != 0)
		{
			error_code = poly_error_code;
			return;
		}
	}
	else
		eta_mc_isen = ms_des_par.m_eta_mc;

	if(ms_des_par.m_eta_t < 0.0)
	{
		int poly_error_code = 0;

		isen_eta_from_poly_eta(m_temp_last[6-cpp_offset], m_pres_last[6-cpp_offset], m_pres_last[7-cpp_offset], fabs(ms_des_par.m_eta_t),
			false, poly_error_code, eta_t_isen);
	
		if(poly_error_code != 0)
		{
			error_code = poly_error_code;
			return;
		}
	}
	else
		eta_t_isen = ms_des_par.m_eta_t;

	// Determine the outlet state and specific work for the main compressor and turbine.
	int comp_error_code = 0;
	double w_mc = std::numeric_limits<double>::quiet_NaN();
		// Main compressor
	calculate_turbomachinery_outlet_1(m_temp_last[1-cpp_offset], m_pres_last[1-cpp_offset], m_pres_last[2-cpp_offset], eta_mc_isen, true,
		comp_error_code, m_enth_last[1-cpp_offset], m_entr_last[1-cpp_offset], m_dens_last[1-cpp_offset], m_temp_last[2-cpp_offset],
		m_enth_last[2-cpp_offset], m_entr_last[2-cpp_offset], m_dens_last[2-cpp_offset], w_mc);

	if(comp_error_code != 0)
	{
		error_code = comp_error_code;
		return;
	}

	int turbine_error_code = 0;
	double w_t = std::numeric_limits<double>::quiet_NaN();
		// Turbine
	calculate_turbomachinery_outlet_1(m_temp_last[6-cpp_offset], m_pres_last[6-cpp_offset], m_pres_last[7-cpp_offset], eta_t_isen, false,
		turbine_error_code, m_enth_last[6-cpp_offset], m_entr_last[6-cpp_offset], m_dens_last[6-cpp_offset], m_temp_last[7-cpp_offset],
		m_enth_last[7-cpp_offset], m_entr_last[7-cpp_offset], m_dens_last[7-cpp_offset], w_t);

	if(turbine_error_code != 0)
	{
		error_code = turbine_error_code;
		return;
	}

	// Check that this cycle can produce power
	double eta_rc_isen = std::numeric_limits<double>::quiet_NaN();
	double w_rc = std::numeric_limits<double>::quiet_NaN();
	if(ms_des_par.m_recomp_frac >= 1.E-12)
	{
		if(ms_des_par.m_eta_rc < 0.0)		// need to convert polytropic efficiency to isentropic efficiency
		{
			int rc_error_code = 0;

			isen_eta_from_poly_eta(m_temp_last[2-cpp_offset], m_pres_last[9-cpp_offset], m_pres_last[10-cpp_offset], fabs(ms_des_par.m_eta_rc),
				true, rc_error_code, eta_rc_isen);
		
			if(rc_error_code != 0)
			{
				error_code = rc_error_code;
				return;
			}
		}
		else
			eta_rc_isen = ms_des_par.m_eta_rc;

		int rc_error_code = 0;
		calculate_turbomachinery_outlet_1(m_temp_last[2-cpp_offset], m_pres_last[9-cpp_offset], m_pres_last[10-cpp_offset], eta_rc_isen,
			true, rc_error_code, w_rc);

		if(rc_error_code != 0)
		{
			error_code = rc_error_code;
			return;
		}
	}
	else
		w_rc = 0.0;

	if(w_mc + w_rc + w_t <= 0.0)	// positive net power is impossible; return an error
	{
		error_code = 25;
		return;
	}

	// Outer iteration loop: temp(8), checking against UA_HT
	double T8_lower_bound, T8_upper_bound, last_HT_residual, last_T8_guess;
	T8_lower_bound = T8_upper_bound = last_HT_residual = last_T8_guess = std::numeric_limits<double>::quiet_NaN();
	if(ms_des_par.m_UA_HT < 1.0E-12)		// no high-temp recuperator
	{
		T8_lower_bound = m_temp_last[7-cpp_offset];		// no iteration necessary
		T8_upper_bound = m_temp_last[7-cpp_offset];		// no iteration necessary
		m_temp_last[8-cpp_offset] = m_temp_last[7-cpp_offset];
		UA_HT_calc = 0.0;
		last_HT_residual = 0.0;
		last_T8_guess = m_temp_last[7-cpp_offset];
	}
	else
	{
		T8_lower_bound = m_temp_last[2-cpp_offset];		// the absolute lower temp(8) could be
		T8_upper_bound = m_temp_last[7-cpp_offset];		// the absolutely highest temp(8) could be
		m_temp_last[8-cpp_offset] = (T8_lower_bound + T8_upper_bound)*0.5;		// bisect bounds for first guess
		UA_HT_calc = -1.0;
		last_HT_residual = ms_des_par.m_UA_HT;			// know a priori that with T8 = T7, UA_calc = 0 therefore residual is UA_HT - 0.0
		last_T8_guess = m_temp_last[7-cpp_offset];
	}

	int prop_error_code = 0;

	double T9_lower_bound, T9_upper_bound, last_LT_residual, last_T9_guess;
	T9_lower_bound = T9_upper_bound = last_LT_residual = last_T9_guess = std::numeric_limits<double>::quiet_NaN();

	double min_DT_LT = std::numeric_limits<double>::quiet_NaN();
	double min_DT_HT = std::numeric_limits<double>::quiet_NaN();

	int T8_iter = -1;
	for( T8_iter = 0; T8_iter < max_iter; T8_iter++ )
	{
		// Fully define state 8
		prop_error_code = CO2_TP(m_temp_last[8-cpp_offset], m_pres_last[8-cpp_offset], &co2_props);
		if(prop_error_code != 0)
		{
			error_code = prop_error_code;
			return;
		}
		m_enth_last[8-cpp_offset] = co2_props.enth;
		m_entr_last[8-cpp_offset] = co2_props.entr;
		m_dens_last[8-cpp_offset] = co2_props.dens;
	
		// Inner iteration loop: temp(9), checking against UA_LT
		if(ms_des_par.m_UA_LT < 1.0E-12)	// no low-temperature recuperator
		{
			T9_lower_bound = m_temp_last[8-cpp_offset];		// no iteration necessary
			T9_upper_bound = m_temp_last[8-cpp_offset];		// no iteration necessary
			m_temp_last[9-cpp_offset] = m_temp_last[8-cpp_offset];
			UA_LT_calc = 0.0;
			last_LT_residual = 0.0;
			last_T9_guess = m_temp_last[8-cpp_offset];		
		}
		else
		{
			T9_lower_bound = m_temp_last[2-cpp_offset];		// the absolute lowest temp(9) could be
			T9_upper_bound = m_temp_last[8-cpp_offset];		// the absolute highest temp(9) could be
			m_temp_last[9-cpp_offset] = (T9_lower_bound + T9_upper_bound)*0.5;	// biset bounds for first guess
			UA_LT_calc = -1.0;
			last_LT_residual = ms_des_par.m_UA_LT;			// know a priori that with T9 = T8, UA_calc = 0 therefore residual is UA_LT - 0
			last_T9_guess = m_temp_last[8-cpp_offset];
		}
	
		int T9_iter = -1;
		for( T9_iter = 0; T9_iter < max_iter; T9_iter++ )
		{
			// Determine the outlet state of the recompressing compressor and its specific work
			if(ms_des_par.m_recomp_frac >= 1.E-12)
			{
				if(ms_des_par.m_eta_rc < 0.0)		// recalculate isentropic efficiency of recompressing compressor (because T9 changes)
				{
					int rc_error_code = 0;
					isen_eta_from_poly_eta(m_temp_last[9-cpp_offset], m_pres_last[9-cpp_offset], m_pres_last[10-cpp_offset], fabs(ms_des_par.m_eta_rc),true,
						rc_error_code, eta_rc_isen);
				
					if(rc_error_code != 0)
					{
						error_code = rc_error_code;
						return;
					}
				}
				else
				{
					eta_rc_isen = ms_des_par.m_eta_rc;
				}			
			
				int rc_error_code = 0;
				calculate_turbomachinery_outlet_1(m_temp_last[9-cpp_offset], m_pres_last[9-cpp_offset], m_pres_last[10-cpp_offset], eta_rc_isen, true, rc_error_code,
					m_enth_last[9-cpp_offset], m_entr_last[9-cpp_offset], m_dens_last[9-cpp_offset], m_temp_last[10-cpp_offset], m_enth_last[10-cpp_offset], m_entr_last[10-cpp_offset],
					m_dens_last[10-cpp_offset], w_rc);

				if(rc_error_code != 0)
				{
					error_code = rc_error_code;
					return;
				}
			}
			else
			{
				w_rc = 0.0;		// the recompressing compressor does not exist
				prop_error_code = CO2_TP(m_temp_last[9-cpp_offset], m_pres_last[9-cpp_offset], &co2_props);
				if(prop_error_code != 0)		// fully define state 9
				{
					error_code = prop_error_code;
					return;
				}
				m_enth_last[9-cpp_offset] = co2_props.enth;
				m_entr_last[9-cpp_offset] = co2_props.entr;
				m_dens_last[9-cpp_offset] = co2_props.dens;
				m_temp_last[10-cpp_offset] = m_temp_last[9-cpp_offset];		// assume state 10 is the same as state 9
				m_enth_last[10-cpp_offset] = m_enth_last[9-cpp_offset];
				m_entr_last[10-cpp_offset] = m_entr_last[9-cpp_offset];
				m_dens_last[10-cpp_offset] = m_dens_last[9-cpp_offset];
			}

			// Knowing the specific work of the recompressor, the required mass flow rate can be calculated
			m_dot_t = ms_des_par.m_W_dot_net/(w_mc*(1.0-ms_des_par.m_recomp_frac) + w_rc*ms_des_par.m_recomp_frac + w_t);	// Required mass flow rate through turbine
			if(m_dot_t < 0.0)		// positive power output is not possible with these inputs
			{
				error_code = 29;
				return;
			}
			m_dot_rc = m_dot_t * ms_des_par.m_recomp_frac;		// apply definition of recompression fraction
			m_dot_mc = m_dot_t - m_dot_rc;						// mass balance

			// Calculate the UA value of the low-temperature recuperator
			if( ms_des_par.m_UA_LT < 1.0E-12 )		// no low-temp recuperator (this check is necessary to prevent pressure drops with UA=0 from causing problems)
				Q_dot_LT = 0.0;
			else
				Q_dot_LT = m_dot_t * (m_enth_last[8-cpp_offset] - m_enth_last[9-cpp_offset]);

			int hx_error_code = 0;
			min_DT_LT = std::numeric_limits<double>::quiet_NaN();
			calculate_hxr_UA_1(ms_des_par.m_N_sub_hxrs,Q_dot_LT,m_dot_mc,m_dot_t,m_temp_last[2-cpp_offset],m_temp_last[8-cpp_offset],
				m_pres_last[2-cpp_offset],m_pres_last[3-cpp_offset],m_pres_last[8-cpp_offset],m_pres_last[9-cpp_offset],
				hx_error_code, UA_LT_calc, min_DT_LT);

			if(hx_error_code != 0)
			{
				if(hx_error_code == 11)		// second-law violation in hxr, therefore temp(9) is too low
				{
					T9_lower_bound = m_temp_last[9-cpp_offset];
					m_temp_last[9-cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);		// bisect bounds for next guess
					continue;				
				}
				else
				{
					error_code = hx_error_code;
					return;
				}
			}

			// Check for convergence and adjust T9 appropriately
			double UA_LT_residual = ms_des_par.m_UA_LT - UA_LT_calc;

			if( fabs(UA_LT_residual) < 1.0E-12 )		// catches no LT case
				break;

			double secant_guess = m_temp_last[9-cpp_offset] - UA_LT_residual*(last_T9_guess-m_temp_last[9-cpp_offset])/(last_LT_residual-UA_LT_residual);	// next guess predicted using secant method

			if(UA_LT_residual < 0.0)		// UA_LT_calc is too big, temp(9) needs to be higher
			{
				if( fabs(UA_LT_residual) / ms_des_par.m_UA_LT < ms_des_par.m_tol )
					break;

				T9_lower_bound = m_temp_last[9-cpp_offset];
			}
			else		// UA_LT_calc is too small, temp(9) needs to be lower
			{
				if( UA_LT_residual / ms_des_par.m_UA_LT < ms_des_par.m_tol )	// UA_LT converged
					break;

				if( min_DT_LT < temperature_tolerance )		// UA_calc is still too low but there isn't anywhere to go so it's ok (catches huge UA values)
					break;

				T9_upper_bound = m_temp_last[9-cpp_offset];
			}

			last_LT_residual = UA_LT_residual;			// reset lsat stored residual value
			last_T9_guess = m_temp_last[9-cpp_offset];	// reset last stored guess value

			// Check if the secant method overshoots and fall back to bisection if it does
			if(secant_guess <= T9_lower_bound || secant_guess >= T9_upper_bound || secant_guess != secant_guess)	// secant method overshot (or is NaN), use bisection
				m_temp_last[9-cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);
			else
				m_temp_last[9-cpp_offset] = secant_guess;

		}	// End T9 iteration

		// Check that T9_loop converged
		if(T9_iter >= max_iter)
		{
			error_code = 31;
			return;
		}

		// State 3 can now be fully defined
		m_enth_last[3-cpp_offset] = m_enth_last[2-cpp_offset] + Q_dot_LT/m_dot_mc;		// Energy balalnce on cold stream of low-temp recuperator
		prop_error_code = CO2_PH(m_pres_last[3-cpp_offset], m_enth_last[3-cpp_offset], &co2_props);
		if(prop_error_code != 0)
		{
			error_code = prop_error_code;
			return;
		}
		m_temp_last[3-cpp_offset] = co2_props.temp;
		m_entr_last[3-cpp_offset] = co2_props.entr;
		m_dens_last[3-cpp_offset] = co2_props.dens;

		// Go through the mixing valve
		if(ms_des_par.m_recomp_frac >= 1.E-12)
		{
			m_enth_last[4-cpp_offset] = (1.0 - ms_des_par.m_recomp_frac)*m_enth_last[3-cpp_offset] + ms_des_par.m_recomp_frac*m_enth_last[10-cpp_offset];	// conservation of energy (both sides divided by m_dot_t)
			prop_error_code = CO2_PH(m_pres_last[4-cpp_offset], m_enth_last[4-cpp_offset], &co2_props);
			if(prop_error_code != 0)
			{
				error_code = prop_error_code;
				return;
			}
			m_temp_last[4 - cpp_offset] = co2_props.temp;
			m_entr_last[4 - cpp_offset] = co2_props.entr;
			m_dens_last[4 - cpp_offset] = co2_props.dens;
		}
		else		// no mixing valve, therefore state 4 is equal to state 3
		{
			m_temp_last[4-cpp_offset] = m_temp_last[3-cpp_offset];
			m_enth_last[4-cpp_offset] = m_enth_last[3-cpp_offset];
			m_entr_last[4-cpp_offset] = m_entr_last[3-cpp_offset];
			m_dens_last[4-cpp_offset] = m_dens_last[3-cpp_offset];
		}

		// Check for a second law violation at the outlet of the high-temp recuperator
		if(m_temp_last[4-cpp_offset] >= m_temp_last[8-cpp_offset])		// temp(8) is not valid and it must be increased
		{
			T8_lower_bound = m_temp_last[8-cpp_offset];
			m_temp_last[8-cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
			continue;
		}

		// Calculate the UA value of the high-temp recuperator
		if(ms_des_par.m_UA_HT < 1.E-12)			// no high-temp recuperator
			Q_dot_HT = 0.0;
		else
			Q_dot_HT = m_dot_t * (m_enth_last[7-cpp_offset] - m_enth_last[8-cpp_offset]);

		int HT_error_code = 0;
		min_DT_HT = std::numeric_limits<double>::quiet_NaN();

		calculate_hxr_UA_1(ms_des_par.m_N_sub_hxrs, Q_dot_HT, m_dot_t, m_dot_t, m_temp_last[4-cpp_offset], m_temp_last[7-cpp_offset], m_pres_last[4-cpp_offset],
			m_pres_last[5-cpp_offset], m_pres_last[7-cpp_offset], m_pres_last[8-cpp_offset], HT_error_code, UA_HT_calc, min_DT_HT);

		if(HT_error_code != 0)
		{
			if(HT_error_code == 11)			// second-law violation in hxr, therefore temp(8) is too low
			{
				T8_lower_bound = m_temp_last[8-cpp_offset];
				m_temp_last[8-cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);	// bisect bounds for next guess
				continue;			
			}
			else
			{
				error_code = HT_error_code;
				return;
			}		
		}

		// Check for convergence and adjust T8 appropriately
		double UA_HT_residual = ms_des_par.m_UA_HT - UA_HT_calc;

		if( fabs(UA_HT_residual) < 1.0E-12 )		// catches no HT case
			break;

		double secant_guess = m_temp_last[8-cpp_offset] - UA_HT_residual*(last_T8_guess - m_temp_last[8-cpp_offset])/(last_HT_residual - UA_HT_residual);		// Next guess predicted using secant method

		if(UA_HT_residual < 0.0)	// UA_HT_calc is too big, temp(8) needs to be higher
		{
			if( fabs(UA_HT_residual) / ms_des_par.m_UA_HT < ms_des_par.m_tol )
				break;
			T8_lower_bound = m_temp_last[8-cpp_offset];
		}
		else						// UA_HT_calc is too small, temp(8) needs to be lower
		{
			if( UA_HT_residual / ms_des_par.m_UA_HT < ms_des_par.m_tol )		// UA_HT converged
				break;
			if( min_DT_HT < temperature_tolerance )								// UA_calc is still too low, but there isn't anywhere to go so it's okay
				break;
			T8_upper_bound = m_temp_last[8-cpp_offset];
		}
		last_HT_residual = UA_HT_residual;				// reset last stored residual value
		last_T8_guess = m_temp_last[8-cpp_offset];		// reset last stored guess value

		// Check if the secant method overshoots and fall back to bisection if it does
		if(secant_guess <= T8_lower_bound || secant_guess >= T8_upper_bound)		// secant method overshot, use bisection
			m_temp_last[8-cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
		else
			m_temp_last[8-cpp_offset] = secant_guess;

	}	// End T8 iteration

	// Check that T8_loop converged
	if(T8_iter >= max_iter)
	{
		error_code = 35;
		return;
	}

	// State 5 can now be fully defined
	m_enth_last[5-cpp_offset] = m_enth_last[4-cpp_offset] + Q_dot_HT/m_dot_t;						// Energy balance on cold stream of high-temp recuperator
	prop_error_code = CO2_PH(m_pres_last[5-cpp_offset], m_enth_last[5-cpp_offset], &co2_props);
	if(prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	m_temp_last[5-cpp_offset] = co2_props.temp;
	m_entr_last[5-cpp_offset] = co2_props.entr;
	m_dens_last[5-cpp_offset] = co2_props.dens;

	// Calculate performance metrics for low-temperature recuperator
	C_HeatExchanger::S_design_parameters LT_des_par;
	double C_dot_hot = m_dot_t*(m_enth_last[8-cpp_offset]-m_enth_last[9-cpp_offset])/(m_temp_last[8-cpp_offset]-m_temp_last[9-cpp_offset]);		// LT recuperator hot stream capacitance rate
	double C_dot_cold = m_dot_mc*(m_enth_last[3-cpp_offset]-m_enth_last[2-cpp_offset])/(m_temp_last[3-cpp_offset]-m_temp_last[2-cpp_offset]);	// LT recuperator cold stream capacitance rate
	double C_dot_min = min(C_dot_hot, C_dot_cold);
	double Q_dot_max = C_dot_min*(m_temp_last[8-cpp_offset]-m_temp_last[2-cpp_offset]);
	double hx_eff = Q_dot_LT / Q_dot_max;				// Definition of effectiveness
	LT_des_par.m_DP_design[0] = m_pres_last[2-cpp_offset] - m_pres_last[3-cpp_offset];
	LT_des_par.m_DP_design[1] = m_pres_last[8-cpp_offset] - m_pres_last[9-cpp_offset];
	LT_des_par.m_eff_design = hx_eff;
	LT_des_par.m_min_DT_design = min_DT_LT;
	LT_des_par.m_m_dot_design[0] = m_dot_mc;
	LT_des_par.m_m_dot_design[1] = m_dot_t;
	LT_des_par.m_N_sub = ms_des_par.m_N_sub_hxrs;
	LT_des_par.m_Q_dot_design = Q_dot_LT;
	LT_des_par.m_UA_design = UA_LT_calc;
	m_LT.initialize(LT_des_par);

	// Calculate performance metrics for high-temperature recuperator
	C_HeatExchanger::S_design_parameters HT_des_par;
	C_dot_hot = m_dot_t*(m_enth_last[7-cpp_offset]-m_enth_last[8-cpp_offset])/(m_temp_last[7-cpp_offset]-m_temp_last[8-cpp_offset]);			// HT recuperator hot stream capacitance rate
	C_dot_cold = m_dot_t*(m_enth_last[5-cpp_offset]-m_enth_last[4-cpp_offset])/(m_temp_last[5-cpp_offset]-m_temp_last[4-cpp_offset]);			// HT recuperator cold stream capacitance rate
	C_dot_min = min(C_dot_hot, C_dot_cold);
	Q_dot_max = C_dot_min*(m_temp_last[7-cpp_offset]-m_temp_last[4-cpp_offset]);
	hx_eff = Q_dot_HT / Q_dot_max;						// Definition of effectiveness
	HT_des_par.m_DP_design[0] = m_pres_last[4-cpp_offset] - m_pres_last[5-cpp_offset];
	HT_des_par.m_DP_design[1] = m_pres_last[7-cpp_offset] - m_pres_last[8-cpp_offset];
	HT_des_par.m_eff_design = hx_eff;
	HT_des_par.m_min_DT_design = min_DT_HT;
	HT_des_par.m_m_dot_design[0] = m_dot_t;
	HT_des_par.m_m_dot_design[1] = m_dot_t;
	HT_des_par.m_N_sub = ms_des_par.m_N_sub_hxrs;
	HT_des_par.m_Q_dot_design = Q_dot_HT;
	HT_des_par.m_UA_design = UA_HT_calc;
	m_HT.initialize(HT_des_par);

	// Set relevant values for other heat exchangers
	C_HeatExchanger::S_design_parameters PHX_des_par;
	PHX_des_par.m_DP_design[0] = m_pres_last[5-cpp_offset] - m_pres_last[6-cpp_offset];
	PHX_des_par.m_DP_design[1] = 0.0;
	PHX_des_par.m_m_dot_design[0] = m_dot_t;
	PHX_des_par.m_m_dot_design[1] = 0.0;
	PHX_des_par.m_Q_dot_design = m_dot_t*(m_enth_last[6-cpp_offset] - m_enth_last[5-cpp_offset]);
	m_PHX.initialize(PHX_des_par);

	C_HeatExchanger::S_design_parameters PC_des_par;
	PC_des_par.m_DP_design[0] = 0.0;
	PC_des_par.m_DP_design[1] = m_pres_last[9-cpp_offset] - m_pres_last[1-cpp_offset];
	PC_des_par.m_m_dot_design[0] = 0.0;
	PC_des_par.m_m_dot_design[1] = m_dot_mc;
	PC_des_par.m_Q_dot_design = m_dot_mc*(m_enth_last[9-cpp_offset] - m_enth_last[1-cpp_offset]);
	m_PC.initialize(PC_des_par);

	// Calculate cycle performance metrics
	m_W_dot_net_last = w_mc*m_dot_mc + w_rc*m_dot_rc + w_t*m_dot_t;
	m_eta_thermal_last = m_W_dot_net_last / PHX_des_par.m_Q_dot_design;
}

void C_RecompCycle::design(S_design_parameters & des_par_in, int & error_code)
{
	ms_des_par = des_par_in;

	design_core(error_code);
}

void C_RecompCycle::opt_design(S_opt_design_parameters & opt_des_par_in, int & error_code)
{
	ms_opt_des_par = opt_des_par_in;

	opt_design_core(error_code);
}

void C_RecompCycle::opt_design_core(int & error_code)
{
	// Map ms_opt_des_par to ms_des_par
	ms_des_par.m_W_dot_net = ms_opt_des_par.m_W_dot_net;
	ms_des_par.m_T_mc_in = ms_opt_des_par.m_T_mc_in;
	ms_des_par.m_T_t_in = ms_opt_des_par.m_T_t_in;
	ms_des_par.m_DP_LT = ms_opt_des_par.m_DP_LT;
	ms_des_par.m_DP_HT = ms_opt_des_par.m_DP_HT;
	ms_des_par.m_DP_PC = ms_opt_des_par.m_DP_PC;
	ms_des_par.m_DP_PHX = ms_opt_des_par.m_DP_PHX;
	ms_des_par.m_eta_mc = ms_opt_des_par.m_eta_mc;
	ms_des_par.m_eta_rc = ms_opt_des_par.m_eta_rc;
	ms_des_par.m_eta_t = ms_opt_des_par.m_eta_t;
	ms_des_par.m_N_sub_hxrs = ms_opt_des_par.m_N_sub_hxrs;
	ms_des_par.m_P_high_limit = ms_opt_des_par.m_P_high_limit;
	ms_des_par.m_tol = ms_opt_des_par.m_tol;

	// ms_des_par members to be defined by optimizer and set in 'design_point_eta':
		// m_P_mc_in
		// m_P_mc_out
		// m_recomp_frac
		// m_UA_LT
		// m_UA_HT

	int index = 0;

	std::vector<double> x(0);
	std::vector<double> lb(0);
	std::vector<double> ub(0);
	std::vector<double> scale(0);

	if( !ms_opt_des_par.m_fixed_P_mc_out )
	{
		x.push_back(ms_opt_des_par.m_P_mc_out_guess);
		lb.push_back(100.0);
		ub.push_back(ms_opt_des_par.m_P_high_limit);
		scale.push_back(500.0);

		index++;
	}

	if( !ms_opt_des_par.m_fixed_PR_mc )
	{
		x.push_back(ms_opt_des_par.m_PR_mc_guess);
		lb.push_back(0.0001);
		double PR_max = ms_opt_des_par.m_P_high_limit / 100.0;
		ub.push_back(PR_max);
		scale.push_back(0.2);

		index++;
	}

	if( !ms_opt_des_par.m_fixed_recomp_frac )
	{
		x.push_back(ms_opt_des_par.m_recomp_frac_guess);
		lb.push_back(0.0);
		ub.push_back(1.0);
		scale.push_back(0.05);

		index++;
	}

	if( !ms_opt_des_par.m_fixed_LT_frac )
	{
		x.push_back(ms_opt_des_par.m_LT_frac_guess);
		lb.push_back(0.0);
		ub.push_back(1.0);
		scale.push_back(0.05);

		index++;
	}

	int no_opt_error_code = 0;
	if( index > 0 )
	{
		// Ensure thermal efficiency is initialized to 0
		m_eta_thermal_opt = 0.0;

		// Set up instance of nlopt class and set optimization parameters
		nlopt::opt		opt_des_cycle(nlopt::LN_SBPLX, index);
		opt_des_cycle.set_lower_bounds(lb);
		opt_des_cycle.set_upper_bounds(ub);
		opt_des_cycle.set_initial_step(scale);
		opt_des_cycle.set_xtol_rel(ms_opt_des_par.m_opt_tol);

		// Set max objective function
		opt_des_cycle.set_max_objective(nlopt_callback_opt_des_1, this);		// Calls wrapper/callback that calls 'design_point_eta', which optimizes design point eta through repeated calls to 'design'
		double max_f = std::numeric_limits<double>::quiet_NaN();
		nlopt::result   result_des_cycle = opt_des_cycle.optimize(x, max_f);
		
		ms_des_par = ms_des_par_optimal;

		design_core(no_opt_error_code);

		/*
		m_W_dot_net_last = m_W_dot_net_opt;
		m_eta_thermal_last = m_eta_thermal_opt;
		m_temp_last = m_temp_opt;
		m_pres_last = m_pres_opt;
		m_enth_last = m_enth_opt;
		m_entr_last = m_entr_opt;
		m_dens_last = m_dens_opt;
		*/
	}
	else
	{
		// Finish defining ms_des_par based on current 'x' values
		ms_des_par.m_P_mc_out = ms_opt_des_par.m_P_mc_out_guess;
		ms_des_par.m_P_mc_in = ms_des_par.m_P_mc_out / ms_opt_des_par.m_PR_mc_guess;
		ms_des_par.m_recomp_frac = ms_opt_des_par.m_recomp_frac_guess;
		ms_des_par.m_UA_LT = ms_opt_des_par.m_UA_rec_total*ms_opt_des_par.m_LT_frac_guess;
		ms_des_par.m_UA_HT = ms_opt_des_par.m_UA_rec_total*(1.0 - ms_opt_des_par.m_LT_frac_guess);
		
		design_core(no_opt_error_code);
	}

}

double C_RecompCycle::design_point_eta(const std::vector<double> &x)
{
	// 'x' is array of inputs either being adjusted by optimizer or set constant
	// Finish defining ms_des_par based on current 'x' values

	int index = 0;

	// Main compressor outlet pressure
	if( !ms_opt_des_par.m_fixed_P_mc_out )
	{
		ms_des_par.m_P_mc_out = x[index];
		if( ms_des_par.m_P_mc_out > ms_opt_des_par.m_P_high_limit )
			return 0.0;
		index++;
	}
	else
		ms_des_par.m_P_mc_out = ms_opt_des_par.m_P_mc_out_guess;

	// Main compressor pressure ratio
	double PR_mc_local = -999.9;
	if( !ms_opt_des_par.m_fixed_PR_mc )
	{
		PR_mc_local = x[index];
		if( PR_mc_local > 50.0 )
			return 0.0;
		index++;
	}
	else
		PR_mc_local = ms_opt_des_par.m_PR_mc_guess;
	
	double P_mc_in = ms_des_par.m_P_mc_out / PR_mc_local;
	if( P_mc_in >= ms_des_par.m_P_mc_out )
		return 0.0;
	if( P_mc_in <= 100.0 )
		return 0.0;
	ms_des_par.m_P_mc_in = P_mc_in;

	// Recompression fraction
	if( !ms_opt_des_par.m_fixed_recomp_frac )
	{
		ms_des_par.m_recomp_frac = x[index];
		if( ms_des_par.m_recomp_frac < 0.0 )
			return 0.0;
		index++;
	}
	else
		ms_des_par.m_recomp_frac = ms_opt_des_par.m_recomp_frac_guess;

	// Recuperator split fraction
	double LT_frac_local = -999.9;
	if( !ms_opt_des_par.m_fixed_LT_frac )
	{
		LT_frac_local = x[index];
		if( LT_frac_local > 1.0 || LT_frac_local < 0.0 )
			return 0.0;
		index++;
	}
	else
		LT_frac_local = ms_opt_des_par.m_LT_frac_guess;
	
	ms_des_par.m_UA_LT = ms_opt_des_par.m_UA_rec_total*LT_frac_local;
	ms_des_par.m_UA_HT = ms_opt_des_par.m_UA_rec_total*(1.0 - LT_frac_local);

	int error_code = 0;

	design_core(error_code);

	double eta_thermal = 0.0;
	if( error_code == 0 )
	{
		eta_thermal = m_eta_thermal_last;

		if( m_eta_thermal_last > m_eta_thermal_opt )
		{
			ms_des_par_optimal = ms_des_par;
			m_W_dot_net_opt = m_W_dot_net_last;
			m_eta_thermal_opt = m_eta_thermal_last;
			m_temp_opt = m_temp_last;
			m_pres_opt = m_pres_last;
			m_enth_opt = m_enth_last;
			m_entr_opt = m_entr_last;
			m_dens_opt = m_dens_last;
		}
	}

	return eta_thermal;
}

void C_RecompCycle::auto_opt_design(S_auto_opt_design_parameters & auto_opt_des_par_in, int & error_code)
{
	ms_auto_opt_des_par = auto_opt_des_par_in;

	// map 'auto_opt_des_par_in' to 'ms_auto_opt_des_par'
	ms_opt_des_par.m_W_dot_net = ms_auto_opt_des_par.m_W_dot_net;
	ms_opt_des_par.m_T_mc_in = ms_auto_opt_des_par.m_T_mc_in;
	ms_opt_des_par.m_T_t_in = ms_auto_opt_des_par.m_T_t_in;
	ms_opt_des_par.m_DP_LT = ms_auto_opt_des_par.m_DP_LT;
	ms_opt_des_par.m_DP_HT = ms_auto_opt_des_par.m_DP_HT;
	ms_opt_des_par.m_DP_PC = ms_auto_opt_des_par.m_DP_PC;
	ms_opt_des_par.m_DP_PHX = ms_auto_opt_des_par.m_DP_PHX;
	ms_opt_des_par.m_UA_rec_total = ms_auto_opt_des_par.m_UA_rec_total;
	ms_opt_des_par.m_eta_mc = ms_auto_opt_des_par.m_eta_mc;
	ms_opt_des_par.m_eta_rc = ms_auto_opt_des_par.m_eta_rc;
	ms_opt_des_par.m_eta_t = ms_auto_opt_des_par.m_eta_t;
	ms_opt_des_par.m_N_sub_hxrs = ms_auto_opt_des_par.m_N_sub_hxrs;
	ms_opt_des_par.m_P_high_limit = ms_auto_opt_des_par.m_P_high_limit;
	ms_opt_des_par.m_tol = ms_auto_opt_des_par.m_tol;
	ms_opt_des_par.m_opt_tol = ms_auto_opt_des_par.m_opt_tol;

	// Outer optimization loop
	m_eta_thermal_auto_opt = 0.0;

	double best_P_high = fminbr(
		ms_auto_opt_des_par.m_P_high_limit*0.2, ms_auto_opt_des_par.m_P_high_limit, &fmin_callback_opt_eta_1, this, 1.0);

	// Check model with P_mc_out set at P_high_limit for a recompression and simple cycle and use the better configuration
	double PR_mc_guess = ms_des_par_auto_opt.m_P_mc_out / ms_des_par_auto_opt.m_P_mc_in;

	// Complete 'ms_opt_des_par' for recompression cycle
	ms_opt_des_par.m_P_mc_out_guess = ms_auto_opt_des_par.m_P_high_limit;
	ms_opt_des_par.m_fixed_P_mc_out = true;
	ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;
	ms_opt_des_par.m_fixed_PR_mc = false;
	ms_opt_des_par.m_recomp_frac_guess = 0.3;
	ms_opt_des_par.m_fixed_recomp_frac = false;
	ms_opt_des_par.m_LT_frac_guess = 0.5;
	ms_opt_des_par.m_fixed_LT_frac = false;

	int rc_error_code = 0;

	opt_design_core(rc_error_code);

	if( rc_error_code == 0 && m_eta_thermal_opt > m_eta_thermal_auto_opt )
	{
		ms_des_par_auto_opt = ms_des_par_optimal;
		m_eta_thermal_auto_opt = m_eta_thermal_opt;
		m_W_dot_net_auto_opt = m_W_dot_net_opt;
	}

	// Complete 'ms_opt_des_par' for simple cycle
	ms_opt_des_par.m_P_mc_out_guess = ms_auto_opt_des_par.m_P_high_limit;
	ms_opt_des_par.m_fixed_P_mc_out = true;
	ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;
	ms_opt_des_par.m_fixed_PR_mc = false;
	ms_opt_des_par.m_recomp_frac_guess = 0.0;
	ms_opt_des_par.m_fixed_recomp_frac = true;
	ms_opt_des_par.m_LT_frac_guess = 0.5;
	ms_opt_des_par.m_fixed_LT_frac = true;

	int s_error_code = 0;

	opt_design_core(s_error_code);

	if( s_error_code == 0 && m_eta_thermal_opt > m_eta_thermal_auto_opt )
	{
		ms_des_par_auto_opt = ms_des_par_optimal;
		m_eta_thermal_auto_opt = m_eta_thermal_opt;
		m_W_dot_net_auto_opt = m_W_dot_net_opt;
	}

	ms_des_par = ms_des_par_optimal = ms_des_par_auto_opt;
	m_W_dot_net_last = m_W_dot_net_opt = m_W_dot_net_auto_opt;
	m_eta_thermal_last = m_eta_thermal_opt = m_eta_thermal_auto_opt;
}

double C_RecompCycle::opt_eta(double P_high_opt)
{
	double PR_mc_guess = 1.1;
	if(P_high_opt > P_pseudocritical_1(ms_opt_des_par.m_T_mc_in))
		PR_mc_guess = P_high_opt / P_pseudocritical_1(ms_opt_des_par.m_T_mc_in);

	// Complete 'ms_opt_des_par' for recompression cycle
	ms_opt_des_par.m_P_mc_out_guess = P_high_opt;
	ms_opt_des_par.m_fixed_P_mc_out = true;
	ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;
	ms_opt_des_par.m_fixed_PR_mc = false;
	ms_opt_des_par.m_recomp_frac_guess = 0.3;
	ms_opt_des_par.m_fixed_recomp_frac = false;
	ms_opt_des_par.m_LT_frac_guess = 0.5;
	ms_opt_des_par.m_fixed_LT_frac = false;

	int rc_error_code = 0;
	opt_design_core(rc_error_code);

	int local_eta_rc = 0.0;
	if( rc_error_code == 0 )
		local_eta_rc = m_eta_thermal_opt;
	
	if(rc_error_code == 0 && m_eta_thermal_opt > m_eta_thermal_auto_opt)
	{
		ms_des_par_auto_opt = ms_des_par_optimal;
		m_eta_thermal_auto_opt = m_eta_thermal_opt;
		m_W_dot_net_auto_opt = m_W_dot_net_opt;
	}

	// Complete 'ms_opt_des_par' for simple cycle
	ms_opt_des_par.m_P_mc_out_guess = P_high_opt;
	ms_opt_des_par.m_fixed_P_mc_out = true;
	ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;
	ms_opt_des_par.m_fixed_PR_mc = false;
	ms_opt_des_par.m_recomp_frac_guess = 0.0;
	ms_opt_des_par.m_fixed_recomp_frac = true;
	ms_opt_des_par.m_LT_frac_guess = 0.5;
	ms_opt_des_par.m_fixed_LT_frac = true;

	int s_error_code = 0;
	opt_design_core(s_error_code);

	int local_eta_s = 0.0;
	if( s_error_code == 0 )
		local_eta_s = m_eta_thermal_opt;

	if(s_error_code == 0 && m_eta_thermal_opt > m_eta_thermal_auto_opt)
	{
		ms_des_par_auto_opt = ms_des_par_optimal;
		m_eta_thermal_auto_opt = m_eta_thermal_opt;
		m_W_dot_net_auto_opt = m_W_dot_net_opt;
	}

	return -max(local_eta_rc, local_eta_s);

}

double fmin_callback_opt_eta_1(double x, void *data)
{
	C_RecompCycle *frame = static_cast<C_RecompCycle*>(data);

	return frame->opt_eta(x);
}

double nlopt_callback_opt_des_1(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_RecompCycle *frame = static_cast<C_RecompCycle*>(data);
	if( frame != NULL ) return frame->design_point_eta(x);
}

double P_pseudocritical_1(double T_K)
{
	return (0.191448*T_K + 45.6661)*T_K - 24213.3;
}