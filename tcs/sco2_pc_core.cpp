#include "sco2_pc_core.h"
#include "CO2_properties.h"
#include <limits>
#include <algorithm>

using namespace std;


const double C_turbine::m_nu_design = 0.7476;
const double C_compressor::m_snl_phi_design = 0.02971;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
const double C_compressor::m_snl_phi_min = 0.02;				//[-] Approximate surge limit for SNL compressor
const double C_compressor::m_snl_phi_max = 0.05;				//[-] Approximate x-intercept for SNL compressor
const double C_recompressor::m_snl_phi_design = 0.02971;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
const double C_recompressor::m_snl_phi_min = 0.02;				//[-] Approximate surge limit for SNL compressor
const double C_recompressor::m_snl_phi_max = 0.05;				//[-] Approximate x-intercept for SNL compressor

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