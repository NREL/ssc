/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/
#include "stdafx.h"//IMPORTANT TO INCLUDE IN ALL *.CPP FILES.
#include "sco2_pc_core_RCMCI_with_reheating.h"
#include "CO2_properties.h"
#include <limits>
#include <algorithm>

#include "nlopt.hpp"
#include "nlopt_callbacks.h"

#include "fmin.h"

#include "lib_util.h"

#include "csp_solver_util.h"

using namespace std;


const double C_turbine_RCMCI_with_ReHeating::m_nu_design = 0.7476;
//const double C_compressor_RCMCI_with_ReHeating::m_snl_phi_design = 0.02971;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
//const double C_compressor_RCMCI_with_ReHeating::m_snl_phi_min = 0.02;				//[-] Approximate surge limit for SNL compressor
//const double C_compressor_RCMCI_with_ReHeating::m_snl_phi_max = 0.05;				//[-] Approximate x-intercept for SNL compressor
//const double C_recompressor_RCMCI_with_ReHeating::m_snl_phi_design = 0.02971;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
//const double C_recompressor_RCMCI_with_ReHeating::m_snl_phi_min = 0.02;				//[-] Approximate surge limit for SNL compressor
//const double C_recompressor_RCMCI_with_ReHeating::m_snl_phi_max = 0.05;				//[-] Approximate x-intercept for SNL compressor
const double C_comp_single_stage_RCMCI_with_ReHeating::m_snl_phi_design = 0.02971;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
const double C_comp_single_stage_RCMCI_with_ReHeating::m_snl_phi_min = 0.02;				//[-] Approximate surge limit for SNL compressor
const double C_comp_single_stage_RCMCI_with_ReHeating::m_snl_phi_max = 0.05;				//[-] Approximate x-intercept for SNL compressor


void calculate_turbomachinery_outlet_1_RCMCI_with_ReHeating(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double eta /*-*/, bool is_comp, int & error_code, double & spec_work /*kJ/kg*/)
{
	double enth_in, entr_in, dens_in, temp_out, enth_out, entr_out, dens_out;

	calculate_turbomachinery_outlet_1_RCMCI_with_ReHeating(T_in, P_in, P_out, eta, is_comp, error_code, enth_in, entr_in, dens_in, temp_out, enth_out, entr_out, dens_out, spec_work);
}

void calculate_turbomachinery_outlet_1_RCMCI_with_ReHeating(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double eta /*-*/, bool is_comp, int & error_code, double & enth_in /*kJ/kg*/, double & entr_in /*kJ/kg-K*/,
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

//void calculate_hxr_UA_1_RCMCI_with_ReHeating(int N_hxrs, double Q_dot /*units?*/, double m_dot_c, double m_dot_h, double T_c_in, double T_h_in, double P_c_in, double P_c_out, double P_h_in, double P_h_out,
//	int & error_code, double & UA, double & min_DT)
//{
//	/*Calculates the UA of a heat exchanger given its mass flow rates, inlet temperatures, and a heat transfer rate.
//	Note: the heat transfer rate must be positive.*/
//
//	// Check inputs
//	if( Q_dot < 0.0 )
//	{
//		error_code = 4;
//		return;
//	}
//	if( T_h_in < T_c_in )
//	{
//		error_code = 5;
//		return;
//	}
//	if( P_h_in < P_h_out )
//	{
//		error_code = 6;
//		return;
//	}
//	if( P_c_in < P_c_out )
//	{
//		error_code = 7;
//		return;
//	}
//	if( Q_dot <= 1.E-14 )		// very low Q_dot; assume it is zero
//	{
//		UA = 0.0;
//		min_DT = T_h_in - T_c_in;
//		return;
//	}
//
//	// Calculate inlet enthalpies from known state points
//	CO2_state co2_props;
//	int prop_error_code = CO2_TP(T_c_in, P_c_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	double h_c_in = co2_props.enth;
//
//	prop_error_code = CO2_TP(T_h_in, P_h_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = 9;
//		return;
//	}
//	double h_h_in = co2_props.enth;
//
//	// Calculate outlet enthalpies from energy balance
//	double h_c_out = h_c_in + Q_dot / m_dot_c;
//	double h_h_out = h_h_in - Q_dot / m_dot_h;
//
//	int N_nodes = N_hxrs + 1;
//	double h_h_prev = 0.0;
//	double T_h_prev = 0.0;
//	double h_c_prev = 0.0;
//	double T_c_prev = 0.0;
//	UA = 0.0;
//	min_DT = T_h_in;
//	// Loop through the sub-heat exchangers
//	for( int i = 0; i < N_nodes; i++ )
//	{
//		// Assume pressure varies linearly through heat exchanger
//		double P_c = P_c_out + i*(P_c_in - P_c_out) / (N_nodes - 1);
//		double P_h = P_h_in - i*(P_h_in - P_h_out) / (N_nodes - 1);
//
//		// Calculate the entahlpy at the node
//		double h_c = h_c_out + i*(h_c_in - h_c_out) / (N_nodes - 1);
//		double h_h = h_h_in - i*(h_h_in - h_h_out) / (N_nodes - 1);
//
//		// Calculate the hot and cold temperatures at the node
//		prop_error_code = CO2_PH(P_h, h_h, &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = 12;
//			return;
//		}
//		double T_h = co2_props.temp;
//
//		prop_error_code = CO2_PH(P_c, h_c, &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = 13;
//			return;
//		}
//		double T_c = co2_props.temp;
//
//		// Check that 2nd law was not violated
//		if( T_c >= T_h )
//		{
//			error_code = 11;
//			return;
//		}
//
//		// Track the minimum temperature difference in the heat exchanger
//		min_DT = fmin(min_DT, T_h - T_c);
//
//		// Perform effectiveness-NTU and UA calculations 
//		if( i > 0 )
//		{
//			double C_dot_h = m_dot_h*(h_h_prev - h_h) / (T_h_prev - T_h);			// [kW/K] hot stream capacitance rate
//			double C_dot_c = m_dot_c*(h_c_prev - h_c) / (T_c_prev - T_c);			// [kW/K] cold stream capacitance rate
//			double C_dot_min = min(C_dot_h, C_dot_c);				// [kW/K] Minimum capacitance stream
//			double C_dot_max = max(C_dot_h, C_dot_c);				// [kW/K] Maximum capacitance stream
//			double C_R = C_dot_min / C_dot_max;						// [-] Capacitance ratio of sub-heat exchanger
//			double eff = (Q_dot / (double)N_hxrs) / (C_dot_min*(T_h_prev - T_c));	// [-] Effectiveness of each sub-heat exchanger
//			double NTU = 0.0;
//			if( C_R != 1.0 )
//				NTU = log((1.0 - eff*C_R) / (1.0 - eff)) / (1.0 - C_R);		// [-] NTU if C_R does not equal 1
//			else
//				NTU = eff / (1.0 - eff);
//			UA += NTU*C_dot_min;						// [kW/K] Sum UAs for each hx section			
//		}
//		h_h_prev = h_h;
//		T_h_prev = T_h;
//		h_c_prev = h_c;
//		T_c_prev = T_c;
//	}
//
//	// Check for NaNs that arose
//	if( UA != UA )
//	{
//		error_code = 14;
//		return;
//	}
//
//	return;
//};

void isen_eta_from_poly_eta_RCMCI_with_ReHeating(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double poly_eta /*-*/, bool is_comp, int & error_code, double & isen_eta)
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

void C_HeatExchanger_RCMCI_with_ReHeating::initialize(const S_design_parameters & des_par_in)
{
	ms_des_par = des_par_in;
	return;
}

void C_HeatExchanger_RCMCI_with_ReHeating::hxr_pressure_drops(const std::vector<double> & m_dots, std::vector<double> & hxr_deltaP)
{
	int N = (int)m_dots.size();
	hxr_deltaP.resize(N);
	for( int i = 0; i < N; i++ )
		hxr_deltaP[i] = ms_des_par.m_DP_design[i] * pow((m_dots[i] / ms_des_par.m_m_dot_design[i]), 1.75);
}

void C_HeatExchanger_RCMCI_with_ReHeating::hxr_conductance(const std::vector<double> & m_dots, double & hxr_UA)
{
	int N = (int)m_dots.size();
	double m_dot_ratio = 0.5*(m_dots[0] / ms_des_par.m_m_dot_design[0] + m_dots[1] / ms_des_par.m_m_dot_design[1]);
	hxr_UA = ms_des_par.m_UA_design*pow(m_dot_ratio, 0.8);
}

void C_turbine_RCMCI_with_ReHeating::turbine_sizing(const S_design_parameters & des_par_in, int & error_code)
{
	/* 9.4.14: code from John Dyreby, converted to C++ by Ty Neises
	! Determine the turbine rotor diameter, effective nozzle area, and design-point shaft
	! speed and store values in recomp_cycle%t.
	!
	! Arguments:
	!   recomp_cycle -- a RecompCycle object that defines the simple/recompression cycle at the design point
	!   error_trace -- an ErrorTrace object
	!
	! Notes:
	!   1) The value for recomp_cycle%t%N_design is required to be set.  If it is <= 0.0 then
	!      the value for recomp_cycle%mc%N_design is used (i.e., link the compressor and turbine
	!      shafts).  For this reason, turbine_sizing must be called after compressor_sizing if
	!      the shafts are to be linked. */

	CO2_state co2_props;

	ms_des_par = des_par_in;

	// Check that a design-point shaft speed is available
	if( ms_des_par.m_N_design <= 0.0 )	// Link shafts
	{
		ms_des_solved.m_N_design = ms_des_par.m_N_comp_design_if_linked;
		if( ms_des_par.m_N_design <= 0.0 )
		{
			error_code = 7;
			return;
		}
	}
	else
		ms_des_solved.m_N_design = ms_des_par.m_N_design;

	// Get speed of sound at inlet
	int prop_error_code = CO2_TD(ms_des_par.m_T_in, ms_des_par.m_D_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double ssnd_in = co2_props.ssnd;

	// Outlet specific enthalpy after isentropic expansion
	prop_error_code = CO2_PS(ms_des_par.m_P_out, ms_des_par.m_s_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double h_s_out = co2_props.enth;

	// Determine necessary turbine parameters
	ms_des_solved.m_nu_design = m_nu_design;
	double w_i = ms_des_par.m_h_in - h_s_out;			//[kJ/kg] Isentropic specific work of turbine
	double C_s = sqrt(2.0*w_i*1000.0);					//[m/s] Spouting velocity
	double U_tip = ms_des_solved.m_nu_design*C_s;		//[m/s] Tip speed
	ms_des_solved.m_D_rotor = U_tip / (0.5*ms_des_solved.m_N_design*0.104719755);	//[m]
	ms_des_solved.m_A_nozzle = ms_des_par.m_m_dot / (C_s*ms_des_par.m_D_in);		//[m^2]

	// Set other turbine variables
	ms_des_solved.m_w_tip_ratio = U_tip / ssnd_in;				//[-]
	ms_des_solved.m_eta = (ms_des_par.m_h_in - ms_des_par.m_h_out) / w_i;	//[-] Isentropic efficiency
}

void C_turbine_RCMCI_with_ReHeating::off_design_turbine(double T_in, double P_in, double P_out, double N, int & error_code, double & m_dot, double & T_out)
{
	/* 9.4.14: code from John Dyreby, converted to C++ by Ty Neises
	! Solve for the outlet state of 'turb' given its inlet conditions, outlet pressure, and shaft speed.
	!
	! Inputs:
	!   turb -- a Turbine object, with design-point values and sizing set
	!   T_in -- turbine inlet temperature (K)
	!   P_in -- turbine inlet pressure (kPa)
	!   P_out -- turbine outlet pressure (kPa)
	!   N -- shaft speed of turbine (rpm)
	!
	! Outputs:
	!   error_trace -- an ErrorTrace object
	!   m_dot -- allowable mass flow rate through the turbine (kg/s)
	!   T_out -- turbine outlet temperature (K)
	!
	! Notes:
	!   1) This subroutine also sets the following values in 'turb': nu, eta, m_dot, w, w_tip_ratio */

	CO2_state co2_props;

	// Get properties at turbine inlet
	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double D_in = co2_props.dens;
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;
	double ssnd_in = co2_props.ssnd;

	prop_error_code = CO2_PS(P_out, s_in, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	double h_s_out = co2_props.enth;

	// Apply the radial turbine equations for efficiency
	double C_s = sqrt(2.0*(h_in - h_s_out)*1000.0);				//[m/s] spouting velocity
	double U_tip = ms_des_solved.m_D_rotor*0.5*N*0.104719755;	//[m/s] tip speed
	ms_od_solved.m_nu = U_tip / C_s;							//[-] ratio of tip speed to spouting velocity

	double eta_0 = (((1.0626*ms_od_solved.m_nu - 3.0874)*ms_od_solved.m_nu + 1.3668)*ms_od_solved.m_nu + 1.3567)*ms_od_solved.m_nu + 0.179921180;
	eta_0 = max(eta_0, 0.0);
	eta_0 = min(eta_0, 1.0);
	ms_od_solved.m_eta = eta_0*ms_des_solved.m_eta;		//[-] Actual turbine efficiency

	// Calculate the outlet state and allowable mass flow rate
	double h_out = h_in - ms_od_solved.m_eta*(h_in - h_s_out);		//[kJ/kg] Enthalpy at turbine outlet
	prop_error_code = CO2_PH(P_out, h_out, &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	T_out = co2_props.temp;

	m_dot = C_s*ms_des_solved.m_A_nozzle*D_in;			//[kg/s] Mass flow rate through turbine
	ms_od_solved.m_w_tip_ratio = U_tip / ssnd_in;		//[-] Ratio of the tip speed to the local (turbine inlet) speed of sound
	ms_od_solved.m_N = N;
	ms_od_solved.m_W_dot_out = m_dot*(h_in-h_out);		//[kW] Turbine power output
}

void C_turbine_RCMCI_with_ReHeating::od_turbine_at_N_des(double T_in, double P_in, double P_out, int & error_code, double & m_dot, double & T_out)
{
	double N = ms_des_solved.m_N_design;		//[rpm]

	off_design_turbine(T_in, P_in, P_out, N, error_code, m_dot, T_out);

	return;
}

int C_comp_single_stage_RCMCI_with_ReHeating::design_given_shaft_speed(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
				double N_rpm /*rpm*/, double eta_isen /*-*/, double & P_out /*kPa*/, double & T_out /*K*/, double & tip_ratio /*-*/)
{
	CO2_state co2_props;

	// Get inlet state
	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
	if (prop_error_code != 0)
	{
		return prop_error_code;
	}
	double h_in = co2_props.enth;	//[kJ/kg]
	double s_in = co2_props.entr;	//[kJ/kg-K]
	double rho_in = co2_props.dens;	//[kg/m^3]

	// Convert shaft speed to rad/s
	double N_rad_s = N_rpm / 9.549296590;		//[rad/s]

	// Solve for the diameter that gives the design flow coefficient
	double D_rotor = std::pow(m_dot / (m_snl_phi_design * rho_in * 0.5 * N_rad_s), 1.0/3.0);		//[m]

	// Calculate psi at the design-point phi using Horner's method
	double psi_design = ((((-498626.0*m_snl_phi_design) + 53224.0) * m_snl_phi_design - 2505.0) * m_snl_phi_design + 54.6) *
		m_snl_phi_design + 0.04049;		// from dimensionless modified head curve(at design - point, psi and modified psi are equal)

	// Solve for idea head
	double U_tip = 0.5 * D_rotor * N_rad_s;		//[m/s]
	double w_i = psi_design * std::pow(U_tip, 2) * 0.001;		//[kJ/kg]

	// Solve for isentropic outlet enthalpy
	double h_out_isen = h_in + w_i;		//[kJ/kg]

	// Get isentropic outlet state
	prop_error_code = CO2_HS(h_out_isen, s_in, &co2_props);
	if (prop_error_code != 0)
	{
		return prop_error_code;
	}
	P_out = co2_props.pres;		//[kPa]

	// Get actual outlet state
	double h_out = h_in + w_i / eta_isen;	//[kJ/kg]
	prop_error_code = CO2_PH(P_out, h_out, &co2_props);
	if (prop_error_code != 0)
	{
		return prop_error_code;
	}
	T_out = co2_props.temp;		//[K]
	double ssnd_out = co2_props.ssnd;	//[m/s]

	// Solve for tip ratio
	tip_ratio = U_tip / ssnd_out;

	ms_des_solved.m_T_in = T_in;	//[K]
	ms_des_solved.m_P_in = P_in;	//[kPa]
	ms_des_solved.m_D_in = rho_in;	//[kg/m^3]
	ms_des_solved.m_h_in = h_in;	//[kJ/kg]
	ms_des_solved.m_s_in = s_in;	//[kJ/kg-K]

	ms_des_solved.m_T_out = T_out;	//[K]
	ms_des_solved.m_P_out = P_out;	//[kPa]
	ms_des_solved.m_h_out = h_out;	//[kJ/kg]
	ms_des_solved.m_D_out = co2_props.dens;	//[kg/m^3]

	ms_des_solved.m_m_dot = m_dot;	//[kg/s]

	ms_des_solved.m_D_rotor = D_rotor;	//[m]
	ms_des_solved.m_N_design = N_rpm;	//[rpm]
	ms_des_solved.m_tip_ratio = tip_ratio;	//[-]
	ms_des_solved.m_eta_design = eta_isen;		//[-]

	ms_des_solved.m_phi_des = m_snl_phi_design;
	ms_des_solved.m_phi_surge = m_snl_phi_min;
	ms_des_solved.m_phi_max = m_snl_phi_max;

	return 0;
}

int C_comp_multi_stage_RCMCI_with_ReHeating::C_MEQ_eta_isen__h_out::operator()(double eta_isen /*-*/, double *h_comp_out /*kJ/kg*/)
{
	C_MEQ_N_rpm__P_out c_stages(mpc_multi_stage, m_T_in, m_P_in, m_m_dot, eta_isen);
	C_monotonic_eq_solver c_solver(c_stages);

	// Set lowr bound
	double N_rpm_lower = 0.0;
	double N_rpm_upper = std::numeric_limits<double>::quiet_NaN();

	// Generate guess values
	double N_rpm_guess_1 = 3000.0;
	double N_rpm_guess_2 = 30000.0;

	c_solver.settings(1.E-4, 50, N_rpm_lower, N_rpm_upper, true);

	// Now solve for the shaft speed
	double N_rpm_solved = std::numeric_limits<double>::quiet_NaN();
	double tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int N_rpm_code = 0;
	try
	{
		N_rpm_code = c_solver.solve(N_rpm_guess_1, N_rpm_guess_2, m_P_out, N_rpm_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception)
	{
		throw(C_csp_exception("C_comp_multi_stage::C_MEQ_eta_isen__h_out threw an exception"));
	}

	if (N_rpm_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (!(N_rpm_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.01))
		{
			throw(C_csp_exception("C_comp_multi_stage::C_MEQ_eta_isen__h_out failed to converge within a reasonable tolerance"));
		}
	}

	int n_stages = mpc_multi_stage->mv_stages.size();

	*h_comp_out = mpc_multi_stage->mv_stages[n_stages - 1].ms_des_solved.m_h_out;	//[kJ/kg]

	return 0;
}

int C_comp_multi_stage_RCMCI_with_ReHeating::C_MEQ_N_rpm__P_out::operator()(double N_rpm /*rpm*/, double *P_comp_out /*kPa*/)
{
	int n_stages = mpc_multi_stage->mv_stages.size();

	double T_in = m_T_in;	//[K]
	double P_in = m_P_in;	//[kPa]

	double P_out = std::numeric_limits<double>::quiet_NaN();
	double T_out = std::numeric_limits<double>::quiet_NaN();
	double tip_ratio = std::numeric_limits<double>::quiet_NaN();

	for (int i = 0; i < n_stages; i++)
	{
		if (i > 0)
		{
			T_in = T_out;	//[K]
			P_in = P_out;	//[kPa]
		}

		mpc_multi_stage->mv_stages[i].design_given_shaft_speed(T_in, P_in, m_m_dot, N_rpm, m_eta_isen, P_out, T_out, tip_ratio);
	}

	*P_comp_out = P_out;	//[kPa]

	return 0;
}

int C_comp_multi_stage_RCMCI_with_ReHeating::design_given_outlet_state(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
	double T_out /*K*/, double P_out /*K*/)
{
	mv_stages.resize(1);
	mv_stages[0].design_single_stage_comp(T_in, P_in, m_dot, T_out, P_out);

	double max_calc_tip_speed = mv_stages[0].ms_des_solved.m_tip_ratio;

	if (mv_stages[0].ms_des_solved.m_tip_ratio > 0.9)
	{
		CO2_state co2_props;

		double h_in = mv_stages[0].ms_des_solved.m_h_in;
		double s_in = mv_stages[0].ms_des_solved.m_s_in;

		int prop_err_code = CO2_PS(P_out, s_in, &co2_props);
		if (prop_err_code != 0)
		{
			return -1;
		}
		double h_out_isen = co2_props.enth;

		double h_out = mv_stages[0].ms_des_solved.m_h_out;

		double eta_isen_total = (h_out_isen - h_in) / (h_out - h_in);
		
		bool is_add_stages = true;
		int n_stages = 1;		

		while (is_add_stages)
		{
			n_stages++;
			
			mv_stages.resize(n_stages);

			C_MEQ_eta_isen__h_out c_stages(this, T_in, P_in, P_out, m_dot);
			C_monotonic_eq_solver c_solver(c_stages);

			// Set bounds on isentropic efficiency
			double eta_isen_lower = 0.1;
			double eta_isen_upper = 1.0;

			// Generate guess values
			double eta_isen_guess_1 = eta_isen_total;
			double eta_isen_guess_2 = 0.95*eta_isen_total;

			c_solver.settings(1.E-4, 50, eta_isen_lower, eta_isen_upper, true);

			// Now solve for the isentropic efficiency for each stage that results in the total compressor design isentropic efficiency
			double eta_isen_solved = std::numeric_limits<double>::quiet_NaN();
			double tol_solved = std::numeric_limits<double>::quiet_NaN();
			int iter_solved = -1;

			int eta_isen_code = 0;

			try
			{
				eta_isen_code = c_solver.solve(eta_isen_guess_1, eta_isen_guess_2, h_out, eta_isen_solved, tol_solved, iter_solved);
			}
			catch (C_csp_exception)
			{
				throw(C_csp_exception("C_comp_multi_stage::design_given_outlet_state threw an exception"));
			}

			if (eta_isen_code != C_monotonic_eq_solver::CONVERGED)
			{
				if (!(eta_isen_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.01))
				{
					throw(C_csp_exception("C_comp_multi_stage::design_given_outlet_state failed to converge within a reasonable tolerance"));
				}
			}
			
			max_calc_tip_speed = 0.0;
			for (int i = 0; i < n_stages; i++)
			{
				max_calc_tip_speed = std::max(max_calc_tip_speed, mv_stages[i].ms_des_solved.m_tip_ratio);
			}

			if (max_calc_tip_speed < 0.9)
			{
				is_add_stages = false;
			}

			if (n_stages > 20)
			{
				return -1;
			}
		}
	}

	int n_stages = mv_stages.size();

	ms_des_solved.m_T_in = T_in;	//[K]
	ms_des_solved.m_P_in = P_in;	//[kPa]
	ms_des_solved.m_D_in = mv_stages[0].ms_des_solved.m_D_in;	//[kg/m^3]
	ms_des_solved.m_s_in = mv_stages[0].ms_des_solved.m_s_in;	//[kJ/kg-K]
	ms_des_solved.m_h_in = mv_stages[0].ms_des_solved.m_h_in;	//[kJ/kg]

	ms_des_solved.m_T_out = mv_stages[n_stages-1].ms_des_solved.m_T_out;	//[K]
	ms_des_solved.m_P_out = mv_stages[n_stages-1].ms_des_solved.m_P_out;	//[kPa]
	ms_des_solved.m_h_out = mv_stages[n_stages-1].ms_des_solved.m_h_out;	//[kJ/kg]
	ms_des_solved.m_D_out = mv_stages[n_stages-1].ms_des_solved.m_D_out;	//[kg/m^3]

	ms_des_solved.m_m_dot = m_dot;					//[kg/s]

	ms_des_solved.m_N_design = mv_stages[n_stages-1].ms_des_solved.m_N_design;		//[rpm]
	ms_des_solved.m_phi_des = mv_stages[0].ms_des_solved.m_phi_des;		//[-]
	ms_des_solved.m_w_tip_ratio = max_calc_tip_speed;					//[-]
	ms_des_solved.m_n_stages = n_stages;								//[-]
	ms_des_solved.m_D_rotor = mv_stages[0].ms_des_solved.m_D_rotor;		//[m]
	ms_des_solved.m_phi_surge = mv_stages[0].m_snl_phi_min;				//[-]

	return 0;
}

int C_comp_single_stage_RCMCI_with_ReHeating::design_single_stage_comp(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
	double T_out /*K*/, double P_out /*K*/)
{
	CO2_state in_props;
	int prop_err_code = CO2_TP(T_in, P_in, &in_props);
	if (prop_err_code != 0)
	{
		return -1;
	}
	double s_in = in_props.entr;	//[kJ/kg-K]
	double h_in = in_props.enth;	//[kJ/kg]
	double rho_in = in_props.dens;	//[kg/m^3]

	CO2_state isen_out_props;
	prop_err_code = CO2_PS(P_out, s_in, &isen_out_props);
	if (prop_err_code != 0)
	{
		return -1;
	}
	double h_isen_out = isen_out_props.enth;	//[kJ/kg]

	CO2_state out_props;
	prop_err_code = CO2_TP(T_out, P_out, &out_props);
	if (prop_err_code != 0)
	{
		return -1;
	}
	double h_out = out_props.enth;

	// Calculate psi at the design-point phi using Horner's method
	double psi_design = ((((-498626.0*m_snl_phi_design) + 53224.0) * m_snl_phi_design - 2505.0) * m_snl_phi_design + 54.6) *
		m_snl_phi_design + 0.04049;		// from dimensionless modified head curve(at design - point, psi and modified psi are equal)

	// Determine required size and speed of compressor
	double w_i = h_isen_out - h_in;						//[kJ/kg] positive isentropic specific work of compressor
	double U_tip = sqrt(1000.0*w_i / psi_design);		//[m/s]
	double D_rotor = sqrt(m_dot / (m_snl_phi_design*rho_in*U_tip));
	double N_rad_s = U_tip*2.0 / D_rotor;				//[rad/s] shaft speed

	double ssnd_out = out_props.ssnd;	//[m/s]

	// Solve for tip ratio
	double tip_ratio = U_tip / ssnd_out;

	ms_des_solved.m_T_in = T_in;	//[K]
	ms_des_solved.m_P_in = P_in;	//[kPa]
	ms_des_solved.m_D_in = rho_in;	//[kg/m^3]
	ms_des_solved.m_h_in = h_in;	//[kJ/kg]
	ms_des_solved.m_s_in = s_in;	//[kJ/kg-K]

	ms_des_solved.m_T_out = T_out;	//[K]
	ms_des_solved.m_P_out = P_out;	//[kPa]
	ms_des_solved.m_h_out = h_out;	//[kJ/kg]
	ms_des_solved.m_D_out = out_props.dens;	//[kg/m^3]

	ms_des_solved.m_m_dot = m_dot;	//[kg/s]

	ms_des_solved.m_D_rotor = D_rotor;	//[m]
	ms_des_solved.m_N_design = N_rad_s * 9.549296590;			//[rpm] shaft speed
	ms_des_solved.m_tip_ratio = tip_ratio;	//[-]
	ms_des_solved.m_eta_design = (h_isen_out - h_in) / (h_out - h_in);		//[-]

	ms_des_solved.m_phi_des = m_snl_phi_design;
	ms_des_solved.m_phi_surge = m_snl_phi_min;
	ms_des_solved.m_phi_max = m_snl_phi_max;

	return 0;
}

//void C_compressor_RCMCI_with_ReHeating::compressor_sizing(const S_design_parameters & des_par_in, int & error_code)
//{
//	ms_des_par = des_par_in;
//
//	CO2_state co2_props;
//
//	int prop_error_code = CO2_TD(ms_des_par.m_T_out, ms_des_par.m_D_out, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	double ssnd_out = co2_props.ssnd;
//
//	prop_error_code = CO2_PS(ms_des_par.m_P_out, ms_des_par.m_s_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	double h_s_out = co2_props.enth;
//
//	// Calculate psi at the design-point phi using Horner's method
//	double psi_design = ((((-498626.0*m_snl_phi_design) + 53224.0) * m_snl_phi_design - 2505.0) * m_snl_phi_design + 54.6) *
//		m_snl_phi_design + 0.04049;		// from dimensionless modified head curve(at design - point, psi and modified psi are equal)
//
//	// Determine required size and speed of compressor
//	double w_i = h_s_out - ms_des_par.m_h_in;		//[kJ/kg] positive isentropic specific work of compressor
//	double U_tip = sqrt(1000.0*w_i / psi_design);		//[m/s]
//	ms_des_solved.m_D_rotor = sqrt(ms_des_par.m_m_dot / (m_snl_phi_design*ms_des_par.m_D_in*U_tip));
//	double N_rad_s = U_tip*2.0 / ms_des_solved.m_D_rotor;		//[rad/s] shaft speed
//	ms_des_solved.m_N_design = N_rad_s * 9.549296590;			//[rpm] shaft speed
//
//	// Set other compressor member data
//	ms_des_solved.m_w_tip_ratio = U_tip / ssnd_out;
//	ms_des_solved.m_eta_design = w_i / (ms_des_par.m_h_out - ms_des_par.m_h_in);
//	ms_des_solved.m_phi_des = m_snl_phi_design;
//	ms_des_solved.m_phi_surge = m_snl_phi_min;
//	ms_des_solved.m_phi_max = m_snl_phi_max;
//
//	ms_des_solved.m_n_stages = 1;
//}
//
//void C_compressor_RCMCI_with_ReHeating::od_comp_phi_opt(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
//	int &error_code, double &T_out /*K*/, double &P_out /*kPa*/)
//{
//	double phi = m_snl_phi_design;
//
//	od_comp_phi(phi, T_in, P_in, m_dot, error_code, T_out, P_out);
//
//	return;
//}
//
//void C_compressor_RCMCI_with_ReHeating::od_comp_phi(double phi_in /*-*/, double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
//	int &error_code, double &T_out /*K*/, double &P_out /*kPa*/)
//{
//	CO2_state co2_props;
//
//	// Get properties at the inlet state of the compressor
//	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = 1;
//		return;
//	}
//	double rho_in = co2_props.dens;
//	double h_in = co2_props.enth;
//	double s_in = co2_props.entr;
//
//	// Want design-point value of phi
//	double U_tip = m_dot / (rho_in*pow(ms_des_solved.m_D_rotor, 2)*phi_in);	//[m/s]
//	double N_mc = U_tip / (ms_des_solved.m_D_rotor*0.5*0.104719755);		//[rpm]
//
//	// Can now call performance call that requires shaft speed
//	off_design_compressor(T_in, P_in, m_dot, N_mc, error_code, T_out, P_out);
//
//	return;
//}
//
//void C_compressor_RCMCI_with_ReHeating::od_comp_at_N_des(double T_in, double P_in, double m_dot, int & error_code, double & T_out, double & P_out)
//{
//	double N = ms_des_solved.m_N_design;	//[rpm]
//
//	off_design_compressor(T_in, P_in, m_dot, N, error_code, T_out, P_out);
//}

void C_comp_multi_stage_RCMCI_with_ReHeating::off_design_at_N_des(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
	int & error_code, double & T_out /*K*/, double & P_out /*kPa*/)
{
	double N = ms_des_solved.m_N_design;	//[rpm]

	off_design_given_N(T_in, P_in, m_dot, N, error_code, T_out, P_out);
}

void C_comp_multi_stage_RCMCI_with_ReHeating::off_design_given_N(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/, double N_rpm /*rpm*/,
	int & error_code, double & T_out /*K*/, double & P_out /*kPa*/)
{
	int n_stages = mv_stages.size();

	double T_stage_in = T_in;	//[K]
	double P_stage_in = P_in;	//[kPa]

	double T_stage_out = std::numeric_limits<double>::quiet_NaN();
	double P_stage_out = std::numeric_limits<double>::quiet_NaN();

	double tip_ratio_max = 0.0;
	bool is_surge = false;
	double surge_safety_min = 10.0;
	double phi_min = 10.0;

	for (int i = 0; i < n_stages; i++)
	{
		if (i > 0)
		{
			T_stage_in = T_stage_out;
			P_stage_in = P_stage_out;
		}

		error_code = mv_stages[i].off_design_given_N(T_stage_in, P_stage_in, m_dot, N_rpm, T_stage_out, P_stage_out);
		if (error_code != 0)
		{
			return;
		}

		if (mv_stages[i].ms_od_solved.m_w_tip_ratio > tip_ratio_max)
		{
			tip_ratio_max = mv_stages[i].ms_od_solved.m_w_tip_ratio;
		}

		if (mv_stages[i].ms_od_solved.m_surge)
		{
			is_surge = true;
		}

		if (mv_stages[i].ms_od_solved.m_surge_safety < surge_safety_min)
		{
			surge_safety_min = mv_stages[i].ms_od_solved.m_surge;
		}

		phi_min = std::min(phi_min, mv_stages[i].ms_od_solved.m_phi);
	}

	P_out = mv_stages[n_stages-1].ms_od_solved.m_P_out;		//[kPa]
	T_out = mv_stages[n_stages - 1].ms_od_solved.m_T_out;	//[K]

	double h_in = mv_stages[0].ms_od_solved.m_h_in;					//[kJ/kg]
	double s_in = mv_stages[0].ms_od_solved.m_s_in;					//[kJ/kg-K]

	CO2_state co2_props;
	int prop_err_code = CO2_PS(P_out, s_in, &co2_props);
	if (prop_err_code != 0)
	{
		error_code = prop_err_code;
		return;
	}
	double h_out_isen = co2_props.enth;

	double h_out = mv_stages[n_stages - 1].ms_od_solved.m_h_out;	//[kJ/kg]

	ms_od_solved.m_P_in = P_in;
	ms_od_solved.m_T_in = T_in;

	ms_od_solved.m_P_out = P_out;
	ms_od_solved.m_T_out = T_out;

	ms_od_solved.m_surge = is_surge;
	ms_od_solved.m_eta = (h_out_isen - h_in) / (h_out - h_in);
	ms_od_solved.m_phi = mv_stages[0].ms_od_solved.m_phi;
	ms_od_solved.m_w_tip_ratio = tip_ratio_max;

	ms_od_solved.m_N = N_rpm;

	ms_od_solved.m_W_dot_in = m_dot*(h_out - h_in);
	ms_od_solved.m_surge_safety = surge_safety_min;

}

int C_comp_single_stage_RCMCI_with_ReHeating::off_design_given_N(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/, double N_rpm /*rpm*/,
	double & T_out /*K*/, double & P_out /*kPa*/)
{
	CO2_state co2_props;

	ms_od_solved.m_N = N_rpm;		//[rpm]

	// Fully define the inlet state of the compressor
	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
	if (prop_error_code != 0)
	{
		return prop_error_code;
	}
	double rho_in = co2_props.dens;	//[kg/m^3]
	double h_in = co2_props.enth;	//[kJ/kg]
	double s_in = co2_props.entr;	//[kJ/kg-K]

	// Calculate the modified flow and head coefficients and efficiency
	double U_tip = ms_des_solved.m_D_rotor*0.5*ms_od_solved.m_N*0.104719755;				//[m/s]
	double phi = m_dot / (rho_in*U_tip*pow(ms_des_solved.m_D_rotor, 2));	//[-]
	if (phi < m_snl_phi_min)
	{
		ms_od_solved.m_surge = true;
		phi = m_snl_phi_min;
	}
	else
		ms_od_solved.m_surge = false;

	double phi_star = phi*pow(ms_od_solved.m_N / ms_des_solved.m_N_design, 0.2);		//[-] modified flow coefficient
	double psi_star = ((((-498626.0*phi_star) + 53224.0)*phi_star - 2505.0)*phi_star + 54.6)*phi_star + 0.04049;	// from dimensionless modified head curve
	double eta_star = ((((-1.638e6*phi_star) + 182725.0)*phi_star - 8089.0)*phi_star + 168.6)*phi_star - 0.7069;	// from dimensionless modified efficiency curve
	double psi = psi_star / pow(ms_des_solved.m_N_design / ms_od_solved.m_N, pow(20.0*phi_star, 3.0));
	double eta_0 = eta_star*1.47528 / pow(ms_des_solved.m_N_design / ms_od_solved.m_N, pow(20.0*phi_star, 5.0));		// Efficiency is normalized so it equals 1.0 at snl_phi_design
	ms_od_solved.m_eta = max(eta_0*ms_des_solved.m_eta_design, 0.0);		//[-] Actual compressor efficiency, not allowed to go negative

	// Check that the specified mass flow rate is possible with the compressor's current shaft speed
	if (psi <= 0.0)
	{
		return 1;
	}

	// Calculate the compressor outlet state
	double dh_s = psi * pow(U_tip, 2.0) * 0.001;			//[kJ/kg] Ideal enthalpy rise in compressor, from definition of head coefficient
	double dh = dh_s / ms_od_solved.m_eta;					//[kJ/kg] Actual enthalpy rise in compressor
	double h_s_out = h_in + dh_s;							//[kJ/kg] Ideal enthalpy at compressor outlet
	double h_out = h_in + dh;								//[kJ/kg] Actual enthalpy at compressor outlet

	// Get the compressor outlet pressure
	prop_error_code = CO2_HS(h_s_out, s_in, &co2_props);
	if (prop_error_code != 0)
	{
		return 2;
	}
	P_out = co2_props.pres;

	// Determine compressor outlet temperature and speed of sound
	prop_error_code = CO2_PH(P_out, h_out, &co2_props);
	if (prop_error_code != 0)
	{
		return 2;
	}
	T_out = co2_props.temp;
	double ssnd_out = co2_props.ssnd;

	// Set a few compressor variables
	ms_od_solved.m_P_in = P_in;		//[kPa]
	ms_od_solved.m_h_in = h_in;		//[kJ/kg]
	ms_od_solved.m_T_in = T_in;		//[K]
	ms_od_solved.m_s_in = s_in;		//[kJ/kg-K]

	ms_od_solved.m_P_out = P_out;	//[kPa]
	ms_od_solved.m_h_out = h_out;	//[kJ/kg]
	ms_od_solved.m_T_out = T_out;	//[K]
	ms_od_solved.m_s_out = co2_props.entr;	//[kJ/kg-K]

	ms_od_solved.m_phi = phi;
	ms_od_solved.m_surge_safety = phi / m_snl_phi_min;	//[-] If > 1, then not in surge
	ms_od_solved.m_w_tip_ratio = U_tip / ssnd_out;
	ms_od_solved.m_W_dot_in = m_dot*(h_out - h_in);	//[kWe]

	return 0;
}

int C_comp_single_stage_RCMCI_with_ReHeating::calc_N_from_phi(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/, double phi_in /*-*/, double & N_rpm /*rpm*/)
{
	CO2_state co2_props;

	// Fully define the inlet state of the compressor
	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
	if (prop_error_code != 0)
	{
		return prop_error_code;
	}
	double rho_in = co2_props.dens;	//[kg/m^3]
	double h_in = co2_props.enth;	//[kJ/kg]

	double U_tip = m_dot / (phi_in*rho_in*std::pow(ms_des_solved.m_D_rotor,2));		//[m/s]
	N_rpm = (U_tip*2.0 / ms_des_solved.m_D_rotor)*9.549296590;		//[rpm]

	return 0;
}

//void C_compressor_RCMCI_with_ReHeating::off_design_compressor(double T_in, double P_in, double m_dot, double N_in, int & error_code, double & T_out, double & P_out)
//{
//	CO2_state co2_props;
//
//	ms_od_solved.m_N = N_in;		//[rpm]
//
//	// Fully define the inlet state of the compressor
//	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = 1;
//		return;
//	}
//	double rho_in = co2_props.dens;
//	double h_in = co2_props.enth;
//	double s_in = co2_props.entr;
//
//	// Calculate the modified flow and head coefficients and efficiency for the SNL compressor
//	double U_tip = ms_des_solved.m_D_rotor*0.5*ms_od_solved.m_N*0.104719755;				//[m/s]
//	double phi = m_dot / (rho_in*U_tip*pow(ms_des_solved.m_D_rotor, 2));	//[-]
//	if( phi < m_snl_phi_min )
//	{
//		ms_od_solved.m_surge = true;
//		phi = m_snl_phi_min;
//	}
//	else
//		ms_od_solved.m_surge = false;
//
//	double phi_star = phi*pow(ms_od_solved.m_N / ms_des_solved.m_N_design, 0.2);		//[-] modified flow coefficient
//	double psi_star = ((((-498626.0*phi_star) + 53224.0)*phi_star - 2505.0)*phi_star + 54.6)*phi_star + 0.04049;	// from dimensionless modified head curve
//	double eta_star = ((((-1.638e6*phi_star) + 182725.0)*phi_star - 8089.0)*phi_star + 168.6)*phi_star - 0.7069;	// from dimensionless modified efficiency curve
//	double psi = psi_star / pow(ms_des_solved.m_N_design / ms_od_solved.m_N, pow(20.0*phi_star, 3.0));
//	double eta_0 = eta_star*1.47528 / pow(ms_des_solved.m_N_design / ms_od_solved.m_N, pow(20.0*phi_star, 5.0));		// Efficiency is normalized so it equals 1.0 at snl_phi_design
//	ms_od_solved.m_eta = max(eta_0*ms_des_solved.m_eta_design, 0.0);		//[-] Actual compressor efficiency, not allowed to go negative
//
//	// Check that the specified mass flow rate is possible with the compressor's current shaft speed
//	if( psi <= 0.0 )
//	{
//		error_code = 1;
//		return;
//	}
//
//	// Calculate the compressor outlet state
//	double dh_s = psi * pow(U_tip, 2.0) * 0.001;			//[kJ/kg] Ideal enthalpy rise in compressor, from definition of head coefficient
//	double dh = dh_s / ms_od_solved.m_eta;					//[kJ/kg] Actual enthalpy rise in compressor
//	double h_s_out = h_in + dh_s;							//[kJ/kg] Ideal enthalpy at compressor outlet
//	double h_out = h_in + dh;								//[kJ/kg] Actual enthalpy at compressor outlet
//
//	// Get the compressor outlet pressure
//	prop_error_code = CO2_HS(h_s_out, s_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = 2;
//		return;
//	}
//	P_out = co2_props.pres;
//
//	// Determine compressor outlet temperature and speed of sound
//	prop_error_code = CO2_PH(P_out, h_out, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = 2;
//		return;
//	}
//	T_out = co2_props.temp;
//	double ssnd_out = co2_props.ssnd;
//
//	// Set a few compressor variables
//	ms_od_solved.m_phi = phi;
//	ms_od_solved.m_surge_safety = phi / m_snl_phi_min;	//[-] If > 1, then not in surge
//	ms_od_solved.m_w_tip_ratio = U_tip / ssnd_out;
//	ms_od_solved.m_W_dot_in = m_dot*(h_out - h_in);	//[kWe]
//	ms_od_solved.m_P_in = P_in;		//[kPa]
//	ms_od_solved.m_P_out = P_out;	//[kPa]
//}

//void C_recompressor_RCMCI_with_ReHeating::recompressor_sizing(const S_design_parameters & des_par_in, int & error_code)
//{
//	ms_des_par = des_par_in;
//
//	CO2_state co2_props;
//
//	// Ideal specific enthalpy after compression
//	int prop_error_code = CO2_PS(ms_des_par.m_P_out, ms_des_par.m_s_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	double h_s_out = co2_props.enth;
//
//	double eta_design = (h_s_out - ms_des_par.m_h_in) / (ms_des_par.m_h_out - ms_des_par.m_h_in);		//[-] overall isentropic efficiency
//	double psi_design = ((((-498626.0*m_snl_phi_design) + 53224.0)*m_snl_phi_design - 2505.0)*m_snl_phi_design + 54.6)*m_snl_phi_design + 0.04049;
//
//	// Prepare intermediate pressure iteration loop
//	double last_residual = 0.0;
//	double last_P_int = 1.E12;			// Ensures bisection will be used for first loop
//	double lower_bound = ms_des_par.m_P_in + 1.E-6;
//	double upper_bound = ms_des_par.m_P_out - 1.E-6;
//	double P_int = 0.5*(lower_bound + upper_bound);
//	double eta_stage = eta_design;	// First guess for stage efficiency
//	int max_iter = 100;
//	double tolerance = 1.0E-8;
//
//	double D_rotor_1 = -999.9;
//	double D_rotor_2 = -999.9;
//	double N_design = -999.9;
//
//	double ssnd_int = std::numeric_limits<double>::quiet_NaN();
//	double U_tip_1 = std::numeric_limits<double>::quiet_NaN();
//	double U_tip_2 = std::numeric_limits<double>::quiet_NaN();
//
//	int i = -1;
//	for( i = 0; i < max_iter; i++ )
//	{
//		// First stage
//		// Ideal outlet specific enthalpy after first stage
//		prop_error_code = CO2_PS(P_int, ms_des_par.m_s_in, &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		double h_s_out = co2_props.enth;
//
//		double w_i = h_s_out - ms_des_par.m_h_in;				//[kJ/kg] positive isentropic specific work of first stage
//		U_tip_1 = sqrt(1000.0 * w_i / psi_design);		//[m/s]
//		D_rotor_1 = sqrt(ms_des_par.m_m_dot / (m_snl_phi_design*ms_des_par.m_D_in*U_tip_1));	//[m]
//		double N_rad_s = U_tip_1*2.0 / D_rotor_1;				//[rad/s]
//		N_design = N_rad_s*9.549296590;					//[rpm]
//		double w = w_i / eta_stage;								//[kJ/kg]
//		double h_int = ms_des_par.m_h_in + w;					//[kJ/kg] Energy balance on first stage
//
//		prop_error_code = CO2_PH(P_int, h_int, &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		double D_int = co2_props.dens;
//		double s_int = co2_props.entr;
//		ssnd_int = co2_props.ssnd;
//
//		// Second stage
//		prop_error_code = CO2_PS(ms_des_par.m_P_out, s_int, &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		h_s_out = co2_props.enth;
//
//		w_i = h_s_out - h_int;			// positive isentropic specific work of second stage
//		U_tip_2 = sqrt(1000.0*w_i / psi_design);
//		D_rotor_2 = 2.0*U_tip_2 / (N_design*0.104719755);
//		double phi = ms_des_par.m_m_dot / (D_int*U_tip_2*pow(D_rotor_2, 2));	//[-] Flow coefficient
//		double eta_2_req = w_i / (ms_des_par.m_h_out - h_int);				//[-] Required second stage efficiency to achieve overall eta_design
//
//		// Check convergence and update guesses
//		double residual = m_snl_phi_design - phi;
//		if( residual < 0.0 )		// P_int guess is too high
//		{
//			if( -residual <= tolerance && fabs(eta_stage - eta_2_req) <= tolerance )
//				break;
//			upper_bound = P_int;
//		}
//		else					// P_int guess is too low
//		{
//			if( -residual <= tolerance && fabs(eta_stage - eta_2_req) <= tolerance )
//				break;
//			lower_bound = P_int;
//		}
//
//		double secant_step = -residual*(last_P_int - P_int) / (last_residual - residual);
//		double P_secant = P_int + secant_step;
//		last_P_int = P_int;
//		last_residual = residual;
//		if( P_secant <= lower_bound || P_secant >= upper_bound )		// secant method overshot
//			P_int = 0.5*(lower_bound + upper_bound);
//		else if( fabs(secant_step) > fabs(0.5*(upper_bound - lower_bound)) )	// take the smaller step to ensure convergence
//			P_int = 0.5*(lower_bound + upper_bound);
//		else
//			P_int = P_secant;		// Use secant guess
//
//		eta_stage = 0.5*(eta_stage + eta_2_req);		// Update guess for stage efficiency
//	}
//
//	// Check for convergence
//	if( i == max_iter )		// did  not converge
//	{
//		error_code = 1;
//		return;
//	}
//
//	// Set recompressor variables
//	ms_des_solved.m_D_rotor = D_rotor_1;
//	ms_des_solved.m_D_rotor_2 = D_rotor_2;
//	ms_des_solved.m_eta_design = eta_stage;
//	ms_des_solved.m_N_design = N_design;
//	
//	// Calculate tip ratios for each stage
//		// 1st stage (ssnd calculated in iteration loop above)
//	ms_des_solved.m_w_tip_ratio_1 = U_tip_1 / ssnd_int;
//
//		// 2nd stage
//	prop_error_code = CO2_TD(ms_des_par.m_T_out, ms_des_par.m_D_out, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	double ssnd_out_2 = co2_props.ssnd;
//	ms_des_solved.m_w_tip_ratio_2 = U_tip_2 / ssnd_out_2;
//	
//
//	ms_des_solved.m_phi_des = m_snl_phi_design;
//	ms_des_solved.m_phi_surge = m_snl_phi_min;
//	ms_des_solved.m_phi_max = m_snl_phi_max;
//	ms_des_solved.m_n_stages = 2;
//
//}

//int C_recompressor_RCMCI_with_ReHeating::C_mono_eq_phi_off_design::operator()(double phi_1 /*-*/, double *P_high /*kPa*/)
//{
//	// Variables that we need to set as member data so the solver can call this method
//	double m_dot = mpc_recompressor->ms_od_inputs.m_m_dot;	//[kg/s]
//	double rho_in = mpc_recompressor->ms_od_inputs.m_rho_in;	//[kg/m^3]
//	double h_in = mpc_recompressor->ms_od_inputs.m_h_in;		//[kJ/kg]
//	double s_in = mpc_recompressor->ms_od_inputs.m_s_in;		//[kJ/kg-K]
//	
//	// First stage - dh_s and eta_stage_1
//	double U_tip_1 = m_dot / (phi_1*rho_in*pow(mpc_recompressor->ms_des_solved.m_D_rotor, 2));		//[m/s]
//	double N_od = (U_tip_1*2.0 / mpc_recompressor->ms_des_solved.m_D_rotor)*9.549296590;			//[rpm] shaft spped
//	double phi_star = phi_1*pow((N_od / mpc_recompressor->ms_des_solved.m_N_design), 0.2);		//[-] Modified flow coefficient
//	double psi_star = ((((-498626.0*phi_star) + 53224.0)*phi_star - 2505.0)*phi_star + 54.6)*phi_star + 0.04049;		//[-] from dimensionless modified head curve
//	double psi = psi_star / (pow(mpc_recompressor->ms_des_solved.m_N_design / N_od, pow(20.0*phi_star, 3.0)));
//	double dh_s = psi*pow(U_tip_1, 2)*0.001;										//[kJ/kg] Calculated ideal enthalpy rise in first stage of compressor
//	double eta_star = ((((-1.638e6*phi_star) + 182725.0)*phi_star - 8089.0)*phi_star + 168.6)*phi_star - 0.7069;		//[-] from dimensionless modified efficiency curve
//	double eta_0 = eta_star*1.47528 / pow((mpc_recompressor->ms_des_solved.m_N_design / N_od), pow(20.0*phi_star, 5));		//[-] Stage efficiency is normalized so it equals 1.0 at snl_phi_design
//	double eta_stage_1 = max(eta_0*mpc_recompressor->ms_des_solved.m_eta_design, 0.0);			//[-] The actual stage efficiency, not allowed to go negative
//
//	// Calculate first - stage outlet (second - stage inlet) state
//	double dh = dh_s / eta_stage_1;		//[kJ/kg] Actual enthalpy rise in first stage
//	double h_s_out = h_in + dh_s;		//[kJ/kg] Ideal enthalpy between stages
//	double h_int = h_in + dh;			//[kJ/kg] Actual enthalpy between stages
//
//	CO2_state co2_props;
//	int prop_error_code = CO2_HS(h_s_out, s_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		*P_high = std::numeric_limits<double>::quiet_NaN();
//		return prop_error_code;
//	}
//	double P_int = co2_props.pres;
//
//	prop_error_code = CO2_PH(P_int, h_int, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		*P_high = std::numeric_limits<double>::quiet_NaN();
//		return prop_error_code;
//	}
//	double D_int = co2_props.dens;
//	double s_int = co2_props.entr;
//	double ssnd_int = co2_props.ssnd;
//
//	// Second stage - dh_s and eta_stage_2
//	double U_tip_2 = mpc_recompressor->ms_des_solved.m_D_rotor_2*0.5*N_od*0.104719755;				//[m/s] second-stage tip speed in m/s
//	double phi_2 = m_dot / (D_int*U_tip_2*pow(mpc_recompressor->ms_des_solved.m_D_rotor_2, 2));	//[-] second-stage flow coefficient
//	phi_star = phi_2*pow((N_od / mpc_recompressor->ms_des_solved.m_N_design), 0.2);				//[-] modified flow coefficient
//	psi_star = ((((-498626.0*phi_star) + 53224.0)*phi_star - 2505.0)*phi_star + 54.6)*phi_star + 0.04049;	// from dimensionless modified head curve
//	psi = psi_star / (pow(mpc_recompressor->ms_des_solved.m_N_design / N_od, pow(20.0*phi_star, 3.0)));
//	dh_s = psi*pow(U_tip_2, 2)*0.001;									// [kJ/kg] Calculated ideal enthalpy rise in second stage of compressor, from definition of head coefficient
//	eta_star = ((((-1.638e6*phi_star) + 182725.0)*phi_star - 8089.0)*phi_star + 168.6)*phi_star - 0.7069;		//[-] from dimensionless modified efficiency curve
//	eta_0 = eta_star*1.47528 / pow((mpc_recompressor->ms_des_solved.m_N_design / N_od), pow(20.0*phi_star, 5));	//[-] Stage efficiency is normalized so it equals 1.0 at snl_phi_design
//	double eta_stage_2 = max(eta_0*mpc_recompressor->ms_des_solved.m_eta_design, 0.0);			//[-] the actual stage efficiency, not allowed to go negative
//
//	// Calculate second-stage outlet state
//	dh = dh_s / eta_stage_2;			// actual enthalpy rise in second stage
//	h_s_out = h_int + dh_s;				// ideal enthalpy at compressor outlet
//	double h_out = h_int + dh;			// actual enthalpy at compressor outlet
//
//	// Get the calculated compressor outlet pressure
//	prop_error_code = CO2_HS(h_s_out, s_int, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		*P_high = std::numeric_limits<double>::quiet_NaN();
//		return prop_error_code;
//	}
//	*P_high = co2_props.pres;
//	
//	// Determine outlet temperature and speed of sound
//	prop_error_code = CO2_PH(*P_high, h_out, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		*P_high = std::numeric_limits<double>::quiet_NaN();
//		return prop_error_code;
//	}
//	mpc_recompressor->ms_od_solved.m_T_out = co2_props.temp;		//[K]
//	double ssnd_out = co2_props.ssnd;
//
//	// Set relevant recompressor variables
//	mpc_recompressor->ms_od_solved.m_N = N_od;		//[rpm] shaft speed
//	mpc_recompressor->ms_od_solved.m_eta = (h_s_out - h_in) / (h_out - h_in);		//[-] use overall isentropic efficiency
//	mpc_recompressor->ms_od_solved.m_phi = phi_1;
//	mpc_recompressor->ms_od_solved.m_phi_2 = phi_2;
//	mpc_recompressor->ms_od_solved.m_w_tip_ratio = max(U_tip_1 / ssnd_int, U_tip_2 / ssnd_out);	//[-] maximum tip speed ratio
//	mpc_recompressor->ms_od_solved.m_surge = phi_1 < m_snl_phi_min || phi_2 < m_snl_phi_min;
//	mpc_recompressor->ms_od_solved.m_W_dot_in = m_dot*(h_out - h_in);		//[kWe]
//	mpc_recompressor->ms_od_solved.m_surge_safety = min(phi_1, phi_2) / m_snl_phi_min;
//
//	return 0;
//}

int C_comp_multi_stage_RCMCI_with_ReHeating::C_MEQ_phi_od__P_out::operator()(double phi_od /*-*/, double *P_comp_out /*kPa*/)
{
	int error_code = 0;
	double N_rpm = std::numeric_limits<double>::quiet_NaN();
	error_code = mpc_multi_stage->mv_stages[0].calc_N_from_phi(m_T_in, m_P_in, m_m_dot, phi_od, N_rpm);
	if (error_code != 0)
	{
		*P_comp_out = std::numeric_limits<double>::quiet_NaN();
		return error_code;
	}

	double T_out = std::numeric_limits<double>::quiet_NaN();
	error_code = 0;
	mpc_multi_stage->off_design_given_N(m_T_in, m_P_in, m_m_dot, N_rpm, error_code, T_out, *P_comp_out);

	if (error_code != 0)
	{
		*P_comp_out = std::numeric_limits<double>::quiet_NaN();
		return error_code;
	}

	return 0;
}

void C_comp_multi_stage_RCMCI_with_ReHeating::off_design_given_P_out(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
	double P_out /*kPa*/, int & error_code, double & T_out /*K*/)
{
	// Apply 1 var solver to find the phi that results in a converged recompressor
	C_MEQ_phi_od__P_out c_rc_od(this, T_in, P_in, m_dot);
	C_monotonic_eq_solver c_rd_od_solver(c_rc_od);

	// Set upper and lower bounds
	double phi_upper = mv_stages[0].ms_des_solved.m_phi_max;
	double phi_lower = mv_stages[0].ms_des_solved.m_phi_surge;

	// Generate first x-y pair
	double phi_guess_lower = ms_des_solved.m_phi_des;
	double P_solved_phi_guess_lower = std::numeric_limits<double>::quiet_NaN();
	int test_code = c_rd_od_solver.test_member_function(phi_guess_lower, &P_solved_phi_guess_lower);
	if (test_code != 0)
	{
		for (int i = 1; i < 9; i++)
		{
			phi_guess_lower = ms_des_solved.m_phi_des*(10 - i) / 10.0 + mv_stages[0].ms_des_solved.m_phi_max*i / 10.0;
			test_code = c_rd_od_solver.test_member_function(phi_guess_lower, &P_solved_phi_guess_lower);
			if (test_code == 0)
				break;
		}
	}
	if (test_code != 0)
	{
		// Can't find a RC phi guess value that returns an outlet pressure
		error_code = -20;
		return;
	}
	C_monotonic_eq_solver::S_xy_pair phi_pair_lower;
	phi_pair_lower.x = phi_guess_lower;
	phi_pair_lower.y = P_solved_phi_guess_lower;

	// Generate second x-y pair
	double phi_guess_upper = phi_guess_lower*0.5 + mv_stages[0].ms_des_solved.m_phi_max*0.5;
	double P_solved_phi_guess_upper = std::numeric_limits<double>::quiet_NaN();
	test_code = c_rd_od_solver.test_member_function(phi_guess_upper, &P_solved_phi_guess_upper);
	if (test_code != 0)
	{
		for (int i = 6; i < 10; i++)
		{
			phi_guess_upper = phi_guess_lower*i / 10.0 + mv_stages[0].ms_des_solved.m_phi_max*(10 - i) / 10.0;
			test_code = c_rd_od_solver.test_member_function(phi_guess_upper, &P_solved_phi_guess_upper);
			if (test_code == 0)
				break;
		}
		if (test_code != 0 && phi_guess_lower == ms_des_solved.m_phi_des)
		{
			for (int i = 6; i < 10; i++)
			{
				phi_guess_upper = phi_guess_lower*i / 10.0 + ms_des_solved.m_phi_surge*(10 - i) / 10.0;
				test_code = c_rd_od_solver.test_member_function(phi_guess_upper, &P_solved_phi_guess_upper);
				if (test_code == 0)
					break;
			}
		}
	}
	if (test_code != 0)
	{
		// Can't find a RC 2nd guess value (which, if we've found a first, means the solution space is really small?)
		error_code = -20;
		return;
	}
	C_monotonic_eq_solver::S_xy_pair phi_pair_upper;
	phi_pair_upper.x = phi_guess_upper;
	phi_pair_upper.y = P_solved_phi_guess_upper;

	// Set solver settings
	c_rd_od_solver.settings(0.001, 50, phi_lower, phi_upper, true);

	// Now, solve for the flow coefficient
	double phi_solved, tol_solved;
	phi_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int phi_code = 0;
	try
	{
		phi_code = c_rd_od_solver.solve(phi_pair_lower, phi_pair_upper, P_out, phi_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception)
	{
		error_code = -1;
		return;
	}

	if (phi_code != C_monotonic_eq_solver::CONVERGED)
	{
		int n_call_history = (int)c_rd_od_solver.get_solver_call_history()->size();

		if (n_call_history > 0)
			error_code = -(*(c_rd_od_solver.get_solver_call_history()))[n_call_history - 1].err_code;

		if (error_code == 0)
		{
			error_code = phi_code;
		}

		return;
	}

	T_out = ms_od_solved.m_T_out;		//[K]
}

//void C_recompressor_RCMCI_with_ReHeating::off_design_recompressor(double T_in, double P_in, double m_dot, double P_out, int & error_code, double & T_out)
//{
//	/* code from John Dyreby, converted to C++ by Ty Neises
//	! Solve for the outlet state (and shaft speed) of 'comp' given its inlet conditions, mass flow rate, and outlet pressure.
//	!
//	! Inputs:
//	!   comp -- a Compressor object, with design-point values and sizing set
//	!   T_in -- compressor inlet temperature (K)
//	!   P_in -- compressor inlet pressure (kPa)
//	!   m_dot -- mass flow rate through compressor (kg/s)
//	!   P_out -- compressor outlet pressure (kPa)
//	!
//	! Outputs:
//	!   error_trace -- an ErrorTrace object
//	!   T_out -- compressor outlet temperature (K)
//	!
//	! Notes:
//	!   1) This subroutine also sets the following values in 'comp': N, surge, eta, w, w_tip_ratio, phi
//	!   2) In order to solve the compressor, the value for flow coefficient (phi) is varied until convergence.
//	!   3) Surge is not allowed; if the corresponding flow coefficient is not between phi_min and phi_max an error is raised.
//	!   4) Two-stage recompressor; surge is true if either stages are in surge conditions; w_tip_ratio is max of the two stages.
//	*/
//
//	CO2_state co2_props;
//
//	// Fully define the inlet state of the compressor
//	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	ms_od_inputs.m_rho_in = co2_props.dens;
//	ms_od_inputs.m_h_in = co2_props.enth;
//	ms_od_inputs.m_s_in = co2_props.entr;
//	ms_od_inputs.m_m_dot = m_dot;
//
//	// Apply 1 var solver to find the phi that results in a converged recompressor
//	C_mono_eq_phi_off_design c_rc_od(this);
//	C_monotonic_eq_solver c_rd_od_solver(c_rc_od);
//	
//	// Set upper and lower bounds
//	double phi_upper = ms_des_solved.m_phi_max;
//	double phi_lower = ms_des_solved.m_phi_surge;
//
//	// Generate first x-y pair
//	double phi_guess_lower = ms_des_solved.m_phi_des;
//	double P_solved_phi_guess_lower = std::numeric_limits<double>::quiet_NaN();
//	int test_code = c_rd_od_solver.test_member_function(phi_guess_lower, &P_solved_phi_guess_lower);
//	if( test_code != 0 )
//	{
//		for(int i = 1; i < 9; i++)
//		{
//			phi_guess_lower = ms_des_solved.m_phi_des*(10-i)/10.0 + ms_des_solved.m_phi_max*i/10.0;
//			test_code = c_rd_od_solver.test_member_function(phi_guess_lower, &P_solved_phi_guess_lower);
//			if(test_code == 0)
//				break;
//		}	
//	}
//	if( test_code != 0 )
//	{
//		// Can't find a RC phi guess value that returns an outlet pressure
//		error_code = -20;
//		return;
//	}
//	C_monotonic_eq_solver::S_xy_pair phi_pair_lower;
//	phi_pair_lower.x = phi_guess_lower;
//	phi_pair_lower.y = P_solved_phi_guess_lower;
//
//	// Generate second x-y pair
//	double phi_guess_upper = phi_guess_lower*0.5 + ms_des_solved.m_phi_max*0.5;
//	double P_solved_phi_guess_upper = std::numeric_limits<double>::quiet_NaN();
//	test_code = c_rd_od_solver.test_member_function(phi_guess_upper, &P_solved_phi_guess_upper);
//	if( test_code != 0 )
//	{
//		for(int i = 6; i < 10; i++)
//		{
//			phi_guess_upper = phi_guess_lower*i/10.0 + ms_des_solved.m_phi_max*(10-i)/10.0;
//			test_code = c_rd_od_solver.test_member_function(phi_guess_upper, &P_solved_phi_guess_upper);
//			if(test_code == 0)
//				break;
//		}
//		if( test_code != 0 && phi_guess_lower == ms_des_solved.m_phi_des )
//		{
//			for(int i = 6; i < 10; i++)
//			{
//				phi_guess_upper = phi_guess_lower*i/10.0 + ms_des_solved.m_phi_surge*(10-i)/10.0;
//				test_code = c_rd_od_solver.test_member_function(phi_guess_upper, &P_solved_phi_guess_upper);
//				if( test_code == 0 )
//					break;
//			}
//		}
//	}
//	if( test_code != 0 )
//	{
//		// Can't find a RC 2nd guess value (which, if we've found a first, means the solution space is really small?)
//		error_code = -20;
//		return;
//	}
//	C_monotonic_eq_solver::S_xy_pair phi_pair_upper;
//	phi_pair_upper.x = phi_guess_upper;
//	phi_pair_upper.y = P_solved_phi_guess_upper;
//
//	// Set solver settings
//	c_rd_od_solver.settings(0.001, 50, phi_lower, phi_upper, true);
//
//	// Now, solve for the flow coefficient
//	double phi_solved, tol_solved;
//	phi_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
//	int iter_solved = -1;
//
//	int phi_code = 0;
//	try
//	{
//		phi_code = c_rd_od_solver.solve(phi_pair_lower, phi_pair_upper, P_out, phi_solved, tol_solved, iter_solved);
//	}
//	catch( C_csp_exception )
//	{
//		error_code = -1;
//		return;
//	}
//
//	if( phi_code != C_monotonic_eq_solver::CONVERGED )
//	{
//		int n_call_history = (int)c_rd_od_solver.get_solver_call_history()->size();
//
//		if( n_call_history > 0 )
//			error_code = -(*(c_rd_od_solver.get_solver_call_history()))[n_call_history - 1].err_code;
//
//		if( error_code == 0 )
//		{
//			error_code = phi_code;
//		}
//
//		return;
//	}
//
//	T_out = ms_od_solved.m_T_out;		//[K]
//}

//void C_recompressor_RCMCI_with_ReHeating::off_design_recompressor(double T_in, double P_in, double m_dot, double P_out, int & error_code, double & T_out)
//{
//	/* code from John Dyreby, converted to C++ by Ty Neises
//	! Solve for the outlet state (and shaft speed) of 'comp' given its inlet conditions, mass flow rate, and outlet pressure.
//	!
//	! Inputs:
//	!   comp -- a Compressor object, with design-point values and sizing set
//	!   T_in -- compressor inlet temperature (K)
//	!   P_in -- compressor inlet pressure (kPa)
//	!   m_dot -- mass flow rate through compressor (kg/s)
//	!   P_out -- compressor outlet pressure (kPa)
//	!
//	! Outputs:
//	!   error_trace -- an ErrorTrace object
//	!   T_out -- compressor outlet temperature (K)
//	!
//	! Notes:
//	!   1) This subroutine also sets the following values in 'comp': N, surge, eta, w, w_tip_ratio, phi
//	!   2) In order to solve the compressor, the value for flow coefficient (phi) is varied until convergence.
//	!   3) Surge is not allowed; if the corresponding flow coefficient is not between phi_min and phi_max an error is raised.
//	!   4) Two-stage recompressor; surge is true if either stages are in surge conditions; w_tip_ratio is max of the two stages.
//	*/
//
//	CO2_state co2_props;
//
//	// Fully define the inlet state of the compressor
//	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	double rho_in = co2_props.dens;
//	double h_in = co2_props.enth;
//	double s_in = co2_props.entr;
//
//	// Iterate on first-stage phi
//	double phi_1 = m_snl_phi_design;		// start with design-point value
//	bool first_pass = true;
//
//	int max_iter = 100;
//	double rel_tol = 1.0E-9;
//
//	double last_phi_1 = std::numeric_limits<double>::quiet_NaN();
//	double last_residual = std::numeric_limits<double>::quiet_NaN();
//	double P_out_calc = std::numeric_limits<double>::quiet_NaN();
//	double h_out = std::numeric_limits<double>::quiet_NaN();
//	double N = std::numeric_limits<double>::quiet_NaN();
//	double phi_2 = std::numeric_limits<double>::quiet_NaN();
//	double U_tip_1 = std::numeric_limits<double>::quiet_NaN();
//	double ssnd_int = std::numeric_limits<double>::quiet_NaN();
//	double U_tip_2 = std::numeric_limits<double>::quiet_NaN();
//
//	int i = -1;
//	for( i = 0; i < max_iter; i++ )
//	{
//		// First stage - dh_s and eta_stage_1
//		U_tip_1 = m_dot / (phi_1*rho_in*pow(ms_des_solved.m_D_rotor, 2));		//[m/s]
//		N = (U_tip_1*2.0 / ms_des_solved.m_D_rotor)*9.549296590;					//[rpm] shaft spped
//		double phi_star = phi_1*pow((N / ms_des_solved.m_N_design), 0.2);				//[-] Modified flow coefficient
//		double psi_star = ((((-498626.0*phi_star) + 53224.0)*phi_star - 2505.0)*phi_star + 54.6)*phi_star + 0.04049;		//[-] from dimensionless modified head curve
//		double psi = psi_star / (pow(ms_des_solved.m_N_design / N, pow(20.0*phi_star, 3.0)));
//		double dh_s = psi*pow(U_tip_1, 2)*0.001;										//[kJ/kg] Calculated ideal enthalpy rise in first stage of compressor
//		double eta_star = ((((-1.638e6*phi_star) + 182725.0)*phi_star - 8089.0)*phi_star + 168.6)*phi_star - 0.7069;		//[-] from dimensionless modified efficiency curve
//		double eta_0 = eta_star*1.47528 / pow((ms_des_solved.m_N_design / N), pow(20.0*phi_star, 5));		//[-] Stage efficiency is normalized so it equals 1.0 at snl_phi_design
//		double eta_stage_1 = max(eta_0*ms_des_solved.m_eta_design, 0.0);				//[-] The actual stage efficiency, not allowed to go negative
//
//		// Calculate first - stage outlet (second - stage inlet) state
//		double dh = dh_s / eta_stage_1;		//[kJ/kg] Actual enthalpy rise in first stage
//		double h_s_out = h_in + dh_s;		//[kJ/kg] Ideal enthalpy between stages
//		double h_int = h_in + dh;			//[kJ/kg] Actual enthalpy between stages
//
//		prop_error_code = CO2_HS(h_s_out, s_in, &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		double P_int = co2_props.pres;
//
//		prop_error_code = CO2_PH(P_int, h_int, &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		double D_int = co2_props.dens;
//		double s_int = co2_props.entr;
//		ssnd_int = co2_props.ssnd;
//
//		// Second stage - dh_s and eta_stage_2
//		U_tip_2 = ms_des_solved.m_D_rotor_2*0.5*N*0.104719755;				// second-stage tip speed in m/s
//		phi_2 = m_dot / (D_int*U_tip_2*pow(ms_des_solved.m_D_rotor_2, 2));	// second-stage flow coefficient
//		phi_star = phi_2*pow((N / ms_des_solved.m_N_design), 0.2);					// modified flow coefficient
//		psi_star = ((((-498626.0*phi_star) + 53224.0)*phi_star - 2505.0)*phi_star + 54.6)*phi_star + 0.04049;	// from dimensionless modified head curve
//		psi = psi_star / (pow(ms_des_solved.m_N_design / N, pow(20.0*phi_star, 3.0)));
//		dh_s = psi*pow(U_tip_2, 2)*0.001;									// [kJ/kg] Calculated ideal enthalpy rise in second stage of compressor, from definition of head coefficient
//		eta_star = ((((-1.638e6*phi_star) + 182725.0)*phi_star - 8089.0)*phi_star + 168.6)*phi_star - 0.7069;		//[-] from dimensionless modified efficiency curve
//		eta_0 = eta_star*1.47528 / pow((ms_des_solved.m_N_design / N), pow(20.0*phi_star, 5));		//[-] Stage efficiency is normalized so it equals 1.0 at snl_phi_design
//		double eta_stage_2 = max(eta_0*ms_des_solved.m_eta_design, 0.0);			// the actual stage efficiency, not allowed to go negative
//
//		// Calculate second-stage outlet state
//		dh = dh_s / eta_stage_2;			// actual enthalpy rise in second stage
//		h_s_out = h_int + dh_s;				// ideal enthalpy at compressor outlet
//		h_out = h_int + dh;					// actual enthalpy at compressor outlet
//
//		// Get the calculated compressor outlet pressure
//		prop_error_code = CO2_HS(h_s_out, s_int, &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		P_out_calc = co2_props.pres;
//
//		// Check for convergence and adjust phi_1 guess
//		double residual = P_out - P_out_calc;
//		if( fabs(residual) / P_out <= rel_tol )
//			break;
//
//		double next_phi = std::numeric_limits<double>::quiet_NaN();
//		if( first_pass )
//		{
//			next_phi = phi_1*1.0001;		// take a small step
//			first_pass = false;
//		}
//		else
//			next_phi = phi_1 - residual*(last_phi_1 - phi_1) / (last_residual - residual);		// next guess predicted using secant method
//
//		last_phi_1 = phi_1;
//		last_residual = residual;
//		phi_1 = next_phi;
//	}
//
//	// Check for convergence
//	if( i == max_iter )		// did not converge
//	{
//		error_code = 1;
//		return;
//	}
//
//	// Determine outlet temperature and speed of sound
//	prop_error_code = CO2_PH(P_out_calc, h_out, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	T_out = co2_props.temp;
//	double ssnd_out = co2_props.ssnd;
//
//	// Outlet specific enthalpy after isentropic compression
//	prop_error_code = CO2_PS(P_out_calc, s_in, &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	double h_s_out = co2_props.enth;
//
//	// Set relevant recompressor variables
//	ms_od_solved.m_N = N;
//	ms_od_solved.m_eta = (h_s_out - h_in) / (h_out - h_in);		// use overall isentropic efficiency
//	ms_od_solved.m_phi = phi_1;
//	ms_od_solved.m_phi_2 = phi_2;
//	ms_od_solved.m_w_tip_ratio = max(U_tip_1 / ssnd_int, U_tip_2 / ssnd_out);	// store ratio
//	ms_od_solved.m_surge = phi_1 < m_snl_phi_min || phi_2 < m_snl_phi_min;
//	ms_od_solved.m_W_dot_in = m_dot*(h_out - h_in);		//[kWe]
//	ms_od_solved.m_surge_safety = min(phi_1,phi_2)/m_snl_phi_min;
//}

//void C_RecompCycle_RCMCI_with_ReHeating::design_core_bypass(int & error_code)
//{
//	CO2_state co2_props;
//
//	int max_iter = 500;
//	double temperature_tolerance = 1.E-6;		// Temp differences below this are considered zero
//
//	int cpp_offset = 1;
//
//	// Initialize a few variables
//	double m_dot_t, m_dot_mc, m_dot_rc, Q_dot_LT, Q_dot_HT, UA_LT_calc, UA_HT_calc;
//	m_dot_t = m_dot_mc = m_dot_rc = Q_dot_LT = Q_dot_HT = UA_LT_calc = UA_HT_calc = 0.0;
//
//	m_temp_last[1 - cpp_offset] = ms_des_par.m_T_mc_in;
//	m_pres_last[1 - cpp_offset] = ms_des_par.m_P_mc_in;
//	m_pres_last[2 - cpp_offset] = ms_des_par.m_P_mc_out;
//	m_temp_last[6 - cpp_offset] = ms_des_par.m_T_t_in;
//
//	// Apply pressure drops to heat exchangers, fully defining the pressures at all states
//	if( ms_des_par.m_DP_LT[1 - cpp_offset] < 0.0 )
//		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset] - m_pres_last[2 - cpp_offset] * fabs(ms_des_par.m_DP_LT[1 - cpp_offset]);	// relative pressure drop specified for LT recuperator (cold stream)
//	else
//		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset] - ms_des_par.m_DP_LT[1 - cpp_offset];									// absolute pressure drop specified for LT recuperator (cold stream)
//
//	if( ms_des_par.m_UA_LT < 1.0E-12 )
//		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset];		// If there is no LT recuperator, there is no pressure drop
//
//	m_pres_last[4 - cpp_offset] = m_pres_last[3 - cpp_offset];			// Assume no pressure drop in mixing valve
//	m_pres_last[10 - cpp_offset] = m_pres_last[3 - cpp_offset];			// Assume no pressure drop in mixing valve
//
//	if( ms_des_par.m_DP_HT[1 - cpp_offset] < 0.0 )
//		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset] - m_pres_last[4 - cpp_offset] * fabs(ms_des_par.m_DP_HT[1 - cpp_offset]);	// relative pressure drop specified for HT recuperator (cold stream)
//	else
//		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset] - ms_des_par.m_DP_HT[1 - cpp_offset];									// absolute pressure drop specified for HT recuperator (cold stream)
//
//	if( ms_des_par.m_UA_HT < 1.0E-12 )
//		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset];		// If there is no HT recuperator, there is no pressure drop
//
//	if( ms_des_par.m_DP_PHX[1 - cpp_offset] < 0.0 )
//		m_pres_last[6 - cpp_offset] = m_pres_last[5 - cpp_offset] - m_pres_last[5 - cpp_offset] * fabs(ms_des_par.m_DP_PHX[1 - cpp_offset]);	// relative pressure drop specified for PHX
//	else
//		m_pres_last[6 - cpp_offset] = m_pres_last[5 - cpp_offset] - ms_des_par.m_DP_PHX[1 - cpp_offset];									// absolute pressure drop specified for PHX
//
//	if( ms_des_par.m_DP_PC[2 - cpp_offset] < 0.0 )
//		m_pres_last[9 - cpp_offset] = m_pres_last[1 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_PC[2 - cpp_offset]));	// relative pressure drop specified for precooler: P1=P9-P9*rel_DP => P1=P9*(1-rel_DP)
//	else
//		m_pres_last[9 - cpp_offset] = m_pres_last[1 - cpp_offset] + ms_des_par.m_DP_PC[2 - cpp_offset];
//
//	if( ms_des_par.m_DP_LT[2 - cpp_offset] < 0.0 )
//		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_LT[2 - cpp_offset]));	// relative pressure drop specified for LT recuperator (hot stream)
//	else
//		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset] + ms_des_par.m_DP_LT[2 - cpp_offset];				// absolute pressure drop specified for LT recuperator (hot stream)
//
//	if( ms_des_par.m_UA_LT < 1.0E-12 )
//		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset];		// if there is no LT recuperator, there is no pressure drop
//
//	if( ms_des_par.m_DP_HT[2 - cpp_offset] < 0.0 )
//		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_HT[2 - cpp_offset]));	// relative pressure drop specified for HT recuperator (hot stream)
//	else
//		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset] + ms_des_par.m_DP_HT[2 - cpp_offset];				// absolute pressure drop specified for HT recuperator (hot stream)
//
//	if( ms_des_par.m_UA_HT < 1.0E-12 )
//		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset];		// if there is no HT recuperator, there is no pressure drop
//
//	// Determine equivalent isentropic efficiencies for main compressor and turbine, if necessary.
//	double eta_mc_isen = std::numeric_limits<double>::quiet_NaN();
//	double eta_t_isen = std::numeric_limits<double>::quiet_NaN();
//	if( ms_des_par.m_eta_mc < 0.0 )
//	{
//		int poly_error_code = 0;
//
//		isen_eta_from_poly_eta(m_temp_last[1 - cpp_offset], m_pres_last[1 - cpp_offset], m_pres_last[2 - cpp_offset], fabs(ms_des_par.m_eta_mc),
//			true, poly_error_code, eta_mc_isen);
//
//		if( poly_error_code != 0 )
//		{
//			error_code = poly_error_code;
//			return;
//		}
//	}
//	else
//		eta_mc_isen = ms_des_par.m_eta_mc;
//
//	if( ms_des_par.m_eta_t < 0.0 )
//	{
//		int poly_error_code = 0;
//
//		isen_eta_from_poly_eta(m_temp_last[6 - cpp_offset], m_pres_last[6 - cpp_offset], m_pres_last[7 - cpp_offset], fabs(ms_des_par.m_eta_t),
//			false, poly_error_code, eta_t_isen);
//
//		if( poly_error_code != 0 )
//		{
//			error_code = poly_error_code;
//			return;
//		}
//	}
//	else
//		eta_t_isen = ms_des_par.m_eta_t;
//
//	// Determine the outlet state and specific work for the main compressor and turbine.
//	int comp_error_code = 0;
//	double w_mc = std::numeric_limits<double>::quiet_NaN();
//	// Main compressor
//	calculate_turbomachinery_outlet_1(m_temp_last[1 - cpp_offset], m_pres_last[1 - cpp_offset], m_pres_last[2 - cpp_offset], eta_mc_isen, true,
//		comp_error_code, m_enth_last[1 - cpp_offset], m_entr_last[1 - cpp_offset], m_dens_last[1 - cpp_offset], m_temp_last[2 - cpp_offset],
//		m_enth_last[2 - cpp_offset], m_entr_last[2 - cpp_offset], m_dens_last[2 - cpp_offset], w_mc);
//
//	if( comp_error_code != 0 )
//	{
//		error_code = comp_error_code;
//		return;
//	}
//
//	int turbine_error_code = 0;
//	double w_t = std::numeric_limits<double>::quiet_NaN();
//	// Turbine
//	calculate_turbomachinery_outlet_1(m_temp_last[6 - cpp_offset], m_pres_last[6 - cpp_offset], m_pres_last[7 - cpp_offset], eta_t_isen, false,
//		turbine_error_code, m_enth_last[6 - cpp_offset], m_entr_last[6 - cpp_offset], m_dens_last[6 - cpp_offset], m_temp_last[7 - cpp_offset],
//		m_enth_last[7 - cpp_offset], m_entr_last[7 - cpp_offset], m_dens_last[7 - cpp_offset], w_t);
//
//	if( turbine_error_code != 0 )
//	{
//		error_code = turbine_error_code;
//		return;
//	}
//
//	// Check that this cycle can produce power
//	double eta_rc_isen = std::numeric_limits<double>::quiet_NaN();
//	double w_rc = std::numeric_limits<double>::quiet_NaN();
//	if( ms_des_par.m_recomp_frac >= 1.E-12 )
//	{
//		if( ms_des_par.m_eta_rc < 0.0 )		// need to convert polytropic efficiency to isentropic efficiency
//		{
//			int rc_error_code = 0;
//
//			isen_eta_from_poly_eta(m_temp_last[2 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], fabs(ms_des_par.m_eta_rc),
//				true, rc_error_code, eta_rc_isen);
//
//			if( rc_error_code != 0 )
//			{
//				error_code = rc_error_code;
//				return;
//			}
//		}
//		else
//			eta_rc_isen = ms_des_par.m_eta_rc;
//
//		int rc_error_code = 0;
//		calculate_turbomachinery_outlet_1(m_temp_last[2 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], eta_rc_isen,
//			true, rc_error_code, w_rc);
//
//		if( rc_error_code != 0 )
//		{
//			error_code = rc_error_code;
//			return;
//		}
//	}
//	else
//		w_rc = 0.0;
//
//	if( w_mc + w_rc + w_t <= 0.0 )	// positive net power is impossible; return an error
//	{
//		error_code = 25;
//		return;
//	}
//
//	// Outer iteration loop: temp(8), checking against UA_HT
//	double T8_lower_bound, T8_upper_bound, last_HT_residual, last_T8_guess;
//	T8_lower_bound = T8_upper_bound = last_HT_residual = last_T8_guess = std::numeric_limits<double>::quiet_NaN();
//	if( ms_des_par.m_UA_HT < 1.0E-12 )		// no high-temp recuperator
//	{
//		T8_lower_bound = m_temp_last[7 - cpp_offset];		// no iteration necessary
//		T8_upper_bound = m_temp_last[7 - cpp_offset];		// no iteration necessary
//		m_temp_last[8 - cpp_offset] = m_temp_last[7 - cpp_offset];
//		UA_HT_calc = 0.0;
//		last_HT_residual = 0.0;
//		last_T8_guess = m_temp_last[7 - cpp_offset];
//	}
//	else
//	{
//		T8_lower_bound = m_temp_last[2 - cpp_offset];		// the absolute lower temp(8) could be
//		T8_upper_bound = m_temp_last[7 - cpp_offset];		// the absolutely highest temp(8) could be
//		m_temp_last[8 - cpp_offset] = (T8_lower_bound + T8_upper_bound)*0.5;		// bisect bounds for first guess
//		UA_HT_calc = -1.0;
//		last_HT_residual = ms_des_par.m_UA_HT;			// know a priori that with T8 = T7, UA_calc = 0 therefore residual is UA_HT - 0.0
//		last_T8_guess = m_temp_last[7 - cpp_offset];
//	}
//
//	int prop_error_code = 0;
//
//	double T9_lower_bound, T9_upper_bound, last_LT_residual, last_T9_guess;
//	T9_lower_bound = T9_upper_bound = last_LT_residual = last_T9_guess = std::numeric_limits<double>::quiet_NaN();
//
//	double min_DT_LT = std::numeric_limits<double>::quiet_NaN();
//	double min_DT_HT = std::numeric_limits<double>::quiet_NaN();
//
//	int T8_iter = -1;
//	for( T8_iter = 0; T8_iter < max_iter; T8_iter++ )
//	{
//		// Fully define state 8
//		prop_error_code = CO2_TP(m_temp_last[8 - cpp_offset], m_pres_last[8 - cpp_offset], &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		m_enth_last[8 - cpp_offset] = co2_props.enth;
//		m_entr_last[8 - cpp_offset] = co2_props.entr;
//		m_dens_last[8 - cpp_offset] = co2_props.dens;
//
//		// Inner iteration loop: temp(9), checking against UA_LT
//		if( ms_des_par.m_UA_LT < 1.0E-12 )	// no low-temperature recuperator
//		{
//			T9_lower_bound = m_temp_last[8 - cpp_offset];		// no iteration necessary
//			T9_upper_bound = m_temp_last[8 - cpp_offset];		// no iteration necessary
//			m_temp_last[9 - cpp_offset] = m_temp_last[8 - cpp_offset];
//			UA_LT_calc = 0.0;
//			last_LT_residual = 0.0;
//			last_T9_guess = m_temp_last[8 - cpp_offset];
//		}
//		else
//		{
//			T9_lower_bound = m_temp_last[2 - cpp_offset];		// the absolute lowest temp(9) could be
//			T9_upper_bound = m_temp_last[8 - cpp_offset];		// the absolute highest temp(9) could be
//			m_temp_last[9 - cpp_offset] = (T9_lower_bound + T9_upper_bound)*0.5;	// biset bounds for first guess
//			UA_LT_calc = -1.0;
//			last_LT_residual = ms_des_par.m_UA_LT;			// know a priori that with T9 = T8, UA_calc = 0 therefore residual is UA_LT - 0
//			last_T9_guess = m_temp_last[8 - cpp_offset];
//		}
//
//		int T9_iter = -1;
//		for( T9_iter = 0; T9_iter < max_iter; T9_iter++ )
//		{
//			// Determine the outlet state of the recompressing compressor and its specific work
//			if( ms_des_par.m_recomp_frac >= 1.E-12 )
//			{
//				if( ms_des_par.m_eta_rc < 0.0 )		// recalculate isentropic efficiency of recompressing compressor (because T9 changes)
//				{
//					int rc_error_code = 0;
//					isen_eta_from_poly_eta(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], fabs(ms_des_par.m_eta_rc), true,
//						rc_error_code, eta_rc_isen);
//
//					if( rc_error_code != 0 )
//					{
//						error_code = rc_error_code;
//						return;
//					}
//				}
//				else
//				{
//					eta_rc_isen = ms_des_par.m_eta_rc;
//				}
//
//				int rc_error_code = 0;
//				calculate_turbomachinery_outlet_1(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], eta_rc_isen, true, rc_error_code,
//					m_enth_last[9 - cpp_offset], m_entr_last[9 - cpp_offset], m_dens_last[9 - cpp_offset], m_temp_last[10 - cpp_offset], m_enth_last[10 - cpp_offset], m_entr_last[10 - cpp_offset],
//					m_dens_last[10 - cpp_offset], w_rc);
//
//				if( rc_error_code != 0 )
//				{
//					error_code = rc_error_code;
//					return;
//				}
//			}
//			else
//			{
//				w_rc = 0.0;		// the recompressing compressor does not exist
//				prop_error_code = CO2_TP(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], &co2_props);
//				if( prop_error_code != 0 )		// fully define state 9
//				{
//					error_code = prop_error_code;
//					return;
//				}
//				m_enth_last[9 - cpp_offset] = co2_props.enth;
//				m_entr_last[9 - cpp_offset] = co2_props.entr;
//				m_dens_last[9 - cpp_offset] = co2_props.dens;
//				m_temp_last[10 - cpp_offset] = m_temp_last[9 - cpp_offset];		// assume state 10 is the same as state 9
//				m_enth_last[10 - cpp_offset] = m_enth_last[9 - cpp_offset];
//				m_entr_last[10 - cpp_offset] = m_entr_last[9 - cpp_offset];
//				m_dens_last[10 - cpp_offset] = m_dens_last[9 - cpp_offset];
//			}
//
//			// Knowing the specific work of the recompressor, the required mass flow rate can be calculated
//			//m_dot_t = ms_des_par.m_W_dot_net / (w_mc*(1.0 - ms_des_par.m_recomp_frac) + w_rc*ms_des_par.m_recomp_frac + w_t);	// Required mass flow rate through turbine
//			m_dot_t = ms_des_par.m_W_dot_net / (w_mc + w_t);	// Required mass flow rate through turbine
//			
//			if( m_dot_t < 0.0 )		// positive power output is not possible with these inputs
//			{
//				error_code = 29;
//				return;
//			}
//			m_dot_rc = m_dot_t * ms_des_par.m_recomp_frac;		// apply definition of recompression fraction
//			m_dot_mc = m_dot_t - m_dot_rc;						// mass balance
//
//			// Calculate the UA value of the low-temperature recuperator
//			if( ms_des_par.m_UA_LT < 1.0E-12 )		// no low-temp recuperator (this check is necessary to prevent pressure drops with UA=0 from causing problems)
//				Q_dot_LT = 0.0;
//			else
//				Q_dot_LT = m_dot_t * (m_enth_last[8 - cpp_offset] - m_enth_last[9 - cpp_offset]);
//
//			min_DT_LT = std::numeric_limits<double>::quiet_NaN();
//			
//			// Define variables that method outputs that we don't use
//			double eff_LT_hx, NTU_LT_hx, T_h_out_LT_hx, T_c_out_LT_hx, q_dot_LT_hx;
//			eff_LT_hx = NTU_LT_hx = T_h_out_LT_hx = T_c_out_LT_hx = q_dot_LT_hx = std::numeric_limits<double>::quiet_NaN();
//
//			try 
//			{
//			mc_LT_recup.calc_req_UA(Q_dot_LT, m_dot_mc, m_dot_t, m_temp_last[MC_OUT], m_temp_last[HTR_LP_OUT], 
//				m_pres_last[MC_OUT], m_pres_last[LTR_HP_OUT], m_pres_last[HTR_LP_OUT], m_pres_last[LTR_LP_OUT],
//				UA_LT_calc, min_DT_LT, eff_LT_hx, NTU_LT_hx, T_h_out_LT_hx, T_c_out_LT_hx, q_dot_LT_hx);
//			}
//			catch( C_csp_exception & csp_except )
//			{
//				if( csp_except.m_error_code == 11 )		// second-law violation in hxr, therefore temp(9) is too low
//				{
//					T9_lower_bound = m_temp_last[9 - cpp_offset];
//					m_temp_last[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);		// bisect bounds for next guess
//					continue;
//				}
//				else
//				{
//					error_code = csp_except.m_error_code;
//					return;
//				}
//			
//			}
//
//			//calculate_hxr_UA_1(ms_des_par.m_N_sub_hxrs, Q_dot_LT, m_dot_mc, m_dot_t, m_temp_last[2 - cpp_offset], m_temp_last[8 - cpp_offset],
//			//	m_pres_last[2 - cpp_offset], m_pres_last[3 - cpp_offset], m_pres_last[8 - cpp_offset], m_pres_last[9 - cpp_offset],
//			//	hx_error_code, UA_LT_calc, min_DT_LT);
//			//
//			//if( hx_error_code != 0 )
//			//{
//			//	if( hx_error_code == 11 )		// second-law violation in hxr, therefore temp(9) is too low
//			//	{
//			//		T9_lower_bound = m_temp_last[9 - cpp_offset];
//			//		m_temp_last[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);		// bisect bounds for next guess
//			//		continue;
//			//	}
//			//	else
//			//	{
//			//		error_code = hx_error_code;
//			//		return;
//			//	}
//			//}
//
//			// Check for convergence and adjust T9 appropriately
//			double UA_LT_residual = ms_des_par.m_UA_LT - UA_LT_calc;
//
//			if( fabs(UA_LT_residual) < 1.0E-12 )		// catches no LT case
//				break;
//
//			double secant_guess = m_temp_last[9 - cpp_offset] - UA_LT_residual*(last_T9_guess - m_temp_last[9 - cpp_offset]) / (last_LT_residual - UA_LT_residual);	// next guess predicted using secant method
//
//			if( UA_LT_residual < 0.0 )		// UA_LT_calc is too big, temp(9) needs to be higher
//			{
//				if( fabs(UA_LT_residual) / ms_des_par.m_UA_LT < ms_des_par.m_tol )
//					break;
//
//				T9_lower_bound = m_temp_last[9 - cpp_offset];
//			}
//			else		// UA_LT_calc is too small, temp(9) needs to be lower
//			{
//				if( UA_LT_residual / ms_des_par.m_UA_LT < ms_des_par.m_tol )	// UA_LT converged
//					break;
//
//				if( min_DT_LT < temperature_tolerance )		// UA_calc is still too low but there isn't anywhere to go so it's ok (catches huge UA values)
//					break;
//
//				T9_upper_bound = m_temp_last[9 - cpp_offset];
//			}
//
//			last_LT_residual = UA_LT_residual;			// reset lsat stored residual value
//			last_T9_guess = m_temp_last[9 - cpp_offset];	// reset last stored guess value
//
//			// Check if the secant method overshoots and fall back to bisection if it does
//			if( secant_guess <= T9_lower_bound || secant_guess >= T9_upper_bound || secant_guess != secant_guess )	// secant method overshot (or is NaN), use bisection
//				m_temp_last[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);
//			else
//				m_temp_last[9 - cpp_offset] = secant_guess;
//
//		}	// End T9 iteration
//
//		// Check that T9_loop converged
//		if( T9_iter >= max_iter )
//		{
//			error_code = 31;
//			return;
//		}
//
//		// State 3 can now be fully defined
//		m_enth_last[3 - cpp_offset] = m_enth_last[2 - cpp_offset] + Q_dot_LT / m_dot_mc;		// Energy balalnce on cold stream of low-temp recuperator
//		prop_error_code = CO2_PH(m_pres_last[3 - cpp_offset], m_enth_last[3 - cpp_offset], &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		m_temp_last[3 - cpp_offset] = co2_props.temp;
//		m_entr_last[3 - cpp_offset] = co2_props.entr;
//		m_dens_last[3 - cpp_offset] = co2_props.dens;
//
//		// Go through the mixing valve
//		if( ms_des_par.m_recomp_frac >= 1.E-12 )
//		{
//			m_enth_last[4 - cpp_offset] = (1.0 - ms_des_par.m_recomp_frac)*m_enth_last[3 - cpp_offset] + ms_des_par.m_recomp_frac*m_enth_last[10 - cpp_offset];	// conservation of energy (both sides divided by m_dot_t)
//			prop_error_code = CO2_PH(m_pres_last[4 - cpp_offset], m_enth_last[4 - cpp_offset], &co2_props);
//			if( prop_error_code != 0 )
//			{
//				error_code = prop_error_code;
//				return;
//			}
//			m_temp_last[4 - cpp_offset] = co2_props.temp;
//			m_entr_last[4 - cpp_offset] = co2_props.entr;
//			m_dens_last[4 - cpp_offset] = co2_props.dens;
//		}
//		else		// no mixing valve, therefore state 4 is equal to state 3
//		{
//			m_temp_last[4 - cpp_offset] = m_temp_last[3 - cpp_offset];
//			m_enth_last[4 - cpp_offset] = m_enth_last[3 - cpp_offset];
//			m_entr_last[4 - cpp_offset] = m_entr_last[3 - cpp_offset];
//			m_dens_last[4 - cpp_offset] = m_dens_last[3 - cpp_offset];
//		}
//
//		// Check for a second law violation at the outlet of the high-temp recuperator
//		if( m_temp_last[4 - cpp_offset] >= m_temp_last[8 - cpp_offset] )		// temp(8) is not valid and it must be increased
//		{
//			T8_lower_bound = m_temp_last[8 - cpp_offset];
//			m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
//			continue;
//		}
//
//		// Calculate the UA value of the high-temp recuperator
//		if( ms_des_par.m_UA_HT < 1.E-12 )			// no high-temp recuperator
//			Q_dot_HT = 0.0;
//		else
//			Q_dot_HT = m_dot_t * (m_enth_last[7 - cpp_offset] - m_enth_last[8 - cpp_offset]);
//
//		int HT_error_code = 0;
//		min_DT_HT = std::numeric_limits<double>::quiet_NaN();
//
//		calculate_hxr_UA_1(ms_des_par.m_N_sub_hxrs, Q_dot_HT, m_dot_t, m_dot_t, m_temp_last[4 - cpp_offset], m_temp_last[7 - cpp_offset], m_pres_last[4 - cpp_offset],
//			m_pres_last[5 - cpp_offset], m_pres_last[7 - cpp_offset], m_pres_last[8 - cpp_offset], HT_error_code, UA_HT_calc, min_DT_HT);
//
//		if( HT_error_code != 0 )
//		{
//			if( HT_error_code == 11 )			// second-law violation in hxr, therefore temp(8) is too low
//			{
//				T8_lower_bound = m_temp_last[8 - cpp_offset];
//				m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);	// bisect bounds for next guess
//				continue;
//			}
//			else
//			{
//				error_code = HT_error_code;
//				return;
//			}
//		}
//
//		// Check for convergence and adjust T8 appropriately
//		double UA_HT_residual = ms_des_par.m_UA_HT - UA_HT_calc;
//
//		if( fabs(UA_HT_residual) < 1.0E-12 )		// catches no HT case
//			break;
//
//		double secant_guess = m_temp_last[8 - cpp_offset] - UA_HT_residual*(last_T8_guess - m_temp_last[8 - cpp_offset]) / (last_HT_residual - UA_HT_residual);		// Next guess predicted using secant method
//
//		if( UA_HT_residual < 0.0 )	// UA_HT_calc is too big, temp(8) needs to be higher
//		{
//			if( fabs(UA_HT_residual) / ms_des_par.m_UA_HT < ms_des_par.m_tol )
//				break;
//			T8_lower_bound = m_temp_last[8 - cpp_offset];
//		}
//		else						// UA_HT_calc is too small, temp(8) needs to be lower
//		{
//			if( UA_HT_residual / ms_des_par.m_UA_HT < ms_des_par.m_tol )		// UA_HT converged
//				break;
//			if( min_DT_HT < temperature_tolerance )								// UA_calc is still too low, but there isn't anywhere to go so it's okay
//				break;
//			T8_upper_bound = m_temp_last[8 - cpp_offset];
//		}
//		last_HT_residual = UA_HT_residual;				// reset last stored residual value
//		last_T8_guess = m_temp_last[8 - cpp_offset];		// reset last stored guess value
//
//		// Check if the secant method overshoots and fall back to bisection if it does
//		if( secant_guess <= T8_lower_bound || secant_guess >= T8_upper_bound )		// secant method overshot, use bisection
//			m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
//		else
//			m_temp_last[8 - cpp_offset] = secant_guess;
//
//	}	// End T8 iteration
//
//	// Check that T8_loop converged
//	if( T8_iter >= max_iter )
//	{
//		error_code = 35;
//		return;
//	}
//
//	// State 5 can now be fully defined
//	m_enth_last[5 - cpp_offset] = m_enth_last[4 - cpp_offset] + Q_dot_HT / m_dot_t;						// Energy balance on cold stream of high-temp recuperator
//	prop_error_code = CO2_PH(m_pres_last[5 - cpp_offset], m_enth_last[5 - cpp_offset], &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	m_temp_last[5 - cpp_offset] = co2_props.temp;
//	m_entr_last[5 - cpp_offset] = co2_props.entr;
//	m_dens_last[5 - cpp_offset] = co2_props.dens;
//
//	// Calculate performance metrics for low-temperature recuperator
//	C_HeatExchanger::S_design_parameters LT_des_par;
//	double C_dot_hot = m_dot_t*(m_enth_last[8 - cpp_offset] - m_enth_last[9 - cpp_offset]) / (m_temp_last[8 - cpp_offset] - m_temp_last[9 - cpp_offset]);		// LT recuperator hot stream capacitance rate
//	double C_dot_cold = m_dot_mc*(m_enth_last[3 - cpp_offset] - m_enth_last[2 - cpp_offset]) / (m_temp_last[3 - cpp_offset] - m_temp_last[2 - cpp_offset]);	// LT recuperator cold stream capacitance rate
//	double C_dot_min = min(C_dot_hot, C_dot_cold);
//	double Q_dot_max = C_dot_min*(m_temp_last[8 - cpp_offset] - m_temp_last[2 - cpp_offset]);
//	double hx_eff = Q_dot_LT / Q_dot_max;				// Definition of effectiveness
//	LT_des_par.m_DP_design[0] = m_pres_last[2 - cpp_offset] - m_pres_last[3 - cpp_offset];
//	LT_des_par.m_DP_design[1] = m_pres_last[8 - cpp_offset] - m_pres_last[9 - cpp_offset];
//	LT_des_par.m_eff_design = hx_eff;
//	LT_des_par.m_min_DT_design = min_DT_LT;
//	LT_des_par.m_m_dot_design[0] = m_dot_mc;
//	LT_des_par.m_m_dot_design[1] = m_dot_t;
//	LT_des_par.m_N_sub = ms_des_par.m_N_sub_hxrs;
//	LT_des_par.m_Q_dot_design = Q_dot_LT;
//	LT_des_par.m_UA_design = UA_LT_calc;
//	m_LT.initialize(LT_des_par);
//
//	// Calculate performance metrics for high-temperature recuperator
//	C_HeatExchanger::S_design_parameters HT_des_par;
//	C_dot_hot = m_dot_t*(m_enth_last[7 - cpp_offset] - m_enth_last[8 - cpp_offset]) / (m_temp_last[7 - cpp_offset] - m_temp_last[8 - cpp_offset]);			// HT recuperator hot stream capacitance rate
//	C_dot_cold = m_dot_t*(m_enth_last[5 - cpp_offset] - m_enth_last[4 - cpp_offset]) / (m_temp_last[5 - cpp_offset] - m_temp_last[4 - cpp_offset]);			// HT recuperator cold stream capacitance rate
//	C_dot_min = min(C_dot_hot, C_dot_cold);
//	Q_dot_max = C_dot_min*(m_temp_last[7 - cpp_offset] - m_temp_last[4 - cpp_offset]);
//	hx_eff = Q_dot_HT / Q_dot_max;						// Definition of effectiveness
//	HT_des_par.m_DP_design[0] = m_pres_last[4 - cpp_offset] - m_pres_last[5 - cpp_offset];
//	HT_des_par.m_DP_design[1] = m_pres_last[7 - cpp_offset] - m_pres_last[8 - cpp_offset];
//	HT_des_par.m_eff_design = hx_eff;
//	HT_des_par.m_min_DT_design = min_DT_HT;
//	HT_des_par.m_m_dot_design[0] = m_dot_t;
//	HT_des_par.m_m_dot_design[1] = m_dot_t;
//	HT_des_par.m_N_sub = ms_des_par.m_N_sub_hxrs;
//	HT_des_par.m_Q_dot_design = Q_dot_HT;
//	HT_des_par.m_UA_design = UA_HT_calc;
//	m_HT.initialize(HT_des_par);
//
//	// Set relevant values for other heat exchangers
//	C_HeatExchanger::S_design_parameters PHX_des_par;
//	PHX_des_par.m_DP_design[0] = m_pres_last[5 - cpp_offset] - m_pres_last[6 - cpp_offset];
//	PHX_des_par.m_DP_design[1] = 0.0;
//	PHX_des_par.m_m_dot_design[0] = m_dot_t;
//	PHX_des_par.m_m_dot_design[1] = 0.0;
//	PHX_des_par.m_Q_dot_design = m_dot_t*(m_enth_last[6 - cpp_offset] - m_enth_last[5 - cpp_offset]);
//	m_PHX.initialize(PHX_des_par);
//
//	C_HeatExchanger::S_design_parameters PC_des_par;
//	PC_des_par.m_DP_design[0] = 0.0;
//	PC_des_par.m_DP_design[1] = m_pres_last[9 - cpp_offset] - m_pres_last[1 - cpp_offset];
//	PC_des_par.m_m_dot_design[0] = 0.0;
//	PC_des_par.m_m_dot_design[1] = m_dot_mc;
//	PC_des_par.m_Q_dot_design = m_dot_mc*(m_enth_last[9 - cpp_offset] - m_enth_last[1 - cpp_offset]);
//	m_PC.initialize(PC_des_par);
//
//	// Calculate/set cycle performance metrics
//	// m_W_dot_net_last = w_mc*m_dot_mc + w_rc*m_dot_rc + w_t*m_dot_t;
//	m_W_dot_net_last = (w_mc + w_t)*m_dot_t;
//	
//	double Q_dot_heat_shield = m_dot_rc * (m_enth_last[10 - cpp_offset] - m_enth_last[2-cpp_offset]);
//	
//	m_eta_thermal_last = m_W_dot_net_last / (PHX_des_par.m_Q_dot_design + Q_dot_heat_shield);
//
//	//double eta_thermal = ms_od_solved.m_eta_thermal;
//
//	double deltaT_hs = m_temp_last[10-cpp_offset] - m_temp_last[2-cpp_offset];
//
//	double deltaT_low_limit = 150.0;
//	double deltaT_high_limit = 250.0;
//
//	double diff_deltaT_hs = max( max(0.0, deltaT_low_limit - deltaT_hs), max(0.0, deltaT_hs - deltaT_high_limit) );
//
//	double Q_hs_frac_target = 10.0 / 65.0;
//	double Q_hs_frac = Q_dot_heat_shield / (PHX_des_par.m_Q_dot_design+Q_dot_heat_shield);
//
//	double diff_Q_hs = max(0.0, fabs(Q_hs_frac - Q_hs_frac_target) - ms_des_par.m_tol);		
//
//	double E_bal = (PHX_des_par.m_Q_dot_design + Q_dot_heat_shield) - (m_W_dot_net_last + PC_des_par.m_Q_dot_design/m_dot_mc*(m_dot_mc + m_dot_rc));
//	
//	m_eta_thermal_last = m_eta_thermal_last*exp(-diff_deltaT_hs)*exp(-100.0*diff_Q_hs);
//
//	m_m_dot_mc = m_dot_mc;
//	m_m_dot_rc = m_dot_rc;
//	m_m_dot_t = m_dot_t;
//}

//void C_RecompCycle_RCMCI_with_ReHeating::design_core_bypass150C(int & error_code)
//{
//	CO2_state co2_props;
//
//	int max_iter = 500;
//	double temperature_tolerance = 1.E-6;		// Temp differences below this are considered zero
//
//	int cpp_offset = 1;
//
//	// Initialize a few variables
//	double m_dot_t, m_dot_mc, m_dot_rc, Q_dot_LT, Q_dot_HT, UA_LT_calc, UA_HT_calc;
//	m_dot_t = m_dot_mc = m_dot_rc = Q_dot_LT = Q_dot_HT = UA_LT_calc = UA_HT_calc = 0.0;
//
//	m_temp_last[1 - cpp_offset] = ms_des_par.m_T_mc_in;
//	m_pres_last[1 - cpp_offset] = ms_des_par.m_P_mc_in;
//	m_pres_last[2 - cpp_offset] = ms_des_par.m_P_mc_out;
//	m_temp_last[6 - cpp_offset] = ms_des_par.m_T_t_in;
//
//	// Apply pressure drops to heat exchangers, fully defining the pressures at all states
//	if( ms_des_par.m_DP_LT[1 - cpp_offset] < 0.0 )
//		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset] - m_pres_last[2 - cpp_offset] * fabs(ms_des_par.m_DP_LT[1 - cpp_offset]);	// relative pressure drop specified for LT recuperator (cold stream)
//	else
//		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset] - ms_des_par.m_DP_LT[1 - cpp_offset];									// absolute pressure drop specified for LT recuperator (cold stream)
//
//	if( ms_des_par.m_UA_LT < 1.0E-12 )
//		m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset];		// If there is no LT recuperator, there is no pressure drop
//
//	m_pres_last[4 - cpp_offset] = m_pres_last[3 - cpp_offset];			// Assume no pressure drop in mixing valve
//	m_pres_last[10 - cpp_offset] = m_pres_last[3 - cpp_offset];			// Assume no pressure drop in mixing valve
//
//	if( ms_des_par.m_DP_HT[1 - cpp_offset] < 0.0 )
//		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset] - m_pres_last[4 - cpp_offset] * fabs(ms_des_par.m_DP_HT[1 - cpp_offset]);	// relative pressure drop specified for HT recuperator (cold stream)
//	else
//		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset] - ms_des_par.m_DP_HT[1 - cpp_offset];									// absolute pressure drop specified for HT recuperator (cold stream)
//
//	if( ms_des_par.m_UA_HT < 1.0E-12 )
//		m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset];		// If there is no HT recuperator, there is no pressure drop
//
//	if( ms_des_par.m_DP_PHX[1 - cpp_offset] < 0.0 )
//		m_pres_last[6 - cpp_offset] = m_pres_last[5 - cpp_offset] - m_pres_last[5 - cpp_offset] * fabs(ms_des_par.m_DP_PHX[1 - cpp_offset]);	// relative pressure drop specified for PHX
//	else
//		m_pres_last[6 - cpp_offset] = m_pres_last[5 - cpp_offset] - ms_des_par.m_DP_PHX[1 - cpp_offset];									// absolute pressure drop specified for PHX
//
//	if( ms_des_par.m_DP_PC[2 - cpp_offset] < 0.0 )
//		m_pres_last[9 - cpp_offset] = m_pres_last[1 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_PC[2 - cpp_offset]));	// relative pressure drop specified for precooler: P1=P9-P9*rel_DP => P1=P9*(1-rel_DP)
//	else
//		m_pres_last[9 - cpp_offset] = m_pres_last[1 - cpp_offset] + ms_des_par.m_DP_PC[2 - cpp_offset];
//
//	if( ms_des_par.m_DP_LT[2 - cpp_offset] < 0.0 )
//		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_LT[2 - cpp_offset]));	// relative pressure drop specified for LT recuperator (hot stream)
//	else
//		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset] + ms_des_par.m_DP_LT[2 - cpp_offset];				// absolute pressure drop specified for LT recuperator (hot stream)
//
//	if( ms_des_par.m_UA_LT < 1.0E-12 )
//		m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset];		// if there is no LT recuperator, there is no pressure drop
//
//	if( ms_des_par.m_DP_HT[2 - cpp_offset] < 0.0 )
//		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_HT[2 - cpp_offset]));	// relative pressure drop specified for HT recuperator (hot stream)
//	else
//		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset] + ms_des_par.m_DP_HT[2 - cpp_offset];				// absolute pressure drop specified for HT recuperator (hot stream)
//
//	if( ms_des_par.m_UA_HT < 1.0E-12 )
//		m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset];		// if there is no HT recuperator, there is no pressure drop
//
//	// Determine equivalent isentropic efficiencies for main compressor and turbine, if necessary.
//	double eta_mc_isen = std::numeric_limits<double>::quiet_NaN();
//	double eta_t_isen = std::numeric_limits<double>::quiet_NaN();
//	if( ms_des_par.m_eta_mc < 0.0 )
//	{
//		int poly_error_code = 0;
//
//		isen_eta_from_poly_eta(m_temp_last[1 - cpp_offset], m_pres_last[1 - cpp_offset], m_pres_last[2 - cpp_offset], fabs(ms_des_par.m_eta_mc),
//			true, poly_error_code, eta_mc_isen);
//
//		if( poly_error_code != 0 )
//		{
//			error_code = poly_error_code;
//			return;
//		}
//	}
//	else
//		eta_mc_isen = ms_des_par.m_eta_mc;
//
//	if( ms_des_par.m_eta_t < 0.0 )
//	{
//		int poly_error_code = 0;
//
//		isen_eta_from_poly_eta(m_temp_last[6 - cpp_offset], m_pres_last[6 - cpp_offset], m_pres_last[7 - cpp_offset], fabs(ms_des_par.m_eta_t),
//			false, poly_error_code, eta_t_isen);
//
//		if( poly_error_code != 0 )
//		{
//			error_code = poly_error_code;
//			return;
//		}
//	}
//	else
//		eta_t_isen = ms_des_par.m_eta_t;
//
//	// Determine the outlet state and specific work for the main compressor and turbine.
//	int comp_error_code = 0;
//	double w_mc = std::numeric_limits<double>::quiet_NaN();
//	// Main compressor
//	calculate_turbomachinery_outlet_1(m_temp_last[1 - cpp_offset], m_pres_last[1 - cpp_offset], m_pres_last[2 - cpp_offset], eta_mc_isen, true,
//		comp_error_code, m_enth_last[1 - cpp_offset], m_entr_last[1 - cpp_offset], m_dens_last[1 - cpp_offset], m_temp_last[2 - cpp_offset],
//		m_enth_last[2 - cpp_offset], m_entr_last[2 - cpp_offset], m_dens_last[2 - cpp_offset], w_mc);
//
//	if( comp_error_code != 0 )
//	{
//		error_code = comp_error_code;
//		return;
//	}
//
//	int turbine_error_code = 0;
//	double w_t = std::numeric_limits<double>::quiet_NaN();
//	// Turbine
//	calculate_turbomachinery_outlet_1(m_temp_last[6 - cpp_offset], m_pres_last[6 - cpp_offset], m_pres_last[7 - cpp_offset], eta_t_isen, false,
//		turbine_error_code, m_enth_last[6 - cpp_offset], m_entr_last[6 - cpp_offset], m_dens_last[6 - cpp_offset], m_temp_last[7 - cpp_offset],
//		m_enth_last[7 - cpp_offset], m_entr_last[7 - cpp_offset], m_dens_last[7 - cpp_offset], w_t);
//
//	if( turbine_error_code != 0 )
//	{
//		error_code = turbine_error_code;
//		return;
//	}
//
//	// Check that this cycle can produce power
//	double eta_rc_isen = std::numeric_limits<double>::quiet_NaN();
//	double w_rc = std::numeric_limits<double>::quiet_NaN();
//	if( ms_des_par.m_recomp_frac >= 1.E-12 )
//	{
//		if( ms_des_par.m_eta_rc < 0.0 )		// need to convert polytropic efficiency to isentropic efficiency
//		{
//			int rc_error_code = 0;
//
//			isen_eta_from_poly_eta(m_temp_last[2 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], fabs(ms_des_par.m_eta_rc),
//				true, rc_error_code, eta_rc_isen);
//
//			if( rc_error_code != 0 )
//			{
//				error_code = rc_error_code;
//				return;
//			}
//		}
//		else
//			eta_rc_isen = ms_des_par.m_eta_rc;
//
//		int rc_error_code = 0;
//		calculate_turbomachinery_outlet_1(m_temp_last[2 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], eta_rc_isen,
//			true, rc_error_code, w_rc);
//
//		if( rc_error_code != 0 )
//		{
//			error_code = rc_error_code;
//			return;
//		}
//	}
//	else
//		w_rc = 0.0;
//
//	if( w_mc + w_rc + w_t <= 0.0 )	// positive net power is impossible; return an error
//	{
//		error_code = 25;
//		return;
//	}
//
//	// Outer iteration loop: temp(8), checking against UA_HT
//	double T8_lower_bound, T8_upper_bound, last_HT_residual, last_T8_guess;
//	T8_lower_bound = T8_upper_bound = last_HT_residual = last_T8_guess = std::numeric_limits<double>::quiet_NaN();
//	if( ms_des_par.m_UA_HT < 1.0E-12 )		// no high-temp recuperator
//	{
//		T8_lower_bound = m_temp_last[7 - cpp_offset];		// no iteration necessary
//		T8_upper_bound = m_temp_last[7 - cpp_offset];		// no iteration necessary
//		m_temp_last[8 - cpp_offset] = m_temp_last[7 - cpp_offset];
//		UA_HT_calc = 0.0;
//		last_HT_residual = 0.0;
//		last_T8_guess = m_temp_last[7 - cpp_offset];
//	}
//	else
//	{
//		T8_lower_bound = m_temp_last[2 - cpp_offset];		// the absolute lower temp(8) could be
//		T8_upper_bound = m_temp_last[7 - cpp_offset];		// the absolutely highest temp(8) could be
//		m_temp_last[8 - cpp_offset] = (T8_lower_bound + T8_upper_bound)*0.5;		// bisect bounds for first guess
//		UA_HT_calc = -1.0;
//		last_HT_residual = ms_des_par.m_UA_HT;			// know a priori that with T8 = T7, UA_calc = 0 therefore residual is UA_HT - 0.0
//		last_T8_guess = m_temp_last[7 - cpp_offset];
//	}
//
//	int prop_error_code = 0;
//
//	double T9_lower_bound, T9_upper_bound, last_LT_residual, last_T9_guess;
//	T9_lower_bound = T9_upper_bound = last_LT_residual = last_T9_guess = std::numeric_limits<double>::quiet_NaN();
//
//	double min_DT_LT = std::numeric_limits<double>::quiet_NaN();
//	double min_DT_HT = std::numeric_limits<double>::quiet_NaN();
//
//	int T8_iter = -1;
//	for( T8_iter = 0; T8_iter < max_iter; T8_iter++ )
//	{
//		// Fully define state 8
//		prop_error_code = CO2_TP(m_temp_last[8 - cpp_offset], m_pres_last[8 - cpp_offset], &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		m_enth_last[8 - cpp_offset] = co2_props.enth;
//		m_entr_last[8 - cpp_offset] = co2_props.entr;
//		m_dens_last[8 - cpp_offset] = co2_props.dens;
//
//		// Inner iteration loop: temp(9), checking against UA_LT
//		if( ms_des_par.m_UA_LT < 1.0E-12 )	// no low-temperature recuperator
//		{
//			T9_lower_bound = m_temp_last[8 - cpp_offset];		// no iteration necessary
//			T9_upper_bound = m_temp_last[8 - cpp_offset];		// no iteration necessary
//			m_temp_last[9 - cpp_offset] = m_temp_last[8 - cpp_offset];
//			UA_LT_calc = 0.0;
//			last_LT_residual = 0.0;
//			last_T9_guess = m_temp_last[8 - cpp_offset];
//		}
//		else
//		{
//			T9_lower_bound = m_temp_last[2 - cpp_offset];		// the absolute lowest temp(9) could be
//			T9_upper_bound = m_temp_last[8 - cpp_offset];		// the absolute highest temp(9) could be
//			m_temp_last[9 - cpp_offset] = (T9_lower_bound + T9_upper_bound)*0.5;	// biset bounds for first guess
//			UA_LT_calc = -1.0;
//			last_LT_residual = ms_des_par.m_UA_LT;			// know a priori that with T9 = T8, UA_calc = 0 therefore residual is UA_LT - 0
//			last_T9_guess = m_temp_last[8 - cpp_offset];
//		}
//
//		int T9_iter = -1;
//		for( T9_iter = 0; T9_iter < max_iter; T9_iter++ )
//		{
//			// Determine the outlet state of the recompressing compressor and its specific work
//			if( ms_des_par.m_recomp_frac >= 1.E-12 )
//			{
//				if( ms_des_par.m_eta_rc < 0.0 )		// recalculate isentropic efficiency of recompressing compressor (because T9 changes)
//				{
//					int rc_error_code = 0;
//					isen_eta_from_poly_eta(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], fabs(ms_des_par.m_eta_rc), true,
//						rc_error_code, eta_rc_isen);
//
//					if( rc_error_code != 0 )
//					{
//						error_code = rc_error_code;
//						return;
//					}
//				}
//				else
//				{
//					eta_rc_isen = ms_des_par.m_eta_rc;
//				}
//
//				int rc_error_code = 0;
//				calculate_turbomachinery_outlet_1(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], eta_rc_isen, true, rc_error_code,
//					m_enth_last[9 - cpp_offset], m_entr_last[9 - cpp_offset], m_dens_last[9 - cpp_offset], m_temp_last[10 - cpp_offset], m_enth_last[10 - cpp_offset], m_entr_last[10 - cpp_offset],
//					m_dens_last[10 - cpp_offset], w_rc);
//
//				if( rc_error_code != 0 )
//				{
//					error_code = rc_error_code;
//					return;
//				}
//			}
//			else
//			{
//				w_rc = 0.0;		// the recompressing compressor does not exist
//				prop_error_code = CO2_TP(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], &co2_props);
//				if( prop_error_code != 0 )		// fully define state 9
//				{
//					error_code = prop_error_code;
//					return;
//				}
//				m_enth_last[9 - cpp_offset] = co2_props.enth;
//				m_entr_last[9 - cpp_offset] = co2_props.entr;
//				m_dens_last[9 - cpp_offset] = co2_props.dens;
//				m_temp_last[10 - cpp_offset] = m_temp_last[9 - cpp_offset];		// assume state 10 is the same as state 9
//				m_enth_last[10 - cpp_offset] = m_enth_last[9 - cpp_offset];
//				m_entr_last[10 - cpp_offset] = m_entr_last[9 - cpp_offset];
//				m_dens_last[10 - cpp_offset] = m_dens_last[9 - cpp_offset];
//			}
//
//			// Knowing the specific work of the recompressor, the required mass flow rate can be calculated
//			m_dot_t = ms_des_par.m_W_dot_net / (w_mc*(1.0 - ms_des_par.m_recomp_frac) + w_rc*ms_des_par.m_recomp_frac + w_t);	// Required mass flow rate through turbine
//
//			if( m_dot_t < 0.0 )		// positive power output is not possible with these inputs
//			{
//				error_code = 29;
//				return;
//			}
//			m_dot_rc = m_dot_t * ms_des_par.m_recomp_frac;		// apply definition of recompression fraction
//			m_dot_mc = m_dot_t - m_dot_rc;						// mass balance
//
//			// Calculate the UA value of the low-temperature recuperator
//			if( ms_des_par.m_UA_LT < 1.0E-12 )		// no low-temp recuperator (this check is necessary to prevent pressure drops with UA=0 from causing problems)
//				Q_dot_LT = 0.0;
//			else
//				Q_dot_LT = m_dot_t * (m_enth_last[8 - cpp_offset] - m_enth_last[9 - cpp_offset]);
//
//			int hx_error_code = 0;
//			min_DT_LT = std::numeric_limits<double>::quiet_NaN();
//			calculate_hxr_UA_1(ms_des_par.m_N_sub_hxrs, Q_dot_LT, m_dot_mc, m_dot_t, m_temp_last[2 - cpp_offset], m_temp_last[8 - cpp_offset],
//				m_pres_last[2 - cpp_offset], m_pres_last[3 - cpp_offset], m_pres_last[8 - cpp_offset], m_pres_last[9 - cpp_offset],
//				hx_error_code, UA_LT_calc, min_DT_LT);
//
//			if( hx_error_code != 0 )
//			{
//				if( hx_error_code == 11 )		// second-law violation in hxr, therefore temp(9) is too low
//				{
//					T9_lower_bound = m_temp_last[9 - cpp_offset];
//					m_temp_last[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);		// bisect bounds for next guess
//					continue;
//				}
//				else
//				{
//					error_code = hx_error_code;
//					return;
//				}
//			}
//
//			// Check for convergence and adjust T9 appropriately
//			double UA_LT_residual = ms_des_par.m_UA_LT - UA_LT_calc;
//
//			if( fabs(UA_LT_residual) < 1.0E-12 )		// catches no LT case
//				break;
//
//			double secant_guess = m_temp_last[9 - cpp_offset] - UA_LT_residual*(last_T9_guess - m_temp_last[9 - cpp_offset]) / (last_LT_residual - UA_LT_residual);	// next guess predicted using secant method
//
//			if( UA_LT_residual < 0.0 )		// UA_LT_calc is too big, temp(9) needs to be higher
//			{
//				if( fabs(UA_LT_residual) / ms_des_par.m_UA_LT < ms_des_par.m_tol )
//					break;
//
//				T9_lower_bound = m_temp_last[9 - cpp_offset];
//			}
//			else		// UA_LT_calc is too small, temp(9) needs to be lower
//			{
//				if( UA_LT_residual / ms_des_par.m_UA_LT < ms_des_par.m_tol )	// UA_LT converged
//					break;
//
//				if( min_DT_LT < temperature_tolerance )		// UA_calc is still too low but there isn't anywhere to go so it's ok (catches huge UA values)
//					break;
//
//				T9_upper_bound = m_temp_last[9 - cpp_offset];
//			}
//
//			last_LT_residual = UA_LT_residual;			// reset lsat stored residual value
//			last_T9_guess = m_temp_last[9 - cpp_offset];	// reset last stored guess value
//
//			// Check if the secant method overshoots and fall back to bisection if it does
//			if( secant_guess <= T9_lower_bound || secant_guess >= T9_upper_bound || secant_guess != secant_guess )	// secant method overshot (or is NaN), use bisection
//				m_temp_last[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);
//			else
//				m_temp_last[9 - cpp_offset] = secant_guess;
//
//		}	// End T9 iteration
//
//		// Check that T9_loop converged
//		if( T9_iter >= max_iter )
//		{
//			error_code = 31;
//			return;
//		}
//
//		// State 3 can now be fully defined
//		m_enth_last[3 - cpp_offset] = m_enth_last[2 - cpp_offset] + Q_dot_LT / m_dot_mc;		// Energy balalnce on cold stream of low-temp recuperator
//		prop_error_code = CO2_PH(m_pres_last[3 - cpp_offset], m_enth_last[3 - cpp_offset], &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		m_temp_last[3 - cpp_offset] = co2_props.temp;
//		m_entr_last[3 - cpp_offset] = co2_props.entr;
//		m_dens_last[3 - cpp_offset] = co2_props.dens;
//
//		// Go through the mixing valve
//		if( ms_des_par.m_recomp_frac >= 1.E-12 )
//		{
//			m_enth_last[4 - cpp_offset] = (1.0 - ms_des_par.m_recomp_frac)*m_enth_last[3 - cpp_offset] + ms_des_par.m_recomp_frac*m_enth_last[10 - cpp_offset];	// conservation of energy (both sides divided by m_dot_t)
//			prop_error_code = CO2_PH(m_pres_last[4 - cpp_offset], m_enth_last[4 - cpp_offset], &co2_props);
//			if( prop_error_code != 0 )
//			{
//				error_code = prop_error_code;
//				return;
//			}
//			m_temp_last[4 - cpp_offset] = co2_props.temp;
//			m_entr_last[4 - cpp_offset] = co2_props.entr;
//			m_dens_last[4 - cpp_offset] = co2_props.dens;
//		}
//		else		// no mixing valve, therefore state 4 is equal to state 3
//		{
//			m_temp_last[4 - cpp_offset] = m_temp_last[3 - cpp_offset];
//			m_enth_last[4 - cpp_offset] = m_enth_last[3 - cpp_offset];
//			m_entr_last[4 - cpp_offset] = m_entr_last[3 - cpp_offset];
//			m_dens_last[4 - cpp_offset] = m_dens_last[3 - cpp_offset];
//		}
//
//		// Check for a second law violation at the outlet of the high-temp recuperator
//		if( m_temp_last[4 - cpp_offset] >= m_temp_last[8 - cpp_offset] )		// temp(8) is not valid and it must be increased
//		{
//			T8_lower_bound = m_temp_last[8 - cpp_offset];
//			m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
//			continue;
//		}
//
//		// Calculate the UA value of the high-temp recuperator
//		if( ms_des_par.m_UA_HT < 1.E-12 )			// no high-temp recuperator
//			Q_dot_HT = 0.0;
//		else
//			Q_dot_HT = m_dot_t * (m_enth_last[7 - cpp_offset] - m_enth_last[8 - cpp_offset]);
//
//		int HT_error_code = 0;
//		min_DT_HT = std::numeric_limits<double>::quiet_NaN();
//
//		calculate_hxr_UA_1(ms_des_par.m_N_sub_hxrs, Q_dot_HT, m_dot_t, m_dot_t, m_temp_last[4 - cpp_offset], m_temp_last[7 - cpp_offset], m_pres_last[4 - cpp_offset],
//			m_pres_last[5 - cpp_offset], m_pres_last[7 - cpp_offset], m_pres_last[8 - cpp_offset], HT_error_code, UA_HT_calc, min_DT_HT);
//
//		if( HT_error_code != 0 )
//		{
//			if( HT_error_code == 11 )			// second-law violation in hxr, therefore temp(8) is too low
//			{
//				T8_lower_bound = m_temp_last[8 - cpp_offset];
//				m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);	// bisect bounds for next guess
//				continue;
//			}
//			else
//			{
//				error_code = HT_error_code;
//				return;
//			}
//		}
//
//		// Check for convergence and adjust T8 appropriately
//		double UA_HT_residual = ms_des_par.m_UA_HT - UA_HT_calc;
//
//		if( fabs(UA_HT_residual) < 1.0E-12 )		// catches no HT case
//			break;
//
//		double secant_guess = m_temp_last[8 - cpp_offset] - UA_HT_residual*(last_T8_guess - m_temp_last[8 - cpp_offset]) / (last_HT_residual - UA_HT_residual);		// Next guess predicted using secant method
//
//		if( UA_HT_residual < 0.0 )	// UA_HT_calc is too big, temp(8) needs to be higher
//		{
//			if( fabs(UA_HT_residual) / ms_des_par.m_UA_HT < ms_des_par.m_tol )
//				break;
//			T8_lower_bound = m_temp_last[8 - cpp_offset];
//		}
//		else						// UA_HT_calc is too small, temp(8) needs to be lower
//		{
//			if( UA_HT_residual / ms_des_par.m_UA_HT < ms_des_par.m_tol )		// UA_HT converged
//				break;
//			if( min_DT_HT < temperature_tolerance )								// UA_calc is still too low, but there isn't anywhere to go so it's okay
//				break;
//			T8_upper_bound = m_temp_last[8 - cpp_offset];
//		}
//		last_HT_residual = UA_HT_residual;				// reset last stored residual value
//		last_T8_guess = m_temp_last[8 - cpp_offset];		// reset last stored guess value
//
//		// Check if the secant method overshoots and fall back to bisection if it does
//		if( secant_guess <= T8_lower_bound || secant_guess >= T8_upper_bound )		// secant method overshot, use bisection
//			m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
//		else
//			m_temp_last[8 - cpp_offset] = secant_guess;
//
//	}	// End T8 iteration
//
//	// Check that T8_loop converged
//	if( T8_iter >= max_iter )
//	{
//		error_code = 35;
//		return;
//	}
//
//	// State 5 can now be fully defined
//	m_enth_last[5 - cpp_offset] = m_enth_last[4 - cpp_offset] + Q_dot_HT / m_dot_t;						// Energy balance on cold stream of high-temp recuperator
//	prop_error_code = CO2_PH(m_pres_last[5 - cpp_offset], m_enth_last[5 - cpp_offset], &co2_props);
//	if( prop_error_code != 0 )
//	{
//		error_code = prop_error_code;
//		return;
//	}
//	m_temp_last[5 - cpp_offset] = co2_props.temp;
//	m_entr_last[5 - cpp_offset] = co2_props.entr;
//	m_dens_last[5 - cpp_offset] = co2_props.dens;
//
//	// Calculate performance metrics for low-temperature recuperator
//	C_HeatExchanger::S_design_parameters LT_des_par;
//	double C_dot_hot = m_dot_t*(m_enth_last[8 - cpp_offset] - m_enth_last[9 - cpp_offset]) / (m_temp_last[8 - cpp_offset] - m_temp_last[9 - cpp_offset]);		// LT recuperator hot stream capacitance rate
//	double C_dot_cold = m_dot_mc*(m_enth_last[3 - cpp_offset] - m_enth_last[2 - cpp_offset]) / (m_temp_last[3 - cpp_offset] - m_temp_last[2 - cpp_offset]);	// LT recuperator cold stream capacitance rate
//	double C_dot_min = min(C_dot_hot, C_dot_cold);
//	double Q_dot_max = C_dot_min*(m_temp_last[8 - cpp_offset] - m_temp_last[2 - cpp_offset]);
//	double hx_eff = Q_dot_LT / Q_dot_max;				// Definition of effectiveness
//	LT_des_par.m_DP_design[0] = m_pres_last[2 - cpp_offset] - m_pres_last[3 - cpp_offset];
//	LT_des_par.m_DP_design[1] = m_pres_last[8 - cpp_offset] - m_pres_last[9 - cpp_offset];
//	LT_des_par.m_eff_design = hx_eff;
//	LT_des_par.m_min_DT_design = min_DT_LT;
//	LT_des_par.m_m_dot_design[0] = m_dot_mc;
//	LT_des_par.m_m_dot_design[1] = m_dot_t;
//	LT_des_par.m_N_sub = ms_des_par.m_N_sub_hxrs;
//	LT_des_par.m_Q_dot_design = Q_dot_LT;
//	LT_des_par.m_UA_design = UA_LT_calc;
//	m_LT.initialize(LT_des_par);
//
//	// Calculate performance metrics for high-temperature recuperator
//	C_HeatExchanger::S_design_parameters HT_des_par;
//	C_dot_hot = m_dot_t*(m_enth_last[7 - cpp_offset] - m_enth_last[8 - cpp_offset]) / (m_temp_last[7 - cpp_offset] - m_temp_last[8 - cpp_offset]);			// HT recuperator hot stream capacitance rate
//	C_dot_cold = m_dot_t*(m_enth_last[5 - cpp_offset] - m_enth_last[4 - cpp_offset]) / (m_temp_last[5 - cpp_offset] - m_temp_last[4 - cpp_offset]);			// HT recuperator cold stream capacitance rate
//	C_dot_min = min(C_dot_hot, C_dot_cold);
//	Q_dot_max = C_dot_min*(m_temp_last[7 - cpp_offset] - m_temp_last[4 - cpp_offset]);
//	hx_eff = Q_dot_HT / Q_dot_max;						// Definition of effectiveness
//	HT_des_par.m_DP_design[0] = m_pres_last[4 - cpp_offset] - m_pres_last[5 - cpp_offset];
//	HT_des_par.m_DP_design[1] = m_pres_last[7 - cpp_offset] - m_pres_last[8 - cpp_offset];
//	HT_des_par.m_eff_design = hx_eff;
//	HT_des_par.m_min_DT_design = min_DT_HT;
//	HT_des_par.m_m_dot_design[0] = m_dot_t;
//	HT_des_par.m_m_dot_design[1] = m_dot_t;
//	HT_des_par.m_N_sub = ms_des_par.m_N_sub_hxrs;
//	HT_des_par.m_Q_dot_design = Q_dot_HT;
//	HT_des_par.m_UA_design = UA_HT_calc;
//	m_HT.initialize(HT_des_par);
//
//	// Set relevant values for other heat exchangers
//	C_HeatExchanger::S_design_parameters PHX_des_par;
//	PHX_des_par.m_DP_design[0] = m_pres_last[5 - cpp_offset] - m_pres_last[6 - cpp_offset];
//	PHX_des_par.m_DP_design[1] = 0.0;
//	PHX_des_par.m_m_dot_design[0] = m_dot_t;
//	PHX_des_par.m_m_dot_design[1] = 0.0;
//	PHX_des_par.m_Q_dot_design = m_dot_t*(m_enth_last[6 - cpp_offset] - m_enth_last[5 - cpp_offset]);
//	m_PHX.initialize(PHX_des_par);
//
//	C_HeatExchanger::S_design_parameters PC_des_par;
//	PC_des_par.m_DP_design[0] = 0.0;
//	PC_des_par.m_DP_design[1] = m_pres_last[9 - cpp_offset] - m_pres_last[1 - cpp_offset];
//	PC_des_par.m_m_dot_design[0] = 0.0;
//	PC_des_par.m_m_dot_design[1] = m_dot_mc;
//	PC_des_par.m_Q_dot_design = m_dot_mc*(m_enth_last[9 - cpp_offset] - m_enth_last[1 - cpp_offset]);
//	m_PC.initialize(PC_des_par);
//
//	// Calculate/set cycle performance metrics
//	m_W_dot_mc = w_mc*m_dot_mc;
//	m_W_dot_rc = w_rc*m_dot_rc;
//	m_W_dot_mc_bypass = w_mc*(m_dot_t);
//	m_W_dot_net_last = w_mc*m_dot_mc + w_rc*m_dot_rc + w_t*m_dot_t;
//
//	m_eta_thermal_last = m_W_dot_net_last / (PHX_des_par.m_Q_dot_design);
//
//	m_Q_dot_PHX = PHX_des_par.m_Q_dot_design;		//[kW]
//	m_Q_dot_bypass = m_dot_rc*(m_enth_last[3-cpp_offset] - m_enth_last[2-cpp_offset]);		//[kW]
//	m_eta_bypass = ((m_enth_last[3-cpp_offset] - m_enth_last[9-cpp_offset]) - (m_enth_last[2-cpp_offset] - m_enth_last[1-cpp_offset])) /
//						(m_enth_last[3-cpp_offset] - m_enth_last[2-cpp_offset]);
//
//
//	//double Q_dot_geo = m_dot_rc * (m_enth_last[10 - cpp_offset] - m_enth_last[2 - cpp_offset]);
//
//	//double E_bal = (PHX_des_par.m_Q_dot_design + Q_dot_heat_shield) - (m_W_dot_net_last + PC_des_par.m_Q_dot_design / m_dot_mc*(m_dot_mc + m_dot_rc));
//
//	double T_limit = 150.0+273.15;
//
//	double over_T_limit = fmax(0.0, m_temp_last[10-cpp_offset] - T_limit);
//
//	m_eta_thermal_last = m_eta_thermal_last*exp(-over_T_limit);
//
//	m_m_dot_mc = m_dot_mc;
//	m_m_dot_rc = m_dot_rc;
//	m_m_dot_t = m_dot_t;
//}

//void C_RecompCycle_RCMCI_with_ReHeating::design_core_HTR_hs(int & error_code)
//{
//	CO2_state co2_props;
//
//	double Q_hs_frac_target = 10.0 / 65.0;
//
//	double f_bypass = 0.25;
//
//	double f_bypass_min = 0.01;
//	double f_bypass_max = 0.8;
//
//	double f_bypass_low = f_bypass_min;
//	double f_bypass_high = f_bypass_max;
//
//	int iter_f_bypass = 0;
//
//	do
//	{		
//		iter_f_bypass++;
//
//		int max_iter = 500;
//		double temperature_tolerance = 1.E-6;		// Temp differences below this are considered zero
//
//		int cpp_offset = 1;
//
//		// Initialize a few variables
//		double m_dot_t, m_dot_mc, m_dot_rc, Q_dot_LT, Q_dot_HT, UA_LT_calc, UA_HT_calc;
//		m_dot_t = m_dot_mc = m_dot_rc = Q_dot_LT = Q_dot_HT = UA_LT_calc = UA_HT_calc = 0.0;
//
//		m_temp_last[1 - cpp_offset] = ms_des_par.m_T_mc_in;
//		m_pres_last[1 - cpp_offset] = ms_des_par.m_P_mc_in;
//		m_pres_last[2 - cpp_offset] = ms_des_par.m_P_mc_out;
//		m_temp_last[6 - cpp_offset] = ms_des_par.m_T_t_in;
//
//		// Apply pressure drops to heat exchangers, fully defining the pressures at all states
//		if( ms_des_par.m_DP_LT[1 - cpp_offset] < 0.0 )
//			m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset] - m_pres_last[2 - cpp_offset] * fabs(ms_des_par.m_DP_LT[1 - cpp_offset]);	// relative pressure drop specified for LT recuperator (cold stream)
//		else
//			m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset] - ms_des_par.m_DP_LT[1 - cpp_offset];									// absolute pressure drop specified for LT recuperator (cold stream)
//
//		if( ms_des_par.m_UA_LT < 1.0E-12 )
//			m_pres_last[3 - cpp_offset] = m_pres_last[2 - cpp_offset];		// If there is no LT recuperator, there is no pressure drop
//
//		m_pres_last[4 - cpp_offset] = m_pres_last[3 - cpp_offset];			// Assume no pressure drop in mixing valve
//		m_pres_last[10 - cpp_offset] = m_pres_last[3 - cpp_offset];			// Assume no pressure drop in mixing valve
//
//		if( ms_des_par.m_DP_HT[1 - cpp_offset] < 0.0 )
//			m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset] - m_pres_last[4 - cpp_offset] * fabs(ms_des_par.m_DP_HT[1 - cpp_offset]);	// relative pressure drop specified for HT recuperator (cold stream)
//		else
//			m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset] - ms_des_par.m_DP_HT[1 - cpp_offset];									// absolute pressure drop specified for HT recuperator (cold stream)
//
//		if( ms_des_par.m_UA_HT < 1.0E-12 )
//			m_pres_last[5 - cpp_offset] = m_pres_last[4 - cpp_offset];		// If there is no HT recuperator, there is no pressure drop
//
//		if( ms_des_par.m_DP_PHX[1 - cpp_offset] < 0.0 )
//			m_pres_last[6 - cpp_offset] = m_pres_last[5 - cpp_offset] - m_pres_last[5 - cpp_offset] * fabs(ms_des_par.m_DP_PHX[1 - cpp_offset]);	// relative pressure drop specified for PHX
//		else
//			m_pres_last[6 - cpp_offset] = m_pres_last[5 - cpp_offset] - ms_des_par.m_DP_PHX[1 - cpp_offset];									// absolute pressure drop specified for PHX
//
//		if( ms_des_par.m_DP_PC[2 - cpp_offset] < 0.0 )
//			m_pres_last[9 - cpp_offset] = m_pres_last[1 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_PC[2 - cpp_offset]));	// relative pressure drop specified for precooler: P1=P9-P9*rel_DP => P1=P9*(1-rel_DP)
//		else
//			m_pres_last[9 - cpp_offset] = m_pres_last[1 - cpp_offset] + ms_des_par.m_DP_PC[2 - cpp_offset];
//
//		if( ms_des_par.m_DP_LT[2 - cpp_offset] < 0.0 )
//			m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_LT[2 - cpp_offset]));	// relative pressure drop specified for LT recuperator (hot stream)
//		else
//			m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset] + ms_des_par.m_DP_LT[2 - cpp_offset];				// absolute pressure drop specified for LT recuperator (hot stream)
//
//		if( ms_des_par.m_UA_LT < 1.0E-12 )
//			m_pres_last[8 - cpp_offset] = m_pres_last[9 - cpp_offset];		// if there is no LT recuperator, there is no pressure drop
//
//		if( ms_des_par.m_DP_HT[2 - cpp_offset] < 0.0 )
//			m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset] / (1.0 - fabs(ms_des_par.m_DP_HT[2 - cpp_offset]));	// relative pressure drop specified for HT recuperator (hot stream)
//		else
//			m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset] + ms_des_par.m_DP_HT[2 - cpp_offset];				// absolute pressure drop specified for HT recuperator (hot stream)
//
//		if( ms_des_par.m_UA_HT < 1.0E-12 )
//			m_pres_last[7 - cpp_offset] = m_pres_last[8 - cpp_offset];		// if there is no HT recuperator, there is no pressure drop
//
//		// Determine equivalent isentropic efficiencies for main compressor and turbine, if necessary.
//		double eta_mc_isen = std::numeric_limits<double>::quiet_NaN();
//		double eta_t_isen = std::numeric_limits<double>::quiet_NaN();
//		if( ms_des_par.m_eta_mc < 0.0 )
//		{
//			int poly_error_code = 0;
//
//			isen_eta_from_poly_eta(m_temp_last[1 - cpp_offset], m_pres_last[1 - cpp_offset], m_pres_last[2 - cpp_offset], fabs(ms_des_par.m_eta_mc),
//				true, poly_error_code, eta_mc_isen);
//
//			if( poly_error_code != 0 )
//			{
//				error_code = poly_error_code;
//				return;
//			}
//		}
//		else
//			eta_mc_isen = ms_des_par.m_eta_mc;
//
//		if( ms_des_par.m_eta_t < 0.0 )
//		{
//			int poly_error_code = 0;
//
//			isen_eta_from_poly_eta(m_temp_last[6 - cpp_offset], m_pres_last[6 - cpp_offset], m_pres_last[7 - cpp_offset], fabs(ms_des_par.m_eta_t),
//				false, poly_error_code, eta_t_isen);
//
//			if( poly_error_code != 0 )
//			{
//				error_code = poly_error_code;
//				return;
//			}
//		}
//		else
//			eta_t_isen = ms_des_par.m_eta_t;
//
//		// Determine the outlet state and specific work for the main compressor and turbine.
//		int comp_error_code = 0;
//		double w_mc = std::numeric_limits<double>::quiet_NaN();
//		// Main compressor
//		calculate_turbomachinery_outlet_1(m_temp_last[1 - cpp_offset], m_pres_last[1 - cpp_offset], m_pres_last[2 - cpp_offset], eta_mc_isen, true,
//			comp_error_code, m_enth_last[1 - cpp_offset], m_entr_last[1 - cpp_offset], m_dens_last[1 - cpp_offset], m_temp_last[2 - cpp_offset],
//			m_enth_last[2 - cpp_offset], m_entr_last[2 - cpp_offset], m_dens_last[2 - cpp_offset], w_mc);
//
//		if( comp_error_code != 0 )
//		{
//			error_code = comp_error_code;
//			return;
//		}
//
//		int turbine_error_code = 0;
//		double w_t = std::numeric_limits<double>::quiet_NaN();
//		// Turbine
//		calculate_turbomachinery_outlet_1(m_temp_last[6 - cpp_offset], m_pres_last[6 - cpp_offset], m_pres_last[7 - cpp_offset], eta_t_isen, false,
//			turbine_error_code, m_enth_last[6 - cpp_offset], m_entr_last[6 - cpp_offset], m_dens_last[6 - cpp_offset], m_temp_last[7 - cpp_offset],
//			m_enth_last[7 - cpp_offset], m_entr_last[7 - cpp_offset], m_dens_last[7 - cpp_offset], w_t);
//
//		if( turbine_error_code != 0 )
//		{
//			error_code = turbine_error_code;
//			return;
//		}
//
//		// Check that this cycle can produce power
//		double eta_rc_isen = std::numeric_limits<double>::quiet_NaN();
//		double w_rc = std::numeric_limits<double>::quiet_NaN();
//		if( ms_des_par.m_recomp_frac >= 1.E-12 )
//		{
//			if( ms_des_par.m_eta_rc < 0.0 )		// need to convert polytropic efficiency to isentropic efficiency
//			{
//				int rc_error_code = 0;
//
//				isen_eta_from_poly_eta(m_temp_last[2 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], fabs(ms_des_par.m_eta_rc),
//					true, rc_error_code, eta_rc_isen);
//
//				if( rc_error_code != 0 )
//				{
//					error_code = rc_error_code;
//					return;
//				}
//			}
//			else
//				eta_rc_isen = ms_des_par.m_eta_rc;
//
//			int rc_error_code = 0;
//			calculate_turbomachinery_outlet_1(m_temp_last[2 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], eta_rc_isen,
//				true, rc_error_code, w_rc);
//
//			if( rc_error_code != 0 )
//			{
//				error_code = rc_error_code;
//				return;
//			}
//		}
//		else
//			w_rc = 0.0;
//
//		if( w_mc + w_rc + w_t <= 0.0 )	// positive net power is impossible; return an error
//		{
//			error_code = 25;
//			return;
//		}
//
//		// Outer iteration loop: temp(8), checking against UA_HT
//		double T8_lower_bound, T8_upper_bound, last_HT_residual, last_T8_guess;
//		T8_lower_bound = T8_upper_bound = last_HT_residual = last_T8_guess = std::numeric_limits<double>::quiet_NaN();
//		if( ms_des_par.m_UA_HT < 1.0E-12 )		// no high-temp recuperator
//		{
//			T8_lower_bound = m_temp_last[7 - cpp_offset];		// no iteration necessary
//			T8_upper_bound = m_temp_last[7 - cpp_offset];		// no iteration necessary
//			m_temp_last[8 - cpp_offset] = m_temp_last[7 - cpp_offset];
//			UA_HT_calc = 0.0;
//			last_HT_residual = 0.0;
//			last_T8_guess = m_temp_last[7 - cpp_offset];
//		}
//		else
//		{
//			T8_lower_bound = m_temp_last[2 - cpp_offset];		// the absolute lower temp(8) could be
//			T8_upper_bound = m_temp_last[7 - cpp_offset];		// the absolutely highest temp(8) could be
//			m_temp_last[8 - cpp_offset] = (T8_lower_bound + T8_upper_bound)*0.5;		// bisect bounds for first guess
//			UA_HT_calc = -1.0;
//			last_HT_residual = ms_des_par.m_UA_HT;			// know a priori that with T8 = T7, UA_calc = 0 therefore residual is UA_HT - 0.0
//			last_T8_guess = m_temp_last[7 - cpp_offset];
//		}
//
//		int prop_error_code = 0;
//
//		double T9_lower_bound, T9_upper_bound, last_LT_residual, last_T9_guess;
//		T9_lower_bound = T9_upper_bound = last_LT_residual = last_T9_guess = std::numeric_limits<double>::quiet_NaN();
//
//		double min_DT_LT = std::numeric_limits<double>::quiet_NaN();
//		double min_DT_HT = std::numeric_limits<double>::quiet_NaN();
//
//		int T8_iter = -1;
//		for( T8_iter = 0; T8_iter < max_iter; T8_iter++ )
//		{
//			// Fully define state 8
//			prop_error_code = CO2_TP(m_temp_last[8 - cpp_offset], m_pres_last[8 - cpp_offset], &co2_props);
//			if( prop_error_code != 0 )
//			{
//				error_code = prop_error_code;
//				return;
//			}
//			m_enth_last[8 - cpp_offset] = co2_props.enth;
//			m_entr_last[8 - cpp_offset] = co2_props.entr;
//			m_dens_last[8 - cpp_offset] = co2_props.dens;
//
//			// Inner iteration loop: temp(9), checking against UA_LT
//			if( ms_des_par.m_UA_LT < 1.0E-12 )	// no low-temperature recuperator
//			{
//				T9_lower_bound = m_temp_last[8 - cpp_offset];		// no iteration necessary
//				T9_upper_bound = m_temp_last[8 - cpp_offset];		// no iteration necessary
//				m_temp_last[9 - cpp_offset] = m_temp_last[8 - cpp_offset];
//				UA_LT_calc = 0.0;
//				last_LT_residual = 0.0;
//				last_T9_guess = m_temp_last[8 - cpp_offset];
//			}
//			else
//			{
//				T9_lower_bound = m_temp_last[2 - cpp_offset];		// the absolute lowest temp(9) could be
//				T9_upper_bound = m_temp_last[8 - cpp_offset];		// the absolute highest temp(9) could be
//				m_temp_last[9 - cpp_offset] = (T9_lower_bound + T9_upper_bound)*0.5;	// biset bounds for first guess
//				UA_LT_calc = -1.0;
//				last_LT_residual = ms_des_par.m_UA_LT;			// know a priori that with T9 = T8, UA_calc = 0 therefore residual is UA_LT - 0
//				last_T9_guess = m_temp_last[8 - cpp_offset];
//			}
//
//			int T9_iter = -1;
//			for( T9_iter = 0; T9_iter < max_iter; T9_iter++ )
//			{
//				// Determine the outlet state of the recompressing compressor and its specific work
//				if( ms_des_par.m_recomp_frac >= 1.E-12 )
//				{
//					if( ms_des_par.m_eta_rc < 0.0 )		// recalculate isentropic efficiency of recompressing compressor (because T9 changes)
//					{
//						int rc_error_code = 0;
//						isen_eta_from_poly_eta(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], fabs(ms_des_par.m_eta_rc), true,
//							rc_error_code, eta_rc_isen);
//
//						if( rc_error_code != 0 )
//						{
//							error_code = rc_error_code;
//							return;
//						}
//					}
//					else
//					{
//						eta_rc_isen = ms_des_par.m_eta_rc;
//					}
//
//					int rc_error_code = 0;
//					calculate_turbomachinery_outlet_1(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], m_pres_last[10 - cpp_offset], eta_rc_isen, true, rc_error_code,
//						m_enth_last[9 - cpp_offset], m_entr_last[9 - cpp_offset], m_dens_last[9 - cpp_offset], m_temp_last[10 - cpp_offset], m_enth_last[10 - cpp_offset], m_entr_last[10 - cpp_offset],
//						m_dens_last[10 - cpp_offset], w_rc);
//
//					if( rc_error_code != 0 )
//					{
//						error_code = rc_error_code;
//						return;
//					}
//				}
//				else
//				{
//					w_rc = 0.0;		// the recompressing compressor does not exist
//					prop_error_code = CO2_TP(m_temp_last[9 - cpp_offset], m_pres_last[9 - cpp_offset], &co2_props);
//					if( prop_error_code != 0 )		// fully define state 9
//					{
//						error_code = prop_error_code;
//						return;
//					}
//					m_enth_last[9 - cpp_offset] = co2_props.enth;
//					m_entr_last[9 - cpp_offset] = co2_props.entr;
//					m_dens_last[9 - cpp_offset] = co2_props.dens;
//					m_temp_last[10 - cpp_offset] = m_temp_last[9 - cpp_offset];		// assume state 10 is the same as state 9
//					m_enth_last[10 - cpp_offset] = m_enth_last[9 - cpp_offset];
//					m_entr_last[10 - cpp_offset] = m_entr_last[9 - cpp_offset];
//					m_dens_last[10 - cpp_offset] = m_dens_last[9 - cpp_offset];
//				}
//
//				// Knowing the specific work of the recompressor, the required mass flow rate can be calculated
//				m_dot_t = ms_des_par.m_W_dot_net / (w_mc*(1.0 - ms_des_par.m_recomp_frac) + w_rc*ms_des_par.m_recomp_frac + w_t);	// Required mass flow rate through turbine
//				if( m_dot_t < 0.0 )		// positive power output is not possible with these inputs
//				{
//					error_code = 29;
//					return;
//				}
//				m_dot_rc = m_dot_t * ms_des_par.m_recomp_frac;		// apply definition of recompression fraction
//				m_dot_mc = m_dot_t - m_dot_rc;						// mass balance
//
//				// Calculate the UA value of the low-temperature recuperator
//				if( ms_des_par.m_UA_LT < 1.0E-12 )		// no low-temp recuperator (this check is necessary to prevent pressure drops with UA=0 from causing problems)
//					Q_dot_LT = 0.0;
//				else
//					Q_dot_LT = m_dot_t * (m_enth_last[8 - cpp_offset] - m_enth_last[9 - cpp_offset]);
//
//				int hx_error_code = 0;
//				min_DT_LT = std::numeric_limits<double>::quiet_NaN();
//				calculate_hxr_UA_1(ms_des_par.m_N_sub_hxrs, Q_dot_LT, m_dot_mc, m_dot_t, m_temp_last[2 - cpp_offset], m_temp_last[8 - cpp_offset],
//					m_pres_last[2 - cpp_offset], m_pres_last[3 - cpp_offset], m_pres_last[8 - cpp_offset], m_pres_last[9 - cpp_offset],
//					hx_error_code, UA_LT_calc, min_DT_LT);
//
//				if( hx_error_code != 0 )
//				{
//					if( hx_error_code == 11 )		// second-law violation in hxr, therefore temp(9) is too low
//					{
//						T9_lower_bound = m_temp_last[9 - cpp_offset];
//						m_temp_last[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);		// bisect bounds for next guess
//						continue;
//					}
//					else
//					{
//						error_code = hx_error_code;
//						return;
//					}
//				}
//
//				// Check for convergence and adjust T9 appropriately
//				double UA_LT_residual = ms_des_par.m_UA_LT - UA_LT_calc;
//
//				if( fabs(UA_LT_residual) < 1.0E-12 )		// catches no LT case
//					break;
//
//				double secant_guess = m_temp_last[9 - cpp_offset] - UA_LT_residual*(last_T9_guess - m_temp_last[9 - cpp_offset]) / (last_LT_residual - UA_LT_residual);	// next guess predicted using secant method
//
//				if( UA_LT_residual < 0.0 )		// UA_LT_calc is too big, temp(9) needs to be higher
//				{
//					if( fabs(UA_LT_residual) / ms_des_par.m_UA_LT < ms_des_par.m_tol )
//						break;
//
//					T9_lower_bound = m_temp_last[9 - cpp_offset];
//				}
//				else		// UA_LT_calc is too small, temp(9) needs to be lower
//				{
//					if( UA_LT_residual / ms_des_par.m_UA_LT < ms_des_par.m_tol )	// UA_LT converged
//						break;
//
//					if( min_DT_LT < temperature_tolerance )		// UA_calc is still too low but there isn't anywhere to go so it's ok (catches huge UA values)
//						break;
//
//					T9_upper_bound = m_temp_last[9 - cpp_offset];
//				}
//
//				last_LT_residual = UA_LT_residual;			// reset lsat stored residual value
//				last_T9_guess = m_temp_last[9 - cpp_offset];	// reset last stored guess value
//
//				// Check if the secant method overshoots and fall back to bisection if it does
//				if( secant_guess <= T9_lower_bound || secant_guess >= T9_upper_bound || secant_guess != secant_guess )	// secant method overshot (or is NaN), use bisection
//					m_temp_last[9 - cpp_offset] = 0.5*(T9_lower_bound + T9_upper_bound);
//				else
//					m_temp_last[9 - cpp_offset] = secant_guess;
//
//			}	// End T9 iteration
//
//			// Check that T9_loop converged
//			if( T9_iter >= max_iter )
//			{
//				error_code = 31;
//				return;
//			}
//
//			// State 3 can now be fully defined
//			m_enth_last[3 - cpp_offset] = m_enth_last[2 - cpp_offset] + Q_dot_LT / m_dot_mc;		// Energy balalnce on cold stream of low-temp recuperator
//			prop_error_code = CO2_PH(m_pres_last[3 - cpp_offset], m_enth_last[3 - cpp_offset], &co2_props);
//			if( prop_error_code != 0 )
//			{
//				error_code = prop_error_code;
//				return;
//			}
//			m_temp_last[3 - cpp_offset] = co2_props.temp;
//			m_entr_last[3 - cpp_offset] = co2_props.entr;
//			m_dens_last[3 - cpp_offset] = co2_props.dens;
//
//			// Go through the mixing valve
//			if( ms_des_par.m_recomp_frac >= 1.E-12 )
//			{
//				m_enth_last[4 - cpp_offset] = (1.0 - ms_des_par.m_recomp_frac)*m_enth_last[3 - cpp_offset] + ms_des_par.m_recomp_frac*m_enth_last[10 - cpp_offset];	// conservation of energy (both sides divided by m_dot_t)
//				prop_error_code = CO2_PH(m_pres_last[4 - cpp_offset], m_enth_last[4 - cpp_offset], &co2_props);
//				if( prop_error_code != 0 )
//				{
//					error_code = prop_error_code;
//					return;
//				}
//				m_temp_last[4 - cpp_offset] = co2_props.temp;
//				m_entr_last[4 - cpp_offset] = co2_props.entr;
//				m_dens_last[4 - cpp_offset] = co2_props.dens;
//			}
//			else		// no mixing valve, therefore state 4 is equal to state 3
//			{
//				m_temp_last[4 - cpp_offset] = m_temp_last[3 - cpp_offset];
//				m_enth_last[4 - cpp_offset] = m_enth_last[3 - cpp_offset];
//				m_entr_last[4 - cpp_offset] = m_entr_last[3 - cpp_offset];
//				m_dens_last[4 - cpp_offset] = m_dens_last[3 - cpp_offset];
//			}
//
//			// Check for a second law violation at the outlet of the high-temp recuperator
//			if( m_temp_last[4 - cpp_offset] >= m_temp_last[8 - cpp_offset] )		// temp(8) is not valid and it must be increased
//			{
//				T8_lower_bound = m_temp_last[8 - cpp_offset];
//				m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
//				continue;
//			}
//
//			double m_dot_bypass = f_bypass*m_dot_t;
//			double m_dot_HTR_cold = (1.0 - f_bypass)*m_dot_t;
//
//			// Calculate the UA value of the high-temp recuperator
//			if( ms_des_par.m_UA_HT < 1.E-12 )			// no high-temp recuperator
//				Q_dot_HT = 0.0;
//			else
//				Q_dot_HT = m_dot_t * (m_enth_last[7 - cpp_offset] - m_enth_last[8 - cpp_offset]);
//
//
//			int HT_error_code = 0;
//			min_DT_HT = std::numeric_limits<double>::quiet_NaN();
//
//			calculate_hxr_UA_1(ms_des_par.m_N_sub_hxrs, Q_dot_HT, m_dot_HTR_cold, m_dot_t, m_temp_last[4 - cpp_offset], m_temp_last[7 - cpp_offset], m_pres_last[4 - cpp_offset],
//				m_pres_last[5 - cpp_offset], m_pres_last[7 - cpp_offset], m_pres_last[8 - cpp_offset], HT_error_code, UA_HT_calc, min_DT_HT);
//
//			if( HT_error_code != 0 )
//			{
//				if( HT_error_code == 11 )			// second-law violation in hxr, therefore temp(8) is too low
//				{
//					T8_lower_bound = m_temp_last[8 - cpp_offset];
//					m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);	// bisect bounds for next guess
//					continue;
//				}
//				else
//				{
//					error_code = HT_error_code;
//					return;
//				}
//			}
//
//			// Check for convergence and adjust T8 appropriately
//			double UA_HT_residual = ms_des_par.m_UA_HT - UA_HT_calc;
//
//			if( fabs(UA_HT_residual) < 1.0E-12 )		// catches no HT case
//				break;
//
//			double secant_guess = m_temp_last[8 - cpp_offset] - UA_HT_residual*(last_T8_guess - m_temp_last[8 - cpp_offset]) / (last_HT_residual - UA_HT_residual);		// Next guess predicted using secant method
//
//			if( UA_HT_residual < 0.0 )	// UA_HT_calc is too big, temp(8) needs to be higher
//			{
//				if( fabs(UA_HT_residual) / ms_des_par.m_UA_HT < ms_des_par.m_tol )
//					break;
//				T8_lower_bound = m_temp_last[8 - cpp_offset];
//			}
//			else						// UA_HT_calc is too small, temp(8) needs to be lower
//			{
//				if( UA_HT_residual / ms_des_par.m_UA_HT < ms_des_par.m_tol )		// UA_HT converged
//					break;
//				if( min_DT_HT < temperature_tolerance )								// UA_calc is still too low, but there isn't anywhere to go so it's okay
//					break;
//				T8_upper_bound = m_temp_last[8 - cpp_offset];
//			}
//			last_HT_residual = UA_HT_residual;				// reset last stored residual value
//			last_T8_guess = m_temp_last[8 - cpp_offset];		// reset last stored guess value
//
//			// Check if the secant method overshoots and fall back to bisection if it does
//			if( secant_guess <= T8_lower_bound || secant_guess >= T8_upper_bound )		// secant method overshot, use bisection
//				m_temp_last[8 - cpp_offset] = 0.5*(T8_lower_bound + T8_upper_bound);
//			else
//				m_temp_last[8 - cpp_offset] = secant_guess;
//
//		}	// End T8 iteration
//
//		// Check that T8_loop converged
//		if( T8_iter >= max_iter )
//		{
//			error_code = 35;
//			return;
//		}
//
//		// State 5 can now be fully defined
//		m_enth_last[5 - cpp_offset] = m_enth_last[4 - cpp_offset] + Q_dot_HT / ((1.0 - f_bypass)*m_dot_t);						// Energy balance on cold stream of high-temp recuperator
//		prop_error_code = CO2_PH(m_pres_last[5 - cpp_offset], m_enth_last[5 - cpp_offset], &co2_props);
//		if( prop_error_code != 0 )
//		{
//			error_code = prop_error_code;
//			return;
//		}
//		m_temp_last[5 - cpp_offset] = co2_props.temp;
//		m_entr_last[5 - cpp_offset] = co2_props.entr;
//		m_dens_last[5 - cpp_offset] = co2_props.dens;
//
//		// Calculate performance metrics for low-temperature recuperator
//		C_HeatExchanger::S_design_parameters LT_des_par;
//		double C_dot_hot = m_dot_t*(m_enth_last[8 - cpp_offset] - m_enth_last[9 - cpp_offset]) / (m_temp_last[8 - cpp_offset] - m_temp_last[9 - cpp_offset]);		// LT recuperator hot stream capacitance rate
//		double C_dot_cold = m_dot_mc*(m_enth_last[3 - cpp_offset] - m_enth_last[2 - cpp_offset]) / (m_temp_last[3 - cpp_offset] - m_temp_last[2 - cpp_offset]);	// LT recuperator cold stream capacitance rate
//		double C_dot_min = min(C_dot_hot, C_dot_cold);
//		double Q_dot_max = C_dot_min*(m_temp_last[8 - cpp_offset] - m_temp_last[2 - cpp_offset]);
//		double hx_eff = Q_dot_LT / Q_dot_max;				// Definition of effectiveness
//		LT_des_par.m_DP_design[0] = m_pres_last[2 - cpp_offset] - m_pres_last[3 - cpp_offset];
//		LT_des_par.m_DP_design[1] = m_pres_last[8 - cpp_offset] - m_pres_last[9 - cpp_offset];
//		LT_des_par.m_eff_design = hx_eff;
//		LT_des_par.m_min_DT_design = min_DT_LT;
//		LT_des_par.m_m_dot_design[0] = m_dot_mc;
//		LT_des_par.m_m_dot_design[1] = m_dot_t;
//		LT_des_par.m_N_sub = ms_des_par.m_N_sub_hxrs;
//		LT_des_par.m_Q_dot_design = Q_dot_LT;
//		LT_des_par.m_UA_design = UA_LT_calc;
//		m_LT.initialize(LT_des_par);
//
//		// Calculate performance metrics for high-temperature recuperator
//		C_HeatExchanger::S_design_parameters HT_des_par;
//		C_dot_hot = m_dot_t*(m_enth_last[7 - cpp_offset] - m_enth_last[8 - cpp_offset]) / (m_temp_last[7 - cpp_offset] - m_temp_last[8 - cpp_offset]);			// HT recuperator hot stream capacitance rate
//		C_dot_cold = m_dot_t*(m_enth_last[5 - cpp_offset] - m_enth_last[4 - cpp_offset]) / (m_temp_last[5 - cpp_offset] - m_temp_last[4 - cpp_offset]);			// HT recuperator cold stream capacitance rate
//		C_dot_min = min(C_dot_hot, C_dot_cold);
//		Q_dot_max = C_dot_min*(m_temp_last[7 - cpp_offset] - m_temp_last[4 - cpp_offset]);
//		hx_eff = Q_dot_HT / Q_dot_max;						// Definition of effectiveness
//		HT_des_par.m_DP_design[0] = m_pres_last[4 - cpp_offset] - m_pres_last[5 - cpp_offset];
//		HT_des_par.m_DP_design[1] = m_pres_last[7 - cpp_offset] - m_pres_last[8 - cpp_offset];
//		HT_des_par.m_eff_design = hx_eff;
//		HT_des_par.m_min_DT_design = min_DT_HT;
//		HT_des_par.m_m_dot_design[0] = m_dot_t;
//		HT_des_par.m_m_dot_design[1] = m_dot_t;
//		HT_des_par.m_N_sub = ms_des_par.m_N_sub_hxrs;
//		HT_des_par.m_Q_dot_design = Q_dot_HT;
//		HT_des_par.m_UA_design = UA_HT_calc;
//		m_HT.initialize(HT_des_par);
//
//		// Set relevant values for other heat exchangers
//		C_HeatExchanger::S_design_parameters PHX_des_par;
//		PHX_des_par.m_DP_design[0] = m_pres_last[5 - cpp_offset] - m_pres_last[6 - cpp_offset];
//		PHX_des_par.m_DP_design[1] = 0.0;
//		PHX_des_par.m_m_dot_design[0] = m_dot_t;
//		PHX_des_par.m_m_dot_design[1] = 0.0;
//		PHX_des_par.m_Q_dot_design = m_dot_t*(m_enth_last[6 - cpp_offset] - m_enth_last[5 - cpp_offset]);
//		m_PHX.initialize(PHX_des_par);
//
//		C_HeatExchanger::S_design_parameters PC_des_par;
//		PC_des_par.m_DP_design[0] = 0.0;
//		PC_des_par.m_DP_design[1] = m_pres_last[9 - cpp_offset] - m_pres_last[1 - cpp_offset];
//		PC_des_par.m_m_dot_design[0] = 0.0;
//		PC_des_par.m_m_dot_design[1] = m_dot_mc;
//		PC_des_par.m_Q_dot_design = m_dot_mc*(m_enth_last[9 - cpp_offset] - m_enth_last[1 - cpp_offset]);
//		m_PC.initialize(PC_des_par);
//
//		double Q_dot_bypass = f_bypass*m_dot_t*(m_enth_last[5 - cpp_offset] - m_enth_last[4 - cpp_offset]);
//
//		// Calculate/set cycle performance metrics
//		m_W_dot_net_last = w_mc*m_dot_mc + w_rc*m_dot_rc + w_t*m_dot_t;
//		m_eta_thermal_last = m_W_dot_net_last / (PHX_des_par.m_Q_dot_design + Q_dot_bypass);
//
//		m_m_dot_mc = m_dot_mc;
//		m_m_dot_rc = m_dot_rc;
//		m_m_dot_t = m_dot_t;
//
//		double E_bal = (PHX_des_par.m_Q_dot_design + Q_dot_bypass) - (m_W_dot_net_last + PC_des_par.m_Q_dot_design);
//
//		//double Q_dot_bypass = f_bypass*m_dot_t*(m_enth_last[5-cpp_offset] - m_enth_last[4-cpp_offset]);
//
//		double Q_hs_frac = Q_dot_bypass / (PHX_des_par.m_Q_dot_design+Q_dot_bypass);
//
//		double diff_q_hs_frac = Q_hs_frac - Q_hs_frac_target;
//
//		if( fabs(diff_q_hs_frac) > ms_des_par.m_tol )
//		{
//			if(diff_q_hs_frac > 0.0)
//			{
//				f_bypass_high = f_bypass;
//				f_bypass = 0.5*(f_bypass_high + f_bypass_low);
//			}
//			else
//			{
//				f_bypass_low = f_bypass;
//				f_bypass = 0.5*(f_bypass_high + f_bypass_low);
//			}
//			if(f_bypass_max - f_bypass_low < 0.005)
//			{
//				m_eta_thermal_last = 0.0;
//				break;
//			}
//			if(f_bypass_high - f_bypass_min < 0.005)
//			{
//				m_eta_thermal_last = 0.0;
//				break;
//			}
//		}
//		else
//		{
//			double this_solved_i_guess = 321.456;
//			break;
//		}
//
//		if(iter_f_bypass > 50)
//		{
//			m_eta_thermal_last = 0.0;
//			break;
//		}
//
//
//	} while( true );
//
//	
//}

void C_RecompCycle_RCMCI_with_ReHeating::design_core_standard(int & error_code)
{
	// twn 1.4.17: put reasonable lower bound on *modeled* recompression fraction
	if( ms_des_par.m_recomp_frac < 0.01 )
	{
		ms_des_par.m_recomp_frac = 0.0;
		double UA_tot = ms_des_par.m_UA_LT + ms_des_par.m_UA_HT;
		ms_des_par.m_UA_LT = UA_tot;
		ms_des_par.m_UA_HT = 0.0;
	}

	CO2_state co2_props;

	// Initialize Recuperators
	// LTR
	mc_LT_recup.initialize(ms_des_par.m_N_sub_hxrs);
	// HTR
	mc_HT_recup.initialize(ms_des_par.m_N_sub_hxrs);

	int max_iter = 500;
	double temperature_tolerance = 1.E-6;		// Temp differences below this are considered zero

	// Initialize a few variables
	double m_dot_t, m_dot_mc, m_dot_rc, Q_dot_LT, Q_dot_HT, UA_LT_calc, UA_HT_calc;
	m_dot_t = m_dot_mc = m_dot_rc = Q_dot_LT = Q_dot_HT = UA_LT_calc = UA_HT_calc = 0.0;

	m_temp_last[MC1_IN] = ms_des_par.m_T_mc1_in; //stream 13
	m_pres_last[MC1_IN] = ms_des_par.m_P_mc1_in; //stream 13
	m_temp_last[MC2_IN] = ms_des_par.m_T_mc2_in; //stream 1
	m_pres_last[MC2_IN] = ms_des_par.m_P_mc2_in; //stream 1
	m_pres_last[MC1_OUT] = ms_des_par.m_P_mc1_out; //stream 14
	m_pres_last[MC2_OUT] = ms_des_par.m_P_mc2_out; //stream 2
	m_temp_last[MT_IN] = ms_des_par.m_T_mt_in; //stream 6
	m_temp_last[RT_IN] = ms_des_par.m_T_rt_in; //stream 12
	m_pres_last[RT_IN] = ms_des_par.m_P_rt_in; //stream 12

//-----------------------------------------------------------------------------------------------------------------------------------------------------
	// Apply pressure drops to heat exchangers, fully defining the pressures at all states
	
	//LTR HP
	if( ms_des_par.m_DP_LT[0] < 0.0 )
		m_pres_last[LTR_HP_OUT] = m_pres_last[MC2_OUT] - m_pres_last[MC2_OUT] * fabs(ms_des_par.m_DP_LT[0]);		// relative pressure drop specified for LT recuperator (cold stream)
	else
		m_pres_last[LTR_HP_OUT] = m_pres_last[MC2_OUT] - ms_des_par.m_DP_LT[0];				// absolute pressure drop specified for LT recuperator (cold stream)

	//LTR ZERO
	if( ms_des_par.m_UA_LT < 1.0E-12 )
		m_pres_last[LTR_HP_OUT] = m_pres_last[MC2_OUT];			// If there is no LT recuperator, there is no pressure drop

	m_pres_last[MIXER_OUT] = m_pres_last[LTR_HP_OUT];			// Assume no pressure drop in mixing valve
	m_pres_last[RC_OUT] = m_pres_last[LTR_HP_OUT];				// Assume no pressure drop in mixing valve

	//HTR HP 
	if( ms_des_par.m_DP_HT[0] < 0.0 )
		m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT] - m_pres_last[MIXER_OUT] * fabs(ms_des_par.m_DP_HT[0]);	// relative pressure drop specified for HT recuperator (cold stream)
	else
		m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT] - ms_des_par.m_DP_HT[0];				// absolute pressure drop specified for HT recuperator (cold stream)

	//HTR ZERO
	if( ms_des_par.m_UA_HT < 1.0E-12 )
		m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT];		// If there is no HT recuperator, there is no pressure drop

	//PHX
	if( ms_des_par.m_DP_PHX[0] < 0.0 )
		m_pres_last[MT_IN] = m_pres_last[HTR_HP_OUT] - m_pres_last[HTR_HP_OUT] * fabs(ms_des_par.m_DP_PHX[0]);	// relative pressure drop specified for PHX
	else
		m_pres_last[MT_IN] = m_pres_last[HTR_HP_OUT] - ms_des_par.m_DP_PHX[0];									// absolute pressure drop specified for PHX
	
	//RHX																										
	if (ms_des_par.m_DP_RHX[0] < 0.0)																			
		m_pres_last[MT_OUT] = m_pres_last[RT_IN] - m_pres_last[RT_IN] * fabs(ms_des_par.m_DP_RHX[0]);           // relative pressure drop specified for RHX
	//RHX pressure drop is positive, Absolute pressure drop specified for RHX (cold stream)
	else
		m_pres_last[MT_OUT] = m_pres_last[RT_IN] - ms_des_par.m_DP_RHX[0];										// absolute pressure drop specified for RHX

	//PC1
	if( ms_des_par.m_DP_PC1[1] < 0.0 )
		m_pres_last[LTR_LP_OUT] = m_pres_last[MC1_IN] / (1.0 - fabs(ms_des_par.m_DP_PC1[1]));					// relative pressure drop specified for precooler: P1=P9-P9*rel_DP => P1=P9*(1-rel_DP)
	else
		m_pres_last[LTR_LP_OUT] = m_pres_last[MC1_IN] + ms_des_par.m_DP_PC1[1];

	//PC2
	if( ms_des_par.m_DP_PC2[1] < 0.0 )
		m_pres_last[MC1_OUT] = m_pres_last[MC2_IN] / (1.0 - fabs(ms_des_par.m_DP_PC2[1]));					// relative pressure drop specified for precooler: P1=P9-P9*rel_DP => P1=P9*(1-rel_DP)
	else
		m_pres_last[MC1_OUT] = m_pres_last[MC2_IN] + ms_des_par.m_DP_PC2[1];
	
	//LTR LP
	if( ms_des_par.m_DP_LT[1] < 0.0 )
		m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT] / (1.0 - fabs(ms_des_par.m_DP_LT[1]));	// relative pressure drop specified for LT recuperator (hot stream)
	else
		m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT] + ms_des_par.m_DP_LT[1];					// absolute pressure drop specified for LT recuperator (hot stream)

	//LTR ZERO
	if( ms_des_par.m_UA_LT < 1.0E-12 )
		m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT];			// if there is no LT recuperator, there is no pressure drop

	//HTR LP
	if( ms_des_par.m_DP_HT[1] < 0.0 )
		m_pres_last[RT_OUT] = m_pres_last[HTR_LP_OUT] / (1.0 - fabs(ms_des_par.m_DP_HT[1]));	// relative pressure drop specified for HT recuperator (hot stream)
	else
		m_pres_last[RT_OUT] = m_pres_last[HTR_LP_OUT] + ms_des_par.m_DP_HT[1];				// absolute pressure drop specified for HT recuperator (hot stream)

	//HTR ZERO
	if( ms_des_par.m_UA_HT < 1.0E-12 )
		m_pres_last[RT_OUT] = m_pres_last[HTR_LP_OUT];		// if there is no HT recuperator, there is no pressure drop

//------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
	
	// Determine equivalent isentropic efficiencies for main compressor and turbine, if necessary.
	double eta_mc1_isen = std::numeric_limits<double>::quiet_NaN();
	double eta_mc2_isen = std::numeric_limits<double>::quiet_NaN();
	double eta_mt_isen = std::numeric_limits<double>::quiet_NaN();
	double eta_rt_isen = std::numeric_limits<double>::quiet_NaN();
	
	//Main Compressor1 
	if( ms_des_par.m_eta_mc1 < 0.0 )
	{
		int poly_error_code1 = 0;

		isen_eta_from_poly_eta_RCMCI_with_ReHeating(m_temp_last[MC1_IN], m_pres_last[MC1_IN], m_pres_last[MC1_OUT], fabs(ms_des_par.m_eta_mc1),
			true, poly_error_code1, eta_mc1_isen);

		if( poly_error_code1 != 0 )
		{
			error_code = poly_error_code1;
			return;
		}
	}
	else
		eta_mc1_isen = ms_des_par.m_eta_mc1;
	
	//Main Compressor2 
	if( ms_des_par.m_eta_mc2 < 0.0 )
	{
		int poly_error_code2 = 0;

		isen_eta_from_poly_eta_RCMCI_with_ReHeating(m_temp_last[MC2_IN], m_pres_last[MC2_IN], m_pres_last[MC2_OUT], fabs(ms_des_par.m_eta_mc2),
			true, poly_error_code2, eta_mc2_isen);

		if( poly_error_code2 != 0 )
		{
			error_code = poly_error_code2;
			return;
		}
	}
	else
		eta_mc2_isen = ms_des_par.m_eta_mc2;

	//Main Turbine design
	if( ms_des_par.m_eta_mt < 0.0 )
	{
		int poly_error_code_mt = 0;

		isen_eta_from_poly_eta_RCMCI_with_ReHeating(m_temp_last[MT_IN], m_pres_last[MT_IN], m_pres_last[MT_OUT], fabs(ms_des_par.m_eta_mt),
			false, poly_error_code_mt, eta_mt_isen);

		if( poly_error_code_mt != 0 )
		{
			error_code = poly_error_code_mt;
			return;
		}
	}
	else
		eta_mt_isen = ms_des_par.m_eta_mt;

	//ReHeating Turbine design
	if (ms_des_par.m_eta_rt < 0.0)
	{
		int poly_error_code_rt = 0;

		isen_eta_from_poly_eta_RCMCI_with_ReHeating(m_temp_last[RT_IN], m_pres_last[RT_IN], m_pres_last[RT_OUT], fabs(ms_des_par.m_eta_mt),
			false, poly_error_code_rt, eta_rt_isen);

		if (poly_error_code_rt != 0)
		{
			error_code = poly_error_code_rt;
			return;
		}
	}
	else
		eta_rt_isen = ms_des_par.m_eta_rt;

	// Determine the outlet state and specific work for the main compressor and turbine.
	int comp1_error_code = 0;
	int comp2_error_code = 0;
	double w_mc1 = std::numeric_limits<double>::quiet_NaN();
	double w_mc2 = std::numeric_limits<double>::quiet_NaN();
	// Main compressor1
	calculate_turbomachinery_outlet_1_RCMCI_with_ReHeating(m_temp_last[MC1_IN], m_pres_last[MC1_IN], m_pres_last[MC1_OUT], eta_mc1_isen, true,
		comp1_error_code, m_enth_last[MC1_IN], m_entr_last[MC1_IN], m_dens_last[MC1_IN], m_temp_last[MC1_OUT],
		m_enth_last[MC1_OUT], m_entr_last[MC1_OUT], m_dens_last[MC1_OUT], w_mc1);
		
	// Main compressor2
	calculate_turbomachinery_outlet_1_RCMCI_with_ReHeating(m_temp_last[MC2_IN], m_pres_last[MC2_IN], m_pres_last[MC2_OUT], eta_mc2_isen, true,
		comp2_error_code, m_enth_last[MC2_IN], m_entr_last[MC2_IN], m_dens_last[MC2_IN], m_temp_last[MC2_OUT],
		m_enth_last[MC2_OUT], m_entr_last[MC2_OUT], m_dens_last[MC2_OUT], w_mc2);

	if( comp1_error_code != 0 )
	{
		error_code = comp1_error_code;
		return;
	}
	
	
	if( comp2_error_code != 0 )
	{
		error_code = comp2_error_code;
		return;
	}

	// Main Turbine
	int m_turbine_error_code = 0;
	double w_mt = std::numeric_limits<double>::quiet_NaN();
	
	calculate_turbomachinery_outlet_1_RCMCI_with_ReHeating(m_temp_last[MT_IN], m_pres_last[MT_IN], m_pres_last[MT_OUT], eta_mt_isen, false,
		m_turbine_error_code, m_enth_last[MT_IN], m_entr_last[MT_IN], m_dens_last[MT_IN], m_temp_last[MT_OUT],
		m_enth_last[MT_OUT], m_entr_last[MT_OUT], m_dens_last[MT_OUT], w_mt);

	if( m_turbine_error_code != 0 )
	{
		error_code = m_turbine_error_code;
		return;
	}

	// ReHeating Turbine
	int r_turbine_error_code = 0;
	double w_rt = std::numeric_limits<double>::quiet_NaN();

	calculate_turbomachinery_outlet_1_RCMCI_with_ReHeating(m_temp_last[RT_IN], m_pres_last[RT_IN], m_pres_last[RT_OUT], eta_rt_isen, false,
		r_turbine_error_code, m_enth_last[RT_IN], m_entr_last[RT_IN], m_dens_last[RT_IN], m_temp_last[RT_OUT],
		m_enth_last[RT_OUT], m_entr_last[RT_OUT], m_dens_last[RT_OUT], w_rt);

	if (r_turbine_error_code != 0)
	{
		error_code = r_turbine_error_code;
		return;
	}

	// Check that this cycle can produce power
	double eta_rc_isen = std::numeric_limits<double>::quiet_NaN();
	double w_rc = std::numeric_limits<double>::quiet_NaN();
	if( ms_des_par.m_recomp_frac >= 1.E-12 )
	{
		if( ms_des_par.m_eta_rc < 0.0 )		// need to convert polytropic efficiency to isentropic efficiency
		{
			int rc_error_code = 0;

			isen_eta_from_poly_eta_RCMCI_with_ReHeating(m_temp_last[LTR_LP_OUT], m_pres_last[LTR_LP_OUT], m_pres_last[RC_OUT], fabs(ms_des_par.m_eta_rc),
				true, rc_error_code, eta_rc_isen);

			if( rc_error_code != 0 )
			{
				error_code = rc_error_code;
				return;
			}
		}
		else
			eta_rc_isen = ms_des_par.m_eta_rc;

		int rc_error_code = 0;
		calculate_turbomachinery_outlet_1_RCMCI_with_ReHeating(m_temp_last[MC1_OUT], m_pres_last[LTR_LP_OUT], m_pres_last[RC_OUT], eta_rc_isen,
			true, rc_error_code, w_rc);

		if( rc_error_code != 0 )
		{
			error_code = rc_error_code;
			return;
		}
	}
	else
		w_rc = 0.0;

	if( w_mc1 + w_mc2 + w_rc + w_mt + w_rt <= 0.0 )	// positive net power is impossible; return an error
	{
		error_code = 25;
		return;
	}

//-------------------------------BEGINING MODIFICATION---------------------------------------------------
//--------------------------------------------------------------------------------------------

	// ****************************************************
	// ****************************************************
	// Solve the recuperators
	double T_HTR_LP_out_lower = m_temp_last[MC2_OUT];		//[K] Coldest possible temperature
	double T_HTR_LP_out_upper = m_temp_last[MT_OUT];		//[K] Hottest possible temperature
	
	double T_HTR_LP_out_guess_lower = min(T_HTR_LP_out_upper - 2.0, max(T_HTR_LP_out_lower + 45.0, 20.0 + 273.15));	//[K] There is nothing special about these guesses...
	double T_HTR_LP_out_guess_upper = min(T_HTR_LP_out_guess_lower + 50.0, T_HTR_LP_out_upper - 1.0);	//[K] There is nothing special about these guesses, either...
	
	C_mono_eq_HTR_des HTR_des_eq(this, w_mc1, w_mc2, w_mt, w_rt);
	C_monotonic_eq_solver HTR_des_solver(HTR_des_eq);
	
	HTR_des_solver.settings(ms_des_par.m_tol*m_temp_last[MC1_IN], 1000, T_HTR_LP_out_lower, T_HTR_LP_out_upper, false);
	
	double T_HTR_LP_out_solved, tol_T_HTR_LP_out_solved;
	T_HTR_LP_out_solved = tol_T_HTR_LP_out_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_T_HTR_LP_out = -1;
	
	int T_HTR_LP_out_code = HTR_des_solver.solve(T_HTR_LP_out_guess_lower, T_HTR_LP_out_guess_upper, 0,
								T_HTR_LP_out_solved, tol_T_HTR_LP_out_solved, iter_T_HTR_LP_out);
	
	if( T_HTR_LP_out_code != C_monotonic_eq_solver::CONVERGED )
	{
		error_code = 35;
		return;
	}

	// Get information calculated in C_mono_eq_HTR_des
	w_rc = HTR_des_eq.m_w_rc;
	m_dot_t = HTR_des_eq.m_m_dot_t;
	m_dot_rc = HTR_des_eq.m_m_dot_rc;
	m_dot_mc = HTR_des_eq.m_m_dot_mc;
	Q_dot_LT = HTR_des_eq.m_Q_dot_LT;
	Q_dot_HT = HTR_des_eq.m_Q_dot_HT;

//----------------------------------------------------------------------------------------------------------------------------------------------------
//-------------------------------------------END MODIFICATION----------------------------------------------------------------------------

// State 5 can now be fully defined
	m_enth_last[HTR_HP_OUT] = m_enth_last[MIXER_OUT] + Q_dot_HT / m_dot_t;						// Energy balance on cold stream of high-temp recuperator
	int prop_error_code = CO2_PH(m_pres_last[HTR_HP_OUT], m_enth_last[HTR_HP_OUT], &co2_props);
	if( prop_error_code != 0 )
	{
		error_code = prop_error_code;
		return;
	}
	m_temp_last[HTR_HP_OUT] = co2_props.temp;
	m_entr_last[HTR_HP_OUT] = co2_props.entr;
	m_dens_last[HTR_HP_OUT] = co2_props.dens;

	// Set relevant values for other heat exchangers
	C_HeatExchanger_RCMCI_with_ReHeating::S_design_parameters PHX_des_par;
	PHX_des_par.m_DP_design[0] = m_pres_last[HTR_HP_OUT] - m_pres_last[MT_IN];
	PHX_des_par.m_DP_design[1] = 0.0;
	PHX_des_par.m_m_dot_design[0] = m_dot_t;
	PHX_des_par.m_m_dot_design[1] = 0.0;
	PHX_des_par.m_Q_dot_design = m_dot_t*(m_enth_last[MT_IN] - m_enth_last[HTR_HP_OUT]);
	m_PHX.initialize(PHX_des_par);

	C_HeatExchanger_RCMCI_with_ReHeating::S_design_parameters	RHX_des_par;
	RHX_des_par.m_DP_design[0] = m_pres_last[MT_OUT] - m_pres_last[RT_IN];
	RHX_des_par.m_DP_design[1] = 0.0;
	RHX_des_par.m_m_dot_design[0] = m_dot_t;
	RHX_des_par.m_m_dot_design[1] = 0.0;
	RHX_des_par.m_Q_dot_design = m_dot_t*(m_enth_last[RT_IN] - m_enth_last[MT_OUT]);
	m_RHX.initialize(RHX_des_par);

	C_HeatExchanger_RCMCI_with_ReHeating::S_design_parameters PC1_des_par;
	PC1_des_par.m_DP_design[0] = 0.0;
	PC1_des_par.m_DP_design[1] = m_pres_last[LTR_LP_OUT] - m_pres_last[MC1_IN];
	PC1_des_par.m_m_dot_design[0] = 0.0;
	PC1_des_par.m_m_dot_design[1] = m_dot_mc;
	PC1_des_par.m_Q_dot_design = m_dot_mc*(m_enth_last[LTR_LP_OUT] - m_enth_last[MC1_IN]);
	m_PC1.initialize(PC1_des_par);

	C_HeatExchanger_RCMCI_with_ReHeating::S_design_parameters PC2_des_par;
	PC2_des_par.m_DP_design[0] = 0.0;
	PC2_des_par.m_DP_design[1] = m_pres_last[MC1_OUT] - m_pres_last[MC2_IN];
	PC2_des_par.m_m_dot_design[0] = 0.0;
	PC2_des_par.m_m_dot_design[1] = m_dot_mc;
	PC2_des_par.m_Q_dot_design = m_dot_mc*(m_enth_last[MC1_OUT] - m_enth_last[MC2_IN]);
	m_PC2.initialize(PC2_des_par);
	
//-----------------------------------------------------------------------------------------------------------------------
	// Calculate/set cycle performance metrics
	m_W_dot_net_last = w_mc1*m_dot_mc + w_mc2*m_dot_mc +w_rc*m_dot_rc + w_mt*m_dot_t + w_rt*m_dot_t;
	m_eta_thermal_last = m_W_dot_net_last / (PHX_des_par.m_Q_dot_design + RHX_des_par.m_Q_dot_design);

	m_m_dot_mc = m_dot_mc;
	m_m_dot_rc = m_dot_rc;
	m_m_dot_t = m_dot_t;

	//Design Results are stored in S_design_solved ms_des_solved structure. 
	ms_des_solved.m_recomp_frac = m_m_dot_rc / m_m_dot_t;
	ms_des_solved.m_UA_HT = ms_des_par.m_UA_HT;
	ms_des_solved.m_UA_LT = ms_des_par.m_UA_LT;
	ms_des_solved.m_pres[1] = m_pres_last[1];
	ms_des_solved.m_pres[0] = m_pres_last[0];
	ms_des_solved.m_eta_thermal = m_eta_thermal_last;
	ms_des_solved.m_m_dot_t = PHX_des_par.m_m_dot_design[0];
	ms_des_solved.ms_mc1_ms_des_solved.m_N_design = ms_des_par.m_N_turbine;
}

int C_RecompCycle_RCMCI_with_ReHeating::C_mono_eq_LTR_des::operator()(double T_LTR_LP_out /*K*/, double *diff_T_LTR_LP_out /*K*/)
{
	m_w_rc = m_m_dot_t = m_m_dot_rc = m_m_dot_mc = m_Q_dot_LT = std::numeric_limits<double>::quiet_NaN();
	
	mpc_rc_cycle->m_temp_last[LTR_LP_OUT] = T_LTR_LP_out;

	// First, solve the recompressor model as necessary
	if(mpc_rc_cycle->ms_des_par.m_recomp_frac >= 1.E-12)
	{
		double eta_rc_isen = std::numeric_limits<double>::quiet_NaN();

		if( mpc_rc_cycle->ms_des_par.m_eta_rc < 0.0 )		// recalculate isen. efficiency of recompressor because inlet temp changes
		{
			int rc_error_code = 0;
			isen_eta_from_poly_eta_RCMCI_with_ReHeating(mpc_rc_cycle->m_temp_last[LTR_LP_OUT], mpc_rc_cycle->m_pres_last[LTR_LP_OUT],
								mpc_rc_cycle->m_pres_last[RC_OUT], fabs(mpc_rc_cycle->ms_des_par.m_eta_rc), true,
								rc_error_code, eta_rc_isen);

			if( rc_error_code != 0 )
			{
				*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
				return rc_error_code;
			}		
		}
		else
		{
			eta_rc_isen = mpc_rc_cycle->ms_des_par.m_eta_rc;
		}
	
		int rc_error_code = 0;

		calculate_turbomachinery_outlet_1_RCMCI_with_ReHeating(mpc_rc_cycle->m_temp_last[LTR_LP_OUT], mpc_rc_cycle->m_pres_last[LTR_LP_OUT], mpc_rc_cycle->m_pres_last[RC_OUT], eta_rc_isen, true, rc_error_code,
			mpc_rc_cycle->m_enth_last[LTR_LP_OUT], mpc_rc_cycle->m_entr_last[LTR_LP_OUT], mpc_rc_cycle->m_dens_last[LTR_LP_OUT], mpc_rc_cycle->m_temp_last[RC_OUT], mpc_rc_cycle->m_enth_last[RC_OUT],
			mpc_rc_cycle->m_entr_last[RC_OUT], mpc_rc_cycle->m_dens_last[RC_OUT], m_w_rc);

		if( rc_error_code != 0 )
		{
			*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
			return rc_error_code;
		}
	}
	else
	{
		m_w_rc = 0.0;		// no recompressor
		int prop_error_code = CO2_TP(mpc_rc_cycle->m_temp_last[LTR_LP_OUT], mpc_rc_cycle->m_pres_last[LTR_LP_OUT], &mpc_rc_cycle->mc_co2_props);
		if( prop_error_code != 0 )
		{
			*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
			return prop_error_code;
		}
		mpc_rc_cycle->m_enth_last[LTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.enth;
		mpc_rc_cycle->m_entr_last[LTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.entr;
		mpc_rc_cycle->m_dens_last[LTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.dens;
		mpc_rc_cycle->m_temp_last[RC_OUT] = mpc_rc_cycle->m_temp_last[LTR_LP_OUT];
		mpc_rc_cycle->m_enth_last[RC_OUT] = mpc_rc_cycle->m_enth_last[LTR_LP_OUT];
		mpc_rc_cycle->m_entr_last[RC_OUT] = mpc_rc_cycle->m_entr_last[LTR_LP_OUT];
		mpc_rc_cycle->m_dens_last[RC_OUT] = mpc_rc_cycle->m_dens_last[LTR_LP_OUT];
	}

	// Calculate the mass flow required to hit cycle target power
	m_m_dot_t = mpc_rc_cycle->ms_des_par.m_W_dot_net / ((m_w_mc1*(1.0 - mpc_rc_cycle->ms_des_par.m_recomp_frac))+ (m_w_mc2*(1.0 - mpc_rc_cycle->ms_des_par.m_recomp_frac)) + (m_w_rc*mpc_rc_cycle->ms_des_par.m_recomp_frac) + m_w_mt + m_w_rt);		//[kg/s]
	if( m_m_dot_t < 0.0 )
	{
		*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return 29;
	}
	m_m_dot_rc = m_m_dot_t * mpc_rc_cycle->ms_des_par.m_recomp_frac;		//[kg/s]
	m_m_dot_mc = m_m_dot_t - m_m_dot_rc;

	double T_LTR_LP_out_calc = std::numeric_limits<double>::quiet_NaN();

	try
	{
		mpc_rc_cycle->mc_LT_recup.design_fix_UA_calc_outlet(mpc_rc_cycle->ms_des_par.m_UA_LT, mpc_rc_cycle->ms_des_par.m_LT_eff_max,
			mpc_rc_cycle->m_temp_last[MC2_OUT], mpc_rc_cycle->m_pres_last[MC2_OUT], m_m_dot_mc, mpc_rc_cycle->m_pres_last[LTR_HP_OUT],
			mpc_rc_cycle->m_temp_last[HTR_LP_OUT], mpc_rc_cycle->m_pres_last[HTR_LP_OUT], m_m_dot_t, mpc_rc_cycle->m_pres_last[LTR_LP_OUT],
			m_Q_dot_LT, mpc_rc_cycle->m_temp_last[LTR_HP_OUT], T_LTR_LP_out_calc);
	}
	catch( C_csp_exception & )
	{
		*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();

		return -1;
	}

	*diff_T_LTR_LP_out = T_LTR_LP_out_calc - mpc_rc_cycle->m_temp_last[LTR_LP_OUT];		//[K]

	return 0;
}

int C_RecompCycle_RCMCI_with_ReHeating::C_mono_eq_HTR_des::operator()(double T_HTR_LP_out /*K*/, double *diff_T_HTR_LP_out /*K*/)
{
	m_w_rc = m_m_dot_t = m_m_dot_rc = m_m_dot_mc = m_Q_dot_LT = m_Q_dot_HT = std::numeric_limits<double>::quiet_NaN();	

	mpc_rc_cycle->m_temp_last[HTR_LP_OUT] = T_HTR_LP_out;		//[K]	

	int prop_error_code = CO2_TP(mpc_rc_cycle->m_temp_last[HTR_LP_OUT], mpc_rc_cycle->m_temp_last[HTR_LP_OUT], &mpc_rc_cycle->mc_co2_props);
	if( prop_error_code != 0 )
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_rc_cycle->m_enth_last[HTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.enth;
	mpc_rc_cycle->m_entr_last[HTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.entr;
	mpc_rc_cycle->m_dens_last[HTR_LP_OUT] = mpc_rc_cycle->mc_co2_props.dens;

	// *********************************************************************************
	// *********************************************************************************
	// Solve for the LTR solution
	double T_LTR_LP_out_lower = mpc_rc_cycle->m_temp_last[MC2_OUT];		//[K] Coldest possible outlet temperature
	double T_LTR_LP_out_upper = mpc_rc_cycle->m_temp_last[HTR_LP_OUT];	//[K] Hottest possible outlet temperature

	double T_LTR_LP_out_guess_upper = min(T_LTR_LP_out_upper, T_LTR_LP_out_lower + 15.0);	//[K] There is nothing special about using 15 here...
	double T_LTR_LP_out_guess_lower = min(T_LTR_LP_out_guess_upper*0.99, T_LTR_LP_out_lower + 2.0);	//[K] There is nothing special about using 2 here...

	C_mono_eq_LTR_des LTR_des_eq(mpc_rc_cycle, m_w_mc1, m_w_mc2, m_w_mt, m_w_rt);
	C_monotonic_eq_solver LTR_des_solver(LTR_des_eq);

	LTR_des_solver.settings(mpc_rc_cycle->ms_des_par.m_tol*mpc_rc_cycle->m_temp_last[MC1_IN], 1000, T_LTR_LP_out_lower,
								T_LTR_LP_out_upper, false);

	double T_LTR_LP_out_solved, tol_T_LTR_LP_out_solved;
	T_LTR_LP_out_solved = tol_T_LTR_LP_out_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_T_LTR_LP_out = -1;

	int T_LTR_LP_out_code = LTR_des_solver.solve(T_LTR_LP_out_guess_lower, T_LTR_LP_out_guess_upper, 0,
		T_LTR_LP_out_solved, tol_T_LTR_LP_out_solved, iter_T_LTR_LP_out);

	if( T_LTR_LP_out_code != C_monotonic_eq_solver::CONVERGED )
	{
		return 31;
	}
	  
	// Get information set in the Monotonic Equation class
	m_w_rc = LTR_des_eq.m_w_rc;
	m_m_dot_t = LTR_des_eq.m_m_dot_t;
	m_m_dot_rc = LTR_des_eq.m_m_dot_rc;
	m_m_dot_mc = LTR_des_eq.m_m_dot_mc;
	m_Q_dot_LT = LTR_des_eq.m_Q_dot_LT;

	// Know LTR performance so we can calculate the HP outlet
		// Energy balance on LTR HP stream
	mpc_rc_cycle->m_enth_last[LTR_HP_OUT] = mpc_rc_cycle->m_enth_last[MC2_OUT] + m_Q_dot_LT/ m_m_dot_mc;		//[kJ/kg]
	prop_error_code = CO2_PH(mpc_rc_cycle->m_pres_last[LTR_HP_OUT], mpc_rc_cycle->m_enth_last[LTR_HP_OUT], &mpc_rc_cycle->mc_co2_props);
	if( prop_error_code != 0 )
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_rc_cycle->m_temp_last[LTR_HP_OUT] = mpc_rc_cycle->mc_co2_props.temp;	//[K]
	mpc_rc_cycle->m_entr_last[LTR_HP_OUT] = mpc_rc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
	mpc_rc_cycle->m_dens_last[LTR_HP_OUT] = mpc_rc_cycle->mc_co2_props.dens;	//[kg/m^3]	

	// Simulate the Mixer
	if( mpc_rc_cycle->ms_des_par.m_recomp_frac >= 1.E-12 )
	{
		mpc_rc_cycle->m_enth_last[MIXER_OUT] = (1.0 - mpc_rc_cycle->ms_des_par.m_recomp_frac)*mpc_rc_cycle->m_enth_last[LTR_HP_OUT] + mpc_rc_cycle->ms_des_par.m_recomp_frac*mpc_rc_cycle->m_enth_last[RC_OUT];	//[kJ/kg]
		prop_error_code = CO2_PH(mpc_rc_cycle->m_pres_last[MIXER_OUT], mpc_rc_cycle->m_enth_last[MIXER_OUT], &mpc_rc_cycle->mc_co2_props);
		if( prop_error_code != 0 )
		{
			*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
			return prop_error_code;
		}
		mpc_rc_cycle->m_temp_last[MIXER_OUT] = mpc_rc_cycle->mc_co2_props.temp;		//[K]
		mpc_rc_cycle->m_entr_last[MIXER_OUT] = mpc_rc_cycle->mc_co2_props.entr;		//[kJ/kg-K]
		mpc_rc_cycle->m_dens_last[MIXER_OUT] = mpc_rc_cycle->mc_co2_props.dens;		//[kg/m^3]
	}
	else
	{	// No recompressor, so no mixing required, and HTR HP inlet = LTR HP outlet
		mpc_rc_cycle->m_temp_last[MIXER_OUT] = mpc_rc_cycle->m_temp_last[LTR_HP_OUT];		//[K]
		mpc_rc_cycle->m_enth_last[MIXER_OUT] = mpc_rc_cycle->m_enth_last[LTR_HP_OUT];		//[kJ/kg]
		mpc_rc_cycle->m_entr_last[MIXER_OUT] = mpc_rc_cycle->m_entr_last[LTR_HP_OUT];		//[kJ/kg-K]
		mpc_rc_cycle->m_dens_last[MIXER_OUT] = mpc_rc_cycle->m_dens_last[LTR_HP_OUT];		//[kg/m^3]
	}

	// Find the design solution of the HTR
	double T_HTR_LP_out_calc = std::numeric_limits<double>::quiet_NaN();

	try
	{
	mpc_rc_cycle->mc_HT_recup.design_fix_UA_calc_outlet(mpc_rc_cycle->ms_des_par.m_UA_HT, mpc_rc_cycle->ms_des_par.m_HT_eff_max,
		mpc_rc_cycle->m_temp_last[MIXER_OUT], mpc_rc_cycle->m_pres_last[MIXER_OUT], m_m_dot_t, mpc_rc_cycle->m_pres_last[HTR_HP_OUT],
		mpc_rc_cycle->m_temp_last[RT_OUT], mpc_rc_cycle->m_pres_last[RT_OUT], m_m_dot_t, mpc_rc_cycle->m_pres_last[HTR_LP_OUT],
		m_Q_dot_HT, mpc_rc_cycle->m_temp_last[HTR_HP_OUT], T_HTR_LP_out_calc);
	}
	catch( C_csp_exception & )
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return -1;
	}
	
	*diff_T_HTR_LP_out = T_HTR_LP_out_calc - mpc_rc_cycle->m_temp_last[HTR_LP_OUT];		//[K]	

	return 0;
}

//Calling function design_core_standard()
void C_RecompCycle_RCMCI_with_ReHeating::design_core(int & error_code)
{
	// 2.16.15 twn: choose which design point model to use

	//design_core_bypass150C(error_code);

	design_core_standard(error_code);

	//design_core_bypass(error_code);

	//design_core_HTR_hs(error_code);
}

//Calling function design_core() and function finalize_design()
void C_RecompCycle_RCMCI_with_ReHeating::design(S_design_parameters & des_par_in, int & error_code)
{
	ms_des_par = des_par_in;

	int design_error_code = 0;

	design_core(design_error_code);

	if (design_error_code != 0)
	{
		error_code = design_error_code;
		return;
	}

	finalize_design(design_error_code);

	error_code = design_error_code;
}

//Function to be updated 
void C_RecompCycle_RCMCI_with_ReHeating::finalize_design(int & error_code)
{
	int cpp_offset = 1;

	//Size Main Compressor1
	int mc1_design_err = m_mc1_ms.design_given_outlet_state(m_temp_last[C_RecompCycle_RCMCI_with_ReHeating::MC1_IN],
		m_pres_last[C_RecompCycle_RCMCI_with_ReHeating::MC1_IN],
		m_m_dot_mc,
		m_temp_last[C_RecompCycle_RCMCI_with_ReHeating::MC1_OUT],
		m_pres_last[C_RecompCycle_RCMCI_with_ReHeating::MC1_OUT]);

	if (mc1_design_err != 0)
	{
		error_code = mc1_design_err;
		return;
	}

	//Size Main Compressor2
	int mc2_design_err = m_mc2_ms.design_given_outlet_state(m_temp_last[C_RecompCycle_RCMCI_with_ReHeating::MC2_IN],
		m_pres_last[C_RecompCycle_RCMCI_with_ReHeating::MC2_IN],
		m_m_dot_mc,
		m_temp_last[C_RecompCycle_RCMCI_with_ReHeating::MC2_OUT],
		m_pres_last[C_RecompCycle_RCMCI_with_ReHeating::MC2_OUT]);

	if (mc2_design_err != 0)
	{
		error_code = mc2_design_err;
		return;
	}

	//// Size main compressor
	//C_compressor::S_design_parameters  mc_des_par;
	//	// Compressor inlet conditions
	//mc_des_par.m_P_in = m_pres_last[C_RecompCycle::MC_IN];
	//mc_des_par.m_T_in = m_temp_last[C_RecompCycle::MC_IN];
	//mc_des_par.m_D_in = m_dens_last[C_RecompCycle::MC_IN];
	//mc_des_par.m_h_in = m_enth_last[C_RecompCycle::MC_IN];
	//mc_des_par.m_s_in = m_entr_last[C_RecompCycle::MC_IN];
	//	// Compressor outlet conditions
	//mc_des_par.m_T_out = m_temp_last[C_RecompCycle::MC_OUT];
	//mc_des_par.m_P_out = m_pres_last[C_RecompCycle::MC_OUT];
	//mc_des_par.m_h_out = m_enth_last[C_RecompCycle::MC_OUT];
	//mc_des_par.m_D_out = m_dens_last[C_RecompCycle::MC_OUT];
	//	// Mass flow
	//mc_des_par.m_m_dot = m_m_dot_mc;

	//int comp_size_error_code = 0;
	//m_mc.compressor_sizing(mc_des_par, comp_size_error_code);
	//if(comp_size_error_code != 0)
	//{
	//	error_code = comp_size_error_code;
	//	return;
	//}

	//Size Recompressor
	if (ms_des_par.m_recomp_frac > 0.01)
	{
		int rc_des_err = m_rc_ms.design_given_outlet_state(m_temp_last[C_RecompCycle_RCMCI_with_ReHeating::LTR_LP_OUT],
			m_pres_last[C_RecompCycle_RCMCI_with_ReHeating::LTR_LP_OUT],
			m_m_dot_rc,
			m_temp_last[C_RecompCycle_RCMCI_with_ReHeating::RC_OUT],
			m_pres_last[C_RecompCycle_RCMCI_with_ReHeating::RC_OUT]);

		if (rc_des_err != 0)
		{
			error_code = rc_des_err;
			return;
		}

		ms_des_solved.m_is_rc = true;
	}
	else
		ms_des_solved.m_is_rc = false;

	// Size main turbine
	C_turbine_RCMCI_with_ReHeating::S_design_parameters  mt_des_par;
	// Set main turbine shaft speed
	mt_des_par.m_N_design = ms_des_par.m_N_turbine;
	mt_des_par.m_N_comp_design_if_linked = m_mc1_ms.get_design_solved()->m_N_design;	//[rpm] m_mc.get_design_solved()->m_N_design;
																					// Turbine inlet state
	mt_des_par.m_P_in = m_pres_last[6 - cpp_offset];
	mt_des_par.m_T_in = m_temp_last[6 - cpp_offset];
	mt_des_par.m_D_in = m_dens_last[6 - cpp_offset];
	mt_des_par.m_h_in = m_enth_last[6 - cpp_offset];
	mt_des_par.m_s_in = m_entr_last[6 - cpp_offset];
	// Main Turbine outlet state
	mt_des_par.m_P_out = m_pres_last[11 - cpp_offset];
	mt_des_par.m_h_out = m_enth_last[11 - cpp_offset];
	// Mass flow
	mt_des_par.m_m_dot = m_m_dot_t;

	//Main Turbine Design
	int mturb_size_error_code = 0;
	m_mt.turbine_sizing(mt_des_par, mturb_size_error_code);
	if (mturb_size_error_code != 0)
	{
		error_code = mturb_size_error_code;
		return;
	}

	// Size reheating turbine
	C_turbine_RCMCI_with_ReHeating::S_design_parameters  rt_des_par;
	// Set reheating turbine shaft speed
	rt_des_par.m_N_design = ms_des_par.m_N_turbine;
	rt_des_par.m_N_comp_design_if_linked = m_mc1_ms.get_design_solved()->m_N_design;	//[rpm] m_mc.get_design_solved()->m_N_design;
																						// Turbine inlet state
	rt_des_par.m_P_in = m_pres_last[12 - cpp_offset];
	rt_des_par.m_T_in = m_temp_last[12 - cpp_offset];
	rt_des_par.m_D_in = m_dens_last[12 - cpp_offset];
	rt_des_par.m_h_in = m_enth_last[12 - cpp_offset];
	rt_des_par.m_s_in = m_entr_last[12 - cpp_offset];
	// ReHeating Turbine outlet state
	rt_des_par.m_P_out = m_pres_last[7 - cpp_offset];
	rt_des_par.m_h_out = m_enth_last[7 - cpp_offset];
	// Mass flow
	rt_des_par.m_m_dot = m_m_dot_t;

	//ReHeating Turbine Design
	int rturb_size_error_code = 0;
	m_rt.turbine_sizing(rt_des_par, rturb_size_error_code);
	if (rturb_size_error_code != 0)
	{
		error_code = rturb_size_error_code;
		return;
	}

	// Get 'design_solved' structures from component classes
	//ms_des_solved.ms_mc_des_solved = *m_mc.get_design_solved();
	ms_des_solved.ms_mc1_ms_des_solved = *m_mc1_ms.get_design_solved();
	ms_des_solved.ms_mc2_ms_des_solved = *m_mc2_ms.get_design_solved();
	ms_des_solved.ms_rc_ms_des_solved = *m_rc_ms.get_design_solved();
	ms_des_solved.ms_mt_des_solved = *m_mt.get_design_solved();
	ms_des_solved.ms_rt_des_solved = *m_rt.get_design_solved();
	ms_des_solved.ms_LT_recup_des_solved = mc_LT_recup.ms_des_solved;
	ms_des_solved.ms_HT_recup_des_solved = mc_HT_recup.ms_des_solved;

	// Set solved design point metrics
	ms_des_solved.m_temp = m_temp_last;
	ms_des_solved.m_pres = m_pres_last;
	ms_des_solved.m_enth = m_enth_last;
	ms_des_solved.m_entr = m_entr_last;
	ms_des_solved.m_dens = m_dens_last;

	ms_des_solved.m_eta_thermal = m_eta_thermal_last;
	ms_des_solved.m_W_dot_net = m_W_dot_net_last;
	ms_des_solved.m_m_dot_mc = m_m_dot_mc;
	ms_des_solved.m_m_dot_rc = m_m_dot_rc;
	ms_des_solved.m_m_dot_t = m_m_dot_t;
	ms_des_solved.m_recomp_frac = m_m_dot_rc / m_m_dot_t;

	ms_des_solved.m_UA_LT = ms_des_par.m_UA_LT;
	ms_des_solved.m_UA_HT = ms_des_par.m_UA_HT;
}

void C_RecompCycle_RCMCI_with_ReHeating::opt_design(S_opt_design_parameters & opt_des_par_in, int & error_code)
{
	ms_opt_des_par = opt_des_par_in;

	int opt_design_error_code = 0;

	opt_design_core(error_code);

	if (opt_design_error_code != 0)
	{
		error_code = opt_design_error_code;
		return;
	}

	finalize_design(opt_design_error_code);

	error_code = opt_design_error_code;
}

void C_RecompCycle_RCMCI_with_ReHeating::opt_design_core(int & error_code)
{
	// Map ms_opt_des_par to ms_des_par
	ms_des_par.m_W_dot_net = ms_opt_des_par.m_W_dot_net;
	ms_des_par.m_T_mc1_in = ms_opt_des_par.m_T_mc1_in;
	ms_des_par.m_T_mc2_in = ms_opt_des_par.m_T_mc2_in;
	ms_des_par.m_T_mt_in = ms_opt_des_par.m_T_mt_in;
	ms_des_par.m_T_rt_in = ms_opt_des_par.m_T_rt_in;
	ms_des_par.m_DP_LT = ms_opt_des_par.m_DP_LT;
	ms_des_par.m_DP_HT = ms_opt_des_par.m_DP_HT;
	ms_des_par.m_DP_PC1 = ms_opt_des_par.m_DP_PC1;
	ms_des_par.m_DP_PC2 = ms_opt_des_par.m_DP_PC2;
	ms_des_par.m_DP_PHX = ms_opt_des_par.m_DP_PHX;
	ms_des_par.m_DP_RHX = ms_opt_des_par.m_DP_RHX;
	ms_des_par.m_LT_eff_max = ms_opt_des_par.m_LT_eff_max;
	ms_des_par.m_HT_eff_max = ms_opt_des_par.m_HT_eff_max;
	ms_des_par.m_eta_mc1 = ms_opt_des_par.m_eta_mc1;
	ms_des_par.m_eta_mc2 = ms_opt_des_par.m_eta_mc2;
	ms_des_par.m_eta_rc = ms_opt_des_par.m_eta_rc;
	ms_des_par.m_eta_mt = ms_opt_des_par.m_eta_mt;
	ms_des_par.m_eta_rt = ms_opt_des_par.m_eta_rt;
	ms_des_par.m_N_sub_hxrs = ms_opt_des_par.m_N_sub_hxrs;
	ms_des_par.m_P_high_limit = ms_opt_des_par.m_P_high_limit;
	ms_des_par.m_tol = ms_opt_des_par.m_tol;
	ms_des_par.m_N_turbine = ms_opt_des_par.m_N_turbine;

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

	if (!ms_opt_des_par.m_fixed_P_rt_in_guess)
	{
		x.push_back(ms_opt_des_par.m_P_rt_in_guess);
		lb.push_back(100);
		ub.push_back(ms_opt_des_par.m_P_high_limit);
		scale.push_back(500.0);

		index++;
	}

	if (!ms_opt_des_par.m_fixed_P_mc1_in)
	{
		x.push_back(ms_opt_des_par.m_P_mc1_in_guess);
		lb.push_back(7377.0);
		ub.push_back(ms_opt_des_par.m_P_high_limit);
		scale.push_back(500.0);

		index++;
	}

	if (!ms_opt_des_par.m_fixed_P_mc2_out)
	{
		x.push_back(ms_opt_des_par.m_P_mc2_out_guess);
		lb.push_back(100.0);
		ub.push_back(ms_opt_des_par.m_P_high_limit);
		scale.push_back(500.0);

		index++;
	}

	if (!ms_opt_des_par.m_fixed_PR_mc2)
	{
		x.push_back(ms_opt_des_par.m_PR_mc2_guess);
		lb.push_back(0.0001);
		double PR_max = ms_opt_des_par.m_P_high_limit / 100.0;
		ub.push_back(PR_max);
		scale.push_back(0.2);

		index++;
	}

	if (!ms_opt_des_par.m_fixed_recomp_frac)
	{
		x.push_back(ms_opt_des_par.m_recomp_frac_guess);
		lb.push_back(0.0);
		ub.push_back(1.0);
		scale.push_back(0.05);

		index++;
	}

	if (!ms_opt_des_par.m_fixed_LT_frac)
	{
		x.push_back(ms_opt_des_par.m_LT_frac_guess);
		lb.push_back(0.0);
		ub.push_back(1.0);
		scale.push_back(0.05);

		index++;
	}

	int no_opt_error_code = 0;
	if (index > 0)
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
		opt_des_cycle.set_max_objective(nlopt_callback_opt_des_1_RCMCI_with_ReHeating, this);		// Calls wrapper/callback that calls 'design_point_eta', which optimizes design point eta through repeated calls to 'design'
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
		ms_des_par.m_P_rt_in = ms_opt_des_par.m_P_rt_in_guess;
		ms_des_par.m_P_mc2_out = ms_opt_des_par.m_P_mc2_out_guess;
		ms_des_par.m_P_mc1_in = ms_des_par.m_P_mc1_out / ms_opt_des_par.m_PR_mc2_guess;
		ms_des_par.m_recomp_frac = ms_opt_des_par.m_recomp_frac_guess;
		ms_des_par.m_UA_LT = ms_opt_des_par.m_UA_rec_total*ms_opt_des_par.m_LT_frac_guess;
		ms_des_par.m_UA_HT = ms_opt_des_par.m_UA_rec_total*(1.0 - ms_opt_des_par.m_LT_frac_guess);

		design_core(no_opt_error_code);

		ms_des_par_optimal = ms_des_par;
	}

}

double nlopt_callback_opt_des_1_RCMCI_with_ReHeating(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_RecompCycle_RCMCI_with_ReHeating *frame = static_cast<C_RecompCycle_RCMCI_with_ReHeating*>(data);
	if (frame != NULL)
		return frame->design_point_eta_RCMCI_with_ReHeating(x);
	else
		return 0.0;
}

double C_RecompCycle_RCMCI_with_ReHeating::design_point_eta_RCMCI_with_ReHeating(const std::vector<double> &x)
{
	// 'x' is array of inputs either being adjusted by optimizer or set constant
	// Finish defining ms_des_par based on current 'x' values

	int index = 0;

	if (!ms_opt_des_par.m_fixed_P_rt_in_guess)
	{
		ms_des_par.m_P_rt_in = x[index];
		if (ms_des_par.m_P_rt_in > ms_opt_des_par.m_P_high_limit)
			return 0.0;
		index++;
	}
	else
		ms_des_par.m_P_rt_in = ms_opt_des_par.m_fixed_P_rt_in_guess;

	// Main compressor1 inlet pressure
	if (!ms_opt_des_par.m_fixed_P_mc1_in)
	{
		ms_des_par.m_P_mc1_in = x[index];
		if (ms_des_par.m_P_mc1_in > ms_opt_des_par.m_P_high_limit)
			return 0.0;
		index++;
	}
	else
		ms_des_par.m_P_mc1_in = ms_opt_des_par.m_P_mc1_in_guess;


	// Main compressor2 outlet pressure
	if (!ms_opt_des_par.m_fixed_P_mc2_out)
	{
		ms_des_par.m_P_mc2_out = x[index];
		if (ms_des_par.m_P_mc2_out > ms_opt_des_par.m_P_high_limit)
			return 0.0;
		index++;
	}
	else
		ms_des_par.m_P_mc2_out = ms_opt_des_par.m_P_mc2_out_guess;

	// Main compressor2 pressure ratio
	double PR_mc2_local = -999.9;
	double P_mc2_in = -999.9;
	if (!ms_opt_des_par.m_fixed_PR_mc2)
	{
		PR_mc2_local = x[index];
		if (PR_mc2_local > 50.0)
			return 0.0;
		index++;
		P_mc2_in = ms_des_par.m_P_mc2_out / PR_mc2_local;
	}
	else
	{
		if (ms_opt_des_par.m_PR_mc2_guess >= 0.0)
		{
			PR_mc2_local = ms_opt_des_par.m_PR_mc2_guess;
			P_mc2_in = ms_des_par.m_P_mc2_out / PR_mc2_local;		//[kPa]
		}
		else
		{
			P_mc2_in = fabs(ms_opt_des_par.m_PR_mc2_guess);		//[kPa]
		}
	}



	if (P_mc2_in >= ms_des_par.m_P_mc2_out)
		return 0.0;
	if (P_mc2_in <= 100.0)
		return 0.0;
	ms_des_par.m_P_mc2_in = P_mc2_in;
	ms_des_par.m_P_mc1_out = P_mc2_in;

	// Recompression fraction
	if (!ms_opt_des_par.m_fixed_recomp_frac)
	{
		ms_des_par.m_recomp_frac = x[index];
		if (ms_des_par.m_recomp_frac < 0.0)
			return 0.0;
		index++;
	}
	else
		ms_des_par.m_recomp_frac = ms_opt_des_par.m_recomp_frac_guess;

	// Recuperator split fraction
	double LT_frac_local = -999.9;
	if (!ms_opt_des_par.m_fixed_LT_frac)
	{
		LT_frac_local = x[index];
		if (LT_frac_local > 1.0 || LT_frac_local < 0.0)
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
	if (error_code == 0)
	{
		eta_thermal = m_eta_thermal_last;

		if (m_eta_thermal_last > m_eta_thermal_opt)
		{
			ms_des_par_optimal = ms_des_par;
			m_eta_thermal_opt = m_eta_thermal_last;
		}
	}

	return eta_thermal;
}

void C_RecompCycle_RCMCI_with_ReHeating::auto_opt_design(S_auto_opt_design_parameters & auto_opt_des_par_in, int & error_code)
{
	ms_auto_opt_des_par = auto_opt_des_par_in;

	int auto_opt_des_error_code = 0;

	auto_opt_design_core(auto_opt_des_error_code);

	error_code = auto_opt_des_error_code;

	return;
}

//
void C_RecompCycle_RCMCI_with_ReHeating::auto_opt_design_core(int & error_code)
{
	// Check that simple/recomp flag is set
	if (ms_auto_opt_des_par.m_is_recomp_ok != 0 && ms_auto_opt_des_par.m_is_recomp_ok != 1)
	{
		throw(C_csp_exception("C_RecompCycle::auto_opt_design_core(...) requires that ms_auto_opt_des_par.m_is_recomp_ok"
			"is either 0 (simple cycle only) or 1 (recomp allowed)\n"));
	}

	// map 'auto_opt_des_par_in' to 'ms_auto_opt_des_par'
	ms_opt_des_par.m_W_dot_net = ms_auto_opt_des_par.m_W_dot_net;
	ms_opt_des_par.m_T_mc1_in = ms_auto_opt_des_par.m_T_mc1_in;
	ms_opt_des_par.m_T_mc2_in = ms_auto_opt_des_par.m_T_mc2_in;
	ms_opt_des_par.m_T_mt_in = ms_auto_opt_des_par.m_T_mt_in;
	ms_opt_des_par.m_T_rt_in = ms_auto_opt_des_par.m_T_rt_in;
	ms_opt_des_par.m_DP_LT = ms_auto_opt_des_par.m_DP_LT;
	ms_opt_des_par.m_DP_HT = ms_auto_opt_des_par.m_DP_HT;
	ms_opt_des_par.m_DP_PC1 = ms_auto_opt_des_par.m_DP_PC1;
	ms_opt_des_par.m_DP_PC2 = ms_auto_opt_des_par.m_DP_PC2;
	ms_opt_des_par.m_DP_PHX = ms_auto_opt_des_par.m_DP_PHX;
	ms_opt_des_par.m_DP_RHX = ms_auto_opt_des_par.m_DP_RHX;
	ms_opt_des_par.m_LT_eff_max = ms_auto_opt_des_par.m_LT_eff_max;
	ms_opt_des_par.m_HT_eff_max = ms_auto_opt_des_par.m_HT_eff_max;
	ms_opt_des_par.m_UA_rec_total = ms_auto_opt_des_par.m_UA_rec_total;
	ms_opt_des_par.m_eta_mc1 = ms_auto_opt_des_par.m_eta_mc1;
	ms_opt_des_par.m_eta_mc2 = ms_auto_opt_des_par.m_eta_mc2;
	ms_opt_des_par.m_eta_rc = ms_auto_opt_des_par.m_eta_rc;
	ms_opt_des_par.m_eta_mt = ms_auto_opt_des_par.m_eta_mt;
	ms_opt_des_par.m_eta_rt = ms_auto_opt_des_par.m_eta_rt;
	ms_opt_des_par.m_N_sub_hxrs = ms_auto_opt_des_par.m_N_sub_hxrs;
	ms_opt_des_par.m_P_high_limit = ms_auto_opt_des_par.m_P_high_limit;
	ms_opt_des_par.m_tol = ms_auto_opt_des_par.m_tol;
	ms_opt_des_par.m_opt_tol = ms_auto_opt_des_par.m_opt_tol;
	ms_opt_des_par.m_N_turbine = ms_auto_opt_des_par.m_N_turbine;

	// Outer optimization loop
	m_eta_thermal_auto_opt = 0.0;

	double best_P_high = fminbr(
		ms_auto_opt_des_par.m_P_high_limit*0.9999, ms_auto_opt_des_par.m_P_high_limit, &fmin_opt_eta_1_RCMCI_with_ReHeating, this, 1.0);

	//double best_P_high = ms_auto_opt_des_par.m_P_high_limit;

	// Check model with P_mc_out set at P_high_limit for a recompression and simple cycle and use the better configuration
	double PR_mc2_guess = ms_des_par_auto_opt.m_P_mc2_out / ms_des_par_auto_opt.m_P_mc2_in;

	if (ms_auto_opt_des_par.m_is_recomp_ok)
	{
		// Complete 'ms_opt_des_par' for recompression cycle
		ms_opt_des_par.m_P_mc2_out_guess = ms_auto_opt_des_par.m_P_high_limit;
		ms_opt_des_par.m_fixed_P_mc2_out = true;

		//ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;
		//ms_opt_des_par.m_fixed_PR_mc = false;
		ms_opt_des_par.m_fixed_PR_mc2 = ms_auto_opt_des_par.m_fixed_PR_mc2;	//[-]
		if (ms_opt_des_par.m_fixed_PR_mc2)
		{
			ms_opt_des_par.m_PR_mc2_guess = ms_auto_opt_des_par.m_PR_mc2_guess;	//[-]
		}
		else
		{
			ms_opt_des_par.m_PR_mc2_guess = PR_mc2_guess;		//[-]
		}

		ms_opt_des_par.m_P_rt_in_guess = 11000.0;
		ms_opt_des_par.m_fixed_P_rt_in_guess = false;
		ms_opt_des_par.m_P_mc1_in_guess = 7400;
		ms_opt_des_par.m_fixed_P_mc1_in = false;
		ms_opt_des_par.m_recomp_frac_guess = 0.3;
		ms_opt_des_par.m_fixed_recomp_frac = false;
		ms_opt_des_par.m_LT_frac_guess = 0.5;
		ms_opt_des_par.m_fixed_LT_frac = false;

		int rc_error_code = 0;

		opt_design_core(rc_error_code);

		if (rc_error_code == 0 && m_eta_thermal_opt > m_eta_thermal_auto_opt)
		{
			ms_des_par_auto_opt = ms_des_par_optimal;
			m_eta_thermal_auto_opt = m_eta_thermal_opt;
		}
	}

	// Complete 'ms_opt_des_par' for SIMPLE Brayton Cycle
	ms_opt_des_par.m_P_mc2_out_guess = ms_auto_opt_des_par.m_P_high_limit;
	ms_opt_des_par.m_fixed_P_mc2_out = true;

	//ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;
	//ms_opt_des_par.m_fixed_PR_mc = false;
	ms_opt_des_par.m_fixed_PR_mc2 = ms_auto_opt_des_par.m_fixed_PR_mc2;	//[-]
	if (ms_opt_des_par.m_fixed_PR_mc2)
	{
		ms_opt_des_par.m_PR_mc2_guess = ms_auto_opt_des_par.m_PR_mc2_guess;	//[-]
	}
	else
	{
		ms_opt_des_par.m_PR_mc2_guess = PR_mc2_guess;		//[-]
	}

	ms_opt_des_par.m_P_rt_in_guess = 11000.0;
	ms_opt_des_par.m_fixed_P_rt_in_guess = false;
	ms_opt_des_par.m_P_mc1_in_guess = 7400;
	ms_opt_des_par.m_fixed_P_mc1_in = false;
	ms_opt_des_par.m_recomp_frac_guess = 0.0;
	ms_opt_des_par.m_fixed_recomp_frac = true;
	ms_opt_des_par.m_LT_frac_guess = 1.0;
	ms_opt_des_par.m_fixed_LT_frac = true;

	int s_error_code = 0;

	opt_design_core(s_error_code);

	if (s_error_code == 0 && m_eta_thermal_opt > m_eta_thermal_auto_opt)
	{
		ms_des_par_auto_opt = ms_des_par_optimal;
		m_eta_thermal_auto_opt = m_eta_thermal_opt;
	}

	ms_des_par = ms_des_par_auto_opt;

	int optimal_design_error_code = 0;
	design_core(optimal_design_error_code);

	if (optimal_design_error_code != 0)
	{
		error_code = optimal_design_error_code;
		return;
	}

	finalize_design(optimal_design_error_code);

	error_code = optimal_design_error_code;
}

//
double fmin_opt_eta_1_RCMCI_with_ReHeating(double x, void *data)
{
	C_RecompCycle_RCMCI_with_ReHeating *frame = static_cast<C_RecompCycle_RCMCI_with_ReHeating*>(data);

	return frame->opt_eta_RCMCI_with_ReHeating(x);
}


//
double C_RecompCycle_RCMCI_with_ReHeating::opt_eta_RCMCI_with_ReHeating(double P_high_opt)
{
	double PR_mc2_guess = 1.1;
	if (P_high_opt > P_pseudocritical_1_RCMCI_with_ReHeating(ms_opt_des_par.m_T_mc2_in))
		PR_mc2_guess = P_high_opt / P_pseudocritical_1_RCMCI_with_ReHeating(ms_opt_des_par.m_T_mc2_in);

	double local_eta_rc = 0.0;
	if (ms_auto_opt_des_par.m_is_recomp_ok)
	{
		// Complete 'ms_opt_des_par' for recompression cycle
		ms_opt_des_par.m_P_mc2_out_guess = P_high_opt;
		ms_opt_des_par.m_fixed_P_mc2_out = true;

		//ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;
		//ms_opt_des_par.m_fixed_PR_mc = false;
		ms_opt_des_par.m_fixed_PR_mc2 = ms_auto_opt_des_par.m_fixed_PR_mc2;	//[-]
		if (ms_opt_des_par.m_fixed_PR_mc2)
		{
			ms_opt_des_par.m_PR_mc2_guess = ms_auto_opt_des_par.m_PR_mc2_guess;	//[-]
		}
		else
		{
			ms_opt_des_par.m_PR_mc2_guess = PR_mc2_guess;		//[-]
		}

		ms_opt_des_par.m_P_rt_in_guess = 11000.0;
		ms_opt_des_par.m_fixed_P_rt_in_guess = false;
		ms_opt_des_par.m_P_mc1_in_guess = 7400;
		ms_opt_des_par.m_fixed_P_mc1_in = false;
		ms_opt_des_par.m_recomp_frac_guess = 0.3;
		ms_opt_des_par.m_fixed_recomp_frac = false;
		ms_opt_des_par.m_LT_frac_guess = 0.5;
		ms_opt_des_par.m_fixed_LT_frac = false;

		int rc_error_code = 0;
		opt_design_core(rc_error_code);

		if (rc_error_code == 0)
			local_eta_rc = m_eta_thermal_opt;

		if (rc_error_code == 0 && m_eta_thermal_opt > m_eta_thermal_auto_opt)
		{
			ms_des_par_auto_opt = ms_des_par_optimal;
			m_eta_thermal_auto_opt = m_eta_thermal_opt;
		}
	}

	// Complete 'ms_opt_des_par' for simple cycle
	ms_opt_des_par.m_P_mc2_out_guess = P_high_opt;
	ms_opt_des_par.m_fixed_P_mc2_out = true;

	//ms_opt_des_par.m_PR_mc_guess = PR_mc_guess;
	//ms_opt_des_par.m_fixed_PR_mc = false;
	ms_opt_des_par.m_fixed_PR_mc2 = ms_auto_opt_des_par.m_fixed_PR_mc2;	//[-]
	if (ms_opt_des_par.m_fixed_PR_mc2)
	{
		ms_opt_des_par.m_PR_mc2_guess = ms_auto_opt_des_par.m_PR_mc2_guess;	//[-]
	}
	else
	{
		ms_opt_des_par.m_PR_mc2_guess = PR_mc2_guess;		//[-]
	}

	ms_opt_des_par.m_P_rt_in_guess = 11000.0;
	ms_opt_des_par.m_fixed_P_rt_in_guess = false;
	ms_opt_des_par.m_P_mc1_in_guess = 7400;
	ms_opt_des_par.m_fixed_P_mc1_in = false;
	ms_opt_des_par.m_recomp_frac_guess = 0.0;
	ms_opt_des_par.m_fixed_recomp_frac = true;
	ms_opt_des_par.m_LT_frac_guess = 1.0;
	ms_opt_des_par.m_fixed_LT_frac = true;

	int s_error_code = 0;
	opt_design_core(s_error_code);

	double local_eta_s = 0.0;
	if (s_error_code == 0)
		local_eta_s = m_eta_thermal_opt;

	if (s_error_code == 0 && m_eta_thermal_opt > m_eta_thermal_auto_opt)
	{
		ms_des_par_auto_opt = ms_des_par_optimal;
		m_eta_thermal_auto_opt = m_eta_thermal_opt;
	}

	return -max(local_eta_rc, local_eta_s);
}


//Calculate the Critical Pressure given the Critical Temperature.
double P_pseudocritical_1_RCMCI_with_ReHeating(double T_K)
{
	return (0.191448*T_K + 45.6661)*T_K - 24213.3;
}

//
void C_RecompCycle_RCMCI_with_ReHeating::auto_opt_design_hit_eta(S_auto_opt_design_hit_eta_parameters & auto_opt_des_hit_eta_in, int & error_code, string & error_msg)
{
	ms_auto_opt_des_par.m_W_dot_net = auto_opt_des_hit_eta_in.m_W_dot_net;				//[kW] Target net cycle power
	ms_auto_opt_des_par.m_T_mc1_in = auto_opt_des_hit_eta_in.m_T_mc1_in;				//[K] Compressor1 inlet temperature
	ms_auto_opt_des_par.m_T_mc2_in = auto_opt_des_hit_eta_in.m_T_mc2_in;				//[K] Compressor2 inlet temperature
	ms_auto_opt_des_par.m_T_mt_in = auto_opt_des_hit_eta_in.m_T_mt_in;					//[K] Main Turbine inlet temperature
	ms_auto_opt_des_par.m_T_rt_in = auto_opt_des_hit_eta_in.m_T_rt_in;					//[K] ReHeating Turbine inlet temperature
	ms_auto_opt_des_par.m_DP_LT = auto_opt_des_hit_eta_in.m_DP_LT;						//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_DP_HT = auto_opt_des_hit_eta_in.m_DP_HT;						//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_DP_PC1 = auto_opt_des_hit_eta_in.m_DP_PC1;						//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_DP_PC2 = auto_opt_des_hit_eta_in.m_DP_PC2;						//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_DP_PHX = auto_opt_des_hit_eta_in.m_DP_PHX;					//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_DP_RHX = auto_opt_des_hit_eta_in.m_DP_RHX;					//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_UA_rec_total = std::numeric_limits<double>::quiet_NaN();		// ***** This method finds the UA required to hit the input efficiency! *****
	ms_auto_opt_des_par.m_LT_eff_max = auto_opt_des_hit_eta_in.m_LT_eff_max;
	ms_auto_opt_des_par.m_HT_eff_max = auto_opt_des_hit_eta_in.m_HT_eff_max;
	ms_auto_opt_des_par.m_eta_mc1 = auto_opt_des_hit_eta_in.m_eta_mc1;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
	ms_auto_opt_des_par.m_eta_mc2 = auto_opt_des_hit_eta_in.m_eta_mc2;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
	ms_auto_opt_des_par.m_eta_rc = auto_opt_des_hit_eta_in.m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
	ms_auto_opt_des_par.m_eta_mt = auto_opt_des_hit_eta_in.m_eta_mt;					    //[-] design-point efficiency of the main turbine; isentropic if positive, polytropic if negative
	ms_auto_opt_des_par.m_eta_rt = auto_opt_des_hit_eta_in.m_eta_rt;					    //[-] design-point efficiency of the main turbine; isentropic if positive, polytropic if negative
	ms_auto_opt_des_par.m_N_sub_hxrs = auto_opt_des_hit_eta_in.m_N_sub_hxrs;			//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
	ms_auto_opt_des_par.m_P_high_limit = auto_opt_des_hit_eta_in.m_P_high_limit;		//[kPa] maximum allowable pressure in cycle
	ms_auto_opt_des_par.m_tol = auto_opt_des_hit_eta_in.m_tol;							//[-] Convergence tolerance
	ms_auto_opt_des_par.m_opt_tol = auto_opt_des_hit_eta_in.m_opt_tol;					//[-] Optimization tolerance
	ms_auto_opt_des_par.m_N_turbine = auto_opt_des_hit_eta_in.m_N_turbine;				//[rpm] Turbine shaft speed (negative values link turbine to compressor)
	ms_auto_opt_des_par.m_is_recomp_ok = auto_opt_des_hit_eta_in.m_is_recomp_ok;		//[-] 1 = yes, 0 = no, other = invalid

	ms_auto_opt_des_par.mf_callback_log = auto_opt_des_hit_eta_in.mf_callback_log;
	ms_auto_opt_des_par.mp_mf_active = auto_opt_des_hit_eta_in.mp_mf_active;

	ms_auto_opt_des_par.m_PR_mc2_guess = auto_opt_des_hit_eta_in.m_PR_mc2_guess;			//[-] Initial guess for ratio of P_mc_out to P_mc_in
	ms_auto_opt_des_par.m_fixed_PR_mc2 = auto_opt_des_hit_eta_in.m_fixed_PR_mc2;			//[-] if true, ratio of P_mc_out to P_mc_in is fixed at PR_mc_guess		

																							// At this point, 'auto_opt_des_hit_eta_in' should only be used to access the targer thermal efficiency: 'm_eta_thermal'

	double Q_dot_rec_des = ms_auto_opt_des_par.m_W_dot_net / auto_opt_des_hit_eta_in.m_eta_thermal;		//[kWt] Receiver thermal input at design

	error_msg = "";
	error_code = 0;

	// Check cycle parameter values are reasonable
	if (ms_auto_opt_des_par.m_is_recomp_ok != 0 && ms_auto_opt_des_par.m_is_recomp_ok != 1)
	{
		throw(C_csp_exception("C_RecompCycle::auto_opt_design_core(...) requires that ms_auto_opt_des_par.m_is_recomp_ok"
			"is either 0 (simple cycle only) or 1 (recomp allowed)\n"));
	}
	// Can't operate compressore1 in 2-phase region
	if (ms_auto_opt_des_par.m_T_mc1_in <= N_co2_props::T_crit)
	{
		error_msg.append(util::format("Only single phase cycle operation is allowed in this model."
			"The compressor inlet temperature (%lg [C]) must be great than the critical temperature: %lg [C]",
			ms_auto_opt_des_par.m_T_mc1_in - 273.15, ((N_co2_props::T_crit) - 273.15)));

		error_code = -1;
		return;
	}

	// "Reasonable" ceiling on compressor inlet temp
	double T_mc1_in_max = 70.0 + 273.15;		//[K] Arbitrary value for max compressor inlet temperature
	if (ms_auto_opt_des_par.m_T_mc1_in > T_mc1_in_max)
	{
		error_msg.append(util::format("The compressor inlet temperature input was %lg [C]. This value was reset internally to the max allowable inlet temperature: %lg [C]\n",
			ms_auto_opt_des_par.m_T_mc1_in - 273.15, T_mc1_in_max - 273.15));

		ms_auto_opt_des_par.m_T_mc1_in = T_mc1_in_max;
	}

	// "Reasonable" floor on Main Turbine inlet temp
	double T_mt_in_min = 300.0 + 273.15;		//[K] Arbitrary value for min Main turbine inlet temperature
	if (ms_auto_opt_des_par.m_T_mt_in < T_mt_in_min)
	{
		error_msg.append(util::format("The main turbine inlet temperature input was %lg [C]. This value was reset internally to the min allowable inlet temperature: %lg [C]\n",
			ms_auto_opt_des_par.m_T_mt_in - 273.15, T_mt_in_min - 273.15));

		ms_auto_opt_des_par.m_T_mt_in = T_mt_in_min;
	}

	// Main Turbine inlet temperature must be hotter than compressor outlet temperature
	if (ms_auto_opt_des_par.m_T_mt_in <= ms_auto_opt_des_par.m_T_mc2_in)
	{
		error_msg.append(util::format("The main turbine inlet temperature, %lg [C], is colder than the specified compressor inlet temperature %lg [C]",
			ms_auto_opt_des_par.m_T_mt_in - 273.15, ms_auto_opt_des_par.m_T_mc2_in - 273.15));

		error_code = -1;
		return;
	}

	// Main Turbine inlet temperature must be colder than property limits
	if (ms_auto_opt_des_par.m_T_mt_in >= N_co2_props::T_upper_limit)
	{
		error_msg.append(util::format("The main turbine inlet temperature, %lg [C], is hotter than the maximum allow temperature in the CO2 property code %lg [C]",
			ms_auto_opt_des_par.m_T_mt_in - 273.15, N_co2_props::T_upper_limit - 273.15));

		error_code = -1;
		return;
	}

	// "Reasonable" floor on ReHeating Turbine inlet temp
	double T_rt_in_min = 300.0 + 273.15;		//[K] Arbitrary value for min Main turbine inlet temperature
	if (ms_auto_opt_des_par.m_T_rt_in < T_rt_in_min)
	{
		error_msg.append(util::format("The ReHeating turbine inlet temperature input was %lg [C]. This value was reset internally to the min allowable inlet temperature: %lg [C]\n",
			ms_auto_opt_des_par.m_T_rt_in - 273.15, T_rt_in_min - 273.15));

		ms_auto_opt_des_par.m_T_rt_in = T_rt_in_min;
	}

	// ReHeating Turbine inlet temperature must be hotter than compressor outlet temperature
	if (ms_auto_opt_des_par.m_T_rt_in <= ms_auto_opt_des_par.m_T_mc2_in)
	{
		error_msg.append(util::format("The ReHeating turbine inlet temperature, %lg [C], is colder than the specified compressor inlet temperature %lg [C]",
			ms_auto_opt_des_par.m_T_rt_in - 273.15, ms_auto_opt_des_par.m_T_mc2_in - 273.15));

		error_code = -1;
		return;
	}

	// ReHeating Turbine inlet temperature must be colder than property limits
	if (ms_auto_opt_des_par.m_T_rt_in >= N_co2_props::T_upper_limit)
	{
		error_msg.append(util::format("The ReHeating turbine inlet temperature, %lg [C], is hotter than the maximum allow temperature in the CO2 property code %lg [C]",
			ms_auto_opt_des_par.m_T_rt_in - 273.15, N_co2_props::T_upper_limit - 273.15));

		error_code = -1;
		return;
	}

	// Check for realistic isentropic efficiencies
	if (ms_auto_opt_des_par.m_eta_mc1 > 1.0)
	{
		error_msg.append(util::format("The main compressor isentropic efficiency, %lg, was reset to theoretical maximum 1.0\n",
			ms_auto_opt_des_par.m_eta_mc1));

		ms_auto_opt_des_par.m_eta_mc1 = 1.0;
	}
	if (ms_auto_opt_des_par.m_eta_rc > 1.0)
	{
		error_msg.append(util::format("The re-compressor isentropic efficiency, %lg, was reset to theoretical maximum 1.0\n",
			ms_auto_opt_des_par.m_eta_rc));

		ms_auto_opt_des_par.m_eta_rc = 1.0;
	}
	if (ms_auto_opt_des_par.m_eta_mt > 1.0)
	{
		error_msg.append(util::format("The main turbine isentropic efficiency, %lg, was reset to theoretical maximum 1.0\n",
			ms_auto_opt_des_par.m_eta_mt));

		ms_auto_opt_des_par.m_eta_mt = 1.0;
	}
	if (ms_auto_opt_des_par.m_eta_rt > 1.0)
	{
		error_msg.append(util::format("The reheating turbine isentropic efficiency, %lg, was reset to theoretical maximum 1.0\n",
			ms_auto_opt_des_par.m_eta_rt));

		ms_auto_opt_des_par.m_eta_rt = 1.0;
	}
	if (ms_auto_opt_des_par.m_eta_mc1 < 0.1)
	{
		error_msg.append(util::format("The main compressor isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability\n",
			ms_auto_opt_des_par.m_eta_mc1));

		ms_auto_opt_des_par.m_eta_mc1 = 0.1;
	}
	if (ms_auto_opt_des_par.m_eta_rc < 0.1)
	{
		error_msg.append(util::format("The re-compressor isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability\n",
			ms_auto_opt_des_par.m_eta_rc));

		ms_auto_opt_des_par.m_eta_rc = 0.1;
	}
	if (ms_auto_opt_des_par.m_eta_mt < 0.1)
	{
		error_msg.append(util::format("The main turbine isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability\n",
			ms_auto_opt_des_par.m_eta_mt));

		ms_auto_opt_des_par.m_eta_mt = 0.1;
	}
	if (ms_auto_opt_des_par.m_eta_rt < 0.1)
	{
		error_msg.append(util::format("The reheating turbine isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability\n",
			ms_auto_opt_des_par.m_eta_rt));

		ms_auto_opt_des_par.m_eta_rt = 0.1;
	}
	if (ms_auto_opt_des_par.m_LT_eff_max > 1.0)
	{
		error_msg.append(util::format("The LT recuperator max effectiveness, %lg, was decreased to the limit of 1.0\n", ms_auto_opt_des_par.m_LT_eff_max));

		ms_auto_opt_des_par.m_LT_eff_max = 1.0;
	}

	if (ms_auto_opt_des_par.m_LT_eff_max < 0.70)
	{
		error_msg.append(util::format("The LT recuperator max effectiveness, %lg, was increased to the internal limit of 0.70 improve convergence\n", ms_auto_opt_des_par.m_LT_eff_max));

		ms_auto_opt_des_par.m_LT_eff_max = 0.7;
	}

	if (ms_auto_opt_des_par.m_HT_eff_max > 1.0)
	{
		error_msg.append(util::format("The HT recuperator max effectiveness, %lg, was decreased to the limit of 1.0\n", ms_auto_opt_des_par.m_HT_eff_max));

		ms_auto_opt_des_par.m_HT_eff_max = 1.0;
	}

	if (ms_auto_opt_des_par.m_HT_eff_max < 0.70)
	{
		error_msg.append(util::format("The LT recuperator max effectiveness, %lg, was increased to the internal limit of 0.70 improve convergence\n", ms_auto_opt_des_par.m_HT_eff_max));

		ms_auto_opt_des_par.m_HT_eff_max = 0.7;
	}

	// Limits on high pressure limit
	if (ms_auto_opt_des_par.m_P_high_limit >= N_co2_props::P_upper_limit)
	{
		error_msg.append(util::format("The upper pressure limit, %lg [MPa], was set to the internal limit in the CO2 properties code %lg [MPa]\n",
			ms_auto_opt_des_par.m_P_high_limit, N_co2_props::P_upper_limit));

		ms_auto_opt_des_par.m_P_high_limit = N_co2_props::P_upper_limit;
	}
	double P_high_limit_min = 10.0*1.E3;	//[kPa]
	if (ms_auto_opt_des_par.m_P_high_limit <= P_high_limit_min)
	{
		error_msg.append(util::format("The upper pressure limit, %lg [MPa], must be greater than %lg [MPa] to ensure solution stability",
			ms_auto_opt_des_par.m_P_high_limit, P_high_limit_min));

		error_code = -1;
		return;
	}

	// Finally, check thermal efficiency
	if (auto_opt_des_hit_eta_in.m_eta_thermal <= 0.0)
	{
		error_msg.append(util::format("The design cycle thermal efficiency, %lg, must be at least greater than 0 ",
			auto_opt_des_hit_eta_in.m_eta_thermal));

		error_code = -1;
		return;
	}
	double eta_carnot = 1.0 - ms_auto_opt_des_par.m_T_mc1_in / ms_auto_opt_des_par.m_T_mt_in;
	if (auto_opt_des_hit_eta_in.m_eta_thermal >= eta_carnot)
	{
		error_msg.append(util::format("To solve the cycle within the allowable recuperator conductance, the design cycle thermal efficiency, %lg, must be at least less than the Carnot efficiency: %lg ",
			auto_opt_des_hit_eta_in.m_eta_thermal, eta_carnot));

		error_code = -1;
		return;
	}

	// Send log update upstream
	if (ms_auto_opt_des_par.mf_callback_log && ms_auto_opt_des_par.mp_mf_active)
	{
		std::string msg_log = "Iterate on total recuperator conductance to hit target cycle efficiency";
		std::string msg_progress = "Designing cycle...";
		if (!ms_auto_opt_des_par.mf_callback_log(msg_log, msg_progress, ms_auto_opt_des_par.mp_mf_active, 0.0, 2))
		{
			std::string error_msg = "User terminated simulation...";
			std::string loc_msg = "C_MEQ_sco2_design_hit_eta__UA_total";
			throw(C_csp_exception(error_msg, loc_msg, 1));
		}
	}

	// Set up monotonic equation solver to find the total recuperator UA that results in the target efficiency
	C_MEQ_sco2_design_hit_eta__UA_total c_eq(this);
	C_monotonic_eq_solver c_solver(c_eq);

	// Generate min and max values
	double UA_recup_total_max = ms_des_limits.m_UA_net_power_ratio_max*ms_auto_opt_des_par.m_W_dot_net;		//[kW/K]
	double UA_recup_total_min = ms_des_limits.m_UA_net_power_ratio_min*ms_auto_opt_des_par.m_W_dot_net;		//[kW/K]
																											// Set solver settings
	c_solver.settings(ms_auto_opt_des_par.m_tol, 50, UA_recup_total_min, UA_recup_total_max, true);

	// Generate guess values
	double UA_recups_guess = 0.1*ms_auto_opt_des_par.m_W_dot_net;

	double UA_recup_total_solved, tol_solved;
	UA_recup_total_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int solver_code = 0;
	try
	{
		solver_code = c_solver.solve(UA_recups_guess, 1.1*UA_recups_guess, auto_opt_des_hit_eta_in.m_eta_thermal,
			UA_recup_total_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception &csp_except)
	{
		if (csp_except.m_error_code == 1)
		{
			throw(C_csp_exception(csp_except));
		}
		else
		{
			throw(C_csp_exception("C_MEQ_sco2_design_hit_eta__UA_total received an exception from the solver"));
		}
	}

	if (solver_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (solver_code < C_monotonic_eq_solver::CONVERGED)
		{
			error_msg.append("Can't find a value of total recuperator conductance that achieves"
				" the target cycle efficiency. Check design parameters.");
		}
		else if (!c_solver.did_solver_find_negative_error(solver_code))
		{
			error_msg.append("Can't find a value of total recuperator conductance that results"
				" in an efficiency smaller than the target efficiency.");
		}
		else if (!c_solver.did_solver_find_positive_error(solver_code))
		{
			error_msg.append("Can't find a value of total recuperator conductance that results"
				" in an efficiency larger than the target efficiency.");
		}
		else
		{
			error_msg.append("Can't find a value of total recuperator conductance that achieves"
				" the target cycle efficiency. Check design parameters.");
		}

		error_code = -1;

		return;
	}

}
//
int C_RecompCycle_RCMCI_with_ReHeating::C_MEQ_sco2_design_hit_eta__UA_total::operator()(double UA_recup_total /*kW/K*/, double *eta /*-*/)
{
	mpc_rc_cycle->ms_auto_opt_des_par.m_UA_rec_total = UA_recup_total;	//[kW/K]

	int error_code = 0;
	mpc_rc_cycle->auto_opt_design_core(error_code);
	if (error_code != 0)
	{
		*eta = std::numeric_limits<double>::quiet_NaN();
		return -1;
	}

	*eta = mpc_rc_cycle->get_design_solved()->m_eta_thermal;	//[-]

	if (mpc_rc_cycle->ms_auto_opt_des_par.mf_callback_log && mpc_rc_cycle->ms_auto_opt_des_par.mp_mf_active)
	{
		msg_log = util::format(" Total recuperator conductance = %lg [kW/K]. Optimized cycle efficiency = %lg [-].  ",
			UA_recup_total, *eta);
		if (!mpc_rc_cycle->ms_auto_opt_des_par.mf_callback_log(msg_log, msg_progress, mpc_rc_cycle->ms_auto_opt_des_par.mp_mf_active, 0.0, 2))
		{
			std::string error_msg = "User terminated simulation...";
			std::string loc_msg = "C_MEQ_sco2_design_hit_eta__UA_total";
			throw(C_csp_exception(error_msg, loc_msg, 1));
		}
	}

	return 0;
}

double nlopt_callback_poly_coefs_RCMCI_with_ReHeating(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_poly_curve_r_squared_RCMCI_with_ReHeating *frame = static_cast<C_poly_curve_r_squared_RCMCI_with_ReHeating*>(data);
	if (frame != NULL)
		return frame->calc_r_squared(x);
	else
		return 0.0;
}

bool C_poly_curve_r_squared_RCMCI_with_ReHeating::init(const std::vector<double> x_data, const std::vector<double> y_data)
{
	m_x = x_data;
	m_y = y_data;

	m_n_points = (int)x_data.size();
	if (m_n_points != y_data.size() || m_n_points < 5)
	{
		return false;
	}

	m_y_bar = 0.0;

	for (int i = 0; i < m_n_points; i++)
	{
		m_y_bar += m_y[i];
	}

	m_y_bar /= (double)m_n_points;

	m_SS_tot = 0.0;

	for (int i = 0; i < m_n_points; i++)
	{
		m_SS_tot += pow(m_y[i] - m_y_bar, 2);
	}

	return true;
}

double C_poly_curve_r_squared_RCMCI_with_ReHeating::calc_r_squared(const std::vector<double> coefs)
{
	double SS_res = 0.0;
	int n_coefs = (int)coefs.size();
	double y_pred = 0.0;
	for (int i = 0; i < m_n_points; i++)
	{
		y_pred = 0.0;
		for (int j = 0; j < n_coefs; j++)
		{
			y_pred += coefs[j] * pow(m_x[i], j);
		}
		SS_res += pow(m_y[i] - y_pred, 2);
	}

	return 1.0 - SS_res / m_SS_tot;
}

double fmin_callback_opt_eta_1_RCMCI_with_ReHeating(double x, void *data)
{
	C_RecompCycle_RCMCI_with_ReHeating *frame = static_cast<C_RecompCycle_RCMCI_with_ReHeating*>(data);

	return frame->opt_eta_RCMCI_with_ReHeating(x);
}


