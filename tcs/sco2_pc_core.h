#ifndef __SCO2_PC_CORE_
#define __SCO2_PC_CORE_

#include <limits>
#include <vector>
#include <algorithm>
#include <math.h>
#include "CO2_properties.h"

using namespace std;

// 'General' Core Routines: Not class methods and don't require pointers to or instances of classes
void calculate_turbomachinery_outlet_1(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double eta /*-*/, bool is_comp, int & error_code, double & enth_in /*kJ/kg*/, double & entr_in /*kJ/kg-K*/,
	double & dens_in /*kg/m3*/, double & temp_out /*K*/, double & enth_out /*kJ/kg*/, double & entr_out /*kJ/kg-K*/, double & dens_out /*kg/m3*/, double & spec_work /*kJ/kg*/);

void calculate_hxr_UA_1(int N_hxrs, double Q_dot /*units?*/, double m_dot_c, double m_dot_h, double T_c_in, double T_h_in, double P_c_in, double P_c_out, double P_h_in, double P_h_out,
	int & error_code, double & UA, double & min_DT);

void isen_eta_from_poly_eta(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double poly_eta /*-*/, bool is_comp, int & error_code, double & isen_eta);

// Heat Exchanger Class
class C_HeatExchanger
{
public:
	struct S_design_parameters
	{
		int m_N_sub;							//[-] Number of sub-heat exchangers used in the model
		std::vector<double> m_m_dot_design;		//[kg/s] Design-point mass flow rates of the two streams
		std::vector<double> m_DP_design;		//[kPa] Design-point pressure drops across the heat exchanger
		double m_UA_design;						//[kW/K] Design-point conductance
		double m_Q_dot_design;					//[kW] Design-point heat transfer
		double m_min_DT_design;					//[K] Minimum temperature difference in heat exchanger
		double m_eff_design;					//[-] Effectiveness at design

		S_design_parameters()
		{
			m_N_sub = -1;

			m_m_dot_design.resize(2);
			std::fill(m_m_dot_design.begin(), m_m_dot_design.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_design.resize(2);
			std::fill(m_DP_design.begin(), m_DP_design.end(), std::numeric_limits<double>::quiet_NaN());

			m_UA_design = m_Q_dot_design = m_min_DT_design = m_eff_design = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:
	S_design_parameters ms_des_par;

public:
	~C_HeatExchanger(){};

	C_HeatExchanger(){};

	void initialize(const S_design_parameters & des_par_in)
	{
		ms_des_par = des_par_in;
		return;
	}

	// Performance Methods:
		// *Some check to ensure member structures are initialized?*
	void hxr_pressure_drops(const std::vector<double> & m_dots, std::vector<double> & hxr_deltaP)
	{
		int N = m_dots.size();
		hxr_deltaP.resize(N);
		for( int i = 0; i < N; i++ )
			hxr_deltaP[i] = ms_des_par.m_DP_design[i]*pow( (m_dots[i]/ms_des_par.m_m_dot_design[i]), 1.75 );
	}

	void hxr_conductance(const std::vector<double> & m_dots, double & hxr_UA)
	{
		int N = m_dots.size();
		double m_dot_ratio = 0.5*(m_dots[0]/ms_des_par.m_m_dot_design[0] + m_dots[1]/ms_des_par.m_m_dot_design[1]);
		hxr_UA = ms_des_par.m_UA_design*pow(m_dot_ratio, 0.8);
	}

};

class C_turbine
{
public:
	struct S_design_parameters
	{
		double m_N_design;					//[rpm] turbine shaft speed
		double m_N_comp_design_if_linked;	//[rpm] compressor shaft speed
			// Compressor inlet state
		double m_P_in;						//[kPa]
		double m_T_in;						//[K] 
		double m_D_in;						//[kg/m^3] 
		double m_h_in;						//[kJ/kg]
		double m_s_in;						//[kJ/kg-K]
		double m_ssnd_in;					//[m/s] Speed of sound
			// Compressor outlet pressure
		double m_P_out;						//[kPa]
		double m_h_out;						//[kJ/kg]
			// Mass flow rate
		double m_m_dot;						//[kg/s]
		
		S_design_parameters()
		{
			m_N_design = m_N_comp_design_if_linked =
			m_P_in = m_T_in = m_D_in = m_h_in = m_s_in = m_ssnd_in = m_P_out = m_h_out =
			m_m_dot = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_design_solved
	{
		double m_nu_design;					//[-] ratio of tip speed to spouting velocity
		double m_D_rotor;					//[m] Turbine diameter
		double m_A_nozzle;					//[m^2] Effective nozzle area
		double m_w_tip_ratio;				//[-] ratio of tip speed to local speed of sound
		double m_eta;						//[-] 

		S_design_solved()
		{
			m_nu_design = m_D_rotor = m_A_nozzle = m_w_tip_ratio = m_eta = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_solved
	{
		double m_nu;						//[-] ratio of tip speed to spouting velocity
		double m_eta;						//[-] turbine efficiency
		double m_w_tip_ratio;				//[-] ratio of the tip speed to the local (turbine inlet) speed of sound
		double m_N;							//[rpm] off-design turbine shaft speed

		S_od_solved()
		{
			m_nu = m_eta = m_w_tip_ratio = m_N = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:
	S_design_parameters ms_des_par;
	S_design_solved ms_des_solved;
	S_od_solved ms_od_solved;

public:
	~C_turbine(){};

	C_turbine(){};

	static const double m_nu_design;
	
	

	void turbine_sizing(const S_design_parameters & des_par_in, int & error_code)
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
			ms_des_par.m_N_design = ms_des_par.m_N_comp_design_if_linked;
			if( ms_des_par.m_N_design <= 0.0 )
			{
				error_code = 7;
				return;
			}
		}

		// Outlet specific enthalpy after isentropic expansion
		int prop_error_code = CO2_PS(ms_des_par.m_P_out, ms_des_par.m_s_in, &co2_props);
		if(prop_error_code != 0)
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
		ms_des_solved.m_D_rotor = U_tip/(0.5*ms_des_par.m_N_design*0.104719755);	//[m]
		ms_des_solved.m_A_nozzle = ms_des_par.m_m_dot/(C_s*ms_des_par.m_D_in);		//[m^2]

		// Set other turbine variables
		ms_des_solved.m_w_tip_ratio = U_tip / ms_des_par.m_ssnd_in;				//[-]
		ms_des_solved.m_eta = (ms_des_par.m_h_in - ms_des_par.m_h_out) / w_i;	//[-] Isentropic efficiency
	}

	void off_design_turbine(double T_in, double P_in, double P_out, double N, int & error_code, double & m_dot, double & T_out)
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
		if(prop_error_code != 0)
		{
			error_code = prop_error_code;
			return;
		}
		double D_in = co2_props.dens;
		double h_in = co2_props.enth;
		double s_in = co2_props.entr;
		double ssnd_in = co2_props.ssnd;

		prop_error_code = CO2_PS(P_out, s_in, &co2_props);
		if(prop_error_code != 0)
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
		if(prop_error_code != 0)
		{
			error_code = prop_error_code;
			return;
		}
		T_out = co2_props.temp;

		m_dot = C_s*ms_des_solved.m_A_nozzle*D_in;			//[kg/s] Mass flow rate through turbine
		ms_od_solved.m_w_tip_ratio = U_tip / ssnd_in;		//[-] Ratio of the tip speed to the local (turbine inlet) speed of sound
		ms_od_solved.m_N = N;
	}

};

class C_compressor
{
public:
	struct S_design_parameters
	{
			// Compressor inlet conditions
		double m_D_in;			//[kg/m^3]
		double m_h_in;			//[kJ/kg]
		double m_s_in;			//[kJ/kg-K]
			// Compressor outlet conditions
		double m_T_out;			//[K]
		double m_P_out;			//[kPa]
		double m_h_out;			//[kJ/kg]
		double m_D_out;			//[kg/m^3]
		double m_ssnd_out;		//[m/s]
			// Mass flow
		double m_m_dot;			//[kg/s]

		S_design_parameters()
		{
			m_D_in = m_h_in = m_s_in =
			m_T_out = m_P_out = m_h_out = m_D_out = m_ssnd_out = std::numeric_limits<double>::quiet_NaN();
		}
	};
	struct S_design_solved
	{
		double m_D_rotor;		//[m]
		double m_N_design;		//[rpm]
		double m_w_tip_ratio;	//[-]
		double m_eta_design;	//[-]

		S_design_solved()
		{
			m_D_rotor = m_N_design = m_w_tip_ratio = m_eta_design = std::numeric_limits<double>::quiet_NaN();
		}
	};
	struct S_od_solved
	{
		bool m_surge;			//[-]
		double m_eta;			//[-]
		double m_phi;			//[-]
		double m_w_tip_ratio;	//[-]

		S_od_solved()
		{
			m_surge = false;
			m_eta = m_phi = m_w_tip_ratio = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:
	S_design_parameters ms_des_par;
	S_design_solved ms_des_solved;
	S_od_solved ms_od_solved;

public:
	~C_compressor(){};

	C_compressor(){};

	static const double m_snl_phi_design;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
	static const double m_snl_phi_min;				//[-] Approximate surge limit for SNL compressor
	static const double m_snl_phi_max;				//[-] Approximate x-intercept for SNL compressor

	const S_design_solved * get_design_solved()
	{
		return &ms_des_solved;
	}

	void compressor_sizing(const S_design_parameters & des_par_in, int & error_code)
	{
		ms_des_par = des_par_in;

		CO2_state co2_props;

		int prop_error_code = CO2_PS(ms_des_par.m_P_out, ms_des_par.m_s_in, &co2_props);
		if(prop_error_code != 0)
		{
			error_code = prop_error_code;
			return;
		}
		double h_s_out = co2_props.enth;

		// Calculate psi at the design-point phi using Horner's method
		double psi_design = ((((-498626.0*m_snl_phi_design) + 53224.0) * m_snl_phi_design - 2505.0) * m_snl_phi_design + 54.6) *
			m_snl_phi_design + 0.04049;		// from dimensionless modified head curve(at design - point, psi and modified psi are equal)

		// Determine required size and speed of compressor
		double w_i = h_s_out - ms_des_par.m_h_in;		//[kJ/kg] positive isentropic specific work of compressor
		double U_tip = sqrt(1000.0*w_i/psi_design);		//[m/s]
		ms_des_solved.m_D_rotor = sqrt(ms_des_par.m_m_dot/(m_snl_phi_design*ms_des_par.m_D_in*U_tip));
		double N_rad_s = U_tip*2.0/ms_des_solved.m_D_rotor;		//[rad/s] shaft speed
		ms_des_solved.m_N_design = N_rad_s * 9.549296590;			//[rpm] shaft speed

		// Set other compressor member data
		ms_des_solved.m_w_tip_ratio = U_tip / ms_des_par.m_ssnd_out;
		ms_des_solved.m_eta_design = w_i / (ms_des_par.m_h_out - ms_des_par.m_h_in);
	}

	void off_design_compressor(double T_in, double P_in, double m_dot, double N, int & error_code, double & T_out, double & P_out)
	{
		CO2_state co2_props;

		// Fully define the inlet state of the compressor
		int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
		if(prop_error_code != 0)
		{
			error_code = 1;
			return;
		}
		double rho_in = co2_props.dens;
		double h_in = co2_props.enth;
		double s_in = co2_props.entr;

		// Calculate the modified flow and head coefficients and efficiency for the SNL compressor
		double U_tip = ms_des_solved.m_D_rotor*0.5*N*0.104719755;				//[m/s]
		double phi = m_dot / (rho_in*U_tip*pow(ms_des_solved.m_D_rotor, 2));	//[-]
		if( phi < m_snl_phi_min )
		{
			ms_od_solved.m_surge = true;
			phi = m_snl_phi_min;
		}
		else
			ms_od_solved.m_surge = false;

		double phi_star = phi*pow(N / ms_des_solved.m_N_design, 0.2);		//[-] modified flow coefficient
		double psi_star = ((((-498626.0*phi_star) + 53224.0)*phi_star - 2505.0)*phi_star + 54.6)*phi_star + 0.04049;	// from dimensionless modified head curve
		double eta_star = ((((-1.638e6*phi_star) + 182725.0)*phi_star - 8089.0)*phi_star + 168.6)*phi_star - 0.7069;	// from dimensionless modified efficiency curve
		double psi = psi_star / pow(ms_des_solved.m_N_design / N, pow(20.0*phi_star, 3.0));
		double eta_0 = eta_star*1.47528 / pow(ms_des_solved.m_N_design / N, pow(20.0*phi_star, 5.0));		// Efficiency is normalized so it equals 1.0 at snl_phi_design
		ms_od_solved.m_eta = max(eta_0*ms_des_solved.m_eta_design, 0.0);		//[-] Actual compressor efficiency, not allowed to go negative

		// Check that the specified mass flow rate is possible with the compressor's current shaft speed
		if(psi <= 0.0)
		{
			error_code = 1;
			return;		
		}

		// Calculate the compressor outlet state
		double dh_s = psi * pow(U_tip, 2.0) * 0.001;			//[kJ/kg] Ideal enthalpy rise in compressor, from definition of head coefficient
		double dh = dh_s / ms_od_solved.m_eta;					//[kJ/kg] Actual enthalpy rise in compressor
		double h_s_out = h_in + dh_s;							//[kJ/kg] Ideal enthalpy at compressor outlet
		double h_out = h_in + dh;								//[kJ/kg] Actual enthalpy at compressor outlet

		// Get the compressor outlet pressure
		prop_error_code = CO2_HS(h_s_out, s_in, &co2_props);
		if(prop_error_code != 0)
		{
			error_code = 2;
			return;
		}
		P_out = co2_props.pres;

		// Determine compressor outlet temperature and speed of sound
		prop_error_code = CO2_PH(P_out, h_out, &co2_props);
		if(prop_error_code != 0)
		{
			error_code = 2;
			return;
		}
		T_out = co2_props.temp;
		double ssnd_out = co2_props.ssnd;

		// Set a few compressor variables
		ms_od_solved.m_phi = phi;
		ms_od_solved.m_w_tip_ratio = U_tip / ssnd_out;	
	}

};


class C_recompressor
{
public:
	struct S_design_parameters
	{
			// Compressor inlet conditions
		double m_P_in;
		double m_D_in;
		double m_h_in;
		double m_s_in;
			// Compressor outlet conditions
		double m_T_out;
		double m_P_out;
		double m_h_out;
		double m_D_out;
			// Flow conditions
		double m_m_dot;

		S_design_parameters()
		{
			m_P_in = m_D_in = m_h_in = m_s_in = 
			m_T_out = m_P_out = m_h_out = m_D_out =
			m_m_dot = std::numeric_limits<double>::quiet_NaN();
		}
	};
	struct S_design_solved
	{
		double m_D_rotor;
		double m_D_rotor_2;
		double m_N_design;
		double m_eta_design;
		double m_surge;

		S_design_solved()
		{
			m_D_rotor = m_D_rotor_2 = m_N_design = m_eta_design = std::numeric_limits<double>::quiet_NaN();
			m_surge = false;
		}
	};

private:
	S_design_parameters ms_des_par;
	S_design_solved ms_des_solved;

public:
	~C_recompressor(){};

	C_recompressor(){};

	static const double m_snl_phi_design;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
	static const double m_snl_phi_min;				//[-] Approximate surge limit for SNL compressor
	static const double m_snl_phi_max;				//[-] Approximate x-intercept for SNL compressor

	void recompressor_sizing(const S_design_parameters & des_par_in, int & error_code)
	{
		CO2_state co2_props;

		int prop_error_code = CO2_TD(ms_des_par.m_T_out, ms_des_par.m_D_out, &co2_props);
		if(prop_error_code != 0)
		{
			error_code = prop_error_code;
			return;
		}
		double ssnd_out = co2_props.ssnd;

		// Ideal specific enthalpy after compression
		prop_error_code = CO2_PS(ms_des_par.m_P_out, ms_des_par.m_s_in, &co2_props);
		if(prop_error_code != 0)
		{
			error_code = prop_error_code;
			return;
		}
		double h_s_out = co2_props.enth;

		double eta_design = (h_s_out - ms_des_par.m_h_in) / (ms_des_par.m_h_out - ms_des_par.m_h_in);		//[-] overall isentropic efficiency
		double psi_design = ((((-498626.0*m_snl_phi_design) + 53224.0)*m_snl_phi_design - 2505.0)*m_snl_phi_design + 54.6)*m_snl_phi_design + 0.04049;

		// Prepare intermediate pressure iteration loop
		double last_residual = 0.0;
		double last_P_int = 1.E12;			// Ensures bisection will be used for first loop
		double lower_bound = ms_des_par.m_P_in + 1.E-6;
		double upper_bound = ms_des_par.m_P_out - 1.E-6;
		double P_int = 0.5*(lower_bound + upper_bound);
		double eta_stage = eta_design;	// First guess for stage efficiency
		int max_iter = 100;
		double tolerance = 1.0E-8;

		double D_rotor_1 = -999.9;
		double D_rotor_2 = -999.9;
		double N_design = -999.9;

		double end_stage = -999.9;
		int i = -1;
		for( i = 0; i < max_iter; i++ )
		{
			// First stage
				// Ideal outlet specific enthalpy after first stage
			prop_error_code = CO2_PS(P_int, ms_des_par.m_s_in, &co2_props);
			if(prop_error_code != 0)
			{
				error_code = prop_error_code;
				return;
			}
			double h_s_out = co2_props.enth;

			double w_i = h_s_out - ms_des_par.m_h_in;				//[kJ/kg] positive isentropic specific work of first stage
			double U_tip_1 = sqrt(1000.0 * w_i / psi_design);		//[m/s]
			D_rotor_1 = sqrt(ms_des_par.m_m_dot/(m_snl_phi_design*ms_des_par.m_D_in*U_tip_1));	//[m]
			double N_rad_s = U_tip_1*2.0 / D_rotor_1;				//[rad/s]
			N_design = N_rad_s*9.549296590;					//[rpm]
			double w = w_i / eta_stage;								//[kJ/kg]
			double h_int = ms_des_par.m_h_in + w;					//[kJ/kg] Energy balance on first stage
		
			prop_error_code = CO2_PH(P_int, h_int, &co2_props);
			if(prop_error_code != 0)
			{
				error_code = prop_error_code;
				return;
			}
			double D_int = co2_props.dens;
			double s_int = co2_props.entr;
			double ssnd_int = co2_props.ssnd;

			// Second stage
			prop_error_code = CO2_PS(ms_des_par.m_P_out, s_int, &co2_props);
			if(prop_error_code != 0)
			{
				error_code = prop_error_code;
				return;
			}

			w_i = h_s_out - h_int;			// positive isentropic specific work of second stage
			double U_tip_2 = sqrt(1000.0*w_i/psi_design);
			D_rotor_2 = 2.0*U_tip_2 / (N_design*0.104719755);
			double phi = ms_des_par.m_m_dot/(D_int*U_tip_2*pow(D_rotor_2,2));	//[-] Flow coefficient
			double eta_2_req = w_i/(ms_des_par.m_h_out - h_int);				//[-] Required second stage efficiency to achieve overall eta_design

			// Check convergence and update guesses
			double residual = m_snl_phi_design - phi;
			if(residual < 0.0)		// P_int guess is too high
			{
				if(-residual <= tolerance && fabs(eta_stage-eta_2_req) <= tolerance)
					break;
				upper_bound = P_int;
			}
			else					// P_int guess is too low
			{
				if( -residual <= tolerance && fabs(eta_stage - eta_2_req) <= tolerance )
					break;
				lower_bound = P_int;
			}

			double secant_step = -residual*(last_P_int - P_int) / (last_residual - residual);
			double P_secant = P_int + secant_step;
			last_P_int = P_int;
			last_residual = residual;
			if( P_secant <= lower_bound || P_secant >= upper_bound )		// secant method overshot
				P_int = 0.5*(lower_bound + upper_bound);
			else if(fabs(secant_step) > fabs(0.5*(upper_bound-lower_bound)))	// take the smaller step to ensure convergence
				P_int = 0.5*(lower_bound + upper_bound);
			else
				P_int = P_secant;		// Use secant guess

			end_stage = 0.5*(eta_stage + eta_2_req);		// Update guess for stage efficiency
		}

	// Check for convergence
	if(i == max_iter)		// did  not converge
	{
		error_code = 1;
		return;
	}

	// Set recompressor variables
	ms_des_solved.m_D_rotor = D_rotor_1;
	ms_des_solved.m_D_rotor_2 = D_rotor_2;
	ms_des_solved.m_eta_design = end_stage;
	ms_des_solved.m_N_design = N_design;
	ms_des_solved.m_surge = false;

	}


};



#endif
