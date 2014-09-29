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
			// Turbine inlet state
		double m_P_in;						//[kPa]
		double m_T_in;						//[K] 
		double m_D_in;						//[kg/m^3] 
		double m_h_in;						//[kJ/kg]
		double m_s_in;						//[kJ/kg-K]
			// Turbine outlet state
		double m_P_out;						//[kPa]
		double m_h_out;						//[kJ/kg]
			// Mass flow rate
		double m_m_dot;						//[kg/s]
		
		S_design_parameters()
		{
			m_N_design = m_N_comp_design_if_linked =
			m_P_in = m_T_in = m_D_in = m_h_in = m_s_in = m_P_out = m_h_out =
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
		double m_N_design;					//[rpm] shaft speed

		S_design_solved()
		{
			m_nu_design = m_D_rotor = m_A_nozzle = m_w_tip_ratio = m_eta = m_N_design = std::numeric_limits<double>::quiet_NaN();
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
	
	const S_design_solved * get_design_solved()
	{
		return &ms_des_solved;
	}

	const S_od_solved * get_od_solved()
	{
		return &ms_od_solved;
	}

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
		if(prop_error_code != 0)
		{
			error_code = prop_error_code;
			return;
		}
		double ssnd_in = co2_props.ssnd;

		// Outlet specific enthalpy after isentropic expansion
		prop_error_code = CO2_PS(ms_des_par.m_P_out, ms_des_par.m_s_in, &co2_props);
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
		ms_des_solved.m_D_rotor = U_tip/(0.5*ms_des_solved.m_N_design*0.104719755);	//[m]
		ms_des_solved.m_A_nozzle = ms_des_par.m_m_dot/(C_s*ms_des_par.m_D_in);		//[m^2]

		// Set other turbine variables
		ms_des_solved.m_w_tip_ratio = U_tip / ssnd_in;				//[-]
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
			// Mass flow
		double m_m_dot;			//[kg/s]

		S_design_parameters()
		{
			m_D_in = m_h_in = m_s_in =
			m_T_out = m_P_out = m_h_out = m_D_out = std::numeric_limits<double>::quiet_NaN();
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

	const S_od_solved * get_od_solved()
	{
		return &ms_od_solved;
	}

	void compressor_sizing(const S_design_parameters & des_par_in, int & error_code)
	{
		ms_des_par = des_par_in;

		CO2_state co2_props;

		int prop_error_code = CO2_TD(ms_des_par.m_T_out, ms_des_par.m_D_out, &co2_props);
		if(prop_error_code != 0)
		{
			error_code = prop_error_code;
			return;
		}
		double ssnd_out = co2_props.ssnd;

		prop_error_code = CO2_PS(ms_des_par.m_P_out, ms_des_par.m_s_in, &co2_props);
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
		ms_des_solved.m_w_tip_ratio = U_tip / ssnd_out;
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

		S_design_solved()
		{
			m_D_rotor = m_D_rotor_2 = m_N_design = m_eta_design = std::numeric_limits<double>::quiet_NaN();
		}
	};
	struct S_od_solved
	{
		double m_N;
		double m_eta;
		double m_phi;
		double m_phi_2;
		double m_w_tip_ratio;
		bool m_surge;

		S_od_solved()
		{
			m_N = m_eta = m_phi = m_phi_2 = m_w_tip_ratio = std::numeric_limits<double>::quiet_NaN();
			m_surge = false;
		}
	};

private:
	S_design_parameters ms_des_par;
	S_design_solved ms_des_solved;
	S_od_solved ms_od_solved;

public:
	~C_recompressor(){};

	C_recompressor(){};

	static const double m_snl_phi_design;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
	static const double m_snl_phi_min;				//[-] Approximate surge limit for SNL compressor
	static const double m_snl_phi_max;				//[-] Approximate x-intercept for SNL compressor

	const S_od_solved * get_od_solved()
	{
		return &ms_od_solved;
	}

	void recompressor_sizing(const S_design_parameters & des_par_in, int & error_code)
	{
		ms_des_par = des_par_in;

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
			h_s_out = co2_props.enth;

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

			eta_stage = 0.5*(eta_stage + eta_2_req);		// Update guess for stage efficiency
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
	ms_des_solved.m_eta_design = eta_stage;
	ms_des_solved.m_N_design = N_design;

	}

	void off_design_recompressor(double T_in, double P_in, double m_dot, double P_out, int & error_code, double & T_out)
	{
		/* code from John Dyreby, converted to C++ by Ty Neises
		! Solve for the outlet state (and shaft speed) of 'comp' given its inlet conditions, mass flow rate, and outlet pressure.
		!
		! Inputs:
		!   comp -- a Compressor object, with design-point values and sizing set
		!   T_in -- compressor inlet temperature (K)
		!   P_in -- compressor inlet pressure (kPa)
		!   m_dot -- mass flow rate through compressor (kg/s)
		!   P_out -- compressor outlet pressure (kPa)
		!
		! Outputs:
		!   error_trace -- an ErrorTrace object
		!   T_out -- compressor outlet temperature (K)
		!
		! Notes:
		!   1) This subroutine also sets the following values in 'comp': N, surge, eta, w, w_tip_ratio, phi
		!   2) In order to solve the compressor, the value for flow coefficient (phi) is varied until convergence.
		!   3) Surge is not allowed; if the corresponding flow coefficient is not between phi_min and phi_max an error is raised.
		!   4) Two-stage recompressor; surge is true if either stages are in surge conditions; w_tip_ratio is max of the two stages.
		*/

		CO2_state co2_props;
		
		// Fully define the inlet state of the compressor
		int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
		if( prop_error_code != 0 )
		{
			error_code = prop_error_code;
			return;
		}
		double rho_in = co2_props.dens;
		double h_in = co2_props.enth;
		double s_in = co2_props.entr;

		// Iterate on first-stage phi
		double phi_1 = m_snl_phi_design;		// start with design-point value
		bool first_pass = true;
		
		int max_iter = 100;
		double rel_tol = 1.0E-9;

		double last_phi_1 = std::numeric_limits<double>::quiet_NaN();
		double last_residual = std::numeric_limits<double>::quiet_NaN();
		double P_out_calc = std::numeric_limits<double>::quiet_NaN();
		double h_out = std::numeric_limits<double>::quiet_NaN();
		double N = std::numeric_limits<double>::quiet_NaN();
		double phi_2 = std::numeric_limits<double>::quiet_NaN();
		double U_tip_1 = std::numeric_limits<double>::quiet_NaN();
		double ssnd_int = std::numeric_limits<double>::quiet_NaN();
		double U_tip_2 = std::numeric_limits<double>::quiet_NaN();

		int i = -1;
		for( i = 0; i < max_iter; i++ )
		{
			// First stage - dh_s and eta_stage_1
			U_tip_1 = m_dot / (phi_1*rho_in*pow(ms_des_solved.m_D_rotor, 2));		//[m/s]
			N = (U_tip_1*2.0 / ms_des_solved.m_D_rotor)*9.549296590;					//[rpm] shaft spped
			double phi_star = phi_1*pow( (N/ms_des_solved.m_N_design), 0.2);				//[-] Modified flow coefficient
			double psi_star = ((((-498626.0*phi_star) + 53224.0)*phi_star - 2505.0)*phi_star + 54.6)*phi_star + 0.04049;		//[-] from dimensionless modified head curve
			double psi = psi_star/( pow(ms_des_solved.m_N_design/N,pow(20.0*phi_star,3.0)));
			double dh_s = psi*pow(U_tip_1, 2)*0.001;										//[kJ/kg] Calculated ideal enthalpy rise in first stage of compressor
			double eta_star = ((((-1.638e6*phi_star) + 182725.0)*phi_star - 8089.0)*phi_star + 168.6)*phi_star - 0.7069;		//[-] from dimensionless modified efficiency curve
			double eta_0 = eta_star*1.47528/pow( (ms_des_solved.m_N_design/N), pow(20.0*phi_star,5) );		//[-] Stage efficiency is normalized so it equals 1.0 at snl_phi_design
			double eta_stage_1 = max(eta_0*ms_des_solved.m_eta_design, 0.0);				//[-] The actual stage efficiency, not allowed to go negative

			// Calculate first - stage outlet (second - stage inlet) state
			double dh = dh_s / eta_stage_1;		//[kJ/kg] Actual enthalpy rise in first stage
			double h_s_out = h_in + dh_s;		//[kJ/kg] Ideal enthalpy between stages
			double h_int = h_in + dh;			//[kJ/kg] Actual enthalpy between stages

			prop_error_code = CO2_HS(h_s_out, s_in, &co2_props);
			if(prop_error_code != 0)
			{
				error_code = prop_error_code;
				return;
			}
			double P_int = co2_props.pres;

			prop_error_code = CO2_PH(P_int, h_int, &co2_props);
			if(prop_error_code != 0)
			{
				error_code = prop_error_code;
				return;
			}
			double D_int = co2_props.dens;
			double s_int = co2_props.entr;
			ssnd_int = co2_props.ssnd;

			// Second stage - dh_s and eta_stage_2
			U_tip_2 = ms_des_solved.m_D_rotor_2*0.5*N*0.104719755;				// second-stage tip speed in m/s
			phi_2 = m_dot / (D_int*U_tip_2*pow(ms_des_solved.m_D_rotor_2, 2));	// second-stage flow coefficient
			phi_star = phi_2*pow((N / ms_des_solved.m_N_design), 0.2);					// modified flow coefficient
			psi_star = ((((-498626.0*phi_star) + 53224.0)*phi_star - 2505.0)*phi_star + 54.6)*phi_star + 0.04049;	// from dimensionless modified head curve
			psi = psi_star / (pow(ms_des_solved.m_N_design / N, pow(20.0*phi_star, 3.0)));
			dh_s = psi*pow(U_tip_2, 2)*0.001;									// [kJ/kg] Calculated ideal enthalpy rise in second stage of compressor, from definition of head coefficient
			eta_star = ((((-1.638e6*phi_star) + 182725.0)*phi_star - 8089.0)*phi_star + 168.6)*phi_star - 0.7069;		//[-] from dimensionless modified efficiency curve
			eta_0 = eta_star*1.47528 / pow((ms_des_solved.m_N_design / N), pow(20.0*phi_star, 5));		//[-] Stage efficiency is normalized so it equals 1.0 at snl_phi_design
			double eta_stage_2 = max(eta_0*ms_des_solved.m_eta_design, 0.0);			// the actual stage efficiency, not allowed to go negative

			// Calculate second-stage outlet state
			dh = dh_s / eta_stage_2;			// actual enthalpy rise in second stage
			h_s_out = h_int + dh_s;				// ideal enthalpy at compressor outlet
			h_out = h_int + dh;					// actual enthalpy at compressor outlet

			// Get the calculated compressor outlet pressure
			prop_error_code = CO2_HS(h_s_out, s_int, &co2_props);
			if(prop_error_code != 0)
			{
				error_code = prop_error_code;
				return;
			}
			P_out_calc = co2_props.pres;

			// Check for convergence and adjust phi_1 guess
			double residual = P_out - P_out_calc;
			if( fabs(residual) / P_out <= rel_tol )
				break;
			
			double next_phi = std::numeric_limits<double>::quiet_NaN();			
			if( first_pass )
			{
				next_phi = phi_1*1.0001;		// take a small step
				first_pass = false;
			}
			else
				next_phi = phi_1 - residual*(last_phi_1 - phi_1) / (last_residual - residual);		// next guess predicted using secant method

			last_phi_1 = phi_1;
			last_residual = residual;
			phi_1 = next_phi;
		}

		// Check for convergence
		if(i == max_iter)		// did not converge
		{
			error_code = 1;
			return;
		}

		// Determine outlet temperature and speed of sound
		prop_error_code = CO2_PH(P_out_calc, h_out, &co2_props);
		if( prop_error_code != 0 )
		{
			error_code = prop_error_code;
			return;
		}
		T_out = co2_props.temp;
		double ssnd_out = co2_props.ssnd;

		// Outlet specific enthalpy after isentropic compression
		prop_error_code = CO2_PS(P_out_calc, s_in, &co2_props);
		if(prop_error_code != 0)
		{
			error_code = prop_error_code;
			return;
		}
		double h_s_out = co2_props.enth;

		// Set relevant recompressor variables
		ms_od_solved.m_N = N;
		ms_od_solved.m_eta = (h_s_out - h_in) / (h_out - h_in);		// use overall isentropic efficiency
		ms_od_solved.m_phi = phi_1;
		ms_od_solved.m_phi_2 = phi_2;
		ms_od_solved.m_w_tip_ratio = max(U_tip_1/ssnd_int, U_tip_2/ssnd_out);	// store ratio
		ms_od_solved.m_surge = phi_1 < m_snl_phi_min || phi_2 < m_snl_phi_min;		
	}
};

class C_RecompCycle
{
public:
	struct S_design_parameters
	{
		double m_W_dot_net;					//[kW] Target net cycle power
		double m_T_mc_in;					//[K] Compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		double m_P_mc_in;					//[kPa] Compressor inlet pressure
		double m_P_mc_out;					//[kPa] Compressor outlet pressure
		std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		double m_UA_LT;						//[kW/K] UA in LTR
		double m_UA_HT;						//[kW/K] UA in HTR
		double m_recomp_frac;				//[-] Fraction of flow that bypasses the precooler and the main compressor at the design point
		double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
		double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_tol;						//[-] Convergence tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)

		S_design_parameters()
		{
			m_W_dot_net = m_T_mc_in = m_T_t_in = m_P_mc_in = m_P_mc_out = m_UA_LT = m_UA_HT = m_recomp_frac = m_eta_mc = m_eta_rc = m_eta_t = m_P_high_limit = m_tol = m_N_turbine = std::numeric_limits<double>::quiet_NaN();
			m_N_sub_hxrs = -1;

			m_DP_LT.resize(2);
			std::fill(m_DP_LT.begin(), m_DP_LT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HT.resize(2);
			std::fill(m_DP_HT.begin(), m_DP_HT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC.resize(2);
			std::fill(m_DP_PC.begin(), m_DP_PC.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};

	struct S_opt_design_parameters
	{
		double m_W_dot_net;					//[kW] Target net cycle power
		double m_T_mc_in;					//[K] Compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		double m_UA_rec_total;				//[kW/K] Total design-point recuperator UA
		double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
		double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_tol;						//[-] Convergence tolerance
		double m_opt_tol;					//[-] Optimization tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)

		double m_P_mc_out_guess;			//[kPa] Initial guess for main compressor outlet pressure
		bool m_fixed_P_mc_out;				//[-] if true, P_mc_out is fixed at P_mc_out_guess
		
		double m_PR_mc_guess;				//[-] Initial guess for ratio of P_mc_out to P_mc_in
		bool m_fixed_PR_mc;					//[-] if true, ratio of P_mc_out to P_mc_in is fixed at PR_mc_guess

		double m_recomp_frac_guess;			//[-] Initial guess for design-point recompression fraction
		bool m_fixed_recomp_frac;			//[-] if true, recomp_frac is fixed at recomp_frac_guess

		double m_LT_frac_guess;				//[-] Initial guess for fraction of UA_rec_total that is in the low-temperature recuperator
		bool m_fixed_LT_frac;				//[-] if true, LT_frac is fixed at LT_frac_guess

		S_opt_design_parameters()
		{
			m_W_dot_net = m_T_mc_in = m_T_t_in = m_UA_rec_total = m_eta_mc = m_eta_rc = m_eta_t = m_P_high_limit = m_tol = m_opt_tol = m_N_turbine =
				m_P_mc_out_guess = m_PR_mc_guess = m_recomp_frac_guess = m_LT_frac_guess = std::numeric_limits<double>::quiet_NaN();

			m_N_sub_hxrs = -1;

			m_DP_LT.resize(2);
			std::fill(m_DP_LT.begin(), m_DP_LT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HT.resize(2);
			std::fill(m_DP_HT.begin(), m_DP_HT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC.resize(2);
			std::fill(m_DP_PC.begin(), m_DP_PC.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};

	struct S_auto_opt_design_parameters
	{
		double m_W_dot_net;					//[kW] Target net cycle power
		double m_T_mc_in;					//[K] Compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		double m_UA_rec_total;				//[kW/K] Total design-point recuperator UA
		double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
		double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_tol;						//[-] Convergence tolerance
		double m_opt_tol;					//[-] Optimization tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)

		S_auto_opt_design_parameters()
		{
			m_W_dot_net = m_T_mc_in = m_T_t_in = m_UA_rec_total = m_eta_mc = m_eta_rc = m_eta_t = m_P_high_limit = m_tol = m_opt_tol = m_N_turbine = std::numeric_limits<double>::quiet_NaN();

			m_N_sub_hxrs = -1;

			m_DP_LT.resize(2);
			std::fill(m_DP_LT.begin(), m_DP_LT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HT.resize(2);
			std::fill(m_DP_HT.begin(), m_DP_HT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC.resize(2);
			std::fill(m_DP_PC.begin(), m_DP_PC.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};

	struct S_design_solved
	{
		std::vector<double> m_temp, m_pres, m_enth, m_entr, m_dens;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
		double m_eta_thermal;
		double m_W_dot_net;
		double m_m_dot_mc;
		double m_m_dot_rc;
		double m_m_dot_t;
		double m_recomp_frac;
		double m_N_mc;
		double m_N_t;

		bool m_is_rc;

		S_design_solved()
		{
			m_eta_thermal = m_W_dot_net = m_m_dot_mc = m_m_dot_rc = m_m_dot_t = m_recomp_frac = m_N_mc = m_N_t = std::numeric_limits<double>::quiet_NaN();

			m_is_rc = true;
		}
	};

	struct S_od_solved
	{
		std::vector<double> m_temp, m_pres, m_enth, m_entr, m_dens;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
		double m_eta_thermal;
		double m_W_dot_net;
		double m_Q_dot;
		double m_m_dot_mc;
		double m_m_dot_rc;
		double m_m_dot_t;
		double m_recomp_frac;
		double m_N_mc;
		double m_N_t;

		S_od_solved()
		{
			m_eta_thermal = m_W_dot_net = m_Q_dot = m_m_dot_mc = m_m_dot_rc = m_m_dot_t = m_recomp_frac = m_N_mc = m_N_t = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_parameters
	{
		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature
		double m_P_mc_in;		//[kPa] Compressor inlet pressure
		double m_recomp_frac;	//[-] Fraction of flow that bypasses the precooler and main compressor
		double m_N_mc;			//[rpm] Main compressor shaft speed
		double m_N_t;			//[rpm] Turbine shaft speed
		int m_N_sub_hxrs;		//[-] Number of sub heat exchangers
		double m_tol;			//[-] Convergence tolerance

		S_od_parameters()
		{
			m_T_mc_in = m_T_t_in = m_P_mc_in = m_recomp_frac = m_N_mc = m_N_t = m_tol = std::numeric_limits<double>::quiet_NaN();
			m_N_sub_hxrs = -1;
		}
	};

	struct S_opt_od_parameters
	{
		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature

		bool m_is_max_W_dot;	//[-] Value to maximize: true = W_dot, false = eta

		int m_N_sub_hxrs;		//[-] Number of sub heat exchangers

		double m_P_mc_in_guess;	//[kPa] Initial guess for P_mc_in when iterating to hit target
		bool m_fixed_P_mc_in;	//[-] if true, P_mc_in is fixed at P_mc_in_guess

		double m_recomp_frac_guess;		//[-] Initial guess for recompression fraction
		bool m_fixed_recomp_frac;		//[-] If true, recomp_frac is fixed at recomp_frac_guess

		double m_N_mc_guess;			//[rpm] Initial guess for main compressor shaft speed
		bool m_fixed_N_mc;				//[-]   If true, N_mc is fixed at N_mc_guess

		double m_N_t_guess;				//[rpm] Initial guess for turbine shaft speed (negative value links it to N_mc)
		bool m_fixed_N_t;				//[-]   If true, N_t is fixed at N_t_guess

		double m_tol;					//[-] Convergence tolerance
		double m_opt_tol;				//[-] Optimization convergence tolerance

		S_opt_od_parameters()
		{
			m_T_mc_in = m_T_t_in = m_P_mc_in_guess = m_recomp_frac_guess = m_N_mc_guess =
				m_N_t_guess = m_tol = m_opt_tol = std::numeric_limits<double>::quiet_NaN();

			m_N_sub_hxrs = -1;

			m_fixed_P_mc_in = m_fixed_recomp_frac = m_fixed_N_mc = m_fixed_N_t = false;
		}
	};

	struct S_target_od_parameters
	{
		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature
		double m_recomp_frac;	//[-] Fraction of flow that bypasses the precooler and main compressor
		double m_N_mc;			//[rpm] Main compressor shaft speed
		double m_N_t;			//[rpm] Turbine shaft speed
		int m_N_sub_hxrs;		//[-] Number of sub heat exchangers
		double m_tol;			//[-] Convergence tolerance
		
		double m_target;		//[kW] type of target: 1) W_dot 2) Q_dot_PHX
		bool m_is_target_Q;		//[-] true = solve for Q_dot_PHX, false = solve for W_dot

		double m_lowest_pressure;	//[-] lowest pressure to check
		double m_highest_pressure;	//[-] highest pressure to check

		S_target_od_parameters()
		{
			m_T_mc_in = m_T_t_in = m_recomp_frac = m_N_mc = m_N_t = m_tol =
				m_target = m_lowest_pressure = m_highest_pressure = std::numeric_limits<double>::quiet_NaN();

			m_is_target_Q = true;

			m_N_sub_hxrs = -1;
		}
	};

	struct S_opt_target_od_parameters
	{
		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature

		double m_target;		//[kW] target value
		bool m_is_target_Q;		//[-] true = solve for Q_dot_PHX, false = solve for W_dot

		int m_N_sub_hxrs;				//[-] Number of sub heat exchangers
		double m_lowest_pressure;		//[-] lowest pressure to check
		double m_highest_pressure;		//[-] highest pressure to check

		double m_recomp_frac_guess;		//[-] Initial guess for recompression fraction
		bool m_fixed_recomp_frac;		//[-] If true, recomp_frac is fixed at recomp_frac_guess

		double m_N_mc_guess;			//[rpm] Initial guess for main compressor shaft speed
		bool m_fixed_N_mc;				//[-]   If true, N_mc is fixed at N_mc_guess

		double m_N_t_guess;				//[rpm] Initial guess for turbine shaft speed (negative value links it to N_mc)
		bool m_fixed_N_t;				//[-]   If true, N_t is fixed at N_t_guess

		double m_tol;					//[-] Convergence tolerance
		double m_opt_tol;				//[-] Optimization convergence tolerance

		S_opt_target_od_parameters()
		{
			m_T_mc_in = m_T_t_in = m_target = m_lowest_pressure = m_highest_pressure = m_recomp_frac_guess =
				m_N_mc_guess = m_N_t_guess = m_tol = m_opt_tol = std::numeric_limits<double>::quiet_NaN();

			m_N_sub_hxrs = -1;
			
			m_is_target_Q = m_fixed_recomp_frac = m_fixed_N_mc = m_fixed_N_t = true;
		}
	};

private:
		// Component classes
	C_turbine m_t;
	C_compressor m_mc;
	C_recompressor m_rc;
	C_HeatExchanger m_LT, m_HT, m_PHX, m_PC;

		// Input/Ouput structures for class methods
	S_design_parameters ms_des_par;
	S_opt_design_parameters ms_opt_des_par;
	S_auto_opt_design_parameters ms_auto_opt_des_par;
	S_design_solved ms_des_solved;
	S_od_parameters ms_od_par;
	S_opt_od_parameters ms_opt_od_par;
	S_target_od_parameters ms_tar_od_par;
	S_opt_target_od_parameters ms_opt_tar_od_par;
	S_od_solved ms_od_solved;

		// Results from last 'design' solution
	std::vector<double> m_temp_last, m_pres_last, m_enth_last, m_entr_last, m_dens_last;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
	double m_eta_thermal_last;
	double m_W_dot_net_last;
	double m_m_dot_mc, m_m_dot_rc, m_m_dot_t;
	
		// Structures and data for optimization
	S_design_parameters ms_des_par_optimal;
	double m_eta_thermal_opt;

		// Structures and data for auto-optimization
	double m_eta_thermal_auto_opt;	
	S_design_parameters ms_des_par_auto_opt;

		// Results from last off-design solution
	std::vector<double> m_temp_od, m_pres_od, m_enth_od, m_entr_od, m_dens_od;					// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
	double m_eta_thermal_od;
	double m_W_dot_net_od;
	double m_Q_dot_PHX_od;

		// Structures and data for off-design optimization
	S_od_parameters ms_od_par_optimal;
	double m_W_dot_net_max;

		// Structures and data for optimal target off-design
	S_od_parameters ms_od_par_tar_optimal;
	double m_eta_best;

	void design_core(int & error_code);	

	void opt_design_core(int & error_code);

	void finalize_design(int & error_code);	

	void off_design_core(int & error_code);

	void optimal_off_design_core(int & error_code);

	void target_off_design_core(int & error_code);

public:

	C_RecompCycle()
	{
		m_temp_last.resize(10);
		std::fill(m_temp_last.begin(), m_temp_last.end(), std::numeric_limits<double>::quiet_NaN());
		
		m_pres_last = m_enth_last = m_entr_last = m_dens_last = m_temp_last;

		/*
		m_pres_last.resize(10);
		std::fill(m_pres_last.begin(), m_pres_last.end(), std::numeric_limits<double>::quiet_NaN());
		m_enth_last.resize(10);
		std::fill(m_enth_last.begin(), m_enth_last.end(), std::numeric_limits<double>::quiet_NaN());
		m_entr_last.resize(10);
		std::fill(m_entr_last.begin(), m_entr_last.end(), std::numeric_limits<double>::quiet_NaN());
		m_dens_last.resize(10);
		std::fill(m_dens_last.begin(), m_dens_last.end(), std::numeric_limits<double>::quiet_NaN());
		*/		

		m_eta_thermal_last = m_m_dot_mc = m_m_dot_rc = m_m_dot_t = std::numeric_limits<double>::quiet_NaN();
		m_W_dot_net_last = std::numeric_limits<double>::quiet_NaN();
			
		m_eta_thermal_opt = m_eta_thermal_opt = std::numeric_limits<double>::quiet_NaN();

		m_temp_od = m_pres_od = m_enth_od = m_entr_od = m_dens_od = m_temp_last;

		m_eta_thermal_od = m_W_dot_net_od = m_Q_dot_PHX_od = std::numeric_limits<double>::quiet_NaN();

		m_W_dot_net_max = m_eta_best = std::numeric_limits<double>::quiet_NaN();
	}

	~C_RecompCycle(){}

	void design(S_design_parameters & des_par_in, int & error_code);

	void opt_design(S_opt_design_parameters & opt_des_par_in, int & error_code);

	void auto_opt_design(S_auto_opt_design_parameters & auto_opt_des_par_in, int & error_code);

	void off_design(S_od_parameters & od_par_in, int & error_code);

	void optimal_off_design(S_opt_od_parameters & opt_od_par_in, int & error_code);

	void target_off_design(S_target_od_parameters & tar_od_par_in, int & error_code);

	void optimal_target_off_design(S_opt_target_od_parameters & opt_tar_od_par_in, int & error_code);

	const S_design_solved * get_design_solved()
	{
		return &ms_des_solved;
	}	

	const S_od_solved * get_od_solved()
	{
		return &ms_od_solved;
	}

	// Called by 'nlopt_callback_opt_des_1', so needs to be public
	double design_point_eta(const std::vector<double> &x);

	// Called by 'fmin_callback_opt_eta', so needs to be public
	double opt_eta(double P_high_opt);

	// Called by 'nlopt_cb_opt_od', so needs to be public
	double off_design_point_value(const std::vector<double> &x);

	// Called by 'nlopt...', so needs to be public
	double eta_at_target(const std::vector<double> &x);
	
};

double nlopt_callback_opt_des_1(const std::vector<double> &x, std::vector<double> &grad, void *data);

double fmin_callback_opt_eta_1(double x, void *data);

double nlopt_cb_opt_od(const std::vector<double> &x, std::vector<double> &grad, void *data);

double nlopt_cb_eta_at_target(const std::vector<double> &x, std::vector<double> &grad, void *data);

double P_pseudocritical_1(double T_K);

#endif
