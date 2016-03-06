#ifndef __SCO2_PC_CORE_
#define __SCO2_PC_CORE_

#include <limits>
#include <vector>
#include <algorithm>
#include <string>
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

	void initialize(const S_design_parameters & des_par_in);

	// Performance Methods:
		// *Some check to ensure member structures are initialized?*
	void hxr_pressure_drops(const std::vector<double> & m_dots, std::vector<double> & hxr_deltaP);

	void hxr_conductance(const std::vector<double> & m_dots, double & hxr_UA);

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

	void turbine_sizing(const S_design_parameters & des_par_in, int & error_code);	

	void off_design_turbine(double T_in, double P_in, double P_out, double N, int & error_code, double & m_dot, double & T_out);

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

	void compressor_sizing(const S_design_parameters & des_par_in, int & error_code);
	
	void off_design_compressor(double T_in, double P_in, double m_dot, double N, int & error_code, double & T_out, double & P_out);
	
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

	const S_design_solved * get_design_solved()
	{
		return &ms_des_solved;
	}

	const S_od_solved * get_od_solved()
	{
		return &ms_od_solved;
	}

	void recompressor_sizing(const S_design_parameters & des_par_in, int & error_code);	

	void off_design_recompressor(double T_in, double P_in, double m_dot, double P_out, int & error_code, double & T_out);
	
};

class C_RecompCycle
{
public:
	
	struct S_design_limits
	{
		double m_UA_net_power_ratio_max;		//[-/K]
		double m_UA_net_power_ratio_min;		//[-/K]

		double m_T_mc_in_min;					//[K]

		S_design_limits()
		{
			m_UA_net_power_ratio_max = m_UA_net_power_ratio_min = std::numeric_limits<double>::quiet_NaN();
		}
	};

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

	struct S_auto_opt_design_hit_eta_parameters
	{
		double m_W_dot_net;					//[kW] Target net cycle power
		double m_eta_thermal;				//[-] Cycle thermal efficiency
		double m_T_mc_in;					//[K] Compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
		double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_tol;						//[-] Convergence tolerance
		double m_opt_tol;					//[-] Optimization tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)
	
		S_auto_opt_design_hit_eta_parameters()
		{
			m_W_dot_net = m_T_mc_in = m_T_t_in = m_eta_mc = m_eta_rc = m_eta_t = m_P_high_limit = m_tol = m_opt_tol = m_N_turbine = std::numeric_limits<double>::quiet_NaN();

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
		double m_eta_thermal;	//[-]
		double m_W_dot_net;		//[kW]
		double m_m_dot_mc;
		double m_m_dot_rc;
		double m_m_dot_t;
		double m_recomp_frac;
		double m_UA_LT;
		double m_UA_HT;

		bool m_is_rc;

		C_compressor::S_design_solved ms_mc_des_solved;
		C_recompressor::S_design_solved ms_rc_des_solved;
		C_turbine::S_design_solved ms_t_des_solved;

		S_design_solved()
		{
			m_eta_thermal = m_W_dot_net = m_m_dot_mc = m_m_dot_rc = m_m_dot_t = m_recomp_frac = 
				m_UA_LT = m_UA_HT = std::numeric_limits<double>::quiet_NaN();

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
		bool m_use_default_res;		//[-] If true, use 20 intervals in pressure range
									// If q_target is close to q_max, use false apply 100 intervals

		S_target_od_parameters()
		{
			m_T_mc_in = m_T_t_in = m_recomp_frac = m_N_mc = m_N_t = m_tol =
				m_target = m_lowest_pressure = m_highest_pressure = std::numeric_limits<double>::quiet_NaN();

			m_is_target_Q = true;

			m_N_sub_hxrs = -1;

			m_use_default_res = true;
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

		bool m_use_default_res;		//[-] If true, use 20 intervals in pressure range
									// If q_target is close to q_max, use false apply 100 intervals

		S_opt_target_od_parameters()
		{
			m_T_mc_in = m_T_t_in = m_target = m_lowest_pressure = m_highest_pressure = m_recomp_frac_guess =
				m_N_mc_guess = m_N_t_guess = m_tol = m_opt_tol = std::numeric_limits<double>::quiet_NaN();

			m_N_sub_hxrs = -1;
			
			m_is_target_Q = m_fixed_recomp_frac = m_fixed_N_mc = m_fixed_N_t = true;

			m_use_default_res = true;
		}
	};

	struct S_PHX_od_parameters
	{
		double m_m_dot_htf_des;		//[kg/s] Design point htf mass flow rate
		
		double m_T_htf_hot;			//[K] Current htf inlet temperature
		double m_m_dot_htf;			//[kg/s] Current htf mass flow rate
		double m_T_htf_cold;		//[K] Target htf cold return temp

		double m_UA_PHX_des;		//[kW/K] Design point PHX conductance

		double m_cp_htf;			//[kW/K] Constant HTF specific heat

		S_PHX_od_parameters()
		{
			m_m_dot_htf_des = m_T_htf_hot = m_m_dot_htf = m_T_htf_cold = m_UA_PHX_des = m_cp_htf = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:
		// Component classes
	C_turbine m_t;
	C_compressor m_mc;
	C_recompressor m_rc;
	C_HeatExchanger m_LT, m_HT, m_PHX, m_PC;

		// Input/Ouput structures for class methods
	S_design_limits ms_des_limits;
	S_design_parameters ms_des_par;
	S_opt_design_parameters ms_opt_des_par;
	S_auto_opt_design_parameters ms_auto_opt_des_par;
	S_design_solved ms_des_solved;
	S_od_parameters ms_od_par;
	S_opt_od_parameters ms_opt_od_par;
	S_target_od_parameters ms_tar_od_par;
	S_opt_target_od_parameters ms_opt_tar_od_par;
	S_od_solved ms_od_solved;
	S_PHX_od_parameters ms_phx_od_par;

		// Results from last 'design' solution
	std::vector<double> m_temp_last, m_pres_last, m_enth_last, m_entr_last, m_dens_last;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
	double m_eta_thermal_last;
	double m_W_dot_net_last;
	double m_m_dot_mc, m_m_dot_rc, m_m_dot_t;
	double m_Q_dot_PHX, m_Q_dot_bypass, m_eta_bypass;
	double m_W_dot_mc, m_W_dot_rc, m_W_dot_mc_bypass;
	
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
	double m_biggest_target;

		// New opt
	bool m_found_opt;
	double m_eta_phx_max;
	double m_UA_diff_eta_max;
	double m_over_deltaP_eta_max;

	void design_core(int & error_code);	

	void design_core_standard(int & error_code);
	
	void design_core_bypass(int & error_code);

	void design_core_bypass150C(int & error_code);

	void design_core_HTR_hs(int & error_code);

	void opt_design_core(int & error_code);

	void auto_opt_design_core(int & error_code);

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
		m_Q_dot_PHX = m_Q_dot_bypass = m_eta_bypass = std::numeric_limits<double>::quiet_NaN();
		m_W_dot_mc = m_W_dot_rc = m_W_dot_mc_bypass = std::numeric_limits<double>::quiet_NaN();

		m_W_dot_net_last = std::numeric_limits<double>::quiet_NaN();
			
		m_eta_thermal_opt = m_eta_thermal_opt = std::numeric_limits<double>::quiet_NaN();

		m_temp_od = m_pres_od = m_enth_od = m_entr_od = m_dens_od = m_temp_last;

		m_eta_thermal_od = m_W_dot_net_od = m_Q_dot_PHX_od = std::numeric_limits<double>::quiet_NaN();

		m_W_dot_net_max = m_eta_best = m_biggest_target = std::numeric_limits<double>::quiet_NaN();

		m_found_opt = false;

		m_eta_phx_max = m_over_deltaP_eta_max = m_UA_diff_eta_max = std::numeric_limits<double>::quiet_NaN();

		// Set design limits!!!!
		ms_des_limits.m_UA_net_power_ratio_max = 2.0;		//[-/K]
		ms_des_limits.m_UA_net_power_ratio_min = 1.E-5;		//[-/K]
		ms_des_limits.m_T_mc_in_min = 32.0 + 273.15;	//[K]
	}

	~C_RecompCycle(){}

	void design(S_design_parameters & des_par_in, int & error_code);

	void opt_design(S_opt_design_parameters & opt_des_par_in, int & error_code);

	void auto_opt_design(S_auto_opt_design_parameters & auto_opt_des_par_in, int & error_code);

	void auto_opt_design_hit_eta(S_auto_opt_design_hit_eta_parameters & auto_opt_des_hit_eta_in, int & error_code, string & error_msg);

	void off_design(S_od_parameters & od_par_in, int & error_code);

	void optimal_off_design(S_opt_od_parameters & opt_od_par_in, int & error_code);
	
	void get_max_output_od(S_opt_target_od_parameters & opt_tar_od_par_in, int & error_code);

	void target_off_design(S_target_od_parameters & tar_od_par_in, int & error_code);

	void optimal_target_off_design(S_opt_target_od_parameters & opt_tar_od_par_in, int & error_code);

	void optimal_target_off_design_no_check(S_opt_target_od_parameters & opt_tar_od_par_in, int & error_code);

	void opt_od_eta_for_hx(S_od_parameters & od_par_in, S_PHX_od_parameters phx_od_par_in, int & error_code);

	const S_design_solved * get_design_solved()
	{
		return &ms_des_solved;
	}	

	const S_od_solved * get_od_solved()
	{
		return &ms_od_solved;
	}

	double get_max_target()
	{
		return m_biggest_target;
	}

	const S_design_limits & get_design_limits()
	{
		return ms_des_limits;
	}

	// Called by 'nlopt_callback_opt_des_1', so needs to be public
	double design_point_eta(const std::vector<double> &x);

	// Called by 'fmin_callback_opt_eta', so needs to be public
	double opt_eta(double P_high_opt);

	// Called by 'nlopt_cb_opt_od', so needs to be public
	double off_design_point_value(const std::vector<double> &x);

	// Called by 'nlopt...', so needs to be public
	double eta_at_target(const std::vector<double> &x);
	
	// Called by 'nlopt...', so needs to be public
	double opt_od_eta(const std::vector<double> &x);
};

double nlopt_callback_opt_des_1(const std::vector<double> &x, std::vector<double> &grad, void *data);

double fmin_callback_opt_eta_1(double x, void *data);

double nlopt_cb_opt_od(const std::vector<double> &x, std::vector<double> &grad, void *data);

double nlopt_cb_eta_at_target(const std::vector<double> &x, std::vector<double> &grad, void *data);

double nlopt_cb_opt_od_eta(const std::vector<double> &x, std::vector<double> &grad, void *data);

double P_pseudocritical_1(double T_K);



bool find_polynomial_coefs(const std::vector<double> x_data, const std::vector<double> y_data, int n_coefs, std::vector<double> & coefs_out, double & r_squared);


class C_poly_curve_r_squared
{
private:
	std::vector<double> m_x;
	std::vector<double> m_y;
	int m_n_points;
	double m_y_bar;
	double m_SS_tot;

public:
	C_poly_curve_r_squared()
	{
		m_x.resize(0);
		m_y.resize(0);
		m_n_points = -1;
		m_y_bar = std::numeric_limits<double>::quiet_NaN();
		m_SS_tot = std::numeric_limits<double>::quiet_NaN();
	}

	bool init(const std::vector<double> x_data, const std::vector<double> y_data);	


	// Called by 'nlopt...', so needs to be public
	double calc_r_squared(const std::vector<double> coefs);

};

double nlopt_callback_poly_coefs(const std::vector<double> &x, std::vector<double> &grad, void *data);

#endif
