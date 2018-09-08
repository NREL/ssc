#ifndef __SCO2_CYCLE_TEMPLATES_
#define __SCO2_CYCLE_TEMPLATES_

#include "sco2_cycle_components.h"
#include "heat_exchangers.h"
#include <string>

class C_sco2_cycle_core
{
public:

	enum E_cycle_state_points
	{
		// index values for c++ 0-based vectors for temperature, pressure, etc.
		MC_IN = 0,		// Main compressor inlet
		MC_OUT,			// Main compressor outlet
		LTR_HP_OUT,		// Low temp recuperator high pressure outlet
		MIXER_OUT,		// Mixer: LTR_HP_OUT + Recompressor outlet
		HTR_HP_OUT,		// High temp recuperator high pressure outlet
		TURB_IN,		// Turbine inlet
		TURB_OUT,		// Turbine outlet
		HTR_LP_OUT,		// High temp recuperator low pressure outlet
		LTR_LP_OUT,		// Low temp recuperator low pressure outlet
		RC_OUT,			// Recompresor outlet
		PC_IN,			// Precompressor inlet (partial cooling cycle)
		PC_OUT,			// Precompressor outlet (partial cooling cycle)

		END_SCO2_STATES
	};

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

	struct S_design_solved
	{
		std::vector<double> m_temp, m_pres, m_enth, m_entr, m_dens;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
		double m_eta_thermal;	//[-]
		double m_W_dot_net;		//[kWe]
		double m_m_dot_mc;		//[kg/s]
		double m_m_dot_rc;		//[kg/s]
		double m_m_dot_pc;		//[kg/s]
		double m_m_dot_t;		//[kg/s]
		double m_recomp_frac;	//[-]
		double m_UA_LTR;			//[kW/K]
		double m_UA_HTR;			//[kW/K]
		double m_W_dot_mc;      //[kWe]
		double m_W_dot_rc;		//[kWe]
		double m_W_dot_pc;		//[kWe]
		double m_W_dot_t;		//[kWe]

		bool m_is_rc;

		C_comp_multi_stage::S_des_solved ms_mc_ms_des_solved;
		C_comp_multi_stage::S_des_solved ms_rc_ms_des_solved;
		C_comp_multi_stage::S_des_solved ms_pc_ms_des_solved;
		C_turbine::S_design_solved ms_t_des_solved;
		C_HX_counterflow::S_des_solved ms_LTR_des_solved;
		C_HX_counterflow::S_des_solved ms_HTR_des_solved;

		C_CO2_to_air_cooler::S_des_solved ms_LP_air_cooler;
		C_CO2_to_air_cooler::S_des_solved ms_IP_air_cooler;

		S_design_solved()
		{
			m_eta_thermal = m_W_dot_net = m_m_dot_mc = m_m_dot_rc = m_m_dot_t = m_recomp_frac =
				m_UA_LTR = m_UA_HTR =
				m_W_dot_mc = m_W_dot_rc = m_W_dot_pc = m_W_dot_t = std::numeric_limits<double>::quiet_NaN();

			m_is_rc = true;
		}
	};

	struct S_auto_opt_design_hit_eta_parameters
	{
		double m_W_dot_net;					//[kW] Target net cycle power
		double m_eta_thermal;				//[-] Cycle thermal efficiency
		double m_T_mc_in;					//[K] Main compressor inlet temperature
		double m_T_pc_in;					//[K] Pre-compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC_pre;    //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC_main;   //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		double m_LTR_eff_max;				//[-] Maximum allowable effectiveness in LT recuperator
		double m_HTR_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
		double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
		double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_pc;					//[-] design-point efficiency of the pre-compressor; 
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_tol;						//[-] Convergence tolerance
		double m_opt_tol;					//[-] Optimization tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)
		
		// Air cooler parameters
		double m_frac_fan_power;		//[-] Fraction of total cycle power 'S_des_par_cycle_dep.m_W_dot_fan_des' consumed by air fan
		double m_deltaP_cooler_frac;	//[-] Fraction of high side (of cycle, i.e. comp outlet) pressure that is allowed as pressure drop to design the ACC
		double m_T_amb_des;				//[K] Design point ambient temperature
		double m_elevation;				//[m] Elevation (used to calculate ambient pressure)

		int m_is_recomp_ok;					//[-] 1 = yes, 0 = no, other = invalid

		int m_des_objective_type;			//[2] = min phx deltat then max eta, [else] max eta
		double m_min_phx_deltaT;			//[C]

		bool m_fixed_P_mc_out;			//[-] if true, P_mc_out is fixed at 'm_P_high_limit'

		double m_PR_mc_guess;				//[-] Initial guess for ratio of P_mc_out to P_mc_in
		bool m_fixed_PR_mc;					//[-] if true, ratio of P_mc_out to P_mc_in is fixed at PR_mc_guess

		// Callback function only log
		bool(*mf_callback_log)(std::string &log_msg, std::string &progress_msg, void *data, double progress, int out_type);
		void *mp_mf_active;

		S_auto_opt_design_hit_eta_parameters()
		{
			m_W_dot_net = m_T_mc_in = m_T_pc_in = m_T_t_in = m_LTR_eff_max = m_HTR_eff_max =
				m_eta_mc = m_eta_rc = m_eta_pc = m_eta_t = m_P_high_limit =
				m_tol = m_opt_tol = m_N_turbine =
				m_frac_fan_power = m_deltaP_cooler_frac = m_T_amb_des = m_elevation =
				m_PR_mc_guess = std::numeric_limits<double>::quiet_NaN();

			m_N_sub_hxrs = -1;

			m_is_recomp_ok = -1;

			// Default to standard optimization to maximize cycle efficiency
			m_des_objective_type = 1;
			m_min_phx_deltaT = 0.0;		//[C]

			m_fixed_PR_mc = false;		//[-] If false, then should default to optimizing this parameter
			m_fixed_P_mc_out = false;	//[-] If fasle, then should default to optimizing this parameter

			mf_callback_log = 0;
			mp_mf_active = 0;

			m_DP_LT.resize(2);
			std::fill(m_DP_LT.begin(), m_DP_LT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HT.resize(2);
			std::fill(m_DP_HT.begin(), m_DP_HT.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC_pre.resize(2);
			std::fill(m_DP_PC_pre.begin(), m_DP_PC_pre.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC_main.resize(2);
			std::fill(m_DP_PC_main.begin(), m_DP_PC_main.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};

	struct S_auto_opt_design_parameters
	{
		double m_W_dot_net;					//[kWe] Target net cycle power
		double m_T_mc_in;					//[K] Main compressor inlet temperature
		double m_T_pc_in;					//[K] Pre-compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		std::vector<double> m_DP_LTR;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HTR;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC_pre;    //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC_main;   //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		double m_UA_rec_total;				//[kW/K] Total design-point recuperator UA
		double m_LTR_eff_max;				//[-] Maximum allowable effectiveness in LT recuperator
		double m_HTR_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
		double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
		double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_pc;					//[-] design-point efficiency of the pre-compressor; 
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_tol;						//[-] Convergence tolerance
		double m_opt_tol;					//[-] Optimization tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)
		
		// Air cooler parameters
		double m_frac_fan_power;		//[-] Fraction of total cycle power 'S_des_par_cycle_dep.m_W_dot_fan_des' consumed by air fan
		double m_deltaP_cooler_frac;	//[-] Fraction of high side (of cycle, i.e. comp outlet) pressure that is allowed as pressure drop to design the ACC
		double m_T_amb_des;				//[K] Design point ambient temperature
		double m_elevation;				//[m] Elevation (used to calculate ambient pressure)

		int m_is_recomp_ok;					//[-] 1 = yes, 0 = no, other = invalid

		bool m_fixed_P_mc_out;			//[-] if true, P_mc_out is fixed at 'm_P_high_limit'

		double m_PR_mc_guess;				//[-] Initial guess for ratio of P_mc_out to P_mc_in
		bool m_fixed_PR_mc;					//[-] if true, ratio of P_mc_out to P_mc_in is fixed at PR_mc_guess
		
		int m_des_objective_type;		//[2] = min phx deltat then max eta, [else] max eta
		double m_min_phx_deltaT;		//[C]

		// Callback function only log
		bool(*mf_callback_log)(std::string &log_msg, std::string &progress_msg, void *data, double progress, int out_type);
		void *mp_mf_active;


		S_auto_opt_design_parameters()
		{
			m_W_dot_net = m_T_mc_in = m_T_pc_in = m_T_t_in =
				m_UA_rec_total = m_LTR_eff_max = m_HTR_eff_max =
				m_eta_mc = m_eta_rc = m_eta_pc = m_eta_t = m_P_high_limit = m_tol = m_N_turbine = 
				m_frac_fan_power = m_deltaP_cooler_frac = m_T_amb_des = m_elevation =
				m_fixed_PR_mc = std::numeric_limits<double>::quiet_NaN();
			m_N_sub_hxrs = -1;

			m_is_recomp_ok = 1;

			m_fixed_PR_mc = false;		//[-] If false, then should default to optimizing this parameter
			m_fixed_P_mc_out = false;	//[-] If fasle, then should default to optimizing this parameter

			// Default to standard optimization to maximize cycle efficiency
			m_des_objective_type = 1;
			m_min_phx_deltaT = 0.0;		//[C]

			mf_callback_log = 0;
			mp_mf_active = 0;

			m_DP_LTR.resize(2);
			std::fill(m_DP_LTR.begin(), m_DP_LTR.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HTR.resize(2);
			std::fill(m_DP_HTR.begin(), m_DP_HTR.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC_pre.resize(2);
			std::fill(m_DP_PC_pre.begin(), m_DP_PC_pre.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC_main.resize(2);
			std::fill(m_DP_PC_main.begin(), m_DP_PC_main.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};

	struct S_od_solved
	{
		std::vector<double> m_temp, m_pres, m_enth, m_entr, m_dens;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
		double m_eta_thermal;	//[-]
		double m_W_dot_net;		//[kWe]
		double m_Q_dot;			//[kWt]
		double m_m_dot_mc;		//[kg/s]
		double m_m_dot_rc;		//[kg/s]
		double m_m_dot_pc;		//[kg/s]
		double m_m_dot_t;		//[kg/s]
		double m_recomp_frac;	//[-]

		double m_mc_f_bypass;	//[-]
		double m_pc_f_bypass;	//[-]

		C_comp_multi_stage::S_od_solved ms_mc_ms_od_solved;
		C_comp_multi_stage::S_od_solved ms_rc_ms_od_solved;
		C_comp_multi_stage::S_od_solved ms_pc_ms_od_solved;
		C_turbine::S_od_solved ms_t_od_solved;
		C_HX_counterflow::S_od_solved ms_LT_recup_od_solved;
		C_HX_counterflow::S_od_solved ms_HT_recup_od_solved;

		S_od_solved()
		{
			m_eta_thermal = m_W_dot_net = m_Q_dot = m_m_dot_mc = m_m_dot_rc = m_m_dot_pc =
				m_m_dot_t = m_recomp_frac = m_mc_f_bypass = m_pc_f_bypass = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_par
	{
		double m_T_mc_in;		//[K] Compressor inlet temperature
		double m_T_pc_in;		//[K] Precompressor inlet temperature
		double m_T_t_in;		//[K] Turbine inlet temperature
		double m_P_LP_comp_in;	//[kPa] Compressor inlet pressure (low pressure comp in partial cooling cycle)
		
		double m_f_mc_pc_bypass;	//[-] Fraction of pre and main compressor flow that is bypassed back to the respective compressor cooler

		int m_N_sub_hxrs;		//[-] Number of sub heat exchangers
		double m_tol;			//[-] Convergence tolerance

		S_od_par()
		{
			m_T_mc_in = m_T_pc_in = m_T_t_in = m_P_LP_comp_in = 
				m_tol = std::numeric_limits<double>::quiet_NaN();
			m_N_sub_hxrs = -1;

			m_f_mc_pc_bypass = 0.0;	//[-]
		}
	};
	
protected:

	S_design_solved ms_des_solved;

	S_auto_opt_design_parameters ms_auto_opt_des_par;

	S_od_solved ms_od_solved;

	S_od_par ms_od_par;

	S_design_limits ms_des_limits;

	void clear_ms_od_solved()
	{
		S_od_solved s_od_solved_temp;
		ms_od_solved = s_od_solved_temp;
	}

public:

	C_sco2_cycle_core()
	{
		// Set design limits!!!!
		ms_des_limits.m_UA_net_power_ratio_max = 2.0;		//[-/K]
		ms_des_limits.m_UA_net_power_ratio_min = 1.E-5;		//[-/K]

		// Set minimum main compressor inlet temperature
		CO2_info s_co2_info;

		get_CO2_info(&s_co2_info);

		ms_des_limits.m_T_mc_in_min = ceil(s_co2_info.T_critical);		//[K]
	}

	const S_design_solved * get_design_solved()
	{
		return &ms_des_solved;
	}

	virtual int auto_opt_design(S_auto_opt_design_parameters & auto_opt_des_par_in) = 0;
	
	virtual int auto_opt_design_hit_eta(S_auto_opt_design_hit_eta_parameters & auto_opt_des_hit_eta_in, std::string & error_msg) = 0;

	const S_od_solved * get_od_solved()
	{
		return &ms_od_solved;
	}

	virtual int off_design_fix_shaft_speeds(S_od_par & od_phi_par_in) = 0;

	virtual const C_comp_multi_stage::S_od_solved * get_rc_od_solved() = 0;

	const S_design_limits & get_design_limits()
	{
		return ms_des_limits;
	}
};


#endif // !__SCO2_CYCLE_TEMPLATES_
