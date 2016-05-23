#ifndef __SCO2_PC_CSP_INT_
#define __SCO2_PC_CSP_INT_

#include "sco2_pc_core.h"
#include "heat_exchangers.h"
#include "csp_solver_util.h"

#include "numeric_solvers.h"

#include "ud_power_cycle.h"

class C_sco2_recomp_csp
{

public:

	struct S_des_par
	{
		// System Design
		int m_hot_fl_code;				//[-] Integer coding the HTF type
		util::matrix_t<double> mc_hot_fl_props;	//[-] Custom HTF properties (if applicable)
		double m_T_htf_hot_in;			//[K] Design-point hot inlet temperature
		double m_phx_dt_hot_approach;	//[K/C] Temperature difference between hot HTF and turbine CO2 inlet
		double m_T_amb_des;				//[K] Ambient temperature
		double m_dt_mc_approach;		//[K] Temperature difference between main compressor inlet and ambient air
		double m_elevation;				//[m] Site elevation
		double m_W_dot_net;				//[kW] Target net cycle power
		double m_eta_thermal;			//[-] Cycle thermal efficiency

		// Cycle design parameters
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

		// PHX design parameters
			// This is a PHX rather than system parameter because we don't know T_CO2_in until cycle model is solved
		double m_phx_dt_cold_approach;	//[K/C] Temperature difference between cold HTF and PHX CO2 inlet

		// Air cooler parameters
		double m_frac_fan_power;		//[-] Fraction of total cycle power 'S_des_par_cycle_dep.m_W_dot_fan_des' consumed by air fan
		double m_deltaP_cooler_frac;    // [-] Fraction of high side (of cycle, i.e. comp outlet) pressure that is allowed as pressure drop to design the ACC

		S_des_par()
		{
			m_hot_fl_code = m_N_sub_hxrs = -1;

			m_T_htf_hot_in = m_phx_dt_hot_approach = m_T_amb_des = m_dt_mc_approach =
				m_elevation = m_W_dot_net = m_eta_thermal =

				m_eta_mc = m_eta_rc = m_eta_t =
				m_P_high_limit = m_tol = m_opt_tol = m_N_turbine =

				m_phx_dt_cold_approach = m_frac_fan_power = m_deltaP_cooler_frac = 
				std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_des_solved
	{
		C_HX_counterflow::S_des_solved ms_phx_des_solved;
		C_RecompCycle::S_design_solved ms_rc_cycle_solved;
	};

	struct S_od_par
	{
		// From CSP System
		double m_T_htf_hot;		//[K] Hot HTF temperature from the receiver or storage
		double m_m_dot_htf;		//[kg/s] HTF mass flow rate 

		// Ambient Conditions
		double m_T_amb;			//[K] Ambient temperature

		S_od_par()
		{
			m_T_htf_hot = m_m_dot_htf = m_T_amb = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_opt_eta_tracking
	{	// The values here are updated on NLOPT calls to the off design models
		bool m_is_opt_found;
		double m_eta_max;
		double m_over_T_t_in_at_eta_max;
		double m_over_P_high_at_eta_max;
	
		S_od_opt_eta_tracking()
		{
			m_is_opt_found = false;

			m_eta_max = m_over_T_t_in_at_eta_max = m_over_P_high_at_eta_max = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_solved
	{
		C_RecompCycle::S_od_solved ms_rc_cycle_od_solved;
		C_HX_counterflow::S_od_solved ms_phx_od_solved;	
	};

private:
	C_RecompCycle mc_rc_cycle;
	C_HX_co2_to_htf mc_phx;
	C_CO2_to_air_cooler mc_air_cooler;

	S_des_par ms_des_par;
	C_RecompCycle::S_auto_opt_design_hit_eta_parameters ms_rc_cycle_des_par;
	C_CO2_to_air_cooler::S_des_par_ind ms_air_cooler_des_par_ind;
	C_CO2_to_air_cooler::S_des_par_cycle_dep ms_air_cooler_des_par_dep;
	C_HX_counterflow::S_des_par ms_phx_des_par;
		
	S_des_solved ms_des_solved;

	S_od_par ms_od_par;
	//C_RecompCycle::S_od_parameters ms_rc_cycle_od_par;
	C_RecompCycle::S_od_phi_par ms_rc_cycle_od_phi_par;
	C_HX_counterflow::S_od_par ms_phx_od_par;
	S_od_opt_eta_tracking ms_od_opt_eta_tracking;

	S_od_solved ms_od_solved;

	double m_T_mc_in_min;

	void design_core();

	int od_fix_T_mc__float_phx_dt__opt_eta();

public:

	C_csp_messages mc_messages;

	enum E_off_design_strategies
	{
		FIX_T_MC_APPROACH__FLOAT_PHX_DT
	};
	
	C_sco2_recomp_csp();

	~C_sco2_recomp_csp(){};

	class C_mono_eq_T_t_in : public C_monotonic_equation
	{
	private: 
		C_sco2_recomp_csp *mpc_sco2_rc;

	public:
		C_mono_eq_T_t_in(C_sco2_recomp_csp *pc_sco2_rc)
		{
			mpc_sco2_rc = pc_sco2_rc;
		}
	
		virtual int operator()(double T_t_in /*K*/, double *diff_T_t_in /*-*/);
	};

	class C_sco2_csp_od : public C_od_pc_function
	{
	private:
		C_sco2_recomp_csp *mpc_sco2_rc;

	public:
		C_sco2_csp_od(C_sco2_recomp_csp *pc_sco2_rc)
		{
			mpc_sco2_rc = pc_sco2_rc;
		}
	
		virtual int operator()(S_f_inputs inputs, S_f_outputs & outputs);
	};

	int generate_ud_pc_tables(double T_htf_low /*C*/, double T_htf_high /*C*/, int n_T_htf /*-*/,
		double T_amb_low /*C*/, double T_amb_high /*C*/, int n_T_amb /*-*/,
		double m_dot_htf_ND_low /*-*/, double m_dot_htf_ND_high /*-*/, int n_m_dot_htf_ND,
		util::matrix_t<double> & T_htf_ind, util::matrix_t<double> & T_amb_ind, util::matrix_t<double> & m_dot_htf_ND_ind);

	void design(C_sco2_recomp_csp::S_des_par des_par);

	int off_design(S_od_par od_par, int off_design_strategy);

	// Class methods linked to nlopt callbacks - must be public
	double od_fix_T_mc_approach__float_phx_dt(const std::vector<double> &x);

	double get_T_mc_in_min()
	{
		return m_T_mc_in_min;		//[K]
	}

	// Methods to private access member data
	const S_des_par * get_design_par()
	{
		return &ms_des_par;
	}

	const S_des_solved * get_design_solved()
	{
		return &ms_des_solved;
	}

	const C_HX_counterflow::S_des_par * get_phx_des_par()
	{
		return &ms_phx_des_par;
	}

	const S_od_solved * get_od_solved()
	{
		return &ms_od_solved;
	}


};

// Optimization method callbacks
double nlopt_cb_opt_od_eta__float_phx_dt(const std::vector<double> &x, std::vector<double> &grad, void *data);


#endif