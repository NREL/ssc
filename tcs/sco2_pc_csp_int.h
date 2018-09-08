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

#ifndef __SCO2_PC_CSP_INT_
#define __SCO2_PC_CSP_INT_

//#include "sco2_pc_core.h"
#include "sco2_recompression_cycle.h"
#include "sco2_partialcooling_cycle.h"
#include "sco2_cycle_templates.h"

#include "heat_exchangers.h"
#include "csp_solver_util.h"

#include "numeric_solvers.h"

#include "ud_power_cycle.h"

#include <iostream>
#include <fstream>

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
		int m_design_method;			//[-] 1 = Specify efficiency, 2 = Specify total recup UA
		double m_eta_thermal;			//[-] Cycle thermal efficiency
		double m_UA_recup_tot_des;		//[kW/K] Total recuperator conductance
		int m_cycle_config;				//[-] 2 = partial cooling, [else] = recompression
	
		// Cycle design parameters
		std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		double m_LT_eff_max;				//[-] Maximum allowable effectiveness in LT recuperator
		double m_HT_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
		double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
		double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_pc;					//[-] design-point efficiency of the precompressor; isentropic if positive, polytropic if negative
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_tol;						//[-] Convergence tolerance
		double m_opt_tol;					//[-] Optimization tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)
		int m_is_recomp_ok;					//[-] 1 = yes, 0 = no, other = invalid
	
		int m_des_objective_type;			//[2] = min phx deltat then max eta, [else] max eta
		double m_min_phx_deltaT;			//[C]
	
		bool m_fixed_P_mc_out;			//[-] if true, P_mc_out is fixed at 'm_P_high_limit'
		
		double m_PR_mc_guess;				//[-] Initial guess for ratio of P_mc_out to P_mc_in
		bool m_fixed_PR_mc;					//[-] if true, ratio of P_mc_out to P_mc_in is fixed at PR_mc_guess
	
		// PHX design parameters
		// This is a PHX rather than system parameter because we don't know T_CO2_in until cycle model is solved
		double m_phx_dt_cold_approach;	//[K/C] Temperature difference between cold HTF and PHX CO2 inlet
	
		// Air cooler parameters
		double m_frac_fan_power;		//[-] Fraction of total cycle power 'S_des_par_cycle_dep.m_W_dot_fan_des' consumed by air fan
		double m_deltaP_cooler_frac;    // [-] Fraction of high side (of cycle, i.e. comp outlet) pressure that is allowed as pressure drop to design the ACC
	
		S_des_par()
		{
			m_hot_fl_code = m_design_method = m_N_sub_hxrs = -1;
	
			// Default cycle config to recompression
			m_cycle_config = 1;
	
			// Default to standard optimization to maximize cycle efficiency
			m_des_objective_type = 1;
			m_min_phx_deltaT = 0.0;		//[C]
	
			m_is_recomp_ok = -1;
	
			m_T_htf_hot_in = m_phx_dt_hot_approach = m_T_amb_des = m_dt_mc_approach =
				m_elevation = m_W_dot_net = m_eta_thermal = m_LT_eff_max = m_HT_eff_max =
	
				m_eta_mc = m_eta_rc = m_eta_pc = m_eta_t =
				m_P_high_limit = m_tol = m_opt_tol = m_N_turbine =
	
				m_PR_mc_guess =
	
				m_phx_dt_cold_approach = m_frac_fan_power = m_deltaP_cooler_frac =
				std::numeric_limits<double>::quiet_NaN();
	
			m_fixed_PR_mc = false;		//[-] If false, then should default to optimizing this parameter
			m_fixed_P_mc_out = false;	//[-] If fasle, then should default to optimizing this parameter
		}
	};

	struct S_des_solved
	{
		C_HX_counterflow::S_des_solved ms_phx_des_solved;
		C_sco2_cycle_core::S_design_solved ms_rc_cycle_solved;
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

	struct S_od_solved
	{
		C_sco2_cycle_core::S_od_solved ms_rc_cycle_od_solved;
		C_HX_counterflow::S_od_solved ms_phx_od_solved;
		int m_od_error_code;
		bool m_is_converged;
	
		S_od_solved()
		{
			m_od_error_code = 0;
			m_is_converged = false;
		}
	};

	struct S_od_operation_inputs
	{
		double m_P_mc_in;		//[kPa]
		double m_recomp_frac;	//[-]
		double m_phi_mc;		//[-]
	
		S_od_operation_inputs()
		{
			m_P_mc_in = m_recomp_frac = m_phi_mc = std::numeric_limits<double>::quiet_NaN();
		}
	};

	enum E_off_design_strategies
	{
		E_MAX_ETA = 1,
		E_MAX_POWER,
		E_TARGET_POWER_ETA_MAX
	};

	enum E_off_design_turbo_operation
	{
		E_FIXED_MC_FIXED_RC_FIXED_T
	};

	enum E_system_op_constraints
	{
		E_TURBINE_INLET_OVER_TEMP = -15,
		E_OVER_PRESSURE,
		E_TIP_RATIO,
		E_MC_SURGE,
		E_RC_SURGE,
		E_PC_SURGE
	};

	// Callback function with progress bar
	bool(*mf_callback_update)(std::string &log_msg, std::string &progress_msg, void *data, double progress, int out_type);
	void *mp_mf_update;

	C_csp_messages mc_messages;

private:
	
	C_sco2_cycle_core * mpc_sco2_cycle;

	C_RecompCycle mc_rc_cycle;
	C_HX_co2_to_htf mc_phx;
	C_PartialCooling_Cycle mc_partialcooling_cycle;

	S_des_par ms_des_par;
	C_sco2_cycle_core::S_auto_opt_design_hit_eta_parameters ms_cycle_des_par;
	C_HX_counterflow::S_des_calc_UA_par ms_phx_des_par;
		
	S_des_solved ms_des_solved;

	S_od_par ms_od_par;
	C_sco2_cycle_core::S_od_par ms_cycle_od_par;
	C_HX_counterflow::S_od_par ms_phx_od_par;

	S_od_solved ms_od_solved;
	
	// Optimization variables: could make into structure...
	int m_od_opt_objective;		//[-]
	double m_od_opt_ftol;		//[-] Relative tolerance for od optimization: objective function convergence
	double m_od_opt_xtol;		//[-] Relative tolerance for od optimization: independent variable convergence
	// ******************************************************

	int m_nlopt_iter;		//[-]

	int m_off_design_turbo_operation;	//[-] How is turbomachinery controlled off-design?

	double m_T_mc_in_min;		//[K]
	double m_T_co2_crit;		//[K]
	double m_P_co2_crit;		//[kPa]

	void design_core();

	double adjust_P_mc_in_away_2phase(double T_co2 /*K*/, double P_mc_in /*kPa*/);

	void setup_off_design_info(C_sco2_recomp_csp::S_od_par od_par, int off_design_strategy, double od_opt_tol);

public:	

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

	void design(S_des_par des_par);

	int optimize_off_design(C_sco2_recomp_csp::S_od_par od_par, int off_design_strategy, double od_opt_tol = 1.E-4);

	int off_design_fix_P_mc_in(S_od_par od_par, double P_mc_in /*MPa*/, int off_design_strategy, double od_opt_tol = 1.E-4);
	
	int opt_P_LP_comp_in__fixed_N_turbo();   // opt_P_mc_in_nest_f_recomp_max_eta_core();

	int off_design(C_sco2_recomp_csp::S_od_par od_par, S_od_operation_inputs od_op_inputs);

	int off_design_core(double & eta_solved);

	double get_T_mc_in_min()
	{
		return m_T_mc_in_min;		//[K]
	}

	// Methods to private access member data
	const S_des_par * get_design_par();

	const S_des_solved * get_design_solved();

	const C_HX_counterflow::S_des_calc_UA_par * get_phx_des_par();

	const S_od_solved * get_od_solved();

	double opt_P_LP_in__fixed_N_turbo__return_f_obj(double P_mc_in /*kPa*/);

};

// Optimization method callbacks
double nlopt_opt_P_LP_in__fixed_N_turbo(const std::vector<double> &x, std::vector<double> &grad, void *data);

double fmin_opt_P_LP_in__fixed_N_turbo(double x, void *data);



#endif
