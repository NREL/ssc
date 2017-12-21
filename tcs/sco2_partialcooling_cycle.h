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

#ifndef __SCO2_PARTIAL_COOLING_
#define __SCO2_PARTIAL_COOLING_

#include "sco2_cycle_components.h"
#include "sco2_cycle_templates.h"

#include "heat_exchangers.h"
#include "CO2_properties.h"

#include <vector>
#include <math.h>
#include <limits>

class C_PartialCooling_Cycle : public C_sco2_cycle_core
{
public:

	enum E_cycle_state_points
	{
		MC_IN = 0,
		MC_OUT,
		LTR_HP_OUT,
		MIXER_OUT,
		HTR_HP_OUT,
		TURB_IN,
		TURB_OUT,
		HTR_LP_OUT,
		LTR_LP_OUT,
		PC_IN,
		PC_OUT,
		RC_OUT
	};

	struct S_des_params
	{
		double m_W_dot_net;					//[kWe] Target net cycle power
		double m_T_mc_in;					//[K] Main compressor inlet temperature
		double m_T_pc_in;					//[K] Pre-compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		double m_P_pc_in;					//[kPa] Pre-compressor inlet pressure
		double m_P_mc_in;					//[kPa] Compressor inlet pressure
		double m_P_mc_out;					//[kPa] Compressor outlet pressure
		std::vector<double> m_DP_LTR;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HTR;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC_full;   //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC_partial; //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		double m_UA_LTR;					//[kW/K] UA in LTR
		double m_UA_HTR;					//[kW/K] UA in HTR
		double m_LTR_eff_max;				//[-] Maximum allowable effectiveness in LT recuperator
		double m_HTR_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
		double m_recomp_frac;				//[-] Fraction of flow that bypasses the precooler and the main compressor at the design point
		double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
		double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
		double m_eta_pc;					//[-] design-point efficiency of the pre-compressor; 
		double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
		int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
		double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
		double m_tol;						//[-] Convergence tolerance
		double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)

		int m_des_objective_type;		//[2] = min phx deltat then max eta, [else] max eta
		double m_min_phx_deltaT;		//[C]

		S_des_params()
		{
			m_W_dot_net = m_T_mc_in = m_T_pc_in = m_T_t_in = 
				m_P_pc_in = m_P_mc_in = m_P_mc_out = m_UA_LTR = m_UA_HTR = m_LTR_eff_max = m_HTR_eff_max = m_recomp_frac =
				m_eta_mc = m_eta_rc = m_eta_pc = m_eta_t = m_P_high_limit = m_tol = m_N_turbine = std::numeric_limits<double>::quiet_NaN();
			m_N_sub_hxrs = -1;

			// Default to standard optimization to maximize cycle efficiency
			m_des_objective_type = 1;
			m_min_phx_deltaT = 0.0;		//[C]

			m_DP_LTR.resize(2);
			std::fill(m_DP_LTR.begin(), m_DP_LTR.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HTR.resize(2);
			std::fill(m_DP_HTR.begin(), m_DP_HTR.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC_full.resize(2);
			std::fill(m_DP_PC_full.begin(), m_DP_PC_full.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC_partial.resize(2);
			std::fill(m_DP_PC_partial.begin(), m_DP_PC_partial.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};

	struct S_opt_des_params
	{
		double m_W_dot_net;					//[kWe] Target net cycle power
		double m_T_mc_in;					//[K] Main compressor inlet temperature
		double m_T_pc_in;					//[K] Pre-compressor inlet temperature
		double m_T_t_in;					//[K] Turbine inlet temperature
		std::vector<double> m_DP_LTR;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_HTR;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC_full;   //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC_partial; //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
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

		int m_des_objective_type;		//[2] = min phx deltat then max eta, [else] max eta
		double m_min_phx_deltaT;		//[C]

		double m_P_mc_out_guess;		//[kPa] Initial guess for main compressor outlet pressure
		bool m_fixed_P_mc_out;			//[-] If true, P_mc_out is fixed at P_mc_out_guess

		double m_PR_total_guess;		//[-] Initial guess for ratio of P_mc_out / P_pc_in
		bool m_fixed_PR_total;			//[-] if true, ratio of P_mc_out to P_pc_in is fixed at PR_guess

		double m_f_PR_mc_guess;			//[-] Initial guess: fraction of total PR that is P_mc_out / P_mc_in
		bool m_fixed_f_PR_mc;			//[-] if true, fixed at f_PR_mc_guess

		double m_recomp_frac_guess;		//[-] Initial guess: recompression fraction
		bool m_fixed_recomp_frac;		//[-] if true, fixed at m_recomp_frac_guess

		double m_LTR_frac_guess;		//[-] Initial guess for fraction of UA_rec_total that is allocated to LTR
		bool m_fixed_LTR_frac;			//[-] if true, fixed at m_LTR_frac_guess

		S_opt_des_params()
		{
			m_W_dot_net = m_T_mc_in = m_T_pc_in = m_T_t_in =
				m_UA_rec_total = m_LTR_eff_max = m_HTR_eff_max = 
				m_eta_mc = m_eta_rc = m_eta_pc = m_eta_t = m_P_high_limit = m_tol = m_N_turbine = 
				m_P_mc_out_guess = m_PR_total_guess = m_f_PR_mc_guess = 
				m_recomp_frac_guess = m_LTR_frac_guess = std::numeric_limits<double>::quiet_NaN();
			m_N_sub_hxrs = -1;

			// Default to standard optimization to maximize cycle efficiency
			m_des_objective_type = 1;
			m_min_phx_deltaT = 0.0;		//[C]

			m_DP_LTR.resize(2);
			std::fill(m_DP_LTR.begin(), m_DP_LTR.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HTR.resize(2);
			std::fill(m_DP_HTR.begin(), m_DP_HTR.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC_full.resize(2);
			std::fill(m_DP_PC_full.begin(), m_DP_PC_full.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC_partial.resize(2);
			std::fill(m_DP_PC_partial.begin(), m_DP_PC_partial.end(), std::numeric_limits<double>::quiet_NaN());
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
		std::vector<double> m_DP_PC_full;   //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		std::vector<double> m_DP_PC_partial; //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
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

		int m_des_objective_type;		//[2] = min phx deltat then max eta, [else] max eta
		double m_min_phx_deltaT;		//[C]

		S_auto_opt_design_parameters()
		{
			m_W_dot_net = m_T_mc_in = m_T_pc_in = m_T_t_in =
				m_UA_rec_total = m_LTR_eff_max = m_HTR_eff_max =
				m_eta_mc = m_eta_rc = m_eta_pc = m_eta_t = m_P_high_limit = m_tol = m_N_turbine = std::numeric_limits<double>::quiet_NaN();
			m_N_sub_hxrs = -1;

			// Default to standard optimization to maximize cycle efficiency
			m_des_objective_type = 1;
			m_min_phx_deltaT = 0.0;		//[C]

			m_DP_LTR.resize(2);
			std::fill(m_DP_LTR.begin(), m_DP_LTR.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_HTR.resize(2);
			std::fill(m_DP_HTR.begin(), m_DP_HTR.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC_full.resize(2);
			std::fill(m_DP_PC_full.begin(), m_DP_PC_full.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PC_partial.resize(2);
			std::fill(m_DP_PC_partial.begin(), m_DP_PC_partial.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_PHX.resize(2);
			std::fill(m_DP_PHX.begin(), m_DP_PHX.end(), std::numeric_limits<double>::quiet_NaN());
		}
	};

	//struct S_des_solved
	//{
	//	std::vector<double> m_temp, m_pres, m_enth, m_entr, m_dens;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
	//	double m_eta_thermal;	//[-]
	//	double m_W_dot_net;		//[kWe]
	//	double m_m_dot_mc;		//[kg/s]
	//	double m_m_dot_rc;		//[kg/s]
	//	double m_m_dot_pc;		//[kg/s]
	//	double m_m_dot_t;		//[kg/s]
	//	double m_recomp_frac;	//[-]
	//	double m_UA_LTR;		//[kW/K]
	//	double m_UA_HTR;		//[kW/K]

	//	bool m_is_rc;

	//	C_comp_multi_stage::S_des_solved ms_mc_ms_des_solved;
	//	C_comp_multi_stage::S_des_solved ms_rc_ms_des_solved;
	//	C_comp_multi_stage::S_des_solved ms_pc_ms_des_solved;
	//	C_turbine::S_design_solved ms_t_des_solved;
	//	C_HX_counterflow::S_des_solved ms_LTR_des_solved;
	//	C_HX_counterflow::S_des_solved ms_HTR_des_solved;

	//	S_des_solved()
	//	{
	//		m_eta_thermal = m_W_dot_net = m_m_dot_mc = m_m_dot_rc = m_m_dot_pc = m_m_dot_t = 
	//			m_recomp_frac = m_UA_LTR = m_UA_HTR = std::numeric_limits<double>::quiet_NaN();

	//		m_is_rc = true;
	//	}
	//};

private:

	// Cycle component classes
	C_turbine mc_t;
	C_comp_multi_stage mc_mc, mc_rc, mc_pc;
	C_HX_co2_to_co2 mc_LTR, mc_HTR;
	C_HeatExchanger mc_PHX, mc_PC_full, mc_PC_partial;	

	S_des_params ms_des_par;
	//S_des_solved ms_des_solved;
	S_opt_des_params ms_opt_des_par;
	S_auto_opt_design_parameters ms_auto_opt_des_par;

	CO2_state mc_co2_props;

	// Results from last 'design' solution
	std::vector<double> m_temp_last, m_pres_last, m_enth_last, m_entr_last, m_dens_last;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
	double m_m_dot_mc, m_m_dot_pc, m_m_dot_rc, m_m_dot_t;	//[kg/s]
	double m_W_dot_mc, m_W_dot_pc, m_W_dot_rc, m_W_dot_t;	//[kWe]
	double m_eta_thermal_calc_last;	//[-]
	double m_W_dot_net_last;	//[kWe]
	double m_energy_bal_last;	//[-]
	double m_objective_metric_last;	//[??]

	// Structures and data for optimization
	S_des_params ms_des_par_optimal;
	double m_objective_metric_opt;

		// Structures and data for auto-optimization
	double m_objective_metric_auto_opt;
	S_des_params ms_des_par_auto_opt;

	int design_core();

	int auto_opt_design_core();

	int finalize_design();

	int opt_design_core();

public:

	C_PartialCooling_Cycle()
	{
		m_temp_last.resize(RC_OUT + 1);
		std::fill(m_temp_last.begin(), m_temp_last.end(), std::numeric_limits<double>::quiet_NaN());

		m_pres_last = m_enth_last = m_entr_last = m_dens_last = m_temp_last;

		m_m_dot_mc = m_m_dot_pc = m_m_dot_rc = m_m_dot_t = std::numeric_limits<double>::quiet_NaN();
		m_W_dot_mc = m_W_dot_pc = m_W_dot_rc = m_W_dot_t = std::numeric_limits<double>::quiet_NaN();
		m_eta_thermal_calc_last = m_W_dot_net_last = m_energy_bal_last =
		m_objective_metric_last = m_objective_metric_opt = m_objective_metric_auto_opt = std::numeric_limits<double>::quiet_NaN();
	}

	class C_MEQ_HTR_des : public C_monotonic_equation
	{
	private:
		C_PartialCooling_Cycle *mpc_pc_cycle;

	public:
		C_MEQ_HTR_des(C_PartialCooling_Cycle *pc_pc_cycle)
		{
			mpc_pc_cycle = pc_pc_cycle;
			m_Q_dot_LTR = m_Q_dot_HTR = std::numeric_limits<double>::quiet_NaN();
		}

		// These values are calculated in the operator() method and need to be extracted from this class
		//     after convergence
		double m_Q_dot_LTR, m_Q_dot_HTR;	//[kWt]

		virtual int operator()(double T_HTR_LP_out /*K*/, double *diff_T_HTR_LP_out /*K*/);
	};

	class C_MEQ_LTR_des : public C_monotonic_equation
	{
	private:
		C_PartialCooling_Cycle *mpc_pc_cycle;

	public:
		C_MEQ_LTR_des(C_PartialCooling_Cycle *pc_pc_cycle)
		{
			mpc_pc_cycle = pc_pc_cycle;
			double m_Q_dot_LTR = std::numeric_limits<double>::quiet_NaN();
		}

		double m_Q_dot_LTR;		//[kWt]

		virtual int operator()(double T_LTR_LP_out /*K*/, double *diff_T_LTR_LP_out /*K*/);
	};


	int design(S_des_params & des_par_in);

	int opt_design(S_opt_des_params & opt_des_par_in);

	int auto_opt_design(S_auto_opt_design_parameters & auto_opt_des_par_in);
	
	// Called by 'nlopt_callback_opt_des_1', so needs to be public
	double design_cycle_return_objective_metric(const std::vector<double> &x);

	// Called by 'fmin_callback_opt_eta', so needs to be public
	double opt_eta_fixed_P_high(double P_high_opt /*kPa*/);

	/*const S_des_solved * get_design_solved()
	{
		return &ms_des_solved;
	}*/

};

double nlopt_cb_opt_partialcooling_des(const std::vector<double> &x, std::vector<double> &grad, void *data);

double fmin_cb_opt_partialcooling_des_fixed_P_high(double P_high /*kPa*/, void *data);

#endif
