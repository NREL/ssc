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

#include "sco2_pc_csp_int.h"

//#include "sco2_pc_core.h"
#include "sco2_recompression_cycle.h"
#include "sco2_partialcooling_cycle.h"

#include "csp_solver_util.h"
#include "CO2_properties.h"
#include <cmath>
#include <string>

#include "nlopt.hpp"

#include "fmin.h"

C_sco2_recomp_csp::C_sco2_recomp_csp()
{
	// Get CO2 critical temperature
	CO2_info co2_fluid_info;
	get_CO2_info(&co2_fluid_info);
	m_T_co2_crit = co2_fluid_info.T_critical;		//[K]
	m_P_co2_crit = co2_fluid_info.P_critical;		//[kPa]

	// Set default optimization strategy
	m_od_opt_objective = E_MAX_ETA;		//[-]
	m_od_opt_ftol = 1.E-4;				//[-] Relative tolerance for od optimization: objective function convergence
	m_od_opt_xtol = 1.E-4;				//[-] Relative tolerance for od optimization: independent variable convergence
	// **************************************

	// Default off-design turbomachinery operation is:
	m_off_design_turbo_operation = E_FIXED_MC_FIXED_RC_FIXED_T;

	mf_callback_update = 0;		// NULL
	mp_mf_update = 0;			// NULL
}

void C_sco2_recomp_csp::design(S_des_par des_par)
{
	ms_des_par = des_par;

	design_core();
}

void C_sco2_recomp_csp::design_core()
{
	// using -> C_RecompCycle::S_auto_opt_design_hit_eta_parameters
	std::string error_msg;
	int auto_err_code = 0;
	std::string s_cycle_config = "";

	if (ms_des_par.m_cycle_config == 2)
	{
		mpc_sco2_cycle = &mc_partialcooling_cycle;
		s_cycle_config = "partial cooling";
	}
	else
	{
		mpc_sco2_cycle = &mc_rc_cycle;
		s_cycle_config = "recompression";
	}

	// Set min temp
	m_T_mc_in_min = mpc_sco2_cycle->get_design_limits().m_T_mc_in_min;		//[K]
	
	if (ms_des_par.m_design_method == 1)
	{
		// Design the cycle to hit a specified efficiency
		// Define sCO2 cycle design parameter structure
		ms_cycle_des_par.m_W_dot_net = ms_des_par.m_W_dot_net;		//[kWe]
		ms_cycle_des_par.m_eta_thermal = ms_des_par.m_eta_thermal;	//[-]
		ms_cycle_des_par.m_T_mc_in = ms_des_par.m_T_amb_des + ms_des_par.m_dt_mc_approach;	//[K]
		if (ms_cycle_des_par.m_T_mc_in < m_T_mc_in_min)
		{
			std::string msg = util::format("The input design main compressor inlet temperature is %lg [C]."
				" The sCO2 cycle design code reset it to the minimum allowable design main compressor inlet temperature: %lg [C].",
				ms_cycle_des_par.m_T_mc_in - 273.15,
				m_T_mc_in_min - 273.15);
		}
		ms_cycle_des_par.m_T_pc_in = ms_cycle_des_par.m_T_mc_in;		//[K]
		ms_cycle_des_par.m_T_t_in = ms_des_par.m_T_htf_hot_in - ms_des_par.m_phx_dt_hot_approach;	//[K]
		ms_cycle_des_par.m_DP_LT = ms_des_par.m_DP_LT;
		ms_cycle_des_par.m_DP_HT = ms_des_par.m_DP_HT;
		ms_cycle_des_par.m_DP_PC_pre = ms_des_par.m_DP_PC;
		ms_cycle_des_par.m_DP_PC_main = ms_des_par.m_DP_PC;
		ms_cycle_des_par.m_DP_PHX = ms_des_par.m_DP_PHX;
		ms_cycle_des_par.m_LTR_eff_max = ms_des_par.m_LT_eff_max;
		ms_cycle_des_par.m_HTR_eff_max = ms_des_par.m_HT_eff_max;
		ms_cycle_des_par.m_eta_mc = ms_des_par.m_eta_mc;
		ms_cycle_des_par.m_eta_rc = ms_des_par.m_eta_rc;
		ms_cycle_des_par.m_eta_pc = ms_des_par.m_eta_pc;
		ms_cycle_des_par.m_eta_t = ms_des_par.m_eta_t;
		ms_cycle_des_par.m_N_sub_hxrs = ms_des_par.m_N_sub_hxrs;
		ms_cycle_des_par.m_P_high_limit = ms_des_par.m_P_high_limit;
		ms_cycle_des_par.m_tol = ms_des_par.m_tol;
		ms_cycle_des_par.m_opt_tol = ms_des_par.m_opt_tol;
		ms_cycle_des_par.m_N_turbine = ms_des_par.m_N_turbine;
		ms_cycle_des_par.m_is_recomp_ok = ms_des_par.m_is_recomp_ok;

		ms_cycle_des_par.m_frac_fan_power = ms_des_par.m_frac_fan_power;			//[-]
		ms_cycle_des_par.m_deltaP_cooler_frac = ms_des_par.m_deltaP_cooler_frac;	//[-]
		ms_cycle_des_par.m_T_amb_des = ms_des_par.m_T_amb_des;						//[K]
		ms_cycle_des_par.m_elevation = ms_des_par.m_elevation;						//[m]

		ms_cycle_des_par.m_des_objective_type = ms_des_par.m_des_objective_type;		//[-]
		ms_cycle_des_par.m_min_phx_deltaT = ms_des_par.m_min_phx_deltaT;				//[C]

		ms_cycle_des_par.m_fixed_P_mc_out = ms_des_par.m_fixed_P_mc_out;	//[-]

		ms_cycle_des_par.m_PR_mc_guess = ms_des_par.m_PR_mc_guess;		//[-]
		ms_cycle_des_par.m_fixed_PR_mc = ms_des_par.m_fixed_PR_mc;		//[-]

		ms_cycle_des_par.mf_callback_log = mf_callback_update;
		ms_cycle_des_par.mp_mf_active = mp_mf_update;

		auto_err_code = mpc_sco2_cycle->auto_opt_design_hit_eta(ms_cycle_des_par, error_msg);
	}
	else if (ms_des_par.m_design_method == 2)
	{
		if (ms_des_par.m_UA_recup_tot_des < 0.0)
		{
			std::string ex_msg = "The " + s_cycle_config + " cycle and CSP integration design, design method 2, conductance must be > 0";
			throw(C_csp_exception(ex_msg.c_str()));
		}
		
		C_sco2_cycle_core::S_auto_opt_design_parameters des_params;
		des_params.m_W_dot_net = ms_des_par.m_W_dot_net;		//[kWe]
		des_params.m_T_mc_in = ms_des_par.m_T_amb_des + ms_des_par.m_dt_mc_approach;	//[K]
		if (ms_cycle_des_par.m_T_mc_in < m_T_mc_in_min)
		{
			std::string msg = util::format("The input design main compressor inlet temperature is %lg [C]."
				" The sCO2 cycle design code reset it to the minimum allowable design main compressor inlet temperature: %lg [C].",
				ms_cycle_des_par.m_T_mc_in - 273.15,
				m_T_mc_in_min - 273.15);
		}
		des_params.m_T_pc_in = des_params.m_T_mc_in;		//[K]
		des_params.m_T_t_in = ms_des_par.m_T_htf_hot_in - ms_des_par.m_phx_dt_hot_approach;	//[K]
		des_params.m_DP_LTR = ms_des_par.m_DP_LT;
		des_params.m_DP_HTR = ms_des_par.m_DP_HT;
		des_params.m_DP_PC_pre = ms_des_par.m_DP_PC;
		des_params.m_DP_PC_main = ms_des_par.m_DP_PC;
		des_params.m_DP_PHX = ms_des_par.m_DP_PHX;
		des_params.m_UA_rec_total = ms_des_par.m_UA_recup_tot_des;	//[kW/K]
		des_params.m_LTR_eff_max = ms_des_par.m_LT_eff_max;			//[-]
		des_params.m_HTR_eff_max = ms_des_par.m_HT_eff_max;			//[-]
		des_params.m_eta_mc = ms_des_par.m_eta_mc;
		des_params.m_eta_rc = ms_des_par.m_eta_rc;
		des_params.m_eta_pc = ms_des_par.m_eta_pc;
		des_params.m_eta_t = ms_des_par.m_eta_t;
		des_params.m_N_sub_hxrs = ms_des_par.m_N_sub_hxrs;
		des_params.m_P_high_limit = ms_des_par.m_P_high_limit;
		des_params.m_tol = ms_des_par.m_tol;
		des_params.m_opt_tol = ms_des_par.m_opt_tol;
		des_params.m_N_turbine = ms_des_par.m_N_turbine;

		des_params.m_frac_fan_power = ms_des_par.m_frac_fan_power;			//[-]
		des_params.m_deltaP_cooler_frac = ms_des_par.m_deltaP_cooler_frac;	//[-]
		des_params.m_T_amb_des = ms_des_par.m_T_amb_des;					//[K]
		des_params.m_elevation = ms_des_par.m_elevation;					//[m]

		des_params.m_des_objective_type = ms_des_par.m_des_objective_type;		//[-]
		des_params.m_min_phx_deltaT = ms_des_par.m_min_phx_deltaT;				//[C]

		des_params.m_fixed_P_mc_out = ms_des_par.m_fixed_P_mc_out;	//[-]

		des_params.m_PR_mc_guess = ms_des_par.m_PR_mc_guess;		//[-]
		des_params.m_fixed_PR_mc = ms_des_par.m_fixed_PR_mc;		//[-]

		des_params.m_is_recomp_ok = ms_des_par.m_is_recomp_ok;

		auto_err_code = mpc_sco2_cycle->auto_opt_design(des_params);
	}
	else
	{
		std::string ex_msg = "The " + s_cycle_config + "cycle and CSP integration design, design method can only be 1 (specify UA) for now";
		throw(C_csp_exception(ex_msg.c_str()));
	}

	if (auto_err_code != 0)
	{
		throw(C_csp_exception(error_msg.c_str()));
	}

	if (error_msg.empty())
	{
		mc_messages.add_notice("The " + s_cycle_config + " cycle design optimization was successful");
	}
	else
	{
		string out_msg = "The sCO2 " + s_cycle_config + " cycle design optimization solved with the following warning(s):\n" + error_msg;
		mc_messages.add_notice(out_msg);
	}

	ms_des_solved.ms_rc_cycle_solved = *mpc_sco2_cycle->get_design_solved();

	// Initialize the PHX
	mc_phx.initialize(ms_des_par.m_hot_fl_code, ms_des_par.mc_hot_fl_props);

	// Design the PHX
	double q_dot_des_phx = ms_des_solved.ms_rc_cycle_solved.m_W_dot_net / ms_des_solved.ms_rc_cycle_solved.m_eta_thermal;
	//ms_phx_des_par.m_Q_dot_design = ms_des_solved.ms_rc_cycle_solved.m_W_dot_net / ms_des_solved.ms_rc_cycle_solved.m_eta_thermal;		//[kWt]
	ms_phx_des_par.m_T_h_in = ms_des_par.m_T_htf_hot_in;	//[K] HTF hot inlet temperature 
		// Okay, but CO2-HTF HX is assumed here. How does "structure inheritance" work?
	ms_phx_des_par.m_P_h_in = 1.0;							// Assuming HTF is incompressible...
	ms_phx_des_par.m_P_h_out = 1.0;						// Assuming HTF is incompressible...
		// .................................................................................
	ms_phx_des_par.m_T_c_in = ms_des_solved.ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::HTR_HP_OUT];		//[K]
	ms_phx_des_par.m_P_c_in = ms_des_solved.ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::HTR_HP_OUT];		//[K]
	ms_phx_des_par.m_P_c_out = ms_des_solved.ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::TURB_IN];		//[K]
	ms_phx_des_par.m_m_dot_cold_des = ms_des_solved.ms_rc_cycle_solved.m_m_dot_t;	//[kg/s]
		// Calculating the HTF mass flow rate in 'design_and_calc_m_dot_htf'
	ms_phx_des_par.m_m_dot_hot_des = std::numeric_limits<double>::quiet_NaN();
		// Set maximum effectiveness
	ms_phx_des_par.m_eff_max = 1.0;
	
	mc_phx.design_and_calc_m_dot_htf(ms_phx_des_par, q_dot_des_phx, ms_des_par.m_phx_dt_cold_approach, ms_des_solved.ms_phx_des_solved);

	//*************************************************************************************
	//*************************************************************************************

	return;
}

int C_sco2_recomp_csp::off_design_fix_P_mc_in(S_od_par od_par, double P_mc_in /*MPa*/, int off_design_strategy, double od_opt_tol)
{
	setup_off_design_info(od_par, off_design_strategy, od_opt_tol);
	
	// Now, call off-design with the optimized compressor inlet pressure		
	ms_cycle_od_par.m_P_LP_comp_in = P_mc_in*1.E3;	//[kPa] convert from MPa
	double eta_od_solved = std::numeric_limits<double>::quiet_NaN();
	int od_core_error_code = off_design_core(eta_od_solved);
	
	if (od_core_error_code == 0)
	{
		double W_dot_fan = std::numeric_limits<double>::quiet_NaN();
		int air_cooler_err_code = mpc_sco2_cycle->calculate_off_design_fan_power(ms_od_par.m_T_amb, W_dot_fan);

		if (air_cooler_err_code != 0)
		{
			throw(C_csp_exception("Off design air cooler model failed"));
		}
	}

	ms_od_solved.ms_rc_cycle_od_solved = *mpc_sco2_cycle->get_od_solved();
	ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

	return od_core_error_code;
}

void C_sco2_recomp_csp::setup_off_design_info(C_sco2_recomp_csp::S_od_par od_par, int off_design_strategy, double od_opt_tol)
{
	ms_od_par = od_par;

	// Optimization variables:
	m_od_opt_objective = off_design_strategy;
	m_od_opt_ftol = od_opt_tol;
	// ****************************************

	ms_cycle_od_par.m_T_mc_in = ms_od_par.m_T_amb + ms_des_par.m_dt_mc_approach;		//[K]
	if (ms_cycle_od_par.m_T_mc_in < m_T_mc_in_min)
	{
		std::string msg = util::format("The off-design main compressor inlet temperature is %lg [C]."
			" The sCO2 cycle off-design code reset it to the minimum allowable main compressor inlet temperature: %lg [C].",
			ms_cycle_od_par.m_T_mc_in - 273.15,
			m_T_mc_in_min - 273.15);
		ms_cycle_od_par.m_T_mc_in = m_T_mc_in_min;
	}

	if (ms_des_par.m_cycle_config == 2)
	{
		ms_cycle_od_par.m_T_pc_in = ms_od_par.m_T_amb + ms_des_par.m_dt_mc_approach;	//[K]
		if (ms_cycle_od_par.m_T_pc_in < m_T_mc_in_min)
		{
			std::string msg = util::format("The off-design main compressor in let temperture is %lg [C]."
				" The sCO2 cycle off-design code reset it to the minimum allowable main compressor inlet temperature: %lg [C].",
				ms_cycle_od_par.m_T_pc_in - 273.15,
				m_T_mc_in_min - 273.15);
			ms_cycle_od_par.m_T_pc_in = m_T_mc_in_min;
		}
	}

	// Begin with no compressor bypass
	ms_cycle_od_par.m_f_mc_pc_bypass = 0.0;

	ms_cycle_od_par.m_N_sub_hxrs = ms_des_par.m_N_sub_hxrs;			//[-]
	ms_cycle_od_par.m_tol = ms_des_par.m_tol;						//[-]

	// Defined downstream
	ms_cycle_od_par.m_T_t_in = std::numeric_limits<double>::quiet_NaN();			//[K]			
	ms_cycle_od_par.m_P_LP_comp_in = std::numeric_limits<double>::quiet_NaN();	//[kPa]
	
	// Define ms_phx_od_par
	ms_phx_od_par.m_T_h_in = ms_od_par.m_T_htf_hot;			//[K]
	ms_phx_od_par.m_P_h_in = ms_phx_des_par.m_P_h_in;		//[kPa] Assuming fluid is incompressible in that pressure doesn't affect its properties
	ms_phx_od_par.m_m_dot_h = ms_od_par.m_m_dot_htf;		//[kg/s]

	// Defined downstream
	ms_phx_od_par.m_T_c_in = std::numeric_limits<double>::quiet_NaN();		//[K]
	ms_phx_od_par.m_P_c_in = std::numeric_limits<double>::quiet_NaN();		//[kPa]
	ms_phx_od_par.m_m_dot_c = std::numeric_limits<double>::quiet_NaN();		//[kg/s]
}

int C_sco2_recomp_csp::optimize_off_design(C_sco2_recomp_csp::S_od_par od_par, int off_design_strategy, double od_opt_tol)
{
	// This sets: T_mc_in, T_pc_in, etc.
	setup_off_design_info(od_par, off_design_strategy, od_opt_tol);

	if (m_off_design_turbo_operation == E_FIXED_MC_FIXED_RC_FIXED_T)
	{
		int opt_P_LP_err = opt_P_LP_comp_in__fixed_N_turbo();
		if (opt_P_LP_err != 0 && opt_P_LP_err != -31)
		{
			throw(C_csp_exception("2D nested optimization to maximize efficiency failed"));
		}
		if (opt_P_LP_err == -31)
		{
			while (opt_P_LP_err == -31 && ms_cycle_od_par.m_f_mc_pc_bypass < 0.9)
			{
				ms_cycle_od_par.m_f_mc_pc_bypass += 0.01;
				opt_P_LP_err = opt_P_LP_comp_in__fixed_N_turbo();

				if (opt_P_LP_err != 0 && opt_P_LP_err != -31)
				{
					throw(C_csp_exception("2D nested optimization to maximize efficiency failed"));
				}
			}
			if(opt_P_LP_err != 0)
				throw(C_csp_exception("off design optimization, fixed shaft speed config, failed"));
		}
		else
		{
			// For the E_FIXED_MC_FIXED_RC_FIXED_T configuration, at off design ambient temperatures colder than design
			// ... the cycle may not be able to reach the target power and/or the cycle efficiency may improve if compressor inlet temperatures increase
			// Increase compressor inlet temperatures
			// Track power output between steps
			// If target power is achieved, store max efficiency case. Otherwise, track maximum power case
			double W_dot_opt = mpc_sco2_cycle->get_od_solved()->m_W_dot_net;	//[kWe]
			
			double eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;	//[-]

			double P_LP_in_opt = ms_cycle_od_par.m_P_LP_comp_in;		//[kPa]

			double T_mc_pc_in_opt = ms_cycle_od_par.m_T_mc_in;			//[C]
			
			while (true)
			{
				// Increase compressor inlet temperatures by constant interval
				ms_cycle_od_par.m_T_mc_in += 0.5;	//[K]
				ms_cycle_od_par.m_T_pc_in += 0.5;	//[K]

				opt_P_LP_err = opt_P_LP_comp_in__fixed_N_turbo();
				if (opt_P_LP_err != 0)
				{	// If off-design breaks, we've solved at colder temperatures, so don't crash the entire simulation
					// just exit loop that increases temperature
					// This can happen especially w/ the partial cooling cycle when bypass is not required initially, but becomes necessary in this loop
					break;
				}

				// Get most recent outputs
				double W_dot_calc = mpc_sco2_cycle->get_od_solved()->m_W_dot_net;	//[kPWe]
				double eta_calc = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;	//[-]

				// If current inlet temperatures generate more power than optimal, reset optimal
				// This relies on 'opt_P_LP_comp_in__fixed_N_turbo()' not return a power that is over-target...
				// '0.002' seems reasonable based on parametric runs but may be suboptimal
				if ((W_dot_calc - W_dot_opt) / W_dot_opt > 0.002)
				{	
					eta_max = ms_od_solved.ms_rc_cycle_od_solved.m_eta_thermal;	//[-]

					P_LP_in_opt = ms_cycle_od_par.m_P_LP_comp_in;		//[kPa]

					T_mc_pc_in_opt = ms_cycle_od_par.m_T_mc_in;			//[C]

					W_dot_opt = W_dot_calc;		//[kWe]
				}

				// Otherwise, if the current inlet temperature generates very close to the current optimal power
				// then check if the efficiency is higher. If so, save efficiency
				else if ((W_dot_calc - W_dot_opt) / W_dot_opt > -0.005)
				{
					if (eta_calc > eta_max)
					{
						eta_max = eta_calc;

						P_LP_in_opt = ms_cycle_od_par.m_P_LP_comp_in;		//[kPa]

						T_mc_pc_in_opt = ms_cycle_od_par.m_T_mc_in;			//[C]

						if(W_dot_calc > W_dot_opt)
							W_dot_opt = W_dot_calc;		//[kWe]
					}
					else if (eta_max - eta_calc > 0.01)
					{
						break;
					}
				}
				else
				{
					break;
				}
			}

			// Now solve cycle off-design with saved optimal inputs
			ms_cycle_od_par.m_P_LP_comp_in = P_LP_in_opt;		//[kPa]
			ms_cycle_od_par.m_T_mc_in = T_mc_pc_in_opt;			//[K]
			ms_cycle_od_par.m_T_pc_in = T_mc_pc_in_opt;			//[K]

			double f_od_obj = std::numeric_limits<double>::quiet_NaN();
			int od_opt_err_code = off_design_core(f_od_obj);

			if (od_opt_err_code != 0)
			{
				throw(C_csp_exception("C_sco2_recomp_csp::optimize_off_design to maximize efficiency failed"));
			}
		}		
	}
	else
	{
		throw(C_csp_exception("Off design turbomachinery operation strategy not recognized"));
	}

	double W_dot_fan = std::numeric_limits<double>::quiet_NaN();
	int air_cooler_err_code = mpc_sco2_cycle->calculate_off_design_fan_power(ms_od_par.m_T_amb, W_dot_fan);

	if (air_cooler_err_code != 0)
	{
		throw(C_csp_exception("Off design air cooler model failed"));
	}

	ms_od_solved.ms_rc_cycle_od_solved = *mpc_sco2_cycle->get_od_solved();
	ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

	return 0;
}

int C_sco2_recomp_csp::opt_P_LP_comp_in__fixed_N_turbo()
{
	// Prior to calling, need to set :
	//	*ms_od_par, ms_rc_cycle_od_phi_par, ms_phx_od_par, ms_od_op_inputs(will set P_mc_in here and f_recomp downstream)
	
	double W_dot_target = (ms_od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des) * ms_des_par.m_W_dot_net;	//[kWe]

	// Get density at design point
	double mc_dens_in_des = std::numeric_limits<double>::quiet_NaN();
	
	if (ms_des_par.m_cycle_config == 1)
		mc_dens_in_des = ms_des_solved.ms_rc_cycle_solved.m_dens[C_sco2_cycle_core::MC_IN];		//[kg/m^3]
	else
		mc_dens_in_des = ms_des_solved.ms_rc_cycle_solved.m_dens[C_sco2_cycle_core::PC_IN];		//[kg/m^3]

	CO2_state co2_props;
	// Then calculate the compressor inlet pressure that achieves this density at the off-design ambient temperature
	CO2_TD(ms_cycle_od_par.m_T_mc_in, mc_dens_in_des, &co2_props);
	double mc_pres_dens_des_od = co2_props.pres;	//[kPa]
	ms_cycle_od_par.m_P_LP_comp_in = mc_pres_dens_des_od;	//[kPa]

	bool is_find_P_LP_in_range = true;
	bool is_search_up = true;

	while (is_find_P_LP_in_range)
	{
		is_find_P_LP_in_range = false;

		double P_mc_in_guess = -1.23;
		double P_mc_in_upper = -1.23;
		double P_mc_in_lower = -1.23;

		int od_core_error_code_dens = 0;
		if (m_off_design_turbo_operation == E_FIXED_MC_FIXED_RC_FIXED_T)
		{
			double eta_od_core = std::numeric_limits<double>::quiet_NaN();
			od_core_error_code_dens = off_design_core(eta_od_core);
			if (od_core_error_code_dens == 0)
			{	// Have found one feasible compressor inlet pressure
				P_mc_in_guess = ms_cycle_od_par.m_P_LP_comp_in;	//[kPa]

				// On the first iteration, want a second pressure that is higher than the first pressure
				// However, in some cases the optimizer will converged on a solution that generates over the target pressure
				// Then, want to rerun pressure guesses and try to get to a lower inlet pressure and lower power output
				if (is_search_up)
				{
					P_mc_in_upper = P_mc_in_guess;
					// Increase compressor inlet temperature until off design returns error code
					int iter_P_mc_in_upper = 0;
					while (true)
					{
						iter_P_mc_in_upper++;
						P_mc_in_upper = 0.95*P_mc_in_upper + 0.05*ms_des_par.m_P_high_limit;	//[kPa]
						ms_cycle_od_par.m_P_LP_comp_in = P_mc_in_upper;	//[kPa]

						int od_core_error_code = off_design_core(eta_od_core);

						if (od_core_error_code == 0)
						{
							if (iter_P_mc_in_upper > 20 || (ms_des_par.m_P_high_limit - P_mc_in_upper) / ms_des_par.m_P_high_limit < 0.01)
							{
								P_mc_in_upper = ms_des_par.m_P_high_limit;	//[kPa]
							}
						}
						else
						{
							break;
						}
					}

					// Calculate P_mc_in_lower such that first guess in fmin is P_mc_in_guess
					double r = (3.0 - sqrt(5.0)) / 2.0;		// Gold section ratio
					P_mc_in_lower = (P_mc_in_guess - (r*P_mc_in_upper)) / (1.0 - r);
				}
				else
				{
					P_mc_in_upper = P_mc_in_guess;
					
					int iter_P_mc_in_guess = 0;

					while (true && ms_cycle_od_par.m_P_LP_comp_in > 1000.0)
					{
						iter_P_mc_in_guess++;

						P_mc_in_guess -= 500.0;

						ms_cycle_od_par.m_P_LP_comp_in = P_mc_in_guess;	//[kPa]

						int od_core_error_code = off_design_core(eta_od_core);

						if (od_core_error_code != 0)
							break;
					}

					if (iter_P_mc_in_guess == 1)
						P_mc_in_guess += 250.0;
					else
						P_mc_in_guess += 500.0;

					// Calculate P_mc_in_lower such that first guess in fmin is P_mc_in_guess
					double r = (3.0 - sqrt(5.0)) / 2.0;		// Gold section ratio
					P_mc_in_lower = (P_mc_in_guess - (r*P_mc_in_upper)) / (1.0 - r);
				}
			}
			else if (od_core_error_code_dens == -14 || od_core_error_code_dens == 4 ||
				od_core_error_code_dens == 5 || od_core_error_code_dens == -11 )
			{	// Compressor outlet pressure is too high; decrease compressor inlet pressure

				while (true)
				{
					P_mc_in_upper = ms_cycle_od_par.m_P_LP_comp_in;	//[kPa]
					P_mc_in_guess = 0.98*P_mc_in_upper;					//[kPa]

					ms_cycle_od_par.m_P_LP_comp_in = P_mc_in_guess;	//[kPa]

					int od_core_error_code = off_design_core(eta_od_core);

					if (od_core_error_code == 0)
					{
						break;
					}
					else if (P_mc_in_guess < 1000)
					{
						return -31;
					}
				}

				// Calculate P_mc_in_lower such that first guess in fmin is P_mc_in_guess
				double r = (3.0 - sqrt(5.0)) / 2.0;		// Gold section ratio
				P_mc_in_lower = (P_mc_in_guess - (r*P_mc_in_upper)) / (1.0 - r);
			}
			else if (od_core_error_code_dens == -12 || od_core_error_code_dens == -10)
			{
				while (true)
				{
					P_mc_in_lower = ms_cycle_od_par.m_P_LP_comp_in;	//[kPa]
					P_mc_in_guess = 1.02*P_mc_in_lower;				//[kPa]

					ms_cycle_od_par.m_P_LP_comp_in = P_mc_in_guess;	//[kPa]

					int od_core_error_code = off_design_core(eta_od_core);

					if (od_core_error_code == 0)
					{
						break;
					}
					else if (od_core_error_code == -14 || P_mc_in_guess > 0.95*ms_des_par.m_P_high_limit)
					{
						return -31;
					}
				}

				// Calculate P_mc_in_upper such that first guess in fmin is P_mc_in_guess
				double r = (3.0 - sqrt(5.0)) / 2.0;		// Gold section ratio
				P_mc_in_upper = (P_mc_in_guess - P_mc_in_lower * (1.0 - r)) / r;
			}
			else
			{
				std::string err_msg = util::format("Off design error code %d current not accounted for", od_core_error_code_dens);
				throw(C_csp_exception(err_msg, ""));
			}

		}
		else
		{
			throw(C_csp_exception("Off design operation mode not recognized"));
		}

		// At this point, should have calculated above in this method:
		// * P_mc_in_lower
		// * P_mc_in_guess
		// * P_mc_in_upper

		int opt_code = 1;

		int opt_iter = 0;
		while (true)
		{
			opt_iter++;

			// Optimize compressor inlet pressure using fmin
			double P_mc_in_opt = std::numeric_limits<double>::quiet_NaN();

			if (opt_code == 0)
			{
				P_mc_in_opt = fminbr(P_mc_in_lower, P_mc_in_upper,
					&fmin_opt_P_LP_in__fixed_N_turbo, this, m_od_opt_ftol);
			}
			else
			{
				std::vector<double> x;
				std::vector<double> lb;
				std::vector<double> ub;
				std::vector<double> scale;

				x.resize(1, P_mc_in_guess);	//[kPa]

				lb.resize(1, P_mc_in_lower);	//[kPa]

				ub.resize(1, P_mc_in_upper);	//[kPa]

				scale.resize(1);
				double diff_P_low = P_mc_in_guess - P_mc_in_lower;
				double diff_P_up = P_mc_in_upper - P_mc_in_guess;
				if (diff_P_up > diff_P_low)
					scale[0] = -0.99*diff_P_low;
				else
					scale[0] = 0.99*diff_P_up;

				//scale[0] = (0.5*P_mc_in_upper + 0.5*P_mc_in_lower) - x[0];	//[kPa]

			// Set up instance of nlopt class and set optimization parameters
			//nlopt::opt  nlopt_P_mc_in_opt_max_of(nlopt::LN_NELDERMEAD, 1);
				nlopt::opt  nlopt_P_mc_in_opt_max_of(nlopt::LN_SBPLX, 1);

				nlopt_P_mc_in_opt_max_of.set_lower_bounds(lb);
				nlopt_P_mc_in_opt_max_of.set_upper_bounds(ub);
				//nlopt_P_mc_in_opt_max_of.set_initial_step(scale);
				nlopt_P_mc_in_opt_max_of.set_xtol_rel(m_od_opt_xtol);
				nlopt_P_mc_in_opt_max_of.set_ftol_rel(m_od_opt_ftol);

				// Set max objective function
				nlopt_P_mc_in_opt_max_of.set_max_objective(nlopt_opt_P_LP_in__fixed_N_turbo, this);

				m_nlopt_iter = 0;

				double nlopt_max_eta = std::numeric_limits<double>::quiet_NaN();
				nlopt::result    nlopt_result = nlopt_P_mc_in_opt_max_of.optimize(x, nlopt_max_eta);

				P_mc_in_opt = x[0];

				if (nlopt_max_eta != nlopt_max_eta)
				{
					P_mc_in_opt = P_mc_in_guess;
				}
			}

			// Now, call off-design with the optimized compressor inlet pressure		
			ms_cycle_od_par.m_P_LP_comp_in = P_mc_in_opt;	//[kPa]
			double eta_od_core_1st = std::numeric_limits<double>::quiet_NaN();
			int od_core_error_code = off_design_core(eta_od_core_1st);
			if (od_core_error_code == 0)
			{
				if ((P_mc_in_opt - P_mc_in_lower) / P_mc_in_lower < 0.005)
				{
					// Need to check for an optimum pressure that is less than P_mc_in_lower...
					double r = (3. - sqrt(5.0)) / 2;       /* Gold section ratio           */

					P_mc_in_guess = P_mc_in_opt;
					P_mc_in_lower = 0.8*P_mc_in_opt;
					P_mc_in_upper = (P_mc_in_guess - (1.0 - r)*P_mc_in_lower) / r;

					continue;

					//throw(C_csp_exception("Don't have code to handle optimal pressure equaling miminum pressure"));
				}
				else if (P_mc_in_opt == P_mc_in_upper && P_mc_in_opt < ms_des_par.m_P_high_limit)
				{
					// Need to check for an optimum pressure that is less than P_mc_in_lower...
					double r = (3. - sqrt(5.0)) / 2;       /* Gold section ratio           */

					P_mc_in_guess = P_mc_in_opt;
					P_mc_in_lower = 0.9*P_mc_in_opt;
					P_mc_in_upper = (P_mc_in_guess - (1.0 - r)*P_mc_in_lower) / r;

					continue;

					//throw(C_csp_exception("Don't have code to handle optimal pressure equaling maximum pressure"));
				}
				else
				{
					break;
				}
			}
			else if (od_core_error_code == -14)
			{
				// The objective function penalizes the output if the cycle is over pressure
				// But it could choose a solution that is slightly over pressure that triggers the error code
				// In this case, step down the inlet pressure until the solution succeeds or 20 iterations
				int i_P_mc_in_guess = 0;
				while (i_P_mc_in_guess < 20 && od_core_error_code == -14)
				{
					i_P_mc_in_guess++;

					P_mc_in_guess *= 0.998;

					ms_cycle_od_par.m_P_LP_comp_in = P_mc_in_guess;		//[kPa]

					od_core_error_code = off_design_core(eta_od_core_1st);
				}
			}

			if (od_core_error_code != 0)
			{
				throw(C_csp_exception("Off-design optimization on compressor inlet pressure failed",
					"C_sco2_recomp_csp::opt_P_mc_in_nest_f_recomp_max_eta_core"));
			}
		}

		double W_dot_nd = (mpc_sco2_cycle->get_od_solved()->m_W_dot_net - W_dot_target) / W_dot_target;	//[-]
		
		// If the solution power output is greater than the target, try decreasing the pressure and re-running the loop
		if (W_dot_nd > 0.002 && ms_cycle_od_par.m_P_LP_comp_in > 1000.0)
		{
			ms_cycle_od_par.m_P_LP_comp_in = 0.99*P_mc_in_lower;
			is_find_P_LP_in_range = true;
			is_search_up = false;
		}
	}

	ms_od_solved.ms_rc_cycle_od_solved = *mpc_sco2_cycle->get_od_solved();
	ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

	return 0;
}

double C_sco2_recomp_csp::adjust_P_mc_in_away_2phase(double T_co2 /*K*/, double P_mc_in /*kPa*/)
{	
	double P_mc_in_restricted = std::numeric_limits<double>::quiet_NaN();	//[kPa]
	CO2_state co2_props;
	// Is T_co2 < the critical temperature
	if (T_co2 < m_T_co2_crit)
	{
		CO2_TQ(T_co2, 0.0, &co2_props);
		P_mc_in_restricted = co2_props.pres;	//[kPa]
	}
	else if (T_co2 < 1.001*m_T_co2_crit)
	{	// Else, T_co2 > the critical temperature
		P_mc_in_restricted = m_P_co2_crit;		//[kPa]
	}
	else
	{
		return P_mc_in;	//[kPa]
	}
	
	if (P_mc_in >= P_mc_in_restricted)
	{
		double P_upper = 1.01*P_mc_in_restricted;
		if (P_mc_in < P_upper)
		{
			double P_mid = 1.005*P_mc_in_restricted;
			return P_upper - (P_upper - P_mc_in)/(P_upper - P_mc_in_restricted)*(P_upper-P_mid);
		}
		else
		{
			return P_mc_in;		//[kPa]
		}
	}
	else
	{
		double P_lower = 0.99*P_mc_in_restricted;
		if (P_mc_in > P_lower)
		{
			return P_lower + (P_mc_in - P_lower)/(P_mc_in_restricted - P_lower)*(P_mc_in-P_lower);
		}
		else
		{
			return P_mc_in;		//[kPa]
		}
	}

}

int C_sco2_recomp_csp::off_design_core(double & eta_solved)
{
	ms_cycle_od_par.m_P_LP_comp_in = adjust_P_mc_in_away_2phase(ms_cycle_od_par.m_T_mc_in, ms_cycle_od_par.m_P_LP_comp_in);

	// Apply 1 var solver to find the turbine inlet temperature that results in a "converged" PHX
	C_mono_eq_T_t_in c_phx_cycle(this);
	C_monotonic_eq_solver c_phx_cycle_solver(c_phx_cycle);

	// Set upper and lower bounds
	double T_t_upper = ms_phx_od_par.m_T_h_in;		//[K] Upper CO2 limit is HTF hot temperature
	double T_t_lower = 373.15;						//[K] Lower CO2 limit is something fairly low, I guess

	// Generate guess values
	double T_t_guess_upper = ms_phx_od_par.m_T_h_in - ms_des_par.m_phx_dt_hot_approach;	//[K] One reasonable guess might be to apply the design approach
	double T_t_guess_lower = T_t_guess_upper - 20.0;		//[K] This might be another reasonable guess...

	// Set solver settings
	// Because this application of solver is trying to get outlet to match guess, need to calculate error in function
	// So it's already relative, and solver is looking at an absolute value
	c_phx_cycle_solver.settings(ms_des_par.m_tol/10.0, 50, T_t_lower, T_t_upper, false);

	// Now, solve for the turbine inlet temperature
	double T_t_solved, tol_solved;
	T_t_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int phx_cycle_code = 0;
	try
	{
		phx_cycle_code = c_phx_cycle_solver.solve(T_t_guess_lower, T_t_guess_upper, 0.0, T_t_solved, tol_solved, iter_solved);
	}
	catch( C_csp_exception )
	{
		eta_solved = 0.0;
		ms_od_solved.m_od_error_code = -1;
		ms_od_solved.m_is_converged = false;
		return ms_od_solved.m_od_error_code;
	}

	if( phx_cycle_code != C_monotonic_eq_solver::CONVERGED )
	{
		int n_call_history = (int)c_phx_cycle_solver.get_solver_call_history()->size();

		eta_solved = 0.0;
		
		int nested_error_code = (*(c_phx_cycle_solver.get_solver_call_history()))[n_call_history - 1].err_code;

		if(nested_error_code == 0)
		{
			nested_error_code = phx_cycle_code;
		}

		ms_od_solved.m_od_error_code = nested_error_code;
		ms_od_solved.m_is_converged = false;
		return nested_error_code;
	}
	ms_od_solved.m_is_converged = true;

	// Now, need to filter results that exceed temperature/pressure/other limitations
	// 1) Don't let the turbine inlet temperature exceed the design inlet temperature
	double over_T_t_in = 0.0;
	// double over_T_t_in = max(0.0, T_t_solved - mc_rc_cycle.get_design_solved()->m_temp[6-1]);

	// 2) Don't let the upper pressure in the system exceed the specified max (typically also = design point P_high)
	double over_P_high = max(0.0, (mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::MC_OUT] - 0.9999*ms_des_par.m_P_high_limit) / 1.E3);

	// 3) Check compressor(s) tip ratio?
	double mc_w_tip_ratio = mpc_sco2_cycle->get_od_solved()->ms_mc_ms_od_solved.m_tip_ratio_max;
	// Recompressor has multiple stages, it's reporting the fastest tip speed
	double rc_w_tip_ratio = 0.0;
	if( ms_des_solved.ms_rc_cycle_solved.m_is_rc )
	{
		rc_w_tip_ratio = mpc_sco2_cycle->get_od_solved()->ms_rc_ms_od_solved.m_tip_ratio_max;
	}
	double pc_w_tip_ratio = 0.0;
	if (ms_des_par.m_cycle_config == 2)
	{
		pc_w_tip_ratio = mpc_sco2_cycle->get_od_solved()->ms_pc_ms_od_solved.m_tip_ratio_max;
	}
	double comp_tip_ratio = max(pc_w_tip_ratio, max(mc_w_tip_ratio, rc_w_tip_ratio));
	double over_tip_ratio = max(0.0, 10.0*(comp_tip_ratio - 0.999));

	// 4) Check for compressor(s) surge?
	// Main compressor
	double mc_phi = mpc_sco2_cycle->get_od_solved()->ms_mc_ms_od_solved.m_phi_min;
	double over_surge_mc = max(0.0, (C_comp_single_stage::m_snl_phi_min - mc_phi) / C_comp_single_stage::m_snl_phi_min*100.0);
	// Recompressor
	double over_surge_rc = 0.0;
	if( ms_des_solved.ms_rc_cycle_solved.m_is_rc )
	{
		double rc_phi_min = mpc_sco2_cycle->get_od_solved()->ms_rc_ms_od_solved.m_phi_min;
		over_surge_rc = max(0.0, (C_comp_single_stage::m_snl_phi_min - rc_phi_min) / C_comp_single_stage::m_snl_phi_min*100.0);
	}
	// Pre-compressor
	double over_surge_pc = 0.0;
	if (ms_des_par.m_cycle_config == 2)
	{
		double pc_phi_min = mpc_sco2_cycle->get_od_solved()->ms_pc_ms_od_solved.m_phi_min;
		over_surge_pc = max(0.0, (C_comp_single_stage::m_snl_phi_min - pc_phi_min) / C_comp_single_stage::m_snl_phi_min*100.0);
	}


	// 5) Constrain HTF temperature difference?

	// Want thermal efficiency gradient, not step change, as turbine inlet temperature exceeds design
	// ... to help the solver
	double eta_T_t_in_scale = exp(-over_T_t_in);
	double eta_P_high_scale = exp(-over_P_high);
	double eta_tip_ratio_scale = exp(-over_tip_ratio);
	double eta_surge_mc_scale = exp(-over_surge_mc);
	double eta_surge_rc_scale = exp(-over_surge_rc);
	double eta_surge_pc_scale = exp(-over_surge_pc);

	int od_solve_code = 0;

	// If a problem with the solved operation
	//  then overwrite integer that code returns to calling program
	if(over_T_t_in != 0.0)
		od_solve_code = E_TURBINE_INLET_OVER_TEMP;
	else if(mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::MC_OUT] > ms_des_par.m_P_high_limit )
		od_solve_code = E_OVER_PRESSURE;
	else if (over_tip_ratio >= 1.0)
		od_solve_code = E_TIP_RATIO;
	else if(over_surge_mc != 0.0)
		od_solve_code = E_MC_SURGE;
	else if(over_surge_rc != 0.0)
		od_solve_code = E_RC_SURGE;
	else if(over_surge_pc != 0.0)
		od_solve_code = E_PC_SURGE;

	double scale_product = eta_T_t_in_scale*
		eta_P_high_scale*
		eta_tip_ratio_scale*
		eta_surge_mc_scale*
		eta_surge_rc_scale*
		eta_surge_pc_scale;

	switch( m_od_opt_objective )
	{
	case E_MAX_ETA:
		eta_solved = mpc_sco2_cycle->get_od_solved()->m_eta_thermal*scale_product;
		break;
	case E_MAX_POWER:
		eta_solved = mpc_sco2_cycle->get_od_solved()->m_W_dot_net / 1.E3*scale_product;		//[MWe]
		break;
	case E_TARGET_POWER_ETA_MAX:
	{
		double W_dot_target = (ms_od_par.m_m_dot_htf / ms_phx_des_par.m_m_dot_hot_des) * ms_des_par.m_W_dot_net;	//[kWe]
		double W_dot_diff_rel = (mpc_sco2_cycle->get_od_solved()->m_W_dot_net - W_dot_target) / W_dot_target;

		// Prefer the optimizer to miss with a under-target power output rather than an over-target power
		// So add weight to the over-target case
		double W_dot_nd = -W_dot_diff_rel;	//[-]
		if (W_dot_diff_rel > 0.0)
			W_dot_nd = 1.25*W_dot_diff_rel;		//[-]
		//if (W_dot_nd > 0.001 || od_solve_code != 0)
		//{
		//	eta_solved = (1.0 - W_dot_nd)*scale_product;
		//}
		//else
		//{
		//	eta_solved = (1.0 - W_dot_nd + mpc_sco2_cycle->get_od_solved()->m_eta_thermal*1.E-3)*scale_product;
		//}
		//eta_solved = eta_solved*1.E3;
		eta_solved = (1.0 - W_dot_nd)*scale_product*1.E3;
	}
		break;

	default:
		std::string err_msg = util::format("The off-design optimization objective code, %d, is not recognized.", m_od_opt_objective);
		throw(C_csp_exception(err_msg, "C_sco2_recomp_csp::off_design_core"));
		
	}


	if(eta_solved != eta_solved)
	{
		eta_solved = 0.0;
	}

	ms_od_solved.m_od_error_code = od_solve_code;

	// Want to make an efficiency value available to the optimization although it may be decreased by system operation constraints
	if( !(od_solve_code == 0 || od_solve_code == E_TURBINE_INLET_OVER_TEMP || od_solve_code == E_OVER_PRESSURE ||
		od_solve_code == E_TIP_RATIO || od_solve_code == E_MC_SURGE || od_solve_code == E_RC_SURGE || od_solve_code == E_PC_SURGE) )
		return 0;


	return od_solve_code;
}

int C_sco2_recomp_csp::off_design(S_od_par od_par, S_od_operation_inputs od_op_inputs)
{
	setup_off_design_info(od_par, -1, 1.E-3);

		// Setting pressure, here
	ms_cycle_od_par.m_P_LP_comp_in = od_op_inputs.m_P_mc_in;			//[kPa]

	double eta_solved = std::numeric_limits<double>::quiet_NaN();

	// Don't care about the objective for a single off design call, but need to set it to something...
	int od_code = off_design_core(eta_solved);
	
	ms_od_solved.ms_rc_cycle_od_solved = *mpc_sco2_cycle->get_od_solved();
	ms_od_solved.ms_phx_od_solved = mc_phx.ms_od_solved;

	return od_code;
}

int C_sco2_recomp_csp::C_mono_eq_T_t_in::operator()(double T_t_in /*K*/, double *diff_T_t_in /*-*/)
{
	// Using:
	//	-mc_rc_cycle
	//	-ms_rc_cycle_od_par
	//	-ms_phx_od_par

	// 1) Update Turbine Inlet Temperature in sco2 cycle off design parameter structure
	mpc_sco2_rc->ms_cycle_od_par.m_T_t_in = T_t_in;

	// 2) Solve the off-design cycle model with off design parameter structure
	int rc_od_error_code = 0;
	
	if( mpc_sco2_rc->m_off_design_turbo_operation == E_FIXED_MC_FIXED_RC_FIXED_T )
	{
		rc_od_error_code = mpc_sco2_rc->mpc_sco2_cycle->off_design_fix_shaft_speeds(mpc_sco2_rc->ms_cycle_od_par);
	}
	else
	{
		throw(C_csp_exception("Off design turbomachinery operation strategy not recognized"));
	}

	// If off-design cycle model did not solve, return to solver
	if( rc_od_error_code != 0 )
	{
		*diff_T_t_in = std::numeric_limits<double>::quiet_NaN();
		return rc_od_error_code;
	}

	// Solve PHX heat exchanger performance using CO2 and HTF *inlet* conditions
	mpc_sco2_rc->ms_phx_od_par.m_T_c_in = mpc_sco2_rc->mpc_sco2_cycle->get_od_solved()->m_temp[C_sco2_cycle_core::HTR_HP_OUT];	//[K]
	mpc_sco2_rc->ms_phx_od_par.m_P_c_in = mpc_sco2_rc->mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::HTR_HP_OUT];	//[kPa]
	mpc_sco2_rc->ms_phx_od_par.m_m_dot_c = mpc_sco2_rc->mpc_sco2_cycle->get_od_solved()->m_m_dot_t;		//[kg/s]
	double P_c_out = mpc_sco2_rc->mpc_sco2_cycle->get_od_solved()->m_pres[C_sco2_cycle_core::TURB_IN];		//[kPa]
	double q_dot, T_co2_phx_out, T_htf_cold;
	q_dot = T_co2_phx_out = T_htf_cold = std::numeric_limits<double>::quiet_NaN();
	
	// Solves HX performance. 
	// If successful, this call updates 'ms_od_solved'
	try
	{
		mpc_sco2_rc->mc_phx.off_design_solution(mpc_sco2_rc->ms_phx_od_par.m_T_c_in, mpc_sco2_rc->ms_phx_od_par.m_P_c_in, mpc_sco2_rc->ms_phx_od_par.m_m_dot_c, P_c_out,
			mpc_sco2_rc->ms_phx_od_par.m_T_h_in, mpc_sco2_rc->ms_phx_od_par.m_P_h_in, mpc_sco2_rc->ms_phx_od_par.m_m_dot_h, mpc_sco2_rc->ms_phx_od_par.m_P_h_in,
			q_dot, T_co2_phx_out, T_htf_cold);
	}						
	catch( C_csp_exception )
	{
		// reset 'diff_T_t_in' to NaN
		*diff_T_t_in = std::numeric_limits<double>::quiet_NaN();
		
		return -1;
	}
	
	*diff_T_t_in = (T_co2_phx_out - T_t_in) / T_t_in;
	return 0;
}

int C_sco2_recomp_csp::C_sco2_csp_od::operator()(S_f_inputs inputs, S_f_outputs & outputs)
{
	S_od_par sco2_od_par;
	sco2_od_par.m_T_htf_hot = inputs.m_T_htf_hot + 273.15;	//[K] convert from C
	sco2_od_par.m_m_dot_htf = mpc_sco2_rc->get_phx_des_par()->m_m_dot_hot_des*inputs.m_m_dot_htf_ND;	//[kg/s] scale from [-]
	sco2_od_par.m_T_amb = inputs.m_T_amb + 273.15;			//[K] convert from C

	int od_strategy = C_sco2_recomp_csp::E_TARGET_POWER_ETA_MAX;

	int off_design_code = -1;	//[-]

	try
	{
		off_design_code = mpc_sco2_rc->optimize_off_design(sco2_od_par, od_strategy);
	}
	catch (C_csp_exception &)
	{
		return -1;
	}
	// Cycle off-design may want to operate below this value, so ND value could be < 1 everywhere
	double W_dot_gross_design = mpc_sco2_rc->get_design_solved()->ms_rc_cycle_solved.m_W_dot_net;	//[kWe]
	double Q_dot_in_design = mpc_sco2_rc->get_design_solved()->ms_rc_cycle_solved.m_W_dot_net
								/ mpc_sco2_rc->get_design_solved()->ms_rc_cycle_solved.m_eta_thermal;	//[kWt]
	double W_dot_cooler_tot_design = mpc_sco2_rc->get_design_solved()->ms_rc_cycle_solved.m_W_dot_cooler_tot;	//[kWe]

	outputs.m_W_dot_gross_ND = mpc_sco2_rc->get_od_solved()->ms_rc_cycle_od_solved.m_W_dot_net
								/ W_dot_gross_design;

	outputs.m_Q_dot_in_ND = mpc_sco2_rc->get_od_solved()->ms_rc_cycle_od_solved.m_Q_dot
								/ Q_dot_in_design;

	outputs.m_W_dot_cooling_ND = mpc_sco2_rc->get_od_solved()->ms_rc_cycle_od_solved.m_W_dot_cooler_tot
								/ W_dot_cooler_tot_design;	
	
	//outputs.m_W_dot_cooling_ND = outputs.m_W_dot_gross_ND;

	outputs.m_m_dot_water_ND = 1.0;	

	return off_design_code;
}

int C_sco2_recomp_csp::generate_ud_pc_tables(double T_htf_low /*C*/, double T_htf_high /*C*/, int n_T_htf /*-*/,
	double T_amb_low /*C*/, double T_amb_high /*C*/, int n_T_amb /*-*/,
	double m_dot_htf_ND_low /*-*/, double m_dot_htf_ND_high /*-*/, int n_m_dot_htf_ND,
	util::matrix_t<double> & T_htf_ind, util::matrix_t<double> & T_amb_ind, util::matrix_t<double> & m_dot_htf_ND_ind)
{
	C_sco2_csp_od c_sco2_csp(this);
	C_ud_pc_table_generator c_sco2_ud_pc(c_sco2_csp);

	c_sco2_ud_pc.mf_callback = mf_callback_update;
	c_sco2_ud_pc.mp_mf_active = mp_mf_update;

	double T_htf_ref = ms_des_par.m_T_htf_hot_in - 273.15;	//[C] convert from K
	double T_amb_ref = ms_des_par.m_T_amb_des - 273.15;		//[C] convert from K
	double m_dot_htf_ND_ref = 1.0;							//[-]

	int ud_pc_error_code = c_sco2_ud_pc.generate_tables(T_htf_ref, T_htf_low, T_htf_high, n_T_htf,
								T_amb_ref, T_amb_low, T_amb_high, n_T_amb,
								m_dot_htf_ND_ref, m_dot_htf_ND_low, m_dot_htf_ND_high, n_m_dot_htf_ND,
								T_htf_ind, T_amb_ind, m_dot_htf_ND_ind);

	return ud_pc_error_code;
}

const C_sco2_recomp_csp::S_des_par * C_sco2_recomp_csp::get_design_par()
{
	return &ms_des_par;
}

const C_sco2_recomp_csp::S_des_solved * C_sco2_recomp_csp::get_design_solved()
{
	return &ms_des_solved;
}

const C_HX_counterflow::S_des_calc_UA_par * C_sco2_recomp_csp::get_phx_des_par()
{
	return &ms_phx_des_par;
}

const C_sco2_recomp_csp::S_od_solved * C_sco2_recomp_csp::get_od_solved()
{
	return &ms_od_solved;
}

double C_sco2_recomp_csp::opt_P_LP_in__fixed_N_turbo__return_f_obj(double P_mc_in /*kPa*/)
{
	m_nlopt_iter++;
	
	ms_cycle_od_par.m_P_LP_comp_in = P_mc_in;	//[kPa]	

	double f_obj_max = std::numeric_limits<double>::quiet_NaN();

	if( m_off_design_turbo_operation == E_FIXED_MC_FIXED_RC_FIXED_T )
	{
		off_design_core(f_obj_max);
	}
	else 
	{
		throw(C_csp_exception("Off design turbomachinery operation strategy not recognized"));
	}

	if( !std::isfinite(f_obj_max) )
	{
		f_obj_max = 0.0;
	}

	return f_obj_max;
}

double nlopt_opt_P_LP_in__fixed_N_turbo(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_sco2_recomp_csp *frame = static_cast<C_sco2_recomp_csp*>(data);
	if( frame != NULL )  
		return frame->opt_P_LP_in__fixed_N_turbo__return_f_obj(x[0]);
	else
		return 0;
}

double fmin_opt_P_LP_in__fixed_N_turbo(double x, void *data)
{
	C_sco2_recomp_csp *frame = static_cast<C_sco2_recomp_csp*>(data);
	if( frame != NULL )  
		return -(frame->opt_P_LP_in__fixed_N_turbo__return_f_obj(x));
	else
		return 0;
}
