/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "core.h"
#include "common.h"

#include "csp_common.h"

#include <ctime>

#include "sco2_pc_csp_int.h"

static var_info _cm_vtab_sco2_csp_system[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                                    UNITS     META  GROUP REQUIRED_IF CONSTRAINTS     UI_HINTS*/
	// ** Off-design Inputs **
    { SSC_INPUT,  SSC_NUMBER,  "od_T_t_in_mode",       "0: model solves co2/HTF PHX od model to calculate turbine inlet temp, 1: model sets turbine inlet temp to HTF hot temp", "", "", "", "?=0", "", ""},  // default to solving PHX
    { SSC_INPUT,  SSC_MATRIX,  "od_cases",             "Columns: T_htf_C, m_dot_htf_ND, T_amb_C, f_N_rc (=1 use design, <0, frac_des = abs(input), f_N_mc (=1 use design, <0, frac_des = abs(input), Rows: cases",   "",           "",    "",      "",      "",       "" },
	{ SSC_INPUT,  SSC_ARRAY,   "od_P_mc_in_sweep",     "Columns: T_htf_C, m_dot_htf_ND, T_amb_C, f_N_rc (=1 use design, <0, frac_des = abs(input), f_N_mc (=1 use design, <0, frac_des = abs(input)", "", "", "", "",  "", "" },
	{ SSC_INPUT,  SSC_MATRIX,  "od_set_control",       "Columns: T_htf_C, m_dot_htf_ND, T_amb_C, P_LP_in, Rows: cases", "", "", "", "", "", "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_gen_od_polynomials","Generate off-design polynomials for Generic CSP models? 1 = Yes, 0 = No", "", "", "",  "?=0",     "",       "" },

	// ** Off-Design Outputs **
		// Parameters
	{ SSC_OUTPUT, SSC_ARRAY,   "m_dot_htf_fracs",      "Normalized mass flow rate",                              "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_amb_od",             "Ambient temperatures",                                   "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_htf_hot_od",         "HTF hot temperatures",                                   "C",          "",    "",      "",     "",       "" },
		// Cycle control parameters
	{ SSC_OUTPUT, SSC_ARRAY,   "od_opt_obj_code",      "1: MAX_ETA, 2: MAX_POWER",                               "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "od_opt_conv_tol",      "Off design optimizer convergence tolerance",             "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "P_comp_in_od",         "Main compressor inlet pressures",                        "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "mc_phi_od",            "Off-design main compressor flow coefficient [od run][stage]", "",      "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "recomp_frac_od",       "Recompression fractions",                                "",           "",    "",      "",     "",       "" },
		// Optimizer outputs
	{ SSC_OUTPUT, SSC_ARRAY,   "sim_time_od",          "Simulation time for off design optimization",            "s",          "",    "",      "",     "",       "" },
		// System solution
	{ SSC_OUTPUT, SSC_ARRAY,   "eta_thermal_od",       "Off-design cycle thermal efficiency",                    "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_mc_in_od",           "Off-design compressor inlet temperature",                "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "P_mc_out_od",          "Off-design high side pressure",                          "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_htf_cold_od",        "Off-design cold return temperature",                     "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "m_dot_co2_full_od",    "Off-design mass flow rate through turbine",              "kg/s",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "W_dot_net_od",         "Off-design cycle net output (no cooling pars)",          "MWe",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "Q_dot_od",             "Off-design thermal input",                               "MWt",        "",    "",      "",     "",       "" },
		// Compressor
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_T_out_od",          "Off-design main compressor outlet temperature",          "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_W_dot_od",          "Off-design main compressor power",                       "MWe",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_m_dot_od",          "Off-design main compressor mass flow",                   "kg/s",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_rho_in_od",         "Off-design main compressor inlet density",               "kg/m3",      "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_MATRIX,  "mc_psi_od",            "Off-design main compressor ideal head coefficient [od run][stage]","", "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "mc_ideal_spec_work_od","Off-design main compressor ideal specific work",         "kJ/kg",      "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_N_od",              "Off-design main compressor speed",                       "rpm",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_eta_od",            "Off-design main compressor overall isentropic efficiency", "",         "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "mc_tip_ratio_od",      "Off-design main compressor tip speed ratio [od run][stage]", "",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "mc_eta_stages_od",     "Off-design main compressor stages isentropic efficiency [od run][stage]", "", "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_f_bypass_od",       "Off-design main compressor bypass to cooler inlet",      "-",          "",    "",      "",     "",       "" },
		// Recompressor
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_T_in_od",           "Off-design recompressor inlet temperature",              "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_P_in_od",           "Off-design recompressor inlet pressure",                 "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_T_out_od",          "Off-design recompressor outlet temperature",             "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_P_out_od",          "Off-design recompressor outlet pressure",                "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_W_dot_od",          "Off-design recompressor power",                          "MWe",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_m_dot_od",          "Off-design recompressor mass flow",                      "kg/s",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_eta_od",            "Off-design recompressor overal isentropic efficiency",   "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "rc_phi_od",            "Off-design recompressor flow coefficients [od run][stage]", "-",	   "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_MATRIX,  "rc_psi_od",            "Off-design recompressor ideal head coefficient [od run][stage]", "-",  "",    "",      "",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "rc_N_od",              "Off-design recompressor shaft speed",                    "rpm",		   "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "rc_tip_ratio_od",      "Off-design recompressor tip speed ratio [od run][stage]","-",		   "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "rc_eta_stages_od",     "Off-design recompressor stages isentropic efficiency [od run][stage]", "",    "",    "",      "",     "",       "" },
		// Precompressor
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_T_in_od",           "Off-design precompressor inlet temperature",             "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_P_in_od",           "Off-design precompressor inlet pressure",                "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_W_dot_od",          "Off-design precompressor power",                         "MWe",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_m_dot_od",          "Off-design precompressor mass flow",                     "kg/s",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_eta_od",            "Off-design precompressor overal isentropic efficiency",  "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "pc_phi_od",            "Off-design precompressor flow coefficient [od run][stage]", "-",	   "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_N_od",              "Off-design precompressor shaft speed",                   "rpm",		   "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "pc_tip_ratio_od",      "Off-design precompressor tip speed ratio [od run][stage]","-",		   "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "pc_eta_stages_od",     "Off-design precompressor stages isentropic efficiency [od run][stage]", "",    "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_f_bypass_od",       "Off-design precompressor bypass to cooler inlet",        "-",          "",    "",      "",     "",       "" },
		// Compressor Totals	
	{ SSC_OUTPUT, SSC_ARRAY,   "c_tot_W_dot_od",       "Compressor total off-design power",                      "MWe",        "",    "",      "",     "",       "" },
		// Turbine																											   
	{ SSC_OUTPUT, SSC_ARRAY,   "t_P_in_od",            "Off-design turbine inlet pressure",                      "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_T_out_od",           "Off-design turbine outlet temperature",                  "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_P_out_od",           "Off-design turbine outlet pressure",                     "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_W_dot_od",           "Off-design turbine power",                               "MWe",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_m_dot_od",           "Off-design turbine mass flow rate",                      "kg/s",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_nu_od",              "Off-design turbine velocity ratio",	                     "-",	       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_N_od",               "Off-design turbine shaft speed",	                     "rpm",	       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_tip_ratio_od",       "Off-design turbine tip speed ratio",                     "-",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_eta_od",             "Off-design turbine efficiency",                          "-",          "",    "",      "",     "",       "" },
		// Recuperators
	{ SSC_OUTPUT, SSC_ARRAY,   "LTR_HP_T_out_od",      "Off-design low temp recup HP outlet temperature",        "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "eff_LTR_od",           "Off-design low temp recup effectiveness",                "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "q_dot_LTR_od",         "Off-design low temp recup heat transfer",                "MWt",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "LTR_LP_deltaP_od",     "Off-design low temp recup low pressure side pressure drop","-",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "LTR_HP_deltaP_od",     "Off-design low temp recup high pressure side pressure drop","-",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "LTR_min_dT_od",        "Off-design low temp recup minimum temperature difference","C",         "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "HTR_LP_T_out_od",      "Off-design high temp recup LP outlet temperature",       "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "HTR_HP_T_in_od",       "Off-design high temp recup HP inlet temperature",        "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "eff_HTR_od",           "Off-design high temp recup effectiveness",               "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "q_dot_HTR_od",         "Off-design high temp recup heat transfer",               "MWt",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "HTR_LP_deltaP_od",     "Off-design high temp recup low pressure side pressure drop","-",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "HTR_HP_deltaP_od",     "Off-design high temp recup high pressure side pressure drop","-",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "HTR_min_dT_od",        "Off-design high temp recup minimum temperature difference","C",         "",    "",      "",     "",       "" },
		// PHX 
	{ SSC_OUTPUT, SSC_ARRAY,   "T_co2_PHX_in_od",      "Off-design PHX co2 inlet temperature",                   "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "P_co2_PHX_in_od",      "Off-design PHX co2 inlet pressure",                      "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_co2_PHX_out_od",     "Off-design PHX co2 outlet temperature",                  "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "deltaT_HTF_PHX_od",    "Off-design HTF temp difference across PHX",              "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "phx_eff_od",           "Off-design PHX effectiveness",                           "-",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "phx_co2_deltaP_od",    "Off-design PHX co2 side pressure drop",                  "-",          "",    "",      "",     "",       "" },
		// Low Pressure Cooler
	{ SSC_OUTPUT, SSC_ARRAY,   "LP_cooler_T_in_od",    "Low pressure cooler inlet temperature",                  "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "LP_cooler_rho_in_od",  "Low pressure cooler inlet density",                      "kg/m3",      "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "LP_cooler_in_isen_deltah_to_P_mc_out_od",  "Low pressure cooler inlet isen enthalpy rise to mc outlet pressure", "kJ/kg", "", "", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "LP_cooler_co2_deltaP_od", "Off-design low pressure cooler co2 side pressure drop","-",         "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "LP_cooler_W_dot_fan_od","Low pressure cooler fan power",                         "MWe",        "",    "",      "",     "",       "" },
		// Intermediate Pressure Cooler
	{ SSC_OUTPUT, SSC_ARRAY,   "IP_cooler_W_dot_fan_od","Intermediate pressure cooler fan power",                "MWe",        "",    "",      "",     "",       "" },
		// Cooler Totals
	{ SSC_OUTPUT, SSC_ARRAY,   "cooler_tot_W_dot_fan_od","Intermediate pressure cooler fan power",               "MWe",        "",    "",      "",     "",       "" },
		// Solver Metrics
	{ SSC_OUTPUT, SSC_ARRAY,   "od_code",              "Diagnostic info",                                        "-",          ""     "",      "",     "",       "" },

	var_info_invalid };

int test_mono_function(double x, double *y);

class cm_sco2_csp_system : public compute_module
{
public:

	// Off-design parameters
	ssc_number_t *p_m_dot_htf_fracs;
	ssc_number_t *p_T_amb_od;
	ssc_number_t *p_T_htf_hot_od;
	// Optimized control parameters
	ssc_number_t *p_od_opt_obj_code;
	ssc_number_t *p_od_opt_conv_tol;
	ssc_number_t *p_P_comp_in_od;
	ssc_number_t *pm_mc_phi_od;
	
	ssc_number_t *p_recomp_frac_od;
	// Optimizer parameters
	ssc_number_t *p_sim_time_od;
	// Systems
	ssc_number_t *p_eta_thermal_od;
	ssc_number_t *p_T_mc_in_od;
	ssc_number_t *p_P_mc_out_od;
	ssc_number_t *p_T_htf_cold_od;
	ssc_number_t *p_m_dot_co2_full_od;
	ssc_number_t *p_W_dot_net_od;
	ssc_number_t *p_Q_dot_od;
	// Compressor
	ssc_number_t *p_mc_T_out_od;
	ssc_number_t *p_mc_W_dot_od;
	ssc_number_t *p_mc_m_dot_od;
	ssc_number_t *p_mc_rho_in_od;
    ssc_number_t *pm_mc_psi_od;
	ssc_number_t *p_mc_ideal_spec_work_od;
	ssc_number_t *p_mc_N_od;
	ssc_number_t *p_mc_eta_od;
	ssc_number_t *pm_mc_tip_ratio_od;
	ssc_number_t *pm_mc_eta_stages_od;
	ssc_number_t *p_mc_f_bypass_od;
	// Recompressor
	ssc_number_t *p_rc_T_in_od;
	ssc_number_t *p_rc_P_in_od;
	ssc_number_t *p_rc_T_out_od;
	ssc_number_t *p_rc_P_out_od;
	ssc_number_t *p_rc_W_dot_od;
	ssc_number_t *p_rc_m_dot_od;
	ssc_number_t *p_rc_eta_od;
	ssc_number_t *pm_rc_phi_od;
    ssc_number_t *pm_rc_psi_od;
	ssc_number_t *p_rc_N_od;
	ssc_number_t *pm_rc_tip_ratio_od;
	ssc_number_t *pm_rc_eta_stages_od;
	// Precompressor
	ssc_number_t *p_pc_T_in_od;
	ssc_number_t *p_pc_P_in_od;
	ssc_number_t *p_pc_W_dot_od;
	ssc_number_t *p_pc_m_dot_od;
	ssc_number_t *p_pc_eta_od;
	ssc_number_t *pm_pc_phi_od;
	ssc_number_t *p_pc_N_od;
	ssc_number_t *pm_pc_tip_ratio_od;
	ssc_number_t *pm_pc_eta_stages_od;
	ssc_number_t *p_pc_f_bypass_od;
	// Compressor Totals
	ssc_number_t *p_c_tot_W_dot_od;
	// Turbine
	ssc_number_t *p_t_P_in_od;
	ssc_number_t *p_t_T_out_od;
	ssc_number_t *p_t_P_out_od;
	ssc_number_t *p_t_W_dot_od;
	ssc_number_t *p_t_m_dot_od;
	ssc_number_t *p_t_nu_od;
	ssc_number_t *p_t_N_od;
	ssc_number_t *p_t_tip_ratio_od;
	ssc_number_t *p_t_eta_od;
	// Recuperator
	ssc_number_t *p_LTR_HP_T_out_od;
	ssc_number_t *p_eff_LTR_od;
	ssc_number_t *p_q_dot_LTR_od;
	ssc_number_t *p_LTR_LP_deltaP_od;
	ssc_number_t *p_LTR_HP_deltaP_od;
	ssc_number_t *p_LTR_min_dT_od;
	ssc_number_t *p_HTR_LP_T_out_od;
	ssc_number_t *p_HTR_HP_T_in_od;
	ssc_number_t *p_eff_HTR_od;
	ssc_number_t *p_q_dot_HTR_od;
	ssc_number_t *p_HTR_LP_deltaP_od;
	ssc_number_t *p_HTR_HP_deltaP_od;
	ssc_number_t *p_HTR_min_dT_od;
	// PHX
	ssc_number_t *p_T_co2_PHX_in_od;
	ssc_number_t *p_P_co2_PHX_in_od;
	ssc_number_t *p_T_co2_PHX_out_od;
	ssc_number_t *p_deltaT_HTF_PHX_od;
	ssc_number_t *p_phx_eff_od;
	ssc_number_t *p_phx_co2_deltaP_od;
	// Low Pressure Cooler
	ssc_number_t *p_LP_cooler_T_in_od;
	ssc_number_t *p_LP_cooler_rho_in_od;
	ssc_number_t *p_LP_cooler_in_isen_deltah_to_P_mc_out_od;
	ssc_number_t *p_LP_cooler_co2_deltaP_od;
	ssc_number_t *p_LP_cooler_W_dot_fan_od;
	// Intermediate Pressure Cooler
	ssc_number_t *p_IP_cooler_W_dot_fan_od;
	// Coolerl Totals
	ssc_number_t *p_cooler_tot_W_dot_fan_od;
	// Solver Metrics
	ssc_number_t *p_od_code;

	cm_sco2_csp_system()
	{
		add_var_info(vtab_sco2_design);
		
		add_var_info(_cm_vtab_sco2_csp_system);
	}

	void exec() throw(general_error)
	{
		C_sco2_phx_air_cooler c_sco2_cycle;

		int sco2_des_err = sco2_design_cmod_common(this, c_sco2_cycle);
		if (sco2_des_err != 0)
			return;

		double m_dot_htf_design = c_sco2_cycle.get_phx_des_par()->m_m_dot_hot_des;	//[kg/s]

		bool is_rc = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_is_rc;

		int n_mc_stages = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_n_stages;
		int n_rc_stages = 1;
		if(is_rc)
			n_rc_stages = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_n_stages;		//[-]
		
		int cycle_config = c_sco2_cycle.get_design_par()->m_cycle_config;
		int n_pc_stages = 1;
		if(cycle_config == 2)
			n_pc_stages = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_n_stages;		//[-]

			   		 
		// Check if 'od_cases' is assigned
		bool is_od_cases_assigned = is_assigned("od_cases");
		bool is_P_mc_in_od_sweep_assigned = is_assigned("od_P_mc_in_sweep");
		bool is_od_set_control = is_assigned("od_set_control");
		if (is_od_cases_assigned && is_P_mc_in_od_sweep_assigned)
		{
			log("Both off design cases and main compressor inlet sweep assigned. Only modeling off design cases");
			is_P_mc_in_od_sweep_assigned = false;
		}
		if (is_od_cases_assigned && is_od_set_control)
		{
			log("Both off design cases and od set control assigned. Only modeling off design cases");
			is_od_set_control = false;
		}
		if (is_P_mc_in_od_sweep_assigned && is_od_set_control)
		{
			log("Both main compressor inlet sweep and od set control assigned. Only modeling off design cases");
			is_od_set_control = false;
		}
		if (!is_od_cases_assigned && !is_P_mc_in_od_sweep_assigned && !is_od_set_control)
		{
			log("No off-design cases or main compressor inlet sweep specified");
			return;
		}
		
		// Set up off-design analysis
		double P_LP_comp_in_des = std::numeric_limits<double>::quiet_NaN();		//[MPa]
		double delta_P = std::numeric_limits<double>::quiet_NaN();

		if (cycle_config == 1)
		{
			P_LP_comp_in_des = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::MC_IN] / 1000.0;		//[MPa] convert from kPa
			delta_P = 10.0;
		}
		else
		{
			P_LP_comp_in_des = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::PC_IN] / 1000.0;		//[MPa] convert from kPa
			delta_P = 6.0;
		}

        // Get turbine inlet mode
        int T_t_in_mode = as_integer("od_T_t_in_mode");

		util::matrix_t<double> od_cases;
		if (is_od_cases_assigned)
		{
			od_cases = as_matrix("od_cases");

			// Check if off cases exist and correctly formatted
			int n_od_cols_loc = (int)od_cases.ncols();
			int n_od_runs_loc = (int)od_cases.nrows();

			if (n_od_cols_loc != 5 && n_od_runs_loc == 1)
			{
				// No off-design cases specified
				log("No off-design cases specified");
				return;
			}
			if (n_od_cols_loc != 5 && n_od_runs_loc > 1)
			{
				std::string err_msg = util::format("The matrix of off design cases requires 5 columns. The entered matrix has %d columns", n_od_cols_loc);
				throw exec_error("sco2_csp_system", err_msg);
			}
		}
		else if(is_P_mc_in_od_sweep_assigned)
		{
			std::vector<double> od_case = as_vector_double("od_P_mc_in_sweep");
			size_t n_od = od_case.size();
			if (n_od != 5)
			{
				std::string err_msg = util::format("The matrix of off design cases requires 5 columns. The entered matrix has %d columns", n_od);
				throw exec_error("sco2_csp_system", err_msg);
			}
			
            int n_P_mc_in = 101;

			double P_mc_in_low = P_LP_comp_in_des - delta_P / 2.0;	//[MPa]

			double delta_P_i = delta_P / (n_P_mc_in - 1);	//[MPa]

			od_cases.resize(n_P_mc_in, 6);
			for (int i = 0; i < n_P_mc_in; i++)
			{
				od_cases(i, 0) = od_case[0];
				od_cases(i, 1) = od_case[1];
				od_cases(i, 2) = od_case[2];
				od_cases(i, 3) = od_case[3];
                od_cases(i, 4) = od_case[4];
				od_cases(i, 5) = P_mc_in_low + delta_P_i * i;	//[MPa]
			}
		}
		else
		{
			od_cases = as_matrix("od_set_control");

			// Check if off cases exist and correctly formatted
			int n_od_cols_loc = (int)od_cases.ncols();
			int n_od_runs_loc = (int)od_cases.nrows();

			if (n_od_cols_loc != 4 && n_od_runs_loc == 1)
			{
				// No off-design cases specified
				log("No off-design cases specified");
				return;
			}
			if (n_od_cols_loc != 4 && n_od_runs_loc > 1)
			{
				std::string err_msg = util::format("The matrix of od set control requires 4 columns. The entered matrix has %d columns", n_od_cols_loc);
				throw exec_error("sco2_csp_system", err_msg);
			}
		}
		
		int n_od_runs = (int)od_cases.nrows();
		allocate_ssc_outputs(n_od_runs, n_mc_stages, n_rc_stages, n_pc_stages);
		C_sco2_phx_air_cooler::S_od_par sco2_rc_od_par;

		// For try/catch below
		int out_type = -1;
		std::string out_msg = "";

		for(int n_run = 0; n_run < n_od_runs; n_run++)
		{			
			// Try calling off-design model with design parameters
				// Set outputs
			p_T_htf_hot_od[n_run] = (ssc_number_t)od_cases(n_run, 0);			//[C]
			p_m_dot_htf_fracs[n_run] = (ssc_number_t)od_cases(n_run, 1);		//[-]
			p_T_amb_od[n_run] = (ssc_number_t)od_cases(n_run, 2);				//[C]
			//p_od_opt_obj_code[n_run] = (ssc_number_t)od_cases(n_run, 3);		//[-]
			//p_od_opt_conv_tol[n_run] = (ssc_number_t)od_cases(n_run, 4);		//[-]
				// Set input structure
			sco2_rc_od_par.m_T_htf_hot = p_T_htf_hot_od[n_run] + 273.15;	//[K]
			sco2_rc_od_par.m_m_dot_htf = m_dot_htf_design*p_m_dot_htf_fracs[n_run];	//[kg/s]
			sco2_rc_od_par.m_T_amb = p_T_amb_od[n_run] + 273.15;				//[K]
			//int od_strategy = (int)p_od_opt_obj_code[n_run];		//[-]
			//double od_opt_tol = p_od_opt_conv_tol[n_run];			//[-]

            sco2_rc_od_par.m_T_t_in_mode = T_t_in_mode;  //[-]

			int off_design_code = 0;
			std::clock_t clock_start = std::clock();
			try
			{
				if (is_od_cases_assigned)
				{
					// 2D optimization
					//off_design_code = sco2_recomp_csp.off_design_opt(sco2_rc_od_par, od_strategy);
						// Nested optimization
					int od_strategy = C_sco2_phx_air_cooler::E_TARGET_POWER_ETA_MAX;
					off_design_code = c_sco2_cycle.optimize_off_design(sco2_rc_od_par, od_strategy);
				}
				else if (is_P_mc_in_od_sweep_assigned)
				{
					int od_strategy = C_sco2_phx_air_cooler::E_TARGET_POWER_ETA_MAX;
                        // RC shaft speed control
                    bool is_rc_N_od_at_design = true;
                    double rc_N_od_f_des = 1.0;
                    if (od_cases(n_run, 3) < 0.0)
                    {
                        is_rc_N_od_at_design = false;
                        rc_N_od_f_des = fabs(od_cases(n_run, 3));
                    }
                        // MC shaft speed control
                    bool is_mc_N_od_at_design = true;
                    double mc_N_od_f_des = 1.0;
                    if (od_cases(n_run, 4) < 0.0)
                    {
                        is_mc_N_od_at_design = false;
                        mc_N_od_f_des = fabs(od_cases(n_run, 4));
                    }

					off_design_code = c_sco2_cycle.off_design_fix_P_mc_in(sco2_rc_od_par, od_cases(n_run, 5), 
                                                                            is_rc_N_od_at_design, rc_N_od_f_des,
                                                                            is_mc_N_od_at_design, mc_N_od_f_des,
                                                                            od_strategy);
				}
				else if (is_od_set_control)
				{
					int od_strategy = C_sco2_phx_air_cooler::E_TARGET_POWER_ETA_MAX;
					double P_LP_comp_in = od_cases(n_run, 3);
					if (od_cases(n_run, 3) < 0.0)
					{
						P_LP_comp_in = P_LP_comp_in_des / fabs(P_LP_comp_in);
					}
					off_design_code = c_sco2_cycle.off_design_fix_P_mc_in(sco2_rc_od_par, P_LP_comp_in, true, 1.0, true, 1.0, od_strategy);
				}
			}
			catch( C_csp_exception &csp_exception )
			{
				// Report warning before exiting with error
				while (c_sco2_cycle.mc_messages.get_message(&out_type, &out_msg))
				{
					log(out_msg);
				}

				log(csp_exception.m_error_message, SSC_ERROR, -1.0);
				throw exec_error("sco2_csp_system", csp_exception.m_error_message);
			}

			std::clock_t clock_end = std::clock();

			double od_opt_duration = (clock_end - clock_start)/(double) CLOCKS_PER_SEC;		//[s]

			p_od_code[n_run] = (ssc_number_t)off_design_code;
			if(off_design_code == 0) // || ((is_P_mc_in_od_sweep_assigned || is_od_set_control) && c_sco2_cycle.get_od_solved()->m_is_converged))
			{	// Off-design call was successful, so write outputs
					// Control parameters
				p_P_comp_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN] / 1000.0);	//[MPa]
				for (int i_s = 0; i_s < n_mc_stages; i_s++)
					pm_mc_phi_od[n_run*n_mc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.mv_phi[i_s];	//[-]
				p_recomp_frac_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_recomp_frac;		//[-]
					// Optimizer parameters
				p_sim_time_od[n_run] = (ssc_number_t)od_opt_duration;		//[s]
					// System
				p_eta_thermal_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_eta_thermal;		//[-]
				p_T_mc_in_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_IN] - 273.15;	//[C]
				p_P_mc_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_OUT] / 1.E3);	//[MPa]
				p_T_htf_cold_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_phx_od_solved.m_T_h_out - 273.15);		//[C]
				p_m_dot_co2_full_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_t;		//[kg/s]
				p_W_dot_net_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_W_dot_net / 1.E3);	//[MWe]
				p_Q_dot_od[n_run] = p_W_dot_net_od[n_run] / p_eta_thermal_od[n_run];		//[MWt]
					// Compressor
				p_mc_T_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_OUT] - 273.15);	//[C]
				p_mc_W_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.m_W_dot_in*1.E-3);	//[MWe] convert from kWe
				double comp_W_dot_od_sum = p_mc_W_dot_od[n_run];	//[MWe]
				p_mc_m_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_mc);			//[kg/s]
				p_mc_rho_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_dens[C_sco2_cycle_core::MC_IN]);	//[kg/m3]
				p_mc_ideal_spec_work_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.m_isen_spec_work);	//[kJ/kg]
				p_mc_N_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.m_N;		//[rpm]
				p_mc_eta_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.m_eta;	//[-]
				for (int i_s = 0; i_s < n_mc_stages; i_s++)
				{
                    pm_mc_psi_od[n_run*n_mc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.mv_psi[i_s];   //[-]
                    pm_mc_tip_ratio_od[n_run*n_mc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.mv_tip_speed_ratio[i_s];	//[-]
					pm_mc_eta_stages_od[n_run*n_mc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.mv_eta[i_s];	//[-]
				}
				p_mc_f_bypass_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_mc_f_bypass;		//[-]
					// Recompressor
				if (is_rc)
				{
					p_rc_T_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_T_in - 273.15);	//[C]
					p_rc_P_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_P_in*1.E-3);		//[MPa] convert from kPa
					p_rc_T_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_T_out - 273.15);	//[C] convert from K
					p_rc_P_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_P_out*1.E-3);	//[MPa] convert from kPa
					p_rc_W_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_W_dot_in*1.E-3);	//[MWe] convert from kWe
					p_rc_m_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_rc);				//[kg/s]
					p_rc_eta_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_eta;		//[-]
                    for (int i_s = 0; i_s < n_rc_stages; i_s++)
                    {
                        pm_rc_phi_od[n_run*n_rc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.mv_phi[i_s];	//[-]
                        pm_rc_psi_od[n_run*n_rc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.mv_psi[i_s];	//[-]
                    }
					p_rc_N_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_N;			//[rpm]
					for (int i_s = 0; i_s < n_rc_stages; i_s++)
					{
						pm_rc_tip_ratio_od[n_run*n_rc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.mv_tip_speed_ratio[i_s];	//[-]
						pm_rc_eta_stages_od[n_run*n_rc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.mv_eta[i_s];	//[-]
					}
					comp_W_dot_od_sum += p_rc_W_dot_od[n_run];		//[MWe]
				}
				else
				{
					p_rc_T_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_rc_P_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_rc_T_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_rc_P_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_rc_W_dot_od[n_run] = 0.0;
					p_rc_m_dot_od[n_run] = 0.0;
					p_rc_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    for (int i_s = 0; i_s < n_rc_stages; i_s++)
                    {
                        pm_rc_phi_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                        pm_rc_psi_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    }
					p_rc_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();		
					for (int i_s = 0; i_s < n_rc_stages; i_s++)
					{
						pm_rc_tip_ratio_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
						pm_rc_eta_stages_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					}
				}
					// Precompressor
				if (cycle_config == 2)
				{
					p_pc_T_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_T_in - 273.15);		//[C]
					p_pc_P_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_P_in*1.E-3);		//[MPa]
					p_pc_W_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_W_dot_in*1.E-3);	//[MWe] convert from kWe
					p_pc_m_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_pc);		//[kg/s]
					p_pc_eta_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_eta;		//[-]
					for (int i_s = 0; i_s < n_pc_stages; i_s++)
						pm_pc_phi_od[n_run*n_pc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.mv_phi[i_s];
					p_pc_N_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_N;			//[rpm]
					for (int i_s = 0; i_s < n_pc_stages; i_s++)
					{
						pm_pc_tip_ratio_od[n_run*n_pc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.mv_tip_speed_ratio[i_s];	//[-]
						pm_pc_eta_stages_od[n_run*n_pc_stages + i_s] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.mv_eta[i_s];				//[-]
					}
					p_pc_f_bypass_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pc_f_bypass;
					comp_W_dot_od_sum += p_pc_W_dot_od[n_run];	//[MWe]
				}
				else
				{
					p_pc_T_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_pc_P_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_pc_W_dot_od[n_run] = 0.0;
					p_pc_m_dot_od[n_run] = 0.0;
					p_pc_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					for (int i_s = 0; i_s < n_pc_stages; i_s++)
						pm_pc_phi_od[n_run*n_pc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_pc_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					for (int i_s = 0; i_s < n_pc_stages; i_s++)
					{
						pm_pc_tip_ratio_od[n_run*n_pc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
						pm_pc_eta_stages_od[n_run*n_pc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					}
					p_pc_f_bypass_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();	//[-]
				}
					// Compressor Totals
				p_c_tot_W_dot_od[n_run] = comp_W_dot_od_sum;		//[MWe]
					// Turbine
				p_t_P_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::TURB_IN]*1.E-3);	//[MPa] convert from kPa
				p_t_T_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::TURB_OUT] - 273.15);	//[C] Convert from K
				p_t_P_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::TURB_OUT]*1.E-3);//[MPa] convert form kPa
				p_t_W_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_W_dot_out*1.E-3);	//[MWe] convert from kWe
				p_t_m_dot_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_t);			//[kg/s]
				p_t_nu_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_nu;		//[-]
				p_t_N_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_N;		//[rpm]
				p_t_tip_ratio_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_w_tip_ratio;	//[-]
				p_t_eta_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_eta;	//[-]
					// Recuperator
				p_LTR_HP_T_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::LTR_HP_OUT] - 273.15);	//[C] LTR HP outlet temp, convert from K
				p_eff_LTR_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_LT_recup_od_solved.m_eff;	//[-]
				p_q_dot_LTR_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_LT_recup_od_solved.m_q_dot*1.E-3);	//[MWt] convert from kWt
				double LTR_LP_deltaP_od = 1.0 - c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::LTR_LP_OUT] / 
											c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::HTR_LP_OUT];
				p_LTR_LP_deltaP_od[n_run] = (ssc_number_t)LTR_LP_deltaP_od;	//[-]
				double LTR_HP_deltaP_od = 1.0 - c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::LTR_HP_OUT] /
											c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_OUT];
				p_LTR_HP_deltaP_od[n_run] = (ssc_number_t)LTR_HP_deltaP_od;	//[-]
				p_LTR_min_dT_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_LT_recup_od_solved.m_min_DT);	//[C]
				
				p_HTR_LP_T_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::HTR_LP_OUT] - 273.15);	//[C] HTR LP outlet temp, convert from K
				p_HTR_HP_T_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MIXER_OUT] - 273.15);	//[C] HTR HP inlet temp, convert from K
				p_eff_HTR_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_HT_recup_od_solved.m_eff;	//[-]
				p_q_dot_HTR_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_HT_recup_od_solved.m_q_dot*1.E-3);	//[MWt] convert from kWt
				double HTR_LP_deltaP_od = 1.0 - c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::HTR_LP_OUT] /
											c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::TURB_OUT];
				p_HTR_LP_deltaP_od[n_run] = (ssc_number_t)HTR_LP_deltaP_od;
				double HTR_HP_deltaP_od = 1.0 - c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::HTR_HP_OUT] /
					c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::LTR_HP_OUT];
				p_HTR_HP_deltaP_od[n_run] = (ssc_number_t)HTR_HP_deltaP_od;
				p_HTR_min_dT_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_HT_recup_od_solved.m_min_DT);	//[C]
					// PHX
				p_T_co2_PHX_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::HTR_HP_OUT] - 273.15);	//[C]
				p_P_co2_PHX_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::HTR_HP_OUT] * 1.E-3);  //[MPa] convert from kPa
				p_T_co2_PHX_out_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::TURB_IN] - 273.15);		//[C]
				p_deltaT_HTF_PHX_od[n_run] = p_T_htf_hot_od[n_run] - p_T_htf_cold_od[n_run];	//[C]
				p_phx_eff_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_phx_od_solved.m_eff;		//[-]
				double phx_co2_deltaP_od = 1.0 - c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::TURB_IN] /
					c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::HTR_HP_OUT];
				p_phx_co2_deltaP_od[n_run] = (ssc_number_t)phx_co2_deltaP_od;
					// Low Pressure Cooler
				p_LP_cooler_T_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::LTR_LP_OUT] - 273.15);	//[C] Convert from K
				p_LP_cooler_rho_in_od[n_run] = (ssc_number_t)(c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_dens[C_sco2_cycle_core::LTR_LP_OUT]);	//[kg/m3]
				double LP_cooler_co2_deltaP_od = 1.0 - c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN] /
					c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::LTR_LP_OUT];
				p_LP_cooler_co2_deltaP_od[n_run] = (ssc_number_t)LP_cooler_co2_deltaP_od;	//[-]
				p_LP_cooler_W_dot_fan_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_LP_air_cooler_od_solved.m_W_dot_fan;	//[MWe]
				double cooler_W_dot_total = c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_LP_air_cooler_od_solved.m_W_dot_fan;	//[MWe]
					// Intermediate Pressure Cooler
				if (cycle_config == 2)
				{
					p_IP_cooler_W_dot_fan_od[n_run] = (ssc_number_t)c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.ms_IP_air_cooler_od_solved.m_W_dot_fan;	//[MWe]
					cooler_W_dot_total += p_IP_cooler_W_dot_fan_od[n_run];	//[MWe]
				}
				else
				{
					p_IP_cooler_W_dot_fan_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				}
				p_cooler_tot_W_dot_fan_od[n_run] = cooler_W_dot_total;	//[MWe]


				double T_cooler_in_od = c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::LTR_LP_OUT];	//[K]
				double P_cooler_in_od = c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::LTR_LP_OUT];	//[K]
				double P_cooler_out_od = c_sco2_cycle.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_OUT];		//[MPa]
				int isen_enth_check_err = 0;
				double h_cooler_in_od = std::numeric_limits<double>::quiet_NaN();
				double s_cooler_in_od = std::numeric_limits<double>::quiet_NaN();
				double rho_cooler_in_od = std::numeric_limits<double>::quiet_NaN();
				double T_isen_out_od = std::numeric_limits<double>::quiet_NaN();
				double h_isen_out_od = std::numeric_limits<double>::quiet_NaN();
				double s_isen_out_od = std::numeric_limits<double>::quiet_NaN();
				double rho_isen_out_od = std::numeric_limits<double>::quiet_NaN();
				double deltah_isen_od = std::numeric_limits<double>::quiet_NaN();

				calculate_turbomachinery_outlet_1(T_cooler_in_od, P_cooler_in_od, P_cooler_out_od, 1.0, true, isen_enth_check_err,
					h_cooler_in_od, s_cooler_in_od, rho_cooler_in_od, T_isen_out_od,
					h_isen_out_od, s_isen_out_od, rho_isen_out_od, deltah_isen_od);

				p_LP_cooler_in_isen_deltah_to_P_mc_out_od[n_run] = (ssc_number_t)-deltah_isen_od;		//[kJ/kg]
			}
			else
			{	// Off-design call failed, write NaN outptus
					// Control parameters
				p_P_comp_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				for(int i_s = 0; i_s < n_mc_stages; i_s++)
					pm_mc_phi_od[n_run*n_mc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_recomp_frac_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// System
				p_eta_thermal_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_T_mc_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_P_mc_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_T_htf_cold_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_m_dot_co2_full_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_W_dot_net_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_Q_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Compressor
				p_mc_T_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_W_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_m_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_rho_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_ideal_spec_work_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				for (int i_s = 0; i_s < n_mc_stages; i_s++)
				{
					pm_mc_phi_od[n_run*n_mc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    pm_mc_tip_ratio_od[n_run*n_mc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					pm_mc_eta_stages_od[n_run*n_mc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				}
				p_mc_f_bypass_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Recompressor
				p_rc_T_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_P_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_T_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_P_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_W_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_m_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                for (int i_s = 0; i_s < n_rc_stages; i_s++)
                {
                    pm_rc_phi_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                    pm_rc_psi_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                }
				p_rc_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				for (int i_s = 0; i_s < n_rc_stages; i_s++)
				{
					pm_rc_tip_ratio_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					pm_rc_eta_stages_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				}
					// Precompressor
				p_pc_T_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_pc_P_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_pc_W_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_pc_m_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_pc_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				for (int i_s = 0; i_s < n_pc_stages; i_s++)
					pm_pc_phi_od[n_run*n_pc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_pc_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				for (int i_s = 0; i_s < n_pc_stages; i_s++)
				{
					pm_pc_tip_ratio_od[n_run*n_pc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					pm_pc_eta_stages_od[n_run*n_pc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				}
				p_pc_f_bypass_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// Compressor Totals
				p_c_tot_W_dot_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// Turbine
				p_t_P_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_T_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_P_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_W_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_m_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_nu_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_tip_ratio_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Recuperator
				p_LTR_HP_T_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_eff_LTR_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_q_dot_LTR_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_LTR_LP_deltaP_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_LTR_HP_deltaP_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_LTR_min_dT_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();

				p_HTR_LP_T_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_HTR_HP_T_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_eff_HTR_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_q_dot_HTR_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_HTR_LP_deltaP_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_HTR_HP_deltaP_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_HTR_min_dT_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// PHX
				p_T_co2_PHX_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_P_co2_PHX_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_T_co2_PHX_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_deltaT_HTF_PHX_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_phx_eff_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_phx_co2_deltaP_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Low Pressure Cooler
				p_LP_cooler_T_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_LP_cooler_rho_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_LP_cooler_co2_deltaP_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_LP_cooler_W_dot_fan_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Intermediate Pressure Cooler
				p_IP_cooler_W_dot_fan_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Coolerl Totals
				p_cooler_tot_W_dot_fan_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();

				p_LP_cooler_in_isen_deltah_to_P_mc_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
			}


		}


		// If all calls were successful, log to SSC any messages from sco2_recomp_csp
		while (c_sco2_cycle.mc_messages.get_message(&out_type, &out_msg))
		{
			log(out_msg + "\n");
		}
		
	}

	void allocate_ssc_outputs(int n_od_runs, int n_mc_stages, int n_rc_stages, int n_pc_stages)
	{
		// Off-design parameters
		p_m_dot_htf_fracs = allocate("m_dot_htf_fracs", n_od_runs);
		p_T_amb_od = allocate("T_amb_od", n_od_runs);
		p_T_htf_hot_od = allocate("T_htf_hot_od", n_od_runs);
		// Optimized control parameters
		p_od_opt_obj_code = allocate("od_opt_obj_code", n_od_runs);
		p_od_opt_conv_tol = allocate("od_opt_conv_tol", n_od_runs);
		p_P_comp_in_od = allocate("P_comp_in_od", n_od_runs);
		pm_mc_phi_od = allocate("mc_phi_od", n_od_runs, n_mc_stages);
		
		p_recomp_frac_od = allocate("recomp_frac_od", n_od_runs);
		// Optimizer parameters
		p_sim_time_od = allocate("sim_time_od", n_od_runs);
		// Systems
		p_eta_thermal_od = allocate("eta_thermal_od", n_od_runs);
		p_T_mc_in_od = allocate("T_mc_in_od", n_od_runs);
		p_P_mc_out_od = allocate("P_mc_out_od", n_od_runs);
		p_T_htf_cold_od = allocate("T_htf_cold_od", n_od_runs);
		p_m_dot_co2_full_od = allocate("m_dot_co2_full_od", n_od_runs);
		p_W_dot_net_od = allocate("W_dot_net_od", n_od_runs);
		p_Q_dot_od = allocate("Q_dot_od", n_od_runs);
		// Compressor
		p_mc_T_out_od = allocate("mc_T_out_od", n_od_runs);
		p_mc_W_dot_od = allocate("mc_W_dot_od", n_od_runs);
		p_mc_m_dot_od = allocate("mc_m_dot_od", n_od_runs);
		p_mc_rho_in_od = allocate("mc_rho_in_od", n_od_runs);
        pm_mc_psi_od = allocate("mc_psi_od", n_od_runs, n_mc_stages);
		p_mc_ideal_spec_work_od = allocate("mc_ideal_spec_work_od", n_od_runs);
		p_mc_N_od = allocate("mc_N_od", n_od_runs);
		p_mc_eta_od = allocate("mc_eta_od", n_od_runs);
		pm_mc_tip_ratio_od = allocate("mc_tip_ratio_od", n_od_runs, n_mc_stages);
		pm_mc_eta_stages_od = allocate("mc_eta_stages_od", n_od_runs, n_mc_stages);
		p_mc_f_bypass_od = allocate("mc_f_bypass_od", n_od_runs);
		// Recompressor
		p_rc_T_in_od = allocate("rc_T_in_od", n_od_runs);
		p_rc_P_in_od = allocate("rc_P_in_od", n_od_runs);
		p_rc_T_out_od = allocate("rc_T_out_od", n_od_runs);
		p_rc_P_out_od = allocate("rc_P_out_od", n_od_runs);
		p_rc_W_dot_od = allocate("rc_W_dot_od", n_od_runs);
		p_rc_m_dot_od = allocate("rc_m_dot_od", n_od_runs);
		p_rc_eta_od = allocate("rc_eta_od", n_od_runs);
		pm_rc_phi_od = allocate("rc_phi_od", n_od_runs, n_rc_stages);
        pm_rc_psi_od = allocate("rc_psi_od", n_od_runs, n_rc_stages);
		p_rc_N_od = allocate("rc_N_od", n_od_runs);
		pm_rc_tip_ratio_od = allocate("rc_tip_ratio_od", n_od_runs, n_rc_stages);
		pm_rc_eta_stages_od = allocate("rc_eta_stages_od", n_od_runs, n_rc_stages);
		// Precompressor
		p_pc_T_in_od = allocate("pc_T_in_od", n_od_runs);
		p_pc_P_in_od = allocate("pc_P_in_od", n_od_runs);
		p_pc_W_dot_od = allocate("pc_W_dot_od", n_od_runs);
		p_pc_m_dot_od = allocate("pc_m_dot_od", n_od_runs);
		p_pc_eta_od = allocate("pc_eta_od", n_od_runs);
		pm_pc_phi_od = allocate("pc_phi_od", n_od_runs, n_pc_stages);
		p_pc_N_od = allocate("pc_N_od", n_od_runs);
		pm_pc_tip_ratio_od = allocate("pc_tip_ratio_od", n_od_runs, n_pc_stages);
		pm_pc_eta_stages_od = allocate("pc_eta_stages_od", n_od_runs, n_pc_stages);
		p_pc_f_bypass_od = allocate("pc_f_bypass_od", n_od_runs);
		// Compressor Totals
		p_c_tot_W_dot_od = allocate("c_tot_W_dot_od", n_od_runs);
		// Turbine
		p_t_P_in_od = allocate("t_P_in_od", n_od_runs);
		p_t_T_out_od = allocate("t_T_out_od", n_od_runs);
		p_t_P_out_od = allocate("t_P_out_od", n_od_runs);
		p_t_W_dot_od = allocate("t_W_dot_od", n_od_runs);
		p_t_m_dot_od = allocate("t_m_dot_od", n_od_runs);
		p_t_nu_od = allocate("t_nu_od", n_od_runs);
		p_t_N_od = allocate("t_N_od", n_od_runs);
		p_t_tip_ratio_od = allocate("t_tip_ratio_od", n_od_runs);
		p_t_eta_od = allocate("t_eta_od", n_od_runs);
		// Recuperator
		p_LTR_HP_T_out_od = allocate("LTR_HP_T_out_od", n_od_runs);
		p_eff_LTR_od = allocate("eff_LTR_od", n_od_runs);
		p_q_dot_LTR_od = allocate("q_dot_LTR_od", n_od_runs);
		p_LTR_LP_deltaP_od = allocate("LTR_LP_deltaP_od", n_od_runs);
		p_LTR_HP_deltaP_od = allocate("LTR_HP_deltaP_od", n_od_runs);
		p_LTR_min_dT_od = allocate("LTR_min_dT_od", n_od_runs);

		p_HTR_LP_T_out_od = allocate("HTR_LP_T_out_od", n_od_runs);
		p_HTR_HP_T_in_od = allocate("HTR_HP_T_in_od", n_od_runs);
		p_eff_HTR_od = allocate("eff_HTR_od", n_od_runs);
		p_q_dot_HTR_od = allocate("q_dot_HTR_od", n_od_runs);
		p_HTR_LP_deltaP_od = allocate("HTR_LP_deltaP_od", n_od_runs);
		p_HTR_HP_deltaP_od = allocate("HTR_HP_deltaP_od", n_od_runs);
		p_HTR_min_dT_od = allocate("HTR_min_dT_od", n_od_runs);
		// PHX
		p_T_co2_PHX_in_od = allocate("T_co2_PHX_in_od", n_od_runs);
		p_P_co2_PHX_in_od = allocate("P_co2_PHX_in_od", n_od_runs);
		p_T_co2_PHX_out_od = allocate("T_co2_PHX_out_od", n_od_runs);
		p_deltaT_HTF_PHX_od = allocate("deltaT_HTF_PHX_od", n_od_runs);
		p_phx_eff_od = allocate("phx_eff_od", n_od_runs);
		p_phx_co2_deltaP_od = allocate("phx_co2_deltaP_od", n_od_runs);
		// Low Pressure Cooler
		p_LP_cooler_T_in_od = allocate("LP_cooler_T_in_od", n_od_runs);
		p_LP_cooler_rho_in_od = allocate("LP_cooler_rho_in_od", n_od_runs);
		p_LP_cooler_in_isen_deltah_to_P_mc_out_od = allocate("LP_cooler_in_isen_deltah_to_P_mc_out_od", n_od_runs);
		p_LP_cooler_co2_deltaP_od = allocate("LP_cooler_co2_deltaP_od", n_od_runs);
		p_LP_cooler_W_dot_fan_od = allocate("LP_cooler_W_dot_fan_od", n_od_runs);
		// Intermediate Pressure Cooler
		p_IP_cooler_W_dot_fan_od = allocate("IP_cooler_W_dot_fan_od", n_od_runs);
		// Coolerl Totals
		p_cooler_tot_W_dot_fan_od = allocate("cooler_tot_W_dot_fan_od", n_od_runs);
		// Solver Metrics
		p_od_code = allocate("od_code", n_od_runs);

		return;
	}

};

DEFINE_MODULE_ENTRY(sco2_csp_system, "...", 0)
