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

#include "core.h"
#include "common.h"

#include <ctime>

#include "sco2_pc_csp_int.h"

static var_info _cm_vtab_sco2_csp_system[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                                    UNITS     META  GROUP REQUIRED_IF CONSTRAINTS     UI_HINTS*/
	// ** Design Parameters **
		// System Design
	{ SSC_INPUT,  SSC_NUMBER,  "htf",                  "Integer code for HTF used in PHX",                       "",           "",    "",      "*",     "",       "" },
    { SSC_INPUT,  SSC_MATRIX,  "htf_props",            "User defined HTF property data",                         "", "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "", "?=[[0]]", "", "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_htf_hot_des",        "HTF design hot temperature (PHX inlet)",                 "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "dT_PHX_hot_approach",  "Temp diff btw hot HTF and turbine inlet",                "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_amb_des",            "Ambient temperature",                                    "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "dT_mc_approach",       "Temp diff btw ambient air and main compressor inlet",    "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "site_elevation",       "Site elevation",                                         "m",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "W_dot_net_des",        "Design cycle power output (no cooling parasitics)",      "MWe",        "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "design_method",        "1 = Specify efficiency, 2 = Specify total recup UA",     "",           "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_thermal_des",      "Power cycle thermal efficiency",                         "",           "",    "",      "?=-1.0","",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "UA_recup_tot_des",     "Total recuperator conductance",                          "kW/K",       "",    "",      "?=-1.0","",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "cycle_config",         "1 = recompression, 2 = partial cooling",                 "",           "",    "",      "?=1",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_recomp_ok",         "1 = Yes, 0 = simple cycle only",                         "",           "",    "",      "?=1",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_PR_fixed",          "0 = No, >0 = fixed pressure ratio",                      "",           "",    "",      "?=0",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "des_objective",        "[2] = hit min phx deltat then max eta, [else] max eta",  "",           "",    "",      "?=0",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "min_phx_deltaT",       "Minimum design temperature difference across PHX",       "C",          "",    "",      "?=0",   "",       "" },	
	{ SSC_INPUT,  SSC_NUMBER,  "rel_tol",              "Baseline solver and optimization relative tolerance exponent (10^-rel_tol)", "-", "", "", "?=3","",       "" },	
		// Cycle Design
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_mc",          "Design main compressor isentropic efficiency",           "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_rc",          "Design re-compressor isentropic efficiency",             "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_pc",          "Design precompressor isentropic efficiency",             "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_t",           "Design turbine isentropic efficiency",                   "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "LT_recup_eff_max",     "Maximum allowable effectiveness in LT recuperator",      "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "HT_recup_eff_max",     "Maximum allowable effectiveness in LT recuperator",      "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "P_high_limit",         "High pressure limit in cycle",                           "MPa",        "",    "",      "*",     "",       "" },
		// PHX Design
	{ SSC_INPUT,  SSC_NUMBER,  "dT_PHX_cold_approach", "Temp diff btw cold HTF and cold CO2",                    "C",          "",    "",      "*",     "",       "" },
		// Air Cooler Design
	{ SSC_INPUT,  SSC_NUMBER,  "fan_power_frac",       "Fraction of net cycle power consumed by air cooler fan", "",           "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "deltaP_cooler_frac",   "Fraction of cycle high pressure that is design point cooler CO2 pressure drop", "", "", "", "*","",       "" },
	// ** Off-design Inputs **
	{ SSC_INPUT,  SSC_MATRIX,  "od_cases",             "Columns: T_htf_C, m_dot_htf_ND, T_amb_C, od_opt_obj (1: MAX_ETA, 2: MAX_POWER), Rows: cases",   "",           "",    "",      "",      "",       "" },
	{ SSC_INPUT,  SSC_ARRAY,   "od_P_mc_in_sweep",     "Columns: T_htf_C, m_dot_htf_ND, T_amb_C, od_opt_obj (1: MAX_ETA, 2: MAX_POWER)", "", "", "", "",  "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_gen_od_polynomials","Generate off-design polynomials for Generic CSP models? 1 = Yes, 0 = No", "", "", "",  "?=0",     "",       "" },

	//{ SSC_INPUT,  SSC_NUMBER,  "is_m_dot_htf_fracs",   "0 = No analysis of HTF off design mass flow rate, 1 = Yes", "",        "",    "",      "*",     "",       "" },
	//{ SSC_INPUT,  SSC_ARRAY,   "m_dot_htf_fracs_in",   "Array of normalized mass flow rate",                     "",           "",    "",      "is_m_dot_htf_fracs=1",     "",       "" },
	//{ SSC_INPUT,  SSC_NUMBER,  "is_T_amb_od",          "0 = N analysis of off design ambient temperature, 1 = Yes", "",        "",    "",      "*",     "",       "" },
	//{ SSC_INPUT,  SSC_ARRAY,   "T_amb_od_in",          "Array of ambient temperatures for off-design parametric","C",          "",    "",      "is_T_amb_od=1",     "",       "" },
	// ** Design OUTPUTS **
		// System Design Solution
	{ SSC_OUTPUT, SSC_NUMBER,  "T_htf_cold_des",       "HTF design cold temperature (PHX outlet)",               "C",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "m_dot_htf_des",        "HTF mass flow rate",                                     "kg/s",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eta_thermal_calc",     "Calculated cycle thermal efficiency",                    "-",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "m_dot_co2_full",       "CO2 mass flow rate through HTR, PHX, turbine",           "kg/s",       "",    "",      "*",     "",       "" },	
	{ SSC_OUTPUT, SSC_NUMBER,  "recomp_frac",          "Recompression fraction",                                 "-",          "",    "",      "*",     "",       "" },
		// Compressor
	{ SSC_OUTPUT, SSC_NUMBER,  "T_comp_in",            "Compressor inlet temperature",                           "C",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_in",            "Compressor inlet pressure",                              "MPa",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_out",           "Compressor outlet pressure",                             "MPa",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_W_dot",             "Compressor power",                                       "MWe",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_m_dot_des",         "Compressor mass flow rate",                              "kg/s",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_phi_des",           "Compressor design flow coefficient",					 "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_tip_ratio_des",     "Compressor design stage tip speed ratio",                "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_n_stages",          "Compressor stages",                                      "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_N_des",             "Compressor design shaft speed",                          "rpm",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_D",                 "Compressor stage diameters",                             "m",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_phi_surge",         "Compressor flow coefficient where surge occurs",         "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_eta_stages_des",    "Compressor design stage isentropic efficiencies",        "",           "",    "",      "*",     "",       "" },
		// Recompressor
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_T_in_des",          "Recompressor inlet temperature",                         "C",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_W_dot",             "Recompressor power",                                     "MWe",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_m_dot_des",         "Recompressor mass flow rate",                            "kg/s",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_phi_des",           "Recompressor design flow coefficient",                   "",           "",    "",      "*",     "",       "" },					
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_tip_ratio_des",     "Recompressor design stage tip speed ratio",              "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_n_stages",          "Recompressor stages",                                    "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_N_des",             "Recompressor design shaft speed",                        "rpm",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_D",                 "Recompressor stage diameters",                           "m",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_phi_surge",         "Recompressor flow coefficient where surge occurs",       "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_eta_stages_des",    "Recompressor design stage isenstropic efficiencies",     "",           "",    "",      "*",     "",       "" },
		// Precompressor	
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_T_in_des",          "Precompressor inlet temperature",                        "C",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_P_in_des",          "Precompressor inlet pressure",                           "MPa",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_W_dot",             "Precompressor power",                                    "MWe",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_m_dot_des",         "Precompressor mass flow rate",                           "kg/s",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_phi_des",           "Precompressor design flow coefficient",                  "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_tip_ratio_des",     "Precompressor design stage tip speed ratio",             "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_n_stages",          "Precompressor stages",                                   "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_N_des",             "Precompressor design shaft speed",                       "rpm",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_D",                 "Precompressor stage diameters",                          "m",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_phi_surge",         "Precompressor flow coefficient where surge occurs",      "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_eta_stages_des",    "Precompressor design stage isenstropic efficiencies",    "",           "",    "",      "*",     "",       "" },
		// Turbine
	{ SSC_OUTPUT, SSC_NUMBER,  "t_W_dot",              "Turbine power",                                          "MWe",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_m_dot_des",          "Turbine mass flow rate",                                 "kg/s",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_turb_in",            "Turbine inlet temperature",                              "C",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_nu_des",             "Turbine design velocity ratio",                          "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_tip_ratio_des",	   "Turbine design tip speed ratio",                         "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_N_des",			   "Turbine design shaft speed",	                         "rpm",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_D",                  "Turbine diameter",                                       "m",          "",    "",      "*",     "",       "" },
		// Recuperators																				 
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_recup_total",       "Total recuperator UA",                                   "MW/K",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_LTR",               "Low temp recuperator UA",                                "MW/K",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eff_LTR",              "Low temp recuperator effectiveness",                     "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "NTU_LTR",              "Low temp recuperator NTU",                               "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "q_dot_LTR",            "Low temp recuperator heat transfer",                     "MWt",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_HTR",               "High temp recuperator UA",                               "MW/K",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eff_HTR",              "High temp recuperator effectiveness",                    "",           "",    "",      "*",     "",       "" },	
	{ SSC_OUTPUT, SSC_NUMBER,  "NTU_HTR",              "High temp recuperator NTRU",                             "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "q_dot_HTR",            "High temp recuperator heat transfer",                    "MWt",        "",    "",      "*",     "",       "" },
		// PHX Design Solution
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_PHX",               "PHX Conductance",                                        "MW/K",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eff_PHX",              "PHX effectiveness",                                      "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "NTU_PHX",              "PHX NTU",                                                "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_co2_PHX_in",         "CO2 temperature at PHX inlet",                           "C",          "",    "",      "*",     "",       "" },	
	{ SSC_OUTPUT, SSC_NUMBER,  "deltaT_HTF_PHX",       "HTF temp difference across PHX",                         "C",          "",    "",      "*",     "",       "" },	
	{ SSC_OUTPUT, SSC_NUMBER,  "q_dot_PHX",            "PHX heat transfer",                                      "MWt",          "",    "",      "*",     "",       "" },	
		// Cooler
	{ SSC_OUTPUT, SSC_NUMBER,  "T_cooler_in",          "Cooler inlet temperature",                               "C",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_cooler_in",          "Compressor inlet pressure",                              "MPa",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "m_dot_co2_cooler",     "CO2 mass flow rate through cooler",                      "kg/s",       "",    "",      "*",     "",       "" },	
		// State Points
	{ SSC_OUTPUT, SSC_ARRAY,  "T_state_points",       "Cycle temperature state points",      "C",	      "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "P_state_points",       "Cycle pressure state points",         "MPa",       "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_state_points",       "Cycle entropy state points",          "kJ/kg-K",   "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "h_state_points",       "Cycle enthalpy state points",         "kJ/kg",     "",   "",   "*",   "",   "" },
		// T-s plot data
	{ SSC_OUTPUT, SSC_ARRAY,  "T_LTR_HP_data",        "Temperature points along LTR HP stream",        "C",	       "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_LTR_HP_data",        "Entropy points along LTR HP stream",            "kJ/kg-K",    "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "T_HTR_HP_data",        "Temperature points along HTR HP stream",        "C",	       "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_HTR_HP_data",        "Entropy points along HTR HP stream",            "kJ/kg-K",    "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "T_PHX_data",           "Temperature points along PHX stream",           "C",	       "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_PHX_data",           "Entropy points along PHX stream",               "kJ/kg-K",    "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "T_HTR_LP_data",        "Temperature points along HTR LP stream",        "C",	       "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_HTR_LP_data",        "Entropy points along HTR LP stream",            "kJ/kg-K",    "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "T_LTR_LP_data",        "Temperature points along LTR LP stream",        "C",	       "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_LTR_LP_data",        "Entropy points along LTR LP stream",            "kJ/kg-K",    "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "T_main_cooler_data",   "Temperature points along main cooler stream",   "C",	       "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_main_cooler_data",   "Entropy points along main cooler stream",       "kJ/kg-K",    "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "T_pre_cooler_data",    "Temperature points along pre cooler stream",    "C",	       "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_pre_cooler_data",    "Entropy points along pre cooler stream",        "kJ/kg-K",    "",   "",   "*",   "",   "" },
		// P-h plot data
	{ SSC_OUTPUT, SSC_ARRAY,  "P_t_data",             "Pressure points along turbine expansion",       "MPa",	   "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "h_t_data",             "Enthalpy points along turbine expansion",       "kJ/kg",    "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "P_mc_data",            "Pressure points along main compression",        "MPa",	   "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "h_mc_data",            "Enthalpy points along main compression",        "kJ/kg",    "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "P_rc_data",            "Pressure points along re compression",          "MPa",	   "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "h_rc_data",            "Enthalpy points along re compression",          "kJ/kg",    "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "P_pc_data",            "Pressure points along pre compression",         "MPa",	   "",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "h_pc_data",            "Enthalpy points along pre compression",         "kJ/kg",    "",   "",   "*",   "",   "" },

		// Air Cooler Design
	// ?????

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
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_W_dot_od",          "Off-design main compressor power",                       "MWe",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_m_dot_od",          "Off-design main compressor mass flow",                   "kg/s",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_N_od",              "Off-design main compressor speed",                       "rpm",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_eta_od",            "Off-design main compressor overall isentropic efficiency", "",         "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "mc_tip_ratio_od",      "Off-design main compressor tip speed ratio [od run][stage]", "",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "mc_eta_stages_od",     "Off-design main compressor stages isentropic efficiency [od run][stage]", "", "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_f_bypass_od",       "Off-design main compressor bypass to cooler inlet",      "-",          "",    "",      "",     "",       "" },
		// Recompressor
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_T_in_od",           "Off-design recompressor inlet temperature",              "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_W_dot_od",          "Off-design recompressor power",                          "MWe",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_m_dot_od",          "Off-design recompressor mass flow",                      "kg/s",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_eta_od",            "Off-design recompressor overal isentropic efficiency",   "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "rc_phi_od",            "Off-design recompressor flow coefficient [od run][stage]", "-",		   "",    "",      "",     "",       "" },
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
		// Turbine																											   
	{ SSC_OUTPUT, SSC_ARRAY,   "t_W_dot_od",           "Off-design turbine power",                               "MWe",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_m_dot_od",           "Off-design turbine mass flow rate",                      "kg/s",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_nu_od",              "Off-design turbine velocity ratio",	                     "-",	       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_N_od",               "Off-design turbine shaft speed",	                     "rpm",	       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_tip_ratio_od",       "Off-design turbine tip speed ratio",                     "-",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_eta_od",             "Off-design turbine efficiency",                          "-",          "",    "",      "",     "",       "" },
		// Recuperators
	{ SSC_OUTPUT, SSC_ARRAY,   "eff_LTR_od",           "Off-design low temp recup effectiveness",                "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "q_dot_LTR_od",         "Off-design low temp recup heat transfer",                "MWt",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "eff_HTR_od",           "Off-design high temp recup effectiveness",               "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "q_dot_HTR_od",         "Off-design high temp recup heat transfer",               "MWt",        "",    "",      "",     "",       "" },
		// PHX 
	{ SSC_OUTPUT, SSC_ARRAY,   "T_co2_PHX_in_od",      "Off-design PHX co2 inlet temperature",                   "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_co2_PHX_out_od",     "Off-design PHX co2 outlet temperature",                  "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "deltaT_HTF_PHX_od",    "Off-design HTF temp difference across PHX",              "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "phx_eff_od",           "Off-design PHX effectiveness",                           "-",          "",    "",      "",     "",       "" },
		// Cooler
	{ SSC_OUTPUT, SSC_ARRAY,   "T_cooler_in_od",       "Off-design cooler inlet temperature",                    "C",          "",    "",      "",     "",       "" },
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
	ssc_number_t *p_mc_W_dot_od;
	ssc_number_t *p_mc_m_dot_od;
	ssc_number_t *p_mc_N_od;
	ssc_number_t *p_mc_eta_od;
	ssc_number_t *pm_mc_tip_ratio_od;
	ssc_number_t *pm_mc_eta_stages_od;
	ssc_number_t *p_mc_f_bypass_od;
	// Recompressor
	ssc_number_t *p_rc_T_in_od;
	ssc_number_t *p_rc_W_dot_od;
	ssc_number_t *p_rc_m_dot_od;
	ssc_number_t *p_rc_eta_od;
	ssc_number_t *pm_rc_phi_od;
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
	// Turbine
	ssc_number_t *p_t_W_dot_od;
	ssc_number_t *p_t_m_dot_od;
	ssc_number_t *p_t_nu_od;
	ssc_number_t *p_t_N_od;
	ssc_number_t *p_t_tip_ratio_od;
	ssc_number_t *p_t_eta_od;
	// Recuperator
	ssc_number_t *p_eff_LTR_od;
	ssc_number_t *p_q_dot_LTR_od;
	ssc_number_t *p_eff_HTR_od;
	ssc_number_t *p_q_dot_HTR_od;
	// PHX
	ssc_number_t *p_T_co2_PHX_in_od;
	ssc_number_t *p_T_co2_PHX_out_od;
	ssc_number_t *p_deltaT_HTF_PHX_od;
	ssc_number_t *p_phx_eff_od;
	// Cooler
	ssc_number_t *p_T_cooler_in_od;
	// Solver Metrics
	ssc_number_t *p_od_code;

	cm_sco2_csp_system()
	{
		add_var_info(_cm_vtab_sco2_csp_system);
	}

	void exec() throw(general_error)
	{
		C_sco2_recomp_csp::S_des_par sco2_rc_des_par;
			// System design parameters
		sco2_rc_des_par.m_hot_fl_code = as_integer("htf");							//[-] Integer code for HTF
		sco2_rc_des_par.mc_hot_fl_props = as_matrix("htf_props");					//[-] Custom HTF properties
		sco2_rc_des_par.m_T_htf_hot_in = as_double("T_htf_hot_des")+273.15;			//[K] Convert from C
		sco2_rc_des_par.m_phx_dt_hot_approach = as_double("dT_PHX_hot_approach");	//[K/C] Temperature difference between hot HTF and turbine CO2 inlet
		sco2_rc_des_par.m_T_amb_des = as_double("T_amb_des")+273.15;				//[K] Convert from C
		sco2_rc_des_par.m_dt_mc_approach = as_double("dT_mc_approach");				//[K/C] Temperature difference between ambient air and main compressor inlet
		sco2_rc_des_par.m_elevation = as_double("site_elevation");					//[m] Site elevation
		sco2_rc_des_par.m_W_dot_net = as_double("W_dot_net_des")*1000.0;			//[kWe] Convert from MWe, cycle power output w/o cooling parasitics
			
		sco2_rc_des_par.m_cycle_config = as_integer("cycle_config");			//[-] 1 = recompression, 2 = partial cooling

		sco2_rc_des_par.m_design_method = as_integer("design_method");			//[-] 1 = Specify efficiency, 2 = Specify total recup UA
		if( sco2_rc_des_par.m_design_method == 1 )
		{
			sco2_rc_des_par.m_eta_thermal = as_double("eta_thermal_des");				//[-] Cycle thermal efficiency
			if( sco2_rc_des_par.m_eta_thermal < 0.0 )
			{
				log("For cycle design method = 1, the input cycle thermal efficiency must be greater than 0", SSC_ERROR, -1.0);
				return;
			}
			sco2_rc_des_par.m_UA_recup_tot_des = std::numeric_limits<double>::quiet_NaN();
		}
		else if( sco2_rc_des_par.m_design_method == 2 )
		{
			sco2_rc_des_par.m_UA_recup_tot_des = as_double("UA_recup_tot_des");		//[kW/K] Total recuperator conductance
			if( sco2_rc_des_par.m_UA_recup_tot_des < 0.0 )
			{
				log("For cycle design method = 2, the input total recuperator conductance must be greater than 0", SSC_ERROR, -1.0);
				return;
			}
			sco2_rc_des_par.m_eta_thermal = std::numeric_limits<double>::quiet_NaN();
		}
		else
		{
			std::string err_msg = util::format("The input cycle design method, %d, is invalid. It must be 1 or 2.", sco2_rc_des_par.m_design_method);
			log(err_msg, SSC_ERROR, -1.0);
		}

		sco2_rc_des_par.m_is_recomp_ok = as_integer("is_recomp_ok");

		double mc_PR_in = as_double("is_PR_fixed");		//[-]
		if (mc_PR_in != 0.0)
		{
			if (mc_PR_in < 0.0)
			{
				sco2_rc_des_par.m_PR_mc_guess = mc_PR_in*1.E3;		//[kPa] convert from MPa
			}
			else
			{
				sco2_rc_des_par.m_PR_mc_guess = mc_PR_in;			//[-] Pressure Ratio!
			}
				sco2_rc_des_par.m_fixed_PR_mc = true;
		}
		else
		{
			sco2_rc_des_par.m_PR_mc_guess = std::numeric_limits<double>::quiet_NaN();
			sco2_rc_des_par.m_fixed_PR_mc = false;
		}

			// Cycle design parameters: hardcode pressure drops, for now
		// Define hardcoded sco2 design point parameters
		std::vector<double> DP_LT(2);
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		DP_LT[0] = 0;
		DP_LT[1] = 0;
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		std::vector<double> DP_HT(2);
		DP_HT[0] = 0;
		DP_HT[1] = 0;
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		std::vector<double> DP_PC(2);
		DP_PC[0] = 0;
		DP_PC[1] = 0;
		/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
		std::vector<double> DP_PHX(2);
		DP_PHX[0] = 0;
		DP_PHX[1] = 0;
		sco2_rc_des_par.m_DP_LT = DP_LT;
		sco2_rc_des_par.m_DP_HT = DP_HT;
		sco2_rc_des_par.m_DP_PC = DP_PC;
		sco2_rc_des_par.m_DP_PHX = DP_PHX;
		sco2_rc_des_par.m_N_sub_hxrs = 10;

		// 1.30.17 twn: try 30k
		//sco2_rc_des_par.m_N_turbine = 3600.0;
		sco2_rc_des_par.m_N_turbine = 30000.0;

		//sco2_rc_des_par.m_tol = 1.E-3;
		//sco2_rc_des_par.m_opt_tol = 1.E-3;
		sco2_rc_des_par.m_tol = pow(10, -as_double("rel_tol"));
		sco2_rc_des_par.m_opt_tol = pow(10, -as_double("rel_tol"));
		
			// Remaining cycle design parameters
		sco2_rc_des_par.m_LT_eff_max = as_double("LT_recup_eff_max");
		sco2_rc_des_par.m_HT_eff_max = as_double("HT_recup_eff_max");
		sco2_rc_des_par.m_eta_mc = as_double("eta_isen_mc");
		sco2_rc_des_par.m_eta_rc = as_double("eta_isen_rc");
		sco2_rc_des_par.m_eta_pc = as_double("eta_isen_pc");
		sco2_rc_des_par.m_eta_t = as_double("eta_isen_t");
		sco2_rc_des_par.m_P_high_limit = as_double("P_high_limit")*1000.0;		//[kPa], convert from MPa		
			
			// PHX design parameters
		sco2_rc_des_par.m_des_objective_type = as_integer("des_objective");		//[-] 
		sco2_rc_des_par.m_min_phx_deltaT = as_double("min_phx_deltaT");			//[C]
		sco2_rc_des_par.m_phx_dt_cold_approach = as_double("dT_PHX_cold_approach");
			
			// Air cooler parameters
		sco2_rc_des_par.m_frac_fan_power = as_double("fan_power_frac");
		sco2_rc_des_par.m_deltaP_cooler_frac = as_double("deltaP_cooler_frac");

		// For try/catch below
		int out_type = -1;
		std::string out_msg = "";

		// Construction class and design system
		C_sco2_recomp_csp c_sco2_recomp_csp;
		C_sco2_recomp_csp *p_sco2_recomp_csp = &c_sco2_recomp_csp;

		// Pass through callback function and pointer
		p_sco2_recomp_csp->mf_callback_update = ssc_cmod_update;
		p_sco2_recomp_csp->mp_mf_update = (void*)(this);

		try
		{
			p_sco2_recomp_csp->design(sco2_rc_des_par);
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while (p_sco2_recomp_csp->mc_messages.get_message(&out_type, &out_msg))
			{
				log(out_msg + "\n");
				log("\n");
			}

			throw exec_error("sco2_csp_system", csp_exception.m_error_message);
		}

		// If all calls were successful, log to SSC any messages from sco2_recomp_csp
		while (p_sco2_recomp_csp->mc_messages.get_message(&out_type, &out_msg))
		{
			log(out_msg + "\n");
		}

		// Helpful to know right away whether cycle contains recompressor
		bool is_rc = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_is_rc;

		// Get data for P-h cycle plot
		std::vector<double> P_t;		//[MPa]
		std::vector<double> h_t;		//[kJ/kg]
		std::vector<double> P_mc;		//[MPa]
		std::vector<double> h_mc;		//[kJ/kg]
		std::vector<double> P_rc;		//[MPa]
		std::vector<double> h_rc;		//[kJ/kg]
		std::vector<double> P_pc;		//[MPa]
		std::vector<double> h_pc;		//[kJ/kg]
		int ph_err_code = sco2_cycle_plot_data_PH(sco2_rc_des_par.m_cycle_config,
			p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_temp,
			p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_pres,
			P_t,
			h_t,
			P_mc,
			h_mc,
			P_rc,
			h_rc,
			P_pc,
			h_pc);

		if (ph_err_code != 0)
			throw exec_error("sco2_csp_system", "cycle plot data routine failed");

		int n_v = P_t.size();
		ssc_number_t *p_P_t_data = allocate("P_t_data", n_v);
		ssc_number_t *p_h_t_data = allocate("h_t_data", n_v);
		for (int i = 0; i < n_v; i++)
		{
			p_P_t_data[i] = (ssc_number_t)(P_t[i]);		//[MPa]
			p_h_t_data[i] = (ssc_number_t)(h_t[i]);		//[kJ/kg]
		}

		n_v = P_mc.size();
		ssc_number_t *p_P_mc_data = allocate("P_mc_data", n_v);
		ssc_number_t *p_h_mc_data = allocate("h_mc_data", n_v);
		for (int i = 0; i < n_v; i++)
		{
			p_P_mc_data[i] = (ssc_number_t)(P_mc[i]);		//[MPa]
			p_h_mc_data[i] = (ssc_number_t)(h_mc[i]);		//[kJ/kg]
		}

		n_v = P_rc.size();
		ssc_number_t *p_P_rc_data = allocate("P_rc_data", n_v);
		ssc_number_t *p_h_rc_data = allocate("h_rc_data", n_v);
		for (int i = 0; i < n_v; i++)
		{
			p_P_rc_data[i] = (ssc_number_t)(P_rc[i]);		//[MPa]
			p_h_rc_data[i] = (ssc_number_t)(h_rc[i]);		//[kJ/kg]
		}

		n_v = P_pc.size();
		ssc_number_t *p_P_pc_data = allocate("P_pc_data", n_v);
		ssc_number_t *p_h_pc_data = allocate("h_pc_data", n_v);
		for (int i = 0; i < n_v; i++)
		{
			p_P_pc_data[i] = (ssc_number_t)(P_pc[i]);		//[MPa]
			p_h_pc_data[i] = (ssc_number_t)(h_pc[i]);		//[kJ/kg]
		}

		// Get data for T-s cycle plot
		std::vector<double> T_LTR_HP;	//[C]
		std::vector<double> s_LTR_HP;	//[kJ/kg-K]
		std::vector<double> T_HTR_HP;	//[C]
		std::vector<double> s_HTR_HP;	//[kJ/kg-K]
		std::vector<double> T_PHX;		//[C]
		std::vector<double> s_PHX;		//[kJ/kg-K]
		std::vector<double> T_HTR_LP;	//[C]
		std::vector<double> s_HTR_LP;	//[kJ/kg-K]
		std::vector<double> T_LTR_LP;	//[C]
		std::vector<double> s_LTR_LP;	//[kJ/kg-K]
		std::vector<double> T_main_cooler;	//[C]
		std::vector<double> s_main_cooler;	//[kJ/kg-K]
		std::vector<double> T_pre_cooler;	//[C]
		std::vector<double> s_pre_cooler;	//[kJ/kg-K]
		int plot_data_err_code = sco2_cycle_plot_data_TS(sco2_rc_des_par.m_cycle_config,
			p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_pres,
			p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_entr,
			T_LTR_HP,
			s_LTR_HP,
			T_HTR_HP,
			s_HTR_HP,
			T_PHX,
			s_PHX,
			T_HTR_LP,
			s_HTR_LP,
			T_LTR_LP,
			s_LTR_LP,
			T_main_cooler,
			s_main_cooler,
			T_pre_cooler,
			s_pre_cooler);
		
		if(plot_data_err_code != 0)
			throw exec_error("sco2_csp_system", "cycle plot data routine failed");

		n_v = T_LTR_HP.size();
		ssc_number_t *p_T_LTR_HP_data = allocate("T_LTR_HP_data", n_v);
		ssc_number_t *p_s_LTR_HP_data = allocate("s_LTR_HP_data", n_v);
		for (int i = 0; i < n_v; i++)
		{
			p_T_LTR_HP_data[i] = (ssc_number_t)(T_LTR_HP[i]);	//[C]
			p_s_LTR_HP_data[i] = (ssc_number_t)(s_LTR_HP[i]);	//[kJ/kg-K]
		}

		n_v = T_HTR_HP.size();
		ssc_number_t *p_T_HTR_HP_data = allocate("T_HTR_HP_data", n_v);
		ssc_number_t *p_s_HTR_HP_data = allocate("s_HTR_HP_data", n_v);
		for (int i = 0; i < n_v; i++)
		{
			p_T_HTR_HP_data[i] = (ssc_number_t)(T_HTR_HP[i]);		//[C]
			p_s_HTR_HP_data[i] = (ssc_number_t)(s_HTR_HP[i]);		//[kJ/kg-K]
		}

		n_v = T_PHX.size();
		ssc_number_t *p_T_PHX_data = allocate("T_PHX_data", n_v);
		ssc_number_t *p_s_PHX_data = allocate("s_PHX_data", n_v);
		for (int i = 0; i < n_v; i++)
		{
			p_T_PHX_data[i] = (ssc_number_t)(T_PHX[i]);			//[C]
			p_s_PHX_data[i] = (ssc_number_t)(s_PHX[i]);			//[kJ/kg-K]
		}

		n_v = T_HTR_LP.size();
		ssc_number_t *p_T_HTR_LP_data = allocate("T_HTR_LP_data", n_v);
		ssc_number_t *p_s_HTR_LP_data = allocate("s_HTR_LP_data", n_v);
		for (int i = 0; i < n_v; i++)
		{
			p_T_HTR_LP_data[i] = (ssc_number_t)(T_HTR_LP[i]);	//[C]
			p_s_HTR_LP_data[i] = (ssc_number_t)(s_HTR_LP[i]);	//[kJ/kg-K]
		}

		n_v = T_LTR_LP.size();
		ssc_number_t *p_T_LTR_LP_data = allocate("T_LTR_LP_data", n_v);
		ssc_number_t *p_s_LTR_LP_data = allocate("s_LTR_LP_data", n_v);
		for (int i = 0; i < n_v; i++)
		{
			p_T_LTR_LP_data[i] = (ssc_number_t)(T_LTR_LP[i]);	//[C]
			p_s_LTR_LP_data[i] = (ssc_number_t)(s_LTR_LP[i]);	//[kJ/kg-K]
		}

		n_v = T_main_cooler.size();
		ssc_number_t *p_T_main_cooler = allocate("T_main_cooler_data", n_v);
		ssc_number_t *p_s_main_cooler = allocate("s_main_cooler_data", n_v);
		for (int i = 0; i < n_v; i++)
		{
			p_T_main_cooler[i] = (ssc_number_t)(T_main_cooler[i]);	//[C]
			p_s_main_cooler[i] = (ssc_number_t)(s_main_cooler[i]);	//[kJ/kg-K]
		}

		n_v = T_pre_cooler.size();
		ssc_number_t *p_T_pre_cooler = allocate("T_pre_cooler_data", n_v);
		ssc_number_t *p_s_pre_cooler = allocate("s_pre_cooler_data", n_v);
		for (int i = 0; i < n_v; i++)
		{
			p_T_pre_cooler[i] = (ssc_number_t)(T_pre_cooler[i]);	//[C]
			p_s_pre_cooler[i] = (ssc_number_t)(s_pre_cooler[i]);	//[kJ/kg-K]
		}

		// Set SSC design outputs
		// System
		double m_dot_htf_design = p_sco2_recomp_csp->get_phx_des_par()->m_m_dot_hot_des;	//[kg/s]
		double T_htf_cold_calc = p_sco2_recomp_csp->get_design_solved()->ms_phx_des_solved.m_T_h_out;		//[K]
		assign("T_htf_cold_des", (ssc_number_t)(T_htf_cold_calc - 273.15));		//[C] convert from K
		assign("m_dot_htf_des", (ssc_number_t)m_dot_htf_design);				//[kg/s]
		assign("eta_thermal_calc", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_eta_thermal);	//[-]
		assign("m_dot_co2_full", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_m_dot_t);		//[kg/s]
		assign("recomp_frac", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_recomp_frac);		//[-]
		// Compressor
		assign("T_comp_in", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::MC_IN] - 273.15));		//[C] convert from K
		assign("P_comp_in", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::MC_IN] / 1000.0));		//[MPa] convert from kPa
		assign("P_comp_out", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::MC_OUT] / 1000.0));		//[MPa] convert from kPa
		assign("mc_W_dot", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_W_dot_mc*1.E-3));	//[MWe] convert kWe
		assign("mc_m_dot_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_m_dot_t*(1.0 - p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_recomp_frac));	//[kg/s]
		assign("mc_phi_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_phi_des);
		assign("mc_tip_ratio_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_tip_ratio_max);		//[-]
		
		int n_mc_stages = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_n_stages;
		assign("mc_n_stages", (ssc_number_t)n_mc_stages);	//[-]
		assign("mc_N_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_N_design);	//[rpm]
		
		ssc_number_t *p_mc_D = allocate("mc_D", n_mc_stages);
		ssc_number_t *p_mc_tip_ratio_des = allocate("mc_tip_ratio_des", n_mc_stages);
		ssc_number_t *p_mc_eta_stages_des = allocate("mc_eta_stages_des", n_mc_stages);
		std::vector<double> v_mc_D = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.mv_D;	//[m]
		std::vector<double> v_mc_tip_ratio_des = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.mv_tip_speed_ratio;	//[-]
		std::vector<double> v_mc_eta_stages_des = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.mv_eta_stages;		//[-]
		for (int i = 0; i < n_mc_stages; i++)
		{
			p_mc_D[i] = (ssc_number_t)v_mc_D[i];		//[m]
			p_mc_tip_ratio_des[i] = (ssc_number_t)v_mc_tip_ratio_des[i];	//[-]
			p_mc_eta_stages_des[i] = (ssc_number_t)v_mc_eta_stages_des[i];	//[-]
		}
		
		assign("mc_phi_surge", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_phi_surge);	//[-]

		// Recompressor
		assign("rc_W_dot", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_W_dot_rc*1.E-3));	//[MWe] convert from kWe
		assign("rc_m_dot_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_m_dot_t*p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_recomp_frac);	//[kg/s]
		int n_rc_stages = 0;
		if (is_rc)
		{
			assign("rc_T_in_des", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_T_in - 273.15));		//[C]
			assign("rc_phi_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_phi_des);	//[-]

			n_rc_stages = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_n_stages;		//[-]
			assign("rc_n_stages", (ssc_number_t)n_rc_stages);	//[-]
			assign("rc_N_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_N_design);	//[rpm]

			ssc_number_t *p_rc_D = allocate("rc_D", n_rc_stages);
			ssc_number_t *p_rc_tip_ratio_des = allocate("rc_tip_ratio_des", n_rc_stages);
			ssc_number_t *p_rc_eta_stages_des = allocate("rc_eta_stages_des", n_rc_stages);
			std::vector<double> v_rc_D = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.mv_D;
			std::vector<double> v_rc_tip_ratio_des = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.mv_tip_speed_ratio;	//[-]
			std::vector<double> v_rc_eta_stages_des = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.mv_eta_stages;	//[-]
			for (int i = 0; i < n_rc_stages; i++)
			{
				p_rc_D[i] = (ssc_number_t)v_rc_D[i];		//[m]
				p_rc_tip_ratio_des[i] = (ssc_number_t)v_rc_tip_ratio_des[i];	//[-]
				p_rc_eta_stages_des[i] = (ssc_number_t)v_rc_eta_stages_des[i];	//[-]
			}
			assign("rc_phi_surge", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_phi_surge);//[-]
		}
		else
		{
			double ssc_nan = std::numeric_limits<ssc_number_t>::quiet_NaN();
			assign("rc_T_in_des", ssc_nan);
			assign("rc_phi_des", ssc_nan);
			assign("rc_n_stages", n_rc_stages);
			assign("rc_N_des", ssc_nan);
			ssc_number_t *p_rc_D = allocate("rc_D", 1);
			p_rc_D[0] = ssc_nan;
			ssc_number_t *p_rc_tip_ratio_des = allocate("rc_tip_ratio_des", 1);
			p_rc_tip_ratio_des[0] = ssc_nan;
			ssc_number_t *p_rc_eta_stages_des = allocate("rc_eta_stages_des", 1);
			p_rc_eta_stages_des[0] = ssc_nan;
			assign("rc_phi_surge", ssc_nan);

			// Set number of RC stages = 1 so nan array allocations work
			n_rc_stages = 1;
		}
		
		// Precompressor		
		assign("pc_W_dot", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_W_dot_pc*1.E-3));	//[MWe] convert from kWe
		assign("pc_m_dot_des", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_m_dot_pc));		//[kg/s]
		int n_pc_stages = 0;
		if (sco2_rc_des_par.m_cycle_config == 2)
		{
			assign("pc_T_in_des", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_T_in - 273.15));
			assign("pc_P_in_des", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_P_in*1.E-3));
			assign("pc_phi_des", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_phi_des)); //[-]
			
			n_pc_stages = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_n_stages;		//[-]
			assign("pc_n_stages", (ssc_number_t)n_pc_stages);	//[-]
			assign("pc_N_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_N_design);	//[rpm]
			
			ssc_number_t *p_pc_D = allocate("pc_D", n_pc_stages);
			ssc_number_t *p_pc_tip_ratio_des = allocate("pc_tip_ratio_des", n_pc_stages);
			ssc_number_t *p_pc_eta_stages_des = allocate("pc_eta_stages_des", n_pc_stages);
			std::vector<double> v_pc_D = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.mv_D;	//[m]
			std::vector<double> v_pc_tip_ratio_des = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.mv_tip_speed_ratio;	//[-]
			std::vector<double> v_pc_eta_stages_des = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.mv_eta_stages;		//[-]
			for (int i = 0; i < n_pc_stages; i++)
			{
				p_pc_D[i] = (ssc_number_t)v_pc_D[i];	//[m]
				p_pc_tip_ratio_des[i] = (ssc_number_t)v_pc_tip_ratio_des[i];		//[-]
				p_pc_eta_stages_des[i] = (ssc_number_t)v_pc_eta_stages_des[i];		//[-]
			}
			assign("pc_phi_surge", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_phi_surge);	//[-]
		}
		else
		{
			double ssc_nan = std::numeric_limits<ssc_number_t>::quiet_NaN();
			assign("pc_T_in_des", ssc_nan);
			assign("pc_P_in_des", ssc_nan);
			assign("pc_phi_des", ssc_nan);
			assign("pc_n_stages", ssc_nan);
			assign("pc_N_des", ssc_nan);

			ssc_number_t *p_pc_D = allocate("pc_D", 1);
			p_pc_D[0] = ssc_nan;
			ssc_number_t *p_pc_tip_ratio_des = allocate("pc_tip_ratio_des", 1);
			p_pc_tip_ratio_des[0] = ssc_nan;
			ssc_number_t *p_pc_eta_stages_des = allocate("pc_eta_stages_des", 1);
			p_pc_eta_stages_des[0] = ssc_nan;

			assign("pc_phi_surge", ssc_nan);

			n_pc_stages = 1;
		}
		// Turbine
		assign("t_W_dot", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_W_dot_t*1.E-3));	//[MWe] convert from kWe
		assign("t_m_dot_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_m_dot_t);		//[kg/s]
		assign("T_turb_in", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::TURB_IN] - 273.15));	//[C] Turbine inlet temp, convert from K
		assign("t_nu_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_nu_design);           //[-]
		assign("t_tip_ratio_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_w_tip_ratio);  //[-]
		assign("t_N_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_N_design);			   //[rpm]
		assign("t_D", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_D_rotor);                  //[m]
		// Recuperator
		double UA_LTR = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_UA_LTR*1.E-3;	//[MW/K] convert from kW/K
		double UA_HTR = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_UA_HTR*1.E-3;	//[MW/K] convert from kW/K
		assign("UA_recup_total", (ssc_number_t)(UA_LTR + UA_HTR));	//[MW/K]
		assign("UA_LTR", (ssc_number_t)UA_LTR);				//[MW/K]
		assign("eff_LTR", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_eff_design);		//[-]
		assign("NTU_LTR", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_NTU_design);		//[-]
		assign("q_dot_LTR", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_Q_dot_design*1.E-3));	//[MWt] convert from kWt
		assign("UA_HTR", (ssc_number_t)UA_HTR);				//[MW/K]
		assign("eff_HTR", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_eff_design);		//[-]
		assign("NTU_HTR", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_NTU_design);		//[-]
		assign("q_dot_HTR", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_Q_dot_design*1.E-3));	//[MWt] convert from kWt
			// PHX
		assign("UA_PHX", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_phx_des_solved.m_UA_design_total*1.E-3));	//[MW/K] convert from kW/K
		assign("eff_PHX", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_phx_des_solved.m_eff_design);				//[-]
		assign("NTU_PHX", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_phx_des_solved.m_NTU_design);				//[-]
		assign("T_co2_PHX_in", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::HTR_HP_OUT] - 273.15));	//[C]
		assign("deltaT_HTF_PHX", (ssc_number_t)sco2_rc_des_par.m_T_htf_hot_in - T_htf_cold_calc);		//[K]
		assign("q_dot_PHX", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_phx_des_solved.m_Q_dot_design*1.E-3));	//[MWt] convert from kWt
			// Cooler
		assign("T_cooler_in", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::LTR_LP_OUT] - 273.15));	//[C]
		assign("P_cooler_in", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::LTR_LP_OUT] / 1.E3));	//[MPa]
		assign("m_dot_co2_cooler", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_m_dot_mc);			//[kg/s]
			// State Points
		ssc_number_t *p_T_state_points = allocate("T_state_points", C_sco2_cycle_core::END_SCO2_STATES);
		ssc_number_t *p_P_state_points = allocate("P_state_points", C_sco2_cycle_core::END_SCO2_STATES);
		ssc_number_t *p_s_state_points = allocate("s_state_points", C_sco2_cycle_core::END_SCO2_STATES);
		ssc_number_t *p_h_state_points = allocate("h_state_points", C_sco2_cycle_core::END_SCO2_STATES);
		for( int i = 0; i < C_sco2_cycle_core::END_SCO2_STATES; i++ )
		{
			p_T_state_points[i] = (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_temp[i]-273.15);	//[C]
			p_P_state_points[i] = (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_pres[i] / 1.E3);	//[MPa]
			p_s_state_points[i] = (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_entr[i]);			//[kJ/kg-K]
			p_h_state_points[i] = (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_enth[i]);			//[kJ/kg]
		}

		// Check if 'od_cases' is assigned
		bool is_od_cases_assigned = is_assigned("od_cases");
		bool is_P_mc_in_od_sweep_assigned = is_assigned("od_P_mc_in_sweep");
		if (is_od_cases_assigned && is_P_mc_in_od_sweep_assigned)
		{
			log("Both off design cases and main compressor inlet sweep assigned. Only modeling off design cases");
			is_P_mc_in_od_sweep_assigned = false;
		}
		if (!is_od_cases_assigned && !is_P_mc_in_od_sweep_assigned)
		{
			log("No off-design cases or main compressor inlet sweep specified");
			return;
		}
		
		// Set up off-design analysis
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
		else
		{
			std::vector<double> od_case = as_vector_double("od_P_mc_in_sweep");
			int n_od = od_case.size();
			if (n_od != 5)
			{
				std::string err_msg = util::format("The matrix of off design cases requires 5 columns. The entered matrix has %d columns", n_od);
				throw exec_error("sco2_csp_system", err_msg);
			}

			double P_LP_comp_in = std::numeric_limits<double>::quiet_NaN();
			double delta_P = std::numeric_limits<double>::quiet_NaN();

			if (sco2_rc_des_par.m_cycle_config == 1)
			{
				P_LP_comp_in = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::MC_IN] / 1000.0;		//[MPa] convert from kPa
				delta_P = 10.0;
			}
			else
			{
				P_LP_comp_in = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::PC_IN] / 1000.0;		//[MPa] convert from kPa
				delta_P = 6.0;
			}
			int n_P_mc_in = 101;

			double P_mc_in_low = P_LP_comp_in - delta_P / 2.0;	//[MPa]

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
		
		int n_od_runs = (int)od_cases.nrows();
		allocate_ssc_outputs(n_od_runs, n_mc_stages, n_rc_stages, n_pc_stages);
		C_sco2_recomp_csp::S_od_par sco2_rc_od_par;

		for(int n_run = 0; n_run < n_od_runs; n_run++)
		{			
			// Try calling off-design model with design parameters
				// Set outputs
			p_T_htf_hot_od[n_run] = (ssc_number_t)od_cases(n_run, 0);			//[C]
			p_m_dot_htf_fracs[n_run] = (ssc_number_t)od_cases(n_run, 1);		//[-]
			p_T_amb_od[n_run] = (ssc_number_t)od_cases(n_run, 2);				//[C]
			p_od_opt_obj_code[n_run] = (ssc_number_t)od_cases(n_run, 3);		//[-]
			p_od_opt_conv_tol[n_run] = (ssc_number_t)od_cases(n_run, 4);		//[-]
				// Set input structure
			sco2_rc_od_par.m_T_htf_hot = p_T_htf_hot_od[n_run] + 273.15;	//[K]
			sco2_rc_od_par.m_m_dot_htf = m_dot_htf_design*p_m_dot_htf_fracs[n_run];	//[kg/s]
			sco2_rc_od_par.m_T_amb = p_T_amb_od[n_run] + 273.15;				//[K]
			int od_strategy = (int)p_od_opt_obj_code[n_run];		//[-]
			//double od_opt_tol = p_od_opt_conv_tol[n_run];			//[-]

			int off_design_code = 0;
			std::clock_t clock_start = std::clock();
			try
			{
				if (is_od_cases_assigned)
				{
					// 2D optimization
					//off_design_code = sco2_recomp_csp.off_design_opt(sco2_rc_od_par, od_strategy);
						// Nested optimization
					od_strategy = C_sco2_recomp_csp::E_TARGET_POWER_ETA_MAX;
					off_design_code = p_sco2_recomp_csp->optimize_off_design(sco2_rc_od_par, od_strategy);
				}
				else if (is_P_mc_in_od_sweep_assigned)
				{
					od_strategy = C_sco2_recomp_csp::E_TARGET_POWER_ETA_MAX;
					off_design_code = p_sco2_recomp_csp->off_design_fix_P_mc_in(sco2_rc_od_par, od_cases(n_run, 5), od_strategy);
				}
			}
			catch( C_csp_exception &csp_exception )
			{
				// Report warning before exiting with error
				while (p_sco2_recomp_csp->mc_messages.get_message(&out_type, &out_msg))
				{
					log(out_msg);
				}

				log(csp_exception.m_error_message, SSC_ERROR, -1.0);
				throw exec_error("sco2_csp_system", csp_exception.m_error_message);
			}

			std::clock_t clock_end = std::clock();

			double od_opt_duration = (clock_end - clock_start)/(double) CLOCKS_PER_SEC;		//[s]

			p_od_code[n_run] = (ssc_number_t)off_design_code;
			if(off_design_code == 0 || (is_P_mc_in_od_sweep_assigned && p_sco2_recomp_csp->get_od_solved()->m_is_converged))
			{	// Off-design call was successful, so write outputs
					// Control parameters
				p_P_comp_in_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_IN] / 1000.0);	//[MPa]
				for (int i_s = 0; i_s < n_mc_stages; i_s++)
					pm_mc_phi_od[n_run*n_mc_stages + i_s] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.mv_phi[i_s];	//[-]
				p_recomp_frac_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_recomp_frac;		//[-]
					// Optimizer parameters
				p_sim_time_od[n_run] = (ssc_number_t)od_opt_duration;		//[s]
					// System
				p_eta_thermal_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_eta_thermal;		//[-]
				p_T_mc_in_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::MC_IN] - 273.15;	//[C]
				p_P_mc_out_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_sco2_cycle_core::MC_OUT] / 1.E3);	//[MPa]
				p_T_htf_cold_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_phx_od_solved.m_T_h_out - 273.15);		//[C]
				p_m_dot_co2_full_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_t;		//[kg/s]
				p_W_dot_net_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_W_dot_net / 1.E3);	//[MWe]
				p_Q_dot_od[n_run] = p_W_dot_net_od[n_run] / p_eta_thermal_od[n_run];		//[MWt]
					// Compressor
				p_mc_W_dot_od[n_run] = (ssc_number_t)(-p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.m_W_dot_in*1.E-3);	//[MWe] convert from kWe
				p_mc_m_dot_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_mc);			//[kg/s]
				p_mc_N_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.m_N;		//[rpm]
				p_mc_eta_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.m_eta;	//[-]
				for (int i_s = 0; i_s < n_mc_stages; i_s++)
				{
					pm_mc_tip_ratio_od[n_run*n_mc_stages + i_s] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.mv_tip_speed_ratio[i_s];	//[-]
					pm_mc_eta_stages_od[n_run*n_mc_stages + i_s] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_mc_ms_od_solved.mv_eta[i_s];	//[-]
				}
				p_mc_f_bypass_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_mc_f_bypass;		//[-]
					// Recompressor
				if (is_rc)
				{
					p_rc_T_in_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_T_in - 273.15);	//[C]
					p_rc_W_dot_od[n_run] = (ssc_number_t)(-p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_W_dot_in*1.E-3);	//[MWe] convert from kWe
					p_rc_m_dot_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_rc);				//[kg/s]
					p_rc_eta_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_eta;		//[-]
					for (int i_s = 0; i_s < n_rc_stages; i_s++)
						pm_rc_phi_od[n_run*n_rc_stages + i_s] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.mv_phi[i_s];	//[-]
					p_rc_N_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.m_N;			//[rpm]
					for (int i_s = 0; i_s < n_rc_stages; i_s++)
					{
						pm_rc_tip_ratio_od[n_run*n_rc_stages + i_s] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.mv_tip_speed_ratio[i_s];	//[-]
						pm_rc_eta_stages_od[n_run*n_rc_stages + i_s] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_rc_ms_od_solved.mv_eta[i_s];	//[-]
					}
				}
				else
				{
					p_rc_T_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_rc_W_dot_od[n_run] = 0.0;
					p_rc_m_dot_od[n_run] = 0.0;
					p_rc_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					for (int i_s = 0; i_s < n_rc_stages; i_s++)
						pm_rc_phi_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					p_rc_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();		
					for (int i_s = 0; i_s < n_rc_stages; i_s++)
					{
						pm_rc_tip_ratio_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
						pm_rc_eta_stages_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					}
				}
					// Precompressor
				if (sco2_rc_des_par.m_cycle_config == 2)
				{
					p_pc_T_in_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_T_in - 273.15);		//[C]
					p_pc_P_in_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_P_in*1.E-3);		//[MPa]
					p_pc_W_dot_od[n_run] = (ssc_number_t)(-p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_W_dot_in*1.E-3);	//[MWe] convert from kWe
					p_pc_m_dot_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_pc);		//[kg/s]
					p_pc_eta_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_eta;		//[-]
					for (int i_s = 0; i_s < n_pc_stages; i_s++)
						pm_pc_phi_od[n_run*n_pc_stages + i_s] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.mv_phi[i_s];
					p_pc_N_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.m_N;			//[rpm]
					for (int i_s = 0; i_s < n_pc_stages; i_s++)
					{
						pm_pc_tip_ratio_od[n_run*n_pc_stages + i_s] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.mv_tip_speed_ratio[i_s];	//[-]
						pm_pc_eta_stages_od[n_run*n_pc_stages + i_s] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_pc_ms_od_solved.mv_eta[i_s];				//[-]
					}
					p_pc_f_bypass_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_pc_f_bypass;
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
					// Turbine
				p_t_W_dot_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_W_dot_out*1.E-3);	//[MWe] convert from kWe
				p_t_m_dot_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_t);			//[kg/s]
				p_t_nu_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_nu;		//[-]
				p_t_N_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_N;		//[rpm]
				p_t_tip_ratio_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_w_tip_ratio;	//[-]
				p_t_eta_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_eta;	//[-]
					// Recuperator
				p_eff_LTR_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_LT_recup_od_solved.m_eff;	//[-]
				p_q_dot_LTR_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_LT_recup_od_solved.m_q_dot*1.E-3);	//[MWt] convert from kWt
				p_eff_HTR_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_HT_recup_od_solved.m_eff;	//[-]
				p_q_dot_HTR_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.ms_HT_recup_od_solved.m_q_dot*1.E-3);	//[MWt] convert from kWt
					// PHX
				p_T_co2_PHX_in_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::HTR_HP_OUT] - 273.15);	//[C]
				p_T_co2_PHX_out_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::TURB_IN] - 273.15);		//[C]
				p_deltaT_HTF_PHX_od[n_run] = p_T_htf_hot_od[n_run] - p_T_htf_cold_od[n_run];	//[C]
				p_phx_eff_od[n_run] = (ssc_number_t)p_sco2_recomp_csp->get_od_solved()->ms_phx_od_solved.m_eff;		//[-]
					// Cooler
				p_T_cooler_in_od[n_run] = (ssc_number_t)(p_sco2_recomp_csp->get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_sco2_cycle_core::LTR_LP_OUT] - 273.15);		//[C]
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
				p_mc_W_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_m_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_mc_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				for (int i_s = 0; i_s < n_mc_stages; i_s++)
				{
					pm_mc_tip_ratio_od[n_run*n_mc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					pm_mc_eta_stages_od[n_run*n_mc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				}
				p_mc_f_bypass_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Recompressor
				p_rc_T_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_W_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_m_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_rc_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				for(int i_s = 0; i_s < n_rc_stages; i_s++)
					pm_rc_phi_od[n_run*n_rc_stages + i_s] = std::numeric_limits<ssc_number_t>::quiet_NaN();
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
					// Turbine
				p_t_W_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_m_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_nu_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_N_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_tip_ratio_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_t_eta_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Recuperator
				p_eff_LTR_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_q_dot_LTR_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_eff_HTR_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_q_dot_HTR_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// PHX
				p_T_co2_PHX_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_T_co2_PHX_out_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_deltaT_HTF_PHX_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
				p_phx_eff_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
					// Cooler
				p_T_cooler_in_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
			}


		}


		// If all calls were successful, log to SSC any messages from sco2_recomp_csp
		while (p_sco2_recomp_csp->mc_messages.get_message(&out_type, &out_msg))
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
		p_mc_W_dot_od = allocate("mc_W_dot_od", n_od_runs);
		p_mc_m_dot_od = allocate("mc_m_dot_od", n_od_runs);
		p_mc_N_od = allocate("mc_N_od", n_od_runs);
		p_mc_eta_od = allocate("mc_eta_od", n_od_runs);
		pm_mc_tip_ratio_od = allocate("mc_tip_ratio_od", n_od_runs, n_mc_stages);
		pm_mc_eta_stages_od = allocate("mc_eta_stages_od", n_od_runs, n_mc_stages);
		p_mc_f_bypass_od = allocate("mc_f_bypass_od", n_od_runs);
		// Recompressor
		p_rc_T_in_od = allocate("rc_T_in_od", n_od_runs);
		p_rc_W_dot_od = allocate("rc_W_dot_od", n_od_runs);
		p_rc_m_dot_od = allocate("rc_m_dot_od", n_od_runs);
		p_rc_eta_od = allocate("rc_eta_od", n_od_runs);
		pm_rc_phi_od = allocate("rc_phi_od", n_od_runs, n_rc_stages);
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
		// Turbine
		p_t_W_dot_od = allocate("t_W_dot_od", n_od_runs);
		p_t_m_dot_od = allocate("t_m_dot_od", n_od_runs);
		p_t_nu_od = allocate("t_nu_od", n_od_runs);
		p_t_N_od = allocate("t_N_od", n_od_runs);
		p_t_tip_ratio_od = allocate("t_tip_ratio_od", n_od_runs);
		p_t_eta_od = allocate("t_eta_od", n_od_runs);
		// Recuperator
		p_eff_LTR_od = allocate("eff_LTR_od", n_od_runs);
		p_q_dot_LTR_od = allocate("q_dot_LTR_od", n_od_runs);
		p_eff_HTR_od = allocate("eff_HTR_od", n_od_runs);
		p_q_dot_HTR_od = allocate("q_dot_HTR_od", n_od_runs);
		// PHX
		p_T_co2_PHX_in_od = allocate("T_co2_PHX_in_od", n_od_runs);
		p_T_co2_PHX_out_od = allocate("T_co2_PHX_out_od", n_od_runs);
		p_deltaT_HTF_PHX_od = allocate("deltaT_HTF_PHX_od", n_od_runs);
		p_phx_eff_od = allocate("phx_eff_od", n_od_runs);
		// Cooler
		p_T_cooler_in_od = allocate("T_cooler_in_od", n_od_runs);
		// Solver Metrics
		p_od_code = allocate("od_code", n_od_runs);

		return;
	}

};

DEFINE_MODULE_ENTRY(sco2_csp_system, "...", 0)
