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

#include "sco2_pc_csp_int.h"

static var_info _cm_vtab_sco2_csp_ud_pc_tables[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                                    UNITS     META  GROUP REQUIRED_IF CONSTRAINTS     UI_HINTS*/
	// ** Design Parameters **
		// System Design
	{ SSC_INPUT,  SSC_NUMBER,  "htf",                  "Integer code for HTF used in PHX",                       "",           "",    "",      "*",     "",       "" },
    { SSC_INPUT,  SSC_MATRIX,  "htf_props",            "User defined HTF property data",                         "", "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "", "?=[[0]]", "", "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_htf_hot_des",        "HTF design hot temperature (PHX inlet)",                 "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "dT_PHX_hot_approach",  "Temp diff btw hot HTF and turbine inlet",                "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_amb_des",            "Ambient temperature",                                    "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "dT_mc_approach",       "Temp diff btw ambient air and main compressor inlet",    "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "site_elevation",       "Site elevation",                                         "m",          "",    "",      "?=300.0","",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "W_dot_net_des",        "Design cycle power output (no cooling parasitics)",      "MWe",        "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "design_method",        "1 = Specify efficiency, 2 = Specify total recup UA",     "",           "",    "",      "?=1",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_thermal_des",      "Power cycle thermal efficiency",                         "",           "",    "",      "?=-1.0","",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "UA_recup_tot_des",     "Total recuperator conductance",                          "kW/K",       "",    "",      "?=-1.0","",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_recomp_ok",         "1 = Yes, 0 = simple cycle only",                         "",           "",    "",      "?=1",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_PR_fixed",          "0 = No, >0 = fixed pressure ratio",                      "",           "",    "",      "?=0",   "",       "" },
		// Cycle Design
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_mc",          "Design main compressor isentropic efficiency",           "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_rc",          "Design re-compressor isentropic efficiency",             "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_t",           "Design turbine isentropic efficiency",                   "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "LT_recup_eff_max",     "Maximum allowable effectiveness in LT recuperator",      "-",          "",    "",      "?=1.0", "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "HT_recup_eff_max",     "Maximum allowable effectiveness in HT recuperator",      "-",          "",    "",      "?=1.0", "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "P_high_limit",         "High pressure limit in cycle",                           "MPa",        "",    "",      "*",     "",       "" },
		// PHX Design
	{ SSC_INPUT,  SSC_NUMBER,  "dT_PHX_cold_approach", "Temp diff btw cold HTF and cold CO2",                    "C",          "",    "",      "?=-1.0","",       "" },
		// Air Cooler Design
	{ SSC_INPUT,  SSC_NUMBER,  "fan_power_frac",       "Fraction of net cycle power consumed by air cooler fan", "",           "",    "",      "?=0.01",       "", "" },
	{ SSC_INPUT,  SSC_NUMBER,  "deltaP_cooler_frac",   "Fraction of cycle high pressure that is design point cooler CO2 pressure drop", "", "", "", "?=0.002", "", "" },
		// Off Design UDPC Options
	{ SSC_INPUT,  SSC_NUMBER,  "is_generate_udpc",     "1 = generate udpc tables, 0 = only calculate design point cyle", "",   "",    "",      "?=1",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_apply_default_htf_mins", "1 = yes (0.5 rc, 0.7 simple), 0 = no, only use 'm_dot_htf_ND_low'", "", "", "",   "?=1",   "",       "" },
	// User Defined Power Cycle Table Inputs
	{ SSC_INOUT,  SSC_NUMBER,  "T_htf_hot_low",        "Lower level of HTF hot temperature",					  "C",         "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "T_htf_hot_high",	   "Upper level of HTF hot temperature",					  "C",		   "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "n_T_htf_hot",		   "Number of HTF hot temperature parametric runs",			  "",		   "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "T_amb_low",			   "Lower level of ambient temperature",					  "C",		   "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "T_amb_high",		   "Upper level of ambient temperature",					  "C",		   "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "n_T_amb",			   "Number of ambient temperature parametric runs",			  "",		   "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "m_dot_htf_ND_low",	   "Lower level of normalized HTF mass flow rate",			  "",		   "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "m_dot_htf_ND_high",	   "Upper level of normalized HTF mass flow rate",			  "",		   "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "n_m_dot_htf_ND",	   "Number of normalized HTF mass flow rate parametric runs", "",		   "",    "",      "",     "",       "" },

	// ** Design OUTPUTS **
		// System Design Solution
	{ SSC_OUTPUT, SSC_NUMBER,  "T_htf_cold_des",       "HTF design cold temperature (PHX outlet)",               "C",          "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "m_dot_htf_des",        "HTF mass flow rate",                                     "kg/s",       "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eta_thermal_calc",     "Calculated cycle thermal efficiency",                    "-",          "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "m_dot_co2_full",       "CO2 mass flow rate through HTR, PHX, turbine",           "kg/s",       "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "recomp_frac",          "Recompression fraction",                                 "-",          "",    "",      "?=1.2345",     "",       "" },
		// Compressor																															
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_in",            "Compressor inlet pressure",                              "MPa",        "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_out",           "Compressor outlet pressure",                             "MPa",        "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_phi_des",           "Compressor design flow coefficient",					 "",           "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_tip_ratio_des",     "Compressor design tip speed ratio",                      "",           "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_n_stages",          "Compressor stages",                                      "",           "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_N_des",             "Compressor design shaft speed",                          "rpm",        "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_D",                 "Compressor diameter",                                    "m",          "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_phi_surge",         "Compressor flow coefficient where surge occurs",         "",           "",    "",      "?=1.2345",     "",       "" },
		// Recompressor																															
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_phi_des",           "Recompressor design flow coefficient",                   "",           "",    "",      "?=1.2345",     "",       "" },					
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_tip_ratio_des",     "Recompressor 1st stage design tip speed ratio",          "",           "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_n_stages",          "Recompressor stages",                                    "",           "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_N_des",             "Recompressor design shaft speed",                        "rpm",        "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_D",                 "Recompressor first stage diameter",                      "m",          "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_phi_surge",         "Compressor flow coefficient where surge occurs",         "",           "",    "",      "?=1.2345",     "",       "" },
		// Turbine																																
	{ SSC_OUTPUT, SSC_NUMBER,  "t_nu_des",             "Turbine design velocity ratio",                          "",           "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_tip_ratio_des",	   "Turbine design tip speed ratio",                         "",           "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_N_des",			   "Turbine design shaft speed",	                         "rpm",        "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_D",                  "Turbine diameter",                                       "m",          "",    "",      "?=1.2345",     "",       "" },
		// Recuperators																				 											
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_recup_total",       "Total recuperator UA",                                   "kW/K",       "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_LTR",               "Low temp recuperator UA",                                "kW/K",       "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eff_LTR",              "Low temp recuperator effectiveness",                     "",           "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "NTU_LTR",              "Low temp recuperator NTU",                               "",           "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_HTR",               "High temp recuperator UA",                               "kW/K",       "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eff_HTR",              "High temp recuperator effectiveness",                    "",           "",    "",      "?=1.2345",     "",       "" },	
	{ SSC_OUTPUT, SSC_NUMBER,  "NTU_HTR",              "High temp recuperator NTRU",                             "",           "",    "",      "?=1.2345",     "",       "" },
		// PHX Design Solution																													
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_PHX",               "PHX Conductance",                                        "kW/K",       "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eff_PHX",              "PHX effectiveness",                                      "",           "",    "",      "?=1.2345",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "NTU_PHX",              "PHX NTU",                                                "",           "",    "",      "?=1.2345",     "",       "" },
		// Air Cooler Design
	// ?????
		// State Points
	{ SSC_OUTPUT, SSC_ARRAY,  "T_co2_des",             "Array of cycle CO2 state point temps",	                 "C",          "",    "",      "?=[1.2,2.3,3,4]",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "P_co2_des",             "Array of cycle CO2 state point pressures",               "MPa",        "",    "",      "?=[1.2,2.3,3,4]",     "",       "" },

	// Power Cycle Tables
	{ SSC_OUTPUT, SSC_MATRIX,  "T_htf_ind",            "Parametric of HTF temperature w/ ND HTF mass flow rate levels",     "",       "",    "",      "?=[[0,1,2,3,4,5,6,7,8,9,10,11,12][0,1,2,3,4,5,6,7,8,9,10,11,12]]",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "T_amb_ind",            "Parametric of ambient temp w/ HTF temp levels",                     "",       "",    "",      "?=[[0,1,2,3,4,5,6,7,8,9,10,11,12][0,1,2,3,4,5,6,7,8,9,10,11,12]]",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "m_dot_htf_ND_ind",     "Parametric of ND HTF mass flow rate w/ ambient temp levels",        "",       "",    "",      "?=[[0,1,2,3,4,5,6,7,8,9,10,11,12][0,1,2,3,4,5,6,7,8,9,10,11,12]]",     "",       "" },

	var_info_invalid };

int test_mono_function(double x, double *y);

class cm_sco2_csp_ud_pc_tables : public compute_module
{
public:

	cm_sco2_csp_ud_pc_tables()
	{
		add_var_info(_cm_vtab_sco2_csp_ud_pc_tables);
	}

	void exec() throw(general_error)
	{
		C_sco2_rc_csp_template::S_des_par sco2_rc_des_par;
			// System design parameters
		sco2_rc_des_par.m_hot_fl_code = as_integer("htf");							//[-] Integer code for HTF
		sco2_rc_des_par.mc_hot_fl_props = as_matrix("htf_props");					//[-] Custom HTF properties
		sco2_rc_des_par.m_T_htf_hot_in = as_double("T_htf_hot_des")+273.15;			//[K] Convert from C
		sco2_rc_des_par.m_phx_dt_hot_approach = as_double("dT_PHX_hot_approach");	//[K/C] Temperature difference between hot HTF and turbine CO2 inlet
		sco2_rc_des_par.m_T_amb_des = as_double("T_amb_des")+273.15;				//[K] Convert from C
		sco2_rc_des_par.m_dt_mc_approach = as_double("dT_mc_approach");				//[K/C] Temperature difference between ambient air and main compressor inlet
		sco2_rc_des_par.m_elevation = as_double("site_elevation");					//[m] Site elevation
		sco2_rc_des_par.m_W_dot_net = as_double("W_dot_net_des")*1000.0;			//[kWe] Convert from MWe, cycle power output w/o cooling parasitics
		sco2_rc_des_par.m_eta_thermal = as_double("eta_thermal_des");				//[-] Cycle thermal efficiency
			
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
		sco2_rc_des_par.m_N_turbine = 3600.0;
		sco2_rc_des_par.m_tol = 1.E-3;
		sco2_rc_des_par.m_opt_tol = 1.E-3;
		
			// Remaining cycle design parameters
		sco2_rc_des_par.m_LT_eff_max = as_double("LT_recup_eff_max");
		sco2_rc_des_par.m_HT_eff_max = as_double("HT_recup_eff_max");
		sco2_rc_des_par.m_eta_mc = as_double("eta_isen_mc");
		sco2_rc_des_par.m_eta_rc = as_double("eta_isen_rc");
		sco2_rc_des_par.m_eta_t = as_double("eta_isen_t");
		sco2_rc_des_par.m_P_high_limit = as_double("P_high_limit")*1000.0;		//[kPa], convert from MPa		
			
			// PHX design parameters
		sco2_rc_des_par.m_phx_dt_cold_approach = as_double("dT_PHX_cold_approach");
		if (sco2_rc_des_par.m_phx_dt_cold_approach < 0.0)
		{
			sco2_rc_des_par.m_phx_dt_cold_approach = sco2_rc_des_par.m_phx_dt_hot_approach;
		}
			
			// Air cooler parameters
		sco2_rc_des_par.m_frac_fan_power = as_double("fan_power_frac");
		sco2_rc_des_par.m_deltaP_cooler_frac = as_double("deltaP_cooler_frac");

		// For try/catch below
		int out_type = -1;
		std::string out_msg = "";

		// Construction class and design system
		C_sco2_rc_csp_template *p_sco2_recomp_csp;

		C_sco2_recomp_csp sco2_recomp_csp_direct;
		C_sco2_recomp_csp_10MWe_scale sco2_recomp_csp_scale;

		if (false)
		{
			p_sco2_recomp_csp = &sco2_recomp_csp_direct;
		}
		else
		{
			p_sco2_recomp_csp = &sco2_recomp_csp_scale;
		}

		// Pass through callback function (with update percent) and pointer
		p_sco2_recomp_csp->mf_callback_update = ssc_cmod_update;
		p_sco2_recomp_csp->mp_mf_update = (void*)(this);

		try
		{
			p_sco2_recomp_csp->design(sco2_rc_des_par);
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( p_sco2_recomp_csp->mc_messages.get_message(&out_type, &out_msg))
			{
				log(out_msg);
			}

			throw exec_error("sco2_csp_system", csp_exception.m_error_message);
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
		assign("P_comp_in", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_pres[1 - 1] / 1000.0));		//[MPa] convert from kPa
		assign("P_comp_out", (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_pres[2 - 1] / 1000.0));		//[MPa] convert from kPa
		assign("mc_phi_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_phi_des);
		assign("mc_tip_ratio_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_tip_ratio_max);		//[-]
		int n_mc_stages = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_n_stages; 
		assign("mc_n_stages", (ssc_number_t)n_mc_stages);	//[-]
		assign("mc_N_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_N_design);	//[rpm]
		
		ssc_number_t *p_mc_D = allocate("mc_D", n_mc_stages);
		std::vector<double> v_mc_D = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.mv_D;
		for (int i = 0; i < n_mc_stages; i++)
		{
			p_mc_D[i] = (ssc_number_t)v_mc_D[i];		//[m]
		}

		assign("mc_phi_surge", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_phi_surge);	//[-]
			// Recompressor
		assign("rc_phi_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_phi_des);	//[-]
		assign("rc_tip_ratio_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_tip_ratio_max);	//[-]
		int n_rc_stages = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_n_stages;		//[-]
		assign("rc_n_stages", (ssc_number_t)n_rc_stages);	//[-]
		assign("rc_N_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_N_design);	//[rpm]
		
		ssc_number_t *p_rc_D = allocate("rc_D", n_rc_stages);
		std::vector<double> v_rc_D = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.mv_D;
		for (int i = 0; i < n_rc_stages; i++)
		{
			p_rc_D[i] = (ssc_number_t)v_rc_D[i];		//[m]
		}
		
		assign("rc_phi_surge", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_phi_surge);//[-]
			// Turbine
		assign("t_nu_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_nu_design);           //[-]
		assign("t_tip_ratio_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_w_tip_ratio);  //[-]
		assign("t_N_des", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_N_design);			   //[rpm]
		assign("t_D", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_D_rotor);                  //[m]
			// Recuperator
		double UA_LTR = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_UA_LTR;		//[kW/K]
		double UA_HTR = p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_UA_HTR;		//[kW/K]
		assign("UA_recup_total", (ssc_number_t)(UA_LTR + UA_HTR));		//[kW/K]
		assign("UA_LTR", (ssc_number_t)UA_LTR);						//[kW/K]
		assign("eff_LTR", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_eff_design);		//[-]
		assign("NTU_LTR", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_NTU_design);		//[-]
		assign("UA_HTR", (ssc_number_t)UA_HTR);						//[kW/K]
		assign("eff_HTR", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_eff_design);		//[-]
		assign("NTU_HTR", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_NTU_design);		//[-]
			// PHX
		assign("UA_PHX", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_phx_des_solved.m_UA_design_total);			//[kW/K]
		assign("eff_PHX", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_phx_des_solved.m_eff_design);				//[-]
		assign("NTU_PHX", (ssc_number_t)p_sco2_recomp_csp->get_design_solved()->ms_phx_des_solved.m_NTU_design);				//[-]
			// Air Cooler

			// State Points
		int n_sp = C_sco2_cycle_core::END_SCO2_STATES;		// C_RecompCycle::RC_OUT + 1;
		ssc_number_t *p_T_co2_des = allocate("T_co2_des", n_sp);
		ssc_number_t *p_P_co2_des = allocate("P_co2_des", n_sp);
		for(int i = 0; i < n_sp; i++)
		{
			p_T_co2_des[i] = (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_temp[i] - 273.15);		//[C]
			p_P_co2_des[i] = (ssc_number_t)(p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_pres[i] / 1.E3);		//[MPa]
		}

		double sco2_f_min = 0.5;
		if (!p_sco2_recomp_csp->get_design_solved()->ms_rc_cycle_solved.m_is_rc)
			sco2_f_min = 0.7;

		double m_dot_htf_ND_low = sco2_f_min;;
		if (is_assigned("m_dot_htf_ND_low"))
		{
			if (as_boolean("is_apply_default_htf_mins"))
				m_dot_htf_ND_low = std::max(sco2_f_min, as_double("m_dot_htf_ND_low"));	//[-]
			else
				m_dot_htf_ND_low = as_double("m_dot_htf_ND_low");
		}

		assign("m_dot_htf_ND_low", m_dot_htf_ND_low);

		if (as_integer("is_generate_udpc") == 0)
		{
			log("\n Design calculations complete; no off-design cases requested");
			return;
		}

		// Get or calculate user-defined power cycle parameters
		double T_htf_hot_low = sco2_rc_des_par.m_T_htf_hot_in - 273.15 - 20.0;		//[C]
		if (is_assigned("T_htf_hot_low"))
		{
			T_htf_hot_low = as_double("T_htf_hot_low");		//[C]
		}
		assign("T_htf_hot_low", T_htf_hot_low);

		double T_htf_hot_high = sco2_rc_des_par.m_T_htf_hot_in - 273.15 + 15.0;	//[C]
		if (is_assigned("T_htf_hot_high"))
		{
			T_htf_hot_high = as_double("T_htf_hot_high");	//[C]
		}
		assign("T_htf_hot_high", T_htf_hot_high);

		int n_T_htf_hot_in = 5;
		if (is_assigned("n_T_htf_hot"))
		{
			n_T_htf_hot_in = as_integer("n_T_htf_hot");			//[-]
		}
		assign("n_T_htf_hot", n_T_htf_hot_in);

		double T_amb_low = 0.0;
		if (is_assigned("T_amb_low"))
		{
			T_amb_low = as_double("T_amb_low");				//[C]
		}
		assign("T_amb_low", T_amb_low);

		double T_amb_high = std::max(45.0, sco2_rc_des_par.m_T_amb_des-273.15 + 5.0);
		if (is_assigned("T_amb_high"))
		{
			T_amb_high = as_double("T_amb_high");			//[C]
		}
		assign("T_amb_high", T_amb_high);

		int n_T_amb_in = 10;
		if (is_assigned("n_T_amb"))
		{
			n_T_amb_in = as_integer("n_T_amb");					//[-]
		}		
		assign("n_T_amb", n_T_amb_in);

		double m_dot_htf_ND_high = 1.05;
		if (is_assigned("m_dot_htf_ND_high"))
		{
			m_dot_htf_ND_high = as_double("m_dot_htf_ND_high");
		}
		assign("m_dot_htf_ND_high", m_dot_htf_ND_high);

		int n_m_dot_htf_ND_in = 10;
		if (is_assigned("n_m_dot_htf_ND"))
		{
			n_m_dot_htf_ND_in = as_integer("n_m_dot_htf_ND");
		}
		assign("n_m_dot_htf_ND", n_m_dot_htf_ND_in);

		if (n_T_htf_hot_in < 3 || n_T_amb_in < 3 || n_m_dot_htf_ND_in < 3)
		{
			throw exec_error("sco2_csp_ud_pc_tables", "Need at 3 three points for each independent variable");
		}

		util::matrix_t<double> T_htf_parametrics, T_amb_parametrics, m_dot_htf_ND_parametrics;

		try
		{
			p_sco2_recomp_csp->generate_ud_pc_tables(T_htf_hot_low, T_htf_hot_high, n_T_htf_hot_in,
							T_amb_low, T_amb_high, n_T_amb_in,
							m_dot_htf_ND_low, m_dot_htf_ND_high, n_m_dot_htf_ND_in,
							T_htf_parametrics, T_amb_parametrics, m_dot_htf_ND_parametrics);
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while (p_sco2_recomp_csp->mc_messages.get_message(&out_type, &out_msg))
			{
				log(out_msg);
			}

			throw exec_error("sco2_csp_system", csp_exception.m_error_message);
		}

		int n_T_htf_hot = (int)T_htf_parametrics.nrows();
		int n_T_amb = (int)T_amb_parametrics.nrows();
		int n_m_dot_htf_ND = (int)m_dot_htf_ND_parametrics.nrows();

		int ncols = (int)T_htf_parametrics.ncols();

		ssc_number_t *p_T_htf_ind = allocate("T_htf_ind", n_T_htf_hot, ncols);
		for(int i = 0; i < n_T_htf_hot; i++)
		{
			for(int j = 0; j < ncols; j++)
			{
				p_T_htf_ind[i*ncols + j] = (ssc_number_t)T_htf_parametrics(i, j);
			}
		}

		ssc_number_t *p_T_amb_ind = allocate("T_amb_ind", n_T_amb, ncols);
		for(int i = 0; i < n_T_amb; i++)
		{
			for(int j = 0; j < ncols; j++)
			{
				p_T_amb_ind[i*ncols + j] = (ssc_number_t)T_amb_parametrics(i, j);
			}
		}

		ssc_number_t *p_m_dot_htf_ND_ind = allocate("m_dot_htf_ND_ind", n_m_dot_htf_ND, ncols);
		for(int i = 0; i < n_m_dot_htf_ND; i++)
		{
			for(int j = 0; j < ncols; j++)
			{
				p_m_dot_htf_ND_ind[i*ncols + j] = (ssc_number_t)m_dot_htf_ND_parametrics(i, j);
			}
		}

		// If all calls were successful, log to SSC any messages from sco2_recomp_csp
		while (p_sco2_recomp_csp->mc_messages.get_message(&out_type, &out_msg))
		{
			log(out_msg);
		}
		
		log("\n UDPC tables complete");
	}

};

DEFINE_MODULE_ENTRY(sco2_csp_ud_pc_tables, "...", 0)
