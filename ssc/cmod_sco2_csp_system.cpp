#include "core.h"

#include <ctime>

#include "sco2_pc_csp_int.h"

static var_info _cm_vtab_sco2_csp_system[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                                    UNITS     META  GROUP REQUIRED_IF CONSTRAINTS     UI_HINTS*/
	// ** Design Parameters **
		// System Design
	{ SSC_INPUT,  SSC_NUMBER,  "htf",                  "Integer code for HTF used in PHX",                       "",           "",    "",      "*",     "",       "" },
    { SSC_INPUT,  SSC_MATRIX,  "htf_props",            "User defined HTF property data",                         "", "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "", "*", "", "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_htf_hot_des",        "HTF design hot temperature (PHX inlet)",                 "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "dT_PHX_hot_approach",  "Temp diff btw hot HTF and turbine inlet",                "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_amb_des",            "Ambient temperature",                                    "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "dT_mc_approach",       "Temp diff btw ambient air and main compressor inlet",    "C",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "site_elevation",       "Site elevation",                                         "m",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "W_dot_net_des",        "Design cycle power output (no cooling parasitics)",      "MWe",        "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_thermal_des",      "Power cycle thermal efficiency",                         "",           "",    "",      "*",     "",       "" },
		// Cycle Design
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_mc",          "Design main compressor isentropic efficiency",           "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_rc",          "Design re-compressor isentropic efficiency",             "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_t",           "Design turbine isentropic efficiency",                   "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "P_high_limit",         "High pressure limit in cycle",                           "MPa",        "",    "",      "*",     "",       "" },
		// PHX Design
	{ SSC_INPUT,  SSC_NUMBER,  "dT_PHX_cold_approach", "Temp diff btw cold HTF and cold CO2",                    "C",          "",    "",      "*",     "",       "" },
		// Air Cooler Design
	{ SSC_INPUT,  SSC_NUMBER,  "fan_power_frac",       "Fraction of net cycle power consumed by air cooler fan", "",           "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "deltaP_cooler_frac",   "Fraction of cycle high pressure that is design point cooler CO2 pressure drop", "", "", "", "*","",       "" },
	// ** Off-design Inputs **
	{ SSC_INPUT,  SSC_MATRIX,  "od_cases",             "Columns: T_htf_C, m_dot_htf_ND, T_amb_C, od_opt_obj (1: MAX_ETA, 2: MAX_POWER), Rows: cases",   "",           "",    "",      "",      "",       "" },

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
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_in",            "Compressor inlet pressure",                              "MPa",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_out",           "Compressor outlet pressure",                             "MPa",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_phi_des",           "Compressor design flow coefficient",					 "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_tip_ratio_des",     "Compressor design tip speed ratio",                      "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_n_stages",          "Compressor stages",                                      "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_N_des",             "Compressor design shaft speed",                          "rpm",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_D",                 "Compressor diameter",                                    "m",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_phi_surge",         "Compressor flow coefficient where surge occurs",         "",           "",    "",      "*",     "",       "" },
		// Recompressor
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_phi_des",           "Recompressor design flow coefficient",                   "",           "",    "",      "*",     "",       "" },					
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_tip_ratio1_des",    "Recompressor 1st stage design tip speed ratio",          "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_tip_ratio2_des",    "Recompressor 2nd stage design tip speed ratio",          "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_n_stages",          "Recompressor stages",                                    "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_N_des",             "Recompressor design shaft speed",                        "rpm",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_D1",                "Recompressor first stage diameter",                      "m",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_D2",                "Recompressor second stage diameter",                     "m",          "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_phi_surge",         "Compressor flow coefficient where surge occurs",         "",           "",    "",      "*",     "",       "" },
		// Turbine
	{ SSC_OUTPUT, SSC_NUMBER,  "t_nu_des",             "Turbine design velocity ratio",                          "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_tip_ratio_des",	   "Turbine design tip speed ratio",                         "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_N_des",			   "Turbine design shaft speed",	                         "rpm",        "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_D",                  "Turbine diameter",                                       "m",          "",    "",      "*",     "",       "" },
		// Recuperators																				 
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_recup_total",       "Total recuperator UA",                                   "kW/K",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_LTR",               "Low temp recuperator UA",                                "kW/K",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_HTR",               "High temp recuperator UA",                               "kW/K",       "",    "",      "*",     "",       "" },
		// PHX Design Solution
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_PHX",               "PHX Conductance",                                        "kW/K",       "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eff_PHX",              "PHX effectiveness",                                      "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "NTU_PHX",              "PHX NTU",                                                "",           "",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_co2_PHX_in",         "CO2 temperature at PHX inlet",                           "C",          "",    "",      "*",     "",       "" },	
	
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
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_phi_od",            "Main compressor flow coefficient",                       "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "recomp_frac_od",       "Recompression fractions",                                "",           "",    "",      "",     "",       "" },
		// Optimizer outputs
	{ SSC_OUTPUT, SSC_ARRAY,   "sim_time_od",          "Simulation time for off design optimization",            "s",          "",    "",      "",     "",       "" },
		// System solution
	{ SSC_OUTPUT, SSC_ARRAY,   "eta_thermal_od",       "Off-design cycle thermal efficiency",                    "",           "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "P_mc_out",             "Off-design high side pressure",                          "MPa",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_htf_cold_od",        "Off-design cold return temperature",                     "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "m_dot_co2_full_od",    "Off-design mass flow rate through turbine",              "kg/s",       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "W_dot_net_od",         "Off-design cycle net output (no cooling pars)",          "MWe",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "Q_dot_od",             "Off-design thermal input",                               "MWt",        "",    "",      "",     "",       "" },
		// Compressor
	{ SSC_OUTPUT, SSC_ARRAY,   "N_mc_od",              "Off-design main compressor speed",                       "rpm",        "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_tip_ratio_od",      "Off-design main compressor tip speed ratio",             "-",          "",    "",      "",     "",       "" },
		// Recompressor
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_phi_1_od",          "Off-design recompressor 1st stage flow coefficient",     "-",		   "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_phi_2_od",          "Off-design recompressor 2nd stage flow coefficient",     "-",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_N_od",              "Off-design recompressor shaft speed",                    "rpm",		   "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_tip_ratio_od",      "Off-design recompressor tip speed ratio",                "-",		   "",    "",      "",     "",       "" },
		// Turbine																											   
	{ SSC_OUTPUT, SSC_ARRAY,   "t_nu_od",              "Off-design turbine velocity ratio",	                     "-",	       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_N_od",               "Off-design turbine shaft speed",	                     "rpm",	       "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "t_tip_ratio_od",       "Off-design turbine tip speed ratio",                     "-",          "",    "",      "",     "",       "" },
		// PHX 
	{ SSC_OUTPUT, SSC_ARRAY,   "T_co2_PHX_in_od",      "Off-design PHX co2 inlet temperature",                   "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "T_co2_PHX_out_od",     "Off-design PHX co2 outlet temperature",                  "C",          "",    "",      "",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "phx_eff_od",           "Off-design PHX effectiveness",                           "-",          "",    "",      "",     "",       "" },

	var_info_invalid };

int test_mono_function(double x, double *y);

class cm_sco2_csp_system : public compute_module
{
public:

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
		sco2_rc_des_par.m_eta_thermal = as_double("eta_thermal_des");				//[-] Cycle thermal efficiency
			
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
		sco2_rc_des_par.m_eta_mc = as_double("eta_isen_mc");
		sco2_rc_des_par.m_eta_rc = as_double("eta_isen_rc");
		sco2_rc_des_par.m_eta_t = as_double("eta_isen_t");
		sco2_rc_des_par.m_P_high_limit = as_double("P_high_limit")*1000.0;		//[kPa], convert from MPa		
			
			// PHX design parameters
		sco2_rc_des_par.m_phx_dt_cold_approach = as_double("dT_PHX_cold_approach");
			
			// Air cooler parameters
		sco2_rc_des_par.m_frac_fan_power = as_double("fan_power_frac");
		sco2_rc_des_par.m_deltaP_cooler_frac = as_double("deltaP_cooler_frac");

		// For try/catch below
		int out_type = -1;
		std::string out_msg = "";

		// Construction class and design system 
		C_sco2_recomp_csp sco2_recomp_csp;
		try
		{
			sco2_recomp_csp.design(sco2_rc_des_par);
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( sco2_recomp_csp.mc_messages.get_message(&out_type, &out_msg))
			{
				log(out_msg);
			}

			log(csp_exception.m_error_message, SSC_ERROR, -1.0);

			return;
		}

		// Set SSC design outputs
		// System
		double m_dot_htf_design = sco2_recomp_csp.get_phx_des_par()->m_m_dot_hot_des;	//[kg/s]
		double T_htf_cold_calc = sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_T_h_out;		//[K]
		assign("T_htf_cold_des", T_htf_cold_calc - 273.15);		//[C] convert from K
		assign("m_dot_htf_des", m_dot_htf_design);				//[kg/s]
		assign("eta_thermal_calc", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_eta_thermal);	//[-]
		assign("m_dot_co2_full", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_m_dot_t);		//[kg/s]
		assign("recomp_frac", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_recomp_frac);		//[-]
		// Compressor
		assign("P_comp_in", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_pres[1 - 1] / 1000.0);		//[MPa] convert from kPa
		assign("P_comp_out", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_pres[2 - 1] / 1000.0);		//[MPa] convert from kPa
		assign("mc_phi_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_phi_des);
		assign("mc_tip_ratio_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_w_tip_ratio);		//[-]
		assign("mc_n_stages", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_n_stages);	//[-]
		assign("mc_N_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_N_design);	//[rpm]
		assign("mc_D", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_D_rotor);			//[m]
		assign("mc_phi_surge", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_phi_surge);	//[-]
		// Recompressor
		assign("rc_phi_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_phi_des);	//[-]
		assign("rc_tip_ratio1_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_w_tip_ratio_1);	//[-]
		assign("rc_tip_ratio2_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_w_tip_ratio_2);	//[-]
		assign("rc_n_stages", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_n_stages);	//[-]
		assign("rc_N_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_N_design);	//[rpm]
		assign("rc_D1", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_D_rotor);		//[m] 
		assign("rc_D2", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_D_rotor_2);		//[m]
		assign("rc_phi_surge", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_rc_des_solved.m_phi_surge);//[-]
		// Turbine
		assign("t_nu_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_nu_design);           //[-]
		assign("t_tip_ratio_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_w_tip_ratio);  //[-]
		assign("t_N_des", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_N_design);			   //[rpm]
		assign("t_D", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_D_rotor);                  //[m]
		// Recuperator
		double UA_LTR = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_UA_LT;		//[kW/K]
		double UA_HTR = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_UA_HT;		//[kW/K]
		assign("UA_recup_total", UA_LTR + UA_HTR);		//[kW/K]
		assign("UA_LTR", UA_LTR);						//[kW/K]
		assign("UA_HTR", UA_HTR);						//[kW/K]
			// PHX
		assign("UA_PHX",sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_UA_design_total);			//[kW/K]
		assign("eff_PHX",sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_eff_design);				//[-]
		assign("NTU_PHX",sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_NTU_design);				//[-]
		assign("T_co2_PHX_in", sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_temp[C_RecompCycle::HTR_HP_OUT] - 273.15);	//[C]


		// Check that off-design model matches design-point with same operation inputs
		C_sco2_recomp_csp::S_od_par sco2_rc_od_par;
		double T_htf_hot_in_des = sco2_recomp_csp.get_design_par()->m_T_htf_hot_in;		//[K]
		double m_dot_htf_des = sco2_recomp_csp.get_phx_des_par()->m_m_dot_hot_des;		//[kg/s]
		double T_amb_des = sco2_recomp_csp.get_design_par()->m_T_amb_des;				//[K]
		
		double m_dot_htf_ND = 0.5;
		double T_htf_hot_in_offset = 0;
		sco2_rc_od_par.m_T_htf_hot = T_htf_hot_in_des - T_htf_hot_in_offset;	//[K]	
		sco2_rc_od_par.m_m_dot_htf = m_dot_htf_ND*m_dot_htf_des;				//[kg/s]
		sco2_rc_od_par.m_T_amb = 273;										//[K]
			
		
		C_sco2_recomp_csp::S_od_operation_inputs sco2_rc_od_op_par;
			// Set-up one-off off-design operation inputs
		// ***************************************************************************
		sco2_rc_od_op_par.m_P_mc_in = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_pres[C_RecompCycle::MC_IN];		//[kPa]
		if( sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_is_rc )
		{	// Recompression Fraction
			sco2_rc_od_op_par.m_recomp_frac = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_recomp_frac;	//[-]
		}
		sco2_rc_od_op_par.m_phi_mc = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_phi_des;	//[-]
			// Call off_design
		int sco2_od_code = sco2_recomp_csp.off_design(sco2_rc_od_par, sco2_rc_od_op_par);
		// ***************************************************************************
		// ***************************************************************************




		// Set up parametric sweep of operation inputs
		//int n_P_mc_in = 5;
		//std::vector<double> v_P_mc_in(n_P_mc_in);	//[kPa]
		//v_P_mc_in[0] = sco2_rc_od_op_par.m_P_mc_in - 1000.0;	//[kPa]
		//v_P_mc_in[1] = sco2_rc_od_op_par.m_P_mc_in - 500.0;	//[kPa]
		//v_P_mc_in[2] = sco2_rc_od_op_par.m_P_mc_in;				//[kPa]
		//v_P_mc_in[3] = sco2_rc_od_op_par.m_P_mc_in + 500.0;	//[kPa]
		//v_P_mc_in[4] = sco2_rc_od_op_par.m_P_mc_in + 1000.0;	//[kPa]

		//int n_f_recomp = 5;
		//std::vector<double> v_f_recomp(n_f_recomp);	//[-]
		//v_f_recomp[0] = sco2_rc_od_op_par.m_recomp_frac - 0.05;	//[-]
		//v_f_recomp[1] = sco2_rc_od_op_par.m_recomp_frac - 0.025;	//[-]
		//v_f_recomp[2] = sco2_rc_od_op_par.m_recomp_frac;		//[-]
		//v_f_recomp[3] = sco2_rc_od_op_par.m_recomp_frac + 0.025;	//[-]
		//v_f_recomp[4] = sco2_rc_od_op_par.m_recomp_frac + 0.05;	//[-]

		//int n_phi_mc = 5;
		//std::vector<double> v_phi_mc(n_phi_mc);	//[-]
		//double phi_min = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_phi_surge;
		//double phi_max = sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.ms_mc_des_solved.m_phi_max;
		//v_phi_mc[0] = sco2_rc_od_op_par.m_phi_mc*0.8 + phi_min*0.8;
		//v_phi_mc[1] = sco2_rc_od_op_par.m_phi_mc*0.9 + phi_min*0.1;
		//v_phi_mc[2] = sco2_rc_od_op_par.m_phi_mc;
		//v_phi_mc[3] = sco2_rc_od_op_par.m_phi_mc*0.9 + phi_max*0.1;
		//v_phi_mc[4] = sco2_rc_od_op_par.m_phi_mc*0.8 * phi_max*0.2;
		//
		//for(int i = 0; i < n_P_mc_in; i++)
		//{
		//	for(int j = 0; j < n_f_recomp; j++)
		//	{
		//		for(int k = 0; k < n_phi_mc; k++)
		//		{
		//			sco2_rc_od_op_par.m_P_mc_in = v_P_mc_in[i];			//[kPa]
		//			sco2_rc_od_op_par.m_recomp_frac = v_f_recomp[j];	//[-]
		//			sco2_rc_od_op_par.m_phi_mc = v_phi_mc[k];			//[-]

		//			int od_err_code = sco2_recomp_csp.off_design(sco2_rc_od_par, sco2_rc_od_op_par);

		//			double blah = 1.23;
		//		}
		//	}		 
		//}

		
		// Set up off-design analysis
		util::matrix_t<double> od_cases = as_matrix("od_cases");

		// Check if off cases exist and correctly formatted
		int n_od_cols = od_cases.ncols();
		int n_od_runs = od_cases.nrows();

		if( n_od_cols != 5 && n_od_runs == 1 )
		{
			// No off-design cases specified
			log("No off-design cases specified");
			return;
		}
		if( n_od_cols != 5 && n_od_runs > 1 )
		{
			std::string err_msg = util::format("The matrix of off design cases requires 3 columns. The entered matrix has %d columns", n_od_cols);
			log(err_msg);
			return;
		}

			// Off-design parameters
		ssc_number_t *p_m_dot_htf_fracs = allocate("m_dot_htf_fracs", n_od_runs);
		ssc_number_t *p_T_amb_od = allocate("T_amb_od", n_od_runs);
		ssc_number_t *p_T_htf_hot_od = allocate("T_htf_hot_od", n_od_runs);
			// Optimized control parameters
		ssc_number_t *p_od_opt_obj_code = allocate("od_opt_obj_code", n_od_runs);
		ssc_number_t *p_od_opt_conv_tol = allocate("od_opt_conv_tol", n_od_runs);
		ssc_number_t *p_P_comp_in_od = allocate("P_comp_in_od", n_od_runs);
		ssc_number_t *p_mc_phi_od = allocate("mc_phi_od", n_od_runs);
		ssc_number_t *p_recomp_frac_od = allocate("recomp_frac_od", n_od_runs);	
			// Optimizer parameters
		ssc_number_t *p_sim_time_od = allocate("sim_time_od", n_od_runs);
			// Systems
		ssc_number_t *p_eta_thermal_od = allocate("eta_thermal_od", n_od_runs);
		ssc_number_t *p_P_mc_out = allocate("P_mc_out", n_od_runs);
		ssc_number_t *p_T_htf_cold_od = allocate("T_htf_cold_od", n_od_runs);
		ssc_number_t *p_m_dot_co2_full_od = allocate("m_dot_co2_full_od", n_od_runs);
		ssc_number_t *p_W_dot_net_od = allocate("W_dot_net_od", n_od_runs);
		ssc_number_t *p_Q_dot_od = allocate("Q_dot_od", n_od_runs);
			// Compressor
		ssc_number_t *p_N_mc_od = allocate("N_mc_od", n_od_runs);
		ssc_number_t *p_mc_tip_ratio_od = allocate("mc_tip_ratio_od", n_od_runs);
			// Recompressor
		ssc_number_t *p_rc_phi_1_od = allocate("rc_phi_1_od", n_od_runs);
		ssc_number_t *p_rc_phi_2_od = allocate("rc_phi_2_od", n_od_runs);
		ssc_number_t *p_rc_N_od = allocate("rc_N_od", n_od_runs);
		ssc_number_t *p_rc_tip_ratio_od = allocate("rc_tip_ratio_od", n_od_runs);
			// Turbine
		ssc_number_t *p_t_nu_od = allocate("t_nu_od", n_od_runs);
		ssc_number_t *p_t_N_od = allocate("t_N_od", n_od_runs);
		ssc_number_t *p_t_tip_ratio_od = allocate("t_tip_ratio_od", n_od_runs);
			// PHX
		ssc_number_t *p_T_co2_PHX_in_od = allocate("T_co2_PHX_in_od", n_od_runs);
		ssc_number_t *p_T_co2_PHX_out_od = allocate("T_co2_PHX_out_od", n_od_runs);
		ssc_number_t *p_phx_eff_od = allocate("phx_eff_od", n_od_runs);

		for(int n_run = 0; n_run < n_od_runs; n_run++)
		{

			
			// Try calling off-design model with design parameters
				// Set outputs
		 	p_T_htf_hot_od[n_run] = od_cases(n_run, 0);			//[C]
			p_m_dot_htf_fracs[n_run] = od_cases(n_run, 1);		//[-]
			p_T_amb_od[n_run] = od_cases(n_run, 2);				//[C]
			p_od_opt_obj_code[n_run] = od_cases(n_run, 3);		//[-]
			p_od_opt_conv_tol[n_run] = od_cases(n_run, 4);		//[-]
				// Set input structure
			sco2_rc_od_par.m_T_htf_hot = p_T_htf_hot_od[n_run] + 273.15;	//[K]
			sco2_rc_od_par.m_m_dot_htf = m_dot_htf_design*p_m_dot_htf_fracs[n_run];	//[kg/s]
			sco2_rc_od_par.m_T_amb = p_T_amb_od[n_run] + 273.15;				//[K]
			int od_strategy = (int)p_od_opt_obj_code[n_run];		//[-]
			double od_opt_tol = p_od_opt_conv_tol[n_run];			//[-]

			int off_design_code = 0;
			std::clock_t clock_start = std::clock();
			try
			{
				off_design_code = sco2_recomp_csp.off_design_opt(sco2_rc_od_par, od_strategy, od_opt_tol);
			}
			catch( C_csp_exception &csp_exception )
			{
				// Report warning before exiting with error
				while( sco2_recomp_csp.mc_messages.get_message(&out_type, &out_msg) )
				{
					log(out_msg);
				}

				log(csp_exception.m_error_message, SSC_ERROR, -1.0);

				return;
			}
			std::clock_t clock_end = std::clock();

			double od_opt_duration = (clock_end - clock_start)/(double) CLOCKS_PER_SEC;		//[s]

			if(off_design_code == 0)
			{	// Off-design call was successful, so write outputs
					// Control parameters
				p_P_comp_in_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_pres[1-1]/1000.0;	//[MPa]
				p_mc_phi_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_od_solved.m_phi;	//[-]
				p_recomp_frac_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_recomp_frac;		//[-]
					// Optimizer parameters
				p_sim_time_od[n_run] = od_opt_duration;		//[s]
					// System
				p_eta_thermal_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_eta_thermal;		//[-]
				p_P_mc_out[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_pres[C_RecompCycle::MC_OUT]/1.E3;	//[MPa]
				p_T_htf_cold_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_phx_od_solved.m_T_h_out - 273.15;		//[C]
				p_m_dot_co2_full_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_m_dot_t;		//[kg/s]
				p_W_dot_net_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_W_dot_net/1.E3;	//[MWe]
				p_Q_dot_od[n_run] = p_W_dot_net_od[n_run] / p_eta_thermal_od[n_run];		//[MWt]
					// Compressor
				p_N_mc_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_od_solved.m_N;		//[rpm]
				p_mc_tip_ratio_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_mc_od_solved.m_w_tip_ratio;	//[-]
					// Recompressor
				p_rc_phi_1_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_od_solved.m_phi;	//[-]
				p_rc_phi_2_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_od_solved.m_phi_2;	//[-]
				p_rc_N_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_od_solved.m_N;			//[rpm]
				p_rc_tip_ratio_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_rc_od_solved.m_w_tip_ratio;	//[-]
					// Turbine
				p_t_nu_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_nu;		//[-]
				p_t_N_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_N;		//[rpm]
				p_t_tip_ratio_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.ms_t_od_solved.m_w_tip_ratio;	//[-]
					// PHX
				p_T_co2_PHX_in_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_RecompCycle::HTR_HP_OUT] - 273.15;	//[C]
				p_T_co2_PHX_out_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_rc_cycle_od_solved.m_temp[C_RecompCycle::TURB_IN] - 273.15;		//[C]
				p_phx_eff_od[n_run] = sco2_recomp_csp.get_od_solved()->ms_phx_od_solved.m_eff;		//[-]
			}
			else
			{	// Off-design call failed, write NaN outptus
					// Control parameters
				p_P_comp_in_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_mc_phi_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_recomp_frac_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// System
				p_eta_thermal_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_P_mc_out[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_T_htf_cold_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_m_dot_co2_full_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_W_dot_net_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// Compressor
				p_N_mc_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_mc_tip_ratio_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// Recompressor
				p_rc_phi_1_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_rc_phi_2_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_rc_N_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_rc_tip_ratio_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// Turbine
				p_t_nu_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_t_N_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_t_tip_ratio_od[n_run] = std::numeric_limits<double>::quiet_NaN();
					// PHX
				p_T_co2_PHX_in_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_T_co2_PHX_out_od[n_run] = std::numeric_limits<double>::quiet_NaN();
				p_phx_eff_od[n_run] = std::numeric_limits<double>::quiet_NaN();
			}


		}


		// If all calls were successful, log to SSC any messages from sco2_recomp_csp
		while( sco2_recomp_csp.mc_messages.get_message(&out_type, &out_msg) )
		{
			log(out_msg);
		}
		
	}

};

DEFINE_MODULE_ENTRY(sco2_csp_system, "...", 0)
