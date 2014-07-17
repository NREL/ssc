#include "core.h"
#include "sco2_power_cycle.h"
#include <vector>

static var_info _cm_vtab_sco2_design_cycle[] = {
/*  VARTYPE   DATATYPE         NAME                  LABEL                                                UNITS     META        GROUP                      REQUIRED_IF          CONSTRAINTS   UI_HINTS*/
{ SSC_INPUT,  SSC_NUMBER,     "I_W_dot_net_des",     "Design cycle power output",                         "MW",     "",         "sCO2 power cycle",         "*",                "",           "" },		
{ SSC_INPUT,  SSC_NUMBER,     "I_T_mc_in_des",       "Main compressor inlet temp at design",              "C",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_T_t_in_des",        "Turbine inlet temp at design",                      "C",      "",         "sCO2 power cycle",         "*",                "",           "" },								
{ SSC_INPUT,  SSC_NUMBER,     "I_N_t_des",           "Design turbine speed, negative links to comp.",     "rpm",    "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_eta_c",             "Design compressor(s) isentropic efficiency",        "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_eta_t",             "Design turbine isentropic efficiency",              "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_tol",               "Convergence tolerance for performance calcs",       "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_opt_tol",           "Convergence tolerance - optimization calcs",        "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_UA_total_des",      "Total UA allocatable to recuperators",              "kW/K",   "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_INPUT,  SSC_NUMBER,     "I_P_high_limit",      "High pressure limit in cycle",                      "MPa",    "",         "sCO2 power cycle",         "*",                "",           "" },

{ SSC_OUTPUT, SSC_NUMBER,     "O_LT_frac_des",       "Optimized design point UA distribution",            "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_P_mc_out_des",      "Optimized design point high side pressure",         "MPa",    "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_PR_mc_des",         "Optimized Pressure Ratio across main compressor",   "",       "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_recomp_frac_des",   "Optimized recompression fraction",                  "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_eta_thermal_des",   "Design cycle thermal efficiency",                   "-",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_DT_LT_min_des",     "Minimum temperature difference in LT recup - des.", "C",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_DT_HT_min_des",     "Minimum temperature difference in HT recup - des.", "C",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_N_mc_des",          "Design point compressor shaft speed",               "rpm",    "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_ARRAY,      "O_T_array_des",       "Cycle temp state points at design",                 "K",      "",         "sCO2 power cycle",         "*",                "",           "" },
{ SSC_OUTPUT, SSC_NUMBER,     "O_m_dot_PHX",         "Mass flow rate through primary HX",                 "kg/s",   "",         "sCO2 power cycle",         "*",                "",           "" },
/* Need to output temp and pres arrays using SSC_ARRAY*/

var_info_invalid };

class cm_sco2_design_cycle : public compute_module
{
public:

	cm_sco2_design_cycle()
	{
		add_var_info(_cm_vtab_sco2_design_cycle);
	}

	void exec() throw(general_error)
	{
		cycle_design_parameters rc_des_par;

		// Only 1 compressor map currently available, so hardcode these
		rc_des_par.m_mc_type = 1;
		rc_des_par.m_rc_type = 1;

		// Read in design parameters
		//rc_des_par.m_W_dot_net = 10.0 * 1000.0;				//[kW]
		//rc_des_par.m_T_mc_in = 32.0 + 273.15;				//[K]
		//rc_des_par.m_T_t_in = 550.0 + 273.15;				//[K]
		rc_des_par.m_W_dot_net = as_double("I_W_dot_net_des")*1000.0;		//[kW] Design cycle power outpt
		rc_des_par.m_T_mc_in = as_double("I_T_mc_in_des") + 273.15;			//[K] Compressor inlet temp at design, convert from C
		rc_des_par.m_T_t_in = as_double("I_T_t_in_des") + 273.15;			//[K] Turbine inlet temp at design, convert from C

		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		// Hardcode for now
		rc_des_par.m_DP_LT[0] = 0.0;
		rc_des_par.m_DP_LT[1] = 0.0;
		rc_des_par.m_DP_HT[0] = 0.0;
		rc_des_par.m_DP_HT[1] = 0.0;
		rc_des_par.m_DP_PC[0] = 0.0;
		rc_des_par.m_DP_PC[1] = 0.0;
		rc_des_par.m_DP_PHX[0] = 0.0;
		rc_des_par.m_DP_PHX[1] = 0.0;

		// Read in turbine speed
		//rc_des_par.m_N_t = -1.0;
		rc_des_par.m_N_t = as_double("I_N_t_des");			//[rpm] Turbine speed, Negative number links to compressor speed
		
		// Read in turbomachinery isentropic efficiencies
		//rc_des_par.m_eta_mc = 0.89;
		//rc_des_par.m_eta_rc = 0.89;
		//rc_des_par.m_eta_t = 0.93;
		double eta_comps = as_double("I_eta_c");
		rc_des_par.m_eta_mc = eta_comps;
		rc_des_par.m_eta_rc = eta_comps;
		rc_des_par.m_eta_t = as_double("I_eta_t");

		// Hardcode for now
		rc_des_par.m_N_sub_hxrs = 20;

		// Read in tolerances
		rc_des_par.m_tol = 1.E-6;						//[-]
		rc_des_par.m_opt_tol = 1.E-6;					//[-]
		rc_des_par.m_tol = as_double("I_tol");			//[-] Convergence tolerance for performance calcs
		rc_des_par.m_opt_tol = as_double("I_opt_tol");	//[-] Convergence tolerance for optimization calcs

		
		// Using auto-optimization, so only need to define total allocatable UA, optimization code will define LT_frac
		//double UA_LT = 500.0;
		//double UA_HT = 500.0;
		//rc_des_par.m_UA_rec_total = UA_LT + UA_HT;
		//rc_des_par.m_LT_frac = UA_LT / rc_des_par.m_UA_rec_total;
		//rc_des_par.m_fixed_LT_frac = true;
		//rc_des_par.m_LT_frac_guess = 0.5;
		rc_des_par.m_UA_rec_total = as_double("I_UA_total_des");		//[kW/K] Total UA allocatable to recuperators

		// Using auto-optimization, so only need to define the high side pressur limit, optimization code will define P_high and P_ratio
		//double P_mc_in = 7.69*1000.0;
		//double P_mc_out = 20.0*1000.0;
		//rc_des_par.m_fixed_P_mc_out = true;
		//rc_des_par.m_P_mc_out = P_mc_out;
		//rc_des_par.m_P_mc_out_guess = rc_des_par.m_P_mc_out;
		//rc_des_par.m_P_high_limit = 25.0*1000.0;
		//rc_des_par.m_fixed_PR_mc = true;
		//rc_des_par.m_PR_mc = P_mc_out / P_mc_in;
		//rc_des_par.m_PR_mc_guess = rc_des_par.m_PR_mc;
		rc_des_par.m_P_high_limit = as_double("I_P_high_limit")*1000.0;

		// Using auto-optimization, optimization code will define recompression fraction
		// rc_des_par.m_fixed_recomp_frac = false;
		// rc_des_par.m_recomp_frac = 0.5;
		// rc_des_par.m_recomp_frac_guess = 0.5;

		RecompCycle rc_cycle(rc_des_par);

		bool auto_cycle_success = rc_cycle.auto_optimal_design();

		double eta = rc_cycle.get_cycle_design_metrics()->m_eta_thermal;
		
		// Example of throwing an error
		//if( eta > 0.4 )
		//	throw general_error("invalid cycle efficiency: too high!");

		assign("O_LT_frac_des", var_data((ssc_number_t)rc_cycle.get_cycle_design_parameters()->m_LT_frac));
		assign("O_P_mc_out_des", var_data((ssc_number_t)rc_cycle.get_cycle_design_parameters()->m_P_mc_out/1000.0));
		assign("O_PR_mc_des", var_data((ssc_number_t)rc_cycle.get_cycle_design_parameters()->m_PR_mc));
		assign("O_recomp_frac_des", var_data((ssc_number_t)rc_cycle.get_cycle_design_parameters()->m_recomp_frac));
		assign("O_eta_thermal_des", var_data((ssc_number_t)rc_cycle.get_cycle_design_metrics()->m_eta_thermal));
		assign("O_DT_LT_min_des", var_data((ssc_number_t)rc_cycle.get_cycle_design_metrics()->m_min_DT_LT));
		assign("O_DT_HT_min_des", var_data((ssc_number_t)rc_cycle.get_cycle_design_metrics()->m_min_DT_HT));
		assign("O_N_mc_des", var_data((ssc_number_t)rc_cycle.get_cycle_design_metrics()->m_N_mc));
		assign("O_m_dot_PHX", var_data((ssc_number_t)rc_cycle.get_cycle_design_metrics()->m_m_dot_PHX));

		// Assign temperatures to array
		const std::vector<double> T_vector = rc_cycle.get_cycle_design_metrics()->m_T;
		int l_T_array = T_vector.size();
		ssc_number_t * T_array = new ssc_number_t[l_T_array];

		for( int i = 0; i < l_T_array; i++ )
			T_array[i] = (ssc_number_t)(T_vector[i]);

		// Assign pressure to array
		int l_P_array = rc_cycle.get_cycle_design_metrics()->m_P.size();
		ssc_number_t * P_array = new ssc_number_t[l_P_array];

		for( int i = 0; i < l_P_array; i++ )
			P_array[i] = (ssc_number_t)(rc_cycle.get_cycle_design_metrics()->m_P[i]);

		// Assign pressure and temp arrays to var_data
		assign("O_T_array_des", var_data(T_array, l_T_array));
		//assign("O_P_array_des", var_data(P_array, l_P_array));

		// Delete dynamically created arrays
		delete [] T_array;
		delete [] P_array;

		//cycle_design_parameters rc_des_par_test;

		//// Only 1 compressor map currently available, so hardcode these
		//rc_des_par_test.m_mc_type = 1;
		//rc_des_par_test.m_rc_type = 1;

		//// Read in design parameters
		//rc_des_par_test.m_W_dot_net = as_double("I_W_dot_net_des")*1000.0;		//[kW] Design cycle power outpt
		//rc_des_par_test.m_T_mc_in = as_double("I_T_mc_in_des") + 273.15;			//[K] Compressor inlet temp at design, convert from C
		//rc_des_par_test.m_T_t_in = as_double("I_T_t_in_des") + 273.15;			//[K] Turbine inlet temp at design, convert from C

		////(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		//// Hardcode for now
		//rc_des_par_test.m_DP_LT[0] = 0.0;
		//rc_des_par_test.m_DP_LT[1] = 0.0;
		//rc_des_par_test.m_DP_HT[0] = 0.0;
		//rc_des_par_test.m_DP_HT[1] = 0.0;
		//rc_des_par_test.m_DP_PC[0] = 0.0;
		//rc_des_par_test.m_DP_PC[1] = 0.0;
		//rc_des_par_test.m_DP_PHX[0] = 0.0;
		//rc_des_par_test.m_DP_PHX[1] = 0.0;

		//// Read in turbine speed
		////rc_des_par.m_N_t = -1.0;
		//rc_des_par_test.m_N_t = as_double("I_N_t_des");			//[rpm] Turbine speed, Negative number links to compressor speed

		//// Hardcode for now
		//rc_des_par_test.m_eta_mc = 0.89;
		//rc_des_par_test.m_eta_rc = 0.89;
		//rc_des_par_test.m_eta_t = 0.9;
		//rc_des_par_test.m_N_sub_hxrs = 20;

		//// Read in tolerances
		//rc_des_par_test.m_tol = 1.E-6;						//[-]
		//rc_des_par_test.m_opt_tol = 1.E-6;					//[-]
		//rc_des_par_test.m_tol = as_double("I_tol");			//[-] Convergence tolerance for performance calcs
		//rc_des_par_test.m_opt_tol = as_double("I_opt_tol");	//[-] Convergence tolerance for optimization calcs


		//// Using auto-optimization, so only need to define total allocatable UA, optimization code will define LT_frac
		////double UA_LT = 500.0;
		////double UA_HT = 500.0;
		////rc_des_par.m_UA_rec_total = UA_LT + UA_HT;
		//rc_des_par_test.m_LT_frac = rc_cycle.get_cycle_design_parameters()->m_LT_frac;
		//rc_des_par_test.m_fixed_LT_frac = true;
		//rc_des_par_test.m_UA_rec_total = as_double("I_UA_total_des");		//[kW/K] Total UA allocatable to recuperators

		//// Using auto-optimization, so only need to define the high side pressur limit, optimization code will define P_high and P_ratio
		////double P_mc_in = 7.69*1000.0;
		////double P_mc_out = 20.0*1000.0;
		//rc_des_par_test.m_fixed_P_mc_out = true;
		//rc_des_par_test.m_P_mc_out = rc_cycle.get_cycle_design_parameters()->m_P_mc_out;
		////rc_des_par.m_P_mc_out_guess = rc_des_par.m_P_mc_out;
		////rc_des_par.m_P_high_limit = 25.0*1000.0;
		//rc_des_par_test.m_fixed_PR_mc = true;
		//rc_des_par_test.m_PR_mc = rc_cycle.get_cycle_design_parameters()->m_PR_mc;
		////rc_des_par.m_PR_mc_guess = rc_des_par.m_PR_mc;
		//rc_des_par_test.m_P_high_limit = as_double("I_P_high_limit")*1000.0;

		//// Using auto-optimization, optimization code will define recompression fraction
		//rc_des_par_test.m_fixed_recomp_frac = true;
		//rc_des_par_test.m_recomp_frac = rc_cycle.get_cycle_design_parameters()->m_recomp_frac;
		//// rc_des_par.m_recomp_frac_guess = 0.5;

		//RecompCycle rc_cycle_test(rc_des_par_test);

		//rc_cycle_test.design_no_opt();

		return;
	}

};

DEFINE_MODULE_ENTRY(sco2_design_cycle, "Calls sCO2 auto-design cycle function", 1)
