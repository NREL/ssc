#include "core.h"
#include "sco2_pc_core.h"

// This compute module finds the optimal cycle design that meets the user-input design point cycle efficiency
//    and calculates the required recuperator UA

static var_info _cm_vtab_sco2_design_point[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                          UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
	{ SSC_INPUT,  SSC_NUMBER,  "W_dot_net_des",   "Design cycle power output",                      "MW",         "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_c",           "Design compressor(s) isentropic efficiency",     "-",          "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_t",           "Design turbine isentropic efficiency",           "-",          "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "P_high_limit",    "High pressure limit in cycle",                   "MPa",        "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "deltaT_PHX",      "Temp diff btw hot HTF and turbine inlet",        "C",          "",    "",      "*",     "",                "" },

	{ SSC_INPUT,  SSC_NUMBER,  "deltaT_ACC",      "Temp diff btw ambient air and compressor inlet", "C",          "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_amb_des",       "Design: Ambient temperature for air cooler",     "C",          "",    "",      "*",     "",                "" },

	{ SSC_INPUT,  SSC_NUMBER,  "T_htf_hot_des",   "Tower design outlet temp",                       "C",          "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_des",         "Power cycle thermal efficiency",                 "",           "",    "",      "*",     "",                "" },

	{ SSC_OUTPUT, SSC_NUMBER,  "eta_thermal_calc","Calculated cycle thermal efficiency",            "-",          "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_total",        "Total recuperator UA",                           "kW/K",       "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "recomp_frac",     "Recompression fraction",                         "-",          "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_in",       "Compressor inlet pressure",                      "MPa",        "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_out",      "Compressor outlet pressure",                     "MPa",        "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_htf_cold",      "Calculated cold HTF temp",                       "C",          "",    "",      "*",     "",                "" },

	var_info_invalid };

class cm_sco2_design_point : public compute_module
{
public:

	cm_sco2_design_point()
	{
		add_var_info(_cm_vtab_sco2_design_point);
	}

	void exec() throw(general_error)
	{
		// Get user-defined parameters
		double W_dot_net_des = as_double("W_dot_net_des")*1.E3;
		double eta_c = as_double("eta_c");
		double eta_t = as_double("eta_t");
		double P_high_limit = as_double("P_high_limit")*1.E3;		//[kPa], convert from MPa
		double delta_T_t = as_double("deltaT_PHX");

		double delta_T_acc = as_double("deltaT_ACC");
		double T_amb_cycle_des = as_double("T_amb_des")+273.15;

		double T_htf_hot = as_double("T_htf_hot_des")+273.15;
		double eta_thermal_des = as_double("eta_des");

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
		int N_sub_hxrs = 10;
		double N_t_des = 3600.0;
		double tol = 1.E-3;
		double opt_tol = 1.E-3;

		// Define error and warning message strings
		std::string error_msg;
		error_msg[0] = NULL;
		int error_code = 0;

		C_RecompCycle::S_auto_opt_design_hit_eta_parameters rc_params;
		rc_params.m_W_dot_net = W_dot_net_des;					//[kW]
		rc_params.m_eta_thermal = eta_thermal_des;
		rc_params.m_T_mc_in = T_amb_cycle_des + delta_T_acc;	//[K]
		rc_params.m_T_t_in = T_htf_hot - delta_T_t;				//[K]
		rc_params.m_DP_LT = DP_LT;
		rc_params.m_DP_HT = DP_HT;
		rc_params.m_DP_PC = DP_PC;
		rc_params.m_DP_PHX = DP_PHX;
		rc_params.m_eta_mc = eta_c;
		rc_params.m_eta_rc = eta_c;
		rc_params.m_eta_t = eta_t;
		rc_params.m_N_sub_hxrs = N_sub_hxrs;
		rc_params.m_P_high_limit = P_high_limit;
		rc_params.m_tol = tol;
		rc_params.m_opt_tol = opt_tol;
		rc_params.m_N_turbine = N_t_des;
		
		C_RecompCycle rc_cycle;

		rc_cycle.auto_opt_design_hit_eta(rc_params, error_code, error_msg);

		if(error_code != 0)
		{
			throw exec_error("sco2 design point calcs", error_msg);
		}

		double eta_thermal_calc = rc_cycle.get_design_solved()->m_eta_thermal;
		double UA_total = rc_cycle.get_design_solved()->m_UA_HT + rc_cycle.get_design_solved()->m_UA_LT;
		double recomp_frac = rc_cycle.get_design_solved()->m_recomp_frac;
		double P_comp_in = rc_cycle.get_design_solved()->m_pres[0] / 1.E3;
		double P_comp_out = rc_cycle.get_design_solved()->m_pres[1] / 1.E3;
		double T_htf_cold = rc_cycle.get_design_solved()->m_temp[5 - 1] + delta_T_t - 273.15;	//[C]

		// Assign SSC outputs
		assign("eta_thermal_calc", eta_thermal_calc);
		assign("UA_total", UA_total);
		assign("recomp_frac", recomp_frac);
		assign("P_comp_in", P_comp_in);
		assign("P_comp_out", P_comp_out);
		assign("T_htf_cold", T_htf_cold); 

		if( error_msg[0] == NULL )
			log("Design point optimization was successful!");
		else
		{
			log("The sCO2 design point optimization solved with the following warning(s):\n" + error_msg);
		}

	}


};

DEFINE_MODULE_ENTRY(sco2_design_point, "Returns optimized sco2 cycle parameters given inputs", 0)
