#include "core.h"
#include "sco2_pc_core.h"

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

	//{ SSC_OUTPUT, SSC_STRING,  "warning_messages","Warning messages from optimization",             "-",          "",    "",      "*",      "",               "" },

	var_info_invalid };

class cm_sco2_design_point : public compute_module
{
public:

	cm_sco2_design_point()
	{
		add_var_info(_cm_vtab_sco2_design_point);
	}

	bool sco2_opt_design_target_eta(double W_dot_net_des /*kW*/, double eta_c, double eta_t, double P_high_limit /*MPa*/, double delta_T_t /*C/K*/, double delta_T_acc /*C/K*/, double T_amb_cycle_des /*K*/,
		                            double T_htf_hot /*K*/, double eta_thermal_des, 
									vector<double> DP_LT /*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/,
									vector<double> DP_HT /*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/,
									vector<double> DP_PC /*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/,
									vector<double> DP_PHX /*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/,
									int N_sub_hxrs,
									double N_t_des /*[rpm] Turbine shaft speed(negative values link turbine to compressor)*/,
									double opt_tol, double tol,

									std::string *error_msg, std::string *warning_msg,
									
									C_RecompCycle::S_design_solved & opt_des_target_eta_pars)
	{

		double Q_dot_rec_des = W_dot_net_des / eta_thermal_des;		//[kWt] Receiver thermal input at design

		// Calculate other cycle design parameters based on User Parameters
		double T_mc_in_des = T_amb_cycle_des + delta_T_acc;	//[K] Compressor inlet temperature
		double T_t_in_des = T_htf_hot - delta_T_t;			//[K] Turbine inlet temperature			

		// Check cycle parameter values are reasonable
			// Can't operate compressor in 2-phase region
		if( T_mc_in_des <= N_co2_props::T_crit )
		{
			char tstr[300];
			sprintf(tstr, "Only single phase cycle operation is allowed in this model. The compressor inlet temperature (%lg [C]) must be great than the critical temperature: %lg [C]",
				T_mc_in_des - 273.15, ((N_co2_props::T_crit) - 273.15));

			error_msg->append(tstr);

			return false;
		}
			// "Reasonable" ceiling on compressor inlet temp
		double T_mc_in_max = 70.0 + 273.15;			//[K] Arbitrary value for max compressor inlet temperature
		if( T_mc_in_des > T_mc_in_max )
		{
			char tstr[300];
			sprintf(tstr, "The compressor inlet temperature input was %lg [C]. This value was reset internally to the max allowable inlet temperature: %lg [C]\n", 
				T_mc_in_des - 273.15, T_mc_in_max - 273.15);

			warning_msg->append(tstr);

			T_mc_in_des = T_mc_in_max;
		}
			// "Reasonable" floor on turbine inlet temp
		double T_t_in_min = 300.0 + 273.15;			//[K] Arbitrary value for min turbine inlet temperature
		if( T_t_in_des < T_t_in_min )
		{
			char tstr[300];
			sprintf(tstr, "The turbine inlet temperature input was %lg [C]. This value was reset internally to the min allowable inlet temperature: %lg [C]\n", 
				T_t_in_des - 273.15, T_t_in_min - 273.15);
			
			warning_msg->append(tstr);
			
			T_t_in_des = T_t_in_min;
		}
			// Turbine inlet temperature must be hotter than compressor outlet temperature
		if( T_t_in_des <= T_mc_in_des )
		{
			char tstr[300];
			sprintf(tstr, "The turbine inlet temperature, %lg [C], is colder than the specified compressor inlet temperature %lg [C]", 
				T_t_in_des - 273.15, T_mc_in_des - 273.15);
			
			error_msg->append(tstr);
			
			return false;
		}
			// Turbine inlet temperature must be colder than property limits
		if( T_t_in_des >= N_co2_props::T_upper_limit )
		{
			char tstr[300];
			sprintf(tstr, "The turbine inlet temperature, %lg [C], is hotter than the maximum allow temperature in the CO2 property code %lg [C]", 
				T_t_in_des - 273.15, N_co2_props::T_upper_limit - 273.15);
			
			error_msg->append(tstr);

			return false;
		}
			// Check for realistic isentropic efficiencies
		if( eta_c > 1.0 )
		{
			char tstr[300];
			sprintf(tstr, "The compressor isentropic efficiency, %lg, was reset to theoretical maximum 1.0\n", eta_c);

			warning_msg->append(tstr);

			eta_c = 1.0;
		}
		if( eta_t > 1.0 )
		{
			char tstr[300];
			sprintf(tstr, "The turbine isentropic efficiency, %lg, was reset to theoretical maximum 1.0\n", 
				eta_t);

			warning_msg->append(tstr);

			eta_t = 1.0;
		}
		if( eta_c < 0.1 )
		{
			char tstr[300];
			sprintf(tstr, "The compressor isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability\n", eta_c);
			
			warning_msg->append(tstr);
			
			eta_c = 0.1;
		}
		if( eta_t < 0.1 )
		{
			char tstr[300];
			sprintf(tstr, "The turbine isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability\n", eta_t);
			
			warning_msg->append(tstr);
			
			eta_t = 0.1;
		}
			// Limits on high pressure limit
		if( P_high_limit >= N_co2_props::P_upper_limit / 1.E3 )
		{
			char tstr[300];
			sprintf(tstr, "The upper pressure limit, %lg [MPa], was set to the internal limit in the CO2 properties code %lg [MPa]\n", 
				P_high_limit, N_co2_props::P_upper_limit / 1.E3);
			
			warning_msg->append(tstr);
			
			P_high_limit = N_co2_props::P_upper_limit / 1.E3;
		}
		double P_high_limit_min = 10.0;
		if( P_high_limit <= P_high_limit_min )
		{
			char tstr[300];
			sprintf(tstr, "The upper pressure limit, %lg [MPa], must be greater than %lg [MPa] to ensure solution stability", 
				P_high_limit, P_high_limit_min);
			
			error_msg->append(tstr);

			return false;
		}
			// Finally, check thermal efficiency
		if( eta_thermal_des <= 0.0 )
		{
			char tstr[300];
			sprintf(tstr, "The design cycle thermal efficiency, %lg, must be at least greater than 0 ", eta_thermal_des);
			
			error_msg->append(tstr);
			
			return false;
		}
		double eta_carnot = 1.0 - T_amb_cycle_des / T_htf_hot;
		if( eta_thermal_des >= eta_carnot )
		{
			char tstr[300];
			sprintf(tstr, "To solve the cycle within the allowable recuperator conductance, the design cycle thermal efficiency, %lg, must be at least less than the Carnot efficiency: %lg ", 
				eta_thermal_des, eta_carnot);
			
			error_msg->append(tstr);
			
			return false;
		}
		// ******************************************************************************************
		// ******************************************************************************************
		// Setup recompressoin cycle autodes parameter structure
		C_RecompCycle::S_auto_opt_design_parameters ms_rc_autodes_par;
		ms_rc_autodes_par.m_DP_HT = DP_HT;
		ms_rc_autodes_par.m_DP_LT = DP_LT;
		ms_rc_autodes_par.m_DP_PC = DP_PC;
		ms_rc_autodes_par.m_DP_PHX = DP_PHX;

		ms_rc_autodes_par.m_eta_mc = eta_c;
		ms_rc_autodes_par.m_eta_rc = eta_c;
		ms_rc_autodes_par.m_eta_t = eta_t;

		ms_rc_autodes_par.m_N_sub_hxrs = N_sub_hxrs;
		ms_rc_autodes_par.m_N_turbine = N_t_des;

		ms_rc_autodes_par.m_opt_tol = opt_tol;
		ms_rc_autodes_par.m_P_high_limit = P_high_limit*1000.0;		// Convert to kPa
		ms_rc_autodes_par.m_tol = tol;
		ms_rc_autodes_par.m_T_mc_in = T_mc_in_des;
		ms_rc_autodes_par.m_T_t_in = T_t_in_des;
		ms_rc_autodes_par.m_W_dot_net = W_dot_net_des;
		// ************************************************************************************
		// ************************************************************************************

		double UA_net_power_ratio_max = 2.0;
		double UA_net_power_ratio_min = 1.E-5;

		double UA_recups_guess = 0.1*W_dot_net_des;		// Somewhat based on plots from Dyreby?
		
		// **********************************************************************
		// **********************************************************************
		// Solve design point model with guessed recups UA
		double UA_total_des = UA_recups_guess;
		ms_rc_autodes_par.m_UA_rec_total = UA_total_des;

		C_RecompCycle ms_rc_cycle;
		int auto_opt_error_code = 0;
		ms_rc_cycle.auto_opt_design(ms_rc_autodes_par, auto_opt_error_code);
		if( auto_opt_error_code != 0 )
		{
			char tstr[300];
			sprintf(tstr, "Can't optimize sCO2 power cycle with current inputs");
			
			error_msg->append(tstr);
			
			return false;
		}

		double eta_calc = ms_rc_cycle.get_design_solved()->m_eta_thermal;
		// **********************************************************************

		// Now need to iterate UA_total_des until eta_thermal_calc = eta_thermal_in
		double diff_eta = (eta_calc - eta_thermal_des);
		bool low_flag = false;
		bool high_flag = false;
		double y_upper = numeric_limits<double>::quiet_NaN();
		double y_lower = numeric_limits<double>::quiet_NaN();
		double x_upper = numeric_limits<double>::quiet_NaN();
		double x_lower = numeric_limits<double>::quiet_NaN();
		double UA_net_power_ratio = numeric_limits<double>::quiet_NaN();

		int opt_des_calls = 1;

		while( abs(diff_eta) > tol )
		{
			opt_des_calls++;

			if( diff_eta > 0.0 )		// Calc > target, UA is too large, decrease UA
			{
				low_flag = true;
				x_lower = UA_recups_guess;
				y_lower = diff_eta;

				if( high_flag )	// Upper and lower bounds set, use false positon interpolation method
				{
					UA_recups_guess = -y_upper*(x_lower - x_upper) / (y_lower - y_upper) + x_upper;
				}
				else			// No upper bound set, try to get there
				{
					if( opt_des_calls > 5 )
						UA_recups_guess = UA_net_power_ratio_min*W_dot_net_des;
					else
						UA_recups_guess *= 0.5;
				}

				if( x_lower / W_dot_net_des <= UA_net_power_ratio_min )
				{
					char tstr[300];
					sprintf(tstr, "The design thermal efficiency, %lg [-], is too small to achieve with the available cycle model and inputs\n", eta_thermal_des);
					error_msg->append(tstr);
					sprintf(tstr, "The lowest possible thermal efficiency for these inputs is roughly %lg [-]", ms_rc_cycle.get_design_solved()->m_eta_thermal);
					error_msg->append(tstr);

					return false;
				}
			}
			else						// Calc < target, UA is too small, decrease UA
			{
				high_flag = true;
				x_upper = UA_recups_guess;
				y_upper = diff_eta;

				if( low_flag )
				{
					UA_recups_guess = -y_upper*(x_lower - x_upper) / (y_lower - y_upper) + x_upper;
				}
				else
				{
					if( opt_des_calls > 5 )
						UA_recups_guess = UA_net_power_ratio_max*W_dot_net_des;
					else
						UA_recups_guess *= 2.5;
				}

				if( x_upper / W_dot_net_des >= UA_net_power_ratio_max )
				{
					char tstr[300];
					sprintf(tstr, "The design thermal efficiency, %lg [-], is too large to achieve with the available cycle model and inputs", eta_thermal_des);
					error_msg->append(tstr);
					sprintf(tstr, "The largest possible thermal efficiency for these inputs is roughly %lg [-] ", ms_rc_cycle.get_design_solved()->m_eta_thermal);
					error_msg->append(tstr);
					return false;
				}
			}

			// Solve design point model with guessed recups UA
			UA_total_des = UA_recups_guess;
			//rc_des_par.m_UA_rec_total = m_UA_total_des;
			ms_rc_autodes_par.m_UA_rec_total = UA_total_des;
			// Create new instance of RecompCycle class and assign to member point
			//rc_cycle->set_design_parameters(rc_des_par);
			//auto_cycle_success = rc_cycle->auto_optimal_design();

			ms_rc_cycle.auto_opt_design(ms_rc_autodes_par, auto_opt_error_code);
			if( auto_opt_error_code != 0 )
			{
				char tstr[300];
				sprintf(tstr, "Can't optimize sCO2 power cycle with current inputs");
				error_msg->append(tstr);
				return false;
			}

			//T_PHX_in_calc = rc_cycle->get_cycle_design_metrics()->m_T[5 - 1];
			//T_PHX_in_calc = ms_rc_cycle.get_design_solved()->m_temp[5 - 1];
			eta_calc = ms_rc_cycle.get_design_solved()->m_eta_thermal;
			// **********************************************************************

			// Now need to iterate UA_total_des until T_PHX_in_calc = m_T_PHX_in
			// diff_T_PHX_in = (T_PHX_in_calc - m_T_PHX_in) / m_T_PHX_in;			//[-]
			diff_eta = (eta_calc - eta_thermal_des);
		}


		opt_des_target_eta_pars = *ms_rc_cycle.get_design_solved();

		return true;
	}

	void exec() throw(general_error)
	{
		// Get user-defined parameters
		double W_dot_net_des = as_double("W_dot_net_des")*1.E3;
		double eta_c = as_double("eta_c");
		double eta_t = as_double("eta_t");
		double P_high_limit = as_double("P_high_limit");
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
		std::string warning_msg;
		warning_msg[0] = NULL;

		// Structure for optimized design parameters
		C_RecompCycle::S_design_solved opt_des_target_eta_pars;

		log("Begin design point optimization...");
		// Call design point optimization routine for a target cycle efficiency
		
		bool opt_sco2_success = sco2_opt_design_target_eta(W_dot_net_des, eta_c, eta_t, P_high_limit, delta_T_t, delta_T_acc, T_amb_cycle_des,
		                            T_htf_hot, eta_thermal_des, 
									DP_LT /*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/,
									DP_HT /*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/,
									DP_PC /*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/,
									DP_PHX /*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/,
									N_sub_hxrs,
									N_t_des /*[rpm] Turbine shaft speed(negative values link turbine to compressor)*/,
									opt_tol, tol,
		
									&error_msg, &warning_msg,
									
									opt_des_target_eta_pars);
									
		if( !opt_sco2_success )		// Optimization failed
		{
			throw exec_error("sco2 design point", error_msg);
		}

		double eta_thermal_calc = opt_des_target_eta_pars.m_eta_thermal;
		double recomp_frac = opt_des_target_eta_pars.m_recomp_frac;
		double UA_total = opt_des_target_eta_pars.m_UA_HT + opt_des_target_eta_pars.m_UA_LT;
		double P_comp_in = opt_des_target_eta_pars.m_pres[0]/1.E3;
		double P_comp_out = opt_des_target_eta_pars.m_pres[1]/1.E3;
		double T_htf_cold = opt_des_target_eta_pars.m_temp[5-1]+delta_T_t-273.15;	//[C]

		// Assign SSC outputs
		assign("eta_thermal_calc", eta_thermal_calc);
		assign("UA_total", UA_total);
		assign("recomp_frac", recomp_frac);
		assign("P_comp_in", P_comp_in);
		assign("P_comp_out", P_comp_out);
		assign("T_htf_cold", T_htf_cold); 

		if( warning_msg[0] == NULL )
			log("Design point optimization was successful!");
		else
		{
			assign("warning_messages", warning_msg);
			log("The sCO2 design point optimization solved with the following warning(s):\n" + warning_msg);
		}

	}


};

DEFINE_MODULE_ENTRY(sco2_design_point, "Returns optimized sco2 cycle parameters given inputs", 0)
