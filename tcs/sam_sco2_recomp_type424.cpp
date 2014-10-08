#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include "htf_props.h"
//#include <vector>
#include "CO2_properties.h"
#include "compact_hx_discretized.h"

#include "sco2_pc_core.h"

#include "nlopt.hpp"
#include "nlopt_callbacks.h"

using namespace std;

enum{	//Parameters
	P_W_dot_net_des,
	P_eta_c,
	P_eta_t,
	P_P_high_limit,
	P_DELTAT_PHX,

	P_DELTAT_ACC,
	P_T_AMB_DES,
	P_FAN_POWER_PERC,
	P_PLANT_ELEVATION,

	P_T_htf_hot,
	P_T_htf_cold_est,
	P_eta_des,
	P_rec_fl,
	P_rec_fl_props,

	P_STARTUP_TIME,
	P_STARTUP_FRAC,
	P_Q_SBY_FRAC,
	P_cycle_cutoff_frac,

	//Inputs
	I_T_HTF_HOT,
	I_M_DOT_HTF,
	I_STANDBY_CONTROL,
	I_T_DB,
	I_P_AMB,

	//Outputs
	O_ETA_CYCLE_DES,
	O_P_LOW_DES,
	O_F_RECOMP_DES,
	O_UA_RECUP_DES,
	O_T_COOLER_IN_DES,
	O_COOLER_VOLUME,

	O_P_CYCLE,
	O_ETA,
	O_T_HTF_COLD,
	O_M_DOT_MAKEUP,
	O_M_DOT_DEMAND,
	O_M_DOT_HTF_REF,
	O_W_COOL_PAR,
	O_F_BAYS,
	O_P_COND,

	
	O_W_DOT_NET,
	O_T_MC_IN,
	O_T_T_IN,
	O_P_MC_IN,
	O_P_MC_OUT,
	O_UA_LT,
	O_UA_HT,
	O_RECOMP_FRAC,
	O_ETA_MC,
	O_ETA_RC,
	O_ETA_T,
	O_N_SUB_HXRS,
	O_P_HIGH_LIMIT,
	O_N_turbine,
	O_DP_LT_C,
	O_DP_LT_H,
	O_DP_HT_C,
	O_DP_HT_H,
	O_DP_PC_H,
	O_DP_PHX_C,
	O_DELTAT_MC,
	O_DELTAT_T,

	//N_MAX
	N_MAX
};

tcsvarinfo sam_sco2_recomp_type424_variables[] = {
	//PARAMETERS
		// Cycle Design Parameters
	{ TCS_PARAM, TCS_NUMBER, P_W_dot_net_des,  "W_dot_net_des",   "Design cycle power output",                      "MW",   "", "", "" },	
	{ TCS_PARAM, TCS_NUMBER, P_eta_c,          "eta_c",           "Design compressor(s) isentropic efficiency",     "-",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_eta_t,          "eta_t",           "Design turbine isentropic efficiency",           "-",    "", "", "" },	
	{ TCS_PARAM, TCS_NUMBER, P_P_high_limit,   "P_high_limit",    "High pressure limit in cycle",                   "MPa",  "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_DELTAT_PHX,     "deltaT_PHX",      "Temp diff btw hot HTF and turbine inlet",        "C",    "", "", "" },
		// Air-cooler Design Parameters
	{ TCS_PARAM, TCS_NUMBER, P_DELTAT_ACC,     "deltaT_ACC",      "Temp diff btw ambient air and compressor inlet", "C",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_T_AMB_DES,      "T_amb_des",       "Design: Ambient temperature for air cooler",     "C",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_FAN_POWER_PERC, "fan_power_perc",  "Percent of net cycle power used for fan",        "%",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_PLANT_ELEVATION,"plant_elevation", "Plant Elevation",                                "m",    "", "", "" },
		// Solar Receiver Design Parameters
	{ TCS_PARAM, TCS_NUMBER, P_T_htf_hot,      "T_htf_hot_des",   "Tower design outlet temp",                       "C",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_T_htf_cold_est, "T_htf_cold_est",  "Estimated tower design inlet temp",              "C",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_eta_des,        "eta_des",         "Power cycle thermal efficiency",                 "",     "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_rec_fl,         "rec_htf",         "The name of the HTF used in the receiver",       "",     "", "", "" },
	{ TCS_PARAM, TCS_MATRIX, P_rec_fl_props,   "rec_fl_props",    "User defined rec fluid property data",           "-", "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "", "" },
		// Cycle Controller Parameters
	{ TCS_PARAM, TCS_NUMBER, P_STARTUP_TIME,   "startup_time",    "Time needed for power block startup",                   "hr",   "", "", "0.5" },
	{ TCS_PARAM, TCS_NUMBER, P_STARTUP_FRAC,   "startup_frac",    "Fraction of design thermal power needed for startup",   "none", "", "", "0.2" },
	{ TCS_PARAM, TCS_NUMBER, P_Q_SBY_FRAC,     "q_sby_frac",      "Fraction of thermal power required for standby mode",   "none", "", "", "0.2" },
	{ TCS_PARAM, TCS_NUMBER, P_cycle_cutoff_frac, "cycle_cutoff_frac", "Minimum turbine operation fraction before shutdown","-",   "", "", "" },

	//INPUTS
	{ TCS_INPUT, TCS_NUMBER, I_T_HTF_HOT,      "T_htf_hot",       "Hot HTF inlet temperature, from storage tank",   "C",    "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_M_DOT_HTF,      "m_dot_htf",       "HTF mass flow rate",                             "kg/hr","", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_STANDBY_CONTROL,"standby_control", "Control signal indicating standby mode",         "none", "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_T_DB,           "T_db",            "Ambient dry bulb temperature",                   "C",    "", "", "" },
	{ TCS_INPUT, TCS_NUMBER, I_P_AMB,          "P_amb",           "Ambient pressure",                               "mbar", "", "", "" },

	//OUTPUTS
	{ TCS_OUTPUT, TCS_NUMBER, O_ETA_CYCLE_DES,   "eta_cycle_des",       "Design: Power cycle efficiency",           "%",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_LOW_DES,       "P_low_des",           "Design: Compressor inlet pressure",        "kPa",  "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_F_RECOMP_DES,    "f_recomp_des",        "Design: Recompression fraction",           "-",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_UA_RECUP_DES,    "UA_recup_des",        "Design: Recuperator conductance UA",       "kW/K", "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_COOLER_IN_DES, "T_cooler_in_des",     "Design: Cooler CO2 inlet temp",            "C",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_COOLER_VOLUME,   "cooler_volume",       "Estimated required cooler material vol.",  "m^3",  "", "", "" },

	{ TCS_OUTPUT, TCS_NUMBER, O_P_CYCLE,         "P_cycle",             "Cycle power output",                                    "MWe",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_ETA,             "eta",                 "Cycle thermal efficiency",                              "none",  "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_HTF_COLD,      "T_htf_cold",          "Heat transfer fluid outlet temperature ",               "C",     "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_M_DOT_MAKEUP,    "m_dot_makeup",        "Cooling water makeup flow rate",                        "kg/hr", "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_M_DOT_DEMAND,    "m_dot_demand",        "HTF required flow rate to meet power load",             "kg/hr", "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_M_DOT_HTF_REF,   "m_dot_htf_ref",       "Calculated reference HTF flow rate at design",          "kg/hr", "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_W_COOL_PAR,      "W_cool_par",          "Cooling system parasitic load",                         "MWe",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_F_BAYS,          "f_bays",              "Fraction of operating heat rejection bays",             "none",  "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_COND,          "P_cond",              "Condenser pressure",                                    "Pa",    "",  "",  "" },
	
	// Cycle Design Parameters to pass to controller
	{ TCS_OUTPUT, TCS_NUMBER, O_W_DOT_NET,       "o_W_dot_net",         "Target net cycle power",                                "kW",    "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_MC_IN,         "o_T_mc_in",           "Compressor inlet temperature",                          "K",     "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_T_IN,          "o_T_t_in",            "Turbine inlet temperature",                             "K",     "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_MC_IN,         "o_P_mc_in",           "Compressor inlet pressure",                             "kPa",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_MC_OUT,        "o_P_mc_out",          "Compressor outlet pressure",                            "kPa",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_UA_LT,           "o_UA_LT",             "UA in LTR",                                             "kW/K",  "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_UA_HT,           "o_UA_HT",             "UA in HTR",                                             "kW/K",  "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_RECOMP_FRAC,     "o_recomp_frac",       "recompresson fraction",                                 "",      "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_ETA_MC,          "o_eta_mc",            "main compressor isentropic efficiency",                 "",      "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_ETA_RC,          "o_eta_rc",            "re-compressor isentropic efficiency",                   "",      "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_ETA_T,           "o_eta_t",             "turbine isentropic efficiency",                         "",      "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_N_SUB_HXRS,      "o_N_sub_hxrs",        "number of sub heat exchangers",                         "",      "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_HIGH_LIMIT,    "o_P_high_limit",      "high pressure limit",                                   "MPa",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_N_turbine,       "o_N_turbine",         "Turbine shaft speed",                                   "rpm",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_DP_LT_C,         "o_DP_LT_c",           "Cold-side pressure drop - LT recup",                    "kPa",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_DP_LT_H,         "o_DP_LT_h",           "Hot-side pressure drop - LT recup",                     "kPa",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_DP_HT_C,         "o_DP_HT_c",           "Cold-side pressure drop - HT recup",                    "kPa",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_DP_HT_H,         "o_DP_HT_h",           "Hot-side pressure drop - HT recup",                     "kPa",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_DP_PC_H,         "o_DP_PC_h",           "Hot-side pressure drop - pre-cooler",                   "kPa",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_DP_PHX_C,        "o_DP_PHX_c",          "Cold-side pressure drop - PHX",                         "kPa",   "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_DELTAT_MC,       "o_deltaT_mc",         "Temperature difference btw comp inlet and Tamb",        "K",     "",  "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_DELTAT_T,        "o_deltaT_t",          "Temperature difference btw hot HTF and turbine inlet",  "K",     "",  "",  "" },

	/*
	double m_W_dot_net;					//[kW] Target net cycle power
	double m_T_mc_in;					//[K] Compressor inlet temperature
	double m_T_t_in;					//[K] Turbine inlet temperature
	double m_P_mc_in;					//[kPa] Compressor inlet pressure
	double m_P_mc_out;					//[kPa] Compressor outlet pressure
	std::vector<double> m_DP_LT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	std::vector<double> m_DP_HT;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	std::vector<double> m_DP_PC;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	std::vector<double> m_DP_PHX;		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	double m_UA_LT;						//[kW/K] UA in LTR
	double m_UA_HT;						//[kW/K] UA in HTR
	double m_recomp_frac;				//[-] Fraction of flow that bypasses the precooler and the main compressor at the design point
	double m_eta_mc;					//[-] design-point efficiency of the main compressor; isentropic if positive, polytropic if negative
	double m_eta_rc;					//[-] design-point efficiency of the recompressor; isentropic if positive, polytropic if negative
	double m_eta_t;						//[-] design-point efficiency of the turbine; isentropic if positive, polytropic if negative
	int m_N_sub_hxrs;					//[-] Number of sub-heat exchangers to use when calculating UA value for a heat exchanger
	double m_P_high_limit;				//[kPa] maximum allowable pressure in cycle
	double m_tol;						//[-] Convergence tolerance
	double m_N_turbine;					//[rpm] Turbine shaft speed (negative values link turbine to compressor)
	*/


	//N_MAX
	{ TCS_INVALID, TCS_INVALID, N_MAX, 0, 0, 0, 0, 0, 0 }
};


class sam_sco2_recomp_type424 : public tcstypeinterface
{
private:
	// Classes and Structures
		// Old sco2 cycle code
	// cycle_design_parameters rc_des_par;
	// RecompCycle * rc_cycle;
		// New sco2 cycle code
	C_RecompCycle ms_rc_cycle;
	C_RecompCycle::S_auto_opt_design_parameters ms_rc_autodes_par;
	//C_RecompCycle::S_opt_target_od_parameters ms_rc_opt_od_par;
	//C_RecompCycle::S_opt_target_od_parameters ms_rc_max_opt_od_par;
	//C_RecompCycle::S_opt_target_od_parameters ms_rc_des_opt_od_par;
	C_RecompCycle::S_od_parameters ms_rc_od_par;
	C_RecompCycle::S_PHX_od_parameters ms_phx_od_par;

	HTFProperties rec_htfProps;		// Instance of HTFProperties class for field HTF
	CO2_state co2_props;
	compact_hx ACC;
	int co2_error;
	
	// Cycle Design Parameters
	double m_W_dot_net_des;            // "Design cycle power output",                      "MW",  
	double m_T_mc_in_des;			   // "Main compressor inlet temp at design",           "C",   
	double m_T_t_in_des;			   // "Turbine inlet temp at design",                   "C",   
	double m_N_t_des;				   // "Design turbine speed, negative links to comp.",  "rpm", 
	double m_eta_c;					   // "Design compressor(s) isentropic efficiency",     "-",   
	double m_eta_t;					   // "Design turbine isentropic efficiency",           "-",   	
	double m_P_high_limit;			   // "High pressure limit in cycle",                   "MPa", 

	// Hardcoded Cycle Design Parameters
	double m_tol;                      // "Convergence tolerance for performance calcs", "-", "",
	double m_opt_tol;				   // "Convergence tolerance - optimization calcs", "-", "", 
	vector<double> m_DP_LT;            // (cold, hot) positive values are absolute [kPa], negative values are relative (-)
	vector<double> m_DP_HT;		       // (cold, hot) positive values are absolute [kPa], negative values are relative (-)
	vector<double> m_DP_PC;		       // (cold, hot) positive values are absolute [kPa], negative values are relative (-)
	vector<double> m_DP_PHX;		   // (cold, hot) positive values are absolute [kPa], negative values are relative (-)
	int m_N_sub_hxrs;                  // [-] Number of sections to model in heat exchangers
	double m_deltaP_cooler_frac;       // [-] Fraction of high side pressure that is allowed as pressure drop to design the ACC
	double m_q_max_sf;

	// Calculated Cycle Design Parameters
	double m_UA_total_des;			   // "Total UA allocatable to recuperators",           "kW/K",
	//double m_T_PHX_in;                 // [K] CO2 cold inlet to Primary Heat Exchanger
	double m_delta_T_acc;			   // [K/C] Temperature difference between compressor inlet temp and ambient
	double m_delta_T_t;				   // [K/C] Temperature difference between htf hot side and turbine inlet
	double m_m_dot_des;			       // [kg/s] CO2 mass flow rat thru cycle at design
	double m_cp_rec;                   // [kJ/kg-K] Specific heat of htf
	double m_UA_PHX_des;		       // [kW/K] PHX Conductance
	double m_T_htf_cold_sby;		   // [K] HTF cold return temperature for standby mode
	double m_m_dot_htf_sby;			   // [kg/s] 
	double m_W_dot_fan_des;            // [kW]
	double m_eta_thermal_des;		   // [-] Power cycle design thermal efficiency
	double m_T_htf_cold_des;           // [K]

	// Solar Receiver Design Parameters
	double m_T_htf_hot;              // [K] Tower design outlet temperature
	//double m_T_htf_cold;             // [K] Tower design inlet temperature	
	double m_Q_dot_rec_des;			// [MWt] Receiver design thermal input

	// Calculated Receiver Design Parameters
	double m_dot_rec_des;				// [kg/s] Receiver design point mass flow rate

	// Cycle Control Parameters
	double m_startup_time;			//[hr] Time needed for power block startup
	double m_startup_frac;			//[-] Fraction of design thermal power needed for startup
	double m_startup_energy;		//[kWt] Startup energy
	double m_q_sby_frac;			//[-] Fraction of thermal power required for standby mode

	// Stored Variables
	int    m_standby_control_prev;
	int    m_standby_control;
	double m_time_su_prev;
	double m_time_su;
	double m_E_su_prev;
	double m_E_su;
	int    m_error_message_code;

	bool   m_is_first_t_call;

	double m_q_dot_cycle_max;
	double m_P_mc_in_q_max;

public:
	sam_sco2_recomp_type424(tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface(cst, ti)
	{
		// Pointers
		// rc_cycle = NULL;

		// Parameters
		m_W_dot_net_des = std::numeric_limits<double>::quiet_NaN();
		m_T_mc_in_des = std::numeric_limits<double>::quiet_NaN();
		m_T_t_in_des = std::numeric_limits<double>::quiet_NaN();
		m_N_t_des = std::numeric_limits<double>::quiet_NaN();
		m_eta_c = std::numeric_limits<double>::quiet_NaN();
		m_eta_t = std::numeric_limits<double>::quiet_NaN();		
		m_P_high_limit = std::numeric_limits<double>::quiet_NaN();

		// Hardcoded values
		m_tol = 1.E-3;
		m_opt_tol = m_tol;
		m_DP_LT.resize(2);
		fill(m_DP_LT.begin(), m_DP_LT.end(), 0.0);
		m_DP_HT.resize(2);
		fill(m_DP_HT.begin(), m_DP_HT.end(), 0.0);
		m_DP_PC.resize(2);
		fill(m_DP_PC.begin(), m_DP_PC.end(), 0.0);
		m_DP_PHX.resize(2);
		fill(m_DP_PHX.begin(), m_DP_PHX.end(), 0.0);
		m_N_sub_hxrs = 10;
		m_deltaP_cooler_frac = 0.002;
		m_N_t_des = 3600.0;
		m_q_max_sf = 0.97;			// Safety factor for optimizer: can safely get with m_q_max_sf * q_max_calculated

		// Calculated Cycle Design Parameters
		m_UA_total_des = std::numeric_limits<double>::quiet_NaN();
		m_delta_T_acc = std::numeric_limits<double>::quiet_NaN();
		m_delta_T_t = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_des = std::numeric_limits<double>::quiet_NaN();
		m_cp_rec = std::numeric_limits<double>::quiet_NaN();
		m_T_htf_cold_sby = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_htf_sby = std::numeric_limits<double>::quiet_NaN();
		m_W_dot_fan_des = std::numeric_limits<double>::quiet_NaN();
		m_eta_thermal_des = std::numeric_limits<double>::quiet_NaN();
		m_T_htf_cold_des = std::numeric_limits<double>::quiet_NaN();

		// Solar Receiver Design Parameters
		m_T_htf_hot = std::numeric_limits<double>::quiet_NaN();
		//m_T_htf_cold = std::numeric_limits<double>::quiet_NaN();
		m_Q_dot_rec_des = std::numeric_limits<double>::quiet_NaN();

		// Calculated Receiver Design Parameters
		m_dot_rec_des = std::numeric_limits<double>::quiet_NaN();
		m_UA_PHX_des = std::numeric_limits<double>::quiet_NaN();

		// Cycle Control Parameters
		m_startup_time = numeric_limits<double>::quiet_NaN();
		m_startup_frac = numeric_limits<double>::quiet_NaN();
		m_startup_energy = numeric_limits<double>::quiet_NaN();
		m_q_sby_frac = numeric_limits<double>::quiet_NaN();

		// Stored Variables
		m_standby_control_prev = -1;
		m_standby_control = -1;
		m_time_su_prev = std::numeric_limits<double>::quiet_NaN();
		m_time_su = std::numeric_limits<double>::quiet_NaN();
		m_E_su_prev = std::numeric_limits<double>::quiet_NaN();
		m_E_su = std::numeric_limits<double>::quiet_NaN();

		m_is_first_t_call = true;

		m_q_dot_cycle_max = std::numeric_limits<double>::quiet_NaN();
		m_P_mc_in_q_max = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_sco2_recomp_type424(){}

	virtual int init()
	{
		// Get Parameters
		m_W_dot_net_des = value(P_W_dot_net_des)*1000.0;		//[kW] Design cycle power outpt 
		m_eta_c = value(P_eta_c);								// "Design compressor(s) isentropic efficiency",     "-",    
		m_eta_t = value(P_eta_t);								// "Design turbine isentropic efficiency",           "-",    		
		m_P_high_limit = value(P_P_high_limit);					// "High pressure limit in cycle",                   "MPa", 
		m_delta_T_t = value(P_DELTAT_PHX);						//[C] temperature difference between hot htf and sco2 turbine inlet
		// Air-cooler specific parameters
		m_delta_T_acc = value(P_DELTAT_ACC);					//[C] temperature difference between ambient air and compressor inlet
		double T_amb_cycle_des = value(P_T_AMB_DES) + 273.15;		//[K] Ambient temperature at power cycle design, convert from C
		double fan_power_frac = value(P_FAN_POWER_PERC) / 100.0;			//[-] Fraction of cycle net power output used by cooler air fan, convert from %
		// Solar Receiver Parameters
		// Receiver inlet/outlet temps and thermal input
		m_T_htf_hot = value(P_T_htf_hot) + 273.15;		//[K] Tower outlet temp at design, convert from C
		//m_T_htf_cold = value(P_T_htf_cold) + 273.15;	//[K] Tower inlet temp at design, convert from C
		m_eta_thermal_des = value(P_eta_des);
		m_Q_dot_rec_des = m_W_dot_net_des / m_eta_thermal_des;		//[kWt] Receiver thermal input at design

		// Calculate other cycle design parameters based on User Parameters
		m_T_mc_in_des = T_amb_cycle_des + m_delta_T_acc;	//[K] Compressor inlet temperature
		m_T_t_in_des = m_T_htf_hot - m_delta_T_t;			//[K] Turbine inlet temperature		

		double P_amb_cycle_des = 101325.0*pow(1 - 2.25577E-5*value(P_PLANT_ELEVATION), 5.25588);	//[Pa] http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html	

		// Check cycle parameter values are reasonable
			// Can't operate compressor in 2-phase region
		if(m_T_mc_in_des <= N_co2_props::T_crit)
		{
			message("Only single phase cycle operation is allowed in this model. The compressor inlet temperature (%lg [C]) must be great than the critical temperature: %lg [C]", m_T_mc_in_des-273.15, ((N_co2_props::T_crit)-273.15));
			return -1;
		}
			// "Reasonable" ceiling on compressor inlet temp
		double T_mc_in_max = 70.0 + 273.15;			//[K] Arbitrary value for max compressor inlet temperature
		if(m_T_mc_in_des > T_mc_in_max)
		{
			message("The compressor inlet temperature input was %lg [C]. This value was reset internally to the max allowable inlet temperature: %lg [C]", m_T_mc_in_des-273.15, T_mc_in_max-273.15);
			m_T_mc_in_des = T_mc_in_max;
		}
			// "Reasonable" floor on turbine inlet temp
		double T_t_in_min = 300.0 + 273.15;			//[K] Arbitrary value for min turbine inlet temperature
		if(m_T_t_in_des < T_t_in_min)
		{
			message("The turbine inlet temperature input was %lg [C]. This value was reset internally to the min allowable inlet temperature: %lg [C]", m_T_t_in_des-273.15, T_t_in_min-273.15);
			m_T_t_in_des = T_t_in_min;
		}
			// Turbine inlet temperature must be hotter than compressor outlet temperature
		if(m_T_t_in_des <= m_T_mc_in_des)
		{
			message("The turbine inlet temperature, %lg [C], is colder than the specified compressor inlet temperature %lg [C]", m_T_t_in_des-273.15, m_T_mc_in_des-273.15);
			return -1;
		}
			// Turbine inlet temperature must be colder than property limits
		if(m_T_t_in_des >= N_co2_props::T_upper_limit)
		{
			message("The turbine inlet temperature, %lg [C], is hotter than the maximum allow temperature in the CO2 property code %lg [C]", m_T_t_in_des-273.15, N_co2_props::T_upper_limit-273.15);
			return -1;
		}
			// Check for realistic isentropic efficiencies
		if(m_eta_c > 1.0)
		{
			message("The compressor isentropic efficiency, %lg, was reset to theoretical maximum 1.0", m_eta_c);
			m_eta_c = 1.0;
		}
		if( m_eta_t > 1.0 )
		{
			message("The turbine isentropic efficiency, %lg, was reset to theoretical maximum 1.0", m_eta_t);
			m_eta_t = 1.0;
		}
		if(m_eta_c < 0.1)
		{
			message("The compressor isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability", m_eta_c);
			m_eta_c = 0.1;
		}
		if( m_eta_t < 0.1 )
		{
			message("The turbine isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability", m_eta_t);
			m_eta_t = 0.1;
		}
			// Limits on high pressure limit
		if(m_P_high_limit >= N_co2_props::P_upper_limit/1.E3)
		{
			message("The upper pressure limit, %lg [MPa], was set to the internal limit in the CO2 properties code %lg [MPa]", m_P_high_limit, N_co2_props::P_upper_limit/1.E3);
			m_P_high_limit = N_co2_props::P_upper_limit / 1.E3;
		}
		double P_high_limit_min = 10.0;
		if(m_P_high_limit <= P_high_limit_min)
		{
			message("The upper pressure limit, %lg [MPa], must be greater than %lg [MPa] to ensure solution stability", m_P_high_limit, P_high_limit_min);
			return -1;
		}
		// ******************************************************************************************
		// ******************************************************************************************
		// Setup recompressoin cycle autodes parameter structure
		ms_rc_autodes_par.m_DP_HT = m_DP_HT;
		ms_rc_autodes_par.m_DP_LT = m_DP_LT;
		ms_rc_autodes_par.m_DP_PC = m_DP_PC;
		ms_rc_autodes_par.m_DP_PHX = m_DP_PHX;

		ms_rc_autodes_par.m_eta_mc = m_eta_c;
		ms_rc_autodes_par.m_eta_rc = m_eta_c;
		ms_rc_autodes_par.m_eta_t = m_eta_t;

		ms_rc_autodes_par.m_N_sub_hxrs = m_N_sub_hxrs;
		ms_rc_autodes_par.m_N_turbine = m_N_t_des;

		ms_rc_autodes_par.m_opt_tol = m_opt_tol;
		ms_rc_autodes_par.m_P_high_limit = m_P_high_limit*1000.0;		// Convert to kPa
		ms_rc_autodes_par.m_tol = m_tol;
		ms_rc_autodes_par.m_T_mc_in = m_T_mc_in_des;
		ms_rc_autodes_par.m_T_t_in = m_T_t_in_des;
		ms_rc_autodes_par.m_W_dot_net = m_W_dot_net_des;
		// ************************************************************************************
		// ************************************************************************************

		// Check air-cooler parameters are within limits
			// Ambient temperature must be cooler than compressor inlet temperature
		if(T_amb_cycle_des > m_T_mc_in_des - 2.0)
		{
			message("The ambient temperature used for the air cooler design, %lg [C], must be 2 [C] less than the specified compressor inlet temperature %lg [C]", T_amb_cycle_des-273.15, m_T_mc_in_des-273.15);
			return -1;
		}
			// Also need some "reasonable" lower limit on the ambient temperature
		if(T_amb_cycle_des < 273.15)
		{
			message("The ambient temperature used for the air cooler design, %lg [C], was reset to 0 [C] to improve solution stability", T_amb_cycle_des-273.15);
			T_amb_cycle_des = 273.15;
		}
			// Set an upper limit on the fraction of cycle net power allocated to the fan on the air cooler
		double fan_power_frac_max = 0.1;
		if(fan_power_frac > fan_power_frac_max)
		{
			message("The fraction of cycle net power used by the cooling fan, %lg, is greater than the internal maximum %lg", fan_power_frac, fan_power_frac_max);
			return -1;
		}
		double fan_power_frac_min = 0.001;
		if(fan_power_frac < fan_power_frac_min)
		{
			message("The fraction of cycle net power used by the cooling fan, %lg, is less than the internal minimum %lg", fan_power_frac, fan_power_frac_min);
			return -1;
		}
		
		// **************************************************************************
		// Check that receiver parameters are within limits
			// Receiver outlet temperature should be greater than turbine inlet temperature
		// if(m_T_htf_hot < m_T_t_in_des + 1.0)
		// {
		// 	message("The htf hot temperature, %lg [C], must be at least 1 [C] greater than the turbine inlet temperature %lg [C]", m_T_htf_hot-273.15, m_T_t_in_des-273.15);
		// 	return -1;
		// }		

		///*********************************
		// Don't need these checks if user input is cycle efficiency
		/*
			// Receiver inlet temperature must be less than the turbine inlet temperature
		if(m_T_htf_cold > m_T_t_in_des - 1.0)
		{
			message("The htf cold temperature, %lg [C], must be at least 1 [C] less than the turbine inlet temperature %lg [C]", m_T_htf_cold-273.15, m_T_t_in_des-273.15);
			return -1;
		}
			// Set a "reasonable" lower limit for the receiver inlet temperature
		double T_rec_cold_min = 150.0 + 273.15;
		if(m_T_htf_cold < T_rec_cold_min)
		{
			message("The htf cold temperature, %lg [C], must be greater than the internal limit for solution stability: %lg [C]", m_T_htf_cold-273.15, T_rec_cold_min-273.15);
			return -1;
		}
			// Receiver inlet temperature must be greater than the compressor inlet temperature
		if(m_T_htf_cold <= m_T_mc_in_des)
		{
			message("The htf cold temperature, %lg [C], must be greater than the specified compressor inlet temperature: %lg [C]", m_T_htf_cold-273.15, m_T_mc_in_des-273.15);
			return -1;
		}
			// Receiver thermal input should be at least as large as the net cycle electric output, but, this input is only an estimate and is recalculated in the code
		if(m_Q_dot_rec_des_est <= m_W_dot_net_des/1.E3)
		{
			m_Q_dot_rec_des_est = m_W_dot_net_des / 1.E3;
		}
		*/

		// Instead, check thermal efficiency
		if(m_eta_thermal_des <= 0.0)
		{
			message("The design cycle thermal efficiency, %lg, must be at least greater than 0 ", m_eta_thermal_des);
			return -1;
		}

		double eta_carnot = 1.0 - T_amb_cycle_des / m_T_htf_hot;
		if(m_eta_thermal_des >= eta_carnot)
		{
			message("The design cycle thermal efficiency, %lg, must be at least greater than the Carnot efficiency: %lg ", m_eta_thermal_des, eta_carnot);
			message("To solve the cycle within the allowable recuperator conductance, the efficiency should be significantly less than the Carnot efficiency");
			return -1;
		}

		// Declare instance of fluid class for FIELD fluid.
		int rec_fl = (int)value(P_rec_fl);
		if( rec_fl != HTFProperties::User_defined && rec_fl < HTFProperties::End_Library_Fluids )
		{
			rec_htfProps.SetFluid(rec_fl); // field_fl should match up with the constants
		}
		else if( rec_fl = HTFProperties::User_defined )
		{
			int nrows = 0, ncols = 0;
			double *fl_mat = value(P_rec_fl_props, &nrows, &ncols);
			if( fl_mat != 0 && nrows > 2 && ncols == 7 )
			{
				util::matrix_t<double> mat(nrows, ncols, 0.0);
				for( int r = 0; r < nrows; r++ )
				for( int c = 0; c < ncols; c++ )
					mat.at(r, c) = TCS_MATRIX_INDEX(var(P_rec_fl_props), r, c);

				if( !rec_htfProps.SetUserDefinedFluid(mat) )
				{
					//message( "user defined htf property table was invalid (rows=%d cols=%d)", nrows, ncols );
					message(rec_htfProps.UserFluidErrMessage(), nrows, ncols);
					return -1;
				}
			}
		}
		else
		{
			message("Receiver HTF code is not recognized");
			return -1;
		}
		// ********************************************************************************
		// ********************************************************************************
		// double T_PHX_in = m_T_htf_cold - m_delta_T_t;					//[K]

		/*
		// Now need to vary UA_recup until m_T_PHX_in is achieved. This could be slow...
		// Is there a good way to calculate a decent guess???
		// 1) Estimate co2 mass flow rate from PHX energy balance
		double T_PHX_cold_ave = 0.5*(m_T_PHX_in + m_T_t_in_des);
		co2_error = CO2_TP(T_PHX_cold_ave, m_P_high_limit*1.E3, &co2_props);
		double cp_cold_ave = co2_props.cp;			//[kJ/kg-K]
		double m_dot_co2_guess = m_Q_dot_rec_des_est*1.E3 / (cp_cold_ave*(m_T_t_in_des - m_T_PHX_in));	//[kg/s]
		// 2) Guess a reasonable pressure ratio and assume operating at upper pressure limit
		double PR_guess = 2.5;
		double P_low_guess = m_P_high_limit / PR_guess;	//[MPa] Guess of lower pressure based on Pressure Ratio guess
		// 3) Calculate turbine outlet temperature
		int turbo_error = 0;
		double dummy[7];
		double T_t_out_guess = -999.9;
		double h_t_out_guess = -999.9;
		calculate_turbomachinery_outlet(m_T_t_in_des, m_P_high_limit*1.E3, P_low_guess*1.E3, m_eta_t, false, turbo_error,
			dummy[0], dummy[1], dummy[2], T_t_out_guess, h_t_out_guess, dummy[4], dummy[5], dummy[6]);
		// 4) Calculate compressor outlet temperature
		double T_c_out_guess = -999.9;
		calculate_turbomachinery_outlet(m_T_mc_in_des, P_low_guess*1.E3, m_P_high_limit*1.E3, m_eta_c, true, turbo_error,
			dummy[0], dummy[1], dummy[2], T_c_out_guess, dummy[3], dummy[4], dummy[5], dummy[6]);
		// 5) Guess hot side recups outlet temp
		double T_recup_hot_outlet_guess = T_c_out_guess + 0.75*(T_t_out_guess - m_T_PHX_in);	// 0.5 multiplier is estimate
		co2_error = CO2_TP(T_recup_hot_outlet_guess, P_low_guess*1.E3, &co2_props);
		double h_recup_hot_outlet_guess = co2_props.enth;
		// 6) Estimate heat transfer rate in recuperators
		double q_dot_recups_guess = m_dot_co2_guess*(h_t_out_guess - h_recup_hot_outlet_guess);		//[kW]
		// 7) Estimate UA
		double UA_recups_guess = q_dot_recups_guess / (0.75*(T_t_out_guess - m_T_PHX_in));		//[kW/K] 0.5 multiplier is estimate
		*/
		// 10.1.14 twn: this guess process doesn't work well for different inputs...

		double UA_net_power_ratio_max = 2.0;
		double UA_net_power_ratio_min = 1.E-5;

		//UA_recups_guess = max(UA_recups_guess, UA_net_power_ratio_min*m_W_dot_net_des);
		//UA_recups_guess = min(UA_recups_guess, UA_net_power_ratio_max*m_W_dot_fan_des);

		double UA_recups_guess = 0.1*m_W_dot_net_des;
				// **********************************************************************
		// **********************************************************************
		// Solve design point model with guessed recups UA
		m_UA_total_des = UA_recups_guess;
		// rc_des_par.m_UA_rec_total = m_UA_total_des;
		ms_rc_autodes_par.m_UA_rec_total = m_UA_total_des;
		// Create new instance of RecompCycle class and assign to member point
		// rc_cycle = new RecompCycle(rc_des_par);
		// bool auto_cycle_success = rc_cycle->auto_optimal_design();

		int auto_opt_error_code = 0;
		ms_rc_cycle.auto_opt_design(ms_rc_autodes_par, auto_opt_error_code);
		if(auto_opt_error_code != 0)
		{
			message("Can't optimize sCO2 power cycle with current inputs");
			return -1;
		}

		// double T_PHX_in_calc = rc_cycle->get_cycle_design_metrics()->m_T[5 - 1];
		// double T_PHX_in_calc = ms_rc_cycle.get_design_solved()->m_temp[5-1];
		double eta_calc = ms_rc_cycle.get_design_solved()->m_eta_thermal;
		// **********************************************************************

		// Now need to iterate UA_total_des until eta_thermal_calc = eta_thermal_in
		// double diff_T_PHX_in = (T_PHX_in_calc - m_T_PHX_in)/m_T_PHX_in;			//[-]
		double diff_eta = (eta_calc - m_eta_thermal_des);
		bool low_flag = false;
		bool high_flag = false;
		double y_upper = numeric_limits<double>::quiet_NaN();
		double y_lower = numeric_limits<double>::quiet_NaN();
		double x_upper = numeric_limits<double>::quiet_NaN();
		double x_lower = numeric_limits<double>::quiet_NaN();
		double UA_net_power_ratio = numeric_limits<double>::quiet_NaN();
		
		
		int opt_des_calls = 1;

		while( abs(diff_eta) > m_tol )
		{
			opt_des_calls++;

			if(diff_eta > 0.0)		// Calc > target, UA is too large, decrease UA
			{
				low_flag = true;
				x_lower = UA_recups_guess;
				y_lower = diff_eta;

				if(high_flag)	// Upper and lower bounds set, use false positon interpolation method
				{
					UA_recups_guess = -y_upper*(x_lower-x_upper)/(y_lower-y_upper) + x_upper;
				}
				else			// No upper bound set, try to get there
				{
					if( opt_des_calls > 5 )
						UA_recups_guess = UA_net_power_ratio_min*m_W_dot_net_des;
					else
						UA_recups_guess *= 0.5;
				}

				if( x_lower / m_W_dot_net_des <= UA_net_power_ratio_min )
				{
					message("The design thermal efficiency, %lg [-], is too small to achieve with the available cycle model and inputs", m_eta_thermal_des);
					message("The lowest possible thermal efficiency for these inputs is roughly %lg [-]", ms_rc_cycle.get_design_solved()->m_eta_thermal);
					return -1;
				}
			}
			else						// Calc < target, UA is too small, decrease UA
			{
				high_flag = true;
				x_upper = UA_recups_guess;
				y_upper = diff_eta;
				
				if(low_flag)
				{
					UA_recups_guess = -y_upper*(x_lower - x_upper) / (y_lower - y_upper) + x_upper;
				}
				else
				{
					if( opt_des_calls > 5 )
						UA_recups_guess = UA_net_power_ratio_max*m_W_dot_net_des;
					else
						UA_recups_guess *= 2.5;
				}

				if( x_upper / m_W_dot_net_des >= UA_net_power_ratio_max )
				{
					message("The design thermal efficiency, %lg [-], is too large to achieve with the available cycle model and inputs", m_eta_thermal_des);
					message("The largest possible thermal efficiency for these inputs is roughly %lg [-] ", ms_rc_cycle.get_design_solved()->m_eta_thermal);
					return -1;
				}
			}

			// Solve design point model with guessed recups UA
			m_UA_total_des = UA_recups_guess;
			//rc_des_par.m_UA_rec_total = m_UA_total_des;
			ms_rc_autodes_par.m_UA_rec_total = m_UA_total_des;
			// Create new instance of RecompCycle class and assign to member point
			//rc_cycle->set_design_parameters(rc_des_par);
			//auto_cycle_success = rc_cycle->auto_optimal_design();
			
			ms_rc_cycle.auto_opt_design(ms_rc_autodes_par,auto_opt_error_code);
			if( auto_opt_error_code != 0 )
			{
				message("Can't optimize sCO2 power cycle with current inputs");
				return -1;
			}
			
			//T_PHX_in_calc = rc_cycle->get_cycle_design_metrics()->m_T[5 - 1];
			//T_PHX_in_calc = ms_rc_cycle.get_design_solved()->m_temp[5 - 1];
			eta_calc = ms_rc_cycle.get_design_solved()->m_eta_thermal;
			// **********************************************************************

			// Now need to iterate UA_total_des until T_PHX_in_calc = m_T_PHX_in
			// diff_T_PHX_in = (T_PHX_in_calc - m_T_PHX_in) / m_T_PHX_in;			//[-]
			diff_eta = (eta_calc - m_eta_thermal_des);
		}

		// Design metrics
		double design_eta = ms_rc_cycle.get_design_solved()->m_eta_thermal;
		double W_dot_net_des_calc = ms_rc_cycle.get_design_solved()->m_W_dot_net;		// Can compare to target net output to check convergence
		m_m_dot_des = ms_rc_cycle.get_design_solved()->m_m_dot_t;			//[kg/s]
		double P_PHX_out = ms_rc_cycle.get_design_solved()->m_pres[6-1];		//[kPa]
		double P_PHX_in = ms_rc_cycle.get_design_solved()->m_pres[5 - 1];			//[kPa]

		double T_htf_cold_est = value(P_T_htf_cold_est) + 273.15;
		// Calculate HTF cold temperature

		double T_PHX_co2_in = ms_rc_cycle.get_design_solved()->m_temp[5 - 1];
		double T_htf_cold = T_PHX_co2_in + m_delta_T_t;

		// Can give PHX inlet --or-- HX UA, but there is going to be a conflict between
		// 1) Assumed thermal input to cycle
		// -- and --
		// 2) Calculated thermal input to cycle: W_dot_net/eta_thermal
		// 
		// So, solar multiple in UI will also depend on estimating the cycle thermal efficiency
		// Should also revisit UA guess assumptions, but probably still OK

		// ***************************************************************
		// Calculate Design UA of PHX
		// Assumes properties behave well here, which is reasonable given distance from critical point
		// Also assumes that CR = 1
		// ***************************************************************
		// Receiver/hot side
		double T_rec_ave = 0.5*(T_htf_cold + m_T_htf_hot);		//[K]
		m_cp_rec = rec_htfProps.Cp(T_rec_ave);					//[kJ/kg-K]
		m_dot_rec_des = m_Q_dot_rec_des / (m_cp_rec*(m_T_htf_hot - T_htf_cold));	//[kg/s]

		// Cycle/cold side
		double T_PHX_co2_ave = 0.5*(m_T_t_in_des + T_PHX_co2_in);		//[K]
		co2_error = CO2_TP(T_PHX_co2_ave, P_PHX_in, &co2_props);
		double cp_PHX_co2 = m_Q_dot_rec_des / (m_m_dot_des*(m_T_t_in_des - T_PHX_co2_in));

		// Because C_dot_c = C_dot_h, q_dot_max = 
		double q_dot_max = m_dot_rec_des*m_cp_rec*(m_T_htf_hot - T_PHX_co2_in);		//[kW]

		// Effectiveness & NTU
		double eff_des = m_Q_dot_rec_des / q_dot_max;
		double NTU = eff_des / (1.0 - eff_des);

		// UA
		m_UA_PHX_des = NTU * m_dot_rec_des * m_cp_rec;

		//// Now set-up iteration on T_cold
		//double T_htf_lower = 0.0;
		//bool know_htf_lower = false;

		//double T_htf_upper = 0.0;
		//bool know_htf_upper = false;

		//double diff_T_htf_cold = ms_rc_autodes_par.m_tol*2.0;

		//C_RecompCycle::S_od_parameters rc_od_par;
		//rc_od_par.m_T_mc_in = m_T_mc_in_des;
		//rc_od_par.m_T_t_in = m_T_t_in_des;
		//rc_od_par.m_N_t = ms_rc_cycle.get_design_solved()->m_N_t;
		//rc_od_par.m_N_sub_hxrs = ms_rc_autodes_par.m_N_sub_hxrs;
		//rc_od_par.m_tol = ms_rc_autodes_par.m_tol;

		//C_RecompCycle::S_PHX_od_parameters phx_od_par;
		//phx_od_par.m_m_dot_htf_des = m_dot_rec_des;
		//phx_od_par.m_T_htf_hot = m_T_htf_hot;
		//phx_od_par.m_m_dot_htf = m_dot_rec_des;
		//phx_od_par.m_UA_PHX_des = m_UA_PHX_des;
		//phx_od_par.m_cp_htf = m_cp_rec;

		//int hx_od_error = 0;

		//for( int iter_T_htf_cold = 0; fabs(diff_T_htf_cold) > ms_rc_autodes_par.m_tol; iter_T_htf_cold++ )
		//{
		//	if(iter_T_htf_cold > 0)			// diff_T_htf_cold = (T_htf_cold_calc - T_htf_cold) / T_htf_cold;
		//	{
		//		if(diff_T_htf_cold > 0.0)		// T_htf_cold is too small
		//		{
		//			T_htf_lower = T_htf_cold;
		//			know_htf_lower = true;
		//			if( know_htf_upper )
		//				T_htf_cold = 0.5*(T_htf_lower + T_htf_upper);
		//			else
		//				T_htf_cold = T_htf_cold + 10.0;
		//		}
		//		else
		//		{
		//			T_htf_upper = T_htf_cold;
		//			know_htf_upper = true;
		//			if( know_htf_lower )
		//				T_htf_cold = 0.5*(T_htf_lower + T_htf_upper);
		//			else
		//				T_htf_cold = T_htf_cold - 10.0;
		//		}			
		//	}

		//	if(iter_T_htf_cold > 30)
		//	{
		//		hx_od_error = 1;
		//		break;
		//	}

		//	// ***************************************************************************
		//	// Run Off-Design Model @ Design Conditions to get behavior with optimized PHX
		//	// ***************************************************************************
		//	m_dot_rec_des = m_Q_dot_rec_des / (m_cp_rec*(m_T_htf_hot - T_htf_cold));	//[kg/s]

		//	phx_od_par.m_m_dot_htf = m_dot_rec_des;

		//	hx_od_error = 0;

		//	double C_dot_htf = phx_od_par.m_m_dot_htf * phx_od_par.m_cp_htf;

		//	ms_rc_cycle.opt_od_eta_for_hx(rc_od_par, phx_od_par, hx_od_error);

		//	if( hx_od_error != 0 )
		//		break;

		//	double Q_dot_PHX = ms_rc_cycle.get_od_solved()->m_Q_dot;
		//	
		//	double T_htf_cold_calc = m_T_htf_hot - Q_dot_PHX / C_dot_htf;

		//	diff_T_htf_cold = (T_htf_cold_calc - T_htf_cold) / T_htf_cold;
		//}
		//
		//if(hx_od_error != 0)
		//{
		//	message("Primary heat exchanger sizing failed");
		//	return -1;
		//}

		m_T_htf_cold_des = T_htf_cold;		//[K]

		message("The calculated cold HTF temperature is %lg [C]. The estimated cold HTF temperature is %lg [C]. This difference may affect the receiver design and hours of thermal storage. Try adjusting the receiver inlet temperature or design cycle efficiency",
			T_htf_cold - 273.15, T_htf_cold_est - 273.15);


		// *****************************************************************
		// Call Air Cooled Condenser
		// *****************************************************************		

		double T_acc_in = ms_rc_cycle.get_design_solved()->m_temp[9 - 1];
		double P_acc_in = ms_rc_cycle.get_design_solved()->m_pres[9 - 1];
		double m_dot_acc_in = ms_rc_cycle.get_design_solved()->m_m_dot_mc;
		
		m_W_dot_fan_des = fan_power_frac*m_W_dot_net_des/1000.0;	//[MW] Cooler air fan power at design
		
		double deltaP_des = m_deltaP_cooler_frac*ms_rc_cycle.get_design_solved()->m_pres[2-1];
				
		double T_acc_out = m_T_mc_in_des;

		// Call air-cooler design method
		ACC.design_hx(T_amb_cycle_des, P_amb_cycle_des, T_acc_in, P_acc_in, m_dot_acc_in,
			m_W_dot_fan_des, deltaP_des, T_acc_out);

		// Get air-cooler design parameters
		// compact_hx::S_hx_design_solved s_hx_design_solved;
		
		// Write outputs
		value(O_ETA_CYCLE_DES, design_eta);
		value(O_P_LOW_DES, ms_rc_cycle.get_design_solved()->m_pres[1-1]);
		value(O_F_RECOMP_DES, ms_rc_cycle.get_design_solved()->m_recomp_frac);
		value(O_UA_RECUP_DES, m_UA_total_des);
		value(O_T_COOLER_IN_DES, T_acc_in-273.15);
		value(O_COOLER_VOLUME, ACC.get_hx_design_solved()->m_material_V);

		m_startup_time = value(P_STARTUP_TIME);		//[hr] Time needed for power block startup
		m_startup_frac = value(P_STARTUP_FRAC);		//[-] Fraction of design thermal power needed for startup
		m_q_sby_frac	= value( P_Q_SBY_FRAC );	//[-] Fraction of thermal power required for standby mode	
		double cutoff_frac = value(P_cycle_cutoff_frac);	//[-] Minimum fraction of thermal power at which power cycle operates in part load (can still go standby)

		// Calculate the startup energy needed
		m_startup_energy = m_startup_frac*m_W_dot_net_des / design_eta;		//[kWt - hr]

		// Initialize stored variables
		m_standby_control_prev = 3;
		m_time_su_prev = m_startup_time;
		m_E_su_prev = m_startup_energy;

		m_time_su = m_time_su_prev;
		m_E_su = m_E_su_prev;
		m_standby_control = m_standby_control_prev;

		m_error_message_code = 0;

		// Initialize member data in 'ms_rc_opt_od_par' that is held constant for CSP simulations
		// ms_rc_opt_od_par.m_is_target_Q = true;
		// 
		// ms_rc_opt_od_par.m_N_sub_hxrs = 10;
		// ms_rc_opt_od_par.m_lowest_pressure = 1000.0;			// twn: this isn't really based on anything...
		// ms_rc_opt_od_par.m_highest_pressure = ms_rc_autodes_par.m_P_high_limit;
		// 
		// // Check if design cycle has a recompressor
		// if( ms_rc_cycle.get_design_solved()->m_is_rc )
		// {
		// 	ms_rc_opt_od_par.m_fixed_recomp_frac = false;			
		// }
		// else
		// {
		// 	
		// 	ms_rc_opt_od_par.m_fixed_recomp_frac = true;
		// 	ms_rc_opt_od_par.m_recomp_frac_guess = 0.0;
		// }
		// 
		// ms_rc_opt_od_par.m_fixed_N_mc = false;
		// 
		// ms_rc_opt_od_par.m_fixed_N_t = true;
		// ms_rc_opt_od_par.m_N_t_guess = ms_rc_cycle.get_design_solved()->m_N_t;
		// 
		// ms_rc_opt_od_par.m_tol = ms_rc_autodes_par.m_tol;
		// ms_rc_opt_od_par.m_opt_tol = ms_rc_autodes_par.m_opt_tol;
		// 
		// // Remaining member data to define:
		// 	// m_T_mc_in
		// 	// m_T_t_in
		// 	// m_target
		// 	// m_recomp_frac_guess (if not 0)
		// 	// m_N_mc_guess
		// 
		// // Calculate standby info
		// 	// Call target off-design at standby frac to get
		// ms_rc_opt_od_par.m_T_mc_in = m_T_mc_in_des;
		// ms_rc_opt_od_par.m_T_t_in = m_T_t_in_des;
		// 
		// ms_rc_opt_od_par.m_target = m_Q_dot_rec_des*cutoff_frac;
		// 
		// ms_rc_opt_od_par.m_use_default_res = true;
		// 
		// if( !ms_rc_opt_od_par.m_fixed_recomp_frac )
		// 	ms_rc_opt_od_par.m_recomp_frac_guess = ms_rc_cycle.get_design_solved()->m_recomp_frac;
		// 
		// ms_rc_opt_od_par.m_N_mc_guess = ms_rc_cycle.get_design_solved()->m_N_mc;
		// 
		// // Separately save design guesses
		// ms_rc_des_opt_od_par = ms_rc_opt_od_par;
		// // And initialize structure for max_q guesses
		// ms_rc_max_opt_od_par = ms_rc_des_opt_od_par;

		int q_sby_error_code = 0;

		// Checking minimum cycle power
		// ms_rc_cycle.optimal_target_off_design_no_check(ms_rc_opt_od_par, q_sby_error_code);

		// Check that broken up 'optimal_target_off_design' codes are working properly
		// ms_rc_cycle.get_max_output_od(ms_rc_opt_od_par, q_sby_error_code);

		ms_rc_od_par.m_T_mc_in = m_T_mc_in_des;
		ms_rc_od_par.m_T_t_in = m_T_t_in_des;
		ms_rc_od_par.m_N_t = ms_rc_cycle.get_design_solved()->m_N_t;
		ms_rc_od_par.m_N_sub_hxrs = ms_rc_autodes_par.m_N_sub_hxrs;
		ms_rc_od_par.m_tol = ms_rc_autodes_par.m_tol;

		ms_phx_od_par.m_m_dot_htf_des = m_dot_rec_des;
		ms_phx_od_par.m_T_htf_hot = m_T_htf_hot;
		ms_phx_od_par.m_m_dot_htf = m_dot_rec_des*cutoff_frac;
		ms_phx_od_par.m_UA_PHX_des = m_UA_PHX_des;
		ms_phx_od_par.m_cp_htf = m_cp_rec;

		double C_dot_htf_sby = ms_phx_od_par.m_m_dot_htf * ms_phx_od_par.m_cp_htf;

		ms_rc_cycle.opt_od_eta_for_hx(ms_rc_od_par, ms_phx_od_par, q_sby_error_code);

		if( q_sby_error_code != 0 )
		{
			message("The power cycle model crashes at the specified cutoff fraction, %lg. Try increasing this value", cutoff_frac);
			return -1;
		}

		double Q_dot_PHX_sby = ms_rc_cycle.get_od_solved()->m_Q_dot;

		double T_htf_cold_calc = m_T_htf_hot - Q_dot_PHX_sby / C_dot_htf_sby;

		double m_T_PHX_in_sby = ms_rc_cycle.get_od_solved()->m_temp[5 - 1];

		m_T_htf_cold_sby = m_T_PHX_in_sby + 5.0;		// Estimate htf return temp w/o heat exchanger
		//double m_m_dot_htf_sby = m_Q_dot_rec_des*m_q_sby_frac/(m_cp_rec*(m_T_htf_hot - m_T_htf_cold_sby));

		// Set outputs that will be constant for this type (required because can share a cmod with molten salt tower)
		value(O_M_DOT_MAKEUP, 0.0);
		value(O_M_DOT_DEMAND, 0.0);							// This output is an input for the controller, but the controller doesn't use it...
		value(O_M_DOT_HTF_REF, m_dot_rec_des*3600.0);		// Not actually the RECEIVER des. should be HTF
		value(O_F_BAYS, 0.0);
		value(O_P_COND, 0.0);								// Probably do want to report some cycle info that is different from steam

		// Set design parameter outputs for type251
		value(O_W_DOT_NET, W_dot_net_des_calc);
		value(O_T_MC_IN, m_T_mc_in_des);
		value(O_T_T_IN, m_T_t_in_des);
		value(O_P_MC_IN, ms_rc_cycle.get_design_solved()->m_pres[1-1]);
		value(O_P_MC_OUT, ms_rc_cycle.get_design_solved()->m_pres[2-1]);
		value(O_UA_LT, ms_rc_cycle.get_design_solved()->m_UA_LT);
		value(O_UA_HT, ms_rc_cycle.get_design_solved()->m_UA_HT);
		value(O_RECOMP_FRAC, ms_rc_cycle.get_design_solved()->m_recomp_frac);
		value(O_ETA_MC, m_eta_c);
		value(O_ETA_RC, m_eta_c);
		value(O_ETA_T, m_eta_t);
		value(O_N_SUB_HXRS, m_N_sub_hxrs);
		value(O_P_HIGH_LIMIT, m_P_high_limit*1000.0);
		value(O_N_turbine, ms_rc_cycle.get_design_solved()->m_N_t);
		value(O_DP_LT_C, m_DP_LT[0]);
		value(O_DP_LT_H, m_DP_LT[1]);
		value(O_DP_HT_C, m_DP_HT[0]);
		value(O_DP_HT_H, m_DP_HT[1]);
		value(O_DP_PC_H, m_DP_PC[1]);
		value(O_DP_PHX_C, m_DP_PHX[0]);
		value(O_DELTAT_MC, m_delta_T_acc);
		value(O_DELTAT_T, m_delta_T_t);

		// Dummy inputs
		//
		//double m_dot_htf = 601.4;
		//double T_db = 0.0;
		//double T_htf_hot = 846.1;
		//
		//
		//
		//// Assume compressor inlet temperature is always design point delta T above ambient: (T_amb_des - T_comp_in)
		//// Floor is ~ critical temp + 1 = 32 C
		//double T_mc_in = max(32.0 + 273.15, T_db + m_delta_T_acc);
		//
		//// Assume turbine inlet temperature is always design point delta T below receiver hot side
		//double T_t_in = T_htf_hot - m_delta_T_t;
		//
		//C_RecompCycle::S_od_parameters rc_od_par;
		//rc_od_par.m_T_mc_in = T_mc_in;
		//rc_od_par.m_T_t_in = T_t_in;
		//rc_od_par.m_N_t = ms_rc_cycle.get_design_solved()->m_N_t;
		//rc_od_par.m_N_sub_hxrs = ms_rc_autodes_par.m_N_sub_hxrs;
		//rc_od_par.m_tol = ms_rc_autodes_par.m_tol;
		//
		//C_RecompCycle::S_PHX_od_parameters phx_od_par;
		//phx_od_par.m_m_dot_htf_des = m_dot_rec_des;
		//phx_od_par.m_T_htf_hot = T_htf_hot;
		//phx_od_par.m_m_dot_htf = m_dot_htf;
		//phx_od_par.m_UA_PHX_des = m_UA_PHX_des;
		//phx_od_par.m_cp_htf = m_cp_rec;
		//
		//int hx_od_error = 0;
		//
		//double C_dot_htf = phx_od_par.m_m_dot_htf * phx_od_par.m_cp_htf;
		//
		//ms_rc_cycle.opt_od_eta_for_hx(rc_od_par, phx_od_par, hx_od_error);		
		//
		//double Q_dot_PHX = ms_rc_cycle.get_od_solved()->m_Q_dot;
		//T_htf_cold = T_htf_hot - Q_dot_PHX / C_dot_htf;
		//
		//// Check error code
		//int blah = hx_od_error;
		//
		/*
		// Set guess values
		rc_od_par.m_P_mc_in = ms_rc_cycle.get_design_solved()->m_pres[1 - 1];
		rc_od_par.m_recomp_frac = ms_rc_cycle.get_design_solved()->m_recomp_frac;

		// Cycle through compressor speeds
		double N_mc_low = ms_rc_cycle.get_design_solved()->m_N_mc*0.1;
		double N_mc_high = ms_rc_cycle.get_design_solved()->m_N_mc*1.5;
		int n_N_mc_intervals = 100;

		// Cycle through recomp fractions
		double f_recomp_low = 0.0;
		double f_recomp_high = ms_rc_cycle.get_design_solved()->m_recomp_frac + 0.05;
		int n_f_recomp = 20;

		// Cycle through pressures
		double P_mc_in_low = 3500.0;
		double P_mc_in_high = 12000.0;
		int n_P_mc_in = 20;

		std::vector<double> P_mc_in_conv(0);
		std::vector<double> f_recomp_conv(0);
		std::vector<double> N_mc_conv(0);
		std::vector<double> eta_conv(0);
		std::vector<double> T_htf_cold_conv(0);
		std::vector<double> Q_PHX_conv(0);
		std::vector<double> P_mc_out_conv(0);

		double P_mc_in_guess = P_mc_in_low;
		for( int i_P_mc_in = 0; P_mc_in_guess < P_mc_in_high; i_P_mc_in++ )
		{
			P_mc_in_guess = P_mc_in_low + (P_mc_in_high - P_mc_in_low) / (double)n_P_mc_in * i_P_mc_in;
			rc_od_par.m_P_mc_in = P_mc_in_guess;

			double f_recomp_guess = f_recomp_low;

			for( int i_f_recomp = 0; f_recomp_guess < f_recomp_high; i_f_recomp++ )
			{
				f_recomp_guess = f_recomp_low + (f_recomp_high - f_recomp_low) / (double)n_f_recomp * i_f_recomp;
				rc_od_par.m_recomp_frac = f_recomp_guess;

				double N_mc_guess = N_mc_low;

				double x_upper, x_lower;
				x_upper = N_mc_high;
				x_lower = N_mc_low;
				bool set_upper = false;
				bool set_lower = false;

				int error_flag = 0;

				double UA_tol = 2.0*ms_rc_opt_od_par.m_tol;

				double UA_diff = 2.0*UA_tol;

				for( int i_N_mc = 0; abs(UA_diff) > UA_tol; i_N_mc++ )
				{
					if( i_N_mc > 0 )
					{
						if(error_flag != 0)
						{
							if(error_flag == 1)
							{
								if(set_lower)
								{
									x_upper = N_mc_guess;
									set_upper = true;
									N_mc_guess = 0.5*(x_upper + x_lower);
								}
								else if(set_upper)
								{
									x_lower = N_mc_guess;
									set_lower = true;
									N_mc_guess = 0.5*(x_upper + x_lower);
								}
								else
								{
									x_lower = N_mc_guess;
									N_mc_guess *= 1.1;
								}
							}
							if(error_flag == 2 || error_flag == 3)
							{
								x_upper = N_mc_guess;
								set_upper = true;
								N_mc_guess = 0.5*(x_upper + x_lower);
							}						
						}
						else
						{
							if( UA_diff > 0.0 )			// UA_diff = (UA_calc - UA_PHX_od) / UA_PHX_od;  -> Q_dot_PHX too large
							{
								x_upper = N_mc_guess;
								set_upper = true;
								if( set_lower )
									N_mc_guess = 0.5*(x_upper + x_lower);
								else
									N_mc_guess = x_lower;
							}
							else						// UA_diff = (UA_calc - UA_PHX_od) / UA_PHX_od;  -> Q_dot_PHX too large	
							{
								x_lower = N_mc_guess;
								set_lower = true;
								if( set_upper )
									N_mc_guess = 0.5*(x_upper + x_lower);
								else
									N_mc_guess = x_upper;
							}
						}
						error_flag = 0;
					}

					if( (x_upper - x_lower) / x_upper < 0.001 )
						break;

					rc_od_par.m_N_mc = N_mc_guess;


					//rc_od_par.m_P_mc_in = 6475.0;
					//rc_od_par.m_recomp_frac = 0.147774;
					//rc_od_par.m_N_mc = 8198.791121;

					int od_error_code = 0;

					ms_rc_cycle.off_design(rc_od_par, od_error_code);

					if( od_error_code != 0 )
					{
						error_flag = 1;
						continue;
					}

					if( ms_rc_cycle.get_od_solved()->m_pres[2 - 1] > 25000.0 )
					{
						error_flag = 2;
						break;
					}

					// Get off design values for PHX calcs
					double m_dot_PHX = ms_rc_cycle.get_od_solved()->m_m_dot_t;
					double T_PHX_in = ms_rc_cycle.get_od_solved()->m_temp[5 - 1];

					double Q_dot_PHX = m_dot_PHX*(ms_rc_cycle.get_od_solved()->m_enth[6 - 1] - ms_rc_cycle.get_od_solved()->m_enth[5 - 1]);		//[kW]

					// Calculate off-design UA
					double m_dot_ratio = 0.5*(m_dot_htf / m_dot_rec_des + m_dot_PHX / m_m_dot_des);
					double UA_PHX_od = m_UA_PHX_des*pow(m_dot_ratio, 0.8);

					double C_dot_co2 = m_dot_PHX*(ms_rc_cycle.get_od_solved()->m_enth[6 - 1] - ms_rc_cycle.get_od_solved()->m_enth[5 - 1]) /
						(ms_rc_cycle.get_od_solved()->m_temp[6 - 1] - ms_rc_cycle.get_od_solved()->m_temp[5 - 1]);	//[kW/K]

					double C_dot_min = min(C_dot_co2, C_dot_htf);
					double C_dot_max = max(C_dot_co2, C_dot_htf);

					double C_R = C_dot_min / C_dot_max;

					double eff = Q_dot_PHX / (C_dot_min*(T_htf_hot - T_PHX_in));

					if( eff > 0.999 )
					{
						error_flag = 3;
						continue;
					}

					double NTU = 0.0;
					if( C_R != 1.0 )
						NTU = log((1.0 - eff*C_R) / (1.0 - eff)) / (1.0 - C_R);		// [-] NTU if C_R does not equal 1
					else
						NTU = eff / (1.0 - eff);

					double UA_calc = NTU*C_dot_min;

					UA_diff = (UA_calc - UA_PHX_od) / UA_PHX_od;

					if( abs(UA_diff) < UA_tol )
					{
						P_mc_in_conv.push_back(P_mc_in_guess);
						f_recomp_conv.push_back(f_recomp_guess);
						N_mc_conv.push_back(N_mc_guess);
						eta_conv.push_back(ms_rc_cycle.get_od_solved()->m_eta_thermal);
						T_htf_cold_conv.push_back(T_htf_hot - Q_dot_PHX / C_dot_htf);
						Q_PHX_conv.push_back(Q_dot_PHX);
						P_mc_out_conv.push_back(ms_rc_cycle.get_od_solved()->m_pres[2-1]);
					}
				}

			}

		}
		*/

		return 0;
	}

	virtual int call(double time, double step, int ncall)
	{
		double T_htf_hot = value(I_T_HTF_HOT) + 273.15;			//[K] Hot HTF temp from the receiver, convert from C
		double m_dot_htf = value(I_M_DOT_HTF)/3600.0;			//[kg/s] Mass flow rate of htf from receiver, convert from kg/s
		m_standby_control = (int)value(I_STANDBY_CONTROL);		//[-] Standby control from the controller
		double T_db = value(I_T_DB) + 273.15;					//[K] Dry bulb temperature, convert from C
		double P_amb = value(I_P_AMB)*100.0;					//[mbar] Ambient air pressure

		//**************************************************
		// Test by setting important inputs to design values
		//**************************************************
		// T_htf_hot = m_T_htf_hot;
		// m_dot_htf = m_dot_rec_des*1.15;
		// m_standby_control = 1;
		// T_db = value(P_T_AMB_DES) + 273.15;
		// P_amb = 101325.0;
		//**************************************************
		//**************************************************
		//**************************************************

		// Reset error message code
		m_error_message_code = 0;

		double W_dot_net = std::numeric_limits<double>::quiet_NaN();
		double eta_thermal = std::numeric_limits<double>::quiet_NaN();
		double T_htf_cold = std::numeric_limits<double>::quiet_NaN();
		double W_dot_par = std::numeric_limits<double>::quiet_NaN();

		double Q_dot_PHX = std::numeric_limits<double>::quiet_NaN();
		double C_dot_htf = std::numeric_limits<double>::quiet_NaN();

		switch( m_standby_control )
		{
		case 1:
		{
			C_dot_htf = m_dot_htf * m_cp_rec;

			// Assume compressor inlet temperature is always design point delta T above ambient: (T_amb_des - T_comp_in)
			// Floor is ~ critical temp + 1 = 32 C
			double T_mc_in = max(32.0 + 273.15, T_db + m_delta_T_acc);

			// Assume turbine inlet temperature is always design point delta T below receiver hot side
			double T_t_in = T_htf_hot - m_delta_T_t;

			ms_rc_od_par.m_T_mc_in = T_mc_in;
			ms_rc_od_par.m_T_t_in = T_t_in;
			ms_rc_od_par.m_N_t = ms_rc_cycle.get_design_solved()->m_N_t;
			ms_rc_od_par.m_N_sub_hxrs = ms_rc_autodes_par.m_N_sub_hxrs;
			ms_rc_od_par.m_tol = ms_rc_autodes_par.m_tol;
			
			ms_phx_od_par.m_m_dot_htf_des = m_dot_rec_des;
			ms_phx_od_par.m_T_htf_hot = T_htf_hot;
			ms_phx_od_par.m_m_dot_htf = m_dot_htf;
			ms_phx_od_par.m_UA_PHX_des = m_UA_PHX_des;
			ms_phx_od_par.m_cp_htf = m_cp_rec;

			int hx_od_error = 0;
			ms_rc_cycle.opt_od_eta_for_hx(ms_rc_od_par, ms_phx_od_par, hx_od_error);

			if( hx_od_error != 0 )
			{
				if( hx_od_error != 1 )
				{
					m_error_message_code = 1;		// Off-design model not solving
					break;
				}
				else
				{
					m_error_message_code = 2;
				}
			}

			W_dot_net = ms_rc_cycle.get_od_solved()->m_W_dot_net;
			Q_dot_PHX = ms_rc_cycle.get_od_solved()->m_Q_dot;
			T_htf_cold = T_htf_hot - Q_dot_PHX/C_dot_htf;
			eta_thermal = ms_rc_cycle.get_od_solved()->m_eta_thermal;
			W_dot_par = ACC.off_design_hx(T_db, P_amb, ms_rc_cycle.get_od_solved()->m_temp[9 - 1], ms_rc_cycle.get_od_solved()->m_pres[9 - 1],
											ms_rc_cycle.get_od_solved()->m_m_dot_mc, T_mc_in);

			break;
		}
		// End "Normal Operation" power cycle mode

		case 2:			// Standby mode
			W_dot_net = 0.0;
			T_htf_cold = m_T_htf_cold_sby;
			eta_thermal = 0.0;
			W_dot_par = 0.0;

			break;

		case 3:
		default:			// OFF
			W_dot_net = 0.0;
			T_htf_cold = m_T_htf_cold_des;;
			eta_thermal = 0.0;
			W_dot_par = 0.0;

		}		// End switch for different PC operating modes

		// Cycle off-design model not solving, but we need to return some results to controller
		if(m_error_message_code == 1)
		{
			W_dot_net = m_W_dot_net_des*(m_dot_htf / m_dot_rec_des);
			eta_thermal = ms_rc_cycle.get_design_solved()->m_eta_thermal;
			double Q_dot_PHX_guess = W_dot_net / eta_thermal;
			T_htf_cold = T_htf_hot - Q_dot_PHX_guess / C_dot_htf;
			W_dot_par = (m_dot_htf / m_dot_rec_des)*m_W_dot_fan_des;
		}

		double W_dot_net_output = W_dot_net;

		if(W_dot_net > 0.0)
		{
			if( (m_standby_control_prev==3 && m_standby_control == 1) || (m_E_su_prev + m_time_su_prev > 0.0) )
			{
				m_time_su = max(m_time_su_prev-step/3600.0, 0.0);
				if(m_E_su_prev < Q_dot_PHX*step/3600.0)		// units?  kWt-hr < kW * s * hr/s
				{
					m_E_su = 0.0;
					if(min(1.0,m_time_su_prev/(step/3600.0)) > m_E_su_prev/(Q_dot_PHX*step/3600.0))
					{
						W_dot_net_output = W_dot_net_output*(1.0 - min(1.0, m_time_su_prev / (step/3600.0)));
					}
					else
					{
						W_dot_net_output = (Q_dot_PHX*step/3600.0 - m_E_su_prev)/(step/3600.0) * eta_thermal;
					}
				}
				else
				{
					m_E_su = m_E_su_prev - Q_dot_PHX*step / 3600.0;
				}						
			}
		}

		// Set Outputs
		value(O_P_CYCLE, W_dot_net_output/1.E3);	//[MWe] 
		value(O_ETA, eta_thermal);					//[-]	
		value(O_T_HTF_COLD, T_htf_cold - 273.15);	//[C]	
		value(O_W_COOL_PAR, W_dot_par);				//[MWe]	
														 
		return 0;										 
	}													 
														 
	virtual int converged(double time)					 
	{
		if(m_standby_control == 3)
		{
			m_E_su_prev = m_startup_energy;
			m_time_su_prev = m_startup_time;
		}
		else
		{
			m_E_su_prev = m_E_su;
			m_time_su_prev = m_time_su;
		}

		m_standby_control_prev = m_standby_control;

		if(m_error_message_code == 1)
		{
			message("The off-design power cylce model did not solve. Performance values for this timestep are design point values scaled by HTF mass flow rate");
		}

		if(m_error_message_code == 2)
		{
			message("The off-design power cycle model solved, but the the PHX performance did not converge. The results at this timestep may be non-physical");
		}

		m_error_message_code = 0;

		m_is_first_t_call = true;

		return 0;
	}

};

TCS_IMPLEMENT_TYPE(sam_sco2_recomp_type424, "Integrated sco2 powerblock", "Ty Neises", 1, sam_sco2_recomp_type424_variables, NULL, 1)

