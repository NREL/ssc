#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include "sco2_power_cycle.h"
#include "htf_props.h"
//#include <vector>
#include "CO2_properties.h"
#include "compact_hx_discretized.h"

using namespace std;

enum{	//Parameters
	P_W_dot_net_des,
	P_T_mc_in_des,
	P_T_t_in_des,
	P_N_t_des,
	P_eta_c,
	P_eta_t,
	P_P_high_limit,

	P_T_AMB_DES,
	P_FAN_POWER_PERC,
	P_DELTAP_COOL_DES,

	P_T_rec_hot,
	P_T_rec_cold,
	P_Q_dot_rec_des,
	P_rec_fl,
	P_rec_fl_props,

	//Inputs
	I_1,

	//Outputs
	O_ETA_CYCLE_DES,
	O_P_LOW_DES,
	O_F_RECOMP_DES,
	O_Q_DOT_CALC_DES,
	O_UA_RECUP_DES,
	O_T_COOLER_IN_DES,
	O_COOLER_VOLUME,

	//N_MAX
	N_MAX
};

tcsvarinfo sam_sco2_recomp_type424_variables[] = {
	//PARAMETERS
		// Cycle Design Parameters
	{ TCS_PARAM, TCS_NUMBER, P_W_dot_net_des,  "W_dot_net_des",   "Design cycle power output",                      "MW",   "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_T_mc_in_des,    "T_mc_in_des",     "Main compressor inlet temp at design",           "C",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_T_t_in_des,     "T_t_in_des",      "Turbine inlet temp at design",                   "C",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_N_t_des,        "N_t_des",         "Design turbine speed, negative links to comp.",  "rpm",  "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_eta_c,          "eta_c",           "Design compressor(s) isentropic efficiency",     "-",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_eta_t,          "eta_t",           "Design turbine isentropic efficiency",           "-",    "", "", "" },	
	{ TCS_PARAM, TCS_NUMBER, P_P_high_limit,   "P_high_limit",    "High pressure limit in cycle",                   "MPa",  "", "", "" },
		// Air-cooler Design Parameters
	{ TCS_PARAM, TCS_NUMBER, P_T_AMB_DES,      "T_amb_des",       "Design: Ambient temperature for air cooler",     "C",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_FAN_POWER_PERC, "fan_power_perc",  "Percent of net cycle power used for fan",        "%",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_DELTAP_COOL_DES,"deltaP_cool_des", "Design: CO2 deltaP thru air cooler wrt Phigh",   "%",    "", "", "" },
		// Solar Receiver Design Parameters
	{ TCS_PARAM, TCS_NUMBER, P_T_rec_hot,      "T_rec_hot",       "Tower design outlet temp",                       "C",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_T_rec_cold,     "T_rec_cold",      "Tower design inlet temp",                        "C",    "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_Q_dot_rec_des,  "Q_dot_rec_des",   "Receiver design thermal input",                  "MWt",  "", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_rec_fl,         "rec_htf",         "The name of the HTF used in the receiver",       "",     "", "", "" },
	{ TCS_PARAM, TCS_MATRIX, P_rec_fl_props,   "rec_fl_props",    "User defined rec fluid property data",           "-", "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "", "" },

	//INPUTS
	{ TCS_INPUT, TCS_NUMBER, I_1, "vwind", "Wind velocity", "m/s", "", "", "" },

	//OUTPUTS
	{ TCS_OUTPUT, TCS_NUMBER, O_ETA_CYCLE_DES,   "eta_cycle_des",       "Design: Power cycle efficiency",           "%",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P_LOW_DES,       "P_low_des",           "Design: Compressor inlet pressure",        "kPa",  "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_F_RECOMP_DES,    "f_recomp_des",        "Design: Recompression fraction",           "-",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_Q_DOT_CALC_DES,  "Q_dot_calc_des",      "Design: Calculated thermal",               "MWt",  "", "", "" },	
	{ TCS_OUTPUT, TCS_NUMBER, O_UA_RECUP_DES,    "UA_recup_des",        "Design: Recuperator conductance UA",       "kW/K", "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_COOLER_IN_DES, "T_cooler_in_des",     "Design: Cooler CO2 inlet temp",            "K",    "", "", "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_COOLER_VOLUME,   "cooler_volume",       "Estimated required cooler material vol.",  "m^3",  "", "", "" },

	//N_MAX
	{ TCS_INVALID, TCS_INVALID, N_MAX, 0, 0, 0, 0, 0, 0 }
};


class sam_sco2_recomp_type424 : public tcstypeinterface
{
private:
	// Classes and Structures
	cycle_design_parameters rc_des_par;
	RecompCycle * rc_cycle;
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
	int m_mc_type;                     // May someday have multiple compressor maps to choose from
	int m_rc_type;                     // May someday have multiple compressor maps to choose from
	vector<double> m_DP_LT;            // (cold, hot) positive values are absolute [kPa], negative values are relative (-)
	vector<double> m_DP_HT;		       // (cold, hot) positive values are absolute [kPa], negative values are relative (-)
	vector<double> m_DP_PC;		       // (cold, hot) positive values are absolute [kPa], negative values are relative (-)
	vector<double> m_DP_PHX;		   // (cold, hot) positive values are absolute [kPa], negative values are relative (-)
	int m_N_sub_hxrs;                  // [-] Number of sections to model in heat exchangers

	// Calculated Cycle Design Parameters
	double m_UA_total_des;			   // "Total UA allocatable to recuperators",           "kW/K",
	double m_T_PHX_in;                 // [K] CO2 cold inlet to Primary Heat Exchanger

	// Solar Receiver Design Parameters
	double m_T_rec_hot;              // [K] Tower design outlet temperature
	double m_T_rec_cold;             // [K] Tower design inlet temperature
	double m_Q_dot_rec_des;          // [MWt] Receiver design thermal input

	// Calculated Receiver Design Parameters
	double m_dot_rec_des;				// [kg/s] Receiver design point mass flow rate

public:
	sam_sco2_recomp_type424(tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface(cst, ti)
	{
		// Pointers
		rc_cycle = NULL;

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
		m_mc_type = 1;
		m_rc_type = 1;
		m_DP_LT.resize(2);
		fill(m_DP_LT.begin(), m_DP_LT.end(), 0.0);
		m_DP_HT.resize(2);
		fill(m_DP_HT.begin(), m_DP_HT.end(), 0.0);
		m_DP_PC.resize(2);
		fill(m_DP_PC.begin(), m_DP_PC.end(), 0.0);
		m_DP_PHX.resize(2);
		fill(m_DP_PHX.begin(), m_DP_PHX.end(), 0.0);
		m_N_sub_hxrs = 20;

		// Calculated Cycle Design Parameters
		m_UA_total_des = std::numeric_limits<double>::quiet_NaN();

		// Solar Receiver Design Parameters
		m_T_rec_hot = std::numeric_limits<double>::quiet_NaN();
		m_T_rec_cold = std::numeric_limits<double>::quiet_NaN();
		m_Q_dot_rec_des = std::numeric_limits<double>::quiet_NaN();

		// Calculated Receiver Design Parameters
		m_dot_rec_des = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_sco2_recomp_type424()
	{
		if(NULL != rc_cycle)
			delete rc_cycle;
	}

	virtual int init()
	{
		// Set parameters to member data
		m_W_dot_net_des = value(P_W_dot_net_des)*1000.0;		//[kW] Design cycle power outpt
		m_T_mc_in_des = value(P_T_mc_in_des) + 273.15;		    //[K] Compressor inlet temp at design, convert from C
		m_T_t_in_des = value(P_T_t_in_des) + 273.15;			//[K] Turbine inlet temp at design, convert from C
		m_N_t_des = value(P_N_t_des);                           // "Design turbine speed, negative links to comp.",  "rpm",  
		m_eta_c = value(P_eta_c);								// "Design compressor(s) isentropic efficiency",     "-",    
		m_eta_t = value(P_eta_t);								// "Design turbine isentropic efficiency",           "-",    		
		m_P_high_limit = value(P_P_high_limit);					// "High pressure limit in cycle",                   "MPa",  

		// Check cycle parameter values are reasonable
		if(m_T_mc_in_des <= N_co2_props::T_crit)
		{
			message("Only single phase cycle operation is allowed in this model. The compressor inlet temperature must be great than the critical temperature: %lg [C]", ((N_co2_props::T_crit)-273.15));
			return -1;
		}
		double T_mc_in_max = 70.0 + 273.15;			//[K] Arbitrary value for max compressor inlet temperature
		if(m_T_mc_in_des > T_mc_in_max)
		{
			message("The compressor inlet temperature input was %lg. This value was reset internally to the max allowable inlet temperature: %lg", m_T_mc_in_des, T_mc_in_max);
			m_T_mc_in_des = T_mc_in_max;
		}

		// Set up cycle_design_parameters structure
		// Integers for compressor maps (hardcoded)
		rc_des_par.m_mc_type = m_mc_type;
		rc_des_par.m_rc_type = m_rc_type;

		// Net output and temperatures
		rc_des_par.m_W_dot_net = m_W_dot_net_des;		//[kW]
		rc_des_par.m_T_mc_in = m_T_mc_in_des;           //[K]
		rc_des_par.m_T_t_in = m_T_t_in_des;				//[K]

		// Pressure Drops (hardcoded)
		rc_des_par.m_DP_LT = m_DP_LT;
		rc_des_par.m_DP_HT = m_DP_HT;
		rc_des_par.m_DP_PC = m_DP_PC;
		rc_des_par.m_DP_PHX = m_DP_PHX;

		// Turbine Speed
		rc_des_par.m_N_t = m_N_t_des;

		// Turbomachinery isentropic efficiency
		rc_des_par.m_eta_mc = m_eta_c;
		rc_des_par.m_eta_rc = m_eta_c;
		rc_des_par.m_eta_t = m_eta_t;

		// Number of heat exchanger sections (hardcoded)
		rc_des_par.m_N_sub_hxrs = m_N_sub_hxrs;

		// Convergence tolerances (hardcoded)
		rc_des_par.m_tol = m_tol;
		rc_des_par.m_opt_tol = m_opt_tol;

		// Upper pressure limit
		rc_des_par.m_P_high_limit = m_P_high_limit*1000.0;		// Convert to kPa
		//************************************************
		// cycle_design_parameters structure fully defined
		// EXCEPT recup UA, which must be solved with iteration below
		//************************************************

		// ************************************************************************************
		// Air-cooler specific parameters
		double T_amb_cycle_des = value(P_T_AMB_DES) + 273.15;		//[K] Ambient temperature at power cycle design, convert from C
		double fan_power_frac = value(P_FAN_POWER_PERC) / 100.0;			//[-] Fraction of cycle net power output used by cooler air fan, convert from %
		double deltaP_cooler_frac = value(P_DELTAP_COOL_DES) / 100.0;		//[-] Fraction of P_high allowed as CO2 pressure drop in air cooler design

		// **************************************************************************
		// Solar Receiver Parameters
		// Receiver inlet/outlet temps and thermal input
		m_T_rec_hot = value(P_T_rec_hot) + 273.15;		//[K] Tower outlet temp at design, convert from C
		m_T_rec_cold = value(P_T_rec_cold) + 273.15;	//[K] Tower inlet temp at design, convert from C
		m_Q_dot_rec_des = value(P_Q_dot_rec_des);		//[MWt] Receiver thermal input at design

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
		double deltaT_co2_hot = m_T_rec_hot - m_T_t_in_des;			//[K]
		m_T_PHX_in = m_T_rec_cold - deltaT_co2_hot;					//[K]

		// Now need to vary UA_recup until m_T_PHX_in is achieved. This could be slow...
		// Is there a good way to calculate a decent guess???
		// 1) Estimate co2 mass flow rate from PHX energy balance
		double T_PHX_cold_ave = 0.5*(m_T_PHX_in + m_T_t_in_des);
		co2_error = CO2_TP(T_PHX_cold_ave, m_P_high_limit*1.E3, &co2_props);
		double cp_cold_ave = co2_props.cp;			//[kJ/kg-K]
		double m_dot_co2_guess = m_Q_dot_rec_des*1.E3 / (cp_cold_ave*(m_T_t_in_des - m_T_PHX_in));	//[kg/s]
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

		// **********************************************************************
		// **********************************************************************
		// Solve design point model with guessed recups UA
		m_UA_total_des = UA_recups_guess;
		rc_des_par.m_UA_rec_total = m_UA_total_des;
		// Create new instance of RecompCycle class and assign to member point
		rc_cycle = new RecompCycle(rc_des_par);
		bool auto_cycle_success = rc_cycle->auto_optimal_design();
		double T_PHX_in_calc = rc_cycle->get_cycle_design_metrics()->m_T[5 - 1];
		// **********************************************************************

		// Now need to iterate UA_total_des until T_PHX_in_calc = m_T_PHX_in
		double diff_T_PHX_in = (T_PHX_in_calc - m_T_PHX_in)/m_T_PHX_in;			//[-]
		bool low_flag = false;
		bool high_flag = false;
		double y_upper = numeric_limits<double>::quiet_NaN();
		double y_lower = numeric_limits<double>::quiet_NaN();
		double x_upper = numeric_limits<double>::quiet_NaN();
		double x_lower = numeric_limits<double>::quiet_NaN();
		int opt_des_calls = 1;

		while( abs(diff_T_PHX_in) > m_tol )
		{
			opt_des_calls++;

			if(diff_T_PHX_in > 0.0)		// Calc > target, UA is too large, decrease UA
			{
				low_flag = true;
				x_lower = UA_recups_guess;
				y_lower = diff_T_PHX_in;

				if(high_flag)	// Upper and lower bounds set, use false positon interpolation method
				{
					UA_recups_guess = -y_upper*(x_lower-x_upper)/(y_lower-y_upper) + x_upper;
				}
				else			// No upper bound set, try to get there
				{
					UA_recups_guess *= 0.6;
				}
			}
			else						// Calc < target, UA is too small, decrease UA
			{
				high_flag = true;
				x_upper = UA_recups_guess;
				y_upper = diff_T_PHX_in;
				
				if(low_flag)
				{
					UA_recups_guess = -y_upper*(x_lower - x_upper) / (y_lower - y_upper) + x_upper;
				}
				else
				{
					UA_recups_guess *= 1.1;
				}
			}

			// Solve design point model with guessed recups UA
			m_UA_total_des = UA_recups_guess;
			rc_des_par.m_UA_rec_total = m_UA_total_des;
			// Create new instance of RecompCycle class and assign to member point
			rc_cycle = new RecompCycle(rc_des_par);
			auto_cycle_success = rc_cycle->auto_optimal_design();
			T_PHX_in_calc = rc_cycle->get_cycle_design_metrics()->m_T[5 - 1];
			// **********************************************************************

			// Now need to iterate UA_total_des until T_PHX_in_calc = m_T_PHX_in
			diff_T_PHX_in = (T_PHX_in_calc - m_T_PHX_in) / m_T_PHX_in;			//[-]
		}

		// Design metrics
		double design_eta = rc_cycle->get_cycle_design_metrics()->m_eta_thermal;
		double W_dot_net_des_calc = rc_cycle->get_cycle_design_metrics()->m_W_dot_net;		// Can compare to target net output to check convergence
		double q_dot_des = m_W_dot_net_des/design_eta;							//[kW]
		double m_dot_des = rc_cycle->get_cycle_design_metrics()->m_m_dot_PHX;		//[kg/s]
		double P_PHX_out = rc_cycle->get_cycle_design_metrics()->m_P[6 - 1];	//[kPa]
		double P_PHX_in = rc_cycle->get_cycle_design_metrics()->m_P[5 - 1];		//[kPa]

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
		double T_rec_ave = 0.5*(m_T_rec_cold + m_T_rec_hot);		//[K]
		double cp_rec = rec_htfProps.Cp(T_rec_ave);					//[kJ/kg-K]
		double m_dot_hot = q_dot_des / (cp_rec*(m_T_rec_hot - m_T_rec_cold));	//[kg/s]
		
			// Cycle/cold side
		double T_PHX_co2_ave = 0.5*(m_T_t_in_des + m_T_PHX_in);		//[K]
		co2_error = CO2_TP(T_PHX_co2_ave, P_PHX_in, &co2_props);	
		double cp_PHX_co2 = q_dot_des/ (m_dot_des*(m_T_t_in_des - m_T_PHX_in));

			// Because C_dot_c = C_dot_h, q_dot_max = 
		double q_dot_max = m_dot_hot*cp_rec*(m_T_rec_hot - m_T_PHX_in);		//[kW]

			// Effectiveness & NTU
		double eff_des = q_dot_des / q_dot_max;
		double NTU = eff_des/(1.0 - eff_des);

			// UA
		double UA_PHX_des = NTU * m_dot_hot * cp_rec;

		// *****************************************************************
		// Call Air Cooled Condenser
		// *****************************************************************

		double P_amb_cycle_des = 101325.0;			//[Pa]

		double T_acc_in = rc_cycle->get_cycle_design_metrics()->m_T[9 - 1];
		double P_acc_in = rc_cycle->get_cycle_design_metrics()->m_P[9 - 1];
		double m_dot_acc_in = rc_cycle->get_cycle_design_metrics()->m_m_dot_PC;
		
		double W_dot_fan_des = fan_power_frac*m_W_dot_net_des/1000.0;	//[MW] Cooler air fan power at design
		
		double deltaP_des = deltaP_cooler_frac*rc_cycle->get_cycle_design_metrics()->m_P[2 - 1];
				
		double T_acc_out = m_T_mc_in_des;

		// Call air-cooler design method
		ACC.design_hx(T_amb_cycle_des, P_amb_cycle_des, T_acc_in, P_acc_in, m_dot_acc_in,
			W_dot_fan_des, deltaP_des, T_acc_out);

		// Get air-cooler design parameters
		// compact_hx::S_hx_design_solved s_hx_design_solved;
		
		// Write outputs
		value(O_ETA_CYCLE_DES, design_eta);
		value(O_P_LOW_DES, rc_cycle->get_cycle_design_metrics()->m_P[1 - 1]);
		value(O_F_RECOMP_DES, rc_cycle->get_cycle_design_parameters()->m_recomp_frac);
		value(O_Q_DOT_CALC_DES, q_dot_des);
		value(O_UA_RECUP_DES, m_UA_total_des);
		value(O_T_COOLER_IN_DES, T_acc_in);
		value(O_COOLER_VOLUME, ACC.get_hx_design_solved()->m_material_V);

		return 0;
	}

	virtual int call(double time, double step, int ncall)
	{
		return 0;
	}

	virtual int converged(double time)
	{

		return 0;
	}

};

TCS_IMPLEMENT_TYPE(sam_sco2_recomp_type424, "Integrated sco2 powerblock", "Ty Neises", 1, sam_sco2_recomp_type424_variables, NULL, 1)

