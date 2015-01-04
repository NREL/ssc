#include "core.h"

#include "ngcc_powerblock.h"

static var_info _cm_vtab_iscc_design_point[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                          UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
	{ SSC_INPUT,  SSC_NUMBER,  "ngcc_model",         "1: NREL, 2: GE",                                 "",        "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "q_pb_design",        "Design point power block thermal power",         "MWt",     "",    "",      "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "elev",               "Plant elevation",                                "m",       "",    "",      "*",     "",                "" },


	{ SSC_OUTPUT, SSC_NUMBER,  "W_dot_fossil",       "Electric output with no solar contribution",     "MWe",     "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_st_inject",        "Steam injection temp into HRSG",                 "C",       "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "q_solar_max",        "Max. solar thermal input at design",             "MWt",     "",    "",      "*",     "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_st_extract",       "Steam extraction temp from HRSG",                "C",	      "",    "",      "*",     "",                "" },

	var_info_invalid };

class cm_iscc_design_point : public compute_module
{
public:

	cm_iscc_design_point()
	{
		add_var_info(_cm_vtab_iscc_design_point);
	}

	void exec() throw(general_error)
	{
		ngcc_power_cycle cycle_calcs;

		int cycle_config = as_integer("ngcc_model");
		cycle_calcs.set_cycle_config(cycle_config);

		// Get table limits
		double T_amb_low, T_amb_high, P_amb_low, P_amb_high;
		T_amb_low = T_amb_high = P_amb_low = P_amb_high = std::numeric_limits<double>::quiet_NaN();
		cycle_calcs.get_table_range(T_amb_low, T_amb_high, P_amb_low, P_amb_high);

		// Cycle design-point conditions
		double q_pb_des = as_double("q_pb_design");   // [MWt]
		double T_amb_des = 20.0;						// [C]
		double plant_elevation = as_double("elev");		// [m]
		double P_amb_des = 101325.0*pow(1 - 2.25577E-5*plant_elevation, 5.25588) / 1.E5;	//[bar] http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html						

		// Check ambient pressure
		if( P_amb_des < P_amb_low )
		{
			P_amb_des = P_amb_low;
			//message(TCS_ERROR, "The design ambient pressure, %d, [bar] is lower than the lowest value of ambient pressure, %d [bar] in the cycle performance lookup table.", m_P_amb_des, m_P_amb_low);
			//return -1;
		}
		if( P_amb_des > P_amb_high )
		{
			P_amb_des = P_amb_high;			
			//message(TCS_ERROR, "The design ambient pressure, %d, [bar] is greater than the largest value of ambient pressure, %d [bar] in the cycle performance lookup table.", m_P_amb_des, m_P_amb_high);
			//return -1;
		}

		// Check design solar thermal input
		double q_pb_max = cycle_calcs.get_ngcc_data(0.0, T_amb_des, P_amb_des, ngcc_power_cycle::E_solar_heat_max);				//[MWt]
		if( q_pb_des > q_pb_max )
		{
			q_pb_des = q_pb_max;
			
			//message(TCS_ERROR, "The design solar thermal input, %d MWt, is greater than the ngcc can accept, %d MWt at the design ambient pressure, %d bar, and designt ambient temperature"
			//	"20 C. The HTF-steam HX was sized using the maximum solar thermal input.", m_q_sf_des, q_dot_sf_max, m_P_amb_des);
			//m_q_sf_des = q_dot_sf_max;
		}
		
		// ********************************************************************************************************
		// Get Steam Pressure, Extraction, Injection, and mass flow rate at design solar input from Regression Model
		double T_st_extract = cycle_calcs.get_ngcc_data(q_pb_des, T_amb_des, P_amb_des, ngcc_power_cycle::E_solar_extraction_t);		// [C]
		double T_st_inject = cycle_calcs.get_ngcc_data(q_pb_des, T_amb_des, P_amb_des, ngcc_power_cycle::E_solar_injection_t);			// [C]
		double W_dot_fossil = cycle_calcs.get_ngcc_data(0.0, T_amb_des, P_amb_des, ngcc_power_cycle::E_plant_power_net);
		// ********************************************************************************************************

		// Return outputs
		assign("W_dot_fossil", W_dot_fossil);
		assign("T_st_inject", T_st_inject);
		assign("q_solar_max", q_pb_max);
		assign("T_st_extract", T_st_extract);
	}


};

DEFINE_MODULE_ENTRY(iscc_design_point, "Calculates design point inject, extraction, fossil output", 0)