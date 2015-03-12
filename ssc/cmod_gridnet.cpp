#include "core.h"
#include <sstream>

static var_info vtab_grid_net[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                                           UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
// arrays should be first year hourly values 
// name kept same for now to work with existing systems and belpe
// can extend to subhourly
// what about instantaneous energy at each timestep? if we go subhourly?
	{ SSC_INPUT, SSC_ARRAY, "hourly_energy", "Energy output from system", "kWh", "", "", "*", "LENGTH=8760", "" },
	{ SSC_INPUT, SSC_ARRAY, "e_load", "Energy required by load", "kWh", "", "", "*", "LENGTH=8760", "" },
	// optional battery energy sign convention + to grid and - from grid
	{ SSC_INPUT, SSC_ARRAY, "battery_energy", "Energy output/consumption from battery", "kWh", "", "", "", "LENGTH=8760", "" },
	// optional battery degradation for out years separate from system degradation
	{ SSC_INPUT, SSC_ARRAY, "battery_degradation", "Annual battery energy degradation", "%", "", "", "", "", "" },


// future for subhourly
//		{ SSC_INPUT, SSC_ARRAY, "system_energy", "Energy output from system", "kWh", "", "", "*", "", "" },
//		{ SSC_INPUT, SSC_ARRAY, "system_load", "Energy required by load)", "kWh", "", "", "*", "", "" },


// for multi-year extensions
	{ SSC_INPUT, SSC_NUMBER, "analysis_period", "Number of years in analysis", "years", "", "", "*", "INTEGER,POSITIVE", "" },
	{ SSC_INPUT, SSC_ARRAY, "degradation", "Annual energy degradation", "%", "", "", "*", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "load_escalation", "Annual load escalation", "%/year", "", "", "?=0", "", "" },
	{ SSC_INPUT,        SSC_ARRAY,      "rate_escalation",          "Annual utility rate escalation",  "%/year", "",                      "",             "?=0",                       "",                              "" },
	



// outputs hourly for all years - system energy used for several outputs in cmod_utilityrate3b
	{ SSC_OUTPUT, SSC_ARRAY, "system_energy", "Hourly grid energy for system lifetime", "kWh", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "grid_energy", "Hourly grid energy for system lifetime", "kWh", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "grid_peak", "Hourly grid peak for system lifetime", "kW", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "system_load", "Hourly load for system lifetime", "kWh", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "system_peak", "Hourly peak load for system lifetime", "kW", "", "", "*", "", "" },



var_info_invalid };




class cm_gridnet : public compute_module
{
private:
public:
	cm_gridnet()
	{
		add_var_info( vtab_grid_net );
	}

	void exec() throw(general_error)
	{
		ssc_number_t *parr = 0;
		size_t count, i, j;

		size_t nyears = (size_t)as_integer("analysis_period");

		// compute annual system output degradation multipliers
		std::vector<ssc_number_t> sys_scale(nyears);
		parr = as_array("degradation", &count);
		if (count == 1)
		{
			for (i = 0; i < nyears; i++)
				sys_scale[i] = (ssc_number_t)pow((double)(1 - parr[0] * 0.01), (double)i);
		}
		else
		{
			for (i = 0; i < nyears && i < count; i++)
				sys_scale[i] = (ssc_number_t)(1.0 - parr[i] * 0.01);
		}
		// battery scale
		std::vector<ssc_number_t> batt_scale(nyears);
		for (i = 0; i < nyears; i++) batt_scale[i] = 1.0;
		if (is_assigned("battery_degradation"))
		{
			parr = as_array("battery_degradation", &count);
			if (count == 1)
			{
				for (i = 0; i < nyears; i++)
					batt_scale[i] = (ssc_number_t)pow((double)(1 - parr[0] * 0.01), (double)i);
			}
			else
			{
				for (i = 0; i < nyears && i < count; i++)
					batt_scale[i] = (ssc_number_t)(1.0 - parr[i] * 0.01);
			}
		}

		// compute load (electric demand) annual escalation multipliers
		std::vector<ssc_number_t> load_scale(nyears);
		parr = as_array("load_escalation", &count);
		if (count == 1)
		{
			for (i = 0; i < nyears; i++)
				load_scale[i] = (ssc_number_t)pow((double)(1 + parr[0] * 0.01), (double)i);
		}
		else
		{
			for (i = 0; i < nyears; i++)
				load_scale[i] = (ssc_number_t)(1 + parr[i] * 0.01);
		}


		// prepare 8760 arrays for load and grid values
		std::vector<ssc_number_t> e_sys(8760), p_sys(8760), e_batt(8760),
			e_load(8760), p_load(8760);

		parr = as_array("hourly_energy", &count);
		for (i = 0; i < 8760; i++)
		{
			e_sys[i] = p_sys[i] = parr[i]; // by default p_sys = e_sys (since it's hourly)
			// others are 0.0
			e_load[i] = p_load[i] = e_batt[i] = 0.0;
		}


		if (is_assigned("e_load"))
		{
			parr = as_array("e_load", &count);
			if (count != 8760) throw general_error("e_load must have 8760 values");
			for (i = 0; i < 8760; i++)
			{
				e_load[i] = -parr[i]; // input sign change 9/12/14
				p_load[i] = -parr[i]; // by default p_load = e_load
			}
		}

		if (is_assigned("battery_energy"))
		{
			parr = as_array("battery_energy", &count);
			if (count != 8760) throw general_error("battery_energy must have 8760 values");
			for (i = 0; i < 8760; i++)
				e_batt[i] = parr[i];
		}

		ssc_number_t *e_grid = allocate("grid_energy", 8760 * nyears);
		ssc_number_t *p_grid = allocate("grid_peak", 8760 * nyears);
		ssc_number_t *e_load_out = allocate("system_load", 8760 * nyears);
		ssc_number_t *p_load_out = allocate("system_peak", 8760 * nyears);
		ssc_number_t *e_system = allocate("system_energy", 8760 * nyears);


		size_t ndx = 0;
		for (i = 0; i < nyears; i++)
		{
			for (j = 0; j < 8760; j++)
			{
				// apply load escalation appropriate for current year
				e_load_out[ndx] = e_load[j] * load_scale[i];
				p_load_out[ndx] = p_load[j] * load_scale[i];

				// calculate e_grid value (e_sys + e_load)
				// note: load is assumed to have negative sign
				// energy to grid assumed to be positive and energy from grid to be negative
				e_system[ndx] = e_sys[j] * sys_scale[i];
				e_grid[ndx] = e_system[ndx] + e_load_out[ndx] 
					+ e_batt[j] * batt_scale[i];
				p_grid[ndx] = p_sys[j] * sys_scale[i] + p_load_out[ndx] 
					+ e_batt[j] * batt_scale[i];
				ndx++;
			}
		}
	}

};

DEFINE_MODULE_ENTRY( gridnet, "Compute net grid energy and load from system output and load (for battery storage testing)", 1 );


