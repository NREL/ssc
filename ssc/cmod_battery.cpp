#include <math.h>

#include "common.h"
#include "core.h"

#include "lib_util.h"
#include "lib_battery.h"
#include "lib_weatherfile.h"

/* -------------------------------------
-------------------------------------- */

#ifndef M_PI
#define M_PI 3.141592653589793238462643
#endif

static var_info _cm_vtab_battery[] = {
	/*   VARTYPE           DATATYPE         NAME                      LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,		SSC_STRING,		"solar_resource_file",	"local weather file path",				"",			"",						"Weather",		"*",						"LOCAL_FILE",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"q20",					"Capacity at 20-hour discharge rate",	"Ah",		"",						"Battery",		"*",						"",									"" },
	{ SSC_INPUT,		SSC_NUMBER,		"q2",					"Capacity at discharge rate for t=t2",	"Ah",		"",						"Battery",		"*",						"",									"" },
	{ SSC_INPUT,		SSC_NUMBER,		"q1",					"Capacity at discharge rate for t=t1",	"Ah",		"",						"Battery",		"*",						"",									"" },
	{ SSC_INPUT,		SSC_NUMBER,		"I20",					"Current at 20-hour discharge rate",	"Ah",		"",						"Battery",		"*",						"",									"" },
	{ SSC_INPUT,		SSC_NUMBER,		"t1",					"Time to discharge",					"h",		"",						"Battery",		"*",						"",									"" },
	{ SSC_INPUT,		SSC_NUMBER,		"t2",					"Time to discharge",					"h",		"",						"Battery",		"*",						"",									"" },
	{ SSC_INPUT,		SSC_NUMBER,		"V20",					"Voltage at 20 hour discharge rate",	"V",		"",						"Battery",		"*",						"",									"" },
	{ SSC_INPUT,		SSC_NUMBER,		"R",					"Battery Internal Resistance",			"Ohm",		"",						"Battery",		"*",						"",									"" },
	{ SSC_INPUT,		SSC_ARRAY,		"DOD_vect",				"Depth of Discharge Curve Fit",			"",			"",						"Battery",		"*",						"",									"" },
	{ SSC_INPUT,		SSC_ARRAY,		"cycle_vect",			"Cycles to Failure Curve Fit",			"",			"",						"Battery",		"*",						"",									"" },
	{ SSC_INOUT,		SSC_ARRAY,		"hourly_energy",		"Hourly energy",						"kWh",		"",						"Time Series",	"*",						"LENGTH=8760",						"" },
	{ SSC_INOUT,		SSC_ARRAY,		"e_load",				"Electric load",						"kWh",		"",						"Load Profile Estimator", "",		"LENGTH=8760",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"pv.storage.p1.charge", "Period 1 Charging Allowed?", "", "", "Battery", "*", "", "" },
	{ SSC_INPUT,		SSC_NUMBER,		"pv.storage.p2.charge", "Period 2 Charging Allowed?", "", "", "Battery", "*", "", "" },
	{ SSC_INPUT,		SSC_NUMBER,		"pv.storage.p3.charge", "Period 3 Charging Allowed?", "", "", "Battery", "*", "", "" },
	{ SSC_INPUT,		SSC_NUMBER,		"pv.storage.p4.charge", "Period 4 Charging Allowed?", "", "", "Battery", "*", "", "" },
	{ SSC_INPUT,		SSC_NUMBER,		"pv.storage.p1.discharge", "Period 1 Discharging Allowed?", "", "", "Battery", "*", "", "" },
	{ SSC_INPUT,		SSC_NUMBER,		"pv.storage.p2.discharge", "Period 2 Discharging Allowed?", "", "", "Battery", "*", "", "" },
	{ SSC_INPUT,		SSC_NUMBER,		"pv.storage.p3.discharge", "Period 3 Discharging Allowed?", "", "", "Battery", "*", "", "" },
	{ SSC_INPUT,		SSC_NUMBER,		"pv.storage.p4.discharge", "Period 4 Discharging Allowed?", "", "", "Battery", "*", "", "" },
	{ SSC_INPUT,		SSC_NUMBER,		"pv.storage.p1.gridcharge", "Period 1 Grid Charging Allowed?", "", "", "Battery", "*", "", "" },
	{ SSC_INPUT,		SSC_NUMBER,		"pv.storage.p2.gridcharge", "Period 2 Grid Charging Allowed?", "", "", "Battery", "*", "", "" },
	{ SSC_INPUT,		SSC_NUMBER,		"pv.storage.p3.gridcharge", "Period 3 Grid Charging Allowed?", "", "", "Battery", "*", "", "" },
	{ SSC_INPUT,		SSC_NUMBER,		"pv.storage.p4.gridcharge", "Period 4 Grid Charging Allowed?", "", "", "Battery", "*", "", "" },
	{ SSC_INPUT,		SSC_MATRIX,		"pv_storage_sched", "Battery Dispatch Schedule", "", "", "Battery", "*", "", "" },

	// economic inputs
	{ SSC_INOUT, SSC_NUMBER, "analysis_period", "Number of years in analysis", "years", "", "", "*", "INTEGER,POSITIVE", "" },
	{ SSC_INOUT, SSC_ARRAY, "degradation", "Annual energy degradation", "%", "", "AnnualOutput", "*", "", "" },
	{ SSC_INOUT, SSC_ARRAY, "load_escalation", "Annual load escalation", "%/year", "", "", "?=0", "", "" },
	{ SSC_INOUT, SSC_ARRAY, "rate_escalation", "Annual utility rate escalation", "%/year", "", "", "?=0", "", "" },
	{ SSC_INOUT, SSC_NUMBER, "inflation_rate", "Inflation rate", "%", "", "Financials", "*", "MIN=0,MAX=100", "" },

	
	{ SSC_OUTPUT, SSC_ARRAY, "q0", "Total Charge", "Ah", "", "Annual", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "q1", "Available Charge", "Ah", "", "Annual", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "q2", "Bound Charge", "Ah", "", "Annual", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "SOC",					"State of Charge",						"%",        "",						"Annual",       "*",						"LENGTH=8760",						"" },
	{ SSC_OUTPUT, SSC_ARRAY, "DOD", "Depth of Discharge", "%", "", "Annual", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "qmaxI", "Max Capacity at Current", "Ah", "", "Annual", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "I", "Current", "A", "", "Annual", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "Damage", "Fractional Damage", "", "", "Annual", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "Cycles", "Number of Cycles", "", "", "Annual", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "battery_energy", "Power to/from Battery", "kWh", "", "Annual", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "grid_energy", "Power from Grid to Battery", "kWh", "", "Annual", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "Dispatch_mode", "Dispatch Mode for Hour", "", "", "Annual", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "Dispatch_profile", "Dispatch Profile for Hour", "", "", "Annual", "*", "LENGTH=8760", "" },



	var_info_invalid };

class cm_battery : public compute_module
{
public:

	cm_battery()
	{
		add_var_info(_cm_vtab_battery);
		// add_var_info(vtab_adjustment_factors);
	}

	void exec() throw(general_error)
	{
		/* **********************************************************************
		Read user specified system parameters from compute engine
		********************************************************************** */
		
		/* Battery Properties */
		double q20 = as_double("q20"); // [Ah]
		double q2 = as_double("q2"); //   [Ah]
		double q1 = as_double("q1"); //   [Ah]
		int I20 = as_double("I20"); //	  [A]
		int t1 = as_double("t1"); //	  [h]
		int t2 = as_double("t2"); //	  [h]
		int V20 = as_double("V20"); //	  [V]
		int R = as_double("R"); //		  [Ohm]

		/* Dispatch Timing Control*/
		size_t months = 12;
		size_t hours = 24;
		bool can_charge_array[4];
		bool can_discharge_array[4];
		bool grid_charge_array[4];
		
		can_charge_array[0] = as_boolean("pv.storage.p1.charge");
		can_charge_array[1] = as_boolean("pv.storage.p2.charge");
		can_charge_array[2] = as_boolean("pv.storage.p3.charge");
		can_charge_array[3] = as_boolean("pv.storage.p4.charge");
		can_discharge_array[0] = as_boolean("pv.storage.p1.discharge");
		can_discharge_array[1] = as_boolean("pv.storage.p2.discharge");
		can_discharge_array[2] = as_boolean("pv.storage.p3.discharge");
		can_discharge_array[3] = as_boolean("pv.storage.p4.discharge");
		grid_charge_array[0] = as_boolean("pv.storage.p1.gridcharge");
		grid_charge_array[1] = as_boolean("pv.storage.p2.gridcharge");
		grid_charge_array[2] = as_boolean("pv.storage.p3.gridcharge");
		grid_charge_array[3] = as_boolean("pv.storage.p4.gridcharge");
		ssc_number_t * pv_storage_schedule = as_matrix("pv_storage_sched", &months, &hours);
		util::matrix_t<float> schedule(12, 24);
		schedule.assign(pv_storage_schedule, months, hours);

		/* arrays */
		size_t len;
		ssc_number_t *hourly_energy = as_array("hourly_energy", &len);
		if (len != 8760) throw exec_error("battery", "hourly energy must have 8760 values");
		
		ssc_number_t *e_load = as_array("e_load", &len);
		if (len != 8760) throw exec_error("battery", "load profile must have 8760 values");


		size_t numberOfPoints1, numberOfPoints2;
		std::vector<double> DOD_vect = as_doublevec("DOD_vect");
		std::vector<double> cycle_vect = as_doublevec("cycle_vect");
		numberOfPoints1 = DOD_vect.size();
		numberOfPoints2 = DOD_vect.size();
		if (numberOfPoints1 != numberOfPoints2) throw exec_error("battery", "Number of Cycles-to-Failure inputs must equal Depth-of-Discharge inputs");		

		/* **********************************************************************
		Read weather file to get subhourly
		********************************************************************** */
		const char *file = as_string("solar_resource_file");

		weatherfile wf(file);
		if (!wf.ok()) throw exec_error("battery", wf.error_message());

		size_t nrec = wf.nrecords;
		size_t step_per_hour = nrec / 8760;
		if (step_per_hour < 1 || step_per_hour > 60 || step_per_hour * 8760 != nrec)
			throw exec_error("swh", util::format("invalid number of data records (%d): must be an integer multiple of 8760", (int)nrec));

		double ts_hour = 1.0 / step_per_hour;
		double ts_sec = 3600.0 / step_per_hour;

		/* **********************************************************************
		Initialize outputs
		********************************************************************** */
		ssc_number_t *outTotalCharge = allocate("q0", nrec);
		ssc_number_t *outAvailableCharge = allocate("q1", nrec);
		ssc_number_t *outBoundCharge = allocate("q2", nrec);
		ssc_number_t *outSOC = allocate("SOC", nrec);
		ssc_number_t *outDOD = allocate("DOD", nrec);
		ssc_number_t *outMaxChargeAtCurrent = allocate("qmaxI", nrec);
		ssc_number_t *outCurrent = allocate("I", nrec);
		ssc_number_t *outDamage = allocate("Damage", nrec);
		ssc_number_t *outCycles = allocate("Cycles", nrec);
		ssc_number_t *outBatteryEnergy = allocate("battery_energy", nrec);
		ssc_number_t *outGridEnergy = allocate("grid_energy", nrec); // Net grid energy required.  Positive indicates putting energy on grid.  Negative indicates pulling off grid
		ssc_number_t *outDispatchMode = allocate("Dispatch_mode", nrec);
		ssc_number_t *outDispatchProfile = allocate("Dispatch_profile", nrec);

		/* *********************************************************************************************
		Battery and Model Initialization
		*********************************************************************************************** */
		int mode = 0;
		double annual_wh = 0.0;
		size_t hour = 0;
		double SOC;
		double dt = ts_hour; 
		capacity_kibam_t CapacityModel(q20, I20, V20, t1, t2, q1, q2);
		lifetime_t LifetimeModel(DOD_vect, cycle_vect, numberOfPoints1);
		battery_t Battery(&CapacityModel,&LifetimeModel, dt);
		output* LifetimeOutput;
		output* CapacityOutput;

		/* *********************************************************************************************
		Storage Dispatch Initialization
		*********************************************************************************************** */
		int profile;
		bool can_charge, can_discharge, grid_charge;
		double p_grid=0.;			// energy needed from grid to charge battery
		double p_tofrom_batt=0.;	// energy transferred to/from the battery.  Positive indicates discharging, Negative indicates charging
		int month;
		int curr_hour;

		/* *********************************************************************************************
		Run Simulation
		*********************************************************************************************** */

		for (hour = 0; hour < 8760; hour++)
		{
			// Get Dispatch Settings for current hour
			// Remotely possible this could need to be updated for sub-hourly profiles
			getMonthHour(hour, &month, &curr_hour);
			profile = schedule.at(month - 1, curr_hour - 1);
			can_charge = can_charge_array[profile - 1];
			can_discharge = can_discharge_array[profile - 1];
			grid_charge = grid_charge_array[profile - 1];

			for (size_t jj = 0; jj<step_per_hour; jj++)
			{

				// current charge state of battery
				double chargeNeededToFill = Battery.chargeNeededToFill();				// [Ah]
				double powerNeededToFill = (chargeNeededToFill * V20)*watt_to_kilowatt;	// [kWh]
				double current_charge = Battery.getCurrentCharge();						// [Ah]
				double current_power = (current_charge * V20) *watt_to_kilowatt;		// [KWh]

				// Is there extra energy from array
				if (hourly_energy[hour] > e_load[hour])
				{
					if (can_charge)
					{
						if (hourly_energy[hour] - e_load[hour] > (powerNeededToFill))
						{
							p_tofrom_batt = -powerNeededToFill;
							hourly_energy[hour] += p_tofrom_batt;
						}
						else
						{
							// check if we want to charge from the grid
							if (grid_charge)
							{
								p_tofrom_batt = -powerNeededToFill;
								hourly_energy[hour] += (-(hourly_energy[hour] - e_load[hour]));
								p_grid = powerNeededToFill - (hourly_energy[hour] - e_load[hour]);
							}
							else
							{
								p_tofrom_batt = -(hourly_energy[hour] - e_load[hour]);
								hourly_energy[hour] += p_tofrom_batt;
							}
						}
					}
					// if we want to charge from grid without charging from array
					else if (grid_charge)
					{
						p_tofrom_batt = -powerNeededToFill;
						p_grid = powerNeededToFill;
					}

				}
				// Or, is the demand greater than or equal to what the array provides
				else if (e_load[hour] >= hourly_energy[hour])
				{
					if (can_discharge)
					{
						if (e_load[hour] - hourly_energy[hour] > current_power)
						{
							p_tofrom_batt = current_power;
							hourly_energy[hour] += p_tofrom_batt;
						}
						else
						{
							p_tofrom_batt = e_load[hour] - hourly_energy[hour];
							hourly_energy[hour] += p_tofrom_batt;
						}
					}
					// if we want to charge from grid
					else if (grid_charge)
					{
						p_tofrom_batt = -powerNeededToFill;
						p_grid = powerNeededToFill;
					}
				}

				// Run Battery Model to update charge based on charge/discharge
				// Battery.run(power[hour], V20);
				Battery.run(kilowatt_to_watt*p_tofrom_batt, V20);
				// Battery.run(0, V20);
				CapacityOutput = Battery.getCapacityOutput();
				LifetimeOutput = Battery.getLifetimeOutput();


				// save output variables 
				outTotalCharge[hour] = (ssc_number_t)(CapacityOutput[TOTAL_CHARGE].value);
				outAvailableCharge[hour] = (ssc_number_t)(CapacityOutput[AVAILABLE_CHARGE].value);
				outBoundCharge[hour] = (ssc_number_t)(CapacityOutput[BOUND_CHARGE].value);
				outSOC[hour] = (ssc_number_t)(CapacityOutput[STATE_OF_CHARGE].value);
				outDOD[hour] = (ssc_number_t)(CapacityOutput[DEPTH_OF_DISCHARGE].value);
				outMaxChargeAtCurrent[hour] = (ssc_number_t)(CapacityOutput[MAX_CHARGE_AT_CURRENT].value);
				outCurrent[hour] = (ssc_number_t)(CapacityOutput[CURRENT].value);
				outDamage[hour] = (ssc_number_t)(LifetimeOutput[FRACTIONAL_DAMAGE].value);
				outCycles[hour] = (int)(LifetimeOutput[NUMBER_OF_CYCLES].value);
				outBatteryEnergy[hour] = p_tofrom_batt;
				outGridEnergy[hour] = hourly_energy[hour] - e_load[hour];
				outDispatchProfile[hour] = profile;
			}
		}
		// Have to finish up the lifetime model
		Battery.finish();
		LifetimeOutput = Battery.getLifetimeOutput();

		outDamage[hour - 1] = (ssc_number_t)(LifetimeOutput[FRACTIONAL_DAMAGE].value);
		outCycles[hour - 1] = (int)(LifetimeOutput[NUMBER_OF_CYCLES].value);
	}

};

DEFINE_MODULE_ENTRY(battery, "Battery storage standalone model .", 10)
