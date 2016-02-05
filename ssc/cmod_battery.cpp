#include <math.h>

#include "common.h"
#include "core.h"
#include "lib_util.h"
#include "cmod_battery.h"




var_info vtab_battery[] = {
/*   VARTYPE           DATATYPE         NAME                                            LABEL                                                   UNITS      META                             GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	
	// simulation inputs - required only if lifetime analysis
	{ SSC_INPUT,        SSC_NUMBER,      "pv_lifetime_simulation",                     "PV lifetime simulation",                                  "0/1",     "",                     "",             "?=0",                        "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,      "analysis_period",                            "Lifetime analysis period",                                "years",   "",                     "",             "pv_lifetime_simulation=1",   "",                               "" },

		// configuration inputs
//  { SSC_INPUT,        SSC_NUMBER,      "batt_ac_or_dc",                              "Battery interconnection (AC or DC)",                      "dc=0,ac=1",  "",                  "Battery",       "",                           "",                              "" },
//	{ SSC_INPUT,        SSC_NUMBER,      "batt_dc_dc_efficiency",                      "PV DC to battery DC efficiency",                          "",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_dc_ac_efficiency",                      "Battery DC to AC efficiency",                             "",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_ac_dc_efficiency",                      "Inverter AC to battery DC efficiency",                    "",        "",                     "Battery",       "",                           "",                              "" },

	// generic battery inputs
	{ SSC_INPUT,        SSC_NUMBER,      "batt_computed_strings",                      "Number of strings of cells",                              "",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_computed_series",                       "Number of cells in series",                               "",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_computed_bank_capacity",                "Computed bank capacity",                                  "kWh",     "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_chem",                                  "Battery chemistry",                                       "",        "0=LeadAcid,1=LiIon",   "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_minimum_SOC",		                   "Minimum allowed state-of-charge",                         "V",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_maximum_SOC",                           "Minimum allowed state-of-charge",                         "V",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_current_charge_max",                    "Maximum charge current",                                  "A",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_current_discharge_max",                 "Maximum discharge current",                               "A",       "",                     "Battery",       "",                           "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_minimum_modetime",                      "Minimum time at charge state",                            "min",     "",                     "Battery",       "",                           "",                              "" },

	// Voltage discharge curve
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Vfull",                                 "Fully charged cell voltage",                              "V",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Vexp",                                  "Cell voltage at end of exponential zone",                 "V",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Vnom",                                  "Cell voltage at end of nominal zone",                     "V",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Vnom_default",                          "Default nominal cell voltage",                            "V",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Qfull",                                 "Fully charged cell capacity",                             "Ah",      "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Qexp",                                  "Cell capacity at end of exponential zone",                "Ah",      "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Qnom",                                  "Cell capacity at end of nominal zone",                    "Ah",      "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_C_rate",                                "Rate at which voltage vs. capacity curve input",          "",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_resistance",                            "Internal resistance",                                     "Ohm",     "",                     "Battery",       "",                           "",                              "" },

	// lead-acid inputs
	{ SSC_INPUT,		SSC_NUMBER,		"LeadAcid_q20_computed",	                   "Capacity at 20-hour discharge rate",                     "Ah",       "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,		SSC_NUMBER,		"LeadAcid_q10_computed",	                   "Capacity at 10-hour discharge rate",                     "Ah",       "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,		SSC_NUMBER,		"LeadAcid_qn_computed",	                       "Capacity at discharge rate for n-hour rate",             "Ah",       "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,		SSC_NUMBER,		"LeadAcid_tn",	                               "Time to discharge",                                      "h",        "",                     "Battery",       "",                           "",                             "" },
																																																						     
	// lifetime inputs
	{ SSC_INPUT,		SSC_MATRIX,     "batt_lifetime_matrix",                        "Cycles vs capacity at different depths-of-discharge",    "",         "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_replacement_capacity",                   "Capacity degradation at which to replace battery",       "%",        "",                     "Battery",       "",                           "",                             "" },

	// thermal inputs
	{ SSC_INPUT,        SSC_NUMBER,     "batt_mass",                                   "Battery mass",                                           "kg",       "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_length",                                 "Battery length",                                         "m",        "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_width",                                  "Battery width",                                          "m",        "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_height",                                 "Battery height",                                         "m",        "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_Cp",                                     "Battery specific heat capacity",                         "J/KgK",    "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_h_to_ambient",                           "Heat transfer between battery and environment",          "W/m2K",    "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "T_room",                                      "Temperature of storage room",                            "C",        "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_MATRIX,     "cap_vs_temp",                                 "Effective capacity as function of temperature",          "C,%",      "",                     "Battery",       "",                           "",                             "" },

	// storage dispatch
	{ SSC_INPUT,        SSC_ARRAY,      "dispatch_manual_charge",                      "Periods 1-6 charging allowed?",                          "",         "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,      "dispatch_manual_discharge",                   "Periods 1-6 discharging allowed?",                       "",         "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,      "dispatch_manual_gridcharge",                  "Periods 1-6 grid charging allowed?",                     "",         "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,      "dispatch_manual_percent_discharge",           "Periods 1-6 discharge percent",                          "%",        "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,      "dispatch_manual_percent_gridcharge",          "Periods 1-6 gridcharge percent",                         "%",        "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_MATRIX,     "dispatch_manual_sched",                       "Battery dispatch schedule for weekday",                  "",         "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_MATRIX,     "dispatch_manual_sched_weekend",               "Battery dispatch schedule for weekend",                  "",         "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,      "batt_target_power",                           "Grid target power for every time step",                  "kW",       "",                     "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,      "batt_target_power_monthly",                   "Grid target power on monthly basis",                     "kW",       "",                     "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_target_choice",                          "Target power input option",                              "0/1",      "",                     "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_dispatch_choice",                        "Battery dispatch algorithm",                             "0/1/2",    "",                     "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_pv_choice",                              "Prioritize PV usage for load or battery",                "0/1",      "",                     "Battery",       "?=0",                        "",                             "" },


	// Capacity, Voltage, Charge outputs
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_q0",                                    "Battery total charge",                                   "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_q1",                                    "Battery available charge",                               "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_q2",                                    "Battery bound charge",                                   "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_SOC",                                   "Battery state of charge",                                "%",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_DOD",                                   "Battery cycle depth of discharge",                       "%",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_qmaxI",                                 "Battery maximum capacity at current",                        "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_qmax",                                  "Battery maximum charge",                                     "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_I",                                     "Battery current",                                        "A",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_voltage_cell",                          "Battery cell voltage",                                   "V",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_voltage",                               "Battery voltage",	                                     "V",        "",                     "Battery",       "",                           "",                              "" },
																		               
	// Lifecycle related outputs											             
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_cycles",                                "Battery number of cycles",                               "",         "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_temperature",                           "Battery temperature",                                    "C",        "",                     "Battery",       "",                           "",                              "" }, 
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_capacity_percent",                      "Battery capacity percent for lifetime",                  "%",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_capacity_thermal_percent",              "Battery capacity percent for temperature",               "%",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_bank_replacement",                      "Battery bank replacements per year",                     "number/year", "",                  "Battery",       "",                           "",                              "" },
																			          
	// Energy outputs	- Power outputs at native time step													        
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_power",                                 "Power to/from battery",                                 "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "grid_power",                                 "Power to/from grid",                                    "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "pv_batt_gen",                                "Power of PV+battery",                                   "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "pv_to_load",                                 "Power to load from PV",                                 "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_to_load",                               "Power to load from battery",                            "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "grid_to_load",                               "Power to load from grid",                               "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "pv_to_batt",                                 "Power to battery from PV",                              "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "grid_to_batt",                               "Power to battery from grid",                            "kW",      "",                       "Battery",       "",                           "",                              "" },

	// monthly outputs
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_pv_to_load",                         "Energy to load from PV",                                "kWh",      "",                      "Battery",       "",                          "LENGTH=12",                     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_batt_to_load",                       "Energy to load from battery",                           "kWh",      "",                      "Battery",       "",                          "LENGTH=12",                     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_grid_to_load",                       "Energy to load from grid",                              "kWh",      "",                      "Battery",       "",                          "LENGTH=12",                     "" },
	
	// annual metrics													          
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_annual_charge_from_pv",                 "Battery annual energy charged from pv",                 "kWh",      "",                      "Battery",       "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_annual_charge_from_grid",               "Battery annual energy charged from grid",               "kWh",      "",                      "Battery",       "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_annual_charge_energy",                  "Battery annual energy charged",                         "kWh",      "",                      "Battery",       "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_annual_discharge_energy",               "Battery annual energy discharged",                      "kWh",      "",                      "Battery",       "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_annual_energy_loss",                    "Battery annual energy loss",                            "kWh",      "",                      "Battery",       "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "annual_export_to_grid_energy",               "Annual energy exported to grid",                        "kWh",      "",                      "Battery",       "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "annual_import_to_grid_energy",               "Annual energy imported from grid",                      "kWh",      "",                      "Battery",       "",                           "",                               "" },
	
	// single value metrics
	{ SSC_OUTPUT,        SSC_NUMBER,     "average_cycle_efficiency",                   "Battery average cycle efficiency",                      "%",        "",                      "Annual",        "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "batt_pv_charge_percent",                     "Battery percent energy charged from PV",                "%",        "",                      "Annual",        "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "batt_bank_installed_capacity",               "Battery bank installed capacity",                       "kWh",      "",                      "Annual",        "",                           "",                               "" },

	// test matrix output
	{ SSC_OUTPUT,        SSC_MATRIX,     "batt_dispatch_sched",                        "Battery dispatch schedule",                              "",         "",                     "Battery",       "",                           "",                               "ROW_LABEL=MONTHS,COL_LABEL=HOURS_OF_DAY"  },
	

var_info_invalid };




battstor::battstor( compute_module &cm, bool setup_model, int replacement_option, size_t nrec, double dt_hr )
{
	// component models
	voltage_model = 0;
	lifetime_model = 0;
	thermal_model = 0;
	battery_model = 0;
	capacity_model = 0;
	dispatch_model = 0;
	losses_model = 0;

	// outputs
	outTotalCharge = 0;
	outAvailableCharge = 0;
	outBoundCharge = 0;
	outMaxChargeAtCurrent = 0;
	outMaxCharge = 0;
	outSOC = 0;
	outDOD = 0;
	outCurrent = 0;
	outCellVoltage = 0;
	outBatteryVoltage = 0;
	outCapacityPercent = 0;
	outCycles = 0;
	outBatteryBankReplacement = 0;
	outBatteryTemperature = 0;
	outCapacityThermalPercent = 0;
	outBatteryPower = 0;
	outGridPower = 0;
	outPVToLoad = 0;
	outBatteryToLoad = 0;
	outGridToLoad = 0;
	outPVToBatt = 0;
	outGridToBatt = 0;
	outAverageCycleEfficiency = 0;
	outPVChargePercent = 0;
	outAnnualPVChargeEnergy = 0;
	outAnnualGridChargeEnergy = 0;
	outAnnualChargeEnergy = 0;
	outAnnualDischargeEnergy = 0;
	outAnnualGridImportEnergy = 0;
	outAnnualGridExportEnergy = 0;

	en = setup_model;
	if ( !en ) return;

	// time quantities
	year = 0;
	nyears = 1;
	_dt_hour = dt_hr;
	step_per_hour = nrec / 8760;
	if (cm.as_boolean("pv_lifetime_simulation"))
		nyears = cm.as_integer("analysis_period");
	else
	{
		if (replacement_option > 0)
			cm.log("Replacements are enabled without running lifetime simulation, please run over lifetime to consider battery replacements", SSC_WARNING);
	}

	chem = cm.as_integer( "batt_chem" );

	size_t ncharge, ndischarge, ngridcharge, ndischarge_percent, ngridcharge_percent, ntarget_powers;
	ssc_number_t *pcharge = cm.as_array( "dispatch_manual_charge", &ncharge );
	ssc_number_t *pdischarge = cm.as_array( "dispatch_manual_discharge", &ndischarge );
	ssc_number_t *pdischarge_percent = cm.as_array("dispatch_manual_percent_discharge", &ndischarge_percent);
	ssc_number_t *pgridcharge_percent = cm.as_array("dispatch_manual_percent_gridcharge", &ngridcharge_percent);
	ssc_number_t *pgridcharge = cm.as_array( "dispatch_manual_gridcharge", &ngridcharge );

	if ( ncharge != 6 || ndischarge != 6 || ngridcharge != 6 )
		throw compute_module::exec_error("battery", "invalid manual dispatch control vector lengths");

	int discharge_index = 0;
	int gridcharge_index = 0;
	;
	for( size_t i=0;i<6;i++ )
	{
		dm_charge[i] = pcharge[i]!=0.0f ? 1 : 0;
		dm_discharge[i] = pdischarge[i]!=0.0f ? 1 : 0;
		dm_gridcharge[i] = pgridcharge[i]!=0.0f ? 1 : 0;
		if (dm_discharge[i])
		{
			if (discharge_index < ndischarge_percent)
			{
				dm_percent_discharge[i+1] = pdischarge_percent[discharge_index];
				discharge_index++;
			}
			else
				throw compute_module::exec_error("battery", "invalid manual dispatch control vector lengths");
		}
		if (dm_gridcharge[i])
		{
			if (gridcharge_index < ngridcharge_percent)
			{
				dm_percent_gridcharge[i+1] = pgridcharge_percent[gridcharge_index];
				gridcharge_index++;
			}
			else
				throw compute_module::exec_error("battery", "invalid manual dispatch control vector lengths");
		}
	}
	size_t m,n;
	batt_dispatch = cm.as_integer("batt_dispatch_choice");
	bool pv_dispatch = cm.as_boolean("batt_pv_choice");
	util::matrix_t<float> &schedule = cm.allocate_matrix("batt_dispatch_sched", 12, 24);
	if (batt_dispatch != dispatch_t::MANUAL)
	{
		m = 12;
		n = 24 * step_per_hour;
		dm_dynamic_sched.resize_fill(m, n, 1);
		dm_dynamic_sched_weekend.resize_fill(m, n, 1);
		if (batt_dispatch == dispatch_t::MAINTAIN_TARGET)
		{
			int target_dispatch = cm.as_integer("batt_target_choice");
			if (target_dispatch == 0)
			{
				target_power_monthly = cm.as_doublevec("batt_target_power_monthly");
				target_power.clear();
				target_power.reserve(8760 * step_per_hour);
				for (int month = 0; month != 12; month++)
				{
					double target = target_power_monthly[month];
					for (int hour = 0; hour != util::hours_in_month(month+1); hour++)
					{
						for (int step = 0; step != step_per_hour; step++)
							target_power.push_back(target);
					}
				}
			}
			else
				target_power = cm.as_doublevec("batt_target_power");
			
			if (target_power.size() != nrec)
				throw compute_module::exec_error("battery", "invalid number of target powers, must be equal to number of records in weather file");
		}
	}
	else if (batt_dispatch == dispatch_t::MANUAL)
	{
		ssc_number_t *psched = cm.as_matrix("dispatch_manual_sched", &m, &n);
		if (m != 12 || n != 24)
			throw compute_module::exec_error("battery", "invalid manual dispatch schedule matrix dimensions, must be 12 x 24");
		dm_dynamic_sched.resize(m, n);

		ssc_number_t *psched_weekend = cm.as_matrix("dispatch_manual_sched_weekend", &m, &n);
		dm_dynamic_sched_weekend.resize(m, n);

		for (size_t i = 0; i < 12; i++)
			for (size_t j = 0; j < 24; j++){
				dm_dynamic_sched(i, j) = psched[i * 24 + j];
				dm_dynamic_sched_weekend(i, j) = psched_weekend[i * 24 + j];
				schedule(i, j) = psched[i * 24 + j];
			} 
		}

		util::matrix_t<double>  batt_lifetime_matrix = cm.as_matrix("batt_lifetime_matrix");
		if (batt_lifetime_matrix.nrows() < 3 || batt_lifetime_matrix.ncols() != 3)
			throw compute_module::exec_error("battery", "Battery lifetime matrix must have three columns and at least three rows");


	/* **********************************************************************
	Initialize outputs
	********************************************************************** */		

	// non-lifetime outputs
	if (nyears <= 1)
	{
		outTotalCharge = cm.allocate("batt_q0", nrec*nyears);

		// only allocate if lead-acid
		if (chem == 0)
		{
			outAvailableCharge = cm.allocate("batt_q1", nrec*nyears);
			outBoundCharge = cm.allocate("batt_q2", nrec*nyears);
		}
		outMaxCharge = cm.allocate("batt_qmax", nrec*nyears);
		outCellVoltage = cm.allocate("batt_voltage_cell", nrec*nyears);
		outCurrent = cm.allocate("batt_I", nrec*nyears);
		outBatteryVoltage = cm.allocate("batt_voltage", nrec*nyears);
		outBatteryTemperature = cm.allocate("batt_temperature", nrec*nyears);
		outCapacityThermalPercent = cm.allocate("batt_capacity_thermal_percent", nrec*nyears);

	}
	
	outCycles = cm.allocate("batt_cycles", nrec*nyears);
	outSOC = cm.allocate("batt_SOC", nrec*nyears);
	outDOD = cm.allocate("batt_DOD", nrec*nyears);
	outCapacityPercent = cm.allocate("batt_capacity_percent", nrec*nyears);
	outBatteryPower = cm.allocate("batt_power", nrec*nyears);
	outGridPower = cm.allocate("grid_power", nrec*nyears); // Net grid energy required.  Positive indicates putting energy on grid.  Negative indicates pulling off grid
	outGenPower = cm.allocate("pv_batt_gen", nrec*nyears);
	outPVToLoad = cm.allocate("pv_to_load", nrec*nyears);
	outBatteryToLoad = cm.allocate("batt_to_load", nrec*nyears);
	outGridToLoad = cm.allocate("grid_to_load", nrec*nyears);
	outPVToBatt = cm.allocate("pv_to_batt", nrec*nyears);
	outGridToBatt = cm.allocate("grid_to_batt", nrec*nyears);
	
	// annual outputs
	int annual_size = nyears+1;
	if (nyears == 1){ annual_size = 1; };
	
	outBatteryBankReplacement = cm.allocate("batt_bank_replacement", annual_size);
	outAnnualChargeEnergy = cm.allocate("batt_annual_charge_energy", annual_size);
	outAnnualDischargeEnergy = cm.allocate("batt_annual_discharge_energy", annual_size);
	outAnnualGridImportEnergy = cm.allocate("annual_import_to_grid_energy", annual_size);
	outAnnualGridExportEnergy = cm.allocate("annual_export_to_grid_energy", annual_size);
	outAnnualEnergyLoss = cm.allocate("batt_annual_energy_loss", annual_size);
	outAnnualPVChargeEnergy = cm.allocate("batt_annual_charge_from_pv", annual_size);
	outAnnualGridChargeEnergy = cm.allocate("batt_annual_charge_from_grid", annual_size);

	outBatteryBankReplacement[0] = 0;
	outAnnualChargeEnergy[0] = 0;
	outAnnualDischargeEnergy[0] = 0;
	outAnnualGridImportEnergy[0] = 0;
	outAnnualGridExportEnergy[0] = 0;
	outAnnualEnergyLoss[0] = 0;

	// model initialization
	voltage_model = new voltage_dynamic_t(cm.as_integer("batt_computed_series"), cm.as_integer("batt_computed_strings"), cm.as_double("batt_Vnom_default"), cm.as_double("batt_Vfull"), cm.as_double("batt_Vexp"),
		cm.as_double("batt_Vnom"), cm.as_double("batt_Qfull"), cm.as_double("batt_Qexp"), cm.as_double("batt_Qnom"), cm.as_double("batt_C_rate"), cm.as_double("batt_resistance"));

	lifetime_model = new  lifetime_t(batt_lifetime_matrix, replacement_option, cm.as_double("batt_replacement_capacity") );
	util::matrix_t<double> cap_vs_temp = cm.as_matrix( "cap_vs_temp" );
	if ( cap_vs_temp.nrows() < 2 || cap_vs_temp.ncols() != 2 )
		throw compute_module::exec_error("battery", "capacity vs temperature matrix must have two columns and at least two rows");

	// thermal_outputs = new thermal_outputs_t();
	thermal_model = new thermal_t(
		cm.as_double("batt_mass"), // [kg]
		cm.as_double("batt_length"), // [m]
		cm.as_double("batt_width"), // [m]
		cm.as_double("batt_height"), // [m]
		cm.as_double("batt_Cp"), // [J/kgK]
		cm.as_double("batt_h_to_ambient"), // W/m2K
		273.15 + cm.as_double("T_room"), // K
		cap_vs_temp);
		
		
	battery_model = new battery_t( 
		dt_hr,
		chem);

	double Vfull = cm.as_double("batt_Vfull");
	int ncell = cm.as_integer("batt_computed_series")*cm.as_integer("batt_computed_strings");

	if ( chem == 0 )
	{
		capacity_model = new capacity_kibam_t(
			cm.as_double( "LeadAcid_q20_computed" ),
			cm.as_double( "LeadAcid_tn" ),
			cm.as_double( "LeadAcid_qn_computed" ),
			cm.as_double("LeadAcid_q10_computed"),
			cm.as_double("batt_maximum_soc"));
	}
	else if ( chem == 1 )
	{
		capacity_model = new capacity_lithium_ion_t(
			cm.as_double("batt_Qfull")*cm.as_integer("batt_computed_strings"), cm.as_double("batt_maximum_soc"));
	}
	
	losses_model = new losses_t(
		lifetime_model,
		thermal_model,
		capacity_model);

	battery_model->initialize( capacity_model, voltage_model, lifetime_model, thermal_model, losses_model);

	dc_dc = ac_dc = dc_ac = 100.;
	// ac_or_dc = cm.as_integer("batt_ac_or_dc");
	ac_or_dc = 1; // hard code to ac for now
	ac_dc = cm.as_double("batt_ac_dc_efficiency");
	dc_ac = cm.as_double("batt_dc_ac_efficiency");
	
	if (batt_dispatch == dispatch_t::MANUAL)
	{
		dispatch_model = new dispatch_manual_t(battery_model, dt_hr, cm.as_double("batt_minimum_SOC"), cm.as_double("batt_maximum_SOC"),
			cm.as_double("batt_current_charge_max"), cm.as_double("batt_current_discharge_max"),
			cm.as_double("batt_minimum_modetime"),
			ac_or_dc, dc_dc, ac_dc, dc_ac,
			batt_dispatch, pv_dispatch,
			dm_dynamic_sched, dm_dynamic_sched_weekend,
			dm_charge, dm_discharge, dm_gridcharge, dm_percent_discharge, dm_percent_gridcharge);
	}
	else
	{
		dispatch_model = new automate_dispatch_t(battery_model, dt_hr, cm.as_double("batt_minimum_SOC"), cm.as_double("batt_maximum_SOC"),
			cm.as_double("batt_current_charge_max"), cm.as_double("batt_current_discharge_max"),
			1, // minimum mode time allowed as 1 minute for auto dispatch
			ac_or_dc, dc_dc, ac_dc, dc_ac,
			batt_dispatch, pv_dispatch,
			dm_dynamic_sched, dm_dynamic_sched_weekend,
			dm_charge, dm_discharge, dm_gridcharge, dm_percent_discharge, dm_percent_gridcharge,
			nyears);
	}
} 
void battstor::initialize_automated_dispatch(ssc_number_t *pv, ssc_number_t *load, int mode)
{
	
	if (batt_dispatch != dispatch_t::MANUAL)
	{
		int nrec;
		prediction_index = 0;
		bool look_ahead = ((mode == dispatch_t::LOOK_AHEAD || mode == dispatch_t::MAINTAIN_TARGET));
		bool look_behind = ((mode == dispatch_t::LOOK_BEHIND));
		automate_dispatch_t * automated_dispatch = dynamic_cast<automate_dispatch_t*>(dispatch_model);


		// automatic look ahead
		if (look_ahead)
			nrec = nyears * 8760 * step_per_hour;
		// look behind
		else if (look_behind)
			nrec = 24 * step_per_hour;

		if (pv != 0) {
			for (int idx = 0; idx != nrec; idx++)
			{
				pv_prediction.push_back(pv[idx]);
				load_prediction.push_back(load[idx]);
			}
		}
		else
		{
			for (int idx = 0; idx != nrec; idx++)
			{
				pv_prediction.push_back(0.);
				load_prediction.push_back(0.);
			}
		}		
		automated_dispatch->update_pv_load_data(pv_prediction, load_prediction);
		if (mode == dispatch_t::MAINTAIN_TARGET)
			automated_dispatch->set_target_power(target_power);
	}
}
battstor::~battstor()
{
	if( voltage_model ) delete voltage_model;
	if( lifetime_model ) delete lifetime_model;
	if( thermal_model ) delete thermal_model;
	if( battery_model ) delete battery_model;
	if( capacity_model ) delete capacity_model;
	if (losses_model) delete losses_model;
	if( dispatch_model ) delete dispatch_model;

}

void battstor::check_replacement_schedule(int batt_replacement_option, size_t count_batt_replacement, ssc_number_t *batt_replacement, int iyear, int hour, int step)
{
	if (batt_replacement_option == 2)
	{
		// don't allow replacement on first hour of first year
		if (hour == 0 && iyear == 0)
			return;

		bool replace = false;
		if (iyear < count_batt_replacement)
		{
			int num_repl = batt_replacement[iyear];
			for (int j_repl = 0; j_repl < num_repl; j_repl++)
			{
				if ((hour == (int)(j_repl*8760.0 / num_repl)) && step == 0)
				{
					replace = true;
					break;
				}
			}
		}
		if (replace)
			force_replacement();
	}
}
void battstor::force_replacement()
{
	lifetime_model->force_replacement();
	battery_model->runLifetimeModel(0);
}


void battstor::advance(compute_module &cm, size_t year, size_t hour_of_year, size_t step, double PV /* [kWh] */, double LOAD /* [kWh] */)
{
	if (PV < 0){ PV = 0; }
	dispatch_model->dispatch(year, hour_of_year, step, PV, LOAD);
	
	int idx = (year * 8760 + hour_of_year)*step_per_hour + step;

	// non-lifetime outputs
	if (nyears <= 1)
	{
		// Capacity Output with Losses Applied
		if (capacity_kibam_t * kibam = dynamic_cast<capacity_kibam_t*>(capacity_model))
		{
			outAvailableCharge[idx] = (ssc_number_t)(kibam->q1());
			outBoundCharge[idx] = (ssc_number_t)(kibam->q2());
		}
		outMaxCharge[idx] = (ssc_number_t)(capacity_model->qmax());
		outTotalCharge[idx] = (ssc_number_t)(capacity_model->q0());
		outCurrent[idx] = (capacity_model->I());
		outCellVoltage[idx] = (ssc_number_t)(voltage_model->cell_voltage());
		outBatteryVoltage[idx] = (ssc_number_t)(voltage_model->battery_voltage());
		outBatteryTemperature[idx] = (ssc_number_t)(thermal_model->T_battery()) - 273.15;
		outCapacityThermalPercent[idx] = (ssc_number_t)(thermal_model->capacity_percent());

	}
	
	// Lifetime outputs
	int annual_index;
	nyears > 1 ? annual_index = year + 1 : annual_index = 0;
	outCycles[idx] = (int)(lifetime_model->cycles_elapsed());
	outSOC[idx] = (ssc_number_t)(capacity_model->SOC());
	outDOD[idx] = (ssc_number_t)(lifetime_model->cycle_range());
	outCapacityPercent[idx] = (ssc_number_t)(lifetime_model->capacity_percent());
	outBatteryBankReplacement[annual_index] = (ssc_number_t)(lifetime_model->replacements());
	if ((hour_of_year == 8759) && (step == step_per_hour - 1))
	{
		int replacements = lifetime_model->replacements();
		lifetime_model->reset_replacements();
		outAnnualGridImportEnergy[annual_index] = (ssc_number_t)(dispatch_model->grid_import_annual());
		outAnnualGridExportEnergy[annual_index] = (ssc_number_t)(dispatch_model->grid_export_annual());
		outAnnualPVChargeEnergy[annual_index] = (ssc_number_t)(dispatch_model->pv_charge_annual());
		outAnnualGridChargeEnergy[annual_index] = (ssc_number_t)(dispatch_model->grid_charge_annual());
		outAnnualChargeEnergy[annual_index] = (ssc_number_t)(dispatch_model->charge_annual());
		outAnnualDischargeEnergy[annual_index] = (ssc_number_t)(dispatch_model->discharge_annual()); 
		outAnnualEnergyLoss[annual_index] = (ssc_number_t)(dispatch_model->energy_loss_annual());
		dispatch_model->new_year();
		year++;
	}
	// dispatch_model output (all Powers in kW)
	outBatteryPower[idx] = (ssc_number_t)(dispatch_model->energy_tofrom_battery())/_dt_hour;
	outGridPower[idx] = (ssc_number_t)(dispatch_model->energy_tofrom_grid()) / _dt_hour;
	outPVToLoad[idx] = (ssc_number_t)(dispatch_model->pv_to_load())/_dt_hour;
	outGenPower[idx] = (ssc_number_t)(dispatch_model->gen()) / _dt_hour;
	outBatteryToLoad[idx] = (ssc_number_t)(dispatch_model->battery_to_load())/_dt_hour;
	outGridToLoad[idx] = (ssc_number_t)(dispatch_model->grid_to_load())/_dt_hour;
	outPVToBatt[idx] = (ssc_number_t)(dispatch_model->pv_to_batt()) / _dt_hour;
	outGridToBatt[idx] = (ssc_number_t)(dispatch_model->grid_to_batt()) / _dt_hour;

	// Average efficiency
	outAverageCycleEfficiency = (ssc_number_t)dispatch_model->average_efficiency();
	if (outAverageCycleEfficiency > 100)
		outAverageCycleEfficiency = 100;
	else if (outAverageCycleEfficiency < 0)
		outAverageCycleEfficiency = 0;

	// PV charge ratio
	outPVChargePercent = (ssc_number_t)dispatch_model->pv_charge_percent();
	if (outPVChargePercent > 100)
		outPVChargePercent = 100;
	else if (outPVChargePercent < 0)
		outPVChargePercent = 0;

}
void battstor::update_post_inverted(compute_module &cm, size_t idx, double PV, double LOAD)
{
	dispatch_model->compute_grid_net(PV, LOAD);
	outGridPower[idx] = (ssc_number_t)(dispatch_model->energy_tofrom_grid()) / _dt_hour;
	outPVToLoad[idx] = (ssc_number_t)(dispatch_model->pv_to_load()) / _dt_hour;
	outBatteryToLoad[idx] = (ssc_number_t)(dispatch_model->battery_to_load()) / _dt_hour;
	outGridToLoad[idx] = (ssc_number_t)(dispatch_model->grid_to_load()) / _dt_hour;
}

void battstor::calculate_monthly_and_annual_outputs( compute_module &cm )
{
	int step_per_hour = (int)( 1.0 / _dt_hour );

	// single value metrics
	cm.assign("average_cycle_efficiency", var_data( (ssc_number_t) outAverageCycleEfficiency ));
	cm.assign("batt_pv_charge_percent", var_data((ssc_number_t)outPVChargePercent));
	cm.assign("batt_bank_installed_capacity", cm.as_double("batt_computed_bank_capacity"));

	// monthly outputs
	cm.accumulate_monthly_for_year( "pv_to_load",   "monthly_pv_to_load",   _dt_hour, step_per_hour );
	cm.accumulate_monthly_for_year( "batt_to_load", "monthly_batt_to_load", _dt_hour, step_per_hour );
	cm.accumulate_monthly_for_year( "grid_to_load", "monthly_grid_to_load", _dt_hour, step_per_hour );
}

///////////////////////////////////////////////////
static var_info _cm_vtab_battery[] = {
	/*   VARTYPE           DATATYPE         NAME                      LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	
	// system energy and load
	{ SSC_INOUT, SSC_ARRAY, "hourly_energy", "Hourly energy", "kWh", "", "Time Series", "*", "", "" },
	{ SSC_INOUT, SSC_ARRAY, "e_load", "Electric load", "kWh", "", "Load Profile Estimator", "", "", "" },

	// other variables come from battstor common table

	var_info_invalid };

class cm_battery : public compute_module
{
public:

	cm_battery()
	{
		add_var_info(_cm_vtab_battery);
		add_var_info( vtab_battery );
	}

	void exec() throw(general_error)
	{
		
		size_t nrec, len;
		ssc_number_t *hourly_energy = as_array("hourly_energy", &nrec);
		ssc_number_t *e_load = as_array("e_load", &len);

		if (len != nrec) 
			throw exec_error("battery", "Load and PV power do not match weatherfile length");

		size_t step_per_hour = nrec / 8760;
		if (step_per_hour < 1 || step_per_hour > 60 || step_per_hour * 8760 != nrec)
			throw exec_error("swh", util::format("invalid number of data records (%d): must be an integer multiple of 8760", (int)nrec));

		double ts_hour = 1.0 / step_per_hour;
		double ts_sec = 3600.0 / step_per_hour;


		battstor batt( *this, true, true, nrec, ts_hour );

		size_t hour = 0;
		
		/* *********************************************************************************************
		Run Simulation
		*********************************************************************************************** */

		int count = 0;
		for (hour = 0; hour < 8760; hour++)
		{
			for (size_t jj = 0; jj < step_per_hour; jj++)
			{
				batt.advance(*this, 0, hour, jj, hourly_energy[count], e_load[count]);
				count++;
			}
		} 
	}

};

DEFINE_MODULE_ENTRY(battery, "Battery storage standalone model .", 10)
