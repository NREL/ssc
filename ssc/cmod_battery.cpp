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
	{ SSC_INPUT,        SSC_NUMBER,      "batt_chem",                                  "Battery chemistry",                                       "",        "0=LeadAcid,1=LiIon",   "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,		 "batt_bank_size",                             "Battery bank desired size",                               "kWh",     "",                     "Battery",       "",                           "",                              "" },
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
	{ SSC_INPUT,        SSC_MATRIX,     "dispatch_manual_sched",                       "Battery dispatch schedule",                              "",         "",                     "Battery",       "",                           "",                             "" },
		


// Capacity, Voltage, Charge outputs
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_q0",                                    "Battery total charge",                                   "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_q1",                                    "Battery available charge",                               "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_q2",                                    "Battery bound charge",                                   "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_SOC",                                   "Battery state of charge",                                "%",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_DOD",                                   "Battery cycle depth of discharge",                       "%",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_qmaxI",                                 "Battery max capacity at current",                        "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_qmax",                                  "Battery max charge",                                     "Ah",       "",                     "Battery",       "",                           "",                              "" },
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
	
	// monthly outputs
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_pv_to_load",                         "Energy to load from PV",                                "kWh",      "",                       "Battery",       "",                          "LENGTH=12",                     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_batt_to_load",                       "Energy to load from battery",                           "kWh",      "",                       "Battery",       "",                          "LENGTH=12",                     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_grid_to_load",                       "Energy to load from grid",                              "kWh",      "",                       "Battery",       "",                          "LENGTH=12",                     "" },
	
	// Efficiency outputs													          
	{ SSC_OUTPUT,        SSC_NUMBER,     "average_cycle_efficiency",                   "Average battery cycle efficiency",                      "%",        "",                     "Annual",        "",                           "",                              "" },
	
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
	outAverageCycleEfficiency = 0;


	en = setup_model;
	if ( !en ) return;

	// time quantities
	year = 0;
	nyears = 1;
	_dt_hour = dt_hr;
	step_per_hour = nrec / 8760;
	if (cm.as_boolean("pv_lifetime_simulation"))
		nyears = cm.as_integer("analysis_period");

	chem = cm.as_integer( "batt_chem" );

	size_t ncharge, ndischarge, ngridcharge, ndischarge_percent;
	ssc_number_t *pcharge = cm.as_array( "dispatch_manual_charge", &ncharge );
	ssc_number_t *pdischarge = cm.as_array( "dispatch_manual_discharge", &ndischarge );
	ssc_number_t *pdischarge_percent = cm.as_array("dispatch_manual_percent_discharge", &ndischarge_percent);
	ssc_number_t *pgridcharge = cm.as_array( "dispatch_manual_gridcharge", &ngridcharge );
	if ( ncharge != 6 || ndischarge != 6 || ngridcharge != 6 )
		throw compute_module::exec_error("battery", "invalid manual dispatch control vector lengths");

	int discharge_index = 0;
	for( size_t i=0;i<6;i++ )
	{
		dm_charge[i] = pcharge[i]!=0.0f ? 1 : 0;
		dm_discharge[i] = pdischarge[i]!=0.0f ? 1 : 0;
		dm_gridcharge[i] = pgridcharge[i]!=0.0f ? 1 : 0;
		if (dm_discharge[i])
		{
			if (discharge_index < ndischarge_percent)
			{
				dm_percent_discharge[i] = pdischarge_percent[discharge_index];
				discharge_index++;
			}
			else
				throw compute_module::exec_error("battery", "invalid manual dispatch control vector lengths");
		}
	}
	size_t m,n;
	ssc_number_t *psched = cm.as_matrix("dispatch_manual_sched", &m, &n);
	if ( m != 12 || n != 24 )
		throw compute_module::exec_error("battery", "invalid manual dispatch schedule matrix dimensions, must be 12 x 24" );

	for( size_t i=0;i<12;i++ )
		for( size_t j=0;j<24;j++ )
			dm_sched(i,j) = psched[ i*24 + j ];
	
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
		outCycles = cm.allocate("batt_cycles", nrec*nyears);
		outBatteryVoltage = cm.allocate("batt_voltage", nrec*nyears);
		outBatteryTemperature = cm.allocate("batt_temperature", nrec*nyears);
		outCapacityThermalPercent = cm.allocate("batt_capacity_thermal_percent", nrec*nyears);

	}
	 
	outSOC = cm.allocate("batt_SOC", nrec*nyears);
	outDOD = cm.allocate("batt_DOD", nrec*nyears);
	outCapacityPercent = cm.allocate("batt_capacity_percent", nrec*nyears);
	outBatteryBankReplacement = cm.allocate("batt_bank_replacement", nyears+1); 
	outBatteryBankReplacement[0] = 0; // consistent with all annual items nyears + 1 length
	outBatteryPower = cm.allocate("batt_power", nrec*nyears);
	outGridPower = cm.allocate("grid_power", nrec*nyears); // Net grid energy required.  Positive indicates putting energy on grid.  Negative indicates pulling off grid
	outGenPower = cm.allocate("pv_batt_gen", nrec*nyears);
	outPVToLoad = cm.allocate("pv_to_load", nrec*nyears);
	outBatteryToLoad = cm.allocate("batt_to_load", nrec*nyears);
	outGridToLoad = cm.allocate("grid_to_load", nrec*nyears);

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
	
	dispatch_model = new dispatch_manual_t(battery_model, dt_hr, cm.as_double("batt_minimum_SOC"), cm.as_double("batt_maximum_SOC"), 
		cm.as_double("batt_current_charge_max"), cm.as_double("batt_current_discharge_max"),
		cm.as_double("batt_minimum_modetime"), 
		ac_or_dc, dc_dc, ac_dc, dc_ac,
		dm_sched, dm_charge, dm_discharge, dm_gridcharge, dm_percent_discharge);
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


void battstor::advance( compute_module &cm, size_t idx, size_t hour_of_year, size_t step, double PV /* [kWh] */, double LOAD /* [kWh] */ )
{
	if (PV < 0){ PV = 0; }
	dispatch_model->dispatch( hour_of_year, PV, LOAD );

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
		outCycles[idx] = (int)(lifetime_model->cycles_elapsed());
		outBatteryTemperature[idx] = (ssc_number_t)(thermal_model->T_battery()) - 273.15;
		outCapacityThermalPercent[idx] = (ssc_number_t)(thermal_model->capacity_percent());

	}
	
	// Lifetime outputs
	outSOC[idx] = (ssc_number_t)(capacity_model->SOC());
	outDOD[idx] = (ssc_number_t)(lifetime_model->cycle_range());
	outCapacityPercent[idx] = (ssc_number_t)(lifetime_model->capacity_percent());
	outBatteryBankReplacement[year+1] = (ssc_number_t)(lifetime_model->replacements());
	if ((hour_of_year == 8759) && (step == step_per_hour - 1))
	{
		int replacements = lifetime_model->replacements();
		year++;
		lifetime_model->reset_replacements();
	}
	// Dispatch output (all Powers in kW)
	outBatteryPower[idx] = (ssc_number_t)(dispatch_model->energy_tofrom_battery())/_dt_hour;
	outGridPower[idx] = (ssc_number_t)(dispatch_model->energy_tofrom_grid()) / _dt_hour;
	outGenPower[idx] = (ssc_number_t)(dispatch_model->gen()) / _dt_hour;
	outPVToLoad[idx] = (ssc_number_t)(dispatch_model->pv_to_load())/_dt_hour;
	outBatteryToLoad[idx] = (ssc_number_t)(dispatch_model->battery_to_load())/_dt_hour;
	outGridToLoad[idx] = (ssc_number_t)(dispatch_model->grid_to_load())/_dt_hour;

	// Average efficiency
	outAverageCycleEfficiency = (ssc_number_t)dispatch_model->average_efficiency();
	if (outAverageCycleEfficiency > 100)
		outAverageCycleEfficiency = 100;
	else if (outAverageCycleEfficiency < 0)
		outAverageCycleEfficiency = 0;
}
void battstor::update_post_inverted(compute_module &cm, size_t idx, double PV, double LOAD)
{
	dispatch_model->compute_grid_net(PV, LOAD);
	outGridPower[idx] = (ssc_number_t)(dispatch_model->energy_tofrom_grid())/_dt_hour;
	outPVToLoad[idx] = (ssc_number_t)(dispatch_model->pv_to_load())/_dt_hour;
	outBatteryToLoad[idx] = (ssc_number_t)(dispatch_model->battery_to_load())/_dt_hour;
	outGridToLoad[idx] = (ssc_number_t)(dispatch_model->grid_to_load())/_dt_hour;
}

void battstor::calculate_monthly_and_annual_outputs( compute_module &cm )
{
	int step_per_hour = (int)( 1.0 / _dt_hour );

	// average battery eff
	cm.assign("average_cycle_efficiency", var_data( (ssc_number_t) outAverageCycleEfficiency ));

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
			// Loop over subhourly
			for (size_t jj = 0; jj<step_per_hour; jj++)
			{
				batt.advance( *this, count, hour, jj, hourly_energy[count], e_load[count] );
				count++;
			}	// End loop over subhourly
		} // End loop over hourly
	}

};

DEFINE_MODULE_ENTRY(battery, "Battery storage standalone model .", 10)
