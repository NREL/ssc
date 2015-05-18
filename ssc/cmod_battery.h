#ifndef _CMOD_BATTERY_COMMON_
#define _CMOD_BATTERY_COMMON_ 1

#include "core.h"
#include "lib_battery.h"


extern var_info vtab_battery[];

struct battstor
{

	battstor( compute_module &cm, bool setup_model, int replacement_option, size_t nrec, double dt_hr );
	~battstor();
	void advance( compute_module &cm, size_t idx, size_t hour_of_year, size_t step, double PV, double LOAD );
	void update_post_inverted(compute_module &cm, size_t idx, double PV, double LOAD);

	// for user schedule
	void force_replacement();

	// time quantities
	int year;
	size_t step_per_hour;
	size_t nyears;

	// member data
	voltage_dynamic_t *voltage_model;
	lifetime_t *lifetime_model;
	thermal_t *thermal_model;
	capacity_t *capacity_model;
	battery_t *battery_model;
	dispatch_manual_t *dispatch_model;
	losses_t *losses_model;

	bool en;
	int chem;
	

	bool dm_charge[4], dm_discharge[4], dm_gridcharge[4]; // manual dispatch
	std::map<int, double> dm_percent_discharge; // <profile, discharge_percent>
	util::matrix_static_t<float, 12, 24> dm_sched;
	
	bool ac_or_dc;
	double dc_dc, ac_dc, dc_ac;

	double e_charge;
	double e_discharge;

	// outputs
	ssc_number_t *outTotalCharge,
		*outAvailableCharge,
		*outBoundCharge,
		*outMaxChargeAtCurrent,
		*outMaxCharge,
		*outSOC,
		*outDOD,
		*outCurrent,
		*outCellVoltage,
		*outBatteryVoltage,
		*outCapacityPercent,
		*outCycles,
		*outBatteryBankReplacement,
		*outBatteryTemperature,
		*outCapacityThermalPercent,
		*outDispatchMode,
		*outBatteryEnergy,
		*outGridEnergy,
		*outGenEnergy,
		*outPVToLoad,
		*outBatteryToLoad,
		*outGridToLoad;
	
	double outAverageCycleEfficiency;
};


#endif
