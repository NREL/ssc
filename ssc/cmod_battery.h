#ifndef _CMOD_BATTERY_COMMON_
#define _CMOD_BATTERY_COMMON_ 1

#include "core.h"
#include "lib_battery.h"


extern var_info vtab_battery[];

struct battstor
{

	battstor( compute_module &cm, bool setup_model, size_t nrec, double dt_hr );
	~battstor();
	void advance( compute_module &cm, size_t idx, size_t hour_of_year, size_t step, double PV, double LOAD );
	void finalize( size_t nrec );


	// member data
	voltage_dynamic_t *voltage_model;
	lifetime_t *lifetime_model;
	thermal_t *thermal_model;
	capacity_t *capacity_model;
	battery_t *battery_model;
	battery_bank_t *battery_bank_model;
	dispatch_manual_t *dispatch_model;
	losses_t *losses_model;

	bool en;
	int chem;
	

	bool dm_charge[4], dm_discharge[4], dm_gridcharge[4]; // manual dispatch
	util::matrix_static_t<float, 12, 24> dm_sched;
	
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
		*outBatteryBankVoltage,
		*outCapacityPercent,
		*outCycles,
		*out40Cycles,
		*out100Cycles,
		*outBatteryTemperature,
		*outCapacityThermalPercent,
		*outDispatchMode,
		*outBatteryEnergy,
		*outGridEnergy,
		*outPVToLoad,
		*outBatteryToLoad,
		*outGridToLoad;
	
	double outAverageCycleEfficiency;
};


#endif
