#include "lib_fuel_cell.h"

FuelCell::FuelCell() { /* Nothing to do */ };

FuelCell::FuelCell(size_t numberOfUnits, double unitPowerMax_kW, double unitPowerMin_kW, double startup_hours, double dynamicResponse_kWperMin,
	double degradation_kWperHour, double degradationRestart_kW, double replacement_percent, util::matrix_t<double> efficiencyTable,
	double lowerHeatingValue_MJperkg, double availableFuel_kg,
	int shutdownOption, int dispatchOption) : 
	numberOfUnits(numberOfUnits), unitPowerMax_kW(unitPowerMax_kW), unitPowerMin_kW(unitPowerMin_kW), startup_hours(startup_hours), 
	dynamicResponse_kWperMin(dynamicResponse_kWperMin), degradation_kWperHour(degradation_kWperHour), degradationRestart_kW(degradationRestart_kW), 
	replacement_percent(replacement_percent), efficiencyTable(efficiencyTable), lowerHeatingValue_MJperKg(lowerHeatingValue_MJperkg), 
	availableFuel_kg(availableFuel_kg), shutdownOption(shutdownOption), dispatchOption(dispatchOption)
{

}

FuelCell::~FuelCell(){ /* Nothing to do */}
