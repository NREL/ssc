#include <memory>

#include "lib_fuel_cell.h"
#include "lib_fuel_cell_dispatch.h"
#include "lib_power_electronics.h"


FuelCellDispatch::FuelCellDispatch(FuelCell * fuelCell, size_t numberOfUnits, int dispatchOption, int shutdownOption, double dt_hour, double fixed_percent, 
	std::vector<double> dispatchInput_kW, std::vector<bool> canCharge, std::vector<bool> canDischarge, 
	std::map<size_t, double> discharge_percent, util::matrix_t<size_t> scheduleWeekday, util::matrix_t<size_t> scheduleWeekend)
	: m_numberOfUnits(numberOfUnits), m_dispatchOption(dispatchOption), m_shutdownOption(shutdownOption), dt_hour(dt_hour), m_fixed_percent(fixed_percent * 0.01), 
	m_dispatchInput_kW(dispatchInput_kW), m_canCharge(canCharge), m_canDischarge(canDischarge), 
	m_discharge_percent(discharge_percent),
	m_scheduleWeekday(scheduleWeekday), m_scheduleWeekend(scheduleWeekend), m_powerTotal_kW(0)
{
	m_fuelCellVector.push_back(fuelCell);
	for (auto percent = m_discharge_percent.begin(); percent != m_discharge_percent.end(); percent++) {
		percent->second *= 0.01;
	}

	m_batteryPower = BatteryPowerFlow::Instance(dt_hour).getBatteryPower();
	m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;

	// figure out for multiple fuel cells 
	/*
	for (size_t fc = 1; fc < numberOfUnits; fc++) {
		std::unique_ptr<FuelCell> fuelCelltmp(new FuelCell());
		fuelCelltmp.get() = fuelCell;
	}
	*/

}

void FuelCellDispatch::runSingleTimeStep(size_t hour_of_year, size_t year_idx, double powerSystem_kWac, double powerLoad_kWac) {

	m_powerTotal_kW = 0;

	// Specified to run at fixed percent of original unit max kW
	if (m_dispatchOption == FuelCellDispatch::FC_DISPATCH_OPTION::FIXED) {
		for (size_t fc = 0; fc < m_fuelCellVector.size(); fc++) {
			double power_kW = m_fuelCellVector[fc]->getMaxPowerOriginal() * m_fixed_percent;
			m_fuelCellVector[fc]->runSingleTimeStep(power_kW);
			m_powerTotal_kW += m_fuelCellVector[fc]->getPower();
		}
	}
	// Specified to follow load, fuel cell attempts to make up difference of system - load
	else if (m_dispatchOption == FuelCellDispatch::FC_DISPATCH_OPTION::LOAD_FOLLOW) {
		for (size_t fc = 0; fc < m_fuelCellVector.size(); fc++) {
			double power_kW = fmax(0, powerLoad_kWac - powerSystem_kWac);
			m_fuelCellVector[fc]->runSingleTimeStep(power_kW);
			m_powerTotal_kW += m_fuelCellVector[fc]->getPower();
		}
	}
	// Specified to follow manual dispatch input (add logic)
	else if (m_dispatchOption == FuelCellDispatch::FC_DISPATCH_OPTION::MANUAL) {
		for (size_t fc = 0; fc < m_fuelCellVector.size(); fc++) {
			size_t month, hour = NULL;
			util::month_hour(hour_of_year, month, hour);
			size_t period = m_scheduleWeekday(month - 1, hour - 1);
			if (!util::weekday(hour_of_year)) {
				period = m_scheduleWeekend(month - 1, hour - 1);
			}

			bool canDischarge = m_canDischarge[period - 1];
			double discharge_percent = m_discharge_percent[period];

			double power_kW = 0;
			if (canDischarge) {
				power_kW = discharge_percent * m_fuelCellVector[fc]->getMaxPowerOriginal();
			}

			m_fuelCellVector[fc]->runSingleTimeStep(power_kW);
			m_powerTotal_kW += m_fuelCellVector[fc]->getPower();
		}
	}
	// Input dispatch
	else {
		for (size_t fc = 0; fc < m_fuelCellVector.size(); fc++) {
			double power_kW = m_dispatchInput_kW[year_idx];
			m_fuelCellVector[fc]->runSingleTimeStep(power_kW);
			m_powerTotal_kW += m_fuelCellVector[fc]->getPower();
		}
	}
	m_batteryPower->powerPV = powerSystem_kWac;
	m_batteryPower->powerLoad = powerLoad_kWac;
	m_batteryPower->powerFuelCell = m_powerTotal_kW;
	BatteryPowerFlow::Instance(dt_hour).calculate();
}

void FuelCellDispatch::setDispatchOption(int dispatchOption) {
	m_dispatchOption = dispatchOption;
}

double FuelCellDispatch::getPower(){
	return m_powerTotal_kW;
}