#include "lib_fuel_cell.h"

FuelCell::FuelCell() { /* Nothing to do */ };

FuelCell::FuelCell(size_t numberOfUnits, double unitPowerMax_kW, double unitPowerMin_kW, double startup_hours, double dynamicResponse_kWperMin,
	double degradation_kWperHour, double degradationRestart_kW, double replacement_percent, util::matrix_t<double> efficiencyTable,
	double lowerHeatingValue_BtuPerFt3, double higherHeatingValue_BtuPerFt3, double availableFuel_Mcf,
	int shutdownOption, int dispatchOption, double dt_hour) :
	m_numberOfUnits(numberOfUnits), m_unitPowerMax_kW(unitPowerMax_kW), m_unitPowerMin_kW(unitPowerMin_kW), m_startup_hours(startup_hours),
	m_dynamicResponse_kWperMin(dynamicResponse_kWperMin), m_degradation_kWperHour(degradation_kWperHour), m_degradationRestart_kW(degradationRestart_kW),
	m_replacement_percent(replacement_percent), m_efficiencyTable(efficiencyTable), m_lowerHeatingValue_BtuPerFt3(lowerHeatingValue_BtuPerFt3),
	m_higherHeatingValue_BtuPerFt3(higherHeatingValue_BtuPerFt3),
	m_availableFuel_MCf(availableFuel_Mcf), m_shutdownOption(shutdownOption), m_dispatchOption(dispatchOption), dt_hour(dt_hour)
{
	// Calculate fuel consumption based on inputs
	for (size_t r = 0; r < m_efficiencyTable.nrows(); r++) {

		// Convert to 0-1 from 0 - 100
		m_efficiencyTable.at(r, 0) *= 0.01;
		m_efficiencyTable.at(r, 1) *= 0.01;
		m_efficiencyTable.at(r, 2) *= 0.01;
	
		// Fuel consumption kWh = dt_hour * operating power kW / (efficiency at LHV * Fuel HHV / Fuel LHV)
		double fuelConsumption_kWh = dt_hour * m_efficiencyTable.at(r, FC_EFFICIENCY_COLUMN::PERCENT_MAX) * m_unitPowerMax_kW / (m_efficiencyTable.at(r, FC_EFFICIENCY_COLUMN::PRECENT_ELECTRICAL_EFFICIENCY) * m_higherHeatingValue_BtuPerFt3 / m_lowerHeatingValue_BtuPerFt3);

		// Fuel consumption in terms of MCf;


		m_fuelConsumption.push_back(0.);
	}

	// Assumption 1: Fuel Cell is not startup up initially
	m_startedUp = false;
	m_hoursSinceStart = 0.0;

	// Assumption 2: Fuel Cell starts up if allowed to idle at min power
	if (shutdownOption == FuelCell::FC_SHUTDOWN_OPTION::IDLE) {
		m_hoursSinceStart += dt_hour;
	}
}

FuelCell::~FuelCell(){ /* Nothing to do */}
