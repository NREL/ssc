#include <iterator>
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
	
		double fuelConsumption_Mcf = 0.0;

		if (m_efficiencyTable.at(r, FC_EFFICIENCY_COLUMN::PERCENT_MAX) > 0 && 
			m_efficiencyTable.at(r, FC_EFFICIENCY_COLUMN::PRECENT_ELECTRICAL_EFFICIENCY) > 0) {
			// Fuel consumption Btu = dt_hour * operating power kW / (efficiency at LHV * Fuel HHV / Fuel LHV) * BTU_PER_KWH
			double fuelConsumption_Btu = BTU_PER_KWH * dt_hour * m_efficiencyTable.at(r, FC_EFFICIENCY_COLUMN::PERCENT_MAX) * m_unitPowerMax_kW / (m_efficiencyTable.at(r, FC_EFFICIENCY_COLUMN::PRECENT_ELECTRICAL_EFFICIENCY) * m_higherHeatingValue_BtuPerFt3 / m_lowerHeatingValue_BtuPerFt3);
			fuelConsumption_Mcf = fuelConsumption_Btu / (m_lowerHeatingValue_BtuPerFt3 * 1000);
		}

		m_fuelConsumption_MCf[m_efficiencyTable.at(r, FC_EFFICIENCY_COLUMN::PERCENT_MAX)] = (fuelConsumption_Mcf);
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

bool FuelCell::isRunning() {
	return m_startedUp;
}

double FuelCell::getFuelConsumptionMCf(double percent) {
	
	double p1, p2, f1, f2, f, m;
	p1 = p2 = f1 = f2 = f = m = 0;
	for (auto fc = m_fuelConsumption_MCf.begin(); fc != m_fuelConsumption_MCf.end(); fc++) {
		auto fc_next = std::next(fc, 1);
		auto fc_end = m_fuelConsumption_MCf.rbegin();

		if (percent == fc->first) {
			f = fc->second;
			break;
		}
		else if (percent == fc_next->first) {
			f = fc_next->second;
			break;
		}
		// interpolate to get the fuel consumption
		else if (percent > fc->first && percent < fc_next->first) {
			p1 = fc->first;
			p2 = fc_next->first;
			f1 = fc->second;
			p2 = fc_next->second;

			if (fabs(p2 - p1) > 0) {
				m = (f2 - f1) / (p2 - p1);
				f = f1 + m * percent;
			}
			break;
		}
		// if percent is higher than the max key, return the max fuel consumption
		else if (percent > fc_end->first) {
			f = fc_end->second;
			break;
		}
	}

	return f;
}