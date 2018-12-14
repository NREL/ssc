#include <iterator>
#include <cmath>

#include "lib_fuel_cell.h" 

FuelCell::FuelCell() { /* Nothing to do */ };

FuelCell::FuelCell(double unitPowerMax_kW, double unitPowerMin_kW, double startup_hours, 
	double dynamicResponseUp_kWperHour, double dynamicResponseDown_kWperHour,
	double degradation_kWperHour, double degradationRestart_kW,
	size_t replacementOption, double replacement_percent, std::vector<size_t> replacementSchedule,
	util::matrix_t<double> efficiencyTable,
	double lowerHeatingValue_BtuPerFt3, double higherHeatingValue_BtuPerFt3, double availableFuel_Mcf,
	int shutdownOption, double dt_hour) :
	m_unitPowerMax_kW(unitPowerMax_kW), m_unitPowerMin_kW(unitPowerMin_kW), m_startup_hours(startup_hours),
	m_dynamicResponseUp_kWperHour(dynamicResponseUp_kWperHour), m_dynamicResponseDown_kWperHour(dynamicResponseDown_kWperHour),
	m_degradation_kWperHour(degradation_kWperHour), m_degradationRestart_kW(degradationRestart_kW),
	m_replacementOption(replacementOption), m_replacement_percent(replacement_percent * 0.01), m_replacementSchedule(replacementSchedule),
	m_efficiencyTable(efficiencyTable), m_lowerHeatingValue_BtuPerFt3(lowerHeatingValue_BtuPerFt3),
	m_higherHeatingValue_BtuPerFt3(higherHeatingValue_BtuPerFt3),
	m_availableFuel_MCf(availableFuel_Mcf), m_shutdownOption(shutdownOption), dt_hour(dt_hour),
	m_powerMax_kW(unitPowerMax_kW), m_power_kW(0), m_powerPrevious_kW(0), m_replacementCount(0)
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

		m_fuelConsumptionMap_MCf[m_efficiencyTable.at(r, FC_EFFICIENCY_COLUMN::PERCENT_MAX)] = (fuelConsumption_Mcf);
		m_efficiencyMap[m_efficiencyTable.at(r, FC_EFFICIENCY_COLUMN::PERCENT_MAX)] = m_efficiencyTable.at(r, FC_EFFICIENCY_COLUMN::PRECENT_ELECTRICAL_EFFICIENCY);
		m_heatRecoveryMap[m_efficiencyTable.at(r, FC_EFFICIENCY_COLUMN::PERCENT_MAX)] = m_efficiencyTable.at(r, FC_EFFICIENCY_COLUMN::PERCENT_HEAT_RECOVERY);
	}

	// Assumption: Fuel Cell is not startup up initially
	init();
}

FuelCell::~FuelCell(){ /* Nothing to do */}


FuelCell::FuelCell(const FuelCell &fuelCell) : 
	m_dynamicResponseUp_kWperHour(fuelCell.m_dynamicResponseUp_kWperHour),
	m_dynamicResponseDown_kWperHour(fuelCell.m_dynamicResponseDown_kWperHour),
	m_degradation_kWperHour(fuelCell.m_degradation_kWperHour), 
	m_degradationRestart_kW(fuelCell.m_degradationRestart_kW),
	m_replacement_percent(fuelCell.m_replacement_percent * 0.01), 
	m_efficiencyTable(fuelCell.m_efficiencyTable), 
	m_lowerHeatingValue_BtuPerFt3(fuelCell.m_lowerHeatingValue_BtuPerFt3),
	m_higherHeatingValue_BtuPerFt3(fuelCell.m_higherHeatingValue_BtuPerFt3),
	m_availableFuel_MCf(fuelCell.m_availableFuel_MCf), 
	m_shutdownOption(fuelCell.m_shutdownOption), 
	m_startup_hours(fuelCell.m_startup_hours),
	dt_hour(fuelCell.dt_hour),
	m_unitPowerMin_kW(fuelCell.m_unitPowerMin_kW),
	m_unitPowerMax_kW(fuelCell.m_unitPowerMax_kW)
{
	init();
}

void FuelCell::init() {
	m_startedUp = 0;
	m_hoursSinceStart = 0;
	m_powerMax_kW = m_unitPowerMax_kW;
	m_power_kW = 0;
	m_powerThermal_kW = 0;
	m_powerPrevious_kW = 0;
	m_fuelConsumed_MCf = 0;
	m_replacementCount = 0;
	m_hour = 0;
	m_year = 0;
}

bool FuelCell::isRunning() {
	return m_startedUp;
}
double FuelCell::interpolateMap(double key, std::map<double, double> mapDouble) {
	
	double p1, p2, f1, f2, f, m;
	p1 = p2 = f1 = f2 = f = m = 0;

	for (auto fc = mapDouble.begin(); fc != mapDouble.end(); fc++) {
		auto fc_next = std::next(fc, 1);
		auto fc_end = mapDouble.rbegin();

		if (key == fc->first) {
			f = fc->second;
			break;
		}
		else if (key == fc_next->first) {
			f = fc_next->second;
			break;
		}
		// interpolate to get the fuel consumption
		else if (key > fc->first && key < fc_next->first) {
			p1 = fc->first;
			p2 = fc_next->first;
			f1 = fc->second;
			f2 = fc_next->second;

			if (fabs(p2 - p1) > 0) {
				m = (f2 - f1) / (p2 - p1);
				f = f1 + m * key;
			}
			break;
		}
		// if percent is higher than the max key, return the max fuel consumption
		else if (key > fc_end->first) {
			f = fc_end->second;
			break;
		}
	}
	return f;
}


void FuelCell::calculateEfficiencyCurve(double percent) {
	
	m_fuelConsumed_MCf = interpolateMap(percent, m_fuelConsumptionMap_MCf);
	m_efficiency_percent = interpolateMap(percent, m_efficiencyMap);
	m_heatRecovery_percent = interpolateMap(percent, m_heatRecoveryMap);
}

double FuelCell::getPercentLoad() {
	return m_power_kW / (m_unitPowerMax_kW);
}

double FuelCell::getPowerResponse(double power_kW) {
	
	double dP = (power_kW - m_powerPrevious_kW) / dt_hour;
	double dP_max = 0.0;

	// ramp up
	if (dP > 0) {
		dP_max = fmin(fabs(dP), m_dynamicResponseUp_kWperHour * dt_hour);
	}
	// ramp down
	else {
		dP_max = fmin(fabs(dP), m_dynamicResponseDown_kWperHour * dt_hour);
	}
	double sign = fabs(dP) > 0 ? dP / fabs(dP) : 1.0;
	return (m_powerPrevious_kW + (dP_max * sign));
}
double FuelCell::getPower() {
	return m_power_kW;
}
double FuelCell::getPowerThermal() {
	return m_powerThermal_kW;
}
double FuelCell::getMaxPowerOriginal() {
	return m_unitPowerMax_kW;
}
double FuelCell::getMaxPower() {
	return m_powerMax_kW;
}
double FuelCell::getFuelConsumption() {
	return m_fuelConsumed_MCf;
}
double FuelCell::getAvailableFuel() {
	return m_availableFuel_MCf;
}
double FuelCell::getElectricalEfficiency() {
	return m_efficiency_percent;
}
double FuelCell::getHeatRecoveryEfficiency() {
	return m_heatRecovery_percent;
}

// Assume that min turndown trumps dynamic response, i.e, that fuel cell
// can go from 0 to min turndown instantaneously.
void FuelCell::checkMinTurndown() {
	if (m_power_kW < m_unitPowerMin_kW && m_power_kW > 0) {
		m_power_kW = m_unitPowerMin_kW;
	}
	else if (m_power_kW == 0) {
		if (m_shutdownOption == FuelCell::FC_SHUTDOWN_OPTION::IDLE) {
			m_power_kW = m_unitPowerMin_kW;
		}
		else {
			m_startedUp = 0;
			m_hoursSinceStart = 0;
		}
	}
}
void FuelCell::checkMaxLimit() {
	if (m_power_kW > m_unitPowerMax_kW) {
		m_power_kW = m_unitPowerMax_kW;
	}
}

void FuelCell::checkAvailableFuel() {
	m_availableFuel_MCf -= m_fuelConsumed_MCf;

	if (m_availableFuel_MCf <= 0) {
		m_startedUp = false;
		m_hoursSinceStart = 0;
	}
}

void FuelCell::applyDegradation() {
	if (isRunning()) {
		m_powerMax_kW -= m_degradation_kWperHour * dt_hour;
		m_power_kW = fmin(m_power_kW, m_powerMax_kW);
	}
	else if (m_powerPrevious_kW > 0 && m_hoursSinceStart == 0) {
		m_powerMax_kW -= m_degradationRestart_kW;
		if (m_powerMax_kW < 0) {
			m_powerMax_kW = 0;
		}
	}

	if (m_replacementOption == FC_REPLACEMENT_OPTION::REPLACE_AT_CAPACITY) {
		if (m_powerMax_kW < m_unitPowerMax_kW * m_replacement_percent) {
			m_powerMax_kW = m_unitPowerMax_kW;
			m_replacementCount += 1;
		}
	}
	else if (m_replacementOption == FC_REPLACEMENT_OPTION::REPLACE_ON_SCHEDULE) {
		int hour = (int)std::floor(m_hour);
		if (hour % 8760 == 0 && m_replacementSchedule[m_year] > 0) {
			m_powerMax_kW = m_unitPowerMax_kW;
			m_replacementCount += 1;
		} 
	}
	if (m_powerMax_kW <= m_unitPowerMin_kW) {
		m_power_kW = 0;
		m_startedUp = 0;
		m_hoursSinceStart = 0;
	}
}

void FuelCell::applyEfficiency() {
	calculateEfficiencyCurve(getPercentLoad());
	m_powerThermal_kW = m_power_kW;
	m_powerThermal_kW *= m_heatRecovery_percent;
}

void FuelCell::calculateTime() {
	m_hour += dt_hour;
	int hour = (int)std::floor(m_hour);

	if (hour % 8760 == 0) {
		m_year++;
	}
}


void FuelCell::runSingleTimeStep(double power_kW) {

	m_powerPrevious_kW = m_power_kW;

	// Initialize based on dynamic response limits
	if (isRunning()) {
		m_power_kW = getPowerResponse(power_kW);
		checkMinTurndown();
		checkMaxLimit();
		applyEfficiency();
		checkAvailableFuel();
	}
	else {
		if ((power_kW > 0 || m_hoursSinceStart > 0) && m_availableFuel_MCf > 0 && m_powerMax_kW > m_unitPowerMin_kW){
			m_hoursSinceStart += dt_hour;
			if (m_hoursSinceStart == m_startup_hours) {
				m_startedUp = true;
			}
		}
	}
	calculateTime();
	applyDegradation();
	
}