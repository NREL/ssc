#include <cmath>
#include "lib_utility_rate.h"


UtilityRate::UtilityRate(
	bool useRealTimePrices,
	util::matrix_t<size_t> ecWeekday, 
	util::matrix_t<size_t> ecWeekend, 
	util::matrix_t<double> ecRatesMatrix,
	std::vector<double> ecRealTimeBuy)
{
	m_useRealTimePrices = useRealTimePrices,
	m_ecWeekday = ecWeekday;
	m_ecWeekend = ecWeekend;
	m_ecRatesMatrix = ecRatesMatrix;
	m_ecRealTimeBuy = std::move(ecRealTimeBuy);
}

UtilityRate::UtilityRate(const UtilityRate& tmp){
    m_useRealTimePrices = tmp.m_useRealTimePrices;
    m_ecWeekday = tmp.m_ecWeekday;
    m_ecWeekend = tmp.m_ecWeekend;
    m_ecRatesMatrix = tmp.m_ecRatesMatrix;
    for (auto& kv : tmp.m_energyTiersPerPeriod) {
        m_energyTiersPerPeriod[kv.first] = kv.second;
    }
    m_ecRealTimeBuy = tmp.m_ecRealTimeBuy;
}


UtilityRateWithPeakLoad::UtilityRateWithPeakLoad(bool useRealTimePrices,
	util::matrix_t<size_t> ecWeekday,
	util::matrix_t<size_t> ecWeekend,
	util::matrix_t<double> ecRatesMatrix,
	std::vector<double> ecRealTimeBuy,
	util::matrix_t<double> dc_schedule_weekday,
	util::matrix_t<double> dc_schedule_weekend,
	util::matrix_t<double> dc_time_of_use,
	util::matrix_t<double> dc_flat) :
	UtilityRate(useRealTimePrices, ecWeekday, ecWeekend, ecRatesMatrix, ecRealTimeBuy)
{
	demand_charge_schedule_weekday = dc_schedule_weekday;
	demand_charge_schedule_weekend = dc_schedule_weekend;
	demand_charge_time_of_use = dc_time_of_use;
	demand_charge_flat = dc_flat;
}

UtilityRateWithPeakLoad::UtilityRateWithPeakLoad(const UtilityRateWithPeakLoad& tmp)
{
	m_useRealTimePrices = tmp.m_useRealTimePrices;
	m_ecWeekday = tmp.m_ecWeekday;
	m_ecWeekend = tmp.m_ecWeekend;
	m_ecRatesMatrix = tmp.m_ecRatesMatrix;
	for (auto& kv : tmp.m_energyTiersPerPeriod) {
		m_energyTiersPerPeriod[kv.first] = kv.second;
	}
	m_ecRealTimeBuy = tmp.m_ecRealTimeBuy;
	demand_charge_schedule_weekday = tmp.demand_charge_schedule_weekday;
	demand_charge_schedule_weekend = tmp.demand_charge_schedule_weekend;
	demand_charge_time_of_use = tmp.demand_charge_time_of_use;
	demand_charge_flat = tmp.demand_charge_flat;
}

UtilityRateCalculator::UtilityRateCalculator(UtilityRate * rate, size_t stepsPerHour) :
	UtilityRate(*rate)
{
	m_stepsPerHour = stepsPerHour;
	initializeRate();
}

UtilityRateCalculator::UtilityRateCalculator(UtilityRate * rate, size_t stepsPerHour, std::vector<double> loadProfile) :
	UtilityRate(*rate)
{
	m_stepsPerHour = stepsPerHour;
	m_loadProfile = std::move(loadProfile);
	initializeRate();
}

UtilityRateCalculator::UtilityRateCalculator(const UtilityRateCalculator& tmp):
UtilityRate(tmp){
    m_electricBill = tmp.m_electricBill;
    m_stepsPerHour = tmp.m_stepsPerHour;
    for (auto& i : tmp.m_loadProfile)
        m_loadProfile.push_back(i);
    for (auto& i : tmp.m_energyUsagePerPeriod)
        m_energyUsagePerPeriod.push_back(i);
}

void UtilityRateCalculator::initializeRate()
{
	
	if (!m_useRealTimePrices) {
		for (size_t r = 0; r != m_ecRatesMatrix.nrows(); r++)
		{
			size_t period = static_cast<size_t>(m_ecRatesMatrix(r, 0));
			size_t tier = static_cast<size_t>(m_ecRatesMatrix(r, 1));

			// assumers table is in monotonically increasing order
			m_energyTiersPerPeriod[period] = tier;

			if (tier == 1)
				m_energyUsagePerPeriod.push_back(0);
		}
	}
}

void UtilityRateCalculator::updateLoad(double loadPower)
{
	m_loadProfile.push_back(loadPower);
}
void UtilityRateCalculator::calculateEnergyUsagePerPeriod()
{
	for (size_t idx = 0; idx != m_loadProfile.size(); idx++)
	{
		size_t hourOfYear = static_cast<size_t>(std::floor(idx / m_stepsPerHour));
		size_t period = static_cast<size_t>(getEnergyPeriod(hourOfYear));
		m_energyUsagePerPeriod[period] += m_loadProfile[idx];
	}
}
double UtilityRateCalculator::getEnergyRate(size_t hourOfYear)
{

	double rate = 0;
	if (m_useRealTimePrices) {
		rate = m_ecRealTimeBuy[hourOfYear];
	}
	else {
		// period is the human readable value from the table (1-based)
		size_t period = getEnergyPeriod(hourOfYear);

		//size_t idx = m_loadProfile.size() - 1;
		//double energy = m_energyTiersPerPeriod[period];
		// add ability to check for tiered usage, for now assume one tier

		// Reduce period to 0-based index
		rate = m_ecRatesMatrix(period - 1, 4);
	}
	return rate;

}
size_t UtilityRateCalculator::getEnergyPeriod(size_t hourOfYear)
{
	size_t period, month, hour;
	util::month_hour(hourOfYear, month, hour);

	if (util::weekday(hourOfYear)) {
		if (m_ecWeekday.nrows() == 1 && m_ecWeekday.ncols() == 1) {
			period = m_ecWeekday.at(0, 0);
		}
		else {
			period = m_ecWeekday.at(month - 1, hour - 1);
		}
	}
	else {
		if (m_ecWeekend.nrows() == 1 && m_ecWeekend.ncols() == 1) {
			period = m_ecWeekend.at(0, 0);
		}
		else {
			period = m_ecWeekend.at(month - 1, hour - 1);
		}
	}
	return period;
}

UtilityRateForecast::UtilityRateForecast(UtilityRateWithPeakLoad* rate, size_t stepsPerHour) :
	UtilityRateWithPeakLoad(*rate),
	peak_power_by_time()
{
	steps_per_hour = stepsPerHour;
	last_step = 0;
	restartMonth(0); // Just starting out, so no net metering carryover
}

UtilityRateForecast::UtilityRateForecast(UtilityRateForecast& tmp) :
	UtilityRateWithPeakLoad(tmp)
{
	steps_per_hour = tmp.steps_per_hour;
	last_step = tmp.last_step;
	peak_power_to_date = tmp.peak_power_to_date;
	peak_power_by_time = tmp.peak_power_by_time;
	total_energy_to_date = tmp.total_energy_to_date;
	current_energy_tier = tmp.current_energy_tier;
	current_demand_tier = tmp.current_demand_tier;
}

double UtilityRateForecast::forecastCost(std::vector<double> predicted_loads)
{
	double cost = 0;
	size_t n = predicted_loads.size();
	for (int i = 0; i < n; i++) {

	}
	return cost;
}

void UtilityRateForecast::restartMonth(double carryOver)
{
	peak_power_to_date = 0;
	peak_power_by_time.clear();
	// TODO: does this work, or do we need to handle periods * tiers?
	for (int i = 0; i < demand_charge_time_of_use.nrows(); i++) {
		peak_power_by_time.push_back(0);
	}
	total_energy_to_date = carryOver;
	current_energy_tier = 0; // TODO: check this
	current_demand_tier = 0;
}
