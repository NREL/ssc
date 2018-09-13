#include <cmath>
#include "lib_utility_rate.h"


UtilityRate::UtilityRate(util::matrix_t<size_t> ecWeekday, util::matrix_t<size_t> ecWeekend, util::matrix_t<double> ecRatesMatrix)
{
	m_ecWeekday = ecWeekday;
	m_ecWeekend = ecWeekend;
	m_ecRatesMatrix = ecRatesMatrix;
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
	m_loadProfile = loadProfile;
	initializeRate();
}

void UtilityRateCalculator::initializeRate()
{
	
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

	size_t period = getEnergyPeriod(hourOfYear);

	//size_t idx = m_loadProfile.size() - 1;
	//double energy = m_energyTiersPerPeriod[period];
	// add ability to check for tiered usage, for now assume one tier

	return m_ecRatesMatrix(period, 4);

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
