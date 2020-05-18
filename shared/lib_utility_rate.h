#ifndef _LIB_UTILITY_RATE_H_
#define _LIB_UTILITY_RATE_H_

#include "lib_util.h"
#include <map>

class UtilityRate
{
public:
	
	UtilityRate(){};

	UtilityRate(bool useRealTimePrices,
		util::matrix_t<size_t> ecWeekday, 
		util::matrix_t<size_t> ecWeekend, 
		util::matrix_t<double> ecRatesMatrix,
		std::vector<double> ecRealTimeBuy);

	UtilityRate(const UtilityRate& tmp);

	virtual ~UtilityRate() {/* nothing to do */ };

protected:
	/// Energy charge schedule for weekdays
	util::matrix_t<size_t> m_ecWeekday;

	/// Energy charge schedule for weekends
	util::matrix_t<size_t> m_ecWeekend;

	/// Energy charge periods, tiers, maxes, units, buy rate, (sell rate optional)
	util::matrix_t<double> m_ecRatesMatrix;

	/// Energy Tiers per period
	std::map<size_t, size_t> m_energyTiersPerPeriod;

	/// Real time energy prices
	std::vector<double> m_ecRealTimeBuy;

	/// Use real time prices or not
	bool m_useRealTimePrices;
};

class UtilityRateWithPeakLoad : protected UtilityRate {

public:

	UtilityRateWithPeakLoad(bool useRealTimePrices,
		util::matrix_t<size_t> ecWeekday,
		util::matrix_t<size_t> ecWeekend,
		util::matrix_t<double> ecRatesMatrix,
		std::vector<double> ecRealTimeBuy,
		util::matrix_t<double> dc_schedule_weekday,
		util::matrix_t<double> dc_schedule_weekend,
		util::matrix_t<double> dc_time_of_use,
		util::matrix_t<double> dc_flat);

	UtilityRateWithPeakLoad(const UtilityRateWithPeakLoad& tmp);

	virtual ~UtilityRateWithPeakLoad() {/* nothing to do */ };

protected:
	util::matrix_t<double> demand_charge_schedule_weekday;
	util::matrix_t<double> demand_charge_schedule_weekend;
	util::matrix_t<double> demand_charge_time_of_use;
	util::matrix_t<double> demand_charge_flat;
};

class UtilityRateCalculator : protected UtilityRate
{
public:
	/// Constructor for rate calculator where load will be input on the fly
	UtilityRateCalculator(UtilityRate * Rate, size_t stepsPerHour);

	/// Constructor for rate calculator where full load is known
	UtilityRateCalculator(UtilityRate * Rate, size_t stepsPerHour, std::vector<double> loadProfile);

	/// Copy Ctor
	UtilityRateCalculator(const UtilityRateCalculator& tmp);

	/// Parse the incoming data
	void initializeRate();

	/// Update the bill to include the load at the current timestep
	void updateLoad(double loadPower);

	/// Calculate the utility bill for the full load
	void calculateEnergyUsagePerPeriod();

	/// Get the energy rate at the given hour of year
	double getEnergyRate(size_t );

	/// Get the period for a given hour of year
	size_t getEnergyPeriod(size_t hourOfYear);

	virtual ~UtilityRateCalculator() {/* nothing to do*/ };

protected:

	/// The load profile to evaluate (kW)
	std::vector<double> m_loadProfile;
	
	/// The calculated electricity bill for the UtilityRate and load profile ($)
	double m_electricBill;

	/// The number of time steps per hour
	size_t m_stepsPerHour;

	/// The energy usage per period
	std::vector<double> m_energyUsagePerPeriod;
};


class UtilityRateForecast : protected UtilityRateWithPeakLoad
{
public:
	UtilityRateForecast(UtilityRateWithPeakLoad* rate, size_t stepsPerHour);

	UtilityRateForecast(UtilityRateForecast& tmp);

	~UtilityRateForecast() {/* Nothing to do */ };

	double forecastCost(std::vector<double> predicted_loads);

	void updateWithLoad(double load);

protected:

	// Net metering carryover should be <= 0, units kWh
	void restartMonth(double carryOver);

	size_t steps_per_hour;

	size_t last_step;

	double peak_power_to_date; // For flat demand charges

	std::vector<double> peak_power_by_time;

	double total_energy_to_date;

	double current_energy_tier;

	double current_demand_tier; // Rare, but can be input via the GUI
};

#endif // !_LIB_UTILITY_RATE_H_
