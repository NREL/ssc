#ifndef _LIB_UTILITY_RATE_H_
#define _LIB_UTILITY_RATE_H_

#include "lib_util.h"
#include "lib_utility_rate_equations.h"
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


class UtilityRateForecast
{
public:
	UtilityRateForecast(rate_data* util_rate, size_t stepsPerHour, std::vector<double> monthly_load_forecast, std::vector<double> monthly_gen_forecast, std::vector<double> monthly_peak_forecast);

	UtilityRateForecast(UtilityRateForecast& tmp);

	~UtilityRateForecast();

	double forecastCost(std::vector<double> predicted_loads);

	void updateWithLoad(double load);

	// Public for testing
	void compute_next_composite_tou(int month, int year);

	std::vector<double> current_sell_rates;
	std::vector<double> current_buy_rates;
	std::vector<double> next_sell_rates;
	std::vector<double> next_buy_rates;
protected:

	void restartMonth();

	rate_data* rate;

	size_t steps_per_hour;

	size_t last_step;

	std::vector<double> m_monthly_load_forecast; // Length is 12 * analysis period
	std::vector<double> m_monthly_gen_forecast; // Length is 12 * analysis period
	std::vector<double> m_monthly_peak_forecast; // Length is 12 * analysis period


};

#endif // !_LIB_UTILITY_RATE_H_
