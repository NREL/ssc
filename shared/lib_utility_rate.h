#ifndef _LIB_UTILITY_RATE_H_
#define _LIB_UTILITY_RATE_H_

#include "lib_util.h"
#include "lib_utility_rate_equations.h"
#include <map>

class UtilityRate
{
public:

    /*
     * Original class for determining costs for grid charging in frong of the meter batteries
     * It may be possible to replace this with rate_data in UtilityRateCalculator in order to handle tiers and demand charges as appropriate for FOM
     */
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

    /*
     * Full forecast function using utility rate data. Computes the impact of tiers, time of use, and time sereis buy and sell rates
     * *_forecast vectors need to be 12 * analysis_period in length. Predictions are used to estimate which tiers will be used for energy charges.
     */
	UtilityRateForecast(rate_data* util_rate, size_t stepsPerHour, std::vector<double> monthly_load_forecast, std::vector<double> monthly_gen_forecast, std::vector<double> monthly_peak_forecast, size_t analysis_period);

	UtilityRateForecast(UtilityRateForecast& tmp);

	~UtilityRateForecast();

	// initialize first month prior to calling this function
	double forecastCost(std::vector<double>& predicted_loads, size_t year, size_t hour_of_year, size_t step);

    // Runs when the new month appears in the forecast for the first time. Year accounts for inflation and other pricing escalations
    void initializeMonth(int month, int year);
    // Runs when the start of the forecast is in the new month. Copies next rates onto current rates
	void copyTOUForecast();
	
	// Public for testing
	void compute_next_composite_tou(int month, int year);

	std::vector<double> current_sell_rates; // Sell rates at the start of the forecast
	std::vector<double> current_buy_rates;
	std::vector<double> next_sell_rates; // Sell rates if the forecast crosses into the next month
	std::vector<double> next_buy_rates;
protected:

    /* Transfer net metering surplus credits from previous month to current month */
	void restartMonth(int prevMonth, int currentMonth, int year);

    double getEnergyChargeNetMetering(int month, std::vector<double>& buy_rates, std::vector<double>& sell_rates, bool crossing_month);
    double getEnergyChargeNetBillingOrTimeSeries(double energy, int year_one_index, int current_month, int year, bool use_next_month);

	std::shared_ptr<rate_data> rate;

	size_t steps_per_hour;
	float dt_hour;

	size_t last_step;
    int last_month_init;
    size_t nyears;

	std::vector<double> m_monthly_load_forecast; // Length is 12 * analysis period
	std::vector<double> m_monthly_gen_forecast; // Length is 12 * analysis period
	std::vector<double> m_monthly_peak_forecast; // Length is 12 * analysis period


};

#endif // !_LIB_UTILITY_RATE_H_
