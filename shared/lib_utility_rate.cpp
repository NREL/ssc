#include <cmath>
#include "lib_utility_rate.h"
#include "lib_utility_rate_equations.h"

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

UtilityRateForecast::UtilityRateForecast(rate_data* util_rate, size_t stepsPerHour, std::vector<double> monthly_load_forecast, std::vector<double> monthly_gen_forecast, std::vector<double> monthly_peak_forecast)
{
	steps_per_hour = stepsPerHour;
	last_step = 0;
	rate = util_rate;
	m_monthly_load_forecast = monthly_load_forecast;
	m_monthly_gen_forecast = monthly_gen_forecast;
	m_monthly_peak_forecast = monthly_peak_forecast;
	restartMonth(); // Just starting out, so no net metering carryover
}

UtilityRateForecast::UtilityRateForecast(UtilityRateForecast& tmp) :
	steps_per_hour(tmp.steps_per_hour),
	last_step(tmp.last_step),
	m_monthly_load_forecast(tmp.m_monthly_load_forecast),
	m_monthly_gen_forecast(tmp.m_monthly_gen_forecast),
	m_monthly_peak_forecast(tmp.m_monthly_peak_forecast)
{
	rate = new rate_data(*tmp.rate);
}

UtilityRateForecast::~UtilityRateForecast()
{
	// TODO: either take ownership of the pointer, or figure out what to do with copies
}

double UtilityRateForecast::forecastCost(std::vector<double> predicted_loads, size_t year, size_t hour_of_year, size_t step)
{
	double cost = 0;
	int month = util::month_of(hour_of_year);

	size_t n = predicted_loads.size();

	// Determine if this forecast crosses a month
	size_t hour_at_end = hour_of_year + (step + n) / steps_per_hour;
	int month_at_end = util::month_of(hour_at_end);

	bool crossing_month = month != month_at_end;
	int year_at_end = year;
	if (month_at_end < month) {
		year_at_end++;
	}

	// Get previous peak cost - may need to run two months
	double previousPeak = rate->get_demand_charge(month, year);
	if (crossing_month)
	{
		// TODO: initalize new month
		previousPeak += rate->get_demand_charge(month_at_end, year_at_end);
	}

	for (int i = 0; i < n; i++) {
		// Determine if this new step crosses a month


	}

	// Compute new peak cost - may need to run two months

	return cost;
}

// Year indexes from 1
void UtilityRateForecast::compute_next_composite_tou(int month, int year)
{
	// Adjust year to be consistent with rate data's peaking functions
	year -= 1;
	ur_month& curr_month = rate->m_month[month];
	double expected_load = m_monthly_load_forecast[year * 12 + month];
	ssc_number_t rate_esc = rate->rate_scale[year];
	next_buy_rates.clear();

	ssc_number_t num_per = (ssc_number_t)curr_month.ec_tou_br.nrows();
	if (expected_load > 0)
	{
		for (size_t ir = 0; ir < num_per; ir++)
		{
			bool done = false;
			double periodCost = 0;
			for (size_t ic = 0; ic < curr_month.ec_tou_ub.ncols() && !done; ic++)
			{
				ssc_number_t ub_tier = curr_month.ec_tou_ub.at(ir, ic);
				ssc_number_t prev_tier = 0;
				if (ic > 0)
				{
					prev_tier = curr_month.ec_tou_ub.at(ir, ic - 1);
				}

				if (expected_load > ub_tier)
				{
					periodCost += (ub_tier - prev_tier) / expected_load * curr_month.ec_tou_br.at(ir, ic) * rate_esc;
				}
				else
				{
					periodCost += (expected_load - prev_tier) / expected_load * curr_month.ec_tou_br.at(ir, ic) * rate_esc;
					done = true;
				}
				
			}
			next_buy_rates.push_back(periodCost);
		}
	}
	else
	{
		for (size_t ir = 0; ir < num_per; ir++)
		{
			double periodBuyRate = curr_month.ec_tou_br.at(ir, 0) * rate_esc;
			next_buy_rates.push_back(periodBuyRate);
		}
	}

	// repeat for surplus
	// TODO: need to incorperate data about rate schedule here. Net metering will be the year end sell rate in all tiers, whereas others have explicit sell rates
	double expected_gen = m_monthly_gen_forecast[year * 12 + month];
	next_sell_rates.clear();
	num_per = (ssc_number_t)curr_month.ec_tou_sr.nrows();

	if (expected_gen > 0)
	{
		for (size_t ir = 0; ir < num_per; ir++)
		{
			bool done = false;
			double periodSellRate = 0;
			for (size_t ic = 0; ic < curr_month.ec_tou_ub.ncols() && !done; ic++)
			{
				ssc_number_t ub_tier = curr_month.ec_tou_ub.at(ir, ic);
				ssc_number_t prev_tier = 0;
				if (ic > 0)
				{
					prev_tier = curr_month.ec_tou_ub.at(ir, ic - 1);
				}

				if (expected_gen > ub_tier)
				{
					periodSellRate += (ub_tier - prev_tier) / expected_gen * curr_month.ec_tou_sr.at(ir, ic) * rate_esc;
				}
				else
				{
					periodSellRate += (expected_gen - prev_tier) / expected_gen * curr_month.ec_tou_sr.at(ir, ic) * rate_esc;
					done = true;
				}
			}
			next_sell_rates.push_back(periodSellRate);
		}
	}
	else
	{
		for (size_t ir = 0; ir < num_per; ir++)
		{
			double periodSellRate = curr_month.ec_tou_sr.at(ir, 0) * rate_esc;
			next_sell_rates.push_back(periodSellRate);
		}
	}
}

void UtilityRateForecast::initializeMonth(int month, int year)
{
	if (next_buy_rates.size() == 0 || next_buy_rates == current_buy_rates)
	{
		compute_next_composite_tou(month, year);

		double avg_load = m_monthly_load_forecast[month] / util::hours_in_month(month);

		ur_month& curr_month = rate->m_month[month];
		curr_month.dc_flat_peak = avg_load;
		for (int period = 0; period < (int)curr_month.dc_tou_ub.nrows(); period++)
		{
			curr_month.dc_tou_peak[period] = avg_load;
		}
	}
}

void UtilityRateForecast::restartMonth()
{
	
}

double UtilityRateForecast::getPreviousDemandCharge(int month)
{
	return rate->monthly_dc_fixed[month] + rate->monthly_dc_tou[month];
}
