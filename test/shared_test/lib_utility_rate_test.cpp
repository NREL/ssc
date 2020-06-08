#include <gtest/gtest.h>
#include <lib_utility_rate.h>
#include <lib_utility_rate_equations.h>

TEST(lib_utility_rate_test, test_copy)
{
	ssc_number_t p_ur_ec_sched_weekday[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1 };
	ssc_number_t p_ur_ec_sched_weekend[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_number_t p_ur_ec_tou_mat[24] = { 1, 1, 9.9999999999999998e+37, 0, 0.10000000000000001, 0,
        2, 1, 9.9999999999999998e+37, 0, 0.050000000000000003, 0,
        3, 1, 9.9999999999999998e+37, 0, 0.20000000000000001, 0,
        4, 1, 9.9999999999999998e+37, 0, 0.25, 0 };
	size_t tou_rows = 4;
	bool sell_eq_buy = false;

	rate_data data;
	data.init();
	data.setup_energy_rates(&p_ur_ec_sched_weekday[0], &p_ur_ec_sched_weekend[0], tou_rows, &p_ur_ec_tou_mat[0], sell_eq_buy);

	int steps_per_hour = 1;
	// TODO - fill these out
	std::vector<double> monthly_load_forecast;
	std::vector<double> monthly_gen_forecast;
	std::vector<double> monthly_peak_forecast;
	UtilityRateForecast rate_forecast_1(&data, steps_per_hour, monthly_load_forecast, monthly_gen_forecast, monthly_peak_forecast);



}

TEST(lib_utility_rate_test, test_tiered_tou_cost_estimates)
{
	// Rate is PG&E's EV9A from https://www.pge.com/includes/docs/pdfs/shared/environment/pge/cleanair/electricdrivevehicles/PEV_rate_options.pdf
	// TODO: schedule is not accurate. Update the weekday/weekend schedules if you're going to use this to test any other functions
	ssc_number_t p_ur_ec_sched_weekday[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1 };
	ssc_number_t p_ur_ec_sched_weekend[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_number_t p_ur_ec_tou_mat[90] = { 1, 1, 8.3, 0, 0.03461, 0,
		1, 2, 10.8, 0, 0.05140, 0,
		1, 3, 16.6, 0, 0.14698, 0,
		1, 4, 24.9, 0, 0.18727, 0,
		1, 5, 9.9999999999999998e+37, 0, 0.18727, 0,
		2, 1, 8.3, 0, 0.09132, 0,
		2, 2, 10.8, 0, 0.10811, 0,
		2, 3, 16.6, 0, 0.26397, 0,
		2, 4, 24.9, 0, 0.3733, 0,
		2, 5, 9.9999999999999998e+37, 0, 0.3733, 0,
		3, 1, 8.3, 0, 0.27904, 0,
		3, 2, 10.8, 0, 0.29583, 0,
		3, 3, 16.6, 0, 0.45169, 0,
		3, 4, 24.9, 0, 0.5611, 0,
		3, 5, 9.9999999999999998e+37, 0, 0.5611, 0, };
	size_t tou_rows = 15;
	bool sell_eq_buy = false;

	rate_data data;
	data.m_num_rec_yearly = 8760;
	data.init();
	data.setup_energy_rates(&p_ur_ec_sched_weekday[0], &p_ur_ec_sched_weekend[0], tou_rows, &p_ur_ec_tou_mat[0], sell_eq_buy);
	data.rate_scale.push_back(1.0);

	int steps_per_hour = 1;
	// TODO - fill these out
	std::vector<double> monthly_load_forecast;
	monthly_load_forecast.push_back(25); // Rates above are kWh/day, but don't want to test that just yet
	std::vector<double> monthly_gen_forecast;
	monthly_gen_forecast.push_back(0);
	std::vector<double> monthly_peak_forecast;
	monthly_peak_forecast.push_back(7);

	UtilityRateForecast rate_forecast(&data, steps_per_hour, monthly_load_forecast, monthly_gen_forecast, monthly_peak_forecast);

	rate_forecast.compute_next_composite_tou(0, 0);

	ASSERT_EQ(3, rate_forecast.next_buy_rates.size());

	EXPECT_NEAR(0.11365, rate_forecast.next_buy_rates[0], 0.0001);
	EXPECT_NEAR(0.22782, rate_forecast.next_buy_rates[1], 0.0001);
	EXPECT_NEAR(0.41554, rate_forecast.next_buy_rates[2], 0.0001);

	ASSERT_EQ(3, rate_forecast.next_sell_rates.size());

	EXPECT_NEAR(0.0, rate_forecast.next_sell_rates[0], 0.0001);
	EXPECT_NEAR(0.0, rate_forecast.next_sell_rates[1], 0.0001);
	EXPECT_NEAR(0.0, rate_forecast.next_sell_rates[2], 0.0001);
}

TEST(lib_utility_rate_test, test_tiered_sell_rates)
{
	// Rate is PG&E's EV9A from https://www.pge.com/includes/docs/pdfs/shared/environment/pge/cleanair/electricdrivevehicles/PEV_rate_options.pdf
	// TODO: schedule is not accurate. Update the weekday/weekend schedules if you're going to use this to test any other functions
	ssc_number_t p_ur_ec_sched_weekday[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1 };
	ssc_number_t p_ur_ec_sched_weekend[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_number_t p_ur_ec_tou_mat[90] = { 1, 1, 8.2, 0, 0.03461, 0.02,
		1, 2, 10.8, 0, 0.05140, 0.02,
		1, 3, 9.9999999999999998e+37, 0, 0.18727, 0.02,
		2, 1, 8.3, 0, 0.09132, 0.03,
		2, 2, 10.8, 0, 0.10811, 0.01,
		2, 3, 9.9999999999999998e+37, 0, 0.3733, 0.01 };
	size_t tou_rows = 6;
	bool sell_eq_buy = false;

	rate_data data;
	data.m_num_rec_yearly = 8760;
	data.init();
	data.setup_energy_rates(&p_ur_ec_sched_weekday[0], &p_ur_ec_sched_weekend[0], tou_rows, &p_ur_ec_tou_mat[0], sell_eq_buy);
	data.rate_scale.push_back(1.0);

	int steps_per_hour = 1;
	// TODO - fill these out
	std::vector<double> monthly_load_forecast;
	monthly_load_forecast.push_back(0); // Rates above are kWh/day, but don't want to test that just yet
	std::vector<double> monthly_gen_forecast;
	monthly_gen_forecast.push_back(10);
	std::vector<double> monthly_peak_forecast;
	monthly_peak_forecast.push_back(7);

	UtilityRateForecast rate_forecast(&data, steps_per_hour, monthly_load_forecast, monthly_gen_forecast, monthly_peak_forecast);

	rate_forecast.compute_next_composite_tou(0, 0);

	ASSERT_EQ(2, rate_forecast.next_buy_rates.size());

	EXPECT_NEAR(0.03461, rate_forecast.next_buy_rates[0], 0.0001);
	EXPECT_NEAR(0.09132, rate_forecast.next_buy_rates[1], 0.0001);

	ASSERT_EQ(2, rate_forecast.next_sell_rates.size());

	EXPECT_NEAR(0.02, rate_forecast.next_sell_rates[0], 0.0001);
	EXPECT_NEAR(0.0266, rate_forecast.next_sell_rates[1], 0.0001);
}