#include <gtest/gtest.h>

#include "cmod_grid_test.h"

TEST_F(CMGrid, SingleYearHourly_cmod_grid) {

	grid_default_60_min(data);

	// Run with fixed output
	int errors = run_module(data, "grid");
	EXPECT_FALSE(errors);
	if (!errors)
	{
		ssc_number_t capacity_factor, annual_energy_pre_interconnect, annual_energy_interconnect;
		int gen_size;
		ssc_data_get_number(data, "capacity_factor_interconnect_ac", &capacity_factor);
		ssc_data_get_number(data, "annual_energy_pre_interconnect_ac", &annual_energy_pre_interconnect);
		ssc_data_get_number(data, "annual_energy", &annual_energy_interconnect);
		ssc_number_t * system_kwac = GetArray("gen", gen_size);
		ssc_number_t * system_pre_interconnect_kwac = GetArray("system_pre_interconnect_kwac", gen_size);

		EXPECT_EQ(gen_size, 8760);
		EXPECT_NEAR(capacity_factor, 29.0167, 0.001);
		EXPECT_NEAR(annual_energy_interconnect, 457534759, 10);
		EXPECT_NEAR(annual_energy_pre_interconnect, 460351000, 10);
	}
}

TEST_F(CMGrid, SingleYearSubHourly_cmod_grid) {

	grid_default_30_min(data);
	int errors = run_module(data, "grid");
	EXPECT_FALSE(errors);
	if (!errors)
	{
		ssc_number_t capacity_factor, annual_energy_pre_interconnect, annual_energy_interconnect;
		int gen_size;
		ssc_data_get_number(data, "capacity_factor_interconnect_ac", &capacity_factor);
		ssc_data_get_number(data, "annual_energy_pre_interconnect_ac", &annual_energy_pre_interconnect);
		ssc_data_get_number(data, "annual_energy", &annual_energy_interconnect);
		ssc_number_t * system_kwac = GetArray("gen", gen_size);
		ssc_number_t * system_pre_interconnect_kwac = GetArray("system_pre_interconnect_kwac", gen_size);

		EXPECT_EQ(gen_size, 8760*2);
		EXPECT_NEAR(capacity_factor, 30.78777, 0.001);
		EXPECT_NEAR(annual_energy_interconnect, 485461500, 10);
		EXPECT_NEAR(annual_energy_pre_interconnect, 997640000, 10);
	}
}

TEST_F(CMGrid, SingleYearSubHourlyLifetime_cmod_grid) {

	grid_default_30_min_lifetime(data);
	int errors = run_module(data, "grid");
	EXPECT_FALSE(errors);
	if (!errors)
	{
		ssc_number_t capacity_factor, annual_energy_pre_interconnect, annual_energy_interconnect;
		int gen_size;
		ssc_data_get_number(data, "capacity_factor_interconnect_ac", &capacity_factor);
		ssc_data_get_number(data, "annual_energy_pre_interconnect_ac", &annual_energy_pre_interconnect);
		ssc_data_get_number(data, "annual_energy", &annual_energy_interconnect);
		ssc_number_t * system_kwac = GetArray("gen", gen_size);
		ssc_number_t * system_pre_interconnect_kwac = GetArray("system_pre_interconnect_kwac", gen_size);

		EXPECT_EQ(gen_size, 8760 * 2 * 2);
		EXPECT_NEAR(capacity_factor, 30.78777, 0.001);
		EXPECT_NEAR(annual_energy_interconnect, 485461500, 10);
		EXPECT_NEAR(annual_energy_pre_interconnect, 997640000, 10);
	}
}

TEST_F(CMGrid, SingleYearNoFinancial_cmod_grid) {

	grid_default_60_min_no_financial(data);
	int errors = run_module(data, "grid");
	EXPECT_FALSE(errors);
	if (!errors)
	{
		ssc_number_t capacity_factor, annual_energy_pre_interconnect, annual_energy_interconnect;
		int gen_size;
		ssc_data_get_number(data, "capacity_factor_interconnect_ac", &capacity_factor);
		ssc_data_get_number(data, "annual_energy_pre_interconnect_ac", &annual_energy_pre_interconnect);
		ssc_data_get_number(data, "annual_energy", &annual_energy_interconnect);
		ssc_number_t * system_kwac = GetArray("gen", gen_size);
		ssc_number_t * system_pre_interconnect_kwac = GetArray("system_pre_interconnect_kwac", gen_size);

		EXPECT_EQ(gen_size, 8760);
		EXPECT_NEAR(capacity_factor, 29.0167, 0.001);
		EXPECT_NEAR(annual_energy_interconnect, 457534759, 10);
		EXPECT_NEAR(annual_energy_pre_interconnect, 460351000, 10);
	}
}