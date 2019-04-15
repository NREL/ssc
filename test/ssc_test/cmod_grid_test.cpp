#include <gtest/gtest.h>

#include "cmod_grid_test.h"

TEST_F(CMGrid, BasicIO) {

	// Run with fixed output
	int errors = run_module(data, "grid");
	EXPECT_FALSE(errors);
	if (!errors)
	{
		ssc_number_t capacity_factor, annual_energy;
		int gen_size;
		ssc_data_get_number(data, "capacity_factor_interconnect_ac", &capacity_factor);
		ssc_data_get_number(data, "annual_energy_pre_interconnect_ac", &annual_energy);
		ssc_number_t * system_kwac = GetArray("gen", gen_size);
		ssc_number_t * system_pre_interconnect_kwac = GetArray("system_pre_interconnect_kwac", gen_size);

		EXPECT_EQ(gen_size, 8760);
		EXPECT_NEAR(capacity_factor, 23.7158, 0.001);
		EXPECT_NEAR(annual_energy, 373950000, 1);

	}
}