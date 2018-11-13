#include <gtest/gtest.h>

#include "cmod_fuelcell_test.h"

/// Test PVSAMv1 with all defaults and no-financial model
TEST_F(CMFuelCell, NoFinancialModelFixed) {

	// Run with fixed output
	int errors = run_module(data, "fuelcell");
	EXPECT_FALSE(errors);
	if (!errors)
	{
		ssc_number_t startup_hours, fixed_pct, dynamic_response;
		ssc_data_get_number(data, "fuelcell_startup_time", &startup_hours);
		ssc_data_get_number(data, "fuelcell_fixed_pct", &fixed_pct);
		ssc_data_get_number(data, "fuelcell_dynamic_response", &dynamic_response);

		SetCalculatedArray("fuelcell_power");
		
		// Not started up
		for (size_t h = 0; h < startup_hours; h++) {
			EXPECT_EQ(calculated_array[h], 0);
		}
		// Ramping up, should be the dynamic response limit
		EXPECT_NEAR(calculated_array[(size_t)startup_hours], dynamic_response, 0.1);
		EXPECT_NEAR(calculated_array[(size_t)(startup_hours + 1)], 2 * dynamic_response, 0.1);

		for (size_t h = size_t(startup_hours + 2); h < 100; h++) {
			EXPECT_NEAR(calculated_array[h], fixed_pct, 0.1);
		}
	}
}