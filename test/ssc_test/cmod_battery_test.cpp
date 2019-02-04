#include <numeric>

#include <gtest/gtest.h>

#include "cmod_battery_test.h"


/// Test standalone battery compute modeule with a input lifetime generation and commercial load
TEST_F(CMBattery, CommercialLifetimePeakShaving) {

	// Run with fixed output
	ssc_number_t n_years;
	ssc_data_get_number(data, "analysis_period", &n_years);
	size_t n_lifetime = (size_t)(n_years) * 8760;

	int errors = run_module(data, "battery");
	EXPECT_FALSE(errors);

	if (!errors)
	{
		// roundtrip efficiency test will ensure that the battery cycled
		ssc_number_t roundtripEfficiency;
		ssc_data_get_number(data, "average_battery_roundtrip_efficiency", &roundtripEfficiency);
		EXPECT_NEAR(roundtripEfficiency, 94.3, 1);

		// test that lifetime output is achieved
		int n;
		calculated_array = ssc_data_get_array(data, "gen", &n);
		EXPECT_EQ(n_lifetime, (size_t)n);

		// test that battery was replaced at some point
		calculated_array = ssc_data_get_array(data, "batt_bank_replacement", &n);
		int replacements = std::accumulate(calculated_array, calculated_array + n, 0);
		
		EXPECT_GT(replacements, 0);
	}
}