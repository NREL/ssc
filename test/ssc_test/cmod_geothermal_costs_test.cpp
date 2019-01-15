#include <gtest/gtest.h>

#include "cmod_geothermal_costs_test.h"

/// Test Geothermal with all defaults and single owner model
TEST_F(CMGeothermal, SingleOwnerDefault) {

	// Run with fixed output
	int geothermal_errors = run_module(data, "geothermal");
	EXPECT_EQ(geothermal_errors, 0);

	int singleowner_errors = run_module(data, "singleowner");
	EXPECT_EQ(singleowner_errors, 0);

	if (!geothermal_errors)	//if geothermal_errors != 0, there were no errors in the compute module. 
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);		
		EXPECT_NEAR(annual_energy, 262800000, 0.1);
	}
}