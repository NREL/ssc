#include "cmod_swh_test.h"
#include "gtest/gtest.h"

TEST_F(CM_SWH, ResidentialDefault_cmod_swh) {
	
	int swh_errors = run_module(data, "swh");
	ASSERT_EQ(swh_errors, 0);
	
	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
//	EXPECT_NEAR(annual_energy, 2406.6, 0.1);
	EXPECT_NEAR(annual_energy, 2362.2, 0.1);

}