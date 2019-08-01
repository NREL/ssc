#include "cmod_mhk_wave_test.h"

TEST_F(CM_MHKWave, ComputeModuleTest) {
	int mhk_wave_errors = run_module(data, "mhk_wave");
	ASSERT_EQ(mhk_wave_errors, 0);
	
	ssc_number_t annual_energy, average_power, rated_capacity, capacity_factor;

	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 751098.3, 0.1);
	
	ssc_data_get_number(data, "average_power", &average_power);
	EXPECT_NEAR(average_power, 85.7, 0.5);
	
	ssc_data_get_number(data, "rated_capacity", &rated_capacity);
	EXPECT_NEAR(rated_capacity, 280, 0.1);
	
	ssc_data_get_number(data, "capacity_factor", &capacity_factor);
	EXPECT_NEAR(capacity_factor, 30.6, 0.1);
}