#include "cmod_mhk_tidal_test.h"

TEST_F(CM_MHKTidal, ComputeModuleTest) {
	int mhk_tidal_errors = run_module(data, "mhk_tidal");
	ASSERT_EQ( mhk_tidal_errors , 0 );

	ssc_number_t annual_energy, average_power, rated_capacity, capacity_factor;

	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 442888288, 1);

	ssc_data_get_number(data, "average_power", &average_power);
	EXPECT_NEAR(average_power, 265.3, 0.5);

	ssc_data_get_number(data, "rated_capacity", &rated_capacity);
	EXPECT_NEAR(rated_capacity, 1115, 0.1);

	ssc_data_get_number(data, "capacity_factor", &capacity_factor);
	EXPECT_NEAR(capacity_factor, 45.3, 0.1);
}