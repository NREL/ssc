#include <gtest/gtest.h>

#include "core.h"
#include "vartab.h"
#include "common.h"
#include "cmod_pvwattsv5_test.h"

///Default PVWattsV5, but with TMY2 instead of TMY3
TEST_F(CMPvwattsV5Integration, DefaultNoFinancialModel){
	compute();

	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 6909.79, error_tolerance) << "Annual energy.";

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 435.384, error_tolerance) << "Monthly energy of January";

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 413.149, error_tolerance) << "Month energy of December";

	ssc_number_t capacity_factor;
	ssc_data_get_number(data, "capacity_factor", &capacity_factor);
	EXPECT_NEAR(monthly_energy, 19.7197, error_tolerance) << "Capacity factor";

}