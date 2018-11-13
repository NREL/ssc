#include <gtest/gtest.h>

#include "../ssc/core.h"
#include "../ssc/vartab.h"
#include "../ssc/common.h"
#include "cmod_pvwattsv5_test.h"

///Default PVWattsV5, but with TMY2 instead of TMY3
TEST_F(CMPvwattsV5Integration, DefaultNoFinancialModel){
	compute();

	double tmp=0;
//	ssc_data_get_number(data, "annual_energy", &annual_energy);
//	EXPECT_NEAR(annual_energy, 6909.79, error_tolerance) << "Annual energy.";
	int count;
	ssc_number_t* monthly_energy = ssc_data_get_array(data, "monthly_energy", &count);

	for (size_t i = 0; i < 12; i++)
		tmp += (double)monthly_energy[i];
	EXPECT_NEAR(tmp, 6909.79, error_tolerance) << "Annual energy.";


	EXPECT_NEAR((double)monthly_energy[0], 435.384, error_tolerance) << "Monthly energy of January";
	EXPECT_NEAR((double)monthly_energy[1], 482.864, error_tolerance) << "Monthly energy of February";
	EXPECT_NEAR((double)monthly_energy[2], 593.982, error_tolerance) << "Monthly energy of March";
	EXPECT_NEAR((double)monthly_energy[3], 673.599, error_tolerance) << "Monthly energy of April";
	EXPECT_NEAR((double)monthly_energy[4], 715.839, error_tolerance) << "Monthly energy of May";
	EXPECT_NEAR((double)monthly_energy[5], 665.064, error_tolerance) << "Monthly energy of June";
	EXPECT_NEAR((double)monthly_energy[6], 665.71, error_tolerance) << "Monthly energy of July";
	EXPECT_NEAR((double)monthly_energy[7], 647.677, error_tolerance) << "Monthly energy of August";
	EXPECT_NEAR((double)monthly_energy[8], 594.505, error_tolerance) << "Monthly energy of September";
	EXPECT_NEAR((double)monthly_energy[9], 568.489, error_tolerance) << "Monthly energy of October";
	EXPECT_NEAR((double)monthly_energy[10], 453.529, error_tolerance) << "Monthly energy of November";
	EXPECT_NEAR((double)monthly_energy[11], 413.149, error_tolerance) << "Month energy of December";

	ssc_number_t capacity_factor;
	ssc_data_get_number(data, "capacity_factor", &capacity_factor);
	EXPECT_NEAR(capacity_factor, 19.7197, error_tolerance) << "Capacity factor";

}