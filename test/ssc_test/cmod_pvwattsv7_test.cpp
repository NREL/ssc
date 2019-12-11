#include <gtest/gtest.h>

#include "../ssc/core.h"
#include "../ssc/vartab.h"
#include "../ssc/common.h"
#include "cmod_pvwattsv7_test.h"



///Default PVWattsV7, but with TMY2 instead of TMY3
TEST_F(CMPvwattsV7Integration, DefaultNoFinancialModel_cmod_pvwattsv7){
	compute();

	double tmp=0;
//	ssc_data_get_number(data, "annual_energy", &annual_energy);
//	EXPECT_NEAR(annual_energy, 6909.79, error_tolerance) << "Annual energy.";
	int count;
	ssc_number_t* monthly_energy = ssc_data_get_array(data, "monthly_energy", &count);

	for (size_t i = 0; i < 12; i++)
		tmp += (double)monthly_energy[i];
	//v5 is 6909.79, decrease of 2.4%: decreases due to shading, module cover losses, and spectral losses 
	EXPECT_NEAR(tmp, 6750.4236, error_tolerance) << "Annual energy."; 


	EXPECT_NEAR((double)monthly_energy[0], 429.606, error_tolerance) << "Monthly energy of January";
	EXPECT_NEAR((double)monthly_energy[1], 474.622, error_tolerance) << "Monthly energy of February";
	EXPECT_NEAR((double)monthly_energy[2], 579.655, error_tolerance) << "Monthly energy of March";
	EXPECT_NEAR((double)monthly_energy[3], 656.420, error_tolerance) << "Monthly energy of April";
	EXPECT_NEAR((double)monthly_energy[4], 695.721, error_tolerance) << "Monthly energy of May";
	EXPECT_NEAR((double)monthly_energy[5], 645.201, error_tolerance) << "Monthly energy of June";
	EXPECT_NEAR((double)monthly_energy[6], 643.988, error_tolerance) << "Monthly energy of July";
	EXPECT_NEAR((double)monthly_energy[7], 629.608, error_tolerance) << "Monthly energy of August";
	EXPECT_NEAR((double)monthly_energy[8], 581.192, error_tolerance) << "Monthly energy of September";
	EXPECT_NEAR((double)monthly_energy[9], 559.397, error_tolerance) << "Monthly energy of October";
	EXPECT_NEAR((double)monthly_energy[10], 446.824, error_tolerance) << "Monthly energy of November";
	EXPECT_NEAR((double)monthly_energy[11], 408.190, error_tolerance) << "Month energy of December";

	ssc_number_t capacity_factor;
	ssc_data_get_number(data, "capacity_factor", &capacity_factor);
	EXPECT_NEAR(capacity_factor, 19.2649, error_tolerance) << "Capacity factor";

}

/// PVWattsV7 using different technology input options
TEST_F(CMPvwattsV7Integration, DifferentTechnologyInputs_cmod_pvwattsv7)
{
	//PVWattsV5 results: annual_energy_expected = { 6909.79, 7123.32, 7336.478, 6909.79, 6804.376, 8601.011, 8727.704, 9690.735};
	std::vector<double> annual_energy_expected = { 6750.42, 7034.39, 7166.88, 6750.42, 6693.49, 8514.26, 8441.60, 9631.76 };
	//standard fixed -2.4%, premium fixed -1.3%, thinfilm fixed -2.4%, standard fixed -2.4%, standard roof -1.7%, standard 1-axis -1.0%, standard backtrack -3.4%, standard 2-axis -0.6%

	std::map<std::string, double> pairs;
	size_t count = 0;
	error_tolerance = 0.01;

	// Module types: Standard, Premium, Thin Film
	for (int module_type = 0; module_type < 3; module_type++)
	{
			pairs["module_type"] = module_type;
			int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
			EXPECT_FALSE(pvwatts_errors);

			if (!pvwatts_errors)
			{
				ssc_number_t annual_energy;
				ssc_data_get_number(data, "annual_energy", &annual_energy);
				EXPECT_NEAR(annual_energy, annual_energy_expected[count], error_tolerance) << "Annual energy.";
			}
			count++;
	}
	pairs["module_type"] = 0; //reset module type to its default value

	// Array types: Fixed open rack, fixed roof mount, 1-axis tracking, 1-axis backtracking, 2-axis tracking
	for (int array_type = 0; array_type < 5; array_type++)
	{		
		pairs["array_type"] = array_type;
		int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
		EXPECT_FALSE(pvwatts_errors);

		if (!pvwatts_errors)
		{
			ssc_number_t annual_energy;
			ssc_data_get_number(data, "annual_energy", &annual_energy);
			EXPECT_NEAR(annual_energy, annual_energy_expected[count], error_tolerance) << "Annual energy.";
		}
		count++;
	}
	pairs["array_type"] = 0; //reset array type to fixed open rack

}

/// PVWattsV7 using a larger system size
TEST_F(CMPvwattsV7Integration, LargeSystem_cmod_pvwattsv7)
{
	//PVWattsV5 results: std::vector<double> annual_energy_expected = { 1727447.4, 1701094.0, 2150252.8, 2181925.8, 2422683.7 };
	std::vector<double> annual_energy_expected = { 1686353.2, 1673371.8, 2123603.8, 2105794.1, 2407940.7 };

	std::map<std::string, double> pairs;
	size_t count = 0;
	error_tolerance = 0.1; //use a larger error tolerance for large numbers

	// Larger size
	pairs["system_capacity"] = 1000; //1 MW system

	// Array types: Fixed open rack, fixed roof mount, 1-axis tracking, 1-axis backtracking, 2-axis tracking
	for (int array_type = 0; array_type < 5; array_type++)
	{
		pairs["array_type"] = array_type;
		int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
		EXPECT_FALSE(pvwatts_errors);

		if (!pvwatts_errors)
		{
			ssc_number_t annual_energy;
			ssc_data_get_number(data, "annual_energy", &annual_energy);
			EXPECT_NEAR(annual_energy, annual_energy_expected[count], error_tolerance) << "Annual energy.";
		}
		count++;
	}
}

/// Test pvwattsv7 with default inputs and a 15-minute weather file 
TEST_F(CMPvwattsV7Integration, SubhourlyWeather_cmod_pvwattsv7) {

	char subhourly[256];
	int b = sprintf(subhourly, "%s/test/input_cases/pvsamv1_data/LosAngeles_WeatherFile_15min.csv", SSCDIR);
	ssc_data_set_string(data, "solar_resource_file", subhourly); //file set above

	//std::map<std::string, std::string> pairs;
	int pvwatts_errors = run_module(data, "pvwattsv7");

	EXPECT_FALSE(pvwatts_errors);

	if (!pvwatts_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 5943.613, error_tolerance) << "Annual energy.";

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 16.9, 0.1) << "Capacity factor";

	}
}

/// Test PVWattsV7 in lifetime mode
TEST_F(CMPvwattsV7Integration, LifetimeModeTest_cmod_pvwattsv7) {

	// set lifetime mode
	std::map<std::string, double> pairs;
	pairs["system_use_lifetime_output"] = 1;
	pairs["analysis_period"] = 25;

	// test degradation array with a length of 1, which should work
	// annual energy of this test should be higher than the array length 25, because this is year 1 energy
	// and with a single value array, degradation doesn't start applying until year 2
	double dc_degradation_single[1];
	dc_degradation_single[0] = 0.5;
	ssc_data_set_array(data, "dc_degradation", (ssc_number_t*)dc_degradation_single, 1);
	int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
	EXPECT_FALSE(pvwatts_errors);
	if (!pvwatts_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 6750.424, error_tolerance) << "Annual energy degradation array length 1.";
	}

	// next, test degradation array with length the same as analysis period, which should also work
	double dc_degradation[25];
	for (size_t i = 0; i < 25; i++) {
		dc_degradation[i] = 0.5;
	}
	ssc_data_set_array(data, "dc_degradation", (ssc_number_t*)dc_degradation, 25);
	pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
	EXPECT_FALSE(pvwatts_errors);
	if (!pvwatts_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 6716.589, error_tolerance) << "Annual energy degradation array length 25.";
	}

	// lastly, test degradation array with the wrong length, which should fail
	double dc_degradation_fail[2];
	for (size_t i = 0; i < 22; i++) {
		dc_degradation_fail[i] = 0.5;
	}
	ssc_data_set_array(data, "dc_degradation", (ssc_number_t*)dc_degradation_fail, 2);
	pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv7", pairs);
	EXPECT_TRUE(pvwatts_errors);
}
