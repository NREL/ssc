
#ifndef _CMOD_PVWATTSV7_TEST_H_
#define _CMOD_PVWATTSV7_TEST_H_

#include <gtest/gtest.h>
#include "../ssc/core.h"
#include "../ssc/vartab.h"
#include "../ssc/common.h"
#include "../input_cases/pvwattsv7_cases.h"

/**
* CMPVWattsV7 tests cmod_pvwattsv7 using a solar resource file. SetUp() creates default case,
* which can be modified within each individual test before running compute() and tests.
*/
class CMPvwattsV7Integration_cmod_pvwattsv7 : public ::testing::Test {
protected: //doesn't really matter if this is protected or public, but you need to declare one or the other or it will default to private which doesn't work
	ssc_data_t data;

	double error_tolerance = 1.0e-3;

	bool compute();
	void SetUp() { //if you always want to set up with the same default case, this can go in the class. otherwise it probably makes sense in the test itself.
		data = ssc_data_create();
		int errors = pvwattsv7_nofinancial_testfile(data);
		EXPECT_FALSE(errors); //make sure that the test ran ok
	}
	void TearDown()
	{
		ssc_data_free(data);
		data = nullptr;
	} 
	void ValidateMonthlyEnergy(std::vector<double> expectedMonthlyEnergy)
	{
		ASSERT_EQ(12, expectedMonthlyEnergy.size());

		std::vector<std::string> messages = {
			"Monthly energy of January",
			"Monthly energy of February",
			"Monthly energy of March",
			"Monthly energy of April",
			"Monthly energy of May",
			"Monthly energy of June",
			"Monthly energy of July",
			"Monthly energy of August",
			"Monthly energy of September",
			"Monthly energy of October",
			"Monthly energy of November",
			"Month energy of December"
		};
		int count;
		ssc_number_t* monthly_energy = ssc_data_get_array(data, "monthly_energy", &count);
		EXPECT_EQ(12, count);

		for (int month = 0; month < count; month++)
		{
			EXPECT_NEAR((double)monthly_energy[month], expectedMonthlyEnergy[month], error_tolerance) << messages[month];
		}

	}
};

//this function will be available to run the pvwattsV7 compute module from within tests
bool CMPvwattsV7Integration_cmod_pvwattsv7::compute() {
	ssc_module_t module = ssc_module_create("pvwattsv7");
	if (NULL == module)
	{
		printf("error: could not create 'pvwattsv7' module.");
		ssc_data_free(data);
		return false;
	}
	if (ssc_module_exec(module, data) == 0)
	{
		printf("error during simulation.");
		ssc_module_free(module);
		ssc_data_free(data);
		return false;
	}
	ssc_module_free(module);
	return 0;
}

#endif // _CMOD_PVWATTSV7_TEST_H_
