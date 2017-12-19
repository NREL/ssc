#ifndef __WINDPOWER_TEST__
#define __WINDPOWER_TEST__

#include <gtest/gtest.h>

#include "core.h"
#include "vartab.h"
#include "common.h"
#include "../input_cases/weather_inputs.h"
#include "../input_cases/windpower_cases.h"


/**
 * CMWindPower tests the cmod_windpower using wind resource files and data arrays. SetUp() creates default case,
 * which can be modified within each individual test before running compute() and tests.
 */
class CMWindPowerIntegration : public ::testing::Test{
protected:
	ssc_data_t data;
	double e = 1000;

	bool compute();
	void SetUp(){
		data = ssc_data_create();
		int errors = windpower_nofinancial_testfile(data);
		EXPECT_FALSE(errors);
	}
	void TearDown(){}
};

bool CMWindPowerIntegration::compute(){
	ssc_module_t module = ssc_module_create("windpower");
	if (NULL == module)
	{
		printf("error: could not create 'windpower' module.");
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

#endif