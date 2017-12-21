
#ifndef _CMOD_PVWATTSV5_TEST_H_
#define _CMOD_PVWATTSV5_TEST_H_

#include <gtest/gtest.h>
#include "core.h"
#include "vartab.h"
#include "common.h"
#include "input_cases/pvwattsv5_cases.h"

/**
* CMPVWattsV5 tests cmod_pvwattsv5 using a solar resource file. SetUp() creates default case,
* which can be modified within each individual test before running compute() and tests.
*/
class CMPvwattsV5Integration : public ::testing::Test {
protected:
	ssc_data_t data;

	bool compute();
	void SetUp() {} //what goes in here????
	void TearDown(){} //why would these be public or protected???? one way in wind, other for pvsam
};

//this function will be available to run the pvwattsV5 compute module from within tests
bool CMPvwattsV5Integration::compute() {
	ssc_module_t module = ssc_module_create("pvwattsv5");
	if (NULL == module)
	{
		printf("error: could not create 'pvwattsv5' module.");
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

#endif // _CMOD_PVWATTSV5_TEST_H_
