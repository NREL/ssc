
#include <gtest/gtest.h>

#include "core.h"
#ifndef _CMOD_PVWATTSV5_TEST_H_
#define _CMOD_PVWATTSV5_TEST_H_

#include "vartab.h"
#include "common.h"
//#include "input_cases/pvwattsv5_cases.h"

/**
write over this!~~~
* CMPVsamv1PowerIntegration tests the cmod_pvsamv1 using the SAM code generator to generate data
* Eventually a method can be written to write this data to a vartable so that lower-level methods of pvsamv1 can be tested
* For now, this uses the SSCAPI interfaces to run the compute module and compare results
*/
class CMPvwattsv5Integration : public ::testing::Test {

public:

	double m_error_tolerance_hi = 1.0;
	double m_error_tolerance_lo = 0.1;


	void SetUp() {}
	void TearDown() {}
};

#endif // !_CMOD_PVWATTSV5_TEST_H_
