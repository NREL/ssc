#include <gtest/gtest.h>

#include <iostream>
#include <vector>

#include "core.h"
#include <lib_windfile.h>
#include "cmod_windpower.h"
#include "input_cases/weather_inputs.h"

/**
 * Tests windfile's interpolation of measurement height's pres, tmp, speed, & dir data points to required hub height.
 */

class windDataProviderCalculatorTest : public ::testing::Test {
protected:
	winddata_provider* windDataProvider;

public: 
	double e = 1;
	void SetUp() {}
	void TearDown() {
		if (windDataProvider) delete windDataProvider;
	}
};

TEST_F(windDataProviderCalculatorTest, FindClosestUsingData_lib_windfile_test) {
	// measurement heights: 80, 90, 100, 110; hubheight: 85
	var_data* windresourcedata = create_winddata_array(1);
	windDataProvider = new winddata(windresourcedata);

	// speed
	int closestIndex = -1;
	windDataProvider->find_closest(closestIndex, 3, 4, 85);

}