#include <gtest/gtest.h>

#include <vector>
#include <iostream>

#include <lib_windwakemodel.h>
#include "lib_windwakemodel_test.h"


/// turbinePower function test
TEST_F(windTurbineTest, turbinePowerTest_lib_windwakemodel){
	double output(0), thrustCoeff(0);
	wt.turbinePower(20., airDensity, &output, &thrustCoeff);
	EXPECT_NEAR(output, 0.0, e) << "Turbine not initialized.";
	EXPECT_NEAR(thrustCoeff, 0.0, e) << "Turbine not initialized.";

	createDefaultTurbine(&wt);
	wt.turbinePower(11.25, airDensity, &output, &thrustCoeff);
	EXPECT_NEAR(output, 1390, e) << "At 11.25m/s, the output should be 1390.";
	EXPECT_NEAR(thrustCoeff, 0.3725, e) << "At 11.25m/s, the thrust coeff should be 0.3725";
}


/// All turbines are in a row at same downwind distance
TEST_F(simpleWakeModelTest, wakeCalcNoInterference_lib_windwakemodel){
	
	for (int i = 0; i < numberTurbines; i++){
		distDownwind[i] = 0;
		distCrosswind[i] = 5 * i;
	}
	swm.wakeCalculations(0.0, &distDownwind[0], &distCrosswind[0], &power[0], &eff[0], &thrust[0], &windSpeed[0], &turbIntensity[0]);
	for (int i = 0; i < numberTurbines; i++){
		EXPECT_NEAR(thrust[i], .7, e) << "No change expected.";
		EXPECT_NEAR(power[i], 10, e) << "No change expected.";
		EXPECT_NEAR(eff[i], 1, e) << "No change expected.";
		EXPECT_NEAR(windSpeed[i], 10, e) << "No change expected.";
		EXPECT_NEAR(turbIntensity[i], 0.1, e) << "No change expected.";
	}
}

/// All turbines are in a line at same crosswind distance
TEST_F(simpleWakeModelTest, wakeCalcAllInterference_lib_windwakemodel){
	setTestTurbineCurve();
	for (int i = 0; i < numberTurbines; i++){
		distDownwind[i] = 5 * i;
		distCrosswind[i] = 0;
	}
	swm.wakeCalculations(0.0, &distDownwind[0], &distCrosswind[0], &power[0], &eff[0], &thrust[0], &windSpeed[0], &turbIntensity[0]);
	std::vector<double>newWindSpeed = {10, 3.04338, 2.7485};
	for (int i = 0; i < numberTurbines; i++){
		EXPECT_NEAR(thrust[i], .7, e) << "No change expected.";
		EXPECT_NEAR(power[i], 10, e) << "No change expected.";
		EXPECT_NEAR(eff[i], 1, e) << "No change expected.";
		EXPECT_NEAR(windSpeed[i], newWindSpeed[i], e) << "windSpeeds at turbine " << i << " should be reduced.";
		if (i >= 1) EXPECT_GT(turbIntensity[i], 0.1) << "Turb intensity at turbine " << i << " should be increased.";
	}
}

/// Turbines form a triangle with two downwind turbines
TEST_F(simpleWakeModelTest, wakeCalcTriangleInterference_lib_windwakemodel){
	setTestTurbineCurve();
	distDownwind = { 0, 5, 5 };
	distCrosswind = { 0, -5, 5 };

	swm.wakeCalculations(0.0, &distDownwind[0], &distCrosswind[0], &power[0], &eff[0], &thrust[0], &windSpeed[0], &turbIntensity[0]);
	std::vector<double>newWindSpeed = { 10, 9.9999, 9.9999 };
	for (int i = 0; i < numberTurbines; i++){
		EXPECT_NEAR(thrust[i], .7, e) << "No change expected.";
		EXPECT_NEAR(power[i], 10, e) << "No change expected.";
		EXPECT_NEAR(eff[i], 1, e) << "No change expected.";
		EXPECT_NEAR(windSpeed[i], newWindSpeed[i], e) << "Minor wind reduction expected at turbine " << i;
		if (i >= 1) EXPECT_NEAR(turbIntensity[i], 0.10031, e) << "Turb intensity should be increased at turbine " << i;
	}
	EXPECT_EQ(turbIntensity[1], turbIntensity[2]);
}

