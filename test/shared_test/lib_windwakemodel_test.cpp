#include <gtest/gtest.h>

#include <vector>
#include <iostream>

#include <lib_physics.h>
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


/// All turbines are in a row at same downwind distance: no effect
TEST_F(simpleWakeModelTest, wakeCalcNoInterference_lib_windwakemodel){
	
	for (int i = 0; i < numberTurbines; i++){
		distDownwind[i] = 0;
		distCrosswind[i] = 5 * i;
	}
	swm.wakeCalculations(seaLevelAirDensity, &distDownwind[0], &distCrosswind[0], &power[0], &eff[0], &thrust[0], &windSpeed[0], &turbIntensity[0]);
	for (int i = 0; i < numberTurbines; i++){
		EXPECT_NEAR(thrust[i], .47669, e) << "Thrust calculated at index " << i;
		EXPECT_NEAR(power[i], 1190, e) << "Power calculated at index " << i;
		EXPECT_NEAR(eff[i], 100, e) << "Eff calculated at index " << i;
		EXPECT_NEAR(windSpeed[i], 10, e) << "No change expected in windspeed at index " << i;
		EXPECT_NEAR(turbIntensity[i], 0.1, e) << "Turb intensity calculated at index " << i;
	}
}

/// All turbines are in a line at same crosswind distance: lot of power reduction
TEST_F(simpleWakeModelTest, wakeCalcAllInterference_lib_windwakemodel){
	for (int i = 0; i < numberTurbines; i++){
		distDownwind[i] = 5 * i;
		distCrosswind[i] = 0;
	}
	swm.wakeCalculations(seaLevelAirDensity, &distDownwind[0], &distCrosswind[0], &power[0], &eff[0], &thrust[0], &windSpeed[0], &turbIntensity[0]);
	std::vector<double>newThrust = { 0.4767, 0.4256, 0.4154};
	std::vector<double>newPower = { 1190, 157.6, 145.98 };
	std::vector<double>newEff = {100, 13.244, 12.267};
	std::vector<double>newWindSpeed = { 10, 5.247, 5.148 };
	for (int i = 0; i < numberTurbines; i++){
		EXPECT_NEAR(thrust[i], newThrust[i], e) << "Thrust calculated at index " << i;
		EXPECT_NEAR(power[i], newPower[i], e) << "Power calculated at index " << i;
		EXPECT_NEAR(eff[i], newEff[i], e) << "Eff calculated at index " << i;
		EXPECT_NEAR(windSpeed[i], newWindSpeed[i], e) << "windSpeeds at turbine " << i << " should be reduced.";
		if (i >= 1) EXPECT_GT(turbIntensity[i], 0.1) << "Turb intensity at turbine " << i << " should be increased.";
	}
}

/// Turbines form a triangle with two downwind turbines: little change due to crosswind distance
TEST_F(simpleWakeModelTest, wakeCalcTriangleInterference_lib_windwakemodel){
	distDownwind = { 0, 5, 5 };
	distCrosswind = { 0, -5, 5 };

	swm.wakeCalculations(seaLevelAirDensity, &distDownwind[0], &distCrosswind[0], &power[0], &eff[0], &thrust[0], &windSpeed[0], &turbIntensity[0]);
	for (int i = 0; i < numberTurbines; i++){
		EXPECT_NEAR(thrust[i], 0.4767, e) << "Thrust calculated at index " << i;
		EXPECT_NEAR(power[i], 1190, e) << "Power calculated at index " << i;
		EXPECT_NEAR(eff[i],100, e) << "Eff calculated at index " << i;
		EXPECT_NEAR(windSpeed[i], 10, e) << "Minor wind reduction expected at turbine " << i;
		if (i >= 1) EXPECT_NEAR(turbIntensity[i], 0.10031, e) << "Turb intensity should be increased at turbine " << i;
	}
	EXPECT_EQ(turbIntensity[1], turbIntensity[2]);
}

