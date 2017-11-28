#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <iostream>
#include <vector>

#include <lib_windwatts.h>
#include <lib_windwakemodel.h>
#include "lib_windwakemodel_test.h"


/**
 * windPowerCalculatorTest requires an initialized windTurbine and wakeModel, and XCoords & YCoords.
 * SetUp() allocates the vectors for input and output variables for windPowerUsingResource. 
 */

class windPowerCalculatorTest : public ::testing::Test{
protected:
	windPowerCalculator wpc;
	windTurbine wt;
	int nTurbines;
	// weather data
	double windSpeedData, windDirData, pressureData, tempData;
	
	 //farm data
	double farmPower;
	std::vector<double> power, thrust, eff, windSpeed;
	std::vector<double> turbulenceCoeff, distX, distY, distDownwind, distCrosswind;

public:
	double e = 0.01;
	void SetUp(){
		// allocate arrays
		nTurbines = 3;
		distDownwind.resize(nTurbines);
		distCrosswind.resize(nTurbines);
		thrust.resize(nTurbines, 0.47669);
		power.resize(nTurbines, 1190);
		eff.resize(nTurbines, 0);
		windSpeed.resize(nTurbines);
		turbulenceCoeff.resize(nTurbines);
		distX = { 0, 5, 10 };
		distY = { 0, 5, 10 };
		distDownwind.resize(nTurbines);
		distCrosswind.resize(nTurbines);

		// initialize values & classes
		for (int i = 0; i < nTurbines; i++){
			windSpeed[i] = 10.;
		}
		createDefaultTurbine(&wt);

		wpc.nTurbines = nTurbines;
		wpc.turbulenceIntensity = 1.0 / 7.0;
		wpc.windTurbine = wt;
		wpc.XCoords = distX;
		wpc.YCoords = distY;
	}
};

TEST_F(windPowerCalculatorTest, windPowerUsingResource_lib_windwatts){
	// weather inputs
	windSpeedData = 10.;
	windDirData = 180;
	tempData = 25;
	pressureData = 1.0;

	std::shared_ptr<mockWakeModel> mock(new mockWakeModel());
	double airDensitySaved = 0.0;

	// set expectations before running method
	EXPECT_CALL(*mock, wakeCalculations(::testing::_ /*wildcard matcher*/, &distDownwind[0], &distCrosswind[0], &power[0], &eff[0], &thrust[0], &windSpeed[0], &turbulenceCoeff[0]))
		.WillOnce(::testing::DoAll(::testing::Assign(&power[0], 1190), ::testing::Assign(&power[1], 1190), ::testing::Assign(&power[1], 1190))); // actions taken when mock is called by wpc

	wpc.InitializeModel(mock); // sets up wpc's wake model as mock
	int run = wpc.windPowerUsingResource(windSpeedData, windDirData, pressureData, tempData, &farmPower, &power[0], &thrust[0],
		&eff[0], &windSpeed[0], &turbulenceCoeff[0], &distDownwind[0], &distCrosswind[0]); // runs method we want to test
	EXPECT_EQ(run, 3);
}

TEST_F(windPowerCalculatorTest, windPowerUsingWeibull_lib_windwatts){
	// weather inputs
	windSpeedData = 10.;
	windDirData = 180;
	tempData = 25;
	pressureData = 1.0;
	// weibull data
	double weibullK = 2.;
	double avgSpeed = 7.25;
	double refHeight = 50.;
	std::vector<double> energy(wpc.windTurbine.powerCurveArrayLength);

	int energyTotal = wpc.windPowerUsingWeibull(weibullK, avgSpeed, refHeight, &energy[0]); // runs method we want to test
	EXPECT_EQ(energyTotal, 5639180);
}