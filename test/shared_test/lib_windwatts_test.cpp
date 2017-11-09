#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <iostream>
#include <vector>

#include <lib_windwatts.h>
#include <lib_windwakemodel.h>

/**
* mockWakeModel is assigned in WPC's InitializeModel and called in windpower function.
*/

class mockWakeModel : public wake_model
{
public:
	mockWakeModel(){}
	MOCK_METHOD1(test, int(int n));
	MOCK_METHOD7(wakeCalculations, void(const double airDensity, const double distanceDownwind[], const double distanceCrosswind[], 
		double power[], double thrust[], double windSpeed[], double turbulenceIntensity[]));
};


/**
 * windPowerCalculatorTest checks individual turbine power output and farm power output.
 * SetUp() creates a wpc with default values for single-value variables (shear exponent), and
 * a test power curve with 41 entries: after the cut-in speed of 6 m/s, the curve is linear, then 0
 * at 25 m/s.
 */

class windPowerCalculatorTest : public ::testing::Test{
protected:
	wind_power_calculator wpc;
	int nTurbines;
	// weather data
	double windSpeedData, windDirData, pressureData, tempData;
	
	// turbine data
	int powerCurveSteps = 41;
	std::vector<double> powerCurveWindSpeed, powerCurveKWOutput, densityCorrectedSpeeds;

	 //farm data
	double farmPower;
	std::vector<double> turbinePower, thrustCoeff, eff, windSpeed;
	std::vector<double> turbulenceCoeff, distX, distY, distDownwind, distCrosswind;


public:
	double e = 0.001;
	void SetUp(){
		// initialize values
		nTurbines = 3;
		turbinePower.resize(nTurbines);
		thrustCoeff.resize(nTurbines);
		eff.resize(nTurbines);
		windSpeed.resize(nTurbines);
		turbulenceCoeff.resize(nTurbines);
		distX.resize(nTurbines);
		distY.resize(nTurbines);
		distDownwind.resize(nTurbines);
		distCrosswind.resize(nTurbines);

		// create test turbine curve
		densityCorrectedSpeeds.resize(powerCurveSteps);
		for (int i = 0; i < powerCurveSteps; i++){
			powerCurveWindSpeed.push_back(i);
			if (i < 6) powerCurveKWOutput.push_back(0);
			else if (i > 25) powerCurveKWOutput.push_back(0);
			else powerCurveKWOutput.push_back(i - 5);
		}

		// assign values (make as function later)
		wpc.m_iNumberOfTurbinesInFarm = nTurbines;
		wpc.m_dShearExponent = 0.14;
		wpc.m_dRotorDiameter = 77;
		wpc.m_adDensityCorrectedWS = densityCorrectedSpeeds;
		wpc.m_adPowerCurveWS = powerCurveWindSpeed;
		wpc.m_adPowerCurveKW = powerCurveKWOutput;
		wpc.m_dHubHeight = 80;
		wpc.m_dMeasurementHeight = 80;
		wpc.m_iLengthOfTurbinePowerCurveArray = powerCurveSteps;
		wpc.m_dLossesPercent = 0;
		wpc.m_dLossesAbsolute = 0;
		wpc.m_adXCoords = distX;
		wpc.m_adYCoords = distY;
	}
};

TEST_F(windPowerCalculatorTest, wind_powerTest_lib_windwatts){
	// weather inputs
	windSpeedData = 25.;
	windDirData = 180;
	pressureData = 1;
	tempData = 25;

	std::shared_ptr<mockWakeModel> mock(new mockWakeModel());
	double airDensitySaved = 0.0;

	// set expectations before running method
	EXPECT_CALL(*mock, test(0));
	//EXPECT_CALL(*mock, wakeCalculations(::testing::_ /*wildcard matcher*/, &distDownwind[0], &distCrosswind[0], ::testing::_, &thrustCoeff[0], &windSpeed[0], &turbulenceCoeff[0]))
		//.WillOnce(::testing::DoAll(::testing::SaveArg<0>(&airDensitySaved), ::testing::Assign(&turbulenceCoeff[0], .5))); // actions taken when mock is called by wpc
	
	wpc.InitializeModel(mock); // sets up wpc's wake model as mock
	wpc.wind_power(windSpeedData, windDirData, pressureData, tempData, &farmPower, &turbinePower[0], &thrustCoeff[0],
		&eff[0], &windSpeed[0], &turbulenceCoeff[0], &distDownwind[0], &distCrosswind[0]); // runs method we want to test

	//EXPECT_NEAR(airDensitySaved, 1.18389, e); // check that saved argument is as expected (can also be done inside expect_call)
	//EXPECT_NEAR(turbulenceCoeff[0], .5, e); // simulate turbulence coeff being modified by wake model
}