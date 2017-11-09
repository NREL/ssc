#include <gtest/gtest.h>

#include <vector>
#include <iostream>

#include <lib_windwakemodel.h>

/**
 * Simple wake Model test
 */

class simpleWakeModelTest : public ::testing::Test{
protected:
	simpleWakeModel swm;
	int numberTurbines;
	std::vector<double> distDownwind;
	std::vector<double> distCrosswind;
	std::vector<double> thrust;
	std::vector<double> power;
	std::vector<double> eff;
	std::vector<double> windSpeed;
	std::vector<double> turbIntensity;
public:
	double e = 0.0001;
	void SetUp(){
		numberTurbines = 3;
		distDownwind.resize(numberTurbines);
		distCrosswind.resize(numberTurbines);
		thrust.resize(numberTurbines);
		power.resize(numberTurbines);
		eff.resize(numberTurbines);
		windSpeed.resize(numberTurbines);
		turbIntensity.resize(numberTurbines);
		swm = simpleWakeModel(numberTurbines);
	}
	void setTestTurbineCurve(){
		for (int i = 0; i < numberTurbines; i++){
			thrust[i] = .7;
			power[i] = 10.;
			eff[i] = 1.;
			windSpeed[i] = 10.;
			turbIntensity[i] = 0.1;
		}
	}
};

/// All turbines are in a row at same downwind distance
TEST_F(simpleWakeModelTest, wakeCalcNoInterference_lib_windwakemodel){
	setTestTurbineCurve();
	for (int i = 0; i < numberTurbines; i++){
		distDownwind[i] = 0;
		distCrosswind[i] = 5 * i;
	}
	swm.wakeCalculations(0.0, &distDownwind[0], &distCrosswind[0], nullptr, &thrust[0], &windSpeed[0], &turbIntensity[0]);
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
	swm.wakeCalculations(0.0, &distDownwind[0], &distCrosswind[0], nullptr, &thrust[0], &windSpeed[0], &turbIntensity[0]);
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

	swm.wakeCalculations(0.0, &distDownwind[0], &distCrosswind[0], nullptr, &thrust[0], &windSpeed[0], &turbIntensity[0]);
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

/**
* Park/WaSp wake Model test
*/

class parkWakeModelTest : public ::testing::Test{
protected:
	simpleWakeModel swm;
	int numberTurbines;
	std::vector<double> distDownwind;
	std::vector<double> distCrosswind;
	std::vector<double> thrust;
	std::vector<double> power;
	std::vector<double> eff;
	std::vector<double> windSpeed;
	std::vector<double> turbIntensity;
public:
	double e = 0.0001;
	void SetUp(){
		numberTurbines = 3;
		distDownwind.resize(numberTurbines);
		distCrosswind.resize(numberTurbines);
		thrust.resize(numberTurbines);
		power.resize(numberTurbines);
		eff.resize(numberTurbines);
		windSpeed.resize(numberTurbines);
		turbIntensity.resize(numberTurbines);
		swm = simpleWakeModel(numberTurbines);
	}
	void setTestTurbineCurve(){
		for (int i = 0; i < numberTurbines; i++){
			thrust[i] = .7;
			power[i] = 10.;
			eff[i] = 1.;
			windSpeed[i] = 10.;
			turbIntensity[i] = 0.1;
		}
	}
};