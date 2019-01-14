#include <gtest/gtest.h>

#include "lib_battery_dispatch_test.h"

size_t year = 0;
size_t hour_of_year = 0;
size_t step_of_hour = 0;

TEST_F(BatteryDispatchTest, DispatchManual)
{
	
	batteryPower = dispatchManual->getBatteryPower();
	batteryPower->connectionMode = ChargeController::AC_CONNECTED;

	// Test max charge power constraint
	batteryPower->powerPV = 1000; batteryPower->voltageSystem = 600;
	dispatchManual->dispatch(year, hour_of_year, step_of_hour);
	EXPECT_NEAR(batteryPower->powerBattery, -powerChargeMax, 2.0);

	// Test max discharge power constraint
	batteryPower->powerPV = 0; batteryPower->voltageSystem = 600; batteryPower->powerLoad = 1000;
	dispatchManual->dispatch(year, hour_of_year, step_of_hour);
	EXPECT_NEAR(batteryPower->powerBattery, powerDischargeMax, 2.0);

}

TEST_F(BatteryDispatchTest, DispatchFOMInput)
{
	std::vector<double> P_batt;
	for (int i = 0; i < 8760 * 60; i++) {
		P_batt.push_back(-336.062);
	}

	batteryPower = dispatchAutoFOM->getBatteryPower();
	batteryPower->connectionMode = ChargeController::AC_CONNECTED;
	batteryPower->powerPV = 750;
	batteryPower->powerFuelCell = 300;

	dispatchAutoFOM->set_custom_dispatch(P_batt);
	dispatchAutoFOM->dispatch(0, 0, 0);

}

/// Test to see if losses model is initialized correctly
TEST_F(BatteryDispatchTest, LossesModel)
{

}