#include <gtest/gtest.h>

#include "lib_battery_dispatch_test.h"

size_t year = 0;
size_t hour_of_year = 0;
size_t step_of_hour = 0;

TEST_F(BatteryDispatchTest, ManualDispatch)
{
	dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
		dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, canGridcharge, canGridcharge, percentDischarge, percentGridcharge);
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
