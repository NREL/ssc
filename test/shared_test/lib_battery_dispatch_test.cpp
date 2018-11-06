#include <gtest/gtest.h>

#include "lib_battery_dispatch_test.h"

size_t year = 0;
size_t hour_of_year = 0;
size_t step_of_hour = 0;

TEST_F(BatteryDispatchTest, ManualDispatch)
{
	dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
		dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, canGridcharge, percentDischarge, percentGridcharge);
	batteryPower = dispatchManual->getBatteryPower();
	batteryPower->connectionMode = ChargeController::AC_CONNECTED;

	// Test max charge power constraint
	P_pv = 1000; V_pv = 600; 
	dispatchManual->dispatch(year, hour_of_year, step_of_hour, P_pv, V_pv, P_load, P_clipped);
	EXPECT_NEAR(batteryPower->powerBattery, -powerChargeMax, 2.0);

	// Test max discharge power constraint
	P_pv = 0; V_pv = 600; P_load = 1000;
	dispatchManual->dispatch(year, hour_of_year, step_of_hour, P_pv, V_pv, P_load, P_clipped);
	EXPECT_NEAR(batteryPower->powerBattery, powerDischargeMax, 2.0);

}
