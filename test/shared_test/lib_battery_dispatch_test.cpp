#include <gtest/gtest.h>

#include "lib_battery_dispatch_test.h"

TEST_F(BatteryDispatchTest, ManualDispatch)
{
	// discharge in period 3
	canDischarge[2] = 1;
	dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
		dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, canGridcharge, percentDischarge, percentGridcharge);


}
