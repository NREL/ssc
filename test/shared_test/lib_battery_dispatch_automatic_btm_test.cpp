#include "lib_battery_dispatch_automatic_btm_test.h"

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTM) {
    double dtHour = 1;
    CreateBattery(dtHour);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax,
        currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax, powerDischargeMax, 0, dispatch_t::BTM_MODES::LOOK_AHEAD, 0, 1, 24, 1, true, true, false, false);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t h = 0; h < 24; h++) {
        pv_prediction.push_back(0); // Set detailed PV later
        if (h < 4) {
            load_prediction.push_back(600);
        }
        else {
            load_prediction.push_back(0);
        }
    }
    
    // Set detailed PV
    pv_prediction[0] = 500; pv_prediction[1] = 400; pv_prediction[2] = 300; pv_prediction[3] = 200;
    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    EXPECT_EQ(batteryPower->powerBatteryChargeMaxAC, 50);

    // TEST 1: Verify no grid charging since disallowed  (_P_battery_use target is ~ -50)
    dispatchAutoBTM->update_dispatch(0, 0, 0);

    dispatchAutoBTM->dispatch(0, 0, 0);     // original target for battery power is
    EXPECT_EQ(batteryPower->powerGridToBattery, 0);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 0.02);

    // TEST 2: Now, allow grid charging, should charge up to Max Charge Power (enforced by restrict_power)
    batteryPower->canGridCharge = true;
    dispatchAutoBTM->update_dispatch(0, 0, 0);
    dispatchAutoBTM->dispatch(0, 0, 0);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 50, 1);
    EXPECT_NEAR(batteryPower->powerBatteryDC, -48, 1);
}
