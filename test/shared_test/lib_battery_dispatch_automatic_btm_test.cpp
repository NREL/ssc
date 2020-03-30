#include "lib_battery_dispatch_automatic_btm_test.h"

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTM) {
    double dtHour = 1;
    CreateBattery(dtHour);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t d = 0; d < 365; d++) {
        for (size_t h = 0; h < 24; h++) {
            if (h > 6 && h < 18) {
                pv_prediction.push_back(fabs(12 - h) * 100);
            }
            load_prediction.push_back(600);
        }
    }

    pv_prediction[0] = 500; pv_prediction[1] = 400; pv_prediction[2] = 300; pv_prediction[3] = 200;
    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    EXPECT_EQ(batteryPower->powerBatteryChargeMaxAC, 50);

    // TEST 1: Verify no grid charging since disallowed  (_P_battery_use target is ~ -50)
    dispatchAutoBTM->update_dispatch(0, 0, 0);
    if( fabs(dispatchAutoBTM->power_batt_target() + 50) < 1){
        dispatchAutoBTM->dispatch(0, 0, 0);     // original target for battery power is
        EXPECT_EQ(batteryPower->powerGridToBattery, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 0.02);

        // TEST 2: Now, allow grid charging, should charge up to Max Charge Power
        batteryPower->canGridCharge = true;
        dispatchAutoBTM->update_dispatch(0, 0, 0);
        dispatchAutoBTM->dispatch(0, 0, 0);
        EXPECT_NEAR(batteryPower->powerGridToBattery, 50, 1);
        EXPECT_NEAR(batteryPower->powerBatteryDC, -48, 1);
    }
    if( fabs(dispatchAutoBTM->power_batt_target() + 189) < 1){
        dispatchAutoBTM->dispatch(0, 0, 0);     // original target for battery power is
        EXPECT_EQ(batteryPower->powerGridToBattery, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 0.02);

        // TEST 2: Now, allow grid charging, should charge up close to Max Charge Power (enforced by restrict_power)
        batteryPower->canGridCharge = true;
        dispatchAutoBTM->update_dispatch(0, 0, 0);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, -189, 1);

        dispatchAutoBTM->dispatch(0, 0, 0);
        EXPECT_NEAR(batteryPower->powerGridToBattery, 46.64, 1);
        EXPECT_NEAR(batteryPower->powerBatteryDC, -44.6, 1);
    }
}
