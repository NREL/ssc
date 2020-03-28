#include "lib_battery_dispatch_automatic_btm_test.h"

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTM) {
    // Setup pv and load signal for peak shaving algorithm
    for (size_t d = 0; d < 365; d++) {
        for (size_t h = 0; h < 24; h++) {
            if (h > 6 && h < 18) {
                pv_prediction.push_back(fabs(12 - h) * 100);
            }
            load_prediction.push_back(600);
        }
    }
    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // TEST 1: Verify no grid charging since disallowed
    dispatchAutoBTM->dispatch(0, 0, 0);
    EXPECT_EQ(batteryPower->powerGridToBattery, 0);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 0.02);

    // TEST 2: Now, allow grid charging, should charge
    batteryPower->canGridCharge = true;
    dispatchAutoBTM->dispatch(0, 1, 0);
    EXPECT_GT(batteryPower->powerGridToBattery, 0);
    EXPECT_LT(batteryPower->powerBatteryDC, 0);
}
