#include "lib_battery_dispatch_automatic_btm_test.h"
#include "code_generator_utilities.h"

#include "shared_rate_data.h"

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMGridCharging) {
    double dtHour = 1;
    CreateBattery(dtHour);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
                                                                max_current,
                                                                max_current, max_power, max_power, max_power, max_power,
                                                                0, dispatch_t::BTM_MODES::LOOK_AHEAD, 0, 1, 24, 1, true,
                                                                true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

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
    pv_prediction[0] = 500;
    pv_prediction[1] = 400;
    pv_prediction[2] = 300;
    pv_prediction[3] = 200;
    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    EXPECT_EQ(batteryPower->powerBatteryChargeMaxAC, 50);

    // TEST 1: Verify no grid charging since disallowed  (_P_battery_use target is ~ -50)
    dispatchAutoBTM->update_dispatch(0, 0, 0, 0);

    dispatchAutoBTM->dispatch(0, 0, 0);     // original target for battery power is
    EXPECT_EQ(batteryPower->powerGridToBattery, 0);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 0.02);

    // TEST 2: Now, allow grid charging, should charge up to Max Charge Power (enforced by restrict_power)
    batteryPower->canGridCharge = true;
    dispatchAutoBTM->update_dispatch(0, 0, 0, 0);
    dispatchAutoBTM->dispatch(0, 0, 0);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 50, 1);
    EXPECT_NEAR(batteryPower->powerBatteryDC, -48, 1);
}

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMPVCharging) {
    double dtHour = 1;
    CreateBattery(dtHour);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
                                                                max_current,
                                                                max_current, max_power, max_power, max_power, max_power,
                                                                0, dispatch_t::BTM_MODES::LOOK_AHEAD, 0, 1, 24, 1, true,
                                                                true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t h = 0; h < 24; h++) {
        if (h > 6 && h < 18) {
            pv_prediction.push_back(700);
        }
        else {
            pv_prediction.push_back(0);
        }
        load_prediction.push_back(500);
    }

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Load never peaks above average load, so battery never discharges
    std::vector<double> expectedPower = {0, 0, 0, 0, 0, 0, 0, -50, -50, -50, -50, -50, -1.63, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                         0, 0, 0, 0, 0};
    for (size_t h = 0; h < 24; h++) {
        if (h > 6 && h < 18) {
            batteryPower->powerPV = 700; // Match the predicted PV
        }
        else {
            batteryPower->powerPV = 0;
        }
        batteryPower->powerLoad = 500; // Match the predicted load
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.2) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMPVChargeAndDischarge) {
    double dtHour = 1;
    CreateBattery(dtHour);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
                                                                max_current,
                                                                max_current, max_power, max_power, max_power, max_power,
                                                                0, dispatch_t::BTM_MODES::LOOK_AHEAD, 0, 1, 24, 1, true,
                                                                true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t h = 0; h < 24; h++) {
        if (h > 6 && h < 18) {
            pv_prediction.push_back(700);
        }
        else {
            pv_prediction.push_back(0);
        }

        if (h > 18) {
            load_prediction.push_back(600);
        }
        else {
            load_prediction.push_back(500);
        }
    }

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Battery will charge when PV is available, then discharge when load increases at 7 pm
    std::vector<double> expectedPower = {0, 0, 0, 0, 0, 0, 0, -50, -50, -50, -50, -50, -1.63, 0, 0, 0, 0, 0, 0, 50, 50,
                                         50, 50, 50, 50, 50, 50};
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerLoad = 500;
        batteryPower->powerPV = 0;
        if (h > 6 && h < 18) {
            batteryPower->powerPV = 700; // Match the predicted PV
        }
        else if (h > 18) {
            batteryPower->powerLoad = 600; // Match the predicted load
        }
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.5) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMPVChargeAndDischargeSubhourly) {
    double dtHour = 0.25;
    CreateBattery(dtHour);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
                                                                max_current,
                                                                max_current, max_power, max_power, max_power, max_power,
                                                                0, dispatch_t::BTM_MODES::LOOK_AHEAD, 0, 1, 24, 1, true,
                                                                true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t h = 0; h < 24; h++) {
        for (size_t step = 0; step < 4; step++) {
            if (h > 6 && h < 18) {
                pv_prediction.push_back(700);
            }
            else {
                pv_prediction.push_back(0);
            }

            if (h > 18) {
                load_prediction.push_back(600);
            }
            else {
                load_prediction.push_back(500);
            }
        }
    }

    std::vector<double> expectedPower = {0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                                         0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                                         0.00, 0.00, -50.00, -50.00, -50.00, -50.00, -50.00, -50.00, -50.00, -50.00,
                                         -50.00, -50.00, -50.00, -50.00, -50.00, -50.00, -50.00, -50.00, -50.00, -50.00,
                                         -50.00, -50.00, -5.19, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                                         0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                                         0.00, 0.00, 0.00, 0.00, 0.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00,
                                         50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00,
                                         50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00,
                                         50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00,
                                         50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 52.26, 54.52, 56.77,
                                         59.03, 61.27, 63.52, 65.76, 68.01, 70.25, 72.48, 74.72, 76.95, 79.18, 81.41,
                                         83.64, 85.87, 88.10, 90.32, 92.55, 94.77, 95.00};

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Battery will charge when PV is available, then discharge when load increases at 7 pm
    int index = 0;
    for (size_t h = 0; h < 24; h++) {
        for (size_t step = 0; step < 4; step++) {
            batteryPower->powerLoad = 500;
            batteryPower->powerPV = 0;
            if (h > 6 && h < 18) {
                batteryPower->powerPV = 700; // Match the predicted PV
            }
            else if (h > 18) {
                batteryPower->powerLoad = 600; // Match the predicted load
            }
            dispatchAutoBTM->dispatch(0, h, step);
            EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[index], 0.2) << " error in expected at step " << index;
            index++;
        }
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMDCClipCharge) {
    double dtHour = 1;
    CreateBattery(dtHour);

    // Only charge from clipped power
    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
                                                                max_current,
                                                                max_current, max_power, max_power, max_power, max_power,
                                                                0, dispatch_t::BTM_MODES::LOOK_AHEAD, 0, 1, 24, 1,
                                                                false, true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t h = 0; h < 24; h++) {
        if (h > 6 && h < 18) {
            pv_prediction.push_back(700);
        }
        else {
            pv_prediction.push_back(0);
        }

        if (h > 18) {
            load_prediction.push_back(600);
        }
        else {
            load_prediction.push_back(500);
        }
    }

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->setSharedInverter(m_sharedInverter);

    // Battery will charge when PV is available, then discharge when load increases at 7 pm
    std::vector<double> expectedPower = {0, 0, 0, 0, 0, 0, 0, -47.93, -47.92, -47.92, -47.91, -48.0, -11.99,
                                         0, 0, 0, 0, 0, 0, 50, 50, 50, 50, 50.25};
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerLoad = 500;
        batteryPower->powerPV = 0;
        if (h > 6 && h < 18) {
            batteryPower->powerPV = 700; // Match the predicted PV
        }
        else if (h > 18) {
            batteryPower->powerLoad = 600; // Match the predicted load
        }
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.2) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, TestBasicForecast) {
    double dtHour = 1;
    CreateBattery(dtHour);
    util_rate = new rate_data();
    set_up_default_commercial_rate_data(*util_rate);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
        max_current,
        max_current, max_power, max_power, max_power, max_power,
        0, dispatch_t::BTM_MODES::FORECAST, 0, 1, 24, 1, true,
        true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t h = 0; h < 48; h++) {
        if (h % 24 > 6 && h % 24 < 18) {
            pv_prediction.push_back(700);
        }
        else {
            pv_prediction.push_back(0);
        }

        if (h % 24 > 18) {
            load_prediction.push_back(600);
        }
        else {
            load_prediction.push_back(500);
        }
    }

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);
    dispatchAutoBTM->setup_rate_forecast();

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Discharge first 4 hours to avoid peak demand charges. Charge while there's solar, then discharge again at 6 pm since this is a high TOU rate
    std::vector<double> expectedPower = { 50, 50, 50, 34.79, 0, 0, 0, -47.84, -47.84, -47.84, -47.84, -47.84, -47.84, -47.84, -47.84, -47.91, -10.0, 0, 50, 50, 50,
                                         50, 50, 50, 50, 50, 50 };
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerLoad = 500;
        batteryPower->powerPV = 0;
        if (h > 6 && h < 18) {
            batteryPower->powerPV = 700; // Match the predicted PV
        }
        else if (h > 18) {
            batteryPower->powerLoad = 600; // Match the predicted load
        }
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.5) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, TestSummerPeak) {
    double dtHour = 1;
    CreateResidentialBattery(dtHour);
    util_rate = new rate_data();
    set_up_residential_1_4_peak(*util_rate);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
        max_current,
        max_current, max_power, max_power, max_power, max_power,
        0, dispatch_t::BTM_MODES::FORECAST, 0, 1, 24, 1, true,
        true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    load_prediction = { 1.44289, 1.27067, 1.1681, 1.09342, 1.12921, 1.39345, 1.57299, 1.63055, 1.85622, 2.44991, 2.61812, 2.90909, 3.29601, 3.64366, 3.88232, 3.99237, 4.09673, 4.11102, 4.09175, 4.13445, 3.91011, 3.27815, 2.67845, 2.11802, 1.78025, 1.57142, 1.42908, 1.32466,
                            1.34971, 1.65378, 1.80832, 1.89189, 2.15165, 2.83263, 2.98228, 3.22567, 3.50516, 3.83516, 3.92251, 4.05548, 4.13676, 4.13277, 4.0915, 4.19724, 4.00006, 3.34509, 2.68845, 2.08509, 1.7126, };

    pv_prediction = { -0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, 0.129814, 0.75348, 1.47006, 2.45093, 2.9696, 3.30167, 3.47537, 3.42799, 3.14281, 2.59477, 1.83033, 0.857618, 0.176968, - 0.00116655, - 0.00116655, - 0.00116655
                - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, 0.078559, 0.420793, 1.35006, 2.03824, 2.47638, 2.70446, 3.22802, 2.74022, 2.81986, 2.39299, 1.68699, 0.881843, 
                0.169532, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655    };

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);
    dispatchAutoBTM->setup_rate_forecast();

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    std::vector<double> expectedPower = { 1.50, 0, 0, 0, 0, 0, 1.6397, 0, 0, 0, 0, -0.058, -0.0054, 0, 0, 0.884, 1.5645, 2.3757, 3.3688,
                                         4.122, 3.366, 0, 0, 0, 0 };
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerPV = pv_prediction[h]; // Match the predicted PV
        batteryPower->powerLoad = load_prediction[h]; // Match the predicted load
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.5) << " error in expected at hour " << h;
    }
}
