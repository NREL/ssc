#include "lib_battery_dispatch_automatic_fom_test.h"


TEST_F(AutoFOMTest_lib_battery_dispatch, DispatchFOMInput) {
    double dtHourFOM = 1.0;
    CreateBattery(dtHourFOM);
    dispatchAutoFOM = new dispatch_automatic_front_of_meter_t(batteryModelFOM, dtHourFOM, 15, 95, 1, 999, 999, 500, 500,
                                                              500, 500, 1, 3, 0, 1, 24, 1, true, true, false, true, 0,
                                                              0, 0, 0, ppaRate, ur, 98, 98, 98);

    std::vector<double> P_batt = {-336.062, 336.062};

    batteryPower = dispatchAutoFOM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->powerPV = 750;
    batteryPower->powerFuelCell = 300;

    dispatchAutoFOM->set_custom_dispatch(P_batt);

    // battery charging from PV to full SOC_max
    EXPECT_FALSE(batteryPower->canGridCharge);
    dispatchAutoFOM->update_dispatch(0, 0, 0);
    EXPECT_NEAR(batteryPower->powerBatteryTarget, -322.6, 0.1);
    dispatchAutoFOM->dispatch(0, 0, 0);

    EXPECT_NEAR(batteryPower->powerBatteryDC, -188.0, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryAC, -195.9, 0.1);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
    EXPECT_NEAR(dispatchAutoFOM->battery_model()->capacity_model()->SOC(), 95, 1e-2);

    dispatchAutoFOM->update_dispatch(0, 0, 1);
    EXPECT_NEAR(batteryPower->powerBatteryTarget, 350.0, 0.1);
    dispatchAutoFOM->dispatch(0, 1, 0);

    EXPECT_NEAR(batteryPower->powerBatteryDC, 350, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryAC, 336, 0.1);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
}

TEST_F(AutoFOMTest_lib_battery_dispatch, DispatchFOMInputSubhourly) {
    double dtHourFOM = 0.5;
    CreateBattery(dtHourFOM);
    dispatchAutoFOM = new dispatch_automatic_front_of_meter_t(batteryModelFOM, dtHourFOM, 15, 95, 1, 999, 999, 500, 500,
                                                              500, 500, 1, 3, 0, 1, 24, 1, true, true, false, true, 0,
                                                              0, 0, 0, ppaRate, ur, 98, 98, 98);

    std::vector<double> P_batt = {-336.062, 336.062};

    batteryPower = dispatchAutoFOM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->powerPV = 750;
    batteryPower->powerFuelCell = 300;

    dispatchAutoFOM->set_custom_dispatch(P_batt);

    // battery charging from PV to full SOC_max
    EXPECT_FALSE(batteryPower->canGridCharge);
    dispatchAutoFOM->update_dispatch(0, 0, 0);
    EXPECT_NEAR(batteryPower->powerBatteryTarget, -322.6, 0.1);
    dispatchAutoFOM->dispatch(0, 0, 0);

    EXPECT_NEAR(batteryPower->powerBatteryDC, -188.0, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryAC, -195.9, 0.1);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
    EXPECT_NEAR(dispatchAutoFOM->battery_model()->capacity_model()->SOC(), 95, 1e-2);

    dispatchAutoFOM->update_dispatch(0, 0, 1);
    EXPECT_NEAR(batteryPower->powerBatteryTarget, 350.0, 0.1);
    dispatchAutoFOM->dispatch(0, 1, 0);

    EXPECT_NEAR(batteryPower->powerBatteryDC, 350, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryAC, 336, 0.1);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
}

TEST_F(AutoFOMDC_lib_battery_dispatch, DispatchFOM_DCCustomCharge) {
    dispatchChoice = dispatch_t::FOM_CUSTOM_DISPATCH;
    dispatchAutoDC = new dispatch_automatic_front_of_meter_t(batteryModelDC, dtHourDC, 10, 100, 1, 49960, 49960, 25000,
                                                             25000, 25000, 25000, 1, dispatchChoice, 0, 1, 18, 1, true,
                                                             true, true, false, 77000, 0, 1, 0.005, ppaRate, ur, 98, 98,
                                                             98);

    std::vector<double> P_batt;
    for (int i = 0; i < 8760; i++) {
        P_batt.push_back(-2915); // Charge as fast as possible
    }

    // battery setup
    dispatchAutoDC->update_pv_data(pv);
    dispatchAutoDC->update_cliploss_data(clip);
    dispatchAutoDC->set_custom_dispatch(P_batt);
    batteryPower = dispatchAutoDC->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->canGridCharge = true;
    batteryPower->setSharedInverter(m_sharedInverter);

    batteryPower->powerGeneratedBySystem = 0;
    batteryPower->powerPV = 0;
    batteryPower->powerPVClipped = 0;
    batteryPower->powerGridToBattery = 30000;

    std::vector<double> p_batterykW;
    double remainingPower = dispatchAutoDC->battery_power_to_fill();
    for (size_t h = 0; h < 24; h++) {
        remainingPower = dispatchAutoDC->battery_power_to_fill();
        dispatchAutoDC->dispatch(0, h, 0);
        p_batterykW.push_back(batteryPower->powerBatteryDC);
        EXPECT_NEAR(batteryPower->powerBatteryDC, -2915, 1e-3) << "error in dispatched power at hour " << h;
        if (remainingPower < -29157) {
            EXPECT_LT(batteryPower->powerBatteryDC, -29157) << "hour " << h;
            EXPECT_LT(batteryPower->powerGridToBattery, -29157) << "hour " << h;
        }
    }
    std::sort(p_batterykW.begin(), p_batterykW.end(), std::greater<double>());

    EXPECT_NEAR(p_batterykW[0], 3046.1, 0.1);
    EXPECT_NEAR(p_batterykW[23], -29403.5, 0.1);
}

TEST_F(AutoFOMDC_lib_battery_dispatch, DispatchFOM_DCAuto) {
    dispatchAutoDC = new dispatch_automatic_front_of_meter_t(batteryModelDC, dtHourDC, 10, 100, 1, 49960, 49960, 25000,
                                                             25000, 25000, 25000, 1, 0, 0, 1, 18, 1, true, true, false,
                                                             false, 77000, 0, 1, 0.005, ppaRate, ur, 98, 98, 98);

    // battery setup
    dispatchAutoDC->update_pv_data(pv);
    dispatchAutoDC->update_cliploss_data(clip);
    batteryPower = dispatchAutoDC->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->setSharedInverter(m_sharedInverter);

    std::vector<double> p_batterykW;
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerGeneratedBySystem = pv[h];
        batteryPower->powerPV = pv[h];
        batteryPower->powerPVClipped = clip[h];
        dispatchAutoDC->dispatch(0, h, 0);
        p_batterykW.push_back(batteryPower->powerBatteryAC);
    }
    std::sort(p_batterykW.begin(), p_batterykW.end(), std::greater<double>());

    EXPECT_NEAR(p_batterykW[0], 3046.1, 0.1);
    EXPECT_NEAR(p_batterykW[23], -29403.5, 0.1);
}


TEST_F(AutoFOMTest_lib_battery_dispatch, DispatchFOM_ACCustomCharge) {
    double dtHour = 1;
    CreateBattery(dtHour);
    dispatchChoice = dispatch_t::FOM_CUSTOM_DISPATCH;
    dispatchAutoFOM = new dispatch_automatic_front_of_meter_t(batteryModelFOM, dtHour, 10, 100, 1, 49960, 49960, 25000,
                                                              25000, 25000, 25000, 1, dispatchChoice, 0, 1, 18, 1, true,
                                                              true, true, false, 77000, 0, 1, 0.005, ppaRate, ur, 98,
                                                              98, 98);

    std::vector<double> P_batt;
    for (int i = 0; i < 8760; i++) {
        P_batt.push_back(-2915); // Charge as fast as possible
    }

    // battery setup
    dispatchAutoFOM->update_pv_data(pv);
    dispatchAutoFOM->set_custom_dispatch(P_batt);
    batteryPower = dispatchAutoFOM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->canGridCharge = true;

    batteryPower->powerGeneratedBySystem = 0;
    batteryPower->powerPV = 0;
    batteryPower->powerPVClipped = 0;
    batteryPower->powerGridToBattery = 30000;

    std::vector<double> p_batterykW;
    double remainingPower = dispatchAutoFOM->battery_power_to_fill();
    for (size_t h = 0; h < 24; h++) {
        remainingPower = dispatchAutoFOM->battery_power_to_fill();
        double current;
        double power = dispatchAutoFOM->battery_model()->calculate_max_charge_kw(&current);
        printf("max power%f\n", power);
        while (current > -1000) {
            printf("soc %f, current %f\n", dispatchAutoFOM->battery_model()->battery_soc(), current);
            power = dispatchAutoFOM->battery_model()->run(0, current);
            printf("soc %f, power %f\n", dispatchAutoFOM->battery_model()->battery_soc(), power);
            current -= 100;
        }
        return;
        dispatchAutoFOM->dispatch(0, h, 0);
        p_batterykW.push_back(batteryPower->powerBatteryDC);
        EXPECT_NEAR(batteryPower->powerBatteryDC, -2915, 1e-3) << "error in dispatched power at hour " << h;
        if (remainingPower < -29157) {
            EXPECT_LT(batteryPower->powerBatteryDC, -29157) << "hour " << h;
            EXPECT_LT(batteryPower->powerGridToBattery, -29157) << "hour " << h;
        }
    }
    std::sort(p_batterykW.begin(), p_batterykW.end(), std::greater<double>());

    EXPECT_NEAR(p_batterykW[0], 3046.1, 0.1);
    EXPECT_NEAR(p_batterykW[23], -29403.5, 0.1);
}

TEST_F(AutoFOMTest_lib_battery_dispatch, DispatchFOM_ACAuto) {
    double dtHour = 1;
    CreateBattery(dtHour);
    dispatchAutoFOM = new dispatch_automatic_front_of_meter_t(batteryModelFOM, dtHour, 10, 100, 1, 49960, 49960, 25000,
                                                              25000, 25000, 25000, 1, 0, 0, 1, 18, 1, true, true, false,
                                                              false, 77000, 0, 1, 0.005, ppaRate, ur, 98, 98, 98);

    // battery setup
    dispatchAutoFOM->update_pv_data(pv);
    batteryPower = dispatchAutoFOM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->voltageSystem = 600;

    std::vector<double> p_batterykW;
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerGeneratedBySystem = pv[h];
        batteryPower->powerPV = pv[h];
        dispatchAutoFOM->dispatch(0, h, 0);
        p_batterykW.push_back(batteryPower->powerBatteryAC);
    }
    std::sort(p_batterykW.begin(), p_batterykW.end(), std::greater<double>());

    EXPECT_NEAR(p_batterykW[0], 3046.1, 0.1);
    EXPECT_NEAR(p_batterykW[23], -29403.5, 0.1);
}


