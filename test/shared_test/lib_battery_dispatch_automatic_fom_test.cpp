#include "lib_battery_dispatch_automatic_fom_test.h"


TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOMInput) {
    double dtHourFOM = 1.0;
    CreateBattery(dtHourFOM);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHourFOM, 15, 95, 1, 999, 999, max_power, max_power,
                                                           max_power, max_power, 1, dispatch_t::FOM_CUSTOM_DISPATCH, dispatch_t::FRONT, 1, 24, 1, true, true, false, true, 0,
                                                              0, 0, 0, ppaRate, ur, 98, 98, 98);

    std::vector<double> P_batt = {-336.062, 336.062};

    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->powerPV = 750;
    batteryPower->powerFuelCell = 300;

    dispatchAuto->set_custom_dispatch(P_batt);

    // battery charging from PV
    EXPECT_FALSE(batteryPower->canGridCharge);
    dispatchAuto->update_dispatch(0, 0, 0);
    EXPECT_NEAR(batteryPower->powerBatteryTarget, -322.6, 0.1);
    dispatchAuto->dispatch(0, 0, 0);

    EXPECT_NEAR(batteryPower->powerBatteryDC, -322.6, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryAC, -336.1, 0.1);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
    EXPECT_NEAR(dispatchAuto->battery_model()->capacity_model()->SOC(), 50.2, 1e-2);

    dispatchAuto->update_dispatch(0, 0, 1);
    EXPECT_NEAR(batteryPower->powerBatteryTarget, 350.0, 0.1);
    dispatchAuto->dispatch(0, 1, 0);

    EXPECT_NEAR(batteryPower->powerBatteryDC, 350, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryAC, 336, 0.1);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOMInputSubhourly) {
    double dtHourFOM = 0.5;
    CreateBattery(dtHourFOM);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHourFOM, 15, 95, 1, 999, 999, max_power, max_power,
                                                           max_power, max_power, 1, dispatch_t::FOM_CUSTOM_DISPATCH, dispatch_t::FRONT, 1, 24, 1, true, true, false, true, 0,
                                                              0, 0, 0, ppaRate, ur, 98, 98, 98);

    std::vector<double> P_batt = {-336.062, 336.062};

    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->powerPV = 750;
    batteryPower->powerFuelCell = 300;

    dispatchAuto->set_custom_dispatch(P_batt);

    // battery charging from PV to full SOC_max
    EXPECT_FALSE(batteryPower->canGridCharge);
    dispatchAuto->update_dispatch(0, 0, 0);
    EXPECT_NEAR(batteryPower->powerBatteryTarget, -322.6, 0.1);
    dispatchAuto->dispatch(0, 0, 0);

    EXPECT_NEAR(batteryPower->powerBatteryDC, -322.6, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryAC, -336.1, 0.1);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
    EXPECT_NEAR(dispatchAuto->battery_model()->capacity_model()->SOC(), 50.1, 0.1);

    dispatchAuto->update_dispatch(0, 0, 1);
    EXPECT_NEAR(batteryPower->powerBatteryTarget, 350.0, 0.1);
    dispatchAuto->dispatch(0, 0, 1);

    EXPECT_NEAR(batteryPower->powerBatteryDC, 350, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryAC, 336, 0.1);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_DCCustomCharge) {
    double dtHour = 1;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_CUSTOM_DISPATCH, dispatch_t::FRONT, 1, 18, 1, true,
                                                           true, true, false, 77000, 0, 1, 0.005, ppaRate, ur, 98, 98,
                                                           98);

    std::vector<double> P_batt(6, -25000);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    dispatchAuto->set_custom_dispatch(P_batt);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->canGridCharge = true;
    batteryPower->setSharedInverter(m_sharedInverter);

    batteryPower->powerGeneratedBySystem = 0;
    batteryPower->powerPV = 0;
    batteryPower->powerPVClipped = 0;

    std::vector<double> SOC = {64.42, 78.77, 94.09, 100., 100., 100.};
    for (size_t h = 0; h < 6; h++) {
        dispatchAuto->update_dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, -25000, 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, h, 0);

        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1);

        if (h < 3){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -25000, 1) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 25868, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 97.6, 0.1);
        }
        else if (h == 3){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -10259, 1) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 10586, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 97.9, 0.1);
        }
        else{
            EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 1e-3) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 0, 0.1);
        }
    }
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_DCCustomChargeSubhourly) {
    double dtHour = 0.5;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_CUSTOM_DISPATCH, dispatch_t::FRONT, 1, 18, 1, true,
                                                           true, true, false, 77000, 0, 1, 0.005, ppaRate, ur, 98, 98,
                                                           98);

    std::vector<double> P_batt(12, -25000);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    dispatchAuto->set_custom_dispatch(P_batt);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->canGridCharge = true;
    batteryPower->setSharedInverter(m_sharedInverter);

    batteryPower->powerGeneratedBySystem = 0;
    batteryPower->powerPV = 0;
    batteryPower->powerPVClipped = 0;

    std::vector<double> SOC = {57.24, 64.45, 71.64, 78.81, 86.20, 93.87, 100.00, 100.00, 100.00, 100.00, 100.00, 100.00};
    for (size_t h = 0; h < 12; h++) {
        size_t hour_of_year = hour_of_year_from_index(h, dtHour);
        size_t step = step_from_index(h, dtHour);

        dispatchAuto->update_dispatch(hour_of_year, step, h);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, -25000, 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, hour_of_year, step);

        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1) << "hour " << h;

        if (h < 6){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -25000, 1) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 25868, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 97.6, 0.1);
        }
        else if (h == 6){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -21499, 1) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 22213, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 97.76, 0.1);
        }
        else{
            EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 1e-3) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 0, 0.1);
        }
    }
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_DCAuto) {
    double dtHour = 1;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_LOOK_AHEAD, dispatch_t::FRONT, 1, 18, 1, true, true, false,
                                                           false, 77000, 0, 1, 0.005, ppaRate, ur, 98, 98, 98);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->setSharedInverter(m_sharedInverter);

    std::vector<double> targetkW = {-9767.18, -10052.40, -9202.19, -7205.42, -1854.60, 0.};
    std::vector<double> SOC = {55.72, 61.58, 66.93, 71.12, 72.21, 72.21};
    for (size_t h = 0; h < 6; h++) {
        batteryPower->powerGeneratedBySystem = pv[h];
        batteryPower->powerPV = pv[h];
        batteryPower->powerPVClipped = clip[h];

        dispatchAuto->update_dispatch(h, 0, h);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, targetkW[h], 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, targetkW[h], 0.1) << "error in dispatched power at hour " << h;
        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1) << "error in SOC at hour " << h;

    }
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_DCAutoSubhourly) {
    double dtHour = 0.5;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_LOOK_AHEAD, dispatch_t::FRONT, 1, 18, 1, true, true, false,
                                                           false, 77000, 0, 1, 0.005, ppaRate, ur, 98, 98, 98);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->setSharedInverter(m_sharedInverter);

    std::vector<double> targetkW = {-9767.18, -10052.40, -9202.19, -7205.42, -1854.60, 0.};
    std::vector<double> SOC = {52.86, 55.80, 58.49, 60.60, 61.14, 61.14};
    for (size_t h = 0; h < 6; h++) {
        size_t hour_of_year = hour_of_year_from_index(h, dtHour);
        size_t step = step_from_index(h, dtHour);

        batteryPower->powerGeneratedBySystem = pv[h];
        batteryPower->powerPV = pv[h];
        batteryPower->powerPVClipped = clip[h];

        dispatchAuto->update_dispatch(hour_of_year, step, h);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, targetkW[h], 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, hour_of_year, step);
        EXPECT_NEAR(batteryPower->powerBatteryDC, targetkW[h], 0.1) << "error in dispatched power at hour " << h;
        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1) << "error in SOC at hour " << h;
    }
}


TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_ACCustomCharge) {
    double dtHour = 1;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_CUSTOM_DISPATCH, dispatch_t::FRONT, 1, 18, 1, true,
                                                             true, true, false, 77000, 0, 1, 0.005, ppaRate, ur, 98, 98,
                                                             98);

    std::vector<double> P_batt(6, -25000);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    dispatchAuto->set_custom_dispatch(P_batt);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->canGridCharge = true;
    batteryPower->setSharedInverter(m_sharedInverter);

    batteryPower->powerGeneratedBySystem = 0;
    batteryPower->powerPV = 0;
    batteryPower->powerPVClipped = 0;

    std::vector<double> SOC = {63.86, 77.64, 91.63, 100.00, 100.00, 100.00};
    for (size_t h = 0; h < 6; h++) {
        dispatchAuto->update_dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, -24000, 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, h, 0);

        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h],0.1);

        if (h < 3){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -24000, 1) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 25000, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 96, 0.1);
        }
        else if (h == 3){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -14684, 1) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 15296, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 96, 0.1);
        }
        else{
            EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 1e-3) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 96, 0.1);
        }
    }
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_ACCustomChargeSubhourly) {
    double dtHour = 0.5;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_CUSTOM_DISPATCH, dispatch_t::FRONT, 1, 18, 1, true,
                                                           true, true, false, 77000, 0, 1, 0.005, ppaRate, ur, 98, 98,
                                                           98);

    std::vector<double> P_batt(12, -25000);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    dispatchAuto->set_custom_dispatch(P_batt);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->canGridCharge = true;
    batteryPower->setSharedInverter(m_sharedInverter);

    batteryPower->powerGeneratedBySystem = 0;
    batteryPower->powerPV = 0;
    batteryPower->powerPVClipped = 0;

    std::vector<double> SOC = {56.95, 63.88, 70.79, 77.68, 84.56, 91.43, 98.79, 100.00, 100.00, 100.00, 100.00, 100.00};
    for (size_t h = 0; h < 12; h++) {
        size_t hour_of_year = hour_of_year_from_index(h, dtHour);
        size_t step = step_from_index(h, dtHour);

        dispatchAuto->update_dispatch(hour_of_year, step, h);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, -24000, 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, hour_of_year, step);

        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1);

        if (h < 7){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -24000, 1) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 25000, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 96, 0.1);
        }
        else if (h == 7){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -4210, 1) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 4386, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 96, 0.1);
        }
        else{
            EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 1e-3) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 96, 0.1);
        }
    }
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_ACAuto) {
    double dtHour = 1;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_LOOK_AHEAD, dispatch_t::FRONT, 1, 18, 1, true, true, false,
                                                           false, 77000, 0, 1, 0.005, ppaRate, ur, 98, 98, 98);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->setSharedInverter(m_sharedInverter);

    std::vector<double> targetkW = {-9767.18, -10052.40, -9202.19, -7205.42, -1854.60, 0.};
    std::vector<double> SOC = {55.72, 61.58, 66.93, 71.12, 72.21, 72.21};
    for (size_t h = 0; h < 6; h++) {
        batteryPower->powerGeneratedBySystem = pv[h];
        batteryPower->powerPV = pv[h];
        batteryPower->powerPVClipped = clip[h];

        dispatchAuto->update_dispatch(h, 0, h);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, targetkW[h], 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, h, 0);

        EXPECT_NEAR(batteryPower->powerBatteryDC, targetkW[h], 0.1) << "error in dispatched power at hour " << h;
        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1) << "error in SOC at hour " << h;
    }
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_ACAutoSubhourly) {
    double dtHour = 0.5;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_LOOK_AHEAD, dispatch_t::FRONT, 1, 18, 1, true, true, false,
                                                           false, 77000, 0, 1, 0.005, ppaRate, ur, 98, 98, 98);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->setSharedInverter(m_sharedInverter);

    std::vector<double> targetkW = {-9767.18, -10052.40, -9202.19, -7205.42, -1854.60, 0.};
    std::vector<double> SOC = {52.86, 55.80, 58.49, 60.60, 61.14, 61.14};
    for (size_t h = 0; h < 6; h++) {
        size_t hour_of_year = hour_of_year_from_index(h, dtHour);
        size_t step = step_from_index(h, dtHour);

        batteryPower->powerGeneratedBySystem = pv[h];
        batteryPower->powerPV = pv[h];
        batteryPower->powerPVClipped = clip[h];

        dispatchAuto->update_dispatch(hour_of_year, step, h);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, targetkW[h], 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, hour_of_year, step);
        EXPECT_NEAR(batteryPower->powerBatteryDC, targetkW[h], 0.1) << "error in dispatched power at hour " << h;
        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1) << "error in SOC at hour " << h;

    }
}

