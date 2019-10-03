#include <gtest/gtest.h>

#include "lib_battery_model_test.h"

TEST_F(lib_battery_thermal_test, SetUpTest){
    EXPECT_NEAR(model->T_battery(), 293.15, tol);
    EXPECT_NEAR(model->capacity_percent(), 96, tol);
}

TEST_F(lib_battery_thermal_test, updateTemperatureTest){
    // Test with various currents at T = 273.15 k
    size_t idx = 0;

    double I = 50;
    model->updateTemperature(I, dt_hour, idx);
    EXPECT_NEAR(model->capacity_percent(), 80.619, tol) << "updateTemperatureTest: 1";
    EXPECT_NEAR(model->T_battery(), 273.923, tol) << "updateTemperatureTest: 1";

    I = 500;
    model->updateTemperature(I, dt_hour, idx);
    EXPECT_NEAR(model->capacity_percent(), 100, tol) << "updateTemperatureTest: 2";
    EXPECT_NEAR(model->T_battery(), 350.114, tol) << "updateTemperatureTest: 2";

    I = 5;
    model->updateTemperature(I, dt_hour, idx);
    EXPECT_NEAR(model->capacity_percent(), 80, tol) << "updateTemperatureTest: 3";
    EXPECT_NEAR(model->T_battery(), 273.15, tol) << "updateTemperatureTest: 3";

    // T = 293.15 K
    idx = 4;

    I = 50;
    model->updateTemperature(I, dt_hour, idx);
    EXPECT_NEAR(model->capacity_percent(), 100, tol) << "updateTemperatureTest: 4";
    EXPECT_NEAR(model->T_battery(), 305.168, tol) << "updateTemperatureTest: 4";

    I = 500;
    model->updateTemperature(I, dt_hour, idx);
    EXPECT_NEAR(model->capacity_percent(), 100, tol) << "updateTemperatureTest: 5";
    EXPECT_NEAR(model->T_battery(), 363.792, tol) << "updateTemperatureTest: 5";

    I = 5;
    model->updateTemperature(I, dt_hour, idx);
    EXPECT_NEAR(model->capacity_percent(), 96, tol) << "updateTemperatureTest: 6";
    EXPECT_NEAR(model->T_battery(), 293.15, tol) << "updateTemperatureTest: 6";

    // T = 313.15
    idx = 8;

    I = 50;
    model->updateTemperature(I, dt_hour, idx);
    EXPECT_NEAR(model->capacity_percent(), 100, tol) << "updateTemperatureTest: 7";
    EXPECT_NEAR(model->T_battery(), 325.168, tol) << "updateTemperatureTest: 7";

    I = 500;
    model->updateTemperature(I, dt_hour, idx);
    EXPECT_NEAR(model->capacity_percent(), 100, tol) << "updateTemperatureTest: 8";
    EXPECT_NEAR(model->T_battery(), 383.792, tol) << "updateTemperatureTest: 8";

    I = 5;
    model->updateTemperature(I, dt_hour, idx);
    EXPECT_NEAR(model->capacity_percent(), 100, tol) << "updateTemperatureTest: 9";
    EXPECT_NEAR(model->T_battery(), 313.15, tol) << "updateTemperatureTest: 9";
}

TEST_F(lib_battery_losses_test, MonthlyLossesTest){
    model = std::unique_ptr<losses_t>(new losses_t(dt_hour, 0, chargingLosses,
                                                   dischargingLosses, chargingLosses, fullLosses));

    // losses for charging and idling are the same
    int charge_mode = capacity_t::CHARGE;

    size_t idx = 0;
    model->run_losses(idx, charge_mode);
    EXPECT_NEAR(model->getLoss(idx), 0, tol) << "MonthlyLossesTest: 1";

    idx = 40 * 24;
    model->run_losses(idx, charge_mode);
    EXPECT_NEAR(model->getLoss(idx), 1, tol) << "MonthlyLossesTest: 2";

    idx = 70 * 24;
    model->run_losses(idx, charge_mode);
    EXPECT_NEAR(model->getLoss(idx), 2, tol) << "MonthlyLossesTest: 3";

    // discharging
    charge_mode = capacity_t::DISCHARGE;

    idx = 0;
    model->run_losses(idx, charge_mode);
    EXPECT_NEAR(model->getLoss(idx), 1, tol) << "MonthlyLossesTest: 4";

    idx = 40 * 24;
    model->run_losses(idx, charge_mode);
    EXPECT_NEAR(model->getLoss(idx), 2, tol) << "MonthlyLossesTest: 5";

    idx = 70 * 24;
    model->run_losses(idx, charge_mode);
    EXPECT_NEAR(model->getLoss(idx), 3, tol) << "MonthlyLossesTest: 6";

}

TEST_F(lib_battery_losses_test, TimeSeriesLossesTest){
    model = std::unique_ptr<losses_t>(new losses_t(dt_hour, 1, chargingLosses,
                                                   dischargingLosses, chargingLosses, fullLosses));

    int charge_mode = -1;       // not used

    size_t idx = 0;
    model->run_losses(idx, charge_mode);
    EXPECT_NEAR(model->getLoss(idx), 0, tol) << "TimeSeriesLossesTest: 1";

    idx = 40;
    model->run_losses(idx, charge_mode);
    EXPECT_NEAR(model->getLoss(idx), 40, tol) << "TimeSeriesLossesTest: 2";

    idx = 70;
    model->run_losses(idx, charge_mode);
    EXPECT_NEAR(model->getLoss(idx), 70, tol) << "TimeSeriesLossesTest: 3";

}

TEST_F(lib_battery_test, runTest){
    size_t idx = 0;
    double I = 5;
    batteryModel->run(idx, I);

    auto s = battery_state({{495, 1000, 960.06, 5, 0, 51.56, 48.44, 0, 2}, // cap
                            562.37, // voltage
                           {{100, 0, 0, 0, 0, 0, 0, std::vector<double>()}, // cycle
                            {0, 102}, 100}, // calendar
                           {293.158, 96.01}, // thermal
                           0});
    compareState(batteryModel, s, "runTest: 1");
}