#include <gtest/gtest.h>

#include "lib_battery_model_test.h"

TEST_F(lib_battery_thermal_test, SetUpTest){
    EXPECT_NEAR(model->get_T_battery(), 293.15, tol);
    EXPECT_NEAR(model->capacity_percent(), 96, tol);
}

TEST_F(lib_battery_thermal_test, updateTemperatureTest) {
    CreateModel(Cp);
    // battery which adjusts quickly to temp
    double I = 50;
    size_t idx = 0;
    model->updateTemperature(I, idx++);
    auto s = thermal_state({93.87, 290.495, 290});
    compareState(model, s, "updateTemperatureTest: 1");

    I = -50;
    model->updateTemperature(I, idx++);
    s = thermal_state({93.87, 290.495, 290});
    compareState(model, s, "updateTemperatureTest: 2");

    I = 50;
    model->updateTemperature(I, idx++);
    s = thermal_state({97.37, 294.86, 295});
    compareState(model, s, "updateTemperatureTest: 3");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({97.49, 295.02, 295});
    compareState(model, s, "updateTemperatureTest: 4");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({94.05, 290.72, 290});
    compareState(model, s, "updateTemperatureTest: 5");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({79.54, 272.92, 270});
    compareState(model, s, "updateTemperatureTest: 6");

    I = 100;
    model->updateTemperature(I, idx++);
    s = thermal_state({77.66, 271.98, 270});
    compareState(model, s, "updateTemperatureTest: 7");
}

TEST_F(lib_battery_thermal_test, updateTemperatureTest2){
    CreateModel(Cp * 2);
    // slower adjusting batt
    double I = 50;
    size_t idx = 0;
    model->updateTemperature(I, idx++);
    auto s = thermal_state({93.87, 290.495, 290});
    compareState(model, s, "updateTemperatureTest: 1");

    I = -50;
    model->updateTemperature(I, idx++);
    s = thermal_state({93.87, 290.495, 290});
    compareState(model, s, "updateTemperatureTest: 2");

    I = 50;
    model->updateTemperature(I, idx++);
    s = thermal_state({96.89, 294.26, 295});
    compareState(model, s, "updateTemperatureTest: 3");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({97.49, 295.02, 295});
    compareState(model, s, "updateTemperatureTest: 4");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({94.58, 291.38, 290});
    compareState(model, s, "updateTemperatureTest: 5");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({82.15, 275.84, 270});
    compareState(model, s, "updateTemperatureTest: 6");

    I = 100;
    model->updateTemperature(I, idx++);
    s = thermal_state({77.69, 271.99, 270});
    compareState(model, s, "updateTemperatureTest: 7");
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