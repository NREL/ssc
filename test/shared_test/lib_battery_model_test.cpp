#include <iostream>

#include <gtest/gtest.h>

#include "lib_battery_model_test.h"

TEST_F(lib_battery_thermal_test, SetUpTest){
    CreateModel(Cp);
    EXPECT_NEAR(model->T_battery(), 290, tol);
    EXPECT_NEAR(model->capacity_percent(), 100, tol);
}

TEST_F(lib_battery_thermal_test, updateTemperatureTest) {
    CreateModel(Cp);
    // battery which adjusts quickly to temp {290,290,295,295,290,270,270};
    double I = 50;
    size_t idx = 0;
    model->updateTemperature(I, idx++);
    auto s = thermal_state({93.49, 290.0, 290});
    compareState(model, s, "updateTemperatureTest: 1");

    I = -50;
    model->updateTemperature(I, idx++);
    s = thermal_state({93.49, 290.0, 290});
    compareState(model, s, "updateTemperatureTest: 2");

    I = 50;
    model->updateTemperature(I, idx++);
    s = thermal_state({94.02, 290.67, 295});
    compareState(model, s, "updateTemperatureTest: 3");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({95.23, 292.2, 295});
    compareState(model, s, "updateTemperatureTest: 4");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({95, 291.9, 290});
    compareState(model, s, "updateTemperatureTest: 5");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({92.72, 289.05, 270});
    compareState(model, s, "updateTemperatureTest: 6");

    I = 100;
    model->updateTemperature(I, idx++);
    s = thermal_state({87.4, 282.42, 270});
    compareState(model, s, "updateTemperatureTest: 7");
}

//
//TEST_F(lib_battery_losses_test, MonthlyLossesTest){
//    model = std::unique_ptr<losses_t>(new losses_t(dt_hour, 0, chargingLosses, fullLosses));
//
//    // losses for charging and idling are the same
//    int charge_mode = capacity_t::CHARGE;
//
//    size_t idx = 0;
//    model->run_losses(idx, charge_mode);
//    EXPECT_NEAR(model->getLoss(idx), 0, tol) << "MonthlyLossesTest: 1";
//
//    idx = 40 * 24;
//    model->run_losses(idx, charge_mode);
//    EXPECT_NEAR(model->getLoss(idx), 1, tol) << "MonthlyLossesTest: 2";
//
//    idx = 70 * 24;
//    model->run_losses(idx, charge_mode);
//    EXPECT_NEAR(model->getLoss(idx), 2, tol) << "MonthlyLossesTest: 3";
//
//    // discharging
//    charge_mode = capacity_t::DISCHARGE;
//
//    idx = 0;
//    model->run_losses(idx, charge_mode);
//    EXPECT_NEAR(model->getLoss(idx), 1, tol) << "MonthlyLossesTest: 4";
//
//    idx = 40 * 24;
//    model->run_losses(idx, charge_mode);
//    EXPECT_NEAR(model->getLoss(idx), 2, tol) << "MonthlyLossesTest: 5";
//
//    idx = 70 * 24;
//    model->run_losses(idx, charge_mode);
//    EXPECT_NEAR(model->getLoss(idx), 3, tol) << "MonthlyLossesTest: 6";
//
//}
//
//TEST_F(lib_battery_losses_test, TimeSeriesLossesTest){
//    model = std::unique_ptr<losses_t>(new losses_t(dt_hour, 1, chargingLosses, fullLosses));
//
//    int charge_mode = -1;       // not used
//
//    size_t idx = 0;
//    model->run_losses(idx, charge_mode);
//    EXPECT_NEAR(model->getLoss(idx), 0, tol) << "TimeSeriesLossesTest: 1";
//
//    idx = 40;
//    model->run_losses(idx, charge_mode);
//    EXPECT_NEAR(model->getLoss(idx), 40, tol) << "TimeSeriesLossesTest: 2";
//
//    idx = 70;
//    model->run_losses(idx, charge_mode);
//    EXPECT_NEAR(model->getLoss(idx), 70, tol) << "TimeSeriesLossesTest: 3";
//
//}

TEST_F(lib_battery_test, SetUpTest){
    ASSERT_TRUE(1);
}

TEST_F(lib_battery_test, runTestCycleAt1C){
    size_t idx = 0;
    double capacity_passed = 0.;
    double I = Qfull * n_strings;
    batteryModel->run(idx++, I);
    capacity_passed += batteryModel->capacity_model()->I() * batteryModel->voltage_model()->battery_voltage() / 1000.;
//    std::cerr << "\n" << idx << ": " << capacity_passed << "\n";

    auto s = battery_state({{479.75, 1000, 960.65, 20.25, 0, 49.94, 50.059, 47.95, 2}, // cap
                            500.66, // voltage
                           100, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                            {102, 0, 0}, // calendar
                           {96.065, 293.23}, // thermal
                           0});
    compareState(batteryModel, s, "runTestCycleAt1C: 1");

    while (batteryModel->capacity_model()->SOC() > SOC_min + 1){
        batteryModel->run(idx++, I);
        capacity_passed += batteryModel->capacity_model()->I() * batteryModel->voltage_model()->battery_voltage() / 1000.;
    }
//    std::cerr <<  idx << ": soc " << batteryModel->capacity_model()->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s = battery_state({{54.5, 1000, 960.65, 20.25, 0, 5.67, 94.32, 92.21, 2}, // cap
                       316.979, // voltage
                       100, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                        {101.976, 0, 21}, // calendar
                       {96.06, 293.23}, // thermal
                       0});
    compareState(batteryModel, s, "runTestCycleAt1C: 2");

    size_t n_cycles = 400;

    while (n_cycles-- > 0){
        I *= -1;
        while (batteryModel->capacity_model()->SOC() < SOC_max - 1){
            batteryModel->run(idx++, I);
            capacity_passed += -batteryModel->capacity_model()->I() * batteryModel->voltage_model()->battery_voltage() / 1000.;
        }
        I *= -1;
        while (batteryModel->capacity_model()->SOC() > SOC_min + 1){
            batteryModel->run(idx++, I);
            capacity_passed += batteryModel->capacity_model()->I() * batteryModel->voltage_model()->battery_voltage() / 1000.;
        }
    }
//    std::cerr <<  idx << ": soc " << batteryModel->capacity_model()->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s = battery_state({{50.55, 920.55, 883.85, 8.95, 0, 5.71, 94.28, 93.267, 2}, // cap
                       353.44, // voltage
                       93.08, {92.05, 398, 89.09, 88.99, 89.163, 89.32, 5, std::vector<double>()}, // cycle
                        {98.035, 2693, 64635, 0.039}, // calendar
                       {96.03, 293.166, 118774800}, // thermal
                       32991});
    compareState(batteryModel, s, "runTestCycleAt1C: 3");

    EXPECT_NEAR(capacity_passed, 361257, 1000) << "Current passing through cell";
    double qmax = fmax(s.capacity.qmax_lifetime, s.capacity.qmax_thermal);
    EXPECT_NEAR(qmax/q, .93, 0.01) << "capacity relative to max capacity";
}

TEST_F(lib_battery_test, runTestCycleAt3C){
    size_t idx = 0;
    double capacity_passed = 0.;
    double I = Qfull * n_strings * 3;
    batteryModel->run(idx++, I);
    capacity_passed += batteryModel->capacity_model()->I() * batteryModel->voltage_model()->battery_voltage() / 1000.;
//    std::cerr << "\n" << idx << ": " << capacity_passed << "\n";

    auto s = battery_state({{439.25, 1000, 965.85, 60.75, 0, 45.47, 54.52, 48.23, 2}, // cap
                            373.39, // voltage
                            100, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                             {102, 0}, // calendar
                            {96.58, 293.88}, // thermal
                            0});
    compareState(batteryModel, s, "runTest: 1");

    while (batteryModel->capacity_model()->SOC() > SOC_min + 1){
        batteryModel->run(idx++, I);
        capacity_passed += batteryModel->capacity_model()->I() * batteryModel->voltage_model()->battery_voltage() / 1000.;
    }
//    std::cerr <<  idx << ": soc " << batteryModel->capacity_model()->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s = battery_state({{48.29, 1000, 961.10, 26.45, 0, 5.02, 94.97, 92.22, 2}, // cap
                       271.17, // voltage
                       101.98, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                        {101.98, 0, 7}, // calendar
                       {96.11, 293.288}, // thermal
                       0});
    compareState(batteryModel, s, "runTest: 2");

    size_t n_cycles = 400;

    while (n_cycles-- > 0){
        I *= -1;
        while (batteryModel->capacity_model()->SOC() < SOC_max - 1){
            batteryModel->run(idx++, I);
            capacity_passed += -batteryModel->capacity_model()->I() * batteryModel->voltage_model()->battery_voltage() / 1000.;
        }
        I *= -1;
        while (batteryModel->capacity_model()->SOC() > SOC_min + 1){
            batteryModel->run(idx++, I);
            capacity_passed += batteryModel->capacity_model()->I() * batteryModel->voltage_model()->battery_voltage() / 1000.;
        }
    }
//    std::cerr <<  idx << ": soc " << batteryModel->capacity_model()->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s = battery_state({{50.05, 920.97, 884.25, 8.96, 0, 5.66, 94.33, 93.328, 2}, // cap
                       351.25, // voltage
                       93.08, {92.09, 396, 89.17, 88.78, 89.24, 89.39, 9, std::vector<double>()}, // cycle
                        {97.87, 2913, 69927, 0.041}, // calendar
                       {96.01, 293.16, 118774800}, // thermal
                       32991});
    compareState(batteryModel, s, "runTest: 3");


    EXPECT_NEAR(capacity_passed, 360643, 100) << "Current passing through cell";
    double qmax = fmax(s.capacity.qmax_lifetime, s.capacity.qmax_thermal);
    EXPECT_NEAR(qmax/q, 0.9209, 0.01) << "capacity relative to max capacity";
}

TEST_F(lib_battery_test, runDuplicates) {
    auto cap_state = batteryModel->capacity_model()->get_state();
    auto volt_state = batteryModel->voltage_model()->get_state();

    auto Battery = new battery_t(*batteryModel);

    double I = 10;
    Battery->run(0, I);

    auto cap_state2 = batteryModel->capacity_model()->get_state();
    auto volt_state2 = batteryModel->voltage_model()->get_state();

    auto cap_state3 = Battery->capacity_model()->get_state();
    auto volt_state3 = Battery->voltage_model()->get_state();
}

TEST_F(lib_battery_test,logging) {
    std::cout << batteryModel->capacity_model()->get_state() << "\n";
    std::cout << batteryModel->capacity_model()->get_params() << "\n\n";

    std::cout << batteryModel->voltage_model()->get_state() << "\n";
    std::cout << batteryModel->voltage_model()->get_params() << "\n\n";

    std::cout << batteryModel->lifetime_model()->get_state() << "\n";
    std::cout << batteryModel->lifetime_model()->get_params() << "\n\n";
}
