#include <gtest/gtest.h>

#include "logger.h"
#include "lib_battery_model_test.h"

TEST_F(lib_battery_thermal_test, SetUpTest){
    CreateModel(Cp);
    EXPECT_NEAR(model->T_battery(), 16.85, tol);
    EXPECT_NEAR(model->capacity_percent(), 100, tol);
}

TEST_F(lib_battery_thermal_test, updateTemperatureTest) {
    CreateModel(Cp);
    // battery which adjusts quickly to temp {16.85, 16.85, 21.85, 21.85, 16.85, -3.15, -3.15};
    double I = 50;
    size_t idx = 0;
    model->updateTemperature(I, idx++);
    auto s = thermal_state({93.49, 16.86, 16.85});
    compareState(model, s, "updateTemperatureTest: 1");

    I = -50;
    model->updateTemperature(I, idx++);
    s = thermal_state({93.49, 16.87, 16.85});
    compareState(model, s, "updateTemperatureTest: 2");

    I = 50;
    model->updateTemperature(I, idx++);
    s = thermal_state({94.02, 17.53, 21.85});
    compareState(model, s, "updateTemperatureTest: 3");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({94.88, 18.61, 21.85});
    compareState(model, s, "updateTemperatureTest: 4");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({95.00, 18.76, 16.85});
    compareState(model, s, "updateTemperatureTest: 5");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({92.55, 15.69, -3.15});
    compareState(model, s, "updateTemperatureTest: 6");

    I = 100;
    model->updateTemperature(I, idx++);
    s = thermal_state({88.85, 11.06, -3.15});
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

    auto s = battery_state({{479.75, 1000, 960.01, 20.25, 0, 49.97, 50.02, 47.91, 2}, // cap
                            550.65, // voltage
                           100, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                            {102, 0, 0}, // calendar
                           {96.00, 20.00, 20}, // thermal
                           0});
    compareState(batteryModel, s, "runTestCycleAt1C: 1");

    while (batteryModel->capacity_model()->SOC() > SOC_min + 1){
        batteryModel->run(idx++, I);
        capacity_passed += batteryModel->capacity_model()->I() * batteryModel->voltage_model()->battery_voltage() / 1000.;
    }
//    std::cerr <<  idx << ": soc " << batteryModel->capacity_model()->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s = battery_state({{54.5, 1000, 960.07, 20.25, 0, 5.67, 94.32, 92.21, 2}, // cap
                       366.96, // voltage
                       100, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                        {101.976, 0, 21}, // calendar
                       {96.01, 20.01, 20}, // thermal
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
    s = battery_state({{47.36, 920.55, 883.74, 8.93, 0, 5.35, 94.64, 93.62, 2}, // cap
                       354.71, // voltage
                       93.08, {92.05, 398, 88.94, 89.05, 88.97, 89.65, 5, std::vector<double>()}, // cycle
                        {98.01, 2754, 66099, 0.039}, // calendar
                       {96.0, 20.00, 20}, // thermal
                       32991});
    compareState(batteryModel, s, "runTestCycleAt1C: 3");

    EXPECT_NEAR(capacity_passed, 352736, 1000) << "Current passing through cell";
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

    auto s = battery_state({{439.25, 1000, 960.15, 60.75, 0, 45.74, 54.25, 47.92, 2}, // cap
                            548.35, // voltage
                            100, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                             {102, 0}, // calendar
                            {96.01, 20.01, 20}, // thermal
                            0});
    compareState(batteryModel, s, "runTest: 1");

    while (batteryModel->capacity_model()->SOC() > SOC_min + 1){
        batteryModel->run(idx++, I);
        capacity_passed += batteryModel->capacity_model()->I() * batteryModel->voltage_model()->battery_voltage() / 1000.;
    }
//    std::cerr <<  idx << ": soc " << batteryModel->capacity_model()->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s = battery_state({{48.03, 1000, 960.47, 26.72, 0, 5.00, 95, 92.22, 2}, // cap
                       339.03, // voltage
                       101.98, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                        {101.98, 0, 7}, // calendar
                       {96.06, 20.07, 20}, // thermal
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
    s = battery_state({{48.84, 920.77, 883.95, 9.00, 0, 5.52, 94.47, 93.45, 2}, // cap
                       361.33, // voltage
                       93.08, {92.07, 397, 88.62, 88.80, 88.65, 89.48, 7, std::vector<double>()}, // cycle
                        {98.06, 2655, 63732, 0.041}, // calendar
                       {96.01, 20, 20}, // thermal
                       32991});
    compareState(batteryModel, s, "runTest: 3");


    EXPECT_NEAR(capacity_passed, 352421, 100) << "Current passing through cell";
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
    logger log;

    log << batteryModel->capacity_model()->get_state() << "\n";
    log << batteryModel->capacity_model()->get_params() << "\n\n";

    log << batteryModel->voltage_model()->get_state() << "\n";
    log << batteryModel->voltage_model()->get_params() << "\n\n";

    log << batteryModel->lifetime_model()->get_state() << "\n";
    log << batteryModel->lifetime_model()->get_params() << "\n\n";

    log << batteryModel->thermal_model()->get_state() << "\n";
    log << batteryModel->thermal_model()->get_params() << "\n\n";
}
