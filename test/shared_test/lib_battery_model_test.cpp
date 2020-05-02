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
    compareState(model->get_state(), s, "updateTemperatureTest: 1");

    I = -50;
    model->updateTemperature(I, idx++);
    s = thermal_state({93.49, 16.87, 16.85});
    compareState(model->get_state(), s, "updateTemperatureTest: 2");

    I = 50;
    model->updateTemperature(I, idx++);
    s = thermal_state({94.02, 17.53, 21.85});
    compareState(model->get_state(), s, "updateTemperatureTest: 3");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({94.88, 18.61, 21.85});
    compareState(model->get_state(), s, "updateTemperatureTest: 4");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({95.00, 18.76, 16.85});
    compareState(model->get_state(), s, "updateTemperatureTest: 5");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({92.55, 15.69, -3.15});
    compareState(model->get_state(), s, "updateTemperatureTest: 6");

    I = 100;
    model->updateTemperature(I, idx++);
    s = thermal_state({88.85, 11.06, -3.15});
    compareState(model->get_state(), s, "updateTemperatureTest: 7");
}


TEST_F(lib_battery_losses_test, MonthlyLossesTest){
    model = std::unique_ptr<losses_t>(new losses_t(chargingLosses, dischargingLosses, chargingLosses));

    // losses for charging and idling are the same
    int charge_mode = capacity_state::CHARGE;

    size_t idx = 0;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 0, tol) << "MonthlyLossesTest: 1";

    idx = 40 * 24;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 1, tol) << "MonthlyLossesTest: 2";

    idx = 70 * 24;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 2, tol) << "MonthlyLossesTest: 3";

    // discharging
    charge_mode = capacity_state::DISCHARGE;

    idx = 0;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 1, tol) << "MonthlyLossesTest: 4";

    idx = 40 * 24;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 2, tol) << "MonthlyLossesTest: 5";

    idx = 70 * 24;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 3, tol) << "MonthlyLossesTest: 6";

}

TEST_F(lib_battery_losses_test, TimeSeriesLossesTest){
    model = std::unique_ptr<losses_t>(new losses_t(fullLosses));

    int charge_mode = -1;       // not used

    size_t idx = 0;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 0, tol) << "TimeSeriesLossesTest: 1";

    idx = 40;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 40./8760, tol) << "TimeSeriesLossesTest: 2";

    idx = 70;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 70./8760, tol) << "TimeSeriesLossesTest: 3";

}

TEST_F(lib_battery_test, SetUpTest){
    ASSERT_TRUE(1);
}

TEST_F(lib_battery_test, runTestCycleAt1C){
    size_t idx = 0;
    double capacity_passed = 0.;
    double I = Qfull * n_strings;
    batteryModel->run(idx++, I);
    capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
//    std::cerr << "\n" << idx << ": " << capacity_passed << "\n";

    auto s = battery_state_test({{479.75, 1000, 960.01, 20.25, 0, 49.97, 50.02, 47.91, 2}, // cap
                            550.65, // voltage
                           100, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                            {102, 0, 0}, // calendar
                           {96.00, 20.00, 20}, // thermal
                           0});
    compareState(batteryModel, s, "runTestCycleAt1C: 1");

    while (batteryModel->SOC() > SOC_min + 1){
        batteryModel->run(idx++, I);
        capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
    }
//    std::cerr <<  idx << ": soc " << batteryModel->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s = battery_state_test({{54.5, 1000, 960.07, 20.25, 0, 5.67, 94.32, 92.21, 2}, // cap
                       366.96, // voltage
                       100, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                        {101.976, 0, 0.0002}, // calendar
                       {96.01, 20.01, 20}, // thermal
                       0});
    compareState(batteryModel, s, "runTestCycleAt1C: 2");

    size_t n_cycles = 400;

    while (n_cycles-- > 0){
        I *= -1;
        while (batteryModel->SOC() < SOC_max - 1){
            batteryModel->run(idx++, I);
            capacity_passed += -batteryModel->I() * batteryModel->V() / 1000.;
        }
        I *= -1;
        while (batteryModel->SOC() > SOC_min + 1){
            batteryModel->run(idx++, I);
            capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
        }
    }
//    std::cerr <<  idx << ": soc " << batteryModel->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s = battery_state_test({{47.36, 920.55, 883.74, 8.93, 0, 5.35, 94.64, 93.62, 2}, // cap
                       354.71, // voltage
                       93.08, {92.05, 398, 88.94, 89.05, 88.97, 89.65, 5, std::vector<double>()}, // cycle
                        {98.01, 2754, 0.039}, // calendar
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
    capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
//    std::cerr << "\n" << idx << ": " << capacity_passed << "\n";

    auto s = battery_state_test({{439.25, 1000, 960.15, 60.75, 0, 45.74, 54.25, 47.92, 2}, // cap
                            548.35, // voltage
                            100, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                             {102, 0}, // calendar
                            {96.01, 20.01, 20}, // thermal
                            0});
    compareState(batteryModel, s, "runTest: 1");

    while (batteryModel->SOC() > SOC_min + 1){
        batteryModel->run(idx++, I);
        capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
    }
//    std::cerr <<  idx << ": soc " << batteryModel->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s = battery_state_test({{48.03, 1000, 960.47, 26.72, 0, 5.00, 95, 92.22, 2}, // cap
                       339.03, // voltage
                       101.98, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                        {101.98, 0}, // calendar
                       {96.06, 20.07, 20}, // thermal
                       0});
    compareState(batteryModel, s, "runTest: 2");

    size_t n_cycles = 400;

    while (n_cycles-- > 0){
        I *= -1;
        while (batteryModel->SOC() < SOC_max - 1){
            batteryModel->run(idx++, I);
            capacity_passed += -batteryModel->I() * batteryModel->V() / 1000.;
        }
        I *= -1;
        while (batteryModel->SOC() > SOC_min + 1){
            batteryModel->run(idx++, I);
            capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
        }
    }
//    std::cerr <<  idx << ": soc " << batteryModel->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s = battery_state_test({{48.84, 920.77, 883.95, 9.00, 0, 5.52, 94.47, 93.45, 2}, // cap
                       361.33, // voltage
                       93.08, {92.07, 397, 88.62, 88.80, 88.65, 89.48, 7, std::vector<double>()}, // cycle
                        {98.06, 2655, 0.0393}, // calendar
                       {96.01, 20, 20}, // thermal
                       32991});
    compareState(batteryModel, s, "runTest: 3");


    EXPECT_NEAR(capacity_passed, 352421, 100) << "Current passing through cell";
    double qmax = fmax(s.capacity.qmax_lifetime, s.capacity.qmax_thermal);
    EXPECT_NEAR(qmax/q, 0.9209, 0.01) << "capacity relative to max capacity";
}

TEST_F(lib_battery_test, runDuplicates) {
    auto state = batteryModel->get_state();
    auto cap_state = state.capacity;
    auto volt_state = state.voltage;

    auto Battery = new battery_t(*batteryModel);

    double I = 10;
    Battery->run(0, I);

    state = batteryModel->get_state();
    auto cap_state2 = state.capacity;
    auto volt_state2 = state.voltage;

    state = Battery->get_state();
    auto cap_state3 = state.capacity;
    auto volt_state3 = state.voltage;
}

TEST_F(lib_battery_test, createFromParams) {
    auto params = std::make_shared<battery_params>(batteryModel->get_params());
    auto bat = battery_t(params);

    double current = 10;
    batteryModel->run(0, current);
    current = 10;
    bat.run(0, current);

    logger log(std::cout);
    log << *params << "\n" << bat.get_params() << "\n\n";
    log << batteryModel->get_state() << "\n" << bat.get_state() << "\n";

}

TEST_F(lib_battery_test,logging) {
    logger log(std::cout);
    auto state = batteryModel->get_state();
    auto params = batteryModel->get_params();

    log << *state.capacity << "\n";
    log << *params.capacity << "\n\n";

    log << *state.voltage << "\n";
    log << *params.voltage << "\n\n";

    log << *state.lifetime << "\n";
    log << *params.lifetime << "\n\n";

    log << *state.thermal << "\n";
    log << *params.thermal << "\n\n";

    log << *state.losses << "\n";
    log << *params.losses << "\n\n";

    log << *state.replacement << "\n";
    log << *params.replacement << "\n\n";

    log << state << "\n";
    log << params << "\n";
}

TEST_F(lib_battery_test, RoundtripEffModel){

    batteryModel->changeSOCLimits(0, 100);

    double full_current = 1000;
    double max_current;
    batteryModel->calculate_max_charge_kw(&max_current);

    double current = fabs(max_current) * 0.01;
    while (current < fabs(max_current)){
        capacityModel->updateCapacity(full_current, 1);   //discharge to empty

        size_t n_t = 0;
        current *= -1;
        double input_power = 0.;
        while(capacityModel->SOC() < 100 ){
            double input_current = current;
            capacityModel->updateCapacity(input_current, 1);
            voltageModel->updateVoltage(capacityModel->q0(), capacityModel->qmax(), capacityModel->I(), 0, 1);
            input_power += capacityModel->I() * voltageModel->battery_voltage();
            n_t += 1;

        }

        current *= -1;
        double output_power = 0.;
        while(voltageModel->calculate_max_discharge_w(capacityModel->q0(), capacityModel->qmax(), 0, nullptr) > 0 ){
            double output_current = current;
            capacityModel->updateCapacity(output_current, 1);
            voltageModel->updateVoltage(capacityModel->q0(), capacityModel->qmax(), capacityModel->I(), 0, 1);
            output_power += capacityModel->I() * voltageModel->battery_voltage();
            n_t += 1;

        }
//        printf("current %f, eff %f, n %d\n", current, -output_power/input_power, n_t);
        current += fabs(max_current) / 100.;
    }
}

TEST_F(lib_battery_test, RoundtripEffTable){
    std::vector<double> vals = {0, Vfull, 1.78, Vexp,
                                88.9, Vnom, 99, 0};
    util::matrix_t<double> table(4, 2, &vals);

    auto capacityModel = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min, dtHour);
    auto voltageModel = new voltage_table_t(n_series, n_strings, Vnom_default, table, resistance, 1);
    capacityModel->change_SOC_limits(0, 100);

    double full_current = 1000;
    double max_current;
    voltageModel->calculate_max_charge_w(capacityModel->q0(), capacityModel->qmax(), 0, &max_current);

    double current = fabs(max_current) * 0.01;
    while (current < fabs(max_current)){
        capacityModel->updateCapacity(full_current, 1);   //discharge to empty

        size_t n_t = 0;
        current *= -1;
        double input_power = 0.;
        while(capacityModel->SOC() < 100 ){
            double input_current = current;
            capacityModel->updateCapacity(input_current, 1);
            voltageModel->updateVoltage(capacityModel->q0(), capacityModel->qmax(), capacityModel->I(), 0, 1);
            input_power += capacityModel->I() * voltageModel->battery_voltage();
            n_t += 1;
        }

        current *= -1;
        double output_power = 0.;
        while(voltageModel->calculate_max_discharge_w(capacityModel->q0(), capacityModel->qmax(), 0, nullptr) > 0 ){
            double output_current = current;
            capacityModel->updateCapacity(output_current, 1);
            voltageModel->updateVoltage(capacityModel->q0(), capacityModel->qmax(), capacityModel->I(), 0, 1);
            output_power += capacityModel->I() * voltageModel->battery_voltage();
            n_t += 1;
        }
//        printf("current %f, eff %f, n %d\n", current, -output_power/input_power, n_t);
        current += fabs(max_current) / 100.;
    }
}

TEST_F(lib_battery_test, HourlyVsSubHourly)
{
    auto cap_hourly = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min, dtHour);
    auto volt_hourly = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom, C_rate, resistance, 1);

    auto cap_subhourly = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min, dtHour);
    auto volt_subhourly = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom, C_rate, resistance, .5);

    EXPECT_EQ(cap_hourly->q0(), cap_subhourly->q0());
    EXPECT_EQ(volt_hourly->battery_voltage(), volt_subhourly->battery_voltage());

    double discharge_watts = 100.;
    while (cap_hourly->SOC() > 16){
        double I_hourly = volt_hourly->calculate_current_for_target_w(discharge_watts, cap_hourly->q0(), cap_hourly->qmax(), 0);
        cap_hourly->updateCapacity(I_hourly, 1);
        volt_hourly->updateVoltage(cap_hourly->q0(), cap_hourly->qmax(), cap_hourly->I(), 0, 1);
        EXPECT_NEAR(cap_hourly->I() * volt_hourly->battery_voltage(), discharge_watts, 0.1);

        double I_subhourly = volt_subhourly->calculate_current_for_target_w(discharge_watts, cap_subhourly->q0(), cap_subhourly->qmax(), 0);
        cap_subhourly->updateCapacity(I_subhourly, 0.5);
        volt_subhourly->updateVoltage(cap_subhourly->q0(), cap_subhourly->qmax(), cap_subhourly->I(), 0, 0.5);
        EXPECT_NEAR(cap_subhourly->I() * volt_subhourly->battery_voltage(), discharge_watts, 0.1);

    }
}
