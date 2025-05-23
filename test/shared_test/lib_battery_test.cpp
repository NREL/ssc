/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#include <cmath>
#include <gtest/gtest.h>

#include "logger.h"
#include "lib_battery_test.h"

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
    s = thermal_state({93.49, 16.87, 16.85, 0.00017});
    compareState(model->get_state(), s, "updateTemperatureTest: 2");

    I = 50;
    model->updateTemperature(I, idx++);
    s = thermal_state({94.02, 17.51, 21.85, -0.17533});
    compareState(model->get_state(), s, "updateTemperatureTest: 3");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({94.88, 18.58, 21.85, -0.13172});
    compareState(model->get_state(), s, "updateTemperatureTest: 4");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({95.00, 18.76, 16.85, 0.07658});
    compareState(model->get_state(), s, "updateTemperatureTest: 5");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({92.55, 15.69, -3.15, 0.75990});
    compareState(model->get_state(), s, "updateTemperatureTest: 6");

    I = 100;
    model->updateTemperature(I, idx++);
    s = thermal_state({88.80, 11.01, -3.15, 0.5714});
    compareState(model->get_state(), s, "updateTemperatureTest: 7");
}

TEST_F(lib_battery_thermal_test, updateTemperatureTestSubMinute) {
    CreateModelSixSecondStep(Cp);
    // battery which adjusts quickly to temp {16.85, 16.85, 21.85, 21.85, 16.85, -3.15, -3.15};
    double I = 50;
    size_t idx = 0;
    double avgTemp = 0;
    for (size_t j = 0; j < 600; j++)
    {
        model->updateTemperature(I, idx);
        avgTemp += model->get_state().T_batt;
    }
    avgTemp /= 600.0;
    EXPECT_NEAR(avgTemp, 16.86, 0.02) << "updateTemperatureTest: 1";
    auto s = thermal_state({ 93.49, 16.86, 16.85 });
    compareState(model->get_state(), s, "updateTemperatureTest: 1");

    I = -50;
    idx++;
    avgTemp = 0;
    for (size_t j = 0; j < 600; j++)
    {
        model->updateTemperature(I, idx);
        avgTemp += model->get_state().T_batt;
    }
    avgTemp /= 600.0;
    EXPECT_NEAR(avgTemp, 16.87, 0.02) << "updateTemperatureTest: 2";
    s = thermal_state({ 93.49, 16.87, 16.85 });
    compareState(model->get_state(), s, "updateTemperatureTest: 2");

    I = 50;
    idx++;
    avgTemp = 0;
    for (size_t j = 0; j < 600; j++)
    {
        model->updateTemperature(I, idx);
        avgTemp += model->get_state().T_batt;
    }
    avgTemp /= 600.0;
    EXPECT_NEAR(avgTemp, 17.51, 0.02) << "updateTemperatureTest: 3";
    s = thermal_state({ 94.47, 18.09, 21.85, -0.1514});
    compareState(model->get_state(), s, "updateTemperatureTest: 3");

    I = 10;
    idx++;
    avgTemp = 0;
    for (size_t j = 0; j < 600; j++)
    {
        model->updateTemperature(I, idx);
        avgTemp += model->get_state().T_batt;
    }
    avgTemp /= 600.0;
    EXPECT_NEAR(avgTemp, 18.59, 0.02) << "updateTemperatureTest: 4";
    s = thermal_state({ 95.22, 19.03, 21.85, -0.1138});
    compareState(model->get_state(), s, "updateTemperatureTest: 4");

    I = 10;
    idx++;
    avgTemp = 0;
    for (size_t j = 0; j < 600; j++)
    {
        model->updateTemperature(I, idx);
        avgTemp += model->get_state().T_batt;
    }
    avgTemp /= 600.0;
    EXPECT_NEAR(avgTemp, 18.74, 0.02) << "updateTemperatureTest: 5";
    s = thermal_state({ 94.79, 18.49, 16.85, 0.06618});
    compareState(model->get_state(), s, "updateTemperatureTest: 5");

    I = 10;
    idx++;
    avgTemp = 0;
    for (size_t j = 0; j < 600; j++)
    {
        model->updateTemperature(I, idx);
        avgTemp += model->get_state().T_batt;
    }
    avgTemp /= 600.0;
    EXPECT_NEAR(avgTemp, 15.69, 0.02) << "updateTemperatureTest: 6";
    s = thermal_state({ 90.49, 13.12, -3.15, 0.6567});
    compareState(model->get_state(), s, "updateTemperatureTest: 6");

    I = 100;
    idx++;
    avgTemp = 0;
    for (size_t j = 0; j < 600; j++)
    {
        model->updateTemperature(I, idx);
        avgTemp += model->get_state().T_batt;
    }
    avgTemp /= 600.0;
    EXPECT_NEAR(avgTemp, 11.01, 0.02) << "updateTemperatureTest: 7";
    s = thermal_state({ 87.27, 9.09, -3.15, 0.4941});
    compareState(model->get_state(), s, "updateTemperatureTest: 7");
}

TEST_F(lib_battery_losses_test, MonthlyLossesTest){
    model = std::unique_ptr<losses_t>(new losses_t(chargingLosses, dischargingLosses, chargingLosses, adjustLosses));

    // losses for charging and idling are the same
    int charge_mode = capacity_state::CHARGE;

    size_t idx = 0;
    double dt_hr = 1;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getAncillaryLoss(), 0, tol) << "MonthlyLossesTest: 1";

    idx = 40 * 24;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getAncillaryLoss(), 1, tol) << "MonthlyLossesTest: 2";

    idx = 70 * 24;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getAncillaryLoss(), 2, tol) << "MonthlyLossesTest: 3";

    // discharging
    charge_mode = capacity_state::DISCHARGE;

    idx = 0;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getAncillaryLoss(), 1, tol) << "MonthlyLossesTest: 4";

    idx = 40 * 24;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getAncillaryLoss(), 2, tol) << "MonthlyLossesTest: 5";

    idx = 70 * 24;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getAncillaryLoss(), 3, tol) << "MonthlyLossesTest: 6";

}

TEST_F(lib_battery_losses_test, TimeSeriesLossesTest){
    model = std::unique_ptr<losses_t>(new losses_t(fullLosses));

    int charge_mode = -1;       // not used - still need to test charge/discharge/idle loss projections since those will be used in dispatch
    double dt_hr = 1;

    size_t idx = 0;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getAncillaryLoss(), 0, tol) << "TimeSeriesLossesTest: 1";

    idx = 40;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getAncillaryLoss(), 40./8760, tol) << "TimeSeriesLossesTest: 2";

    idx = 70;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getAncillaryLoss(), 70./8760, tol) << "TimeSeriesLossesTest: 3";

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

    auto s = battery_state_test(lifetime_params::CALCYC);
    s.capacity = {479.75, 1000, 960.01, 20.25, 0, 49.97, 52.09, 0.0, 0.0, 2};
    s.batt_voltage = 552.03;
    s.lifetime.calendar->q_relative_calendar = 102;
    s.lifetime.cycle->q_relative_cycle = 100;
    s.lifetime.cycle->rainflow_jlt = 1;
    s.lifetime.q_relative = 100;
    s.thermal = {96.00, 20.00, 20};
    compareState(batteryModel, s, "runTestCycleAt1C: 1");

    while (batteryModel->SOC() > SOC_min + 1){
        batteryModel->run(idx++, I);
        capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
    }
//    std::cerr <<  idx << ": soc " << batteryModel->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s.capacity = {54.5, 1000, 960.01, 20.25, 0, 5.67, 7.79, 0.0, 0.0, 2};
    s.batt_voltage = 470.17;
    s.lifetime.day_age_of_battery = 0.875;
    s.lifetime.q_relative = 100;
    s.lifetime.cycle->q_relative_cycle = 100;
    s.lifetime.calendar->q_relative_calendar = 101.976;
    s.lifetime.calendar->dq_relative_calendar_old = 0.0002;
    s.thermal = {96.01, 20.01, 20};
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
    s.capacity = { 45.00, 900.219, 864.213, 8.753, 0, 5.207, 6.220, 0.0, 0.0, 2};
    s.batt_voltage = 465.943;
    s.lifetime.q_relative = 93.08;
    s.lifetime.cycle->q_relative_cycle = 92.03;
    s.lifetime.n_cycles = 399;
    s.lifetime.cycle_range = 89.10;
    s.lifetime.average_range = 88.91;
    s.lifetime.cycle->rainflow_Xlt = 89.13;
    s.lifetime.cycle->rainflow_Ylt = 89.79;
    s.lifetime.cycle->rainflow_jlt = 3;
    s.lifetime.day_age_of_battery = 2765.96;
    s.lifetime.calendar->q_relative_calendar = 98.0;
    s.lifetime.calendar->dq_relative_calendar_old = 0.039;
    s.thermal = {96.0, 20.00, 20};
    s.last_idx = 32991;

    compareState(batteryModel, s, "runTestCycleAt1C: 3");

    EXPECT_NEAR(capacity_passed, 354541, 1000) << "Current passing through cell";
    double qmax = fmax(s.capacity.qmax_lifetime, s.capacity.qmax_thermal);
    EXPECT_NEAR(qmax/q, .90, 0.01) << "capacity relative to max capacity";
}

TEST_F(lib_battery_test, runTestCycleAt3C){
    size_t idx = 0;
    double capacity_passed = 0.;
    double I = Qfull * n_strings * 3;
    batteryModel->run(idx++, I);
    capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
//    std::cerr << "\n" << idx << ": " << capacity_passed << "\n";

    auto s = battery_state_test(lifetime_params::CALCYC);
    s.capacity = {439.25, 1000, 960.02, 60.75, 0, 45.75, 52.08, 0.0, 0.0, 2};
    s.batt_voltage = 550.10;
    s.lifetime.q_relative = 100;
    s.lifetime.cycle->q_relative_cycle = 100;
    s.lifetime.cycle->rainflow_jlt = 1;
    s.lifetime.calendar->q_relative_calendar = 102;
    s.thermal = {96.01, 20.01, 20};
    s.last_idx = 0;
    compareState(batteryModel, s, "runTest: 1");

    while (batteryModel->SOC() > SOC_min + 1){
        batteryModel->run(idx++, I);
        capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
    }
//    std::cerr <<  idx << ": soc " << batteryModel->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s.capacity = {48.01, 1000, 960.11, 26.74, 0, 5.00, 7.78, 0.0, 0.0, 2};
    s.batt_voltage = 463.93;
    s.lifetime.day_age_of_battery = 0.29;
    s.lifetime.q_relative = 101.98;
    s.lifetime.calendar->q_relative_calendar = 101.98;
    s.last_idx = 0;
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
    s.capacity = { 47.12, 901.56, 865.505, 8.87, 0, 5.444, 6.47, 0.0, 0.0, 2};
    s.batt_voltage = 468.126;
    s.lifetime.q_relative = 90.16;
    s.lifetime.day_age_of_battery = 2587.83;
    s.lifetime.cycle->q_relative_cycle = 92.08;
    s.lifetime.n_cycles = 399;
    s.lifetime.cycle_range = 89.12;
    s.lifetime.average_range = 89.18;
    s.lifetime.cycle->rainflow_Xlt = 89.15;
    s.lifetime.cycle->rainflow_Ylt = 89.56;
    s.lifetime.cycle->rainflow_jlt = 3;
    s.lifetime.cycle->q_relative_cycle = 92.04;
    s.lifetime.calendar->q_relative_calendar = 98.13;
    s.lifetime.calendar->dq_relative_calendar_old = 0.0393;
    s.thermal = {96.01, 20, 20};
    s.last_idx = 32991;
    compareState(batteryModel, s, "runTest: 3", 0.1);

    EXPECT_NEAR(capacity_passed, 355949, 100) << "Current passing through cell";
    double qmax = fmax(s.capacity.qmax_lifetime, s.capacity.qmax_thermal);
    EXPECT_NEAR(qmax/q, 0.9016, 0.01) << "capacity relative to max capacity";
}

TEST_F(lib_battery_test, runDuplicates) {
    // get initial state
    auto state = batteryModel->get_state();
    auto cap_state = state.capacity;
    auto volt_state = state.voltage;

    // create a duplicate and discharge it
    auto Battery = new battery_t(*batteryModel);
    double I = 10;
    Battery->run(0, I);

    // get state of initial battery
    auto state2 = batteryModel->get_state();
    auto cap_state2 = state2.capacity;
    auto volt_state2 = state2.voltage;

    // get state of duplicate battery
    auto state3 = Battery->get_state();
    auto cap_state3 = state3.capacity;
    auto volt_state3 = state3.voltage;

    EXPECT_FALSE(*cap_state3 == *cap_state2);
    EXPECT_NE(volt_state3->cell_voltage, volt_state2->cell_voltage);
    
    delete Battery;
}

TEST_F(lib_battery_test, createFromParams) {
    auto params = std::make_shared<battery_params>(batteryModel->get_params());
    auto bat = battery_t(params);

    double current = 10;
    double P_orig = batteryModel->run(0, current);

    double P_new = bat.run(0, current);

    EXPECT_EQ(P_orig, P_new);
    EXPECT_EQ(batteryModel->I(), bat.I());
    EXPECT_EQ(batteryModel->charge_maximum(), bat.charge_maximum());
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

    std::vector<double> eff_vs_current;
    double current = std::abs(max_current) * 0.01;
    while (current < std::abs(max_current)){
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
        current += std::abs(max_current) / 100.;
        eff_vs_current.emplace_back(std::abs(output_power/input_power));
    }
    std::vector<double> eff_expected = { 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.98, 0.98, 0.98, 0.98, // i = 12
                                        0.98, 0.98, 0.98, 0.98, 0.97, 0.97, 0.97, 0.97, 0.97, 0.97, 0.96, 0.96, 0.96, // i = 25
                                        0.96, 0.96, 0.96, 0.96, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, // i = 38
                                        0.95, 0.95, 0.94, 0.93, 0.94, 0.94, 0.94, 0.94, 0.94, 0.94, 0.94, 0.94, 0.93, // i = 51
                                        0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.93, 0.93, 0.93, 0.93, 0.93, 0.93, // i = 64
                                        0.93, 0.93, 0.92, 0.92, 0.91, 0.89, 0.88, 0.89, 0.89, 0.89, 0.89, 0.89, 0.90, // i = 77
                                        0.90, 0.90, 0.90, 0.90, 0.91, 0.91, 0.91, 0.91, 0.91, 0.91, 0.91, 0.91, 0.92, // i = 90
                                        0.92, 0.92, 0.92, 0.92, 0.91, 0.91, 0.91, 0.91, // i = 98
    };
    for (size_t i = 0; i < eff_expected.size(); i++)
        EXPECT_NEAR(eff_vs_current[i], eff_expected[i], .01) << " i = " << i;
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

    std::vector<double> eff_vs_current;
    double current = std::abs(max_current) * 0.01;
    while (current < std::abs(max_current)){
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
        current += std::abs(max_current) / 100.;
        eff_vs_current.emplace_back(std::abs(output_power/input_power));
    }

    std::vector<double> eff_expected = {0.99, 0.99, 0.98, 0.98, 0.97, 0.96, 0.96, 0.95, 0.95, 0.94, 0.93, 0.94, 0.93,
                                        0.92, 0.92, 0.92, 0.91, 0.90, 0.90, 0.89, 0.88, 0.87, 0.88, 0.87, 0.86, 0.87,
                                        0.86, 0.84, 0.86, 0.87, 0.85, 0.83, 0.81, 0.84, 0.85, 0.86, 0.84, 0.82, 0.79,
                                        0.78, 0.80, 0.82, 0.84, 0.85, 0.85, 0.82, 0.80, 0.77, 0.74, 0.73, 0.74, 0.76,
                                        0.77, 0.78, 0.79, 0.81, 0.82, 0.83, 0.84, 0.83, 0.80, 0.77, 0.74, 0.71, 0.67,
                                        0.64, 0.65, 0.65, 0.66, 0.67, 0.68, 0.69, 0.70, 0.71, 0.71, 0.72, 0.73, 0.74,
                                        0.75, 0.75, 0.76, 0.77, 0.78, 0.79, 0.79, 0.80, 0.81, 0.82, 0.82, 0.79, 0.76,
                                        0.73, 0.69, 0.66, 0.62, 0.59, 0.55, 0.51, 0.47};
    for (size_t i = 0; i < eff_expected.size(); i++)
        EXPECT_NEAR(eff_vs_current[i], eff_expected[i], .01);
    
    delete capacityModel;
    delete voltageModel;
}

TEST_F(lib_battery_test, RoundtripEffVanadiumFlow){
    auto vol = new voltage_vanadium_redox_t(1, 1, 1.41, 0.001, dtHour);
    auto cap = new capacity_lithium_ion_t(11, 30, 100, 0, dtHour);

    cap->change_SOC_limits(0, 100);


    double full_current = 1000;
    double max_current;
    vol->calculate_max_charge_w(cap->q0(), cap->qmax(), 300, &max_current);

    double current = std::abs(max_current) * 0.01;
    while (current < std::abs(max_current)){
        cap->updateCapacity(full_current, 1);   //discharge to empty

        std::vector<double> inputs, outputs;

        size_t n_t = 0;
        current *= -1;
        double input_power = 0.;
        while(cap->SOC() < 100 ){
            double input_current = current;
            cap->updateCapacity(input_current, 1);
            vol->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 300, 1);
            input_power += cap->I() * vol->battery_voltage();
            n_t += 1;
            inputs.push_back(vol->battery_voltage());
        }

        current *= -1;
        double output_power = 0.;
        while(vol->calculate_max_discharge_w(cap->q0(), cap->qmax(), 300, nullptr) > 0 ){
            double output_current = current;
            cap->updateCapacity(output_current, 1);
            vol->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 300, 1);
            output_power += cap->I() * vol->battery_voltage();
            n_t += 1;
            outputs.push_back(vol->battery_voltage());
        }

//        std::reverse(outputs.begin(), outputs.end());
//        for (size_t i = 0; i < inputs.size(); i++) {
//            printf("%f, %f\n", inputs[i], outputs[i]);
//        }
//        printf("current %f, eff %f, n %zd\n", current, -output_power/input_power, n_t);

        current += std::abs(max_current) / 100.;
    }
    delete vol;
    delete cap;
}

TEST_F(lib_battery_test, HourlyVsSubHourly)
{
    n_strings = 444;
    auto cap_hourly = new capacity_lithium_ion_t(Qfull * n_strings, SOC_init, SOC_max, SOC_min, dtHour);
    auto volt_hourly = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom, Vcut,
                                             C_rate, resistance, 1 );

    auto cap_subhourly = new capacity_lithium_ion_t(Qfull * n_strings, SOC_init, SOC_max, SOC_min, dtHour);
    auto volt_subhourly = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom, Vcut,
                                                C_rate, resistance, .5 );

    EXPECT_EQ(cap_hourly->q0(), cap_subhourly->q0());
    EXPECT_EQ(volt_hourly->battery_voltage(), volt_subhourly->battery_voltage());

    double discharge_watts = 100.22;
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
    delete cap_hourly;
    delete volt_hourly;
    delete cap_subhourly;
    delete volt_subhourly;
}

TEST_F(lib_battery_test, AdaptiveTimestep) {
    size_t steps_per_hour = 4;
    auto batt_subhourly = new battery_t(*batteryModel);
    batt_subhourly->ChangeTimestep(1. / (double)steps_per_hour);
    auto batt_adaptive = new battery_t(*batt_subhourly);

    EXPECT_EQ(batt_adaptive->charge_total(), batteryModel->charge_total());
    EXPECT_EQ(batt_adaptive->charge_maximum(), batteryModel->charge_maximum());
    EXPECT_EQ(batt_adaptive->V(), batteryModel->V());
    EXPECT_EQ(batt_adaptive->I(), batteryModel->I());
    ASSERT_NEAR(batt_subhourly->get_params().lifetime->dt_hr, 0.25, 1e-3);

    double kw_hourly = 100.;
    size_t count = 0;
    while (count < 2000){
        double hourly_E = 0;
        double subhourly_E = 0;
        double adaptive_E = 0;
        while (batteryModel->SOC() > 15) {
            // run hourly
            batteryModel->runPower(kw_hourly);
            hourly_E += batteryModel->get_state().P;
            ASSERT_NEAR(batt_subhourly->get_params().lifetime->dt_hr, 0.25, 1e-3);

            // run subhourly
            for (size_t i = 0; i < steps_per_hour; i++) {
                batt_subhourly->runPower(kw_hourly);
                subhourly_E += batt_subhourly->get_state().P / (double)steps_per_hour;
            }

            // run adaptive
            if (count % 2 == 0) {
                batt_adaptive->ChangeTimestep(1);
                batt_adaptive->runPower(kw_hourly);
                adaptive_E += batt_adaptive->get_state().P;
            }
            else {
                batt_adaptive->ChangeTimestep(1. / (double)steps_per_hour);
                for (size_t i = 0; i < steps_per_hour; i++) {
                    batt_adaptive->runPower(kw_hourly);
                    adaptive_E += batt_adaptive->get_state().P / (double)steps_per_hour;
                }
            }
            EXPECT_NEAR(batteryModel->get_state().lifetime->day_age_of_battery,
                        batt_subhourly->get_state().lifetime->day_age_of_battery, 1e-3);
            EXPECT_NEAR(batteryModel->get_state().lifetime->day_age_of_battery,
                        batt_adaptive->get_state().lifetime->day_age_of_battery, 1e-3);
        }
        while (batteryModel->SOC() < 85) {
            batteryModel->runPower(-kw_hourly);
            hourly_E -= batteryModel->get_state().P;

            for (size_t i = 0; i < steps_per_hour; i++) {
                batt_subhourly->runPower(-kw_hourly);
                subhourly_E -= batt_subhourly->get_state().P / (double)steps_per_hour;

            }
            if (count % 2 == 0) {
                batt_adaptive->ChangeTimestep(1);
                batt_adaptive->runPower(-kw_hourly);
                adaptive_E -= batt_adaptive->get_state().P;
            }
            else {
                batt_adaptive->ChangeTimestep(1. / (double)steps_per_hour);
                for (size_t i = 0; i < steps_per_hour; i++) {
                    batt_adaptive->runPower(-kw_hourly);
                    adaptive_E -= batt_adaptive->get_state().P / (double)steps_per_hour;
                }
            }
        }
        count++;

        // max energy throughput error is about 10% from hourly runs and 15% from 15 min runs
        EXPECT_NEAR(hourly_E, adaptive_E, hourly_E * 0.10) << "At count " <<  count;
        EXPECT_NEAR(subhourly_E, adaptive_E, subhourly_E * 0.15) << "At count " << count;

        // max lifetime degradation error is about 15, out of charge max of ~600 -> 20 / 600 = 3.3 % error
        EXPECT_NEAR(batteryModel->charge_maximum(), batt_adaptive->charge_maximum(), 20) << "At count " <<  count;
        EXPECT_NEAR(batt_subhourly->charge_maximum(), batt_adaptive->charge_maximum(), 20) << "At count " << count;

    }

    EXPECT_NEAR(batteryModel->charge_maximum(), 576.95, 1e-2);
    EXPECT_NEAR(batt_subhourly->charge_maximum(), 557.69, 1e-2);
    EXPECT_NEAR(batt_adaptive->charge_maximum(), 577.26, 1e-2);

    EXPECT_NEAR(batteryModel->SOC(), 94.98, 1e-2);
    EXPECT_NEAR(batt_subhourly->SOC(), 94.95, 1e-2);
    EXPECT_NEAR(batt_adaptive->SOC(), 94.95, 1e-2);
    
    delete batt_adaptive;
    delete batt_subhourly;
}


TEST_F(lib_battery_test, AugmentCapacity) {
    std::vector<double> augmentation_percent = {50, 40, 30};
    batteryModel->setupReplacements(augmentation_percent);

    // Correct future approach for augmenting batteries, by treating as separate entities
    std::vector<battery_t *> batteries;
    batteries.push_back(new battery_t(*batteryModel));

    batteries.push_back(new battery_t(*batteryModel));
    batteries[1]->setupReplacements(augmentation_percent);

    batteries.push_back(new battery_t(*batteryModel));
    batteries[2]->setupReplacements(augmentation_percent);

    size_t i = 0;
    double I = 100;
    double mult = 1.0;
    size_t replaceCount = 0;
    for (size_t y = 0; y < augmentation_percent.size(); y++) {
        for (size_t t = 0; t < 8760; t++) {
            mult = fmod(t, 2) == 0 ? 1 : -1;
            double current = mult * I;
            batteries[replaceCount]->runReplacement(y, t, 0);
            batteries[replaceCount]->run(i, current);
        }
        if (augmentation_percent[y] > 0) {
            replaceCount++;
        }
    }

    EXPECT_EQ(batteries[0]->getNumReplacementYear(), 0);
    EXPECT_EQ(batteries[1]->getNumReplacementYear(), 1);
    EXPECT_EQ(batteries[2]->getNumReplacementYear(), 1);

    for (auto i : batteries)
        delete i;
}

TEST_F(lib_battery_test, ReplaceByCapacityTest){
    batteryModel->setupReplacements(91);

    size_t idx = 0;
    double I = Qfull * n_strings * 2;
    while (idx < 100000) {
        batteryModel->run(idx, I);
        batteryModel->runReplacement(0, idx, 0);
        idx ++;
        I = -Qfull * n_strings * 2;
        batteryModel->run(idx, I);
        batteryModel->runReplacement(0, idx, 0);
        idx ++;
    }
    double rep = batteryModel->getNumReplacementYear();
    EXPECT_EQ(rep, 1);
}

TEST_F(lib_battery_test, NMCLifeModel) {
    auto lifetimeModelNMC = new lifetime_nmc_t(dtHour);
    auto thermalModelNMC = new thermal_t(dtHour, mass, surface_area, resistance, Cp, h, T_room);
    auto capacityModelNMC = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min, dtHour);
    auto voltageModelNMC = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom, Vcut,
                                         C_rate, resistance, dtHour);
    auto lossModelNMC = new losses_t(monthlyLosses, monthlyLosses, monthlyLosses, adjustLosses);

    auto batteryNMC = std::unique_ptr<battery_t>(new battery_t(dtHour, chemistry, capacityModelNMC, voltageModelNMC, lifetimeModelNMC, thermalModelNMC, lossModelNMC));
    double I = Qfull * n_strings * 2;

    batteryNMC->run(0, I);

    auto state = batteryNMC->get_state().lifetime;

    EXPECT_NEAR(state->q_relative, 100, 1e-3);
    EXPECT_NEAR(state->n_cycles, 0, 1e-3);
    EXPECT_NEAR(state->cycle_range, 0, 1e-3);
    EXPECT_NEAR(state->average_range, 0, 1e-3);
    EXPECT_NEAR(state->nmc_li_neg->q_relative_li, 100, 1e-3);
    EXPECT_NEAR(state->nmc_li_neg->q_relative_neg, 100, 1e-3);
    EXPECT_NEAR(state->nmc_li_neg->dq_relative_neg, 0, 1e-3);
    EXPECT_NEAR(state->nmc_li_neg->dq_relative_li1, 0, 1e-3);
    EXPECT_NEAR(state->nmc_li_neg->dq_relative_li2, 0, 1e-3);
    EXPECT_NEAR(state->nmc_li_neg->dq_relative_li3, 0, 1e-3);
    EXPECT_NEAR(state->cycle->DOD_min, 0.5, 1e-3);
    EXPECT_NEAR(state->cycle->DOD_max, 0.54, 1e-3);
    EXPECT_NEAR(state->nmc_li_neg->b1_dt, 1.143e-4, 1e-6);
    EXPECT_NEAR(state->nmc_li_neg->b2_dt, 0.05593, 1e-3);
    EXPECT_NEAR(state->nmc_li_neg->b3_dt, 8.829e-4, 1e-7);
    EXPECT_NEAR(state->nmc_li_neg->c0_dt, 3.105, 1e-3);
    EXPECT_NEAR(state->nmc_li_neg->c2_dt, 3.035e-6, 1e-8);
    

}

TEST_F(lib_battery_test, AdaptiveTimestepNMC) {
    auto lifetimeModelNMC = new lifetime_nmc_t(dtHour);
    auto thermalModelNMC = new thermal_t(dtHour, mass, surface_area, resistance, Cp, h, T_room);
    auto capacityModelNMC = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min, dtHour);
    auto voltageModelNMC = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom,
                                                 Vcut, C_rate, resistance, dtHour);
    auto lossModelNMC = new losses_t(monthlyLosses, monthlyLosses, monthlyLosses, adjustLosses);

    batteryModel = std::unique_ptr<battery_t>(new battery_t(dtHour, chemistry, capacityModelNMC, voltageModelNMC, lifetimeModelNMC, thermalModelNMC, lossModelNMC));

    size_t steps_per_hour = 4;
    auto batt_subhourly = new battery_t(*batteryModel);
    batt_subhourly->ChangeTimestep(1. / (double)steps_per_hour);
    auto batt_adaptive = new battery_t(*batt_subhourly);

    EXPECT_EQ(batt_adaptive->charge_total(), batteryModel->charge_total());
    EXPECT_EQ(batt_adaptive->charge_maximum(), batteryModel->charge_maximum());
    EXPECT_EQ(batt_adaptive->V(), batteryModel->V());
    EXPECT_EQ(batt_adaptive->I(), batteryModel->I());
    ASSERT_NEAR(batt_subhourly->get_params().lifetime->dt_hr, 0.25, 1e-3);

    double kw_hourly = 100.;
    size_t count = 0;
    while (count < 2000){
        double hourly_E = 0;
        double subhourly_E = 0;
        double adaptive_E = 0;
        while (batteryModel->SOC() > 15) {
            // run hourly
            batteryModel->runPower(kw_hourly);
            hourly_E += batteryModel->get_state().P;
            // run subhourly
            for (size_t i = 0; i < steps_per_hour; i++) {
                batt_subhourly->runPower(kw_hourly);
                subhourly_E += batt_subhourly->get_state().P / (double)steps_per_hour;
            }

            // run adaptive
            if (count % 2 == 0) {
                batt_adaptive->ChangeTimestep(1);
                batt_adaptive->runPower(kw_hourly);
                adaptive_E += batt_adaptive->get_state().P;
            }
            else {
                batt_adaptive->ChangeTimestep(1. / (double)steps_per_hour);
                for (size_t i = 0; i < steps_per_hour; i++) {
                    batt_adaptive->runPower(kw_hourly);
                    adaptive_E += batt_adaptive->get_state().P / (double)steps_per_hour;
                }
            }
            EXPECT_NEAR(batteryModel->get_state().lifetime->day_age_of_battery,
                        batt_subhourly->get_state().lifetime->day_age_of_battery, 1e-3);
            EXPECT_NEAR(batteryModel->get_state().lifetime->day_age_of_battery,
                        batt_adaptive->get_state().lifetime->day_age_of_battery, 1e-3);
        }
        while (batteryModel->SOC() < 85) {
            batteryModel->runPower(-kw_hourly);
            hourly_E -= batteryModel->get_state().P;

            for (size_t i = 0; i < steps_per_hour; i++) {
                batt_subhourly->runPower(-kw_hourly);
                subhourly_E -= batt_subhourly->get_state().P / (double)steps_per_hour;

            }
            if (count % 2 == 0) {
                batt_adaptive->ChangeTimestep(1);
                batt_adaptive->runPower(-kw_hourly);
                adaptive_E -= batt_adaptive->get_state().P;
            }
            else {
                batt_adaptive->ChangeTimestep(1. / (double)steps_per_hour);
                for (size_t i = 0; i < steps_per_hour; i++) {
                    batt_adaptive->runPower(-kw_hourly);
                    adaptive_E -= batt_adaptive->get_state().P / (double)steps_per_hour;
                }
            }
        }
        count++;

        // max energy throughput error is about 10% from hourly runs and 15% from 15 min runs
        ASSERT_NEAR(hourly_E, adaptive_E, hourly_E * 0.10) << "At count " <<  count;
        ASSERT_NEAR(subhourly_E, adaptive_E, subhourly_E * 0.15) << "At count " << count;

        // max lifetime degradation error is about 15, out of charge max of ~600 -> 20 / 600 = 3.3 % error
        ASSERT_NEAR(batteryModel->charge_maximum(), batt_adaptive->charge_maximum(), 20) << "At count " <<  count;
        ASSERT_NEAR(batt_subhourly->charge_maximum(), batt_adaptive->charge_maximum(), 20) << "At count " << count;

    }

    EXPECT_NEAR(batteryModel->charge_maximum(), 900.63, 1e-2);
    EXPECT_NEAR(batt_subhourly->charge_maximum(), 904.56, 1e-2);
    EXPECT_NEAR(batt_adaptive->charge_maximum(), 903.19, 1e-2);

    EXPECT_NEAR(batteryModel->SOC(), 85.46, 1e-2);
    EXPECT_NEAR(batt_subhourly->SOC(), 85.94, 1e-2);
    EXPECT_NEAR(batt_adaptive->SOC(), 86.06, 1e-2);

    delete batt_adaptive;
    delete batt_subhourly;
}

TEST_F(lib_battery_test, AdaptiveTimestepNonIntegerStep) {
    auto lifetimeModelNMC = new lifetime_nmc_t(dtHour);
    auto thermalModelNMC = new thermal_t(dtHour, mass, surface_area, resistance, Cp, h, T_room);
    auto capacityModelNMC = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min, dtHour);
    auto voltageModelNMC = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom, 0,
                                                 C_rate, resistance, dtHour);
    auto lossModelNMC = new losses_t(monthlyLosses, monthlyLosses, monthlyLosses, adjustLosses);

    auto batt_adaptive = std::unique_ptr<battery_t>(new battery_t(dtHour, chemistry, capacityModelNMC, voltageModelNMC, lifetimeModelNMC, thermalModelNMC, lossModelNMC));

    double dt_hr = 0.5;
    batt_adaptive->ChangeTimestep(dt_hr);
    batt_adaptive->runPower(100);

    EXPECT_ANY_THROW(batt_adaptive->ChangeTimestep(1));
    

}

TEST_F(lib_battery_test, LMOLTOLifeModel) {
    auto lifetimeModelNMC = new lifetime_lmolto_t(dtHour);
    auto thermalModelNMC = new thermal_t(dtHour, mass, surface_area, resistance, Cp, h, T_room);
    auto capacityModelNMC = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min, dtHour);
    auto voltageModelNMC = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom, Vcut,
                                                 C_rate, resistance, dtHour);
    auto lossModelNMC = new losses_t(monthlyLosses, monthlyLosses, monthlyLosses, adjustLosses);

    auto batteryNMC = std::unique_ptr<battery_t>(new battery_t(dtHour, chemistry, capacityModelNMC, voltageModelNMC, lifetimeModelNMC, thermalModelNMC, lossModelNMC));
    double I = Qfull * n_strings * 2;

    batteryNMC->run(0, I);

    auto state = batteryNMC->get_state().lifetime;

    EXPECT_NEAR(state->q_relative, 100, 1e-3);
    EXPECT_NEAR(state->n_cycles, 0, 1e-3);
    EXPECT_NEAR(state->cycle_range, 0, 1e-3);
    EXPECT_NEAR(state->average_range, 0, 1e-3);
    EXPECT_NEAR(state->cycle->DOD_min, 0.5, 1e-3);
    EXPECT_NEAR(state->cycle->DOD_max, 0.54, 1e-3);
    EXPECT_NEAR(state->lmo_lto->dq_relative_cal, 0, 1e-3);
    EXPECT_NEAR(state->lmo_lto->dq_relative_cyc, 0, 1e-3);
    EXPECT_NEAR(state->lmo_lto->temp_avg, 12.214, 1e-3);
    EXPECT_NEAR(state->lmo_lto->EFC, 0.0202, 1e-3);
    EXPECT_NEAR(state->lmo_lto->EFC_dt, 0.0202, 1e-3);
    

}

TEST_F(lib_battery_test, AdaptiveTimestepLMOLTO) {
    auto lifetimeModel = new lifetime_lmolto_t(dtHour);
    auto thermalModel = new thermal_t(dtHour, mass, surface_area, resistance, Cp, h, T_room);
    auto capacityModel = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min, dtHour);
    auto voltageModel = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom,
                                              Vcut, C_rate, resistance, dtHour);
    auto lossModel = new losses_t(monthlyLosses, monthlyLosses, monthlyLosses, adjustLosses);

    batteryModel = std::unique_ptr<battery_t>(new battery_t(dtHour, chemistry, capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel));

    size_t steps_per_hour = 4;
    auto batt_subhourly = new battery_t(*batteryModel);
    batt_subhourly->ChangeTimestep(1. / (double)steps_per_hour);
    auto batt_adaptive = new battery_t(*batt_subhourly);

    EXPECT_EQ(batt_adaptive->charge_total(), batteryModel->charge_total());
    EXPECT_EQ(batt_adaptive->charge_maximum(), batteryModel->charge_maximum());
    EXPECT_EQ(batt_adaptive->V(), batteryModel->V());
    EXPECT_EQ(batt_adaptive->I(), batteryModel->I());
    ASSERT_NEAR(batt_subhourly->get_params().lifetime->dt_hr, 0.25, 1e-3);

    double kw_hourly = 100.;
    size_t count = 0;
    while (count < 2000){
        double hourly_E = 0;
        double subhourly_E = 0;
        double adaptive_E = 0;
        while (batteryModel->SOC() > 15) {
            // run hourly
            batteryModel->runPower(kw_hourly);
            hourly_E += batteryModel->get_state().P;
            // run subhourly
            for (size_t i = 0; i < steps_per_hour; i++) {
                batt_subhourly->runPower(kw_hourly);
                subhourly_E += batt_subhourly->get_state().P / (double)steps_per_hour;
            }

            // run adaptive
            if (count % 2 == 0) {
                batt_adaptive->ChangeTimestep(1);
                batt_adaptive->runPower(kw_hourly);
                adaptive_E += batt_adaptive->get_state().P;
            }
            else {
                batt_adaptive->ChangeTimestep(1. / (double)steps_per_hour);
                for (size_t i = 0; i < steps_per_hour; i++) {
                    batt_adaptive->runPower(kw_hourly);
                    adaptive_E += batt_adaptive->get_state().P / (double)steps_per_hour;
                }
            }
            EXPECT_NEAR(batteryModel->get_state().lifetime->day_age_of_battery,
                        batt_subhourly->get_state().lifetime->day_age_of_battery, 1e-3);
            EXPECT_NEAR(batteryModel->get_state().lifetime->day_age_of_battery,
                        batt_adaptive->get_state().lifetime->day_age_of_battery, 1e-3);
        }
        while (batteryModel->SOC() < 85) {
            batteryModel->runPower(-kw_hourly);
            hourly_E -= batteryModel->get_state().P;

            for (size_t i = 0; i < steps_per_hour; i++) {
                batt_subhourly->runPower(-kw_hourly);
                subhourly_E -= batt_subhourly->get_state().P / (double)steps_per_hour;

            }
            if (count % 2 == 0) {
                batt_adaptive->ChangeTimestep(1);
                batt_adaptive->runPower(-kw_hourly);
                adaptive_E -= batt_adaptive->get_state().P;
            }
            else {
                batt_adaptive->ChangeTimestep(1. / (double)steps_per_hour);
                for (size_t i = 0; i < steps_per_hour; i++) {
                    batt_adaptive->runPower(-kw_hourly);
                    adaptive_E -= batt_adaptive->get_state().P / (double)steps_per_hour;
                }
            }
        }
        count++;

        ASSERT_NEAR(hourly_E, adaptive_E, hourly_E * 0.10) << "At count " <<  count;
        ASSERT_NEAR(subhourly_E, adaptive_E, subhourly_E * 0.15) << "At count " << count;

        ASSERT_NEAR(batteryModel->charge_maximum(), batt_adaptive->charge_maximum(), batteryModel->charge_maximum() * 0.11) << "At count " <<  count;
        ASSERT_NEAR(batt_subhourly->charge_maximum(), batt_adaptive->charge_maximum(), batt_subhourly->charge_maximum() * 0.1) << "At count " << count;

    }

    EXPECT_NEAR(batteryModel->charge_maximum(), 989.39, 1e-2);
    EXPECT_NEAR(batt_subhourly->charge_maximum(), 989.39, 1e-2);
    EXPECT_NEAR(batt_adaptive->charge_maximum(), 989.39, 1e-2);

    EXPECT_NEAR(batteryModel->SOC(), 95, 1e-2);
    EXPECT_NEAR(batt_subhourly->SOC(), 95, 1e-2);
    EXPECT_NEAR(batt_adaptive->SOC(), 95, 1e-2);
    
    delete batt_adaptive;
    delete batt_subhourly;
}

TEST_F(lib_battery_test, testCyclesWithAvailabilityLosses) {
    auto lifetimeModel = new lifetime_calendar_cycle_t(cycleLifeMatrix, dtHour, 1.02, 2.66e-3, -7280, 930);
    auto thermalModel = new thermal_t(dtHour, mass, surface_area, resistance, Cp, h, capacityVsTemperature, T_room);
    auto capacityModel = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min, dtHour);
    auto voltageModel = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom,
        Vcut, C_rate, resistance, dtHour);

    adjustLosses.clear();
    for (size_t i = 0; i < 8760; i++) {
        adjustLosses.push_back(0.5);
    }

    auto lossModel = new losses_t(monthlyLosses, monthlyLosses, monthlyLosses, adjustLosses);

    batteryModel = std::unique_ptr<battery_t>(new battery_t(dtHour, chemistry, capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel));


    size_t idx = 0;
    double capacity_passed = 0.;
    double I = Qfull * n_strings;
    batteryModel->run(idx++, I);
    capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
    //    std::cerr << "\n" << idx << ": " << capacity_passed << "\n";

    auto s = battery_state_test(lifetime_params::CALCYC);
    // 50% availabilty loss means half of SOC lost out of the gate
    s.capacity = { 229.75, 1000, 960.0, 20.25, 0, 23.93, 26.04, 0.5, 0.0, 2 };
    s.batt_voltage = 532.94;
    s.lifetime.calendar->q_relative_calendar = 102;
    s.lifetime.cycle->q_relative_cycle = 100;
    s.lifetime.cycle->rainflow_jlt = 1;
    s.lifetime.q_relative = 100;
    s.thermal = { 96.0, 20.00, 20 };
    compareState(batteryModel, s, "testCyclesWithAvailabilityLosses: 1");

    while (batteryModel->SOC() > SOC_min + 1) {
        batteryModel->run(idx++, I);
        capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
    }
    //    std::cerr <<  idx << ": soc " << batteryModel->SOC() << ", cap " << capacity_passed << "\n";
    // 
    s.capacity = { 47.5, 1000, 960.015, 20.25, 0, 4.94, 7.05, 0.5, 0.5, 2 };
    s.batt_voltage = 463.43;
    s.lifetime.day_age_of_battery = 0.375;
    s.lifetime.q_relative = 100;
    s.lifetime.cycle->q_relative_cycle = 100;
    s.lifetime.calendar->q_relative_calendar = 101.99;
    s.lifetime.calendar->dq_relative_calendar_old = 0.0002;
    s.thermal = { 96.01, 20.01, 20 };
    compareState(batteryModel, s, "testCyclesWithAvailabilityLosses: 2");

    size_t n_cycles = 400;

    while (n_cycles-- > 0) {
        I *= -1;
        while (batteryModel->SOC() < SOC_max * 0.5 - 1) {
            batteryModel->run(idx++, I);
            capacity_passed += -batteryModel->I() * batteryModel->V() / 1000.;
        }
        I *= -1;
        while (batteryModel->SOC() > SOC_min + 1) {
            batteryModel->run(idx++, I);
            capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
        }
    }
    //    std::cerr <<  idx << ": soc " << batteryModel->SOC() << ", cap " << capacity_passed << "\n";
    // Lower DOD cycles relative to runTestCycleAt1C means lower degradation after 400 cycles
    s.capacity = { 50.44, 960.60, 922.18, 9.31, 0, 5.47, 6.48, 0.5, 0.5, 2 };
    s.batt_voltage = 468.37;
    s.lifetime.q_relative = 93.08;
    s.lifetime.cycle->q_relative_cycle = 96.06;
    s.lifetime.n_cycles = 399;
    s.lifetime.cycle_range = 41.39;
    s.lifetime.average_range = 42.02;
    s.lifetime.cycle->rainflow_Xlt = 41.40;
    s.lifetime.cycle->rainflow_Ylt = 42.03;
    s.lifetime.cycle->rainflow_jlt = 3;
    s.lifetime.day_age_of_battery = 972.20;
    s.lifetime.calendar->q_relative_calendar = 101.25;
    s.lifetime.calendar->dq_relative_calendar_old = 0.007;
    s.thermal = { 96.0, 20.00, 20 };
    s.last_idx = 32991;

    compareState(batteryModel, s, "testCyclesWithAvailabilityLosses: 3");

    EXPECT_NEAR(capacity_passed, 167601, 1000) << "Current passing through cell";
    double qmax = fmax(s.capacity.qmax_lifetime, s.capacity.qmax_thermal);
    EXPECT_NEAR(qmax / q, 0.9606, 0.01) << "capacity relative to max capacity";
}
