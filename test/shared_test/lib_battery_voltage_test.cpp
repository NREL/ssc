#include <gtest/gtest.h>

//#include "lib_battery_capacity.h"
#include "lib_battery.h"
#include "lib_battery_voltage_test.h"

TEST_F(voltage_dynamic_lib_battery_voltage_test, SetUpTest) {
    CreateModel(1);

    EXPECT_EQ(model->cell_voltage(), 4.1);
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, NickelMetalHydrideFromPaperTest){
    CreateModel(1);

    // Figure 3 from A Generic Battery Model for the Dynamic Simulation of Hybrid Electric Vehicles
    cap = std::unique_ptr<capacity_lithium_ion_t>(new capacity_lithium_ion_t(6.5, 100, 100, 0));

    model = std::unique_ptr<voltage_t>(new voltage_dynamic_t(1, 1,1.2, 1.4,
            1.25, 1.2, 6.5, 1.3, 5.2, 0.2, 0.0046));
    std::vector<double> dt_hr = {1./6., 1./3., 1./3.};
    // testing with 1lt curve
    std::vector<double> voltages = {1.25, 1.22, 1.17};
    for (size_t i = 0; i < 3; i++){
        double I = 6.5;
        cap->updateCapacity(I, dt_hr[i]);
        model->updateVoltage(cap.get(), 0, dt_hr[i]);
        EXPECT_NEAR(model->battery_voltage(), voltages[i], 0.05) << "NickelMetalHydrideFromPaperTest: " + std::to_string(i);
    }
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, updateCapacityTest){
    double dt_hour = 1;
    CreateModel(dt_hour);

    double I = 2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 3
    model->updateVoltage(cap.get(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.9, tol);

    I = -2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 5
    model->updateVoltage(cap.get(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 4.1, tol);

    I = 5;
    cap->updateCapacity(I, dt_hour); // qmx = 10, I = 4.5, q0 = 0.5
    model->updateVoltage(cap.get(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 2.49, tol);
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, calculateMaxChargeHourly){
    double dt_hour = 1;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 2989, 1);
    double max_current_calc = model->calculate_current_for_target_w(-power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);


    // start at full SOC
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 292, 1);
    max_current_calc = model->calculate_current_for_target_w(-power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);


    // start at empty SOC
    double I = 2;
    while (cap->SOC() > 5)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 5811, 1);
    max_current_calc = model->calculate_current_for_target_w(-power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, calculateMaxChargeSubHourly){
    double dt_hour = 0.5;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 6132, 1);
    double max_current_calc = model->calculate_current_for_target_w(-power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);

    // start at full SOC
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 585, 1);
    max_current_calc = model->calculate_current_for_target_w(-power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);


    // start at empty SOC
    double I = 2;
    while (cap->SOC() > 5)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 12180, 1);
    max_current_calc = model->calculate_current_for_target_w(-power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, calculateMaxDischargeHourly){
    double dt_hour = 1;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 1845, 1);        // current ~4
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 10, 1e-3);

    // start at empty SOC
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 181, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-1);
    // Empties battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 5, 1e-3);

    // start at full SOC
    double I = -2;
    while (cap->SOC() < 95)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 3829, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 19, 1e-3);
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, calculateMaxDischargeSubHourly){
    double dt_hour = 0.5;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 3592, 1);        // current ~8
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 0.2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 10, 1e-3);

    // start at empty SOC
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 365, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-1);
    // Empties battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 5, 1e-3);

    // start at full SOC
    double I = -2;
    while (cap->SOC() < 95)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 7390, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 0.3);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 14.25, 1e-3);
}

TEST_F(voltage_vanadium_redox_lib_battery_voltage_test, SetUpTest) {
    EXPECT_EQ(model->cell_voltage(), 3.6);
}

TEST_F(voltage_vanadium_redox_lib_battery_voltage_test, updateCapacityTest){
    double dt_hour = 0.5;

    double I = 2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 3
    model->updateVoltage(cap.get(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.55, tol);

    I = -2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 5
    model->updateVoltage(cap.get(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.64, tol);

    I = 5;
    cap->updateCapacity(I, dt_hour); // qmx = 10,q0 = 0.5
    model->updateVoltage(cap.get(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.5, tol);
}
