#include <gtest/gtest.h>

//#include "lib_battery_capacity.h"
#include "lib_battery.h"
#include "lib_battery_voltage_test.h"

TEST_F(voltage_table_lib_battery_voltage_test, SetUpTest) {
    EXPECT_EQ(model->cell_voltage(), 3.6);
}

TEST_F(voltage_table_lib_battery_voltage_test, updateCapacityTest){
    double I = 2;
    cap->updateCapacity(I, dt_hour); // cap = 3
    model->updateVoltage(cap.get(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 1.65, tol);
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, SetUpTest) {
    EXPECT_EQ(model->cell_voltage(), 4.1);
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, updateCapacityTest){
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

TEST_F(voltage_vanadium_redox_lib_battery_voltage_test, SetUpTest) {
    EXPECT_EQ(model->cell_voltage(), 3.6);
}

TEST_F(voltage_vanadium_redox_lib_battery_voltage_test, updateCapacityTest){
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