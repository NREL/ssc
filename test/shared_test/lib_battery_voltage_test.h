#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_VOLTAGE_TEST_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_VOLTAGE_TEST_H

#include <gtest/gtest.h>

#include "lib_util.h"
//#include "lib_battery_voltage.h"
#include "lib_battery.h"

class lib_battery_voltage_test : public ::testing::Test
{
protected:
//    std::unique_ptr<battery_capacity_interface> new_cap;
    std::unique_ptr<voltage_t> model;
    std::unique_ptr<capacity_t> cap;

//    std::shared_ptr<storage_time_params> time;

//    battery_capacity_params params;
    double tol = 0.01;
    double error;

    int n_cells_series = 139;
    int n_strings = 9;
    double voltage_nom = 3.6;
    double R = 0.2;

    double dt_hour = 1;
    int nyears = 1;

public:
};

class voltage_table_lib_battery_voltage_test : public lib_battery_voltage_test
{
protected:
    std::vector<double> table_vals = {75, 1.5, 25, 3.5};

    void SetUp() override {
        cap = std::unique_ptr<capacity_lithium_ion_t>(new capacity_lithium_ion_t(10, 50, 95, 5));
        util::matrix_t<double> voltage_table = util::matrix_t<double>(2, 2, &table_vals);
        model = std::unique_ptr<voltage_t>(new voltage_table_t(n_cells_series, n_strings, voltage_nom, voltage_table, R));
    };
};

class voltage_dynamic_lib_battery_voltage_test : public lib_battery_voltage_test
{
protected:
    double Vfull = 4.1;
    double Vexp = 4.05;
    double Vnom = 3.4;
    double Qfull = 2.25;
    double Qexp = 0.04;
    double Qnom = 2.0;
    double C_rate = 0.2;

    void SetUp(){
        cap = std::unique_ptr<capacity_lithium_ion_t>(new capacity_lithium_ion_t(10, 50, 95, 5));

        model = std::unique_ptr<voltage_t>(new voltage_dynamic_t(n_cells_series, n_strings,
                voltage_nom, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom, C_rate, R));
    }
};

class voltage_vanadium_redox_lib_battery_voltage_test : public lib_battery_voltage_test
{
protected:
    void SetUp(){
        cap = std::unique_ptr<capacity_lithium_ion_t>(new capacity_lithium_ion_t(10, 50, 95, 5));

        model = std::unique_ptr<voltage_t>(new voltage_vanadium_redox_t(n_cells_series, n_strings, voltage_nom, R));
    }
};

#endif //SAM_SIMULATION_CORE_LIB_BATTERY_VOLTAGE_TEST_H
