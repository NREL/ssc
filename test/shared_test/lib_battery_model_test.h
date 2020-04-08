/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#ifndef SYSTEM_ADVISOR_MODEL_LIB_BATTERY_THERMAL_TEST_H
#define SYSTEM_ADVISOR_MODEL_LIB_BATTERY_THERMAL_TEST_H


#include <gtest/gtest.h>

//#include "lib_battery_model.h"
#include "lib_battery.h"
#include "lib_battery_capacity_test.h"
#include "lib_battery_lifetime_test.h"

struct thermal_state{
    double capacity_percent; //[%]
    double T_avg;       // avg during timestep [K]
    double time_at_current_T_room;
    double T_room_init;
    double T_batt_init;
};

static void compareState(thermal_t* model, const thermal_state& state, const std::string& msg){
    double tol = 0.02;
    EXPECT_NEAR(model->T_battery(), state.T_avg, tol) << msg;
//    EXPECT_NEAR(model->capacity_percent(), state.T_avg, tol) << msg;
    EXPECT_NEAR(model->capacity_percent(), state.capacity_percent, tol) << msg;
}

static void compareState(std::unique_ptr<thermal_t>& model, const thermal_state& state, const std::string& msg) {
    compareState(model.get(), state, msg);
}

class lib_battery_thermal_test : public ::testing::Test
{
protected:
//    std::unique_ptr<battery_capacity_interface> new_cap;
    std::unique_ptr<thermal_t> model;
//    std::shared_ptr<storage_time_params> time;

//    battery_capacity_params params;
    double tol = 0.01;
    double error;

    double mass = 507;
    double length = 0.58;
    double width = 0.58;
    double height = 0.58;
    double batt_R = 0.0002;
    double Cp = 1004;
    double h = 20;
    std::vector<double> T_room = {290,290,295,295,290,270,270};
    util::matrix_t<double> capacityVsTemperature;

    double dt_hour = 1;
    int nyears = 1;

public:

    void SetUp() override {
        double vals3[] = { -10, 60, 0, 80, 25, 100, 40, 100 };
        capacityVsTemperature.assign(vals3, 4, 2);
    }
    void CreateModel(double Cp){
        model = std::unique_ptr<thermal_t>(new thermal_t(dt_hour, mass, length, width,
                                                         height, batt_R, Cp, h, T_room, capacityVsTemperature));
    }
};

class lib_battery_losses_test : public ::testing::Test
{
protected:
//    std::unique_ptr<battery_capacity_interface> new_cap;
    std::unique_ptr<losses_t> model;
//    std::shared_ptr<storage_time_params> time;

//    battery_capacity_params params;

    double tol = 0.01;
    double error;

    std::vector<double> chargingLosses;
    std::vector<double> dischargingLosses;

    std::vector<double> fullLosses;

    double dt_hour = 1;
    int nyears = 1;

public:

    void SetUp() override {
        // losses
        for (size_t m = 0; m < 12; m++) {
            chargingLosses.push_back((double)m);
            dischargingLosses.push_back((double)m + 1.);
        }
        for (size_t i = 0; i < 8760; i++) {
            fullLosses.push_back(i);
        }
    }
};

struct battery_state{
    capacity_state capacity;
    double batt_voltage;
    lifetime_state lifetime;
    thermal_state thermal;

    size_t last_idx;
};

static void compareState(std::unique_ptr<battery_t>&model, const battery_state& state, const std::string& msg){
    auto cap = model->capacity_model();
    compareState(cap, state.capacity, msg);

    EXPECT_NEAR(model->battery_voltage(), state.batt_voltage, 0.01) << msg;

    auto l = model->lifetime_model();
    compareState(l, state.lifetime, msg);

    auto t = model->thermal_model();
    compareState(t, state.thermal, msg);

}

// entire suite of tests runs in 177ms - 10/7
class lib_battery_test : public ::testing::Test
{
public:

    // capacity
    double q;
    double SOC_min;
    double SOC_max;
    double SOC_init;

    // voltage
    int n_series;
    int n_strings;
    double Vnom_default;
    double Vfull;
    double Vexp;
    double Vnom;
    double Qfull;
    double Qexp;
    double Qnom;
    double C_rate;
    double resistance;

    // lifetime
    util::matrix_t<double> cycleLifeMatrix;
    util::matrix_t<double> calendarLifeMatrix;
    int calendarChoice;
    int replacementOption;
    double replacementCapacity;

    // thermal
    double mass;
    double length;
    double width;
    double height;
    double Cp;
    double h;
    std::vector<double> T_room;
    util::matrix_t<double> capacityVsTemperature;

    // losses
    std::vector<double> monthlyLosses;
    std::vector<double> fullLosses;
    std::vector<double> fullLossesMinute;
    int lossChoice;

    // battery
    int chemistry;
    double dtHour;

    // models
    double tol = 0.02;

    capacity_lithium_ion_t * capacityModel;
    voltage_dynamic_t * voltageModel;
    thermal_t * thermalModel;
    lifetime_calendar_t * calendarModel;
    lifetime_cycle_t * cycleModel;
    lifetime_t * lifetimeModel;
    losses_t * lossModel;
    std::unique_ptr<battery_t> batteryModel;

    void SetUp() override
    {
        // capacity
        q = 1000;
        SOC_init = 50;
        SOC_min = 5;
        SOC_max = 95;

        // voltage
        n_series = 139;
        n_strings = 9;
        Vnom_default = 3.6;
        Vfull = 4.1;
        Vexp = 4.05;
        Vnom = 3.4;
        Qfull = 2.25;
        Qexp = 0.04;
        Qnom = 2.0;
        C_rate = 0.2;
        resistance = 0.2;

        // lifetime
        double vals[] = { 20, 0, 100, 20, 5000, 80, 20, 10000, 60, 80, 0, 100, 80, 1000, 80, 80, 2000, 60 };
        cycleLifeMatrix.assign(vals, 6, 3);
        double vals2[] = { 0, 100, 3650, 80, 7300, 50 };
        calendarLifeMatrix.assign(vals, 3, 2);
        calendarChoice = 1;
        replacementOption = 0;

        // thermal
        mass = 507;
        length = 0.58;
        width = 0.58;
        height = 0.58;
        Cp = 1004;
        h = 500;
        T_room.emplace_back(293.15);

        double vals3[] = { -10, 60, 0, 80, 25, 100, 40, 100 };
        capacityVsTemperature.assign(vals3, 4, 2);

        // losses
        for (size_t m = 0; m < 12; m++) {
            monthlyLosses.push_back((double)m);
        }
        for (size_t i = 0; i < 8760; i++) {
            fullLosses.push_back(0);
        }
        for (size_t i = 0; i < 8760 * 60; i++) {
            fullLossesMinute.push_back(0);
        }
        lossChoice = 0;

        // battery
        chemistry = 1;
        dtHour = 1.0;

        capacityModel = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min, dtHour);
        voltageModel = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom, C_rate, resistance);
        cycleModel = new lifetime_cycle_t(cycleLifeMatrix);
        calendarModel = new lifetime_calendar_t(calendarChoice, calendarLifeMatrix, dtHour);
        lifetimeModel = new lifetime_t(cycleModel, calendarModel, replacementOption, replacementCapacity);
        thermalModel = new thermal_t(1.0, mass, length, width, height, resistance, Cp, h, T_room,
                                     capacityVsTemperature);
        lossModel = new losses_t(dtHour, lifetimeModel, thermalModel, capacityModel, lossChoice, monthlyLosses, monthlyLosses, monthlyLosses, fullLosses);
        batteryModel = std::unique_ptr<battery_t>(new battery_t(dtHour, chemistry));
        batteryModel->initialize(capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);
    }

    void TearDown(){
        delete capacityModel;
        delete voltageModel;
        delete cycleModel;
        delete calendarModel;
        delete lifetimeModel;
        delete thermalModel;
        delete lossModel;
    }

};

#endif //SYSTEM_ADVISOR_MODEL_LIB_BATTERY_THERMAL_TEST_H
