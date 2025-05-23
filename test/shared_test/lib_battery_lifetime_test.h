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


#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_TEST_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_TEST_H

#include <gtest/gtest.h>

#include "lib_util.h"
//#include "lifetime_t.h"
#include "lib_battery.h"

class lib_battery_lifetime_cycle_test : public ::testing::Test
{
protected:
//    std::unique_ptr<battery_capacity_interface> new_cap;
    std::unique_ptr<lifetime_cycle_t> cycle_model;

//    std::shared_ptr<storage_time_params> time;

//    battery_capacity_params params;
    double tol = 0.01;

    util::matrix_t<double> cycles_vs_DOD;

    double dt_hour = 1;

public:
    void SetUp(){
        double table_vals[18] = {20, 0, 100, 20, 5000, 80, 20, 10000, 60, 80, 0, 100, 80, 1000, 80, 80, 2000, 60};
        cycles_vs_DOD.assign(table_vals, 6, 3);
        cycle_model = std::unique_ptr<lifetime_cycle_t>(new lifetime_cycle_t(cycles_vs_DOD));
    }
};

struct calendar_lifetime_state {
    size_t day_age_of_battery;
    double q;

    // Li Ion model, relative capacity (0 - 1)
    size_t last_idx;
    double dq_old;
    double dq_new;
};

class lib_battery_lifetime_calendar_matrix_test : public ::testing::Test
{
protected:
//    std::unique_ptr<battery_capacity_interface> new_cap;
    std::unique_ptr<lifetime_calendar_t> cal_model;

//    std::shared_ptr<storage_time_params> time;

//    battery_capacity_params params;
    double tol = 0.01;

    util::matrix_t<double> calendar_matrix;

    double dt_hour = 1;

public:
    void SetUp() override {
        double table_vals[18] = {0, 100, 3650, 80, 7300, 50};
        calendar_matrix.assign(table_vals, 3, 2);
        cal_model = std::unique_ptr<lifetime_calendar_t>(new lifetime_calendar_t(dt_hour, calendar_matrix));
    }
};


class lib_battery_lifetime_calendar_model_test : public ::testing::Test
{
protected:
//    std::unique_ptr<battery_capacity_interface> new_cap;
    std::unique_ptr<lifetime_calendar_t> cal_model;

//    std::shared_ptr<storage_time_params> time;

//    battery_capacity_params params;
    double tol = 0.01;

    double dt_hour = 1;

public:
    void SetUp() override {
        cal_model = std::unique_ptr<lifetime_calendar_t>(new lifetime_calendar_t(dt_hour));
    }
};

class lib_battery_lifetime_test : public ::testing::Test{
protected:
    std::unique_ptr<lifetime_calendar_cycle_t> model;

    util::matrix_t<double> cycles_vs_DOD;

    double dt_hour = 1;
public:
    void SetUp() override {
        double table_vals[18] = {20, 0, 100, 20, 5000, 80, 20, 10000, 60, 80, 0, 100, 80, 1000, 80, 80, 2000, 60};
        cycles_vs_DOD.assign(table_vals, 6, 3);
        model = std::unique_ptr<lifetime_calendar_cycle_t>(new lifetime_calendar_cycle_t(cycles_vs_DOD, dt_hour, 1.02, 2.66e-3, -7280, 930));
    }
};





#endif //SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_TEST_H
