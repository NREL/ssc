#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_TEST_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_TEST_H

#include <gtest/gtest.h>

#include "lib_util.h"
//#include "lib_battery_lifetime.h"
#include "lib_battery.h"

struct cycle_lifetime_state {
    double relative_q;
    double Xlt;
    double Ylt;
    double Range;
    double average_range;
    size_t nCycles;
    double jlt;
    std::vector<double> Peaks;
};

static void compareState(std::unique_ptr<lifetime_cycle_t>& cycle_model, cycle_lifetime_state state, const std::string& msg){
    double tol = 0.01;
    EXPECT_NEAR(cycle_model->capacity_percent(), state.relative_q, tol) << msg;
//        EXPECT_NEAR(model->qmax_thermal(), state.Xlt, tol) << msg;
//        EXPECT_NEAR(model->qmax(), state.Ylt, tol) << msg;
    EXPECT_NEAR(cycle_model->cycle_range(), state.Range, tol) << msg;
    EXPECT_NEAR(cycle_model->average_range(), state.average_range, tol) << msg;
    EXPECT_NEAR(cycle_model->cycles_elapsed(), state.nCycles, tol) << msg;
//        EXPECT_NEAR(model->DOD(), state.Peaks, tol) << msg;
//        EXPECT_NEAR(model->charge_operation(), state.jlt, tol) << msg;
}

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

static void compareState(std::unique_ptr<lifetime_calendar_t>& cal_model, calendar_lifetime_state state, const std::string& msg){
    double tol = 0.001;
//        EXPECT_NEAR(model->, state.day_age_of_battery, tol) << msg;
    EXPECT_NEAR(cal_model->capacity_percent(), state.q, tol) << msg;
//        EXPECT_NEAR(model->capacity_percent(), state.last_idx, tol) << msg;
//        EXPECT_NEAR(model->average_range(), state.dq_old, tol) << msg;
//        EXPECT_NEAR(model->cycle_range(), state.dq_new, tol) << msg;
}

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
        cal_model = std::unique_ptr<lifetime_calendar_t>(new lifetime_calendar_t(2, calendar_matrix, dt_hour));
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
        cal_model = std::unique_ptr<lifetime_calendar_t>(new lifetime_calendar_t(1, util::matrix_t<double>(), dt_hour));
    }
};

struct lifetime_state{
    cycle_lifetime_state cycle;
    calendar_lifetime_state calendar;

    double q;               // battery relative capacity (0 - 100%)
    int replacements;       // Number of replacements this year
};

static void compareState(std::unique_ptr<lifetime_t>& model, const lifetime_state& s, const std::string& msg){
    auto cycle = std::unique_ptr<lifetime_cycle_t>(model->cycleModel());
    compareState(cycle, s.cycle, msg);
    auto cal = std::unique_ptr<lifetime_calendar_t>(model->calendarModel());
    compareState(cal, s.calendar, msg);
}

class lib_battery_lifetime_test : public ::testing::Test{
protected:
    std::unique_ptr<lifetime_t> model;
    std::unique_ptr<lifetime_calendar_t> cal_model;
    std::unique_ptr<lifetime_cycle_t> cycle_model;

    util::matrix_t<double> cycles_vs_DOD;

    double dt_hour = 1;
public:
    void SetUp() override {
        cal_model = std::unique_ptr<lifetime_calendar_t>(new lifetime_calendar_t(1, util::matrix_t<double>(), dt_hour));
        double table_vals[18] = {20, 0, 100, 20, 5000, 80, 20, 10000, 60, 80, 0, 100, 80, 1000, 80, 80, 2000, 60};
        cycles_vs_DOD.assign(table_vals, 6, 3);
        cycle_model = std::unique_ptr<lifetime_cycle_t>(new lifetime_cycle_t(cycles_vs_DOD));
    }
};
#endif //SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_TEST_H
