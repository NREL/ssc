#include <gtest/gtest.h>

//#include "lib_battery_capacity.h"
#include "lib_battery.h"
#include "lib_battery_lifetime_test.h"

TEST_F(lib_battery_lifetime_cycle_test, SetUpTest) {
    EXPECT_EQ(cycle_model->capacity_percent(), 100);
}


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

TEST_F(lib_battery_lifetime_cycle_test, runCycleLifetimeTest) {
    double DOD = 5;       // not used but required for function
    int idx = 0;
    while (idx < 500){
        if (idx % 2 != 0){
            DOD = 95;
        }
        else
            DOD = 5;
        cycle_model->runCycleLifetime(DOD);
        idx++;
    }
    cycle_state s = cycle_model->get_state();
    EXPECT_NEAR(s.q_relative_cycle, 95.02, tol);
    EXPECT_NEAR(s.rainflow_Xlt, 90, tol);
    EXPECT_NEAR(s.rainflow_Ylt, 90, tol);
    EXPECT_NEAR(s.rainflow_jlt, 2, tol);
    EXPECT_NEAR(s.range, 90, tol);
    EXPECT_NEAR(s.average_range, 90, tol);
    EXPECT_NEAR(s.n_cycles, 249, tol);

    while (idx < 1000){
        if (idx % 2 != 0){
            DOD = 90;
        }
        cycle_model->runCycleLifetime(DOD);
        idx++;
    }
    s = cycle_model->get_state();
    EXPECT_NEAR(s.q_relative_cycle, 91.244, tol);
    EXPECT_NEAR(s.rainflow_Xlt, 0, tol);
    EXPECT_NEAR(s.rainflow_Ylt, 0, tol);
    EXPECT_NEAR(s.rainflow_jlt, 2, tol);
    EXPECT_NEAR(s.range, 0, tol);
    EXPECT_NEAR(s.average_range, 44.9098, tol);
    EXPECT_NEAR(s.n_cycles, 499, tol);
}


TEST_F(lib_battery_lifetime_cycle_test, replaceBatteryTest) {
    double DOD = 5;       // not used but required for function
    int idx = 0;
    while (idx < 1500){
        if (idx % 2 != 0){
            DOD = 95;
        }
        else
            DOD = 5;
        cycle_model->runCycleLifetime(DOD);
        idx++;
    }
    auto st = cycle_lifetime_state({85.02,90,90,90,90, 749, 2});
    cycle_state s = cycle_model->get_state();
    EXPECT_NEAR(s.q_relative_cycle, 85.02, tol);
    EXPECT_NEAR(s.rainflow_Xlt, 90, tol);
    EXPECT_NEAR(s.rainflow_Ylt, 90, tol);
    EXPECT_NEAR(s.rainflow_jlt, 2, tol);
    EXPECT_NEAR(s.range, 90, tol);
    EXPECT_NEAR(s.average_range, 90, tol);
    EXPECT_NEAR(s.n_cycles, 749, tol);

    cycle_model->replaceBattery(5);

    s = cycle_model->get_state();
    EXPECT_NEAR(s.q_relative_cycle, 90.019, tol);
    EXPECT_NEAR(s.rainflow_Xlt, 0, tol);
    EXPECT_NEAR(s.rainflow_Ylt, 0, tol);
    EXPECT_NEAR(s.rainflow_jlt, 0, tol);
    EXPECT_NEAR(s.range, 0, tol);
    EXPECT_NEAR(s.average_range, 90, tol);
    EXPECT_NEAR(s.n_cycles, 749, tol);
}

TEST_F(lib_battery_lifetime_calendar_matrix_test, runCalendarMatrixTest) {
    double T = 278, SOC = 20;       // not used but required for function
    int idx = 0;
    while (idx < 500){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    calendar_state s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 20, tol);
    EXPECT_NEAR(s.q_relative_calendar, 99.89, tol);
    EXPECT_NEAR(s.dq_relative_calendar_old, 0, tol);

    while (idx < 1000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 41, tol);
    EXPECT_NEAR(s.q_relative_calendar, 99.775, tol);
    EXPECT_NEAR(s.dq_relative_calendar_old, 0, tol);
}

TEST_F(lib_battery_lifetime_calendar_matrix_test, replaceBatteryTest) {
    double T = 4.85, SOC = 20;
    int idx = 0;
    while (idx < 200000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    calendar_state s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 8333, tol);
    EXPECT_NEAR(s.q_relative_calendar, 41.51, tol);
    EXPECT_NEAR(s.dq_relative_calendar_old, 0, tol);

    cal_model->replaceBattery(5);

    s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 0, tol);
    EXPECT_NEAR(s.q_relative_calendar, 46.51, tol);
    EXPECT_NEAR(s.dq_relative_calendar_old, 0, tol);
}

TEST_F(lib_battery_lifetime_calendar_model_test, SetUpTest) {
    EXPECT_EQ(cal_model->capacity_percent(), 102);
}

TEST_F(lib_battery_lifetime_calendar_model_test, runCalendarModelTest) {
    double T = 4.85, SOC = 20;       // not used but required for function
    int idx = 0;
    while (idx < 500){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    calendar_state s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 20, tol);
    EXPECT_NEAR(s.q_relative_calendar, 101.78, tol);
    EXPECT_NEAR(s.dq_relative_calendar_old, 0.00217, tol);

    while (idx < 1000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 41, tol);
    EXPECT_NEAR(s.q_relative_calendar, 101.69, tol);
    EXPECT_NEAR(s.dq_relative_calendar_old, 0.00306, tol);
}

TEST_F(lib_battery_lifetime_calendar_model_test, replaceBatteryTest) {
    double T = 4.85, SOC = 20;
    int idx = 0;
    while (idx < 200000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    calendar_state s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 8333, tol);
    EXPECT_NEAR(s.q_relative_calendar, 97.67, tol);
    EXPECT_NEAR(s.dq_relative_calendar_old, 0.043, tol);

    cal_model->replaceBattery(5);

    s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 0, tol);
    EXPECT_NEAR(s.q_relative_calendar, 102, tol);
    EXPECT_NEAR(s.dq_relative_calendar_old, 0.0, tol);
}

TEST_F(lib_battery_lifetime_calendar_matrix_test, TestLifetimeDegradation) {
    double vals[] = { 0, 100, 365, 50 };
    util::matrix_t<double> lifetime_matrix;
    lifetime_matrix.assign(vals, 2, 2);

    double dt_hour = 1;
    lifetime_calendar_t hourly_lifetime(dt_hour, lifetime_matrix);

    for (int idx = 0; idx < 8760; idx++) {
        hourly_lifetime.runLifetimeCalendarModel(idx, 20, 80);
    }

    EXPECT_NEAR(hourly_lifetime.capacity_percent(), 50, 1);

    dt_hour = 1.0 / 12.0; // Every 5 mins
    lifetime_calendar_t subhourly_lifetime(dt_hour, lifetime_matrix);

    for (int idx = 0; idx < 8760 * 12; idx++) {
        subhourly_lifetime.runLifetimeCalendarModel(idx, 20, 80);
    }

    EXPECT_NEAR(subhourly_lifetime.capacity_percent(), 50, 1);
}


TEST_F(lib_battery_lifetime_calendar_model_test, TestLifetimeDegradation) {

    for (int idx = 0; idx < 8760; idx++) {
        cal_model->runLifetimeCalendarModel(idx, 20, 80);
    }

    EXPECT_NEAR(cal_model->capacity_percent(), 99.812, 1);

    dt_hour = 1.0 / 12.0; // Every 5 mins
    lifetime_calendar_t subhourly_lifetime(dt_hour);

    for (int idx = 0; idx < 8760 * 12; idx++) {
        subhourly_lifetime.runLifetimeCalendarModel(idx, 20, 80);
    }

    EXPECT_NEAR(subhourly_lifetime.capacity_percent(), 99.812, 1);
}

TEST_F(lib_battery_lifetime_test, updateCapacityTest) {
    size_t idx = 0;
    while (idx < 876){
        model->runLifetimeModels(idx, true, 5,95, 25);
        model->runLifetimeModels(idx, true, 95, 5, 25);

        auto state = model->get_state();
        EXPECT_EQ(state.cycle->q_relative_cycle, model->capacity_percent_cycle());
        EXPECT_EQ(state.calendar->q_relative_calendar, model->capacity_percent_calendar());

        idx ++;
    }

}


//TEST_F(lib_battery_lifetime_test, ReplaceByCapacityTest){
//    model = std::unique_ptr<lifetime_t>(new lifetime_t(cycle_model.get(), cal_model.get(), 1, 60));
//
//    size_t idx = 0;
//    while (idx < 876){
//        model->runLifetimeModels(idx, true, 5,95, 293);
//        model->runLifetimeModels(idx, true, 95, 5, 293);
//        idx ++;
//    }
//    auto s = lifetime_state({{82.5, 90, 90, 90, 90, 875, 2, std::vector<double>()},
//                                 {36, 101.937, 875, 0.000632, 0.000632}, 82.5, 0});
//    compareState(model, s, "ReplaceByCapacityTest: 1");
//
//    while (idx < 2101){
//        model->runLifetimeModels(idx, true, 25, 75, 293);
//        model->runLifetimeModels(idx, true, 75, 25, 293);
//        idx ++;
//    }
//    s = lifetime_state({{59.998, 50, 70, 50, 66.682, 2099, 4, std::vector<double>()},
//                                 {87, 101.845, 2099, 0.00155, 0.00155}, 60.015, 0});
//
//    compareState(model, s, "ReplaceByCapacityTest: 2");
//
//    bool rep = model->check_replaced();
//    EXPECT_TRUE(rep);
//
//    s = lifetime_state({{100, 0, 0, 0, 66.682, 0, 0, std::vector<double>()},
//                        {0, 102, 0, 0, 0}, 100, 1});
//
//    compareState(model, s, "ReplaceByCapacityTest: 3");
//
//}

