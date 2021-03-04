#include <gtest/gtest.h>
#include <random>

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
    lifetime_state s = cycle_model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 95.02, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 2, tol);
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
    EXPECT_NEAR(s.cycle->q_relative_cycle, 91.244, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 0, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 0, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 2, tol);
    EXPECT_NEAR(s.range, 0, tol);
    EXPECT_NEAR(s.average_range, 44.9098, tol);
    EXPECT_NEAR(s.n_cycles, 499, tol);
}

TEST_F(lib_battery_lifetime_cycle_test, runCycleLifetimeTestJaggedProfile) {
    std::vector<double> DOD = { 5, 95, 50, 85, 10, 50, 5, 95, 5 };  // 4 cycles
    int idx = 0;
    while (idx < DOD.size()) {
        cycle_model->runCycleLifetime(DOD[idx]);
        idx++;
    }
    lifetime_state s = cycle_model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 99.95, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 1, tol);
    EXPECT_NEAR(s.range, 90, tol);
    EXPECT_NEAR(s.average_range, 63.75, tol);
    EXPECT_NEAR(s.n_cycles, 4, tol);

}

TEST_F(lib_battery_lifetime_cycle_test, runCycleLifetimeTestKokamProfile) {
    std::vector<double> DOD = { 0.66, 1.0, 0.24722075172048893, 1.0, 0.24559790735021855, 0.9989411900454035, 0.24559790735021936, 0.9989411900454057, 0.24573025859454606, 0.9990735412897335, 0.24625966357184892, 0.9992058925340614, 0.2466567173048243, 0.9992058925340647, 0.2465243660605033, 0.9982794338237967, 0.24718612228213058, 0.9992058925340731, 0.24718612228213466, 0.9982794338238032, 0.24612731232753976, 0.9981470825794796, 0.24625966357186685, 0.9984117850681331, 0.24678906854916766, 0.9984117850681358, 0.24731847352646982, 0.9985441363124643, 0.24784787850377074, 0.9988088388011173, 0.24784787850377454 };  // 4 cycles
    int idx = 0;
    while (idx < DOD.size()) {
        cycle_model->runCycleLifetime((1-DOD[idx]) * 100.0);
        idx++;
    }
    lifetime_state s = cycle_model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 99.79, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 75.09, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 75.27, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 5, tol);
    EXPECT_NEAR(s.range, 75.07, tol);
    EXPECT_NEAR(s.average_range, 72.03, tol);
    EXPECT_NEAR(s.n_cycles, 13, tol);

}

TEST_F(lib_battery_lifetime_cycle_test, runCycleLifetimeTestWithNoise) {
    int seed = 100;
    double tol_high = 1.6; // Randomness will generate different results on different platforms

    // Initialize a default_random_engine with the seed
    std::default_random_engine randomEngine(seed);

    // Initialize a uniform_real_distribution to produce values between -1 and 1
    std::uniform_real_distribution<double> unifRealDist(-1.0, 1.0);

    double DOD = 5;       // not used but required for function
    int idx = 0;
    while (idx < 500) {
        double number = unifRealDist(randomEngine);
        if (idx % 2 != 0) {
            DOD = 95 + number;
        }
        else
            DOD = 5 + number;
        cycle_model->runCycleLifetime(DOD);
        idx++;
    }
    lifetime_state s = cycle_model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 95.06, tol_high);
    EXPECT_NEAR(s.range, 90.6, tol_high);
    EXPECT_NEAR(s.average_range, 90.02, tol_high);

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
    lifetime_state s = cycle_model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 85.02, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 2, tol);
    EXPECT_NEAR(s.range, 90, tol);
    EXPECT_NEAR(s.average_range, 90, tol);
    EXPECT_NEAR(s.n_cycles, 749, tol);

    cycle_model->replaceBattery(5);

    s = cycle_model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 90.019, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 0, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 0, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 0, tol);
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
    lifetime_state s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 20.79, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 99.89, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0, tol);

    while (idx < 1000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 41.625, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 99.775, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0, tol);
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
    lifetime_state s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 8333.29, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 41.51, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0, tol);

    cal_model->replaceBattery(5);

    s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 0, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 46.51, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0, tol);
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
    lifetime_state s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 20.79, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 101.78, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0.00217, tol);

    while (idx < 1000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 41.625, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 101.69, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0.00306, tol);
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
    lifetime_state s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 8333.29, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 97.67, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0.043, tol);

    cal_model->replaceBattery(5);

    s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 0, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 102, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0.0, tol);
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

TEST_F(lib_battery_lifetime_test, runCycleLifetimeTestWithRestPeriod) {
    double tol = 0.01;

    std::vector<double> DOD = { 5, 50, 95, 50, 5, 5, 5, 50, 95, 50, 5, 5, 5, 50, 95, 50, 5 };  // 3 cycles 90% DOD
    std::vector<bool> charge_changed = { true, false, false, true, false, false, false, true, false, true, false, false, false, true, false, true, false };
    int idx = 0;
    double T_battery = 25; // deg C
    while (idx < DOD.size()) {
        double DOD_prev = 0;
        if (idx > 0) {
            DOD_prev = DOD[idx - 1];
        }
        model->runLifetimeModels(idx, charge_changed[idx], DOD_prev, DOD[idx], T_battery);
        idx++;
    }


    lifetime_state s = model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 99.96, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 2, tol);
    EXPECT_NEAR(s.range, 90, tol);
    EXPECT_NEAR(s.average_range, 90, tol);
    EXPECT_NEAR(s.n_cycles, 2, tol);
}

TEST_F(lib_battery_lifetime_nmc_test, InitTest) {
    double tol = 0.001;

    //check lifetime_nmc_state_initialization
    auto lifetime_state = model->get_state();
    EXPECT_NEAR(lifetime_state.nmc_li_neg->q_relative_neg, 100.853, tol);
    EXPECT_NEAR(lifetime_state.nmc_li_neg->q_relative_li, 107.142, tol);
    EXPECT_EQ(model->get_state().day_age_of_battery, 0);
    EXPECT_EQ(model->get_state().n_cycles, 0);

    //check U_neg, and Voc functions (SOC as a fractional input)
    EXPECT_NEAR(model->calculate_Uneg(0.1), 0.242, tol);
    EXPECT_NEAR(model->calculate_Voc(0.1), 3.4679, tol);
    EXPECT_NEAR(model->calculate_Uneg(0.5), 0.1726, tol);
    EXPECT_NEAR(model->calculate_Voc(0.5), 3.6912, tol);
    EXPECT_NEAR(model->calculate_Uneg(0.9), 0.1032, tol);
    EXPECT_NEAR(model->calculate_Voc(0.9), 4.0818, tol);
}

/// run at different days
TEST_F(lib_battery_lifetime_nmc_test, StorageDays) {
    std::vector<double> days = {0, 10, 50 , 500, 5000};
    std::vector<double> expected_q_li = {106.50, 104.36, 103.97, 103.72, 102.93};

    for (size_t i = 0; i < days.back() + 1; i++) {
        for (size_t h = 0; h < 24; h++) {
            size_t hr = i * 24 + h;
            model->runLifetimeModels(hr, false, 50, 50, 25);
        }
        auto pos = std::find(days.begin(), days.end(), i);
        if (pos != days.end()) {
            auto state = model->get_state();
            EXPECT_NEAR(state.nmc_li_neg->q_relative_li, expected_q_li[pos - days.begin()], 0.5);
        }
    }
    EXPECT_EQ((size_t)model->get_state().day_age_of_battery, 5001);
}

/// Run with minute timestep instead
TEST_F(lib_battery_lifetime_nmc_test, StorageMinuteTimestep) {
    double dt_hr = 1. / 60;
    model = std::unique_ptr<lifetime_nmc_t>(new lifetime_nmc_t(dt_hr));

    std::vector<double> days = {0, 10, 50 , 500, 5000};
    std::vector<double> expected_q_li = {106.50, 104.36, 103.97, 103.72, 102.93};

    auto steps_per_day = (size_t)(24 / dt_hr);
    for (size_t i = 0; i < days.back() + 1; i++) {
        for (size_t h = 0; h < steps_per_day; h++) {
            size_t hr = i * steps_per_day + h;
            model->runLifetimeModels(hr, false, 50, 50, 25);
        }
        auto pos = std::find(days.begin(), days.end(), i);
        if (pos != days.end()) {
            auto state = model->get_state();
            EXPECT_NEAR(state.nmc_li_neg->q_relative_li, expected_q_li[pos - days.begin()], 0.5);
        }
    }
    EXPECT_EQ((size_t)model->get_state().day_age_of_battery, 5001);
}

/// run at different days at different temperatures
TEST_F(lib_battery_lifetime_nmc_test, StorageTemp) {
    std::vector<double> temps = {0, 10, 15, 40};
    std::vector<double> expected_q_li = {81.73, 93.08, 97.43, 102.33};

    for (size_t n = 3; n < temps.size(); n++) {
        model = std::unique_ptr<lifetime_nmc_t>(new lifetime_nmc_t(dt_hour));
        for (size_t d = 0; d < 5000 + 1; d++) {
            for (size_t h = 0; h < 24; h++) {
                size_t hr = d * 24 + h;
                model->runLifetimeModels(hr, false, 50, 50, temps[n]);
            }
        }
        auto state = model->get_state();
        EXPECT_NEAR((size_t)state.nmc_li_neg->q_relative_li, expected_q_li[n], 1);
    }
}

TEST_F(lib_battery_lifetime_nmc_test, CyclingHighDOD) {
    size_t day = 0;
    double T = 25.15;
    while (day < 87) {
        for (size_t i = 0; i < 24; i++) {
            size_t idx = day * 24 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 90, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 90, 10, T);
            else if (i == 3)
                model->runLifetimeModels(idx, true, 10, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
        }
        day ++;
    }

    auto state = model->get_state();

    EXPECT_EQ(state.n_cycles, 86);
    EXPECT_EQ(state.nmc_li_neg->DOD_max, 50);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 103.23, 0.5);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 100.6, 0.5);
    EXPECT_NEAR(state.day_age_of_battery, 87, 1e-3);

    while (day < 870) {
        for (size_t i = 0; i < 24; i++) {
            size_t idx = day * 24 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 90, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 90, 10, T);
            else if (i == 3)
                model->runLifetimeModels(idx, true, 10, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
        }
        day ++;
    }

    state = model->get_state();

    EXPECT_EQ(state.n_cycles, 869);
    EXPECT_EQ(state.nmc_li_neg->DOD_max, 50);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 99.6, 0.5);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 98.00, 0.5);
    EXPECT_NEAR(state.day_age_of_battery, 870, 1e-3);

    while (day < 8700) {
        for (size_t i = 0; i < 24; i++) {
            size_t idx = day * 24 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 90, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 90, 10, T);
            else if (i == 3)
                model->runLifetimeModels(idx, true, 10, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
        }
        day ++;
    }

    state = model->get_state();

    EXPECT_EQ(state.n_cycles, 8699);
    EXPECT_EQ(state.nmc_li_neg->DOD_max, 50);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 84.19, 3);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 67.00, 0.5);
    EXPECT_NEAR(state.day_age_of_battery, 8700, 1e-3);
}

TEST_F(lib_battery_lifetime_nmc_test, CyclingHighTemp) {
    size_t day = 0;
    double T = 35.;

    while (day < 87) {
        for (size_t i = 0; i < 24; i++) {
            size_t idx = day * 24 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 70, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 70, 30, T);
            else if (i == 3)
                model->runLifetimeModels(idx, true, 30, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
        }
        day ++;
    }

    auto state = model->get_state();

    EXPECT_EQ(state.n_cycles, 86);
    EXPECT_EQ(state.nmc_li_neg->DOD_max, 50);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 105.45, 0.6);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 103.79, 0.5);
    EXPECT_NEAR(state.day_age_of_battery, 87, 1e-3);

    while (day < 870) {
        for (size_t i = 0; i < 24; i++) {
            size_t idx = day * 24 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 70, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 70, 30, T);
            else if (i == 3)
                model->runLifetimeModels(idx, true, 30, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
        }
        day ++;
    }

    state = model->get_state();

    EXPECT_EQ(state.n_cycles, 869);
    EXPECT_EQ(state.nmc_li_neg->DOD_max, 50);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 103.49, 0.5);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 103.35, 0.5);
    EXPECT_NEAR(state.day_age_of_battery, 870, 1e-3);

    while (day < 8700) {
        for (size_t i = 0; i < 24; i++) {
            size_t idx = day * 24 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 70, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 70, 30, T);
            else if (i == 3)
                model->runLifetimeModels(idx, true, 30, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
        }
        day ++;
    }

    state = model->get_state();

    EXPECT_EQ(state.n_cycles, 8699);
    EXPECT_EQ(state.nmc_li_neg->DOD_max, 50);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 92.38, 0.5);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 98.93, 0.5);
    EXPECT_NEAR(state.day_age_of_battery, 8700, 1e-3);
}

TEST_F(lib_battery_lifetime_nmc_test, CyclingCRate) {
    size_t day = 0;

    // 90 DOD cycle once per day, slower Crate than above
    std::vector<double> DODs_day = {50., 56.67, 63.33, 70., 76.67, 83.33,
                                    90., 83.33, 76.67, 70., 63.33, 56.67, 50., 43.33, 36.67, 30., 23.33, 16.67,
                                    10., 16.67, 23.33, 30., 36.67, 43.33};

    while (day < 87) {
        for (size_t i = 0; i < DODs_day.size(); i++) {
            size_t idx = day * 24 + i;
            bool charge_changed = i == 7 || i == 19;
            double prev_DOD = DODs_day[i % 24];
            double DOD = DODs_day[i];
            model->runLifetimeModels(idx, charge_changed, prev_DOD, DOD, 25);
        }
        day ++;
    }

    auto state = model->get_state();

    EXPECT_EQ(state.n_cycles, 86);
    EXPECT_EQ(state.nmc_li_neg->DOD_max, 43.33);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 103, 1);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 100, 1);
    EXPECT_NEAR(state.day_age_of_battery, 87, 1e-3);

    while (day < 870) {
        for (size_t i = 0; i < DODs_day.size(); i++) {
            size_t idx = day * 24 + i;
            bool charge_changed = i == 7 || i == 19;
            double prev_DOD = DODs_day[i % 24];
            double DOD = DODs_day[i];
            model->runLifetimeModels(idx, charge_changed, prev_DOD, DOD, 25);
        }
        day ++;
    }

    state = model->get_state();

    EXPECT_EQ(state.n_cycles, 869);
    EXPECT_EQ(state.nmc_li_neg->DOD_max, 43.33);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 97.61, 1);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 98, 1);
    EXPECT_NEAR(state.day_age_of_battery, 870, 1e-3);
}

TEST_F(lib_battery_lifetime_nmc_test, CyclingCRateMinuteTimestep) {
    double dt_hr = 1. / 60;
    auto steps_per_day = (size_t)(24 / dt_hr);
    model = std::unique_ptr<lifetime_nmc_t>(new lifetime_nmc_t(dt_hr));

    size_t day = 0;
    size_t idx = 0;
    // 90 DOD cycle once per day, slower Crate than above
    std::vector<double> DODs_day = {50., 56.67, 63.33, 70., 76.67, 83.33,
                                    90., 83.33, 76.67, 70., 63.33, 56.67, 50., 43.33, 36.67, 30., 23.33, 16.67,
                                    10., 16.67, 23.33, 30., 36.67, 43.33};

    while (day < 87) {
        for (size_t hr = 0; hr < DODs_day.size(); hr++) {
            double prev_DOD = DODs_day[hr % 24];
            double DOD = DODs_day[hr];
            for (size_t min = 0; min < (size_t)(1. / dt_hr); min++) {
                bool charge_changed = (hr == 7 || hr == 19) && min == 0;
                if (min != 0)
                    prev_DOD = DOD;
                model->runLifetimeModels(idx, charge_changed, prev_DOD, DOD, 25);
                idx ++;
            }
        }
        day ++;
    }

    auto state = model->get_state();

    EXPECT_EQ(state.n_cycles, 86);
    EXPECT_EQ(state.nmc_li_neg->DOD_max, 43.33);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 103, 1);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 100, 1);
    EXPECT_NEAR(state.day_age_of_battery, 87, 1e-3);

    while (day < 870) {
        for (size_t hr = 0; hr < DODs_day.size(); hr++) {
            double prev_DOD = DODs_day[hr % 24];
            double DOD = DODs_day[hr];
            for (size_t min = 0; min < (size_t)(1. / dt_hr); min++) {
                bool charge_changed = (hr == 7 || hr == 19) && min == 0;
                if (min != 0)
                    prev_DOD = DOD;
                model->runLifetimeModels(idx, charge_changed, prev_DOD, DOD, 25);
                idx ++;
            }
        }
        day ++;
    }

    state = model->get_state();

    EXPECT_EQ(state.n_cycles, 869);
    EXPECT_EQ(state.nmc_li_neg->DOD_max, 43.33);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 97.61, 1);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 98, 1);
    EXPECT_NEAR(state.day_age_of_battery, 870, 1e-3);
}

/// There's less accuracy since the degradation coefficients from the first day are lost when doing computation on day 2
TEST_F(lib_battery_lifetime_nmc_test, CyclingEveryTwoDays) {
    size_t day = 0;
    double T = 25.15;
    while (day < 87) {
        for (size_t i = 0; i < 48; i++) {
            size_t idx = day * 48 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 10, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 10, 90, T);
            else if (i == 46)
                model->runLifetimeModels(idx, true, 90, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
        }
        day += 2;
    }

    auto state = model->get_state();

    EXPECT_EQ(state.n_cycles, 43);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 103.29, 1);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 100.6, 0.5);
    EXPECT_NEAR(state.day_age_of_battery, 88, 1e-3);
}

TEST_F(lib_battery_lifetime_nmc_test, CyclingCustom) {
    double dt_hour = 1. / 60.;
    model = std::unique_ptr<lifetime_nmc_t>(new lifetime_nmc_t(dt_hour));

    double T = 25.15;
    double batt_size = 130.715;
    std::vector<double> batt_socs = {0.6382301183891653, 1.6493076437309546, 2.6405066988371018, 3.6243814268059253, 4.5902092656906754, 5.537276369550534, 6.465482309963582, 7.3749733349794235, 8.2660019829734, 9.138814688137575, 9.993597128651608, 10.830466075302441, 11.64948188251311, 12.450665237866838, 13.234012202104015, 13.999506484390148, 14.7471293413935, 15.476867645985173, 16.188720594564995, 16.88270534682058, 17.558861765093354, 18.21725627928446, 18.857984876938083, 19.481175177522754, 20.086987602621562, 20.675615661010895, 21.247285377346614, 21.80225391972889, 22.34080748438742, 22.863258535751346, 23.36994261768155, 23.861215027866926, 24.33744763621049, 24.799025958948885, 25.2463463200218, 25.679812680288865, 26.099832976460753, 26.50681558940696, 26.901166569584355, 27.28328815913386, 27.65358045073707, 28.012442950906102, 28.36026575034239, 28.69742491728352, 29.024286725196664, 29.341208926383324, 29.64853937118584, 29.946614290092207, 30.235757240466647, 30.516279718294943, 30.788482539900386, 31.052654691559447, 31.30907358728673, 31.558008601214805, 31.799722802787638, 32.0344726691287, 32.262507430847855, 32.48406843552182, 32.69938778100084, 32.90868663478787, 33.11217631442703, 33.310055013518465, 33.50250936264339, 33.68971792914681, 33.871852113203936, 34.04907790701138, 34.22155563030684, 34.389439681271774, 34.55287840554154, 34.71201429468242, 34.86698440643105, 35.017920863388355, 35.16495125037349, 35.30819877158805, 35.44778214611104, 35.58381543721496, 35.716407967155405, 35.84566441837207, 35.97168522156549, 36.09456712097441, 36.21440381359885, 36.33128639920696, 36.44530344524474, 36.556540736901276, 36.66508097253917, 36.77100367011039, 36.87438538076204, 36.97529990269615, 37.07381836899749, 37.17000919371826, 37.26393803217118, 37.3556678287853, 37.44525880663598, 37.532768570617044, 37.618252845355975, 37.70176674868333, 37.78336572876357, 37.863105022521175, 37.941037550787726, 38.017211808929126, 38.09167140441246, 38.16445601664159, 38.235602883406166, 38.305148856210295, 38.373133379225386, 38.43960274422883, 38.504613982124965, 38.56823119879698, 38.63051053157458, 38.69149158334519, 38.751202565623885, 38.80966847609, 38.86691390568879, 38.92296131382346, 38.977830597099015, 39.031541627388485, 39.08411893914233, 39.13559718038461, 39.1860236349885, 39.235450991535295, 39.28392075060341, 39.33145651588363, 39.378072385385266, 39.42378166167064, 39.46859655166099, 39.51252655389309, 39.55558023694894, 39.59777047885719, 39.6391208199964, 39.67967057024787, 39.71947214345874, 39.75857343784983, 39.79700123871285, 39.834766722794775, 39.87187793896412, 39.90834320857632, 39.944168339076334, 39.979356655396344, 40.01391286011001, 40.04784892689519, 40.081189769954136, 40.113974536307886, 40.14624536164664, 40.17802959278213, 40.20933919223344, 40.24018079243126, 40.270560944958625, 40.30048437471192, 40.32995319647863, 40.358969121158665, 40.387537660240994, 40.415672574769275, 40.44339872301255, 40.4707472685052, 40.497742532385445, 40.52439679683631, 40.55071652820048, 40.57670725598958, 40.602373253625075, 40.62771651855488, 40.65273775514214, 40.67743903401277, 40.70182703995721, 40.72591602037878, 40.749727126405546, 40.773280532202776, 40.796588153768276, 40.81965602962274, 40.84248854945017, 40.86508959572335, 40.887461487258825, 40.909604966703284, 40.93152055005606, 40.95321061954113, 40.97468158335537, 40.99594505266661, 41.01701514857322, 41.03790275019055, 41.05861397725653, 41.07915267154015, 41.09952203534503, 41.11972429458451, 41.139760307509185, 41.15963008777359, 41.179334043106074, 41.19887448908791, 41.218257074756444, 41.23749076220836, 41.25658476991413, 41.27554525986316, 41.294375711129064, 41.313078554434, 41.331655793852164, 41.350108537271005, 41.36843688443298, 41.38664046085114, 41.40471936885805, 41.42267526316515, 41.44051223634099, 41.45823641103226, 41.47585362739924, 41.49336762665531, 41.510780560679954, 41.52809398454662, 41.5453090385183, 41.56242612386024, 41.57944490233081, 41.596364729412336, 41.6131853553498, 41.629907700980965, 41.64653446367147, 41.663069776757744, 41.67951759785497, 41.69588046259983, 41.712159797575104, 41.72835659394153, 41.74447153944944, 41.76050478706884, 41.776455940260114, 41.792324346366804, 41.80810958784559, 41.82381203153248, 41.839433289905855, 41.85497613105236, 41.870443513459364, 41.88583757237723, 41.901159597556365, 41.91641048355599, 41.931590956286314, 41.946701429121, 41.961741920362556, 41.97671218084647, 41.991611988679075, 42.00644152711234, 42.02120177489677, 42.0358947229234, 42.05052307473019, 42.065089464706844, 42.07959593632828, 42.09404407225953, 42.108435298733816, 42.122771001798235, 42.13705242467245, 42.15128061461479, 42.165456493860546, 42.179581018757645, 42.193655376122194, 42.20768116759027, 42.22166052868772, 42.23559608826813, 42.249490751955946, 42.26334743446188, 42.27713837816191, 42.29089934002617, 42.30463393662216, 42.31834547405129, 42.332037209852444, 42.3457120803904, 42.35937279342345, 42.37302194377258, 42.38666214056514, 42.400296357316904, 42.4139280097816, 42.42756106310154, 42.44119979154348, 42.45484849156266, 42.46851117198971, 42.482191642930175, 42.49589357110413, 42.509620638890446, 42.52337633621471, 42.53716391534838, 42.55098663334553, 42.5648476666575, 42.578750257857244, 42.592698204241934, 42.60669582941053, 42.62074801977647, 42.63485973658907, 42.64903585724771, 42.66328111524842, 42.67760019049173, 42.691997774982084, 42.70647852496305, 42.72104699263489, 42.73571071256038, 42.75047075255228, 42.76533153911778, 42.78029772166491, 42.7953746095828, 42.810568165370725, 42.82588498763488, 42.84133199956381, 42.85691612085542, 42.872644370581234, 42.888523825320476, 42.90456175685053, 42.92076554381603, 42.937142699933446, 42.95370066126905, 42.97039508045506, 42.987293862780554, 43.004418048758744, 43.02175796133399, 43.03932327183855, 43.05712672138897, 43.07518242337152, 43.09350561514408, 43.11211178279895, 43.13101754915783, 43.15024060644688, 43.169799095461265, 43.18971180792712, 43.20999802375181, 43.2306774241184, 43.25177036731004, 43.273298711878184, 43.29528746873297, 43.317767906473506, 43.34097739042566, 43.36477180646982, 43.389180448241596, 43.414235086758005, 43.43996806599238, 43.46641070411895, 43.49359214473229, 43.52153839249222, 43.550271719841604, 43.579810786231235, 43.61017174999977, 43.641370402237904, 43.673424954925345, 43.706358922890296, 43.74020239739046, 43.774989599819435, 43.81075316228358, 43.847518902902074, 43.885304989075635, 43.92412395247989, 43.96398488276339, 44.00489308785854, 44.04684890843068, 44.089847540356075, 44.1338805442989, 44.17893904665167, 44.22501793705062, 44.27211981307095, 44.32015900032668, 44.36926638755004, 44.41948664611002, 44.47086273746732, 44.523426278038805, 44.57719620397072, 44.63218283506171, 44.6883951372308, 44.745991369239555, 44.804825956684816, 44.864891540334646, 44.92617914794334, 44.9886761976348, 45.0521233751226, 45.116779328494324, 45.18267317504517, 45.249848195193145, 45.31835898752611, 45.38824884850435, 45.459585097312754, 45.53240603580085, 45.60696099862254, 45.68303415346723, 45.76062115860927, 45.83969705445072, 45.91750320991167, 45.996755371559175, 46.077523859417546, 46.15985019354453, 46.24378495894821, 46.32937908140793, 46.41669769688666, 46.505823728305465, 46.596846733579234, 46.689877033301805, 46.78501329287311, 46.88233868005746, 46.98190966030265, 47.083789552821955, 47.18802213167002, 47.2946501082766, 47.40373069613849, 47.515316064271786, 47.62946740744893, 47.7462293611306, 47.8656729051615, 47.98787066003995, 48.11289328913549, 48.240851470386, 48.37184580998212, 48.505971133882085, 48.643352052735054, 48.78407126246165, 48.92822871280639, 49.07589001148262, 49.22715025795037, 49.38208418746872, 49.5407144294545, 49.694028470973905, 49.86165029674767, 50.033539887162135, 50.209716133834604, 50.39030261218076, 50.57544327813599, 50.76530588408619, 50.96004669277634, 51.15982679001323, 51.36482516842607, 51.57520504556638, 51.79114556873837, 52.01281615831339, 52.240394306821244, 52.47403910331306, 52.710828145043365, 52.95404809269088, 53.203992894821994, 53.460910090891964, 53.72504524913193, 53.9966918399379, 54.276153904887146, 54.56370643853191, 54.8596796704622, 55.164443110135664, 55.478343348455184, 55.80180304697516, 56.13525111096681, 56.47910425428021, 56.83372427081183, 57.19966323789362, 57.577567861354176, 57.96786959911173, 58.37128265952067, 58.78869501129096, 59.231487677335494, 59.69002690990294, 60.16483580298381, 60.65681151923855, 61.167005249020555, 61.69653879426898, 62.246566119973934, 62.818301872796, 63.41301420530261, 64.0307698669789, 64.6742950672854, 65.34517337056015, 66.04506816526418, 66.77573592972662, 67.53902397716385, 68.33685987185804, 69.17125076499543, 70.04426275752363, 70.95800888719212, 71.91462874488194, 72.91626219185615, 73.9650292347318, 75.0629924620764, 76.21212734892279, 77.41429404071107, 78.67119684960363, 79.9843421676708, 81.35496136211165, 82.78382215405014, 84.27071445543939, 85.81271008963924, 87.397537778774, 88.99508015796246, 90.58953494572276, 92.17877488645475, 93.7615208344107, 95.33646096924679, 96.90217051395193, 98.45710627199676, 99.99974255949638, 98.44375944854968, 96.87813203304158, 95.30492987715769, 93.72565791353955, 92.14172488579133, 90.55444905794775, 88.9650799848994, 87.37482136705717, 85.78496544174456, 84.19661756878267, 82.61108666203873, 81.03015248164768, 79.4611081581862, 77.94905619334071, 76.52134386198219, 75.1802912015844, 73.92448872449079, 72.75204258513092, 71.66121390371596, 70.65062056282733, 69.71933092680871, 68.8669264622803, 68.09355650093536, 67.39999386914329, 66.78769533321062, 66.25878586261732, 65.81636952079498, 65.46439282935052, 65.20773830794953, 65.05227753043894, 65.00506510310694, 64.97099936967187, 64.83263421097872, 64.81646409651974, 64.80615154726928, 64.80093991300959, 64.79916648228881, 64.79826458325422, 64.7974235662399, 64.79658791881168, 64.79627351613338, 64.79598813643871, 64.79567014676995, 64.79545787489722, 64.79526205758314, 64.79508318879124, 64.79491708473269, 64.79476203107978, 64.7946166181488, 64.79447979286584, 64.79435069139066, 64.7942285703027, 64.79411280969539, 64.79400287952693, 64.79389831600115, 64.79379871313836, 64.79370371344774, 64.79361300059756, 64.79352629002683, 64.79344329386385, 64.79336382588397, 64.7932876731092, 64.7932146469636, 64.79314457576059, 64.79307730282872, 64.79301268512653, 64.79295059131212, 64.79289090079479, 64.7928335023427, 64.79277829335743, 64.79272517906315, 64.7926740715856, 64.79262489546433, 64.79257756852678, 64.79253202043228, 64.79248818568458, 64.79244600318283, 64.79240541589368, 64.79236637052216, 64.79232881721133, 64.79229270929872, 64.79225800306322, 64.79222465752159, 64.79219263423904, 64.7921618971514, 64.7921324123965, 64.79210414818118, 64.79207707464118, 64.79205116340897, 64.79202638845209, 64.79200272495521, 64.7919801496938, 64.79195864085915, 64.79193817799695, 64.7919187428196, 64.79190031645278, 64.79188288208047, 64.79186642398521, 64.79185092749823, 64.79183637895389, 64.79182276564572, 64.79181007579166, 64.79179829849885, 64.79178742373034, 64.79177744212356, 64.79176834541595, 64.79176012597733, 64.79175277693534, 64.79174629215356, 64.7917406662165, 64.79173589441275, 64.79173197272459, 64.79172889781746, 64.79172666722089, 64.79172527875151, 64.79172473107585, 64.79172502351803, 64.79172615605789, 64.79172812934965, 64.7917309446563, 64.79173460389643, 64.79173910971762, 64.79174446540993, 64.79175067494302, 64.79175774297727, 64.79176567487846, 64.79177447672939, 64.79178415534557, 64.79179471829471, 64.79180617408848, 64.79181853142796, 64.79183180053369, 64.79184599221998, 64.79186111818395, 64.7918771910392, 64.7918942243537, 64.7919122326906, 64.79193123165507, 64.79195123794051, 64.79197226938338, 64.79199434502235, 64.79201748516138, 64.79204171144146, 64.79206704691495, 64.79209351612714, 64.79212114521019, 64.79214996198044, 64.79217999604427, 64.79221127892211, 64.79224384417341, 64.7922777275843, 64.79231296723604, 64.79234960373475, 64.79238768038311, 64.79242724341276, 64.79246834190924, 64.79251102891503, 64.79255536088885, 64.7926013983482, 64.79264920613826, 64.79269885389992, 64.79275041654321, 64.79280397463731, 64.79285961505795, 64.79291743170144, 64.79297752622347, 64.7930400088162, 64.7931049991604, 64.79317262774798, 64.79324304027288, 64.79331639088113, 64.79339285197726, 64.79347261293098, 64.79355588615313, 64.79364290415643, 64.79373380690572, 64.7938289729038, 64.7939287230717, 64.79403341516091, 64.79414346991022, 64.79425943425198, 64.79438177737413, 64.79451160138217, 64.79464882546523, 64.79479476096294, 64.79495018159423, 64.79512274645799, 64.79530298452504, 64.79553757045171, 64.79576909400123, 64.79601243105476, 64.79630790717763, 64.79661327455281, 64.79704420696496, 64.7976167588771, 64.79839241108044, 64.79964194007975, 64.8010963052534, 64.80612886719366, 64.81407728879644, 64.82467529647087, 64.8386464438821, 64.86610294373317, 64.98855176048411, 65.07167970545264, 65.07259899901435, 65.1430894543299, 65.2868287218341, 65.59127409542617, 65.96206618806171, 66.3940435787198, 66.88416071183944, 67.43007750522507, 68.02992543595579, 68.6822391186151, 69.3859321083429, 70.14026351480189, 70.94479731460729, 71.79935621290367, 72.70397657997786, 73.65886497581484, 74.66435731947799, 75.72088248311435, 76.82893177402741, 77.98903206581488, 79.20172296068128, 80.46753185900185, 81.78693418213088, 83.16026925128303, 84.58752847496925, 86.06774670095722, 87.59693345191975, 89.16074743588489, 90.73132144085785, 92.29779774356422, 93.85797649086084, 95.41010400542322, 96.9523637037377, 98.4828242202782, 99.999567890904, 98.42707521514438, 96.84553118460764, 95.25715514770773, 93.66316306990726, 92.0646870175345, 90.46278743227536, 88.85847017536423, 87.25271193792675, 85.64649487907789, 84.04086515575511, 82.43716231116416, 80.84010991322577, 79.29903316580717, 77.85062297425289, 76.49637035360581, 75.23322872128792, 74.05766070642079, 72.96635517988722, 71.95644415300593, 71.02559871499693, 70.17208615116303, 69.39481549509858, 68.69338015794283, 68.06810103444295, 67.52007122709925, 67.05120139327553, 66.66425688476984, 66.35999551952895, 66.14564595785235, 66.02542534766721, 65.99192354792652, 65.97235003772414, 65.9658141271283, 65.96073140859492, 65.95644203357621, 65.95266186825285, 65.94925020196015, 65.94612147201259, 65.94322606750565, 65.94052584396395, 65.93799375567131, 65.9356085748156, 65.93335322048422, 65.9312137314357, 65.92917855182229, 65.92723771122674, 65.92538266117074, 65.92360601308562, 65.92190130288891, 65.92026285060922, 65.91868562345441, 65.91716514670044, 65.91569742372609, 65.91427886868559, 65.9129062528699, 65.91157665654802, 65.91028743035436, 65.90903616450812, 65.90782066070769, 65.90663890580728, 65.90548906503923, 65.90436942948503, 65.90327835749062, 65.90221448466292, 65.90117647306285, 65.90016308124258, 65.89917315548858, 65.89820562207919, 65.89725947962336, 65.89633379288014, 65.89542768713383, 65.89454030528643, 65.89367091872754, 65.89281880539161, 65.8919832860121, 65.89116372100659, 65.8903595073466, 65.88957007573013, 65.88879488825022, 65.88803343621534, 65.88728523785363, 65.88654983654195, 65.88582679904191, 65.88511571394102, 65.88441619029514, 65.88372785615535, 65.88305035738615, 65.88238335666475, 65.8817265322734, 65.88107957719667, 65.88044219667462, 65.87981411074107, 65.87919505162021, 65.87858476314949, 65.87798299988744, 65.87738952684435, 65.87680411865453, 65.87622655903972, 65.87565665743408, 65.87509419752297, 65.87453898786532, 65.87399084436937, 65.87344959000421, 65.87291505436096, 65.8723870702029, 65.8718654825616, 65.87135014687178, 65.8708409086077, 65.87033762616333, 65.86984016288626, 65.86934838682488, 65.8688621705478, 65.86838132892284, 65.86790580496212, 65.86743548369954, 65.86697025362483, 65.86651000727869, 65.8660546403848, 65.8656040519925, 65.86515814430301, 65.8647168222318, 65.86427999413603, 65.8638475709478, 65.8634194662849, 65.86299559610573, 65.8625758791885, 65.86216023653282, 65.86174859122211, 65.86134086893782, 65.86093701263073, 65.86053693682801, 65.86014057286918, 65.85974785442772, 65.85935871681096, 65.85897309668522, 65.85859092047096, 65.85821213995456, 65.8578366965996, 65.85746453468516, 65.8570955987111, 65.85672983573959, 65.85636719270394, 65.85600761921555, 65.85565106546233, 65.85529748204999, 65.85494682241232, 65.85459912869793, 65.85425427062854, 65.85391220444397, 65.85357288744108, 65.85323627819038, 65.85290233599179, 65.85257102110667, 65.85224229473182, 65.85191611897602, 65.85159245682034, 65.85127120259307, 65.85095239098194, 65.85063598767701, 65.85032195860492, 65.8500102905157, 65.84970093175362, 65.84939385080779, 65.84908901686494, 65.8487863996456, 65.84848596990011, 65.84818766294845, 65.84789148683788, 65.84759741415223, 65.84730541224008, 65.8470154605582, 65.8467275331632, 65.84644160426724, 65.84615764932121, 65.84587564412303, 65.84559556487174, 65.84531738768082, 65.84504109039868, 65.84476665074398, 65.84449404589886, 65.84422324615372, 65.84395424110217, 65.8436870125809, 65.84342154063079, 65.84315780592812, 65.84289578957271, 65.84263547305703, 65.84237683854762, 65.84211986859636, 65.84186454611863, 65.84161091840647, 65.84135890440659, 65.84110848874906, 65.84085965661153, 65.840612393713, 65.84036668627249, 65.84012251803496, 65.83987987977636, 65.83963875994297, 65.8393991479113, 65.83916103411926, 65.83892441590473, 65.83868928461197, 65.83845563884668, 65.83822348068155, 65.83799282473353, 65.8377636847563, 65.83753575604877, 65.83730927363256, 65.83708424794528, 65.83686067232817, 65.83663855748105, 65.83641791274023, 65.8361987446454, 65.83598107010886, 65.83576491134414, 65.8355502978197, 65.83533724827791, 65.83512578360268, 65.83491592082042, 65.83470770391797, 65.83450118527456, 65.83429648026637, 65.83409366745774, 65.83389252807179, 65.83369276062302, 65.8334943140553, 65.8332972186051, 65.83310143239864, 65.83290689086209, 65.83271362368194, 65.83252138615168, 65.8323300954921, 65.83213954181554, 65.83194901729729, 65.83175672255858, 65.83158852736898, 65.83141805865522, 65.83125883325548, 65.83112950607796, 65.83108126058832, 65.83097022402389, 65.83121319387298, 65.83109932482685, 65.83098326515379, 65.83095257653827, 65.83088966931851, 65.83072071342954, 65.8306742848564, 65.8308046907746, 64.98878296194607, 64.0818199920326, 63.121019600438736, 62.11700187329482, 61.06867787733243, 59.98070727672056, 58.85673254800008, 57.69733766233725, 56.50335701547456, 55.27116990806438, 54.00545142501348, 52.70653888952713, 51.375032084102536, 50.00960885375114, 48.610219949243195, 47.17783427648058, 45.711891451588684, 44.20780999992443, 42.66549400005453, 41.086380446342844, 39.49325488857028, 37.88512371581405, 36.26337438996179, 34.631074403879076, 32.994485649023716, 31.347250950913452, 29.692687774306766, 28.0301652964669, 26.363498629891076, 24.699361250779067, 23.037751422285584, 21.378558526702175, 21.378376907844547, 21.378195496903604, 21.37801429543197, 21.377833314893934, 21.37765253719161, 21.377472281516756, 21.37729236447034, 21.377112446118353, 21.376932685071395, 21.376753082587943, 21.376573629782325, 21.376394287182293, 21.376215078355035, 21.376035995583916, 21.37585703124783, 21.375678177285916, 21.375499426542312, 21.375320771075177, 21.375142225015974, 21.37496375816976, 21.374785364403774, 21.374607033693806, 21.374428757674774, 21.37425052831304, 21.374072338366943, 21.373894179420454, 21.373716043642272, 21.373537923161738, 21.37335981001695, 21.373181696479858, 21.373003574743592, 21.372825437089922, 21.37264727587549, 21.37246908337967, 21.3722908521099, 21.37211257443825, 21.37193424301203, 21.37175585029331, 21.371577389050934, 21.371398863988322, 21.37122025601153, 21.37104155809429, 21.37086276312504, 21.37068386412029, 21.370504854236064, 21.370325684071474, 21.370146385905215, 21.369966952673565, 21.369787435766586, 21.369607774040507, 21.36942796105329, 21.36924799061964, 21.3690678564258, 21.368887552175313, 21.275904750754908, 21.146886553386178, 20.991303228006203, 20.81360500568789, 20.61361728232113, 20.393329889403063, 20.1551046541039, 19.898747699333683, 19.623138151357377, 19.32846166545956, 19.018026785443297, 18.690977543997448, 18.34916463779801, 17.98979929817883, 17.61516329422067, 17.22470146182306, 16.81640181334289, 16.389159120146495, 15.94197755144558, 15.476165069367987, 14.988533429839324, 14.478060371025627, 13.945252146450068, 13.391889388313654, 11.813964058968663, 10.2122566843007, 8.5832054864523, 6.930372194202144, 5.263705527734271, 4.597551736754082, 3.9235334799283796, 3.2415355934453167, 2.551363104601508, 1.852258226005624, 1.1456878218311384, 3.5851943081754257e-06, 0.7534034442853439, 1.4965796835834302, 2.230596691579836, 2.9551114556572013, 3.67103698359826, 4.376094576761482, 4.786418841318296, 5.18501573995085, 5.5732593006766535, 5.949236195982803, 6.316214131615393, 6.679182267636853, 7.033280734946739, 7.378693121886398, 7.7119539713346725, 8.037538742282184, 8.35506589127925, 8.66470073317007, 8.970130914816547, 9.27037608916753, 9.565152886899632, 9.854090591184814, 10.138654947160783, 10.414678903410604, 10.683948306806515, 10.94700331385833, 11.202881113294765, 11.452315288232228, 11.694456803510988, 11.930353171813882, 12.158818392801422, 12.376753980991527, 12.58666025248068, 12.787953153173905, 13.488293330593448, 14.184177893714809, 14.875418712629132, 15.560863483134767, 16.250718302328714, 16.931782856442286, 17.479367345654477, 18.34751776712486, 19.20288187142987, 20.046714823833746, 20.880175957206887, 21.704542822772115, 22.5199088572955, 23.208808477462934, 23.93601821812662, 24.657746332347383, 25.37424953055683, 26.086000105320128, 26.793103681285864, 27.49577309299807, 28.194299980408378, 28.88858895479471, 29.578857867205915, 30.264843424557704, 30.946332421457296, 31.623571764394683, 32.29619509984787, 32.96427917325264, 33.627738292559094, 32.63295427205932, 31.63016581899443, 30.787931684868212, 29.936161135601424, 29.07829521296506, 28.214842765312582, 27.346312945438722, 26.47321489649534, 25.594527772793867, 24.71254583820455, 23.827200314631877, 22.938353796855786, 22.04396346065911, 21.14568483103791, 20.243261257583754, 19.33643644690785, 18.42202163084404, 17.50168953686285, 16.574821213094975, 15.637211262329842, 14.691411425730161, 13.736529892897329, 12.76580922290689, 11.783967122569251, 10.800438636289623, 9.817139658124189, 8.833702108360612, 7.850117387525696, 6.86638087098633, 5.882490020824779, 4.8984433783587065, 3.914240076036226, 2.9298795261017125, 3.0646877984791288, 3.144938462349854, 3.1779971471604522, 3.1936999450777943, 3.206553919821458, 3.218656555482699, 3.2303095624024434, 3.2415693449297933, 3.252465999226565, 3.2630221265122343, 3.273258098192667, 3.283191493934159, 3.2928389871687345, 3.302213699550545, 3.3113285712735725, 3.3201954762761927, 3.3288253587623116, 3.337228350088474, 3.345413865622287, 3.353390681621196, 3.3611669982384207, 3.368750493465808, 3.3761483700761032, 3.3833673967034144, 3.3904139439590177, 3.397294016441226, 3.4040132813195103, 3.4105770940677953, 3.416990521688362, 3.423258363841788, 3.429385172127042, 3.435375267713702, 3.441232757519943, 3.446961549104912, 3.452565364389732, 3.458047752325346, 3.4634121005809573, 3.4686616463835405, 3.473799486531606, 3.478828586679544, 3.483751789936671, 3.48857182484599, 3.4932913127735743, 3.49791277476047, 3.5024386378738384, 3.5068712410973735, 3.5112128407904937, 3.515465615742646, 3.519631671857673, 3.5237130464859026, 3.527711712437135, 3.5316295816910253, 3.535468508826815, 3.53923029419519, 3.542916686846652, 3.546529387234821, 3.55007004971284, 3.5535402848344257, 3.556941661478203, 3.5602757088049026, 3.5635439180643713, 3.5667477442608306, 3.5698886076866088, 3.5729678953448163, 3.575986962262024, 3.5789471327077047, 2.996993047707014, 2.4094632366885143, 1.8167396204498212, 1.2158890954010448, 0.6102262127494069, 5.4252812074100254e-06, 0.001347790763898252, 0.001849595276058699, 0.0021924284920386153, 0.0024608068189921205, 0.0026848329946932183, 0.0028789583977257066, 0.0030509090237423094, 0.003205894757452548, 0.003347290111040533, 0.00347746909061153, 0.003598208891151629, 0.0037108610325670677, 0.003816483095009224, 0.003915917031644646, 0.004009851256341183, 0.0040988549798971555, 0.004183404837482677, 0.004263904436713975, 0.004340699858748305, 0.004414089736857253, 0.004484333967543296, 0.004551660252995009, 0.0046162694341521406, 0.004678339648894341, 0.004738029638361171, 0.004795481664703105, 0.004850824023049696, 0.0049041719301328715, 0.004955630151656166, 0.005005293984267518, 0.005053250380016925, 0.005099578911968053, 0.005144352595321207, 0.005187638597446771, 0.005229498851777402, 0.00526999059140079, 0.005309166813721026, 0.005347076687895433, 0.005383765913206928, 0.005419277035115147, 0.005453649724891758, 0.005486921027890775, 0.005519125584641522, 0.005550295828263595, 0.0055804621612066336, 0.005609653113860076, 0.0056378954872451205, 0.005665214481665324, 0.00569163381295609, 0.0057171758177346765, 0.005741861548881821, 0.0057657108623122705, 0.005788742495970113, 0.005810974141862683, 0.005832422511841368, 0.00585310339776711, 0.0058730317266100255, 0.005892221610973448, 0.005910686395479638, 0.005928438699397548, 0.0059454904570389626, 0.0059618529503179755, 0.005977536845601228, 0.005992552223196778, 0.006006908605668008, 0.006020614983970748, 0.006033679841591527, 0.006046111176840766, 0.006057916523445387, 0.00606910296956777, 0.006079677175368678, 0.006089645389213296, 0.0060990134626198065, 0.006107786864031879, 0.0061159706914905194, 0.0061235696842775935, 0.0061305882335912715, 0.006137030392309409, 0.006142899883890122, 0.006148200110457152, 0.006152934160109015, 0.006157104813487583, 0.006160714549638481, 0.006163765551193088, 0.006166259708892173, 0.006168198625478424, 0.006169583618971955, 0.006170415725345107, 0.006170695700608982, 0.006170424022320261, 0.006169600890517988, 0.00616822622809055, 0.006166299680576528, 0.0061638206153987254, 0.006160788120527927, 0.006157201002568097, 0.006153057784255326, 0.006148356701360455, 0.006143095698977459, 0.006137272427182527, 0.006130884236043795, 0.006123928169953937, 0.006116400961263141, 0.0061082990231776865, 0.006099618441889458, 0.006090354967899208, 0.006080504006485679, 0.0060700606072738115, 0.0060590194528471085, 0.00604737484634199, 0.006035120697955121, 0.006022250510293671, 0.006008757362478863, 0.0059946338929148055, 0.005979872280620207, 0.005964464225005955, 0.005948400923977544, 0.005931673050219172, 0.005914270725504205, 0.00589618349286086, 0.005877400287807513, 0.0058579094018416, 0.005837698449842342, 0.005816754331374778, 0.005795063188317307, 0.005772610359220282, 0.00574938032958067, 0.005725356677610171, 0.0057005220150133885, 0.005674857922225432, 0.005648344877495116, 0.005620962179105131, 0.005592687859922264, 0.0055634985933604355, 0.0055333695897109465, 0.0055022744816492246, 0.005470185197537017, 0.005437071820854135, 0.005402902433871752, 0.0053676429435204846, 0.0053312568867413075, 0.005293705212822615, 0.005254946039182433, 0.005214934373975663, 0.0051736218068762245, 0.005130956153917255, 0.005086881056733656, 0.005041335523173134, 0.004994253395526284, 0.004945562770258096, 0.004895185259490436, 0.004843035204927678, 0.004789018718678725, 0.004733032575114098, 0.00467496281915252, 0.004614683401058034, 0.004552054136087064, 0.004486918453808605, 0.004419100676233702, 0.004348402635346808, 0.004274599386233162, 0.004197434323685564, 0.0041166124226544466, 0.0040317928968819465, 0.003942576580873631, 0.0038484885857060585, 0.0037489672020239234, 0.00364333112047686, 0.0035307303709900266, 0.003410130920851367, 0.0032801648187232506, 0.0031389510472101933, 0.0029839935660024865, 0.002811814309417522, 0.0026165388414554147, 0.0023925374551774623, 0.0021238885550554244, 0.0017792317302158776, 0.0012893704652483984, 1.2790343304988285e-06};
    std::vector<double> batt_powers = {0.3829380707534806, 0.6066465152051631, 0.5947194330637775, 0.5903248367813833, 0.5794967033309394, 0.5682402623160037, 0.5569235642479176, 0.5456946150095949, 0.5346171887964741, 0.5236876230985954, 0.5128694643085062, 0.5021213679905894, 0.4914094843264912, 0.48071001321232454, 0.4700081785423952, 0.4592965693717684, 0.44857371420210085, 0.4378429827550929, 0.4271117691479825, 0.4163908513534365, 0.40569385096375504, 0.39503670851474976, 0.3844371585922657, 0.37391418035089197, 0.3634874550593731, 0.35317683503369096, 0.3430018298015193, 0.3329811254294576, 0.32313213879520597, 0.313470630818444, 0.3040104491582113, 0.2947634461113148, 0.2857395650062275, 0.2769469936431274, 0.26839221664383717, 0.2600798161603278, 0.2520121777032211, 0.24418956776781467, 0.23661058810652721, 0.22927295372979065, 0.22217537496201703, 0.2153175001015103, 0.20869367966186134, 0.2022955001647657, 0.19611708474797843, 0.19015332071208035, 0.18439826688160244, 0.17884495134390824, 0.1734857702247563, 0.16831348669706503, 0.1633216929633571, 0.15850329099552604, 0.15385133743645987, 0.14936100835693714, 0.14502852094378854, 0.1408499198047294, 0.13682085703158087, 0.1329366028044687, 0.12919160728750154, 0.1255793122723092, 0.12209380778358786, 0.11872721945494986, 0.11547260947504452, 0.11232513990214142, 0.109280510434375, 0.10633547628455024, 0.10348663397737097, 0.10073043057905341, 0.09806323456195075, 0.0954815334846158, 0.09298206704926971, 0.09056187417447228, 0.08821823219117075, 0.08594851272883085, 0.08375002471388453, 0.08161997466244332, 0.0795555179643578, 0.07755387073008899, 0.07561248191614525, 0.07372913964544447, 0.07190201557475417, 0.07012955136496049, 0.06841022762275822, 0.06674237499401448, 0.06512414138283056, 0.06355361854282052, 0.062029026391080246, 0.060548713160557134, 0.05911107978089765, 0.05771449483254746, 0.05635730307185611, 0.05503787796855513, 0.053754586710504676, 0.052505858388730516, 0.05129056484344904, 0.05010834199651049, 0.04895938804823713, 0.04784357625465081, 0.046759516960020064, 0.04570455488493715, 0.04467575729008411, 0.04367076733757406, 0.042688120058835995, 0.041727583682571796, 0.04079071380914605, 0.03988161900215964, 0.03900674273777011, 0.038170330003299315, 0.03736759966666286, 0.03658863106245293, 0.03582658936731286, 0.03507954627976171, 0.034347257759365785, 0.03362844488089382, 0.032921569965422476, 0.03222661817377891, 0.03154638705239716, 0.030886944745457338, 0.03025587276242602, 0.02965641392817401, 0.029081855440959616, 0.028521459168228003, 0.027969521701073558, 0.02742556577131621, 0.026888933994305374, 0.026358001339351244, 0.02583220983360803, 0.025314145145041763, 0.024810204683620597, 0.024329850150977557, 0.023880943926614825, 0.023460776634747, 0.02305668051790612, 0.022659290449247985, 0.02226672970169718, 0.021879161767416576, 0.02149507830010249, 0.021112989792104058, 0.020733722828293864, 0.02036164007119225, 0.020004505835463572, 0.019670859812343088, 0.01936249520334761, 0.019070538681390126, 0.018785759670876735, 0.018504960118785416, 0.01822809151651553, 0.01795405785207327, 0.017681293060117103, 0.017409554808113153, 0.017141123449490113, 0.016880948717066892, 0.016635688946055844, 0.016409127295686377, 0.016197158328240598, 0.01599255867061479, 0.015791838818593657, 0.015594436673554406, 0.015399598581390226, 0.015205958957977285, 0.015012741952452962, 0.014820767322467955, 0.014632803566758655, 0.014453388253041235, 0.014286663616151093, 0.01413204347843008, 0.013984572939395881, 0.013840725512771278, 0.013699511896549202, 0.0135606277640018, 0.013423134921386944, 0.013286087666765513, 0.013149350011760081, 0.013014041691135432, 0.012882578288642655, 0.01275808158683639, 0.012642057544064871, 0.012532560970485494, 0.012426736239687201, 0.012323216570266757, 0.012221618283020543, 0.012121355543781605, 0.012021607754897832, 0.01192186815874323, 0.01182237319958308, 0.011724267589197033, 0.011629551401213373, 0.011540212471242487, 0.011456404623557888, 0.011376293969508578, 0.011298270759641574, 0.011221705983063598, 0.011146343650988103, 0.011071646051396069, 0.01099700829727968, 0.01092214585099151, 0.010847344804242539, 0.010773536584352032, 0.010702183905602469, 0.010634504814854557, 0.010570329820281738, 0.010508399553741386, 0.010447760414878961, 0.01038805432009518, 0.010329032383101896, 0.0102702512052642, 0.01021126708243344, 0.010151896249005515, 0.010092375562575358, 0.010033407378793505, 0.009976057614402354, 0.00992118785185809, 0.009868692658429764, 0.009817718847013728, 0.009767600985264415, 0.00971807781994562, 0.009668967304842492, 0.00961994857173641, 0.009570691914859115, 0.009521043664113993, 0.009471144887366384, 0.00942146621223004, 0.009372755024117465, 0.009325704687999052, 0.009280429444299406, 0.00923643535081141, 0.009193215107580456, 0.009150531599874927, 0.00910828363828622, 0.00906628370090274, 0.009024294745032512, 0.008982156290444047, 0.008939884699650584, 0.008897723060059703, 0.008856148670753185, 0.008815768816080339, 0.00877701108416722, 0.008739833986087882, 0.008703882972958507, 0.00866888155884296, 0.008634735884669974, 0.008601421838752368, 0.00856885372462208, 0.008536913965501245, 0.008505527547545914, 0.008474714938359027, 0.008444614418826641, 0.008415474880943807, 0.008387616658566296, 0.008361335748341999, 0.008336798212789747, 0.00831400950364687, 0.008274566220118122, 0.008256577118656069, 0.008240757957689871, 0.008226922457578278, 0.00821504148078553, 0.008204922322872432, 0.008196427819920585, 0.008189490209581995, 0.00818411807563299, 0.008180530051149234, 0.008178991478918813, 0.008179831992063201, 0.00818323706526013, 0.008189220011600145, 0.008197608256327638, 0.00820828256437713, 0.008221156904471406, 0.008236240671884908, 0.008253418394658277, 0.008272547480299949, 0.00829363079838823, 0.008316619987279754, 0.008341554719944877, 0.008368767830911632, 0.00839857510125975, 0.00843131421966146, 0.008467030087657701, 0.008505672395282136, 0.008547154800528341, 0.008591445146080429, 0.008638550694316412, 0.008688449988673911, 0.008741080603202108, 0.008798231955394635, 0.008856023995242204, 0.008916471939400115, 0.008979709528375865, 0.009046132750834582, 0.009116133472847716, 0.009190093358595643, 0.009268207157460186, 0.009350472775067576, 0.00943694983558288, 0.009527672843641788, 0.009622758918137488, 0.00972227217940031, 0.009826293670548597, 0.009934776801465622, 0.010016651511702985, 0.0101392693954046, 0.01027451158701368, 0.010403947545243795, 0.01053918630283533, 0.010682069730351225, 0.010833421189635106, 0.010993915063643288, 0.01116370059301349, 0.011343459815437459, 0.01153383437352367, 0.01173509340873794, 0.011947627479610978, 0.012171729494914086, 0.012407640220063337, 0.012655765915083909, 0.012917006740991627, 0.013193254112976003, 0.013488262644422417, 0.013925690371397167, 0.014276649626594147, 0.014645185063172509, 0.015032783109949663, 0.015439787540728772, 0.015865582876048636, 0.01630886436810541, 0.01676774865606343, 0.017239996409730767, 0.0177234398338849, 0.018216578261227703, 0.018719191342981223, 0.019232731612574938, 0.019760380779076908, 0.02030608470020152, 0.0208723214574906, 0.021458137478590492, 0.02205944437120567, 0.022671651704238772, 0.023291378042653323, 0.02391655817021253, 0.024544923057195703, 0.025173492343390128, 0.02579917915534114, 0.026419802365801322, 0.027035101411771068, 0.02764733423947926, 0.028261125612300406, 0.028823512353546323, 0.02946443233412146, 0.03013215513609919, 0.0308256548144858, 0.031538124342998956, 0.03226195555925461, 0.03299197865470112, 0.03372738130156281, 0.03455773920535407, 0.03530075246726707, 0.036039350190006814, 0.03677256456531648, 0.03749822981499437, 0.03806830649278835, 0.03879357202314459, 0.03953630793061351, 0.04030501208889395, 0.04110647539988931, 0.04193391658704777, 0.042801749285154574, 0.0436925630929603, 0.044732977693124835, 0.0456438929069223, 0.04655220308533339, 0.047445537504978326, 0.046683693276678206, 0.047551296988619804, 0.048461092715130674, 0.04939580047629484, 0.05036085924231882, 0.051356473475937546, 0.05239116928734569, 0.0534756188513951, 0.054613803164372136, 0.05581817983365594, 0.0570817557428944, 0.05839523231071087, 0.059742588147228555, 0.06112793551169257, 0.06253954730895349, 0.06397678596405147, 0.0654483527172543, 0.0669512208800789, 0.0684908059063982, 0.07005717220911696, 0.07166612641865337, 0.0733186529271795, 0.0750135774574295, 0.07677490875041737, 0.07859660375778561, 0.08047519434008876, 0.08242855131189274, 0.08443152583606661, 0.08649447020696088, 0.08859677920584902, 0.09075614788075914, 0.09296035771112313, 0.09517814519158505, 0.09198842491175029, 0.10057309546436792, 0.10313375424879254, 0.10570574800359665, 0.10835188700779544, 0.11108439957325314, 0.11391756357023144, 0.11684448521420261, 0.11986805834224992, 0.12299902704781517, 0.12622792628429577, 0.129564313903309, 0.13300235374512287, 0.13654688910482968, 0.14018687789519915, 0.14207342503828915, 0.1459319685886303, 0.14996688127878488, 0.15415031764209136, 0.15848109494408769, 0.16298795448369463, 0.16767723896965986, 0.1725315201869733, 0.1775839391582898, 0.18285806380418926, 0.18834014299182728, 0.19407581911209598, 0.20006883839510145, 0.20631188598816358, 0.21277200991907486, 0.21956338024919259, 0.22674277407644366, 0.23418104265465223, 0.24204783624547685, 0.2504474110622923, 0.2656755996268333, 0.27512353954058094, 0.2848853358486368, 0.29518542975296164, 0.30611623786930703, 0.31772012714917885, 0.33001639542308303, 0.3430414516933563, 0.35682739950408154, 0.37065339700588784, 0.3861151201840155, 0.402526981964969, 0.4199368768225364, 0.43840065867757316, 0.45797282846244275, 0.47870153681663896, 0.5006345358825534, 0.5238071955170301, 0.5482476778012064, 0.5739719146140113, 0.6009800681846423, 0.6292602257254989, 0.658777936406877, 0.6894809321079526, 0.7213000150730952, 0.7541416853356331, 0.7878871908404264, 0.8223715166646295, 0.8573164751632146, 0.8921353808336581, 0.9251973805200128, 0.9508966134809792, 0.9585254275132022, 0.9566728726562905, 0.953543964439316, 0.9496475687736898, 0.9449640809017656, 0.9394257268232038, 0.9329614548270156, 0.9255817725039642, -0.9335898665677028, -0.9393764493045423, -0.9439212935300231, -0.9475631781705548, -0.9503598166486149, -0.9523654967058317, -0.953621443828691, -0.9541551707050191, -0.953913555187248, -0.9530087237768073, -0.9513185440460554, -0.9485605082343012, -0.9414265940765719, -0.9072311789069706, -0.8566273988147945, -0.8046315962383601, -0.7534814862558525, -0.703467683615595, -0.6544972088486645, -0.6063560045328675, -0.5587737816108561, -0.5114426787167238, -0.46402197680665214, -0.41613757907493354, -0.36737912155929137, -0.3173456823556747, -0.26544980509310273, -0.21118601486637065, -0.15399271284029997, -0.0932764665060482, -0.02832745639890318, -0.02043944006074933, -0.08301909521559685, -0.009702068675119187, -0.00618752954998161, -0.0031269805555385164, -0.001064058432188689, -0.0005411394204841564, -0.0005046102083121002, -0.0005013884566534168, -0.00018864160671656119, -0.00017122781653209588, -0.000190793800983338, -0.00012736312337578785, -0.00011749038818017866, -0.00010732127487086673, -9.966243486228603e-05, -9.303219147293755e-05, -8.724775832711097e-05, -8.209516951330055e-05, -7.746088484539867e-05, -7.327265249305763e-05, -6.945636412978868e-05, -6.595810080345804e-05, -6.273811520033062e-05, -5.9761717405443264e-05, -5.69998141050883e-05, -5.442770984289717e-05, -5.20263421686301e-05, -4.979769752501184e-05, -4.7680787658108954e-05, -4.5691664591212365e-05, -4.3815687100204044e-05, -4.204272153363284e-05, -4.036375887620895e-05, -3.877062103936247e-05, -3.7256288374806385e-05, -3.5814310127191105e-05, -3.443907098594505e-05, -3.312539088965261e-05, -3.1868576307013105e-05, -3.066448626322037e-05, -2.950567249382084e-05, -2.8396162257103384e-05, -2.7328856443388776e-05, -2.6300848356820932e-05, -2.530950077585701e-05, -2.4352373216771822e-05, -2.3427222649556745e-05, -2.2531986231877755e-05, -2.1664747293215306e-05, -2.082374102780166e-05, -2.0007324719231938e-05, -1.9213969255818737e-05, -1.8442252317830702e-05, -1.7690852676248217e-05, -1.6958528923063485e-05, -1.624412371853632e-05, -1.5546739062705e-05, -1.4864973867865547e-05, -1.4198097857581744e-05, -1.3545156576401486e-05, -1.29053005267612e-05, -1.2277717044329596e-05, -1.1661106149335567e-05, -1.1055819815401564e-05, -1.0460623125581737e-05, -9.87485688528122e-06, -9.297891922475278e-06, -8.729126340289781e-06, -8.167984631031697e-06, -7.613912170914977e-06, -7.0663754119997094e-06, -6.524860839796029e-06, -5.988963788971529e-06, -5.4580243136771195e-06, -4.93166289744264e-06, -4.409424922394349e-06, -3.8908688043902054e-06, -3.3755619583501657e-06, -2.863081986050571e-06, -2.353012620665043e-06, -1.8449440211779263e-06, -1.3383576717265027e-06, -8.330813526959018e-07, -3.2860512772399165e-07, 1.75465575421867e-07, 6.795241913382119e-07, 1.1839753253991306e-06, 1.6891842450071392e-06, 2.1955443505386356e-06, 2.703492993155333e-06, 3.213415661481616e-06, 3.7257201135884183e-06, 4.240820818061898e-06, 4.7591409866738055e-06, 5.281110826048778e-06, 5.807169980652002e-06, 6.33776975145445e-06, 6.873476534529404e-06, 7.4144039553758825e-06, 7.961463704772736e-06, 8.515012047278401e-06, 9.075578647238575e-06, 9.643713428790056e-06, 1.021998896861259e-05, 1.0805002413757644e-05, 1.1399378938205635e-05, 1.2003771545109386e-05, 1.261886599063e-05, 1.3245383650306166e-05, 1.3884083681491513e-05, 1.4535768318485242e-05, 1.5201284374892984e-05, 1.588152757866193e-05, 1.65774500972933e-05, 1.729006242560559e-05, 1.802043856583344e-05, 1.8769726974658666e-05, 1.953915105166227e-05, 2.033004680533219e-05, 2.114379131467969e-05, 2.198189949707958e-05, 2.2845989286655253e-05, 2.3737818062664833e-05, 2.4659098144590913e-05, 2.5612203749570734e-05, 2.659918456911586e-05, 2.7622475875018604e-05, 2.8684674313228883e-05, 2.978865727281899e-05, 3.093758622729648e-05, 3.2134856746405436e-05, 3.3384252648389566e-05, 3.468998635849735e-05, 3.605671348586373e-05, 3.7489555918922054e-05, 3.8994206792894535e-05, 4.057715280853851e-05, 4.224751521910693e-05, 4.401036521464914e-05, 4.587665795445977e-05, 4.7856572499174244e-05, 4.99639335601262e-05, 5.221080225349944e-05, 5.4541649840322516e-05, 5.709959911376825e-05, 5.9850101015428e-05, 6.281525379459415e-05, 6.60328498544509e-05, 6.957860532388751e-05, 7.340587356163356e-05, 7.789440508328199e-05, 8.233445010449866e-05, 8.756129889325675e-05, 9.325237905105871e-05, 0.0001035389185122088, 0.00010814284049893913, 0.00014075155627323293, 0.00013891412997283923, 0.00014600223238244817, 0.00017728567398900253, 0.00018322042536404688, 0.0002585594475535976, 0.0003435311475387928, 0.0004653913222579068, 0.0007497173998489336, 0.0008726191044281602, 0.0030195371644057014, 0.0047690529619161355, 0.006358804604902896, 0.008382688446964146, 0.016473899910884906, 0.07346929005079712, 0.04987676698134167, 0.000551576137260618, 0.04229427318954885, 0.08624356050273226, 0.1826672241554609, 0.22247525558153392, 0.2591864343950641, 0.2940702798719829, 0.3275500760315839, 0.35990875843863007, 0.3913882095957957, 0.4222157938368824, 0.452598843875593, 0.4827202798834502, 0.5127353389780241, 0.5427722202447008, 0.57293303750239, 0.6032954061980946, 0.6339150981820034, 0.6648295745480408, 0.6960601750726712, 0.7276145369200434, 0.7594853389925401, 0.7916413938776052, 0.8240010414914862, 0.8563555342119398, 0.8881309355929717, 0.9175120505777186, 0.938288390379275, 0.9423444029839847, 0.9398857816240106, 0.9361072483781582, 0.9312765087376305, 0.9253558189888802, 0.9182763099244846, 0.9100462023827586, -0.9434956054553411, -0.9489264183216519, -0.953025622139534, -0.9563952466798646, -0.9590856314232531, -0.9611397511550654, -0.9625903541462645, -0.9634549424620761, -0.9637302353089021, -0.9633778339932504, -0.9622217067541682, -0.9582314387626054, -0.9246460484507559, -0.8690461149321532, -0.8125515723878379, -0.7578849793903262, -0.7053408089198486, -0.6547833159197503, -0.605946616128338, -0.5585072628049834, -0.5121075382999272, -0.4663623936382503, -0.42086120229303403, -0.37516747409950346, -0.3288178844058047, -0.28132190029380166, -0.23216670510300352, -0.18255681914409927, -0.12860973700554618, -0.07213236611065467, -0.020101079843984498, -0.011744106120997466, -0.003921546357066542, -0.003049631119581351, -0.002573625010791995, -0.0022680991935801987, -0.00204699977516589, -0.0018772379680761682, -0.001737242703718418, -0.0016201341245629503, -0.001519252975121863, -0.001431108512969377, -0.0013532125983514852, -0.0012836934286550798, -0.0012211077675705127, -0.0011645043568554762, -0.0011130300331108182, -0.0010659888506097928, -0.0010228261175359427, -0.0009830713673308743, -0.0009463362923876182, -0.0009122860518806449, -0.0008806337841286398, -0.0008511330237938697, -0.0008235694888958339, -0.0007977577926315063, -0.0007735357156738796, -0.0007507595072321349, -0.0007293022797424184, -0.0007090529397190402, -0.0006899044603039841, -0.0006717813319936415, -0.0006546431961146432, -0.0006383236960747705, -0.0006228069595108413, -0.0006080350916255364, -0.0005939554518400018, -0.0005805200450982831, -0.0005676854729368141, -0.0005554120453866505, -0.0005436634472116956, -0.0005324291078939256, -0.0005216319347585261, -0.0005112680010029689, -0.0005013116271225653, -0.0004917390027368479, -0.0004825281954146948, -0.0004736589692994652, -0.00046511248735781583, -0.00045687122035022095, -0.00044891901642400884, -0.000441240786418761, -0.0004338224994322827, -0.00042665105992624496, -0.0004197141869241114, -0.0004130004832663473, -0.0004064992609053022, -0.0004002004322276251, -0.00039409463418790174, -0.00038817304541937455, -0.0003824283126108622, -0.0003768515595064686, -0.0003714354718712213, -0.0003661730818177597, -0.0003610579565897642, -0.00035608382519615765, -0.0003512449132470862, -0.0003465357682392288, -0.0003419409627359921, -0.0003374759460175609, -0.0003331257939260447, -0.0003288860969072305, -0.0003247526184275698, -0.0003207213852849274, -0.00031679049415589243, -0.00031295258410845054, -0.0003092014132238265, -0.00030554295775085167, -0.00030196946594296625, -0.0002984779655530362, -0.00029506563613970933, -0.0002917297655554551, -0.0002885049742791871, -0.0002853143757321608, -0.00028219275683962093, -0.0002791380441199592, -0.0002761478069715563, -0.00027322013562009894, -0.00027035303466418155, -0.00026754461296515, -0.00026479324201593776, -0.0002620968567228198, -0.0002594539122078774, -0.00025686279700074384, -0.000254322106763926, -0.00025183014959960466, -0.0002493855926692832, -0.000246987185664371, -0.0002446333698351214, -0.00024231378349620736, -0.00024004548087219388, -0.00023781837453442278, -0.0002356310641207923, -0.00023348256928691096, -0.00023137207466408834, -0.00022930572778392006, -0.00022726830905916298, -0.00022526601219422882, -0.0002232971478773316, -0.0002213615836468072, -0.00021945778211517346, -0.00021758582058734545, -0.00021574409224837536, -0.0002139322511238244, -0.0002121500465985005, -0.00021039578179182775, -0.00020861622782496523, -0.00020691484080953443, -0.000205239709925617, -0.0002035902009051819, -0.00020196554960145776, -0.00020036531832552193, -0.0001987889302367876, -0.00019723582407775453, -0.00019570545262717886, -0.0001941972925765502, -0.00019275253551585876, -0.00019128696583082375, -0.00018984198210756118, -0.0001884174423917207, -0.0001870008526792245, -0.00018561525638025901, -0.00018424856664625428, -0.00018290036483079806, -0.000181570330730361, -0.0001802578464235072, -0.00017898417011035842, -0.00017770566545991933, -0.00017644361050353017, -0.0001752011463995061, -0.00017397100823554867, -0.00017275643610306342, -0.00017155733667843315, -0.0001703729667154576, -0.0001692031180064768, -0.00016804754985469286, -0.00016690631363996475, -0.00016577836836273043, -0.00016466379190082452, -0.0001635629061586978, -0.00016247984615813787, -0.0001614030299978712, -0.00016033711182492113, -0.00015928316912868466, -0.00015824082066595365, -0.00015720981230138987, -0.00015618990845818906, -0.00015518070469121577, -0.00015418196981304008, -0.0001531934856795706, -0.00015217662632426246, -0.00015120839897113747, -0.0001502493935570052, -0.0001492992815468448, -0.0001483577381398683, -0.00014742446332698217, -0.0001465009415404413, -0.00014558295417875736, -0.0001446718990528684, -0.00014376721801102275, -0.0001428682742259161, -0.00014197092771410356, -0.00014107877466019522, -0.00014018745816566974, -0.00013929489807288186, -0.0001383935678040349, -0.00013748398531771564, -0.0001367572235150219, -0.0001358894487000732, -0.00013501541133365498, -0.0001341453692546329, -0.00013326890724027794, -0.0001323868434536843, -0.00013150085586582816, -0.0001306047208917264, -0.00012969525777867892, -0.00012876811362757677, -0.00012782972400710943, -0.00012687880410290156, -0.0001259176682945923, -0.0001249301404161948, -0.00012391118498637923, -0.00012282300385431578, -0.00012168768410664777, -0.00012068363051955726, -0.00011986046818177585, -0.00011906793955276007, -0.00011825726904625811, -0.00011747172279521501, -0.00011672492083780865, -0.00011596030700742547, -0.00011534251707064721, -0.00011477439465673539, -0.00011433220483702152, -0.00011431470986076173, -0.00011537684212242816, -0.00010091711266065652, -0.00010228122714533866, -9.553523873891723e-05, -7.759630540874696e-05, -2.894729267066022e-05, -6.662193753948894e-05, 0.00014578191056738914, -6.8321426562564e-05, -6.96358027163784e-05, -1.841316818921673e-05, -3.7744330742130836e-05, -0.00010137353225269573, -2.7857142771668366e-05, 7.824355204338647e-05, -0.5052130372959918, -0.5441777819469659, -0.5764802349551922, -0.6024106362852322, -0.6289943975763048, -0.6527823603659979, -0.6743848372311684, -0.6956369313965738, -0.7163883881164878, -0.7393122644449936, -0.7594310898294059, -0.7793475212906977, -0.7989040832536233, -0.8192539382097066, -0.8396333427036511, -0.859431403656436, -0.8795656949340188, -0.9024488709974273, -0.9253895999208126, -0.947468132225887, -0.9558753346624198, -0.9648787036526065, -0.9730495955102317, -0.9793799916485001, -0.9819532529120858, -0.9883408188650329, -0.9927379059628829, -0.9975134867027914, -0.9999999999443661, -0.9984824274660761, -0.9969658970949626, -0.9955157373489157, -0.00010897131345044318, -0.00010884656343464687, -0.00010872088185087599, -0.00010858832169318679, -0.00010846662026414502, -0.00010815340378382855, -0.00010795022671934304, -0.00010795101006070627, -0.00010785662704522386, -0.00010776148894258256, -0.00010767168223939686, -0.00010760555888874511, -0.00010752529522578312, -0.0001074496615397226, -0.00010737860052219969, -0.00010731237601382403, -0.0001072504450316848, -0.0001071932791479753, -0.00010712763438983832, -0.00010708010659813751, -0.00010703625845941003, -0.00010699842484974811, -0.00010696561028885701, -0.00010693761590506639, -0.00010691396652597529, -0.0001068953667609836, -0.00010688146577788536, -0.00010687228718824111, -0.00010686788573982535, -0.00010686812112348129, -0.0001068730406280876, -0.00010688259106802715, -0.00010689672752794583, -0.00010691549635755223, -0.00010693876072996264, -0.0001069666018575271, -0.00010699885459756288, -0.00010703563010115977, -0.00010707674429475318, -0.00010711503643085292, -0.0001071647849440862, -0.00010721874921199666, -0.00010727698041492669, -0.00010733940171862298, -0.00010740592940334176, -0.00010750209762041228, -0.00010757889862600184, -0.00010765993785226561, -0.00010771014305615797, -0.00010779703451740396, -0.00010788779119520627, -0.00010798225905855018, -0.00010808051517199763, -0.00010818254915987544, -0.05578968085111027, -0.07741091842010514, -0.0933499952268523, -0.10661893338985501, -0.11999263401892309, -0.1321724357497111, -0.14293514117836095, -0.15381417286100038, -0.16536572878464667, -0.17680589153756107, -0.18626092800862737, -0.19622954486637492, -0.20508774371853003, -0.21561920377037966, -0.2247816023737592, -0.234277099437437, -0.24497978908696644, -0.2563456159167061, -0.2683089412194167, -0.2794874892454266, -0.29257898371606383, -0.30628383528708536, -0.31968493474420306, -0.3320176548807156, -0.9467551976058639, -0.9610244247996452, -0.9774307187079067, -0.9916999753489634, -0.9999999998795909, -0.3996922745869822, -0.40441095409428895, -0.40919873188870604, -0.4141034933051529, -0.4194629271563993, -0.42394224250355944, -0.6874105419813112, 0.45203991545485506, 0.44590574357908513, 0.4404102047980771, 0.4347088584466526, 0.42955531676486836, 0.42303455589816696, 0.24619455873432142, 0.23915813917976478, 0.2329461364357152, 0.22558613718392342, 0.22018676137978666, 0.21778088161310905, 0.2124590803861646, 0.2072474321640276, 0.19995650966919765, 0.1953508625687386, 0.1905162893984731, 0.18578090513472487, 0.18325810898811773, 0.18014710461082284, 0.17686607863949202, 0.17336262257134216, 0.17073861358581321, 0.16561437375012436, 0.16156164203777923, 0.15783300423131869, 0.1535266796620939, 0.149660504962709, 0.14528490916748565, 0.14153782098196863, 0.13707913259275434, 0.13076135291429403, 0.12594376289372294, 0.12077574041616676, 0.42020410645195666, 0.4175307378730469, 0.4147444913488242, 0.4112668623036112, 0.4139128915165987, 0.4086387324683695, 0.3285506935275461, 0.5208902528824605, 0.5132184625832311, 0.5062997714425587, 0.5000766800241135, 0.494620119339365, 0.48921962071425934, 0.4133397721006911, 0.43632584439843763, 0.4330368685326902, 0.4299019189258978, 0.4270503448582029, 0.4242621455796708, 0.4216016470275544, 0.4191161324464124, 0.41657338463202936, 0.41416134744695315, 0.411591334411301, 0.40889339813998415, 0.4063436057626626, 0.4035740012721402, 0.40085044404309067, 0.3980754715841038, -0.5968704122996348, -0.6016730718387063, -0.5053404804755008, -0.5110623295598457, -0.5147195535815863, -0.5180714685912614, -0.5211178919240914, -0.5238588293658019, -0.5272122742206503, -0.5291891607533625, -0.531207314143374, -0.5333079106654255, -0.5366342017177793, -0.5389671777724916, -0.5414541440722641, -0.5440948864053148, -0.5486488896380588, -0.5521992563884864, -0.5561209942604937, -0.5625659704588529, -0.5674799019595805, -0.5729289196994692, -0.5824324019940348, -0.5891052602023563, -0.5901170917675479, -0.5899793868990321, -0.5900625298579179, -0.5901508325007225, -0.5902419099233911, -0.5903345100967028, -0.5904279854794157, -0.5905219813932598, -0.5906163299604803, 0.0808849634266772, 0.04815039832266306, 0.019835210886586793, 0.009421678750632798, 0.007712384846424548, 0.007261581396971798, 0.006991804152072464, 0.006755869516636987, 0.006537992578288839, 0.006333676371626992, 0.0061415830084855785, 0.005960037445120718, 0.005788495940969882, 0.005624827429311421, 0.005468923034041517, 0.005320143001796793, 0.005177929491895605, 0.005041794795921788, 0.004911309320511529, 0.004786089599569051, 0.0046657899705584, 0.0045500971366557235, 0.0044387259664005075, 0.0043314159766095536, 0.004227928353584895, 0.004128043489547927, 0.004031558927192769, 0.003938287649192997, 0.0038480565725624426, 0.003760705292277396, 0.0036760849713742745, 0.0035940573522178533, 0.00351449388396609, 0.0034372749512027257, 0.0033622891711126627, 0.003289432761589939, 0.003218608953587555, 0.003149727481770677, 0.003082704089060075, 0.0030174600889830675, 0.002953921954496709, 0.0028920209458121165, 0.002831692756770264, 0.002772877192357529, 0.0027155178682409806, 0.0026595619343405936, 0.0026049598160919544, 0.0025516649715107255, 0.0024996336692361927, 0.00244882477715669, 0.0023991995709588573, 0.0023507215525536054, 0.0023033562816927706, 0.0022570712212435216, 0.0022118355910961105, 0.0021676202331200934, 0.002124397487030796, 0.0020821410731696314, 0.002040825986484923, 0.0020004283962384924, 0.0019609255558996523, 0.0019222957180938403, 0.0018845180556852663, 0.0018475725951428122, 0.0018114401505432282, 0.00177610226762621, -0.349172451000196, -0.3525178866108818, -0.3556341697429978, -0.36051031502904773, -0.363397729590765, -0.3661324724807235, 0.0008054192897810352, 0.00030108270746579064, 0.00020569992976010278, 0.00016102699634680813, 0.00013441570559765654, 0.0001164752419984848, 0.00010317037579070921, 9.299144040842612e-05, 8.483721233642935e-05, 7.810738792744729e-05, 7.244388051000284e-05, 6.759128503619958e-05, 6.337323765314207e-05, 5.966036216994608e-05, 5.6360535007395146e-05, 5.340223432378594e-05, 5.072991474219663e-05, 4.8299759730301076e-05, 4.6077253412723104e-05, 4.403392705806079e-05, 4.214653860485707e-05, 4.039577146476845e-05, 3.876550888849871e-05, 3.72421290400024e-05, 3.581399387521507e-05, 3.4471216000696376e-05, 3.320541520388715e-05, 3.2008744446216676e-05, 3.087493311065402e-05, 2.9798299763836788e-05, 2.8773837647000814e-05, 2.7797119368356936e-05, 2.6864210209878403e-05, 2.5971601473619157e-05, 2.51161527969456e-05, 2.429504397286993e-05, 2.350573359124374e-05, 2.2745924704000237e-05, 2.2013535386497394e-05, 2.1306673344766594e-05, 2.0623614066032664e-05, 1.996278199969392e-05, 1.9322734250945498e-05, 1.8702146373945728e-05, 1.8099799966724374e-05, 1.751457179316082e-05, 1.6945424232308284e-05, 1.639139685358616e-05, 1.585159897610039e-05, 1.532520306895912e-05, 1.4811438890263072e-05, 1.4309588260407328e-05, 1.381898039699849e-05, 1.3338987737989146e-05, 1.2869022189808476e-05, 1.240853175818575e-05, 1.1956997508631558e-05, 1.1513930821072718e-05, 1.107887090687115e-05, 1.0651382554035145e-05, 1.0231054788263903e-05, 9.817496170952629e-06, 9.410337373620161e-06, 9.009226761119403e-06, 8.613829686648476e-06, 8.22382718567135e-06, 7.838914776610808e-06, 7.4588013537994346e-06, 7.083208167143034e-06, 6.711867877909952e-06, 6.344523685135976e-06, 5.980928511469051e-06, 5.620844248715014e-06, 5.264041052154863e-06, 4.910296680205156e-06, 4.5593958773691904e-06, 4.211129793436091e-06, 3.8652954362161705e-06, 3.5216951538645074e-06, 3.1801361457599414e-06, 2.840429996762319e-06, 2.5023922328883556e-06, 2.165841896394053e-06, 1.830601138719385e-06, 1.4964948255142292e-06, 1.1633501579174069e-06, 8.30996302392035e-07, 4.992640302707847e-07, 1.6798536481276483e-07, -1.6300676663643935e-07, -4.938788746577087e-07, -8.247972496450262e-07, -1.1559283014844248e-06, -1.4874388996379023e-06, -1.8194967153181557e-06, -2.152270568623017e-06, -2.485930780266059e-06, -2.8206495294054816e-06, -3.1566012221575443e-06, -3.4939628691919857e-06, -3.83291447534424e-06, -4.173639445889235e-06, -4.516325006318035e-06, -4.861162642977447e-06, -5.208348564501056e-06, -5.5580841855732345e-06, -5.910576639395221e-06, -6.266039318247419e-06, -6.624692446995098e-06, -6.986763693889369e-06, -7.352488822776437e-06, -7.722112387361072e-06, -8.095888479208845e-06, -8.474081528580747e-06, -8.85696716672819e-06, -9.24483315833746e-06, -9.637980406641182e-06, -1.0036724044425071e-05, -1.0441394618183232e-05, -1.0852339375001763e-05, -1.1269922820792025e-05, -1.1694531368113112e-05, -1.2126570987894306e-05, -1.2566470868648183e-05, -1.301468562235413e-05, -1.3471697245836839e-05, -1.3938017571135007e-05, -1.4414190969405538e-05, -1.4900797344905295e-05, -1.539845545932517e-05, -1.5907826624454554e-05, -1.6429618819957294e-05, -1.6964591295377105e-05, -1.7513559722431472e-05, -1.807740197470036e-05, -1.8657064621695826e-05, -1.9253570251632048e-05, -1.986802579367011e-05, -2.0501631972989242e-05, -2.115569399392277e-05, -2.1831633850262077e-05, -2.253100413354534e-05, -2.3255503965997006e-05, -2.40069989054917e-05, -2.4787540040616316e-05, -2.5599391555840414e-05, -2.6445058090101033e-05, -2.7327319915718466e-05, -2.8249276366956277e-05, -2.9214374939173064e-05, -3.0226506238248104e-05, -3.129003251466997e-05, -3.2409891525720274e-05, -3.3591685914426105e-05, -3.4841853351868986e-05, -3.61676506308468e-05, -3.757755875592841e-05, -3.9081409139577416e-05, -4.069066631654763e-05, -4.2418824302800096e-05, -4.428194923785432e-05, -4.629903729716509e-05, -4.849314038615198e-05, -5.089171522978249e-05, -5.352978936998872e-05, -5.6452796864164966e-05, -5.9712829971419215e-05, -6.338164868876677e-05, -6.75604494508781e-05, -7.23596698400555e-05, -7.797966103161198e-05, -8.472826266023642e-05, -9.297448847446485e-05, -0.00010330755369808285, -0.00011716528052176712, -0.00013440083150905263, -0.00016118933981448976, -0.00020679409464778947, -0.00029391675874926857, -0.0007728548568458996};

    auto state = model->get_state();

    printf("%f\n", state.q_relative);
    size_t idx = 0;
    for (size_t d = 0; d < 30; d++) {
        for (idx = 0; idx < batt_socs.size(); idx ++) {
            bool charge_changed = false;
            charge_changed = (batt_powers[idx] * batt_powers[idx - 1 % batt_powers.size()] < 0);

            double DOD = batt_socs[idx] / batt_size * 100.;
            double DOD_prev = batt_socs[idx - 1] / batt_size * 100;

            model->runLifetimeModels(idx, charge_changed, DOD_prev, DOD, T);
        }
    }

    state = model->get_state();

    printf("%f, %d, %f\n", state.q_relative, state.n_cycles, state.average_range);
}
