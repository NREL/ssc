#include <gtest/gtest.h>

//#include "lib_battery_capacity.h"
#include "lib_battery.h"
#include "lib_battery_lifetime_test.h"

TEST_F(lib_battery_lifetime_cycle_test, SetUpTest) {
    EXPECT_EQ(model->capacity_percent(), 100);
}

TEST_F(lib_battery_lifetime_calendar_matrix_test, runCalendarModelTest) {
    double T = 278, SOC = 20;       // not used but required for function
    int idx = 0;
    while (idx < 500){
        if (idx % 2 != 0){
            SOC = 90;
        }
        model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    auto s = calendar_lifetime_state({20,99.89,499,0,0});
    compareState(s, "runCalendarModelTest: 1");

    while (idx < 1000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    s = calendar_lifetime_state({41,99.76,999,0,0});
    compareState(s, "runCalendarModelTest: 2");
}

TEST_F(lib_battery_lifetime_calendar_matrix_test, replaceBatteryTest) {
    double T = 278, SOC = 20;
    int idx = 0;
    while (idx < 200000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    auto s = calendar_lifetime_state({8333,41.51,199999,0,0});
    compareState(s, "runCalendarModelTest: 1");

    model->replaceBattery(5);

    s = calendar_lifetime_state({8333,46.51,199999,0,0});
    compareState(s, "runCalendarModelTest: 2");
}

TEST_F(lib_battery_lifetime_calendar_model_test, SetUpTest) {
    EXPECT_EQ(model->capacity_percent(), 102);
}

TEST_F(lib_battery_lifetime_calendar_model_test, runCalendarModelTest) {
    double T = 278, SOC = 20;       // not used but required for function
    int idx = 0;
    while (idx < 500){
        if (idx % 2 != 0){
            SOC = 90;
        }
        model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    auto s = calendar_lifetime_state({20,101.78,499,0.00217,0.00217});
    compareState(s, "runCalendarModelTest: 1");

    while (idx < 1000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    s = calendar_lifetime_state({41,101.69,999,0.00306,0.00306});
    compareState(s, "runCalendarModelTest: 2");
}

TEST_F(lib_battery_lifetime_calendar_model_test, replaceBatteryTest) {
    double T = 278, SOC = 20;
    int idx = 0;
    while (idx < 200000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    auto s = calendar_lifetime_state({8333,97.67,199999,0.043,0.043});
    compareState(s, "runCalendarModelTest: 1");

    model->replaceBattery(5);

    s = calendar_lifetime_state({8333,102,199999,0,0});
    compareState(s, "runCalendarModelTest: 2");
}