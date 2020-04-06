#include <gtest/gtest.h>

//#include "lib_battery_capacity.h"
#include "lib_battery.h"
#include "lib_battery_lifetime_test.h"

TEST_F(lib_battery_lifetime_cycle_test, SetUpTest) {
    EXPECT_EQ(cycle_model->capacity_percent(), 100);
}

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
    auto s = cycle_lifetime_state({95.02,90,90,90,90, 249, 2});
    compareState(cycle_model, s, "runCycleLifetimeTest: 1");

    while (idx < 1000){
        if (idx % 2 != 0){
            DOD = 90;
        }
        cycle_model->runCycleLifetime(DOD);
        idx++;
    }
    s = cycle_lifetime_state({91.244,0,0,0,44.9098, 499, 2});
    compareState(cycle_model, s, "runCycleLifetimeTest: 2");
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
    auto s = cycle_lifetime_state({85.02,90,90,90,90, 749, 2});
    compareState(cycle_model, s, "replaceBatteryTest: 1");

    cycle_model->replaceBattery(5);

    s = cycle_lifetime_state({90.019,0,0,0,90, 749, 2});
    compareState(cycle_model, s, "replaceBatteryTest: 2");
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
    auto s = calendar_lifetime_state({20,99.89,499,0,0});
    compareState(cal_model, s, "runCalendarMatrixTest: 1");

    while (idx < 1000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    s = calendar_lifetime_state({41,99.775,999,0,0});
    compareState(cal_model, s, "runCalendarMatrixTest: 2");
}

TEST_F(lib_battery_lifetime_calendar_matrix_test, replaceBatteryTest) {
    double T = 278, SOC = 20;
    int idx = 0;
    while (idx < 200000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    auto s = calendar_lifetime_state({8333,41.51,199999,0,0});
    compareState(cal_model, s, "replaceBatteryTest: 1");

    cal_model->replaceBattery(5);

    s = calendar_lifetime_state({8333,46.51,199999,0,0});
    compareState(cal_model, s, "replaceBatteryTest: 2");
}

TEST_F(lib_battery_lifetime_calendar_model_test, SetUpTest) {
    EXPECT_EQ(cal_model->capacity_percent(), 102);
}

TEST_F(lib_battery_lifetime_calendar_model_test, runCalendarModelTest) {
    double T = 278, SOC = 20;       // not used but required for function
    int idx = 0;
    while (idx < 500){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    auto s = calendar_lifetime_state({20,101.78,499,0.00217,0.00217});
    compareState(cal_model, s, "runCalendarModelTest: 1");

    while (idx < 1000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    s = calendar_lifetime_state({41,101.69,999,0.00306,0.00306});
    compareState(cal_model, s, "runCalendarModelTest: 2");
}

TEST_F(lib_battery_lifetime_calendar_model_test, replaceBatteryTest) {
    double T = 278, SOC = 20;
    int idx = 0;
    while (idx < 200000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    auto s = calendar_lifetime_state({8333,97.67,199999,0.043,0.043});
    compareState(cal_model, s, "runCalendarModelTest: 1");

    cal_model->replaceBattery(5);

    s = calendar_lifetime_state({8333,102,199999,0,0});
    compareState(cal_model, s, "runCalendarModelTest: 2");
}

//TEST_F(lib_battery_lifetime_test, ReplaceByCapacityTest){
//    model = std::unique_ptr<lifetime_t>(new lifetime_t(cycle_model.get(), cal_model.get(), 1, 60));
//
//    size_t idx = 0;
//    while (idx < 876){
//        model->runLifetimeModels(idx, 5, true, 293);
//        model->runLifetimeModels(idx, 95, true, 293);
//        idx ++;
//    }
//    auto s = lifetime_state({{82.5, 90, 90, 90, 90, 875, 2, std::vector<double>()},
//                                 {36, 101.937, 875, 0.000632, 0.000632}, 82.5, 0});
//    compareState(model, s, "ReplaceByCapacityTest: 1");
//
//    while (idx < 2101){
//        model->runLifetimeModels(idx, 25, true, 293);
//        model->runLifetimeModels(idx, 75, true, 293);
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
