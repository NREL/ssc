#include <chrono>

#include "cmod_battery_stateful_test.h"

typedef std::chrono::high_resolution_clock Clock;

TEST_F(CMBatteryStatefulIntegration_cmod_battery_stateful, TestStep) {
    double last_idx, current, SOC, V, P;

    ssc_module_exec(mod, data);
    ssc_data_get_number(data, "last_idx", &last_idx);
    ssc_data_get_number(data, "I", &current);
    ssc_data_get_number(data, "P", &P);
    ssc_data_get_number(data, "V", &V);
    ssc_data_get_number(data, "SOC", &SOC);
    EXPECT_EQ(last_idx, 1);
    EXPECT_NEAR(current, 1, 1e-2);
    EXPECT_NEAR(P, 0.477, 1e-2);
    EXPECT_NEAR(V, 477.15, 1e-2);
    EXPECT_NEAR(SOC, 46.94, 1e-2);

    // make a copy
    std::string js = ssc_data_to_json(data);
    auto copy = json_to_ssc_data(js.c_str());

    ssc_module_exec(mod, data);
    ssc_data_get_number(data, "last_idx", &last_idx);
    ssc_data_get_number(data, "I", &current);
    ssc_data_get_number(data, "P", &P);
    ssc_data_get_number(data, "V", &V);
    ssc_data_get_number(data, "SOC", &SOC);
    EXPECT_EQ(last_idx, 2);
    EXPECT_NEAR(current, 1, 1e-2);
    EXPECT_NEAR(P, 0.457, 1e-2);
    EXPECT_NEAR(V, 457.93, 1e-2);
    EXPECT_NEAR(SOC, 41.79, 1e-2);

    // run the copy, should end up in same place
    ssc_module_exec(mod, copy);
    ssc_data_get_number(data, "last_idx", &last_idx);
    ssc_data_get_number(data, "I", &current);
    ssc_data_get_number(data, "P", &P);
    ssc_data_get_number(data, "V", &V);
    ssc_data_get_number(data, "SOC", &SOC);
    EXPECT_EQ(last_idx, 2);
    EXPECT_NEAR(current, 1, 1e-2);
    EXPECT_NEAR(P, 0.457, 1e-2);
    EXPECT_NEAR(V, 457.93, 1e-2);
    EXPECT_NEAR(SOC, 41.79, 1e-2);
}
