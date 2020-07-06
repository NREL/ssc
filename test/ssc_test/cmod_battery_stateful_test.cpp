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

TEST_F(CMBatteryStatefulIntegration_cmod_battery_stateful, ReadJson) {
    std::string js = "{\"control_mode\": 1.0, \"dt_hr\": 0.016666666666666666, \"input_power\": 0.0, \"run_sequentially\": 0, \"C_rate\": 0.2, \"Qexp\": 0.04, \"Qfull\": 2.25, \"Qnom\": 2.0, \"Vexp\": 4.05, \"Vfull\": 4.1, \"Vnom\": 3.4, \"Vnom_default\": 3.6, \"calendar_a\": 0.003, \"calendar_b\": -7280.0, \"calendar_c\": 930.0, \"calendar_choice\": 1.0, \"calendar_matrix\": [[-3.1e+231]], \"calendar_q0\": 1.02, \"chem\": 1.0, \"cycling_matrix\": [[20.0, 0.0, 100.0], [20.0, 5000.0, 80.0], [20.0, 10000.0, 60.0], [80.0, 0.0, 100.0], [80.0, 1000.0, 80.0], [80.0, 2000.0, 60.0]], \"initial_SOC\": 50.0, \"maximum_SOC\": 95.0, \"minimum_SOC\": 5.0, \"resistance\": 0.0002, \"voltage_choice\": 0.0, \"Cp\": 1004.0, \"T_room_init\": 20.0, \"cap_vs_temp\": [[-10.0, 60.0], [0.0, 80.0], [25.0, 100.0], [40.0, 100.0]], \"h\": 20.0, \"loss_choice\": 0.0, \"mass\": 507.0, \"monthly_charge_loss\": [0.0], \"monthly_discharge_loss\": [0.0], \"monthly_idle_loss\": [0.0], \"nominal_energy\": 1000.0, \"nominal_voltage\": 500.0, \"replacement_capacity\": 0.0, \"replacement_option\": 0.0, \"replacement_schedule\": [-1.2882297539194267e-231], \"replacement_schedule_percent\": [-1.2882297539194267e-231], \"schedule_loss\": [0.0], \"surface_area\": 2.018, \"I\": 0.0, \"I_chargeable\": 190.33177772490922, \"I_dischargeable\": 2569.4789992862825, \"P\": 0.0, \"P_chargeable\": 108.47132706790202, \"P_dischargeable\": 1099.227288824885, \"Q\": 63.44392590830313, \"Q_max\": 63.44392590830313, \"SOC\": 100.0, \"T_batt\": 226.82315180344995, \"T_room\": 32.22642662920505, \"V\": 561.2000913308102, \"heat_dissipated\": 7853.923347885358, \"indices_replaced\": [0.0], \"last_idx\": 1284.0, \"loss_percent\": 0.0, \"n_replacements\": 0.0, \"I_loss\": 13408.681959960068, \"SOC_prev\": 100.0, \"T_batt_prev\": 226.36064113001987, \"average_range\": 0.0, \"cell_current\": 0.0, \"cell_voltage\": 4.037410728998634, \"chargeChange\": 0.0, \"charge_mode\": 1.0, \"n_cycles\": 0.0, \"prev_charge\": 0.0, \"q0\": 63.44392590830313, \"q1_0\": 0.0, \"q2\": 0.0, \"q2_0\": 0.0, \"q_relative\": 3.1753716670822385, \"q_relative_calendar\": 3.1753716670822385, \"q_relative_cycle\": 100.0, \"q_relative_thermal\": 100.0, \"qmax_lifetime\": 63.44392590830313, \"qmax_thermal\": 457.8992581207122, \"qn\": 0.0, \"rainflow_Xlt\": 43.23627857126032, \"rainflow_Ylt\": 0.9882462833291776, \"rainflow_jlt\": 3.0, \"rainflow_peaks\": [0.0, 48.233992293487496, 4.997713722227175, 88.72575144015839], \"range\": 0.0}";

    auto copy = json_to_ssc_data(js.c_str());

    mod = ssc_stateful_module_create("battery_stateful", copy);
    copy = json_to_ssc_data(js.c_str());
    ssc_module_exec(mod, copy);

    js = ssc_data_to_json(copy);

    printf("%s\n", js.c_str());
}
