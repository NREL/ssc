#include <gtest/gtest.h>

#include "cmod_pv_smoothing_test.h"

#include <iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <cmath>
#include <json/json.h>
#include <json/writer.h>



TEST_F(CMPVSmoothing, EPRI_example_script) {

    char gen_path_pv[256];
    int nfc1 = sprintf(gen_path_pv, "%s/test/input_cases/general_data/pv_smoothing_1min_kWac.csv", SSCDIR);

    ssc_data_set_string(data, "batt_dispatch_pvs_pv_power_file", gen_path_pv);
    ssc_data_set_number(data, "batt_dispatch_pvs_max_ramp", 0.1);
    ssc_data_set_number(data, "batt_dispatch_pvs_timestep_multiplier", 10);
    ssc_data_set_number(data, "batt_dispatch_pvs_ramp_interval", 10);
    ssc_data_set_number(data, "batt_dispatch_pvs_ac_ub_enable", 1);
    ssc_data_set_number(data, "batt_dispatch_pvs_ac_lb_enable", 1);
    ssc_data_set_number(data, "batt_dispatch_pvs_ac_ub", 1.05);
    ssc_data_set_number(data, "batt_dispatch_pvs_ac_lb", -0.01);
    ssc_data_set_number(data, "batt_dispatch_pvs_short_forecast_enable", 0);
    ssc_data_set_number(data, "batt_dispatch_pvs_forecast_shift_periods", 3);
    ssc_data_set_number(data, "batt_dispatch_pvs_battery_energy", 0.2);
    ssc_data_set_number(data, "batt_dispatch_pvs_battery_power", 1);
    ssc_data_set_number(data, "batt_dispatch_pvs_battery_rte", 0.9);
    ssc_data_set_number(data, "batt_dispatch_pvs_nameplate_ac", 500);
    ssc_data_set_number(data, "batt_dispatch_pvs_interconnection_limit", 100000);
    ssc_data_set_number(data, "batt_dispatch_pvs_curtail_as_control", 0);
    ssc_data_set_number(data, "batt_dispatch_pvs_curtail_if_violation", 0);
    ssc_data_set_number(data, "batt_dispatch_pvs_kp", 1.2);
    ssc_data_set_number(data, "batt_dispatch_pvs_ki", 1.8);
    ssc_data_set_number(data, "batt_dispatch_pvs_kf", 0.3);
    ssc_data_set_number(data, "batt_dispatch_pvs_soc_rest", 0.5);


	// Run with fixed output
	int errors = run_module(data, "pv_smoothing");
	EXPECT_FALSE(errors);
	if (!errors)
	{
		ssc_number_t violation_count, total_energy;
		ssc_data_get_number(data, "batt_dispatch_pvs_violation_count", &violation_count);
		ssc_data_get_number(data, "batt_dispatch_pvs_total_energy", &total_energy);

		EXPECT_NEAR(violation_count, 516, 0.001);
		EXPECT_NEAR(total_energy, 1861.72, 0.01);
	}

}

TEST_F(CMPVSmoothing, EPRI_example_script_all_on) {

    char gen_path_pv[256];
    int nfc1 = sprintf(gen_path_pv, "%s/test/input_cases/general_data/pv_smoothing_1min_kWac.csv", SSCDIR);

    ssc_data_set_string(data, "batt_dispatch_pvs_pv_power_file", gen_path_pv);
    ssc_data_set_number(data, "batt_dispatch_pvs_max_ramp", 0.1);
    ssc_data_set_number(data, "batt_dispatch_pvs_timestep_multiplier", 10);
    ssc_data_set_number(data, "batt_dispatch_pvs_ramp_interval", 10);
    ssc_data_set_number(data, "batt_dispatch_pvs_ac_ub_enable", 1);
    ssc_data_set_number(data, "batt_dispatch_pvs_ac_lb_enable", 1);
    ssc_data_set_number(data, "batt_dispatch_pvs_ac_ub", 1.05);
    ssc_data_set_number(data, "batt_dispatch_pvs_ac_lb", -0.01);
    ssc_data_set_number(data, "batt_dispatch_pvs_short_forecast_enable", 1);
    ssc_data_set_number(data, "batt_dispatch_pvs_forecast_shift_periods", 3);
    ssc_data_set_number(data, "batt_dispatch_pvs_battery_energy", 0.2);
    ssc_data_set_number(data, "batt_dispatch_pvs_battery_power", 1);
    ssc_data_set_number(data, "batt_dispatch_pvs_battery_rte", 0.9);
    ssc_data_set_number(data, "batt_dispatch_pvs_nameplate_ac", 500);
    ssc_data_set_number(data, "batt_dispatch_pvs_interconnection_limit", 100000);
    ssc_data_set_number(data, "batt_dispatch_pvs_curtail_as_control", 1);
    ssc_data_set_number(data, "batt_dispatch_pvs_curtail_if_violation", 1);
    ssc_data_set_number(data, "batt_dispatch_pvs_kp", 1.2);
    ssc_data_set_number(data, "batt_dispatch_pvs_ki", 1.8);
    ssc_data_set_number(data, "batt_dispatch_pvs_kf", 0.3);
    ssc_data_set_number(data, "batt_dispatch_pvs_soc_rest", 0.5);


    // Run with fixed output
    int errors = run_module(data, "pv_smoothing");
    EXPECT_FALSE(errors);
    if (!errors)
    {
        ssc_number_t violation_count, total_energy;
        ssc_data_get_number(data, "batt_dispatch_pvs_violation_count", &violation_count);
        ssc_data_get_number(data, "batt_dispatch_pvs_total_energy", &total_energy);

        EXPECT_NEAR(violation_count, 259, 0.001);
        EXPECT_NEAR(total_energy, 1854.48, 0.01);
    }

}

TEST_F(CMPVSmoothing, Phoenix__all_on) {

    char file_path[256];
    int nfc1 = sprintf(file_path, "%s/test/input_cases/general_data/pvsmoothing_Phoenix_Validation_alloptions.json", SSCDIR);
    std::ifstream file(file_path);
    std::ostringstream tmp;
    tmp << file.rdbuf();
    std::string json_string = tmp.str();

    ssc_data_t dat = json_to_ssc_data(json_string.c_str());

    // setup path to weather file
    nfc1 = sprintf(file_path, "%s/test/input_cases/general_data/phoenix_az_33.450495_-111.983688_psmv3_60_tmy_1mInterpolated.csv", SSCDIR);
    ssc_data_set_string(dat, "solar_resource_file", file_path);

    // Run with fixed output
    int errors = run_module(dat, "pvsamv1");
    errors = run_module(dat, "grid");
    errors = run_module(dat, "utilityrate5");
    errors = run_module(dat, "singleowner");

    EXPECT_FALSE(errors);
    if (!errors)
    {
        ssc_number_t violation_count, violation_percent, grid_percent, grid_percent_sam;
        ssc_data_get_number(dat, "batt_pvs_violation_count", &violation_count);
        ssc_data_get_number(dat, "batt_pvs_violation_percent", &violation_percent);
        ssc_data_get_number(dat, "batt_pvs_energy_to_grid_percent", &grid_percent);
        ssc_data_get_number(dat, "batt_pvs_energy_to_grid_percent_sam", &grid_percent_sam);

        // valudes from Phoenix_Validation_testing.py EPRI code
        EXPECT_NEAR(violation_count, 12, 0.001);
        EXPECT_NEAR(violation_percent, 0.02, 0.01);
        EXPECT_NEAR(grid_percent, 99.90, 0.01);
        EXPECT_NEAR(grid_percent_sam, 99.90, 0.01);
    }

}

