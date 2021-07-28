#include <gtest/gtest.h>

#include "lib_battery_dispatch_pvsmoothing_fom_test.h"

#include <iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <cmath>
#include <json/json.h>
#include <json/writer.h>

// more can be added but these tests take a while...
TEST_F(PVSmoothing_lib_battery_dispatch, PV_Phoenix_all_on) {

    char file_path[1024];
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

        // values from Phoenix_Validation_testing.py EPRI code
        EXPECT_NEAR(violation_count, 12, 0.001);
        EXPECT_NEAR(violation_percent, 0.022, 0.001);
        EXPECT_NEAR(grid_percent, 99.90, 0.01);
        EXPECT_NEAR(grid_percent_sam, 99.90, 0.01);
    }

}

TEST_F(PVSmoothing_lib_battery_dispatch, Generic_w_PV_input_all_on) {

    char file_path[1024];
    int nfc1 = sprintf(file_path, "%s/test/input_cases/general_data/pvsmoothing_Generic_alloptions.json", SSCDIR);
    std::ifstream file(file_path);
    std::ostringstream tmp;
    tmp << file.rdbuf();
    std::string json_string = tmp.str();

    ssc_data_t dat = json_to_ssc_data(json_string.c_str());

    // Run with fixed output
    int errors = run_module(dat, "generic_system");
    errors = run_module(dat, "battery");
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

        // values from Phoenix_Validation_testing.py EPRI code
        EXPECT_NEAR(violation_count, 12, 0.001);
        EXPECT_NEAR(violation_percent, 0.022, 0.001);
        EXPECT_NEAR(grid_percent, 99.90, 0.01);
        EXPECT_NEAR(grid_percent_sam, 99.90, 0.01);
    }

}


TEST_F(PVSmoothing_lib_battery_dispatch, FuelCell_PV_Phoenix_all_on) {

    char file_path[1024];
    int nfc1 = sprintf(file_path, "%s/test/input_cases/general_data/pvsmoothing_Fuel_Cell_Phoenix_Validation_alloptions.json", SSCDIR);
    std::ifstream file(file_path);
    std::ostringstream tmp;
    tmp << file.rdbuf();
    std::string json_string = tmp.str();

    ssc_data_t dat = json_to_ssc_data(json_string.c_str());

    // setup path to weather file
    nfc1 = sprintf(file_path, "%s/test/input_cases/general_data/phoenix_az_33.450495_-111.983688_psmv3_60_tmy_1mInterpolated.csv", SSCDIR);
    ssc_data_set_string(dat, "solar_resource_file", file_path);

    // Run with fixed output
    int errors = run_module(dat, "pvwattsv7");
    errors = run_module(dat, "fuelcell");
    errors = run_module(dat, "battery");
    errors = run_module(dat, "grid");
    errors = run_module(dat, "utilityrate5");
    errors = run_module(dat, "thermalrate");
    errors = run_module(dat, "singleowner");

    EXPECT_FALSE(errors);
    if (!errors)
    {
        ssc_number_t violation_count, violation_percent, grid_percent, grid_percent_sam;
        ssc_data_get_number(dat, "batt_pvs_violation_count", &violation_count);
        ssc_data_get_number(dat, "batt_pvs_violation_percent", &violation_percent);
        ssc_data_get_number(dat, "batt_pvs_energy_to_grid_percent", &grid_percent);
        ssc_data_get_number(dat, "batt_pvs_energy_to_grid_percent_sam", &grid_percent_sam);

        // values from Phoenix_FuelCell_Validation_testing.sam
        EXPECT_NEAR(violation_count, 6, 0.001);
        EXPECT_NEAR(violation_percent, 0.0114, 0.001);
        EXPECT_NEAR(grid_percent, 99.92, 0.01);
        EXPECT_NEAR(grid_percent_sam, 99.92, 0.01);
    }

}

