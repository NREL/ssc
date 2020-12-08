#include <string>
#include <iostream>
#include <fstream>
#include <gtest/gtest.h>

#include "../ssc/vartab.h"
#include "json/json.h" // Jsoncpp

static const char* SSCDIR = std::getenv("SSCDIR");

char inputs_as_JSON[256] = {};
int nj1 = sprintf(inputs_as_JSON, "%s/test/input_json/pvwatts_merchantplant.json", SSCDIR);
char JSON_for_PySAM_PVWatts[256] = {};
int nj2 = sprintf(JSON_for_PySAM_PVWatts, "%s/test/input_json/pvwattsv7.json", SSCDIR);
char JSON_for_PySAM_Grid[256] = {};
int nj3 = sprintf(JSON_for_PySAM_Grid, "%s/test/input_json/grid.json", SSCDIR);
char JSON_for_PySAM_MerchantPlant[256] = {};
int nj4 = sprintf(JSON_for_PySAM_MerchantPlant, "%s/test/input_json/merchantplant.json", SSCDIR);

TEST(save_as_JSON_test, pvwatts_mechant_plant_read_file_to_string) {
    std::ifstream test(inputs_as_JSON);
    std::string str((std::istreambuf_iterator<char>(test)), std::istreambuf_iterator<char>());
    EXPECT_EQ(str.length(), 12263249);
}

TEST(save_as_JSON_test, pvwatts_mechant_plant_jsoncpp_parse_file) {
    Json::Value root;  
    Json::Reader reader;
    std::ifstream test(inputs_as_JSON, std::ifstream::binary);
    bool parsingSuccessful = reader.parse(test, root, false);
    if (!parsingSuccessful)
    {
        // report to the user the failure and their locations in the document.
        std::cout << reader.getFormatedErrorMessages()
            << "\n";
    }
    EXPECT_TRUE(parsingSuccessful);
}

TEST(save_as_JSON_test, pvwatts_mechant_plant_jsoncpp_read_file_to_ssc_var_table) {
    std::ifstream test(inputs_as_JSON);
    std::string json_str((std::istreambuf_iterator<char>(test)), std::istreambuf_iterator<char>());
    auto vt = json_to_ssc_data(json_str.c_str());
    std::string str(ssc_data_first(vt));
    EXPECT_EQ(str, "debt_option");
}

TEST(save_as_JSON_test, pvwatts_mechant_plant_jsoncpp_read_file_run_pvwatts) {
    std::ifstream test(inputs_as_JSON);
    std::string json_str((std::istreambuf_iterator<char>(test)), std::istreambuf_iterator<char>());
    auto data = json_to_ssc_data(json_str.c_str());
    auto mod = ssc_module_create("pvwattsv7");
    ssc_module_exec_set_print(0);
    bool success = ssc_module_exec(mod, data);
    EXPECT_TRUE(success);
    ssc_number_t annual_energy;
    ssc_data_get_number(data, "annual_energy", &annual_energy);
    EXPECT_NEAR(annual_energy, 87966056, 87966056/1e6);
}

TEST(save_as_JSON_test, pvwatts_mechant_plant_jsoncpp_read_file_run_pvwatts_and_merchant_plant) {
    std::ifstream test(inputs_as_JSON);
    std::string json_str((std::istreambuf_iterator<char>(test)), std::istreambuf_iterator<char>());
    auto data = json_to_ssc_data(json_str.c_str());
    ssc_module_exec_set_print(0);
    auto mod_pv = ssc_module_create("pvwattsv7");
    bool success = ssc_module_exec(mod_pv, data);
    EXPECT_TRUE(success);
    auto mod_mp = ssc_module_create("merchantplant");
    success = ssc_module_exec(mod_mp, data);
    EXPECT_TRUE(success);
    ssc_number_t npv;
    ssc_data_get_number(data, "project_return_aftertax_npv", &npv);
    EXPECT_NEAR(npv, -60929408, fabs(60929408)/1e6);
}

