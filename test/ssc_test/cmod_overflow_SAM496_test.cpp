#include <string>
#include <cmath>
#include <iostream>
#include <fstream>
#include <gtest/gtest.h>

#include "../ssc/vartab.h"
#include "json/json.h" // Jsoncpp
static const char* SSCDIR = std::getenv("SSCDIR");

char inputs_as_JSON[256] = {};
int nj1 = sprintf(inputs_as_JSON, "%s/test/input_cases/json/SSC_Test_Case_SAM496.json", SSCDIR);



TEST(cmod_overflow_SAM496_test_run, SSC_Test_Case_SAM496) {
    std::ifstream test(inputs_as_JSON);
    std::string json_str((std::istreambuf_iterator<char>(test)), std::istreambuf_iterator<char>());
    auto data = json_to_ssc_data(json_str.c_str());
    ssc_module_exec_set_print(0);

    auto mod_belpe = ssc_module_create("belpe");
    bool success = ssc_module_exec(mod_belpe, data);
    EXPECT_TRUE(success);
    auto mod_pv = ssc_module_create("pvsamv1");
    success = ssc_module_exec(mod_pv, data);
    EXPECT_TRUE(success);
    auto mod_grid = ssc_module_create("grid");
    success = ssc_module_exec(mod_grid, data);
    EXPECT_TRUE(success);
    auto mod_utilityrate5 = ssc_module_create("utilityrate5");
    success = ssc_module_exec(mod_utilityrate5, data);
    EXPECT_TRUE(success);
    auto mod_cashloan = ssc_module_create("cashloan");
    success = ssc_module_exec(mod_cashloan, data);
    EXPECT_TRUE(success);
    ssc_number_t annual_energy;
    ssc_data_get_number(data, "annual_energy", &annual_energy);
    EXPECT_NEAR(annual_energy, 43703867532725786247168.0, fabs(43703867532725786247168.0) *1e-7); 
    ssc_number_t npv;
    ssc_data_get_number(data, "npv", &npv);
    EXPECT_NEAR(npv, -5.88276711537766E+22, fabs(-5.88276711537766E+22) * 1e-7);
}

