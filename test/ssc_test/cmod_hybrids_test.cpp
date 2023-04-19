/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#include "cmod_hybrids_test.h"

#include "gtest/gtest.h"

// TODO - update input JSON for test paths for resource files

TEST_F(CmodHybridsTest, PVWattsv8) {

    char file_path[256];
    int nfc1 = sprintf(file_path, "%s/test/input_json/hybrids/pvwattsv8.json", SSCDIR);
    std::ifstream file(file_path);
    std::ostringstream tmp;
    tmp << file.rdbuf();
    file.close();
    ssc_data_t dat = json_to_ssc_data(tmp.str().c_str());
    tmp.str("");

    auto table = ssc_data_get_table(dat, "input");
    char solar_resource_path[256];
    int npvy1 = sprintf(solar_resource_path, "%s/test/input_cases/general_data/phoenix_az_33.450495_-111.983688_psmv3_60_tmy.csv", std::getenv("SSCDIR")); 
    ssc_data_set_string(table, "solar_resource_file", solar_resource_path);

    int errors = run_module(dat, "hybrid");

    EXPECT_FALSE(errors);
    if (!errors)
    {
        ssc_number_t annualenergy;
        auto outputs = ssc_data_get_table(dat, "output");
        ssc_data_get_number(outputs, "annual_energy", &annualenergy);

         EXPECT_NEAR(annualenergy, 165112880, 165112880 * 0.01);
    }
    ssc_data_free(dat);
    dat = nullptr;
}


TEST_F(CmodHybridsTest, Wind) {

    char file_path[256];
    int nfc1 = sprintf(file_path, "%s/test/input_json/hybrids/wind.json", SSCDIR);
    std::ifstream file(file_path);
    std::ostringstream tmp;
    tmp << file.rdbuf();
    file.close();
    ssc_data_t dat = json_to_ssc_data(tmp.str().c_str());
    tmp.str("");
    int errors = run_module(dat, "hybrid");

    auto table = ssc_data_get_table(dat, "input");
    char wind_resource_path[256];
    int npvy1 = sprintf(wind_resource_path, "%s/test/input_cases/general_data/WY Southern-Flat Lands.srw", std::getenv("SSCDIR"));
    ssc_data_set_string(table, "wind_resource_filename", wind_resource_path);

    EXPECT_FALSE(errors);
    if (!errors)
    {
        ssc_number_t annualenergy;
        auto outputs = ssc_data_get_table(dat, "output");
        ssc_data_get_number(outputs, "annual_energy", &annualenergy);

        EXPECT_NEAR(annualenergy, 201595968, 201595968 * 0.01);
    }
    ssc_data_free(dat);
    dat = nullptr;
}


TEST_F(CmodHybridsTest, PVWattsv8Wind) {

    char file_path[256];
    int nfc1 = sprintf(file_path, "%s/test/input_json/hybrids/pvwattsv8wind.json", SSCDIR);
    std::ifstream file(file_path);
    std::ostringstream tmp;
    tmp << file.rdbuf();
    file.close();
    ssc_data_t dat = json_to_ssc_data(tmp.str().c_str());
    tmp.str("");

    char solar_resource_path[256];
    int npvy1 = sprintf(solar_resource_path, "%s/test/input_cases/general_data/phoenix_az_33.450495_-111.983688_psmv3_60_tmy.csv", std::getenv("SSCDIR"));
    ssc_data_set_string(dat, "solar_resource_file", solar_resource_path);

    char wind_resource_path[256];
    int npvy2 = sprintf(wind_resource_path, "%s/test/input_cases/general_data/WY Southern-Flat Lands.srw", std::getenv("SSCDIR"));
    ssc_data_set_string(dat, "wind_resource_filename", wind_resource_path);

    int errors = run_module(dat, "hybrid");

    EXPECT_FALSE(errors);
    if (!errors)
    {
        ssc_number_t annualenergy;
        auto outputs = ssc_data_get_table(dat, "output");
        auto wind_outputs = ssc_data_get_table(outputs, "mod2");
        ssc_data_get_number(wind_outputs, "annual_energy", &annualenergy);

        EXPECT_NEAR(annualenergy, 201595968, 201595968 * 0.01);
    }
    ssc_data_free(dat);
    dat = nullptr;
}


/*
TEST_F(CmodHybridsTest, PVWattsv8) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/hybrids/pvwattsv8.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/hybrids/pvwattsv8_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "tax_investor_aftertax_npv", "sponsor_aftertax_npv", "lcoe_real", "lppa_nom"};
    std::vector<std::string> compare_array_variables = {"cf_tax_investor_aftertax", "cf_sponsor_aftertax", "cf_annual_costs"};

    Test("pvwattsv8", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}
*/
