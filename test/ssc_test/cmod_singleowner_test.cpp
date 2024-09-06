/*
BSD 3-Clause License

Copyright Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE


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


#include "cmod_singleowner_test.h"

#include "gtest/gtest.h"

TEST_F(CMSingleOwner, ResidentialDefault_cmod_swh) {

    int errors = run_module(data, "singleowner");
    ASSERT_EQ(errors, 0);

    ssc_number_t npv;
    ssc_data_get_number(data, "project_return_aftertax_npv", &npv);
    EXPECT_NEAR(npv, -647727845.8, 0.1);

}


TEST_F(CmodSingleOwnerTest, heat_electricity) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/Default_SO_10_IBI_PVWatts_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/Default_SO_10_IBI_PVWatts_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    //Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);


    std::string weather_file = "phoenix_az_33.450495_-111.983688_psmv3_60_tmy.csv";
    char solar_resource_path[256];
    std::string sWeatherFile = "%s/test/input_cases/general_data/" + weather_file;
    int npvy1 = sprintf(solar_resource_path, sWeatherFile.c_str(), std::getenv("SSCDIR")); // TODO - update for robustness
    std::ifstream file(file_inputs);
    std::ostringstream tmp;
    tmp << file.rdbuf();
    file.close();
    ssc_data_t dat_inputs = json_to_ssc_data(tmp.str().c_str());
    ssc_data_set_string(dat_inputs, "solar_resource_file", solar_resource_path);

    tmp.str("");
    int errors = run_module(dat_inputs, "singleowner");

    EXPECT_FALSE(errors);
    if (!errors)
    {
        file.open(file_outputs);
        tmp << file.rdbuf();
        file.close();
        ssc_data_t dat_outputs = json_to_ssc_data(tmp.str().c_str());
        tmp.str("");

        std::vector<ssc_number_t> values_to_compare(compare_number_variables.size());
        std::vector<ssc_number_t> values_to_match(compare_number_variables.size());

        std::vector< std::vector<ssc_number_t> > arrays_to_compare(compare_array_variables.size());
        std::vector< std::vector<ssc_number_t> > arrays_to_match(compare_array_variables.size());

        for (size_t i = 0; i < compare_number_variables.size(); i++) {
            ssc_data_get_number(dat_inputs, compare_number_variables[i].c_str(), &values_to_compare[i]);
            ssc_data_get_number(dat_outputs, compare_number_variables[i].c_str(), &values_to_match[i]);
            if (!isnan(values_to_compare[i]) || !isnan(values_to_match[i]))
                EXPECT_NEAR(values_to_compare[i], values_to_match[i], tolerance) << " number issue at index i=" << i << " for " << compare_number_variables[i];
        }

        int len_currentrun, len_comparerun;
        for (size_t i = 0; i < compare_array_variables.size(); i++) {
            auto pCurrentOutputs = ssc_data_get_array(dat_inputs, compare_array_variables[i].c_str(), &len_currentrun);
            auto pCompareOutputs = ssc_data_get_array(dat_outputs, compare_array_variables[i].c_str(), &len_comparerun);
            EXPECT_EQ(len_currentrun, len_comparerun) << " for " << compare_array_variables[i];
            for (int j = 0; j < len_currentrun && j < len_comparerun; j++) {
                EXPECT_NEAR(pCurrentOutputs[j], pCompareOutputs[j], tolerance) << " array issue at index i=" << i << " and array index j=" << j << " for " << compare_array_variables[i];
            }
        }

        ssc_data_free(dat_outputs);
        dat_outputs = nullptr;
    }
    ssc_data_free(dat_inputs);
    dat_inputs = nullptr;
}



TEST_F(CmodSingleOwnerTest, ssc_1047) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/Default_SO_10_IBI_PVWatts_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/Default_SO_10_IBI_PVWatts_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}


TEST_F(CmodSingleOwnerTest, Biopower) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Biopower_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Biopower_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom"};
    std::vector<std::string> compare_array_variables = {"cf_project_return_aftertax", "cf_annual_costs"};

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}

TEST_F(CmodSingleOwnerTest, DSLF) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_DSLF_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_DSLF_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}

TEST_F(CmodSingleOwnerTest, EmpiricalTrough) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Empirical_Trough_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Empirical_Trough_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}

TEST_F(CmodSingleOwnerTest, ETES) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_ETES_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_ETES_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}

TEST_F(CmodSingleOwnerTest, PV) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Flat_Plate_PV_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Flat_Plate_PV_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}

TEST_F(CmodSingleOwnerTest, FuelCell) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Fuel_Cell_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Fuel_Cell_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}

TEST_F(CmodSingleOwnerTest, GenericBattery) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Generic_Battery_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Generic_Battery_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}

TEST_F(CmodSingleOwnerTest, GenericCSP) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Generic_CSP_System_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Generic_CSP_System_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}

TEST_F(CmodSingleOwnerTest, Generic) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Generic_System_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Generic_System_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}


TEST_F(CmodSingleOwnerTest, Geotherrmal) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Geothermal_Power_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Geothermal_Power_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}

TEST_F(CmodSingleOwnerTest, CPV) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_High-X_Concentrating_PV_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_High-X_Concentrating_PV_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };


    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}


TEST_F(CmodSingleOwnerTest, MSLF) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_MSLF_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_MSLF_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}

TEST_F(CmodSingleOwnerTest, MSPT) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_MSPT_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_MSPT_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}

TEST_F(CmodSingleOwnerTest, PhysicalTrough) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Physical_Trough_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Physical_Trough_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}


TEST_F(CmodSingleOwnerTest, PVBattery) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_PV_Battery_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_PV_Battery_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}


TEST_F(CmodSingleOwnerTest, PVWatts) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_PVWatts_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_PVWatts_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}

TEST_F(CmodSingleOwnerTest, StandaloneBattery) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Standalone_Battery_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Standalone_Battery_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}


TEST_F(CmodSingleOwnerTest, Wind) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Wind_Power_Single_Owner_cmod_singleowner.json";
    std::string file_outputs = SSCDIR;
    file_outputs += "/test/input_json/FinancialModels/singleowner/2022.08.08_develop_branch_Wind_Power_Single_Owner_cmod_singleowner_outputs.json";
    std::vector<std::string> compare_number_variables = { "ppa", "project_return_aftertax_npv", "lcoe_real", "lppa_nom" };
    std::vector<std::string> compare_array_variables = { "cf_project_return_aftertax", "cf_annual_costs" };

    Test("singleowner", file_inputs, file_outputs, compare_number_variables, compare_array_variables);
}


