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


#include <gtest/gtest.h>
#include "cmod_financial_eqns.h"
#include "csp_common_test.h"
#include "vs_google_test_explorer_namespace.h"

namespace csp_common {}
using namespace csp_common;

double GetNum(var_table* vd, std::string name) {
    var_data* data = vd->lookup(name.c_str());
    if (data) {
        return data->num;
    }
    throw std::invalid_argument(name + " is null");
}
double GetNum(ssc_data_t data, std::string name) {
    auto data_vtab = static_cast<var_table*>(data);
    return data_vtab->as_number(name.c_str());
}


NAMESPACE_TEST(csp_common, TowerSharedWithUi, Tes) {
    double error_tolerance = 0.01;
    ssc_data_t data = ssc_data_create();
    auto data_vtab = static_cast<var_table*>(data);

    data_vtab->assign("P_ref", 115.);
    data_vtab->assign("design_eff", 0.412);
    data_vtab->assign("tshours", 10.);
    data_vtab->assign("T_htf_hot_des", 574.);
    data_vtab->assign("T_htf_cold_des", 290.);
    data_vtab->assign("store_fluid", 17);
    std::vector<double> field_fluid_properties{ 1, 7, 0, 0, 0, 0, 0, 0, 0 };
    util::matrix_t<double> field_fl_props(1, 9, &field_fluid_properties);
    data_vtab->assign("store_fl_props", field_fl_props);
    data_vtab->assign("h_tank_min", 1.);
    data_vtab->assign("h_tank", 12.);
    data_vtab->assign("tank_pairs", 1.);
    data_vtab->assign("u_tank", 0.4);
    data_vtab->assign("field_fluid", 17);
    data_vtab->assign("field_fl_props", field_fl_props);
    data_vtab->assign("dt_hot", 5);

    int errors = run_module(data, "ui_tes_calcs");
    EXPECT_FALSE(errors);

    ASSERT_NEAR_FRAC(GetNum(data, "q_tes"), 2791.3, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "tes_avail_vol"), 12986., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "vol_tank"), 14166., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "csp_pt_tes_tank_diameter"), 38.8, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "q_dot_tes_est"), 0.88141, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "csp_pt_tes_htf_density"), 1808.48, kErrorToleranceHi);
    ssc_data_free(data);
}
NAMESPACE_TEST(csp_common, TowerSharedWithUi, TesWithPeriodUse) {
    double error_tolerance = 0.01;
    ssc_data_t data = ssc_data_create();
    auto data_vtab = static_cast<var_table*>(data);

    data_vtab->assign("P_ref", 115.);
    data_vtab->assign("design_eff", 0.412);
    data_vtab->assign("tshours", 10.);
    data_vtab->assign("T_htf_hot_des", 574.);
    data_vtab->assign("T_htf_cold_des", 290.);
    data_vtab->assign("store_fluid", 17);
    std::vector<double> field_fluid_properties{ 1, 7, 0, 0, 0, 0, 0, 0, 0 };
    util::matrix_t<double> field_fl_props(1, 9, &field_fluid_properties);
    data_vtab->assign("store_fl_props", field_fl_props);
    data_vtab->assign("h_tank_min", 1.);
    data_vtab->assign("h_tank", 12.);
    data_vtab->assign("tank_pairs", 1.);
    data_vtab->assign("u_tank", 0.4);
    data_vtab->assign("field_fluid", 17);
    data_vtab->assign("field_fl_props", field_fl_props);
    data_vtab->assign("dt_hot", 5);

    int errors = run_module(data, "ui_tes_calcs");
    EXPECT_FALSE(errors);

    ASSERT_NEAR_FRAC(GetNum(data, "q_tes"), 2791.3, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "tes_avail_vol"), 12986., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "vol_tank"), 14166., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "csp_pt_tes_tank_diameter"), 38.8, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "q_dot_tes_est"), 0.88141, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(data, "csp_pt_tes_htf_density"), 1808.48, kErrorToleranceHi);
    
    ssc_data_free(data);
}

//======Financial Equations=======================================================================
NAMESPACE_TEST(csp_common, TowerSharedWithUi, FinancialCase1) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("total_installed_cost", 673465536.);

    vd->assign("const_per_percent1", 100.);
    vd->assign("const_per_upfront_rate1", 1.);
    vd->assign("const_per_months1", 24.);
    vd->assign("const_per_interest_rate1", 4.);

    vd->assign("const_per_percent2", 0.);
    vd->assign("const_per_upfront_rate2", 0.);
    vd->assign("const_per_months2", 0.);
    vd->assign("const_per_interest_rate2", 0.);

    vd->assign("const_per_percent3", 0.);
    vd->assign("const_per_upfront_rate3", 0.);
    vd->assign("const_per_months3", 0.);
    vd->assign("const_per_interest_rate3", 0.);

    vd->assign("const_per_percent4", 0.);
    vd->assign("const_per_upfront_rate4", 0.);
    vd->assign("const_per_months4", 0.);
    vd->assign("const_per_interest_rate4", 0.);

    vd->assign("const_per_percent5", 0.);
    vd->assign("const_per_upfront_rate5", 0.);
    vd->assign("const_per_months5", 0.);
    vd->assign("const_per_interest_rate5", 0.);

    Financial_Construction_Financing_Equations(vd);

    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_principal1"), 673465472., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_interest1"), 26938618., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_total1"), 33673272., kErrorToleranceHi);

    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_principal2"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_interest2"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_total2"), 0., kErrorToleranceHi);

    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_principal3"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_interest3"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_total3"), 0., kErrorToleranceHi);

    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_principal4"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_interest4"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_total4"), 0., kErrorToleranceHi);

    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_principal5"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_interest5"), 0., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_total5"), 0., kErrorToleranceHi);

    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_percent_total"), 100., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_principal_total"), 673465472., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "const_per_interest_total"), 26938618., kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "construction_financing_cost"), 33673272., kErrorToleranceHi);
    
    delete vd;

}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, FinancialCase2) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("system_capacity", 103500.);

    Financial_Capacity_Payments_Equations(vd);

    //double cp_system_nameplate = vd->lookup("cp_system_nameplate")->num;
    ASSERT_NEAR_FRAC(GetNum(vd, "cp_system_nameplate"), 103.5, kErrorToleranceHi);
    
    delete vd;

}

//======/Testing Molten Salt Power Tower UI Equations=============================================


//TEST(Mspt_cmod_csp_tower_eqns, NoData) {
//	ASSERT_THROW(MSPT_System_Design_Equations(nullptr), std::runtime_error);
//	ASSERT_THROW(Tower_SolarPilot_Solar_Field_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(MSPT_Receiver_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(MSPT_System_Control_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_MSPT_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_DSPT_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_ISCC_Equations(nullptr), std::runtime_error);
//}

//TEST(Mspt_cmod_csp_tower_eqns, MissingVariables) {
//	var_table* vd = new var_table;
//	ASSERT_THROW(MSPT_System_Design_Equations(vd), std::runtime_error);
//	ASSERT_THROW(Tower_SolarPilot_Solar_Field_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(MSPT_Receiver_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(MSPT_System_Control_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_MSPT_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_DSPT_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_ISCC_Equations(vd), std::runtime_error);
//}
