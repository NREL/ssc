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


#include <gtest/gtest.h>
//#include "tcsmolten_salt_defaults.h"
#include "csp_common_test.h"
#include "vs_google_test_explorer_namespace.h"

//#include "../input_cases/code_generator_utilities.h"

namespace sco2_tests {}
using namespace sco2_tests;

//========Tests===================================================================================
NAMESPACE_TEST(sco2_tests, SCO2Cycle, Parametrics)
{
    
    ssc_data_t data = ssc_data_create();

    ssc_data_set_number(data, "t_amb_des", 26);
    ssc_data_set_number(data, "dt_mc_approach", 6);
    ssc_data_set_number(data, "t_htf_hot_des", 720);
    ssc_number_t p_od_cases[12] = { 720, 1, 26, 1, 1, 1, 720, 1, 20, 1, 1, 1 };

    ssc_data_set_number(data, "n_nodes_air_cooler_pass", 10);
    ssc_data_set_number(data, "htf", 6);
    ssc_data_set_number(data, "design_method", 3);
    ssc_data_set_number(data, "fan_power_frac", 0.02);
    ssc_data_set_number(data, "deltap_counterhx_frac", -1);
    ssc_data_set_number(data, "w_dot_net_des", 50);
    ssc_data_set_number(data, "ltr_ua_des_in", -1);
    ssc_data_set_number(data, "dt_phx_hot_approach", 20);
    ssc_data_set_number(data, "site_elevation", 588);
    ssc_data_set_number(data, "ua_recup_tot_des", -1);
    ssc_data_set_number(data, "eta_thermal_des", -1);
    ssc_data_set_number(data, "rel_tol", 3);
    ssc_data_set_number(data, "ltr_design_code", 2);
    ssc_data_set_number(data, "is_gen_od_polynomials", 0);
    ssc_data_set_number(data, "ltr_min_dt_des_in", 10);
    ssc_data_set_number(data, "lt_recup_eff_max", 1);
    ssc_data_set_number(data, "ltr_eff_des_in", -1);
    ssc_data_set_number(data, "p_high_limit", 25);
    ssc_data_set_number(data, "eta_isen_mc", 0.84999999999999998);
    ssc_data_set_number(data, "ltr_lp_deltap_des_in", 0.031099999999999999);
    ssc_data_set_number(data, "ltr_hp_deltap_des_in", 0.0055999999999999999);
    ssc_data_set_number(data, "htr_design_code", 2);
    ssc_data_set_number(data, "htr_ua_des_in", -1);
    ssc_data_set_number(data, "od_rel_tol", 3);
    ssc_data_set_number(data, "htr_min_dt_des_in", 10);
    ssc_data_set_number(data, "od_opt_objective", 0);
    ssc_data_set_number(data, "ht_recup_eff_max", 1);
    ssc_data_set_number(data, "htr_eff_des_in", -1);
    ssc_data_set_number(data, "htr_lp_deltap_des_in", 0.031099999999999999);
    ssc_data_set_number(data, "htr_hp_deltap_des_in", 0.0055999999999999999);
    
    ssc_data_set_matrix(data, "od_cases", p_od_cases, 2, 6);
    ssc_data_set_number(data, "cycle_config", 1);
    ssc_data_set_number(data, "des_objective", 1);
    ssc_data_set_number(data, "is_recomp_ok", 1);
    ssc_data_set_number(data, "is_p_high_fixed", 1);
    ssc_data_set_number(data, "is_pr_fixed", 0);
    ssc_data_set_number(data, "od_t_t_in_mode", 0);
    ssc_data_set_number(data, "is_ip_fixed", 0);
    ssc_data_set_number(data, "min_phx_deltat", 1000);
    ssc_data_set_number(data, "ltr_od_model", 1);
    ssc_data_set_number(data, "deltap_cooler_frac", 0.0050000000000000001);
    ssc_data_set_number(data, "eta_isen_rc", 0.84999999999999998);
    ssc_data_set_number(data, "eta_isen_pc", 0.84999999999999998);
    ssc_data_set_number(data, "eta_isen_t", 0.90000000000000002);
    ssc_data_set_number(data, "phx_co2_deltap_des_in", 0.0055999999999999999);
    ssc_data_set_number(data, "mc_comp_type", 1);
    ssc_data_set_number(data, "dt_phx_cold_approach", 20);
    ssc_data_set_number(data, "ltr_n_sub_hx", 10);
    ssc_data_set_number(data, "htr_n_sub_hx", 10);
    ssc_data_set_number(data, "htr_od_model", 1);
    ssc_data_set_number(data, "phx_n_sub_hx", 10);
    ssc_data_set_number(data, "phx_od_model", 1);
    ssc_data_set_number(data, "is_design_air_cooler", 1);
    ssc_data_set_number(data, "eta_air_cooler_fan", 0.5);
    
    CmodUnderTest sco2 = CmodUnderTest("sco2_csp_system", data);
    
    int errors = sco2.RunModule();
    EXPECT_FALSE(errors);
    
    if (!errors) {
        EXPECT_NEAR_FRAC(sco2.GetOutput("T_htf_cold_des"), 529.6897, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(sco2.GetOutput("eta_thermal_calc"), 0.5071197, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(sco2.GetOutput("m_dot_htf_des"), 513.344, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(sco2.GetOutput("m_dot_co2_full"), 410.528, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(sco2.GetOutput("P_comp_in"), 7.67490, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(sco2.GetOutput("cycle_cost"), 53.2909, kErrorToleranceLo);

        std::vector<ssc_number_t> eta_thermal_od_exp{ 0.50689, 0.507197 };
        EXPECT_FLOATS_NEARLY_EQ(sco2.GetOutputVector("eta_thermal_od"), eta_thermal_od_exp, kErrorToleranceLo*eta_thermal_od_exp[0]);

        std::vector<ssc_number_t> P_comp_in_od_exp{ 7.68424, 7.64808 };
        EXPECT_FLOATS_NEARLY_EQ(sco2.GetOutputVector("P_comp_in_od"), P_comp_in_od_exp, kErrorToleranceLo*P_comp_in_od_exp[0]);

        std::vector<ssc_number_t> W_dot_net_less_cooling_od_exp{ 48.9136, 49.98451 };
        EXPECT_FLOATS_NEARLY_EQ(sco2.GetOutputVector("W_dot_net_less_cooling_od"), W_dot_net_less_cooling_od_exp, kErrorToleranceLo*W_dot_net_less_cooling_od_exp[0]);

    }
    
}
