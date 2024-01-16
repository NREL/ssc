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
#include "trough_physical_defaults.h"
#include "csp_common_test.h"
#include "vs_google_test_explorer_namespace.h"

namespace csp_trough {}
using namespace csp_trough;

//========Tests===================================================================================
NAMESPACE_TEST(csp_trough, PowerTroughCmod, Default_NoFinancial)
{
    ssc_data_t defaults = trough_physical_defaults();
    CmodUnderTest power_trough = CmodUnderTest("trough_physical", defaults);
    int errors = power_trough.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {

        double annual_energy = power_trough.GetOutput("annual_energy");
        double annual_thermal_consumption = power_trough.GetOutput("annual_thermal_consumption");
        double annual_tes_freeze_protection = power_trough.GetOutput("annual_tes_freeze_protection");
        double annual_field_freeze_protection = power_trough.GetOutput("annual_field_freeze_protection");
        double capacity_factor = power_trough.GetOutput("capacity_factor");
        double annual_W_cycle_gross = power_trough.GetOutput("annual_W_cycle_gross");
        double kwh_per_kw = power_trough.GetOutput("kwh_per_kw");
        double conversion_factor = power_trough.GetOutput("conversion_factor");
        double annual_total_water_use = power_trough.GetOutput("annual_total_water_use");

        double time_hr = power_trough.GetOutputSum("time_hr");
        double month = power_trough.GetOutputSum("month");
        double beam = power_trough.GetOutputSum("beam");
        double defocus = power_trough.GetOutputSum("defocus");
        double q_dc_tes = power_trough.GetOutputSum("q_dc_tes");
        double P_fixed = power_trough.GetOutputSum("P_fixed");
        double op_mode_1 = power_trough.GetOutputSum("op_mode_1");
        double n_op_modes = power_trough.GetOutputSum("n_op_modes");
        double is_rec_su_allowed = power_trough.GetOutputSum("is_rec_su_allowed");

        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_energy"), 377677449, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_thermal_consumption"), 872331, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_tes_freeze_protection"), 692200, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_field_freeze_protection"), 180131, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("capacity_factor"), 43.16, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_W_cycle_gross"), 430999201, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("kwh_per_kw"), 3781, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("conversion_factor"), 87.63, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_total_water_use"), 82009, kErrorToleranceHi);

        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("time_hr"), 38373180, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("month"), 57168, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("beam"), 2687889, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("defocus"), 8747, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("q_dc_tes"), 343833, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("P_fixed"), 5348, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("op_mode_1"), 53565, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("n_op_modes"), 9800, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("is_rec_su_allowed"), 8759, kErrorToleranceHi);
        //EXPECT_NEAR_FRAC(power_trough.GetOutputSum("operating_modes_a"), 35458021, kErrorToleranceHi);
    }
    //ssc_data_t defaults = singleowner_defaults();
    //CmodUnderTest singleowner = CmodUnderTest("singleowner", defaults);
    //int errors = singleowner.RunModule();
    //EXPECT_FALSE(errors);
    //if (!errors) {
    //    EXPECT_NEAR_FRAC(singleowner.GetOutput(""), , kErrorToleranceLo);
    //}
}

NAMESPACE_TEST(csp_trough, PowerTroughCmod, Start_Stop_Initialize_Default_NoFinancial)
{
    ssc_data_t defaults = trough_physical_defaults();
    CmodUnderTest power_trough = CmodUnderTest("trough_physical", defaults);
    int errors = power_trough.RunModule();
    EXPECT_FALSE(errors);
    std::vector<double> original_gen = power_trough.GetOutputVector("gen");
    // first short run
    power_trough.SetInput("time_start", 0);
    power_trough.SetInput("time_stop", 3600 * 10);   // first 10 hours
    power_trough.SetInput("vacuum_arrays", 1);
    errors = power_trough.RunModule();
    EXPECT_FALSE(errors);
    std::vector<double> step1_gen = power_trough.GetOutputVector("gen");
    double step1_gen_sum = power_trough.GetOutputSum("gen");

    // second short run with restart
    power_trough.SetInput("time_start", 3600 * 10);
    power_trough.SetInput("time_stop", 3600 * 20);   // next 10 hours
    power_trough.SetInput("vacuum_arrays", 1);
    // Reset plant state - solar field
    power_trough.SetInput("defocus_initial", power_trough.GetOutputVector("defocus_final").back());
    power_trough.SetInput("rec_op_mode_initial", power_trough.GetOutputVector("rec_op_mode_final").back());
    power_trough.SetInput("T_in_loop_initial", power_trough.GetOutputVector("T_in_loop_final").back());
    power_trough.SetInput("T_out_loop_initial", power_trough.GetOutputVector("T_out_loop_final").back());
    std::vector<ssc_number_t> T_out_scas_initial = power_trough.GetOutputVector("T_out_scas_last_final");
    power_trough.SetInput("T_out_scas_initial", &T_out_scas_initial[0], T_out_scas_initial.size()); // pass the whole array
    // Power cycle
    power_trough.SetInput("pc_op_mode_initial", power_trough.GetOutputVector("pc_op_mode_final").back());
    power_trough.SetInput("pc_startup_time_remain_init", power_trough.GetOutputVector("pc_startup_time_remain_final").back());
    power_trough.SetInput("pc_startup_energy_remain_initial", power_trough.GetOutputVector("pc_startup_energy_remain_final").back());
    // TES
    power_trough.SetInput("T_tank_cold_init", power_trough.GetOutputVector("T_tes_cold").back());
    power_trough.SetInput("T_tank_hot_init", power_trough.GetOutputVector("T_tes_hot").back());
    power_trough.SetInput("init_hot_htf_percent", power_trough.GetOutputVector("hot_tank_htf_percent_final").back());
    errors = power_trough.RunModule();
    EXPECT_FALSE(errors);
    std::vector<double> step2_gen = power_trough.GetOutputVector("gen");
    double step2_gen_sum = power_trough.GetOutputSum("gen");

    if (!errors) {
        EXPECT_GT(step1_gen_sum, 0.0);  // positive generation
        EXPECT_GT(step2_gen_sum, 0.0);
        // checking generation array
        for (size_t i = 0; i < step1_gen.size() + step2_gen.size(); i++) {
            if (i < step1_gen.size()) {
                EXPECT_NEAR_FRAC(std::abs(original_gen[i]), std::abs(step1_gen[i]), kErrorToleranceHi);
            }
            else {
                EXPECT_NEAR_FRAC(std::abs(original_gen[i]), std::abs(step2_gen[i - step1_gen.size()]), kErrorToleranceHi);
            }
        }
    }
}
