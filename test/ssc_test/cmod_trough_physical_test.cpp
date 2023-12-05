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
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_energy"), 376824118, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_thermal_consumption"), 628322, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_tes_freeze_protection"), 590329, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_field_freeze_protection"), 38042, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("capacity_factor"), 43.05, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_W_cycle_gross"), 429613242, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("kwh_per_kw"), 3772, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("conversion_factor"), 87.84, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutput("annual_total_water_use"), 80708, kErrorToleranceHi);

        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("time_hr"), 38373180, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("month"), 57168, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("beam"), 2687889, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("defocus"), 8753, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("q_dc_tes"), 333996, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("P_fixed"), 5347, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("op_mode_1"), 52902, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("n_op_modes"), 9799, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_trough.GetOutputSum("is_rec_su_allowed"), 8760, kErrorToleranceHi);
        //EXPECT_NEAR_FRAC(power_trough.GetOutputSum("operating_modes_a"), 35458021, kErrorToleranceHi);
    }
    else
    {
        int x = 0;
    }

    //ssc_data_t defaults = singleowner_defaults();
    //CmodUnderTest singleowner = CmodUnderTest("singleowner", defaults);
    //int errors = singleowner.RunModule();
    //EXPECT_FALSE(errors);
    //if (!errors) {
    //    EXPECT_NEAR_FRAC(singleowner.GetOutput(""), , kErrorToleranceLo);
    //}
}
