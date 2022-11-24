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
#include "tcsdirect_steam_defaults.h"
#include "csp_common_test.h"
#include "vs_google_test_explorer_namespace.h"


namespace csp_tower {}
using namespace csp_tower;

////========Tests===================================================================================
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, Default_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 263809742, kErrorToleranceHi);
//        EXPECT_NEAR(steam_tower.GetOutput("annual_fuel_usage"), 0., kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 30.08, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 296630582, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 2635, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 92.64, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 3.413, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 55716, kErrorToleranceHi);
//    }
//}
//
//// Alternative condenser type : Evaporative
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, EvaporativeCondenser_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    steam_tower.SetInput("ct", 1);
//    steam_tower.SetInput("eta_ref", 0.404);
//    steam_tower.SetInput("startup_frac", 0.5);
//    steam_tower.SetInput("P_cond_min", 2);
//
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 280975356, kErrorToleranceHi);
//        EXPECT_NEAR(steam_tower.GetOutput("annual_fuel_usage"), 0., kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 32.03, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 307624737, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 2806, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 95.14, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 3.413, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 893431, kErrorToleranceHi);
//    }
//}
//
//// Alternative condenser type : Hybrid
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, HybridCondenser_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    steam_tower.SetInput("ct", 3);
//    steam_tower.SetInput("eta_ref", 0.404);
//    steam_tower.SetInput("startup_frac", 0.5);
//    steam_tower.SetInput("P_cond_min", 2);
//
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 268116066, kErrorToleranceHi);
//        EXPECT_NEAR(steam_tower.GetOutput("annual_fuel_usage"), 0., kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 30.57, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 304066728, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 2678, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 91.85, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 3.413, kErrorToleranceHi);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 55716, kErrorToleranceHi);
//    }
//}

//// Fossil dispatch
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, FossilDispatch_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    steam_tower.SetInput("fossil_mode", 2);
//    steam_tower.SetInput("eta_ref", 0.404);
//    steam_tower.SetInput("startup_frac", 0.5);
//    steam_tower.SetInput("P_cond_min", 2);
//
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Receiver material : T91 Steel
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, ReceiverT91Steel_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    steam_tower.SetInput("mat_boiler", 28);
//    steam_tower.SetInput("mat_sh", 28);
//    steam_tower.SetInput("mat_rh", 28);
//    steam_tower.SetInput("eta_ref", 0.404);
//    steam_tower.SetInput("startup_frac", 0.5);
//    steam_tower.SetInput("P_cond_min", 2);
//
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Alternative receiver flow pattern
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, FlowPattern1_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    steam_tower.SetInput("flowtype", 1);
//    steam_tower.SetInput("eta_ref", 0.404);
//    steam_tower.SetInput("startup_frac", 0.5);
//    steam_tower.SetInput("P_cond_min", 2);
//
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Alternative heliostat focusing method: Flat
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, HeliostatFlatFocusing_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    steam_tower.SetInput("focus_type", 0);
//    steam_tower.SetInput("eta_ref", 0.404);
//    steam_tower.SetInput("startup_frac", 0.5);
//    steam_tower.SetInput("P_cond_min", 2);
//
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//
//// Alternative heliostat canting method: Equinox
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, HeliostatEquinoxCanting_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    steam_tower.SetInput("cant_type", 2);
//    steam_tower.SetInput("eta_ref", 0.404);
//    steam_tower.SetInput("startup_frac", 0.5);
//    steam_tower.SetInput("P_cond_min", 2);
//
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Phoenix, AZ
//NAMESPACE_TEST(csp_tower, SteamTowerCmod, Phoeniz_NoFinancial)
//{
//    ssc_data_t defaults = tcsdirect_steam_defaults();
//    CmodUnderTest steam_tower = CmodUnderTest("tcsdirect_steam", defaults);
//    char solar_resource_path_tucson[512];
//    int n2 = sprintf(solar_resource_path_tucson, "%s/test/input_cases/directsteam_data/tucson_az_32.116521_-110.933042_psmv3_60_tmy.csv", std::getenv("SSCDIR"));
//    steam_tower.SetInput("solar_resource_file", solar_resource_path_tucson);
//
//    int errors = steam_tower.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(steam_tower.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
