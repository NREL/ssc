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
#include "tcsfresnel_molten_salt_defaults.h"
#include "csp_common_test.h"
#include "vs_google_test_explorer_namespace.h"

namespace csp_tower {}
using namespace csp_tower;

//========Tests===================================================================================
NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, Default_NoFinancial)
{
    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
    int errors = power_fresnel.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 334764976, kErrorToleranceHi);
        EXPECT_NEAR(power_fresnel.GetOutput("annual_fuel_usage"), 0, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 38.21, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 370674015, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 3347, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 94.27, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 3.413, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 29688, kErrorToleranceHi);
    }
}

// Defocusing strategy: Sequenced
NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, SequencedDefocusing_NoFinancial)
{
    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
    power_fresnel.SetInput("fthrctrl", 1);

    int errors = power_fresnel.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 334764976, kErrorToleranceHi);
        EXPECT_NEAR(power_fresnel.GetOutput("annual_fuel_usage"), 0, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 38.21, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 370674015, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 3347, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 94.27, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 3.413, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 29688, kErrorToleranceHi);
    }
}

// Field HTF: Therminol VP-1
NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, TherminolVp1Htf_NoFinancial)
{
    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
    power_fresnel.SetInput("Fluid", 21);
    power_fresnel.SetInput("field_fluid", 21);
    power_fresnel.SetInput("is_hx", 1);
    power_fresnel.SetInput("V_tank_hot_ini", 1290.5642);
    power_fresnel.SetInput("vol_tank", 6452.821);

    int errors = power_fresnel.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 331997624, kErrorToleranceHi);
        EXPECT_NEAR(power_fresnel.GetOutput("annual_fuel_usage"), 0, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 37.89, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 367478229, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 3319, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 94.31, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 3.413, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 29445, kErrorToleranceHi);
    }
}

// Optical characterization method: Solar position 
NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, SolarPositioinOpticalChar_NoFinancial)
{
    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
    power_fresnel.SetInput("opt_model", 1);

    int errors = power_fresnel.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 223974947, kErrorToleranceHi);
        EXPECT_NEAR(power_fresnel.GetOutput("annual_fuel_usage"), 0, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 25.57, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 250756546, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 2239, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 93.26, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 3.413, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 21326, kErrorToleranceHi);
    }
}

//// Receiver model type: Polynomial heat loss model
//NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, PolynomialHeatLoss_NoFinancial)
//{
//    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
//    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
//    power_fresnel.SetInput("nLoops", 148);
//    power_fresnel.SetInput("rec_model", 1);
//
//    int errors = power_fresnel.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Condenser type: Evaporative
//NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, EvaporativeCondenser_NoFinancial)
//{
//    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
//    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
//    power_fresnel.SetInput("CT", 1);
//
//    int errors = power_fresnel.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Condenser type: Hybrid
//NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, HybridCondenser_NoFinancial)
//{
//    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
//    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
//    power_fresnel.SetInput("CT", 3);
//
//    int errors = power_fresnel.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Turbine inlet pressure control: Sliding pressure
//NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, TurbineSlidingPressure_NoFinancial)
//{
//    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
//    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
//    power_fresnel.SetInput("tech_type", 3);
//
//    int errors = power_fresnel.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// HTF freeze protection mode: Electric heating
//NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, ElectricFreezeProtection_NoFinancial)
//{
//    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
//    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
//    power_fresnel.SetInput("fp_mode", 1);
//
//    int errors = power_fresnel.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Storage HTF: Therminol VP-1
//NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, TherminolVp1Storage_NoFinancial)
//{
//    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
//    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
//    power_fresnel.SetInput("store_fluid", 21);
//    power_fresnel.SetInput("is_hx", 1);
//    power_fresnel.SetInput("V_tank_hot_ini", 1963.66443);
//    power_fresnel.SetInput("vol_tank", 9818.3223);
//
//    int errors = power_fresnel.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
//
//// Power Cycle: User Defined
//NAMESPACE_TEST(csp_fresnel, PowerFresnelCmod, UserDefinedPowerCycle_NoFinancial)
//{
//    ssc_data_t defaults = tcsfresnel_molten_salt_defaults();
//    CmodUnderTest power_fresnel = CmodUnderTest("tcsmslf", defaults);
//    power_fresnel.SetInput("pc_config", 1);
//
//    int errors = power_fresnel.RunModule();
//    EXPECT_FALSE(errors);
//    if (!errors) {
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_energy"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_fuel_usage"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("capacity_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_W_cycle_gross"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("kwh_per_kw"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("conversion_factor"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("system_heat_rate"), 571408807, kErrorToleranceLo);
//        EXPECT_NEAR_FRAC(power_fresnel.GetOutput("annual_total_water_use"), 571408807, kErrorToleranceLo);
//    }
//}
