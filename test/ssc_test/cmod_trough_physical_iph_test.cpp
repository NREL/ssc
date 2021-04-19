#include <gtest/gtest.h>
#include "trough_physical_iph_defaults.h"
#include "csp_common_test.h"
#include "vs_google_test_explorer_namespace.h"

namespace csp_trough {}
using namespace csp_trough;

//========Tests===================================================================================
NAMESPACE_TEST(csp_trough, HeatTroughCmod, Default_NoFinancial)
{
    ssc_data_t defaults = trough_physical_iph_defaults();
    CmodUnderTest heat_trough = CmodUnderTest("trough_physical_process_heat", defaults);
    int errors = heat_trough.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {

        EXPECT_NEAR_FRAC(heat_trough.GetOutput("annual_gross_energy"), 24267285, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(heat_trough.GetOutput("annual_energy"), 24267070, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(heat_trough.GetOutput("annual_electricity_consumption"), 92979, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(heat_trough.GetOutput("annual_thermal_consumption"), 215.00, kErrorToleranceHi);
        EXPECT_NEAR_FRAC(heat_trough.GetOutput("annual_tes_freeze_protection"), 215.00, kErrorToleranceHi);
        EXPECT_NEAR(heat_trough.GetOutput("annual_field_freeze_protection"), 0., kErrorToleranceHi);
        EXPECT_NEAR_FRAC(heat_trough.GetOutput("annual_total_water_use"), 176.3, kErrorToleranceHi);

    }
}
