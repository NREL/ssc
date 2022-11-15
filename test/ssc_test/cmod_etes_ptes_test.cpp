/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <gtest/gtest.h>
#include "etes_ptes_defaults.h"
#include "csp_common_test.h"
#include "vs_google_test_explorer_namespace.h"

namespace etes_ptes_test {}
using namespace etes_ptes_test;

//========Tests===================================================================================
NAMESPACE_TEST(etes_ptes_test, EtesPtesCmod, Default_NoFinancial)
{
    ssc_data_t defaults = etes_ptes_defaults();
    CmodUnderTest ptes_system = CmodUnderTest("etes_ptes", defaults);
    ptes_system.SetInput("is_dispatch", 0);
    ptes_system.SetInput("disp_timeout", 60);
    ptes_system.SetInput("disp_max_iter", 1000000);

    int errors = ptes_system.RunModule();
    double ann_energy = ptes_system.GetOutput("annual_energy");
    EXPECT_FALSE(errors);
    if (!errors) {
//        EXPECT_NEAR_FRAC(std::abs(ptes_system.GetOutput("annual_energy")), std::abs(-2.64339E8), kErrorToleranceHi) << " annual_energy = " << ann_energy;
        EXPECT_NEAR_FRAC(std::abs(ptes_system.GetOutput("annual_energy")), std::abs(-2.64339E8), 0.0) << " annual_energy = " << ann_energy;
    }

    ptes_system.SetInput("is_dispatch", 1);
    errors = ptes_system.RunModule();
    ann_energy = ptes_system.GetOutput("annual_energy");
    EXPECT_FALSE(errors);
    if (!errors) {
//        EXPECT_NEAR_FRAC(std::abs(ptes_system.GetOutput("annual_energy")), std::abs(-2.03107e+08), kErrorToleranceHi) << " dispatch annual_energy = " << ann_energy;
        EXPECT_NEAR_FRAC(std::abs(ptes_system.GetOutput("annual_energy")), std::abs(-2.03107e+08), 0.0) << " dispatch annual_energy = " << ann_energy;
    }

}
