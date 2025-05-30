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

#include "cmod_fresnel_physical_test.h"


TEST_F(CmodFresnelPhysicalTest, MSLFDefault) {
    std::string file_inputs = SSCDIR;
    file_inputs += "/test/input_json/TechnologyModels/fresnel_physical/2025.03.13_develop_branch_MSLF_Single_Owner_cmod_fresnel_physical.json";
    std::string file_outputs = SSCDIR;
    file_outputs +="/test/input_json/TechnologyModels/fresnel_physical/2025.03.13_develop_branch_MSLF_Single_Owner_cmod_fresnel_physical_outputs.json";
    std::vector<std::string> compare_number_variables = { "annual_energy", "q_dot_loss_tes_des" };
    std::vector<std::string> compare_array_variables = { "monthly_energy" };

    Test("fresnel_physical", file_inputs, file_outputs, compare_number_variables, compare_array_variables, 0.001, "tucson_az_32.116521_-110.933042_psmv3_60_tmy.csv");
}
