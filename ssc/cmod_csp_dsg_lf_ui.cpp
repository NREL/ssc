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

#include "core.h"

#include "water_properties.h"


static var_info _cm_vtab_csp_dsg_lf_ui[] = {

    /*   VARTYPE   DATATYPE         NAME               LABEL                                            UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
    { SSC_INPUT,   SSC_NUMBER,   "P_boil",           "Boiling pressure",                             "bar",    "", "",  "*",  "", "" },

    { SSC_OUTPUT,  SSC_NUMBER,   "T_saturation",     "Saturation pressure",                          "C",      "", "",  "*",  "", "" },

    var_info_invalid };

class cm_csp_dsg_lf_ui : public compute_module
{
public:

    cm_csp_dsg_lf_ui()
    {
        add_var_info(_cm_vtab_csp_dsg_lf_ui);
    }

    void exec() override
    {
        water_state wp;

        double P_boil = as_double("P_boil") * 100.0;    //[kPa]

        int wp_code = water_PQ(P_boil, 0.0, &wp);

        double T_sat = std::numeric_limits<double>::quiet_NaN();
        if (wp_code != 0) {
            T_sat = -987.6;
        }
        else {
            T_sat = wp.temp - 273.15;   //[C]
        }

        assign("T_saturation", (ssc_number_t)T_sat);

        return;
    }
};

DEFINE_MODULE_ENTRY(csp_dsg_lf_ui, "Calculates values for all calculated values on UI TES page(s)", 0)

