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
#include "python_handler.h"


static var_info _cm_vtab_wind_landbosse[] = {
/*   VARTYPE           DATATYPE         NAME                              LABEL                                                      UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

        // Inputs
        { SSC_INPUT,        SSC_NUMBER,      "machine_rating",                "Machine Rating",                                          "kW",     "",                      "wind_bos",      "*",                       "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "rotor_diameter",                "Rotor Diameter",                                          "m",      "",                      "wind_bos",      "*",                       "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "hub_height",                    "Hub Height",                                              "m",      "",                      "wind_bos",      "*",                       "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "number_of_turbines",            "Number of Turbines",                                      "",       "",                      "wind_bos",      "*",                       "INTEGER",                       "" },
        { SSC_INPUT,        SSC_NUMBER,      "interconnect_voltage",          "Interconnect Voltage",                                    "kV",     "",                      "wind_bos",      "*",                       "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "distance_to_interconnect",      "Distance to Interconnect",                                "miles",  "",                      "wind_bos",      "*",                       "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "site_terrain",                  "Site Terrain",                                            "",       "",                      "wind_bos",      "*",                       "INTEGER",                       "" },
        { SSC_INPUT,        SSC_NUMBER,      "turbine_layout",                "Turbine Layout",                                          "",       "",                      "wind_bos",      "*",                       "INTEGER",                       "" },
        { SSC_INPUT,        SSC_NUMBER,      "soil_condition",                "Soil Condition",                                          "",       "",                      "wind_bos",      "*",                       "INTEGER",                       "" },
        var_info_invalid
};

class cm_wind_landbosse : public compute_module {
public:
    cm_wind_landbosse() {
        add_var_info(_cm_vtab_wind_landbosse);
    }

    void exec() override {

    }
};

DEFINE_MODULE_ENTRY( wind_landbosse, "Land-based Balance-of-System Systems Engineering (LandBOSSE) cost model", 1 )
