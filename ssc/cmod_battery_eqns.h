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


#ifndef _CMOD_BATTERY_EQNS_H_
#define _CMOD_BATTERY_EQNS_H_

#include "sscapi.h"

#ifdef __cplusplus
extern "C" {
#endif

static const char* size_batterystateful_doc =
    "Resizes the battery for a battery_stateful data object \\n\\n"
    "Input: var_table with key-value pairs\\n"
    "     'nominal_energy': double [kWh]\\n"
    "     'desired_capacity': double [kWh]\\n"
    "     'desired_voltage': double [V]\\n"
    "     'mass': double [kg] \\n"
    "     'surface_area': double [m^2],\\n"
    "     'module_capacity': double [kWh], optional\\n"
    "     'module_surface_area': double [m^2], optional\\n"
    "Output: key-value pairs added to var_table, mass, surface_area, and nominal_energy will be modified\\n"
    "     'original_capacity': kWh [kWh]\\n";


SSCEXPORT bool Size_batterystateful(ssc_data_t data);

static const char* calculate_thermal_params_doc =
    "Resizes the battery for a battery_stateful data object \\n\\n"
    "Input: var_table with key-value pairs\\n"
    "     'original_capacity': double [kWh]\\n"
    "     'desired_capacity': double [kWh]\\n"
    "     'mass': double [kg] \\n"
    "     'surface_area': double [m^2],\\n"
    "     'module_capacity': double [kWh], optional\\n"
    "     'module_surface_area': double [m^2], optional\\n"
    "Output: mass and surface_area will be modified\\n";

SSCEXPORT bool Calculate_thermal_params(ssc_data_t data);

#ifdef __cplusplus
}
#endif

#endif
