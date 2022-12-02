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

#include "cmod_battery_eqns.h"

#include "core.h"
#include "vartab.h"

#include <cmath>

bool Size_batterystateful(ssc_data_t data) {
    auto vt = static_cast<var_table*>(data);
    char errmsg[250];
    if (!vt) {
        return false;
    }

    double nominal_energy, desired_voltage, desired_capacity;

    vt_get_number(vt, "nominal_energy", &nominal_energy);
    vt_get_number(vt, "desired_voltage", &desired_voltage);
    vt_get_number(vt, "desired_capacity", &desired_capacity);

    // Cannot specify energy of zero (less than mW, really) due to resulting errors in scaling factors
    if (nominal_energy < 1e-7) {
        sprintf(errmsg, "nominal_energy cannot be less than 1e-7. Current value: %f", nominal_energy);
        vt->assign("error", std::string(errmsg));
        return false;
    }

    if (desired_capacity < 1e-7) {
        sprintf(errmsg, "desired_capacity cannot be less than 1e-7. Current value: %f", desired_capacity);
        vt->assign("error", std::string(errmsg));
        return false;
    }

    vt->assign("original_capacity", nominal_energy);

    bool thermal_success = Calculate_thermal_params(data);

    vt->assign("nominal_energy", desired_capacity);
    vt->assign("nominal_voltage", desired_voltage);

    return thermal_success;
}

bool Calculate_thermal_params(ssc_data_t data) {
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        return false;
    }

    double mass, surface_area, original_capacity, desired_capacity, module_capacity, module_surface_area;

    vt_get_number(vt, "mass", &mass);
    vt_get_number(vt, "surface_area", &surface_area);
    vt_get_number(vt, "original_capacity", &original_capacity);
    vt_get_number(vt, "desired_capacity", &desired_capacity);

    double mass_per_specific_energy = mass / original_capacity;

    double volume = std::pow((surface_area / 6.0), (3.0 / 2.0));

    double volume_per_specific_energy = volume / original_capacity;

    mass = mass_per_specific_energy * desired_capacity;

    surface_area = std::pow((volume_per_specific_energy * desired_capacity), (2.0 / 3.0)) * 6;

    if (vt->is_assigned("module_capacity") && vt->is_assigned("module_surface_area")) {
        vt_get_number(vt, "module_capacity", &module_capacity);
        vt_get_number(vt, "module_surface_area", &module_surface_area);
        surface_area = module_surface_area * desired_capacity / module_capacity;
    }

    vt->assign("mass", mass);
    vt->assign("surface_area", surface_area);

    return true;
}

