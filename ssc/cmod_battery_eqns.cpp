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
#include "cmod_battery_eqns.h"

#include "vartab.h"

#include <cmath>

void Size_batterystateful(ssc_data_t data) {
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    double nominal_energy, desired_voltage, desired_capacity;

    vt_get_number(vt, "nominal_energy", &nominal_energy);
    vt_get_number(vt, "desired_voltage", &desired_voltage);
    vt_get_number(vt, "desired_capacity", &desired_capacity);

    vt->assign("original_capacity", nominal_energy);

    Calculate_thermal_params(data);

    vt->assign("nominal_energy", desired_capacity);
    vt->assign("nominal_voltage", desired_voltage);
}

void Calculate_thermal_params(ssc_data_t data) {
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
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
}

