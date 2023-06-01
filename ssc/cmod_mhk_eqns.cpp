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


#include <cmath>

#include "vartab.h"

#include "cmod_mhk_eqns.h"
#pragma warning(disable: 4297)  // ignore warning: 'function assumed not to throw an exception but does'

bool me_array_cable_length(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt){
        return false;
    }

    double devices_per_row, device_spacing_in_row, number_rows, row_spacing, cable_system_overbuild, floating_array, export_cable_redundancy, water_depth, number_devices, distance_to_shore;

    vt_get_number(vt, "devices_per_row", &devices_per_row);
    vt_get_number(vt, "device_spacing_in_row", &device_spacing_in_row);
    vt_get_number(vt, "number_rows", &number_rows);
    vt_get_number(vt, "row_spacing", &row_spacing);
    vt_get_number(vt, "cable_system_overbuild", &cable_system_overbuild);
    vt_get_number(vt, "floating_array", &floating_array);
    vt_get_number(vt, "export_cable_redundancy", &export_cable_redundancy);
    vt_get_number(vt, "water_depth", &water_depth);
    vt_get_number(vt, "number_devices", &number_devices);
    vt_get_number(vt, "distance_to_shore", &distance_to_shore);


	double length = (devices_per_row - 1) * device_spacing_in_row * number_rows + row_spacing * (number_rows - 1);
	length *= (1.0 + cable_system_overbuild / 100.0);
	var_data cablelength = var_data(length);
	vt->assign("inter_array_cable_length", cablelength);

	if (std::abs(floating_array) > 0.1)
	{
		length = 1.5 * water_depth * number_devices;
		length *= (1.0 + cable_system_overbuild / 100.0);
	}
	else
	{
		length = 0;
	}
	vt->assign("riser_cable_length", var_data(length));

	if (std::abs(export_cable_redundancy) > 0.1)
	{
		length = (water_depth + distance_to_shore) * 2;
		length *= (1.0 + cable_system_overbuild / 100.0);
	}
	else
	{
		length = water_depth + distance_to_shore;
		length *= (1.0 + cable_system_overbuild / 100.0);
	}
	vt->assign("export_cable_length", var_data(length));
    return true;
}

bool tidal_turbine_calculate_powercurve(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        return false;
    }

    double turbine_size, rotor_diameter, elevation, max_tip_speed, max_tip_sp_ratio, cut_in,
        cut_out, rotor_area, generator_rated_capacity, water_depth, velocity_power_law_fit, number_rotors;
    int drive_train;
    util::matrix_t<double> tidal_resource;
    double min_vel;
    int max_cp_length, pto_efficiency_length;
    std::vector<double> pto_efficiency;
    std::vector<double> max_cp;

    try {
        vt_get_number(vt, "tidal_turbine_rotor_diameter", &rotor_diameter);     // ssc input
        vt_get_number(vt, "number_rotors", &number_rotors);
        vt_get_array_vec(vt, "tidal_turbine_max_cp", max_cp);                     // ssc input
        vt_get_array_vec(vt, "pto_efficiency", pto_efficiency);
        vt_get_number(vt, "cut_in", &cut_in);
        vt_get_number(vt, "cut_out", &cut_out);
        vt_get_matrix(vt, "tidal_resource", tidal_resource);
        vt_get_number(vt, "generator_rated_capacity", &generator_rated_capacity);

    }
    catch (std::runtime_error& e) {
        vt->assign("error", var_data(e.what()));
        return false;
    }

    util::matrix_t<ssc_number_t> powercurve_tidespeeds;
    util::matrix_t<ssc_number_t> powercurve_powerout;
    util::matrix_t<ssc_number_t> powercurve_hub_efficiency;

    char errmsg[250];


    size_t array_size = tidal_resource.nrows();

    powercurve_tidespeeds.resize(array_size);
    powercurve_powerout.resize(array_size);
    rotor_area = pow((rotor_diameter / 2), 2) * M_PI * number_rotors;
    double tidal_vel, p_fluid, p_rotor, eff, p_electric;
    double max_cp_value, pto_eff_value;
    for (size_t i = 0; i < array_size; i += 1) {
        tidal_vel = tidal_resource.at(i,0);
        p_fluid = 0.5 * pow(tidal_vel, 3) * 1.025 * rotor_area;
        
        if (max_cp.size() == 1) {
            max_cp_value = max_cp[0];
        }
        else {
            max_cp_value = max_cp[i];
        }
        p_rotor = p_fluid * max_cp_value;
        if (pto_efficiency.size() == 1) {
            pto_eff_value = pto_efficiency[0];
        }
        else {
            pto_eff_value = pto_efficiency[i];
        }
        eff = pto_eff_value/100.0;
        if (tidal_vel < cut_in) eff = 0;
        if (tidal_vel > cut_out) eff = 0;
        p_electric = std::min(eff * p_rotor, generator_rated_capacity);
        powercurve_powerout[i] = p_electric;
        powercurve_tidespeeds[i] = tidal_vel;
        
    }

    var_data windspeeds = var_data(powercurve_tidespeeds.data(), powercurve_tidespeeds.ncols());
    var_data powerout = var_data(powercurve_powerout.data(), powercurve_powerout.ncols());

    vt->assign("tidal_turbine_powercurve_tidespeeds", windspeeds);
    vt->assign("tidal_turbine_powercurve_powerout", powerout);
    sprintf(errmsg, "None");
    vt->assign("error", std::string(errmsg));
    return true;
}



