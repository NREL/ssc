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

bool me_array_cable_voltage(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        return false;
    }

    double devices_per_row, device_spacing_in_row, number_rows, row_spacing, cable_system_overbuild, floating_array, export_cable_redundancy, water_depth, number_devices, distance_to_shore;
    double device_rated_power, system_capacity, inter_array_cable_length, riser_cable_length, export_cable_length;
    double use_onshore_substation, load_grid_voltage;
    vt_get_number(vt, "devices_per_row", &devices_per_row);
    vt_get_number(vt, "device_rated_power", &device_rated_power);
    vt_get_number(vt, "system_capacity", &system_capacity);
    vt_get_number(vt, "device_spacing_in_row", &device_spacing_in_row);
    vt_get_number(vt, "inter_array_cable_length", &inter_array_cable_length);
    vt_get_number(vt, "riser_cable_length", &riser_cable_length);
    vt_get_number(vt, "export_cable_length", &export_cable_length);
    vt_get_number(vt, "use_onshore_substation", &use_onshore_substation);
    vt_get_number(vt, "load_grid_voltage", &load_grid_voltage);


    double PF = 0.95; //Power Factor
    double angle = acos(PF);
    
    double riser_cable_rated_power_per_device = device_rated_power / (sqrt(3.0) * PF * 1000.0);
    double array_cable_rated_power_per_row = (device_rated_power * devices_per_row) / (sqrt(3.0) * PF * 1000.0);
    double export_cable_rated_power_array_ac = (system_capacity) / (sqrt(3.0) * PF * 1000.0);
    double export_cable_rated_power_array_hvdc = (system_capacity) / 1000.0;
    double reactive_power = sqrt(3.0) * array_cable_rated_power_per_row * sin(angle); //Where is this used
    //Cable Electrical Specifications
    double export_cable_type = 0; //0 - AC, 1 - HVDC Appendix A Electrical Instracture Model spreadsheet
    if (system_capacity >= 200000 && distance_to_shore >= 150000 ||
        (system_capacity >= 300000 && distance_to_shore >= 100000) ||
        (system_capacity >= 500000 && distance_to_shore >= 90000) ||
        (system_capacity >=600000 && distance_to_shore >= 80000) ||
        (system_capacity >= 1100000 && distance_to_shore >= 70000)) {
        export_cable_type = 1;
    }
    vt->assign("export_cable_type", export_cable_type);
    //Riser Cable
    double riser_cable_voltage = 0;
    double riser_cable_cost = 0; //$/m
    if ( array_cable_rated_power_per_row < 4) {
        riser_cable_voltage = 7.2;
        riser_cable_cost = 57.955 *  array_cable_rated_power_per_row;
    }
    else if (riser_cable_rated_power_per_device >= 5 && riser_cable_rated_power_per_device < 9) {
        riser_cable_voltage = 12;
        riser_cable_cost = 47.214 * riser_cable_rated_power_per_device - 91.05;
    }
    else if (riser_cable_rated_power_per_device >= 9 && riser_cable_rated_power_per_device < 14) {
        riser_cable_voltage = 24;
        riser_cable_cost = 22.748 * riser_cable_rated_power_per_device - 68.376;
    }
    else if (riser_cable_rated_power_per_device >= 14) {
        riser_cable_voltage = 36;
        riser_cable_cost = 20.82 * riser_cable_rated_power_per_device - 163.14;
    }
    vt->assign("riser_cable_voltage", riser_cable_voltage);
    vt->assign("riser_cable_cost", riser_cable_cost);
    double riser_cable_cost_total = riser_cable_cost * riser_cable_length;
    vt->assign("riser_cable_cost_total", riser_cable_cost_total);

    //Array Cable
    double array_cable_voltage = 0;
    double array_cable_cost = 0;
    if ( array_cable_rated_power_per_row < 4) {
        array_cable_voltage = 7.2;
        array_cable_cost = 44.245 *  array_cable_rated_power_per_row;
    }
    else if ( array_cable_rated_power_per_row >= 4 &&  array_cable_rated_power_per_row < 9) {
        array_cable_voltage = 12;
        array_cable_cost = 31.029 *  array_cable_rated_power_per_row - 40.744;
    }
    else if ( array_cable_rated_power_per_row >= 9 &&  array_cable_rated_power_per_row < 14) {
        array_cable_voltage = 24;
        array_cable_cost = 17.348 *  array_cable_rated_power_per_row - 61.467;
    }
    else if ( array_cable_rated_power_per_row >= 14 && array_cable_rated_power_per_row < 30) {
        array_cable_voltage = 36;
        array_cable_cost = 13.791 *  array_cable_rated_power_per_row - 93.272;
    }
    else if (array_cable_rated_power_per_row >= 30) {
        array_cable_voltage = 66;
        array_cable_cost = 11.984 * array_cable_rated_power_per_row - 155.97;
    }
    vt->assign("array_cable_voltage", array_cable_voltage);
    vt->assign("array_cable_cost", array_cable_cost);
    double array_cable_cost_total = array_cable_cost * inter_array_cable_length;
    vt->assign("array_cable_cost_total", array_cable_cost_total);

    //Export Cable
    double export_cable_voltage = 0;
    double export_cable_cost = 0;
    double offshore_substation_voltage = 0;
    if (export_cable_type == 0) {
        if (export_cable_rated_power_array_ac < 4) {
            export_cable_voltage = 7.2;
            export_cable_cost = 44.245 * export_cable_rated_power_array_ac;
            offshore_substation_voltage = 8; //kVAC
        }
        else if (export_cable_rated_power_array_ac >= 4 && export_cable_rated_power_array_ac < 9) {
            export_cable_voltage = 12;
            export_cable_cost = 31.029 * export_cable_rated_power_array_ac - 40.744;
            offshore_substation_voltage = 15; //kVAC
        }
        else if (export_cable_rated_power_array_ac >= 9 && export_cable_rated_power_array_ac < 14) {
            export_cable_voltage = 24;
            export_cable_cost = 17.348 * export_cable_rated_power_array_ac - 61.467;
            offshore_substation_voltage = 25; //kVAC
        }
        else if (export_cable_rated_power_array_ac >= 14 && export_cable_rated_power_array_ac < 30) {
            export_cable_voltage = 36;
            export_cable_cost = 13.791 * export_cable_rated_power_array_ac - 93.272;
            offshore_substation_voltage = 46; //kVAC
        }
        else if (export_cable_rated_power_array_ac >= 30 && export_cable_rated_power_array_ac < 40) {
            export_cable_voltage = 66;
            array_cable_cost = 11.984 * export_cable_rated_power_array_ac - 155.97;
            offshore_substation_voltage = 69; //kVAC
        }
        else if (export_cable_rated_power_array_ac >= 40 && export_cable_rated_power_array_ac < 121) {
            export_cable_voltage = 72.5;
            array_cable_cost = 9.8977 * export_cable_rated_power_array_ac - 195.75;
            offshore_substation_voltage = 115; //kVAC
        }
        else if (export_cable_rated_power_array_ac >= 121 && export_cable_rated_power_array_ac < 250) {
            export_cable_voltage = 145;
            array_cable_cost = 10.046 * export_cable_rated_power_array_ac - 886.49;
            offshore_substation_voltage = 161; //kVAC
        }
        else if (export_cable_rated_power_array_ac >= 250 && export_cable_rated_power_array_ac < 550) {
            export_cable_voltage = 220;
            array_cable_cost = 5.2937 * export_cable_rated_power_array_ac - 318.15;
            offshore_substation_voltage = 230; //kVAC
        }
        else if (export_cable_rated_power_array_ac >= 550) {
            export_cable_voltage = 400;
            array_cable_cost = 7.7566 * export_cable_rated_power_array_ac - 2704.6;
            offshore_substation_voltage = 415; //kVAC
        }
    }
    else {
        if (export_cable_rated_power_array_hvdc < 500) {
            export_cable_voltage = 150;
            export_cable_cost = 2.5026 * export_cable_rated_power_array_hvdc;
            offshore_substation_voltage = 161; //kV HVDC
        }
        else {
            export_cable_voltage = 300;
            export_cable_cost = 2.0375 * export_cable_rated_power_array_hvdc - 516.02;
            offshore_substation_voltage = 345; //kVAC
        }
    }
    vt->assign("export_cable_voltage", export_cable_voltage);
    vt->assign("export_cable_cost", export_cable_cost);
    vt->assign("export_cable_type", export_cable_type);
    double export_cable_cost_total = export_cable_cost * export_cable_length;
    vt->assign("export_cable_cost_total", export_cable_cost_total);

    //Offshore substation costs
    double offshore_foundation_cost = 303.09 * system_capacity;
    //AC Electrical equipment
    double circuit_breaker_cost = 818.42 * offshore_substation_voltage;
    double ac_switchgear_cost = 14018 * offshore_substation_voltage;
    double transformer_cost = 11879 * export_cable_rated_power_array_ac;
    double shunt_reactor_cost = 35226 * reactive_power;
    double series_capacitor_cost = 22047 * reactive_power;
    double static_var_compensator_cost = 105060 * reactive_power;
    //HVDC Electrical equipment
    double hvdc_converter_station_cost = 142.61 * system_capacity;
    double offshore_substation_cost_total = 0;
    if (array_cable_voltage == export_cable_voltage && export_cable_type == 0) {
        offshore_substation_cost_total = offshore_foundation_cost + circuit_breaker_cost + ac_switchgear_cost + transformer_cost +
            shunt_reactor_cost + series_capacitor_cost + static_var_compensator_cost;
    }
    else if (array_cable_voltage == export_cable_voltage && export_cable_type == 1) {
        offshore_substation_cost_total = offshore_foundation_cost + hvdc_converter_station_cost;
    }
    vt->assign("offshore_substation_cost_total", offshore_substation_cost_total);

    //Onshore substation
    double onshore_substation_voltage = load_grid_voltage;
    double onshore_foundation_cost = 3590.4 * onshore_substation_voltage + 1000000;
    double onshore_circuit_breaker_cost = 818.42 * onshore_substation_voltage;
    double onshore_ac_switchgear_cost = 14018 * onshore_substation_voltage;
    double onshore_transformer_cost = 11346 * export_cable_rated_power_array_ac;
    double onshore_shunt_reactor_cost = 35226 * reactive_power;
    double onshore_series_capacitor_cost = 22047 * reactive_power;
    double onshore_static_var_compensator_cost = 105060 * reactive_power;
    //HVDC Electrical equipment
    double onshore_hvdc_converter_station_cost = 142.61 * system_capacity;
    double onshore_substation_cost_total;
    if (use_onshore_substation==1 && export_cable_type == 0) {
        onshore_substation_cost_total = onshore_foundation_cost + onshore_circuit_breaker_cost + onshore_ac_switchgear_cost + onshore_transformer_cost +
            onshore_shunt_reactor_cost + onshore_series_capacitor_cost + onshore_static_var_compensator_cost;
    }
    else if (use_onshore_substation==1 && export_cable_type == 1) {
        onshore_substation_cost_total = onshore_foundation_cost + onshore_hvdc_converter_station_cost;
    }
    vt->assign("onshore_substation_cost_total", onshore_substation_cost_total);
}



