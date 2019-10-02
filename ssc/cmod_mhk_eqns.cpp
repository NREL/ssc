#include "vartab.h"
#include "../shared/lib_util.h"

#include "cmod_mhk_eqns.h"

void mhk_array_interarray_cable_length(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt){
        throw std::runtime_error("ssc_data_t data invalid");
    }

    double devices_per_row, device_spacing_in_row, number_rows, row_spacing, cable_system_overbuild;

    VT_GET_INPUT(vt, "devices_per_row", devices_per_row)
    VT_GET_INPUT(vt, "device_spacing_in_row", device_spacing_in_row)
    VT_GET_INPUT(vt, "number_rows", number_rows)
    VT_GET_INPUT(vt, "row_spacing", row_spacing)                    
    VT_GET_INPUT(vt, "cable_system_overbuild", cable_system_overbuild)

	double length = (devices_per_row - 1) * device_spacing_in_row * number_rows + row_spacing * (number_rows - 1);
	length *= (1 + cable_system_overbuild / 100.0);
	   	 
	var_data cablelength = var_data(length);
    vt->assign( "inter_array_cable_length", cablelength);
}



