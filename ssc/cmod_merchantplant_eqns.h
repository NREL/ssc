#ifndef _CMOD_MERCHANTPLANT_BUILDER_H_
#define _CMOD_MERCHANTPLANT_BUILDER_H_

#include "vartab.h"
#include "sscapi.h"

#ifdef __cplusplus
extern "C" {
#endif

static const char* mp_capacity_check_doc =
    "Calculates the cable length in an ME array\\n"
    "Input: var_table with key-value pairs\\n"
    "     'devices_per_row' - double [-]\\n"
    "     'device_spacing_in_row' - double [m]\\n"
	"     'number_rows' - double [-]\\n"
	"     'row_spacing' - double [m]\\n"
	"     'cable_system_overbuild' - double [%]\\n"
    "Output: key-value pairs added to var_table\\n"
    "     'inter_array_cable_length' - double [m]\\n";

SSCEXPORT void mp_capacity_check(ssc_data_t data);


#ifdef __cplusplus
}
#endif

#endif
