#ifndef _CMOD_SOLAR_THERMAL_EQNS_H_
#define _CMOD_SOLAR_THERMAL_EQNS_H_

#include "sscapi.h"

#ifdef __cplusplus
extern "C" {
#endif

    static const char* Flat_Plate_Array_Design_Equations_doc =
        "Sizes the design point system parameters of a flat plate solar array, as used in the UI forms\\n"
        "Input: var_table with key-value pairs\\n"
        "     'solarm' - double [-]\\n"
        "Output: key-value pairs added to var_table\\n"
        "     'tshours_sf' - double [hr]";

    SSCEXPORT void Flat_Plate_Array_Design_Equations(ssc_data_t data);

#ifdef __cplusplus
}
#endif

#endif
