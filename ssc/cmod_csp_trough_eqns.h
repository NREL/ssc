#ifndef _CMOD_CSP_TROUGH_EQNS_H_
#define _CMOD_CSP_TROUGH_EQNS_H_

#include "sscapi.h"

#ifdef __cplusplus
extern "C" {
#endif

    static const char* Physical_Trough_System_Design_Equations_doc =
        "Sizes the design point system parameters of a power trough plant, as used on the System Design UI form\\n"
        "Input: var_table with key-value pairs\\n"
        "     'design_eff' - double [-]\\n"
        "Output: key-value pairs added to var_table\\n"
        "     'nameplate' - double [MWe]\\n";

    SSCEXPORT void Physical_Trough_System_Design_Equations(ssc_data_t data);


    static const char* Physical_Trough_Solar_Field_Equations_doc =
        "Sizes and lays out the trough field for a physical trough power plant, as used on the Solar Field UI form\\n"
        "Input: var_table with key-value pairs\\n"
        "     'design_eff' - double [-]\\n"
        "Output: key-value pairs added to var_table\\n"
        "     'nameplate' - double [MWe]\\n";

    SSCEXPORT void Physical_Trough_Solar_Field_Equations(ssc_data_t data);

#ifdef __cplusplus
}
#endif

#endif
