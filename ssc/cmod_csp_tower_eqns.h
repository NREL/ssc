#ifndef _CMOD_CSP_TOWER_EQNS_H_
#define _CMOD_CSP_TOWER_EQNS_H_

#include "sscapi.h"

#ifdef __cplusplus
extern "C" {
#endif

    static const char* MSPT_System_Design_Equations_doc =
        "Sizes the design point system parameters of a molten salt power tower plant, as used on the System Design UI form\\n"
        "Input: var_table with key-value pairs\\n"
        "     'P_ref' - double [MWe]\\n"
        "     'gross_net_conversion_factor' - double [-]\\n"
        "     'design_eff' - double [-]\\n"
        "     'solarm' - double [-]\\n"
        "     'tshours' - double [hr]\\n"
        "Output: key-value pairs added to var_table\\n"
        "     'nameplate' - double [MWe]\\n"
        "     'q_pb_design' - double [MWt]\\n"
        "     'Q_rec_des' - double [MWt]\\n"
        "     'tshours_sf' - double [hr]";

    SSCEXPORT void MSPT_System_Design_Equations(ssc_data_t data);


#ifdef __cplusplus
}
#endif

#endif
