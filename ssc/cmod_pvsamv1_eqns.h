#ifndef SYSTEM_ADVISOR_MODEL_CMOD_PVSAMV1_EQNS_H
#define SYSTEM_ADVISOR_MODEL_CMOD_PVSAMV1_EQNS_H

#include "vartab.h"
#include "sscapi.h"

#ifdef __cplusplus
extern "C" {
#endif

static const char *Reopt_size_battery_params_doc =
        "Calls the ReOpt Lite API with technoeconomic parameters to calculate the optimal battery size for\\n"
        "Residential, Commercial, Third Party and Host Developer financial models.\\n"
        "Input: var_table with key-value pairs:  \\n"
        "     required inputs for 'Pvsamv1', detailed photovoltaic model\\n"
        "     required inputs for 'Utilityrate5', utility rate model\\n"
        "     'total_installed_cost' - double [$]\\n"
        "     'depr_bonus_fed' - double [%], Percent of upfront project costs to depreciate in year one in addition to scheduled depreciation\\n"
        "     'depr_bonus_fed_macrs_5' - boolean, Federal bonus depreciation follows 5-yr MACRS\\n"
        "Output: key-value pairs added to var_table\\n"
        "     'reopt_params' - string";

SSCEXPORT void Reopt_size_battery_params(ssc_data_t data);


}

#endif //SYSTEM_ADVISOR_MODEL_CMOD_PVSAMV1_EQNS_H