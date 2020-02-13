#ifndef __ssc_eqn_h
#define __ssc_eqn_h

#include "sscapi.h"
#include "cmod_windpower_eqns.h"
#include "cmod_merchantplant_eqns.h"
#include "cmod_pvsamv1_eqns.h"


typedef void (*ssc_equation_ptr)(ssc_data_t data);

struct ssc_equation_entry{
    const char* name;
    ssc_equation_ptr func;
    const char* cmod;
    const char* doc;
};

/**
 * Name format: <ssc group>_<eqn name>
 *      The ssc group prefix allows attaching
 */
static ssc_equation_entry ssc_equation_table [] = {
		{"mp_ancillary_services", mp_ancillary_services,
						   "Merchant plant", mp_ancillary_services_doc},
        {"Reopt_size_battery_post", Reopt_size_battery_params,
            "Pvsamv1", Reopt_size_battery_params_doc},
        {"Reopt_size_battery_post", Reopt_size_battery_params,
            "Pvwattsv7", Reopt_size_battery_params_doc},
        {"Turbine_calculate_powercurve", Turbine_calculate_powercurve,
            "Windpower", Turbine_calculate_powercurve_doc},
        {nullptr, nullptr, nullptr, nullptr}
};


#endif