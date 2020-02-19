#ifndef __ssc_eqn_h
#define __ssc_eqn_h

#include "sscapi.h"
#include "cmod_windpower_eqns.h"
#include "cmod_mhk_eqns.h"
#include "cmod_merchantplant_eqns.h"
#include "cmod_pvsamv1_eqns.h"
#include "cmod_csp_tower_eqns.h"


typedef void (*ssc_equation_ptr)(ssc_data_t data);

struct ssc_equation_entry{
    const char* name;
    ssc_equation_ptr func;
    const char* cmod;
    const char* doc;
};

static ssc_equation_entry ssc_equation_table [] = {
		{"me_array_cable_length", me_array_cable_length,
						   "Marine energy", me_array_cable_length_doc},
		{"mp_ancillary_services", mp_ancillary_services,
						   "Merchant plant", mp_ancillary_services_doc},
        {"Reopt_size_battery_post", Reopt_size_battery_params,
            "Pvsamv1", Reopt_size_battery_params_doc},
        {"Reopt_size_battery_post", Reopt_size_battery_params,
            "Pvwattsv5", Reopt_size_battery_params_doc},
        {"Turbine_calculate_powercurve", Turbine_calculate_powercurve,
            "Windpower", Turbine_calculate_powercurve_doc},
        {"MSPT_System_Design_Equations", MSPT_System_Design_Equations,
            "Tcsmolten_salt", MSPT_System_Design_Equations_doc},
        {"Tower_SolarPilot_Solar_Field_Equations", Tower_SolarPilot_Solar_Field_Equations,
            "Tcsmolten_salt", Tower_SolarPilot_Solar_Field_Equations_doc},
        {"MSPT_Receiver_Equations", MSPT_Receiver_Equations,
            "Tcsmolten_salt", MSPT_Receiver_Equations_doc},
        {"MSPT_System_Control_Equations", MSPT_System_Control_Equations,
            "Tcsmolten_salt", MSPT_System_Control_Equations_doc},
        {nullptr, nullptr, nullptr, nullptr}
};


#endif