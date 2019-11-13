#ifndef __ssc_eqn_h
#define __ssc_eqn_h

#include "sscapi.h"
#include "cmod_windpower_eqns.h"
#include "cmod_mhk_eqns.h"
#include "cmod_merchantplant_eqns.h"

typedef void (*ssc_equation_ptr)(ssc_data_t data);

struct ssc_equation_entry{
    const char* name;
    ssc_equation_ptr func;
    const char* cmod;
    const char* doc;
};

static ssc_equation_entry ssc_equation_table [] = {
		{"Turbine_calculate_powercurve", Turbine_calculate_powercurve,
						   "Windpower", Turbine_calculate_powercurve_doc},
		{"me_array_cable_length", me_array_cable_length,
						   "Marine energy", me_array_cable_length_doc},
		{"mp_capacity_check", mp_capacity_check,
						   "Merchant plant", mp_capacity_check_doc},
{nullptr, nullptr, nullptr, nullptr}
};

#endif