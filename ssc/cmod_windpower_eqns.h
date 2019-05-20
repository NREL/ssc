#ifndef _CMOD_WINDPOWER_BUILDER_H_
#define _CMOD_WINDPOWER_BUILDER_H_

#include "vartab.h"
#include "sscapi.h"

#ifdef __cplusplus
extern "C" {
#endif


SSCEXPORT void Windpower_turbine_powercurve(ssc_data_t data);
//
// Evaluates wind_turbine_powercurve_windspeeds, wind_turbine_powercurve_powerout, wind_turbine_rated_wind_speed, wind_turbine_powercurve_err_msg, wind_turbine_powercurve_hub_efficiency for a Wind Turbine Design module
// @param *vt: a var_table* that contains: wind.turbine.radio_list_or_design, wind_turbine_powercurve_windspeeds_from_lib, wind_turbine_powercurve_powerout_from_lib, wind_turbine_kw_rating_from_lib, wind_turbine_kw_rating_input, wind_turbine_rotor_diameter_input, wind_turbine_hub_ht, wind.turbine.elevation, wind_resource_model_choice, wind_turbine_max_cp, wind.turbine.max_tip_speed, wind.turbine.max_tspeed_ratio, wind.turbine.region2nhalf_slope, wind_turbine_cutin, wind_turbine_cut_out, wind.turbine.drive_train
// @returns single value or var_table
//



#ifdef __cplusplus
}
#endif

#endif
