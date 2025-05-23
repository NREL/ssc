/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#include <algorithm>
#include <cmath>
#include "cmod_pvsamv1.h"
#include "lib_pv_io_manager.h"
#include "lib_resilience.h"
#include "lib_time.h"

// comment following define if do not want shading database validation outputs
//#define SHADE_DB_OUTPUTS

static var_info _cm_vtab_pvsamv1[] = {
      /*VARTYPE     DATATYPE      NAME                                    LABEL                                                  UNITS     META                             GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
        {SSC_INPUT, SSC_STRING,   "solar_resource_file",                  "Weather file in TMY2, TMY3, EPW, or SAM CSV",         "",       "",                                                                                                                                                                                      "Solar Resource",                                        "?",                                  "",                    "" },
        {SSC_INPUT, SSC_TABLE,    "solar_resource_data",                  "Weather data",                                        "",       "lat,lon,tz,elev,year,month,hour,minute,gh,dn,df,poa,tdry,twet,tdew,rhum,pres,snow,alb,aod,wspd,wdir",                                                                                   "Solar Resource",                                        "?",                                  "",                    "" },

        // transformer model percent of rated ac output
        {SSC_INPUT, SSC_NUMBER,   "transformer_no_load_loss",             "Power transformer no load loss",                      "%",      "percent of inverter AC capacity",                                                                                                                                                                                      "Losses",                                                "?=0",                                "",                    "" },
        {SSC_INPUT, SSC_NUMBER,   "transformer_load_loss",                "Power transformer load loss",                         "%",      "percent of AC output",                                                                                                                                                                                      "Losses",                                                "?=0",                                "",                    "" },

            // optional for lifetime analysis
        {SSC_INPUT, SSC_NUMBER,   "system_use_lifetime_output",           "PV lifetime simulation",                              "0/1",    "",                                                                                                                                                                                      "Lifetime",                                              "?=0",                                "INTEGER,MIN=0,MAX=1", "" },
        {SSC_INPUT, SSC_NUMBER,   "analysis_period",                      "Lifetime analysis period",                            "years",  "",                                                                                                                                                                                      "Lifetime",                                              "system_use_lifetime_output=1",       "",                    "" },
        {SSC_INPUT, SSC_ARRAY,    "dc_degradation",                       "Annual DC degradation",                           "%/year", "",                                                                                                                                                                                      "Lifetime",                                              "system_use_lifetime_output=1",       "",                    "" },
        {SSC_OUTPUT,SSC_ARRAY,    "dc_degrade_factor",                    "Annual DC degradation factor",                        "",       "",                                                                                                                                                                                      "Lifetime",                                              "system_use_lifetime_output=1",       "",                    "" },
        {SSC_INPUT, SSC_NUMBER,   "en_dc_lifetime_losses",                "Enable lifetime daily DC losses",                     "0/1",    "",                                                                                                                                                                                      "Lifetime",                                              "?=0",                                "INTEGER,MIN=0,MAX=1", "" },
        {SSC_INPUT, SSC_ARRAY,    "dc_lifetime_losses",                   "Lifetime daily DC losses",                            "%",      "",                                                                                                                                                                                      "Lifetime",                                              "en_dc_lifetime_losses=1",            "",                    "" },
        {SSC_INPUT, SSC_NUMBER,   "en_ac_lifetime_losses",                "Enable lifetime daily AC losses",                     "0/1",    "",                                                                                                                                                                                      "Lifetime",                                              "?=0",                                "INTEGER,MIN=0,MAX=1", "" },
        {SSC_INPUT, SSC_ARRAY,    "ac_lifetime_losses",                   "Lifetime daily AC losses",                            "%",      "",                                                                                                                                                                                      "Lifetime",                                              "en_ac_lifetime_losses=1",            "",                    "" },
        {SSC_INPUT, SSC_NUMBER,   "save_full_lifetime_variables",         "Save and display vars for full lifetime",             "0/1",    "",                                                                                                                                                                                      "Lifetime",                                              "?=1",       "INTEGER,MIN=0,MAX=1", "" },

        // misc inputs
        {SSC_INPUT, SSC_NUMBER,   "en_snow_model",                        "Toggle snow loss estimation",                         "0/1",    "",                                                                                                                                                                                      "Losses",                                                "?=0",                                "BOOLEAN",             "" },
        { SSC_INPUT,        SSC_NUMBER,     "snow_slide_coefficient",		"Snow Slide Coefficient",			"",					"",					"Losses", "?=1.97",           "",                             "" },

        {SSC_INPUT, SSC_NUMBER,   "system_capacity",                      "DC Nameplate capacity",                               "kWdc",   "",                                                                                                                                                                                      "System Design",                                         "*",                                  "",                    "" },
        {SSC_INPUT, SSC_NUMBER,   "use_wf_albedo",                        "Use albedo in weather file if provided",              "0/1",    "0=user-specified,1=weatherfile",                                                                                                                                                        "Solar Resource",                                        "?=1",                                "BOOLEAN",             "" },
        {SSC_INPUT, SSC_NUMBER,   "use_spatial_albedos",                  "Use spatial albedo values",                           "0/1",    "0=no,1=yes",                                                                                                                                                                            "Solar Resource",                                        "?=0",                                "BOOLEAN",             "" },
        {SSC_INPUT, SSC_ARRAY,    "albedo",                               "User specified monthly ground albedo (non-spatial)",  "0..1",   "",                                                                                                                                                                                      "Solar Resource",                                        "use_spatial_albedos=0",              "LENGTH=12",           "" },
        {SSC_INPUT, SSC_MATRIX,   "albedo_spatial",                       "User specified monthly ground albedo (spatial)",      "0..1",   "",                                                                                                                                                                                      "Solar Resource",                                        "use_spatial_albedos=1",              "",                    "" },
        {SSC_INPUT, SSC_NUMBER,   "irrad_mode",                           "Irradiance input translation mode",                   "",       "0=beam&diffuse,1=total&beam,2=total&diffuse,3=poa_reference,4=poa_pyranometer",                                                                                                         "Solar Resource",                                        "?=0",                                "INTEGER,MIN=0,MAX=4", "" },
        {SSC_INPUT, SSC_NUMBER,   "sky_model",                            "Diffuse sky model",                                   "",       "0=isotropic,1=hkdr,2=perez",                                                                                                                                                            "Solar Resource",                                        "?=2",                                "INTEGER,MIN=0,MAX=2", "" },
        {SSC_INPUT, SSC_NUMBER,   "inverter_count",                       "Number of inverters",                                 "",       "",                                                                                                                                                                                      "System Design",                                         "*",                                  "INTEGER,POSITIVE",    "" },
        {SSC_INPUT, SSC_NUMBER,   "enable_mismatch_vmax_calc",            "Enable mismatched subarray Vmax calculation",         "",       "",                                                                                                                                                                                      "System Design",                                         "?=0",                                "BOOLEAN",             "" },
        {SSC_INPUT, SSC_NUMBER,   "calculate_rack_shading",               "Calculate rack shading",                              "",       "",                                                                                                                                                                                      "Losses",                                                "?=0",                                "BOOLEAN",             "" },
        {SSC_INPUT, SSC_NUMBER,   "calculate_bifacial_electrical_mismatch", "Calculate bifacial electrical mismatch",            "",       "",                                                                                                                                                                                      "Losses",                                                "?=1",                                "BOOLEAN",             "" },

        // subarray 1
        {SSC_INPUT, SSC_NUMBER,   "subarray1_nstrings",                   "Sub-array 1 Number of parallel strings",              "",       "",                                                                                                                                                                                      "System Design",                                         "",                                   "INTEGER,POSITIVE",             "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_modules_per_string",         "Sub-array 1 Modules per string",                      "",       "",                                                                                                                                                                                      "System Design",                                         "*",                                  "INTEGER,POSITIVE",    "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_mppt_input",                 "Sub-array 1 Inverter MPPT input number",              "",       "",                                                                                                                                                                                      "System Design",                                         "?=1",                                "INTEGER,POSITIVE",    "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_tilt",                       "Sub-array 1 Tilt",                                    "degrees",    "0=horizontal,90=vertical",                                                                                                                                                              "System Design",                                         "subarray1_track_mode~2&subarray1_track_mode~4",                                   "MIN=0,MAX=90",        "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_tilt_eq_lat",                "Sub-array 1 Tilt=latitude override",                  "0/1",    "0=false,1=override",                                                                                                                                                                    "System Design",                                         "?=0",                                   "BOOLEAN",             "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_azimuth",                    "Sub-array 1 Azimuth",                                 "degrees",    "0=N,90=E,180=S,270=W",                                                                                                                                                                  "System Design",                                         "subarray1_track_mode~2&subarray1_track_mode~3",            "MIN=0,MAX=359.9",     "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_track_mode",                 "Sub-array 1 Tracking mode",                           "",       "0=fixed,1=1axis,2=2axis,3=azi,4=monthly",                                                                                                                                               "System Design",                                         "*",                                  "INTEGER,MIN=0,MAX=4", "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_rotlim",                     "Sub-array 1 Tracker rotation limit",                  "degrees",    "",                                                                                                                                                                                      "System Design",                                         "?=45",                               "MIN=0,MAX=85",        "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_shade_mode",                 "Sub-array 1 shading mode (fixed tilt or 1x tracking)","0/1/2",  "0=none,1=standard(non-linear),2=thin film(linear)",                                                                                                                                     "Shading",                                               "*",                                  "INTEGER,MIN=0,MAX=2", "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_gcr",                        "Sub-array 1 Ground coverage ratio",                   "0..1",   "",                                                                                                                                                                                      "System Design",                                         "?=0.3",                              "MIN=0.01,MAX=0.99",   "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_slope_tilt",                 "Sub-array 1 terrain tilt",                        "degrees",   "",                                                                                                                                                                                      "System Design",                                         "subarray1_track_mode=1",                                        "MIN=0,MAX=90",   "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_slope_azm",                  "Sub-array 1 terrain azimuth",                        "degrees",   "",                                                                                                                                                                                      "System Design",                                         "subarray1_track_mode=1",                                     "MIN=0,MAX=359.9",   "" },
        {SSC_INPUT, SSC_ARRAY,    "subarray1_monthly_tilt",               "Sub-array 1 monthly tilt input",                      "degrees",    "",                                                                                                                                                                                      "System Design",                                         "subarray1_track_mode=4",             "LENGTH=12",           "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_use_custom_rot_angles",                    "Sub-array 1 Use custom tracker rotation angles",         "0/1",       "",                                                                                                                                                                                      "System Design",                                         "?=0",                                "INTEGER,MIN=0,MAX=1",             "" },
        {SSC_INPUT, SSC_ARRAY,    "subarray1_custom_rot_angles_array",                   "Sub-array 1 Custom tracker rotation angles",                            "deg",      "",                                                                                                                                                                                      "System Design",                                              "subarray1_use_custom_rot_angles=1",            "",                    "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_use_custom_cell_temp",                    "Subarray 1 Use custom cell temperatures",         "0/1",       "",                                                                                                                                                                                      "System Design",                                         "?=0",                                "INTEGER,MIN=0,MAX=1",             "" },
        {SSC_INPUT, SSC_ARRAY,    "subarray1_custom_cell_temp_array",                   "Subarray 1 custom cell temperature",                            "C",      "",                                                                                                                                                                                      "System Design",                                              "subarray1_use_custom_cell_temp=1",            "",                    "" },

        {SSC_INPUT, SSC_NUMBER,   "subarray1_shading_en_string_option",   "Enable Sub-array 1 shading string option",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_shading_string_option",      "Sub-array 1 shading string option",                   "",       "0=shadingdb,1=average,2=maximum,3=minimum",                                                                                                                            "Shading",                                               "?=-1",                               "INTEGER,MIN=-1,MAX=4","" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_shading_en_timestep",         "Enable Sub-array 1 timestep beam shading losses",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        {SSC_INPUT, SSC_MATRIX,   "subarray1_shading_timestep",           "Sub-array 1 timestep beam shading losses",            "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_shading_en_mxh",               "Enable Sub-array 1 Month x Hour beam shading losses",  "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        {SSC_INPUT, SSC_MATRIX,   "subarray1_shading_mxh",                "Sub-array 1 Month x Hour beam shading losses",        "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_shading_en_azal",               "Enable Sub-array 1 Azimuth x altitude beam shading losses",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        {SSC_INPUT, SSC_MATRIX,   "subarray1_shading_azal",               "Sub-array 1 Azimuth x altitude beam shading losses",  "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_shading_en_diff",               "Enable Sub-array 1 Diffuse shading loss",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_shading_diff",               "Sub-array 1 Diffuse shading loss",                    "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },


        {SSC_INPUT, SSC_ARRAY,    "subarray1_soiling",                    "Sub-array 1 Monthly soiling loss",                    "%",      "",                                                                                                                                                                                      "Losses",                                                "*",                                  "LENGTH=12",           "" },

        // loss diagram outputs, also used to calculate total dc derate
        {SSC_INPUT, SSC_NUMBER,   "subarray1_rear_soiling_loss",          "Sub-array 1 rear soiling loss",                       "%",      "",                                                                                                                                                                                      "Losses",                                                "*",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_rack_shading",               "Sub-array 1 rack shading loss",                       "%",      "",                                                                                                                                                                                      "Losses",                                                "*",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_mismatch_loss",              "Sub-array 1 DC mismatch loss",                        "%",      "",                                                                                                                                                                                      "Losses",                                                "*",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_diodeconn_loss",             "Sub-array 1 DC diodes and connections loss",          "%",      "",                                                                                                                                                                                      "Losses",                                                "*",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_dcwiring_loss",              "Sub-array 1 DC wiring loss",                          "%",      "",                                                                                                                                                                                      "Losses",                                                "*",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_tracking_loss",              "Sub-array 1 DC tracking error loss",                  "%",      "",                                                                                                                                                                                      "Losses",                                                "*",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_nameplate_loss",             "Sub-array 1 DC nameplate loss",                       "%",      "",                                                                                                                                                                                      "Losses",                                                "*",                                  "MIN=-5,MAX=100",      "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_electrical_mismatch",        "Sub-array 1 bifacial electrical mismatch loss",       "%",      "",                                                                                                                                                                                      "Losses",                                                "*",                                  "MIN=0,MAX=100",       "" },

        {SSC_INPUT, SSC_NUMBER,   "subarray2_rear_soiling_loss",          "Sub-array 2 rear soiling loss",                       "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray2_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_rack_shading",               "Sub-array 2 rack shading loss",                       "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray2_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_mismatch_loss",              "Sub-array 2 DC mismatch loss",                        "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray2_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_diodeconn_loss",             "Sub-array 2 DC diodes and connections loss",          "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray2_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_dcwiring_loss",              "Sub-array 2 DC wiring loss",                          "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray2_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_tracking_loss",              "Sub-array 2 DC tracking error loss",                  "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray2_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_nameplate_loss",             "Sub-array 2 DC nameplate loss",                       "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray2_enable=1",                                  "MIN=-5,MAX=100",      "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_electrical_mismatch",        "Sub-array 2 bifacial electrical mismatch loss",       "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray2_enable=1",                                  "MIN=0,MAX=100",       "" },

        {SSC_INPUT, SSC_NUMBER,   "subarray3_rear_soiling_loss",          "Sub-array 3 rear soiling loss",                       "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray3_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray3_rack_shading",               "Sub-array 3 rack shading loss",                       "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray3_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray3_mismatch_loss",              "Sub-array 3 DC mismatch loss",                        "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray3_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray3_diodeconn_loss",             "Sub-array 3 DC diodes and connections loss",          "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray3_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray3_dcwiring_loss",              "Sub-array 3 DC wiring loss",                          "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray3_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray3_tracking_loss",              "Sub-array 3 DC tracking error loss",                  "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray3_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray3_nameplate_loss",             "Sub-array 3 DC nameplate loss",                       "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray3_enable=1",                                  "MIN=-5,MAX=100",      "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray3_electrical_mismatch",        "Sub-array 3 bifacial electrical mismatch loss",       "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray3_enable=1",                                  "MIN=0,MAX=100",       "" },

        {SSC_INPUT, SSC_NUMBER,   "subarray4_rear_soiling_loss",          "Sub-array 4 rear soiling loss",                       "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray4_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray4_rack_shading",               "Sub-array 4 rack shading loss",                       "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray4_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray4_mismatch_loss",              "Sub-array 4 DC mismatch loss",                        "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray4_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray4_diodeconn_loss",             "Sub-array 4 DC diodes and connections loss",          "%",      "?",                                                                                                                                                                                     "Losses",                                                "subarray4_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray4_dcwiring_loss",              "Sub-array 4 DC wiring loss",                          "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray4_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray4_tracking_loss",              "Sub-array 4 DC tracking error loss",                  "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray4_enable=1",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray4_nameplate_loss",             "Sub-array 4 DC nameplate loss",                       "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray4_enable=1",                                  "MIN=-5,MAX=100",      "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray4_electrical_mismatch",        "Sub-array 4 bifacial electrical mismatch loss",       "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray4_enable=1",                                  "MIN=0,MAX=100",       "" },

        //losses that are applied uniformly to all subarrays
        {SSC_INPUT, SSC_NUMBER,   "dcoptimizer_loss",                     "DC power optimizer loss",                             "%",      "",                                                                                                                                                                                      "Losses",                                                "*",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "acwiring_loss",                        "AC wiring loss",                                      "%",      "percent of inverter AC output",                                                                                                                                                                                      "Losses",                                                "*",                                  "MIN=0,MAX=100",       "" },
        {SSC_INPUT, SSC_NUMBER,   "transmission_loss",                    "Transmission loss",                                   "%",      "percent of AC output after transformer losses",                                                                                                                                                                                      "Losses",                                                "*",                                  "MIN=0,MAX=100",       "" },

        //system design inputs
        {SSC_INPUT, SSC_NUMBER,   "subarray1_mod_orient",                 "Sub-array 1 Module orientation",                      "0/1",    "0=portrait,1=landscape",                                                                                                                                                                "Layout",                                                "*",                                  "INTEGER,MIN=0,MAX=1", "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_nmodx",                      "Sub-array 1 Number of modules along bottom of row",   "",       "",                                                                                                                                                                                      "Layout",                                                "*",                                  "INTEGER,POSITIVE",    "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_nmody",                      "Sub-array 1 Number of modules along side of row",     "",       "",                                                                                                                                                                                      "Layout",                                                "*",                                  "INTEGER,POSITIVE",    "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray1_backtrack",                  "Sub-array 1 Backtracking enabled",                    "",       "0=no backtracking,1=backtrack",                                                                                                                                                         "System Design",                                         "subarray1_track_mode=1",             "BOOLEAN",             "" },

        // subarray 2
        {SSC_INPUT, SSC_NUMBER,   "subarray2_enable",                     "Sub-array 2 Enable",                                  "0/1",    "0=disabled,1=enabled",                                                                                                                                                                  "System Design",                                         "?=0",                                "BOOLEAN",             "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_modules_per_string",         "Sub-array 2 Modules per string",                      "",       "",                                                                                                                                                                                      "System Design",                                         "subarray2_enable=1",                 "INTEGER,MIN=1",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_nstrings",                   "Sub-array 2 Number of parallel strings",              "",       "",                                                                                                                                                                                      "System Design",                                         "subarray2_enable=1",                 "INTEGER,MIN=1",       "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_mppt_input",                 "Sub-array 2 Inverter MPPT input number",              "",       "",                                                                                                                                                                                      "System Design",                                         "?=1",                                "INTEGER,POSITIVE",    "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_tilt",                       "Sub-array 2 Tilt",                                    "degrees",    "0=horizontal,90=vertical",                                                                                                                                                              "System Design",                                         "subarray2_enable=1&subarray2_track_mode~2&subarray2_track_mode~4",                 "MIN=0,MAX=90",        "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_tilt_eq_lat",                "Sub-array 2 Tilt=latitude override",                  "0/1",    "0=false,1=override",                                                                                                                                                                    "System Design",                                         "?=0",                                   "BOOLEAN",             "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_azimuth",                    "Sub-array 2 Azimuth",                                 "degrees",    "0=N,90=E,180=S,270=W",                                                                                                                                                                  "System Design",                                         "subarray2_enable=1&subarray2_track_mode~2&subarray2_track_mode~3",                 "MIN=0,MAX=359.9",     "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_track_mode",                 "Sub-array 2 Tracking mode",                           "",       "0=fixed,1=1axis,2=2axis,3=azi,4=monthly",                                                                                                                                               "System Design",                                         "subarray2_enable=1",                 "INTEGER,MIN=0,MAX=4", "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_rotlim",                     "Sub-array 2 Tracker rotation limit",                  "degrees",    "",                                                                                                                                                                                      "System Design",                                         "?=45",                               "MIN=0,MAX=85",        "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_shade_mode",                 "Sub-array 2 Shading mode (fixed tilt or 1x tracking)","0/1/2",  "0=none,1=standard(non-linear),2=thin film(linear)",                                                                                                                                     "Shading",                                               "subarray2_enable=1",                 "INTEGER,MIN=0,MAX=2", "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_gcr",                        "Sub-array 2 Ground coverage ratio",                   "0..1",   "",                                                                                                                                                                                      "System Design",                                         "?=0.3",                              "MIN=0.01,MAX=0.99",   "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_slope_tilt",                 "Sub-array 2 terrain tilt",                        "degrees",   "",                                                                                                                                                                                      "System Design",                                         "subarray2_enable=1&subarray2_track_mode=1",                                                "MIN=0,MAX=90",   "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_slope_azm",                  "Sub-array 2 terrain azimuth",                        "degrees",   "",                                                                                                                                                                                      "System Design",                                         "subarray2_enable=1&subarray2_track_mode=1",                                                "MIN=0,MAX=359.9",   "" },
        {SSC_INPUT, SSC_ARRAY,    "subarray2_monthly_tilt",               "Sub-array 2 Monthly tilt input",                      "degrees",    "",                                                                                                                                                                                      "System Design",                                         "subarray2_enable=1&subarray2_track_mode=4", "LENGTH=12",           "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray2_shading_en_string_option",   "Enable Sub-array 2 shading string option",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray2_shading_string_option",      "Sub-array 2 shading string option",                   "",       "0=shadingdb,1=average,2=maximum,3=minimum",                                                                                                                            "Shading",                                               "?=-1",                               "INTEGER,MIN=-1,MAX=4","" },
        { SSC_INPUT, SSC_NUMBER,   "subarray2_shading_en_timestep",         "Enable Sub-array 2 timestep beam shading losses",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "subarray2_shading_timestep",           "Sub-array 2 timestep beam shading losses",            "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray2_shading_en_mxh",               "Enable Sub-array 2 Month x Hour beam shading losses",  "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "subarray2_shading_mxh",                "Sub-array 2 Month x Hour beam shading losses",        "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray2_shading_en_azal",               "Enable Sub-array 2 Azimuth x altitude beam shading losses",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "subarray2_shading_azal",               "Sub-array 2 Azimuth x altitude beam shading losses",  "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray2_shading_en_diff",               "Enable Sub-array 2 Diffuse shading loss",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray2_shading_diff",               "Sub-array 2 Diffuse shading loss",                    "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray2_use_custom_rot_angles",                    "Sub-array 2 Use custom tracker rotation angles",         "0/1",       "",                                                                                                                                                                                      "System Design",                                         "?=0",                                "INTEGER,MIN=0,MAX=1",             "" },
        { SSC_INPUT, SSC_ARRAY,    "subarray2_custom_rot_angles_array",                   "Sub-array 2 Custom tracker rotation angles",                            "deg",      "",                                                                                                                                                                                      "System Design",                                              "subarray2_use_custom_rot_angles=1",            "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray2_use_custom_cell_temp",                    "Subarray 2 Use custom cell temperatures",         "0/1",       "",                                                                                                                                                                                      "System Design",                                         "?=0",                                "INTEGER,MIN=0,MAX=1",             "" },
        { SSC_INPUT, SSC_ARRAY,    "subarray2_custom_cell_temp_array",                   "Subarray 2 custom cell temperatures",                            "C",      "",                                                                                                                                                                                      "System Design",                                              "subarray2_use_custom_cell_temp=1",            "",                    "" },



        {SSC_INPUT, SSC_ARRAY,    "subarray2_soiling",                    "Sub-array 2 Monthly soiling loss",                    "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray2_enable=1",                 "LENGTH=12",           "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_mod_orient",                 "Sub-array 2 Module orientation",                      "0/1",    "0=portrait,1=landscape",                                                                                                                                                                "Layout",                                                "subarray2_enable=1",                 "INTEGER,MIN=0,MAX=1", "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_nmodx",                      "Sub-array 2 Number of modules along bottom of row",   "",       "",                                                                                                                                                                                      "Layout",                                                "subarray2_enable=1",                 "INTEGER,POSITIVE",    "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_nmody",                      "Sub-array 2 Number of modules along side of row",     "",       "",                                                                                                                                                                                      "Layout",                                                "subarray2_enable=1",                 "INTEGER,POSITIVE",    "" },
        {SSC_INPUT, SSC_NUMBER,   "subarray2_backtrack",                  "Sub-array 2 Backtracking enabled",                    "",       "0=no backtracking,1=backtrack",                                                                                                                                                         "System Design",                                         "",                                   "BOOLEAN",             "" },

        // subarray 3
        { SSC_INPUT, SSC_NUMBER,   "subarray3_enable",                     "Sub-array 3 Enable",                                  "0/1",    "0=disabled,1=enabled",                                                                                                                                                                  "System Design",                                         "?=0",                                "BOOLEAN",             "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_modules_per_string",         "Sub-array 3 Modules per string",                      "",       "",                                                                                                                                                                                      "System Design",                                         "subarray3_enable=1",                 "INTEGER,MIN=1",       "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_nstrings",                   "Sub-array 3 Number of parallel strings",              "",       "",                                                                                                                                                                                      "System Design",                                         "subarray3_enable=1",                 "INTEGER,MIN=1",       "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_mppt_input",                 "Sub-array 3 Inverter MPPT input number",              "",       "",                                                                                                                                                                                      "System Design",                                         "?=1",                                "INTEGER,POSITIVE",    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_tilt",                       "Sub-array 3 Tilt",                                    "degrees",    "0=horizontal,90=vertical",                                                                                                                                                              "System Design",                                         "subarray3_enable=1&subarray3_track_mode~2&subarray3_track_mode~4",                 "MIN=0,MAX=90",        "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_tilt_eq_lat",                "Sub-array 3 Tilt=latitude override",                  "0/1",    "0=false,1=override",                                                                                                                                                                    "System Design",                                         "?=0",                                   "BOOLEAN",             "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_azimuth",                    "Sub-array 3 Azimuth",                                 "degrees",    "0=N,90=E,180=S,270=W",                                                                                                                                                                  "System Design",                                         "subarray3_enable=1&subarray3_track_mode~2&subarray3_track_mode~3",                 "MIN=0,MAX=359.9",     "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_track_mode",                 "Sub-array 3 Tracking mode",                           "",       "0=fixed,1=1axis,2=2axis,3=azi,4=monthly",                                                                                                                                               "System Design",                                         "subarray3_enable=1",                 "INTEGER,MIN=0,MAX=4", "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_rotlim",                     "Sub-array 3 Tracker rotation limit",                  "degrees",    "",                                                                                                                                                                                      "System Design",                                         "?=45",                               "MIN=0,MAX=85",        "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_shade_mode",                 "Sub-array 3 Shading mode (fixed tilt or 1x tracking)","0/1/2",  "0=none,1=standard(non-linear),2=thin film(linear)",                                                                                                                                     "Shading",                                               "subarray3_enable=1",                 "INTEGER,MIN=0,MAX=2", "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_gcr",                        "Sub-array 3 Ground coverage ratio",                   "0..1",   "",                                                                                                                                                                                      "System Design",                                         "?=0.3",                              "MIN=0.01,MAX=0.99",   "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_slope_tilt",                 "Sub-array 3 terrain tilt",                        "degrees",   "",                                                                                                                                                                                      "System Design",                                         "subarray3_enable=1&subarray3_track_mode=1",                                                "MIN=0,MAX=90",   "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_slope_azm",                  "Sub-array 3 terrain azimuth",                        "degrees",   "",                                                                                                                                                                                      "System Design",                                         "subarray3_enable=1&subarray3_track_mode=1",                                             "MIN=0,MAX=359.9",   "" },
        { SSC_INPUT, SSC_ARRAY,    "subarray3_monthly_tilt",               "Sub-array 3 Monthly tilt input",                      "degrees",    "",                                                                                                                                                                                      "System Design",                                         "subarray3_enable=1&subarray3_track_mode=4",                                   "LENGTH=12",           "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_use_custom_rot_angles",                    "Sub-array 3 Use custom tracker rotation angles",         "0/1",       "",                                                                                                                                                                                      "System Design",                                         "?=0",                                "INTEGER,MIN=0,MAX=1",             "" },
        { SSC_INPUT, SSC_ARRAY,    "subarray3_custom_rot_angles_array",                   "Sub-array 3 Custom tracker rotation angles",                            "deg",      "",                                                                                                                                                                                      "System Design",                                              "subarray3_use_custom_rot_angles=1",            "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_use_custom_cell_temp",                    "Subarray 3 Use custom cell temperatures",         "0/1",       "",                                                                                                                                                                                      "System Design",                                         "?=0",                                "INTEGER,MIN=0,MAX=1",             "" },
        { SSC_INPUT, SSC_ARRAY,    "subarray3_custom_cell_temp_array",                   "Subarray 3 custom cell temperatures",                            "C",      "",                                                                                                                                                                                      "System Design",                                              "subarray3_use_custom_cell_temp=1",            "",                    "" },

        { SSC_INPUT, SSC_NUMBER,   "subarray3_shading_en_string_option",   "Enable Sub-array 3 shading string option",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_shading_string_option",      "Sub-array 3 shading string option",                   "",       "0=shadingdb,1=average,2=maximum,3=minimum",                                                                                                                            "Shading",                                               "?=-1",                               "INTEGER,MIN=-1,MAX=4","" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_shading_en_timestep",         "Enable Sub-array 3 timestep beam shading losses",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "subarray3_shading_timestep",           "Sub-array 3 timestep beam shading losses",            "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_shading_en_mxh",               "Enable Sub-array 3 Month x Hour beam shading losses",  "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "subarray3_shading_mxh",                "Sub-array 3 Month x Hour beam shading losses",        "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_shading_en_azal",               "Enable Sub-array 3 Azimuth x altitude beam shading losses",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "subarray3_shading_azal",               "Sub-array 3 Azimuth x altitude beam shading losses",  "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_shading_en_diff",               "Enable Sub-array 3 Diffuse shading loss",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_shading_diff",               "Sub-array 3 Diffuse shading loss",                    "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },



        { SSC_INPUT, SSC_ARRAY,    "subarray3_soiling",                    "Sub-array 3 Monthly soiling loss",                    "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray3_enable=1",                 "LENGTH=12",           "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_mod_orient",                 "Sub-array 3 Module orientation",                      "0/1",    "0=portrait,1=landscape",                                                                                                                                                                "Layout",                                                "subarray3_enable=1",                 "INTEGER,MIN=0,MAX=1", "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_nmodx",                      "Sub-array 3 Number of modules along bottom of row",   "",       "",                                                                                                                                                                                      "Layout",                                                "subarray3_enable=1",                 "INTEGER,POSITIVE",    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_nmody",                      "Sub-array 3 Number of modules along side of row",     "",       "",                                                                                                                                                                                      "Layout",                                                "subarray3_enable=1",                 "INTEGER,POSITIVE",    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray3_backtrack",                  "Sub-array 3 Backtracking enabled",                    "",       "0=no backtracking,1=backtrack",                                                                                                                                                         "System Design",                                         "",                                   "BOOLEAN",             "" },

        // subarray 4
        { SSC_INPUT, SSC_NUMBER,   "subarray4_enable",                     "Sub-array 4 Enable",                                  "0/1",    "0=disabled,1=enabled",                                                                                                                                                                  "System Design",                                         "?=0",                                "BOOLEAN",             "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_modules_per_string",         "Sub-array 4 Modules per string",                      "",       "",                                                                                                                                                                                      "System Design",                                         "subarray4_enable=1",                 "INTEGER,MIN=1",       "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_nstrings",                   "Sub-array 4 Number of parallel strings",              "",       "",                                                                                                                                                                                      "System Design",                                         "subarray4_enable=1",                 "INTEGER,MIN=1",       "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_mppt_input",                 "Sub-array 4 Inverter MPPT input number",              "",       "",                                                                                                                                                                                      "System Design",                                         "?=1",                                "INTEGER,POSITIVE",    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_tilt",                       "Sub-array 4 Tilt",                                    "degrees",    "0=horizontal,90=vertical",                                                                                                                                                              "System Design",                                         "subarray4_enable=1&subarray4_track_mode~2&subarray4_track_mode~4",                 "MIN=0,MAX=90",        "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_tilt_eq_lat",                "Sub-array 4 Tilt=latitude override",                  "0/1",    "0=false,1=override",                                                                                                                                                                    "System Design",                                         "?=0",                                   "BOOLEAN",             "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_azimuth",                    "Sub-array 4 Azimuth",                                 "degrees",    "0=N,90=E,180=S,270=W",                                                                                                                                                                  "System Design",                                         "subarray4_enable=1&subarray4_track_mode~2&subarray4_track_mode~3",                 "MIN=0,MAX=359.9",     "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_track_mode",                 "Sub-array 4 Tracking mode",                           "",       "0=fixed,1=1axis,2=2axis,3=azi,4=monthly",                                                                                                                                               "System Design",                                         "subarray4_enable=1",                 "INTEGER,MIN=0,MAX=4", "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_rotlim",                     "Sub-array 4 Tracker rotation limit",                  "degrees",    "",                                                                                                                                                                                      "System Design",                                         "?=45",                               "MIN=0,MAX=85",        "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_shade_mode",                 "Sub-array 4 shading mode (fixed tilt or 1x tracking)","0/1/2",  "0=none,1=standard(non-linear),2=thin film(linear)",                                                                                                                                     "Shading",                                               "subarray4_enable=1",                 "INTEGER,MIN=0,MAX=2", "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_gcr",                        "Sub-array 4 Ground coverage ratio",                   "0..1",   "",                                                                                                                                                                                      "System Design",                                         "?=0.3",                              "MIN=0.01,MAX=0.99",   "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_slope_tilt",                 "Sub-array 4 terrain tilt",                        "degrees",   "",                                                                                                                                                                                      "System Design",                                         "subarray4_enable=1&subarray4_track_mode=1",                                                "MIN=0,MAX=90",   "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_slope_azm",                  "Sub-array 4 terrain azimuth",                        "degrees",   "",                                                                                                                                                                                      "System Design",                                         "subarray4_enable=1&subarray4_track_mode=1",                                                "MIN=0,MAX=359.9",   "" },
        { SSC_INPUT, SSC_ARRAY,    "subarray4_monthly_tilt",               "Sub-array 4 Monthly tilt input",                      "degrees",    "",                                                                                                                                                                                      "System Design",                                         "subarray2_enable=1&subarray2_track_mode=4",                                   "LENGTH=12",           "" },
//        { SSC_INPUT, SSC_TABLE,   "subarray4_shading",           "Sub-array 4 shading losses",            "",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_use_custom_rot_angles",                    "Sub-array 4 Use custom tracker rotation angles",         "0/1",       "",                                                                                                                                                                                      "System Design",                                         "?=0",                                "INTEGER,MIN=0,MAX=1",             "" },
        { SSC_INPUT, SSC_ARRAY,    "subarray4_custom_rot_angles_array",                   "Sub-array 4 Custom tracker rotation angles",                            "deg",      "",                                                                                                                                                                                      "System Design",                                              "subarray4_use_custom_rot_angles=1",            "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_use_custom_cell_temp",                    "Subarray 4 Use custom cell temperatures",         "0/1",       "",                                                                                                                                                                                      "System Design",                                         "?=0",                                "INTEGER,MIN=0,MAX=1",             "" },
        { SSC_INPUT, SSC_ARRAY,    "subarray4_custom_cell_temp_array",                   "Subarray 4 custom cell temperatures",                            "C",      "",                                                                                                                                                                                      "System Design",                                              "subarray4_use_custom_cell_temp=1",            "",                    "" },

        { SSC_INPUT, SSC_NUMBER,   "subarray4_shading_en_string_option",   "Enable Sub-array 4 shading string option",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_shading_string_option",      "Sub-array 4 shading string option",                   "",       "0=shadingdb,1=average,2=maximum,3=minimum",                                                                                                                            "Shading",                                               "?=-1",                               "INTEGER,MIN=-1,MAX=4","" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_shading_en_timestep",         "Enable Sub-array 4 timestep beam shading losses",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "subarray4_shading_timestep",           "Sub-array 4 timestep beam shading losses",            "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_shading_en_mxh",               "Enable Sub-array 4 Month x Hour beam shading losses",  "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "subarray4_shading_mxh",                "Sub-array 4 Month x Hour beam shading losses",        "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_shading_en_azal",               "Enable Sub-array 4 Azimuth x altitude beam shading losses",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "subarray4_shading_azal",               "Sub-array 4 Azimuth x altitude beam shading losses",  "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_shading_en_diff",               "Enable Sub-array 4 Diffuse shading loss",          "0/1",    "0=false,1=true",                                                                                                                                                                        "Shading",                                               "?=0",                                  "BOOLEAN",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_shading_diff",               "Sub-array 4 Diffuse shading loss",                    "%",      "",                                                                                                                                                                                      "Shading",                                               "?",                                  "",                    "" },



        { SSC_INPUT, SSC_ARRAY,    "subarray4_soiling",                    "Sub-array 4 Monthly soiling loss",                    "%",      "",                                                                                                                                                                                      "Losses",                                                "subarray4_enable=1",                 "LENGTH=12",           "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_mod_orient",                 "Sub-array 4 Module orientation",                      "0/1",    "0=portrait,1=landscape",                                                                                                                                                                "Layout",                                                "subarray4_enable=1",                 "INTEGER,MIN=0,MAX=1", "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_nmodx",                      "Sub-array 4 Number of modules along bottom of row",   "",       "",                                                                                                                                                                                      "Layout",                                                "subarray4_enable=1",                 "INTEGER,POSITIVE",    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_nmody",                      "Sub-array 4 Number of modules along side of row",     "",       "",                                                                                                                                                                                      "Layout",                                                "subarray4_enable=1",                 "INTEGER,POSITIVE",    "" },
        { SSC_INPUT, SSC_NUMBER,   "subarray4_backtrack",                  "Sub-array 4 Backtracking enabled",                    "",       "0=no backtracking,1=backtrack",                                                                                                                                                         "System Design",                                         "",                                   "BOOLEAN",             "" },

        // module
        { SSC_INPUT, SSC_NUMBER,   "module_model",                         "Photovoltaic module model specifier",                 "",       "0=spe,1=cec,2=6par_user,3=snl,4=sd11-iec61853,5=PVYield",                                                                                                                               "Module",                                                "*",                                  "INTEGER,MIN=0,MAX=5", "" },
        { SSC_INPUT, SSC_NUMBER,   "module_aspect_ratio",                  "Module aspect ratio",                                 "",       "",                                                                                                                                                                                      "Layout",                                                "?=1.7",                              "POSITIVE",            "" },

        // spe model
        { SSC_INPUT, SSC_NUMBER,   "spe_area",                             "Module area",                                         "m2",     "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_rad0",                             "Irradiance level 0",                                  "W/m2",   "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_rad1",                             "Irradiance level 1",                                  "W/m2",   "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_rad2",                             "Irradiance level 2",                                  "W/m2",   "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_rad3",                             "Irradiance level 3",                                  "W/m2",   "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_rad4",                             "Irradiance level 4",                                  "W/m2",   "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_eff0",                             "Efficiency at irradiance level 0",                    "%",      "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_eff1",                             "Efficiency at irradiance level 1",                    "%",      "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_eff2",                             "Efficiency at irradiance level 2",                    "%",      "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_eff3",                             "Efficiency at irradiance level 3",                    "%",      "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_eff4",                             "Efficiency at irradiance level 4",                    "%",      "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_reference",                        "Reference irradiance level",                          "",       "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "INTEGER,MIN=0,MAX=4", "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_module_structure",                 "Mounting and module structure",                       "",       "0=glass/cell/polymer sheet - open rack,1=glass/cell/glass - open rack,2=polymer/thin film/steel - open rack,3=Insulated back, building-integrated PV,4=close roof mount,5=user-defined","Simple Efficiency Module Model",                        "module_model=0",                     "INTEGER,MIN=0,MAX=5", "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_a",                                "Cell temp parameter a",                               "",       "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_b",                                "Cell temp parameter b",                               "",       "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_dT",                               "Cell temp parameter dT",                              "",       "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_temp_coeff",                       "Temperature coefficient",                             "%/C",    "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_fd",                               "Diffuse fraction",                                    "0..1",   "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "MIN=0,MAX=1",         "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_vmp",                              "Nominal max power voltage",                           "V",      "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "POSITIVE",            "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_voc",                              "Nominal open circuit voltage",                        "V",      "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "POSITIVE",            "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_is_bifacial",                      "Modules are bifacial",                                "0/1",    "0=monofacial,1=bifacial",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_bifacial_transmission_factor",     "Bifacial transmission factor",                        "0-1",    "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_bifaciality",                      "Bifaciality factor",                                  "%",      "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "spe_bifacial_ground_clearance_height", "Module ground clearance height",                      "m",      "",                                                                                                                                                                                      "Simple Efficiency Module Model",                        "module_model=0",                     "POSITIVE",                    "" },

// cec model
{ SSC_INPUT, SSC_NUMBER,   "cec_area",                             "Module area",                                         "m2",     "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_a_ref",                            "Nonideality factor a",                                "",       "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_adjust",                           "Temperature coefficient adjustment",                  "%",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_alpha_sc",                         "Short circuit current temperature coefficient",       "A/C",    "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_beta_oc",                          "Open circuit voltage temperature coefficient",        "V/C",    "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_i_l_ref",                          "Light current",                                       "A",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_i_mp_ref",                         "Maximum power point current",                         "A",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_i_o_ref",                          "Saturation current",                                  "A",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_i_sc_ref",                         "Short circuit current",                               "A",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_n_s",                              "Number of cells in series",                           "",       "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "POSITIVE",            "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_r_s",                              "Series resistance",                                   "ohm",    "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_r_sh_ref",                         "Shunt resistance",                                    "ohm",    "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_t_noct",                           "Nominal operating cell temperature",                  "C",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_v_mp_ref",                         "Maximum power point voltage",                         "V",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_v_oc_ref",                         "Open circuit voltage",                                "V",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_temp_corr_mode",                   "Cell temperature model selection",                    "",       "0=noct,1=mc",                                                                                                                                                                           "CEC Performance Model with Module Database",            "module_model=1",                     "INTEGER,MIN=0,MAX=1", "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_is_bifacial",                      "Modules are bifacial",                                "0/1",    "0=monofacial,1=bifacial",                                                                                                                                                               "CEC Performance Model with Module Database",            "module_model=1",                     "INTEGER,MIN=0,MAX=1", "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_bifacial_transmission_factor",     "Bifacial transmission factor",                        "0-1",    "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "MIN=0,MAX=1",         "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_bifaciality",                      "Bifaciality factor",                                  "%",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_bifacial_ground_clearance_height", "Module ground clearance height",                      "m",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1",                     "POSITIVE",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_standoff",                         "Standoff mode",                                       "",       "0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack",                                                                                                            "CEC Performance Model with Module Database",            "module_model=1",                     "INTEGER,MIN=0,MAX=6", "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_height",                           "Array mounting height",                               "",       "0=one story,1=two story",                                                                                                                                                               "CEC Performance Model with Module Database",            "module_model=1",                     "INTEGER,MIN=0,MAX=1", "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_mounting_config",                  "Mounting configuration",                              "",       "0=rack,1=flush,2=integrated,3=gap",                                                                                                                                                     "CEC Performance Model with Module Database",            "module_model=1&cec_temp_corr_mode=1","INTEGER,MIN=0,MAX=3", "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_heat_transfer",                    "Heat transfer dimensions",                            "",       "0=module,1=array",                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1&cec_temp_corr_mode=1","INTEGER,MIN=0,MAX=1", "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_mounting_orientation",             "Mounting structure orientation",                      "",       "0=do not impede flow,1=vertical supports,2=horizontal supports",                                                                                                                        "CEC Performance Model with Module Database",            "module_model=1&cec_temp_corr_mode=1","INTEGER,MIN=0,MAX=2", "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_gap_spacing",                      "Gap spacing",                                         "m",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1&cec_temp_corr_mode=1","",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_module_width",                     "Module width",                                        "m",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1&cec_temp_corr_mode=1","",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_module_length",                    "Module height",                                       "m",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1&cec_temp_corr_mode=1","",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_array_rows",                       "Rows of modules in array",                            "",       "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1&cec_temp_corr_mode=1","",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_array_cols",                       "Columns of modules in array",                         "",       "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1&cec_temp_corr_mode=1","",                    "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_backside_temp",                    "Module backside temperature",                         "C",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "module_model=1&cec_temp_corr_mode=1","POSITIVE",            "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_lacunarity_enable",                    "Enable lacunarity heat transfer model",                         "0/1",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "?=0","",            "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_lacunarity_length",                    "Module lacurnarity length for spatial heterogeneity",                         "C",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "cec_lacunarity_enable=1&cec_temp_corr_mode=1",  "",            "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_ground_clearance_height",                    "Module ground clearance height for heat transfer coefficient",                         "m",      "",                                                                                                                                                                                      "CEC Performance Model with Module Database",            "cec_lacunarity_enable=1&cec_temp_corr_mode=1","POSITIVE",            "" },
{ SSC_INPUT, SSC_NUMBER,   "cec_transient_thermal_model_unit_mass","Module unit mass",                                    "kg/m^2",      "",                                                                                                                                                                        "CEC Performance Model with Module Database",                     "module_model=1","POSITIVE",                                 "" },

        // 6 par model
        { SSC_INPUT, SSC_NUMBER,   "6par_celltech",                        "Solar cell technology type",                          "",       "monoSi=0,multiSi=1,CdTe=2,CIS=3,CIGS=4,Amorphous=5",                                                                                                                                    "CEC Performance Model with User Entered Specifications","module_model=2",                     "INTEGER,MIN=0,MAX=5", "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_vmp",                             "Maximum power point voltage",                         "V",      "",                                                                                                                                                                                      "CEC Performance Model with User Entered Specifications","module_model=2",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_imp",                             "Imp",                                                 "A",      "",                                                                                                                                                                                      "CEC Performance Model with User Entered Specifications","module_model=2",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_voc",                             "Voc",                                                 "V",      "",                                                                                                                                                                                      "CEC Performance Model with User Entered Specifications","module_model=2",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_isc",                             "Isc",                                                 "A",      "",                                                                                                                                                                                      "CEC Performance Model with User Entered Specifications","module_model=2",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_bvoc",                            "Open circuit voltage temperature coefficient",        "V/C",    "",                                                                                                                                                                                      "CEC Performance Model with User Entered Specifications","module_model=2",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_aisc",                            "Short circuit current temperature coefficient",       "A/C",    "",                                                                                                                                                                                      "CEC Performance Model with User Entered Specifications","module_model=2",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_gpmp",                            "Maximum power point temperature coefficient",         "%/C",    "",                                                                                                                                                                                      "CEC Performance Model with User Entered Specifications","module_model=2",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_nser",                            "Nseries",                                             "",       "",                                                                                                                                                                                      "CEC Performance Model with User Entered Specifications","module_model=2",                     "INTEGER,POSITIVE",    "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_area",                            "Module area",                                         "m2",     "",                                                                                                                                                                                      "CEC Performance Model with User Entered Specifications","module_model=2",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_tnoct",                           "Nominal operating cell temperature",                  "C",      "",                                                                                                                                                                                      "CEC Performance Model with User Entered Specifications","module_model=2",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_standoff",                        "Standoff mode",                                       "",       "0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack",                                                                                                            "CEC Performance Model with User Entered Specifications","module_model=2",                     "INTEGER,MIN=0,MAX=6", "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_mounting",                        "Array mounting height",                               "",       "0=one story,1=two story",                                                                                                                                                               "CEC Performance Model with User Entered Specifications","module_model=2",                     "INTEGER,MIN=0,MAX=1", "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_is_bifacial",                     "Modules are bifacial",                                "0/1",    "0=monofacial,1=bifacial",                                                                                                                                                                                      "CEC Performance Model with User Entered Specifications","module_model=2",                     "INTEGER,MIN=0,MAX=1", "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_bifacial_transmission_factor",    "Bifacial transmission factor",                        "0-1",    "",                                                                                                                                                                                      "CEC Performance Model with User Entered Specifications","module_model=2",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_bifaciality",                     "Bifaciality factor",                                  "%",      "",                                                                                                                                                                                      "CEC Performance Model with User Entered Specifications","module_model=2",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_bifacial_ground_clearance_height","Module ground clearance height",                      "m",      "",                                                                                                                                                                                      "CEC Performance Model with User Entered Specifications","module_model=2",                     "POSITIVE",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "6par_transient_thermal_model_unit_mass","Module unit mass",                      "kg/m^2",      "",                                                                                                                                                                                              "CEC Performance Model with User Entered Specifications","module_model=2",                     "",                    "" },

        // snl module model
        { SSC_INPUT, SSC_NUMBER,   "snl_module_structure",                 "Module and mounting structure configuration",         "",       "0=Use Database Values,1=glass/cell/polymer sheet-open rack,2=glass/cell/glass-open rack,3=polymer/thin film/steel-open rack,4=Insulated back BIPV,5=close roof mount,6=user-defined",   "Sandia PV Array Performance Model with Module Database","module_model=3",                     "INTEGER,MIN=0,MAX=6", "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_a",                                "Temperature coefficient a",                           "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_b",                                "Temperature coefficient b",                           "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_dtc",                              "Temperature coefficient dT",                          "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_ref_a",                            "User-specified a",                                    "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_ref_b",                            "User-specified b",                                    "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_ref_dT",                           "User-specified dT",                                   "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_fd",                               "Diffuse fraction",                                    "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_a0",                               "Air mass polynomial coeff 0",                         "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_a1",                               "Air mass polynomial coeff 1",                         "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_a2",                               "Air mass polynomial coeff 2",                         "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_a3",                               "Air mass polynomial coeff 3",                         "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_a4",                               "Air mass polynomial coeff 4",                         "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_aimp",                             "Max power point current temperature coefficient",     "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_aisc",                             "Short circuit current temperature coefficient",       "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_area",                             "Module area",                                         "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_b0",                               "Incidence angle modifier polynomial coeff 0",         "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_b1",                               "Incidence angle modifier polynomial coeff 1",         "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_b2",                               "Incidence angle modifier polynomial coeff 2",         "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_b3",                               "Incidence angle modifier polynomial coeff 3",         "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_b4",                               "Incidence angle modifier polynomial coeff 4",         "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_b5",                               "Incidence angle modifier polynomial coeff 5",         "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_bvmpo",                            "Max power point voltage temperature coefficient",     "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_bvoco",                            "Open circuit voltage temperature coefficient",        "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_c0",                               "C0",                                                  "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_c1",                               "C1",                                                  "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_c2",                               "C2",                                                  "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_c3",                               "C3",                                                  "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_c4",                               "C4",                                                  "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_c5",                               "C5",                                                  "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_c6",                               "C6",                                                  "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_c7",                               "C7",                                                  "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_impo",                             "Max power point current",                             "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_isco",                             "Short circuit current",                               "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_ixo",                              "Ix midpoint current",                                 "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_ixxo",                             "Ixx midpoint current",                                "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_mbvmp",                            "Irradiance dependence of Vmp temperature coefficient","",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_mbvoc",                            "Irradiance dependence of Voc temperature coefficient","",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_n",                                "Diode factor",                                        "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_series_cells",                     "Number of cells in series",                           "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "INTEGER",             "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_vmpo",                             "Max power point voltage",                             "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_voco",                             "Open circuit voltage",                                "",       "",                                                                                                                                                                                      "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "snl_transient_thermal_model_unit_mass",                             "Module unit mass",                                "kg/m^2",       "",                                                                                                                                                        "Sandia PV Array Performance Model with Module Database","module_model=3",                     "",                   "" },

        // 11 parameter model
        { SSC_INPUT, SSC_NUMBER,   "sd11par_nser",                         "Nseries",                                             "",       "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "INTEGER,POSITIVE",    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_area",                         "Module area",                                         "m2",     "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_AMa0",                         "Air mass modifier coeff 0",                           "",       "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_AMa1",                         "Air mass modifier coeff 1",                           "",       "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_AMa2",                         "Air mass modifier coeff 2",                           "",       "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_AMa3",                         "Air mass modifier coeff 3",                           "",       "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_AMa4",                         "Air mass modifier coeff 4",                           "",       "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_glass",                        "Module cover glass type",                             "",       "0=normal,1=AR glass",                                                                                                                                                                   "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_tnoct",                        "Nominal operating cell temperature",                  "C",      "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_standoff",                     "Standoff mode",                                       "",       "0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack",                                                                                                            "IEC61853 Single Diode Model",                           "module_model=4",                     "INTEGER,MIN=0,MAX=6", "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_mounting",                     "Array mounting height",                               "",       "0=one story,1=two story",                                                                                                                                                               "IEC61853 Single Diode Model",                           "module_model=4",                     "INTEGER,MIN=0,MAX=1", "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_Vmp0",                         "Vmp (STC)",                                           "V",      "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_Imp0",                         "Imp (STC)",                                           "A",      "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_Voc0",                         "Voc (STC)",                                           "V",      "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_Isc0",                         "Isc (STC)",                                           "A",      "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_alphaIsc",                     "Short curcuit current temperature coefficient",       "A/C",    "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_n",                            "Diode nonideality factor",                            "",       "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_Il",                           "Light current",                                       "A",      "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_Io",                           "Saturation current",                                  "A",      "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_Egref",                        "Bandgap voltage",                                     "eV",     "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_d1",                           "Rs fit parameter 1",                                  "",       "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_d2",                           "Rs fit parameter 2",                                  "",       "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_d3",                           "Rs fit parameter 3",                                  "",       "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_c1",                           "Rsh fit parameter 1",                                 "",       "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_c2",                           "Rsh fit parameter 2",                                 "",       "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "sd11par_c3",                           "Rsh fit parameter 3",                                 "",       "",                                                                                                                                                                                      "IEC61853 Single Diode Model",                           "module_model=4",                     "",                    "" },

        // mlm model
        { SSC_INPUT, SSC_NUMBER,   "mlm_N_series",                         "Number of cells in series",                           "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_N_parallel",                       "Number of cells in parallel",                         "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_N_diodes",                         "Number of diodes",                                    "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_Width",                            "Module width (short side)",                           "m",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_Length",                           "Module length (long side)",                           "m",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_V_mp_ref",                         "V_mp at STC",                                         "V",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_I_mp_ref",                         "I_mp at STC",                                         "A",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_V_oc_ref",                         "V_oc at STC",                                         "V",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_I_sc_ref",                         "I_sc at STC",                                         "A",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_S_ref",                            "Reference irradiance (Typically 1000W/m²)",          "W/m²",  "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_T_ref",                            "Reference temperature (Typically 25°C)",             "°C",    "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_R_shref",                          "Reference shunt resistance",                          "V/A",    "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_R_sh0",                            "Rsh,0",                                               "V/A",    "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_R_shexp",                          "Rsh exponential coefficient",                         "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_R_s",                              "Series resistance",                                   "V/A",    "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_alpha_isc",                        "Temperature coefficient for I_sc",                    "A/K",    "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_beta_voc_spec",                    "Temperature coefficient for V_oc",                    "V/K",    "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_E_g",                              "Reference bandgap energy",                            "eV",     "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_n_0",                              "Gamma",                                               "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_mu_n",                             "Temperature coefficient of gamma",                    "1/K",    "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_D2MuTau",                          "Coefficient for recombination losses",                "V",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_T_mode",                           "Cell temperature model mode",                         "-",      "1: NOCT",                                                                                                                                                                               "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_T_c_no_tnoct",                     "NOCT cell temperature",                               "°C",    "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_T_c_no_mounting",                  "NOCT Array mounting height",                          "-",      "0=one story,1=two story",                                                                                                                                                               "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_T_c_no_standoff",                  "NOCT standoff mode",                                  "-",      "0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack",                                                                                                            "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_T_c_fa_alpha",                     "Extended Faiman model absorptivity",                  "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_T_c_fa_U0",                        "Extended Faiman model U_0",                           "W/m²K", "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_T_c_fa_U1",                        "Extended Faiman model U_1",                           "W/m³sK","",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_AM_mode",                          "Air-mass modifier mode",                              "-",      "1: Do not consider AM effects, 2: Use Sandia polynomial [corr=f(AM)], 3: Use standard coefficients from DeSoto model [corr=f(AM)], 4: Use First Solar polynomial [corr=f(AM, p_wat)]",  "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_AM_c_sa0",                         "Coefficient 0 for Sandia Air Mass Modifier",          "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_AM_c_sa1",                         "Coefficient 1 for Sandia Air Mass Modifier",          "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_AM_c_sa2",                         "Coefficient 2 for Sandia Air Mass Modifier",          "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_AM_c_sa3",                         "Coefficient 3 for Sandia Air Mass Modifier",          "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_AM_c_sa4",                         "Coefficient 4 for Sandia Air Mass Modifier",          "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_AM_c_lp0",                         "Coefficient 0 for Lee/Panchula Air Mass Modifier",    "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_AM_c_lp1",                         "Coefficient 1 for Lee/Panchula Air Mass Modifier",    "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_AM_c_lp2",                         "Coefficient 2 for Lee/Panchula Air Mass Modifier",    "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_AM_c_lp3",                         "Coefficient 3 for Lee/Panchula Air Mass Modifier",    "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_AM_c_lp4",                         "Coefficient 4 for Lee/Panchula Air Mass Modifier",    "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_AM_c_lp5",                         "Coefficient 5 for Lee/Panchula Air Mass Modifier",    "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_IAM_mode",                         "Incidence Angle Modifier mode",                       "-",      "1: Use ASHRAE formula, 2: Use Sandia polynomial, 3: Use cubic spline with user-supplied data",                                                                                          "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_IAM_c_as",                         "ASHRAE incidence modifier coefficient b_0",           "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_IAM_c_sa0",                        "Sandia IAM coefficient 0",                            "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_IAM_c_sa1",                        "Sandia IAM coefficient 1",                            "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_IAM_c_sa2",                        "Sandia IAM coefficient 2",                            "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_IAM_c_sa3",                        "Sandia IAM coefficient 3",                            "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_IAM_c_sa4",                        "Sandia IAM coefficient 4",                            "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_IAM_c_sa5",                        "Sandia IAM coefficient 5",                            "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_ARRAY,    "mlm_IAM_c_cs_incAngle",                "Spline IAM - Incidence angles",                       "degrees",    "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_ARRAY,    "mlm_IAM_c_cs_iamValue",                "Spline IAM - IAM values",                             "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_groundRelfectionFraction",         "Ground reflection fraction",                          "-",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_is_bifacial",                      "Modules are bifacial",                                "0/1",    "0=monofacial,1=bifacial",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "INTEGER,MIN=0,MAX=1", "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_bifacial_transmission_factor",     "Bifacial transmission factor",                        "0-1",    "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "MIN=0,MAX=1",         "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_bifaciality",                      "Bifaciality factor",                                  "%",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mlm_bifacial_ground_clearance_height", "Module ground clearance height",                      "m",      "",                                                                                                                                                                                      "Mermoud Lejeune Single Diode Model",                    "module_model=5",                     "POSITIVE",                    "" },


        // inverter model
        { SSC_INPUT, SSC_NUMBER,   "inverter_model",                       "Inverter model specifier",                            "",       "0=cec,1=datasheet,2=partload,3=coefficientgenerator,4=PVYield",                                                                                                                         "Inverter",                                              "*",                                  "INTEGER,MIN=0,MAX=4", "" },
        { SSC_INPUT, SSC_NUMBER,   "mppt_low_inverter",                    "Minimum inverter MPPT voltage window",                "Vdc",    "",                                                                                                                                                                                      "Inverter",                                              "?=0",                                "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "mppt_hi_inverter",                     "Maximum inverter MPPT voltage window",                "Vdc",    "",                                                                                                                                                                                      "Inverter",                                              "?=0",                                "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_num_mppt",                         "Number of MPPT inputs",                               "",       "",                                                                                                                                                                                      "Inverter",                                              "?=1",                                "INTEGER,MIN=0,MAX=4", "" },

        { SSC_INPUT, SSC_NUMBER,   "inv_snl_c0",                           "Curvature between AC power and DC power at ref",      "1/W",    "",                                                                                                                                                                                      "Inverter CEC Database",                                 "inverter_model=0",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_snl_c1",                           "Coefficient of Pdco variation with DC input voltage", "1/V",    "",                                                                                                                                                                                      "Inverter CEC Database",                                 "inverter_model=0",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_snl_c2",                           "Coefficient of Pso variation with DC input voltage",  "1/V",    "",                                                                                                                                                                                      "Inverter CEC Database",                                 "inverter_model=0",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_snl_c3",                           "Coefficient of Co variation with DC input voltage",   "1/V",    "",                                                                                                                                                                                      "Inverter CEC Database",                                 "inverter_model=0",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_snl_paco",                         "AC maximum power rating",                             "Wac",    "",                                                                                                                                                                                      "Inverter CEC Database",                                 "inverter_model=0",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_snl_pdco",                         "DC input power at which AC power rating is achieved", "Wdc",    "",                                                                                                                                                                                      "Inverter CEC Database",                                 "inverter_model=0",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_snl_pnt",                          "AC power consumed by inverter at night",              "Wac",    "",                                                                                                                                                                                      "Inverter CEC Database",                                 "inverter_model=0",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_snl_pso",                          "DC power required to enable the inversion process",   "Wdc",    "",                                                                                                                                                                                      "Inverter CEC Database",                                 "inverter_model=0",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_snl_vdco",                         "DC input voltage for the rated AC power rating",      "Vdc",    "",                                                                                                                                                                                      "Inverter CEC Database",                                 "inverter_model=0",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_snl_vdcmax",                       "Maximum DC input operating voltage",                  "Vdc",    "",                                                                                                                                                                                      "Inverter CEC Database",                                 "inverter_model=0",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_cec_cg_c0",                        "Curvature between AC power and DC power at ref",      "1/W",    "",                                                                                                                                                                                      "Inverter CEC Coefficient Generator",                    "inverter_model=3",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_cec_cg_c1",                        "Coefficient of Pdco variation with DC input voltage", "1/V",    "",                                                                                                                                                                                      "Inverter CEC Coefficient Generator",                    "inverter_model=3",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_cec_cg_c2",                        "Coefficient of Pso variation with DC input voltage",  "1/V",    "",                                                                                                                                                                                      "Inverter CEC Coefficient Generator",                    "inverter_model=3",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_cec_cg_c3",                        "Coefficient of Co variation with DC input voltage",   "1/V",    "",                                                                                                                                                                                      "Inverter CEC Coefficient Generator",                    "inverter_model=3",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_cec_cg_paco",                      "AC maximum power rating",                             "Wac",    "",                                                                                                                                                                                      "Inverter CEC Coefficient Generator",                    "inverter_model=3",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_cec_cg_pdco",                      "DC input power at which AC power rating is achieved", "Wdc",    "",                                                                                                                                                                                      "Inverter CEC Coefficient Generator",                    "inverter_model=3",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_cec_cg_pnt",                       "AC power consumed by inverter at night",              "Wac",    "",                                                                                                                                                                                      "Inverter CEC Coefficient Generator",                    "inverter_model=3",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_cec_cg_psco",                      "DC power required to enable the inversion process",   "Wdc",    "",                                                                                                                                                                                      "Inverter CEC Coefficient Generator",                    "inverter_model=3",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_cec_cg_vdco",                      "DC input voltage for the rated AC power rating",      "Vdc",    "",                                                                                                                                                                                      "Inverter CEC Coefficient Generator",                    "inverter_model=3",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_cec_cg_vdcmax",                    "Maximum DC input operating voltage",                  "Vdc",    "",                                                                                                                                                                                      "Inverter CEC Coefficient Generator",                    "inverter_model=3",                   "",                    "" },

        { SSC_INPUT, SSC_NUMBER,   "inv_ds_paco",                          "AC maximum power rating",                             "Wac",    "",                                                                                                                                                                                      "Inverter Datasheet",                                    "inverter_model=1",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_ds_eff",                           "Weighted or Peak or Nominal Efficiency",              "Wdc",    "",                                                                                                                                                                                      "Inverter Datasheet",                                    "inverter_model=1",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_ds_pnt",                           "AC power consumed by inverter at night",              "Wac",    "",                                                                                                                                                                                      "Inverter Datasheet",                                    "inverter_model=1",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_ds_pso",                           "DC power required to enable the inversion process",   "Wdc",    "",                                                                                                                                                                                      "Inverter Datasheet",                                    "inverter_model=1",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_ds_vdco",                          "DC input voltage for the rated AC power rating",      "Vdc",    "",                                                                                                                                                                                      "Inverter Datasheet",                                    "inverter_model=1",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_ds_vdcmax",                        "Maximum DC input operating voltage",                  "Vdc",    "",                                                                                                                                                                                      "Inverter Datasheet",                                    "inverter_model=1",                   "",                    "" },

        { SSC_INPUT, SSC_NUMBER,   "inv_pd_paco",                          "AC maximum power rating",                             "Wac",    "",                                                                                                                                                                                      "Inverter Part Load Curve",                              "inverter_model=2",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_pd_pdco",                          "DC input power at which AC power rating is achieved", "Wdc",    "",                                                                                                                                                                                      "Inverter Part Load Curve",                              "inverter_model=2",                   "",                    "" },
        { SSC_INPUT, SSC_ARRAY,    "inv_pd_partload",                      "Partload curve partload values",                      "%",      "",                                                                                                                                                                                      "Inverter Part Load Curve",                              "inverter_model=2",                   "",                    "" },
        { SSC_INPUT, SSC_ARRAY,    "inv_pd_efficiency",                    "Partload curve efficiency values",                    "%",      "",                                                                                                                                                                                      "Inverter Part Load Curve",                              "inverter_model=2",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_pd_pnt",                           "AC power consumed by inverter at night",              "Wac",    "",                                                                                                                                                                                      "Inverter Part Load Curve",                              "inverter_model=2",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_pd_vdco",                          "DC input voltage for the rated AC power rating",      "Vdc",    "",                                                                                                                                                                                      "Inverter Part Load Curve",                              "inverter_model=2",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "inv_pd_vdcmax",                        "Maximum DC input operating voltage",                  "Vdc",    "",                                                                                                                                                                                      "Inverter Part Load Curve",                              "inverter_model=2",                   "",                    "" },

        { SSC_INPUT, SSC_NUMBER,   "ond_PNomConv",                         "",                                                    "W",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_PMaxOUT",                          "",                                                    "W",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_VOutConv",                         "",                                                    "W",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_VMppMin",                          "",                                                    "V",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_VMPPMax",                          "",                                                    "V",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_VAbsMax",                          "",                                                    "V",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_PSeuil",                           "",                                                    "W",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_STRING,   "ond_ModeOper",                         "",                                                    "-",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_STRING,   "ond_CompPMax",                         "",                                                    "-",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_STRING,   "ond_CompVMax",                         "",                                                    "-",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_STRING,   "ond_ModeAffEnum",                      "",                                                    "-",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_PNomDC",                           "",                                                    "W",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_PMaxDC",                           "",                                                    "W",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_IMaxDC",                           "",                                                    "A",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_INomDC",                           "",                                                    "A",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_INomAC",                           "",                                                    "A",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_IMaxAC",                           "",                                                    "A",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_TPNom",                            "",                                                    "°C",    "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_TPMax",                            "",                                                    "°C",    "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_TPLim1",                           "",                                                    "°C",    "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_TPLimAbs",                         "",                                                    "°C",    "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_PLim1",                            "",                                                    "W",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_PLimAbs",                          "",                                                    "W",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_ARRAY,    "ond_VNomEff",                          "",                                                    "V",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_NbInputs",                         "",                                                    "-",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_NbMPPT",                           "",                                                    "-",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_Aux_Loss",                         "",                                                    "W",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_Night_Loss",                       "",                                                    "W",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_lossRDc",                          "",                                                    "V/A",    "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_lossRAc",                          "",                                                    "A",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_effCurve_elements",                "",                                                    "-",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "ond_effCurve_Pdc",                     "",                                                    "W",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "ond_effCurve_Pac",                     "",                                                    "W",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "ond_effCurve_eta",                     "",                                                    "-",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_Aux_Loss",                         "",                                                    "W",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_Aux_Loss",                         "",                                                    "W",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_doAllowOverpower",                 "",                                                    "-",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_NUMBER,   "ond_doUseTemperatureLimit",            "",                                                    "-",      "",                                                                                                                                                                                      "Inverter Mermoud Lejeune Model",                        "inverter_model=4",                   "",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "inv_tdc_cec_db",                       "Temperature derate curves for CEC Database",          "(Vdc, C, %/C)",    "",                                                                                                                                                                            "Inverter CEC Database",                                 "inverter_model=0",                   "",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "inv_tdc_cec_cg",                       "Temperature derate curves for CEC Coef Gen",          "(Vdc, C, %/C)",    "",                                                                                                                                                                            "Inverter CEC Coefficient Generator",                    "inverter_model=3",                   "",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "inv_tdc_ds",                           "Temperature derate curves for Inv Datasheet",         "(Vdc, C, %/C)",    "",                                                                                                                                                                            "Inverter Datasheet",                                    "inverter_model=1",                   "",                    "" },
        { SSC_INPUT, SSC_MATRIX,   "inv_tdc_plc",                          "Temperature derate curves for Part Load Curve",       "(Vdc, C, %/C)",    "",                                                                                                                                                                            "Inverter Part Load Curve",                              "inverter_model=2",                   "",                    "" },

        // battery storage and dispatch
        { SSC_INPUT, SSC_NUMBER,   "en_batt",                              "Enable battery storage model",                        "0/1",    "",                                                                                                                                                                                      "BatterySystem",                                      "?=0",                                "INTEGER,MIN=0,MAX=1", "" },
        { SSC_INPUT, SSC_NUMBER,   "en_standalone_batt",                   "Enable standalone battery storage model",             "0/1",    "",                     "BatterySystem",                      "?=0",                    "INTEGER,MIN=0,MAX=1",                               "" },
        { SSC_INPUT, SSC_ARRAY,    "load",                                 "Electricity load (year 1)",                           "kW",     "",                                                                                                                                                                                      "Load",                                               "?",                                  "",                    "" },
        { SSC_INPUT, SSC_ARRAY,    "crit_load",                            "Critical Electricity load (year 1)",                  "kW",     "",                                                                                                                                                                                      "Load",                                               "",                                   "",                    "" },
        { SSC_INPUT, SSC_ARRAY,    "grid_outage",                          "Grid outage in this time step",                          "0/1",    "0=GridAvailable,1=GridUnavailable,Length=load", "Load",    "",                       "",                               "" },
        { SSC_INPUT, SSC_NUMBER,   "run_resiliency_calcs",                 "Enable resilence calculations for every timestep",    "0/1",    "0=DisableCalcs,1=EnableCalcs",                  "Load",    "?=0",                    "INTEGER,MIN=0,MAX=1",                               "" },
        { SSC_INPUT, SSC_ARRAY,    "load_escalation",                      "Annual load escalation",                              "%/year", "",                                                                                                                                                                                      "Load",                                               "?=0",                                "",                    "" },
        { SSC_INPUT, SSC_ARRAY,    "crit_load_escalation",                 "Annual critical load escalation",                     "%/year", "",                                                                                                                                                                                      "Load",                                               "?=0",                                "",                    "" },
        // NOTE:  other battery storage model inputs and outputs are defined in batt_common.h/batt_common.cpp

        // PV subhourly clipping inputs
        { SSC_INPUT, SSC_NUMBER,   "enable_subhourly_clipping",                              "Enable subhourly clipping",                        "0/1",    "",                                                                                                                                                                                      "PV Losses",                                      "?=0",                                "INTEGER,MIN=0,MAX=1", "" },
        { SSC_INPUT, SSC_MATRIX,   "subhourly_clipping_matrix",                   "PV Subhourly clipping correction matrix",             "",    "",                     "PV Losses",                      "",                    "",                               "" },
        { SSC_INPUT, SSC_NUMBER,   "enable_subinterval_distribution",                              "Enable subinterval distribution of PV power output",                        "0/1",    "",                                                                                                                                                                                      "PV Losses",                                      "?=0",                                "INTEGER,MIN=0,MAX=1", "" },


    // outputs

    /* environmental conditions */
        // irradiance data from weather file
        { SSC_OUTPUT,        SSC_ARRAY,      "gh",                                         "Irradiance GHI from weather file",                                     "W/m2",   "",                      "Time Series",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "dn",                                         "Irradiance DNI from weather file",                                     "W/m2",   "",                      "Time Series",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "df",                                         "Irradiance DHI from weather file",                                     "W/m2",   "",                      "Time Series",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "wfpoa",                                      "Irradiance POA from weather file",                                     "W/m2",   "",                      "Time Series",       "",                     "",                              "" },

        // header data from weather file
        { SSC_OUTPUT,        SSC_NUMBER,      "lat",                                         "Latitude from weather file",                                          "degrees",   "",                      "Location",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,      "lon",                                         "Longitude from weather file",                                         "degrees",   "",                      "Location",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,      "tz",                                          "Time zone from weather file",                                         "UTC offset",   "",              "Location",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,      "elev",                                        "Site elevation from weather file",                                    "m",   "",                       "Location",       "",                    "",                              "" },

        //not all of these three calculated values will be reported, based on irrad_mode selection
        { SSC_OUTPUT,        SSC_ARRAY,      "gh_calc",                                    "Irradiance GHI calculated",                                       "W/m2",   "",                      "Time Series",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "dn_calc",                                    "Irradiance DNI calculated",                                       "W/m2",   "",                      "Time Series",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "df_calc",                                    "Irradiance DHI calculated",                                       "W/m2",   "",                      "Time Series",       "",                     "",                              "" },

        // non-irradiance data from weather file, note that albedo may be from weather file or monthly array
        { SSC_OUTPUT,        SSC_ARRAY,      "wspd",                                       "Weather file wind speed",                                                        "m/s",    "",                      "Time Series",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "tdry",                                       "Weather file ambient temperature",                                               "C",      "",                      "Time Series",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "alb",                                        "Albedo",							                                 "",       "",                     "Time Series",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_MATRIX,     "alb_spatial",                                "Albedo spatial",  				                                     "",       "",                     "Time Series",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "snowdepth",                                  "Weather file snow depth",							                            "cm",       "",                    "Time Series",       "",                    "",                              "" },

        // calculated sun position data
        { SSC_OUTPUT,        SSC_ARRAY,      "sol_zen",                                    "Sun zenith angle",                                                  "degrees",    "",                      "Time Series",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "sol_alt",                                    "Sun altitude angle",                                                "degrees",    "",                      "Time Series",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "sol_azi",                                    "Sun azimuth angle",                                                 "degrees",    "",                      "Time Series",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "sunup",                                      "Sun up over horizon",                                               "0/1/2/3", "",                     "Time Series",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "sunpos_hour",                                "Sun position time",                                     "hour",   "",                      "Time Series",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "airmass",                                    "Absolute air mass",                                                 "",       "",                      "Time Series",       "*",                    "",                              "" },

        /* sub-array level outputs */
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_surf_tilt",                  "Subarray 1 Surface tilt",                                              "degrees",    "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_surf_azi",                   "Subarray 1 Surface azimuth",                                           "degrees",    "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_aoi",                        "Subarray 1 Angle of incidence",                                        "degrees",    "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_aoi_modifier",               "Subarray 1 Angle of incidence Modifier",                               "0-1",    "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_axisrot",                    "Subarray 1 Axis rotation for 1 axis trackers",                         "degrees",    "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_idealrot",                   "Subarray 1 Axis rotation ideal for 1 axis trackers",                   "degrees",    "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_eff_beam",               "Subarray 1 POA front beam irradiance after shading and soiling",       "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_eff_diff",               "Subarray 1 POA front diffuse irradiance after shading and soiling",    "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_nom",                    "Subarray 1 POA front total irradiance nominal",                        "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_shaded",                 "Subarray 1 POA front total irradiance after shading",             "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_shaded_soiled",          "Subarray 1 POA front total irradiance after shading soiling",      "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_front",                  "Subarray 1 POA front total irradiance after shading soiling reflection (IAM)",         "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_rear",                   "Subarray 1 POA rear total irradiance after reflection (IAM)",          "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_MATRIX,     "subarray1_poa_rear_spatial",           "Subarray 1 POA spatial rear total irradiance after reflection (IAM)",  "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_MATRIX,     "subarray1_ground_rear_spatial",        "Subarray 1 spatial irradiance incident on the ground between rows",    "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_eff",                    "Subarray 1 POA total irradiance after shading soiling reflection (IAM)",               "W/m2",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_soiling_derate",             "Subarray 1 Soiling beam irradiance factor",                            "frac",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_beam_shading_factor",        "Subarray 1 External shading and soiling beam irradiance factor",       "frac",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_linear_derate",              "Subarray 1 Self-shading linear beam irradiance factor",                "frac",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_ss_diffuse_derate",          "Subarray 1 Self-shading non-linear sky diffuse irradiance factor",     "frac",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_ss_reflected_derate",        "Subarray 1 Self-shading non-linear ground diffuse irradiance factor",  "frac",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_ss_derate",                  "Subarray 1 Self-shading non-linear DC factor",                         "frac",   "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "shadedb_subarray1_shade_frac",         "Subarray 1 Partial external shading DC factor",                        "frac",   "", "Time Series (Subarray 1)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_snow_coverage",              "Subarray 1 Fraction of row covered by snow",                                                "0..1",   "", "Time Series (Subarray 1)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_snow_loss",                  "Subarray 1 Snow cover DC power loss",                                  "kW",     "", "Time Series (Subarray 1)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_modeff",                     "Subarray 1 Module efficiency",                                         "%",      "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_celltemp",                   "Subarray 1 Cell temperature",                                          "C",      "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_celltempSS",                 "Subarray 1 Cell temperature (steady state)",                           "C",      "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_beam_front_cs",                 "Subarray 1 beam POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_diffuse_front_cs",                 "Subarray 1 diffuse POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_ground_front_cs",                 "Subarray 1 ground reflected POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_poa_rear_cs",                 "Subarray 1 rear POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },

    { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_dc_voltage",                 "Subarray 1 Operating DC voltage",                                         "V",      "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
    { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_dc_gross",                   "Subarray 1 DC power gross",                                             "kW",      "", "Time Series (Subarray 1)",       "*",                    "",                              "" },
    { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_voc",                        "Subarray 1 Open circuit DC voltage",                                      "V",      "", "Time Series (Subarray 1)",       "",                     "",                              "" },
    { SSC_OUTPUT,        SSC_ARRAY,      "subarray1_isc",                        "Subarray 1 String short circuit DC current",                                     "A",      "", "Time Series (Subarray 1)",       "",                     "",                              "" },

        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_surf_tilt",                  "Subarray 2 Surface tilt",                                              "degrees",    "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_surf_azi",                   "Subarray 2 Surface azimuth",                                           "degrees",    "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_aoi",                        "Subarray 2 Angle of incidence",                                        "degrees",    "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_aoi_modifier",               "Subarray 2 Angle of incidence Modifier",                               "0-1",    "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_axisrot",                    "Subarray 2 Axis rotation for 1 axis trackers",                         "degrees",    "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_idealrot",                   "Subarray 2 Axis rotation ideal for 1 axis trackers",                   "degrees",    "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_eff_beam",               "Subarray 2 POA front beam irradiance after shading and soiling",       "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_eff_diff",               "Subarray 2 POA front diffuse irradiance after shading and soiling",    "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_nom",                    "Subarray 2 POA front total irradiance nominal",                        "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_shaded",                 "Subarray 2 POA front total irradiance after shading",             "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_shaded_soiled",          "Subarray 2 POA front total irradiance after shading soiling",      "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_front",                  "Subarray 2 POA front total irradiance after shading soiling reflection (IAM)",         "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_rear",                   "Subarray 2 POA rear irradiance after reflection (IAM)",                "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_MATRIX,     "subarray2_poa_rear_spatial",           "Subarray 2 POA spatial rear total irradiance after reflection (IAM)",  "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_MATRIX,     "subarray2_ground_rear_spatial",        "Subarray 2 spatial irradiance incident on the ground between rows",    "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_eff",                    "Subarray 2 POA total irradiance after shading soiling reflection (IAM)",        "W/m2",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_soiling_derate",             "Subarray 2 Soiling beam irradiance factor",                            "frac",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_beam_shading_factor",        "Subarray 2 External shading and soiling beam irradiance factor",       "frac",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_linear_derate",              "Subarray 2 Self-shading linear beam irradiance factor",                "frac",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_ss_diffuse_derate",          "Subarray 2 Self-shading non-linear sky diffuse irradiance factor",     "frac",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_ss_reflected_derate",        "Subarray 2 Self-shading non-linear ground diffuse irradiance factor",  "frac",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_ss_derate",                  "Subarray 2 Self-shading non-linear DC factor",                         "frac",   "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "shadedb_subarray2_shade_frac",         "Subarray 2 Partial shading DC factor",                                 "frac",   "", "Time Series (Subarray 2)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_snow_coverage",				 "Subarray 2 Fraction of row covered by snow",                                                "0..1",   "", "Time Series (Subarray 2)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_snow_loss",					 "Subarray 2 Snow cover DC power loss",                                  "kW",     "", "Time Series (Subarray 2)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_modeff",                     "Subarray 2 Module efficiency",                                         "%",      "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_celltemp",                   "Subarray 2 Cell temperature",                                          "C",      "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_celltempSS",                 "Subarray 2 Cell temperature (steady state)",                           "C",      "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_dc_voltage",                 "Subarray 2 Operating DC voltage",                                         "V",      "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_dc_gross",                 "Subarray 2 DC power gross",                                         "kW",      "", "Time Series (Subarray 2)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_voc",                        "Subarray 2 Open circuit DC voltage",                                      "V",      "", "Time Series (Subarray 2)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_isc",                        "Subarray 2 String short circuit DC current",                                     "A",      "", "Time Series (Subarray 2)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_beam_front_cs",                 "Subarray 2 beam POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_diffuse_front_cs",                 "Subarray 2 diffuse POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_ground_front_cs",                 "Subarray 2 ground reflected POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray2_poa_rear_cs",                 "Subarray 2 rear POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },

        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_surf_tilt",                  "Subarray 3 Surface tilt",                                              "degrees",    "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_surf_azi",                   "Subarray 3 Surface azimuth",                                           "degrees",    "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_aoi",                        "Subarray 3 Angle of incidence",                                        "degrees",    "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_aoi_modifier",               "Subarray 3 Angle of incidence Modifier",                               "0-1",    "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_axisrot",                    "Subarray 3 Axis rotation for 1 axis trackers",                         "degrees",    "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_idealrot",                   "Subarray 3 Axis rotation ideal for 1 axis trackers",                   "degrees",    "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_eff_beam",               "Subarray 3 POA front beam irradiance after shading and soiling",       "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_eff_diff",               "Subarray 3 POA front diffuse irradiance after shading and soiling",    "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_nom",                    "Subarray 3 POA font total irradiance nominal",                        "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_shaded",                 "Subarray 3 POA front total irradiance after shading",             "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_shaded_soiled",          "Subarray 3 POA front total irradiance after shading soiling",      "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_front",                  "Subarray 3 POA front total irradiance after shading soiling reflection (IAM)",		 "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_rear",                   "Subarray 3 POA rear irradiance after reflection (IAM)",                "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_MATRIX,     "subarray3_poa_rear_spatial",           "Subarray 3 POA spatial rear total irradiance after reflection (IAM)",  "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_MATRIX,     "subarray3_ground_rear_spatial",        "Subarray 3 spatial irradiance incident on the ground between rows",    "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_eff",                    "Subarray 3 POA total irradiance after shading soiling reflection (IAM)",               "W/m2",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_soiling_derate",             "Subarray 3 Soiling beam irradiance factor",                            "frac",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_beam_shading_factor",        "Subarray 3 External shading and soiling beam irradiance factor",       "frac",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_linear_derate",              "Subarray 3 Self-shading linear beam irradiance factor",                "frac",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_ss_diffuse_derate",          "Subarray 3 Self-shading non-linear sky diffuse irradiance factor",     "frac",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_ss_reflected_derate",        "Subarray 3 Self-shading non-linear ground diffuse irradiance factor",  "frac",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_ss_derate",                  "Subarray 3 Self-shading non-linear DC factor",                         "frac",   "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "shadedb_subarray3_shade_frac",         "Subarray 3 Partial external shading DC factor",                        "frac",   "", "Time Series (Subarray 3)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_snow_coverage",				 "Subarray 3 Fraction of row covered by snow",                                                "0..1",   "", "Time Series (Subarray 3)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_snow_loss",					 "Subarray 3 Snow cover DC power loss",			                         "kW",     "", "Time Series (Subarray 3)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_modeff",                     "Subarray 3 Module efficiency",                                         "%",      "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_celltemp",                   "Subarray 3 Cell temperature",                                          "C",      "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_celltempSS",                 "Subarray 3 Cell temperature (steady state)",                           "C",      "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_dc_voltage",                 "Subarray 3 Operating DC voltage",                                         "V",      "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_dc_gross",                 "Subarray 3 DC power gross",                                         "kW",      "", "Time Series (Subarray 3)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_voc",                        "Subarray 3 Open circuit DC voltage",                                      "V",      "", "Time Series (Subarray 3)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_isc",                        "Subarray 3 String short circuit DC current",                                     "A",      "", "Time Series (Subarray 3)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_beam_front_cs",                 "Subarray 3 beam POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_diffuse_front_cs",                 "Subarray 3 diffuse POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_ground_front_cs",                 "Subarray 3 ground reflected POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray3_poa_rear_cs",                 "Subarray 3 rear POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },

        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_surf_tilt",                  "Subarray 4 Surface tilt",                                              "degrees",    "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_surf_azi",                   "Subarray 4 Surface azimuth",                                           "degrees",    "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_aoi",                        "Subarray 4 Angle of incidence",                                        "degrees",    "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_aoi_modifier",               "Subarray 4 Angle of incidence Modifier",                               "0-1",    "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_axisrot",                    "Subarray 4 Axis rotation for 1 axis trackers",                         "degrees",    "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_idealrot",                   "Subarray 4 Axis rotation ideal for 1 axis trackers",                   "degrees",    "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_eff_beam",               "Subarray 4 POA front beam irradiance after shading and soiling",       "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_eff_diff",               "Subarray 4 POA front diffuse irradiance after shading and soiling",    "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_nom",                    "Subarray 4 POA front total irradiance nominal",                        "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_shaded",                 "Subarray 4 POA front total irradiance after shading",             "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_shaded_soiled",          "Subarray 4 POA front total irradiance after shading soiling",      "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_front",                  "Subarray 4 POA front total irradiance after shading soiling reflection (IAM)",         "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_rear",                   "Subarray 4 POA rear irradiance after reflection (IAM)",                "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_MATRIX,     "subarray4_poa_rear_spatial",           "Subarray 4 POA spatial rear total irradiance after reflection (IAM)",  "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_MATRIX,     "subarray4_ground_rear_spatial",        "Subarray 4 spatial irradiance incident on the ground between rows",    "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_eff",                    "Subarray 4 POA total irradiance after shading soiling reflection (IAM)",               "W/m2",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_soiling_derate",             "Subarray 4 Soiling beam irradiance factor",                            "frac",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_beam_shading_factor",        "Subarray 4 External shading and soiling beam irradiance factor",       "frac",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_linear_derate",              "Subarray 4 Self-shading linear beam irradiance factor",                "frac",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_ss_diffuse_derate",          "Subarray 4 Self-shading non-linear sky diffuse irradiance factor",     "frac",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_ss_reflected_derate",        "Subarray 4 Self-shading non-linear ground diffuse irradiance factor",  "frac",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_ss_derate",                  "Subarray 4 Self-shading non-linear DC factor",                         "frac",   "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "shadedb_subarray4_shade_frac",         "Subarray 4 Partial external shading DC factor",                        "frac",   "", "Time Series (Subarray 4)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_snow_coverage",				 "Subarray 4 Fraction of row covered by snow",                                                "0..1",   "", "Time Series (Subarray 4)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_snow_loss",					 "Subarray 4 Snow cover DC power loss",                                  "kW",     "", "Time Series (Subarray 4)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_modeff",                     "Subarray 4 Module efficiency",                                         "%",      "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_celltemp",                   "Subarray 4 Cell temperature",                                          "C",      "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_celltempSS",                 "Subarray 4 Cell temperature (steady state)",                           "C",      "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_dc_voltage",                 "Subarray 4 Operating DC voltage",                                         "V",      "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_dc_gross",                 "Subarray 4 DC power gross",                                         "kW",      "", "Time Series (Subarray 4)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_voc",                        "Subarray 4 Open circuit DC voltage",                                      "V",      "", "Time Series (Subarray 4)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_isc",                        "Subarray 4 String short circuit DC current",                                     "A",      "", "Time Series (Subarray 4)",       "",                     "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_beam_front_cs",                 "Subarray 4 beam POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_diffuse_front_cs",                 "Subarray 4 diffuse POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_ground_front_cs",                 "Subarray 4 ground reflected POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subarray4_poa_rear_cs",                 "Subarray 4 rear POA clearsky irradiance",             "W/m2",   "", "Time Series (Subarray 1)",       "",                    "",                              "" },

    /* aggregate array level outputs */
        { SSC_OUTPUT,        SSC_ARRAY,      "poa_nom",                              "Array POA front-side total radiation nominal",                    "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "poa_beam_nom",                         "Array POA front-side beam radiation nominal",                     "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "poa_beam_eff",                         "Array POA beam radiation after shading and soiling",              "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "poa_shaded",                           "Array POA front-side total radiation after shading",         "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "poa_shaded_soiled",                    "Array POA front-side total radiation after shading soiling",  "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "poa_front",                            "Array POA front-side total radiation after shading soiling reflection (IAM)",     "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "poa_rear",                             "Array POA rear-side total radiation after reflection (IAM)",      "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "poa_rear_ground_reflected",            "Array POA rear-side radiation reflected from the ground after (IAM)",   "kW", "", "Time Series (Array)",    "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "poa_rear_row_reflections",             "Array POA rear-side radiation reflected from the rear row after (IAM)", "kW", "", "Time Series (Array)",    "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "poa_rear_direct_diffuse",              "Array POA rear-side direct and diffuse radiation after (IAM)",    "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "poa_rear_self_shaded",                 "Array POA rear-side radiation blocked by self shading",           "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "poa_rear_rack_shaded",                 "Array POA rear-side radiation blocked by racks",                  "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "poa_rear_soiled",                      "Array POA rear-side radiation blocked by rear soiling",           "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "bifacial_electrical_mismatch",         "Array bifacial electrical mismatch loss",                         "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "poa_eff",                              "Array POA radiation total after shading soiling reflection (IAM)",                "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "ground_incident",                      "Irradiance incident on the ground within the array",              "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "ground_absorbed",                      "Irradiance absorbed by the ground within the array",              "kW",   "",  "Time Series (Array)",       "*",                    "",                              "" },

        //SEV: total dc snow loss time series (not a required output)
        { SSC_OUTPUT,        SSC_ARRAY,      "dc_snow_loss",                         "DC power loss due to snow",						 "kW",   "",   "Time Series (Array)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "dc_net",                               "Inverter DC input power",                                       "kW",   "",   "Time Series (Array)",       "*",                    "",                              "" },

        // DC Daily losses
        { SSC_OUTPUT,        SSC_ARRAY,     "dc_lifetime_loss",                       "DC lifetime daily loss",                                     "kW", "",    "Time Series (DC Loss)",                 "",                     "",                   "" },

        //mppt outputs
        { SSC_OUTPUT,        SSC_ARRAY,      "inverterMPPT1_DCVoltage",              "Inverter MPPT 1 Nominal DC voltage",                  "V",    "",  "Time Series (MPPT)",           "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "inverterMPPT2_DCVoltage",              "Inverter MPPT 2 Nominal DC voltage",                  "V",    "",  "Time Series (MPPT)",           "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "inverterMPPT3_DCVoltage",              "Inverter MPPT 3 Nominal DC voltage",                  "V",    "",  "Time Series (MPPT)",           "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "inverterMPPT4_DCVoltage",              "Inverter MPPT 4 Nominal DC voltage",                  "V",    "",  "Time Series (MPPT)",           "",                    "",                              "" },

        //inverter outputs
        { SSC_OUTPUT,        SSC_ARRAY,      "inv_eff",                              "Inverter efficiency",                                  "%",    "",  "Time Series (Inverter)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "dc_invmppt_loss",                      "Inverter clipping loss DC MPPT voltage limits",         "kW",  "",  "Time Series (Inverter)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "inv_cliploss",                         "Inverter clipping loss AC power limit",                "kW",   "",  "Time Series (Inverter)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "inv_psoloss",                          "Inverter power consumption loss",                      "kW",   "",  "Time Series (Inverter)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "inv_pntloss",                          "Inverter night time loss",                             "kW",   "",  "Time Series (Inverter)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "inv_tdcloss",                       	 "Inverter thermal derate DC power loss",                "kW",   "",   "Time Series (Inverter)",      "*",             "",                   "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "inv_total_loss",                       "Inverter total power loss",                            "kW",   "",   "Time Series (Inverter)",      "*",             "",                   "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "ac_wiring_loss",                       "AC wiring loss",                                       "kW",   "",   "Time Series (Inverter)",      "*",                        "",                   "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "ac_gross",                             "Inverter AC output power",                                       "kW",   "",   "Time Series (Array)",       "*",                    "",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "subhourly_clipping_loss",                       "Subhourly clipping correction loss",                                       "kW",   "",   "Time Series (Inverter)",      "",                        "",                   "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "distribution_clipping_loss",                       "Subinterval distribution clipping correction loss",                                       "kW",   "",   "Time Series (Inverter)",      "",                        "",                   "" },

        // transformer model outputs
        { SSC_OUTPUT,        SSC_ARRAY,      "xfmr_nll_ts",                          "Transformer no load loss",                              "kW", "",    "Time Series (Transformer)", "", "", "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "xfmr_ll_ts",                           "Transformer load loss",                                 "kW", "",    "Time Series (Transformer)", "", "", "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "xfmr_loss_ts",                         "Transformer total loss",                                "kW", "",    "Time Series (Transformer)", "", "", "" },

        { SSC_OUTPUT,        SSC_ARRAY,     "ac_transmission_loss",                   "Transmission loss",                                     "kW", "",    "Time Series (Transmission)",                 "",                     "",                   "" },

        // Post batt AC losses - record so the powerflows from PV and batt to grid add up properly
        { SSC_OUTPUT,        SSC_ARRAY,     "ac_perf_adj_loss",                       "AC availability loss",                                   "kW", "",    "Time Series (AC Loss)",                 "",                     "",                   "" },
        { SSC_OUTPUT,        SSC_ARRAY,     "ac_lifetime_loss",                       "AC lifetime daily loss",                                     "kW", "",    "Time Series (AC Loss)",                 "",                     "",                   "" },

       // monthly and annual outputs

        { SSC_OUTPUT,		 SSC_NUMBER,     "annual_energy",						 "Annual AC energy in Year 1",                                       "kWh",       "",                      "Annual (Year 1)", "", "", "" },

        { SSC_OUTPUT,        SSC_NUMBER,     "annual_dc_invmppt_loss",               "Inverter clipping loss DC MPPT voltage limits",          "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_inv_cliploss",                  "Inverter clipping loss AC power limit",                  "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_inv_psoloss",                   "Inverter power consumption loss",                        "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_inv_pntloss",                   "Inverter night time loss",                               "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_inv_tdcloss",                   "Inverter thermal derate DC power loss",				   "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },

        { SSC_OUTPUT,        SSC_NUMBER,     "subarray1_dcloss",                     "Subarray 1 Total DC power loss",                                       "%",      "", "Annual (Year 1)",              "",                        "",                   "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "subarray2_dcloss",                     "Subarray 2 Total DC power loss",                                       "%",      "", "Annual (Year 1)",              "",                        "",                   "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "subarray3_dcloss",                     "Subarray 3 Total DC power loss",                                       "%",      "", "Annual (Year 1)",              "",                        "",                   "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "subarray4_dcloss",                     "Subarray 4 Total DC power loss",                                       "%",      "", "Annual (Year 1)",              "",                        "",                   "" },

        { SSC_OUTPUT,        SSC_NUMBER,     "xfmr_nll_year1",                              "Transformer no load loss",                               "kWh/yr", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "xfmr_ll_year1",                               "Transformer load loss",                                  "kWh/yr", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "xfmr_loss_year1",                             "Transformer total loss",                                 "kWh/yr", "", "Annual (Year 1)", "", "", "" },

        { SSC_OUTPUT,        SSC_ARRAY,      "monthly_poa_nom",                             "POA front-side irradiance total nominal",                          "kWh/mo",    "",                      "Monthly",       "",                    "LENGTH=12",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "monthly_poa_beam_nom",                        "POA front-side irradiance beam nominal",                           "kWh/mo",    "",                      "Monthly",       "",                    "LENGTH=12",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "monthly_poa_front",                           "POA front-side irradiance total",                       "kWh/mo",    "",                      "Monthly",       "",                    "LENGTH=12",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "monthly_poa_rear",                            "POA rear-side irradiance total",                       "kWh/mo",    "",                      "Monthly",       "",                    "LENGTH=12",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "monthly_poa_eff",                             "POA irradiance total after shading and soiling",          "kWh/mo",    "",                      "Monthly",       "",                    "LENGTH=12",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "monthly_poa_beam_eff",                        "POA front-side irradiance beam after shading and soiling",           "kWh/mo",    "",                      "Monthly",       "",                    "LENGTH=12",                              "" },

        { SSC_OUTPUT,        SSC_ARRAY,      "monthly_dc",                                  "Monthly DC energy in Year 1",                                   "kWh/mo",    "",                      "Monthly",       "",                    "LENGTH=12",                              "" },
        { SSC_OUTPUT,        SSC_ARRAY,      "monthly_energy",                              "Monthly AC energy in Year 1",                                     "kWh/mo",    "",                      "Monthly",       "",                    "LENGTH=12",                              "" },

        { SSC_OUTPUT,        SSC_NUMBER,     "annual_gh",                                   "Annual GHI",                                                    "Wh/m2/yr",  "",                      "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_nom",                              "POA front-side irradiance total nominal",                       "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_beam_nom",                         "POA front-side irradiance beam nominal",                        "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_shaded",                           "POA front-side irradiance total after shading",                 "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_shaded_soiled",                    "POA front-side irradiance total after shading and soiling",     "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_front",                            "POA front-side irradiance total after reflection (IAM)",            "kWh/yr",    "",                  "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_rear",                             "POA rear-side irradiance total after reflection (IAM)",             "kWh/yr",    "",                  "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_rear_ground_reflected",            "POA rear-side radiation reflected from the ground after (IAM)",     "kWh/yr",    "",                  "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_rear_row_reflections",             "POA rear-side radiation reflected from the rear row after (IAM)",   "kWh/yr",    "",                  "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_rear_direct_diffuse",              "POA rear-side direct and diffuse radiation after reflection (IAM)", "kWh/yr",    "",                  "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_rear_self_shaded",                 "POA blocked by self shading",                                       "kWh/yr",    "",                  "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_rear_rack_shaded",                 "POA blocked by racks",                                              "kWh/yr",    "",                  "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_rear_soiled",                      "POA blocked by rear soiling",                                       "kWh/yr",    "",                  "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_bifacial_electrical_mismatch",         "Bifacial electrical mismatch loss",                                 "kWh/yr",    "",                  "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_ground_incident",                      "Irradiance incident on the ground within the array",                "kWh/yr",    "",                  "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_ground_absorbed",                      "Irradiance absorbed by the ground within the array",                "kWh/yr",    "",                  "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_eff",                              "POA irradiance total after reflection (IAM)",                       "kWh/yr",    "",                  "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_beam_eff",                         "POA front-side irradiance beam after shading and soiling",          "kWh/yr",    "",                  "Annual (Year 1)",       "",                    "",                              "" },

        { SSC_OUTPUT,        SSC_NUMBER,     "annual_dc_nominal",                           "Annual DC energy nominal",                           "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_dc_gross",                             "Annual DC energy gross",                             "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_dc_net",                               "Annual DC energy",                                   "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_ac_gross",                             "Annual AC energy gross",                               "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },

        // OND inverter model AC and DC loss reporting
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_dc_loss_ond",                          "Annual DC loss OND model",                           "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_ac_loss_ond",                          "Annual AC loss OND model",                           "kWh/yr",    "",                      "Annual (Year 1)",       "",                    "",                              "" },

        //SEV: total dc snow loss monthy array and annual value (not a required output)
        { SSC_OUTPUT,        SSC_ARRAY,      "monthly_snow_loss",                    "Snow DC energy loss",					       "kWh/mo",    "",                       "Monthly",       "",                    "",                              "" },
        { SSC_OUTPUT,        SSC_NUMBER,     "annual_snow_loss",                     "Snow DC energy loss",						   "kWh/yr",    "",                       "Annual (Year 1)",       "",                    "",                              "" },

        // loss diagram - order applied
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray1_dc_gross", "Subarray 1 Gross DC energy", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray1_dc_mismatch_loss", "Subarray 1 DC mismatch loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray1_dc_diodes_loss", "Subarray 1 DC diodes and connections loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray1_dc_wiring_loss", "Subarray 1 DC wiring loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray1_dc_tracking_loss", "Subarray 1 DC tracking loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray1_dc_nameplate_loss", "Subarray 1 DC nameplate loss", "kWh", "", "Annual (Year 1)", "", "", "" },

        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray2_dc_gross", "Subarray 2 Gross DC energy", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray2_dc_mismatch_loss", "Subarray 2 DC mismatch loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray2_dc_diodes_loss", "Subarray 2 DC diodes and connections loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray2_dc_wiring_loss", "Subarray 2 DC wiring loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray2_dc_tracking_loss", "Subarray 2 DC tracking loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray2_dc_nameplate_loss", "Subarray 2 DC nameplate loss", "kWh", "", "Annual (Year 1)", "", "", "" },

        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray3_dc_gross", "Subarray 3 Gross DC energy", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray3_dc_mismatch_loss", "Subarray 3 DC mismatch loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray3_dc_diodes_loss", "Subarray 3 DC diodes and connections loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray3_dc_wiring_loss", "Subarray 3 DC wiring loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray3_dc_tracking_loss", "Subarray 3 DC tracking loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray3_dc_nameplate_loss", "Subarray 3 DC nameplate loss", "kWh", "", "Annual (Year 1)", "", "", "" },

        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray4_dc_gross", "Subarray 4 Gross DC energy", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray4_dc_mismatch_loss", "Subarray 4 DC mismatch loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray4_dc_diodes_loss", "Subarray 4 DC diodes and connections loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray4_dc_wiring_loss", "Subarray 4 DC wiring loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray4_dc_tracking_loss", "Subarray 4 DC tracking loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subarray4_dc_nameplate_loss", "Subarray 4 DC nameplate loss", "kWh", "", "Annual (Year 1)", "", "", "" },

        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_mismatch_loss", "DC mismatch loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_diodes_loss", "DC diodes and connections loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_wiring_loss", "DC wiring loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_tracking_loss", "DC tracking loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_nameplate_loss", "DC nameplate loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_optimizer_loss", "DC power optimizer loss", "kWh", "", "Annual (Year 1)", "", "", "" },

        // loss diagram energy outputs nominal poa, nominal array at STC, net dc, net ac, system output
        // annual_poa_nom, annual_dc_nominal, annual_dc_net, annual_ac_net, annual_energy
        // loss diagram % losses
        // annual_poa_nom
        { SSC_OUTPUT, SSC_NUMBER, "annual_poa_shading_loss_percent", "POA front-side shading loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_poa_soiling_loss_percent", "POA front-side soiling loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_poa_cover_loss_percent",   "POA front-side reflection (IAM) loss",   "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_poa_rear_gain_percent",    "POA rear-side bifacial gain", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_ground_incident_percent",   "Ground incident gain", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_ground_absorbed_percent",   "Ground absorbed loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_rear_ground_reflected_percent",   "POA rear-side view factor reflected from ground gain", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_rear_row_reflections_percent",  "POA rear-side rear row reflections gain", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_rear_direct_diffuse_percent",   "POA rear-side direct and diffuse gain", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_rear_self_shaded_percent",      "POA rear-side self shaded loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_rack_shaded_percent",           "POA rear-side rack shaded loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_rear_soiled_percent",           "POA rear-side soiled loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_bifacial_electrical_mismatch_percent",   "Bifacial electrical mismatch loss", "%", "", "Loss", "", "", "" },

        // annual_dc_nominal
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_module_loss_percent", "DC module deviation from STC", "%", "", "Loss", "", "", "" },
        // annual_dc_gross
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_snow_loss_percent", "DC snow loss", "%", "", "Loss", "", "", "" },


        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_mppt_clip_loss_percent", "DC inverter MPPT clipping loss", "%", "", "Loss", "", "", "" },


        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_mismatch_loss_percent", "DC mismatch loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_diodes_loss_percent", "DC diodes and connections loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_wiring_loss_percent", "DC wiring loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_tracking_loss_percent", "DC tracking loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_nameplate_loss_percent", "DC nameplate loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_optimizer_loss_percent", "DC power optimizer loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_perf_adj_loss_percent", "DC availability loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_lifetime_loss_percent", "Lifetime daily DC loss- year 1", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_battery_loss_percent", "DC connected battery loss- year 1", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_dc_inv_tdc_loss_percent", "DC inverter thermal derate loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_distribution_clipping_loss_percent", "Subinterval distribution clipping correction loss percent", "%", "", "Loss", "", "", "" },

        //annual_dc_net
        { SSC_OUTPUT, SSC_NUMBER, "annual_ac_inv_clip_loss_percent", "AC inverter power clipping loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_ac_inv_pso_loss_percent", "AC inverter power consumption loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_ac_inv_pnt_loss_percent", "AC inverter night tare loss", "%", "", "Loss", "", "", "" },

        // annual_ac_gross
        { SSC_OUTPUT, SSC_NUMBER, "annual_ac_inv_eff_loss_percent", "AC inverter efficiency loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_ac_wiring_loss_percent", "AC wiring loss", "%", "", "Loss", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_subhourly_clipping_loss_percent", "Subhourly clipping correction loss percent", "%", "", "Loss", "", "", "" },

        { SSC_OUTPUT, SSC_NUMBER, "annual_transmission_loss_percent", "AC transmission loss", "%", "", "Loss", "", "", "" },
        //	{ SSC_OUTPUT, SSC_NUMBER, "annual_ac_transformer_loss_percent", "AC step-up transformer loss", "%", "", "Loss", "", "", "" },
            { SSC_OUTPUT, SSC_NUMBER, "annual_ac_lifetime_loss_percent", "AC lifetime daily loss - year 1", "%", "", "Loss", "", "", "" },
            { SSC_OUTPUT, SSC_NUMBER, "annual_ac_battery_loss_percent", "AC-connected battery loss - year 1", "%", "", "Loss", "", "", "" },
            { SSC_OUTPUT, SSC_NUMBER, "annual_xfmr_loss_percent", "Transformer loss percent", "%", "", "Loss", "", "", "" },


            // annual_ac_net
            { SSC_OUTPUT, SSC_NUMBER, "annual_ac_perf_adj_loss_percent", "AC availability loss", "%", "", "Loss", "", "", "" },
            // annual_energy

            /*
            { SSC_OUTPUT, SSC_NUMBER, "annual_dc_after_mismatch_loss", "DC output after mismatch loss", "kWh", "", "Annual (Year 1)", "", "", "" },
            { SSC_OUTPUT, SSC_NUMBER, "annual_dc_after_diodes_loss", "DC output after diodes and connections loss", "kWh", "", "Annual (Year 1)", "", "", "" },
            { SSC_OUTPUT, SSC_NUMBER, "annual_dc_after_wiring_loss", "DC output after wiring loss", "kWh", "", "Annual (Year 1)", "", "", "" },
            { SSC_OUTPUT, SSC_NUMBER, "annual_dc_after_tracking_loss", "DC output after tracking loss", "kWh", "", "Annual (Year 1)", "", "", "" },
            { SSC_OUTPUT, SSC_NUMBER, "annual_dc_after_nameplate_loss", "DC output after nameplate loss", "kWh", "", "Annual (Year 1)", "", "", "" },

            { SSC_OUTPUT, SSC_NUMBER, "annual_ac_after_inv_cliploss", "AC output after inverter clipping loss", "kWh", "", "Annual (Year 1)", "", "", "" },
            { SSC_OUTPUT, SSC_NUMBER, "annual_ac_after_inv_psoloss", "AC output after inverter power consumption loss", "kWh", "", "Annual (Year 1)", "", "", "" },
            { SSC_OUTPUT, SSC_NUMBER, "annual_ac_after_inv_pntloss", "AC output after inverter night tare loss", "kWh", "", "Annual (Year 1)", "", "", "" },
            */
            { SSC_OUTPUT, SSC_NUMBER, "annual_ac_wiring_loss", "AC wiring loss", "kWh", "", "Annual (Year 1)", "", "", "" },
            { SSC_OUTPUT, SSC_NUMBER, "annual_subhourly_clipping_loss", "Subhourly clipping correction loss", "kWh", "", "Annual (Year 1)", "", "", "" },
        { SSC_OUTPUT, SSC_NUMBER, "annual_distribution_clipping_loss", "Subinterval distribution clipping correction loss", "kWh", "", "Annual (Year 1)", "", "", "" },

        { SSC_OUTPUT, SSC_NUMBER, "annual_transmission_loss", "Transmission loss", "kWh", "", "Annual (Year 1)", "", "", "" },
            
                { SSC_OUTPUT, SSC_NUMBER, "annual_dc_optimizer_loss", "DC power optimizer loss", "kWh", "", "Annual (Year 1)", "", "", "" },

                { SSC_OUTPUT, SSC_NUMBER, "annual_total_loss_percent", "Total loss from nominal POA to net AC", "%", "", "Annual (Year 1)", "", "", "" },

                //

                { SSC_OUTPUT,        SSC_NUMBER,     "6par_a",                                      "CEC 6-parameter: a",        "",       "", "Module CEC 6-parameter model parameters",       "*",                    "",                              "" },
                { SSC_OUTPUT,        SSC_NUMBER,     "6par_Io",                                     "CEC 6-parameter: Io",       "",       "", "Module CEC 6-parameter model parameters",       "*",                    "",                              "" },
                { SSC_OUTPUT,        SSC_NUMBER,     "6par_Il",                                     "CEC 6-parameter: Il",       "",       "", "Module CEC 6-parameter model parameters",       "*",                    "",                              "" },
                { SSC_OUTPUT,        SSC_NUMBER,     "6par_Rs",                                     "CEC 6-parameter: Rs",       "",       "", "Module CEC 6-parameter model parameters",       "*",                    "",                              "" },
                { SSC_OUTPUT,        SSC_NUMBER,     "6par_Rsh",                                    "CEC 6-parameter: Rsh",      "",       "", "Module CEC 6-parameter model parameters",       "*",                    "",                              "" },
                { SSC_OUTPUT,        SSC_NUMBER,     "6par_Adj",                                    "CEC 6-parameter: Adj",      "",       "", "Module CEC 6-parameter model parameters",       "*",                    "",                              "" },

                { SSC_OUTPUT,        SSC_NUMBER,     "performance_ratio",                           "Performance ratio",         "",       "",  "Annual (Year 1)",       "",                    "",                              "" },
                { SSC_OUTPUT,        SSC_NUMBER,     "capacity_factor",                             "Capacity factor based on nameplate DC capacity",           "%",      "",  "Annual (Year 1)", "", "", "" },
                { SSC_OUTPUT,        SSC_NUMBER,     "capacity_factor_ac",                          "Capacity factor based on total AC capacity",           "%",      "",  "Annual (Year 1)", "", "", "" },
                { SSC_OUTPUT,        SSC_NUMBER,     "kwh_per_kw",                                  "Energy yield", "kWh/kW", "",	"Annual (Year 1)", "", "", "" },

                //miscellaneous outputs
                { SSC_OUTPUT,        SSC_NUMBER,     "ts_shift_hours",                            "Sun position time offset",   "hours",  "",  "Miscellaneous", "",                       "",                          "" },
                { SSC_OUTPUT,        SSC_NUMBER,     "nameplate_dc_rating",                        "System nameplate DC rating", "kW",     "",  "Miscellaneous",       "*",                    "",                              "" },
                { SSC_OUTPUT,       SSC_NUMBER,      "system_capacity_ac",             "System nameplate AC rating", "kWac",     "",                                             "Miscellaneous", "",                        "",                          "" },



                // test outputs
                #ifdef SHADE_DB_OUTPUTS
                    // ShadeDB validation

                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray1_gpoa", "ShadeDB subarray 1 global poa input", "W/m2", "", "Time Series (Subarray 1)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray1_dpoa", "ShadeDB subarray 1 diffuse poa input", "W/m2", "", "Time Series (Subarray 1)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray1_pv_cell_temp", "ShadeDB subarray 1 pv cell temp input", "C", "", "Time Series (Subarray 1)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray1_mods_per_str", "ShadeDB subarray 1 modules per string input", "", "", "Time Series (Subarray 1)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray1_str_vmp_stc", "ShadeDB subarray 1 string Vmp at STC input", "V", "", "Time Series (Subarray 1)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray1_mppt_lo", "ShadeDB subarray 1 MPPT low input", "V", "", "Time Series (Subarray 1)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray1_mppt_hi", "ShadeDB subarray 1 MPPT high input", "V", "", "Time Series (Subarray 1)", "", "", "" },

                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray2_gpoa", "ShadeDB subarray 2 global poa input", "W/m2", "", "Time Series (Subarray 2)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray2_dpoa", "ShadeDB subarray 2 diffuse poa input", "W/m2", "", "Time Series (Subarray 2)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray2_pv_cell_temp", "ShadeDB subarray 2 pv cell temp input", "C", "", "Time Series (Subarray 2)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray2_mods_per_str", "ShadeDB subarray 2 modules per string input", "", "", "Time Series (Subarray 2)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray2_str_vmp_stc", "ShadeDB subarray 2 string Vmp at STC input", "V", "", "Time Series (Subarray 2)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray2_mppt_lo", "ShadeDB subarray 2 MPPT low input", "V", "", "Time Series (Subarray 2)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray2_mppt_hi", "ShadeDB subarray 2 MPPT high input", "V", "", "Time Series (Subarray 2)", "", "", "" },

                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray3_gpoa", "ShadeDB subarray 3 global poa input", "W/m2", "", "Time Series (Subarray 3)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray3_dpoa", "ShadeDB subarray 3 diffuse poa input", "W/m2", "", "Time Series (Subarray 3)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray3_pv_cell_temp", "ShadeDB subarray 3 pv cell temp input", "C", "", "Time Series (Subarray 3)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray3_mods_per_str", "ShadeDB subarray 3 modules per string input", "", "", "Time Series (Subarray 3)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray3_str_vmp_stc", "ShadeDB subarray 3 string Vmp at STC input", "V", "", "Time Series (Subarray 3)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray3_mppt_lo", "ShadeDB subarray 3 MPPT low input", "V", "", "Time Series (Subarray 3)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray3_mppt_hi", "ShadeDB subarray 3 MPPT high input", "V", "", "Time Series (Subarray 3)", "", "", "" },


                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray4_gpoa", "ShadeDB subarray 4 global poa input", "W/m2", "", "Time Series (Subarray 4)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray4_dpoa", "ShadeDB subarray 4 diffuse poa input", "W/m2", "", "Time Series (Subarray 4)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray4_pv_cell_temp", "ShadeDB subarray 4 pv cell temp input", "C", "", "Time Series (Subarray 4)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray4_mods_per_str", "ShadeDB subarray 4 modules per string input", "", "", "Time Series (Subarray 4)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray4_str_vmp_stc", "ShadeDB subarray 4 string Vmp at STC input", "V", "", "Time Series (Subarray 4)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray4_mppt_lo", "ShadeDB subarray 4 MPPT low input", "V", "", "Time Series (Subarray 4)", "", "", "" },
                    { SSC_OUTPUT, SSC_ARRAY, "shadedb_subarray4_mppt_hi", "ShadeDB subarray 4 MPPT high input", "V", "", "Time Series (Subarray 4)", "", "", "" },

                #endif

                    // a couple debugging outputs
                    /*
                    { SSC_OUTPUT,        SSC_ARRAY,      "p_nonlinear_dc_derate0",                      "SS1x dc derate",                                          "",    "",                      "pvsamv1",       "*",                    "",                              "" },
                    { SSC_OUTPUT,        SSC_ARRAY,      "p_nonlinear_derate_X",                        "SS1x X",                                          "",    "",                      "pvsamv1",       "*",                    "",                              "" },
                    { SSC_OUTPUT,        SSC_ARRAY,      "p_nonlinear_derate_S",                        "SS1x S",                                          "",    "",                      "pvsamv1",       "*",                    "",                              "" },
                    { SSC_OUTPUT,        SSC_ARRAY,      "p_nonlinear_shad1xf",                         "SS1x shade fraction",                                          "",    "",                      "pvsamv1",       "*",                    "",                              "" },
                    { SSC_OUTPUT,        SSC_ARRAY,      "p_nonlinear_Ee_ratio",                        "SS1x Ee ratio",                                          "",    "",                      "pvsamv1",       "*",                    "",                              "" },
                    { SSC_OUTPUT,        SSC_ARRAY,      "p_nonlinear_skyd1xf",                         "SS1x skydiff derate",                                          "",    "",                      "pvsamv1",       "*",                    "",                              "" },
                    { SSC_OUTPUT,        SSC_ARRAY,      "p_nonlinear_gndd1xf",                         "SS1x gnddiff derate",                                          "",    "",                      "pvsamv1",       "*",                    "",                              "" },
                    */



                var_info_invalid };

cm_pvsamv1::cm_pvsamv1()
{
    add_var_info(_cm_vtab_pvsamv1);
    add_var_info(vtab_adjustment_factors);
    add_var_info(vtab_dc_adjustment_factors);
    add_var_info(vtab_technology_outputs);
    add_var_info(vtab_battery_inputs);
    add_var_info(vtab_batt_adjustment_factors);
    add_var_info(vtab_forecast_price_signal);
    add_var_info(vtab_battery_outputs);
    add_var_info(vtab_resilience_outputs);
    add_var_info(vtab_utility_rate_common); // Required by battery
    add_var_info(vtab_grid_curtailment); // Required by battery
    add_var_info(vtab_hybrid_tech_om_outputs);
}


void cm_pvsamv1::exec()
{

    /// Underlying class which parses the compute module structure and sets up model inputs and outputs
    std::unique_ptr<PVIOManager> IOManager(new PVIOManager(this, "pvsamv1"));
    Simulation_IO* Simulation = IOManager->getSimulationIO();
    Irradiance_IO* Irradiance = IOManager->getIrradianceIO();
    std::vector<Subarray_IO*> Subarrays = IOManager->getSubarrays();
    PVSystem_IO* PVSystem = IOManager->getPVSystemIO();
    ShadeDB8_mpp* shadeDatabase = IOManager->getShadeDatabase();

    size_t nrec = Simulation->numberOfWeatherFileRecords;
    size_t nlifetime = Simulation->numberOfSteps;
    size_t nyears = Simulation->numberOfYears;
    double ts_hour = Simulation->dtHour; //simulation timestep in fraction of an hour (e.g. 15-min data would be 0.25)
    size_t step_per_hour = Simulation->stepsPerHour; //number of timesteps in one hour. hourly data will be 1, 15-min data will be 4, etc.
    bool system_use_lifetime_output = Simulation->useLifetimeOutput;
    bool save_full_lifetime_variables = Simulation->saveLifetimeVars;

    // Get Irradiance Inputs for now (eventually models can use these directly)
    weather_header hdr = Irradiance->weatherHeader;
    weather_data_provider* wdprov = Irradiance->weatherDataProvider.get();
    int radmode = Irradiance->radiationMode;
    double bifaciality = 0.0;
    double elev, pres, t_amb;

    // Get System or Subarray Inputs
    double aspect_ratio = Subarrays[0]->moduleAspectRatio;
    size_t num_subarrays = PVSystem->numberOfSubarrays;
    int mod_type = Subarrays[0]->Module->modulePowerModel;
    double ref_area_m2 = Subarrays[0]->Module->referenceArea;
    double module_watts_stc = Subarrays[0]->Module->moduleWattsSTC;
    SharedInverter* sharedInverter = PVSystem->m_sharedInverter.get();

    

    

    //overwrite tilt with latitude if flag is set- can't do this in PVIOManager because need latitude from weather file
    //also check here for tilt > 0 for tracking systems, since this is a very uncommon configuration but an easy mistake to make
    for (size_t nn = 0; nn < num_subarrays; nn++)
    {
        size_t user_tilt_angles_size;
        if (Subarrays[nn]->useCustomRotAngles == 1) {
            user_tilt_angles_size = Subarrays[nn]->customRotAngles.size();
            if (user_tilt_angles_size != nrec)
                throw exec_error("pvsamv1", "The custom tracker rotation angle array must be the size of nrecords per year");
            Subarrays[nn]->trackMode = irrad::SINGLE_AXIS;
            //Subarrays[nn]->tiltDegrees = 0; //reset to 0 to then be replaced in loop?
            Subarrays[nn]->backtrackingEnabled = false; //account for backtracking in user-specified angles [deg]

        }

        size_t custom_cell_temp_size;
        if (Subarrays[nn]->useCustomCellTemp == 1) {
            custom_cell_temp_size = Subarrays[nn]->customCellTempArray.size();
            if (custom_cell_temp_size != nrec)
                throw exec_error("pvsamv1", "The custom cell temperature array must be the size of nrecords per year");
        }

        if (Subarrays[nn]->tiltEqualLatitude)
            Subarrays[nn]->tiltDegrees = std::abs(Irradiance->weatherHeader.lat);

        if (Subarrays[nn]->trackMode == irrad::SINGLE_AXIS && Subarrays[nn]->tiltDegrees > 0)
            log(util::format("Subarray %d has one-axis tracking with a tilt angle of %f degrees. SAM can simulate one-axis tracking with non-zero tilt angles, but large one-axis tracking arrays typically have a tilt angle of zero. This message is a reminder in case you forgot to set the tilt angle to zero.", nn + 1, Subarrays[nn]->tiltDegrees), SSC_WARNING);
    }

    // check for snow model with non-annual simulations: because snow model coefficients need to know the timestep, and we don't know timestep if non-annual
    if (!Simulation->annualSimulation && PVSystem->enableSnowModel)
        log("For simulation period that is not continuous over one or more years, the snow model may over-estimate snow losses.", SSC_WARNING);

    // check: timeseries beam shading not allowed with a non-annual simulation
    if (!Simulation->annualSimulation)
    {
        for (size_t nn = 0; nn < num_subarrays; nn++)
        {
            if (is_assigned("subarray" + util::to_string(static_cast<int>(nn + 1)) + "_shading_timestep"))
                throw exec_error("pvsamv1", "Time series beam shading inputs cannot be used for a simulation period that is not continuous over one or more years.");
        }
    }

    double annual_snow_loss = 0;

    // SELF-SHADING MODULE INFORMATION
    double width = sqrt((ref_area_m2 / aspect_ratio));
    if (width <= 0)
        throw exec_error("pvsamv1", "Area of PV module must be > 0, but is instead " + util::to_string(width));
    for (size_t nn = 0; nn < num_subarrays; nn++)
    {
        Subarrays[nn]->selfShadingInputs.width = width;
        Subarrays[nn]->selfShadingInputs.length = width * aspect_ratio;
        Subarrays[nn]->selfShadingInputs.FF0 = Subarrays[nn]->Module->selfShadingFillFactor;
        Subarrays[nn]->selfShadingInputs.Vmp = Subarrays[nn]->Module->voltageMaxPower;
        double b = 0;
        if (Subarrays[nn]->selfShadingInputs.mod_orient == 0)
            b = Subarrays[nn]->selfShadingInputs.nmody * Subarrays[nn]->selfShadingInputs.length;
        else
            b = Subarrays[nn]->selfShadingInputs.nmody * Subarrays[nn]->selfShadingInputs.width;
        Subarrays[nn]->selfShadingInputs.row_space = b / Subarrays[nn]->groundCoverageRatio;
    }

    double nameplate_kw = 0;
    for (size_t nn = 0; nn < num_subarrays; nn++)
    {
        nameplate_kw += Subarrays[nn]->nModulesPerString * Subarrays[nn]->nStrings * module_watts_stc * util::watt_to_kilowatt;
    }

    // Warning workaround
    static bool is32BitLifetime = (__ARCHBITS__ == 32 && system_use_lifetime_output);
    if (is32BitLifetime)
        throw exec_error("pvsamv1", "Lifetime simulation of PV systems is only available in 64-bit versions of SAM.");

    // lifetime outputs
    std::vector<ssc_number_t> p_load_full; p_load_full.reserve(nlifetime);
    std::vector<ssc_number_t> p_crit_load_full; // Reserve within an en_batt call, below
    std::vector<ssc_number_t> p_load_forecast_full; // Reserve within an en_batt call, below

    //dc hourly adjustment factors
    int nyears_haf = nyears;
    if (!system_use_lifetime_output) nyears_haf = 1;
    adjustment_factors dc_haf(this->get_var_table(), "dc_adjust");
    if (!dc_haf.setup((int)nrec, nyears_haf))
        throw exec_error("pvsamv1", "failed to setup DC adjustment factors: " + dc_haf.error());

    // hourly adjustment factors
    adjustment_factors haf(this->get_var_table(), "adjust");
    if (!haf.setup((int)nrec, nyears_haf))
        throw exec_error("pvsamv1", "failed to setup AC adjustment factors: " + haf.error());

    // clipping losses for battery dispatch
    std::vector<ssc_number_t> p_invcliploss_full;
    p_invcliploss_full.reserve(nlifetime);

    // Multiple MPPT inverters not enabled with PVyield inverter model
    if (PVSystem->Inverter->nMpptInputs > 1 && PVSystem->Inverter->inverterType == INVERTER_PVYIELD)
        throw exec_error("pvsamv1", "The PVYield inverter model does not work with multiple MPPT inputs.");

    std::vector<ssc_number_t> p_pv_clipping_forecast;
    std::vector<ssc_number_t> p_pv_ac_forecast;
    std::vector<ssc_number_t> p_pv_ac_use;

    // electric load - lifetime load data?
    double cur_load = 0.0;
    size_t nload = 0;
    std::vector<ssc_number_t> p_load_in;
    std::vector<ssc_number_t> p_crit_load_in;
    std::vector<ssc_number_t> p_load_forecast_in;
    if (is_assigned("load"))
    {
        p_load_in = as_vector_ssc_number_t("load");
        nload = p_load_in.size();
        if (nload != nrec && nload != 8760)
            throw exec_error("pvsamv1", "Load must have either the same time step as the weather file, or 8760 time steps.");
    }
    if (is_assigned("crit_load"))
    {
        p_crit_load_in = as_vector_ssc_number_t("crit_load");
        size_t n_crit_load = p_crit_load_in.size();
        if (n_crit_load != nrec && n_crit_load != 8760)
            throw exec_error("pvsamv1", "Critical load crit_load must have either the same time step as the weather file, or 8760 time steps.");
        if (n_crit_load != nload)
            throw exec_error("pvsamv1", "Critical load crit_load must have same number of values as load.");
    }

    // resilience metrics for battery
    std::unique_ptr<resilience_runner> resilience = nullptr;

    // setup battery model
    std::shared_ptr<battstor> batt = nullptr;
    bool en_batt = as_boolean("en_batt");
    int batt_topology = 0;
    if (en_batt) {

        // Single timestep or non-annual simulations are not enabled with batteries
        if (!Simulation->annualSimulation)
            throw exec_error("pvsamv1", "The PV Battery configuration requires a simulation period that is continuous over one or more years.");

        batt = std::make_shared<battstor>(*m_vartab, en_batt, nrec, ts_hour);
        batt->setSharedInverter(sharedInverter);
        batt_topology = batt->batt_vars->batt_topology;

        if (is_assigned("batt_load_ac_forecast"))
        {
            p_load_forecast_in = as_vector_ssc_number_t("batt_load_ac_forecast");
            nload = p_load_forecast_in.size();
            if (nload == 1 || !batt->uses_forecast()) {
                // Length 1 is "empty" to UI lk
                p_load_forecast_in.clear();
            }
            else if (nload != nrec && nload != 8760) {
                throw exec_error("pvsamv1", "The electric load forecast profile must have either same number of time steps as the weather file, or 8760 time steps.");
            }
        }

        p_load_forecast_full.reserve(nlifetime);

        int batt_forecast_choice = as_integer("batt_dispatch_wf_forecast_choice");
        if (is_assigned("batt_pv_clipping_forecast")) {
            p_pv_clipping_forecast = as_vector_ssc_number_t("batt_pv_clipping_forecast");
            // Annual simulation is enforced above
            if (p_pv_clipping_forecast.size() < step_per_hour * 8760 && batt_forecast_choice == dispatch_t::WEATHER_FORECAST_CHOICE::WF_CUSTOM) {
                if (batt->uses_forecast()) {
                    throw exec_error("pvsamv1", "batt_pv_clipping_forecast forecast length is " + std::to_string(p_pv_clipping_forecast.size()) + " when custom weather file forecast is selected. Change batt_dispatch_wf_forecast_choice or provide a forecast of at least length " + std::to_string(step_per_hour * 8760));
                }
                else {
                    p_pv_clipping_forecast.clear();
                }
            }
        }
        if (is_assigned("batt_pv_ac_forecast")) {
            p_pv_ac_forecast = as_vector_ssc_number_t("batt_pv_ac_forecast");
            // Annual simulation is enforced above
            if (p_pv_ac_forecast.size() < step_per_hour * 8760 && batt_forecast_choice == dispatch_t::WEATHER_FORECAST_CHOICE::WF_CUSTOM) {
                if (batt->uses_forecast()) {
                    throw exec_error("pvsamv1", "batt_pv_ac_forecast forecast length is " + std::to_string(p_pv_ac_forecast.size()) + " when custom weather file forecast is selected. Change batt_dispatch_wf_forecast_choice or provide a forecast of at least length " + std::to_string(step_per_hour * 8760));
                }
                else {
                    p_pv_ac_forecast.clear();
                }
            }
        }

        // Multiple MPPT inverters not enabled with DC-connected batteries
        if (PVSystem->Inverter->nMpptInputs > 1 && en_batt && batt_topology == ChargeController::DC_CONNECTED)
            throw exec_error("pvsamv1", "DC-connected batteries do not work with multiple MPPT input inverters.");

        bool crit_load_specified = !p_crit_load_in.empty() && *std::max_element(p_crit_load_in.begin(), p_crit_load_in.end()) > 0;

        bool run_resilience = as_boolean("run_resiliency_calcs");
        if (run_resilience) {
            if (crit_load_specified) {
                resilience = std::unique_ptr<resilience_runner>(new resilience_runner(batt));
                auto logs = resilience->get_logs();
                if (!logs.empty()) {
                    log(logs[0], SSC_WARNING);
                }
            }
            else {
                throw exec_error("pvsamv1", "If run_resiliency_calcs is 1, crit_load must have length > 0 and values > 0");
            }
        }
        if (!crit_load_specified && batt->analyze_outage) {
            throw exec_error("battery", "If grid_outage is specified in any time step, crit_load must have length > 0 and values > 0");
        }
    }

    // for reporting status updates
    float percent_baseline = 0.;
    float percent_complete = 0.;
    size_t ireport = 0;
    size_t ireplast = 0;
    size_t insteps = 3 * nyears * nrec; //there are 3 loops through time (DC, AC, post AC)
    size_t irepfreq = insteps / (50 * nyears); //report status updates 50 times per year

    // variables used to calculate loss diagram
    double annual_energy = 0, annual_ac_gross = 0, annual_ac_pre_avail = 0, dc_gross[4] = { 0, 0, 0, 0 }, annualMpptVoltageClipping = 0, annual_dc_adjust_loss = 0, annual_dc_lifetime_loss = 0, annual_ac_lifetime_loss = 0, annual_ac_battery_loss = 0, annual_xfmr_nll = 0, annual_xfmr_ll = 0, annual_xfmr_loss = 0, annual_ac_wiring_loss = 0, annual_transmission_loss = 0;

    /* *********************************************************************************************
    PV DC calculation
    *********************************************************************************************** */
    std::vector<double> dcPowerNetPerMppt_kW; //Vector of Net DC power in kW for each MPPT input on the system for THIS TIMESTEP ONLY
    std::vector<double> dcPowerNetPerSubarray; //Net DC power in W for each subarray for THIS TIMESTEP ONLY
    std::vector<double> dcPowerNetPerSubarrayCS;
    std::vector<double> dcVoltagePerMppt; //Voltage in V at each MPPT input on the system for THIS TIMESTEP ONLY
    std::vector<std::vector<double>> dcStringVoltage; // Voltage of string for each subarray
    double dcPowerNetTotalSystem = 0; //Net DC power in W for the entire system (sum of all subarrays)

    scalefactors scale_calculator(m_vartab);
    // compute load (electric demand) annual escalation multipliers
    std::vector<ssc_number_t> load_scale = scale_calculator.get_factors("load_escalation");

    if (Simulation->annualSimulation) {
        double interpolation_factor = 1.0;
        single_year_to_lifetime_interpolated<ssc_number_t>(
            (bool)as_integer("system_use_lifetime_output"),
            nyears,
            nlifetime,
            p_load_in,
            load_scale,
            interpolation_factor,
            p_load_full,
            nrec,
            ts_hour);
    }
    else {
        p_load_full = p_load_in;
    }

    if (en_batt) {
        if (p_load_forecast_in.size() > 0) {
            std::vector<ssc_number_t> load_forecast_scale = scale_calculator.get_factors("batt_load_ac_forecast_escalation");
            double interpolation_factor = 1.0;
            single_year_to_lifetime_interpolated<ssc_number_t>(
                (bool)as_integer("system_use_lifetime_output"),
                nyears,
                nlifetime,
                p_load_forecast_in,
                load_forecast_scale,
                interpolation_factor,
                p_load_forecast_full,
                nrec,
                ts_hour);
        }
        else {
            p_load_forecast_full = p_load_full;
        }

        p_crit_load_full.reserve(nlifetime);
        std::vector<ssc_number_t> crit_load_scale = scale_calculator.get_factors("crit_load_escalation");
        double interpolation_factor = 1.0;
        // Will pad with zeroes if crit load is not defined - which is fine by the dispatch and powerflow code
        single_year_to_lifetime_interpolated<ssc_number_t>(
            (bool)as_integer("system_use_lifetime_output"),
            nyears,
            nlifetime,
            p_crit_load_in,
            crit_load_scale,
            interpolation_factor,
            p_crit_load_full,
            nrec,
            ts_hour);

    }

    for (size_t mpptInput = 0; mpptInput < PVSystem->Inverter->nMpptInputs; mpptInput++)
    {
        dcPowerNetPerMppt_kW.push_back(0);
        dcVoltagePerMppt.push_back(0);
        PVSystem->p_dcPowerNetPerMppt[mpptInput][0] = 0;
    }
    for (size_t nn = 0; nn < PVSystem->numberOfSubarrays; nn++) {
        dcPowerNetPerSubarray.push_back(0);
        dcPowerNetPerSubarrayCS.push_back(0);
        std::vector<double> tmp;
        dcStringVoltage.push_back(tmp);
    }

    std::vector<std::shared_ptr<irrad>> irrads;
    for (size_t nn = 0; nn < num_subarrays; nn++) {
        auto irr = std::shared_ptr<irrad>(new irrad(Irradiance->weatherHeader, Irradiance->skyModel, Irradiance->radiationMode, Subarrays[nn]->trackMode,
            Irradiance->instantaneous, Subarrays[nn]->backtrackingEnabled, false,
            Irradiance->dtHour, Subarrays[nn]->tiltDegrees, Subarrays[nn]->azimuthDegrees, Subarrays[nn]->trackerRotationLimitDegrees, 0.0, 
            Subarrays[nn]->groundCoverageRatio, Subarrays[nn]->slopeTilt, Subarrays[nn]->slopeAzm,
            Subarrays[nn]->poa.poaAll.get(), (as_boolean("enable_subhourly_clipping") || as_boolean("enable_subinterval_distribution"))));
        
        if (nyears > 1)
            irr->setup_solarpos_outputs_for_lifetime(nrec);

        irrads.push_back(irr);
    }

    //idx is the LIFETIME index in the (possibly subhourly) year of weather data, or the normal index in a non-annual array (lifetime is 1)
    size_t idx = 0;
    //for normal annual simulations, this works as expected. for non-annual weather data inputs, nyears is 1,
    //so iyear will always be 0, meaning that timeseries outputs will be output for the entire length of nrec
    for (size_t iyear = 0; iyear < nyears; iyear++)
    {
        for (size_t inrec = 0; inrec < nrec; inrec++)
        {
            idx = inrec + iyear * nrec;
            if (!wdprov->read(&Irradiance->weatherRecord))
                throw exec_error("pvsamv1", "Could not read data line " + util::to_string((int)(inrec + 1)) + " in weather file.");

            weather_record wf = Irradiance->weatherRecord;
            size_t hour = wf.hour; //this is the current timestamp hour from 0-24 from the weather file
            size_t hour_of_year = util::hour_of_year(wf.month, wf.day, wf.hour); //this is the index of the hour in the year (0-8759) given the weather file date & timestamp

            // report progress updates to the caller
            ireport++;
            if (ireport - ireplast > irepfreq)
            {
                percent_complete = percent_baseline + 100.0f * (float)(idx) / (float)(insteps);
                if (!update("", percent_complete))
                    throw exec_error("pvsamv1", "Simulation stopped at hour " + util::to_string(hour_of_year + 1.0) + " in year " + util::to_string((int)iyear + 1) + "in DC loop.");
                ireplast = ireport;
            }

            // Reset dcPower calculation for new timestep
            dcPowerNetTotalSystem = 0;

            //update POA data structure indicies if radmode is POA model is enabled
            if (radmode == irrad::POA_R || radmode == irrad::POA_P) {
                for (size_t nn = 0; nn < num_subarrays; nn++) {
                    if (!Subarrays[nn]->enable) continue;

                    Subarrays[nn]->poa.poaAll->tDew = wf.tdew;
                    Subarrays[nn]->poa.poaAll->i = inrec;
                    if (wf.hour == 0 && (inrec % step_per_hour == 0)) {
                        Subarrays[nn]->poa.poaAll->dayStart = inrec;
                        Subarrays[nn]->poa.poaAll->doy += 1;
                    }
                }
            }

            double solazi = 0, solzen = 0, solalt = 0;
            int sunup = 0;

            // accumulators for radiation power (W) over this
            // timestep from each subarray
            double ts_accum_poa_front_nom = 0.0;
            double ts_accum_poa_front_beam_nom = 0.0;
            double ts_accum_poa_front_shaded = 0.0;
            double ts_accum_poa_front_shaded_soiled = 0.0;
            double ts_accum_poa_front_total = 0.0;
            double ts_accum_poa_front_beam_eff = 0.0;
            double ts_accum_poa_total_eff = 0.0;
            double ts_accum_poa_rear = 0.0;
            double ts_accum_poa_rear_after_losses = 0.0;
            double ts_accum_ground_incident = 0.0;
            double ts_accum_ground_absorbed = 0.0;
            double ts_accum_poa_rear_ground_reflected = 0.0;
            double ts_accum_poa_rear_row_reflections = 0.0;
            double ts_accum_poa_rear_direct_diffuse = 0.0;
            double ts_accum_poa_rear_self_shaded = 0.0;
            double ts_accum_poa_rack_shaded = 0.0;
            double ts_accum_poa_rear_soiled = 0.0;
            double ts_accum_electrical_mismatch = 0.0;

            // calculate incident irradiance on each subarray
            std::vector<double> ipoa_rear, ipoa_rear_after_losses, ipoa_rear_cs, ipoa_rear_after_losses_cs, ipoa_front, ipoa;
            std::vector<std::vector<double>> ipoa_rear_spatial, ipoa_rear_spatial_after_losses, ignd_rear;
            double alb = 0.;
            std::vector<double> alb_spatial;

            for (size_t nn = 0; nn < num_subarrays; nn++)
            {
                ipoa_rear.push_back(0);
                ipoa_rear_after_losses.push_back(0);
                ipoa_rear_cs.push_back(0);
                ipoa_rear_after_losses_cs.push_back(0);
                ipoa_front.push_back(0);
                ipoa.push_back(0);
                ipoa_rear_spatial.push_back(std::vector<double>());
                ipoa_rear_spatial_after_losses.push_back(std::vector<double>());
                ignd_rear.push_back(std::vector<double>());

                if (!Subarrays[nn]->enable
                    || Subarrays[nn]->nStrings < 1)
                    continue; // skip disabled subarrays

                double custom_rot = (Subarrays[nn]->useCustomRotAngles) ? Subarrays[nn]->customRotAngles[inrec] : 0.0;

                std::shared_ptr<irrad> irr = irrads[nn];
                irr->set_from_weather_record(Irradiance->weatherRecord, Irradiance->weatherHeader, Subarrays[nn]->trackMode, Subarrays[nn]->monthlyTiltDegrees, 
                    Irradiance->useWeatherFileAlbedo, Irradiance->userSpecifiedMonthlyAlbedo,
                    Subarrays[nn]->poa.poaAll.get(), Irradiance->useSpatialAlbedos, &Irradiance->userSpecifiedMonthlySpatialAlbedos, 
                    Subarrays[nn]->useCustomRotAngles, custom_rot);

                int code = irr->calc();

                if (code < 0) //jmf updated 11/30/18 so that negative numbers are errors, positive numbers are warnings, 0 is everything correct. implemented in patch for POA model only, will be added to develop for other irrad models as well
                    throw exec_error("pvsamv1",
                        util::format("Failed to calculate POA irradiance %d (code: %d) [y:%d m:%d d:%d h:%d minute:%lg]",
                            nn + 1, code, wf.year, wf.month, wf.day, wf.hour, wf.minute));
                
                if (code == 40)
                    log(util::format("POA decomposition model calculated negative direct normal irradiance at time [y:%d m:%d d:%d h:%d minute:%lg], set to zero.",
                        wf.year, wf.month, wf.day, wf.hour, wf.minute), SSC_NOTICE, (float)idx);
                else if (code == 41)
                    log(util::format("POA decomposition model calculated negative diffuse horizontal irradiance at time [y:%d m:%d d:%d h:%d minute:%lg], set to zero.",
                        wf.year, wf.month, wf.day, wf.hour, wf.minute), SSC_NOTICE, (float)idx);
                else if (code == 42)
                    log(util::format("POA decomposition model calculated negative global horizontal irradiance at time [y:%d m:%d d:%d h:%d minute:%lg], set to zero.",
                        wf.year, wf.month, wf.day, wf.hour, wf.minute), SSC_NOTICE, (float)idx);

                // p_irrad_calc is only weather file records long...
                if (iyear == 0 || save_full_lifetime_variables == 1)
                {
                    if (radmode == irrad::POA_R || radmode == irrad::POA_P) {
                        double gh_temp, df_temp, dn_temp;
                        gh_temp = df_temp = dn_temp = 0;
                        irr->get_irrad(&gh_temp, &dn_temp, &df_temp);
                        Irradiance->p_IrradianceCalculated[1][idx] = (ssc_number_t)df_temp;
                        Irradiance->p_IrradianceCalculated[2][idx] = (ssc_number_t)dn_temp;
                    }
                }
                // beam, skydiff, and grounddiff IN THE PLANE OF ARRAY (W/m2)
                double ibeam, iskydiff, ignddiff;
                double ibeam_csky, iskydiff_csky, ignddiff_csky;
                double ghi_cs, dni_cs, dhi_cs;
                double aoi, stilt, sazi, rot, btd;

                irr->get_clearsky_irrad(&ghi_cs, &dni_cs, &dhi_cs);

                // Ensure that the usePOAFromWF flag is false unless a reference cell has been used.
                //  This will later get forced to false if any shading has been applied (in any scenario)
                //  also this will also be forced to false if using the cec mcsp thermal model OR if using the spe module model with a diffuse util. factor < 1.0
                Subarrays[nn]->poa.usePOAFromWF = false;
                if (radmode == irrad::POA_R) {
                    ipoa[nn] = wf.poa;
                    Subarrays[nn]->poa.usePOAFromWF = true;
                }
                else if (radmode == irrad::POA_P) {
                    ipoa[nn] = wf.poa;
                }

                if (Subarrays[nn]->Module->simpleEfficiencyForceNoPOA && (radmode == irrad::POA_R || radmode == irrad::POA_P)) {  // only will be true if using a poa model AND spe module model AND spe_fp is < 1
                    Subarrays[nn]->poa.usePOAFromWF = false;
                    if (idx == 0)
                        log("POA decomposition model calculating POA diffuse irradiance for single point efficiency module model with module diffuse utilization factor.", SSC_WARNING);
                }

                if (Subarrays[nn]->Module->mountingSpecificCellTemperatureForceNoPOA && (radmode == irrad::POA_R || radmode == irrad::POA_P)) {
                    Subarrays[nn]->poa.usePOAFromWF = false;
                    if (idx == 0)
                        log("POA decomposition model calculating POA beam irradiance for heat transfer method cell temperature model.", SSC_WARNING);
                }


                // Get Incident angles and irradiances
                irr->get_sun(&solazi, &solzen, &solalt, 0, 0, 0, &sunup, 0, 0, 0);
                irr->get_angles(&aoi, &stilt, &sazi, &rot, &btd);
                irr->get_poa(&ibeam, &iskydiff, &ignddiff, 0, 0, 0);
                irr->get_poa_clearsky(&ibeam_csky, &iskydiff_csky, &ignddiff_csky, 0, 0, 0);
                irr->get_optional(&elev, &pres, &t_amb);
                alb = irr->getAlbedo();
                alb_spatial = irr->getAlbedoSpatial();


                

                if (iyear == 0 || save_full_lifetime_variables == 1)
                    Irradiance->p_sunPositionTime[idx] = (ssc_number_t)irr->get_sunpos_calc_hour();

                // save weather file beam, diffuse, and global for output and for use later in pvsamv1- year 1 only
                /*jmf 2016: these calculations are currently redundant with calculations in irrad.calc() because ibeam and idiff in that function are DNI and DHI, **NOT** in the plane of array
                we'll have to fix this redundancy in the pvsamv1 rewrite. it will require allowing irradproc to report the errors below
                and deciding what to do if the weather file DOES contain the third component but it's not being used in the calculations.*/
                if (iyear == 0 || save_full_lifetime_variables == 1)
                {
                    // Apply all irradiance component data from weather file (if it exists)
                    Irradiance->p_weatherFilePOA[0][idx] = (ssc_number_t)wf.poa;
                    Irradiance->p_weatherFileDNI[idx] = (ssc_number_t)wf.dn;
                    Irradiance->p_weatherFileGHI[idx] = (ssc_number_t)(wf.gh);
                    Irradiance->p_weatherFileDHI[idx] = (ssc_number_t)(wf.df);

                    // calculate beam if global & diffuse are selected as inputs
                    if (radmode == irrad::GH_DF)
                    {
                        Irradiance->p_IrradianceCalculated[2][idx] = (ssc_number_t)((wf.gh - wf.df) / cos(solzen * 3.1415926 / 180));
                        if (Irradiance->p_IrradianceCalculated[2][idx] < -1)
                        {
                            log(util::format("Calculated negative beam irradiance of %lg W/m2 at time [y:%d m:%d d:%d h:%d, minute:%lg], set to zero.",
                                Irradiance->p_IrradianceCalculated[2][idx], wf.year, wf.month, wf.day, wf.hour, wf.minute), SSC_NOTICE, (float)idx);
                            Irradiance->p_IrradianceCalculated[2][idx] = 0;
                        }
                    }

                    // calculate global if beam & diffuse are selected as inputs
                    if (radmode == irrad::DN_DF)
                    {
                        Irradiance->p_IrradianceCalculated[0][idx] = (ssc_number_t)(wf.df + wf.dn * cos(solzen * 3.1415926 / 180));
                        if (Irradiance->p_IrradianceCalculated[0][idx] < -1)
                        {
                            log(util::format("Calculated negative global horizontal irradiance of %lg W/m2 at time [y:%d m:%d d:%d h:%d minute:%lg], set to zero.",
                                Irradiance->p_IrradianceCalculated[0][idx], wf.year, wf.month, wf.day, wf.hour, wf.minute), SSC_NOTICE, (float)idx);
                            Irradiance->p_IrradianceCalculated[0][idx] = 0;
                        }
                    }

                    // calculate diffuse if total & beam are selected as inputs
                    if (radmode == irrad::DN_GH)
                    {
                        Irradiance->p_IrradianceCalculated[1][idx] = (ssc_number_t)(wf.gh - wf.dn * cos(solzen * 3.1415926 / 180));
                        if (Irradiance->p_IrradianceCalculated[1][idx] < -1)
                        {
                            log(util::format("Calculated negative diffuse horizontal irradiance of %lg W/m2 at time [y:%d m:%d d:%d h:%d minute:%lg], set to zero.",
                                Irradiance->p_IrradianceCalculated[1][idx], wf.year, wf.month, wf.day, wf.hour, wf.minute), SSC_NOTICE, (float)idx);
                            Irradiance->p_IrradianceCalculated[1][idx] = 0;
                        }
                    }
                }

                // record sub-array plane of array output before computing shading and soiling
                if (iyear == 0 || save_full_lifetime_variables == 1)
                {
                    if (radmode != irrad::POA_R)
                        PVSystem->p_poaNominalFront[nn][idx] = (ssc_number_t)((ibeam + iskydiff + ignddiff));
                    else
                        PVSystem->p_poaNominalFront[nn][idx] = (ssc_number_t)((ipoa[nn]));
                }


                // record sub-array contribution to total POA power for this time step  (W)
                if (radmode != irrad::POA_R)
                    ts_accum_poa_front_nom += (ibeam + iskydiff + ignddiff) * ref_area_m2 * Subarrays[nn]->nModulesPerString * Subarrays[nn]->nStrings;
                else
                    ts_accum_poa_front_nom += (ipoa[nn]) * ref_area_m2 * Subarrays[nn]->nModulesPerString * Subarrays[nn]->nStrings;

                // record sub-array contribution to total POA beam power for this time step (W)
                ts_accum_poa_front_beam_nom += ibeam * ref_area_m2 * Subarrays[nn]->nModulesPerString * Subarrays[nn]->nStrings;

                // for non-linear shading from shading database
                if (Subarrays[nn]->shadeCalculator.use_shade_db())
                {
                    double shadedb_gpoa = ibeam + iskydiff + ignddiff;
                    double shadedb_dpoa = iskydiff + ignddiff;

                    // update cell temperature - unshaded value per Sara 1/25/16
                    double tcell = wf.tdry;
                    if (sunup > 0)
                    {
                        // calculate cell temperature using selected temperature model
                        pvinput_t in(ibeam, iskydiff, ignddiff, 0, ipoa[nn],
                            wf.tdry, wf.tdew, wf.wspd, wf.wdir, wf.pres,
                            solzen, aoi, elev,
                            stilt, sazi,
                            ((double)wf.hour) + wf.minute / 60.0,
                            radmode, Subarrays[nn]->poa.usePOAFromWF);
                        // voltage set to -1 for max power
                        if (Subarrays[nn]->useCustomCellTemp == 1)
                            tcell = Subarrays[nn]->customCellTempArray[inrec];
                        else
                            (*Subarrays[nn]->Module->cellTempModel)(in, *Subarrays[nn]->Module->moduleModel, -1.0, tcell);
                    }
                    double shadedb_str_vmp_stc = Subarrays[nn]->nModulesPerString * Subarrays[nn]->Module->voltageMaxPower;
                    double shadedb_mppt_lo = PVSystem->Inverter->mpptLowVoltage;
                    double shadedb_mppt_hi = PVSystem->Inverter->mpptHiVoltage;

                    // shading database if necessary
                    if (!Subarrays[nn]->shadeCalculator.fbeam_shade_db(shadeDatabase, hour_of_year, wf.minute, solalt, solazi, shadedb_gpoa, shadedb_dpoa, tcell, Subarrays[nn]->nModulesPerString, shadedb_str_vmp_stc, shadedb_mppt_lo, shadedb_mppt_hi))
                    {
                        throw exec_error("pvsamv1", util::format("Error calculating shading factor for Subarray %d.", nn));
                    }
                    if (iyear == 0 || save_full_lifetime_variables == 1)
                    {
#ifdef SHADE_DB_OUTPUTS
                        p_shadedb_gpoa[nn][idx] = (ssc_number_t)shadedb_gpoa;
                        p_shadedb_dpoa[nn][idx] = (ssc_number_t)shadedb_dpoa;
                        p_shadedb_pv_cell_temp[nn][idx] = (ssc_number_t)tcell;
                        p_shadedb_mods_per_str[nn][idx] = (ssc_number_t)Subarrays[nn]->nModulesPerString;
                        p_shadedb_str_vmp_stc[nn][idx] = (ssc_number_t)shadedb_str_vmp_stc;
                        p_shadedb_mppt_lo[nn][idx] = (ssc_number_t)shadedb_mppt_lo;
                        p_shadedb_mppt_hi[nn][idx] = (ssc_number_t)shadedb_mppt_hi;
                        log("shade db hour " + util::to_string((int)hour_of_year) + "\n" + shadeCalculator->get_warning());
#endif
                        // fraction shaded for comparison
                        PVSystem->p_shadeDBShadeFraction[nn][idx] = (ssc_number_t)(Subarrays[nn]->shadeCalculator.dc_shade_factor());
                    }
                }
                else
                {
                    if (!Subarrays[nn]->shadeCalculator.fbeam(hour_of_year, wf.minute, solalt, solazi))
                    {
                        throw exec_error("pvsamv1", util::format("Error calculating shading factor for Subarray %d at index %d.", nn, (float)idx));
                    }
                }

                // apply hourly shading factors to beam (if none enabled, factors are 1.0)
                // shj 3/21/16 - update to handle negative shading loss
                if (Subarrays[nn]->shadeCalculator.beam_shade_factor() != 1.0) {
                    //							if (sa[nn].shad.beam_shade_factor() < 1.0){
                    // Sara 1/25/16 - shading database derate applied to dc only
                    // shading loss applied to beam if not from shading database
                    ibeam *= Subarrays[nn]->shadeCalculator.beam_shade_factor();
                    ibeam_csky *= Subarrays[nn]->shadeCalculator.beam_shade_factor();
                    if (radmode == irrad::POA_R || radmode == irrad::POA_P) {
                        Subarrays[nn]->poa.usePOAFromWF = false;
                        if (Subarrays[nn]->poa.poaShadWarningCount == 0) {
                            log(util::format("POA irradiance as input with the beam shading losses at time [y:%d m:%d d:%d h:%d minute:%lg]: Using POA decomposition model to calculate incident beam irradiance.",
                                wf.year, wf.month, wf.day, wf.hour, wf.minute), SSC_WARNING, (float)idx);
                        }
                        else {
                            log(util::format("POA irradiance as input with the beam shading losses at time [y:%d m:%d d:%d h:%d minute:%lg]: Using POA decomposition model to calculate incident beam irradiance.",
                                wf.year, wf.month, wf.day, wf.hour, wf.minute), SSC_NOTICE, (float)idx);
                        }
                        Subarrays[nn]->poa.poaShadWarningCount++;
                    }
                }

                // apply sky diffuse shading factor (specified as constant, nominally 1.0 if disabled in UI)
                if (Subarrays[nn]->shadeCalculator.fdiff() < 1.0) {
                    iskydiff *= Subarrays[nn]->shadeCalculator.fdiff();
                    if (radmode == irrad::POA_R || radmode == irrad::POA_P) {
                        if (idx == 0)
                            log("POA irradiance as input with the diffuse shading losses: Using POA decomposition model to calculate incident diffuse irradiance.", SSC_WARNING);
                        Subarrays[nn]->poa.usePOAFromWF = false;
                    }
                }

                double beam_shading_factor = Subarrays[nn]->shadeCalculator.beam_shade_factor();

                //self-shading calculations
                if (((Subarrays[nn]->trackMode == 0 || Subarrays[nn]->trackMode == 4) && (Subarrays[nn]->shadeMode == 1 || Subarrays[nn]->shadeMode == 2)) //fixed tilt or timeseries tilt, self-shading (linear or non-linear) OR
                    || (Subarrays[nn]->trackMode == 1 && (Subarrays[nn]->shadeMode == 1 || Subarrays[nn]->shadeMode == 2))) //one-axis tracking (both backtracking and true tracking), self-shading (linear or non-linear)
                {

                    if (radmode == irrad::POA_R || radmode == irrad::POA_P) {
                        if (idx == 0)
                            log("POA irradiance as input with self shading: Using POA decomposition model to calculate incident beam irradiance.", SSC_WARNING);
                        Subarrays[nn]->poa.usePOAFromWF = false;
                    }

                    // info to be passed to self-shading function
                    bool trackbool = (Subarrays[nn]->trackMode == 1);	// 0 for fixed tilt and timeseries tilt, 1 for one-axis
                    bool linear = (Subarrays[nn]->shadeMode == 2); //0 for full self-shading, 1 for linear self-shading

                    //geometric fraction of the array that is shaded for one-axis trackers.
                    //USES A DIFFERENT FUNCTION THAN THE SELF-SHADING BECAUSE SS IS MEANT FOR FIXED ONLY. shadeFraction1x IS FOR TRUE-TRACKING ONE-AXIS TRACKERS ONLY.
                    //used in the non-linear self-shading calculator for one-axis tracking only
                    double shad1xf = 0.0;
                    if (trackbool && ((Subarrays[nn]->backtrackingEnabled == false) || (Subarrays[nn]->slopeTilt != 0 || Subarrays[nn]->slopeAzm != 0)))
                        shad1xf = shadeFraction1x(solazi, solzen, Subarrays[nn]->tiltDegrees, Subarrays[nn]->azimuthDegrees, Subarrays[nn]->groundCoverageRatio, rot, Subarrays[nn]->slopeTilt, Subarrays[nn]->slopeAzm);

                    //execute self-shading calculations
                    ssc_number_t beam_to_use; //some self-shading calculations require DNI, NOT ibeam (beam in POA). Need to know whether to use DNI from wf or calculated, depending on radmode
                    ssc_number_t dhi_to_use; //some self-shading calculations require DHI, NOT iskydiff (sky diff in POA). Need to know whether to use DHI from wf or calculated, depending on radmode
                    if (radmode == irrad::DN_DF || radmode == irrad::DN_GH) beam_to_use = (ssc_number_t)wf.dn;
                    else beam_to_use = Irradiance->p_IrradianceCalculated[2][hour * step_per_hour]; // top of hour in first year
                    if (radmode == irrad::DN_DF || radmode == irrad::GH_DF) dhi_to_use = (ssc_number_t)wf.df;
                    else dhi_to_use = Irradiance->p_IrradianceCalculated[1][hour * step_per_hour]; // top of hour in first year

                    if (ss_exec(Subarrays[nn]->selfShadingInputs,
                        stilt, sazi, solzen, solazi, beam_to_use, dhi_to_use, ibeam, iskydiff, ignddiff, alb, trackbool, linear, shad1xf,
                        Subarrays[nn]->selfShadingSkyDiffTable,
                        Subarrays[nn]->selfShadingOutputs))
                    {

                        if (linear && trackbool) //one-axis linear
                        {
                            ibeam *= (1 - shad1xf); //derate beam irradiance linearly by the geometric shading fraction calculated above per Chris Deline 2/10/16
                            ibeam_csky *= (1 - shad1xf);
                            beam_shading_factor *= (1 - shad1xf);
                            // Sky diffuse and ground-reflected diffuse are derated according to C. Deline's algorithm
                            iskydiff *= Subarrays[nn]->selfShadingOutputs.m_diffuse_derate;
                            ignddiff *= Subarrays[nn]->selfShadingOutputs.m_reflected_derate;
                            iskydiff_csky *= Subarrays[nn]->selfShadingOutputs.m_diffuse_derate;
                            ignddiff_csky *= Subarrays[nn]->selfShadingOutputs.m_reflected_derate;

                            if (iyear == 0 || save_full_lifetime_variables == 1)
                            {
                                PVSystem->p_derateSelfShading[nn][idx] = (ssc_number_t)1;
                                PVSystem->p_derateLinear[nn][idx] = (ssc_number_t)(1 - shad1xf);
                                PVSystem->p_derateSelfShadingDiffuse[nn][idx] = (ssc_number_t)Subarrays[nn]->selfShadingOutputs.m_diffuse_derate;
                                PVSystem->p_derateSelfShadingReflected[nn][idx] = (ssc_number_t)Subarrays[nn]->selfShadingOutputs.m_reflected_derate;
                            }
                        }

                        else if (linear) //fixed tilt linear
                        {
                            ibeam *= (1 - Subarrays[nn]->selfShadingOutputs.m_shade_frac_fixed);
                            ibeam_csky *= (1 - shad1xf);
                            beam_shading_factor *= (1 - Subarrays[nn]->selfShadingOutputs.m_shade_frac_fixed);
                            iskydiff *= Subarrays[nn]->selfShadingOutputs.m_diffuse_derate;
                            ignddiff *= Subarrays[nn]->selfShadingOutputs.m_reflected_derate;
                            iskydiff_csky *= Subarrays[nn]->selfShadingOutputs.m_diffuse_derate;
                            ignddiff_csky *= Subarrays[nn]->selfShadingOutputs.m_reflected_derate;

                            if (iyear == 0 || save_full_lifetime_variables == 1)
                            {
                                PVSystem->p_derateSelfShading[nn][idx] = (ssc_number_t)1;
                                PVSystem->p_derateLinear[nn][idx] = (ssc_number_t)(1 - Subarrays[nn]->selfShadingOutputs.m_shade_frac_fixed);
                                PVSystem->p_derateSelfShadingDiffuse[nn][idx] = (ssc_number_t)Subarrays[nn]->selfShadingOutputs.m_diffuse_derate;
                                PVSystem->p_derateSelfShadingReflected[nn][idx] = (ssc_number_t)Subarrays[nn]->selfShadingOutputs.m_reflected_derate;
                            }
                        }

                        else if (trackbool && (Subarrays[nn]->backtrackingEnabled == true)) //non-linear backtracking one-axis
                        {
                            iskydiff *= Subarrays[nn]->selfShadingOutputs.m_diffuse_derate;
                            ignddiff *= Subarrays[nn]->selfShadingOutputs.m_reflected_derate;
                            iskydiff_csky *= Subarrays[nn]->selfShadingOutputs.m_diffuse_derate;
                            ignddiff_csky *= Subarrays[nn]->selfShadingOutputs.m_reflected_derate;

                            if (iyear == 0 || save_full_lifetime_variables == 1)
                            {
                                PVSystem->p_derateSelfShading[nn][idx] = (ssc_number_t)1;
                                PVSystem->p_derateLinear[nn][idx] = (ssc_number_t)1;
                                PVSystem->p_derateSelfShadingDiffuse[nn][idx] = (ssc_number_t)Subarrays[nn]->selfShadingOutputs.m_diffuse_derate;
                                PVSystem->p_derateSelfShadingReflected[nn][idx] = (ssc_number_t)Subarrays[nn]->selfShadingOutputs.m_reflected_derate;
                            }
                        }

                        else //non-linear: fixed tilt AND one-axis true-tracking
                        {
                            // Beam is not derated- all beam derate effects (linear and non-linear) are taken into account in the nonlinear_dc_shading_derate
                            Subarrays[nn]->poa.nonlinearDCShadingDerate = Subarrays[nn]->selfShadingOutputs.m_dc_derate;

                            iskydiff *= Subarrays[nn]->selfShadingOutputs.m_diffuse_derate;
                            ignddiff *= Subarrays[nn]->selfShadingOutputs.m_reflected_derate;
                            iskydiff_csky *= Subarrays[nn]->selfShadingOutputs.m_diffuse_derate;
                            ignddiff_csky *= Subarrays[nn]->selfShadingOutputs.m_reflected_derate;

                            if (iyear == 0 || save_full_lifetime_variables == 1)
                            {
                                PVSystem->p_derateSelfShadingDiffuse[nn][idx] = (ssc_number_t)Subarrays[nn]->selfShadingOutputs.m_diffuse_derate;
                                PVSystem->p_derateSelfShadingReflected[nn][idx] = (ssc_number_t)Subarrays[nn]->selfShadingOutputs.m_reflected_derate;
                                PVSystem->p_derateSelfShading[nn][idx] = (ssc_number_t)Subarrays[nn]->selfShadingOutputs.m_dc_derate;
                                PVSystem->p_derateLinear[nn][idx] = (ssc_number_t)1;
                            }
                        }
                    }
                    else
                        throw exec_error("pvsamv1", util::format("Self-shading calculation failed at %d", (int)idx));
                }

                double poashad = (radmode == irrad::POA_R) ? ipoa[nn] : (ibeam + iskydiff + ignddiff);

                // determine sub-array contribution to total shaded plane of array for this hour
                ts_accum_poa_front_shaded += poashad * ref_area_m2 * Subarrays[nn]->nModulesPerString * Subarrays[nn]->nStrings;

                // apply soiling derate to all components of irradiance
                double soiling_factor = 1.0;
                int month_idx = wf.month - 1;
                if (month_idx >= 0 && month_idx < 12)
                {
                    soiling_factor = Subarrays[nn]->monthlySoiling[month_idx];
                    ibeam *= soiling_factor;
                    iskydiff *= soiling_factor;
                    ignddiff *= soiling_factor;
                    ibeam_csky *= soiling_factor;
                    iskydiff_csky *= soiling_factor;
                    ignddiff_csky *= soiling_factor;
                    if (radmode == irrad::POA_R || radmode == irrad::POA_P) {
                        ipoa[nn] *= soiling_factor;
                        if (soiling_factor < 1 && idx == 0)
                            log("Soiling may already be accounted for in the input POA data. Check that the input data does not contain soiling effects, or remove the additional losses on the Losses page.", SSC_WARNING);
                    }
                    beam_shading_factor *= soiling_factor;
                }

                // Calculate total front irradiation after soiling added to shading
                if (radmode == irrad::POA_R || radmode == irrad::POA_P) 
                    ipoa_front[nn] = ipoa[nn];
                else
                    ipoa_front[nn] = ibeam + iskydiff + ignddiff;

                ts_accum_poa_front_shaded_soiled += ipoa_front[nn] * ref_area_m2 * Subarrays[nn]->nModulesPerString * Subarrays[nn]->nStrings;

                // Calculate rear-side irradiance
                double module_length = Subarrays[nn]->selfShadingInputs.mod_orient == 1 ? Subarrays[nn]->selfShadingInputs.width : Subarrays[nn]->selfShadingInputs.length;
                double slopeLength = module_length * Subarrays[nn]->selfShadingInputs.nmody;
                irr->calc_rear_side(Subarrays[nn]->Module->bifacialTransmissionFactor, Subarrays[nn]->Module->groundClearanceHeight, slopeLength);
                ipoa_rear[nn] = irr->get_poa_rear();
                ipoa_rear_cs[nn] = irr->get_poa_rear_clearsky();
                double rack_shading_loss_factor = 0.;
                if (Subarrays[nn]->calculateRackShading) {
                    //rack_shading_loss_factor = calculated_rack_shading_loss;        // TODO: implement
                }
                else {
                    rack_shading_loss_factor = Subarrays[nn]->rackShadingLossPercent;
                }
                double rear_irradiance_loss_factor = (1 - rack_shading_loss_factor);
                rear_irradiance_loss_factor *= (1 - Subarrays[nn]->rearSoilingLossPercent);
                ipoa_rear_after_losses[nn] = ipoa_rear[nn] * rear_irradiance_loss_factor;
                ipoa_rear_after_losses_cs[nn] = ipoa_rear_cs[nn] * rear_irradiance_loss_factor;

                ipoa_rear_spatial[nn] = irr->get_poa_rear_spatial();
                ipoa_rear_spatial_after_losses[nn].clear();
                for (size_t i = 0; i < ipoa_rear_spatial[nn].size(); i++) {
                    ipoa_rear_spatial_after_losses[nn].push_back(ipoa_rear_spatial[nn].at(i) * rear_irradiance_loss_factor);
                }
                ignd_rear[nn] = irr->get_ground_spatial();

                double area_subarray = ref_area_m2 * Subarrays[nn]->nModulesPerString * Subarrays[nn]->nStrings;
                ts_accum_poa_front_beam_eff += ibeam * area_subarray;
                ts_accum_poa_rear += ipoa_rear[nn] * area_subarray;
                ts_accum_poa_rear_after_losses = ts_accum_poa_rear * rear_irradiance_loss_factor;                               // rear with all losses, minus electrical mismatch
                ts_accum_ground_incident += irr->get_ground_incident() * area_subarray / Subarrays[nn]->groundCoverageRatio;     // irradiance incident on ground
                ts_accum_ground_absorbed += irr->get_ground_absorbed() * area_subarray / Subarrays[nn]->groundCoverageRatio;     // irradiance absorbed by ground
                ts_accum_poa_rear_ground_reflected += irr->get_ground_reflected() * area_subarray;                               // irradiance reflected from ground to rear
                ts_accum_poa_rear_row_reflections += irr->get_rear_row_reflections() * area_subarray;                            // irradiance reflected from rear row to rear
                ts_accum_poa_rear_direct_diffuse += irr->get_rear_direct_diffuse() * area_subarray;                              // irradiance from sky beam and diffuse to rear
                ts_accum_poa_rear_self_shaded += irr->get_rear_self_shaded() * area_subarray;                                    // irradiance blocked by self shading
                ts_accum_poa_rack_shaded += ipoa_rear[nn] * area_subarray * rack_shading_loss_factor;                           // irradiance blocked by racks
                ts_accum_poa_rear_soiled += ipoa_rear[nn] * area_subarray * (1 - rack_shading_loss_factor) * Subarrays[nn]->rearSoilingLossPercent;      // irradiance blocked by soiling

                if (iyear == 0 || save_full_lifetime_variables == 1)
                {
                    // save sub-array level outputs
                    PVSystem->p_poaShadedFront[nn][idx] = (ssc_number_t)poashad;
                    PVSystem->p_poaShadedSoiledFront[nn][idx] = (ssc_number_t)ipoa_front[nn];
                    PVSystem->p_poaBeamFront[nn][idx] = (ssc_number_t)ibeam;
                    PVSystem->p_poaDiffuseFront[nn][idx] = (ssc_number_t)(iskydiff + ignddiff);
                    PVSystem->p_poaRear[nn][idx] = (ssc_number_t)(ipoa_rear_after_losses[nn]);
                    PVSystem->p_beamShadingFactor[nn][idx] = (ssc_number_t)beam_shading_factor;
                    PVSystem->p_axisRotation[nn][idx] = (ssc_number_t)rot;
                    PVSystem->p_idealRotation[nn][idx] = (ssc_number_t)(rot - btd);
                    PVSystem->p_angleOfIncidence[nn][idx] = (ssc_number_t)aoi;
                    PVSystem->p_surfaceTilt[nn][idx] = (ssc_number_t)stilt;
                    PVSystem->p_surfaceAzimuth[nn][idx] = (ssc_number_t)sazi;
                    PVSystem->p_derateSoiling[nn][idx] = (ssc_number_t)soiling_factor;

                    PVSystem->p_poaBeamFrontCS[nn][idx] = (ssc_number_t)ibeam_csky;
                    PVSystem->p_poaDiffuseFrontCS[nn][idx] = (ssc_number_t)(iskydiff_csky);
                    PVSystem->p_poaDiffuseFrontCS[nn][idx] = (ssc_number_t)(ignddiff_csky);
                    PVSystem->p_poaRearCS[nn][idx] = (ssc_number_t)(ipoa_rear_after_losses_cs[nn]);
                    if (dni_cs != 0) {
                        if (!isnan(Irradiance->p_weatherFileDNI[idx])) {
                            PVSystem->p_DNIIndex[nn][idx] = (ssc_number_t)(Irradiance->p_weatherFileDNI[idx] / dni_cs);
                        }
                        else if (isnan(Irradiance->p_weatherFileDNI[idx]) && (radmode == irrad::DN_DF || radmode == irrad::DN_GH))
                        {
                            PVSystem->p_DNIIndex[nn][idx] = 0; //Simulation should exit when looking for DNI but no DNI found
                        }
                        else {
                            PVSystem->p_DNIIndex[nn][idx] = (ssc_number_t)(Irradiance->p_IrradianceCalculated[2][idx] / dni_cs);
                        }
                    }
                    else {
                        PVSystem->p_DNIIndex[nn][idx] = 0;
                    }

                    // only save first-year spatial outputs
                    if (iyear == 0) {
                        if (idx == 0) {
                            // add column labels to output (first row)
                            // spatial module rear irradiance:
                            int n = ipoa_rear_spatial_after_losses[nn].size();
                            for (size_t i = 0; i <= n; i++) {
                                double slope_distance;      // distance along slope length from bottom of row
                                if (i == 0) {
                                    slope_distance = 0.;    // unused in output
                                }
                                else {
                                    slope_distance = slopeLength / n * ((i - 1) + 0.5);
                                }
                                PVSystem->p_poaRearSpatial[nn][i] = static_cast<double>(slope_distance);
                            }
                            // spatial ground rear irradiance:
                            n = ignd_rear[nn].size();
                            for (size_t i = 0; i <= n; i++) {
                                double row_to_row = slopeLength / Subarrays[nn]->groundCoverageRatio;
                                double row_distance;        // distance along ground from front or center of first row
                                if (i == 0) {
                                    row_distance = 0.;      // unused in output
                                }
                                else {
                                    row_distance = row_to_row / n * ((i - 1) + 0.5);
                                }
                                PVSystem->p_groundRear[nn][i] = static_cast<double>(row_distance);
                            }
                        }
                        std::vector<double> ipoa_rear_output(ipoa_rear_spatial_after_losses[nn]);
                        std::vector<double> ignd_rear_output(ignd_rear[nn]);

                        // add row labels (first column)
                        ipoa_rear_output.insert(ipoa_rear_output.begin(), static_cast<double>(idx));
                        ignd_rear_output.insert(ignd_rear_output.begin(), static_cast<double>(idx));

                        // copy data to output variables
                        std::copy(ipoa_rear_output.begin(), ipoa_rear_output.end(), PVSystem->p_poaRearSpatial[nn] + (idx + 1) * ipoa_rear_output.size());  // +1 for column label row
                        std::copy(ignd_rear_output.begin(), ignd_rear_output.end(), PVSystem->p_groundRear[nn] + (idx + 1) * ignd_rear_output.size());      // +1 for column label row

                        // If using spatial albedos, repeat the above
                        if (Irradiance->useSpatialAlbedos) {
                            if (idx == 0) {
                                // add column labels to output (first row), (same as ground rear irradiance)
                                std::copy(PVSystem->p_groundRear[nn], PVSystem->p_groundRear[nn] + ignd_rear[nn].size() + 1,
                                    Irradiance->p_weatherFileAlbedoSpatial);
                            }
                            std::vector<double> ialb_rear_output(alb_spatial);
                            ialb_rear_output.insert(ialb_rear_output.begin(), static_cast<double>(idx));    // add row labels (first column)
                            std::copy(ialb_rear_output.begin(), ialb_rear_output.end(), Irradiance->p_weatherFileAlbedoSpatial + (idx + 1) * ialb_rear_output.size());      // +1 for column label row
                        }
                    }
                }

                double electrical_mismatch_loss_fraction = 0.;
                if (Subarrays[nn]->Module->isBifacial) {
                    bifaciality = Subarrays[nn]->Module->bifaciality;
                    if (Subarrays[nn]->calculateBifacialElectricalMismatch) {
                        electrical_mismatch_loss_fraction = intraElecMismatch(ipoa_front[nn], ipoa_rear_spatial_after_losses[nn], bifaciality, Subarrays[nn]->Module->selfShadingFillFactor);
                    }
                    else {
                        electrical_mismatch_loss_fraction = Subarrays[nn]->electricalMismatchLossPercent;
                    }
                }
                else {
                    bifaciality = 0.;       // TODO: remove and uncomment lines 1352 in lib_pv_io_manager.cpp instead
                }
                ipoa_rear_after_losses[nn] *= (1 - electrical_mismatch_loss_fraction);
                ipoa_rear_after_losses_cs[nn] *= (1 - electrical_mismatch_loss_fraction);

                ts_accum_electrical_mismatch += ipoa_rear[nn] * area_subarray * rear_irradiance_loss_factor * electrical_mismatch_loss_fraction;    // energy lost due to intra-module electrical mismatch

                // save the required irradiance inputs on array plane for the module output calculations.
                Subarrays[nn]->poa.poaBeamFront = ibeam;
                Subarrays[nn]->poa.poaDiffuseFront = iskydiff;
                Subarrays[nn]->poa.poaGroundFront = ignddiff;
                Subarrays[nn]->poa.poaRear = Subarrays[nn]->Module->isBifacial ? ipoa_rear_after_losses[nn] : 0.;       // TODO: why is setting to 0 necessary for some tests to pass?
                Subarrays[nn]->poa.poaTotal = (radmode == irrad::POA_R) ? ipoa[nn] : (ipoa_front[nn] + ipoa_rear_after_losses[nn] * bifaciality);
                Subarrays[nn]->poa.angleOfIncidenceDegrees = aoi;
                Subarrays[nn]->poa.sunUp = sunup;
                Subarrays[nn]->poa.surfaceTiltDegrees = stilt;
                Subarrays[nn]->poa.surfaceAzimuthDegrees = sazi;

                Subarrays[nn]->poa.poaBeamFrontCS = ibeam_csky;
                Subarrays[nn]->poa.poaDiffuseFrontCS = iskydiff_csky;
                Subarrays[nn]->poa.poaGroundFrontCS = ignddiff_csky;
                Subarrays[nn]->poa.poaRearCS = ipoa_rear_after_losses_cs[nn];
            }

            std::vector<double> mpptVoltageClipping; //a vector to store power that is clipped due to the inverter MPPT low & high voltage limits for each subarray
            for (size_t nn = 0; nn < PVSystem->numberOfSubarrays; nn++) {
                mpptVoltageClipping.push_back(0.0);
            }

            //Calculate power of each MPPT input
            for (size_t mpptInput = 0; mpptInput < PVSystem->Inverter->nMpptInputs; mpptInput++) //remember that actual named mppt inputs are 1-indexed, and these are 0-indexed
            {
                int nSubarraysOnMpptInput = (int)(PVSystem->mpptMapping[mpptInput].size()); //number of subarrays attached to this MPPT input
                std::vector<int> SubarraysOnMpptInput = PVSystem->mpptMapping[mpptInput]; //vector of which subarrays are attached to this MPPT input

                //string voltage for this MPPT input- if 1 subarray, this will be the string voltage. if >1 subarray and mismatch enabled, this
                //will be the string voltage found by the mismatch calculation. if >1 subarray and mismatch not enabled, this will be the average
                //voltage of the strings from all the subarrays on this mppt input.
                //initialize it as -1 and check for that later
                double stringVoltage = -1;

                //mismatch calculations assume that the inverter MPPT operates all strings on that MPPT input at the same voltage.
                //this algorithm sweeps across a range of string voltages, calculating total power for all strings on this MPPT input at each voltage.
                //it finds the maximum total power of all string voltages swept, then uses that in subsequent power calculations for each subarray.
                if (PVSystem->enableMismatchVoltageCalc)
                {
                    double vmax = PVSystem->Inverter->mpptHiVoltage; //the upper MPPT range of the inverter is the high end for string voltages that it will control
                    double vmin = PVSystem->Inverter->mpptLowVoltage; //the lower MPPT range of the inverter is the low end for string voltages that it will control
                    const int NP = 100; //number of points in between max and min voltage to sweep
                    double Pmax = 0; //variable to store the maximum power for comparison between different points along the voltage sweep
                    // sweep voltage, calculating current for each subarray, add all subarray currents together at each voltage
                    for (int i = 0; i < NP; i++)
                    {
                        double stringV = vmin + (vmax - vmin) * i / ((double)NP); //voltage of a string at this point in the voltage sweep

                        //if the voltage is ok, continue to calculate total power on this MPPT input at this voltage
                        double P = 0; //temporary variable to store the total power on this MPPT input at this voltage
                        for (int nSubarray = 0; nSubarray < nSubarraysOnMpptInput; nSubarray++) //sweep across all subarrays connected to this MPPT input
                        {
                            int nn = SubarraysOnMpptInput[nSubarray]; //get the index of the subarray we're checking here
                            double V = stringV / (double)Subarrays[nn]->nModulesPerString; //voltage of an individual module on a string on this subarray

                            //initalize pvinput and pvoutput structures for the model
                            pvinput_t in(Subarrays[nn]->poa.poaBeamFront, Subarrays[nn]->poa.poaDiffuseFront, Subarrays[nn]->poa.poaGroundFront, Subarrays[nn]->poa.poaRear * bifaciality, Subarrays[nn]->poa.poaTotal,
                                wf.tdry, wf.tdew, wf.wspd, wf.wdir, wf.pres,
                                solzen, Subarrays[nn]->poa.angleOfIncidenceDegrees, elev,
                                Subarrays[nn]->poa.surfaceTiltDegrees, Subarrays[nn]->poa.surfaceAzimuthDegrees,
                                ((double)wf.hour) + wf.minute / 60.0,
                                radmode, Subarrays[nn]->poa.usePOAFromWF);
                            pvoutput_t out(0, 0, 0, 0, 0, 0, 0, 0);

                            //calculate the output power for one module in this subarray at this voltage
                            if (Subarrays[nn]->poa.sunUp)
                            {
                                double tcell = wf.tdry;
                                // calculate cell temperature using selected temperature model
                                if (Subarrays[nn]->useCustomCellTemp == 1)
                                    tcell = Subarrays[nn]->customCellTempArray[inrec];
                                else
                                    (*Subarrays[nn]->Module->cellTempModel)(in, *Subarrays[nn]->Module->moduleModel, V, tcell);
                                // calculate module power output using conversion model previously specified
                                (*Subarrays[nn]->Module->moduleModel)(in, tcell, V, out);
                            }
                            //add the power from this subarray to the total power
                            P += V * out.Current * (double)Subarrays[nn]->nModulesPerString * (double)Subarrays[nn]->nStrings;
                        }

                        //check if the total power at this voltage is higher than the power values we've calculated before, if so, set it as the new max
                        if (P > Pmax)
                        {
                            Pmax = P;
                            stringVoltage = stringV;
                        }
                    }

                } //now we have the string voltage at which the MPPT input will produce max power, to be used in subsequent calcs

                //now calculate power for each subarray on this mppt input. stringVoltage will still be -1 if mismatch calcs aren't enabled, or the value decided by mismatch calcs if they are enabled
                std::vector<pvinput_t> in{ num_subarrays }; //create arrays for the pv input and output structures because we have to deal with them in multiple loops to check for MPPT clipping
                std::vector<pvoutput_t> out{ num_subarrays };
                std::vector<pvinput_t> in_cs{ num_subarrays }; //create arrays for the pv input and output structures because we have to deal with them in multiple loops to check for MPPT clipping
                std::vector<pvoutput_t> out_cs{ num_subarrays };

                pvoutput_t out_temp_csky(0, 0, 0, 0, 0, 0, 0, 0);
                double tcell = wf.tdry;
                double tcell_cs = wf.tdry;
                double tcellSS = wf.tdry;

                for (int nSubarray = 0; nSubarray < nSubarraysOnMpptInput; nSubarray++) //sweep across all subarrays connected to this MPPT input
                {
                    int nn = SubarraysOnMpptInput[nSubarray]; //get the index of the subarray we're checking here
                    //initalize pvinput and pvoutput structures for the model
                    pvinput_t in_temp(Subarrays[nn]->poa.poaBeamFront, Subarrays[nn]->poa.poaDiffuseFront, Subarrays[nn]->poa.poaGroundFront, Subarrays[nn]->poa.poaRear * bifaciality, Subarrays[nn]->poa.poaTotal,
                        wf.tdry, wf.tdew, wf.wspd, wf.wdir, wf.pres,
                        solzen, Subarrays[nn]->poa.angleOfIncidenceDegrees, elev,
                        Subarrays[nn]->poa.surfaceTiltDegrees, Subarrays[nn]->poa.surfaceAzimuthDegrees,
                        ((double)wf.hour) + wf.minute / 60.0,
                        radmode, Subarrays[nn]->poa.usePOAFromWF);

                    pvinput_t in_temp_csky(Subarrays[nn]->poa.poaBeamFrontCS, Subarrays[nn]->poa.poaDiffuseFrontCS, Subarrays[nn]->poa.poaGroundFrontCS, Subarrays[nn]->poa.poaRearCS * bifaciality, Subarrays[nn]->poa.poaTotal,
                        wf.tdry, wf.tdew, wf.wspd, wf.wdir, wf.pres,
                        solzen, Subarrays[nn]->poa.angleOfIncidenceDegrees, elev,
                        Subarrays[nn]->poa.surfaceTiltDegrees, Subarrays[nn]->poa.surfaceAzimuthDegrees,
                        ((double)wf.hour) + wf.minute / 60.0,
                        0, false);
                    
                    pvoutput_t out_temp(0, 0, 0, 0, 0, 0, 0, 0);
                    in[nn] = in_temp;
                    out[nn] = out_temp;
                    in_cs[nn] = in_temp_csky;
                    out[nn] = out_temp_csky;

                    if (Subarrays[nn]->poa.sunUp)
                    {
                        //module voltage value to be passed into module power function.
                        //if -1 is passed in, power will be calculated at max power point.
                        //if a voltage value is passed in, power will be calculated at the specified voltage for all single-diode module models
                        double module_voltage = -1;
                        if (stringVoltage != -1) module_voltage = stringVoltage / (double)Subarrays[nn]->nModulesPerString;
                        // calculate cell temperature using selected temperature model
                        // calculate module power output using conversion model previously specified
                        if (Subarrays[nn]->useCustomCellTemp == 1)
                            tcell = Subarrays[nn]->customCellTempArray[inrec];
                        else {
                            (*Subarrays[nn]->Module->cellTempModel)(in[nn], *Subarrays[nn]->Module->moduleModel, module_voltage, tcell);
                            if (std::isnan(tcell)) throw exec_error("pvsamv1", Subarrays[nn]->Module->cellTempModel->error());
                            (*Subarrays[nn]->Module->cellTempModel)(in_cs[nn], *Subarrays[nn]->Module->moduleModel, module_voltage, tcell_cs);
                        }
                        // begin Transient Thermal model
                        // steady state cell temperature - confirm modification from module model to cell temp
                        tcellSS = tcell;
                        // calculate weighted moving average cell temperature base on "Transient Weighted Moving Average Model of Photovoltaic Module Back-Surface Temperature" Prilliman, et. al.
                        // wind speed corrected to 2m, assumed measured at 10m, equation 9 in reference
                        // ssc_number_t wma_z0 = 0.25;
                        // ssc_number_t wma_ws = wf.wspd * std::log(2.0/wma_z0) / std::log(10.0/wma_z0);
                        // precalculate to save execution time
                        ssc_number_t wma_ws = wf.wspd * 0.563705;
                        // module unit mass - Figure 2 in reference and size and weight from https://news.energysage.com/average-solar-panel-size-weight/
                        ssc_number_t wma_mu = 11.09186; // kg/m2
                        if (as_double("module_model") == 1) {
                            wma_mu = as_double("cec_transient_thermal_model_unit_mass");
                        }
                        if (as_double("module_model") == 2) {
                            wma_mu = as_double("6par_transient_thermal_model_unit_mass");
                        }
                        if (as_double("module_model") == 3) {
                            wma_mu = as_double("snl_transient_thermal_model_unit_mass");
                        }
                        // weight function
                        ssc_number_t wma_a0 = 0.0046; // Table II in reference
                        ssc_number_t wma_a1 = 0.00046; // Table II in reference
                        ssc_number_t wma_a2 = -0.00023; // Table II in reference
                        ssc_number_t wma_a3 = -1.6e-5; // Table II in reference
                        ssc_number_t wma_P = wma_a0 + wma_a1 * wma_ws + wma_a2 * wma_mu + wma_a3 * wma_ws * wma_mu;
                        // 20 minute window of weighting values does not include current timestep SS value per refernce
                        int wma_window_minutes = 20;
                        // determine number of past timesteps to average
                        int wma_timestep_minutes = 60 / (int)step_per_hour; //steps per hour is set to 1 for non-annual simulations, so this won't trigger transient thermal model
                        if (wma_timestep_minutes <= 0)
                            throw exec_error("pvsamv1", "Transient thermal timestep minutes <= 0.");

                        // if timestep minute >= window minute use steady state - reference...
                        if (wma_timestep_minutes >= wma_window_minutes)
                            tcell = tcellSS;  // redundant but for code clarification
                        else
                        {
                            int wma_num_prior_timesteps = (wma_window_minutes / wma_timestep_minutes);
                            if (wma_num_prior_timesteps <= 0)
                                throw exec_error("pvsamv1", "Transient thermal prior timesteps <= 0.");
                            ssc_number_t wma_tcellMA_numerator = 0.0;
                            ssc_number_t wma_tcellMA_denominator = 0.0;
                            for (size_t wma_i = 1; wma_i <= (size_t)wma_num_prior_timesteps; wma_i++)
                            {
                                size_t wma_ti = 60 * wma_timestep_minutes * (wma_i); //number of seconds in the past
                                ssc_number_t wma_weight = std::exp(0.0 - wma_P * (ssc_number_t)wma_ti);
                                size_t wma_ts_idx = (idx - iyear * Simulation->numberOfWeatherFileRecords);
                                // limited to first year only

                                if (wma_ts_idx > wma_i)
                                    wma_ts_idx -= wma_i;
                                else
                                    wma_ts_idx = 0;
                                wma_tcellMA_numerator += wma_weight * PVSystem->p_temperatureCellSS[nn][wma_ts_idx];
                                wma_tcellMA_denominator += wma_weight;
                            }
                            if (wma_tcellMA_denominator <= 0)
                                throw exec_error("pvsamv1", "Transient thermal weighting factor sum <= 0.");
                            tcell = wma_tcellMA_numerator / wma_tcellMA_denominator;
                        }
                        // end Transient Thermal model

                        (*Subarrays[nn]->Module->moduleModel)(in[nn], tcell, module_voltage, out[nn]);
                        //ClearSky DC calculations
                        (*Subarrays[nn]->Module->moduleModel)(in_cs[nn], tcell_cs, module_voltage, out_cs[nn]);
                    }
                }

                //assign input voltage at this MPPT input
                //if mismatch was enabled, the voltage already was clipped to the inverter MPPT range as needed and
                //the string voltage is the same for all subarrays, so the voltage at the MPPT input is the same as the string voltage of any subarray
                if (PVSystem->enableMismatchVoltageCalc) {
                    PVSystem->p_mpptVoltage[mpptInput][idx] = (ssc_number_t)out[SubarraysOnMpptInput[0]].Voltage * Subarrays[SubarraysOnMpptInput[0]]->nModulesPerString;
                }
                //if mismatch wasn't enabled, we assume the MPPT input voltage is a weighted average of the string voltages on this MPPT input,
                //and still need to check that average against the inverter MPPT bounds
                else
                {
                    //create temporary values to calculate the weighted average string voltage
                    double nStrings = 0;
                    double avgVoltage = 0;
                    for (int nSubarray = 0; nSubarray < nSubarraysOnMpptInput; nSubarray++)
                    {
                        int nn = SubarraysOnMpptInput[nSubarray]; //get the index of the subarray itself
                        nStrings += Subarrays[nn]->nStrings;
                        avgVoltage += out[nn].Voltage * Subarrays[nn]->nModulesPerString * Subarrays[nn]->nStrings;
                    }
                    avgVoltage /= nStrings;

                    //check the weighted average string voltage against the inverter MPPT bounds
                    bool recalculatePower = false;
                    if (PVSystem->clipMpptWindow)
                    {
                        if (avgVoltage < PVSystem->Inverter->mpptLowVoltage && sunup > 0) //check for sunup to avoid setting MPPT voltage at night
                        {
                            avgVoltage = PVSystem->Inverter->mpptLowVoltage;
                            recalculatePower = true;
                        }
                        else if (avgVoltage > PVSystem->Inverter->mpptHiVoltage)
                        {
                            avgVoltage = PVSystem->Inverter->mpptHiVoltage;
                            recalculatePower = true;
                        }

                        //if MPPT clipping occurs, we need to recalculate the module power for each subarray
                        if (recalculatePower)
                        {
                            for (int nSubarray = 0; nSubarray < nSubarraysOnMpptInput; nSubarray++) //sweep across all subarrays connected to this MPPT input
                            {
                                int nn = SubarraysOnMpptInput[nSubarray]; //get the index of the subarray we're checking here

                                if (iyear == 0 || save_full_lifetime_variables == 1) mpptVoltageClipping[nn] = out[nn].Power; //initialize the voltage clipping loss with the power at module MPP, subtract from this later for the actual MPPT clipping loss

                                //recalculate power at the correct voltage
                                double module_voltage = avgVoltage / (double)Subarrays[nn]->nModulesPerString;
                                if (Subarrays[nn]->useCustomCellTemp == 1)
                                    tcell = Subarrays[nn]->customCellTempArray[inrec];
                                else
                                    (*Subarrays[nn]->Module->cellTempModel)(in[nn], *Subarrays[nn]->Module->moduleModel, module_voltage, tcell);
                                (*Subarrays[nn]->Module->moduleModel)(in[nn], tcell, module_voltage, out[nn]);

                                if (iyear == 0 || save_full_lifetime_variables == 1)	mpptVoltageClipping[nn] -= out[nn].Power; //subtract the power that remains after voltage clipping in order to get the total loss. if no power was lost, all the power will be subtracted away again.
                            }
                        }
                    }

                    //assign final voltage at the MPPT input now that it has been checked against MPPT bounds
                    PVSystem->p_mpptVoltage[mpptInput][idx] = (ssc_number_t)avgVoltage;
                }

                //now that we have the correct power for all subarrays, subject to inverter MPPT clipping, save outputs
                for (int nSubarray = 0; nSubarray < nSubarraysOnMpptInput; nSubarray++) //sweep across all subarrays connected to this MPPT input
                {
                    int nn = SubarraysOnMpptInput[nSubarray]; //get the index of the subarray we're checking here

                    //check for weird results
                    if (out[nn].Voltage > Subarrays[nn]->Module->moduleModel->VocRef() * 1.3)
                        log(util::format("Module voltage is unrealistically high (exceeds 1.3*VocRef) at [mdhm: %d %d %d %lg]: %lg V:\n", wf.month, wf.day, wf.hour, wf.minute, out[nn].Voltage), SSC_NOTICE);
                    if (!std::isfinite(out[nn].Power))
                    {
                        out[nn].Power = 0;
                        out[nn].Voltage = 0;
                        out[nn].Current = 0;
                        out[nn].Efficiency = 0;
                        out[nn].CellTemp = tcell;
                        log(util::format("Power output value calculated at [mdhm: %d %d %d %lg] is not finite, set to zero.\n"
                            "Could be due to anomolous equation behavior at very low irradiances (poa: %lg W/m2).",
                            wf.month, wf.day, wf.hour, wf.minute, Subarrays[nn]->poa.poaTotal), SSC_NOTICE);
                    }

                    //Clearsky DC power
                    Subarrays[nn]->Module->dcPowerWCS = out_cs[nn].Power;

                    // save DC module outputs for this subarray
                    Subarrays[nn]->Module->dcPowerW = out[nn].Power;
                    Subarrays[nn]->Module->dcEfficiency = out[nn].Efficiency * 100;
                    Subarrays[nn]->Module->dcVoltage = out[nn].Voltage;
                    if (Subarrays[nn]->useCustomCellTemp) {
                        Subarrays[nn]->Module->temperatureCellCelcius = Subarrays[nn]->customCellTempArray[inrec];
                        Subarrays[nn]->Module->temperatureCellCelciusSS = Subarrays[nn]->customCellTempArray[inrec];
                    }
                    else {
                        Subarrays[nn]->Module->temperatureCellCelcius = out[nn].CellTemp;
                        Subarrays[nn]->Module->temperatureCellCelciusSS = tcellSS;
                    }
                    Subarrays[nn]->Module->currentShortCircuit = out[nn].Isc_oper;
                    Subarrays[nn]->Module->voltageOpenCircuit = out[nn].Voc_oper;
                    Subarrays[nn]->Module->angleOfIncidenceModifier = out[nn].AOIModifier;



                    // Lifetime dcStringVoltage
                    dcStringVoltage[nn].push_back(Subarrays[nn]->Module->dcVoltage * Subarrays[nn]->nModulesPerString);

                    // Output front-side irradiance after the reflection (IAM) loss - needs to be after the module model for now because reflection effects are part of the module model
                    if (iyear == 0 || save_full_lifetime_variables == 1)
                    {
                        ipoa_front[nn] *= out[nn].AOIModifier;
                        PVSystem->p_poaFront[nn][idx] = (radmode == irrad::POA_R) ? (ssc_number_t)ipoa[nn] : (ssc_number_t)(ipoa_front[nn]);
                        PVSystem->p_poaTotal[nn][idx] = (radmode == irrad::POA_R) ? (ssc_number_t)ipoa[nn] : (ssc_number_t)(ipoa_front[nn] + ipoa_rear_after_losses[nn] * bifaciality);

                        ts_accum_poa_front_total += ipoa_front[nn] * ref_area_m2 * Subarrays[nn]->nModulesPerString * Subarrays[nn]->nStrings;
                        ts_accum_poa_total_eff += ((radmode == irrad::POA_R) ? ipoa[nn] : (ipoa_front[nn] + ipoa_rear_after_losses[nn] * bifaciality)) * ref_area_m2 * Subarrays[nn]->nModulesPerString * Subarrays[nn]->nStrings;

                        //assign final string voltage output
                        PVSystem->p_dcStringVoltage[nn][idx] = (ssc_number_t)Subarrays[nn]->Module->dcVoltage * Subarrays[nn]->nModulesPerString;
                    }
                }
            }

            // sum up all DC power from the whole array
            PVSystem->p_systemDCPower[idx] = 0;
            for (size_t nn = 0; nn < num_subarrays; nn++)
            {
                // DC derates for snow and shading must be applied first
                // these can't be applied before the power calculation because they are POWER derates

                // self-shading derate (by default it is 1.0 if disbled)
                Subarrays[nn]->Module->dcPowerW *= Subarrays[nn]->poa.nonlinearDCShadingDerate;
                Subarrays[nn]->Module->dcPowerWCS *= Subarrays[nn]->poa.nonlinearDCShadingDerate;
                if (iyear == 0 || save_full_lifetime_variables == 1) mpptVoltageClipping[nn] *= Subarrays[nn]->poa.nonlinearDCShadingDerate;

                // Sara 1/25/16 - shading database derate applied to dc only
                // shading loss applied to beam if not from shading database
                Subarrays[nn]->Module->dcPowerW *= Subarrays[nn]->shadeCalculator.dc_shade_factor();

                // scale power and mppt voltage clipping to subarray dimensions
                Subarrays[nn]->dcPowerSubarray = Subarrays[nn]->Module->dcPowerW * Subarrays[nn]->nModulesPerString * Subarrays[nn]->nStrings;
                Subarrays[nn]->dcPowerSubarrayCS = Subarrays[nn]->Module->dcPowerWCS * Subarrays[nn]->nModulesPerString * Subarrays[nn]->nStrings;

                if (iyear == 0 || save_full_lifetime_variables == 1) mpptVoltageClipping[nn] *= Subarrays[nn]->nModulesPerString * Subarrays[nn]->nStrings;

                // Calculate and apply snow coverage losses if activated
                if (PVSystem->enableSnowModel)
                {
                    float smLoss = 0.0f;

                    if (!Subarrays[nn]->snowModel.getLoss((float)(Subarrays[nn]->poa.poaBeamFront + Subarrays[nn]->poa.poaDiffuseFront + Subarrays[nn]->poa.poaGroundFront + ipoa_rear_after_losses[nn]),
                        (float)Subarrays[nn]->poa.surfaceTiltDegrees, (float)wf.wspd, (float)wf.tdry, (float)wf.snow, sunup, 1.0f / step_per_hour, smLoss))
                    {
                        if (!Subarrays[nn]->snowModel.good)
                            throw exec_error("pvsamv1", Subarrays[nn]->snowModel.msg);
                    }
                    float poa_front = Subarrays[nn]->poa.poaBeamFront + Subarrays[nn]->poa.poaDiffuseFront + Subarrays[nn]->poa.poaGroundFront;
                    float poa = poa_front + Subarrays[nn]->poa.poaRear * bifaciality;

                    if (poa != 0) {
                        smLoss *= poa_front / poa;
                    }
                 
                    if (iyear == 0 || save_full_lifetime_variables == 1)
                    {
                        PVSystem->p_snowLoss[nn][idx] = (ssc_number_t)(util::watt_to_kilowatt * Subarrays[nn]->dcPowerSubarray * smLoss);
                        PVSystem->p_snowLossTotal[idx] += (ssc_number_t)(util::watt_to_kilowatt * Subarrays[nn]->dcPowerSubarray * smLoss);
                        PVSystem->p_snowCoverage[nn][idx] = (ssc_number_t)(Subarrays[nn]->snowModel.coverage);
                        if (iyear == 0) annual_snow_loss += (ssc_number_t)(util::watt_to_kilowatt * Subarrays[nn]->dcPowerSubarray * smLoss);
                        Subarrays[nn]->dcPowerSubarray *= (1 - smLoss);
                        Subarrays[nn]->dcPowerSubarrayCS *= (1 - smLoss);
                        
                    }
                    
                    Subarrays[nn]->Module->dcPowerW *= (1 - smLoss);

                    Subarrays[nn]->Module->dcPowerWCS *= (1 - smLoss);
                    if (iyear == 0 || save_full_lifetime_variables == 1) mpptVoltageClipping[nn] *= (1 - smLoss);
                }


                //assign gross outputs per subarray at this point
                if (iyear == 0 || save_full_lifetime_variables == 1)
                {
                    //Gross DC power
                    if (iyear == 0) dc_gross[nn] += Subarrays[nn]->dcPowerSubarray * util::watt_to_kilowatt * ts_hour; //power W to	energy kWh
                    //PVSystem->p_dcPowerGross[nn][idx] = (ssc_number_t)dc_gross[nn]; // cumulative gross DC power per subarray
                    PVSystem->p_dcPowerGross[nn][idx] = Subarrays[nn]->dcPowerSubarray * util::watt_to_kilowatt; // time series gross DC power per subarray
                    //Add to annual MPPT clipping
                    if (iyear == 0) annualMpptVoltageClipping += mpptVoltageClipping[nn] * util::watt_to_kilowatt * ts_hour; //power W to energy kWh
                    // save to SSC output arrays
                    PVSystem->p_temperatureCellSS[nn][idx] = (ssc_number_t)Subarrays[nn]->Module->temperatureCellCelciusSS;

                    PVSystem->p_temperatureCell[nn][idx] = (ssc_number_t)Subarrays[nn]->Module->temperatureCellCelcius;
                    PVSystem->p_moduleEfficiency[nn][idx] = (ssc_number_t)Subarrays[nn]->Module->dcEfficiency;
                    PVSystem->p_voltageOpenCircuit[nn][idx] = (ssc_number_t)(Subarrays[nn]->Module->voltageOpenCircuit * (double)Subarrays[nn]->nModulesPerString);
                    PVSystem->p_currentShortCircuit[nn][idx] = (ssc_number_t)Subarrays[nn]->Module->currentShortCircuit;
                    PVSystem->p_angleOfIncidenceModifier[nn][idx] = (ssc_number_t)(Subarrays[nn]->Module->angleOfIncidenceModifier);

                }

                //calculate net power for each subarray

                // apply pre-inverter power derate
                dcPowerNetPerSubarray[nn] = Subarrays[nn]->dcPowerSubarray * (1 - Subarrays[nn]->dcLossTotalPercent);
                dcPowerNetPerSubarrayCS[nn] = Subarrays[nn]->dcPowerSubarrayCS * (1 - Subarrays[nn]->dcLossTotalPercent);
                //dcPowerNetPerSubarrayCS[nn] = Subarrays[nn]->dcPowerSubarrayCS;
                //module degradation and lifetime DC losses apply to all subarrays
                if (save_full_lifetime_variables == 1) {
                    dcPowerNetPerSubarray[nn] *= PVSystem->dcDegradationFactor[iyear];
                    dcPowerNetPerSubarrayCS[nn] *= PVSystem->dcDegradationFactor[iyear];
                }

                //dc adjustment factors apply to all subarrays
                if (iyear == 0) annual_dc_adjust_loss += dcPowerNetPerSubarray[nn] * (1 - dc_haf(iyear * nrec + inrec)) * util::watt_to_kilowatt * ts_hour; //only keep track of this loss for year 0, convert from power W to energy kWh
                dcPowerNetPerSubarray[nn] *= dc_haf(iyear * nrec + inrec);
                dcPowerNetPerSubarrayCS[nn] *= dc_haf(iyear * nrec + inrec);

                //lifetime daily DC losses apply to all subarrays and should be applied last. Only applied if they are enabled.
                if (PVSystem->enableDCLifetimeLosses)
                {
                    //current index of the lifetime daily DC losses is the number of years that have passed (iyear, because it is 0-indexed) * the number of days + the number of complete days that have passed
                    int dc_loss_index = (int)iyear * 365 + (int)floor(hour_of_year / 24); //in units of days
                    ssc_number_t dc_lifetime_loss = dcPowerNetPerSubarray[nn] * (PVSystem->dcLifetimeLosses[dc_loss_index] / 100) * util::watt_to_kilowatt * ts_hour; //this loss is still in percent, convert to energy kWh
                    if (iyear == 0 || save_full_lifetime_variables == 1) {
                        PVSystem->p_dcLifetimeLoss[idx] = dc_lifetime_loss;
                    }
                    if (iyear == 0) annual_dc_lifetime_loss += dc_lifetime_loss;
                    dcPowerNetPerSubarray[nn] *= (100 - PVSystem->dcLifetimeLosses[dc_loss_index]) / 100;
                    dcPowerNetPerSubarrayCS[nn] *= (100 - PVSystem->dcLifetimeLosses[dc_loss_index]) / 100;
                }

                //assign net DC power output
                PVSystem->p_systemDCPower[idx] += (ssc_number_t)(dcPowerNetPerSubarray[nn] * util::watt_to_kilowatt);

                //Clearsky DC Power
                PVSystem->p_systemDCPowerCS[idx] += (ssc_number_t)(dcPowerNetPerSubarrayCS[nn] * util::watt_to_kilowatt);

                //add this subarray's net DC power to the appropriate MPPT input and to the total system DC power
                PVSystem->p_dcPowerNetPerMppt[Subarrays[nn]->mpptInput - 1][idx] += (ssc_number_t)(dcPowerNetPerSubarray[nn]); //need to subtract 1 from mppt input number because those are 1-indexed
                dcPowerNetTotalSystem += dcPowerNetPerSubarray[nn];
            }

            // save other array-level environmental and irradiance outputs	- year 1 only outputs
            if (iyear == 0 || save_full_lifetime_variables == 1)
            {
                Irradiance->p_weatherFileWindSpeed[idx] = (ssc_number_t)wf.wspd;
                Irradiance->p_weatherFileAmbientTemp[idx] = (ssc_number_t)wf.tdry;
                Irradiance->p_weatherFileSnowDepth[idx] = (ssc_number_t)wf.snow;
                Irradiance->p_sunZenithAngle[idx] = (ssc_number_t)solzen;
                Irradiance->p_sunAltitudeAngle[idx] = (ssc_number_t)solalt;
                Irradiance->p_sunAzimuthAngle[idx] = (ssc_number_t)solazi;
                Irradiance->p_absoluteAirmass[idx] = sunup > 0 ? (ssc_number_t)(exp(-0.0001184 * elev) / (cos(solzen * 3.1415926 / 180) + 0.5057 * pow(96.080 - solzen, -1.634))) : 0.0f;
                Irradiance->p_sunUpOverHorizon[idx] = (ssc_number_t)sunup;
                if (!Irradiance->useSpatialAlbedos) {
                    Irradiance->p_weatherFileAlbedo[idx] = (ssc_number_t)alb;
                }
            }

            if (iyear == 0 || save_full_lifetime_variables == 1)
            {
                // Sum of radiation power on each subarray for the current timestep [kW]
                PVSystem->p_poaFrontNominalTotal[idx] = (ssc_number_t)(ts_accum_poa_front_nom * util::watt_to_kilowatt);
                PVSystem->p_poaFrontBeamNominalTotal[idx] = (ssc_number_t)(ts_accum_poa_front_beam_nom * util::watt_to_kilowatt);
                PVSystem->p_poaFrontShadedTotal[idx] = (ssc_number_t)(ts_accum_poa_front_shaded * util::watt_to_kilowatt);
                PVSystem->p_poaFrontShadedSoiledTotal[idx] = (ssc_number_t)(ts_accum_poa_front_shaded_soiled * util::watt_to_kilowatt);
                PVSystem->p_poaFrontTotal[idx] = (ssc_number_t)(ts_accum_poa_front_total * util::watt_to_kilowatt);
                PVSystem->p_poaRearTotal[idx] = (ssc_number_t)(ts_accum_poa_rear_after_losses * util::watt_to_kilowatt);
                PVSystem->p_groundIncidentTotal[idx] = (ssc_number_t)(ts_accum_ground_incident * util::watt_to_kilowatt);
                PVSystem->p_groundAbsorbedTotal[idx] = (ssc_number_t)(ts_accum_ground_absorbed * util::watt_to_kilowatt);
                PVSystem->p_poaRearGroundReflectedTotal[idx] = (ssc_number_t)(ts_accum_poa_rear_ground_reflected * util::watt_to_kilowatt);
                PVSystem->p_poaRearRowReflectionsTotal[idx] = (ssc_number_t)(ts_accum_poa_rear_row_reflections * util::watt_to_kilowatt);
                PVSystem->p_poaRearDirectDiffuseTotal[idx] = (ssc_number_t)(ts_accum_poa_rear_direct_diffuse * util::watt_to_kilowatt);
                PVSystem->p_poaRearSelfShadedTotal[idx] = (ssc_number_t)(ts_accum_poa_rear_self_shaded * util::watt_to_kilowatt);
                PVSystem->p_poaRackShadedTotal[idx] = (ssc_number_t)(ts_accum_poa_rack_shaded * util::watt_to_kilowatt);
                PVSystem->p_poaRearSoiledTotal[idx] = (ssc_number_t)(ts_accum_poa_rear_soiled * util::watt_to_kilowatt);
                PVSystem->p_bifacialElectricalMismatchTotal[idx] = (ssc_number_t)(ts_accum_electrical_mismatch * util::watt_to_kilowatt);
                PVSystem->p_poaTotalAllSubarrays[idx] = (ssc_number_t)(ts_accum_poa_total_eff * util::watt_to_kilowatt);
                PVSystem->p_poaFrontBeamTotal[idx] = (ssc_number_t)(ts_accum_poa_front_beam_eff * util::watt_to_kilowatt);
                PVSystem->p_inverterMPPTLoss[idx] = 0;
                for (size_t nn = 0; nn < num_subarrays; nn++) {
                    PVSystem->p_inverterMPPTLoss[idx] += (ssc_number_t)(mpptVoltageClipping[nn] * util::watt_to_kilowatt);
                }
            }

            // Predict clipping for DC battery controller
            if (en_batt)
            {
                double cliploss = 0;
                double dcpwr_kw = PVSystem->p_systemDCPower[idx];

                //DC batteries not allowed with multiple MPPT, so can just use MPPT 1's voltage
                sharedInverter->calculateACPower(dcpwr_kw, PVSystem->p_mpptVoltage[0][idx], 0.0);
                PVSystem->p_systemACPower[idx] = sharedInverter->powerAC_kW;

                double pv_ac_kw = sharedInverter->powerAC_kW;
                if (p_pv_ac_forecast.size() > 1) {
                    if (p_pv_ac_forecast.size() > idx) {
                        pv_ac_kw = p_pv_ac_forecast[idx];
                    }
                    else if (p_pv_ac_forecast.size() > idx % (8760 * step_per_hour)) {
                        pv_ac_kw = p_pv_ac_forecast[idx % (8760 * step_per_hour)];
                    }
                }
                p_pv_ac_use.push_back(static_cast<ssc_number_t>(pv_ac_kw));

                if (p_pv_clipping_forecast.size() > 1) {
                    if (p_pv_clipping_forecast.size() > idx) {
                        cliploss = p_pv_clipping_forecast[idx];
                    }
                    else if (p_pv_clipping_forecast.size() > idx % (8760 * step_per_hour)) {
                        cliploss = p_pv_clipping_forecast[idx % (8760 * step_per_hour)];
                    }
                }
                else {
                    cliploss = sharedInverter->powerClipLoss_kW;
                }

                p_invcliploss_full.push_back(static_cast<ssc_number_t>(cliploss));
            }
        }
        // using single weather file initially - so rewind to use for next year
        wdprov->rewind();

        // Assign annual lifetime DC outputs
        if (system_use_lifetime_output) {
            PVSystem->p_dcDegradationFactor[iyear] = (ssc_number_t)(PVSystem->dcDegradationFactor[iyear]);
        }
    }

    //extend DC degradation output for year 0
    if (system_use_lifetime_output) prepend_to_output(this, "dc_degrade_factor", nyears + 1, 1.0);

    // Initialize DC battery predictive controller
    if (en_batt && batt_topology == ChargeController::DC_CONNECTED)
    {
        batt->initialize_automated_dispatch(p_pv_ac_use, p_load_forecast_full, p_invcliploss_full);
    }
    else {
        // Recompute the forecast after AC losses
        p_pv_ac_use.clear();
    }

    /* *********************************************************************************************
    PV AC calculation
    *********************************************************************************************** */
    ireport = 0; ireplast = 0; percent_baseline = percent_complete;
    double annual_battery_loss = 0;
    wdprov->rewind();

    double annual_dc_loss_ond = 0, annual_ac_loss_ond = 0; // (TR)
    double annual_subhourly_clipping_loss = 0;
    double nominal_annual_clipping_output = 0;
    double annual_distribution_clipping_loss = 0;
    util::matrix_t<double> sub_clipping_matrix(21, 21);
    if (as_boolean("enable_subhourly_clipping")) {
        //check that both clipping models are not enabled
        if (as_boolean("enable_subinterval_distribution")) {
            throw exec_error("pvsamv1", "The Allen and Walker methods cannot be used at the same time. Please set either enable_subhourly_clipping or enable_subinterval_distribution to 0.");
        }
        if (is_assigned("subhourly_clipping_matrix")) {
            sub_clipping_matrix = as_matrix("subhourly_clipping_matrix");
        }
        else {
            sub_clipping_matrix = sharedInverter->SubhourlyClippingMatrix();
        }
        for (size_t inrec = 0; inrec < nrec; inrec++) {
            idx = inrec;
            double dcPower_kW_nominal = PVSystem->p_systemDCPower[idx];
            //Calculate Clipping Potential ((P_dc,dryclean - P_ac,0) / P_ac,0) (time step basis)
            sharedInverter->calculateACPower(dcPower_kW_nominal, PVSystem->p_mpptVoltage[0][idx], Irradiance->weatherRecord.tdry, as_boolean("enable_subhourly_clipping")); //DC batteries not allowed with multiple MPPT, so can just use MPPT 1's voltage
            nominal_annual_clipping_output += sharedInverter->powerAC_kW_clipping; //AC nominal output
        }
        assign("nominal_annual_clipping_output", nominal_annual_clipping_output);
    }
    if (as_boolean("enable_subinterval_distribution")) {
        //check that both clipping models are not enabled
        if (as_boolean("enable_subhourly_clipping")) {
            throw exec_error("pvsamv1", "The Allen and Walker methods cannot be used at the same time. Please set either enable_subhourly_clipping or enable_subinterval_distribution to 0.");
        }

        for (size_t inrec = 0; inrec < nrec; inrec++) {
            idx = inrec;
            double dcPower_kW_max = PVSystem->p_systemDCPowerCS[idx];
            sharedInverter->calculateACPower(dcPower_kW_max, PVSystem->p_mpptVoltage[0][idx], Irradiance->weatherRecord.tdry);
            PVSystem->p_systemACPowerMax[idx] = sharedInverter->powerAC_kW + sharedInverter->powerClipLoss_kW;
        }
    }

    for (size_t iyear = 0; iyear < nyears; iyear++)
    {
        //idx is the current array index in the (possibly subhourly) year of weather data or the non-annual array
        for (size_t inrec = 0; inrec < nrec; inrec++)
        {
            idx = inrec + iyear * nrec;
            if (!wdprov->read(&Irradiance->weatherRecord))
                throw exec_error("pvsamv1", "Could not read data line " + util::to_string((int)(inrec + 1)) + " in weather file.");

            size_t hour_of_year = util::hour_of_year(Irradiance->weatherRecord.month, Irradiance->weatherRecord.day, Irradiance->weatherRecord.hour); //this is the index of the hour in the year (0-8759) given the weather file date & timestamp

            // report progress updates to the caller
            ireport++;
            if (ireport - ireplast > irepfreq)
            {
                percent_complete = percent_baseline + 100.0f * (float)(inrec + iyear * nrec) / (float)(insteps);
                if (!update("", percent_complete))
                    throw exec_error("pvsamv1", "Simulation stopped at hour " + util::to_string(hour_of_year + 1.0) + " in year " + util::to_string((int)iyear + 1) + "in AC loop.");
                ireplast = ireport;
            }

            double dcPower_kW = PVSystem->p_systemDCPower[idx];

            double dcPower_kW_csky = PVSystem->p_systemDCPowerCS[idx];

            // Battery replacement
            if (en_batt && (batt_topology == ChargeController::DC_CONNECTED))
            {
                // calculate timestep in hour for battery models
                // jj represents which timestep within the hour you're on, 0-indexed
                // i.e. if idx is 7 in a 15-minute weather file (time 1:45), hour_of_year will be 1, so jj = 7 - (1*4) = 3 (which is correct for 0-indexed jj)
                // and non-annual simulations are not allowed for battery models, so this code won't be encountered in that case
                size_t jj = (idx % nrec) - hour_of_year * step_per_hour;
                batt->initialize_time(iyear, hour_of_year, jj);
                batt->check_replacement_schedule();
            }

            double acpwr_gross = 0, ac_wiringloss = 0, transmissionloss = 0;
            double ac_subhourlyclipping_loss = 0;
            double ac_subinterval_clipping_loss = 0;
            cur_load = p_load_full[idx];

            //set DC voltages for use in AC power calculation
            for (size_t m = 0; m < PVSystem->Inverter->nMpptInputs; m++)
            {
                dcVoltagePerMppt[m] = PVSystem->p_mpptVoltage[m][idx];
                dcPowerNetPerMppt_kW[m] = PVSystem->p_dcPowerNetPerMppt[m][idx] * util::watt_to_kilowatt;
            }

            // Get percentages for transformer loss
            ssc_number_t transformerRatingkW = static_cast<ssc_number_t>(PVSystem->ratedACOutput * util::watt_to_kilowatt);
            ssc_number_t xfmr_ll = PVSystem->transformerLoadLossFraction / step_per_hour;
            ssc_number_t xfmr_nll = PVSystem->transformerNoLoadLossFraction * static_cast<ssc_number_t>(ts_hour * transformerRatingkW);

            bool offline = false;
            if (en_batt && (batt_topology == ChargeController::DC_CONNECTED)) {
                if (batt->is_outage_step(idx % 8760)) {
                    offline = batt->is_offline(idx);
                }
            }


            //run AC power calculation
            if (en_batt && (batt_topology == ChargeController::DC_CONNECTED)) // DC-connected battery
            {
                batt->setXfmrRating(transformerRatingkW);
                double xfmr_nll_kw = PVSystem->transformerNoLoadLossFraction * static_cast<ssc_number_t>(transformerRatingkW);

                // Add up AC loss percents for DC connected batteries
                double delivered_percent = 1 - PVSystem->acLossPercent * 0.01;

                // No transmission losses during grid outage
                if (!offline) {
                    delivered_percent *= 1 - PVSystem->transmissionLossPercent * 0.01;
                }

                ssc_number_t dc_loss_post_inverter = 1 - delivered_percent;
                delivered_percent = 1; // Re-use variable for post batt losses

                ssc_number_t adj_factor = haf(iyear * nrec + inrec);
                delivered_percent *= adj_factor;
                if (system_use_lifetime_output && PVSystem->enableACLifetimeLosses) {
                    int ac_loss_index = (int)iyear * 365 + (int)floor(hour_of_year / 24); //in units of days
                    delivered_percent *= (1 - PVSystem->acLifetimeLosses[ac_loss_index] / 100);
                }
                ssc_number_t dc_loss_post_battery = 1 - delivered_percent;

				// Compute PV clipping before adding battery
				sharedInverter->calculateACPower(dcPower_kW, dcVoltagePerMppt[0], Irradiance->weatherRecord.tdry); //DC batteries not allowed with multiple MPPT, so can just use MPPT 1's voltage
                batt->outGenWithoutBattery[idx] = sharedInverter->powerAC_kW;

                if (resilience) {
                    resilience->add_battery_at_outage_timestep(*batt->dispatch_model, idx);
                    resilience->run_surviving_batteries(p_crit_load_full[idx], sharedInverter->powerAC_kW, dcPower_kW,
                        dcVoltagePerMppt[0], sharedInverter->powerClipLoss_kW, Irradiance->weatherRecord.tdry);
                }

                // Run PV plus battery through sharedInverter, returns AC power
                batt->advance(m_vartab, dcPower_kW, dcVoltagePerMppt[0], cur_load, p_crit_load_full[idx], dc_loss_post_inverter, dc_loss_post_battery, sharedInverter->powerClipLoss_kW, PVSystem->transformerLoadLossFraction, xfmr_nll_kw);
                acpwr_gross = batt->outGenPower[idx];
            }
            else if (PVSystem->Inverter->inverterType == INVERTER_PVYIELD) //PVyield inverter model not currently enabled for multiple MPPT
            {
                sharedInverter->calculateACPower(dcPower_kW, dcVoltagePerMppt[0], Irradiance->weatherRecord.tdry);
                acpwr_gross = sharedInverter->powerAC_kW;
            }
            else
            {
                // inverter: runs at all hours of the day, even if no DC power.  important
                // for capturing tare losses
                sharedInverter->calculateACPower(dcPowerNetPerMppt_kW, dcVoltagePerMppt, Irradiance->weatherRecord.tdry);
                acpwr_gross = sharedInverter->powerAC_kW;
            }
            double paco = sharedInverter->getACNameplateCapacitykW();
            if (as_boolean("enable_subhourly_clipping")) {
                //Calculate DNI clearness index (time step basis)
                double dni_clearness_index = PVSystem->p_DNIIndex[0][idx];
                //Calculate Clipping Potential ((P_dc,dryclean - P_ac,0) / P_ac,0) (time step basis)
                double clip_pot = (dcPower_kW_csky - paco ) / (paco);
                
                PVSystem->p_ClippingPotential[idx] = clip_pot;
                
                //Lookup matrix for percentage effect based on DNI index, Clipping potential

                size_t nrows = sub_clipping_matrix.nrows();
                size_t ncols = sub_clipping_matrix.ncols();
                size_t dni_row = 0;
                size_t clip_pot_col = 0;
                double clip_correction = 0;
                if (dni_clearness_index < sub_clipping_matrix.at(1, 0)) dni_row = 1;
                else if (dni_clearness_index > sub_clipping_matrix.at(nrows - 1, 0)) dni_row = nrows - 1;
                else {
                    for (size_t r = 1; r < nrows; r++) {
                        if (dni_clearness_index > sub_clipping_matrix.at(r, 0) && dni_clearness_index < sub_clipping_matrix.at(r + 1, 0)) {
                            dni_row = r;
                        }
                    }
                }
                PVSystem->p_DNIIndexBin[idx] = dni_row;

                //Clipping potential indexing
                if (clip_pot < sub_clipping_matrix.at(0, 1)) clip_pot_col = 1;
                else if (clip_pot > sub_clipping_matrix.at(0, ncols - 1)) clip_pot_col = ncols - 1;
                else {
                    for (size_t c = 1; c < ncols; c++) {
                        if (clip_pot > sub_clipping_matrix.at(0, c) && clip_pot < sub_clipping_matrix.at(0, c + 1)) {
                            clip_pot_col = c;
                        }
                    }
                }
                PVSystem->p_CPBin[idx] = clip_pot_col;

                if (dcPower_kW > 0.0) {
                    //Sunrise + hour after, sunset + hour preceding are excluding from correction
                    //check if sunrise happens in hour, or if sunrise was last hour, exclude those hours
                    if (Irradiance->p_sunUpOverHorizon[idx] == 2) ac_subhourlyclipping_loss = 0.0;
                    //check if sunset happens in hour, or in next hour, exclude both from correction
                    else if (Irradiance->p_sunUpOverHorizon[idx] == 3) ac_subhourlyclipping_loss = 0.0;
                    else ac_subhourlyclipping_loss = sub_clipping_matrix.at(dni_row, clip_pot_col) * nominal_annual_clipping_output;
                }
                else { //No inverter clipping at night time, skip checks?
                    ac_subhourlyclipping_loss = 0.0;
                }
            }
            if (as_integer("enable_subinterval_distribution") == 1) {
                if (dcPower_kW > 0.0) {
                    double acPower_kW_max = dcPower_kW_csky;
                    double acPower_kW_avg = sharedInverter->powerAC_kW + sharedInverter->powerClipLoss_kW;
                    acpwr_gross += sharedInverter->powerClipLoss_kW; //add clipping loss back in
                    sharedInverter->powerClipLoss_kW = 0; //set standard clipping to zero;
                    acPower_kW_max = std::max(PVSystem->p_systemACPowerMax[idx], acPower_kW_avg);
                    double alt_angle = Irradiance->p_sunAltitudeAngle[idx] * M_PI / 180;
                    if (Irradiance->p_sunAltitudeAngle[idx] < 0.001) alt_angle = 0.001 * M_PI / 180; //For negative alt angle
                    double AM = 1.0 / sin(alt_angle);
                    if (AM > 38.0) AM = 38.0;
                    double acPower_kW_min = std::min(acPower_kW_max * 0.045 / AM, acPower_kW_avg); //AM?
                    //double acPower_kW_avg = sharedInverter->powerAC_kW + sharedInverter->powerClipLoss_kW;
                    double CF = (acPower_kW_max - acPower_kW_min) > 0.0 ? (acPower_kW_avg - acPower_kW_min) / (acPower_kW_max - acPower_kW_min) : 0.0;
                    if (CF == 1.0) CF = 0.999999;
                    double n = CF / (1 - CF);
                    double inv_ac_max = sharedInverter->getACNameplateCapacitykW();
                    double T = 1.0;
                    double log_test = (acPower_kW_max - inv_ac_max) > 0.0 ? (1.0 - (inv_ac_max - acPower_kW_min) / (acPower_kW_max - acPower_kW_min)) : 0.0;
                    ssc_number_t t_lm = 0.0;
                    if (log_test > 0.0 && n > 0.0) {
                        t_lm = T * std::exp(std::log(1.0 - (inv_ac_max - acPower_kW_min) / (acPower_kW_max - acPower_kW_min)) / n); //fraction of hours
                    }
                    double E_clipped = acPower_kW_max * t_lm - ((acPower_kW_max - acPower_kW_min) * pow(t_lm, n + 1) / ((n + 1) * pow(T, n))) - inv_ac_max * t_lm;
                    double E_remaining = (inv_ac_max - acPower_kW_max) * T + ((acPower_kW_max - acPower_kW_min) * pow(T, n + 1) / ((n + 1) * pow(T, n))) - (inv_ac_max - acPower_kW_max) * t_lm -
                        ((acPower_kW_max - acPower_kW_min) * pow(t_lm, n + 1) / ((n + 1) * pow(T, n)));
                    if (E_clipped > 0.0 && E_clipped < 1.0e38) {
                        ac_subinterval_clipping_loss = E_clipped;
                    }
                    else {
                        ac_subinterval_clipping_loss = 0.0;
                    }
                }
                else {
                    ac_subinterval_clipping_loss = (ssc_number_t)0;
                }
                
            }
            
            

            ac_wiringloss = std::abs(acpwr_gross) * PVSystem->acLossPercent * 0.01;
            //ac_subhourlyclipping_loss = std::abs(acpwr_gross) * annual_subhourly_clipping_loss;

            // accumulate first year annual energy
            if (iyear == 0)
            {
                annual_ac_gross += acpwr_gross * ts_hour;

                annual_dc_loss_ond += sharedInverter->dcWiringLoss_ond_kW * ts_hour; // (TR)
                annual_ac_loss_ond += sharedInverter->dcWiringLoss_ond_kW * ts_hour; // (TR)

                annual_ac_wiring_loss += ac_wiringloss * ts_hour;
                annual_subhourly_clipping_loss += ac_subhourlyclipping_loss;
                annual_distribution_clipping_loss += ac_subinterval_clipping_loss;
            }

            if (iyear == 0 || save_full_lifetime_variables == 1)
            {
                PVSystem->p_inverterACOutputPreLoss[idx] = acpwr_gross;
                PVSystem->p_inverterEfficiency[idx] = (ssc_number_t)(sharedInverter->efficiencyAC);
                PVSystem->p_inverterClipLoss[idx] = (ssc_number_t)(sharedInverter->powerClipLoss_kW);
                PVSystem->p_inverterPowerConsumptionLoss[idx] = (ssc_number_t)(sharedInverter->powerConsumptionLoss_kW);
                PVSystem->p_inverterThermalLoss[idx] = (ssc_number_t)(sharedInverter->powerTempLoss_kW);
                PVSystem->p_acWiringLoss[idx] = (ssc_number_t)(ac_wiringloss);
                if (as_boolean("enable_subhourly_clipping")) {
                    PVSystem->p_subhourlyClippingLoss[idx] = (ssc_number_t)(ac_subhourlyclipping_loss);
                }
                if (as_boolean("enable_subinterval_distribution")) {
                    PVSystem->p_DistributionClippingLoss[idx] = (ssc_number_t)(ac_subinterval_clipping_loss);
                }
                
                if (offline) {
                    PVSystem->p_inverterNightTimeLoss[idx] = 0.0;
                    PVSystem->p_inverterTotalLoss[idx] = (ssc_number_t)(sharedInverter->powerLossTotal_kW - sharedInverter->powerNightLoss_kW);
                    if (batt->outUnmetLosses[idx] > 0) {
                        batt->outUnmetLosses[idx] -= sharedInverter->powerNightLoss_kW;
                    }
                    batt->outGenWithoutBattery[idx] += sharedInverter->powerNightLoss_kW;
                }
                else {
                    PVSystem->p_inverterNightTimeLoss[idx] = (ssc_number_t)(sharedInverter->powerNightLoss_kW);
                    PVSystem->p_inverterTotalLoss[idx] = (ssc_number_t)(sharedInverter->powerLossTotal_kW);
                }
            }
            PVSystem->p_systemDCPower[idx] = (ssc_number_t)(sharedInverter->powerDC_kW);

			//ac losses should always be subtracted, this means you can't just multiply by the derate because at nighttime it will add power
            PVSystem->p_systemACPower[idx] = (ssc_number_t)(acpwr_gross - ac_wiringloss - ac_subhourlyclipping_loss - ac_subinterval_clipping_loss);
            // AC connected batteries will set this laster
            if (en_batt && (batt_topology == ChargeController::DC_CONNECTED)) {
                batt->outGenWithoutBattery[idx] -= std::abs(batt->outGenWithoutBattery[idx]) * PVSystem->acLossPercent * 0.01;;
            }

            ssc_number_t xfmr_loss = 0;
            if (!offline) {
                // Apply transformer loss - reset variables after DC connected calculations
                transformerRatingkW = static_cast<ssc_number_t>(PVSystem->ratedACOutput * util::watt_to_kilowatt);
                xfmr_ll = PVSystem->transformerLoadLossFraction / step_per_hour;
                xfmr_nll = PVSystem->transformerNoLoadLossFraction * static_cast<ssc_number_t>(ts_hour * transformerRatingkW);
                // total load loss
                xfmr_loss = Transformer::transformerLoss(PVSystem->p_systemACPower[idx], PVSystem->transformerLoadLossFraction, transformerRatingkW, xfmr_ll, xfmr_nll);

                PVSystem->p_systemACPower[idx] -= xfmr_loss / ts_hour; // kW


                if (en_batt && (batt_topology == ChargeController::DC_CONNECTED)) {
                    // Recompute transformer loss as if the battery didn't run
                    ssc_number_t xfmr_ll_no_batt = PVSystem->transformerLoadLossFraction / step_per_hour;
                    ssc_number_t xfmr_nll_no_batt = PVSystem->transformerNoLoadLossFraction * static_cast<ssc_number_t>(ts_hour * transformerRatingkW);

                    // total load loss
                    ssc_number_t xfmr_loss_no_batt = Transformer::transformerLoss(PVSystem->p_systemACPower[idx], PVSystem->transformerLoadLossFraction, transformerRatingkW, xfmr_ll_no_batt, xfmr_nll_no_batt);
                    batt->outGenWithoutBattery[idx] -= xfmr_loss_no_batt / ts_hour;
                }

                // transmission loss if AC power is produced
                if (PVSystem->p_systemACPower[idx] > 0) {
                    transmissionloss = std::abs(PVSystem->p_systemACPower[idx]) * PVSystem->transmissionLossPercent * 0.01;
                    PVSystem->p_systemACPower[idx] -= (ssc_number_t)(transmissionloss);

                    if (en_batt && (batt_topology == ChargeController::DC_CONNECTED)) {
                        batt->outGenWithoutBattery[idx] -= (ssc_number_t)(transmissionloss);
                    }
                }
            }

            // Re-compute PV AC forecast for AC connected batteries
            if (en_batt && batt_topology == ChargeController::AC_CONNECTED)
            {
                double pv_ac_kw = sharedInverter->powerAC_kW;
                if (p_pv_ac_forecast.size() > 1) {
                    if (p_pv_ac_forecast.size() > idx) {
                        pv_ac_kw = p_pv_ac_forecast[idx];
                    }
                    else if (p_pv_ac_forecast.size() > idx % (8760 * step_per_hour)) {
                        pv_ac_kw = p_pv_ac_forecast[idx % (8760 * step_per_hour)];
                    }
                }
                p_pv_ac_use.push_back(static_cast<ssc_number_t>(pv_ac_kw));
            }

            // accumulate first year annual energy
            if (iyear == 0)
            {
                annual_xfmr_nll += xfmr_nll;
                annual_xfmr_ll += xfmr_ll;
                annual_xfmr_loss += xfmr_loss;
                annual_transmission_loss += transmissionloss;
            }

            if (iyear == 0 || save_full_lifetime_variables == 1)
            {
                PVSystem->p_transformerNoLoadLoss[idx] = xfmr_nll / ts_hour;
                PVSystem->p_transformerLoadLoss[idx] = xfmr_ll / ts_hour;
                PVSystem->p_transformerLoss[idx] = xfmr_loss / ts_hour;
                PVSystem->p_transmissionLoss[idx] = (ssc_number_t)(transmissionloss);
            }
        }

        if (iyear == 0)
        {
            int year_idx = 0;
            if (system_use_lifetime_output) {
                year_idx = 1;
            }
            // accumulate DC power after the battery
            if (en_batt && (batt_topology == ChargeController::DC_CONNECTED)) {
                annual_battery_loss = batt->outAnnualEnergyLoss[year_idx];
            }
        }

        wdprov->rewind();
    }

    // Initialize AC connected battery predictive control
    if (en_batt && batt_topology == ChargeController::AC_CONNECTED)
        batt->initialize_automated_dispatch(p_pv_ac_use, p_load_forecast_full);

    /* *********************************************************************************************
    Post PV AC
    *********************************************************************************************** */
    ireport = 0; ireplast = 0; percent_baseline = percent_complete;
    double annual_energy_pre_battery = 0.;
    for (size_t iyear = 0; iyear < nyears; iyear++)
    {
        //idx is the current array index in the (possibly subhourly) year of weather data or the non-annual array
        for (size_t inrec = 0; inrec < nrec; inrec++)
        {
            idx = inrec + iyear * nrec;
            if (!wdprov->read(&Irradiance->weatherRecord))
                throw exec_error("pvsamv1", "Could not read data line " + util::to_string((int)(inrec + 1)) + " in weather file.");

            size_t hour_of_year = util::hour_of_year(Irradiance->weatherRecord.month, Irradiance->weatherRecord.day, Irradiance->weatherRecord.hour); //this is the index of the hour in the year (0-8759) given the weather file date & timestamp

            // report progress updates to the caller
            ireport++;
            if (ireport - ireplast > irepfreq)
            {
                percent_complete = percent_baseline + 100.0f * (float)(inrec + iyear * nrec) / (float)(insteps);
                if (!update("", percent_complete))
                    throw exec_error("pvsamv1", "Simulation stopped at hour " + util::to_string(hour_of_year + 1.0) + " in year " + util::to_string((int)iyear + 1) + "in post AC loop.");
                ireplast = ireport;
            }

            if (iyear == 0)
                annual_energy_pre_battery += PVSystem->p_systemACPower[idx] * ts_hour;

            // Compute AC loss percent for AC connected batteries
            ssc_number_t adj_factor = haf(iyear* nrec + inrec);

            if (en_batt && batt_topology == ChargeController::AC_CONNECTED)
            {
                //apply lifetime daily AC losses only if they are enabled
                if (system_use_lifetime_output && PVSystem->enableACLifetimeLosses)
                {
                    //current index of the lifetime daily AC losses is the number of years that have passed (iyear, because it is 0-indexed) * days in a year + the number of complete days that have passed
                    int ac_loss_index = (int)iyear * 365 + (int)floor(hour_of_year / 24); //in units of days
                    ssc_number_t ac_lifetime_loss = PVSystem->p_systemACPower[idx] * (PVSystem->acLifetimeLosses[ac_loss_index] / 100); // loss in kWac
                    if (iyear == 0 || save_full_lifetime_variables == 1) {
                        PVSystem->p_acLifetimeLoss[idx] = ac_lifetime_loss;
                    }
                    if (iyear == 0) annual_ac_lifetime_loss += ac_lifetime_loss * ts_hour; // convert to kWh for yr 1 annual sum
                    PVSystem->p_systemACPower[idx] *= (100 - PVSystem->acLifetimeLosses[ac_loss_index]) / 100;
                }
              
                if (iyear == 0 || save_full_lifetime_variables == 1) {
                    PVSystem->p_acPerfAdjLoss[idx] = PVSystem->p_systemACPower[idx] * (1 - adj_factor);
                }
                //apply availability
                PVSystem->p_systemACPower[idx] *= adj_factor;

                double ac_loss_post_inverter = 0; // Already accounted for in pv AC power (including transformer losses)
                double ac_pv_availability_loss_for_batt = 0; // Already accounted for as well

                // calculate timestep in hour for battery models
                // jj represents which timestep within the hour you're on, 0-indexed
                // i.e. if idx is 7 in a 15-minute weather file (time 1:45), hour_of_year will be 1, so jj = 7 - (1*4) = 3 (which is correct for 0-indexed jj)
                // and non-annual simulations are not allowed for battery models, so this code won't be encountered in that case
                size_t jj = (idx % nrec) - hour_of_year * step_per_hour;
                batt->initialize_time(iyear, hour_of_year, jj);
                batt->check_replacement_schedule();

                if (resilience) {
                    resilience->add_battery_at_outage_timestep(*batt->dispatch_model, idx);
                    resilience->run_surviving_batteries(p_crit_load_full[idx], PVSystem->p_systemACPower[idx], 0, 0, 0, 0);
                }

				batt->advance(m_vartab, PVSystem->p_systemACPower[idx], 0, p_load_full[idx], p_crit_load_full[idx], ac_loss_post_inverter, ac_pv_availability_loss_for_batt);
                batt->outGenWithoutBattery[idx] = PVSystem->p_systemACPower[idx];
                PVSystem->p_systemACPower[idx] = batt->outGenPower[idx];
                // accumulate system generation availability loss
                if (iyear == 0) {
                    annual_ac_pre_avail += (PVSystem->p_systemACPower[idx] + PVSystem->p_acPerfAdjLoss[idx]) * ts_hour;
                }

                bool offline = false;
                if (batt->is_outage_step(idx % 8760)) {
                    offline = batt->is_offline(idx);
                }
                
                if (offline && PVSystem->p_systemACPower[idx] < tolerance) {

                    PVSystem->p_inverterTotalLoss[idx] = (ssc_number_t)(PVSystem->p_inverterTotalLoss[idx] - PVSystem->p_inverterNightTimeLoss[idx]);
                    ssc_number_t avoided_losses = PVSystem->p_inverterNightTimeLoss[idx] + PVSystem->p_acWiringLoss[idx] + PVSystem->p_transmissionLoss[idx];
                    batt->outGenWithoutBattery[idx] += avoided_losses;
                    batt->outUnmetLosses[idx] -= avoided_losses;
                    PVSystem->p_inverterNightTimeLoss[idx] = 0.0;
                    PVSystem->p_acWiringLoss[idx] = 0.0;
                    PVSystem->p_transmissionLoss[idx] = 0.0;
                }
            }
            else {

                //apply lifetime daily AC losses only if they are enabled
                if (system_use_lifetime_output && PVSystem->enableACLifetimeLosses)
                {
                    //current index of the lifetime daily AC losses is the number of years that have passed (iyear, because it is 0-indexed) * days in a year + the number of complete days that have passed
                    int ac_loss_index = (int)iyear * 365 + (int)floor(hour_of_year / 24); //in units of days
                    ssc_number_t ac_lifetime_loss = PVSystem->p_systemACPower[idx] * (PVSystem->acLifetimeLosses[ac_loss_index] / 100); // loss in kWac
                    if (iyear == 0 || save_full_lifetime_variables == 1) {
                        PVSystem->p_acLifetimeLoss[idx] = ac_lifetime_loss;
                    }
                    if (iyear == 0) annual_ac_lifetime_loss += ac_lifetime_loss * ts_hour; // convert to kWh for yr 1 annual sum
                    PVSystem->p_systemACPower[idx] *= (100 - PVSystem->acLifetimeLosses[ac_loss_index]) / 100;
                    if (en_batt) {
                        batt->outGenWithoutBattery[idx] *= (100 - PVSystem->acLifetimeLosses[ac_loss_index]) / 100;
                    }
                }

                // accumulate system generation before curtailment and availability
                if (iyear == 0) {
                    annual_ac_pre_avail += PVSystem->p_systemACPower[idx] * ts_hour;
                }

                if (iyear == 0 || save_full_lifetime_variables == 1) {
                    PVSystem->p_acPerfAdjLoss[idx] = PVSystem->p_systemACPower[idx] * (1 - adj_factor);
                }
                //apply availability
                PVSystem->p_systemACPower[idx] *= adj_factor;
                if (en_batt) {
                    batt->outGenWithoutBattery[idx] *= adj_factor;
                }
            }

			// Update battery with final gen to compute grid power
            // TODO: use this block to apply adjustment losses to PV and battery seperately
            if (en_batt) {
                if (batt->is_outage_step(idx % nrec)) {
                    batt->update_grid_power(*this, PVSystem->p_systemACPower[idx], p_crit_load_full[idx], idx); // Updates interconnection losses and similar
                    batt->outGridPower[idx] = 0;
                }
                else {
                    batt->update_grid_power(*this, PVSystem->p_systemACPower[idx], p_load_full[idx], idx);
                }
            }

            if (iyear == 0) {
                annual_energy += (ssc_number_t)(PVSystem->p_systemACPower[idx] * ts_hour);

                
            }
        }
        wdprov->rewind();
    }
    if (wdprov->annualSimulation())
        ssc_number_t* p_annual_energy_dist_time = gen_heatmap(this, 1 / ts_hour);
    // Check the snow models and if neccessary report a warning
    //  *This only needs to be done for subarray1 since all of the activated subarrays should
    //   have the same number of bad values
    //  *Also accumulate monthly and annual loss values

    if (PVSystem->enableSnowModel) {
        if (Subarrays[0]->snowModel.badValues > 0) {
            log(util::format("The snow model has detected %d bad snow depth values (less than 0 or greater than 610 cm), set to zero.", Subarrays[0]->snowModel.badValues), SSC_WARNING);
        }

        // scale by ts_hour to convert power -> energy
        if (Simulation->annualSimulation)
        {
            accumulate_monthly_for_year("dc_snow_loss", "monthly_snow_loss", ts_hour, step_per_hour);
            accumulate_annual_for_year("dc_snow_loss", "annual_snow_loss", ts_hour, step_per_hour);
        }
    }

    //Outputs that are always assigned
    assign("nameplate_dc_rating", var_data((ssc_number_t)nameplate_kw));

    assign("lat", hdr.lat);
    assign("lon", hdr.lon);
    assign("tz", hdr.tz);
    assign("elev", elev);

    inverter_vdcmax_check();
    inverter_size_check();

    Irradiance->AssignOutputs(this);
    Subarrays[0]->AssignOutputs(this);

    //Outputs that are only assigned for annual simulations
    if (Simulation->annualSimulation)
    {
        accumulate_monthly_for_year("dc_net", "monthly_dc", ts_hour, step_per_hour);
        accumulate_monthly_for_year("gen", "monthly_energy", ts_hour, step_per_hour);

        // scale by ts_hour to convert power -> energy
        accumulate_annual_for_year("gh", "annual_gh", ts_hour, step_per_hour);

        // scale by ts_hour to convert power -> energy
        double annual_poa_nom = accumulate_annual_for_year("poa_nom", "annual_poa_nom", ts_hour, step_per_hour);
        double annual_poa_beam_nom = accumulate_annual_for_year("poa_beam_nom", "annual_poa_beam_nom", ts_hour, step_per_hour);
        double annual_poa_shaded = accumulate_annual_for_year("poa_shaded", "annual_poa_shaded", ts_hour, step_per_hour);
        double annual_poa_shaded_soiled = accumulate_annual_for_year("poa_shaded_soiled", "annual_poa_shaded_soiled", ts_hour, step_per_hour);
        double annual_poa_front = accumulate_annual_for_year("poa_front", "annual_poa_front", ts_hour, step_per_hour);
        double annual_poa_rear = accumulate_annual_for_year("poa_rear", "annual_poa_rear", ts_hour, step_per_hour);
        double annual_poa_rear_ground_reflected = accumulate_annual_for_year("poa_rear_ground_reflected", "annual_poa_rear_ground_reflected", ts_hour, step_per_hour);
        double annual_poa_rear_row_reflections = accumulate_annual_for_year("poa_rear_row_reflections", "annual_poa_rear_row_reflections", ts_hour, step_per_hour);
        double annual_poa_rear_direct_diffuse = accumulate_annual_for_year("poa_rear_direct_diffuse", "annual_poa_rear_direct_diffuse", ts_hour, step_per_hour);
        double annual_poa_rear_self_shaded = accumulate_annual_for_year("poa_rear_self_shaded", "annual_poa_rear_self_shaded", ts_hour, step_per_hour);
        double annual_poa_rear_rack_shaded = accumulate_annual_for_year("poa_rear_rack_shaded", "annual_poa_rear_rack_shaded", ts_hour, step_per_hour);
        double annual_poa_rear_soiled = accumulate_annual_for_year("poa_rear_soiled", "annual_poa_rear_soiled", ts_hour, step_per_hour);
        double annual_ground_incident = accumulate_annual_for_year("ground_incident", "annual_ground_incident", ts_hour, step_per_hour);
        double annual_ground_absorbed = accumulate_annual_for_year("ground_absorbed", "annual_ground_absorbed", ts_hour, step_per_hour);
        double annual_bifacial_electrical_mismatch = accumulate_annual_for_year("bifacial_electrical_mismatch", "annual_bifacial_electrical_mismatch", ts_hour, step_per_hour);
        double annual_poa_eff = accumulate_annual_for_year("poa_eff", "annual_poa_eff", ts_hour, step_per_hour);
        double annual_poa_beam_eff = accumulate_annual_for_year("poa_beam_eff", "annual_poa_beam_eff", ts_hour, step_per_hour);

        accumulate_monthly_for_year("poa_nom", "monthly_poa_nom", ts_hour, step_per_hour);
        accumulate_monthly_for_year("poa_beam_nom", "monthly_poa_beam_nom", ts_hour, step_per_hour);
        accumulate_monthly_for_year("poa_front", "monthly_poa_front", ts_hour, step_per_hour);
        accumulate_monthly_for_year("poa_rear", "monthly_poa_rear", ts_hour, step_per_hour);
        accumulate_monthly_for_year("poa_eff", "monthly_poa_eff", ts_hour, step_per_hour);
        accumulate_monthly_for_year("poa_beam_eff", "monthly_poa_beam_eff", ts_hour, step_per_hour);

        // scale by ts_hour to convert power -> energy
        double annual_dc_net = accumulate_annual_for_year("dc_net", "annual_dc_net", ts_hour, step_per_hour);
        accumulate_annual_for_year("gen", "annual_ac_net", ts_hour, step_per_hour);
        double annual_inv_cliploss = accumulate_annual_for_year("inv_cliploss", "annual_inv_cliploss", ts_hour, step_per_hour);
        accumulate_annual_for_year("dc_invmppt_loss", "annual_dc_invmppt_loss", ts_hour, step_per_hour);
        double annual_inv_psoloss = accumulate_annual_for_year("inv_psoloss", "annual_inv_psoloss", ts_hour, step_per_hour);
        double annual_inv_pntloss = accumulate_annual_for_year("inv_pntloss", "annual_inv_pntloss", ts_hour, step_per_hour);
        double annual_inv_tdcloss = accumulate_annual_for_year("inv_tdcloss", "annual_inv_tdcloss", ts_hour, step_per_hour);

        double nom_rad = Subarrays[0]->Module->isConcentratingPV ? annual_poa_beam_nom : annual_poa_nom;
        double inp_rad = Subarrays[0]->Module->isConcentratingPV ? annual_poa_beam_eff : annual_poa_eff;
        double ac_net = as_double("annual_ac_net");
        double mod_eff = module_eff(mod_type);

        // calculate system performance factor
        // reference: (http://files.sma.de/dl/7680/Perfratio-UEN100810.pdf)
        // additional reference: (http://www.nrel.gov/docs/fy05osti/37358.pdf)
        // PR = net_ac (kWh) / ( total input radiation (kWh) * stc efficiency (%) )
        // bug fix 6/15/15 jmf: total input radiation for PR should NOT including shading or soiling, hence use Nominal value.
        assign("performance_ratio", var_data((ssc_number_t)(ac_net / (nom_rad * mod_eff / 100.0))));

        // accumulate annual and monthly battery model outputs
        if (en_batt) batt->calculate_monthly_and_annual_outputs(*this);
        //else assign("average_battery_roundtrip_efficiency", var_data(0.0f)); // if battery disabled, since it's shown in the metrics table

        // calculate nominal dc input
        double annual_dc_nominal = (inp_rad * mod_eff / 100.0);
        assign("annual_dc_nominal", var_data((ssc_number_t)annual_dc_nominal));

        assign("annual_energy", var_data((ssc_number_t)annual_energy));

        double annual_mismatch_loss = 0, annual_diode_loss = 0, annual_dc_wiring_loss = 0, annual_tracking_loss = 0, annual_nameplate_loss = 0, annual_dcopt_loss = 0;
        double annual_dc_gross = 0;

        // loop over subarrays
        for (size_t nn = 0; nn < num_subarrays; nn++)
        {
            if (Subarrays[nn]->enable)
            {
                std::string prefix = "subarray" + util::to_string(static_cast<int>(nn + 1)) + "_";

                double mismatch_loss = 0, diode_loss = 0, wiring_loss = 0, tracking_loss = 0, nameplate_loss = 0, dcopt_loss = 0;
                // dc derate for each sub array
                double dc_loss = dc_gross[nn] * Subarrays[nn]->dcLossTotalPercent;
                annual_dc_gross += dc_gross[nn];

                if (Subarrays[nn]->dcLossTotalPercent != 0)
                {
                    double total_percent = Subarrays[nn]->dcLossTotalPercent;
                    mismatch_loss = Subarrays[nn]->mismatchLossPercent / total_percent * dc_loss;
                    diode_loss = Subarrays[nn]->diodesLossPercent / total_percent * dc_loss;
                    wiring_loss = Subarrays[nn]->dcWiringLossPercent / total_percent * dc_loss;
                    tracking_loss = Subarrays[nn]->trackingLossPercent / total_percent * dc_loss;
                    nameplate_loss = Subarrays[nn]->nameplateLossPercent / total_percent * dc_loss;
                    dcopt_loss = Subarrays[nn]->dcOptimizerLossPercent / total_percent * dc_loss;
                }
                annual_mismatch_loss += mismatch_loss;
                annual_diode_loss += diode_loss;
                annual_dc_wiring_loss += wiring_loss;
                annual_tracking_loss += tracking_loss;
                annual_nameplate_loss += nameplate_loss;
                annual_dcopt_loss += dcopt_loss;

                assign("annual_" + prefix + "dc_gross", var_data((ssc_number_t)dc_gross[nn]));
                assign("annual_" + prefix + "dc_mismatch_loss", var_data((ssc_number_t)mismatch_loss));
                assign("annual_" + prefix + "dc_diodes_loss", var_data((ssc_number_t)diode_loss));
                assign("annual_" + prefix + "dc_wiring_loss", var_data((ssc_number_t)wiring_loss));
                assign("annual_" + prefix + "dc_tracking_loss", var_data((ssc_number_t)tracking_loss));
                assign("annual_" + prefix + "dc_nameplate_loss", var_data((ssc_number_t)nameplate_loss));
            }
        }

        assign("annual_dc_gross", var_data((ssc_number_t)annual_dc_gross));
        assign("annual_ac_gross", var_data((ssc_number_t)annual_ac_gross));

        // AC/DC loss reporting OND model
        assign("annual_dc_loss_ond", var_data((ssc_number_t)annual_dc_loss_ond));
        assign("annual_ac_loss_ond", var_data((ssc_number_t)annual_ac_loss_ond));

        assign("xfmr_nll_year1", (ssc_number_t)annual_xfmr_nll);
        assign("xfmr_ll_year1", (ssc_number_t)annual_xfmr_ll);
        assign("xfmr_loss_year1", (ssc_number_t)annual_xfmr_loss);

        assign("annual_dc_mismatch_loss", var_data((ssc_number_t)annual_mismatch_loss));
        assign("annual_dc_diodes_loss", var_data((ssc_number_t)annual_diode_loss));
        assign("annual_dc_wiring_loss", var_data((ssc_number_t)annual_dc_wiring_loss));
        assign("annual_dc_tracking_loss", var_data((ssc_number_t)annual_tracking_loss));
        assign("annual_dc_nameplate_loss", var_data((ssc_number_t)annual_nameplate_loss));
        assign("annual_dc_optimizer_loss", var_data((ssc_number_t)annual_dcopt_loss));

        // dc user input losses
        // order taken from ui - meaningless if out of order - use percentages per 9/18/14 meeting
        double sys_output = annual_dc_gross;
        sys_output -= annual_mismatch_loss;
        assign("annual_dc_after_mismatch_loss", var_data((ssc_number_t)sys_output));
        sys_output -= annual_diode_loss;
        assign("annual_dc_after_diodes_loss", var_data((ssc_number_t)sys_output));
        sys_output -= annual_dc_wiring_loss;
        assign("annual_dc_after_wiring_loss", var_data((ssc_number_t)sys_output));
        sys_output -= annual_tracking_loss;
        assign("annual_dc_after_tracking_loss", var_data((ssc_number_t)sys_output));
        sys_output -= annual_nameplate_loss;
        assign("annual_dc_after_nameplate_loss", var_data((ssc_number_t)sys_output));
        sys_output -= annual_inv_tdcloss;
        assign("annual_dc_after_inv_tdcloss", var_data((ssc_number_t)sys_output));
        //#define WITH_CHECKS

#ifdef WITH_CHECKS
    // check that sys_output=dc_net
        if (std::abs(annual_dc_net - sys_output) / annual_dc_net > 0.00001)
            log(util::format("Internal discrepancy in calculated output dc_gross: %lg != %lg at DC1.  Please report to SAM support.", annual_dc_net, sys_output), SSC_WARNING);
#endif

        // dc to ac losses
        sys_output -= annual_inv_cliploss;
        assign("annual_ac_after_inv_cliploss", var_data((ssc_number_t)sys_output));
        sys_output -= annual_inv_psoloss;
        assign("annual_ac_after_inv_psoloss", var_data((ssc_number_t)sys_output));
        sys_output -= annual_inv_pntloss;
        assign("annual_ac_after_inv_pntloss", var_data((ssc_number_t)sys_output));

        // ac losses

        assign("annual_ac_wiring_loss", var_data((ssc_number_t)annual_ac_wiring_loss));
        if (as_boolean("enable_subhourly_clipping")) {
            assign("annual_subhourly_clipping_loss", var_data((ssc_number_t)annual_subhourly_clipping_loss));
        }
        if (as_boolean("enable_subinterval_distribution")) {
            assign("annual_distribution_clipping_loss", var_data((ssc_number_t)annual_distribution_clipping_loss));
        }

        assign("annual_transmission_loss", var_data((ssc_number_t)annual_transmission_loss));

        double percent = 0.;
        if (annual_poa_nom > 0) percent = 100 * (annual_poa_nom - annual_poa_shaded) / annual_poa_nom;
        assign("annual_poa_shading_loss_percent", var_data((ssc_number_t)percent));
        percent = 0.;
        if (annual_poa_shaded > 0) percent = 100 * (annual_poa_shaded - annual_poa_shaded_soiled) / annual_poa_shaded;
        assign("annual_poa_soiling_loss_percent", var_data((ssc_number_t)percent));
        percent = 0.;
        if (annual_poa_shaded > 0) percent = 100 * (annual_poa_shaded_soiled - annual_poa_front) / annual_poa_shaded_soiled;
        assign("annual_poa_cover_loss_percent", var_data((ssc_number_t)percent));
        percent = 0.;
        if (annual_poa_rear > 0) percent = 100 * (annual_poa_rear) / annual_poa_front;
        assign("annual_poa_rear_gain_percent", var_data((ssc_number_t)percent));
        percent = 0.;
        if (annual_poa_front > 0) percent = 100 * (annual_ground_incident) / annual_poa_front;
        assign("annual_ground_incident_percent", var_data((ssc_number_t)percent));  // too wide for loss diagram
        percent = 0.;
        if (annual_ground_incident > 0) percent = 100 * (annual_ground_absorbed) / annual_ground_incident;
        assign("annual_ground_absorbed_percent", var_data((ssc_number_t)percent));  // too wide for loss diagram
        percent = 0.;
        // Basing off poa_front because ground incident and absorbed are excluded as they're too wide for loss diagram:
        double rear_total_current = annual_poa_front;
        if (rear_total_current > 0) percent = 100 * (annual_poa_rear_ground_reflected) / rear_total_current;
        assign("annual_rear_ground_reflected_percent", var_data((ssc_number_t)percent));
        rear_total_current += annual_poa_rear_ground_reflected;
        percent = 0.;
        if (rear_total_current > 0) percent = 100 * (annual_poa_rear_row_reflections) / rear_total_current;
        assign("annual_rear_row_reflections_percent", var_data((ssc_number_t)percent));
        rear_total_current += annual_poa_rear_row_reflections;      // insignificant
        percent = 0.;
        if (rear_total_current > 0) percent = 100 * (annual_poa_rear_direct_diffuse) / rear_total_current;
        assign("annual_rear_direct_diffuse_percent", var_data((ssc_number_t)percent));
        rear_total_current += annual_poa_rear_direct_diffuse;       // insignificant
        percent = 0.;
        if (rear_total_current > 0) percent = 100 * (annual_poa_rear_self_shaded) / rear_total_current;
        assign("annual_rear_self_shaded_percent", var_data((ssc_number_t)percent));
        rear_total_current -= annual_poa_rear_self_shaded;          // insignificant
        percent = 0.;
        if (rear_total_current > 0) percent = 100 * (annual_poa_rear_rack_shaded) / rear_total_current;
        assign("annual_rack_shaded_percent", var_data((ssc_number_t)percent));
        rear_total_current -= annual_poa_rear_rack_shaded;
        percent = 0.;
        if (rear_total_current > 0) percent = 100 * (annual_poa_rear_soiled) / rear_total_current;
        assign("annual_rear_soiled_percent", var_data((ssc_number_t)percent));
        rear_total_current -= annual_poa_rear_soiled;
        percent = 0.;
        if (rear_total_current > 0) percent = 100 * (annual_bifacial_electrical_mismatch) / rear_total_current;
        assign("annual_bifacial_electrical_mismatch_percent", var_data((ssc_number_t)percent));
        rear_total_current -= annual_bifacial_electrical_mismatch;

        // annual_dc_nominal
        percent = 0.;
        // SEV: Apply Snow loss to loss diagram
        if (annual_dc_nominal > 0) percent = 100 * annual_snow_loss / annual_dc_nominal;
        assign("annual_dc_snow_loss_percent", var_data((ssc_number_t)percent));

        // apply clipping window loss
        if (annual_dc_nominal > 0) percent = 100 * annualMpptVoltageClipping / annual_dc_nominal;
        assign("annual_dc_mppt_clip_loss_percent", var_data((ssc_number_t)percent));

        // module loss depends on if MPPT clipping enabled.
        percent = 0.;
        if (annual_dc_nominal > 0) percent = 100 * (annual_dc_nominal - (annual_dc_gross + annual_snow_loss + annualMpptVoltageClipping)) / annual_dc_nominal;
        assign("annual_dc_module_loss_percent", var_data((ssc_number_t)percent));


        // annual_dc_gross
        percent = 0.;
        if (annual_dc_gross > 0) percent = 100 * annual_mismatch_loss / annual_dc_gross;
        assign("annual_dc_mismatch_loss_percent", var_data((ssc_number_t)percent));

        percent = 0.;
        if (annual_dc_gross > 0) percent = 100 * annual_diode_loss / annual_dc_gross;
        assign("annual_dc_diodes_loss_percent", var_data((ssc_number_t)percent));

        percent = 0.;
        if (annual_dc_gross > 0) percent = 100 * annual_dc_wiring_loss / annual_dc_gross;
        assign("annual_dc_wiring_loss_percent", var_data((ssc_number_t)percent));

        percent = 0.;
        if (annual_dc_gross > 0) percent = 100 * annual_tracking_loss / annual_dc_gross;
        assign("annual_dc_tracking_loss_percent", var_data((ssc_number_t)percent));

        percent = 0.;
        if (annual_dc_gross > 0) percent = 100 * annual_nameplate_loss / annual_dc_gross;
        assign("annual_dc_nameplate_loss_percent", var_data((ssc_number_t)percent));

        percent = 0.;
        if (annual_dc_gross > 0) percent = 100 * annual_dcopt_loss / annual_dc_gross;
        assign("annual_dc_optimizer_loss_percent", var_data((ssc_number_t)percent));

        percent = 0.;
        if (annual_dc_gross > 0) percent = 100 * annual_inv_tdcloss / annual_dc_gross;
        assign("annual_dc_inv_tdc_loss_percent", var_data((ssc_number_t)percent));

        percent = 0.;
        if (annual_dc_gross > 0) percent = 100 * annual_dc_adjust_loss / annual_dc_gross;
        assign("annual_dc_perf_adj_loss_percent", var_data((ssc_number_t)percent));

        percent = 0.;
        if (annual_dc_gross > 0) percent = 100 * annual_battery_loss / annual_dc_gross;
        assign("annual_dc_battery_loss_percent", var_data((ssc_number_t)percent));

        percent = 0.;
        if (annual_dc_gross > 0) percent = 100 * annual_dc_lifetime_loss / annual_dc_gross;
        assign("annual_dc_lifetime_loss_percent", var_data((ssc_number_t)percent));

        percent = 0.;
        if (annual_dc_gross > 0) percent = 100 * annual_distribution_clipping_loss / annual_dc_gross;
        if (as_boolean("enable_subinterval_distribution")) {
            assign("annual_distribution_clipping_loss_percent", var_data((ssc_number_t)percent));
        }


        //annual_dc_net
        percent = 0.;
        if (annual_dc_net > 0) percent = 100 * annual_inv_cliploss / annual_dc_net;
        assign("annual_ac_inv_clip_loss_percent", var_data((ssc_number_t)percent));

        percent = 0.;
        if (annual_dc_net > 0) percent = 100 * annual_inv_psoloss / annual_dc_net;
        assign("annual_ac_inv_pso_loss_percent", var_data((ssc_number_t)percent));

        percent = 0.;
        if (annual_dc_net > 0) percent = 100 * annual_inv_pntloss / annual_dc_net;
        assign("annual_ac_inv_pnt_loss_percent", var_data((ssc_number_t)percent));

        sys_output = annual_dc_net;
        sys_output -= (annual_inv_cliploss + annual_inv_pntloss + annual_inv_psoloss);
        percent = 0.;
        if (sys_output > 0) percent = 100 * (sys_output - annual_ac_gross) / sys_output;
        assign("annual_ac_inv_eff_loss_percent", var_data((ssc_number_t)percent));


        // annual_ac_gross
        sys_output *= (1.0 - percent / 100.0);

#ifdef WITH_CHECKS
        // check that ac_gross = sys_output at this point
        if (std::abs(annual_ac_gross - sys_output) / annual_ac_gross > 0.00001)
            log(util::format("Internal discrepancy in calculated output ac_gross: %lg != %lg at AC1.  Please report to SAM support.", annual_ac_gross, sys_output), SSC_WARNING);
#endif

        percent = 0.;
        annual_ac_battery_loss = (annual_energy_pre_battery - annual_ac_pre_avail);
        if (annual_ac_gross > 0) percent = 100.0 * annual_ac_battery_loss / annual_ac_gross;
        assign("annual_ac_battery_loss_percent", var_data((ssc_number_t)percent));
        sys_output -= annual_ac_battery_loss;

        if (annual_ac_gross > 0) percent = 100.0 * annual_subhourly_clipping_loss / annual_ac_gross;
        if (as_boolean("enable_subhourly_clipping")) {
            assign("annual_subhourly_clipping_loss_percent", var_data((ssc_number_t)percent));
        }
        sys_output -= annual_subhourly_clipping_loss;

        percent = 0.;
        if (annual_ac_gross > 0) percent = 100.0 * annual_ac_wiring_loss / annual_ac_gross;
        assign("annual_ac_wiring_loss_percent", var_data((ssc_number_t)percent));
        sys_output -= annual_ac_wiring_loss;

        percent = 0.;
        if (annual_ac_gross > 0) percent = 100.0 * annual_transmission_loss / annual_ac_gross;
        assign("annual_transmission_loss_percent", var_data((ssc_number_t)percent));
        sys_output -= annual_transmission_loss;
        //		percent = 0;
        //		if (annual_ac_gross > 0) percent = 100.0 * transformer_loss / annual_ac_gross;
        //		assign("annual_ac_transformer_loss_percent", var_data((ssc_number_t)percent));
        //		sys_output -= transformer_loss;
            // annual_ac_pre_avail

        percent = 0.;
        if (annual_ac_gross > 0) percent = 100 * annual_ac_lifetime_loss / annual_ac_gross;
        assign("annual_ac_lifetime_loss_percent", var_data((ssc_number_t)percent));
        sys_output -= annual_ac_lifetime_loss;

        percent = 0.;
        if (annual_xfmr_loss > 0) percent = 100 * annual_xfmr_loss / annual_ac_gross;
        assign("annual_xfmr_loss_percent", var_data((ssc_number_t)percent));
        sys_output -= annual_xfmr_loss;


#ifdef WITH_CHECKS
        // check that ac_net = sys_output at this point
        if (std::abs(annual_ac_pre_avail - sys_output) / annual_ac_pre_avail > 0.00001)
            log(util::format("Internal discrepancy in calculated output ac_net: %lg != %lg at AC2.  Please report to SAM support.", annual_ac_pre_avail, sys_output), SSC_WARNING);
#endif



        percent = 0.;
        if (annual_ac_pre_avail > 0) percent = 100.0 * (annual_ac_pre_avail - annual_energy) / annual_ac_pre_avail;
        assign("annual_ac_perf_adj_loss_percent", var_data((ssc_number_t)percent));
        sys_output *= (1.0 - percent / 100.0);

        assign("annual_total_loss_percent", var_data((ssc_number_t)((1. - annual_energy / as_number("annual_poa_eff")) * 100.)));
        // annual_ac_net = system_output
        //After calculating total loss remove Battery loss percentages from outputs
        if (!en_batt) {
            unassign("annual_dc_battery_loss_percent");
            unassign("annual_ac_battery_loss_percent");
        }

#ifdef WITH_CHECKS
    // check that ac_net = sys_output at this point
        if (std::abs(annual_ac_net - sys_output) / annual_ac_net > 0.00001)
            log(util::format("Internal discrepancy in calculated output ac_net: %lg != %lg at AC3.  Please report to SAM support.", annual_ac_net, sys_output), SSC_WARNING);
#endif


        // DC Capacity Factor
        double kWhACperkWDC = 0.0;
        double nameplate_dc = as_double("system_capacity");
        if (nameplate_dc > 0) {
            kWhACperkWDC = annual_energy / nameplate_dc;
        }
        assign("capacity_factor", var_data((ssc_number_t)(kWhACperkWDC / 87.6)));
        assign("kwh_per_kw", var_data((ssc_number_t)kWhACperkWDC));

        // AC Capacity Factor
        double kWhACperkWAC = 0.0;
        double nameplate_ac_kW = sharedInverter->getACNameplateCapacitykW();
        if (nameplate_ac_kW > 0) {
            kWhACperkWAC = annual_energy / nameplate_ac_kW;
        }
        assign("capacity_factor_ac", var_data((ssc_number_t)(kWhACperkWAC / 87.6)));
        assign("system_capacity_ac", var_data((ssc_number_t)nameplate_ac_kW));

        if (is_assigned("load"))
        {
            p_load_in = as_vector_ssc_number_t("load");
            nload = p_load_in.size();
        }
    }


    //	_CrtDumpMemoryLeaks();
    //	_CrtSetReportMode(_CRT_ERROR, _CRTDBG_MODE_DEBUG);

        // resiliency metrics
    if (resilience) {
        resilience->run_surviving_batteries_by_looping(&p_crit_load_full[0], PVSystem->p_systemACPower, PVSystem->p_systemDCPower,
            PVSystem->p_mpptVoltage[0], PVSystem->p_inverterClipLoss, Irradiance->p_weatherFileAmbientTemp);
        calculate_resilience_outputs(this, resilience);
    }
    
}

double cm_pvsamv1::intraElecMismatch(double irrad_front_avg /*W/m2*/, std::vector<double> irrad_back /*W/m2*/, double bifaciality /*-*/, double fill_factor_stc /*-*/)
{
    /*
    * The intra-module electrical mismatch loss factor [-] from:
    * Deline, "Estimating and Parameterizing Mismatch Power Loss in Bifacial Photovoltaic Systems"
    */
    const double kFillFactorReference = 0.79;

    if (irrad_front_avg == 0 || irrad_back.size() == 0 || bifaciality == 0) {
        return 0.;                  // avoid nan's
    }

    std::vector<double> irrad_total;
    for (size_t i = 0; i < irrad_back.size(); i++) {
        irrad_total.push_back(irrad_front_avg + bifaciality * irrad_back.at(i));
    }

    double sum_of_deviations = 0.;
    for (size_t i = 0; i < irrad_total.size(); i++) {
        for (size_t j = 0; j < irrad_total.size(); j++) {
            sum_of_deviations += std::abs(irrad_total[i] - irrad_total[j]);
        }
    }

    double irrad_total_avg = std::accumulate(irrad_total.begin(), irrad_total.end(), 0.) / irrad_total.size();
    if (irrad_total_avg == 0) {
        return 0.;                  // mean absolute difference would be nan
    }

    double mean_abs_diff = sum_of_deviations / ( pow(irrad_total.size(), 2) * irrad_total_avg ) * 100.;         // [%] Eqn. 4
    double mismatch_loss_fit3 = 0.054 * mean_abs_diff + 0.068 * pow(mean_abs_diff, 2);                          // [%] Eqn. 12
    double mismatch_factor = std::min(10., mismatch_loss_fit3 * (fill_factor_stc / kFillFactorReference));      // [%] Eqn. 7, limited to 10% max

    double irrad_back_avg = std::accumulate(irrad_back.begin(), irrad_back.end(), 0.) / irrad_back.size();
    double bifacial_irrad_gain = irrad_back_avg * bifaciality / irrad_front_avg * 100.;                         // [%] Eqn. 5
    double loss_factor;
    if (bifacial_irrad_gain != 0) {
        loss_factor = std::max(0., mismatch_factor * (1 + 100. / bifacial_irrad_gain));                         // [%] Eqn. 15
    }
    else {
        loss_factor = 0.;
    }
    return loss_factor / 100.;     // [-]
}

double cm_pvsamv1::module_eff(int mod_type)
{
    double eff = -1;

    switch (mod_type)
    {
    case 0: // SPE
        eff = as_double(util::format("spe_eff%d", as_integer("spe_reference")));
        break;
    case 1: // CEC
    {
        double a_c = as_double("cec_area");
        double i_noct = 1000; // as_double("cec_i_noct");
        double v_mp_ref = as_double("cec_v_mp_ref");
        double i_mp_ref = as_double("cec_i_mp_ref");

        if (a_c == 0) a_c = -1;
        //	if (i_noct == 0) i_noct = 1000.0;

        eff = 100.0 * ((v_mp_ref * i_mp_ref) / a_c) / i_noct;
    }
    break;
    case 2: // 6par user entered
    {
        double area = as_double("6par_area");
        double vmp = as_double("6par_vmp");
        double imp = as_double("6par_imp");
        if (area == 0) area = 1;
        eff = 100.0 * ((vmp * imp) / area) / 1000.0;
    }
    break;
    case 3: // Sandia
    {
        double area = as_double("snl_area");
        double vmpo = as_double("snl_vmpo");
        double impo = as_double("snl_impo");

        eff = vmpo * impo;
        if (area > 0)
            eff = eff / area;
        eff = eff / 1000.0;
        eff = eff * 100;
    }
    break;
    case 4: // IEC 61853
    {
        double area = as_double("sd11par_area");
        double vmp = as_double("sd11par_Vmp0");
        double imp = as_double("sd11par_Imp0");
        if (area == 0) area = 1;
        eff = 100.0 * ((vmp * imp) / area) / 1000.0;
    }
    break;
        case 5: // Mermoud Lejeune
    {
        double area = as_double("mlm_Length") * as_double("mlm_Width");
        double vmp = as_double("mlm_V_mp_ref");
        double imp = as_double("mlm_I_mp_ref");
        eff = 100.0 * ((vmp * imp) / area) / 1000.0;
    }
    }

    if (eff == 0.0) eff = -1;
    return eff;
}

void cm_pvsamv1::inverter_vdcmax_check()
{
    // check that no hourly vmp values exceed Vdcmax
    // add max value and number of times > Vdcmax
    int numVmpGTVdcmax = 0;
    double maxVmp = 0;
    int maxVmpHour = 0;
    int invType = as_integer("inverter_model");
    double vdcmax;
    switch (invType)
    {
    case 0: // cec
        vdcmax = as_double("inv_snl_vdcmax");
        break;
    case 1: // datasheet
        vdcmax = as_double("inv_ds_vdcmax");
        break;
    case 2: // partload curve
        vdcmax = as_double("inv_pd_vdcmax");
        break;
    case 3: // coefficient generator
        vdcmax = as_double("inv_cec_cg_vdcmax");
        break;
    case 4: // ondInverter (PVYield)
        vdcmax = as_double("ond_VAbsMax");
        break;
    default:
        // message
        return;
    }

    // warning on inverter page
    if (vdcmax <= 0) return;

    size_t count;
    ssc_number_t* da = as_array("inverterMPPT1_DCVoltage", &count);
    for (size_t i = 0; i < count; i++)
    {
        if (da[i] > vdcmax)
        {
            numVmpGTVdcmax++;
            if (da[i] > maxVmp)
            {
                maxVmp = da[i];
                maxVmpHour = (int)i;
            }
        }
    }

    if (numVmpGTVdcmax > 0)
    {
        log(util::format("PV array maximum power voltage Vmp exceeds inverter rated maximum voltage Vdcmax (%.2lfV) %d times.\n"
            "   The maximum Vmp value is %.2lfV at timestep %d.\n"
            "   Try reducing number of modules per string to reduce Vmp.", vdcmax, numVmpGTVdcmax, maxVmp, maxVmpHour),
            SSC_WARNING);
    }
}

void cm_pvsamv1::inverter_size_check()
{
    // undersized - check that no hourly output exceeds the rated output of the inverter
    // 9/26/10 note that e_net automatically clipped - must look at derated dc power
    // oversized - add max output > 75% of inverter ourput
    ssc_number_t* acPower;
    size_t acCount;
    ssc_number_t* dcPower;
    size_t dcCount;
    int numHoursClipped = 0;
    double maxACOutput = 0;
    int invType = as_integer("inverter_model");
    int numInv = as_integer("inverter_count");

    double ratedACOutput = 0;
    double ratedDCOutput = 0;
    switch (invType)
    {
    case 0: // cec
        ratedACOutput = as_double("inv_snl_paco");
        ratedDCOutput = as_double("inv_snl_pdco");
        break;
    case 1: // datasheet
        ratedACOutput = as_double("inv_ds_paco");
        ratedDCOutput = as_double("inv_ds_eff") / 100.0;
        if (ratedDCOutput != 0) ratedDCOutput = ratedACOutput / ratedDCOutput;
        break;
    case 2: // partload curve
        ratedACOutput = as_double("inv_pd_paco");
        ratedDCOutput = as_double("inv_pd_pdco");
        break;
    case 3: // coefficient generator
        ratedACOutput = as_double("inv_cec_cg_paco");
        ratedDCOutput = as_double("inv_cec_cg_pdco");
        break;
    case 4: // ond inverter (PVYield)
        ratedACOutput = as_double("ond_PMaxOUT");
        ratedDCOutput = as_double("ond_PMaxDC");
        break;
    default:
        // message
        return;
    }
    ratedACOutput *= numInv;
    ratedDCOutput *= numInv;

    if ((ratedACOutput <= 0) || (ratedDCOutput <= 0)) return;

    ratedACOutput = ratedACOutput * util::watt_to_kilowatt; // W to kW to compare to hourly output
    ratedDCOutput = ratedDCOutput * util::watt_to_kilowatt; // W to kW to compare to hourly output

    acPower = as_array("ac_gross", &acCount);
    dcPower = as_array("dc_net", &dcCount);
    if (acCount == dcCount)
    {
        for (size_t i = 0; i < acCount; i++)
        {
            if (dcPower[i] > ratedDCOutput) numHoursClipped++;
            if (acPower[i] > maxACOutput) maxACOutput = acPower[i];
        }
    }
    if (numHoursClipped >= (int)(acCount / 4)) //more than one quarter of the entire timeseries is clipped
        log(util::format("Inverter undersized: The array output exceeded the inverter DC power rating %.2lf kWdc for %d hours.",
            ratedDCOutput, numHoursClipped),
            SSC_WARNING);

    if ((maxACOutput < 0.75 * ratedACOutput) && (maxACOutput > 0))
        log(util::format("Inverter oversized: The maximum inverter output was %.2lf%% of the rated value %lg kWac.",
            100 * maxACOutput / ratedACOutput, ratedACOutput),
            SSC_WARNING);
}

DEFINE_MODULE_ENTRY(pvsamv1, "Photovoltaic performance model, SAM component models V.1", 1)
