
#include <string>
#include <cmath>
#include <limits>
#include <vector>

#include "core.h"

#include "lib_weatherfile.h"
#include "lib_irradproc.h"
#include "lib_cec6par.h"
#include "lib_sandia.h"
#include "lib_pvinv.h"
#include "6par_jacobian.h"
#include "6par_lu.h"
#include "6par_search.h"
#include "6par_newton.h"
#include "6par_gamma.h"
#include "6par_solve.h"
#include "lib_pvshade.h"

#define sind(x) sin( (3.1415926/180.0)*(x) )
#define cosd(x) cos( (3.1415926/180.0)*(x) )
#define max(a,b) (((a) > (b))?(a):(b))
#define min(a,b) (((a) < (b))?(a):(b))

static inline double to_double(double x) { return x; }


static var_info _cm_vtab_pvsamv1[] = {
/*   VARTYPE           DATATYPE         NAME                                            LABEL                                                   UNITS      META                             GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "weather_file",                                "Weather file in TMY2, TMY3, EPW, or SMW.",                "",       "",                              "pvsamv1",              "*",                        "LOCAL_FILE",                    "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "use_wf_albedo",                               "Use albedo in weather file if provided",                  "0/1",    "",                              "pvsamv1",              "?=1",                      "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_ARRAY,       "albedo",                                      "User specified ground albedo",                            "0..1",   "",                              "pvsamv1",              "*",						  "LENGTH=12",					   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "irrad_mode",                                  "Irradiance input translation mode",                       "",       "0=beam&diffuse,1=total&beam",   "pvsamv1",              "?=0",                      "INTEGER,MIN=0,MAX=1",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sky_model",                                   "Diffuse sky model",                                       "",       "0=isotropic,1=hkdr,2=perez",    "pvsamv1",              "?=2",                      "INTEGER,MIN=0,MAX=2",           "" },

	{ SSC_INPUT,        SSC_NUMBER,      "ac_derate",                                   "Interconnection AC derate",                               "0..1",   "",                              "pvsamv1",              "*",                        "MIN=0,MAX=1",                   "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "modules_per_string",                          "Modules per string",                                      "",       "",                              "pvsamv1",              "*",                        "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "strings_in_parallel",                         "String in parallel",                                      "",       "",                              "pvsamv1",              "*",                        "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inverter_count",                              "Number of inverters",                                     "",       "",                              "pvsamv1",              "*",                        "INTEGER,POSITIVE",              "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "enable_mismatch_vmax_calc",                   "Enable mismatched subarray Vmax calculation",             "",       "",                              "pvsamv1",              "?=0",                      "BOOLEAN",                       "" },

	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_tilt",                              "Sub-array 1 Tilt",                                        "deg",    "0=horizontal,90=vertical",      "pvsamv1",              "naof:subarray1_tilt_eq_lat", "MIN=0,MAX=90",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_tilt_eq_lat",                       "Sub-array 1 Tilt=latitude override",                      "0/1",    "",                              "pvsamv1",              "na:subarray1_tilt",          "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_azimuth",                           "Sub-array 1 Azimuth",                                     "deg",    "0=N,90=E,180=S,270=W",          "pvsamv1",              "*",                        "MIN=0,MAX=359.9",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_track_mode",                        "Sub-array 1 Tracking mode",                               "",       "0=fixed,1=1axis,2=2axis,3=azi", "pvsamv1",              "*",                        "INTEGER,MIN=0,MAX=3",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_rotlim",                            "Sub-array 1 Tracker rotation limit",                      "deg",    "",                              "pvsamv1",              "?=45",                     "MIN=5,MAX=85",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_shade_mode_1x",                     "Sub-array 1 Tracker self-shading mode",                   "0/1/2",  "0=shading,1=backtrack,2=none",  "pvsamv1",              "?=0",                      "INTEGER,MIN=0,MAX=2",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_gcr",                               "Sub-array 1 Ground coverage ratio",                       "0..1",   "",                              "pvsamv1",              "?=0.3",                    "MIN=0,MAX=1",               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray1_shading_hourly",                    "Sub-array 1 Hourly beam shading factors",                 "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "subarray1_shading_mxh",                       "Sub-array 1 Month x Hour beam shading factors",           "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "subarray1_shading_azal",                      "Sub-array 1 Azimuth x altitude beam shading factors",     "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_shading_diff",                      "Sub-array 1 Diffuse shading factor",                      "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray1_soiling",                           "Sub-array 1 Monthly soiling derate",                      "0..1",   "",                              "pvsamv1",              "*",                        "LENGTH=12",                      "" },         
	{ SSC_INPUT,        SSC_NUMBER,      "subarray1_derate",                            "Sub-array 1 DC power derate",                             "0..1",   "",                              "pvsamv1",              "*",                        "MIN=0,MAX=1",                   "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_enable",                            "Sub-array 2 Enable",                                      "0/1",    "0=disabled,1=enabled",          "pvsamv1",              "?=0",                      "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_nstrings",                          "Sub-array 2 Number of parallel strings",                  "",       "",                              "pvsamv1",              "subarray2_enable=1",       "INTEGER",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_tilt",                              "Sub-array 2 Tilt",                                        "deg",    "0=horizontal,90=vertical",      "pvsamv1",              "naof:subarray2_tilt_eq_lat", "MIN=0,MAX=90",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_tilt_eq_lat",                       "Sub-array 2 Tilt=latitude override",                      "0/1",    "",                              "pvsamv1",              "na:subarray2_tilt",          "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_azimuth",                           "Sub-array 2 Azimuth",                                     "deg",    "0=N,90=E,180=S,270=W",          "pvsamv1",              "subarray2_enable=1",       "MIN=0,MAX=359.9",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_track_mode",                        "Sub-array 2 Tracking mode",                               "",       "0=fixed,1=1axis,2=2axis,3=azi", "pvsamv1",              "subarray2_enable=1",       "INTEGER,MIN=0,MAX=3",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_rotlim",                            "Sub-array 2 Tracker rotation limit",                      "deg",    "",                              "pvsamv1",              "?=45",                     "MIN=5,MAX=85",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_shade_mode_1x",                     "Sub-array 2 Tracker self-shading mode",                   "0/1/2",  "0=shading,1=backtrack,2=none",  "pvsamv1",              "?=0",                      "INTEGER,MIN=0,MAX=2",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_gcr",                               "Sub-array 2 Ground coverage ratio",                       "0..1",   "",                              "pvsamv1",              "?=0.3",                    "MIN=0,MAX=1",               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray2_shading_hourly",                    "Sub-array 2 Hourly beam shading factors",                 "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "subarray2_shading_mxh",                       "Sub-array 2 Month x Hour beam shading factors",           "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "subarray2_shading_azal",                      "Sub-array 2 Azimuth x altitude beam shading factors",     "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_shading_diff",                      "Sub-array 2 Diffuse shading factor",                      "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray2_soiling",                           "Sub-array 2 Monthly soiling derate",                      "0..1",   "",                              "pvsamv1",              "subarray2_enable=1",       "LENGTH=12",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray2_derate",                            "Sub-array 2 DC power derate",                             "0..1",   "",                              "pvsamv1",              "subarray2_enable=1",       "MIN=0,MAX=1",                   "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_enable",                            "Sub-array 3 Enable",                                      "0/1",    "0=disabled,1=enabled",          "pvsamv1",              "?=0",                      "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_nstrings",                          "Sub-array 3 Number of parallel strings",                  "",       "",                              "pvsamv1",              "subarray3_enable=1",       "INTEGER",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_tilt",                              "Sub-array 3 Tilt",                                        "deg",    "0=horizontal,90=vertical",      "pvsamv1",              "naof:subarray3_tilt_eq_lat", "MIN=0,MAX=90",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_tilt_eq_lat",                       "Sub-array 3 Tilt=latitude override",                      "0/1",    "",                              "pvsamv1",              "na:subarray3_tilt",          "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_azimuth",                           "Sub-array 3 Azimuth",                                     "deg",    "0=N,90=E,180=S,270=W",          "pvsamv1",              "subarray3_enable=1",       "MIN=0,MAX=359.9",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_track_mode",                        "Sub-array 3 Tracking mode",                               "",       "0=fixed,1=1axis,2=2axis,3=azi", "pvsamv1",              "subarray3_enable=1",       "INTEGER,MIN=0,MAX=3",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_rotlim",                            "Sub-array 3 Tracker rotation limit",                      "deg",    "",                              "pvsamv1",              "?=45",                     "MIN=5,MAX=85",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_shade_mode_1x",                     "Sub-array 3 Tracker self-shading mode",                   "0/1/2",  "0=shading,1=backtrack,2=none",  "pvsamv1",              "?=0",                      "INTEGER,MIN=0,MAX=2",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_gcr",                               "Sub-array 3 Ground coverage ratio",                       "0..1",   "",                              "pvsamv1",              "?=0.3",                    "MIN=0,MAX=1",               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray3_shading_hourly",                    "Sub-array 3 Hourly beam shading factors",                 "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "subarray3_shading_mxh",                       "Sub-array 3 Month x Hour beam shading factors",           "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "subarray3_shading_azal",                      "Sub-array 3 Azimuth x altitude beam shading factors",     "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_shading_diff",                      "Sub-array 3 Diffuse shading factor",                      "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray3_soiling",                           "Sub-array 3 Monthly soiling derate",                      "0..1",   "",                              "pvsamv1",              "subarray3_enable=1",       "LENGTH=12",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray3_derate",                            "Sub-array 3 DC power derate",                             "0..1",   "",                              "pvsamv1",              "subarray3_enable=1",       "MIN=0,MAX=1",                   "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_enable",                            "Sub-array 4 Enable",                                      "0/1",    "0=disabled,1=enabled",          "pvsamv1",              "?=0",                      "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_nstrings",                          "Sub-array 4 Number of parallel strings",                  "",       "",                              "pvsamv1",              "subarray4_enable=1",       "INTEGER",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_tilt",                              "Sub-array 4 Tilt",                                        "deg",    "0=horizontal,90=vertical",      "pvsamv1",              "naof:subarray4_tilt_eq_lat", "MIN=0,MAX=90",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_tilt_eq_lat",                       "Sub-array 4 Tilt=latitude override",                      "0/1",    "",                              "pvsamv1",              "na:subarray4_tilt",          "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_azimuth",                           "Sub-array 4 Azimuth",                                     "deg",    "0=N,90=E,180=S,270=W",          "pvsamv1",              "subarray4_enable=1",       "MIN=0,MAX=359.9",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_track_mode",                        "Sub-array 4 Tracking mode",                               "",       "0=fixed,1=1axis,2=2axis,3=azi", "pvsamv1",              "subarray4_enable=1",       "INTEGER,MIN=0,MAX=3",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_rotlim",                            "Sub-array 4 Tracker rotation limit",                      "deg",    "",                              "pvsamv1",              "?=45",                     "MIN=5,MAX=85",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_shade_mode_1x",                     "Sub-array 4 Tracker self-shading mode",                   "0/1/2",  "0=shading,1=backtrack,2=none",  "pvsamv1",              "?=0",                      "INTEGER,MIN=0,MAX=2",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_gcr",                               "Sub-array 4 Ground coverage ratio",                       "0..1",   "",                              "pvsamv1",              "?=0.3",                    "MIN=0,MAX=3",               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray4_shading_hourly",                    "Sub-array 4 Hourly beam shading factors",                 "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "subarray4_shading_mxh",                       "Sub-array 4 Month x Hour beam shading factors",           "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "subarray4_shading_azal",                      "Sub-array 4 Azimuth x altitude beam shading factors",     "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_shading_diff",                      "Sub-array 4 Diffuse shading factor",                      "",       "",                              "pvsamv1",              "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "subarray4_soiling",                           "Sub-array 4 Monthly soiling derate",                      "0..1",   "",                              "pvsamv1",              "subarray4_enable=1",       "LENGTH=12",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "subarray4_derate",                            "Sub-array 4 DC power derate",                             "0..1",   "",                              "pvsamv1",              "subarray4_enable=1",       "MIN=0,MAX=1",                   "" },

	{ SSC_INPUT,        SSC_NUMBER,      "module_model",                                "Photovoltaic module model specifier",                     "",       "0=spe,1=cec,2=6par_user,3=snl", "pvsamv1",              "*",                        "INTEGER,MIN=0,MAX=3",           "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "spe_area",                                    "Module area",                                             "m2",     "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad0",                                    "Irradiance level 0",                                      "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad1",                                    "Irradiance level 1",                                      "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad2",                                    "Irradiance level 2",                                      "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad3",                                    "Irradiance level 3",                                      "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad4",                                    "Irradiance level 4",                                      "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff0",                                    "Efficiency at irradiance level 0",                        "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff1",                                    "Efficiency at irradiance level 1",                        "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff2",                                    "Efficiency at irradiance level 2",                        "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff3",                                    "Efficiency at irradiance level 3",                        "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff4",                                    "Efficiency at irradiance level 4",                        "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_reference",                               "Reference irradiance level",                              "",       "",                              "pvsamv1",              "module_model=0",           "INTEGER,MIN=0,MAX=4",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_module_structure",                        "Mounting and module structure",                           "",       "0=glass/cell/polymer sheet - open rack,1=glass/cell/glass - open rack,2=polymer/thin film/steel - open rack,3=Insulated back, building-integrated PV,4=close roof mount,5=user-defined",                      "pvsamv1",       "module_model=0",                    "INTEGER,MIN=0,MAX=5",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_a",                                       "Cell temp parameter a",                                   "",       "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_b",                                       "Cell temp parameter b",                                   "",       "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_dT",                                      "Cell temp parameter dT",                                  "",       "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_temp_coeff",                              "Temperature coefficient",                                 "%/C",    "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_fd",                                      "Diffuse fraction",                                        "0..1",   "",                              "pvsamv1",              "module_model=0",           "MIN=0,MAX=1",                   "" },

	{ SSC_INPUT,        SSC_NUMBER,      "cec_area",                                    "Module area",                                             "m2",     "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_a_ref",                                   "Nonideality factor a",                                    "",       "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_adjust",                                  "Temperature coefficient adjustment",                      "%",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_alpha_sc",                                "Short circuit current temperature coefficient",           "A/C",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_beta_oc",                                 "Open circuit voltage temperature coefficient",            "V/C",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_gamma_r",                                 "Maximum power point temperature coefficient",             "%/C",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_i_l_ref",                                 "Light current",                                           "A",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_i_mp_ref",                                "Maximum power point current",                             "A",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_i_o_ref",                                 "Saturation current",                                      "A",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_i_sc_ref",                                "Short circuit current",                                   "A",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_n_s",                                     "Number of cells in series",                               "",       "",                              "pvsamv1",              "module_model=1",           "POSITIVE",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_r_s",                                     "Series resistance",                                       "ohm",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_r_sh_ref",                                "Shunt resistance",                                        "ohm",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_t_noct",                                  "Nominal operating cell temperature",                      "C",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_v_mp_ref",                                "Maximum power point voltage",                             "V",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_v_oc_ref",                                "Open circuit voltage",                                    "V",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_temp_corr_mode",                          "Cell temperature model selection",                        "",       "0=noct,1=mc",                   "pvsamv1",              "module_model=1",           "INTEGER,MIN=0,MAX=1",           "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "cec_standoff",                                "Standoff mode",                                           "",       "0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=<0.5in,6=ground/rack",  "pvsamv1",       "module_model=1",                           "INTEGER,MIN=0,MAX=6",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_height",                                  "Array mounting height",                                   "",       "0=one story,1=two story",                                           "pvsamv1",       "module_model=1",                           "INTEGER,MIN=0,MAX=1",       "" },

	{ SSC_INPUT,        SSC_NUMBER,      "cec_mounting_config",                         "Mounting configuration",                                  "",       "0=rack,1=flush,2=integrated,3=gap",                                 "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "INTEGER,MIN=0,MAX=3",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_heat_transfer",                           "Heat transfer dimensions",                                "",       "0=module,1=array",                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "INTEGER,MIN=0,MAX=1",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_mounting_orientation",                    "Mounting structure orientation",                          "",       "0=do not impede flow,1=vertical supports,2=horizontal supports",    "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "INTEGER,MIN=0,MAX=2",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_gap_spacing",                             "Gap spacing",                                             "m",      "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_module_width",                            "Module width",                                            "m",      "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_module_length",                           "Module height",                                           "m",      "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_array_rows",                              "Rows of modules in array",                                "",       "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_array_cols",                              "Columns of modules in array",                             "",       "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_backside_temp",                           "Module backside temperature",                             "C",      "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "POSITIVE",                  "" },
		
	{ SSC_INPUT,        SSC_NUMBER,      "6par_celltech",                               "Solar cell technology type",                              "",       "monoSi=0,multiSi=1,CdTe=2,CIS=3,CIGS=4,Amorphous=5",                "pvsamv1",       "module_model=2",                           "INTEGER,MIN=0,MAX=5",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_vmp",                                    "Maximum power point voltage",                             "V",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_imp",                                    "Imp",                                                     "A",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_voc",                                    "Voc",                                                     "V",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_isc",                                    "Isc",                                                     "A",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_bvoc",                                   "Short circuit current temperature coefficient",           "V/C",    "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_aisc",                                   "Open circuit voltage temperature coefficient",            "A/C",    "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_gpmp",                                   "Maximum power point temperature coefficient",             "%/C",    "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_nser",                                   "Nseries",                                                 "",       "",                                                                  "pvsamv1",       "module_model=2",                           "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_area",                                   "Module area",                                             "m2",     "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_tnoct",                                  "Nominal operating cell temperature",                      "C",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_standoff",                               "Standoff mode",                                           "",       "0=bipv,1=>3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,6=<0.5in,5=ground/rack",  "pvsamv1",       "module_model=2",                           "INTEGER,MIN=0,MAX=6",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_mounting",                               "Array mounting height",                                   "",       "0=one story,1=two story",                                           "pvsamv1",       "module_model=2",                           "INTEGER,MIN=0,MAX=1",           "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "snl_module_structure",                        "Module and mounting structure configuration",             "",       "0=Use Database Values,1=glass/cell/polymer sheet - open rack,2=glass/cell/glass - open rack,3=polymer/thin film/steel - open rack,4=Insulated back building-integrated PV,5=close roof mount,6=user-defined",                      "pvsamv1",       "module_model=3",                    "INTEGER,MIN=0,MAX=6",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a",                                       "Temperature coefficient a",                               "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b",                                       "Temperature coefficient b",                               "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_dtc",                                     "Temperature coefficient dT",                              "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ref_a",                                   "User-specified a",                                        "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ref_b",                                   "User-specified b",                                        "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ref_dT",                                  "User-specified dT",                                       "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_fd",                                      "Diffuse fraction",                                        "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a0",                                      "Air mass polynomial coeff 0",                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a1",                                      "Air mass polynomial coeff 1",                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a2",                                      "Air mass polynomial coeff 2",                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a3",                                      "Air mass polynomial coeff 3",                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a4",                                      "Air mass polynomial coeff 4",                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_aimp",                                    "Max power point current temperature coefficient",         "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_aisc",                                    "Short circuit current temperature coefficient",           "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_area",                                    "Module area",                                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b0",                                      "Incidence angle modifier polynomial coeff 0",             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b1",                                      "Incidence angle modifier polynomial coeff 1",             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b2",                                      "Incidence angle modifier polynomial coeff 2",             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b3",                                      "Incidence angle modifier polynomial coeff 3",             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b4",                                      "Incidence angle modifier polynomial coeff 4",             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b5",                                      "Incidence angle modifier polynomial coeff 5",             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_bvmpo",                                   "Max power point voltage temperature coefficient",         "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_bvoco",                                   "Open circuit voltage temperature coefficient",            "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c0",                                      "C0",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c1",                                      "C1",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c2",                                      "C2",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c3",                                      "C3",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c4",                                      "C4",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c5",                                      "C5",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c6",                                      "C6",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c7",                                      "C7",                                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_impo",                                    "Max power point current",                                 "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_isco",                                    "Short circuit current",                                   "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ixo",                                     "Ix midpoint current",                                     "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ixxo",                                    "Ixx midpoint current",                                    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_mbvmp",                                   "Irradiance dependence of Vmp temperature coefficient",    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_mbvoc",                                   "Irradiance dependence of Voc temperature coefficient",    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_n",                                       "Diode factor",                                            "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_series_cells",                            "Number of cells in series",                               "",       "",                      "pvsamv1",       "module_model=3",                    "INTEGER",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_vmpo",                                    "Max power point voltage",                                 "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_voco",                                    "Open circuit voltage",                                    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	

	{ SSC_INPUT,        SSC_NUMBER,      "inverter_model",                              "Inverter model specifier",                                "",       "0=cec,1=datasheet,2=partload",        "pvsamv1",       "*",                                 "INTEGER,MIN=0,MAX=2",           "" },

	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c0",                                  "Curvature between ac-power and dc-power at ref",          "1/W",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c1",                                  "Coefficient of Pdco variation with dc input voltage",     "1/V",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c2",                                  "Coefficient of Pso variation with dc input voltage",      "1/V",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c3",                                  "Coefficient of Co variation with dc input voltage",       "1/V",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_paco",                                "AC maximum power rating",                                 "Wac",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pdco",                                "DC input power at which ac-power rating is achieved",     "Wdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pnt",                                 "AC power consumed by inverter at night",                  "Wac",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pso",                                 "DC power required to enable the inversion process",       "Wdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_vdco",                                "DC input voltage for the rated ac-power rating",          "Vdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_vdcmax",                              "Maximum dc input operating voltage",                      "Vdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,      "inv_ds_paco",                                "AC maximum power rating",                                 "Wac",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_ds_eff",                                "Weighted or Peak or Nominal Efficiency",     "Wdc",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_ds_pnt",                                 "AC power consumed by inverter at night",                  "Wac",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_ds_pso",                                 "DC power required to enable the inversion process",       "Wdc",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_ds_vdco",                                "DC input voltage for the rated ac-power rating",          "Vdc",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_ds_vdcmax",                              "Maximum dc input operating voltage",                      "Vdc",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,      "inv_pd_paco",                                "AC maximum power rating",                                 "Wac",     "",                     "pvsamv1",       "inverter_model=2",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_pd_pdco",                                "DC input power at which ac-power rating is achieved",     "Wdc",     "",                     "pvsamv1",       "inverter_model=2",                    "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,      "inv_pd_partload",                                 "Partload curve partload values",                  "%",     "",                     "pvsamv1",       "inverter_model=2",                    "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,      "inv_pd_efficiency",                                 "Partload curve efficiency values",       "%",     "",                     "pvsamv1",       "inverter_model=2",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_pd_pnt",                                 "AC power consumed by inverter at night",                  "Wac",     "",                     "pvsamv1",       "inverter_model=2",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_pd_vdco",                                "DC input voltage for the rated ac-power rating",          "Vdc",     "",                     "pvsamv1",       "inverter_model=2",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_pd_vdcmax",                              "Maximum dc input operating voltage",                      "Vdc",     "",                     "pvsamv1",       "inverter_model=2",                    "",                              "" },


	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_enabled",                        "Enable self-shading calculator",                          "",       "",                       "pvsamv1",       "?=0",                                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_length",                         "Module length",                                           "m",      "",                       "pvsamv1",       "self_shading_enabled=1",              "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_width",                          "Module width",                                            "m",      "",                       "pvsamv1",       "self_shading_enabled=1",              "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_mod_orient",                     "Module orientation",                                      "",       "0=portrait,1=landscape", "pvsamv1",       "self_shading_enabled=1",              "INTEGER,MIN=0,MAX=1",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_str_orient",                     "String orientation",                                      "",       "0=vertical,1=horizontal","pvsamv1",       "self_shading_enabled=1",              "INTEGER,MIN=0,MAX=1",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_ncellx",                         "Number of cells along bottom",                            "",       "",                       "pvsamv1",       "self_shading_enabled=1",              "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_ncelly",                         "Number of cells along side",                              "",       "",                       "pvsamv1",       "self_shading_enabled=1",              "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_ndiode",                         "Number of bypass diodes",                                 "",       "",                       "pvsamv1",       "self_shading_enabled=1",              "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_nmodx",                          "Number of modules along bottom",                          "",       "",                       "pvsamv1",       "self_shading_enabled=1",              "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_nstrx",                          "Number of strings along bottom",                          "",       "",                       "pvsamv1",       "self_shading_enabled=1",              "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_nmody",                          "Number of modules along side",                            "",       "",                       "pvsamv1",       "self_shading_enabled=1",              "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_nrows",                          "Number of rows",                                          "",       "",                       "pvsamv1",       "self_shading_enabled=1",              "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_rowspace",                       "Row spacing",                                             "m",      "",                       "pvsamv1",       "self_shading_enabled=1",              "",                              "" },
// removed per 11/7/12 meeting
//	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_slopens",                        "North-south slope",                                       "deg",    "",                       "pvsamv1",       "self_shading_enabled=1",              "",                              "" },
//	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_slopeew",                        "East-west slope",                                         "deg",    "",                       "pvsamv1",       "self_shading_enabled=1",              "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_mask_angle_calc_method",                     "Mask angle calculation method",                                      "",       "0=worst case,1=average", "pvsamv1",       "self_shading_enabled=1",              "INTEGER,MIN=0,MAX=1",           "" },

// outputs

/* environmental conditions */
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_glob_horiz_rad",                       "Global horizontal irradiance",                                      "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_beam",                                 "Beam irradiance",                                                   "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_diff",                                 "Diffuse irradiance",                                                "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_windspd",                              "Wind speed",                                                        "m/s",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_ambtemp",                              "Ambient temperature",                                               "C",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_input_radiation",                      "Input radiation",                                                   "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_input_radiation_beam",                 "Beam input radiation",                                              "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_sol_zen",                              "Solar zenith angle",                                                "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_sol_alt",                              "Solar altitude angle",                                              "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_sol_azi",                              "Solar azimuth angle",                                               "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_sunup",                                "Sun up over horizon",                                               "0/1",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_airmass",                              "Absolute air mass",                                                 "",       "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_albedo",                               "Albedo",							                                 "",       "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },

	/* sub-array level outputs */
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_aoi",                        "Subarray 1 Angle of incidence",                                     "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_surf_tilt",                  "Subarray 1 Surface tilt",                                           "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_surf_azi",                   "Subarray 1 Surface azimuth",                                        "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_axisrot",                    "Subarray 1 Axis rotation for 1 axis trackers",                      "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_idealrot",                   "Subarray 1 Ideal axis rotation for 1 axis trackers",                "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_poa_nom",                    "Subarray 1 Nominal POA total irradiance",                           "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_poa_shaded",                 "Subarray 1 POA total irradiance after shading only",                "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_poa_eff_beam",               "Subarray 1 POA beam irradiance after shading and soiling",          "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_poa_eff_diff",               "Subarray 1 POA diffuse irradiance after shading and soiling",       "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_poa_eff",                    "Subarray 1 POA total irradiance after shading and soiling",         "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_beam_shading_factor",        "Subarray 1 Beam irradiance shading factor",                         "frac",   "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_modeff",                     "Subarray 1 Module efficiency",                                      "%",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_soiling_derate",             "Subarray 1 Soiling derate",                                         "frac",   "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_celltemp",                   "Subarray 1 Cell temperature",                                       "C",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_dc_gross",                   "Subarray 1 Gross DC output",                                        "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray1_dc_voltage",                 "Subarray 1 DC string voltage",                                      "V",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_aoi",                        "Subarray 2 Angle of incidence",                                     "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_surf_tilt",                  "Subarray 2 Surface tilt",                                           "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_surf_azi",                   "Subarray 2 Surface azimuth",                                        "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_axisrot",                    "Subarray 2 Axis rotation for 1 axis trackers",                      "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_idealrot",                   "Subarray 2 Ideal axis rotation for 1 axis trackers",                "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_poa_nom",                    "Subarray 2 Nominal POA total irradiance",                           "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_poa_shaded",                 "Subarray 2 POA total irradiance after shading only",                "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_poa_eff_beam",               "Subarray 2 POA beam irradiance after shading and soiling",          "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_poa_eff_diff",               "Subarray 2 POA diffuse irradiance after shading and soiling",       "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_poa_eff",                    "Subarray 2 POA total irradiance after shading and soiling",         "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_beam_shading_factor",        "Subarray 2 Beam irradiance shading factor",                         "frac",   "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_modeff",                     "Subarray 2 Module efficiency",                                      "%",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_soiling_derate",             "Subarray 2 Soiling derate",                                         "frac",   "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_celltemp",                   "Subarray 2 Cell temperature",                                       "C",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_dc_gross",                   "Subarray 2 Gross DC output",                                        "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray2_dc_voltage",                 "Subarray 2 DC string voltage",                                      "V",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_aoi",                        "Subarray 3 Angle of incidence",                                     "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_surf_tilt",                  "Subarray 3 Surface tilt",                                           "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_surf_azi",                   "Subarray 3 Surface azimuth",                                        "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_axisrot",                    "Subarray 3 Axis rotation for 1 axis trackers",                      "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_idealrot",                   "Subarray 3 Ideal axis rotation for 1 axis trackers",                "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_poa_nom",                    "Subarray 3 Nominal POA total irradiance",                           "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_poa_shaded",                 "Subarray 3 POA total irradiance after shading only",                "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_poa_eff_beam",               "Subarray 3 POA beam irradiance after shading and soiling",          "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_poa_eff_diff",               "Subarray 3 POA diffuse irradiance after shading and soiling",       "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_poa_eff",                    "Subarray 3 POA total irradiance after shading and soiling",         "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_beam_shading_factor",        "Subarray 3 Beam irradiance shading factor",                         "frac",   "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_modeff",                     "Subarray 3 Module efficiency",                                      "%",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_soiling_derate",             "Subarray 3 Soiling derate",                                         "frac",   "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_celltemp",                   "Subarray 3 Cell temperature",                                       "C",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_dc_gross",                   "Subarray 3 Gross DC output",                                        "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray3_dc_voltage",                 "Subarray 3 DC string voltage",                                      "V",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_aoi",                        "Subarray 4 Angle of incidence",                                     "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_surf_tilt",                  "Subarray 4 Surface tilt",                                           "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_surf_azi",                   "Subarray 4 Surface azimuth",                                        "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_axisrot",                    "Subarray 4 Axis rotation for 1 axis trackers",                      "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_idealrot",                   "Subarray 4 Ideal axis rotation for 1 axis trackers",                "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_poa_nom",                    "Subarray 4 Nominal POA total irradiance",                           "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_poa_shaded",                 "Subarray 4 POA total irradiance after shading only",                "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_poa_eff_beam",               "Subarray 4 POA beam irradiance after shading and soiling",          "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_poa_eff_diff",               "Subarray 4 POA diffuse irradiance after shading and soiling",       "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_poa_eff",                    "Subarray 4 POA total irradiance after shading and soiling",         "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_beam_shading_factor",        "Subarray 4 Beam irradiance shading factor",                         "frac",   "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_modeff",                     "Subarray 4 Module efficiency",                                      "%",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_soiling_derate",             "Subarray 4 Soiling derate",                                         "frac",   "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_celltemp",                   "Subarray 4 Cell temperature",                                       "C",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_dc_gross",                   "Subarray 4 Gross DC output",                                        "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_subarray4_dc_voltage",                 "Subarray 4 DC string voltage",                                      "V",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },


/* aggregate array level outputs */
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_poa_nom",                              "Nominal POA total radiation",                            "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_poa_shaded",                           "POA total radiation after shading only",                 "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_poa_eff",                              "POA total radiation after shading and soiling",          "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_ss_derate",                            "Self-shading derate",                                    "",       "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_ss_diffuse_derate",                    "Self-shading diffuse derate",                            "",       "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_ss_reflected_derate",                  "Self-shading reflected derate",                          "",       "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_ss_diffuse_loss",                      "Self-shading diffuse loss",                              "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_inverter_dc_voltage",                  "Inverter dc input voltage",                              "V",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_dc_gross",                             "Gross dc array output",                                  "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_dc_net",                               "Net dc array output",                                    "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_ac_gross",                             "Gross ac output",                                        "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_ac_net",                               "Net ac output",                                          "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
		
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_inv_eff",                              "Inverter efficiency",                                    "%",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_inv_cliploss",                         "Inverter clipping loss",                                 "Wac",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_inv_psoloss",                          "Inverter power consumption loss",                        "Wdc",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_inv_pntloss",                          "Inverter night time loss",                               "Wac",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_inv_cliploss",                         "Inverter clipping loss",                                 "Wac",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_inv_psoloss",                          "Inverter power consumption loss",                        "Wdc",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_inv_pntloss",                          "Inverter night time loss",                               "Wac",    "",                      "pvsamv1",       "*",                    "",                              "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_poa_nom",                             "Monthly nominal POA total radiation",                    "kWh/m2", "",                      "pvsamv1",       "*",                    "LENGTH=12",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_poa_eff",                             "Monthly POA total radiation after shading and soiling",  "kWh/m2", "",                      "pvsamv1",       "*",                    "LENGTH=12",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_poa_eff_beam",                        "Monthly POA beam radiation after shading and soiling",   "kWh/m2", "",                      "pvsamv1",       "*",                    "LENGTH=12",                              "" },
	
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_dc_net",                              "Net dc output",                                          "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=12",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_ac_net",                              "Net ac output",                                          "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=12",                              "" },
	
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_glob_horiz_rad",                       "Global horizontal irradiance",                           "kWh/m2", "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_input_radiation",                      "Input radiation",                                        "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_input_radiation_beam",                 "Beam input radiation",                                   "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_nom",                              "Annual Nominal POA total radiation",                     "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_shaded",                           "Annual POA total radiation after shading only",          "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_poa_eff",                              "Annual POA total radiation after shading and soiling",   "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_dc_nominal",                           "Nominal dc energy",                                      "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_dc_gross",                             "Gross dc output",                                        "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_dc_net",                               "Net dc output",                                          "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_ac_gross",                             "Gross ac output",                                        "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_ac_net",                               "Net ac output",                                          "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "annual_performance_factor",                   "System performance factor",                              "%",      "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "nameplate_dc_rating",                         "Nameplate system dc rating",                             "kW",     "",                      "pvsamv1",       "*",                    "",                              "" },
	
	{ SSC_OUTPUT,        SSC_NUMBER,     "6par_a",                                      "CEC 6-parameter: a",                                     "",       "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "6par_Io",                                     "CEC 6-parameter: Io",                                    "",       "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "6par_Il",                                     "CEC 6-parameter: Il",                                    "",       "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "6par_Rs",                                     "CEC 6-parameter: Rs",                                    "",       "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "6par_Rsh",                                    "CEC 6-parameter: Rsh",                                   "",       "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "6par_Adj",                                    "CEC 6-parameter: Adj",                                   "",       "",                      "pvsamv1",       "*",                    "",                              "" },
var_info_invalid };

struct subarray
{
	subarray()
	{
		enable = false;
		nstrings = 0;
		tilt_eq_lat = false;
		tilt = azimuth = 0;
		track_mode = 0;
		rotlim = 0;
		shade_mode_1x = 0; // 0=self-shading by geometric (beam area loss plus diffuse viewfactor) 1=backtracking 2=none
		gcr = 0.3;

		derate = 1.0;
		
		for (size_t i=0;i<12;i++)
			soiling[i] = 1.0;

		
		shad_skydiff_factor = 1.0;
		en_azal = false;

		poa.ibeam = 0;
		poa.iskydiff = 0;
		poa.ignddiff = 0;
		poa.sunup = 0;
		poa.aoi = 0;
		poa.stilt = 0;
		poa.sazi = 0;

		module.dcpwr = 0;
		module.dcv = 0;
		module.dceff = 0;
		module.tcell = 0;

		for (int i=0;i<12;i++)
			monthly_poa_nom[i] = monthly_poa_eff_beam[i] = monthly_poa_eff[i] = 0.0;
	}

	bool enable;
	int nstrings;
	double tilt;
	bool tilt_eq_lat;
	double azimuth;
	int track_mode;
	double rotlim;
	double soiling[12];
	double derate;
	
	int shade_mode_1x;
	double gcr;
	
	std::vector<double> shad_beam_factor; // hourly factors as well as mxh
	bool en_azal; // enable az x al shading factors
	util::matrix_t<double> shad_azal;
	double shad_skydiff_factor;

	// calculated by irradiance processor
	struct {
		double ibeam;
		double iskydiff;
		double ignddiff;
		int sunup;
		double aoi;
		double stilt;
		double sazi;
	} poa;

	// calculated by module model
	struct {
		double dcpwr;
		double dcv;
		double dceff;
		double tcell;
	} module;
	
	double monthly_poa_nom[12];
	double monthly_poa_eff[12];
	double monthly_poa_eff_beam[12];
};
	
class cm_pvsamv1 : public compute_module
{
public:
	
	cm_pvsamv1()
	{
		add_var_info( _cm_vtab_pvsamv1 );
	}


	
	void exec( ) throw( general_error )
	{
		// open the weather file
		// define variables consistent across subarrays
		

		weatherfile wf( as_string("weather_file") );
		if ( !wf.ok() ) throw exec_error( "pvsamv1", "failed to open weather file for reading");
			
		if ( wf.nrecords != 8760 ) throw exec_error("pvsamv1", "pv simulator only accepts hourly weather data");

		int modules_per_string = as_integer("modules_per_string");
		int strings_in_parallel = as_integer("strings_in_parallel");
		int num_inverters = as_integer("inverter_count");
		double ac_derate = as_double("ac_derate");

		size_t alb_len = 0;
		ssc_number_t *alb_array = as_array("albedo", &alb_len); // monthly albedo array

		bool use_wf_alb = (as_integer("use_wf_albedo") > 0); // weather file albedo

		int radmode = as_integer("irrad_mode"); // 0=B&D, 1=G&B
		int skymodel = as_integer("sky_model"); // 0=isotropic, 1=hdkr, 2=perez

		// load the subarray parameter information
		subarray sa[4];		
		int num_subarrays = 1;


		// loop over subarrays
		for ( size_t nn=0;nn<4;nn++ )
		{
			sa[nn].enable = true;
			sa[nn].nstrings = strings_in_parallel;
			std::string prefix = "subarray" + util::to_string( (int)(nn+1) ) + "_";

			if (nn > 0)
			{
				sa[nn].nstrings = 0;
				sa[nn].enable = as_boolean( prefix+"enable" );
				if (sa[nn].enable) sa[nn].nstrings = as_integer( prefix+"nstrings" );

				sa[0].nstrings -= sa[nn].nstrings;
				
				if (sa[nn].nstrings > 0 && sa[nn].enable)
					num_subarrays++;
			}

			// don't read in all the other variables
			// if the subarrays are disabled.
			if ( !sa[nn].enable )
				continue;
			
			size_t soil_len = 0;
			ssc_number_t *soiling = as_array(prefix+"soiling", &soil_len); // monthly soiling array
			if (soil_len != 12) throw exec_error( "pvsamv1", "soiling derate must have 12 values: subarray " + util::to_string((int)(nn+1)) );
			for (int k=0;k<12;k++)
				sa[nn].soiling[k] = (double) soiling[k];
	
			sa[nn].derate = as_double( prefix+"derate" );
			sa[nn].track_mode = as_integer( prefix+"track_mode"); // 0=fixed, 1=1axis, 2=2axis, 3=aziaxis

			sa[nn].tilt = wf.lat;
			if ( !lookup( prefix+"tilt_eq_lat" ) || !as_boolean( prefix+"tilt_eq_lat" ) )
				sa[nn].tilt = fabs( as_double( prefix+"tilt" ) );

			sa[nn].azimuth = as_double( prefix+"azimuth" );
			sa[nn].rotlim = as_double( prefix+"rotlim" );
			
			sa[nn].shade_mode_1x = as_integer( prefix+"shade_mode_1x" );
			if ( sa[nn].shade_mode_1x != 2 ) // shade_mode_1x=2 is no shading calculation for 1 axis trackers
			{
				sa[nn].gcr = as_double( prefix+"gcr" );

				if (sa[nn].gcr < 0.01 || sa[nn].gcr > 0.99)
					throw exec_error("pvsamv1", "array ground coverage ratio must obey 0.01 < gcr < 0.99");
			}

			sa[nn].shad_skydiff_factor = 1;
			if ( is_assigned( prefix+"shading_diff" ) )
				sa[nn].shad_skydiff_factor = as_double( prefix+"shading_diff" );
			
			sa[nn].shad_beam_factor.resize(8760);
			for (size_t j=0;j<8760;j++)
				sa[nn].shad_beam_factor[j] = 1.0;

			if ( is_assigned( prefix+"shading_hourly" ) )
			{
				size_t len = 0;
				ssc_number_t *vals = as_array( prefix+"shading_hourly", &len );
				if ( len == 8760 )
				{
					for ( size_t j=0;j<8760;j++)
						sa[nn].shad_beam_factor[j] = (double) vals[j];
				}
				else
					throw exec_error("pvsamv1", "hourly shading beam factors must have 8760 values: subarray " + util::to_string((int)(nn+1)));
			}

			if ( is_assigned( prefix+"shading_mxh" ) )
			{
				size_t nrows, ncols;
				ssc_number_t *mat = as_matrix( prefix+"shading_mxh", &nrows, &ncols );
				if ( nrows != 12 || ncols != 24 )
					throw exec_error("pvsamv1", "month x hour shading factors must have 12 rows and 24 columns: subarray " + util::to_string((int)(nn+1)));

				int c=0;
				for (int m=0;m<12;m++)
					for (int d=0;d<util::nday[m];d++)
						for (int h=0;h<24;h++)
							sa[nn].shad_beam_factor[c++] *= mat[ m*ncols + h ];
			}

			if ( is_assigned( prefix+"shading_azal" ) )
			{
				size_t nrows, ncols;
				ssc_number_t *mat = as_matrix( prefix+"shading_azal", &nrows, &ncols );
				if ( nrows < 3 || ncols < 3 )
					throw exec_error("pvsamv1", "azimuth x altitude shading factors must have at least 3 rows and 3 columns: subarray " + util::to_string((int)(nn+1)));

				sa[nn].shad_azal.resize_fill( nrows, ncols, 1.0 );
				for ( size_t r=0;r<nrows;r++ )
					for ( size_t c=0;c<ncols;c++ )
						sa[nn].shad_azal.at(r,c) = mat[r*ncols+c];

				if ( !check_azal_monotonic_increase( sa[nn].shad_azal ) )
					throw exec_error("pvsamv1", "azimuth x altitude shading factor table row 1 and col 1 must increase monotonically: subarray " + util::to_string((int)(nn+1)));

				sa[nn].en_azal = true;
			}
		}
		

		if (sa[0].nstrings < 0)
			throw exec_error("pvsamv1", "invalid string allocation between subarrays.  all subarrays must have zero or positive number of strings.");
		
		if (as_integer("self_shading_enabled") && sa[0].nstrings != strings_in_parallel)
			throw exec_error("pvsamv1", "Self-shading cannot be enabled for arrays with multiple subarrays.");

		// run some preliminary checks on inputs
		if (as_integer("self_shading_enabled") && as_integer("subarray1_track_mode"))
			throw exec_error( "pvsamv1", "Self-shading is enabled on the Shading page, but does not\n"
						  "work with the 1-axis, 2-axis, or azimuth tracking options.\n"
						  "To correct this problem, either disable self-shading on the\n"
						  "Shading page, or choose the Fixed tracking option on the\n"
						  "Array page.");

		if (as_integer("self_shading_enabled"))
		{
			int ncellx = as_integer("self_shading_ncellx");
			int ndiode = as_integer("self_shading_ndiode");
		
			if (ncellx%ndiode)
				throw exec_error( "pvsamv1", "Self-Shading Calculator: Division of Number of Cells Along Bottom of Module and Number of Diodes must be an integer.");
		}

		int mod_type = as_integer("module_model");
		
		spe_module_t spe;
		sandia_celltemp_t spe_tc;

		cec6par_module_t cec;
		noct_celltemp_t noct_tc;
		mcsp_celltemp_t mcsp_tc;

		sandia_module_t snl;
		sandia_celltemp_t snl_tc;

		pvcelltemp_t *celltemp_model = 0;
		pvmodule_t *module_model = 0;		

		double ref_area_m2 = 0;

		double self_shading_fill_factor = 0;

		double module_watts_stc = -1.0;

		bool enable_mismatch_vmax_calc = as_boolean("enable_mismatch_vmax_calc");
		if (enable_mismatch_vmax_calc 
			&& mod_type < 1 && mod_type > 2 )
			throw exec_error( "pvsamv1", "String level subarray mismatch can only be calculated using the 5 parameter model.");


		if ( mod_type == 0 )
		{
			spe.Area = as_double("spe_area");
			ref_area_m2 = spe.Area;
			for (int i=0;i<5;i++)
			{
				spe.Rad[i] = as_double( util::format("spe_rad%d", i));
				spe.Eff[i] = 0.01*as_double( util::format("spe_eff%d", i));
				if (i > 0 && spe.Rad[i] <= spe.Rad[i-1])
					throw exec_error( "pvsamv1", "SPE model radiation levels must increase monotonically");
			}
		
			spe.Gamma = as_double("spe_temp_coeff");
			spe.Reference = as_integer("spe_reference");
		
			switch(as_integer("spe_module_structure"))
			{
			case 0: //glass/cell/polymer sheet - open rack
				spe_tc.a = -3.56;
				spe_tc.b = -0.0750;
				spe_tc.DT0 = 3;
				break;
			case 1: //glass/cell/glass - open rack
				spe_tc.a = -3.47;
				spe_tc.b = -0.0594;
				spe_tc.DT0 = 3;
				break;
			case 2: //polymer/thin film/steel - open rack
				spe_tc.a = -3.58;
				spe_tc.b = -0.113;
				spe_tc.DT0 = 3;
				break;
			case 3: //Insulated back (building-integrated PV)
				spe_tc.a = -2.81;
				spe_tc.b = -0.0455;
				spe_tc.DT0 = 0;
				break;
			case 4: //close roof mount
				spe_tc.a = -2.98;
				spe_tc.b = -0.0471;
				spe_tc.DT0 = 1;
				break;
			case 5: //user defined
				spe_tc.a = as_double("spe_a");
				spe_tc.b = as_double("spe_b");
				spe_tc.DT0 = as_double("spe_dT");
				break;
			default:
				throw exec_error("pvsamv1", "invalid spe module structure and mounting");
			}

			spe.fd = as_double("spe_fd");
			spe_tc.fd = spe.fd;
			
			celltemp_model = &spe_tc;
			module_model = &spe;
			module_watts_stc = spe.WattsStc();
		}
		else if ( mod_type == 1 )
		{
			cec.Area = as_double("cec_area");
			ref_area_m2 = cec.Area;
			cec.Vmp = as_double("cec_v_mp_ref");
			cec.Imp = as_double("cec_i_mp_ref");
			cec.Voc = as_double("cec_v_oc_ref");
			cec.Isc = as_double("cec_i_sc_ref");
			cec.alpha_isc = as_double("cec_alpha_sc");
			cec.beta_voc = as_double("cec_beta_oc");
			cec.a = as_double("cec_a_ref");
			cec.Il = as_double("cec_i_l_ref");
			cec.Io = as_double("cec_i_o_ref");
			cec.Rs = as_double("cec_r_s");
			cec.Rsh = as_double("cec_r_sh_ref");
			cec.Adj = as_double("cec_adjust");

			self_shading_fill_factor = cec.Vmp * cec.Imp / cec.Voc / cec.Isc;

			if ( as_integer("cec_temp_corr_mode") == 0 )
			{
				noct_tc.Tnoct = as_double("cec_t_noct");
				int standoff = as_integer("cec_standoff");
				noct_tc.standoff_tnoct_adj = 0;
				switch(standoff)
				{
				case 2: noct_tc.standoff_tnoct_adj = 2; break; // between 2.5 and 3.5 inches
				case 3: noct_tc.standoff_tnoct_adj = 6; break; // between 1.5 and 2.5 inches
				case 4: noct_tc.standoff_tnoct_adj = 11; break; // between 0.5 and 1.5 inches
				case 5: noct_tc.standoff_tnoct_adj = 18; break; // less than 0.5 inches
					// note: all others, standoff_tnoct_adj = 0;
				}

				int height = as_integer("cec_height");
				noct_tc.ffv_wind = 0.51;
				if ( height == 1 )
					noct_tc.ffv_wind = 0.61;

				celltemp_model = &noct_tc;
			}
			else
			{
				/*	int MC; // Mounting configuration (1=rack,2=flush,3=integrated,4=gap)
					int HTD; // Heat transfer dimension (1=Module,2=Array)
					int MSO; // Mounting structure orientation (1=does not impede flow beneath, 2=vertical supports, 3=horizontal supports)
					int Nrows, Ncols; // number of modules in rows and columns, when using array heat transfer dimensions
					double Length; // module length, along horizontal dimension, (m)
					double Width; // module width, along vertical dimension, (m)
					double Wgap;  // gap width spacing (m)
					double TbackInteg; */

				mcsp_tc.DcDerate = sa[0].derate;  // TODO dc_derate needs to updated for each subarray
				mcsp_tc.MC = as_integer("cec_mounting_config")+1;
				mcsp_tc.HTD = as_integer("cec_heat_transfer")+1;
				mcsp_tc.MSO = as_integer("cec_mounting_orientation")+1;
				mcsp_tc.Wgap = as_double("cec_gap_spacing");
				mcsp_tc.Length = as_double("cec_module_length");
				mcsp_tc.Width = as_double("cec_module_width");
				mcsp_tc.Nrows = (int)as_integer("cec_array_rows");
				mcsp_tc.Ncols = (int)as_integer("cec_array_cols");
				mcsp_tc.TbackInteg = as_double("cec_backside_temp");

				celltemp_model = &mcsp_tc;
			}
			
			module_model = &cec;
			module_watts_stc = cec.Vmp * cec.Imp;
		}
		else if ( mod_type == 3 )
		{
			snl.A0 = as_double("snl_a0");
			snl.A1 = as_double("snl_a1");
			snl.A2 = as_double("snl_a2");
			snl.A3 = as_double("snl_a3");
			snl.A4 = as_double("snl_a4");
			snl.aImp = as_double("snl_aimp");
			snl.aIsc = as_double("snl_aisc");
			snl.Area = as_double("snl_area");
			ref_area_m2 = snl.Area;
			snl.B0 = as_double("snl_b0");
			snl.B1 = as_double("snl_b1");
			snl.B2 = as_double("snl_b2");
			snl.B3 = as_double("snl_b3");
			snl.B4 = as_double("snl_b4");
			snl.B5 = as_double("snl_b5");
			snl.BVmp0 = as_double("snl_bvmpo");
			snl.BVoc0 = as_double("snl_bvoco");
			snl.C0 = as_double("snl_c0");
			snl.C1 = as_double("snl_c1");
			snl.C2 = as_double("snl_c2");
			snl.C3 = as_double("snl_c3");
			snl.C4 = as_double("snl_c4");
			snl.C5 = as_double("snl_c5");
			snl.C6 = as_double("snl_c6");
			snl.C7 = as_double("snl_c7");
			snl.fd = as_double("snl_fd");
			snl.Imp0 = as_double("snl_impo");
			snl.Isc0 = as_double("snl_isco");
			snl.Ix0 = as_double("snl_ixo");
			snl.Ixx0 = as_double("snl_ixxo");
			snl.mBVmp = as_double("snl_mbvmp");
			snl.mBVoc = as_double("snl_mbvoc");
			snl.DiodeFactor = as_double("snl_n");
			snl.NcellSer = as_integer("snl_series_cells");
			snl.Vmp0 = as_double("snl_vmpo");
			snl.Voc0 = as_double("snl_voco");

			self_shading_fill_factor = snl.Vmp0 * snl.Imp0 / snl.Voc0 / snl.Isc0;

			// by default, use database values
			double A = as_double("snl_a");
			double B = as_double("snl_b");
			double DT = as_double("snl_dtc");
	
			switch(as_integer("snl_module_structure"))
			{
			case 1: //glass/cell/polymer sheet - open rack
				A = -3.56;
				B = -0.0750;
				DT = 3;
				break;
			case 2: //glass/cell/glass - open rack
				A = -3.47;
				B = -0.0594;
				DT = 3;
				break;
			case 3: //polymer/thin film/steel - open rack
				A = -3.58;
				B = -0.113;
				DT = 3;
				break;
			case 4: //Insulated back (building-integrated PV)
				A = -2.81;
				B = -0.0455;
				DT = 0;
				break;
			case 5: //close roof mount
				A = -2.98;
				B = -0.0471;
				DT = 1;
				break;
			case 6: //user defined
				A = as_double("snl_ref_a");
				B = as_double("snl_ref_b");
				DT = as_double("snl_ref_dT");
				break;

			default:
				break;
			}
		
			snl_tc.a = A;
			snl_tc.b = B;
			snl_tc.DT0 = DT;
			snl_tc.fd = snl.fd;
			
			celltemp_model = &snl_tc;
			module_model = &snl;
			module_watts_stc = snl.Vmp0 * snl.Imp0;
		}
		else if ( mod_type == 2 )
		{
			// calculate the 6 parameters
			// adjust TNOCT and FFV_wind

			int tech_id = module6par::monoSi;
			int type = as_integer("6par_celltech"); // "monoSi,multiSi,CdTe,CIS,CIGS,Amorphous"
			switch( type )
			{
			case 0: tech_id = module6par::monoSi; break;
			case 1: tech_id = module6par::multiSi; break;
			case 2: tech_id = module6par::CdTe; break;
			case 3: tech_id = module6par::CIS; break;
			case 4: tech_id = module6par::CIGS; break;
			case 5: tech_id = module6par::Amorphous; break;
			}

			double Vmp = as_double("6par_vmp");
			double Imp = as_double("6par_imp");
			double Voc = as_double("6par_voc");
			double Isc = as_double("6par_isc");
			double alpha = as_double("6par_aisc");
			double beta = as_double("6par_bvoc");
			double gamma = as_double("6par_gpmp");
			int nser = as_integer("6par_nser");
		
			module6par m(tech_id, Vmp, Imp, Voc, Isc, beta, alpha, gamma, nser, 298.15);
			int err = m.solve_with_sanity_and_heuristics<double>( 300, 1e-7 );

			if (err != 0)
				throw exec_error( "pvsamv1", "CEC 6 parameter model:  Could not solve for normalized coefficients.  Please check your inputs.");

			cec.Area = as_double("6par_area");
			ref_area_m2 = cec.Area;
			cec.Vmp = Vmp;
			cec.Imp = Imp;
			cec.Voc = Voc;
			cec.Isc = Isc;
			cec.alpha_isc = alpha;
			cec.beta_voc = beta;
			cec.a = m.a;
			cec.Il = m.Il;
			cec.Io = m.Io;
			cec.Rs = m.Rs;
			cec.Rsh = m.Rsh;
			cec.Adj = m.Adj;
		
			self_shading_fill_factor = cec.Vmp * cec.Imp / cec.Voc / cec.Isc;

			noct_tc.Tnoct = as_double("6par_tnoct");
			noct_tc.ffv_wind = 0.51; // less than 22ft high (1 story)
			if ( as_integer("6par_mounting") == 1 ) noct_tc.ffv_wind = 0.61;  // greater than 22ft high (2 story)
		
			int standoff = as_integer("6par_standoff"); // bipv,3.5in,2.5-3.5in,1.5-2.5in,0.5-1.5in,ground/rack
			noct_tc.standoff_tnoct_adj = 0;
			switch( standoff )
			{
			case 2: noct_tc.standoff_tnoct_adj = 2; break; // between 2.5 and 3.5 inches
			case 3: noct_tc.standoff_tnoct_adj = 6; break; // between 1.5 and 2.5 inches
			case 4: noct_tc.standoff_tnoct_adj = 11; break; // between 0.5 and 1.5 inches
			case 5: noct_tc.standoff_tnoct_adj = 18; break; // less than 0.5 inches
			}
			
			celltemp_model = &noct_tc;
			module_model = &cec;
			module_watts_stc = cec.Vmp * cec.Imp;
		}
		else
			throw exec_error("pvsamv1", "invalid pv module model type");

		
		double nameplate_kw = modules_per_string * strings_in_parallel * module_watts_stc/1000.0;

		::sandia_inverter_t snlinv;
		::partload_inverter_t plinv;

		int inv_type = as_integer("inverter_model");

		if (inv_type == 0) // cec database
		{
			snlinv.Paco = as_double("inv_snl_paco");
			snlinv.Pdco = as_double("inv_snl_pdco");
			snlinv.Vdco = as_double("inv_snl_vdco");
			snlinv.Pso = as_double("inv_snl_pso");
			snlinv.Pntare = as_double("inv_snl_pnt");
			snlinv.C0 = as_double("inv_snl_c0");
			snlinv.C1 = as_double("inv_snl_c1");
			snlinv.C2 = as_double("inv_snl_c2");
			snlinv.C3 = as_double("inv_snl_c3");
		}
		else if (inv_type == 1) // datasheet data
		{
			double eff_ds = as_double("inv_ds_eff")/100.0;
			snlinv.Paco = as_double("inv_ds_paco");
			if (eff_ds != 0)
				snlinv.Pdco = snlinv.Paco/eff_ds;
			else
				snlinv.Pdco = 0;
			snlinv.Vdco = as_double("inv_ds_vdco");
			snlinv.Pso = as_double("inv_ds_pso");
			snlinv.Pntare = as_double("inv_ds_pnt");
			snlinv.C0 = 0;
			snlinv.C1 = 0;
			snlinv.C2 = 0;
			snlinv.C3 = 0;
		}
		else if (inv_type == 2) // partload curve
		{
			plinv.Paco = as_double("inv_pd_paco");
			plinv.Pdco = as_double("inv_pd_pdco");
			plinv.Pntare = as_double("inv_pd_pnt");

			std::vector<double> pl_pd = as_doublevec("inv_pd_partload");
			std::vector<double> eff_pd = as_doublevec("inv_pd_efficiency");

			plinv.Partload = pl_pd;
			plinv.Efficiency = eff_pd;
		}
		else
		{
			throw exec_error("pvsamv1", "invalid inverter model type");
		}




		// self-shading
		selfshade_t sscalc;
		ssarrdat ssarr;
		ssarr.slope_ns = 0;
		bool self_shading_enabled = as_boolean("self_shading_enabled");
		if ( self_shading_enabled )
		{
			ssarr.azimuth = sa[0].azimuth;
			ssarr.tilt = sa[0].tilt;
			ssarr.length = as_double("self_shading_length");
			ssarr.width = as_double("self_shading_width");
			ssarr.row_space = as_double("self_shading_rowspace");
			ssarr.mod_space = 0; // no ui input - assumed 0 in trnsys inputs
			//ssarr.slope_ns = as_double("self_shading_slopens");
			//ssarr.slope_ew = as_double("self_shading_slopeew");
			// set to zero per 11/7/12 meeting
			ssarr.slope_ns = 0;
			ssarr.slope_ew = 0;
			ssarr.mod_orient = as_integer("self_shading_mod_orient");
			ssarr.str_orient = as_integer("self_shading_str_orient");
			ssarr.nmodx = as_integer("self_shading_nmodx");
			ssarr.nstrx = as_integer("self_shading_nstrx");
			ssarr.nmody = as_integer("self_shading_nmody");
			ssarr.nrows = as_integer("self_shading_nrows");
			ssarr.ncellx = as_integer("self_shading_ncellx");
			ssarr.ncelly = as_integer("self_shading_ncelly");
			ssarr.ndiode = as_integer("self_shading_ndiode");
			ssarr.mask_angle_calc_method = as_integer("self_shading_mask_angle_calc_method");

			sscalc = selfshade_t( ssarr );
		}



		// setup output arrays

		// self-shading
		ssc_number_t *p_ss_derate = allocate( "hourly_ss_derate", 8760 );
		ssc_number_t *p_ss_diffuse_derate = allocate( "hourly_ss_diffuse_derate", 8760 );
		ssc_number_t *p_ss_reflected_derate = allocate( "hourly_ss_reflected_derate", 8760 );
		ssc_number_t *p_ss_diffuse_loss = allocate( "hourly_ss_diffuse_loss", 8760 );

		ssc_number_t *p_glob = allocate( "hourly_glob_horiz_rad", 8760 );
		ssc_number_t *p_beam = allocate( "hourly_beam", 8760 );
		ssc_number_t *p_diff = allocate( "hourly_diff", 8760 );
		ssc_number_t *p_wspd = allocate( "hourly_windspd", 8760 );
		ssc_number_t *p_tdry = allocate( "hourly_ambtemp", 8760 );
		ssc_number_t *p_albedo = allocate( "hourly_albedo", 8760 );

		ssc_number_t *p_solzen = allocate("hourly_sol_zen", 8760);
		ssc_number_t *p_solalt = allocate("hourly_sol_alt", 8760);
		ssc_number_t *p_solazi = allocate("hourly_sol_azi", 8760);
		ssc_number_t *p_airmass = allocate("hourly_airmass", 8760);
		ssc_number_t *p_sunup = allocate("hourly_sunup", 8760);
		
		ssc_number_t *p_aoi[4];
		ssc_number_t *p_surftilt[4];  
		ssc_number_t *p_surfazi[4];   
		ssc_number_t *p_rot[4];       
		ssc_number_t *p_idealrot[4];
		ssc_number_t *p_poanom[4];
		ssc_number_t *p_poashaded[4];
		ssc_number_t *p_poaeffbeam[4];   
		ssc_number_t *p_poaeffdiff[4];   
		ssc_number_t *p_poaeff[4];  
		ssc_number_t *p_soiling[4];   
		ssc_number_t *p_shad[4];      	
		ssc_number_t *p_tcell[4];     
		ssc_number_t *p_modeff[4];    
		ssc_number_t *p_dcv[4];
		ssc_number_t *p_dcsubarray[4];

		for (int nn=0;nn<4;nn++)
		{
			std::string prefix = "hourly_subarray" + util::to_string( (int)(nn+1) ) + "_";
			p_aoi[nn]        = allocate( prefix+"aoi", 8760 );
			p_surftilt[nn]   = allocate( prefix+"surf_tilt", 8760);
			p_surfazi[nn]    = allocate( prefix+"surf_azi", 8760);		
			p_rot[nn]        = allocate( prefix+"axisrot", 8760 );
			p_idealrot[nn]   = allocate( prefix+"idealrot", 8760);
			p_poanom[nn]     = allocate( prefix+"poa_nom", 8760);
			p_poashaded[nn]  = allocate( prefix+"poa_shaded", 8760);
			p_poaeffbeam[nn]    = allocate( prefix+"poa_eff_beam", 8760 );
			p_poaeffdiff[nn]    = allocate( prefix+"poa_eff_diff", 8760 );
			p_poaeff[nn]   = allocate( prefix+"poa_eff", 8760 );		
			p_soiling[nn]    = allocate( prefix+"soiling_derate", 8760);
			p_shad[nn]       = allocate( prefix+"beam_shading_factor", 8760 );
			p_tcell[nn]      = allocate( prefix+"celltemp", 8760 );
			p_modeff[nn]     = allocate( prefix+"modeff", 8760 );
			p_dcv[nn]        = allocate( prefix+"dc_voltage", 8760 );
			p_dcsubarray[nn] = allocate( prefix+"dc_gross", 8760 );
		}
				
		ssc_number_t *p_inv_dc_voltage = allocate( "hourly_inverter_dc_voltage", 8760 );
		ssc_number_t *p_inrad = allocate( "hourly_input_radiation", 8760 );
		ssc_number_t *p_inradbeam = allocate( "hourly_input_radiation_beam", 8760 );
		ssc_number_t *p_poanomhr = allocate( "hourly_poa_nom", 8760 );
		ssc_number_t *p_poashadedhr = allocate("hourly_poa_shaded", 8760 );
		ssc_number_t *p_poaeffhr = allocate("hourly_poa_eff", 8760 );
		ssc_number_t *p_dcgross = allocate( "hourly_dc_gross", 8760 );
		ssc_number_t *p_dcpwr = allocate( "hourly_dc_net", 8760 );
		ssc_number_t *p_acgross = allocate( "hourly_ac_gross", 8760 );
		ssc_number_t *p_acpwr = allocate( "hourly_ac_net", 8760 );
		ssc_number_t *p_inveff = allocate( "hourly_inv_eff", 8760 );
		ssc_number_t *p_invcliploss = allocate( "hourly_inv_cliploss", 8760 );
		ssc_number_t *p_invpsoloss = allocate( "hourly_inv_psoloss", 8760 );
		ssc_number_t *p_invpntloss = allocate( "hourly_inv_pntloss", 8760 );





		int istep = 0, nstep = wf.nrecords;
		while( istep < 8760 )
		{
			if (!wf.read())
				throw exec_error("pvsamv1", "could not read data line " + util::to_string(istep+1) + " of 8760 in weather file");


// causing AV in all wrapppers - default handler not working through wrapper layer
			if ( istep % (nstep/20) == 0)
				update( "calculating", 100.0f * ((float)istep) / ((float)nstep), (float)istep );

		
			double solazi=0, solzen=0, solalt=0;
			int sunup = 0;
			double dcpwr_gross = 0.0, dcpwr_net = 0.0, dc_string_voltage = 0.0;
			double inprad_total = 0.0;
			double inprad_beam = 0.0;
			double poa_nom_hr = 0.0;
			double poa_shaded_hr = 0.0;
			double alb = 0.2;
			double poa_eff_hr = 0.0;
								
			int month_idx = wf.month - 1;

			if (use_wf_alb && wf.albedo >= 0 && wf.albedo <= 1)
				alb = wf.albedo;
			else if ( month_idx >= 0 && month_idx < 12 )
				alb = alb_array[month_idx];
			else
				throw exec_error( "pvsamv1", 
						util::format("Error retrieving albedo value: Invalid month in weather file or invalid albedo value in weather file"));
	
			// calculate incident irradiance on each subarray
			for (int nn=0;nn<4;nn++)
			{
				if ( !sa[nn].enable
					|| sa[nn].nstrings < 1 )
					continue; // skip disabled subarrays
				
				irrad irr;
				irr.set_time( wf.year, wf.month, wf.day, wf.hour, wf.minute, wf.step / 3600.0 );
				irr.set_location( wf.lat, wf.lon, wf.tz );
			
				irr.set_sky_model( skymodel, alb );
				if ( radmode == 0 ) irr.set_beam_diffuse( wf.dn, wf.df );
				else if (radmode == 1) irr.set_global_beam( wf.gh, wf.dn );

				double gcr = sa[nn].gcr;
				double btwidth = 1.0;
				double btspace = btwidth/gcr - btwidth; // convert from gcr and assumed width to spacing (edge-to-edge btw rows)  recall:  GCR = W/(W+S)
				if ( sa[nn].shade_mode_1x != 1 )
					btwidth = btspace = -1;

				irr.set_surface( sa[nn].track_mode,
					sa[nn].tilt,
					sa[nn].azimuth,
					sa[nn].rotlim,
					btwidth,
					btspace );

				int code = irr.calc();
				if ( code != 0 )
					throw exec_error( "pvsamv1", 
						util::format("failed to compute irradiation on surface %d (code: %d) [y:%d m:%d d:%d h:%d]", 
						nn+1, code, wf.year, wf.month, wf.day, wf.hour));

				double ibeam, iskydiff, ignddiff;
				double aoi, stilt, sazi, rot, btd;

				irr.get_sun( &solazi, &solzen, &solalt, 0, 0, 0, &sunup, 0, 0, 0 );
				irr.get_angles( &aoi, &stilt, &sazi, &rot, &btd );
				irr.get_poa( &ibeam, &iskydiff, &ignddiff, 0, 0, 0);
				
				// record sub-array plane of array output before computing shading and soiling
				p_poanom[nn][istep] = (ssc_number_t) ( (ibeam + iskydiff + ignddiff) * 0.001 );
				
				//record sub-array contribution to total plane of array for this hour
				poa_nom_hr += p_poanom[nn][istep]* ref_area_m2 * modules_per_string * sa[nn].nstrings;

				//accumulate monthly nominal poa
				sa[nn].monthly_poa_nom[ month_idx ] += ( (ibeam+iskydiff+ignddiff) * 0.001 );

				double beam_shad_factor = sa[nn].shad_beam_factor[istep]; // variable to accumulate total beam shading derate for this hour

				if ( sunup > 0 && sa[nn].track_mode == 1
					&& sa[nn].shade_mode_1x == 0 )
				{	
					arr1x_data arr1x;
					arr1x.width = 1.0;
					arr1x.row_spacing = 1.0/gcr - 1.0; 
					arr1x.length = 10*arr1x.width; // assume long trackers
					arr1x.axis_azimuth = sa[nn].azimuth;
					arr1x.axis_tilt = sa[nn].tilt;
					arr1x.rotlim = sa[nn].rotlim;
					arr1x.solazi = solazi;
					arr1x.solzen = solzen;

					double shad1xf = shade_fraction_1x( arr1x, rot );

					ibeam *= (1-shad1xf);
					beam_shad_factor *= (1-shad1xf);

					if ( sa[nn].shade_mode_1x == 0 && iskydiff > 0 )
					{
						// view factor calculations assume isotropic sky
						double Gd = iskydiff+ignddiff; // total plane-of-array diffuse
						double Gdh = Gd * 2/(1+cosd(stilt)); // total
						double Gbh = wf.dn * cosd( solzen ); // beam irradiance on horizontal surface
						
						// include diffuse reduction due to view factor
						double B = arr1x.width; // row width
						double R = arr1x.width + arr1x.row_spacing; // center-to-center row spacing

						// worst-case mask angle using calculated surface tilt
						double phi0 = atan2( B*sind( stilt ), R - B*cosd( stilt ) );

						// sky diffuse reduction
						double reduced_skydiff = Gd - Gdh*(1 - pow( cos(phi0/2), 2 ));
						double Fskydiff = reduced_skydiff / Gd;
						

						// ground reflected reduction is not determined yet
						double F1 = alb * pow( sind(stilt/2.0), 2);
						double Y1 = R - B * sind( 180.0 - solalt - stilt ) / sind(solalt);
						Y1 = max(0.00001, Y1); // constraint per Chris 4/23/12
						double F2 = 0.5 * alb * ( 1.0 + Y1/B - sqrt( pow(Y1,2)/pow(B,2) - 2*Y1/B * cosd(180 - stilt) + 1.0 ) );
						double F3 = 0.5 * alb * ( 1.0 + R/B - sqrt( pow(R,2)/pow(B,2) - 2*R/B * cosd(180 - stilt) + 1.0 ) );
						
						double Gr1 = F1 * (Gbh + Gdh);
						double nr = 1000; // some large number of rows;
						double reduced_gnddiff = ( (F1 + (nr-1)*F2)/ nr ) * Gbh
							+ ( (F1 + (nr-1) * F3)/ nr ) * Gdh;

						double Fgnddiff = 1.0;
						if ( Gr1 > 0 )
							Fgnddiff = reduced_gnddiff / Gr1;

						if ( Fskydiff >= 0 && Fskydiff <= 1 ) iskydiff *= Fskydiff;
						else log( util::format("sky diffuse reduction factor invalid at hour %d: fskydiff=%lg, stilt=%lg", istep, Fskydiff, stilt), SSC_NOTICE, (float)istep );

						if ( Fgnddiff >= 0 && Fgnddiff <= 1 ) ignddiff *= Fgnddiff;
						else log( util::format("gnd diffuse reduction factor invalid at hour %d: fgnddiff=%lg, stilt=%lg", istep, Fgnddiff, stilt), SSC_NOTICE, (float)istep );
					}
				}
				
				// apply hourly shading factors to beam (if none enabled, factors are 1.0)
				ibeam *= sa[nn].shad_beam_factor[istep];	
				
				// apply beam shading based on solar azimuth/altitude table
				if ( sa[nn].en_azal )
				{
					double factor = util::bilinear( solalt, solazi, sa[nn].shad_azal );		
					ibeam *= factor;
					beam_shad_factor *= factor;
				}
				
				// apply sky diffuse shading factor (specified as constant, nominally 1.0 if disabled in UI)
				iskydiff *= sa[nn].shad_skydiff_factor;

				// record sub-array level output after shading, before soiling
				p_poashaded[nn][istep] = (ssc_number_t) ( (ibeam + iskydiff + ignddiff) * 0.001 );

				//record sub-array contribution to total shaded plane of array for this hour
				poa_shaded_hr += p_poashaded[nn][istep]* ref_area_m2 * modules_per_string * sa[nn].nstrings;

				// apply soiling derate to all components of irradiance
//				int month_idx = wf.month - 1;
				double soiling_factor = 1.0;
				if ( month_idx >= 0 && month_idx < 12 )
				{
					soiling_factor = sa[nn].soiling[month_idx];
					ibeam *= soiling_factor;
					iskydiff *= soiling_factor;
					ignddiff *= soiling_factor;
					beam_shad_factor *= soiling_factor;
				}

				// record sub-array level outputs				
				p_poaeffbeam[nn][istep] = (ssc_number_t) ( ibeam * 0.001 );
				p_poaeffdiff[nn][istep] = (ssc_number_t) ( (iskydiff+ignddiff) * 0.001 );
				p_poaeff[nn][istep] = (ssc_number_t) ( (ibeam+iskydiff+ignddiff) * 0.001 );
				p_shad[nn][istep] = (ssc_number_t) beam_shad_factor;
				p_rot[nn][istep] = (ssc_number_t) rot;
				p_idealrot[nn][istep] = (ssc_number_t) ( rot-btd );
				p_aoi[nn][istep] = (ssc_number_t) aoi;
				p_surftilt[nn][istep] = (ssc_number_t) stilt;
				p_surfazi[nn][istep] = (ssc_number_t) sazi;
				p_soiling[nn][istep] = (ssc_number_t) soiling_factor;

				// save the required irradiance inputs on array plane for the module output calculations.
				sa[nn].poa.ibeam = ibeam;
				sa[nn].poa.iskydiff = iskydiff;
				sa[nn].poa.ignddiff = ignddiff;
				sa[nn].poa.aoi = aoi;
				sa[nn].poa.sunup = sunup;
				sa[nn].poa.stilt = stilt;
				sa[nn].poa.sazi = sazi;

				// accumulate hourly incident total
				poa_eff_hr += p_poaeff[nn][istep]* ref_area_m2 * modules_per_string * sa[nn].nstrings;

				// accumulate monthly incident total & beam
				sa[nn].monthly_poa_eff[ month_idx ] += ( (ibeam+iskydiff+ignddiff) * 0.001 );
				sa[nn].monthly_poa_eff_beam[ month_idx ] += ( ibeam * 0.001 );
								
				// calculate total input radiaton to system (array-level output)
				inprad_total += (sa[nn].poa.ibeam + sa[nn].poa.iskydiff + sa[nn].poa.ignddiff)
					* ref_area_m2 * 0.001 * modules_per_string * sa[nn].nstrings;

				inprad_beam += sa[nn].poa.ibeam * ref_area_m2 * 0.001 * modules_per_string * sa[nn].nstrings;
			}



			// apply self-shading if enabled (subarray 1 only)
			if ( self_shading_enabled )
			{
				if ( sscalc.exec( solzen, solazi, 
						wf.dn, 
						sa[0].poa.ibeam, 
						(sa[0].poa.iskydiff+sa[0].poa.ignddiff), 
						self_shading_fill_factor, 
						alb ) )
				{
					p_ss_diffuse_derate[istep] = (ssc_number_t) sscalc.diffuse_derate();
					p_ss_reflected_derate[istep] = (ssc_number_t) sscalc.reflected_derate();
					p_ss_diffuse_loss[istep] = (ssc_number_t) sscalc.m_diffuse_loss_term/1000;
					p_ss_derate[istep] = (ssc_number_t) sscalc.dc_derate();

					// only diffuse irradiance for subarray 1 affected (other subarrays disabled)
					sa[0].poa.iskydiff *= sscalc.diffuse_derate();
					sa[0].poa.ignddiff *= sscalc.reflected_derate();
				}
				else
					throw exec_error("pvsamv1", util::format("Self-shading calculation failed at hour %d", istep) ) ;

			}
			

			// compute dc power output of one module in each subarray
			double module_voltage = -1;

			if ( enable_mismatch_vmax_calc && num_subarrays > 1 )
			{
				double vmax = module_model->VocRef()*1.3; // maximum voltage
				double vmin = 0.4 * vmax; // minimum voltage
				const int NP=100;
				double V[NP], I[NP], P[NP];
				double Pmax = 0;
				// sweep voltage, calculating current for each subarray module, and adding
				for( int i=0;i<NP;i++)
				{
					V[i] = vmin + (vmax-vmin)*i/((double)NP);
					I[i] = 0;
					for (int nn=0;nn<4;nn++)
					{
						if ( !sa[nn].enable	|| sa[nn].nstrings < 1 ) continue; // skip disabled subarrays

						pvinput_t in( sa[nn].poa.ibeam, sa[nn].poa.iskydiff, sa[nn].poa.ignddiff, 
							wf.tdry, wf.tdew, wf.wspd, wf.wdir, wf.pres, 
							solzen, sa[nn].poa.aoi, wf.elev, 
							sa[nn].poa.stilt, sa[nn].poa.sazi, 
							((double)wf.hour) + wf.minute/60.0 );
						pvoutput_t out(0, 0, 0, 0, 0, 0, 0);
						if ( sa[nn].poa.sunup > 0 )
						{	
							double tcell = wf.tdry;
							// calculate cell temperature using selected temperature model
							(*celltemp_model)( in, *module_model, V[i], tcell );
							// calculate module power output using conversion model previously specified
							(*module_model)( in, tcell, V[i], out );
						}
						I[i] += out.Current;
					}

					P[i] = V[i]*I[i];
					if (P[i] > Pmax)
					{
						Pmax = P[i];
						module_voltage = V[i];
					}
				}
			}
			

			//  at this point we have 
			// a array maximum power module voltage

			// for averaging voltage in the case that mismatch calcs are disabled.
			int n_voltage_values = 0; 
			double voltage_sum = 0.0;

			for (int nn=0;nn<4;nn++)
			{
				if ( !sa[nn].enable
					|| sa[nn].nstrings < 1 )
					continue; // skip disabled subarrays
				
				pvinput_t in( sa[nn].poa.ibeam, sa[nn].poa.iskydiff, sa[nn].poa.ignddiff, 
					wf.tdry, wf.tdew, wf.wspd, wf.wdir, wf.pres, 
					solzen, sa[nn].poa.aoi, wf.elev, 
					sa[nn].poa.stilt, sa[nn].poa.sazi, 
					((double)wf.hour) + wf.minute/60.0 );
				pvoutput_t out(0, 0, 0, 0, 0, 0, 0);

				double tcell = wf.tdry;
				if ( sa[nn].poa.sunup > 0 )
				{	
					// calculate cell temperature using selected temperature model
					// calculate module power output using conversion model previously specified
					(*celltemp_model)( in, *module_model, module_voltage, tcell );
					(*module_model)( in, tcell, module_voltage, out );
				}

				// save DC module outputs for this subarray
				sa[nn].module.dcpwr = out.Power;
				sa[nn].module.dceff = out.Efficiency*100;
				sa[nn].module.dcv = out.Voltage;
				sa[nn].module.tcell = out.CellTemp;

				voltage_sum += out.Voltage;
				n_voltage_values++;
			}
			

			if ( enable_mismatch_vmax_calc && num_subarrays > 1 )
				dc_string_voltage = module_voltage * modules_per_string;
			else // when mismatch calculation is disabled and subarrays are enabled, simply average the voltages together for the inverter input
				dc_string_voltage = voltage_sum / n_voltage_values * modules_per_string;
			
			// sum up all DC power from the whole array
			for (int nn=0;nn<4;nn++)
			{
				if ( !sa[nn].enable
					|| sa[nn].nstrings < 1 )
					continue; // skip disabled subarrays

				// apply self-shading derate
				if (self_shading_enabled && nn == 0) sa[nn].module.dcpwr *= p_ss_derate[istep];
				
				// scale power and voltage to array dimensions
				sa[nn].module.dcpwr *=  modules_per_string*sa[nn].nstrings;

				// apply pre-inverter power derate
				dcpwr_gross += sa[nn].module.dcpwr;
				dcpwr_net += sa[nn].module.dcpwr * sa[nn].derate;
				
				// save to SSC output arrays
				p_tcell[nn][istep] = (ssc_number_t) sa[nn].module.tcell;
				p_modeff[nn][istep] = (ssc_number_t) sa[nn].module.dceff;
				p_dcv[nn][istep] = (ssc_number_t) sa[nn].module.dcv * modules_per_string;
				p_dcsubarray[nn][istep] = (ssc_number_t) ( sa[nn].module.dcpwr * 0.001 );		
			}
			
			// inverter: runs at all hours of the day, even if no DC power.  important
			// for capturing tare losses			
			double acpwr_gross=0, aceff=0, pntloss=0, psoloss=0, cliploss=0;
			if (( inv_type == 0 ) || ( inv_type == 1 ))
			{
				double _par, _plr;
				snlinv.acpower( dcpwr_net/num_inverters, dc_string_voltage, 
					&acpwr_gross, &_par, &_plr, &aceff, &cliploss, &psoloss, &pntloss );
				acpwr_gross *= num_inverters;
				aceff *= 100;
			}
			else if ( inv_type == 2 )
			{
				double _par, _plr;
				plinv.acpower( dcpwr_net/num_inverters,	&acpwr_gross, &_par, &_plr, &aceff, &cliploss, &pntloss );
				acpwr_gross *= num_inverters;
				aceff *= 100;
			}
			
			// save array-level outputs		
			p_beam[istep] = (ssc_number_t) (wf.dn * 0.001);
			// calculate global if beam & diffuse are selected as inputs
			if (radmode == 0)
				p_glob[istep] = (ssc_number_t) (0.001 * (wf.df + wf.dn * cos( solzen*3.1415926/180 )));
			else
				p_glob[istep] = (ssc_number_t)( wf.gh * 0.001);
			// calculate diffuse if total & beam are selected as inputs
			if (radmode == 1)
				p_diff[istep] = (ssc_number_t) (0.001 * (wf.gh - wf.dn * cos( solzen*3.1415926/180 )));
			else
				p_diff[istep] = (ssc_number_t) (wf.df * 0.001);

			p_wspd[istep] = (ssc_number_t) wf.wspd;
			p_tdry[istep] = (ssc_number_t) wf.tdry;
			p_albedo[istep] = (ssc_number_t) alb;

			p_solzen[istep] = (ssc_number_t) solzen;
			p_solalt[istep] = (ssc_number_t) solalt;
			p_solazi[istep] = (ssc_number_t) solazi;
			
			// absolute relative airmass calculation as f(zenith angle, site elevation)
			p_airmass[istep] = (ssc_number_t) ( exp(-0.0001184 * wf.elev)/(cos( solzen*3.1415926/180 )+0.5057*pow(96.080-solzen, -1.634)) );
			p_sunup[istep] = (ssc_number_t) sunup;
						
			p_inrad[istep] = (ssc_number_t) inprad_total;
			p_inradbeam[istep] = (ssc_number_t) inprad_beam;
			p_poanomhr[istep] = (ssc_number_t) poa_nom_hr;
			p_poashadedhr[istep] = (ssc_number_t) poa_shaded_hr;
			p_poaeffhr[istep] = (ssc_number_t) poa_eff_hr;

			p_inv_dc_voltage[istep] = (ssc_number_t) dc_string_voltage;
			p_dcgross[istep] = (ssc_number_t) ( dcpwr_gross * 0.001 );
			p_dcpwr[istep] = (ssc_number_t) ( dcpwr_net * 0.001 );

			p_acgross[istep] = (ssc_number_t) ( acpwr_gross * 0.001 );
			p_acpwr[istep] = (ssc_number_t) ( acpwr_gross*ac_derate * 0.001 );
			p_inveff[istep] = (ssc_number_t) ( aceff );
			p_invcliploss[istep] = (ssc_number_t) ( cliploss );
			p_invpsoloss[istep] = (ssc_number_t) ( psoloss );
			p_invpntloss[istep] = (ssc_number_t) ( pntloss );

			istep++;

		}



		if (istep != 8760)
			throw exec_error( "pvsamv1", "failed to simulate all 8760 hours, error in weather file?");
	
		// calculate monthly_poa_nom, monthly_poa_eff, monthly_poa_eff_beam
		// sum up monthly_inc for each subarray.  weight total it by relative area of each subarray to total pv system area.
		ssc_number_t *p_monthly_poa_nom = allocate ("monthly_poa_nom", 12);
		ssc_number_t *p_monthly_poa_eff = allocate( "monthly_poa_eff", 12 );
		ssc_number_t *p_monthly_poa_eff_beam = allocate( "monthly_poa_eff_beam", 12 );

		for (int i=0;i<12;i++)
		{
			double nom = 0;
			double total = 0;
			double beam = 0;
			double num_strings = 0;
			for (int nn=0;nn<4;nn++)
			{
				if (sa[nn].enable && sa[nn].nstrings > 0)
				{
					nom += sa[nn].monthly_poa_nom[i] * sa[nn].nstrings;
					total += sa[nn].monthly_poa_eff[i] * sa[nn].nstrings;
					beam += sa[nn].monthly_poa_eff_beam[i] * sa[nn].nstrings;
					num_strings += sa[nn].nstrings;
				}
			}

			p_monthly_poa_nom[i] = (ssc_number_t) (nom / num_strings );
			p_monthly_poa_eff[i] = (ssc_number_t) ( total / num_strings );
			p_monthly_poa_eff_beam[i] = (ssc_number_t) ( beam / num_strings );
		}

		accumulate_monthly( "hourly_ac_net", "monthly_ac_net" );
		accumulate_monthly( "hourly_dc_net", "monthly_dc_net" );

		accumulate_annual( "hourly_glob_horiz_rad", "annual_glob_horiz_rad" );
		accumulate_annual( "hourly_input_radiation", "annual_input_radiation" );
		accumulate_annual( "hourly_input_radiation_beam", "annual_input_radiation_beam" );
		accumulate_annual( "hourly_poa_nom", "annual_poa_nom" );
		accumulate_annual( "hourly_poa_shaded", "annual_poa_shaded" );
		accumulate_annual( "hourly_poa_eff", "annual_poa_eff" );
		accumulate_annual( "hourly_dc_gross", "annual_dc_gross" );
		accumulate_annual( "hourly_dc_net", "annual_dc_net" );
		accumulate_annual( "hourly_ac_gross", "annual_ac_gross" );
		accumulate_annual( "hourly_ac_net", "annual_ac_net" );

		accumulate_annual( "hourly_inv_cliploss", "annual_inv_cliploss" );
		accumulate_annual( "hourly_inv_psoloss", "annual_inv_psoloss" );
		accumulate_annual( "hourly_inv_pntloss", "annual_inv_pntloss" );


	
		bool is_cpv = false;

		if ( as_integer("module_model") == 3 /* sandia model */
			&&  as_double("snl_fd") == 0 )
			is_cpv = true;

		double inp_rad = is_cpv ? as_double("annual_input_radiation_beam") : as_double("annual_input_radiation");
		double ac_net = as_double("annual_ac_net");
		double mod_eff = module_eff();

		// calculate system performance factor
		// reference: (http://files.sma.de/dl/7680/Perfratio-UEN100810.pdf)
		// PR = net_ac (kWh) / ( total input radiation (kWh) * stc efficiency (%) )
		assign( "annual_performance_factor", var_data( (ssc_number_t)( ac_net / ( inp_rad * mod_eff/100.0 ) ) ) );

		// calculate nominal dc input
		assign( "annual_dc_nominal", var_data( (ssc_number_t)( inp_rad * mod_eff/100.0 ) ) );

		assign( "nameplate_dc_rating", var_data( (ssc_number_t)nameplate_kw ) );

		inverter_vdcmax_check();
		
		inverter_size_check();
		
		assign( "6par_a", var_data((ssc_number_t) cec.a) );
		assign( "6par_Io", var_data((ssc_number_t) cec.Io) );
		assign( "6par_Il", var_data((ssc_number_t) cec.Il) );
		assign( "6par_Rs", var_data((ssc_number_t) cec.Rs) );
		assign( "6par_Rsh", var_data((ssc_number_t) cec.Rsh) );
		assign( "6par_Adj", var_data((ssc_number_t) cec.Adj) );
	}
	
	bool check_azal_monotonic_increase( const util::matrix_t<double> &azal )
	{
		if (azal.nrows() < 3 || azal.ncols() < 3) return false;

		for (size_t i=2;i<azal.nrows();i++)
			if (azal.at(i,0) < azal.at(i-1,0))				
				return false;

		for (size_t i=2;i<azal.ncols();i++)
			if (azal.at(0,i) < azal.at(0,i-1))
				return false;

		return true;
	}


	double module_eff()
	{
		double eff = -1;
	
		int mod_type = as_integer("module_model");
		switch (mod_type)
		{
		case 0: // SPE
			eff = as_double( util::format("spe_eff%d", as_integer("spe_reference")) );
			break;
		case 1: // CEC
			{
				double a_c = as_double("cec_area");
				double i_noct = 1000; // as_double("cec_i_noct");
				double v_mp_ref = as_double("cec_v_mp_ref");
				double i_mp_ref = as_double("cec_i_mp_ref");

				if (a_c == 0) a_c = -1;
			//	if (i_noct == 0) i_noct = 1000.0;

				eff = 100.0 * (( v_mp_ref * i_mp_ref ) / a_c) / i_noct;
			}
			break;
		case 2: // 6par user entered
			{
				double area = as_double("6par_area");
				double vmp = as_double("6par_vmp");
				double imp = as_double("6par_imp");
				if (area == 0) area = 1;
				eff = 100.0 * ((vmp*imp)/area)/1000.0;
			}
			break;
		case 3: // Sandia
			{
				double area = as_double("snl_area");
				double vmpo = as_double("snl_vmpo");
				double impo = as_double("snl_impo");

				eff = vmpo*impo;
				if (area > 0)
					eff = eff/area;
				eff = eff / 1000.0;
				eff = eff * 100;
			}
			break;
		}

		if (eff == 0.0) eff = -1;
		return eff;
	}

	void inverter_vdcmax_check()
	{
		// check that no hourly vmp values exceed Vdcmax
		// add max value and number of times > Vdcmax
		int numVmpGTVdcmax = 0;
		double maxVmp=0;
		int maxVmpHour=0;
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
			default:
				// message
				return;
		}

		// warning on inverter page
		if (vdcmax <=0) return;

		size_t count;
		ssc_number_t *da = as_array("hourly_inverter_dc_voltage", &count);
		if (count == 8760)
		{
			for (size_t i=0; i < count;i++)
			{
				if (da[i] > vdcmax)
				{
					numVmpGTVdcmax++;
					if (da[i] > maxVmp) 
					{
						maxVmp = da[i];
						maxVmpHour = i;
					}
				}
			}
		}

		if (numVmpGTVdcmax > 0) 
		{
			log( util::format( "Module array voltage Vmp exceeds the Vdcmax (%.2lfV) of inverter %d times.\n"
					"   The maximum Vmp value is %.2lfV at hour %d.\n"
					"   It is recommended that you reduce the number of modules per string.", vdcmax, numVmpGTVdcmax, maxVmp, maxVmpHour ),
					SSC_WARNING );
		}
	}

	void inverter_size_check()
	{
		// undersized - check that no hourly output exceeds the rated output of the inverter
		// 9/26/10 note that e_net automatically clipped - must look at derated dc power
		// oversized - add max output > 75% of inverter ourput
		ssc_number_t *acPower;
		size_t acCount;
		ssc_number_t *dcPower;
		size_t dcCount;
		int numHoursClipped = 0;
		double maxACOutput=0;
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
				ratedDCOutput = as_double("inv_ds_eff")/100.0;
				if (ratedDCOutput != 0) ratedDCOutput = ratedACOutput/ratedDCOutput;
				break;
			case 2: // partload curve
				ratedACOutput = as_double("inv_pd_paco");
				ratedDCOutput = as_double("inv_pd_pdco");
				break;
			default:
				// message
				return;
		}
		ratedACOutput *= numInv;
		ratedDCOutput *= numInv;

		if ((ratedACOutput <= 0) || (ratedDCOutput <= 0)) return;

		ratedACOutput = ratedACOutput / 1000.0; // W to kW to compare to hourly output
		ratedDCOutput = ratedDCOutput / 1000.0; // W to kW to compare to hourly output

		acPower = as_array("hourly_ac_net", &acCount);
		dcPower = as_array("hourly_dc_net", &dcCount);
		if ((acCount == 8760) && (dcCount == 8760))
		{
			for (size_t i=0; i < acCount;i++)
			{
				if (dcPower[i] > ratedDCOutput) numHoursClipped++;
				if (acPower[i] > maxACOutput) maxACOutput = acPower[i]; 
			}
		}
		if (numHoursClipped > 0) 
			log( util::format("Inverter undersized: The array output exceeded the inverter rating %.2lf kWdc for %d hours.", 
				ratedDCOutput, numHoursClipped), 
				SSC_WARNING );

		if ((maxACOutput < 0.75 * ratedACOutput) && (maxACOutput > 0))
			log( util::format("Inverter oversized: The maximum inverter output was %.2lf%% of the rated value %lg kWac.", 
				100 * maxACOutput / ratedACOutput, ratedACOutput), 
				SSC_WARNING);
	}
};

DEFINE_MODULE_ENTRY( pvsamv1, "Photovoltaic performance model, SAM component models V.1", 1 )
