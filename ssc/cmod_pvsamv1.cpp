
#include <string>
#include <cmath>
#include <limits>
#include <vector>

#include "core.h"

#include "lib_weatherfile.h"
#include "lib_irradproc.h"
#include "lib_cec6par.h"
#include "lib_sandia.h"
#include "6par_jacobian.h"
#include "6par_lu.h"
#include "6par_search.h"
#include "6par_newton.h"
#include "6par_gamma.h"
#include "6par_solve.h"
#include "lib_pvshade.h"

inline double to_double(double x) { return x; }

static var_info _cm_vtab_pvsamv1[] = {
/*   VARTYPE           DATATYPE         NAME                                            LABEL                                                   UNITS      META                             GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "weather_file",                                "Weather file in TMY2, TMY3, or EPW.",                   "",       "",                              "pvsamv1",              "*",                        "LOCAL_FILE",                    "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "use_wf_albedo",                               "Use albedo in weather file if provided",                "0/1",    "",                              "pvsamv1",              "?=1",                      "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "albedo",                                      "User specified ground albedo",                          "0..1",   "",                              "pvsamv1",              "?=0.2",                    "MIN=0,MAX=100",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "irrad_mode",                                  "Irradiance input translation mode",                     "",       "0=beam&diffuse,1=total&beam",   "pvsamv1",              "?=0",                      "INTEGER,MIN=0,MAX=1",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sky_model",                                   "Diffuse sky model",                                     "",       "0=isotropic,1=hkdr,2=perez",    "pvsamv1",              "?=2",                      "INTEGER,MIN=0,MAX=2",           "" },

	{ SSC_INPUT,        SSC_ARRAY,       "monthly_soiling",                             "Monthly soiling derate",                                "%",      "",                              "pvsamv1",              "*",                        "LENGTH=12",                     "" },

	{ SSC_INPUT,        SSC_NUMBER,      "pre_derate",                                  "Pre-inverter derate",                                   "%",      "",                              "pvsamv1",              "*",                        "MIN=0,MAX=100",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "post_derate",                                 "Post-inverter derate",                                  "%",      "",                              "pvsamv1",              "*",                        "MIN=0,MAX=100",                 "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "modules_per_string",                          "Modules per string",                                    "",       "",                              "pvsamv1",              "*",                        "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "strings_in_parallel",                         "String in parallel",                                    "",       "",                              "pvsamv1",              "*",                        "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inverter_count",                              "Number of inverters",                                   "",       "",                              "pvsamv1",              "*",                        "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                                        "Tilt",                                                  "deg",    "0=horizontal,90=vertical",      "pvsamv1",              "naof:tilt_eq_lat",         "MIN=0,MAX=90",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt_eq_lat",                                 "Tilt=latitude override",                                "0/1",    "",                              "pvsamv1",              "na:tilt",                  "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                                     "Azimuth",                                               "deg",    "0=N,90=E,180=S,270=W",          "pvsamv1",              "*",                        "MIN=0,MAX=359.9",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "track_mode",                                  "Tracking mode",                                         "",       "0=fixed,1=1axis,2=2axis,3=azi", "pvsamv1",              "*",                        "INTEGER,MIN=0,MAX=3",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rotlim",                                      "Tracker rotation limit",                                "deg",    "",                              "pvsamv1",              "?=45",                     "MIN=5,MAX=85",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "enable_backtracking",                         "Enable backtracking for 1 axis trackers",               "0/1",    "",                              "pvsamv1",              "?=0",                      "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "btwidth",                                     "Width of backtracking row",                             "m",      "",                              "pvsamv1",              "enable_backtracing=1",     "POSITIVE",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "btspacing",                                   "Spacing between edges of backtracking rows",            "m",      "",                              "pvsamv1",              "enable_backtracing=1",     "POSITIVE",                      "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "module_model",                                "Photovoltaic module model specifier",                   "",       "0=spe,1=cec,2=6par_user,3=snl", "pvsamv1",              "*",                        "INTEGER,MIN=0,MAX=3",           "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "spe_area",                                    "Module area",                                           "m2",     "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad0",                                    "Irradiance level 0",                                    "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad1",                                    "Irradiance level 1",                                    "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad2",                                    "Irradiance level 2",                                    "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad3",                                    "Irradiance level 3",                                    "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_rad4",                                    "Irradiance level 4",                                    "W/m2",   "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff0",                                    "Efficiency at irradiance level 0",                      "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff1",                                    "Efficiency at irradiance level 1",                      "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff2",                                    "Efficiency at irradiance level 2",                      "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff3",                                    "Efficiency at irradiance level 3",                      "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_eff4",                                    "Efficiency at irradiance level 4",                      "%",      "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_reference",                               "Reference irradiance level",                            "",       "",                              "pvsamv1",              "module_model=0",           "INTEGER,MIN=0,MAX=4",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_module_structure",                        "Mounting and module structure",                         "",       "0=glass/cell/polymer sheet - open rack,1=glass/cell/glass - open rack,2=polymer/thin film/steel - open rack,3=Insulated back (building-integrated PV),4=close roof mount,5=user-defined",                      "pvsamv1",       "module_model=0",                    "INTEGER,MIN=0,MAX=5",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_a",                                       "Cell temp parameter a",                                 "",       "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_b",                                       "Cell temp parameter b",                                 "",       "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_dT",                                      "Cell temp parameter dT",                                "",       "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_temp_coeff",                              "Temperature coefficient",                               "%/C",    "",                              "pvsamv1",              "module_model=0",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spe_fd",                                      "Diffuse fraction",                                      "0..1",   "",                              "pvsamv1",              "module_model=0",           "MIN=0,MAX=1",                   "" },

	{ SSC_INPUT,        SSC_NUMBER,      "cec_area",                                    "Module area",                                           "m2",     "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_a_ref",                                   "Nonideality factor a",                                  "",       "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_adjust",                                  "Temperature coefficient adjustment",                    "%",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_alpha_sc",                                "Short circuit current temperature coefficient",         "A/C",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_beta_oc",                                 "Open circuit voltage temperature coefficient",          "V/C",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_gamma_r",                                 "Maximum power point temperature coefficient",           "%/C",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_i_l_ref",                                 "Light current",                                         "A",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_i_mp_ref",                                "Maximum power point current",                           "A",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_i_o_ref",                                 "Saturation current",                                    "A",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_i_sc_ref",                                "Short circuit current",                                 "A",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_n_s",                                     "Number of cells in series",                             "",       "",                              "pvsamv1",              "module_model=1",           "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_r_s",                                     "Series resistance",                                     "ohm",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_r_sh_ref",                                "Shunt resistance",                                      "ohm",    "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_t_noct",                                  "Nominal operating cell temperature",                    "C",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_v_mp_ref",                                "Maximum power point voltage",                           "V",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_v_oc_ref",                                "Open circuit voltage",                                  "V",      "",                              "pvsamv1",              "module_model=1",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_temp_corr_mode",                          "Cell temperature model selection",                      "",       "0=noct,1=mc",                   "pvsamv1",              "module_model=1",           "INTEGER,MIN=0,MAX=1",           "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "cec_standoff",                                "Standoff mode",                                         "",       "0=bipv,1=3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=ground/rack",  "pvsamv1",       "module_model=1",                           "INTEGER,MIN=0,MAX=5",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_height",                                  "Array mounting height",                                 "",       "0=one story,1=two story",                                           "pvsamv1",       "module_model=1",                           "INTEGER,MIN=0,MAX=1",       "" },

	{ SSC_INPUT,        SSC_NUMBER,      "cec_mounting_config",                         "Mounting configuration",                                "",       "0=rack,1=flush,2=integrated,3=gap",                                 "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "INTEGER,MIN=0,MAX=3",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_heat_transfer",                           "Heat transfer dimensions",                              "",       "0=module,1=array",                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "INTEGER,MIN=0,MAX=1",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_mounting_orientation",                    "Mounting structure orientation",                        "",       "0=do not impede flow,1=vertical supports,2=horizontal supports",    "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "INTEGER,MIN=0,MAX=2",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_gap_spacing",                             "Gap spacing",                                           "m",      "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_module_width",                            "Module width",                                          "m",      "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_module_length",                           "Module height",                                         "m",      "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_array_rows",                              "Rows of modules in array",                              "",       "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "INTEGER",                   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_array_cols",                              "Columns of modules in array",                           "",       "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "INTEGER",                   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cec_backside_temp",                           "Module backside temperature",                           "C",      "",                                                                  "pvsamv1",       "module_model=1&cec_temp_corr_mode=1",      "POSITIVE",                  "" },
		
	{ SSC_INPUT,        SSC_NUMBER,      "6par_celltech",                               "Solar cell technology type",                            "",       "monoSi=0,multiSi=1,CdTe=2,CIS=3,CIGS=4,Amorphous=5",                "pvsamv1",       "module_model=2",                           "INTEGER,MIN=0,MAX=5",       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_vmp",                                    "Maximum power point voltage (Vmp)"                      "V",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_imp",                                    "Imp",                                                   "A",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_voc",                                    "Voc",                                                   "V",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_isc",                                    "Isc",                                                   "A",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_bvoc",                                   "Short circuit current temperature coefficient",         "V/C",    "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_aisc",                                   "Open circuit voltage temperature coefficient",          "A/C",    "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_gpmp",                                   "Maximum power point temperature coefficient",           "%/C",    "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_nser",                                   "Nseries",                                               "",       "",                                                                  "pvsamv1",       "module_model=2",                           "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_area",                                   "Module area",                                           "m2",     "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_tnoct",                                  "Nominal operating cell temperature",                    "C",      "",                                                                  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_standoff",                               "Standoff mode",                                         "",       "0=bipv,1=3.5in,2=2.5-3.5in,3=1.5-2.5in,4=0.5-1.5in,5=ground/rack",  "pvsamv1",       "module_model=2",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "6par_mounting",                               "Array mounting height",                                 "",       "0=one story,1=two story",                                           "pvsamv1",       "module_model=2",                           "",                              "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "snl_module_structure",                        "Module and mounting structure configuration",           "",       "0=Use Database Values,1=glass/cell/polymer sheet - open rack,2=glass/cell/glass - open rack,3=polymer/thin film/steel - open rack,4=Insulated back (building-integrated PV),5=close roof mount,6=user-defined",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a",                                       "Temperature coefficient a",                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b",                                       "Temperature coefficient b",                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_dtc",                                     "Temperature coefficient dT",                            "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ref_a",                                   "User-specified a",                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ref_b",                                   "User-specified b",                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ref_dT",                                  "User-specified dT",                                     "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_fd",                                      "Diffuse fraction",                                      "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a0",                                      "Air mass polynomial coeff 0",                           "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a1",                                      "Air mass polynomial coeff 1",                           "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a2",                                      "Air mass polynomial coeff 2",                           "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a3",                                      "Air mass polynomial coeff 3",                           "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_a4",                                      "Air mass polynomial coeff 4",                           "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_aimp",                                    "Max power point current temperature coefficient",       "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_aisc",                                    "Short circuit current temperature coefficient",         "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_area",                                    "Module area",                                           "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b0",                                      "Incidence angle modifier polynomial coeff 0",           "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b1",                                      "Incidence angle modifier polynomial coeff 1",           "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b2",                                      "Incidence angle modifier polynomial coeff 2",           "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b3",                                      "Incidence angle modifier polynomial coeff 3",           "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b4",                                      "Incidence angle modifier polynomial coeff 4",           "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_b5",                                      "Incidence angle modifier polynomial coeff 5",           "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_bvmpo",                                   "Max power point voltage temperature coefficient",       "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_bvoco",                                   "Open circuit voltage temperature coefficient",          "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c0",                                      "C0",                                                    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c1",                                      "C1",                                                    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c2",                                      "C2",                                                    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c3",                                      "C3",                                                    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c4",                                      "C4",                                                    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c5",                                      "C5",                                                    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c6",                                      "C6",                                                    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_c7",                                      "C7",                                                    "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_impo",                                    "Max power point current",                               "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_isco",                                    "Short circuit current",                                 "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ixo",                                     "Ix midpoint current",                                   "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_ixxo",                                    "Ixx midpoint current",                                  "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_mbvmp",                                   "Irradiance dependence of Vmp temperature coefficient",  "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_mbvoc",                                   "Irradiance dependence of Voc temperature coefficient",  "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_n",                                       "Diode factor",                                          "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_series_cells",                            "Number of cells in series",                             "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_vmpo",                                    "Max power point voltage",                               "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "snl_voco",                                    "Open circuit voltage",                                  "",       "",                      "pvsamv1",       "module_model=3",                    "",                              "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "inverter_model",                              "Inverter model specifier",                              "",       "0=spe,1=sandia",        "pvsamv1",       "*",                                 "INTEGER,MIN=0,MAX=1",           "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "inv_spe_efficiency",                          "Single point inverter efficiency",                      "%",       "",                     "pvsamv1",       "inverter_model=0",                  "MIN=0,MAX=100",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_spe_power_ac",                            "Rated inverter power",                                  "Wac",     "",                     "pvsamv1",       "inverter_model=0",                  "POSITIVE",                      "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c0",                                  "Curvature between ac-power and dc-power at ref (C0)",   "1/W",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c1",                                  "Coefficient of Pdco variation with dc input voltage",   "1/V",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c2",                                  "Coefficient of Pso variation with dc input voltage",    "1/V",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c3",                                  "Coefficient of Co variation with dc input voltage",     "1/V",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_paco",                                "AC maximum power rating",                               "Wac",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pdco",                                "DC input power at which ac-power rating is achieved",   "Wdc",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pnt",                                 "AC power consumed by inverter at night",                "Wac",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pso",                                 "DC power required to enable the inversion process",     "Wdc",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_vdco",                                "DC input voltage for the rated ac-power rating",        "Vdc",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_vdcmax",                              "Maximum dc input operating voltage",                    "Vdc",     "",                     "pvsamv1",       "inverter_model=1",                    "",                              "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "shading_beam_hourly_enabled",                 "Enable hourly beam shading factors",                    "0/1",     "",                     "pvsamv1",       "?=0",                                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_ARRAY,       "shading_beam_hourly_factors",                 "Hourly beam shading factor array",                      "0..1",    "",                     "pvsamv1",       "shading_beam_hourly_enabled=1",       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "shading_mxh_enabled",                         "Enable beam time-based shading factor",                 "0/1",     "",                     "pvsamv1",       "?=0",                                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_MATRIX,      "shading_mxh_factors",                         "Beam shading factor time-based matrix",                 "0..1",    "",                     "pvsamv1",       "shading_mxh_enabled=1",               "ROWS=12,COLS=24",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "shading_azalt_enabled",                       "Enable azimuth-altitude beam shading factor",           "0/1",     "",                     "pvsamv1",       "?=0",                                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_MATRIX,      "shading_azalt_table",                         "Azimuth-altitude beam shading factor matrix",           "0..1",    "",                     "pvsamv1",       "shading_azalt_enabled=1",             "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "shading_diffuse_enabled",                     "Enable diffuse shading factor",                         "0/1",     "",                     "pvsamv1",       "?=0",                                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "shading_diffuse_factor",                      "Diffuse shading factor",                                "0..1",    "",                     "pvsamv1",       "shading_diffuse_enabled=1",           "",                              "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_enabled",                        "Enable self-shading calculator",                        "",       "",                       "pvsamv1",       "?=0",                                 "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_length",                         "Module length",                                         "m",      "",                       "pvsamv1",       "self_shading_enabled=1",              "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_width",                          "Module width",                                          "m",      "",                       "pvsamv1",       "self_shading_enabled=1",              "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_mod_orient",                     "Module orientation",                                    "",       "0=portrait,1=landscape", "pvsamv1",       "self_shading_enabled=1",              "INTEGER,MIN=0,MAX=1",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_str_orient",                     "String orientation",                                    "",       "0=vertical,1=horizontal","pvsamv1",       "self_shading_enabled=1",              "INTEGER,MIN=0,MAX=1",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_ncellx",                         "Number of cells along bottom",                          "",       "",                       "pvsamv1",       "self_shading_enabled=1",              "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_ncelly",                         "Number of cells along side",                            "",       "",                       "pvsamv1",       "self_shading_enabled=1",              "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_ndiode",                         "Number of bypass diodes",                               "",       "",                       "pvsamv1",       "self_shading_enabled=1",              "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_nmodx",                          "Number of modules along bottom",                        "",       "",                       "pvsamv1",       "self_shading_enabled=1",              "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_nstrx",                          "Number of strings along bottom",                        "",       "",                       "pvsamv1",       "self_shading_enabled=1",              "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_nmody",                          "Number of modules along side",                          "",       "",                       "pvsamv1",       "self_shading_enabled=1",              "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_nrows",                          "Number of rows",                                        "",       "",                       "pvsamv1",       "self_shading_enabled=1",              "INTEGER,POSITIVE",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_rowspace",                       "Row spacing",                                           "m",      "",                       "pvsamv1",       "self_shading_enabled=1",              "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_slopens",                        "North-south slope",                                     "deg",    "",                       "pvsamv1",       "self_shading_enabled=1",              "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "self_shading_slopeew",                        "East-west slope",                                       "deg",    "",                       "pvsamv1",       "self_shading_enabled=1",              "",                              "" },

// outputs
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_glob_horiz_rad",                      "Global horizontal irradiance",                           "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_beam",                                "Nominal incident beam irradiance",                       "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_diff",                                "Nominal incident diffuse irradiance",                    "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_inc_beam",                            "Incident beam irradiance (after shading and soiling)",   "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_inc_diff",                            "Incident diffuse irradiance (after shading and soiling)","kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_inc_total",                           "Incident total irradiance (after shading and soiling)",  "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_input_radiation",                     "Input radiation",                                        "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_modeff",                              "Module efficiency",                                      "%",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_surf_tilt",                           "Surface tilt",                                           "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_surf_azi",                            "Surface azimuth",                                        "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_soiling_derate",                      "Soiling derate",                                         "%",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_sol_zen",                             "Solar zenith angle",                                     "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_sol_azi",                             "Solar azimuth angle",                                    "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_windspd",                             "Wind speed",                                             "m/s",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_ambtemp",                             "Ambient temperature",                                    "C",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_celltemp",                            "Cell temperature",                                       "C",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_axisrot",                             "Axis rotation (for 1 axis trackers)",                    "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_idealrot",                            "Ideal axis rotation (for 1 axis trackers)",              "deg",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_ss_derate",                           "Self-shading derate",                                    "",       "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_ss_diffuse_derate",                   "Self-shading diffuse derate",                            "",       "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_ss_reflected_derate",                 "Self-shading reflected derate",                          "",       "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_ss_diffuse_loss",                     "Self-shading diffuse loss",                              "kW/m2",  "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_vmp",                                 "Max power point voltage",                                "V",      "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_dc_gross",                            "Gross dc output",                                        "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_dc_net",                              "Net dc output",                                          "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_ac_gross",                            "Gross ac output",                                        "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hourly_ac_net",                              "Net ac output",                                          "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=8760",                              "" },
	
	{ SSC_OUTPUT,        SSC_ARRAY,       "monthly_inc_total",                          "Total incident radiation",                               "kWh/m2", "",                      "pvsamv1",       "*",                    "LENGTH=12",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "monthly_inc_beam",                           "Beam incident radiation",                                "kWh/m2", "",                      "pvsamv1",       "*",                    "LENGTH=12",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "monthly_dc_net",                             "Net dc output",                                          "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=12",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "monthly_ac_net",                             "Net ac output",                                          "kWh",    "",                      "pvsamv1",       "*",                    "LENGTH=12",                              "" },
	
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_incident_beam",                       "Incident beam annual",                                   "kW/m2",  "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_input_radiation",                     "Input radiation",                                        "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_dc_nominal",                          "Nominal dc energy",                                      "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_dc_gross",                            "Gross dc output",                                        "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_dc_net",                              "Net dc output",                                          "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_ac_gross",                            "Gross ac output",                                        "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_ac_net",                              "Net ac output",                                          "kWh",    "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_performance_factor",                  "System performance factor",                              "%",      "",                      "pvsamv1",       "*",                    "",                              "" },
	
	{ SSC_OUTPUT,        SSC_NUMBER,      "6par_a",                                     "CEC 6-parameter (a)",                                    "",       "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "6par_Io",                                    "CEC 6-parameter (Io)",                                   "",       "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "6par_Il",                                    "CEC 6-parameter (Il)",                                   "",       "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "6par_Rs",                                    "CEC 6-parameter (Rs)",                                   "",       "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "6par_Rsh",                                   "CEC 6-parameter (Rsh)",                                  "",       "",                      "pvsamv1",       "*",                    "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "6par_Adj",                                   "CEC 6-parameter (Adj)",                                  "",       "",                      "pvsamv1",       "*",                    "",                              "" },
var_info_invalid };


	
class cm_pvsamv1 : public compute_module
{
public:
	
	cm_pvsamv1()
	{
		add_var_info( _cm_vtab_pvsamv1 );
	}


	
	void exec( ) throw( general_error )
	{
		static const int nday[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

		// run some preliminary checks on inputs
		if (as_integer("self_shading_enabled") && as_integer("track_mode"))
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

		// open the weather file
		weatherfile wf( as_string("weather_file") );
		if ( !wf.ok() ) throw exec_error( "pvsamv1", "failed to open weather file for reading");
			
		if ( wf.nrecords != 8760 ) throw exec_error("pvsamv1", "pv simulator only accepts hourly weather data");

		int modules_per_string = as_integer("modules_per_string");
		int strings_in_parallel = as_integer("strings_in_parallel");
		int num_inverters = as_integer("inverter_count");

		double alb_const = as_double("albedo");
		bool use_wf_alb = (as_integer("use_wf_albedo") > 0);

		int radmode = as_integer("irrad_mode"); // 0=B&D, 1=G&B
		int skymodel = as_integer("sky_model"); // 0=isotropic, 1=hdkr, 2=perez

		size_t soil_len = 0;
		ssc_number_t *soiling = as_array("monthly_soiling", &soil_len); // monthly soiling array
		if (soil_len != 12) throw exec_error( "pvsamv1", "soiling derate must have 12 values");
	
		double pre_derate = as_double("pre_derate")/100.0;
		double post_derate = as_double("post_derate")/100.0;

		int tracking = as_integer("track_mode"); // 0=fixed, 1=1axis, 2=2axis, 3=aziaxis

		double tilt = as_double("tilt");
		if ( as_integer("tilt_eq_lat") )
			tilt = fabs( wf.lat );

		double azi = as_double("azimuth");
		double rotlim = as_double("rotlim");
	
		double btwidth = -1, btspacing = -1;
		if (as_integer("enable_backtracking"))
		{
			btwidth = as_double("btwidth");
			btspacing = as_double("btspacing");
		}

		int mod_type = as_integer("module_model");

		double spe_area = 1, spe_rad[5], spe_eff[5], spe_a = 0, spe_b = 0, spe_dT = 0, spe_gamma = 0, spe_fd = 1.0;
		int spe_ref = 0;

		cec6par_module_t cec;
		int cec_celltemp_mode = as_integer("cec_temp_corr_mode");
		noct_celltemp_t noct_tc;
		mcsp_celltemp_t mcsp_tc;

		sandia_module_t snl;
		sandia_celltemp_t snltc;

		double ref_area_m2 = 0;

		double self_shading_fill_factor = 0;

		double module_watts_stc = -1.0;

		if ( mod_type == 0 )
		{
			spe_area = as_double("spe_area");
			ref_area_m2 = spe_area;
			for (int i=0;i<5;i++)
			{
				spe_rad[i] = as_double( util::format("spe_rad%d", i));
				spe_eff[i] = as_double( util::format("spe_eff%d", i));
				if (i > 0 && spe_rad[i] <= spe_rad[i-1])
					throw exec_error( "pvsamv1", "SPE model radiation levels must increase monotonically");
			}
		
			spe_gamma = as_double("spe_temp_coeff");
			spe_ref = as_integer("spe_reference");
		
			switch(as_integer("spe_module_structure"))
			{
			case 0: //glass/cell/polymer sheet - open rack
				spe_a = -3.56;
				spe_b = -0.0750;
				spe_dT = 3;
				break;
			case 1: //glass/cell/glass - open rack
				spe_a = -3.47;
				spe_b = -0.0594;
				spe_dT = 3;
				break;
			case 2: //polymer/thin film/steel - open rack
				spe_a = -3.58;
				spe_b = -0.113;
				spe_dT = 3;
				break;
			case 3: //Insulated back (building-integrated PV)
				spe_a = -2.81;
				spe_b = -0.0455;
				spe_dT = 0;
				break;
			case 4: //close roof mount
				spe_a = -2.98;
				spe_b = -0.0471;
				spe_dT = 1;
				break;
			case 5: //user defined
				spe_a = as_double("spe_a");
				spe_b = as_double("spe_b");
				spe_dT = as_double("spe_dT");
				break;
			default:
				throw exec_error("pvsamv1", "invalid spe module structure and mounting");
			}

			spe_fd = as_double("spe_fd");
			
			module_watts_stc = spe_eff[spe_ref]/100 * spe_rad[spe_ref] * spe_area;
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


			if ( cec_celltemp_mode == 0 )
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

				mcsp_tc.DcDerate = pre_derate;
				mcsp_tc.MC = as_integer("cec_mounting_config")+1;
				mcsp_tc.HTD = as_integer("cec_heat_transfer")+1;
				mcsp_tc.MSO = as_integer("cec_mounting_orientation")+1;
				mcsp_tc.Wgap = as_double("cec_gap_spacing");
				mcsp_tc.Length = as_double("cec_module_length");
				mcsp_tc.Width = as_double("cec_module_width");
				mcsp_tc.Nrows = (int)as_integer("cec_array_rows");
				mcsp_tc.Ncols = (int)as_integer("cec_array_cols");
				mcsp_tc.TbackInteg = as_double("cec_backside_temp");
			}
			
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

			double A = as_double("snl_a");
			double B = as_double("snl_b");
			double DT = as_double("snl_dtc");
	
			switch(as_integer("snl_module_structure"))
			{
			case 0: // use database values;
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
			}
		
			snltc.a = A;
			snltc.b = B;
			snltc.DT0 = DT;
			snltc.fd = snl.fd;
			
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
		
			int alpha_units = as_integer("6par_aisc_units");
			if (alpha_units == 1)
				alpha = 0.01*alpha*Isc;

			int beta_units = as_integer("6par_bvoc_units");
			if (beta_units == 1)
				beta = 0.01*beta*Voc;
		
			module6par m(tech_id, Vmp, Imp, Voc, Isc, beta, alpha, gamma, nser);
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

			cec_celltemp_mode = 0; // use NOCT model
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
			
			module_watts_stc = cec.Vmp * cec.Imp;

		}
		else
			throw exec_error("pvsamv1", "invalid pv module model type");

		
		double nameplate_kw = modules_per_string * strings_in_parallel * module_watts_stc/1000.0;

		double speinv_pac = 0, speinv_eff = 0;
		::sandia_inverter_t snlinv;

		int inv_type = as_integer("inverter_model");
		if ( inv_type == 0 )
		{
			speinv_pac = as_double("inv_spe_power_ac");
			speinv_eff = as_double("inv_spe_efficiency");
		}
		else
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


		double shad_skydiff_factor = 1;
		if ( as_integer("shading_diffuse_enabled") )
			shad_skydiff_factor = as_double("shading_diffuse_factor");

		std::vector<double> shad_beam_factor(8760, 1.0);
		if (as_integer( "shading_beam_hourly_enabled") )
		{
			size_t len = 0;
			ssc_number_t *ph = as_array("shading_beam_hourly_factors", &len);
			if (ph == 0 || len != 8760)
				throw exec_error( "pvsamv1", "hourly beam shading factor enabled, but provided array does not have 8760 values");

			for (int i=0;i<8760;i++)
				shad_beam_factor[i] *= ph[i];
		}

		if ( as_integer("shading_mxh_enabled") )
		{
			var_data *v = lookup("shading_mxh_factors");
			if (!v || v->num.nrows() != 12 || v->num.ncols() != 24 )
				throw exec_error("pvsamv1", "Bad shading factor data for time-based matrix specification, 12x24 values required");

		
			int c=0;
			for (int m=0;m<12;m++)
				for (int d=0;d<nday[m];d++)
					for (int h=0;h<24;h++)
						shad_beam_factor[c++] *= v->num.at(m,h);
		}

		// check that azimuth values increase and altitude values decrease
		bool enable_azalt_beam_shading = false;
		util::matrix_t<double> azaltvals;
		if (as_integer("shading_azalt_enabled"))
		{
			var_data *v = lookup("shading_azalt_table");
			if (!v || v->num.nrows() < 2 || v->num.ncols() < 2)
				throw exec_error( "pvsamv1", "Azimuth-altitude shading factor table data too small" );

			azaltvals = v->num;
			for (size_t i=2;i<azaltvals.nrows();i++)
				if (azaltvals.at(i,0) > azaltvals.at(i-1,0))				
					throw exec_error("pvsamv1", "Error in azimuth-altitude shading table: altitude values must decrease monotonically");

			for (size_t i=2;i<azaltvals.ncols();i++)
				if (azaltvals.at(0,i) < azaltvals.at(0,i-1))
					throw exec_error("pvsamv1", "Error in azimuth-altitude shading table: azimuth values must increase monotonically");

			enable_azalt_beam_shading = true;
		}

		// self-shading
		bool self_shading_enabled = (as_integer("self_shading_enabled")==1);
		ssarrdat ssarr;
		ssarr.azimuth = azi;
		ssarr.tilt = tilt;
		ssarr.length = as_double("self_shading_length");
		ssarr.width = as_double("self_shading_width");
		ssarr.row_space = as_double("self_shading_rowspace");
		ssarr.mod_space = 0; // no ui input - assumed 0 in trnsys inputs
		ssarr.slope_ns = as_double("self_shading_slopens");
		ssarr.slope_ew = as_double("self_shading_slopeew");
		ssarr.mod_orient = as_integer("self_shading_mod_orient");
		ssarr.str_orient = as_integer("self_shading_str_orient");
		ssarr.nmodx = as_integer("self_shading_nmodx");
		ssarr.nstrx = as_integer("self_shading_nstrx");
		ssarr.nmody = as_integer("self_shading_nmody");
		ssarr.nrows = as_integer("self_shading_nrows");
		ssarr.ncellx = as_integer("self_shading_ncellx");
		ssarr.ncelly = as_integer("self_shading_ncelly");
		ssarr.ndiode = as_integer("self_shading_ndiode");
		selfshade_t sscalc( ssarr );


		// setup output arrays

		// self-shading
		ssc_number_t *p_ss_derate = allocate( "hourly_ss_derate", 8760 );
		ssc_number_t *p_ss_diffuse_derate = allocate( "hourly_ss_diffuse_derate", 8760 );
		ssc_number_t *p_ss_reflected_derate = allocate( "hourly_ss_reflected_derate", 8760 );
		ssc_number_t *p_ss_diffuse_loss = allocate( "hourly_ss_diffuse_loss", 8760 );

		ssc_number_t *p_glob = allocate( "hourly_glob_horiz_rad", 8760 );
		ssc_number_t *p_beam = allocate( "hourly_beam", 8760 );
		ssc_number_t *p_diff = allocate( "hourly_diff", 8760 );
		ssc_number_t *p_incbeam = allocate( "hourly_inc_beam", 8760 );
		ssc_number_t *p_incdiff = allocate( "hourly_inc_diff", 8760 );
		ssc_number_t *p_inctotal = allocate( "hourly_inc_total", 8760 );
		ssc_number_t *p_inrad = allocate( "hourly_input_radiation", 8760 );
		ssc_number_t *p_tcell = allocate( "hourly_celltemp", 8760 );
		ssc_number_t *p_modeff = allocate( "hourly_modeff", 8760 );
		ssc_number_t *p_wspd = allocate( "hourly_windspd", 8760 );
		ssc_number_t *p_tdry = allocate( "hourly_ambtemp", 8760 );
		ssc_number_t *p_rot = allocate( "hourly_axisrot", 8760 );
		ssc_number_t *p_idealrot = allocate( "hourly_idealrot", 8760);
		ssc_number_t *p_vmp = allocate( "hourly_vmp", 8760 );
		ssc_number_t *p_dcgross = allocate( "hourly_dc_gross", 8760 );
		ssc_number_t *p_dcpwr = allocate( "hourly_dc_net", 8760 );
		ssc_number_t *p_acgross = allocate( "hourly_ac_gross", 8760 );
		ssc_number_t *p_acpwr = allocate( "hourly_ac_net", 8760 );

		ssc_number_t *p_surftilt = allocate("hourly_surf_tilt", 8760);
		ssc_number_t *p_surfazi = allocate("hourly_surf_azi", 8760);
		ssc_number_t *p_soiling = allocate("hourly_soiling_derate", 8760);
		ssc_number_t *p_solzen = allocate("hourly_sol_zen", 8760);
		ssc_number_t *p_solazi = allocate("hourly_sol_azi", 8760);
	
		int istep = 0, nstep = wf.nrecords;
		while( wf.read() && istep < 8760 )
		{
			if ( istep % (nstep/10) == 0)
				update( "calculating", 100.0f * ((float)istep) / ((float)nstep), (float)istep );

			irrad irr;
			irr.set_time( wf.year, wf.month, wf.day, wf.hour, wf.minute, wf.step / 3600.0 );
			irr.set_location( wf.lat, wf.lon, wf.tz );
		
			double alb = alb_const;
			if (use_wf_alb && wf.albedo >= 0 && wf.albedo <= 1)
				alb = wf.albedo;


			irr.set_sky_model( skymodel, alb );
			if ( radmode == 0 ) irr.set_beam_diffuse( wf.dn, wf.df );
			else if (radmode == 1) irr.set_global_beam( wf.gh, wf.dn );


			// 4/29/12 added for consistency with surface when self-shading present - see lib_pvshade in recore
			if (self_shading_enabled) tilt = tilt - ssarr.slope_ns;


			irr.set_surface( tracking, tilt, azi, rotlim, btwidth, btspacing );

			int code = irr.calc();
			if ( code < 0 )
				throw exec_error( "pvsamv1", util::format("failed to compute irradiation on surface (code: %d) [y:%d m:%d d:%d h:%d]", code, wf.year, wf.month, wf.day, wf.hour));

			double ibeam, iskydiff, ignddiff;
			irr.get_poa( &ibeam, &iskydiff, &ignddiff, 0, 0, 0);

			// apply hourly shading factors to beam (if none enabled, factors are 1.0)
			ibeam *= shad_beam_factor[istep];

			// apply sky diffuse shading factor (specified as constant, nominally 1.0 if disabled in UI)
			iskydiff *= shad_skydiff_factor;

			// apply soiling derate to all components of irradiance
			int midx = wf.month - 1;
			double soiling_factor = 1.0;
			if ( midx >= 0 && midx < 12 )
			{
				soiling_factor = soiling[midx]/100;
				ibeam *= soiling_factor;
				iskydiff *= soiling_factor;
				ignddiff *= soiling_factor;
			}
		
			double solazi, solzen, solalt, aoi, stilt, sazi, rot, btd;
			int sunup;

			irr.get_sun( &solazi, &solzen, &solalt, 0, 0, 0, &sunup, 0, 0, 0 );
			irr.get_angles( &aoi, &stilt, &sazi, &rot, &btd );
				
			// apply beam shading based on solar azimuth/altitude table
			if ( enable_azalt_beam_shading )
				ibeam *= azaltinterp( solazi, solalt, azaltvals );

			double dcpwr = 0, dcv = 0, dceff = 0, tcell = wf.tdry;
			double acpwr = 0, aceff = 0;

			if ( sunup > 0 )
			{
				// apply self-shading if enabled
				if ( self_shading_enabled )
				{
					if ( sscalc.exec( solzen, solazi, wf.dn, wf.df, self_shading_fill_factor, alb ) )
					{
						p_ss_diffuse_derate[istep] = (ssc_number_t) sscalc.diffuse_derate();
						p_ss_reflected_derate[istep] = (ssc_number_t) sscalc.reflected_derate();
						p_ss_diffuse_loss[istep] = (ssc_number_t) sscalc.m_diffuse_loss_term/1000;
						p_ss_derate[istep] = (ssc_number_t) sscalc.dc_derate();
						iskydiff = sscalc.diffuse_derate() * iskydiff;
						ignddiff = sscalc.reflected_derate() * ignddiff;
					}
					else
						throw exec_error("pvsamv1", util::format("Self-shading calculation failed at hour %d", istep) ) ;
				}


				// compute dc power, voltage, efficiency, and cell temp of the whole array
				if ( mod_type == 0)
				{
					double idiff = spe_fd*(iskydiff+ignddiff);
					dceff = eff_interpolate( ibeam+idiff, spe_rad, spe_eff );
					dcpwr = (dceff/100.0*(ibeam+idiff)*spe_area)*modules_per_string*strings_in_parallel;
					dcv = 12 * modules_per_string;
					double tmod = sandia_celltemp_t::sandia_module_temperature( ibeam, idiff, wf.wspd, wf.tdry, spe_fd, spe_a, spe_b );
					tcell = sandia_celltemp_t::sandia_tcell_from_tmodule( tmod, ibeam, idiff, spe_fd, spe_dT );

					dcpwr += dcpwr*(spe_gamma/100.0)*(tcell - 25.0);
					if (dcpwr < 0) dcpwr = 0;

				}
				else if ( mod_type == 1 || mod_type == 2 ) 
				{
					pvinput_t in( ibeam, iskydiff, ignddiff, wf.tdry, wf.tdew, wf.wspd, wf.wdir, wf.pres, 
						solzen, aoi, wf.elev, stilt, sazi, ((double)wf.hour) + wf.minute/60.0 );
					pvoutput_t out;

					if ( cec_celltemp_mode == 0 ) noct_tc( in, cec, -1, tcell );
					else mcsp_tc( in, cec, -1, tcell );

					cec( in, tcell, -1, out );

					dceff = out.Efficiency*100;
					dcpwr = out.Power * modules_per_string * strings_in_parallel;
					dcv = out.Voltage * modules_per_string;
					tcell = out.CellTemp;

				}
				else if ( mod_type == 3 )
				{
					pvinput_t in( ibeam, iskydiff, ignddiff, wf.tdry, wf.tdew, wf.wspd, wf.wdir, wf.pres, 
						solzen, aoi, wf.elev, stilt, sazi, ((double)wf.hour) + wf.minute/60.0 );
					pvoutput_t out;
					snltc( in, snl, -1, tcell );
					snl( in, tcell, -1, out );

					dceff = out.Efficiency*100;
					dcpwr = out.Power * modules_per_string * strings_in_parallel;
					dcv = out.Voltage * modules_per_string;
					tcell = out.CellTemp;

				}

				// apply self-shading derate
				if (self_shading_enabled) dcpwr *= p_ss_derate[istep];

				// apply pre-inverter power derate
				dcpwr *= pre_derate;

				// inverter, 
				if ( inv_type == 0 )
				{

					/*
	Power = MAX(([3,7]*[12,1]*PREIDRATE)/(3.6*NumInv),.0000001)
	DCPowerKW = MAX(([3,7]*[12,1]*PREIDRATE)/3.6,.0000001)/1000

	* INVRatedPow is in AC and so needs to be converted to DC Power at full load 
	* This line calculates the part load ratio and makes sure it is no greater than 1 (so it dumps if it is too large).
	INVPLR = MIN(1,Power/(INVRatedPow/INVeffinp))
	* This equation first checks that the power is greater than the low-end cutoff, then multiplies the Power (PLR*RatedPower) by the efficiency to include
	* the dumping properly. Everything is unitless except the rated Power so the final units are in Watts which is divided by 1000 to get kWs
	* Note: The cutoff level is assumed to be in AC watts and so the DC power level is converted to AC to check (GT()) if lower than the cutoff level.
	*ACPowermax = GT((([3,7]*[12,1]*PREIDRATE)/(3.6*NumInv))*INVEFFinp,INVcutoff)*INVPLR*INVRatedPow/1000
	* SJ 10/4/10 - clip inverter output at rated power
	ACPowermax = MIN(GT((([3,7]*[12,1]*PREIDRATE)/(3.6*NumInv))*INVEFFinp,INVcutoff)*INVPLR*INVRatedPow/1000,
						INVRatedPow/1000/NumInv)
	ACPower = ACPowermax*POSTIDRATE*NUMINV
	TotRadkW = ITotal_soil/3600. */

					double pwr1inv = dcpwr/num_inverters;
					double opeff = speinv_eff/100.0;
					double invplr =  pwr1inv/(speinv_pac/opeff);
					// clipping loss
					if ( invplr > 1 ) invplr = 1;
					// efficincy loss = 1-opeff/100
					double pwrmax = pwrmax = invplr * speinv_pac;

					acpwr = pwrmax * post_derate * num_inverters;
					aceff = speinv_eff;
		
				}
				else if ( inv_type == 1 )
				{
					double _par, _plr;
					snlinv.acpower( dcpwr/num_inverters, dcv, &acpwr, &_par, &_plr, &aceff );
					acpwr *= num_inverters;
					acpwr *= post_derate;
					aceff *= 100;
				}

			}


			// save outputs
		
			p_glob[istep] = (ssc_number_t) wf.gh/1000;
			p_beam[istep] = (ssc_number_t) wf.dn/1000;
			p_diff[istep] = (ssc_number_t) wf.df/1000;
			p_incbeam[istep] = (ssc_number_t) ibeam/1000;
			p_incdiff[istep] = (ssc_number_t) (iskydiff+ignddiff)/1000;
			p_inctotal[istep] = (ssc_number_t) ( (ibeam+iskydiff+ignddiff)/1000 );
			p_inrad[istep] = (ssc_number_t) ( (ibeam+iskydiff+ignddiff)*ref_area_m2/1000*modules_per_string*strings_in_parallel );
			p_tcell[istep] = (ssc_number_t) tcell;
			p_modeff[istep] = (ssc_number_t) dceff;
			p_wspd[istep] = (ssc_number_t) wf.wspd;
			p_tdry[istep] = (ssc_number_t) wf.tdry;
			p_rot[istep] = (ssc_number_t) rot;
			p_idealrot[istep] = (ssc_number_t) ( rot-btd );
			p_vmp[istep] = (ssc_number_t) dcv;
			p_dcgross[istep] = (ssc_number_t) dcpwr / 1000;

			if (pre_derate != 0) 
				p_dcgross[istep] = (ssc_number_t) (dcpwr / pre_derate / 1000);
			else
				p_dcgross[istep] = (ssc_number_t) dcpwr / 1000;

			p_dcpwr[istep] = (ssc_number_t) dcpwr/1000;

			if (post_derate != 0) 
				p_acgross[istep] = (ssc_number_t) (acpwr / post_derate / 1000);
			else
				p_acgross[istep] = (ssc_number_t) acpwr/1000;

			p_acpwr[istep] = (ssc_number_t) acpwr / 1000;
		
			p_surftilt[istep] = (ssc_number_t) stilt;
			p_surfazi[istep] = (ssc_number_t) sazi;
			p_soiling[istep] = (ssc_number_t) soiling_factor;
			p_solzen[istep] = (ssc_number_t) solzen;
			p_solazi[istep] = (ssc_number_t) solazi;

			istep++;
		}


		if (istep != 8760)
			throw exec_error( "pvsamv1", "failed to simulate all 8760 hours, error in weather file?");
	
	
		accumulate_monthly( "hourly_ac_net", "monthly_ac_net" );
		accumulate_monthly( "hourly_dc_net", "monthly_dc_net" );
		accumulate_monthly( "hourly_inc_total", "monthly_inc_total" );
		accumulate_monthly( "hourly_inc_beam", "monthly_inc_beam" );

		accumulate_annual( "hourly_input_radiation", "annual_input_radiation" );
		accumulate_annual( "hourly_inc_beam", "annual_incident_beam" );
		accumulate_annual( "hourly_dc_gross", "annual_dc_gross" );
		accumulate_annual( "hourly_dc_net", "annual_dc_net" );
		accumulate_annual( "hourly_ac_gross", "annual_ac_gross" );
		accumulate_annual( "hourly_ac_net", "annual_ac_net" );
	
		double inp_rad = as_double("annual_input_radiation");
		assign( "annual_dc_nominal", var_data( (ssc_number_t)( inp_rad * module_eff() / 100 ) ) );

		assign( "annual_performance_factor", var_data( (ssc_number_t) system_performance_factor( nameplate_kw ) ) );

		if (as_integer("inverter_model") == 1) inverter_vdcmax_check();
		
		inverter_size_check();
		
		assign( "6par_a", var_data((ssc_number_t) cec.a) );
		assign( "6par_Io", var_data((ssc_number_t) cec.Io) );
		assign( "6par_Il", var_data((ssc_number_t) cec.Il) );
		assign( "6par_Rs", var_data((ssc_number_t) cec.Rs) );
		assign( "6par_Rsh", var_data((ssc_number_t) cec.Rsh) );
		assign( "6par_Adj", var_data((ssc_number_t) cec.Adj) );
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

	double system_performance_factor( double nameplate_kw )
	{
		//system performance factor AC out / (dc rating * incident radiation)
		double radiation = 0.0;
		double deratedOutput = 0.0;
		size_t count;
		ssc_number_t *incRad=0; // incident radiation - total for pv and beam only for cpv
		ssc_number_t *ACPower=0; // derated AC Power from TRNSYS output

		int mod_type = as_integer("module_model");

		ACPower = as_array("monthly_ac_net", &count );
		if ( count == 12 ) 
		{
			for(int i=0;i<12;i++)
				deratedOutput += ACPower[i];
		}
		else
			throw exec_error( "pvsamv1", "invalid monthly ACPower output variable length when calculating system performance factor" );


		switch (mod_type)
		{
		case 0: case 1: case 2: // SEM, CEC, User6par
			incRad = as_array("monthly_inc_total", &count );
			break;
		case 3: // Sandia
			if (as_integer("snl_fd") == 0) // CPV
				incRad = as_array("monthly_inc_beam", &count );
			else // flatplate
				incRad = as_array("monthly_inc_total", &count );
			break;
		}

		if ( count != 12 ) 
			throw exec_error( "pvsamv1", "invalid monthly incident radiation output variable length when calculating system performance factor" );


		for(int i=0;i<12;i++)
			radiation += incRad[i];

		if ( radiation == 0 ) // no division by zero
		{	
			log( util::format("PV[%d] SystemPerformanceFactor - incident radiation is zero.", mod_type), SSC_WARNING );
			radiation = 1.0;
		}
		
		return deratedOutput / (nameplate_kw * radiation);
	}

	void inverter_vdcmax_check()
	{
		// check that no hourly vmp values exceed Vdcmax
		// add max value and number of times > Vdcmax
		int numVmpGTVdcmax = 0;
		double maxVmp=0;
		int maxVmpHour=0;
		double vdcmax = as_double("inv_snl_vdcmax");

		// warning on inverter page
		if (vdcmax <=0) return;

		size_t count;
		ssc_number_t *da = as_array("hourly_vmp", &count);
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
		double inv_eff = 0;
		switch (invType)
		{
			case 0: // spe
				ratedACOutput = as_double("inv_spe_power_ac");
				inv_eff = as_double("inv_spe_efficiency") / 100.0;
				if (inv_eff > 0)
					ratedDCOutput = ratedACOutput / inv_eff;
				break;
			case 1: // sandia
				ratedACOutput = as_double("inv_snl_paco");
				ratedDCOutput = as_double("inv_snl_pdco");
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
	
	double eff_interpolate( double irrad, double rad[5], double eff[5] )
	{
		if ( irrad < rad[0] )
			return eff[0];
		else if ( irrad > rad[4] )
			return eff[4];

		int i = 1;
		for ( i=1;i<5;i++ )
			if ( irrad < rad[i] ) break;
      
		int i1 = i-1;

		double wx=(irrad-rad[i1])/(rad[i]-rad[i1]);
		return (1-wx)*eff[i1]+wx*eff[i];
	}

	double azaltinterp(double azimuth, double altitude, const util::matrix_t<double> &azaltvals)
	{
		int r = azaltvals.nrows();
		int c = azaltvals.ncols();

		int i, j;
		double reduc = 1.0;
	
		if (azimuth < 0 || azimuth > 360 || altitude < 0 || altitude > 90) return reduc;

		int alt_l = 1;
		int azi_l = 1;
		double alt_d = 0;
		double azi_d = 0;
		util::matrix_t<double> x(2);
		util::matrix_t<double> y(2);			
		util::matrix_t<double> fQ(2,2);

		for (i=0;i<2;i++)
			for (j=0;j<2;j++)
				fQ.at(i,j) = 1.0;

		for (i=0;i<2;i++)
		{
			x[i]=1.0;
			y[i]=1.0;
		}

		for (i=1;i<r;i++)
		{
			if ((azaltvals.at(i,0) - altitude) > 0)
			{
				alt_l = i;
				if (i == r-1) alt_d = 0;
				else alt_d = azaltvals.at(i,0) - altitude;
			
			}
		}

		for (i=1;i<c;i++)
		{
			if (azimuth - azaltvals.at(0,i) > 0)
			{
				azi_l = i;
				if (i == c-1) azi_d = 0;
				else azi_d = azimuth-azaltvals.at(0,i);	
			}
		}

		if (alt_d == 0 && azi_d == 0) reduc = azaltvals.at(alt_l,azi_l);
		else if (alt_d == 0) reduc = azaltvals.at(alt_l,azi_l)+
			((azaltvals.at(alt_l,azi_l+1)-(azaltvals.at(alt_l,azi_l)))
			/(azaltvals.at(0,azi_l+1)-(azaltvals.at(0,azi_l))))*azi_d;
		else if (azi_d == 0) reduc = azaltvals.at(alt_l,azi_l)+
			((azaltvals.at(alt_l+1,azi_l)-(azaltvals.at(alt_l,azi_l)))/
			(azaltvals.at(alt_l+1,0)-(azaltvals.at(alt_l,0))))*alt_d;
		else 
		{
			for (i=0;i<2;i++)
				for (j=0;j<2;j++)
					fQ.at(i,j) = azaltvals.at(alt_l+i,azi_l+j);

			for (i=0;i<2;i++) 
			{
				x.at(i) = azaltvals.at(alt_l+i,0);
				y.at(i) = azaltvals.at(0,azi_l+i);
			}
		
			if (x.at(1) - x.at(0) == 0 && y.at(1) - y.at(0) == 0) reduc = azaltvals.at(alt_l,azi_l);
			else if (x.at(1) - x.at(0) == 0) reduc = azaltvals.at(alt_l,azi_l)+
				((azaltvals.at(alt_l,azi_l+1)-(azaltvals.at(alt_l,azi_l)))
				/(azaltvals.at(0,azi_l+1)-(azaltvals.at(0,azi_l))))*azi_d;
			else if (y.at(1) - y.at(0) == 0) reduc = azaltvals.at(alt_l,azi_l)+
				((azaltvals.at(alt_l+1,azi_l)-(azaltvals.at(alt_l,azi_l)))/
				(azaltvals.at(alt_l+1,0)-(azaltvals.at(alt_l,0))))*alt_d;
			else reduc = (fQ.at(0,0)/((x.at(1)-x.at(0))*(y.at(1)-y.at(0))))*(x.at(1)-altitude)*(y.at(1)-azimuth)
				+(fQ.at(1,0)/((x.at(1)-x.at(0))*(y.at(1)-y.at(0))))*(altitude-x.at(0))*(y.at(1)-azimuth)
				+(fQ.at(0,1)/((x.at(1)-x.at(0))*(y.at(1)-y.at(0))))*(x.at(1)-altitude)*(azimuth-y.at(0))
				+(fQ.at(1,1)/((x.at(1)-x.at(0))*(y.at(1)-y.at(0))))*(altitude-x.at(0))*(azimuth-y.at(0));
		}

		return reduc;
	}
	

	void accumulate_monthly(const std::string &hourly_var, const std::string &monthly_var)
	{
		
		size_t count = 0;
		ssc_number_t *hourly = as_array(hourly_var, &count);

		if (!hourly || count != 8760)
			throw exec_error("pvsamv1", "Failed to accumulate hourly: " + hourly_var + " to monthly: " + monthly_var);

	
		ssc_number_t *monthly = allocate( monthly_var, 12 );

		int c = 0;
		for (int i=0;i<12;i++) // each month
		{
			monthly[i] = 0;
			for (int d=0;d<util::nday[i];d++) // for each day in each month
				for (int h=0;h<24;h++) // for each hour in each day
					monthly[i] += hourly[c++];
		}
	}

	void accumulate_annual(const std::string &hourly_var, const std::string &annual_var)
	{
		size_t count = 0;
		ssc_number_t *hourly = as_array(hourly_var, &count);

		if (!hourly || count != 8760)
			throw exec_error("pvsamv1", "Failed to accumulate hourly: " + hourly_var + " to annual: " + annual_var);
		
		double annual = 0;
		for (int i=0;i<8760;i++)
			annual += hourly[i];

		assign( annual_var, var_data( (ssc_number_t) annual ) );
	}

};

DEFINE_MODULE_ENTRY( pvsamv1, "Photovoltaic performance model, SAM component models V.1", 1 )
