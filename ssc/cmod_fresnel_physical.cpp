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

#include "core.h"

#include "common.h"

#include "csp_solver_fresnel_collector_receiver.h"
#include "csp_solver_pc_Rankine_indirect_224.h"
#include "csp_solver_tou_block_schedules.h"
#include "csp_dispatch.h"

#include <algorithm>
#include <iterator>

static var_info _cm_vtab_fresnel_physical[] = {

    // Solar Field (from cmod_tcsmslf.cpp)
    { SSC_INPUT,    SSC_NUMBER,         "nMod",                   "Number of collector modules in a loop",                                                 "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "nRecVar",                "Number of receiver variantions",                                                        "",              "",  "controller",            "?=4",      "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "nLoops",                 "Number of loops in the field",                                                          "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "eta_pump",               "HTF pump efficiency",                                                                   "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "HDR_rough",              "Header pipe roughness",                                                                 "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "theta_stow",             "stow angle",                                                                            "deg",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "theta_dep",              "deploy angle",                                                                          "deg",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "FieldConfig",            "Number of subfield headers",                                                            "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_startup",              "Power block startup temperature",                                                       "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "pb_rated_cap",           "Rated plant capacity",                                                                  "MWe",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "m_dot_htfmin",           "Minimum loop HTF flow rate",                                                            "kg/s",          "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "m_dot_htfmax",           "Maximum loop HTF flow rate",                                                            "kg/s",          "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_loop_in_des",          "Design loop inlet temperature",                                                         "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_loop_out",             "Target loop outlet temperature",                                                        "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "Fluid",                  "Field HTF fluid number",                                                                "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_field_ini",            "Initial field temperature",                                                             "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_MATRIX,         "field_fl_props",         "Fluid property data",                                                                   "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_fp",                   "Freeze protection temperature (heat trace activation temperature)",                     "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "I_bn_des",               "Solar irradiation at design",                                                           "W/m2",          "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "V_hdr_max",              "Maximum HTF velocity in the header at design",                                          "m/s",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "V_hdr_min",              "Minimum HTF velocity in the header at design",                                          "m/s",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "Pipe_hl_coef",           "Loss coefficient from the header - runner pipe - and non-HCE piping",                   "W/m2-K",        "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "SCA_drives_elec",        "Tracking power in Watts per SCA drive",                                                 "W/module",      "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "fthrok",                 "Flag to allow partial defocusing of the collectors",                                    "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "fthrctrl",               "Defocusing strategy",                                                                   "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "ColAz",                  "Collector azimuth angle",                                                               "deg",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "solar_mult",             "Solar multiple",                                                                        "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "mc_bal_hot",             "The heat capacity of the balance of plant on the hot side",                             "kWht/K-MWt",    "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "mc_bal_cold",            "The heat capacity of the balance of plant on the cold side",                            "kWht/K-MWt",    "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "mc_bal_sca",             "Non-HTF heat capacity associated with each SCA - per meter basis",                      "Wht/K-m",       "",  "controller",            "*",        "",              ""},

    { SSC_INPUT,    SSC_NUMBER,         "water_per_wash",         "Water usage per wash",                                                                  "L/m2_aper",     "",  "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,    SSC_NUMBER,         "washes_per_year",        "Mirror washing frequency",                                                              "none",          "",  "solar_field",    "*",                       "",                      "" },

    { SSC_INPUT,    SSC_NUMBER,         "opt_model",              "The optical model",                                                                     "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_NUMBER,         "A_aperture",             "Reflective aperture area of the collector",                                             "m2",            "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "reflectivity",           "Solar-weighted mirror reflectivity value",                                              "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "TrackingError",          "Tracking error derate",                                                                 "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "GeomEffects",            "Geometry effects derate",                                                               "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "Dirt_mirror",            "User-defined dirt on mirror derate",                                                    "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "Error",                  "User-defined general optical error derate",                                             "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "L_mod",                  "The length of the collector module",                                                    "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "IAM_T_coefs",            "Incidence angle modifier coefficients - transversal plane",                             "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "IAM_L_coefs",            "Incidence angle modifier coefficients - longitudinal plane",                            "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_MATRIX,         "OpticalTable",           "Values of the optical efficiency table",                                                "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "rec_model",              "Receiver model type (1=Polynomial ; 2=Evac tube)",                                      "",              "",  "controller",            "*",        "INTEGER",       ""},
    { SSC_INPUT,    SSC_ARRAY,          "HCE_FieldFrac",          "The fraction of the field occupied by this HCE type",                                   "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "D_abs_in",               "The inner absorber tube diameter",                                                      "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "D_abs_out",              "The outer absorber tube diameter",                                                      "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "D_glass_in",             "The inner glass envelope diameter",                                                     "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "D_glass_out",            "The outer glass envelope diameter",                                                     "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "D_plug",                 "The diameter of the absorber flow plug (optional)",                                     "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "Flow_type",              "The flow type through the absorber",                                                    "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "Rough",                  "Roughness of the internal surface",                                                     "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "alpha_env",              "Envelope absorptance",                                                                  "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_MATRIX,         "epsilon_abs_1",          "Absorber emittance - HCE variation 1",                                                  "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_MATRIX,         "epsilon_abs_2",          "Absorber emittance - HCE variation 2",                                                  "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_MATRIX,         "epsilon_abs_3",          "Absorber emittance - HCE variation 3",                                                  "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_MATRIX,         "epsilon_abs_4",          "Absorber emittance - HCE variation 4",                                                  "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "alpha_abs",              "Absorber absorptance",                                                                  "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "Tau_envelope",           "Envelope transmittance",                                                                "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "epsilon_glass",          "Glass envelope emissivity",                                                             "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "GlazingIntactIn",        "The glazing intact flag",                                                               "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "P_a",                    "Annulus gas pressure",                                                                  "torr",          "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "AnnulusGas",             "Annulus gas type (1=air; 26=Ar; 27=H2)",                                                "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "AbsorberMaterial",       "Absorber material type",                                                                "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "Shadowing",              "Receiver bellows shadowing loss factor",                                                "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "dirt_env",               "Loss due to dirt on the receiver envelope",                                             "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "Design_loss",            "Receiver heat loss at design",                                                          "W/m",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "L_mod_spacing",          "Piping distance between sequential modules in a loop",                                  "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "L_crossover",            "Length of crossover piping in a loop",                                                  "m",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "HL_T_coefs",             "HTF temperature-dependent heat loss coefficients",                                      "W/m-K",         "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "HL_w_coefs",             "Wind-speed-dependent heat loss coefficients",                                           "W/m-(m/s)",     "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "DP_nominal",             "Pressure drop across a single collector assembly at design",                            "bar",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_ARRAY,          "DP_coefs",               "Pressure drop mass flow based part-load curve",                                         "",              "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "rec_htf_vol",            "Volume of HTF in a single collector unit per unit aperture area",                       "L/m2-ap",       "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_amb_sf_des",           "Ambient design-point temperature for the solar field",                                  "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "V_wind_des",             "Design-point wind velocity",                                                            "m/s",           "",  "controller",            "*",        "",              ""},

    { SSC_INPUT,    SSC_NUMBER,         "I_b",                    "Direct normal incident solar irradiation",                                              "kJ/m2-hr",      "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_db",                   "Dry bulb air temperature",                                                              "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "V_wind",                 "Ambient windspeed",                                                                     "m/s",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "P_amb",                  "Ambient pressure",                                                                      "atm",           "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_dp",                   "The dewpoint temperature",                                                              "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "T_cold_in",              "HTF return temperature",                                                                "C",             "",  "controller",            "*",        "",              ""},
    { SSC_INPUT,    SSC_NUMBER,         "defocus",                "Defocus control",                                                                       "",              "",  "controller",            "*",        "",              ""},

    { SSC_INPUT,        SSC_NUMBER,      "tilt",              "Tilt angle of surface/axis",                                                          "",              "",            "Weather",        "*",                       "",                      "" },

    // All other inputs from (cmod_trough_physical.cpp)

    // Weather Reader
    { SSC_INPUT,        SSC_STRING,      "file_name",                 "Local weather file with path",                                                     "none",         "",               "weather",        "*",                       "LOCAL_FILE",            "" },

    // Power Cycle
    { SSC_INPUT,        SSC_NUMBER,      "pc_config",                 "0: Steam Rankine (224), 1: user defined",                                          "-",            "",               "powerblock",     "?=0",                     "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_ref",                     "Rated plant capacity",                                                             "MWe",          "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_ref",                   "Power cycle efficiency at design",                                                 "none",         "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_max_frac",            "Maximum turbine over design operation fraction",                                   "-",            "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_cutoff_frac",         "Minimum turbine operation fraction before shutdown",                               "-",            "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",                "Fraction of thermal power required for standby mode",                              "none",         "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_time",              "Time needed for power block startup",                                              "hr",           "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",              "Fraction of design thermal power needed for startup",                              "none",         "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_pump_coef",              "Pumping power to move 1kg of HTF through PB loop",                                 "kW/kg",        "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",                 "Reference condenser cooling water inlet/outlet T diff",                            "C",            "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",                 "Reference ambient temperature at design point",                                    "C",            "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                        "Flag for using dry cooling or wet cooling system",                                 "none",         "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tech_type",                 "Turbine inlet pressure control flag (sliding=user, fixed=trough)",                 "1/2/3",        "tower/trough/user", "powerblock",  "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",                "Cooling tower approach temperature",                                               "C",            "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",                 "ITD at design for dry system",                                                     "C",            "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",              "Condenser pressure ratio",                                                         "none",         "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",                "Power block blowdown steam fraction ",                                             "none",         "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",                "Minimum condenser pressure",                                                       "inHg",         "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",                  "Number of part-load increments for the heat rejection system",                     "none",         "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",                      "Fraction indicating wet cooling use for hybrid system",                            "none",         "constant=[0,0,0,0,0,0,0,0,0]",     "powerblock", "pc_config=0", "",                    "" },

    // UDPC parameters
    { SSC_INPUT,        SSC_NUMBER,      "ud_f_W_dot_cool_des",       "Percent of user-defined power cycle design gross output consumed by cooling",      "%",            "",               "powerblock",     "pc_config=1",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ud_m_dot_water_cool_des",   "Mass flow rate of water required at user-defined power cycle design point",        "kg/s",         "",               "powerblock",     "pc_config=1",             "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "ud_ind_od",                 "Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb",   "", "",          "powerblock",     "pc_config=1",             "",                      "" },

    // TES
    { SSC_INPUT,        SSC_NUMBER,      "store_fluid",               "Material number for storage fluid",                                                "-",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "store_fl_props",            "User defined storage fluid property data",                                         "-",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tshours",                   "Equivalent full-load thermal storage hours",                                       "hr",           "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank",                    "Total height of tank (height of HTF when tank is full",                            "m",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "u_tank",                    "Loss coefficient from the tank",                                                   "W/m2-K",       "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_pairs",                "Number of equivalent tank pairs",                                                  "-",            "",               "TES",            "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_Thtr",             "Minimum allowable hot tank HTF temp",                                              "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_max_heat",         "Rated heater capacity for hot tank heating",                                       "MWe",          "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_Thtr",            "Minimum allowable cold tank HTF temp",                                             "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_max_heat",        "Rated heater capacity for cold tank heating",                                      "MWe",          "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dt_hot",                    "Hot side HX approach temp",                                                        "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank_min",                "Minimum allowable HTF height in storage tank",                                     "m",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "init_hot_htf_percent",      "Initial fraction of avail. vol that is hot",                                       "%",            "",               "TES",            "*",                       "",                      "" },

    // TOU
    { SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",          "12x24 CSP operation Time-of-Use Weekday schedule",                                 "-",            "",               "tou",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",          "12x24 CSP operation Time-of-Use Weekend schedule",                                 "-",            "",               "tou",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_tod_pc_target_also_pc_max", "Is the TOD target cycle heat input also the max cycle heat input?",             "",             "",               "tou",            "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_dispatch",               "Allow dispatch optimization?",  /*TRUE=1*/                                         "-",            "",               "tou",            "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "can_cycle_use_standby",     "Can the cycle use standby operation?",                                             "",             "",               "tou",            "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_write_ampl_dat",         "Write AMPL data files for dispatch run",                                           "-",            "",               "tou",            "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_ampl_engine",            "Run dispatch optimization with external AMPL engine",                              "-",            "",               "tou",            "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_STRING,      "ampl_data_dir",             "AMPL data file directory",                                                         "-",            "",               "tou",            "?=''",                    "",                      "" },
    { SSC_INPUT,        SSC_STRING,      "ampl_exec_call",            "System command to run AMPL code",                                                  "-",            "",               "tou",            "?='ampl sdk_solution.run'", "",                    "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_frequency",            "Frequency for dispatch optimization calculations",                                 "hour",         "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_steps_per_hour",       "Time steps per hour for dispatch optimization calculations",                       "-",            "",               "tou",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_horizon",              "Time horizon for dispatch optimization",                                           "hour",         "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_max_iter",             "Max. no. dispatch optimization iterations",                                        "-",            "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_timeout",              "Max. dispatch optimization solve duration",                                        "s",            "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_mip_gap",              "Dispatch optimization solution tolerance",                                         "-",            "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_spec_presolve",        "Dispatch optimization presolve heuristic",                                         "-",            "",               "tou",            "?=-1",                    "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_spec_bb",              "Dispatch optimization B&B heuristic",                                              "-",            "",               "tou",            "?=-1",                    "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_reporting",            "Dispatch optimization reporting level",                                            "-",            "",               "tou",            "?=-1",                    "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_spec_scaling",         "Dispatch optimization scaling heuristic",                                          "-",            "",               "tou",            "?=-1",                    "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_time_weighting",       "Dispatch optimization future time discounting factor",                             "-",            "",               "tou",            "?=0.99",                  "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_rsu_cost_rel",         "Receiver startup cost",                                                            "$/MWt/start",  "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_csu_cost_rel",         "Cycle startup cost",                                                               "$/MWe-cycle/start", "",          "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_pen_ramping",          "Dispatch cycle production change penalty",                                         "$/MWe-change", "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_inventory_incentive",  "Dispatch storage terminal inventory incentive multiplier",                         "",             "",               "System Control", "?=0.0",                   "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_rec_standby",             "Receiver standby energy consumption",                                              "kWt",          "",               "tou",            "?=9e99",                  "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_rec_heattrace",           "Receiver heat trace energy consumption during startup",                            "kWe-hr",       "",               "tou",            "?=0.0",                   "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_wlim_series",            "Use time-series net electricity generation limits",                                "",             "",               "tou",            "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "wlim_series",               "Time series net electicity generation limits",                                     "kWe",          "",               "tou",            "is_wlim_series=1",        "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "f_turb_tou_periods",        "Dispatch logic for turbine load fraction",                                         "-",            "",               "tou",            "*",                       "",                      "" },

    { SSC_INPUT,        SSC_NUMBER,      "csp_financial_model",       "",                                                                                 "1-8",          "",               "Financial Model",        "?=1",                                                      "INTEGER,MIN=0",  "" },
    { SSC_INPUT,        SSC_NUMBER,      "ppa_multiplier_model",      "PPA multiplier model 0: dispatch factors dispatch_factorX, 1: hourly multipliers dispatch_factors_ts", "0/1", "0=diurnal,1=timestep", "tou",   "?=0",  /*need a default so this var works in required_if*/ "INTEGER,MIN=0",  "" },
    { SSC_INPUT,        SSC_ARRAY,       "dispatch_factors_ts",       "Dispatch payment factor array",                                                    "",             "",               "tou",                    "ppa_multiplier_model=1&csp_financial_model<5&is_dispatch=1","",              "" },
    { SSC_INPUT,        SSC_NUMBER,      "ppa_soln_mode",             "PPA solution mode (0=Specify IRR target, 1=Specify PPA price)",                    "",             "",               "Financial Solution Mode","ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",              "" },
    { SSC_INPUT,        SSC_NUMBER,      "en_electricity_rates",      "Enable electricity rates for grid purchase",                                       "0/1",          "",               "Electricity Rates",      "?=0",                                                       "",              "" },
    { SSC_INPUT,        SSC_MATRIX,      "dispatch_sched_weekday",    "12x24 PPA pricing Weekday schedule",                                               "",             "",               "tou",                     "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",             "" },
    { SSC_INPUT,        SSC_MATRIX,      "dispatch_sched_weekend",    "12x24 PPA pricing Weekend schedule",                                               "",             "",               "tou",                     "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",             "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor1",          "Dispatch payment factor 1",                                                        "",             "",               "tou",                     "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",             "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor2",          "Dispatch payment factor 2",                                                        "",             "",               "tou",                     "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",             "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor3",          "Dispatch payment factor 3",                                                        "",             "",               "tou",                     "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",             "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor4",          "Dispatch payment factor 4",                                                        "",             "",               "tou",                     "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",             "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor5",          "Dispatch payment factor 5",                                                        "",             "",               "tou",                     "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",             "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor6",          "Dispatch payment factor 6",                                                        "",             "",               "tou",                     "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",             "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor7",          "Dispatch payment factor 7",                                                        "",             "",               "tou",                     "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",             "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor8",          "Dispatch payment factor 8",                                                        "",             "",               "tou",                     "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",             "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor9",          "Dispatch payment factor 9",                                                        "",             "",               "tou",                     "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",             "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_dispatch_series",        "Use time-series dispatch factors",                                                 "",             "",               "tou",                     "?=1",                                                       "",             "" },
    { SSC_INPUT,        SSC_ARRAY,       "dispatch_series",           "Time series dispatch factors",                                                     "",             "",               "tou",                     "",                                                          "",             "" },
    { SSC_INPUT,        SSC_ARRAY,       "timestep_load_fractions",   "Turbine load fraction for each timestep, alternative to block dispatch",           "",             "",               "tou",                     "?",                                                         "",             "" },
    { SSC_INPUT,        SSC_ARRAY,       "ppa_price_input",			  "PPA prices - yearly",			                                                  "$/kWh",	      "",	            "Revenue",			       "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",      	    "" },
    { SSC_INPUT,        SSC_MATRIX,      "mp_energy_market_revenue",  "Energy market revenue input",                                                      "",             "Lifetime x 2[Cleared Capacity(MW),Price($/MWh)]", "Revenue", "csp_financial_model=6&is_dispatch=1",      "",             "" },

    { SSC_INPUT,        SSC_NUMBER,      "pb_fixed_par",              "Fraction of rated gross power constantly consumed",                                "MWe/MWcap",    "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "bop_array",                 "Balance of plant parasitic power fraction, mult frac and const, linear and quad coeff", "",        "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "aux_array",                 "Auxiliary heater, mult frac and const, linear and quad coeff",                     "",             "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "gross_net_conversion_factor", "Estimated gross to net conversion factor",                                       "",             "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "water_usage_per_wash",      "Water usage per wash",                                                             "L/m2_aper",    "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "washing_frequency",         "Mirror washing frequency",                                                         "-/year",       "",               "system",         "*",                       "",                      "" },

    // Newly added
    { SSC_INPUT,        SSC_NUMBER,      "calc_design_pipe_vals",     "Calculate temps and pressures at design conditions for runners and headers",       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_cold_max",            "Maximum HTF velocity in the cold headers at design",                               "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_cold_min",            "Minimum HTF velocity in the cold headers at design",                               "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_hot_max",             "Maximum HTF velocity in the hot headers at design",                                "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_hot_min",             "Minimum HTF velocity in the hot headers at design",                                "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "N_max_hdr_diams",           "Maximum number of diameters in each of the hot and cold headers",                  "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_rnr_pb",                  "Length of runner pipe in power block",                                             "m",            "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_rnr_per_xpan",            "Threshold length of straight runner pipe without an expansion loop",               "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_xpan_hdr",                "Compined perpendicular lengths of each header expansion loop",                     "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_xpan_rnr",                "Compined perpendicular lengths of each runner expansion loop",                     "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Min_rnr_xpans",             "Minimum number of expansion loops per single-diameter runner section",             "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "northsouth_field_sep",      "North/south separation between subfields. 0 = SCAs are touching",                  "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "N_hdr_per_xpan",            "Number of collector loops per expansion loop",                                     "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "offset_xpan_hdr",           "Location of first header expansion loop. 1 = after first collector loop",          "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "K_cpnt",                    "Interconnect component minor loss coefficients, row=intc, col=cpnt",               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_cpnt",                    "Interconnect component diameters, row=intc, col=cpnt",                             "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "L_cpnt",                    "Interconnect component lengths, row=intc, col=cpnt",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Type_cpnt",                 "Interconnect component type, row=intc, col=cpnt",                                  "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "custom_sf_pipe_sizes",      "Use custom solar field pipe diams, wallthks, and lengths",                         "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_rnr_diams",              "Custom runner diameters",                                                          "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_rnr_wallthicks",         "Custom runner wall thicknesses",                                                   "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_rnr_lengths",            "Custom runner lengths",                                                            "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_hdr_diams",              "Custom header diameters",                                                          "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_hdr_wallthicks",         "Custom header wall thicknesses",                                                   "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_hdr_lengths",            "Custom header lengths",                                                            "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tanks_in_parallel",         "Tanks are in parallel, not in series, with solar field",                           "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "has_hot_tank_bypass",       "Bypass valve connects field outlet to cold tank",                                  "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_hot_inlet_min",      "Minimum hot tank htf inlet temperature",                                           "C",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_pump_coef",             "Pumping power to move 1kg of HTF through tes loop",                                "kW/(kg/s)",    "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_tes_des",                 "Design-point velocity to size the TES pipe diameters",                             "m/s",          "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "custom_tes_p_loss",         "TES pipe losses are based on custom lengths and coeffs",                           "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "k_tes_loss_coeffs",         "Minor loss coeffs for the coll, gen, and bypass loops",                            "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "custom_tes_pipe_sizes",     "Use custom TES pipe diams, wallthks, and lengths",                                 "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "tes_diams",                 "Custom TES diameters",                                                             "m",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "tes_wallthicks",            "Custom TES wall thicknesses",                                                      "m",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "tes_lengths",               "Custom TES lengths",                                                               "m",            "",               "controller",     "",                        "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "DP_SGS",                    "Pressure drop within the steam generator",                                         "bar",          "",               "controller",     "*",                       "",                      "" },

    // Needed for auto-updating dependent inputs
    { SSC_INPUT,        SSC_NUMBER,      "use_solar_mult_or_aperture_area",     "Use solar multiple or total field aperture area",                        "-",            "",               "controller",     "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "specified_solar_multiple",            "specified_solar_multiple",                                               "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "specified_total_aperture",            "specified_total_aperture",                                               "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "non_solar_field_land_area_multiplier", "non_solar_field_land_area_multiplier",                                  "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "trough_loop_control",                 "trough_loop_control",                                                    "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_wlim_maxspec",                   "disp_wlim_maxspec",                                                      "-",            "",               "controller",     "*",                       "",                      "" },


    var_info_invalid };

class cm_fresnel_physical : public compute_module
{
public:

    cm_fresnel_physical()
    {
        add_var_info(_cm_vtab_fresnel_physical);
    }

    void exec()
    {

        // General
        bool is_dispatch = as_boolean("is_dispatch");

        // Weather reader
        C_csp_weatherreader weather_reader;
        C_csp_solver::S_sim_setup sim_setup;
        int n_steps_fixed;
        int steps_per_hour;
        {
            weather_reader.m_weather_data_provider = std::make_shared<weatherfile>(as_string("file_name"));
            weather_reader.m_filename = as_string("file_name");
            weather_reader.m_trackmode = 0;
            weather_reader.m_tilt = 0.0;
            weather_reader.m_azimuth = 0.0;
            // Initialize to get weather file info
            weather_reader.init();
            if (weather_reader.has_error()) throw exec_error("fresnel_physical", weather_reader.get_error());

            // Set up ssc output arrays
            // Set steps per hour
            double nhourssim = 8760.0;                                  //[hr] Number of hours to simulate
            
            sim_setup.m_sim_time_start = 0.0;                           //[s] starting first hour of year
            sim_setup.m_sim_time_end = nhourssim * 3600.;                 //[s] full year simulation

            steps_per_hour = 1;                                     //[-]

            int n_wf_records = (int)weather_reader.m_weather_data_provider->nrecords();
            steps_per_hour = n_wf_records / 8760;                       //[-]

            n_steps_fixed = steps_per_hour * 8760;                    //[-]
            sim_setup.m_report_step = 3600.0 / (double)steps_per_hour;  //[s]
        }

        // Solar field, trough
        C_csp_fresnel_collector_receiver c_fresnel;
        {
            c_fresnel.m_nMod = as_integer("nMod");
            c_fresnel.m_nRecVar = as_integer("nRecVar");
            c_fresnel.m_nLoops = as_integer("nLoops");
            c_fresnel.m_eta_pump = as_number("eta_pump");
            c_fresnel.m_HDR_rough = as_number("HDR_rough");
            c_fresnel.m_theta_stow = as_number("theta_stow");
            c_fresnel.m_theta_dep = as_number("theta_dep");
            c_fresnel.m_FieldConfig = as_integer("FieldConfig");
            c_fresnel.m_T_startup = as_number("T_startup");

            c_fresnel.m_m_dot_htfmin = as_number("m_dot_htfmin");
            c_fresnel.m_m_dot_htfmax = as_number("m_dot_htfmax");
            c_fresnel.m_T_loop_in_des = as_number("T_loop_in_des");

            c_fresnel.m_T_loop_out_des = as_number("T_loop_out");
            c_fresnel.m_Fluid = as_integer("Fluid");

            c_fresnel.m_field_fl_props = as_matrix("field_fl_props");
            c_fresnel.m_T_fp = as_number("T_fp");
            c_fresnel.m_I_bn_des = as_number("I_bn_des");
            c_fresnel.m_V_hdr_max = as_number("V_hdr_max");
            c_fresnel.m_V_hdr_min = as_number("V_hdr_min");
            c_fresnel.m_Pipe_hl_coef = as_number("Pipe_hl_coef");
            c_fresnel.m_SCA_drives_elec = as_number("SCA_drives_elec");
            c_fresnel.m_fthrok = as_integer("fthrok");
            c_fresnel.m_fthrctrl = as_integer("fthrctrl");
            c_fresnel.m_ColAz = as_number("ColAz");
            c_fresnel.m_ColTilt = as_number("tilt");

            c_fresnel.m_solar_mult= as_number("solar_mult");

            c_fresnel.m_mc_bal_hot = as_number("mc_bal_hot");
            c_fresnel.m_mc_bal_cold = as_number("mc_bal_cold");
            c_fresnel.m_mc_bal_sca = as_number("mc_bal_sca");

            c_fresnel.m_opt_model = as_integer("opt_model");


            c_fresnel.m_A_aperture = as_number("A_aperture");
            c_fresnel.m_reflectivity = as_number("reflectivity");
            c_fresnel.m_TrackingError = as_number("TrackingError");
            c_fresnel.m_GeomEffects = as_number("GeomEffects");
            c_fresnel.m_Dirt_mirror = as_number("Dirt_mirror");
            c_fresnel.m_Error = as_number("Error");
            c_fresnel.m_L_mod = as_number("L_mod");

            size_t size;
            double* IAM_T_coefs = as_array("IAM_T_coefs", &size);
            c_fresnel.m_IAM_T_coefs.assign(IAM_T_coefs, IAM_T_coefs + size);

            double* IAM_L_coefs = as_array("IAM_L_coefs", &size);
            c_fresnel.m_IAM_L_coefs.assign(IAM_L_coefs, IAM_L_coefs + size);

            c_fresnel.m_OpticalTable = as_matrix("OpticalTable");
            
            c_fresnel.m_rec_model = as_integer("rec_model");

            double* HCE_FieldFrac = as_array("HCE_FieldFrac", &size);
            c_fresnel.m_HCE_FieldFrac.assign(HCE_FieldFrac, HCE_FieldFrac + size);

            double* D_abs_in = as_array("D_abs_in", &size);
            c_fresnel.m_D_abs_in.assign(D_abs_in, D_abs_in + size);

            double* D_abs_out = as_array("D_abs_out", &size);
            c_fresnel.m_D_abs_out.assign(D_abs_out, D_abs_out + size);

            double* D_glass_in = as_array("D_glass_in", &size);
            c_fresnel.m_D_glass_in.assign(D_glass_in, D_glass_in + size);

            double* D_glass_out = as_array("D_glass_out", &size);
            c_fresnel.m_D_glass_out.assign(D_glass_out, D_glass_out + size);

            double* D_plug = as_array("D_plug", &size);
            c_fresnel.m_D_plug.assign(D_plug, D_plug + size);

            double* Flow_type = as_array("Flow_type", &size);
            c_fresnel.m_Flow_type.assign(Flow_type, Flow_type + size);

            double* Rough = as_array("Rough", &size);
            c_fresnel.m_Rough.assign(Rough, Rough + size);

            double* alpha_env = as_array("alpha_env", &size);
            c_fresnel.m_alpha_env.assign(alpha_env, alpha_env + size);

            c_fresnel.m_epsilon_abs_1 = as_matrix_transpose("epsilon_abs_1");
            c_fresnel.m_epsilon_abs_2 = as_matrix_transpose("epsilon_abs_2");
            c_fresnel.m_epsilon_abs_3 = as_matrix_transpose("epsilon_abs_3");
            c_fresnel.m_epsilon_abs_4 = as_matrix_transpose("epsilon_abs_4");

            double* alpha_abs = as_array("alpha_abs", &size);
            c_fresnel.m_alpha_abs.assign(alpha_abs, alpha_abs + size);

            double* Tau_envelope = as_array("Tau_envelope", &size);
            c_fresnel.m_Tau_envelope.assign(Tau_envelope, Tau_envelope + size);

            double* epsilon_glass = as_array("epsilon_glass", &size);
            c_fresnel.m_epsilon_glass.assign(epsilon_glass, epsilon_glass + size);

            double* GlazingIntact = as_array("GlazingIntactIn", &size);
            c_fresnel.m_GlazingIntact.assign(GlazingIntact, GlazingIntact + size);






            double* P_a = as_array("P_a", &size);
            c_fresnel.m_P_a.assign(P_a, P_a + size);

            double* AnnulusGas = as_array("AnnulusGas", &size);
            c_fresnel.m_AnnulusGas.assign(AnnulusGas, AnnulusGas + size);

            double* AbsorberMaterial = as_array("AbsorberMaterial", &size);
            c_fresnel.m_AbsorberMaterial.assign(AbsorberMaterial, AbsorberMaterial + size);

            double* Shadowing = as_array("Shadowing", &size);
            c_fresnel.m_Shadowing.assign(Shadowing, Shadowing + size);

            double* dirt_env = as_array("dirt_env", &size);
            c_fresnel.m_dirt_env.assign(dirt_env, dirt_env + size);

            double* Design_loss = as_array("Design_loss", &size);
            c_fresnel.m_Design_loss.assign(Design_loss, Design_loss + size);

            c_fresnel.m_L_mod_spacing = as_number("L_mod_spacing");
            c_fresnel.m_L_crossover = as_number("L_crossover");

            double* HL_T_coefs = as_array("HL_T_coefs", &size);
            c_fresnel.m_HL_T_coefs.assign(HL_T_coefs, HL_T_coefs + size);

            double* HL_w_coefs = as_array("HL_w_coefs", &size);
            c_fresnel.m_HL_w_coefs.assign(HL_w_coefs, HL_w_coefs + size);

            c_fresnel.m_DP_nominal = as_number("DP_nominal");

            double* DP_coefs = as_array("DP_coefs", &size);
            c_fresnel.m_DP_coefs.assign(DP_coefs, DP_coefs + size);

            c_fresnel.m_rec_htf_vol = as_number("rec_htf_vol");



            //////////////////////// Questionable
            c_fresnel.m_I_b = as_number("I_b");
            c_fresnel.m_V_wind_des = as_number("V_wind_des");
            c_fresnel.m_T_amb_sf_des = as_number("T_amb_sf_des");
        }

        // Power cycle
        C_csp_power_cycle* p_csp_power_cycle;
        C_pc_Rankine_indirect_224 rankine_pc; // Steam Rankine and User Defined power cycle classes
        {
            int pb_tech_type = as_integer("pc_config");
            if (!(pb_tech_type == 0 || pb_tech_type == 1))  // 0 = Rankine, 1 = UDPC
            {
                throw exec_error("trough_physical", "unsupported power cycle");
            }
            else
            {
                C_pc_Rankine_indirect_224::S_params* pc = &rankine_pc.ms_params;
                pc->m_P_ref = as_double("P_ref");
                pc->m_eta_ref = as_double("eta_ref");
                pc->m_T_htf_hot_ref = as_double("T_loop_out");
                pc->m_T_htf_cold_ref = as_double("T_loop_in_des");
                pc->m_cycle_max_frac = as_double("cycle_max_frac");
                pc->m_cycle_cutoff_frac = as_double("cycle_cutoff_frac");
                pc->m_q_sby_frac = as_double("q_sby_frac");
                pc->m_startup_time = as_double("startup_time");
                pc->m_startup_frac = as_double("startup_frac");
                pc->m_htf_pump_coef = as_double("pb_pump_coef");
                pc->m_pc_fl = as_integer("Fluid");                            // power cycle HTF is same as receiver HTF
                pc->m_pc_fl_props = as_matrix("field_fl_props");
                pc->DP_SGS = as_double("DP_SGS");

                if (pb_tech_type == 0)
                {
                    pc->m_dT_cw_ref = as_double("dT_cw_ref");
                    pc->m_T_amb_des = as_double("T_amb_des");
                    //pc->m_P_boil = as_double("P_boil");
                    pc->m_P_boil_des = 100.0;       //[bar]
                    pc->m_CT = as_integer("CT");                    // cooling tech type: 1=evaporative, 2=air, 3=hybrid    
                    pc->m_tech_type = as_integer("tech_type");      // turbine inlet pressure: 1: Fixed, 3: Sliding
                    if (pc->m_tech_type == 1) { pc->m_tech_type = 2; }; // changing fixed pressure for the tower to fixed pressure for the trough
                    if (pc->m_tech_type == 3) { pc->m_tech_type = 8; }; // changing sliding pressure for the tower to sliding pressure for the trough
                    if (!(pc->m_tech_type == 2 || pc->m_tech_type == 5 || pc->m_tech_type == 6 || pc->m_tech_type == 8))
                    {
                        std::string tech_msg = util::format("tech_type must be either 2 (fixed pressure) or 8 (sliding). Input was %d."
                            " Simulation proceeded with fixed pressure", pc->m_tech_type);
                        pc->m_tech_type = 2;
                    }
                    pc->m_T_approach = as_double("T_approach");
                    pc->m_T_ITD_des = as_double("T_ITD_des");
                    pc->m_P_cond_ratio = as_double("P_cond_ratio");
                    pc->m_pb_bd_frac = as_double("pb_bd_frac");
                    pc->m_P_cond_min = as_double("P_cond_min");
                    pc->m_n_pl_inc = as_integer("n_pl_inc");

                    size_t n_F_wc = 0;
                    ssc_number_t* p_F_wc = as_array("F_wc", &n_F_wc);
                    pc->m_F_wc.resize(n_F_wc, 0.0);
                    for (size_t i = 0; i < n_F_wc; i++)
                        pc->m_F_wc[i] = (double)p_F_wc[i];

                    pc->m_is_user_defined_pc = false;
                    pc->m_W_dot_cooling_des = std::numeric_limits<double>::quiet_NaN();
                }
                else if (pb_tech_type == 1)
                {
                    pc->m_is_user_defined_pc = true;

                    // User-Defined Cycle Parameters
                    pc->m_W_dot_cooling_des = as_double("ud_f_W_dot_cool_des") / 100.0 * as_double("P_ref");  //[MWe]
                    pc->m_m_dot_water_des = as_double("ud_m_dot_water_cool_des");       //[kg/s]

                    // User-Defined Cycle Off-Design Tables 
                    pc->mc_combined_ind = as_matrix("ud_ind_od");
                }

                // Set pointer to parent class
                p_csp_power_cycle = &rankine_pc;

                // Set power cycle outputs common to all power cycle technologies
                p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_Q_DOT_HTF, allocate("q_pb", n_steps_fixed), n_steps_fixed);
                p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_M_DOT_HTF, allocate("m_dot_pc", n_steps_fixed), n_steps_fixed);
                p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_Q_DOT_STARTUP, allocate("q_dot_pc_startup", n_steps_fixed), n_steps_fixed);
                p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_W_DOT, allocate("P_cycle", n_steps_fixed), n_steps_fixed);
                p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_T_HTF_IN, allocate("T_pc_in", n_steps_fixed), n_steps_fixed);
                p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_T_HTF_OUT, allocate("T_pc_out", n_steps_fixed), n_steps_fixed);
                p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_M_DOT_WATER, allocate("m_dot_water_pc", n_steps_fixed), n_steps_fixed);
                p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_W_DOT_HTF_PUMP, allocate("cycle_htf_pump_power", n_steps_fixed), n_steps_fixed);
                p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_W_DOT_COOLER, allocate("P_cooling_tower_tot", n_steps_fixed), n_steps_fixed);

                // Dependent reported variable
                p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_ETA_THERMAL, allocate("eta", n_steps_fixed), n_steps_fixed);
            }

        }

        // TES
        C_csp_two_tank_tes storage;
        {
            util::matrix_t<double> tes_lengths;
            if (is_assigned("tes_lengths")) {
                tes_lengths = as_matrix("tes_lengths");               //[m]
            }
            if (!is_assigned("tes_lengths") || tes_lengths.ncells() < 11) {
                double vals1[11] = { 0., 90., 100., 120., 0., 30., 90., 80., 80., 120., 80. };
                tes_lengths.assign(vals1, 11);
            }
            storage = C_csp_two_tank_tes(
                as_integer("Fluid"),
                as_matrix("field_fl_props"),
                as_integer("store_fluid"),
                as_matrix("store_fl_props"),
                as_double("P_ref") / as_double("eta_ref"),
                as_double("solar_mult"),
                as_double("P_ref") / as_double("eta_ref") * as_double("tshours"),
                as_double("h_tank"),
                as_double("u_tank"),
                as_integer("tank_pairs"),
                as_double("hot_tank_Thtr"),
                as_double("hot_tank_max_heat"),
                as_double("cold_tank_Thtr"),
                as_double("cold_tank_max_heat"),
                as_double("dt_hot"),
                as_double("T_loop_in_des"),
                as_double("T_loop_out"),
                as_double("T_loop_out"),
                as_double("T_loop_in_des"),
                as_double("h_tank_min"),
                as_double("init_hot_htf_percent"),
                as_double("pb_pump_coef"),
                as_boolean("tanks_in_parallel"),
                as_double("V_tes_des"),
                as_boolean("calc_design_pipe_vals"),
                as_double("tes_pump_coef"),
                as_double("eta_pump"),
                as_boolean("has_hot_tank_bypass"),
                as_double("T_tank_hot_inlet_min"),
                as_boolean("custom_tes_p_loss"),
                as_boolean("custom_tes_pipe_sizes"),
                as_matrix("k_tes_loss_coeffs"),
                as_matrix("tes_diams"),
                as_matrix("tes_wallthicks"),
                tes_lengths,
                as_double("HDR_rough"),
                as_double("DP_SGS")
            );

            // Set storage outputs
            int n_wf_records = (int)weather_reader.m_weather_data_provider->nrecords();
            double steps_per_hour = n_wf_records / 8760;
            int n_steps_fixed = steps_per_hour * 8760;
            storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_Q_DOT_LOSS, allocate("tank_losses", n_steps_fixed), n_steps_fixed);
            storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_W_DOT_HEATER, allocate("q_tes_heater", n_steps_fixed), n_steps_fixed);
            storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_TES_T_HOT, allocate("T_tes_hot", n_steps_fixed), n_steps_fixed);
            storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_TES_T_COLD, allocate("T_tes_cold", n_steps_fixed), n_steps_fixed);
            storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_M_DOT_TANK_TO_TANK, allocate("m_dot_cold_tank_to_hot_tank", n_steps_fixed), n_steps_fixed);
            storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_MASS_COLD_TANK, allocate("mass_tes_cold", n_steps_fixed), n_steps_fixed);
            storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_MASS_HOT_TANK, allocate("mass_tes_hot", n_steps_fixed), n_steps_fixed);
            storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_W_DOT_HTF_PUMP, allocate("tes_htf_pump_power", n_steps_fixed), n_steps_fixed);

        }
        
        // TOU
        C_csp_tou_block_schedules tou;
        C_csp_tou_block_schedules::S_params* tou_params = &tou.ms_params;
        double ppa_price_year1 = std::numeric_limits<double>::quiet_NaN();
        {
            tou_params->mc_csp_ops.mc_weekdays = as_matrix("weekday_schedule");
            tou_params->mc_csp_ops.mc_weekends = as_matrix("weekend_schedule");
            if (tou_params->mc_pricing.mc_weekdays.ncells() == 1) {
                // Resize default value from var table to proper dimensions
                tou_params->mc_pricing.mc_weekdays = util::matrix_t<double>(12, 24, 1.0);
            }
            if (tou_params->mc_pricing.mc_weekends.ncells() == 1) {
                // Resize default value from var table to proper dimensions
                tou_params->mc_pricing.mc_weekends = util::matrix_t<double>(12, 24, 1.0);
            }

            tou.mc_dispatch_params.m_is_tod_pc_target_also_pc_max = as_boolean("is_tod_pc_target_also_pc_max");
            tou.mc_dispatch_params.m_is_block_dispatch = !as_boolean("is_dispatch");      //mw
            tou.mc_dispatch_params.m_use_rule_1 = true;
            tou.mc_dispatch_params.m_standby_off_buffer = 2.0;
            tou.mc_dispatch_params.m_use_rule_2 = false;
            tou.mc_dispatch_params.m_q_dot_rec_des_mult = -1.23;
            tou.mc_dispatch_params.m_f_q_dot_pc_overwrite = -1.23;

            size_t n_f_turbine = 0;
            ssc_number_t* p_f_turbine = as_array("f_turb_tou_periods", &n_f_turbine);
            tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC].resize(n_f_turbine, 0.0);
            //tou_params->mv_t_frac.resize(n_f_turbine, 0.0);
            for (size_t i = 0; i < n_f_turbine; i++)
                tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC][i] = (double)p_f_turbine[i];

            // Load fraction by time step:
            bool is_load_fraction_by_timestep = is_assigned("timestep_load_fractions");
            tou_params->mc_csp_ops.mv_is_diurnal = !(is_load_fraction_by_timestep);
            if (is_load_fraction_by_timestep) {
                size_t N_load_fractions;
                ssc_number_t* load_fractions = as_array("timestep_load_fractions", &N_load_fractions);
                std::copy(load_fractions, load_fractions + N_load_fractions, std::back_inserter(tou_params->mc_csp_ops.timestep_load_fractions));
            }

            int csp_financial_model = as_integer("csp_financial_model");

            
            if (csp_financial_model > 0 && csp_financial_model < 5) {   // Single Owner financial models

                // Get first year base ppa price
                bool is_ppa_price_input_assigned = is_assigned("ppa_price_input");
                if (is_dispatch && !is_ppa_price_input_assigned) {
                    throw exec_error("trough_physical", "\n\nYou selected dispatch optimization which requires that the array input ppa_price_input is defined\n");
                }

                if (is_ppa_price_input_assigned) {
                    size_t count_ppa_price_input;
                    ssc_number_t* ppa_price_input_array = as_array("ppa_price_input", &count_ppa_price_input);
                    ppa_price_year1 = (double)ppa_price_input_array[0];  // [$/kWh]
                }
                else {
                    ppa_price_year1 = 1.0;      //[-] don't need ppa multiplier if not optimizing
                }

                int ppa_soln_mode = as_integer("ppa_soln_mode");    // PPA solution mode (0=Specify IRR target, 1=Specify PPA price)
                if (ppa_soln_mode == 0 && is_dispatch) {
                    throw exec_error("trough_physical", "\n\nYou selected dispatch optimization and the Specify IRR Target financial solution mode, "
                        "but dispatch optimization requires known absolute electricity prices. Dispatch optimization requires "
                        "the Specify PPA Price financial solution mode. You can continue using dispatch optimization and iteratively "
                        "solve for the PPA that results in a target IRR by running a SAM Parametric analysis or script.\n");
                }

                int en_electricity_rates = as_integer("en_electricity_rates");  // 0 = Use PPA, 1 = Use Retail
                if (en_electricity_rates == 1 && is_dispatch) {
                    throw exec_error("trough_physical", "\n\nYou selected dispatch optimization and the option to Use Retail Electricity Rates on the Electricity Purchases page, "
                        "but the dispatch optimization model currently does not accept separate buy and sell prices. Please use the Use PPA or Market Prices option "
                        "on the Electricity Purchases page.\n");
                }

                // Time-of-Delivery factors by time step:
                int ppa_mult_model = as_integer("ppa_multiplier_model");
                if (ppa_mult_model == 1)    // use dispatch_ts input
                {
                    tou_params->mc_pricing.mv_is_diurnal = false;

                    if (is_assigned("dispatch_factors_ts") || is_dispatch) {
                        size_t nmultipliers;
                        ssc_number_t* multipliers = as_array("dispatch_factors_ts", &nmultipliers);
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(nmultipliers, 0.0);
                        for (size_t ii = 0; ii < nmultipliers; ii++)
                            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][ii] = multipliers[ii];
                    }
                    else {
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(n_steps_fixed, -1.0);
                    }
                }
                else if (ppa_mult_model == 0) // standard diuranal input
                {
                    tou_params->mc_pricing.mv_is_diurnal = true;

                    bool are_all_assigned = is_assigned("dispatch_sched_weekday") || is_assigned("dispatch_sched_weekend")
                        || is_assigned("dispatch_factor1") || is_assigned("dispatch_factor2") || is_assigned("dispatch_factor3")
                        || is_assigned("dispatch_factor4") || is_assigned("dispatch_factor5") || is_assigned("dispatch_factor6")
                        || is_assigned("dispatch_factor7") || is_assigned("dispatch_factor8") || is_assigned("dispatch_factor9");

                    if (are_all_assigned || is_dispatch) {

                        tou_params->mc_pricing.mc_weekdays = as_matrix("dispatch_sched_weekday");
                        tou_params->mc_pricing.mc_weekends = as_matrix("dispatch_sched_weekend");

                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(9, 0.0);
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][0] = as_double("dispatch_factor1");
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][1] = as_double("dispatch_factor2");
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][2] = as_double("dispatch_factor3");
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][3] = as_double("dispatch_factor4");
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][4] = as_double("dispatch_factor5");
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][5] = as_double("dispatch_factor6");
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][6] = as_double("dispatch_factor7");
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][7] = as_double("dispatch_factor8");
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][8] = as_double("dispatch_factor9");
                    }
                    else {
                        tou_params->mc_pricing.mc_weekdays.resize_fill(12, 24, 1.);
                        tou_params->mc_pricing.mc_weekends.resize_fill(12, 24, 1.);
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(9, -1.0);
                    }
                }
            }
            else if (csp_financial_model == 5) {    // Commercial
                if (is_dispatch) {
                    throw exec_error("trough_physical", "\nDispatch optimization current not enabled for the Commercial financial model\n");
                    // need to add pricing lookup for Commercial financial model
                }
                else {
                    tou_params->mc_pricing.mv_is_diurnal = false;

                    tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(n_steps_fixed, -1.0);
                }
            }
            else if (csp_financial_model == 6) {     // use 'mp_energy_market_revenue' -> from Merchant Plant model

                tou_params->mc_pricing.mv_is_diurnal = false;

                if (is_dispatch) {
                    util::matrix_t<double> mp_energy_market_revenue = as_matrix("mp_energy_market_revenue"); // col 0 = cleared capacity, col 1 = $/MWh
                    size_t n_rows = mp_energy_market_revenue.nrows();
                    if (n_rows < n_steps_fixed) {
                        string ppa_msg = util::format("mp_energy_market_revenue input has %d rows but there are %d number of timesteps", n_rows, n_steps_fixed);
                        throw exec_error("trough_physical", ppa_msg);
                    }

                    double conv_dolmwh_to_centkwh = 0.1;
                    tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(n_steps_fixed, 0.0);
                    for (size_t ii = 0; ii < n_steps_fixed; ii++) {
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][ii] = mp_energy_market_revenue(ii, 1) * conv_dolmwh_to_centkwh; //[cents/kWh]
                    }
                }
                else { // if no dispatch optimization, don't need an input pricing schedule
                    tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(n_steps_fixed, -1.0);
                }
            }
            else if (csp_financial_model == 7 || csp_financial_model == 8) {    // LCOE (7) and None (8)

                tou_params->mc_pricing.mv_is_diurnal = false;

                // No hourly electricity pricing in these financial models
                // However, may still want to solve with dispatch optimization to avoid rapid startup/shutdown, so set to uniform schedule
                tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(n_steps_fixed, 1.0);
            }
            else {
                throw exec_error("trough_physical", "csp_financial_model must be 1-8");
            }
        }

        // System Parameters
        C_csp_solver::S_csp_system_params system;
        {
            system.m_pb_fixed_par = as_double("pb_fixed_par");
            size_t nval_bop_array = 0;
            ssc_number_t* bop_array = as_array("bop_array", &nval_bop_array);
            if (nval_bop_array != 5) throw exec_error("trough_physical", "Should be 5 elements in bop_array, has " + util::to_string((int)nval_bop_array) + ".");
            system.m_bop_par = bop_array[0];    //as_double("bop_par");
            system.m_bop_par_f = bop_array[1];    //as_double("bop_par_f");
            system.m_bop_par_0 = bop_array[2];    //as_double("bop_par_0");
            system.m_bop_par_1 = bop_array[3];    //as_double("bop_par_1");
            system.m_bop_par_2 = bop_array[4];    //as_double("bop_par_2");
        }

        // System Dispatch
        csp_dispatch_opt dispatch;
        {
            if (as_boolean("is_dispatch")) {

                // System Design Parameters
                double W_dot_cycle_des = as_double("P_ref");    //[MWe]
                double eta_cycle = as_double("eta_ref");        //[-]

                // System Design Calcs
                double q_dot_cycle_des = W_dot_cycle_des / eta_cycle;   //[MWt]
                double q_dot_rec_des = q_dot_cycle_des * as_double("solar_mult"); //[MWt]

                dispatch.solver_params.set_user_inputs(as_boolean("is_dispatch"), as_integer("disp_steps_per_hour"), as_integer("disp_frequency"), as_integer("disp_horizon"),
                    as_integer("disp_max_iter"), as_double("disp_mip_gap"), as_double("disp_timeout"),
                    as_integer("disp_spec_presolve"), as_integer("disp_spec_bb"), as_integer("disp_spec_scaling"), as_integer("disp_reporting"),
                    as_boolean("is_write_ampl_dat"), as_boolean("is_ampl_engine"), as_string("ampl_data_dir"), as_string("ampl_exec_call"));

                double disp_csu_cost_calc = as_double("disp_csu_cost_rel") * W_dot_cycle_des; //[$/start]
                double disp_rsu_cost_calc = as_double("disp_rsu_cost_rel") * q_dot_rec_des;   //[$/start]
                dispatch.params.set_user_params(as_boolean("can_cycle_use_standby"), as_double("disp_time_weighting"),
                    disp_rsu_cost_calc, 0.0, disp_csu_cost_calc, as_double("disp_pen_ramping"),
                    as_double("disp_inventory_incentive"), as_double("q_rec_standby"), as_double("q_rec_heattrace"), ppa_price_year1);
            }
            else {
                dispatch.solver_params.dispatch_optimize = false;
            }
        }
        

        // Instantiate Solver
        C_csp_solver csp_solver(weather_reader,
            c_fresnel,
            *p_csp_power_cycle,
            storage,
            tou,
            dispatch,
            system,
            NULL,
            nullptr,
            ssc_cmod_update,
            (void*)(this));


        // Set solver reporting outputs
        {
            // Simulation Kernel
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TIME_FINAL, allocate("time_hr", n_steps_fixed), n_steps_fixed);
            // Weather reader
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::MONTH, allocate("month", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::HOUR_DAY, allocate("hour_day", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::SOLAZ, allocate("solazi", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::SOLZEN, allocate("solzen", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::BEAM, allocate("beam", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TDRY, allocate("tdry", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TWET, allocate("twet", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::RH, allocate("RH", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::WSPD, allocate("wspd", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PRES, allocate("pres", n_steps_fixed), n_steps_fixed);

            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CR_DEFOCUS, allocate("defocus", n_steps_fixed), n_steps_fixed);
            // TES
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_Q_DOT_DC, allocate("q_dc_tes", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_Q_DOT_CH, allocate("q_ch_tes", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_E_CH_STATE, allocate("e_ch_tes", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::M_DOT_CR_TO_TES_HOT, allocate("m_dot_cr_to_tes_hot", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::M_DOT_TES_HOT_OUT, allocate("m_dot_tes_hot_out", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::M_DOT_PC_TO_TES_COLD, allocate("m_dot_pc_to_tes_cold", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::M_DOT_TES_COLD_OUT, allocate("m_dot_tes_cold_out", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::M_DOT_FIELD_TO_CYCLE, allocate("m_dot_field_to_cycle", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::M_DOT_CYCLE_TO_FIELD, allocate("m_dot_cycle_to_field", n_steps_fixed), n_steps_fixed);
            // System
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::SYS_W_DOT_FIXED, allocate("P_fixed", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::SYS_W_DOT_BOP, allocate("P_plant_balance_tot", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::W_DOT_NET, allocate("P_out_net", n_steps_fixed), n_steps_fixed);
            // Controller
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::OP_MODE_1, allocate("op_mode_1", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::OP_MODE_2, allocate("op_mode_2", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::OP_MODE_3, allocate("op_mode_3", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::ERR_M_DOT, allocate("m_dot_balance", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::ERR_Q_DOT, allocate("q_balance", n_steps_fixed), n_steps_fixed);

            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::N_OP_MODES, allocate("n_op_modes", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TOU_PERIOD, allocate("tou_value", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PRICING_MULT, allocate("pricing_mult", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PC_Q_DOT_SB, allocate("q_dot_pc_sb", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PC_Q_DOT_MIN, allocate("q_dot_pc_min", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PC_Q_DOT_TARGET, allocate("q_dot_pc_target", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PC_Q_DOT_MAX, allocate("q_dot_pc_max", n_steps_fixed), n_steps_fixed);

            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_IS_REC_SU, allocate("is_rec_su_allowed", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_IS_PC_SU, allocate("is_pc_su_allowed", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_IS_PC_SB, allocate("is_pc_sb_allowed", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::EST_Q_DOT_CR_SU, allocate("q_dot_est_cr_su", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::EST_Q_DOT_CR_ON, allocate("q_dot_est_cr_on", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::EST_Q_DOT_DC, allocate("q_dot_est_tes_dc", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::EST_Q_DOT_CH, allocate("q_dot_est_tes_ch", n_steps_fixed), n_steps_fixed);

            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_OP_MODE_SEQ_A, allocate("operating_modes_a", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_OP_MODE_SEQ_B, allocate("operating_modes_b", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_OP_MODE_SEQ_C, allocate("operating_modes_c", n_steps_fixed), n_steps_fixed);

            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_REL_MIP_GAP, allocate("disp_rel_mip_gap", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_STATE, allocate("disp_solve_state", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SUBOPT_FLAG, allocate("disp_subopt_flag", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_ITER, allocate("disp_solve_iter", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_OBJ, allocate("disp_objective", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_OBJ_RELAX, allocate("disp_obj_relax", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_QSF_EXPECT, allocate("disp_qsf_expected", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_QSFPROD_EXPECT, allocate("disp_qsfprod_expected", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_QSFSU_EXPECT, allocate("disp_qsfsu_expected", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_TES_EXPECT, allocate("disp_tes_expected", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_PCEFF_EXPECT, allocate("disp_pceff_expected", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SFEFF_EXPECT, allocate("disp_thermeff_expected", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_QPBSU_EXPECT, allocate("disp_qpbsu_expected", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_WPB_EXPECT, allocate("disp_wpb_expected", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_REV_EXPECT, allocate("disp_rev_expected", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_PRES_NCONSTR, allocate("disp_presolve_nconstr", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_PRES_NVAR, allocate("disp_presolve_nvar", n_steps_fixed), n_steps_fixed);
            csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_TIME, allocate("disp_solve_time", n_steps_fixed), n_steps_fixed);
        }

        update("Initialize physical fresnel model...", 0.0);

        // Initialize Solver
        int out_type = -1;
        std::string out_msg = "";
        try
        {
            // Initialize Solver
            csp_solver.init();
        }
        catch (C_csp_exception& csp_exception)
        {
            // Report warning before exiting with error
            while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
            {
                log(out_msg, out_type);
            }

            throw exec_error("fresnel_physical", csp_exception.m_error_message);
        }

        // If no exception, then report messages
        while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
        {
            log(out_msg, out_type);
        }

        //if the pricing schedule is provided as hourly, overwrite the tou schedule
        if (as_boolean("is_dispatch_series"))
        {
            size_t n_dispatch_series;
            ssc_number_t* dispatch_series = as_array("dispatch_series", &n_dispatch_series);

            //if( n_dispatch_series != n_steps_fixed)
            //    throw exec_error("trough_physical", "Invalid dispatch pricing series dimension. Array length must match number of simulation time steps ("+my_to_string(n_steps_fixed)+").");

            //resize the m_hr_tou array
            if (tou_params->mc_pricing.m_hr_tou != 0)
                delete[] tou_params->mc_pricing.m_hr_tou;
            tou_params->mc_pricing.m_hr_tou = new double[n_steps_fixed];
            //set the tou period as unique for each time step
            for (int i = 0; i < n_steps_fixed; i++)
                tou_params->mc_pricing.m_hr_tou[i] = i + 1;
            //allocate reported arrays
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(n_steps_fixed);
            for (int i = 0; i < n_steps_fixed; i++)
                tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][i] = dispatch_series[i];
        }

        update("Begin timeseries simulation...", 0.0);

        std::clock_t clock_start = std::clock();

        // Run Simulation
        try
        {
            // Simulate
            csp_solver.Ssimulate(sim_setup);
        }
        catch (C_csp_exception& csp_exception)
        {
            // Report warning before exiting with error
            while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
            {
                log(out_msg);
            }

            throw exec_error("trough_physical", csp_exception.m_error_message);
        }

        // If no exception, then report messages
        while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
        {
            log(out_msg, out_type);
        }

        std::clock_t clock_end = std::clock();
        double sim_duration = (clock_end - clock_start) / (double)CLOCKS_PER_SEC;		//[s]
        assign("sim_duration", (ssc_number_t)sim_duration);
        assign("solar_multiple_actual", as_double("solar_mult"));   // calculated during verify() using cmod_csp_trough_eqns.cpp

        // Convert Units
        return;
        {
            // Do unit post-processing here
            double* p_q_pc_startup = allocate("q_pc_startup", n_steps_fixed);
            size_t count_pc_su = 0;
            ssc_number_t* p_q_dot_pc_startup = as_array("q_dot_pc_startup", &count_pc_su);
            if ((int)count_pc_su != n_steps_fixed)
            {
                log("q_dot_pc_startup array is a different length than 'n_steps_fixed'.", SSC_WARNING);
                return;
            }
            for (int i = 0; i < n_steps_fixed; i++)
            {
                p_q_pc_startup[i] = (float)(p_q_dot_pc_startup[i] * (sim_setup.m_report_step / 3600.0));    //[MWh]
            }

            // Convert mass flow rates from [kg/hr] to [kg/s]
            size_t count_m_dot_pc, count_m_dot_water_pc; //count_m_dot_rec, count_m_dot_tes_dc, count_m_dot_tes_ch;
            count_m_dot_pc = count_m_dot_water_pc = 0; //count_m_dot_rec = count_m_dot_tes_dc = count_m_dot_tes_ch = 0;
            ssc_number_t* p_m_dot_pc = as_array("m_dot_pc", &count_m_dot_pc);
            ssc_number_t* p_m_dot_water_pc = as_array("m_dot_water_pc", &count_m_dot_water_pc);
            //ssc_number_t *p_m_dot_tes_dc = as_array("m_dot_tes_dc", &count_m_dot_tes_dc);
            //ssc_number_t *p_m_dot_tes_ch = as_array("m_dot_tes_ch", &count_m_dot_tes_ch);
            if ((int)count_m_dot_pc != n_steps_fixed || (int)count_m_dot_water_pc != n_steps_fixed)
                //|| count_m_dot_rec != n_steps_fixed || count_m_dot_tes_dc != n_steps_fixed || count_m_dot_tes_ch != n_steps_fixed)
            {
                log("At least one m_dot array is a different length than 'n_steps_fixed'.", SSC_WARNING);
                return;
            }
            for (int i = 0; i < n_steps_fixed; i++)
            {
                //p_m_dot_rec[i] = (ssc_number_t)(p_m_dot_rec[i] / 3600.0); //[kg/s] convert from kg/hr
                p_m_dot_pc[i] = (ssc_number_t)(p_m_dot_pc[i] / 3600.0);     //[kg/s] convert from kg/hr
                p_m_dot_water_pc[i] = (ssc_number_t)(p_m_dot_water_pc[i] / 3600.0); //[kg/s] convert from kg/hr
                //p_m_dot_tes_dc[i] = (ssc_number_t)(p_m_dot_tes_dc[i] / 3600.0);     //[kg/s] convert from kg/hr
                //p_m_dot_tes_ch[i] = (ssc_number_t)(p_m_dot_tes_ch[i] / 3600.0);     //[kg/s] convert from kg/hr
            }

            size_t count;
            ssc_number_t* p_W_dot_net = as_array("P_out_net", &count);
            ssc_number_t* p_time_final_hr = as_array("time_hr", &count);
            if ((int)count != n_steps_fixed)
                throw exec_error("trough_physical", "The number of fixed steps does not match the length of output data arrays");

            // 'adjustment_factors' class stores factors in hourly array, so need to index as such
            adjustment_factors haf(this, "adjust");
            if (!haf.setup(n_steps_fixed))
                throw exec_error("trough_physical", "failed to setup adjustment factors: " + haf.error());

            ssc_number_t* p_gen = allocate("gen", n_steps_fixed);
            //ssc_number_t *p_W_dot_par_tot_haf = allocate("W_dot_par_tot_haf", n_steps_fixed);
            ssc_number_t* p_q_dot_defocus_est = allocate("q_dot_defocus_est", n_steps_fixed);

            //ssc_number_t *p_W_dot_parasitic_tot = as_array("W_dot_parasitic_tot", &count);
            //if (count != n_steps_fixed)
            //    throw exec_error("trough_physical", "The number of fixed steps does not match the length of output data arrays1");

            ssc_number_t* p_SCAs_def = as_array("SCAs_def", &count);
            if ((int)count != n_steps_fixed)
                throw exec_error("trough_physical", "The number of fixed steps does not match the length of output data arrays2");

            ssc_number_t* p_q_dot_htf_sf_out = as_array("q_dot_htf_sf_out", &count);
            if ((int)count != n_steps_fixed)
                throw exec_error("trough_physical", "The number of fixed steps does not match the length of output data arrays3");

            //ssc_number_t *p_m_dot_tes_dc = as_array("m_dot_tes_dc", &count);
            //if ((int)count != n_steps_fixed)
            //    throw exec_error("trough_physical", "The number of fixed steps for 'm_dot_tes_dc' does not match the length of output data arrays");
            //
            //ssc_number_t *p_m_dot_tes_ch = as_array("m_dot_tes_ch", &count);
            //if ((int)count != n_steps_fixed)
            //    throw exec_error("trough_physical", "The number of fixed steps for 'm_dot_tes_ch' does not match the length of output data arrays");
            for (int i = 0; i < n_steps_fixed; i++)
            {
                size_t hour = (size_t)ceil(p_time_final_hr[i]);
                p_gen[i] = (ssc_number_t)(p_W_dot_net[i] * haf(hour) * 1.E3);     //[kWe]
                //p_W_dot_parasitic_tot[i] *= -1.0;           //[kWe] Label is total parasitics, so change to a positive value
                //p_W_dot_par_tot_haf[i] = (ssc_number_t)(p_W_dot_parasitic_tot[i] * haf(hour) * 1.E3);       //[kWe]
                p_q_dot_defocus_est[i] = (ssc_number_t)(1.0 - p_SCAs_def[i]) * p_q_dot_htf_sf_out[i]; //[MWt]
                //p_m_dot_tes_dc[i] = (ssc_number_t)(p_m_dot_tes_dc[i] / 3600.0);     //[kg/s] convert from kg/hr
                //p_m_dot_tes_ch[i] = (ssc_number_t)(p_m_dot_tes_ch[i] / 3600.0);     //[kg/s] convert from kg/hr

            }
            ssc_number_t* p_annual_energy_dist_time = gen_heatmap(this, steps_per_hour);


        }

        int x = 0;
       
    }

};

DEFINE_MODULE_ENTRY(fresnel_physical, "Physical Fresnel applications", 1)
