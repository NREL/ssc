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
#include "csp_solver_fresnel_collector_receiver.h"

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
        int x = 0;

        //C_csp_fresnel_collector_receiver* c = new C_csp_fresnel_collector_receiver();
        C_csp_fresnel_collector_receiver* c = new C_csp_fresnel_collector_receiver();
        int y = 0;

    }

};

DEFINE_MODULE_ENTRY(fresnel_physical, "Physical trough applications", 1)
