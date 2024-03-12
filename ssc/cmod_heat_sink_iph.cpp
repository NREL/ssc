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

// Trough CSP - physical model
#include "core.h"
//#include "tckernel.h"

// for adjustment factors
#include "common.h"

//#include "lib_weatherfile.h
//#include "csp_solver_util.h"
#include "csp_solver_core.h"
#include "csp_solver_trough_collector_receiver.h"
#include "csp_solver_pc_heat_sink.h"
#include "csp_solver_two_tank_tes.h"
#include "csp_solver_tou_block_schedules.h"
#include "csp_dispatch.h"
#include "csp_system_costs.h"
//#include "cmod_csp_common_eqns.h"

#include <ctime>
#include <algorithm>
#include <iterator>

// signed/unsigned mismatch
#pragma warning (disable : 4388)

static var_info _cm_vtab_heat_sink_iph[] = {




    /* VARTYPE          DATATYPE         NAME                         LABEL                                                                               UNITS           META              GROUP             REQUIRED_IF                CONSTRAINTS         UI_HINTS*/

    { SSC_INPUT,        SSC_NUMBER,      "sim_type",                  "1 (default): timeseries, 2: design only",                                          "",             "",               "System Control", "?=1",                    "",                       "SIMULATION_PARAMETER"},

    // Weather Reader
    { SSC_INPUT,        SSC_STRING,      "file_name",                 "Local weather file with path",                                                     "none",         "",               "weather",        "?",                       "LOCAL_FILE",            "" },
    { SSC_INPUT,        SSC_TABLE,       "solar_resource_data",       "Weather resource data in memory",                                                  "",             "",               "weather",        "?",                       "",                      "SIMULATION_PARAMETER" },
    //{ SSC_INPUT,        SSC_NUMBER,      "track_mode",                "Tracking mode",                                                                    "none",         "",               "weather",        "*",                       "",                      "" },

    // Solar Field, Trough

    { SSC_INPUT,        SSC_NUMBER,      "q_pb_design",               "Design heat input to power block",                                                 "MWt",          "",               "System_Design",  "*",                       "",                      "" },

    { SSC_INPUT,        SSC_NUMBER,      "nHCEt",                     "Number of HCE types",                                                              "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nColt",                     "Number of collector types",                                                        "none",         "constant=4",     "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nHCEVar",                   "Number of HCE variants per type",                                                  "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "FieldConfig",               "Number of subfield headers",                                                       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_pump",                  "HTF pump efficiency",                                                              "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Fluid",                     "Field HTF fluid ID number",                                                        "none",         "",               "solar_field",    "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "fthrok",                    "Flag to allow partial defocusing of the collectors",                               "W/SCA",        "",               "solar_field",    "*",                       "INTEGER",               "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "fthrctrl",                  "Defocusing strategy",                                                              "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "accept_loc",                "In acceptance testing mode - temperature sensor location",                         "1/2",          "hx/loop",        "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "HDR_rough",                 "Header pipe roughness",                                                            "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "theta_stow",                "Stow angle",                                                                       "deg",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "theta_dep",                 "Deploy angle",                                                                     "deg",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Row_Distance",              "Spacing between rows (centerline to centerline)",                                  "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_loop_in_des",             "Design loop inlet temperature",                                                    "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_loop_out",                "Target loop outlet temperature",                                                   "C",            "",               "solar_field",    "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_startup",                 "Required temperature of the system before the power block can be switched on",     "C",            "",               "solar_field",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htfmin",              "Minimum loop HTF flow rate",                                                       "kg/s",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htfmax",              "Maximum loop HTF flow rate",                                                       "kg/s",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "field_fl_props",            "User defined field fluid property data",                                           "-",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_fp",                      "Freeze protection temperature (heat trace activation temperature)",                "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_bn_des",                  "Solar irradiation at design",                                                      "C",            "",               "solar_field",    "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "V_hdr_max",                 "Maximum HTF velocity in the header at design",                                     "W/m2",         "",               "solar_field",    "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "V_hdr_min",                 "Minimum HTF velocity in the header at design",                                     "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Pipe_hl_coef",              "Loss coefficient from the header, runner pipe, and non-HCE piping",                "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "SCA_drives_elec",           "Tracking power, in Watts per SCA drive",                                           "W/m2-K",       "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tilt",                      "Tilt angle of surface/axis",                                                       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "azimuth",                   "Azimuth angle of surface/axis",                                                    "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "wind_stow_speed",           "Trough wind stow speed",                                                           "m/s",          "",               "solar_field",    "?=50",                    "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "accept_mode",               "Acceptance testing mode?",                                                         "0/1",          "no/yes",         "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "accept_init",               "In acceptance testing mode - require steady-state startup",                        "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_hot",                "Heat capacity of the balance of plant on the hot side",                            "kWht/K-MWt",   "none",           "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_cold",               "Heat capacity of the balance of plant on the cold side",                           "kWht/K-MWt",   "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_sca",                "Non-HTF heat capacity associated with each SCA - per meter basis",                 "Wht/K-m",      "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "W_aperture",                "The collector aperture width (Total structural area used for shadowing)",          "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "A_aperture",                "Reflective aperture area of the collector",                                        "m2",           "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "TrackingError",             "User-defined tracking error derate",                                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "GeomEffects",               "User-defined geometry effects derate",                                             "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Rho_mirror_clean",          "User-defined clean mirror reflectivity",                                           "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Dirt_mirror",               "User-defined dirt on mirror derate",                                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Error",                     "User-defined general optical error derate ",                                       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Ave_Focal_Length",          "Average focal length of the collector ",                                           "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "L_SCA",                     "Length of the SCA ",                                                               "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "L_aperture",                "Length of a single mirror/HCE unit",                                               "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "ColperSCA",                 "Number of individual collector sections in an SCA ",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Distance_SCA",              "Piping distance between SCA's in the field",                                       "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "IAM_matrix",                "IAM coefficients, matrix for 4 collectors",                                        "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "HCE_FieldFrac",             "Fraction of the field occupied by this HCE type ",                                 "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_2",                       "Inner absorber tube diameter",                                                     "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_3",                       "Outer absorber tube diameter",                                                     "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_4",                       "Inner glass envelope diameter ",                                                   "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_5",                       "Outer glass envelope diameter ",                                                   "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_p",                       "Diameter of the absorber flow plug (optional) ",                                   "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Flow_type",                 "Flow type through the absorber",                                                   "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Rough",                     "Relative roughness of the internal HCE surface ",                                  "-",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "alpha_env",                 "Envelope absorptance ",                                                            "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_11",              "Absorber emittance for receiver type 1 variation 1",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_12",              "Absorber emittance for receiver type 1 variation 2",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_13",              "Absorber emittance for receiver type 1 variation 3",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_14",              "Absorber emittance for receiver type 1 variation 4",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_21",              "Absorber emittance for receiver type 2 variation 1",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_22",              "Absorber emittance for receiver type 2 variation 2",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_23",              "Absorber emittance for receiver type 2 variation 3",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_24",              "Absorber emittance for receiver type 2 variation 4",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_31",              "Absorber emittance for receiver type 3 variation 1",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_32",              "Absorber emittance for receiver type 3 variation 2",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_33",              "Absorber emittance for receiver type 3 variation 3",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_34",              "Absorber emittance for receiver type 3 variation 4",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_41",              "Absorber emittance for receiver type 4 variation 1",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_42",              "Absorber emittance for receiver type 4 variation 2",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_43",              "Absorber emittance for receiver type 4 variation 3",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_44",              "Absorber emittance for receiver type 4 variation 4",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "alpha_abs",                 "Absorber absorptance ",                                                            "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Tau_envelope",              "Envelope transmittance",                                                           "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "EPSILON_4",                 "Inner glass envelope emissivities (Pyrex) ",                                       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "EPSILON_5",                 "Outer glass envelope emissivities (Pyrex) ",                                       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "GlazingIntactIn",           "Glazing intact (broken glass) flag {1=true, else=false}",                          "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "P_a",                       "Annulus gas pressure",                                                             "torr",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "AnnulusGas",                "Annulus gas type (1=air, 26=Ar, 27=H2)",                                           "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "AbsorberMaterial",          "Absorber material type",                                                           "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Shadowing",                 "Receiver bellows shadowing loss factor",                                           "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Dirt_HCE",                  "Loss due to dirt on the receiver envelope",                                        "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Design_loss",               "Receiver heat loss at design",                                                     "W/m",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_su_delay",              "Fixed startup delay time for the receiver",                                        "hr",           "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_qf_delay",              "Energy-based receiver startup delay (fraction of rated thermal power)",            "-",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "p_start",                   "Collector startup energy, per SCA",                                                "kWe-hr",       "",               "solar_field",    "*",                       "",                      "" },

    // Heat Sink

    /*Heat Sink*/{ SSC_INPUT,     SSC_NUMBER,         "pb_pump_coef",                "Pumping power to move 1kg of HTF through PB loop",                                      "kW/kg",               "",                              "Heat Sink",           "*",                "",                 "" },



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
    //{ SSC_INPUT,        SSC_NUMBER,      "dt_cold",                   "Cold side HX approach temp",                                                       "C",            "",               "TES",            "*",                      "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_tank_hot_ini",            "Initial hot tank fluid tmeperature",                                               "C",            "",               "TES",            "*",                      "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_tank_cold_ini",           "Initial cold tank fluid tmeperature",                                              "C",            "",               "TES",            "*",                      "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank_min",                "Minimum allowable HTF height in storage tank",                                     "m",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "init_hot_htf_percent",      "Initial fraction of avail. vol that is hot",                                       "%",            "",               "TES",            "*",                       "",                      "" },

    // TOU
    { SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",          "12x24 CSP operation Time-of-Use Weekday schedule",                                 "-",            "",               "tou",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",          "12x24 CSP operation Time-of-Use Weekend schedule",                                 "-",            "",               "tou",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_tod_pc_target_also_pc_max", "Is the TOD target cycle heat input also the max cycle heat input?",             "",             "",               "tou",            "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "can_cycle_use_standby",     "Can the cycle use standby operation?",                                             "",             "",               "tou",            "?=0",                     "",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,      "is_write_ampl_dat",         "Write AMPL data files for dispatch run",                                           "-",            "",               "tou",            "?=0",                     "",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,      "is_ampl_engine",            "Run dispatch optimization with external AMPL engine",                              "-",            "",               "tou",            "?=0",                     "",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_STRING,      "ampl_data_dir",             "AMPL data file directory",                                                         "-",            "",               "tou",            "?=''",                    "",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_STRING,      "ampl_exec_call",            "System command to run AMPL code",                                                  "-",            "",               "tou",            "?='ampl sdk_solution.run'", "",                    "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,      "q_rec_standby",             "Receiver standby energy consumption",                                              "kWt",          "",               "tou",            "?=9e99",                  "",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,      "q_rec_heattrace",           "Receiver heat trace energy consumption during startup",                            "kWe-hr",       "",               "tou",            "?=0.0",                   "",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_ARRAY,       "f_turb_tou_periods",        "Dispatch logic for turbine load fraction",                                         "-",            "",               "tou",            "*",                       "",                      "" },

    { SSC_INPUT,        SSC_NUMBER,      "csp_financial_model",       "",                                                                                 "1-8",          "",               "Financial Model",        "?=1",                                                      "INTEGER,MIN=0",  "" },
    { SSC_INPUT,        SSC_NUMBER,      "ppa_multiplier_model",      "PPA multiplier model 0: dispatch factors dispatch_factorX, 1: hourly multipliers dispatch_factors_ts", "0/1", "0=diurnal,1=timestep", "tou",   "?=0",  /*need a default so this var works in required_if*/ "INTEGER,MIN=0",  "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_ARRAY,       "dispatch_factors_ts",       "Dispatch payment factor array",                                                    "",             "",               "tou",                    "ppa_multiplier_model=1&csp_financial_model<5&is_dispatch=1","",              "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,      "ppa_soln_mode",             "PPA solution mode (0=Specify IRR target, 1=Specify PPA price)",                    "",             "",               "Financial Solution Mode","ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",              "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,      "en_electricity_rates",      "Enable electricity rates for grid purchase",                                       "0/1",          "",               "Electricity Rates",      "?=0",                                                       "",              "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_MATRIX,      "dispatch_sched_weekday",    "12x24 PPA pricing Weekday schedule",                                               "",             "",               "tou",                     "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",             "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_MATRIX,      "dispatch_sched_weekend",    "12x24 PPA pricing Weekend schedule",                                               "",             "",               "tou",                     "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",             "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_ARRAY,       "dispatch_tod_factors",      "TOD factors for periods 1 through 9",                                              "",
        "We added this array input after SAM 2022.12.21 to replace the functionality of former single value inputs dispatch_factor1 through dispatch_factor9",                              "Time of Delivery Factors","ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",  "SIMULATION_PARAMETER" },

    { SSC_INPUT,        SSC_NUMBER,      "is_dispatch_series",        "Use time-series dispatch factors",                                                 "",             "",               "tou",                     "?=1",                                                       "",             "" },
    { SSC_INPUT,        SSC_ARRAY,       "dispatch_series",           "Time series dispatch factors",                                                     "",             "",               "tou",                     "",                                                          "",             "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_timestep_load_fractions","Use turbine load fraction for each timestep instead of block dispatch?",           "",             "",               "tou",                     "?=0",                                                       "",             "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_ARRAY,       "timestep_load_fractions",   "Turbine load fraction for each timestep, alternative to block dispatch",           "",             "",               "tou",                     "?",                                                         "",             "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_ARRAY,       "ppa_price_input",			  "PPA prices - yearly",			                                                  "$/kWh",	      "",	            "Revenue",			       "ppa_multiplier_model=0&csp_financial_model<5&is_dispatch=1","",      	    "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_MATRIX,      "mp_energy_market_revenue",  "Energy market revenue input",                                                      "",             "Lifetime x 2[Cleared Capacity(MW),Price($/MWh)]", "Revenue", "csp_financial_model=6&is_dispatch=1",      "",             "SIMULATION_PARAMETER" },

    // System
    { SSC_INPUT,        SSC_NUMBER,      "pb_fixed_par",              "Fraction of rated gross power constantly consumed",                                "MWe/MWcap",    "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "bop_array",                 "Balance of plant parasitic power fraction, mult frac and const, linear and quad coeff", "",        "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "aux_array",                 "Auxiliary heater, mult frac and const, linear and quad coeff",                     "",             "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "water_usage_per_wash",      "Water usage per wash",                                                             "L/m2_aper",    "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "washing_frequency",         "Mirror washing frequency",                                                         "-/year",       "",               "system",         "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",           "Nameplate capacity",                                                               "kW",           "",               "system",         "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "disp_frequency",            "Frequency for dispatch optimization calculations",                                 "hour",         "",               "Sys_Control",    "is_dispatch=1",           "",                      "" },

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
    //{ SSC_INPUT,        SSC_MATRIX,      "K_cpnt",                    "Interconnect component minor loss coefficients, row=intc, col=cpnt",               "none",         "",               "solar_field",    "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_MATRIX,      "D_cpnt",                    "Interconnect component diameters, row=intc, col=cpnt",                             "none",         "",               "solar_field",    "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_MATRIX,      "L_cpnt",                    "Interconnect component lengths, row=intc, col=cpnt",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_MATRIX,      "Type_cpnt",                 "Interconnect component type, row=intc, col=cpnt",                                  "none",         "",               "solar_field",    "*",                       "",                      "" },
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
    { SSC_INPUT,        SSC_MATRIX,      "tes_diams",                 "Custom TES diameters",                                                             "m",            "",               "controller",     "custom_tes_pipe_sizes=1",                      "",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_MATRIX,      "tes_wallthicks",            "Custom TES wall thicknesses",                                                      "m",            "",               "controller",     "custom_tes_pipe_sizes=1", "",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_MATRIX,      "tes_lengths",               "Custom TES lengths",                                                               "m",            "",               "controller",     "custom_tes_pipe_sizes=1", "",                      "SIMULATION_PARAMETER" },

    // Needed for auto-updating dependent inputs
    { SSC_INPUT,        SSC_NUMBER,      "use_solar_mult_or_aperture_area",     "Use solar multiple or total field aperture area",                        "-",            "",               "controller",     "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "specified_solar_multiple",            "specified_solar_multiple",                                               "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "specified_total_aperture",            "specified_total_aperture",                                               "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "non_solar_field_land_area_multiplier", "non_solar_field_land_area_multiplier",                                  "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "trough_loop_control",                 "trough_loop_control",                                                    "-",            "",               "controller",     "*",                       "",                      "" },


    // ****************************************************************************************************************************************
    //     DEPRECATED INPUTS -- exec() checks if a) variable is assigned and b) if replacement variable is assigned. throws exception if a=true and b=false
    // ****************************************************************************************************************************************
    { SSC_INPUT,        SSC_NUMBER,      "piping_loss",                         "Thermal loss per meter of piping",                                       "Wt/m",         "",               "Tower and Receiver", "",           "",              "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_csu_cost",                       "Cycle startup cost",                                                     "$",            "",               "System Control",     "",           "",              "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_rsu_cost",                       "Receiver startup cost",                                                  "$",            "",               "System Control",     "",           "",              "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_pen_delta_w",                    "Dispatch cycle production change penalty",                               "$/kWe-change", "",               "tou",                "",           "",              "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil",                              "Boiler operating pressure",                                              "bar",          "",               "powerblock",         "",           "",              "SIMULATION_PARAMETER" },


    // ADDED For Design Point
    { SSC_INPUT,        SSC_NUMBER,      "lat",                                 "Latitude",                                                               "degree",       "",               "",                   "*",          "",              "" },


    // Direct Capital Costs
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.site_improvements.cost_per_m2", "Site Improvement Cost per m2",                                     "$/m2",         "",               "Capital_Costs",      "?=0",        "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.solar_field.cost_per_m2",       "Solar Field Cost per m2",                                          "$/m2",         "",               "Capital_Costs",      "?=0",        "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.htf_system.cost_per_m2",        "HTF System Cost Per m2",                                           "$/m2",         "",               "Capital_Costs",      "?=0",        "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.storage.cost_per_kwht",         "Storage cost per kWht",                                            "$/kWht",       "",               "Capital_Costs",      "?=0",        "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.heat_sink.cost_per_kwe",        "Heat Sink Cost per kWe",                                         "$/kWe",        "",               "Capital_Costs",      "?=0",        "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.bop_per_kwe",                   "Balance of Plant Cost per kWe",                                    "$/kWe",        "",               "Capital_Costs",      "?=0",        "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.contingency_percent",           "Contingency Percent",                                              "%",            "",               "Capital_Costs",      "?=0",        "",              "" },
                                                                                                                                                                                                                                
        // Indirect Capital Costs                                                                                                                                                                                               
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.epc.per_acre",                  "EPC Costs per acre",                                               "$/acre",       "",               "Capital_Costs",      "?=0",        "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.epc.percent",                   "EPC Costs % direct",                                               "%",            "",               "Capital_Costs",      "?=0",        "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.epc.per_watt",                  "EPC Cost Wac",                                                     "$/Wac",        "",               "Capital_Costs",      "?=0",        "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.epc.fixed",                     "Fixed EPC Cost",                                                   "$",            "",               "Capital_Costs",      "?=0",        "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.plm.per_acre",                  "Land Cost per acre",                                               "$/acre",       "",               "Capital_Costs",      "?=0",        "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.plm.percent",                   "Land Cost % direct",                                               "%",            "",               "Capital_Costs",      "?=0",        "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.plm.per_watt",                  "Land Cost Wac",                                                    "$/Wac",        "",               "Capital_Costs",      "?=0",        "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.plm.fixed",                     "Fixed Land Cost",                                                  "$",            "",               "Capital_Costs",      "?=0",        "",              "" },
    
        // Sales Tax
    { SSC_INPUT,    SSC_NUMBER,         "csp.dtr.cost.sales_tax.percent",            "Sales Tax Percentage of Direct Cost",                               "%",            "",               "Capital_Costs",      "?=0",        "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "sales_tax_rate",                            "Sales Tax Rate",                                                    "%",            "",               "Capital_Costs",      "?=0",        "",              "" },


    // Construction financing inputs/outputs (SSC variable table from cmod_cb_construction_financing)
    { SSC_INPUT,    SSC_NUMBER,         "const_per_interest_rate1",                  "Interest rate, loan 1",                                             "%",            "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_interest_rate2",                  "Interest rate, loan 2",                                             "%",            "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_interest_rate3",                  "Interest rate, loan 3",                                             "%",            "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_interest_rate4",                  "Interest rate, loan 4",                                             "%",            "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_interest_rate5",                  "Interest rate, loan 5",                                             "%",            "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_months1",                         "Months prior to operation, loan 1",                                 "",             "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_months2",                         "Months prior to operation, loan 2",                                 "",             "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_months3",                         "Months prior to operation, loan 3",                                 "",             "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_months4",                         "Months prior to operation, loan 4",                                 "",             "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_months5",                         "Months prior to operation, loan 5",                                 "",             "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_percent1",                        "Percent of total installed cost, loan 1",                           "%",            "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_percent2",                        "Percent of total installed cost, loan 2",                           "%",            "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_percent3",                        "Percent of total installed cost, loan 3",                           "%",            "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_percent4",                        "Percent of total installed cost, loan 4",                           "%",            "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_percent5",                        "Percent of total installed cost, loan 5",                           "%",            "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_upfront_rate1",                   "Upfront fee on principal, loan 1",                                  "%",            "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_upfront_rate2",                   "Upfront fee on principal, loan 2",                                  "%",            "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_upfront_rate3",                   "Upfront fee on principal, loan 3",                                  "%",            "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_upfront_rate4",                   "Upfront fee on principal, loan 4",                                  "%",            "",               "Financial Parameters",  "*",       "",              "" },
    { SSC_INPUT,    SSC_NUMBER,         "const_per_upfront_rate5",                   "Upfront fee on principal, loan 5",                                  "%",            "",               "Financial Parameters",  "*",       "",              "" },

    // *************************************************************************************************
    //    OUTPUTS
    // *************************************************************************************************

    // Design Point Outputs
    { SSC_OUTPUT,       SSC_NUMBER,      "solar_mult",                       "Actual solar multiple",                                                    "",              "",               "System Design Calc","*",                             "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "nameplate",                        "Nameplate capacity",                                                       "MWe",           "",               "System Design Calc","*",                             "",                      "" },

    // System capacity required by downstream financial model
    { SSC_OUTPUT,       SSC_NUMBER,      "system_capacity",                  "System capacity",                                                          "kWt",          "",                "System Design",  "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "cp_system_nameplate",              "System capacity for capacity payments",                                    "MWt",          "",                "System Design",  "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "cp_battery_nameplate",             "Battery nameplate",                                                        "MWt",          "",                "System Design",  "*",                                "",                      "" },

    // Solar Field
    { SSC_OUTPUT,       SSC_NUMBER,      "nSCA",                             "Number of SCAs in a loop",                                                 "none",          "",               "solar_field",     "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "field_htf_min_temp",               "Minimum field htf temp",                                                   "C",             "",               "Power Cycle",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "field_htf_max_temp",               "Maximum field htf temp",                                                   "C",             "",               "Power Cycle",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "field_htf_cp_avg_des",             "Field average htf cp at design",                                           "kJ/kgK",        "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "single_loop_aperture",             "Single loop aperture",                                                     "m2",            "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "min_inner_diameter",               "Minimum absorber inner diameter in loop",                                  "m",             "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "csp_dtr_hce_design_heat_losses",   "Heat loss at design",                                                      "W/m",           "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp_dtr_loop_hce_heat_loss",       "Loop Heat Loss from HCE at Design",                                        "W/m",           "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "csp_dtr_sca_calc_sca_effs",        "SCA optical efficiencies at design",                                       "",              "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "loop_optical_efficiency",          "Loop total optical effiency at design",                                    "",              "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "csp_dtr_hce_optical_effs",         "HCE optical efficiencies at design",                                       "",              "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_MATRIX,      "SCAInfoArray",                     "Receiver (,1) and collector (,2) type for each assembly in loop",          "",              "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "SCADefocusArray",                  "Order in which the SCA's should be defocused",                             "",              "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "max_field_flow_velocity",          "Maximum field flow velocity",                                              "m/s",           "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "min_field_flow_velocity",          "Minimum field flow velocity",                                              "m/s",           "",               "Solar Field",    "*",                                "",                      "" },

    { SSC_OUTPUT,       SSC_NUMBER,      "total_loop_conversion_efficiency", "Total Loop Conversion Efficiency at Design",                               "",              "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "total_required_aperture_for_SM1",  "Aperture required for solar mult = 1",                                     "m2",            "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "required_number_of_loops_for_SM1", "Heat loss at design",                                                      "",              "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "nLoops",                           "Number of loops in the field",                                             "",              "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "total_aperture",                   "Total field aperture area",                                                "m2",            "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "fixed_land_area",                  "Fixed Land Area",                                                          "acre",          "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "total_land_area",                  "Total Land Area",                                                          "acre",          "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "total_tracking_power",             "Total Tracking Power",                                                     "MWe",           "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_MATRIX,      "K_cpnt",                           "Minor loss coefficients of the components in each loop interconnect",      "",              "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_MATRIX,      "D_cpnt",                           "Inner diameters of the components in each loop interconnect",              "m",             "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_MATRIX,      "L_cpnt",                           "Lengths of the components in each loop interconnect",                      "m",             "",               "Solar Field",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_MATRIX,      "Type_cpnt",                        "Type of component in each loop interconnect [0=fitting | 1=pipe | 2=flex_hose]",  "Wm",     "",               "Solar Field",    "*",                                "",                      "" },

    { SSC_OUTPUT,       SSC_NUMBER,      "field_thermal_output_actual",      "Design-point thermal power from the solar field limited by mass flow", "MW",           "",         "Receiver",                       "*",                                                                "",              "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "field_thermal_output_ideal",       "Design-point thermal power from the solar field with no limit",        "MW",           "",         "Receiver",                       "*",                                                                "",              "" },


    // Thermal Storage
    { SSC_OUTPUT,       SSC_NUMBER,      "vol_tank",                         "Total tank volume",                                                        "m3",            "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "q_tes",                            "TES design capacity",                                                      "MWt-hr",        "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp_pt_tes_tank_diameter",         "Tank diameter",                                                            "m",             "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "q_dot_tes_est",                    "Estimated TES Heat Loss",                                                  "MW",            "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp_pt_tes_htf_density",           "Storage htf density",                                                      "kg/m3",         "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "tes_avail_vol",                    "Available HTF volume",                                                     "m3",            "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "is_hx",                            "System has heat exchanger no/yes (0/1)",                                   "",              "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "vol_min",                          "Minimum Fluid Volume",                                                     "m3",            "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "V_tank_hot_ini",                   "Initial hot tank volume",                                                  "m3",            "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "tes_htf_avg_temp",                 "HTF Average Temperature at Design",                                        "C",             "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "tes_htf_min_temp",                 "Minimum storage htf temp",                                                 "C",             "",               "Power Cycle",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "tes_htf_max_temp",                 "Maximum storage htf temp",                                                 "C",             "",               "Power Cycle",    "*",                                "",                      "" },


    // Collector
    { SSC_OUTPUT,       SSC_MATRIX,      "csp_dtr_sca_ap_lengths",           "Length of single module",                                                  "m",             "",               "Collector",      "?=0",                              "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp_dtr_sca_calc_zenith",          "Calculated zenith",                                                        "degree",        "",               "Collector",      "?=0",                              "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp_dtr_sca_calc_costh",           "Calculated costheta",                                                      "",              "",               "Collector",      "?=0",                              "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp_dtr_sca_calc_theta",           "Calculated theta",                                                         "degree",        "",               "Collector",      "?=0",                              "",                      "" },
    { SSC_OUTPUT,       SSC_MATRIX,      "csp_dtr_sca_calc_end_gains",       "End gain factor",                                                          "",              "",               "Collector",      "?=0",                              "",                      "" },
    { SSC_OUTPUT,       SSC_MATRIX,      "csp_dtr_sca_calc_end_losses",      "Use time-series net electricity generation limits",                        "",              "",               "Collector",      "?=0",                              "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp_dtr_sca_calc_latitude",        "Latitude",                                                                 "degree",        "",               "Collector",      "?=0",                              "",                      "" },
    { SSC_OUTPUT,       SSC_MATRIX,      "csp_dtr_sca_calc_iams",            "IAM at summer solstice",                                                   "",              "",               "Collector",      "?=0",                              "",                      "" },

    // System Control
    { SSC_OUTPUT,       SSC_NUMBER,      "bop_design",                       "BOP parasitics at design",                                                 "MWe",           "",               "System Control", "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "aux_design",                       "Aux parasitics at design",                                                 "MWe",           "",               "System Control", "*",                                "",                      "" },

        // Capital Costs

           // Direct Capital Costs
    { SSC_OUTPUT,       SSC_NUMBER,      "csp.dtr.cost.site_improvements",   "Site improvements cost",                                                   "$",             "",               "Capital Costs",  "",                                 "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp.dtr.cost.solar_field",         "Solar field cost",                                                         "$",             "",               "Capital Costs",  "",                                 "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp.dtr.cost.htf_system",          "HTF system cost",                                                          "$",             "",               "Capital Costs",  "",                                 "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp.dtr.cost.storage",             "Thermal storage cost",                                                     "$",             "",               "Capital Costs",  "",                                 "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp.dtr.cost.heat_sink",           "Heat sink cost",                                                         "$",             "",               "Capital Costs",  "",                                 "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp.dtr.cost.bop",                 "Balance of plant cost",                                                    "$",             "",               "Capital Costs",  "",                                 "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp.dtr.cost.contingency",         "Contingency cost",                                                         "$",             "",               "Capital Costs",  "",                                 "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "total_direct_cost",                "Total direct cost",                                                        "$",             "",               "Capital Costs",  "",                                 "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "direct_subtotal",                  "Direct subtotal",                                                          "$",             "",               "Capital Costs",  "",                                 "",                      "" },


        // Indirect Capital Costs        
    { SSC_OUTPUT,       SSC_NUMBER,      "csp.dtr.cost.epc.total",           "EPC total cost",                                                           "$",             "",               "Capital Costs",  "",                                 "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp.dtr.cost.plm.total",           "Total land cost",                                                          "$",             "",               "Capital Costs",  "",                                 "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "total_indirect_cost",              "Total direct cost",                                                        "$",             "",               "Capital Costs",  "",                                 "",                      "" },


        // Sales Tax
    { SSC_OUTPUT,       SSC_NUMBER,      "csp.dtr.cost.sales_tax.total",    "Sales tax total",                                                           "$",             "",               "Capital Costs",  "",                                 "",                      "" },

        // Total Installed Costs
    { SSC_OUTPUT,       SSC_NUMBER,      "total_installed_cost",      "Total installed cost",                                                            "$",             "",               "Capital Costs",  "",                                 "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp.dtr.cost.installed_per_capacity", "Estimated total installed cost per net capacity ($/kW)",                "$/kW",          "",               "Capital Costs",  "",                                 "",                      "" },

        // Financing
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_principal1",             "Principal, loan 1",                                                        "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_principal2",             "Principal, loan 2",                                                        "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_principal3",             "Principal, loan 3",                                                        "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_principal4",             "Principal, loan 4",                                                        "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_principal5",             "Principal, loan 5",                                                        "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_interest1",              "Interest cost, loan 1",                                                    "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_interest2",              "Interest cost, loan 2",                                                    "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_interest3",              "Interest cost, loan 3",                                                    "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_interest4",              "Interest cost, loan 4",                                                    "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_interest5",              "Interest cost, loan 5",                                                    "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_total1",                 "Total financing cost, loan 1",                                             "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_total2",                 "Total financing cost, loan 2",                                             "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_total3",                 "Total financing cost, loan 3",                                             "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_total4",                 "Total financing cost, loan 4",                                             "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_total5",                 "Total financing cost, loan 5",                                             "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_percent_total",          "Total percent of installed costs, all loans",                              "%",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_principal_total",        "Total principal, all loans",                                               "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "const_per_interest_total",         "Total interest costs, all loans",                                          "$",             "",               "Financial Parameters",   "*",                        "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "construction_financing_cost",      "Total construction financing cost",                                        "$",             "",               "Financial Parameters",   "*",                        "",                      "" },

    // Simulation Kernel
    { SSC_OUTPUT,       SSC_ARRAY,       "time_hr",                   "Time at end of timestep",                                                          "hr",           "",               "solver",         "sim_type=1",                       "",                      "" },
        
    // Weather Reader
    { SSC_OUTPUT,       SSC_ARRAY,       "month",                     "Resource Month",                                                                   "",             "",               "weather",        "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hour_day",                  "Resource Hour of Day",                                                             "",             "",               "weather",        "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solazi",                    "Resource Solar Azimuth",                                                           "deg",          "",               "weather",        "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solzen",                    "Resource Solar Zenith",                                                            "deg",          "",               "weather",        "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "beam",                      "Resource Beam normal irradiance",                                                  "W/m2",         "",               "weather",        "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tdry",                      "Resource Dry bulb temperature",                                                    "C",            "",               "weather",        "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "twet",                      "Resource Wet bulb temperature",                                                    "C",            "",               "weather",        "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "rh",                        "Resource Relative Humidity",                                                       "%",            "",               "weather",        "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "wspd",                      "Resource Wind Speed",                                                              "m/s",          "",               "weather",        "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pres",                      "Resource Pressure",                                                                "mbar",         "",               "weather",        "sim_type=1",                       "",                      "" },
   
    { SSC_OUTPUT,       SSC_ARRAY,       "defocus",                   "Field optical focus fraction",                                                     "",             "",               "weather",        "sim_type=1",                       "",                      "" },

    // Solar Field                                                                                                                                                                                                                   
    { SSC_OUTPUT,       SSC_ARRAY,       "Theta_ave",                 "Field collector solar incidence angle",                                            "deg",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "CosTh_ave",                 "Field collector cosine efficiency",                                                "",             "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "IAM_ave",                   "Field collector incidence angle modifier",                                         "",             "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "RowShadow_ave",             "Field collector row shadowing loss",                                               "",             "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "EndLoss_ave",               "Field collector optical end loss",                                                 "",             "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "dni_costh",                 "Field collector DNI-cosine product",                                               "W/m2",         "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "EqOpteff",                  "Field optical efficiency before defocus",                                          "",             "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "SCAs_def",                  "Field fraction of focused SCAs",                                                   "",             "",               "solar_field",    "sim_type=1",                       "",                      "" },
                                                                                                                                                                                                                                                               
    { SSC_OUTPUT,       SSC_ARRAY,       "q_inc_sf_tot",              "Field thermal power incident",                                                     "MWt",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "qinc_costh",                "Field thermal power incident after cosine",                                        "MWt",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_rec_inc",             "Receiver thermal power incident",                                                  "MWt",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_rec_thermal_loss",    "Receiver thermal losses",                                                          "MWt",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_rec_abs",             "Receiver thermal power absorbed",                                                  "MWt",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_piping_loss",         "Field piping thermal losses",                                                      "MWt",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "e_dot_field_int_energy",    "Field change in material/htf internal energy",                                     "MWt",          "",               "solar_field",    "sim_type=1",                       "",                      "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_htf_sf_out",          "Field thermal power leaving in HTF",                                               "MWt",          "",               "solar_field",    "sim_type=1",                       "",                      "" },  
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_freeze_prot",         "Field freeze protection required",                                                 "MWt",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
                                                                                                                                                                                                                                                               
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_loop",                "Receiver mass flow rate",                                                          "kg/s",         "",               "solar_field",    "sim_type=1",                       "",                      "" },  
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_field_recirc",        "Field total mass flow recirculated",                                               "kg/s",         "",               "solar_field",    "sim_type=1",                       "",                      "" },  
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_field_delivered",     "Field total mass flow delivered",                                                  "kg/s",         "",               "solar_field",    "sim_type=1",                       "",                      "" },  
    { SSC_OUTPUT,       SSC_ARRAY,       "T_field_cold_in",           "Field timestep-averaged inlet temperature",                                        "C",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_rec_cold_in",             "Loop timestep-averaged inlet temperature",                                         "C",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_rec_hot_out",             "Loop timestep-averaged outlet temperature",                                        "C",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_field_hot_out",           "Field timestep-averaged outlet temperature",                                       "C",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "deltaP_field",              "Field pressure drop",                                                              "bar",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
                                                                                                                                                                                                                                                               
    { SSC_OUTPUT,       SSC_ARRAY,       "W_dot_sca_track",           "Field collector tracking power",                                                   "MWe",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "W_dot_field_pump",          "Field htf pumping power",                                                          "MWe",          "",               "solar_field",    "sim_type=1",                       "",                      "" },

    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_diams",         "Field piping header diameters",                                                    "m",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_wallthk",       "Field piping header wall thicknesses",                                             "m",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_lengths",       "Field piping header lengths",                                                      "m",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_expansions",    "Number of field piping header expansions",                                         "-",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_mdot_dsn",      "Field piping header mass flow at design",                                          "kg/s",         "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_vel_dsn",       "Field piping header velocity at design",                                           "m/s",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_T_dsn",         "Field piping header temperature at design",                                        "C",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_P_dsn",         "Field piping header pressure at design",                                           "bar",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_diams",         "Field piping runner diameters",                                                    "m",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_wallthk",       "Field piping runner wall thicknesses",                                             "m",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_lengths",       "Field piping runner lengths",                                                      "m",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_expansions",    "Number of field piping runner expansions",                                         "-",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_mdot_dsn",      "Field piping runner mass flow at design",                                          "kg/s",         "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_vel_dsn",       "Field piping runner velocity at design",                                           "m/s",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_T_dsn",         "Field piping runner temperature at design",                                        "C",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_P_dsn",         "Field piping runner pressure at design",                                           "bar",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_loop_T_dsn",           "Field piping loop temperature at design",                                          "C",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_loop_P_dsn",           "Field piping loop pressure at design",                                             "bar",          "",               "solar_field",    "sim_type=1",                       "",                      "" },
    
    //// Power Block
    //{ SSC_OUTPUT,       SSC_ARRAY,       "eta",                       "PC efficiency: gross",                                                             "",             "",               "powerblock",     "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "q_pb",                      "PC input energy",                                                                  "MWt",          "",               "powerblock",     "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_pc",                  "PC HTF mass flow rate",                                                            "kg/s",         "",               "powerblock",     "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_startup",          "PC startup thermal power",                                                         "MWt",          "",               "powerblock",     "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "P_cycle",                   "PC electrical power output: gross",                                                "MWe",          "",               "powerblock",     "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "T_pc_in",                   "PC HTF inlet temperature",                                                         "C",            "",               "powerblock",     "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "T_pc_out",                  "PC HTF outlet temperature",                                                        "C",            "",               "powerblock",     "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_water_pc",            "PC water consumption: makeup + cooling",                                           "kg/s",         "",               "powerblock",     "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "q_pc_startup",              "PC startup thermal energy",                                                        "MWht",         "",               "powerblock",     "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "cycle_htf_pump_power",      "PC HTF pump power",                                                                "MWe",          "",               "powerblock",     "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "P_cooling_tower_tot",       "Parasitic power condenser operation",                                              "MWe",          "",               "powerblock",     "sim_type=1",                       "",                      "" },

    // Heat Sink
    { SSC_OUTPUT,       SSC_ARRAY,      "q_dot_to_heat_sink",               "Heat sink thermal power",                                              "MWt",          "",         "Heat_Sink",      "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "W_dot_pc_pump",                    "Heat sink pumping power",                                              "MWe",          "",         "Heat_Sink",      "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "m_dot_htf_heat_sink",              "Heat sink HTF mass flow",                                              "kg/s",         "",         "Heat_Sink",      "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "T_heat_sink_in",                   "Heat sink HTF inlet temp",                                             "C",            "",         "Heat_Sink",      "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "T_heat_sink_out",                  "Heat sink HTF outlet temp",                                            "C",            "",         "Heat_Sink",      "sim_type=1",                       "",                      "" },


    // TES
    { SSC_OUTPUT,       SSC_ARRAY,       "tank_losses",               "TES thermal losses",                                                               "MWt",          "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_tes_heater",              "TES freeze protection power",                                                      "MWe",          "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_tes_hot",                 "TES hot temperature",                                                              "C",            "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_tes_cold",                "TES cold temperature",                                                             "C",            "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "mass_tes_cold",             "TES cold tank mass (end)",                                                         "kg",           "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "mass_tes_hot",              "TES hot tank mass (end)",                                                          "kg",           "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dc_tes",                  "TES discharge thermal power",                                                      "MWt",          "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_ch_tes",                  "TES charge thermal power",                                                         "MWt",          "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "e_ch_tes",                  "TES charge state",                                                                 "MWht",         "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_cr_to_tes_hot",       "Mass flow: field to hot TES",                                                      "kg/s",         "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_tes_hot_out",         "Mass flow: TES hot out",                                                           "kg/s",         "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_pc_to_tes_cold",      "Mass flow: cycle to cold TES",                                                     "kg/s",         "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_tes_cold_out",        "Mass flow: TES cold out",                                                          "kg/s",         "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_field_to_cycle",      "Mass flow: field to cycle",                                                        "kg/s",         "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_cycle_to_field",      "Mass flow: cycle to field",                                                        "kg/s",         "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_cold_tank_to_hot_tank", "Mass flow: cold tank to hot tank",                                               "kg/s",         "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tes_htf_pump_power",         "TES HTF pump power",                                                              "MWe",          "",               "TES",            "sim_type=1",                       "",                      "" },


    //{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_tes_dc",              "TES discharge mass flow rate",                                                     "kg/s",         "",               "TES",            "*",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_tes_ch",              "TES charge mass flow rate",                                                        "kg/s",         "",               "TES",            "*",                       "",                      "" },
    
    // SYSTEM
    { SSC_OUTPUT,       SSC_ARRAY,       "W_dot_parasitic_tot",       "System total electrical parasitic",                                                "MWe",          "",               "system",         "sim_type=1",                       "",                      "" },
                                                                                                                                                                                                            
    // Controller                                                                                                                                                                                           
    { SSC_OUTPUT,       SSC_ARRAY,       "op_mode_1",                 "1st operating mode",                                                               "",             "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "op_mode_2",                 "2nd op. mode, if applicable",                                                      "",             "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "op_mode_3",                 "3rd op. mode, if applicable",                                                      "",             "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_balance",             "Relative mass flow balance error",                                                 "",             "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_balance",                 "Relative energy balance error",                                                    "",             "",               "solver",         "sim_type=1",                       "",                      "" },

    // Monthly Outputs
    { SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",            "Monthly Energy Gross",                                                                   "kWh",          "",               "Post-process",   "sim_type=1",              "LENGTH=12",                      "" },

    // Annual Outputs
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",                   "Annual Net Electrical Energy Production w/ avail derate",                    "kWe-hr",       "",               "Post-process",   "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_NUMBER,      "annual_gross_energy",             "Annual Gross Electrical Energy Production w/ avail derate",                  "kWe-hr",       "",               "Post-process",   "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_thermal_consumption",      "Annual thermal freeze protection required",                                  "kWt-hr",       "",               "Post-process",   "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_electricity_consumption",  "Annual electricity consumption w/ avail derate",                             "kWe-hr",       "",               "Post-process",   "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_total_water_use",          "Total Annual Water Usage",                                                   "m^3",          "",               "Post-process",   "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_field_freeze_protection",  "Annual thermal power for field freeze protection",                           "kWt-hr",       "",               "Post-process",   "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_tes_freeze_protection",    "Annual thermal power for TES freeze protection",                             "kWt-hr",       "",               "Post-process",   "sim_type=1",                       "",                      "" },

    // Newly added
    { SSC_OUTPUT,       SSC_ARRAY,       "n_op_modes",                "Operating modes in reporting timestep",                                            "",             "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tou_value",                 "CSP operating Time-of-use value",                                                  "",             "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pricing_mult",              "PPA price multiplier",                                                             "",             "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_sb",               "Thermal power for PC standby",                                                     "MWt",          "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_min",              "Thermal power for PC min operation",                                               "MWt",          "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_target",           "Target thermal power to PC",                                                       "MWt",          "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_max",              "Max thermal power to PC",                                                          "MWt",          "",               "solver",         "sim_type=1",                       "",                      "" },
                                                                                                                                                                                                                                                                  
    { SSC_OUTPUT,       SSC_ARRAY,       "is_rec_su_allowed",         "is receiver startup allowed",                                                      "",             "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "is_pc_su_allowed",          "is power cycle startup allowed",                                                   "",             "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "is_pc_sb_allowed",          "is power cycle standby allowed",                                                   "",             "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_est_cr_su",           "Estimate rec. startup thermal power",                                              "MWt",           "",              "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_est_cr_on",           "Estimate rec. thermal power TO HTF",                                               "MWt",          "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_est_tes_dc",          "Estimate max TES discharge thermal power",                                         "MWt",          "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_est_tes_ch",          "Estimate max TES charge thermal power",                                            "MWt",          "",               "solver",         "sim_type=1",                       "",                      "" },
                                                                                                                                                                                                                                                                  
    { SSC_OUTPUT,       SSC_ARRAY,       "operating_modes_a",         "First 3 operating modes tried",                                                    "",             "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "operating_modes_b",         "Next 3 operating modes tried",                                                     "",             "",               "solver",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "operating_modes_c",         "Final 3 operating modes tried",                                                    "",             "",               "solver",         "sim_type=1",                       "",                      "" },
                                                                                                                                                                                                                                                                  
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_rel_mip_gap",          "Dispatch relative MIP gap",                                                        "",             "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_solve_state",          "Dispatch solver state",                                                            "",             "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_subopt_flag",          "Dispatch suboptimal solution flag",                                                "",             "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_solve_iter",           "Dispatch iterations count",                                                        "",             "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_objective",            "Dispatch objective function value",                                                "",             "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_obj_relax",            "Dispatch objective function - relaxed max",                                        "",             "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_qsf_expected",         "Dispatch expected solar field available energy",                                   "MWt",          "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_qsfprod_expected",     "Dispatch expected solar field generation",                                         "MWt",          "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_qsfsu_expected",       "Dispatch expected solar field startup enegy",                                      "MWt",          "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_tes_expected",         "Dispatch expected TES charge level",                                               "MWht",         "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_pceff_expected",       "Dispatch expected power cycle efficiency adj.",                                    "",             "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_thermeff_expected",    "Dispatch expected SF thermal efficiency adj.",                                     "",             "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_qpbsu_expected",       "Dispatch expected power cycle startup energy",                                     "MWht",         "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_wpb_expected",         "Dispatch expected power generation",                                               "MWe",          "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_rev_expected",         "Dispatch expected revenue factor",                                                 "",             "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_presolve_nconstr",     "Dispatch number of constraints in problem",                                        "",             "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_presolve_nvar",        "Dispatch number of variables in problem",                                          "",             "",               "tou",            "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "disp_solve_time",           "Dispatch solver time",                                                             "sec",          "",               "tou",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "avg_suboptimal_rel_mip_gap","Average suboptimal relative MIP gap",                                              "%",            "",               "tou",            "sim_type=1",                       "",                      "" },

    { SSC_OUTPUT,       SSC_ARRAY,       "P_fixed",                   "Parasitic power fixed load",                                                       "MWe",          "",               "system",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "P_plant_balance_tot",       "Parasitic power generation-dependent load",                                        "MWe",          "",               "system",         "sim_type=1",                       "",                      "" },
                                                                                                                                                                                                                                                                  
    { SSC_OUTPUT,       SSC_ARRAY,       "gen",                       "Total thermal power to grid w/ avail. derate",                                    "kWe",          "",               "system",         "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_NUMBER,      "conversion_factor",         "Gross to Net Conversion Factor",                                                   "%",            "",               "system",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "capacity_factor",           "Capacity factor",                                                                  "%",            "",               "system",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "kwh_per_kw",                "First year kWh/kW",                                                                "kWh/kW",       "",               "system",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "sim_duration",              "Computational time of timeseries simulation",                                      "s",            "",               "system",         "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "W_dot_par_tot_haf",         "Adjusted parasitic power",                                                         "kWe",          "",               "system",         "sim_type=1",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_NUMBER,      "q_dot_defocus_est",         "Thermal energy intentionally lost by defocusing",                                  "MWt",          "",               "system",         "*",                       "",                      "" },

    { SSC_OUTPUT,       SSC_ARRAY,       "recirculating",             "Field recirculating (bypass valve open)",                                          "-",            "",               "solar_field",    "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_tes_diams",            "Pipe diameters in TES",                                                            "m",            "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_tes_wallthk",          "Pipe wall thickness in TES",                                                       "m",            "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_tes_lengths",          "Pipe lengths in TES",                                                              "m",            "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_tes_mdot_dsn",         "Mass flow TES pipes at design conditions",                                         "kg/s",         "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_tes_vel_dsn",          "Velocity in TES pipes at design conditions",                                       "m/s",          "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_tes_T_dsn",            "Temperature in TES pipes at design conditions",                                    "C",            "",               "TES",            "sim_type=1",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_tes_P_dsn",            "Pressure in TES pipes at design conditions",                                       "bar",          "",               "TES",            "sim_type=1",                       "",                      "" },

    //{ SSC_OUTPUT,       SSC_ARRAY,       "defocus",                   "Field optical focus fraction",                                                     "",             "",               "solver",         "*",                       "",                      "" },

    var_info_invalid };
    
    
    
class cm_heat_sink_iph : public compute_module
{
public:

    cm_heat_sink_iph()
    {
        add_var_info( _cm_vtab_heat_sink_iph);
        add_var_info( vtab_adjustment_factors );
        add_var_info(vtab_technology_outputs);
    }

    void exec( )
    {
        // Uncomment following 2 lines to write cmod inputs to LK script
        //FILE* fp = fopen("trough_iph_cmod_to_lk.lk", "w");
        //write_cmod_to_lk_script(fp, m_vartab);

        // Common Parameters
        int sim_type = as_number("sim_type");
        int csp_financial_model = as_integer("csp_financial_model");
        double T_htf_cold_des = as_double("T_loop_in_des");    //[C]
        double T_htf_hot_des = as_double("T_loop_out");      //[C]
        double tshours = as_double("tshours");                  //[-]
        double q_dot_pc_des = as_double("q_pb_design");         //[MWt] HEAT SINK design thermal power
        double Q_tes = q_dot_pc_des * tshours;                  //[MWt-hr]
        int is_dispatch = 0;

        // Heat Sink
        int n_steps_fixed = 1;
        C_pc_heat_sink c_heat_sink;
        {
            size_t n_f_turbine1 = 0;
            ssc_number_t* p_f_turbine1 = as_array("f_turb_tou_periods", &n_f_turbine1);   // heat sink, not turbine
            double f_turbine_max1 = 1.0;
            for (size_t i = 0; i < n_f_turbine1; i++) {
                f_turbine_max1 = max(f_turbine_max1, p_f_turbine1[i]);
            }

            c_heat_sink.ms_params.m_T_htf_hot_des = T_htf_hot_des;		//[C] FIELD design outlet temperature
            c_heat_sink.ms_params.m_T_htf_cold_des = T_htf_cold_des;	//[C] FIELD design inlet temperature
            c_heat_sink.ms_params.m_q_dot_des = q_dot_pc_des;			//[MWt] HEAT SINK design thermal power (could have field solar multiple...)
            // 9.18.2016 twn: assume for now there's no pressure drop though heat sink
            c_heat_sink.ms_params.m_htf_pump_coef = as_double("pb_pump_coef");		//[kWe/kg/s]
            c_heat_sink.ms_params.m_max_frac = f_turbine_max1;

            c_heat_sink.ms_params.m_pc_fl = as_integer("Fluid");
            c_heat_sink.ms_params.m_pc_fl_props = as_matrix("field_fl_props");


            // Allocate heat sink outputs
            c_heat_sink.mc_reported_outputs.assign(C_pc_heat_sink::E_Q_DOT_HEAT_SINK, allocate("q_dot_to_heat_sink", n_steps_fixed), n_steps_fixed);
            c_heat_sink.mc_reported_outputs.assign(C_pc_heat_sink::E_W_DOT_PUMPING, allocate("W_dot_pc_pump", n_steps_fixed), n_steps_fixed);
            c_heat_sink.mc_reported_outputs.assign(C_pc_heat_sink::E_M_DOT_HTF, allocate("m_dot_htf_heat_sink", n_steps_fixed), n_steps_fixed);
            c_heat_sink.mc_reported_outputs.assign(C_pc_heat_sink::E_T_HTF_IN, allocate("T_heat_sink_in", n_steps_fixed), n_steps_fixed);
            c_heat_sink.mc_reported_outputs.assign(C_pc_heat_sink::E_T_HTF_OUT, allocate("T_heat_sink_out", n_steps_fixed), n_steps_fixed);
        }

        // Initialize heat sink model


        // Simulate heat sink model


        // Outputs from heat sink model
        


    }

    template <typename T>
    void set_vector(const std::string& name, const vector<T> vec)
    {
        int size = vec.size();
        ssc_number_t* alloc_vals = allocate(name, size);
        for (int i = 0; i < size; i++)
            alloc_vals[i] = vec[i];    // []
    }

};

DEFINE_MODULE_ENTRY(heat_sink_iph, "Physical trough iph applications", 1)
