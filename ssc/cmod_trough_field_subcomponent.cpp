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

#include "csp_solver_core.h"
#include "csp_solver_trough_collector_receiver.h"
#include "csp_solver_pc_heat_sink.h"
#include "csp_solver_two_tank_tes.h"
#include "csp_solver_tou_block_schedules.h"
#include "csp_dispatch.h"
#include "csp_system_costs.h"
#include <ctime>
#include <algorithm>
#include <iterator>

// signed/unsigned mismatch
#pragma warning (disable : 4388)

static var_info _cm_vtab_trough_field_subcomponent[] = {




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
    { SSC_INPUT,        SSC_NUMBER,      "custom_sf_pipe_sizes",      "Use custom solar field pipe diams, wallthks, and lengths",                         "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_rnr_diams",              "Custom runner diameters",                                                          "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_rnr_wallthicks",         "Custom runner wall thicknesses",                                                   "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_rnr_lengths",            "Custom runner lengths",                                                            "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_hdr_diams",              "Custom header diameters",                                                          "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_hdr_wallthicks",         "Custom header wall thicknesses",                                                   "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_hdr_lengths",            "Custom header lengths",                                                            "m",            "",               "solar_field",    "*",                       "",                      "" },
    
    // Needed for auto-updating dependent inputs
    { SSC_INPUT,        SSC_NUMBER,      "use_solar_mult_or_aperture_area",     "Use solar multiple or total field aperture area",                        "-",            "",               "controller",     "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "specified_solar_multiple",            "specified_solar_multiple",                                               "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "specified_total_aperture",            "specified_total_aperture",                                               "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "non_solar_field_land_area_multiplier", "non_solar_field_land_area_multiplier",                                  "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "trough_loop_control",                 "trough_loop_control",                                                    "-",            "",               "controller",     "*",                       "",                      "" },

    // Optional Component Initialization (state at start of first timestep)
    // Trough field
    { SSC_INPUT,        SSC_NUMBER,      "rec_op_mode_initial",       "Initial receiver operating mode 0: off, 1: startup, 2: on",                        "-",            "",               "System Control", "",                        "",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,      "defocus_initial",           "Initial receiver defocus",                                                         "-",            "",               "System Control", "",                        "",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,      "T_in_loop_initial",         "Initial loop inlet, cold header and cold runner fluid temperature",                "-",            "",               "System Control", "",                        "",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,      "T_out_loop_initial",        "Initial loop outlet, hot header and hot runner fluid temperature",                 "-",            "",               "System Control", "",                        "",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_ARRAY,       "T_out_scas_initial",        "Initial SCA outlet temperatures",                                                  "-",            "",               "System Control", "",                        "",                      "SIMULATION_PARAMETER" },



    
    // *************************************************************************************************
    //    OUTPUTS
    // *************************************************************************************************

    // Design Point Outputs
    { SSC_OUTPUT,       SSC_NUMBER,      "solar_mult",                       "Actual solar multiple",                                                    "",              "",               "System Design Calc","*",                             "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "nameplate",                        "Nameplate capacity",                                                       "MWe",           "",               "System Design Calc","*",                             "",                      "" },

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

var_info_invalid };



class cm_trough_field_subcomponent : public compute_module
{
public:

    cm_trough_field_subcomponent()
    {
        add_var_info(_cm_vtab_trough_field_subcomponent);
    }

    void exec()
    {
        // Uncomment following 2 lines to write cmod inputs to LK script
        //FILE* fp = fopen("trough_iph_cmod_to_lk.lk", "w");
        //write_cmod_to_lk_script(fp, m_vartab);

        // Common Parameters
        int sim_type = as_number("sim_type");
        double q_dot_pc_des = as_double("q_pb_design");         //[MWt] HEAT SINK design thermal power

        // *****************************************************
        // System Design Parameters


        // ********************************
        // ********************************
        // Weather reader
        // ********************************
        // ********************************
        C_csp_weatherreader weather_reader;
        C_csp_solver::S_sim_setup sim_setup;
        int n_steps_fixed;
        int steps_per_hour;
        {
            if (is_assigned("file_name")) {
                weather_reader.m_weather_data_provider = make_shared<weatherfile>(as_string("file_name"));
                if (weather_reader.m_weather_data_provider->has_message()) log(weather_reader.m_weather_data_provider->message(), SSC_WARNING);
            }
            if (is_assigned("solar_resource_data")) {
                weather_reader.m_weather_data_provider = make_shared<weatherdata>(lookup("solar_resource_data"));
                if (weather_reader.m_weather_data_provider->has_message()) log(weather_reader.m_weather_data_provider->message(), SSC_WARNING);
            }

            weather_reader.m_filename = as_string("file_name");
            weather_reader.m_trackmode = 0;
            weather_reader.m_tilt = 0.0;
            weather_reader.m_azimuth = 0.0;
            // Initialize to get weather file info
            weather_reader.init();
            if (weather_reader.has_error()) throw exec_error("trough_physical", weather_reader.get_error());

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


        // ********************************
        // ********************************
        // Solar field, trough
        // ********************************
        // ********************************
        C_csp_trough_collector_receiver c_trough;
        {
            // Collect Inputs
            {
                // ADDED Trough Inputs (TMB 10/06/2023) for design point calculations
                std::vector<double> trough_loop_vec = as_vector_double("trough_loop_control");
                c_trough.m_trough_loop_control.assign(&trough_loop_vec[0], trough_loop_vec.size());

                int actual_nSCA = trough_loop_vec[0];

                c_trough.m_use_solar_mult_or_aperture_area = as_number("use_solar_mult_or_aperture_area"); // Use specified solar mult (0) or total aperture (1)
                c_trough.m_specified_solar_mult = as_number("specified_solar_multiple");            // User specified solar mult
                c_trough.m_specified_total_aperture = as_number("specified_total_aperture");    //[m2] User specified total aperture
                c_trough.m_nSCA = actual_nSCA;                              //[-] Number of SCA's in a loop
                c_trough.m_nHCEt = as_integer("nHCEt");                     //[-] Number of HCE types
                c_trough.m_nColt = as_integer("nColt");                     //[-] Number of collector types
                c_trough.m_nHCEVar = as_integer("nHCEVar");                 //[-] Number of HCE variants per t
                //c_trough.m_nLoops = as_integer("nLoops");                   //[-] Number of loops in the field
                c_trough.m_FieldConfig = as_integer("FieldConfig");         //[-] Number of subfield headers
                //c_trough.m_L_power_block_piping = as_double("L_power_block_piping");                           //[m] Length of piping (full mass flow) through power block (if applicable)
                //c_trough.m_include_fixed_power_block_runner = as_boolean("include_fixed_power_block_runner");  //[-] Should model consider piping through power block?
                c_trough.m_eta_pump = as_double("eta_pump");                //[-] HTF pump efficiency
                c_trough.m_Fluid = as_integer("Fluid");                     //[-] Field HTF fluid number
                //c_trough.m_fthrok = as_integer("fthrok");                 //[-] Flag to allow partial defocusing of the collectors
                c_trough.m_fthrctrl = 2;                                    //[-] Defocusing strategy; hardcode = 2 for now
                c_trough.m_accept_loc = as_integer("accept_loc");           //[-] In acceptance testing mode - temperature sensor location (1=hx,2=loop)
                c_trough.m_HDR_rough = as_double("HDR_rough");              //[-] Header pipe relative roughness
                c_trough.m_theta_stow = as_double("theta_stow");            //[deg] stow angle
                c_trough.m_theta_dep = as_double("theta_dep");              //[deg] deploy angle
                c_trough.m_Row_Distance = as_double("Row_Distance");        //[m] Spacing between rows (centerline to centerline)

                double T_loop_in_des = as_double("T_loop_in_des");          //[C] Design loop inlet temperature, converted to K in init
                c_trough.m_T_loop_in_des = T_loop_in_des;                   //[C] Design loop inlet temperature, converted to K in init
                double T_loop_out_des = as_double("T_loop_out");            //[C] Target loop outlet temperature, converted to K in init
                c_trough.m_T_loop_out_des = T_loop_out_des;                 //[C] Target loop outlet temperature, converted to K in init
                double T_startup_min = T_loop_in_des;
                if (T_loop_out_des > 600.0)
                {
                    T_startup_min = T_loop_out_des - 70.0;
                }
                double T_startup = max(T_startup_min, 0.67 * T_loop_in_des + 0.33 * T_loop_out_des); //[C]
                c_trough.m_T_startup = T_startup;                           //[C] The required temperature (converted to K in init) of the system before the power block can be switched on

                // Check initialization variables
                if (is_assigned("rec_op_mode_initial")) {
                    c_trough.m_operating_mode_initial = (C_csp_collector_receiver::E_csp_cr_modes)as_integer("rec_op_mode_initial");
                }
                if (is_assigned("defocus_initial")) {
                    c_trough.m_defocus_initial = as_integer("defocus_initial");
                }
                if (is_assigned("T_in_loop_initial")) {
                    c_trough.m_T_in_loop_initial = as_double("T_in_loop_initial");
                }
                if (is_assigned("T_out_loop_initial")) {
                    c_trough.m_T_out_loop_initial = as_double("T_out_loop_initial");
                }
                if (is_assigned("T_out_scas_initial")) {
                    size_t n_T_out_scas_last_initial = -1;
                    ssc_number_t* T_out_scas_last_initial = as_array("T_out_scas_initial", &n_T_out_scas_last_initial);
                    std::copy(T_out_scas_last_initial, T_out_scas_last_initial + n_T_out_scas_last_initial, back_inserter(c_trough.m_T_out_scas_last_initial));
                }


                c_trough.m_m_dot_htfmin = as_double("m_dot_htfmin");        //[kg/s] Minimum loop HTF flow rate
                c_trough.m_m_dot_htfmax = as_double("m_dot_htfmax");        //[kg/s] Maximum loop HTF flow rate
                c_trough.m_field_fl_props = as_matrix("field_fl_props");    //[-] User-defined field HTF properties
                c_trough.m_T_fp = as_double("T_fp");                        //[C] Freeze protection temperature (heat trace activation temperature), convert to K in init
                c_trough.m_I_bn_des = as_double("I_bn_des");                //[W/m^2] Solar irradiation at design
                c_trough.m_V_hdr_cold_max = as_double("V_hdr_cold_max");    //[m/s] Maximum HTF velocity in the cold header at design
                c_trough.m_V_hdr_cold_min = as_double("V_hdr_cold_min");    //[m/s] Minimum HTF velocity in the cold header at design
                c_trough.m_V_hdr_hot_max = as_double("V_hdr_hot_max");      //[m/s] Maximum HTF velocity in the hot header at design
                c_trough.m_V_hdr_hot_min = as_double("V_hdr_hot_min");      //[m/s] Minimum HTF velocity in the hot header at design
                c_trough.m_Pipe_hl_coef = as_double("Pipe_hl_coef");        //[W/m2-K] Loss coefficient from the header, runner pipe, and non-HCE piping
                c_trough.m_SCA_drives_elec = as_double("SCA_drives_elec");  //[W/SCA] Tracking power, in Watts per SCA drive
                c_trough.m_ColTilt = as_double("tilt");                     //[deg] Collector tilt angle (0 is horizontal, 90deg is vertical)
                c_trough.m_ColAz = as_double("azimuth");                    //[deg] Collector azimuth angle
                c_trough.m_wind_stow_speed = as_double("wind_stow_speed");  //[m/s] Wind speed at and above which the collectors will be stowed
                c_trough.m_accept_mode = as_integer("accept_mode");         //[-] Acceptance testing mode? (1=yes, 0=no)
                c_trough.m_accept_init = as_boolean("accept_init");         //[-] In acceptance testing mode - require steady-state startup
                c_trough.m_mc_bal_hot_per_MW = as_double("mc_bal_hot");     //[kWht/K-MWt] The heat capacity of the balance of plant on the hot side
                c_trough.m_mc_bal_cold_per_MW = as_double("mc_bal_cold");   //[kWht/K-MWt] The heat capacity of the balance of plant on the cold side
                c_trough.m_mc_bal_sca = as_double("mc_bal_sca");            //[Wht/K-m] Non-HTF heat capacity associated with each SCA - per meter basis

                c_trough.m_P_ref = q_dot_pc_des * 1e6;                      //[W] Design Turbine Net Output
                c_trough.m_eta_ref = 1;                                     //[] Design cycle thermal efficiency
                c_trough.m_non_solar_field_land_area_multiplier = as_double("non_solar_field_land_area_multiplier");    //[]

                //[m] The collector aperture width (Total structural area.. used for shadowing)
                size_t nval_W_aperture = 0;
                ssc_number_t* W_aperture = as_array("W_aperture", &nval_W_aperture);
                c_trough.m_W_aperture.resize(nval_W_aperture);
                for (size_t i = 0; i < nval_W_aperture; i++)
                    c_trough.m_W_aperture[i] = (double)W_aperture[i];

                //[m^2] Reflective aperture area of the collector
                size_t nval_A_aperture = 0;
                ssc_number_t* A_aperture = as_array("A_aperture", &nval_A_aperture);
                c_trough.m_A_aperture.resize(nval_A_aperture);
                for (size_t i = 0; i < nval_A_aperture; i++)
                    c_trough.m_A_aperture[i] = (double)A_aperture[i];

                //[-] Tracking error derate
                size_t nval_TrackingError = 0;
                ssc_number_t* TrackingError = as_array("TrackingError", &nval_TrackingError);
                c_trough.m_TrackingError.resize(nval_TrackingError);
                for (size_t i = 0; i < nval_TrackingError; i++)
                    c_trough.m_TrackingError[i] = (double)TrackingError[i];

                //[-] Geometry effects derate
                size_t nval_GeomEffects = 0;
                ssc_number_t* GeomEffects = as_array("GeomEffects", &nval_GeomEffects);
                c_trough.m_GeomEffects.resize(nval_GeomEffects);
                for (size_t i = 0; i < nval_GeomEffects; i++)
                    c_trough.m_GeomEffects[i] = (double)GeomEffects[i];

                //[-] Clean mirror reflectivity
                size_t nval_Rho_mirror_clean = 0;
                ssc_number_t* Rho_mirror_clean = as_array("Rho_mirror_clean", &nval_Rho_mirror_clean);
                c_trough.m_Rho_mirror_clean.resize(nval_Rho_mirror_clean);
                for (size_t i = 0; i < nval_Rho_mirror_clean; i++)
                    c_trough.m_Rho_mirror_clean[i] = (double)Rho_mirror_clean[i];

                //[-] Dirt on mirror derate
                size_t nval_Dirt_mirror = 0;
                ssc_number_t* Dirt_mirror = as_array("Dirt_mirror", &nval_Dirt_mirror);
                c_trough.m_Dirt_mirror.resize(nval_Dirt_mirror);
                for (size_t i = 0; i < nval_Dirt_mirror; i++)
                    c_trough.m_Dirt_mirror[i] = (double)Dirt_mirror[i];

                //[-] General optical error derate
                size_t nval_Error = 0;
                ssc_number_t* Error = as_array("Error", &nval_Error);
                c_trough.m_Error.resize(nval_Error);
                for (size_t i = 0; i < nval_Error; i++)
                    c_trough.m_Error[i] = (double)Error[i];

                //[m] The average focal length of the collector 
                size_t nval_Ave_Focal_Length = 0;
                ssc_number_t* Ave_Focal_Length = as_array("Ave_Focal_Length", &nval_Ave_Focal_Length);
                c_trough.m_Ave_Focal_Length.resize(nval_Ave_Focal_Length);
                for (size_t i = 0; i < nval_Ave_Focal_Length; i++)
                    c_trough.m_Ave_Focal_Length[i] = (double)Ave_Focal_Length[i];

                //[m] The length of the SCA 
                size_t nval_L_SCA = 0;
                ssc_number_t* L_SCA = as_array("L_SCA", &nval_L_SCA);
                c_trough.m_L_SCA.resize(nval_L_SCA);
                for (size_t i = 0; i < nval_L_SCA; i++)
                    c_trough.m_L_SCA[i] = (double)L_SCA[i];

                //[m] The length of a single mirror/HCE unit
                size_t nval_L_aperture = 0;
                ssc_number_t* L_aperture = as_array("L_aperture", &nval_L_aperture);
                c_trough.m_L_aperture.resize(nval_L_aperture);
                for (size_t i = 0; i < nval_L_aperture; i++)
                    c_trough.m_L_aperture[i] = (double)L_aperture[i];

                //[-] The number of individual collector sections in an SCA
                size_t nval_ColperSCA = 0;
                ssc_number_t* ColperSCA = as_array("ColperSCA", &nval_ColperSCA);
                c_trough.m_ColperSCA.resize(nval_ColperSCA);
                for (size_t i = 0; i < nval_ColperSCA; i++)
                    c_trough.m_ColperSCA[i] = (double)ColperSCA[i];

                //[m] Piping distance between SCA's in the field
                size_t nval_Distance_SCA = 0;
                ssc_number_t* Distance_SCA = as_array("Distance_SCA", &nval_Distance_SCA);
                c_trough.m_Distance_SCA.resize(nval_Distance_SCA);
                for (size_t i = 0; i < nval_Distance_SCA; i++)
                    c_trough.m_Distance_SCA[i] = (double)Distance_SCA[i];

                c_trough.m_IAM_matrix = as_matrix("IAM_matrix");                //[-] IAM coefficients, matrix for 4 collectors

                // Why are these matrices - can't they be arrays?               
                c_trough.m_HCE_FieldFrac = as_matrix("HCE_FieldFrac");          //[-] Fraction of the field occupied by this HCE type
                c_trough.m_D_2 = as_matrix("D_2");                              //[m] Inner absorber tube diameter
                c_trough.m_D_3 = as_matrix("D_3");                              //[m] Outer absorber tube diameter
                c_trough.m_D_4 = as_matrix("D_4");                              //[m] Inner glass envelope diameter
                c_trough.m_D_5 = as_matrix("D_5");                              //[m] Outer glass envelope diameter
                c_trough.m_D_p = as_matrix("D_p");                              //[m] Diameter of the absorber flow plug (optional)
                c_trough.m_Flow_type = as_matrix("Flow_type");                  //[-] Flow type through the absorber
                c_trough.m_Rough = as_matrix("Rough");                          //[-] Relative roughness of the internal HCE surface
                c_trough.m_alpha_env = as_matrix("alpha_env");                  //[-] Envelope absorptance
                // **********************************************************

                // Emittance vs. temperature profile for each receiver type and variation
                c_trough.m_epsilon_3_11 = as_matrix_transpose("epsilon_3_11");  //[-] Absorber emittance for receiver type 1 variation 1
                c_trough.m_epsilon_3_12 = as_matrix_transpose("epsilon_3_12");  //[-] Absorber emittance for receiver type 1 variation 2
                c_trough.m_epsilon_3_13 = as_matrix_transpose("epsilon_3_13");  //[-] Absorber emittance for receiver type 1 variation 3
                c_trough.m_epsilon_3_14 = as_matrix_transpose("epsilon_3_14");  //[-] Absorber emittance for receiver type 1 variation 4
                c_trough.m_epsilon_3_21 = as_matrix_transpose("epsilon_3_21");  //[-] Absorber emittance for receiver type 2 variation 1
                c_trough.m_epsilon_3_22 = as_matrix_transpose("epsilon_3_22");  //[-] Absorber emittance for receiver type 2 variation 2
                c_trough.m_epsilon_3_23 = as_matrix_transpose("epsilon_3_23");  //[-] Absorber emittance for receiver type 2 variation 3
                c_trough.m_epsilon_3_24 = as_matrix_transpose("epsilon_3_24");  //[-] Absorber emittance for receiver type 2 variation 4
                c_trough.m_epsilon_3_31 = as_matrix_transpose("epsilon_3_31");  //[-] Absorber emittance for receiver type 3 variation 1
                c_trough.m_epsilon_3_32 = as_matrix_transpose("epsilon_3_32");  //[-] Absorber emittance for receiver type 3 variation 2
                c_trough.m_epsilon_3_33 = as_matrix_transpose("epsilon_3_33");  //[-] Absorber emittance for receiver type 3 variation 3
                c_trough.m_epsilon_3_34 = as_matrix_transpose("epsilon_3_34");  //[-] Absorber emittance for receiver type 3 variation 4
                c_trough.m_epsilon_3_41 = as_matrix_transpose("epsilon_3_41");  //[-] Absorber emittance for receiver type 4 variation 1
                c_trough.m_epsilon_3_42 = as_matrix_transpose("epsilon_3_42");  //[-] Absorber emittance for receiver type 4 variation 2
                c_trough.m_epsilon_3_43 = as_matrix_transpose("epsilon_3_43");  //[-] Absorber emittance for receiver type 4 variation 3
                c_trough.m_epsilon_3_44 = as_matrix_transpose("epsilon_3_44");  //[-] Absorber emittance for receiver type 4 variation 4

                c_trough.m_alpha_abs = as_matrix("alpha_abs");                  //[-] Absorber absorptance
                c_trough.m_Tau_envelope = as_matrix("Tau_envelope");            //[-] Envelope transmittance
                c_trough.m_EPSILON_4 = as_matrix("EPSILON_4");                  //[-] Inner glass envelope emissivities
                c_trough.m_EPSILON_5 = as_matrix("EPSILON_5");                  //[-] Outer glass envelope emissivities

                //c_trough.m_GlazingIntact = (as_matrix("GlazingIntactIn") > 0);  //[-] Glazing intact (broken glass) flag {1=true, else=false}
                util::matrix_t<double> glazing_intact_double = as_matrix("GlazingIntactIn"); //[-] Is the glazing intact?
                int n_gl_row = (int)glazing_intact_double.nrows();
                int n_gl_col = (int)glazing_intact_double.ncols();
                c_trough.m_GlazingIntact.resize(n_gl_row, n_gl_col);
                for (int i = 0; i < n_gl_row; i++)
                {
                    for (int j = 0; j < n_gl_col; j++)
                    {
                        c_trough.m_GlazingIntact(i, j) = (glazing_intact_double(i, j) > 0);
                    }
                }

                c_trough.m_P_a = as_matrix("P_a");                              //[torr] Annulus gas pressure
                c_trough.m_AnnulusGas = as_matrix("AnnulusGas");                //[-] Annulus gas type (1=air, 26=Ar, 27=H2)
                c_trough.m_AbsorberMaterial = as_matrix("AbsorberMaterial");    //[-] Absorber material type
                c_trough.m_Shadowing = as_matrix("Shadowing");                  //[-] Receiver bellows shadowing loss factor
                c_trough.m_Dirt_HCE = as_matrix("Dirt_HCE");                    //[-] Loss due to dirt on the receiver envelope
                c_trough.m_Design_loss = as_matrix("Design_loss");              //[-] Receiver heat loss at design
                c_trough.m_rec_su_delay = as_double("rec_su_delay");            //[hr] Fixed startup delay time for the receiver
                c_trough.m_rec_qf_delay = as_double("rec_qf_delay");            //[-] Energy-based receiver startup delay (fraction of rated thermal power)
                c_trough.m_p_start = as_double("p_start");                      //[kWe-hr] Collector startup energy, per SCA
                c_trough.m_calc_design_pipe_vals = as_boolean("calc_design_pipe_vals"); //[-] Should the HTF state be calculated at design conditions
                c_trough.m_L_rnr_pb = as_double("L_rnr_pb");                      //[m] Length of hot or cold runner pipe around the power block
                c_trough.m_N_max_hdr_diams = as_double("N_max_hdr_diams");        //[-] Maximum number of allowed diameters in each of the hot and cold headers
                c_trough.m_L_rnr_per_xpan = as_double("L_rnr_per_xpan");          //[m] Threshold length of straight runner pipe without an expansion loop
                c_trough.m_L_xpan_hdr = as_double("L_xpan_hdr");                  //[m] Combined length in meters of the two perpendicular segments of a header expansion loop
                c_trough.m_L_xpan_rnr = as_double("L_xpan_rnr");                  //[m] Combined length in meters of the two perpendicular segments of a runner expansion loop
                c_trough.m_Min_rnr_xpans = as_double("Min_rnr_xpans");            //[-] Minimum number of expansion loops per single-diameter runner section
                c_trough.m_northsouth_field_sep = as_double("northsouth_field_sep"); //[m] Shortest north/south distance between SCAs in different subfields
                c_trough.m_N_hdr_per_xpan = as_double("N_hdr_per_xpan");          //[-] Number of collector loops per header expansion loops. 1 = expansion loop between every collector loop
                c_trough.m_offset_xpan_hdr = as_double("offset_xpan_hdr");        //[-] Location of first header expansion loop. 1 = after first collector loop
                //c_trough.m_K_cpnt = as_matrix("K_cpnt");                          //[-] Minor loss coefficients of the components in each loop interconnect
                //c_trough.m_D_cpnt = as_matrix("D_cpnt");                          //[m] Inner diameters of the components in each loop interconnect
                //c_trough.m_L_cpnt = as_matrix("L_cpnt");                          //[m] Lengths of the components in each loop interconnect
                //c_trough.m_Type_cpnt = as_matrix("Type_cpnt");                    //[-] Type of component in each loop interconnect [0=fitting | 1=pipe | 2=flex_hose]
                c_trough.m_custom_sf_pipe_sizes = as_boolean("custom_sf_pipe_sizes"); //[-] Should the field pipe diameters, wall thickness and lengths be imported instead of calculated
                c_trough.m_sf_rnr_diams = as_matrix("sf_rnr_diams");              //[m] Imported runner diameters, used if custom_sf_pipe_sizes is true
                c_trough.m_sf_rnr_wallthicks = as_matrix("sf_rnr_wallthicks");    //[m] Imported runner wall thicknesses, used if custom_sf_pipe_sizes is true
                c_trough.m_sf_rnr_lengths = as_matrix("sf_rnr_lengths");          //[m] Imported runner lengths, used if custom_sf_pipe_sizes is true
                c_trough.m_sf_hdr_diams = as_matrix("sf_hdr_diams");              //[m] Imported header diameters, used if custom_sf_pipe_sizes is true
                c_trough.m_sf_hdr_wallthicks = as_matrix("sf_hdr_wallthicks");    //[m] Imported header wall thicknesses, used if custom_sf_pipe_sizes is true
                c_trough.m_sf_hdr_lengths = as_matrix("sf_hdr_lengths");          //[m] Imported header lengths, used if custom_sf_pipe_sizes is true

            }

            // Calculate solar multiple (needed for other component constructors)
            bool success = c_trough.design_solar_mult();
            if (success == false)
                throw exec_error("trough_physical_iph", "Negative solar mult or total aperture.");



            // Allocate trough outputs
            {
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_THETA_AVE, allocate("Theta_ave", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_COSTH_AVE, allocate("CosTh_ave", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_IAM_AVE, allocate("IAM_ave", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_ROWSHADOW_AVE, allocate("RowShadow_ave", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_ENDLOSS_AVE, allocate("EndLoss_ave", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_DNI_COSTH, allocate("dni_costh", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_EQUIV_OPT_ETA_TOT, allocate("EqOpteff", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_DEFOCUS, allocate("SCAs_def", n_steps_fixed), n_steps_fixed);

                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_INC_SF_TOT, allocate("q_inc_sf_tot", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_INC_SF_COSTH, allocate("qinc_costh", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_REC_INC, allocate("q_dot_rec_inc", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_REC_THERMAL_LOSS, allocate("q_dot_rec_thermal_loss", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_REC_ABS, allocate("q_dot_rec_abs", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_PIPING_LOSS, allocate("q_dot_piping_loss", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_E_DOT_INTERNAL_ENERGY, allocate("e_dot_field_int_energy", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_HTF_OUT, allocate("q_dot_htf_sf_out", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_FREEZE_PROT, allocate("q_dot_freeze_prot", n_steps_fixed), n_steps_fixed);

                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_M_DOT_LOOP, allocate("m_dot_loop", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_IS_RECIRCULATING, allocate("recirculating", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_M_DOT_FIELD_RECIRC, allocate("m_dot_field_recirc", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_M_DOT_FIELD_DELIVERED, allocate("m_dot_field_delivered", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_T_FIELD_COLD_IN, allocate("T_field_cold_in", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_T_REC_COLD_IN, allocate("T_rec_cold_in", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_T_REC_HOT_OUT, allocate("T_rec_hot_out", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_T_FIELD_HOT_OUT, allocate("T_field_hot_out", n_steps_fixed), n_steps_fixed);
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_PRESSURE_DROP, allocate("deltaP_field", n_steps_fixed), n_steps_fixed);          //[bar]

                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_W_DOT_SCA_TRACK, allocate("W_dot_sca_track", n_steps_fixed), n_steps_fixed);     //[MWe]
                c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_W_DOT_PUMP, allocate("W_dot_field_pump", n_steps_fixed), n_steps_fixed);         //[MWe]
            }

        }

        // Initialize Trough Field
        C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs;
        init_inputs.m_latitude = weather_reader.ms_solved_params.m_lat;		//[deg]
        init_inputs.m_longitude = weather_reader.ms_solved_params.m_lon;	//[deg]
        init_inputs.m_tz = weather_reader.ms_solved_params.m_tz;	    	//[hr]
        init_inputs.m_shift = weather_reader.ms_solved_params.m_shift;		//[deg]
        init_inputs.m_elev = weather_reader.ms_solved_params.m_elev;		//[m]
        C_csp_collector_receiver::S_csp_cr_solved_params cr_solved_params;

        c_trough.init(init_inputs, cr_solved_params);

        // Output Design Point Calculaions
        {
            assign("nameplate", q_dot_pc_des);  // [MWt]

            assign("nSCA", c_trough.m_nSCA);
            assign("field_htf_min_temp", c_trough.m_htfProps.min_temp() - 273.15);    // [C]
            assign("field_htf_max_temp", c_trough.m_htfProps.max_temp() - 273.15);    // [C]
            assign("field_htf_cp_avg_des", c_trough.m_field_htf_cp_avg_des);          // [kJ/kg-K]
            assign("single_loop_aperture", c_trough.m_single_loop_aperture);  // [m2]
            assign("min_inner_diameter", c_trough.m_min_inner_diameter);      // [m]
            ssc_number_t* hce_design = allocate("csp_dtr_hce_design_heat_losses", c_trough.m_HCE_heat_loss_des.size());
            for (int i = 0; i < c_trough.m_HCE_heat_loss_des.size(); i++)
                hce_design[i] = c_trough.m_HCE_heat_loss_des[i];    // [W/m]
            assign("csp_dtr_loop_hce_heat_loss", c_trough.m_HCE_heat_loss_loop_des); //[W/m]
            ssc_number_t* sca_effs = allocate("csp_dtr_sca_calc_sca_effs", c_trough.m_csp_dtr_sca_calc_sca_effs.size());
            for (int i = 0; i < c_trough.m_csp_dtr_sca_calc_sca_effs.size(); i++)
                sca_effs[i] = c_trough.m_csp_dtr_sca_calc_sca_effs[i];    // []
            assign("loop_optical_efficiency", c_trough.m_opteff_des);  //[]
            ssc_number_t* hce_effs = allocate("csp_dtr_hce_optical_effs", c_trough.m_csp_dtr_hce_optical_effs.size());
            for (int i = 0; i < c_trough.m_csp_dtr_hce_optical_effs.size(); i++)
                hce_effs[i] = c_trough.m_csp_dtr_hce_optical_effs[i];    // []
            assign("SCAInfoArray", c_trough.m_SCAInfoArray);    // []
            set_vector("SCADefocusArray", c_trough.m_SCADefocusArray);
            assign("max_field_flow_velocity", c_trough.m_max_field_flow_velocity);  //[m/s]
            assign("min_field_flow_velocity", c_trough.m_min_field_flow_velocity);  //[m/s]
            assign("total_loop_conversion_efficiency", c_trough.m_total_loop_conversion_efficiency_des);
            assign("total_required_aperture_for_SM1", c_trough.m_total_required_aperture_for_SM1);
            assign("required_number_of_loops_for_SM1", c_trough.m_required_number_of_loops_for_SM1);
            assign("nLoops", c_trough.m_nLoops);
            assign("total_aperture", c_trough.m_Ap_tot);    //[m2]
            assign("field_thermal_output_actual", c_trough.m_q_design_actual / 1e6); // [MWt]
            assign("field_thermal_output_ideal", c_trough.m_q_design_ideal / 1e6); // [MWt]
            assign("solar_mult", c_trough.m_solar_mult);
            assign("fixed_land_area", c_trough.m_fixed_land_area);  //[acre]
            assign("total_land_area", c_trough.m_total_land_area);  //[acre]
            double total_tracking_power = c_trough.get_tracking_power();
            assign("total_tracking_power", total_tracking_power);  //[MWe]
            assign("K_cpnt", c_trough.m_K_cpnt);
            assign("D_cpnt", c_trough.m_D_cpnt);    //[m]
            assign("L_cpnt", c_trough.m_L_cpnt);    //[m]
            assign("Type_cpnt", c_trough.m_Type_cpnt);   //[]
        }

        // Return if only called for design point
        if (sim_type != 1)
            return;

        // Simulate
        int mode = 1;

        double T_htf_inlet = 90;    // [C]
        double mdot_htf_inlet = 1;  // [kg/s]

        // ON
        if (mode == 1)
        {
            // Starting Time (starts at 0 = Jan 1, midnight)
            double day = 4;     // [day] (starts at 0!!!!!!)
            double hr = 12;     // [hr]

            // Time step (simulation duration)
            double step = 3600; // [s]

            C_csp_solver_sim_info sim_info;
            sim_info.ms_ts.m_step = step;       // [s] Size of timestep

            double num_steps = (24 * day) + hr;              // Number of time steps (i.e. if step == 1 hr, then this would be hrs)

            weather_reader.read_time_step(num_steps, sim_info);
            weather_reader.ms_outputs;

            // HTF State
            C_csp_solver_htf_1state htf_state;
            htf_state.m_m_dot = mdot_htf_inlet;         // [kg/s]
            htf_state.m_pres = 17 / 1.e-5;              // [Pa]
            htf_state.m_qual = 1;          // [-]
            htf_state.m_temp = T_htf_inlet;             // [C] Inlet Temp

            double q_dot_elec_to_CR_heat = 0;   // [MWt]
            double field_control = 1;           // [-] Defocus control (1 is no defocus)

            C_csp_collector_receiver::S_csp_cr_out_solver cr_out_solver; // Output class

            c_trough.on(weather_reader.ms_outputs, htf_state, q_dot_elec_to_CR_heat, field_control,
                cr_out_solver, sim_info);
            c_trough.converged();
        }

        // Output
        
        // Non-timeseries array outputs
        ssc_number_t* p_pipe_runner_diams = allocate("pipe_runner_diams", c_trough.m_D_runner.size());
        std::copy(c_trough.m_D_runner.begin(), c_trough.m_D_runner.end(), p_pipe_runner_diams);
        ssc_number_t* p_pipe_runner_wallthk = allocate("pipe_runner_wallthk", c_trough.m_WallThk_runner.size());
        std::copy(c_trough.m_WallThk_runner.begin(), c_trough.m_WallThk_runner.end(), p_pipe_runner_wallthk);
        ssc_number_t* p_pipe_runner_lengths = allocate("pipe_runner_lengths", c_trough.m_L_runner.size());
        std::copy(c_trough.m_L_runner.begin(), c_trough.m_L_runner.end(), p_pipe_runner_lengths);
        ssc_number_t* p_pipe_runner_expansions = allocate("pipe_runner_expansions", c_trough.m_N_rnr_xpans.size());
        std::copy(c_trough.m_N_rnr_xpans.begin(), c_trough.m_N_rnr_xpans.end(), p_pipe_runner_expansions);
        ssc_number_t* p_pipe_runner_mdot_dsn = allocate("pipe_runner_mdot_dsn", c_trough.m_m_dot_rnr_dsn.size());
        std::copy(c_trough.m_m_dot_rnr_dsn.begin(), c_trough.m_m_dot_rnr_dsn.end(), p_pipe_runner_mdot_dsn);
        ssc_number_t* p_pipe_runner_vel_dsn = allocate("pipe_runner_vel_dsn", c_trough.m_V_rnr_dsn.size());
        std::copy(c_trough.m_V_rnr_dsn.begin(), c_trough.m_V_rnr_dsn.end(), p_pipe_runner_vel_dsn);
        ssc_number_t* p_pipe_runner_T_dsn = allocate("pipe_runner_T_dsn", c_trough.m_T_rnr_dsn.size());
        std::copy(c_trough.m_T_rnr_dsn.begin(), c_trough.m_T_rnr_dsn.end(), p_pipe_runner_T_dsn);
        ssc_number_t* p_pipe_runner_P_dsn = allocate("pipe_runner_P_dsn", c_trough.m_P_rnr_dsn.size());
        std::copy(c_trough.m_P_rnr_dsn.begin(), c_trough.m_P_rnr_dsn.end(), p_pipe_runner_P_dsn);

        ssc_number_t* p_pipe_header_diams = allocate("pipe_header_diams", c_trough.m_D_hdr.size());
        std::copy(c_trough.m_D_hdr.begin(), c_trough.m_D_hdr.end(), p_pipe_header_diams);
        ssc_number_t* p_pipe_header_wallthk = allocate("pipe_header_wallthk", c_trough.m_WallThk_hdr.size());
        std::copy(c_trough.m_WallThk_hdr.begin(), c_trough.m_WallThk_hdr.end(), p_pipe_header_wallthk);
        ssc_number_t* p_pipe_header_lengths = allocate("pipe_header_lengths", c_trough.m_L_hdr.size());
        std::copy(c_trough.m_L_hdr.begin(), c_trough.m_L_hdr.end(), p_pipe_header_lengths);
        ssc_number_t* p_pipe_header_expansions = allocate("pipe_header_expansions", c_trough.m_N_hdr_xpans.size());
        std::copy(c_trough.m_N_hdr_xpans.begin(), c_trough.m_N_hdr_xpans.end(), p_pipe_header_expansions);
        ssc_number_t* p_pipe_header_mdot_dsn = allocate("pipe_header_mdot_dsn", c_trough.m_m_dot_hdr_dsn.size());
        std::copy(c_trough.m_m_dot_hdr_dsn.begin(), c_trough.m_m_dot_hdr_dsn.end(), p_pipe_header_mdot_dsn);
        ssc_number_t* p_pipe_header_vel_dsn = allocate("pipe_header_vel_dsn", c_trough.m_V_hdr_dsn.size());
        std::copy(c_trough.m_V_hdr_dsn.begin(), c_trough.m_V_hdr_dsn.end(), p_pipe_header_vel_dsn);
        ssc_number_t* p_pipe_header_T_dsn = allocate("pipe_header_T_dsn", c_trough.m_T_hdr_dsn.size());
        std::copy(c_trough.m_T_hdr_dsn.begin(), c_trough.m_T_hdr_dsn.end(), p_pipe_header_T_dsn);
        ssc_number_t* p_pipe_header_P_dsn = allocate("pipe_header_P_dsn", c_trough.m_P_hdr_dsn.size());
        std::copy(c_trough.m_P_hdr_dsn.begin(), c_trough.m_P_hdr_dsn.end(), p_pipe_header_P_dsn);

        ssc_number_t* p_pipe_loop_T_dsn = allocate("pipe_loop_T_dsn", c_trough.m_T_loop_dsn.size());
        std::copy(c_trough.m_T_loop_dsn.begin(), c_trough.m_T_loop_dsn.end(), p_pipe_loop_T_dsn);
        ssc_number_t* p_pipe_loop_P_dsn = allocate("pipe_loop_P_dsn", c_trough.m_P_loop_dsn.size());
        std::copy(c_trough.m_P_loop_dsn.begin(), c_trough.m_P_loop_dsn.end(), p_pipe_loop_P_dsn);

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

DEFINE_MODULE_ENTRY(trough_field_subcomponent, "Physical trough field subcomponent", 1)
