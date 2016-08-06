// Trough CSP - physical model
#include "core.h"
#include "tckernel.h"
// for adjustment factors
#include "common.h"

#include "lib_weatherfile.h"

#include "csp_solver_trough_collector_receiver.h"
#include "csp_solver_pc_heat_sink.h"
#include "csp_solver_two_tank_tes.h"
#include "csp_solver_tou_block_schedules.h"
#include "csp_solver_core.h"

static bool ssc_trough_physical_process_heat_sim_progress(void *data, double percent, C_csp_messages *csp_msg, float time_sec);

static var_info _cm_vtab_trough_physical_process_heat[] = {
//   weather reader inputs
//   VARTYPE            DATATYPE          NAME                        LABEL                                                                               UNITS           META            GROUP             REQUIRED_IF                CONSTRAINTS              UI_HINTS

    { SSC_INPUT,        SSC_STRING,      "file_name",                 "Local weather file with path",                                                     "none",         "",             "Weather",        "*",                       "LOCAL_FILE",            "" },
    { SSC_INPUT,        SSC_NUMBER,      "track_mode",                "Tracking mode",                                                                    "none",         "",             "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tilt",                      "Tilt angle of surface/axis",                                                       "none",         "",             "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "azimuth",                   "Azimuth angle of surface/axis",                                                    "none",         "",             "Weather",        "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",           "Nameplate capacity",                                                               "kW",           "",             "trough",         "*",                       "",                      "" },

//   solar field (type 250) inputs	
//   VARTYPE            DATATYPE          NAME                        LABEL                                                                               UNITS           META              GROUP             REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_NUMBER,      "nSCA",                      "Number of SCAs in a loop",                                                         "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nHCEt",                     "Number of HCE types",                                                              "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nColt",                     "Number of collector types",                                                        "none",         "constant=4",              "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nHCEVar",                   "Number of HCE variants per type",                                                  "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nLoops",                    "Number of loops in the field",                                                     "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_pump",                  "HTF pump efficiency",                                                              "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "HDR_rough",                 "Header pipe roughness",                                                            "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "theta_stow",                "Stow angle",                                                                       "deg",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "theta_dep",                 "Deploy angle",                                                                     "deg",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Row_Distance",              "Spacing between rows (centerline to centerline)",                                  "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "FieldConfig",               "Number of subfield headers",                                                       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_startup",                 "Required temperature of the system before the power block can be switched on",     "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_ref",                     "Rated plant capacity",                                                             "MWe",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htfmin",              "Minimum loop HTF flow rate",                                                       "kg/s",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htfmax",              "Maximum loop HTF flow rate",                                                       "kg/s",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_loop_in_des",             "Design loop inlet temperature",                                                    "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_loop_out",                "Target loop outlet temperature",                                                   "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Fluid",                     "Field HTF fluid ID number",                                                        "none",         "",               "solar_field",    "*",                       "",                      "" },
    
	{ SSC_INPUT,        SSC_NUMBER,      "T_fp",                      "Freeze protection temperature (heat trace activation temperature)",                "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_bn_des",                  "Solar irradiation at design",                                                      "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_max",                 "Maximum HTF velocity in the header at design",                                     "W/m2",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_min",                 "Minimum HTF velocity in the header at design",                                     "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Pipe_hl_coef",              "Loss coefficient from the header, runner pipe, and non-HCE piping",                "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "SCA_drives_elec",           "Tracking power, in Watts per SCA drive",                                           "W/m2-K",       "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fthrok",                    "Flag to allow partial defocusing of the collectors",                               "W/SCA",        "",               "solar_field",    "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "fthrctrl",                  "Defocusing strategy",                                                              "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "water_usage_per_wash",      "Water usage per wash",                                                             "L/m2_aper",    "",               "solar_field",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "washing_frequency",         "Mirror washing frequency",                                                         "none",         "",               "solar_field",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "accept_mode",               "Acceptance testing mode?",                                                         "0/1",          "no/yes",         "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "accept_init",               "In acceptance testing mode - require steady-state startup",                        "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "accept_loc",                "In acceptance testing mode - temperature sensor location",                         "1/2",          "hx/loop",        "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "solar_mult",                "Solar multiple",                                                                   "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_hot",                "Heat capacity of the balance of plant on the hot side",                            "kWht/K-MWt",   "none",           "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_cold",               "Heat capacity of the balance of plant on the cold side",                           "kWht/K-MWt",   "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_sca",                "Non-HTF heat capacity associated with each SCA - per meter basis",                 "Wht/K-m",      "",               "solar_field",    "*",                       "",                      "" },
                                                                                                                                                             
    { SSC_INPUT,        SSC_ARRAY,       "W_aperture",                "The collector aperture width (Total structural area used for shadowing)",          "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "A_aperture",                "Reflective aperture area of the collector",                                        "m2",           "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "TrackingError",             "User-defined tracking error derate",                                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "GeomEffects",               "User-defined geometry effects derate",                                             "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Rho_mirror_clean",          "User-defined clean mirror reflectivity",                                           "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Dirt_mirror",               "User-defined dirt on mirror derate",                                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Error",                     "User-defined general optical error derate ",                                       "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Ave_Focal_Length",          "Average focal length of the collector ",                                           "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "L_SCA",                     "Length of the SCA ",                                                               "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "L_aperture",                "Length of a single mirror/HCE unit",                                               "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "ColperSCA",                 "Number of individual collector sections in an SCA ",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Distance_SCA",              "Piping distance between SCA's in the field",                                       "m",            "",             "solar_field",    "*",                       "",                      "" },

	{ SSC_INPUT,        SSC_MATRIX,      "IAM_matrix",                "IAM coefficients, matrix for 4 collectors",                                        "none",         "",             "solar_field",    "*",                       "",                      "" },

	{ SSC_INPUT,        SSC_MATRIX,      "HCE_FieldFrac",             "Fraction of the field occupied by this HCE type ",                                 "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_2",                       "Inner absorber tube diameter",                                                     "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_3",                       "Outer absorber tube diameter",                                                     "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_4",                       "Inner glass envelope diameter ",                                                   "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_5",                       "Outer glass envelope diameter ",                                                   "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_p",                       "Diameter of the absorber flow plug (optional) ",                                   "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Flow_type",                 "Flow type through the absorber",                                                   "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Rough",                     "Roughness of the internal surface ",                                               "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "alpha_env",                 "Envelope absorptance ",                                                            "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_11",              "Absorber emittance for receiver type 1 variation 1",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_12",              "Absorber emittance for receiver type 1 variation 2",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_13",              "Absorber emittance for receiver type 1 variation 3",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_14",              "Absorber emittance for receiver type 1 variation 4",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_21",              "Absorber emittance for receiver type 2 variation 1",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_22",              "Absorber emittance for receiver type 2 variation 2",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_23",              "Absorber emittance for receiver type 2 variation 3",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_24",              "Absorber emittance for receiver type 2 variation 4",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_31",              "Absorber emittance for receiver type 3 variation 1",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_32",              "Absorber emittance for receiver type 3 variation 2",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_33",              "Absorber emittance for receiver type 3 variation 3",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_34",              "Absorber emittance for receiver type 3 variation 4",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_41",              "Absorber emittance for receiver type 4 variation 1",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_42",              "Absorber emittance for receiver type 4 variation 2",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_43",              "Absorber emittance for receiver type 4 variation 3",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_44",              "Absorber emittance for receiver type 4 variation 4",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "alpha_abs",                 "Absorber absorptance ",                                                            "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Tau_envelope",              "Envelope transmittance",                                                           "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "EPSILON_4",                 "Inner glass envelope emissivities (Pyrex) ",                                       "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "EPSILON_5",                 "Outer glass envelope emissivities (Pyrex) ",                                       "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "GlazingIntactIn",           "Glazing intact (broken glass) flag {1=true, else=false}",                          "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "P_a",                       "Annulus gas pressure",                                                             "torr",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "AnnulusGas",                "Annulus gas type (1=air, 26=Ar, 27=H2)",                                           "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "AbsorberMaterial",          "Absorber material type",                                                           "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Shadowing",                 "Receiver bellows shadowing loss factor",                                           "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Dirt_HCE",                  "Loss due to dirt on the receiver envelope",                                        "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Design_loss",               "Receiver heat loss at design",                                                     "W/m",          "",             "solar_field",    "*",                       "",                      "" },

    { SSC_INPUT,        SSC_MATRIX,      "SCAInfoArray",              "Receiver (,1) and collector (,2) type for each assembly in loop",                 "none",          "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "SCADefocusArray",           "Collector defocus order",                                                         "none",          "",             "solar_field",    "*",                       "",                      "" },      
															          
//   controller (type 251) inputs
//   VARTYPE            DATATYPE          NAME                        LABEL                                                             UNITS           META            GROUP             REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_MATRIX,      "field_fl_props",            "User defined field fluid property data",                         "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "store_fl_props",            "User defined storage fluid property data",                       "-",            "",             "controller",     "*",                       "",                      "" },    
	{ SSC_INPUT,        SSC_NUMBER,      "store_fluid",               "Material number for storage fluid",                              "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tshours",                   "Equivalent full-load thermal storage hours",                     "hr",           "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_hx",                     "Heat exchanger (HX) exists (1=yes, 0=no)" ,                       "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dt_hot",                    "Hot side HX approach temp",                                      "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dt_cold",                   "Cold side HX approach temp",                                     "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hx_config",                 "HX configuration",                                               "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_max_aux",                 "Max heat rate of auxiliary heater",                              "MWt",          "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_set_aux",                 "Aux heater outlet temp set point",                               "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_tank_hot_ini",            "Initial hot tank fluid volume",                                  "m3",           "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_cold_ini",           "Initial cold tank fluid tmeperature",                            "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "vol_tank",                  "Total tank volume, including unusable HTF at bottom",            "m3",           "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank",                    "Total height of tank (height of HTF when tank is full",          "m",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank_min",                "Minimum allowable HTF height in storage tank",                   "m",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "u_tank",                    "Loss coefficient from the tank",                                 "W/m2-K",       "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_pairs",                "Number of equivalent tank pairs",                                "-",            "",             "controller",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_Thtr",            "Minimum allowable cold tank HTF temp",                           "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_Thtr",             "Minimum allowable hot tank HTF temp",                            "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_max_heat",             "Rated heater capacity for tank heating",                         "MW",           "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_pb_design",               "Design heat input to power block",                               "MWt",          "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "W_pb_design",               "Rated plant capacity",                                           "MWe",          "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_max_frac",            "Maximum turbine over design operation fraction",                 "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_cutoff_frac",         "Minimum turbine operation fraction before shutdown",             "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_pump_coef",              "Pumping power to move 1kg of HTF through PB loop",               "kW/kg",        "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_pump_coef",             "Pumping power to move 1kg of HTF through tes loop",              "kW/kg",        "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_fixed_par",              "Fraction of rated gross power constantly consumed",              "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "bop_array",                 "Coefficients for balance of plant parasitics calcs",             "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "aux_array",                 "Coefficients for auxiliary heater parasitics calcs",             "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fossil_mode",               "Fossil backup mode 1=Normal 2=Topping",                          "-",            "",             "controller",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",                "Fraction of thermal power required for standby",                 "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_standby_reset",           "Maximum allowable time for PB standby operation",                "hr",           "",             "controller",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sf_type",                   "Solar field type, 1 = trough, 2 = tower",                        "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_type",                  "1=2-tank, 2=thermocline",                                        "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_a",                 "Dispatch logic without solar",                                   "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_b",                 "Dispatch logic with solar",                                      "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_c",                 "Dispatch logic for turbine load fraction",                       "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "ffrac",                     "Fossil dispatch logic",                                          "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tc_fill",                   "Thermocline fill material",                                      "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tc_void",                   "Thermocline void fraction",                                      "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_dis_out_min",             "Min allowable hot side outlet temp during discharge",            "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_ch_out_max",              "Max allowable cold side outlet temp during charge",              "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nodes",                     "Nodes modeled in the flow path",                                 "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_tc_cold",                 "0=entire tank is hot, 1=entire tank is cold",                    "-",            "",             "controller",     "*",                       "",                      "" },

    // Time of use schedules for thermal storage
    { SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",          "Dispatch 12mx24h schedule for week days",                         "",             "",             "tou_translator", "*",                       "",                      "" }, 
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",          "Dispatch 12mx24h schedule for weekends",                          "",             "",             "tou_translator", "*",                       "",                      "" }, 
															          																	                  
						          
//   VARTYPE            DATATYPE          NAME                LABEL                                                                        UNITS           META                            GROUP             REQUIRED_IF                CONSTRAINTS              UI_HINTS
	// Power Cycle Inputs
	{ SSC_INPUT,        SSC_NUMBER,      "pc_config",         "0: Steam Rankine (224), 1: user defined",                                   "-",            "",                             "powerblock",     "?=0",                     "INTEGER",               "" },        
	{ SSC_INPUT,        SSC_NUMBER,      "eta_ref",           "Reference conversion efficiency at design condition",                       "none",         "",                             "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "startup_time",      "Time needed for power block startup",                                       "hr",           "",                             "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",      "Fraction of design thermal power needed for startup",                       "none",         "",                             "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",        "Fraction of thermal power required for standby mode",                       "none",         "",                             "powerblock",     "*",                       "",                      "" },
    

	// Steam Rankine cycle
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",         "Reference condenser cooling water inlet/outlet T diff",                     "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",         "Reference ambient temperature at design point",                             "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil",            "Boiler operating pressure",                                                 "bar",          "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                "Flag for using dry cooling or wet cooling system",                          "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",        "Cooling tower approach temperature",                                        "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",         "ITD at design for dry system",                                              "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",      "Condenser pressure ratio",                                                  "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",        "Power block blowdown steam fraction ",                                      "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",        "Minimum condenser pressure",                                                "inHg",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",          "Number of part-load increments for the heat rejection system",              "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",              "Fraction indicating wet cooling use for hybrid system",                     "none",         "constant=[0,0,0,0,0,0,0,0,0]", "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tech_type",         "Turbine inlet pressure control flag (sliding=user, fixed=trough)",          "1/2/3",         "tower/trough/user",           "powerblock",     "pc_config=0",             "",                      "" },
	
		// User Defined cycle
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_amb_des",         "Ambient temperature at user-defined power cycle design point",                   "C",	    "",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_f_W_dot_cool_des",  "Percent of user-defined power cycle design gross output consumed by cooling",    "%",	    "",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_m_dot_water_cool_des", "Mass flow rate of water required at user-defined power cycle design point",   "kg/s",  "",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_htf_low",         "Low level HTF inlet temperature for T_amb parametric",                           "C",     "",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_htf_high",        "High level HTF inlet temperature for T_amb parametric",                          "C",		"",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_amb_low",         "Low level ambient temperature for HTF mass flow rate parametric",                "C",		"",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_amb_high",        "High level ambient temperature for HTF mass flow rate parametric",               "C",		"",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_m_dot_htf_low",     "Low level normalized HTF mass flow rate for T_HTF parametric",                   "-",	    "",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_m_dot_htf_high",    "High level normalized HTF mass flow rate for T_HTF parametric",                  "-",	    "",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_MATRIX,      "ud_T_htf_ind_od",      "Off design table of user-defined power cycle performance formed from parametric on T_htf_hot [C]", "", "",               "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_MATRIX,      "ud_T_amb_ind_od",      "Off design table of user-defined power cycle performance formed from parametric on T_amb [C]",	 "", "",               "user_defined_PC", "pc_config=1",            "",                      "" }, 
	{ SSC_INPUT,        SSC_MATRIX,      "ud_m_dot_htf_ind_od",  "Off design table of user-defined power cycle performance formed from parametric on m_dot_htf [ND]","", "",               "user_defined_PC", "pc_config=1",            "",                      "" }, 
		
																																												  
 //  enet calculator																																							  
    { SSC_INPUT,        SSC_NUMBER,      "eta_lhv",           "Fossil fuel lower heating value - Thermal power generated per unit fuel",   "MW/MMBTU",     "",                             "enet",           "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_tes_htr",       "Thermal storage tank heater efficiency (fp_mode=1 only)",                   "none",         "",                             "enet",           "*",                       "",                      "" },



// OUTPUTS
// The names of the output variables should match the parameter names for the TCS units in order to signal the TCS kernel to store the values by timestep
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_thermal",            "Thermal power to HTF",                                                   "MWt",          "",            "CR",             "",                       "",           "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "gen",                  "Total electric power to grid w/ avail. derate",                          "kWe",          "",            "System",         "",                       "",           "" },


	var_info_invalid };
	
	
	
class cm_trough_physical_process_heat : public compute_module
{
public:

	cm_trough_physical_process_heat()
	{
		add_var_info( _cm_vtab_trough_physical_process_heat );
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);
	}

	void exec( ) throw( general_error )
	{	
		int tes_type = as_integer("tes_type");
		if( tes_type != 1 )
		{
			throw exec_error("Physical Trough CSP Solver", "The tes_type input must be = 1. Additional TES options may be added in future versions.\n");
		}

		//***************************************************************************
		//***************************************************************************
			// Weather reader
		C_csp_weatherreader weather_reader;
		weather_reader.m_filename = as_string("file_name");
		weather_reader.m_trackmode = 0;
		weather_reader.m_tilt = 0.0;
		weather_reader.m_azimuth = 0.0;
		weather_reader.init();

		// Set up ssc output arrays
		// Set steps per hour
		double nhourssim = 8760.0;				//[hr] Number of hours to simulate
		C_csp_solver::S_sim_setup sim_setup;
		sim_setup.m_sim_time_start = 0.0;				//[s] starting first hour of year
		sim_setup.m_sim_time_end = nhourssim*3600.0;	//[s] full year simulation

		int steps_per_hour = 1;			//[-]

		int n_wf_records = weather_reader.get_n_records();
		steps_per_hour = n_wf_records / 8760;	//[-]

		int n_steps_fixed = steps_per_hour*8760.0;	//[-]
		sim_setup.m_report_step = 3600.0 / (double)steps_per_hour;	//[s]
		//***************************************************************************
		//***************************************************************************

		C_csp_trough_collector_receiver c_trough;

		c_trough.m_nSCA = as_integer("nSCA");						//[-] Number of SCA's in a loop
		c_trough.m_nHCEt = as_integer("nHCEt");						//[-] Number of HCE types
		c_trough.m_nColt = as_integer("nColt");						//[-] Number of collector types
		c_trough.m_nHCEVar = as_integer("nHCEVar");					//[-] Number of HCE variants per t
		c_trough.m_nLoops = as_integer("nLoops");					//[-] Number of loops in the field
		c_trough.m_FieldConfig = as_integer("FieldConfig");			//[-] Number of subfield headers
		c_trough.m_Fluid = as_integer("Fluid");						//[-] Field HTF fluid number
		c_trough.m_fthrok = as_integer("fthrok");					//[-] Flag to allow partial defocusing of the collectors
		c_trough.m_fthrctrl = as_integer("fthrctrl");				//[-] Defocusing strategy
		c_trough.m_accept_loc = as_integer("accept_loc");			//[-] In acceptance testing mode - temperature sensor location (1=hx,2=loop)
		c_trough.m_HDR_rough = as_double("HDR_rough");				//[m] Header pipe roughness
		c_trough.m_theta_stow = as_double("theta_stow");			//[deg] stow angle
		c_trough.m_theta_dep = as_double("theta_dep");				//[deg] deploy angle
		c_trough.m_Row_Distance = as_double("Row_Distance");		//[m] Spacing between rows (centerline to centerline)
		c_trough.m_T_startup = as_double("T_startup");				//[C] The required temperature (converted to K in init) of the system before the power block can be switched on
		c_trough.m_m_dot_htfmin = as_double("m_dot_htfmin");		//[kg/s] Minimum loop HTF flow rate
		c_trough.m_m_dot_htfmax = as_double("m_dot_htfmax");		//[kg/s] Maximum loop HTF flow rate
		c_trough.m_T_loop_in_des = as_double("T_loop_in_des");		//[C] Design loop inlet temperature, converted to K in init
		c_trough.m_T_loop_out_des = as_double("T_loop_out");		//[C] Target loop outlet temperature, converted to K in init
		c_trough.m_field_fl_props = as_matrix("field_fl_props");	//[-] User-defined field HTF properties
		c_trough.m_T_fp = as_double("T_fp");						//[C] Freeze protection temperature (heat trace activation temperature), convert to K in init
		c_trough.m_I_bn_des = as_double("I_bn_des");				//[W/m^2] Solar irradiation at design
		c_trough.m_V_hdr_max = as_double("V_hdr_max");				//[m/s] Maximum HTF velocity in the header at design
		c_trough.m_V_hdr_min = as_double("V_hdr_min"); 				//[m/s] Minimum HTF velocity in the header at design
		c_trough.m_Pipe_hl_coef = as_double("Pipe_hl_coef");		//[W/m2-K] Loss coefficient from the header, runner pipe, and non-HCE piping
		c_trough.m_SCA_drives_elec = as_double("SCA_drives_elec");  //[W/SCA] Tracking power, in Watts per SCA drive
		c_trough.m_ColTilt = as_double("tilt");						//[deg] Collector tilt angle (0 is horizontal, 90deg is vertical)
		c_trough.m_ColAz = as_double("azimuth"); 					//[deg] Collector azimuth angle
		c_trough.m_accept_mode = as_integer("accept_mode");			//[-] Acceptance testing mode? (1=yes, 0=no)
		c_trough.m_accept_init = as_double("accept_init");			//[-] In acceptance testing mode - require steady-state startup
		c_trough.m_solar_mult = as_double("solar_mult");			//[-] Solar Multiple
		c_trough.m_mc_bal_hot_per_MW = as_double("mc_bal_hot");     //[kWht/K-MWt] The heat capacity of the balance of plant on the hot side
		c_trough.m_mc_bal_cold_per_MW = as_double("mc_bal_cold");	//[kWht/K-MWt] The heat capacity of the balance of plant on the cold side
		c_trough.m_mc_bal_sca = as_double("mc_bal_sca"); 			//[Wht/K-m] Non-HTF heat capacity associated with each SCA - per meter basis
		
		//[m] The collector aperture width (Total structural area.. used for shadowing)
		size_t nval_W_aperture = -1;
		ssc_number_t *W_aperture = as_array("W_aperture", &nval_W_aperture);
		c_trough.m_W_aperture.resize(nval_W_aperture);
		for (int i = 0; i < nval_W_aperture; i++)
			c_trough.m_W_aperture[i] = (double)W_aperture[i];
		
		//[m^2] Reflective aperture area of the collector
		size_t nval_A_aperture = -1;
		ssc_number_t *A_aperture = as_array("A_aperture", &nval_A_aperture);
		c_trough.m_A_aperture.resize(nval_A_aperture);
		for (int i = 0; i < nval_A_aperture; i++)
			c_trough.m_A_aperture[i] = (double)A_aperture[i];

		//[-] Tracking error derate
		size_t nval_TrackingError = -1;
		ssc_number_t *TrackingError = as_array("TrackingError", &nval_TrackingError);
		c_trough.m_TrackingError.resize(nval_TrackingError);
		for (int i = 0; i < nval_TrackingError; i++)
			c_trough.m_TrackingError[i] = (double)TrackingError[i];
		
		//[-] Geometry effects derate
		size_t nval_GeomEffects = -1;
		ssc_number_t *GeomEffects = as_array("GeomEffects", &nval_GeomEffects);
		c_trough.m_GeomEffects.resize(nval_GeomEffects);
		for (int i = 0; i < nval_GeomEffects; i++)
			c_trough.m_GeomEffects[i] = (double)GeomEffects[i];

		//[-] Clean mirror reflectivity
		size_t nval_Rho_mirror_clean = -1;
		ssc_number_t *Rho_mirror_clean = as_array("Rho_mirror_clean", &nval_Rho_mirror_clean);
		c_trough.m_Rho_mirror_clean.resize(nval_Rho_mirror_clean);
		for (int i = 0; i < nval_Rho_mirror_clean; i++)
			c_trough.m_Rho_mirror_clean[i] = (double)Rho_mirror_clean[i];
		
		//[-] Dirt on mirror derate
		size_t nval_Dirt_mirror = -1;
		ssc_number_t *Dirt_mirror = as_array("Dirt_mirror", &nval_Dirt_mirror);
		c_trough.m_Dirt_mirror.resize(nval_Dirt_mirror);
		for (int i = 0; i < nval_Dirt_mirror; i++)
			c_trough.m_Dirt_mirror[i] = (double)Dirt_mirror[i];
		
		//[-] General optical error derate
		size_t nval_Error = -1;
		ssc_number_t *Error = as_array("Error", &nval_Error);
		c_trough.m_Error.resize(nval_Error);
		for (int i = 0; i < nval_Error; i++)
			c_trough.m_Error[i] = (double)Error[i];
		
		//[m] The average focal length of the collector 
		size_t nval_Ave_Focal_Length = -1;
		ssc_number_t *Ave_Focal_Length = as_array("Ave_Focal_Length", &nval_Ave_Focal_Length);
		c_trough.m_Ave_Focal_Length.resize(nval_Ave_Focal_Length);
		for (int i = 0; i < nval_Ave_Focal_Length; i++)
			c_trough.m_Ave_Focal_Length[i] = (double)Ave_Focal_Length[i];
		
		//[m] The length of the SCA 
		size_t nval_L_SCA = -1;
		ssc_number_t *L_SCA = as_array("L_SCA", &nval_L_SCA);
		c_trough.m_L_SCA.resize(nval_L_SCA);
		for (int i = 0; i < nval_L_SCA; i++)
			c_trough.m_L_SCA[i] = (double)L_SCA[i];

		//[m] The length of a single mirror/HCE unit
		size_t nval_L_aperture = -1;
		ssc_number_t *L_aperture = as_array("L_aperture", &nval_L_aperture);
		c_trough.m_L_aperture.resize(nval_L_aperture);
		for (int i = 0; i < nval_L_aperture; i++)
			c_trough.m_L_aperture[i] = (double)L_aperture[i];
		
		//[-] The number of individual collector sections in an SCA
		size_t nval_ColperSCA = -1;
		ssc_number_t *ColperSCA = as_array("ColperSCA", &nval_ColperSCA);
		c_trough.m_ColperSCA.resize(nval_ColperSCA);
		for (int i = 0; i < nval_ColperSCA; i++)
			c_trough.m_ColperSCA[i] = (double)ColperSCA[i];

		//[m] Piping distance between SCA's in the field
		size_t nval_Distance_SCA = -1;
		ssc_number_t *Distance_SCA = as_array("Distance_SCA", &nval_Distance_SCA);
		c_trough.m_Distance_SCA.resize(nval_Distance_SCA);
		for (int i = 0; i < nval_Distance_SCA; i++)
			c_trough.m_Distance_SCA[i] = (double)Distance_SCA[i];

		c_trough.m_IAM_matrix = as_matrix("IAM_matrix");		//[-] IAM coefficients, matrix for 4 collectors
		
		// Why are these matrices - can't they be arrays?
		c_trough.m_HCE_FieldFrac = as_matrix("HCE_FieldFrac");	//[-] Fraction of the field occupied by this HCE type
		c_trough.m_D_2 = as_matrix("D_2");                      //[m] Inner absorber tube diameter
		c_trough.m_D_3 = as_matrix("D_3");                      //[m] Outer absorber tube diameter
		c_trough.m_D_4 = as_matrix("D_4");                      //[m] Inner glass envelope diameter
		c_trough.m_D_5 = as_matrix("D_5");                      //[m] Outer glass envelope diameter
		c_trough.m_D_p = as_matrix("D_p");                      //[m] Diameter of the absorber flow plug (optional)
		c_trough.m_Flow_type = as_matrix("Flow_type");			//[-] Flow type through the absorber
		c_trough.m_Rough = as_matrix("Rough");					//[m] Roughness of the internal surface
		c_trough.m_alpha_env = as_matrix("alpha_env");			//[-] Envelope absorptance
		// **********************************************************
		
		// Emittance vs. temperature profile for each receiver type and variation
		c_trough.m_epsilon_3_11 = as_matrix_transpose("epsilon_3_11");   //[-] Absorber emittance for receiver type 1 variation 1
		c_trough.m_epsilon_3_12 = as_matrix_transpose("epsilon_3_12"); 	 //[-] Absorber emittance for receiver type 1 variation 2
		c_trough.m_epsilon_3_13 = as_matrix_transpose("epsilon_3_13"); 	 //[-] Absorber emittance for receiver type 1 variation 3
		c_trough.m_epsilon_3_14 = as_matrix_transpose("epsilon_3_14"); 	 //[-] Absorber emittance for receiver type 1 variation 4
		c_trough.m_epsilon_3_21 = as_matrix_transpose("epsilon_3_21"); 	 //[-] Absorber emittance for receiver type 2 variation 1
		c_trough.m_epsilon_3_22 = as_matrix_transpose("epsilon_3_22"); 	 //[-] Absorber emittance for receiver type 2 variation 2
		c_trough.m_epsilon_3_23 = as_matrix_transpose("epsilon_3_23"); 	 //[-] Absorber emittance for receiver type 2 variation 3
		c_trough.m_epsilon_3_24 = as_matrix_transpose("epsilon_3_24"); 	 //[-] Absorber emittance for receiver type 2 variation 4
		c_trough.m_epsilon_3_31 = as_matrix_transpose("epsilon_3_31"); 	 //[-] Absorber emittance for receiver type 3 variation 1
		c_trough.m_epsilon_3_32 = as_matrix_transpose("epsilon_3_32"); 	 //[-] Absorber emittance for receiver type 3 variation 2
		c_trough.m_epsilon_3_33 = as_matrix_transpose("epsilon_3_33"); 	 //[-] Absorber emittance for receiver type 3 variation 3
		c_trough.m_epsilon_3_34 = as_matrix_transpose("epsilon_3_34"); 	 //[-] Absorber emittance for receiver type 3 variation 4
		c_trough.m_epsilon_3_41 = as_matrix_transpose("epsilon_3_41"); 	 //[-] Absorber emittance for receiver type 4 variation 1
		c_trough.m_epsilon_3_42 = as_matrix_transpose("epsilon_3_42"); 	 //[-] Absorber emittance for receiver type 4 variation 2
		c_trough.m_epsilon_3_43 = as_matrix_transpose("epsilon_3_43"); 	 //[-] Absorber emittance for receiver type 4 variation 3
		c_trough.m_epsilon_3_44 = as_matrix_transpose("epsilon_3_44"); 	 //[-] Absorber emittance for receiver type 4 variation 4

		c_trough.m_alpha_abs = as_matrix("alpha_abs");                   //[-] Absorber absorptance
		c_trough.m_Tau_envelope = as_matrix("Tau_envelope");             //[-] Envelope transmittance
		c_trough.m_EPSILON_4 = as_matrix("EPSILON_4");                   //[-] Inner glass envelope emissivities
		c_trough.m_EPSILON_5 = as_matrix("EPSILON_5");                   //[-] Outer glass envelope emissivities
		c_trough.m_GlazingIntact = as_matrix("GlazingIntactIn");         //[-] Glazing intact (broken glass) flag {1=true, else=false}
		c_trough.m_P_a = as_matrix("P_a");		                         //[torr] Annulus gas pressure				 
		c_trough.m_AnnulusGas = as_matrix("AnnulusGas");		         //[-] Annulus gas type (1=air, 26=Ar, 27=H2)
		c_trough.m_AbsorberMaterial = as_matrix("AbsorberMaterial");	 //[-] Absorber material type
		c_trough.m_Shadowing = as_matrix("Shadowing");                   //[-] Receiver bellows shadowing loss factor
		c_trough.m_Dirt_HCE = as_matrix("Dirt_HCE");                     //[-] Loss due to dirt on the receiver envelope
		c_trough.m_Design_loss = as_matrix("Design_loss");               //[-] Receiver heat loss at design
				
		c_trough.m_SCAInfoArray = as_matrix("SCAInfoArray");			 //[-] Receiver (,1) and collector (,2) type for each assembly in loop 
		
		//[-] Collector defocus order
		size_t nval_SCADefocusArray = -1;
		ssc_number_t *SCADefocusArray = as_array("SCADefocusArray", &nval_SCADefocusArray);
		c_trough.m_SCADefocusArray.resize(nval_SCADefocusArray);
		for (int i = 0; i < nval_SCADefocusArray; i++)
			c_trough.m_SCADefocusArray[i] = (int)SCADefocusArray[i];

		// Allocate trough outputs
		//"Q_thermal_trough"

		// ********************************
		// ********************************
		// Now add the Heat Sink as a power cycle class
		// ********************************
		// ********************************
		// Heat Sink
		C_pc_heat_sink heat_sink;
		heat_sink.ms_params.m_T_htf_hot_des = as_double("T_loop_out");		//[C] FIELD design outlet temperature
		heat_sink.ms_params.m_T_htf_cold_des = as_double("T_loop_in_des");	//[C] FIELD design inlet temperature
		heat_sink.ms_params.m_q_dot_des = as_double("W_pb_design") / as_double("eta_ref");	//[MWt] FIELD design thermal power
		heat_sink.ms_params.m_htf_pump_coef = as_double("pb_pump_coef");	//[kWe/kg/s]
		
		heat_sink.ms_params.m_pc_fl = as_integer("Fluid");
		heat_sink.ms_params.m_pc_fl_props = as_matrix("field_fl_props");
		
		//// Logic to choose between steam and sco2 power cycle 
		//int pb_tech_type = as_integer("pc_config");		//[-] 0: Steam Rankine (224), 1: user defined

		//if( pb_tech_type == 2 )
		//{
		//	log("The sCO2 power cycle is not yet supported by the new CSP Solver and Dispatch Optimization models.\n", SSC_WARNING);
		//	return;
		//}

		//C_pc_Rankine_indirect_224 power_cycle;
		//C_pc_Rankine_indirect_224::S_params *pc = &power_cycle.ms_params;
		//pc->m_P_ref = as_double("W_pb_design");                         //[MWe] Rated plant capacity
		//pc->m_eta_ref = as_double("eta_ref");					        //[-] Reference conversion efficiency at design conditions
		//pc->m_T_htf_hot_ref = as_double("T_loop_out");			        //[C] FIELD design outlet temperature
		//pc->m_T_htf_cold_ref = as_double("T_loop_in_des");			    //[C] FIELD design inlet temperature
		//pc->m_cycle_max_frac = as_double("cycle_max_frac");			    //[-]
		//pc->m_cycle_cutoff_frac = as_double("cycle_cutoff_frac");	    //[-]
		//pc->m_q_sby_frac = as_double("q_sby_frac");					    //[-]
		//pc->m_startup_time = as_double("startup_time");				    //[hr]
		//pc->m_startup_frac = as_double("startup_frac");				    //[-]
		//pc->m_htf_pump_coef = as_double("pb_pump_coef");			    //[kW/kg/s]
		//pc->m_pc_fl = as_integer("Fluid");							    //[-]
		//pc->m_pc_fl_props = as_matrix("field_fl_props");                //[-]

		//if( pb_tech_type == 0 )
		//{
		//	pc->m_dT_cw_ref = as_double("dT_cw_ref");			//[C]
		//	pc->m_T_amb_des = as_double("T_amb_des");			//[C]
		//	pc->m_P_boil = as_double("P_boil");					//[bar]
		//	pc->m_CT = as_integer("CT");						//[-]
		//	pc->m_tech_type = as_double("tech_type");			//[-]					
		//	pc->m_T_approach = as_double("T_approach");			//[C/K]
		//	pc->m_T_ITD_des = as_double("T_ITD_des");			//[C/K]
		//	pc->m_P_cond_ratio = as_double("P_cond_ratio");		//[-]
		//	pc->m_pb_bd_frac = as_double("pb_bd_frac");			//[-]
		//	pc->m_P_cond_min = as_double("P_cond_min");			//[inHg]
		//	pc->m_n_pl_inc = as_integer("n_pl_inc");			//[-]

		//	size_t n_F_wc = -1;
		//	ssc_number_t *p_F_wc = as_array("F_wc", &n_F_wc);	//[-]
		//	pc->m_F_wc.resize(n_F_wc, 0.0);
		//	for( int i = 0; i < n_F_wc; i++ )
		//		pc->m_F_wc[i] = (double)p_F_wc[i];

		//	// Set User Defined cycle parameters to appropriate values
		//	pc->m_is_user_defined_pc = false;
		//	pc->m_W_dot_cooling_des = std::numeric_limits<double>::quiet_NaN();
		//}
		//else if( pb_tech_type == 1 )
		//{
		//	pc->m_is_user_defined_pc = true;

		//	// User-Defined Cycle Parameters
		//	pc->m_T_amb_des = as_double("ud_T_amb_des");			//[C]
		//	pc->m_W_dot_cooling_des = as_double("ud_f_W_dot_cool_des") / 100.0*pc->m_P_ref;	//[MWe]
		//	pc->m_m_dot_water_des = as_double("ud_m_dot_water_cool_des");		//[kg/s]

		//	// Also need lower and upper levels for the 3 independent variables...
		//	pc->m_T_htf_low = as_double("ud_T_htf_low");			//[C]
		//	pc->m_T_htf_high = as_double("ud_T_htf_high");			//[C]
		//	pc->m_T_amb_low = as_double("ud_T_amb_low");			//[C]
		//	pc->m_T_amb_high = as_double("ud_T_amb_high");			//[C]
		//	pc->m_m_dot_htf_low = as_double("ud_m_dot_htf_low");	//[-]
		//	pc->m_m_dot_htf_high = as_double("ud_m_dot_htf_high");	//[-]

		//	// User-Defined Cycle Off-Design Tables 
		//	pc->mc_T_htf_ind = as_matrix("ud_T_htf_ind_od");
		//	pc->mc_T_amb_ind = as_matrix("ud_T_amb_ind_od");
		//	pc->mc_m_dot_htf_ind = as_matrix("ud_m_dot_htf_ind_od");
		//}

		// ********************************
		// ********************************
		// Now add the storage class
		// ********************************
		// ********************************
		C_csp_two_tank_tes storage;
		C_csp_two_tank_tes::S_params *tes = &storage.ms_params;
		tes->m_field_fl = as_integer("Fluid");
		tes->m_field_fl_props = as_matrix("field_fl_props");
		tes->m_tes_fl = as_integer("Fluid");
		tes->m_tes_fl_props = as_matrix("field_fl_props");
		tes->m_is_hx = false;									//[-] Assuming direct storage here
		tes->m_W_dot_pc_design = as_double("W_pb_design");		//[MWe]
		tes->m_eta_pc = as_double("eta_ref");					//[-]
		tes->m_solarm = as_double("solar_mult");				//[-]
		tes->m_ts_hours = as_double("tshours");					//[hr]

		// Hardcode NO TES for now
		tes->m_ts_hours = 0.0;		//[hr]

		tes->m_h_tank = as_double("h_tank");					//[m]
		tes->m_u_tank = as_double("u_tank");					//[W/m^2-K]
		tes->m_tank_pairs = as_integer("tank_pairs");			//[-]
		tes->m_hot_tank_Thtr = as_double("hot_tank_Thtr");		//[C]
		tes->m_hot_tank_max_heat = as_double("tank_max_heat");	//[MW]
		tes->m_cold_tank_Thtr = as_double("cold_tank_Thtr");	//[C]
		tes->m_cold_tank_max_heat = as_double("tank_max_heat");	//[MW]
		tes->m_dt_hot = 0.0;									//[-] Assuming direct storage here
		tes->m_T_field_in_des = as_double("T_loop_in_des");		//[C]
		tes->m_T_field_out_des = as_double("T_loop_out");		//[C]
		tes->m_T_tank_hot_ini = as_double("T_loop_in_des");		//[C]
		tes->m_T_tank_cold_ini = as_double("T_loop_out");		//[C]
		tes->m_h_tank_min = as_double("h_tank_min");			//[m]
		tes->m_f_V_hot_ini = as_double("V_tank_hot_ini");		//[-]
		tes->m_htf_pump_coef = as_double("pb_pump_coef");		//[kW/kg/s]
	
		// ********************************
		// ********************************
		// Now add the TOU class
		// ********************************
		// ********************************
		C_csp_tou_block_schedules tou;
		tou.setup_block_uniform_tod();
		//C_csp_tou_block_schedules::S_params *tou_params = &tou.ms_params;
		//tou_params->mc_csp_ops.mc_weekdays = as_matrix("weekday_schedule");
		//tou_params->mc_csp_ops.mc_weekends = as_matrix("weekend_schedule");
		//tou_params->mc_pricing.mc_weekdays = as_matrix("weekday_schedule");
		//tou_params->mc_pricing.mc_weekends = as_matrix("weekend_schedule");
		tou.mc_dispatch_params.m_dispatch_optimize = false;
		//tou.mc_dispatch_params.m_is_write_ampl_dat = false;
		//tou.mc_dispatch_params.m_is_ampl_engine = false;
		//tou.mc_dispatch_params.m_ampl_data_dir = "";
		//tou.mc_dispatch_params.m_ampl_exec_call = "";
		//if( tou.mc_dispatch_params.m_dispatch_optimize )
		//{
		//	tou.mc_dispatch_params.m_optimize_frequency = as_integer("disp_frequency");
		//	tou.mc_dispatch_params.m_optimize_horizon = as_integer("disp_horizon");
		//	tou.mc_dispatch_params.m_max_iterations = as_integer("disp_max_iter");
		//	tou.mc_dispatch_params.m_solver_timeout = as_double("disp_timeout");
		//	tou.mc_dispatch_params.m_mip_gap = as_double("disp_mip_gap");
		//	tou.mc_dispatch_params.m_presolve_type = as_integer("disp_spec_presolve");
		//	tou.mc_dispatch_params.m_bb_type = as_integer("disp_spec_bb");
		//	tou.mc_dispatch_params.m_scaling_type = as_integer("disp_spec_scaling");
		//}
		//tou.mc_dispatch_params.m_is_block_dispatch = !tou.mc_dispatch_params.m_dispatch_optimize;      //mw
		//tou.mc_dispatch_params.m_use_rule_1 = true;
		//tou.mc_dispatch_params.m_standby_off_buffer = 2.0;
		//tou.mc_dispatch_params.m_use_rule_2 = false;
		//tou.mc_dispatch_params.m_q_dot_rec_des_mult = -1.23;
		//tou.mc_dispatch_params.m_f_q_dot_pc_overwrite = -1.23;

		//size_t n_f_turbine = -1;
		//ssc_number_t *p_f_turbine = as_array("tslogic_c", &n_f_turbine);
		//tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC].resize(n_f_turbine, 0.0);
		////tou_params->mv_t_frac.resize(n_f_turbine, 0.0);
		//for( int i = 0; i < n_f_turbine; i++ )
		//	tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC][i] = (double)p_f_turbine[i];

		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(9, 0.0);
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][0] = 1.0;  //as_double("dispatch_factor1");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][1] = 1.0;  //as_double("dispatch_factor2");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][2] = 1.0;  //as_double("dispatch_factor3");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][3] = 1.0;  //as_double("dispatch_factor4");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][4] = 1.0;  //as_double("dispatch_factor5");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][5] = 1.0;  //as_double("dispatch_factor6");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][6] = 1.0;  //as_double("dispatch_factor7");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][7] = 1.0;  //as_double("dispatch_factor8");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][8] = 1.0;  //as_double("dispatch_factor9");

		// System parameters
		C_csp_solver::S_csp_system_params system;
		system.m_pb_fixed_par = as_double("pb_fixed_par");
		system.m_bop_par = 0.0;
		system.m_bop_par_f = 0.0;
		system.m_bop_par_0 = 0.0;
		system.m_bop_par_1 = 0.0;
		system.m_bop_par_2 = 0.0;

		// Instantiate Solver
		C_csp_solver csp_solver(weather_reader, c_trough, heat_sink, storage, tou, system);

		int out_type = -1;
		std::string out_msg = "";
		try
		{
			// Initialize Solver
			csp_solver.init();
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( csp_solver.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				log(out_msg, out_type);
			}

			log(csp_exception.m_error_message, SSC_ERROR, -1.0);

			return;
		}
		
		

		float **ptr_array = new float*[C_csp_solver::N_END];
		//float **post_proc_array = new float*[C_csp_solver::N_END_POST_PROC];
		//
		//for( int i = 0; i < C_csp_solver::N_END_POST_PROC; i++ )
		//{
		//	post_proc_array[i] = 0;
		//}
		//
		//post_proc_array[C_csp_solver::PC_Q_STARTUP] = allocate("q_pc_startup", n_steps_fixed);

		for( int i = 0; i < C_csp_solver::N_END; i++ )
		{
			ptr_array[i] = 0;
		}

			// Simulation outputs
		ptr_array[C_csp_solver::TIME_FINAL] = allocate("time_hr1", n_steps_fixed);
		ptr_array[C_csp_solver::SOLZEN] = allocate("solzen1", n_steps_fixed);
		ptr_array[C_csp_solver::SOLAZ] = allocate("solaz1", n_steps_fixed);
		ptr_array[C_csp_solver::BEAM] = allocate("beam1", n_steps_fixed);
		ptr_array[C_csp_solver::TDRY] = allocate("tdry1", n_steps_fixed);
		ptr_array[C_csp_solver::TWET] = allocate("twet1", n_steps_fixed);
		ptr_array[C_csp_solver::RH] = allocate("rh1", n_steps_fixed);


		// Collector-receiver outputs
		//ptr_array[C_csp_solver::CR_Q_INC] = allocate("q_sf_inc", n_steps_fixed);
		//ptr_array[C_csp_solver::CR_OPT_ETA] = allocate("eta_field", n_steps_fixed);
		ptr_array[C_csp_solver::CR_DEFOCUS] = allocate("defocus1", n_steps_fixed);
		//ptr_array[C_csp_solver::CR_ADJUST] = allocate("sf_adjust_out", n_steps_fixed);
		//ptr_array[C_csp_solver::REC_Q_DOT_INC] = allocate("q_dot_rec_inc", n_steps_fixed);
		//ptr_array[C_csp_solver::REC_ETA_THERMAL] = allocate("eta_therm", n_steps_fixed);
			// 7.26.16, twn: Need to keep this for now, for mass balance
		ptr_array[C_csp_solver::REC_Q_DOT] = allocate("Q_thermal", n_steps_fixed);
		ptr_array[C_csp_solver::REC_M_DOT] = allocate("m_dot_rec1", n_steps_fixed);
		//ptr_array[C_csp_solver::REC_Q_DOT_STARTUP] = allocate("q_startup", n_steps_fixed);
		//ptr_array[C_csp_solver::REC_T_IN] = allocate("T_rec_in", n_steps_fixed);
		//ptr_array[C_csp_solver::REC_T_OUT] = allocate("T_rec_out", n_steps_fixed);
		//ptr_array[C_csp_solver::CR_Q_DOT_PIPING_LOSS] = allocate("q_piping_losses", n_steps_fixed);

		// Power cycle outputs
		//ptr_array[C_csp_solver::PC_ETA_THERMAL] = allocate("eta1", n_steps_fixed);
		ptr_array[C_csp_solver::PC_Q_DOT] = allocate("q_pb1", n_steps_fixed);
		ptr_array[C_csp_solver::PC_M_DOT] = allocate("m_dot_pc1", n_steps_fixed);
		//ptr_array[C_csp_solver::PC_Q_DOT_STARTUP] = allocate("q_dot_pc_startup1", n_steps_fixed);
		//ptr_array[C_csp_solver::PC_W_DOT] = allocate("P_cycle1", n_steps_fixed);
		//ptr_array[C_csp_solver::PC_T_IN] = allocate("T_pc_in1", n_steps_fixed);
		//ptr_array[C_csp_solver::PC_T_OUT] = allocate("T_pc_out1", n_steps_fixed);
		//ptr_array[C_csp_solver::PC_M_DOT_WATER] = allocate("m_dot_water_pc1", n_steps_fixed);

		// Thermal energy storage outputs
		ptr_array[C_csp_solver::TES_Q_DOT_LOSS] = allocate("tank_losses1", n_steps_fixed);
		ptr_array[C_csp_solver::TES_W_DOT_HEATER] = allocate("q_heater1", n_steps_fixed);
		ptr_array[C_csp_solver::TES_T_HOT] = allocate("T_tes_hot1", n_steps_fixed);
		ptr_array[C_csp_solver::TES_T_COLD] = allocate("T_tes_cold1", n_steps_fixed);
		ptr_array[C_csp_solver::TES_Q_DOT_DC] = allocate("q_dc_tes1", n_steps_fixed);
		ptr_array[C_csp_solver::TES_Q_DOT_CH] = allocate("q_ch_tes1", n_steps_fixed);
		ptr_array[C_csp_solver::TES_E_CH_STATE] = allocate("e_ch_tes1", n_steps_fixed);
		ptr_array[C_csp_solver::TES_M_DOT_DC] = allocate("m_dot_tes_dc1", n_steps_fixed);
		ptr_array[C_csp_solver::TES_M_DOT_CH] = allocate("m_dot_tes_ch1", n_steps_fixed);

		// Parasitics outputs
		ptr_array[C_csp_solver::COL_W_DOT_TRACK] = allocate("pparasi1", n_steps_fixed);
		ptr_array[C_csp_solver::CR_W_DOT_PUMP] = allocate("P_tower_pump1", n_steps_fixed);
		ptr_array[C_csp_solver::SYS_W_DOT_PUMP] = allocate("htf_pump_power1", n_steps_fixed);
		ptr_array[C_csp_solver::PC_W_DOT_COOLING] = allocate("P_cooling_tower_tot1", n_steps_fixed);
		ptr_array[C_csp_solver::SYS_W_DOT_FIXED] = allocate("P_fixed1", n_steps_fixed);
		ptr_array[C_csp_solver::SYS_W_DOT_BOP] = allocate("P_plant_balance_tot1", n_steps_fixed);

		// System outputs
		ptr_array[C_csp_solver::W_DOT_NET] = allocate("P_out_net1", n_steps_fixed);

		// Controller outputs
		ptr_array[C_csp_solver::TOU_PERIOD] = allocate("tou_value1", n_steps_fixed);
		ptr_array[C_csp_solver::PRICING_MULT] = allocate("pricing_mult1", n_steps_fixed);
		ptr_array[C_csp_solver::N_OP_MODES] = allocate("n_op_modes1", n_steps_fixed);
		ptr_array[C_csp_solver::OP_MODE_1] = allocate("op_mode_11", n_steps_fixed);
		ptr_array[C_csp_solver::OP_MODE_2] = allocate("op_mode_21", n_steps_fixed);
		ptr_array[C_csp_solver::OP_MODE_3] = allocate("op_mode_31", n_steps_fixed);
		ptr_array[C_csp_solver::ERR_M_DOT] = allocate("m_dot_balance1", n_steps_fixed);
		ptr_array[C_csp_solver::ERR_Q_DOT] = allocate("q_balance1", n_steps_fixed);


		ptr_array[C_csp_solver::PC_Q_DOT_SB] = allocate("q_dot_pc_sb1", n_steps_fixed);
		ptr_array[C_csp_solver::PC_Q_DOT_MIN] = allocate("q_dot_pc_min1", n_steps_fixed);
		ptr_array[C_csp_solver::PC_Q_DOT_MAX] = allocate("q_dot_pc_max1", n_steps_fixed);
		ptr_array[C_csp_solver::PC_Q_DOT_TARGET] = allocate("q_dot_pc_target1", n_steps_fixed);

		ptr_array[C_csp_solver::CTRL_IS_REC_SU] = allocate("is_rec_su_allowed1", n_steps_fixed);
		ptr_array[C_csp_solver::CTRL_IS_PC_SU] = allocate("is_pc_su_allowed1", n_steps_fixed);
		ptr_array[C_csp_solver::CTRL_IS_PC_SB] = allocate("is_pc_sb_allowed1", n_steps_fixed);
		ptr_array[C_csp_solver::EST_Q_DOT_CR_SU] = allocate("q_dot_est_cr_su1", n_steps_fixed);
		ptr_array[C_csp_solver::EST_Q_DOT_CR_ON] = allocate("q_dot_est_cr_on1", n_steps_fixed);
		ptr_array[C_csp_solver::EST_Q_DOT_DC] = allocate("q_dot_est_tes_dc1", n_steps_fixed);
		ptr_array[C_csp_solver::EST_Q_DOT_CH] = allocate("q_dot_est_tes_ch1", n_steps_fixed);

		ptr_array[C_csp_solver::CTRL_OP_MODE_SEQ_A] = allocate("operating_modes_a1", n_steps_fixed);
		ptr_array[C_csp_solver::CTRL_OP_MODE_SEQ_B] = allocate("operating_modes_b1", n_steps_fixed);
		ptr_array[C_csp_solver::CTRL_OP_MODE_SEQ_C] = allocate("operating_modes_c1", n_steps_fixed);

		ptr_array[C_csp_solver::DISPATCH_SOLVE_STATE] = allocate("disp_solve_state1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_SOLVE_ITER] = allocate("disp_solve_iter1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_SOLVE_OBJ] = allocate("disp_objective1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_SOLVE_OBJ_RELAX] = allocate("disp_obj_relax1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_QSF_EXPECT] = allocate("disp_qsf_expected1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_QSFPROD_EXPECT] = allocate("disp_qsfprod_expected1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_QSFSU_EXPECT] = allocate("disp_qsfsu_expected1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_TES_EXPECT] = allocate("disp_tes_expected1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_PCEFF_EXPECT] = allocate("disp_pceff_expected1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_SFEFF_EXPECT] = allocate("disp_thermeff_expected1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_QPBSU_EXPECT] = allocate("disp_qpbsu_expected1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_WPB_EXPECT] = allocate("disp_wpb_expected1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_REV_EXPECT] = allocate("disp_rev_expected1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_PRES_NCONSTR] = allocate("disp_presolve_nconstr1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_PRES_NVAR] = allocate("disp_presolve_nvar1", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_SOLVE_TIME] = allocate("disp_solve_time1", n_steps_fixed);

		try
		{
			// Simulate !
			csp_solver.Ssimulate(sim_setup,
				ssc_trough_physical_process_heat_sim_progress, (void*)this,
				ptr_array);
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( csp_solver.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				log(out_msg);
			}

			log(csp_exception.m_error_message, SSC_WARNING);
			delete[] ptr_array;

			return;
		}


		// ************************************
		// ************************************
		delete[] ptr_array;
		// ************************************
		// ************************************


	}

};

static bool ssc_trough_physical_process_heat_sim_progress(void *data, double percent, C_csp_messages *csp_msg, float time_sec)
{
	cm_trough_physical_process_heat *cm = static_cast<cm_trough_physical_process_heat*> (data);
	if( !cm )
		false;

	if( csp_msg != 0 )
	{
		int out_type;
		string message;
		while( csp_msg->get_message(&out_type, &message) )
		{
			cm->log(message, out_type == C_csp_messages::WARNING ? SSC_WARNING : SSC_NOTICE, time_sec);
		}
	}
	bool ret = cm->update("", percent);

	return ret;
}

DEFINE_MODULE_ENTRY(trough_physical_process_heat, "Physical trough process heat applications", 1)