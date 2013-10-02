#include "core.h"
#include "tckernel.h"

static var_info _cm_vtab_tcstrough_physical[] = {
//   weather reader inputs
//   VARTYPE            DATATYPE          NAME                LABEL                                                             UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "file_name",         "local weather file path",                                        "",             "",             "Weather",       "*",                       "LOCAL_FILE",            "" },
    { SSC_INPUT,        SSC_NUMBER,      "track_mode",        "Tracking mode",                                                  "",             "",             "Weather",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tilt",              "Tilt angle of surface/axis",                                     "",             "",             "Weather",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "azimuth",           "Azimuth angle of surface/axis",                                  "",             "",             "Weather",       "*",                       "",                      "" },

    // general
    { SSC_INPUT,        SSC_NUMBER,      "TOUPeriod",         "Time of Use Period",                                             "",             "",             "other",         "*",                       "INTEGER",               "" },

//   solar field (type 250) inputs
//   VARTYPE            DATATYPE          NAME                LABEL                                                             UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_NUMBER,      "nSCA",              "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nHCEt",             "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nColt",             "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nHCEVar",           "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nLoops",            "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_pump",          "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "HDR_rough",         "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "theta_stow",        "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "theta_dep",         "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Row_Distance",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "FieldConfig",       "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_startup",         "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_rated_cap",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htfmin",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htfmax",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_loop_in_des",     "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_loop_out",        "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Fluid",             "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_field_ini",       "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_fp",              "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_bn_des",          "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_max",         "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_min",         "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Pipe_hl_coef",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "SCA_drives_elec",   "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fthrok",            "Label",                                                          "",             "",             "u1",            "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "fthrctrl",          "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ColTilt",           "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ColAz",             "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "accept_mode",       "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "accept_init",       "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "accept_loc",        "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "solar_mult",        "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_hot",        "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_cold",       "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_sca",        "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "OptCharType",       "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "CollectorType",     "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "W_aperture",        "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "A_aperture",        "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "IamF0",             "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "IamF1",             "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "IamF2",             "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "reflectivity",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "TrackingError",     "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "GeomEffects",       "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Rho_mirror_clean",  "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Dirt_mirror",       "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Error",             "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Ave_Focal_Length",  "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "L_SCA",             "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "L_aperture",        "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "ColperSCA",         "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Distance_SCA",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "HCE_FieldFrac",     "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_2",               "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_3",               "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_4",               "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_5",               "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_p",               "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Flow_type",         "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Rough",             "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "alpha_env",         "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_11",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_12",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_13",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_14",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_21",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_22",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_23",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_24",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_31",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_32",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_33",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_34",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_41",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_42",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_43",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_44",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "alpha_abs",         "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Tau_envelope",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "EPSILON_4",         "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "EPSILON_5",         "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "GlazingIntactIn",   "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "P_a",               "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "AnnulusGas",        "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "AbsorberMaterial",  "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Shadowing",         "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Dirt_HCE",          "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Design_loss",       "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "SCAInfoArray",      "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "SCADefocusArray",   "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    // solar field (type 250) initial condition inputs
    { SSC_INPUT,        SSC_NUMBER,      "I_b",               "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_db",              "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_wind",            "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_amb",             "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_dp",              "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "SolarAz",           "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "defocus",           "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_cold_in",         "Label",                                                          "",             "",             "u1",            "*",                       "",                      "" },


//   controller (type 251) inputs
//   VARTYPE            DATATYPE          NAME                LABEL                                                             UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_NUMBER,      "field_fluid",       "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "field_fl_props",    "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "store_fluid",       "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tshours",           "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_hx",             "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dt_hot",            "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dt_cold",           "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hx_config",         "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_max_aux",         "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_set_aux",         "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_tank_hot_ini",    "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_hot_ini",    "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_cold_ini",   "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "vol_tank",          "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank",            "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank_min",        "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "u_tank",            "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_pairs",        "Label",                                                          "",             "",             "controller",    "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_Thtr",    "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_Thtr",     "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_max_heat",     "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_field_in_des",    "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_field_out_des",   "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_pb_design",       "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "W_pb_design",       "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_max_frac",    "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_cutoff_frac", "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "solarm",            "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_pump_coef",      "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_pump_coef",     "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_fixed_par",      "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "bop_array",         "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "aux_array",         "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_startup",         "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fossil_mode",       "Label",                                                          "",             "",             "controller",    "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "fthr_ok",           "Label",                                                          "",             "",             "controller",    "*",                       "INTEGER",               "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "nSCA",              "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "I_bn_des",          "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fc_on",             "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",        "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_standby_reset",   "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_type",          "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_a",         "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_b",         "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_c",         "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "ffrac",             "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tc_fill",           "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tc_void",           "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_dis_out_min",     "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_ch_out_max",      "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nodes",             "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_tc_cold",         "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    // controller (type 251)  initial conditions
    { SSC_INPUT,        SSC_NUMBER,      "I_bn",              "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb",             "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_field",       "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htf_ref",     "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_field_out",       "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_pb_out_init",     "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_pb_demand",       "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_startup",         "Label",                                                          "",             "",             "controller",    "*",                       "",                      "" },

//   powerblock (type 224) inputs
//   VARTYPE            DATATYPE          NAME                LABEL                                                             UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_NUMBER,      "P_ref",             "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_ref",           "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_hot_ref",     "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_cold_ref",    "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",         "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",         "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "HTF",               "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",        "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil",            "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_time",      "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",      "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tech_type",         "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",        "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",         "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",      "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",        "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",        "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",          "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",              "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mode",              "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    // powerblock (type 224) initial conditions
    { SSC_INPUT,        SSC_NUMBER,      "T_wb",              "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
//	{ SSC_INPUT,        SSC_NUMBER,      "T_db",              "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
//	{ SSC_INPUT,        SSC_NUMBER,      "P_amb",             "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rh",                "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_hot",         "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "m_dot_htf_init",    "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "demand_var",        "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "standby_control",   "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "TOU",               "Label",                                                          "",             "",             "powerblock",    "*",                       "",                      "" },

 //  enet calculator
    { SSC_INPUT,        SSC_NUMBER,      "eta_lhv",           "Label",                                                          "",             "",             "enet",          "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_tes_htr",       "Label",                                                          "",             "",             "enet",          "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fp_mode",           "Label",                                                          "",             "",             "enet",          "*",                       "",                      "" },



// OUTPUTS
// The names of the output variables should match the parameter names for the TCS units in order to signal the TCS kernel to store the values by timestep

//   weather file reader
//   VARTYPE            DATATYPE          NAME                LABEL                                                             UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_OUTPUT,       SSC_ARRAY,       "beam",              "Beam normal irradiance",                                         "W/m2",         "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hour",              "Hour of Year",                                                   "",             "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pres",              "Pressure",                                                       "mbar",         "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "wspd",              "Wind Speed",                                                     "m/s",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solazi",            "Solar Azimuth",                                                  "deg",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solzen",            "Solar Zenith",                                                   "deg",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tdry",              "Dry bulb temperature",                                           "C",            "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "twet",              "Wet bulb temperature",                                           "C",            "",            "weather",        "*",                       "LENGTH=8760",           "" },

    { SSC_OUTPUT,       SSC_ARRAY,       "EqOpteff",          "Collector equivalent optical efficiency",                        "none",         "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "Theta_ave",         "Field average theta value",                                      "deg",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "CosTh_ave",         "Field average costheta value",                                   "none",         "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "IAM_ave",           "Field average incidence angle modifier",                         "none",         "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "RowShadow_ave",     "Field average row shadowing loss",                               "none",         "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "EndLoss_ave",       "Field average end loss",                                         "none",         "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "dni_costh",         "DNI-cosine effect product",                                      "W/m2",         "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "qinc_costh",        "Incident energy-cosine effect product",                          "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "P_cond",            "Condenser pressure",                                             "Pa",           "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "f_bays",            "Fraction of operating heat rejection bays",                      "none",         "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_avail",       "HTF mass flow rate from the field",                              "kg/hr",        "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_pb",          "Mass flow rate of HTF to PB",                                    "kg/hr",        "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_charge_field","Mass flow rate on field side of HX",                             "kg/hr",        "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_discharge_tank","Mass flow rate on storage side of HX",                         "kg/hr",        "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_aux",         "Auxiliary heater mass flow rate",                                "kg/hr",        "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_htf",         "Flow rate in a single loop",                                     "kg/s",         "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_makeup",      "Cooling water makeup flow rate",                                 "kg/hr",        "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "SCAs_def",          "The fraction of focused SCA's",                                  "none",         "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "mass_tank_cold",    "Mass of total fluid in the cold tank",                           "kg",           "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "mass_tank_hot",     "Mass of total fluid in the hot tank",                            "kg",           "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "DP_tot",            "Total HTF pressure drop",                                        "bar",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "eta",               "Cycle thermal efficiency",                                       "none",         "",            "Type224",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "W_dot_pump",        "Required solar field pumping power",                             "MWh",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "htf_pump_power",    "Pumping power for storage, power block loops",                   "MWh",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "SCA_par_tot",       "Parasitic electric power consumed by the SC drives",             "MWh",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "bop_par",           "Parasitic power as a function of power block load",              "MWh",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "fixed_par",         "Fixed parasitic power losses - every hour of operation",         "MWh",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "aux_par",           "Parasitic power associated with auxiliary heater",               "MWh",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "W_cool_par",        "Cooling system parasitic load",                                  "MWh",          "",            "Type224",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_cycle_gross",     "Electrical source - Power cycle gross output",                   "MWe",          "",            "Net_E_Calc",     "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_net",             "Cycle power output",                                             "MWe",          "",            "Net_E_Calc",     "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_avail",           "Thermal power produced by the field",                            "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_inc_sf_tot",      "Total power incident on the field",                              "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_abs_tot",         "Total absorbed energy",                                          "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_pb",              "Thermal energy to the power block",                              "MWt",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_to_tes",          "Thermal energy into storage",                                    "MWt",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "Pipe_hl",           "Pipe heat loss in the hot header and the hot runner",            "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dump",            "Dumped thermal energy",                                          "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tank_losses",       "Thermal losses from tank",                                       "MWt",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_loss_tot",        "Total receiver thermal and optical losses",                      "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_loss_spec_tot",   "Field-average receiver thermal losses",                          "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "Q_aux_backup",      "Thermal power provided by auxiliary fossil backup system",       "MWt",          "",            "SumCalc",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "Fuel_usage",        "Total fossil fuel usage by all plant subsystems",                "MMBTU",        "",            "SumCalc",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "Q_par_sf_fp",       "Thermal energy for freeze protection in receiver/solar field",   "MWt",          "",            "SumCalc",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "Q_par_tes_fp",      "Thermal energy used for freeze protection in TES",               "MWt",          "",            "SumCalc",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "E_bal_startup",     "Startup energy consumed",                                        "MWt",          "",            "Type250",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_field_in",        "HTF temperature into collector field header",                    "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_sys_h",           "Solar field HTF outlet temperature",                             "C",            "",            "Type250",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_sys_c",           "Collector inlet temperature",                                    "C",            "",            "Type250",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank_cold_in",    "Cold tank HTF inlet temperature",                                "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank_hot_in",     "Hot tank HTF inlet temperature",                                 "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank_cold_fin",   "Cold tank HTF temperature at end of timestep",                   "K",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank_hot_fin",    "Hot tank HTF temperature at end of timestep",                    "K",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Ts_hot",            "Hot HTF exiting storage HX",                                     "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Ts_cold",           "Cold HTF exiting storage HX",                                    "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_pb_in",           "HTF temperature to power block",                                 "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_pb_out",          "Fluid temperature from the power block",                         "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "vol_tank_cold_fin", "Cold tank HTF volume at end of timestep",                        "m3",           "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "vol_tank_hot_fin",  "Hot tank HTF volume at end of timestep",                         "m3",           "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "vol_tank_total",    "Total HTF volume in storage",                                    "m3",           "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	var_info_invalid };



class cm_tcstrough_physical : public tcKernel
{
public:

	cm_tcstrough_physical(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info( _cm_vtab_tcstrough_physical );
		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above
	}

	void exec( ) throw( general_error )
	{
		bool debug_mode = (__DEBUG__ == 1);  // When compiled in VS debug mode, this will use the trnsys weather file; otherwise, it will attempt to open the file with name that was passed in
		//Add weather file reader unit
		int weather = 0;
		if(debug_mode) weather = add_unit("trnsys_weatherreader", "TRNSYS weather reader");
		else weather = add_unit("weatherreader", "TCS weather reader");
		//Add Physical Solar Field Model
		int	u1 = add_unit( "sam_mw_trough_type250", "Physical Trough Solar Field" );
		//Add Physical Controller Model
		int u2 = add_unit( "sam_mw_trough_type251", "Controller" );
		//Add Physical Power Block Model
		int u3 = add_unit( "sam_mw_pt_type224", "Power Block" );
		//E_net calculator
		int u4 = add_unit( "sam_mw_csp_SumCalcs", "Net Energy Calculator" );

		if(debug_mode)
		{
			set_unit_value( weather, "file_name", "C:/svn_NREL/main/ssc/tcs/typelib/TRNSYS_weather_outputs/tucson_trnsys_weather.out" );
			set_unit_value( weather, "i_hour", "TIME" );
			set_unit_value( weather, "i_month", "month" );
			set_unit_value( weather, "i_day", "day" );
			set_unit_value( weather, "i_global", "GlobalHorizontal" );
			set_unit_value( weather, "i_beam", "DNI" );
			set_unit_value( weather, "i_diff", "DiffuseHorizontal" );
			set_unit_value( weather, "i_tdry", "T_dry" );
			set_unit_value( weather, "i_twet", "T_wet" );
			set_unit_value( weather, "i_tdew", "T_dew" );
			set_unit_value( weather, "i_wspd", "WindSpeed" );
			set_unit_value( weather, "i_wdir", "WindDir" );
			set_unit_value( weather, "i_rhum", "RelHum" );
			set_unit_value( weather, "i_pres", "AtmPres" );
			set_unit_value( weather, "i_snow", "SnowCover" );
			set_unit_value( weather, "i_albedo", "GroundAlbedo" );
			set_unit_value( weather, "i_poa", "POA" );
			set_unit_value( weather, "i_solazi", "Azimuth" );
			set_unit_value( weather, "i_solzen", "Zenith" );
			set_unit_value( weather, "i_lat", "Latitude" );
			set_unit_value( weather, "i_lon", "Longitude" );
			set_unit_value( weather, "i_shift", "Shift" );
		}
		else
		{
			//Set weatherreader parameters
			set_unit_value_ssc_string( weather, "file_name" );
			set_unit_value_ssc_double( weather, "track_mode" );    //, 0 ); SET TO 3 IN TRNSYS FILE, no user input !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			set_unit_value_ssc_double( weather, "tilt" );          //, 0 );
			set_unit_value_ssc_double( weather, "azimuth" );       //, 0 );
		}

		//Set Solar Field Parameters ===========================================
        set_unit_value_ssc_double(u1, "nSCA" ); // , 8);
        set_unit_value_ssc_double(u1, "nHCEt" ); // , 4);
        set_unit_value_ssc_double(u1, "nColt" ); // , 4);
        set_unit_value_ssc_double(u1, "nHCEVar" ); // , 4);
        set_unit_value_ssc_double(u1, "nLoops" ); // , 230);
        set_unit_value_ssc_double(u1, "eta_pump" ); // , 0.85);
        set_unit_value_ssc_double(u1, "HDR_rough" ); // , 4.57E-05);
        set_unit_value_ssc_double(u1, "theta_stow" ); // , 170);
        set_unit_value_ssc_double(u1, "theta_dep" ); // , 10);
        set_unit_value_ssc_double(u1, "Row_Distance" ); // , 15);
        set_unit_value_ssc_double(u1, "FieldConfig" ); // , 2);
        set_unit_value_ssc_double(u1, "T_startup" ); // , 300);
        set_unit_value_ssc_double(u1, "pb_rated_cap" ); // , 111);
        set_unit_value_ssc_double(u1, "m_dot_htfmin" ); // , 1);
        set_unit_value_ssc_double(u1, "m_dot_htfmax" ); // , 12);
        set_unit_value_ssc_double(u1, "T_loop_in_des" ); // , 293);
        set_unit_value_ssc_double(u1, "T_loop_out" ); // , 391);
        set_unit_value_ssc_double(u1, "Fluid" ); // , 21);
        set_unit_value_ssc_double(u1, "T_field_ini" ); // , 150);
        set_unit_value_ssc_double(u1, "T_fp" ); // , 150);
        set_unit_value_ssc_double(u1, "I_bn_des" ); // , 950);
        set_unit_value_ssc_double(u1, "V_hdr_max" ); // , 3);
        set_unit_value_ssc_double(u1, "V_hdr_min" ); // , 2);
        set_unit_value_ssc_double(u1, "Pipe_hl_coef" ); // , 0.45);
        set_unit_value_ssc_double(u1, "SCA_drives_elec" ); // , 125);
        set_unit_value_ssc_double(u1, "fthrok" ); // , 1);
        set_unit_value_ssc_double(u1, "fthrctrl" ); // , 2);
        set_unit_value_ssc_double(u1, "ColTilt" ); // , 0);
        set_unit_value_ssc_double(u1, "ColAz" ); // , 0);
        set_unit_value_ssc_double(u1, "accept_mode" ); // , 0);
        set_unit_value_ssc_double(u1, "accept_init" ); // , 0);
        set_unit_value_ssc_double(u1, "accept_loc" ); // , 1);
        set_unit_value_ssc_double(u1, "solar_mult" ); // , 2);
        set_unit_value_ssc_double(u1, "mc_bal_hot" ); // , 0.2);
        set_unit_value_ssc_double(u1, "mc_bal_cold" ); // , 0.2);
        set_unit_value_ssc_double(u1, "mc_bal_sca" ); // , 4.5);
        set_unit_value_ssc_array(u1, "OptCharType" ); // , [1,1,1,1]);
        set_unit_value_ssc_array(u1, "CollectorType" ); // , [1,1,1,1]);
        set_unit_value_ssc_array(u1, "W_aperture" ); // , [5,5,5,5]);
        set_unit_value_ssc_array(u1, "A_aperture" ); // , [470.3,470.3,470.3,470.3]);
        set_unit_value_ssc_array(u1, "IamF0" ); // , [1,1,1,1]);
        set_unit_value_ssc_array(u1, "IamF1" ); // , [0.0506,0.0506,0.0506,0.0506]);
        set_unit_value_ssc_array(u1, "IamF2" ); // , [-0.1763,-0.1763,-0.1763,-0.1763]);
        set_unit_value_ssc_array(u1, "reflectivity" ); // , [1,1,1,1]);
        set_unit_value_ssc_array(u1, "TrackingError" ); // , [0.994,0.994,0.994,0.994]);
        set_unit_value_ssc_array(u1, "GeomEffects" ); // , [0.98,0.98,0.98,0.98]);
        set_unit_value_ssc_array(u1, "Rho_mirror_clean" ); // , [0.935,0.935,0.935,0.935]);
        set_unit_value_ssc_array(u1, "Dirt_mirror" ); // , [0.95,0.95,0.95,0.95]);
        set_unit_value_ssc_array(u1, "Error" ); // , [0.99,0.99,0.99,0.99]);
        set_unit_value_ssc_array(u1, "Ave_Focal_Length" ); // , [1.8,1.8,1.8,1.8]);
        set_unit_value_ssc_array(u1, "L_SCA" ); // , [100,100,100,100]);
        set_unit_value_ssc_array(u1, "L_aperture" ); // , [8.33333,8.33333,8.33333,8.33333]);
        set_unit_value_ssc_array(u1, "ColperSCA" ); // , [12,12,12,12]);
        set_unit_value_ssc_array(u1, "Distance_SCA" ); // , [1,1,1,1]);
        set_unit_value_ssc_matrix(u1, "HCE_FieldFrac" ); // , [[0.985,0.01,0.005,0],[0.985,0.01,0.005,0],[0.985,0.01,0.005,0],[0.985,0.01,0.005,0]]);
        set_unit_value_ssc_matrix(u1, "D_2" ); // , [[0.066,0.066,0.066,0.066],[0.066,0.066,0.066,0.066],[0.066,0.066,0.066,0.066],[0.066,0.066,0.066,0.066]]);
        set_unit_value_ssc_matrix(u1, "D_3" ); // , [[0.07,0.07,0.07,0.07],[0.07,0.07,0.07,0.07],[0.07,0.07,0.07,0.07],[0.07,0.07,0.07,0.07]]);
        set_unit_value_ssc_matrix(u1, "D_4" ); // , [[0.115,0.115,0.115,0.115],[0.115,0.115,0.115,0.115],[0.115,0.115,0.115,0.115],[0.115,0.115,0.115,0.115]]);
        set_unit_value_ssc_matrix(u1, "D_5" ); // , [[0.12,0.12,0.12,0.12],[0.12,0.12,0.12,0.12],[0.12,0.12,0.12,0.12],[0.12,0.12,0.12,0.12]]);
        set_unit_value_ssc_matrix(u1, "D_p" ); // , [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]);
        set_unit_value_ssc_matrix(u1, "Flow_type" ); // , [[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1]]);
        set_unit_value_ssc_matrix(u1, "Rough" ); // , [[4.50E-05,4.50E-05,4.50E-05,4.50E-05],[4.50E-05,4.50E-05,4.50E-05,4.50E-05],[4.50E-05,4.50E-05,4.50E-05,4.50E-05],[4.50E-05,4.50E-05,4.50E-05,4.50E-05]]);
        set_unit_value_ssc_matrix(u1, "alpha_env" ); // , [[0.02,0.02,0,0],[0.02,0.02,0,0],[0.02,0.02,0,0],[0.02,0.02,0,0]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_11" ); // , [[100,150,200,250,300,350,400,450,500],[0.064,0.0665,0.07,0.0745,0.08,0.0865,0.094,0.1025,0.112]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_12" ); // , [[0],[0.65]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_13" ); // , [[0],[0.65]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_14" ); // , [[0],[0]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_21" ); // , [[100,150,200,250,300,350,400,450,500],[0.064,0.0665,0.07,0.0745,0.08,0.0865,0.094,0.1025,0.112]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_22" ); // , [[0],[0.65]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_23" ); // , [[0],[0.65]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_24" ); // , [[0],[0]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_31" ); // , [[100,150,200,250,300,350,400,450,500],[0.064,0.0665,0.07,0.0745,0.08,0.0865,0.094,0.1025,0.112]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_32" ); // , [[0],[0.65]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_33" ); // , [[0],[0.65]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_34" ); // , [[0],[0]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_41" ); // , [[100,150,200,250,300,350,400,450,500],[0.064,0.0665,0.07,0.0745,0.08,0.0865,0.094,0.1025,0.112]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_42" ); // , [[0],[0.65]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_43" ); // , [[0],[0.65]]);
        set_unit_value_ssc_matrix(u1, "epsilon_3_44" ); // , [[0],[0]]);
        set_unit_value_ssc_matrix(u1, "alpha_abs" ); // , [[0.96,0.96,0.8,0],[0.96,0.96,0.8,0],[0.96,0.96,0.8,0],[0.96,0.96,0.8,0]]);
        set_unit_value_ssc_matrix(u1, "Tau_envelope" ); // , [[0.963,0.963,1,0],[0.963,0.963,1,0],[0.963,0.963,1,0],[0.963,0.963,1,0]]);
        set_unit_value_ssc_matrix(u1, "EPSILON_4" ); // , [[0.86,0.86,1,0],[0.86,0.86,1,0],[0.86,0.86,1,0],[0.86,0.86,1,0]]);
        set_unit_value_ssc_matrix(u1, "EPSILON_5" ); // , [[0.86,0.86,1,0],[0.86,0.86,1,0],[0.86,0.86,1,0],[0.86,0.86,1,0]]);
        set_unit_value_ssc_matrix(u1, "GlazingIntactIn" ); // , [[1,1,0,1],[1,1,0,1],[1,1,0,1],[1,1,0,1]]);
        set_unit_value_ssc_matrix(u1, "P_a" ); // , [[0.0001,750,750,0],[0.0001,750,750,0],[0.0001,750,750,0],[0.0001,750,750,0]]);
        set_unit_value_ssc_matrix(u1, "AnnulusGas" ); // , [[27,1,1,27],[27,1,1,27],[27,1,1,27],[27,1,1,27]]);
        set_unit_value_ssc_matrix(u1, "AbsorberMaterial" ); // , [[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1]]);
        set_unit_value_ssc_matrix(u1, "Shadowing" ); // , [[0.96,0.96,0.96,0.963],[0.96,0.96,0.96,0.963],[0.96,0.96,0.96,0.963],[0.96,0.96,0.96,0.963]]);
        set_unit_value_ssc_matrix(u1, "Dirt_HCE" ); // , [[0.98,0.98,1,0.98],[0.98,0.98,1,0.98],[0.98,0.98,1,0.98],[0.98,0.98,1,0.98]]);
        set_unit_value_ssc_matrix(u1, "Design_loss" ); // , [[150,1100,1500,0],[150,1100,1500,0],[150,1100,1500,0],[150,1100,1500,0]]);
        set_unit_value_ssc_matrix(u1, "SCAInfoArray" ); // , [[1,1],[1,1],[1,1],[1,1],[1,1],[1,1],[1,1],[1,1]]);
        set_unit_value_ssc_array(u1, "SCADefocusArray" ); // , [8,7,6,5,4,3,2,1]);
		//Set the initial values
        set_unit_value_ssc_double(u1, "I_b" ); // , 0.);
        set_unit_value_ssc_double(u1, "T_db" ); // , 15.);
        set_unit_value_ssc_double(u1, "V_wind" ); // , 1.5);
        set_unit_value_ssc_double(u1, "P_amb" ); // , 1.);
        set_unit_value_ssc_double(u1, "T_dp" ); // , 10.);
        set_unit_value_ssc_double(u1, "SolarAz" ); // , 0.);
        set_unit_value_ssc_double(u1, "defocus" ); // , 1.);
        set_unit_value_ssc_double(u1, "T_cold_in" ); // , 293.);
		//Connect Solar Field Inputs
		bool bConnected = connect(weather, "beam", u1, "I_b", 0);
		bConnected &= connect(weather, "tdry", u1, "T_db", 0);
		bConnected &= connect(weather, "wspd", u1, "V_wind", 0);
		bConnected &= connect(weather, "pres", u1, "P_amb", 0);
		bConnected &= connect(weather, "tdew", u1, "T_dp", 0);
		bConnected &= connect(weather, "solazi", u1, "SolarAz", 0);
		bConnected &= connect(weather, "lat", u1, "latitude", 0);
		bConnected &= connect(weather, "lon", u1, "longitude", 0);
		bConnected &= connect(weather, "shift", u1, "shift", 0);
		bConnected &= connect(u2, "defocus", u1, "defocus" );
		bConnected &= connect(u2, "T_field_in", u1, "T_cold_in" );

		//Set controller parameters ===========================================
		set_unit_value_ssc_double(u2, "field_fluid" ); // , 21);
		set_unit_value_ssc_array(u2, "field_fl_props" ); // , [0]);
		set_unit_value_ssc_double(u2, "store_fluid" ); // , 18);
		set_unit_value_ssc_double(u2, "tshours" ); // , 6);
		set_unit_value_ssc_double(u2, "is_hx" ); // , 1);
		set_unit_value_ssc_double(u2, "dt_hot" ); // , 5);
		set_unit_value_ssc_double(u2, "dt_cold" ); // , 7);
		set_unit_value_ssc_double(u2, "hx_config" ); // , 2);
		set_unit_value_ssc_double(u2, "q_max_aux" ); // , 294.118);
		set_unit_value_ssc_double(u2, "T_set_aux" ); // , 391);
		set_unit_value_ssc_double(u2, "V_tank_hot_ini" ); // , 1313.43);
		set_unit_value_ssc_double(u2, "T_tank_hot_ini" ); // , 300);
		set_unit_value_ssc_double(u2, "T_tank_cold_ini" ); // , 300);
		set_unit_value_ssc_double(u2, "vol_tank" ); // , 26268.7);
		set_unit_value_ssc_double(u2, "h_tank" ); // , 20);
		set_unit_value_ssc_double(u2, "h_tank_min" ); // , 1);
		set_unit_value_ssc_double(u2, "u_tank" ); // , 0.4);
		set_unit_value_ssc_double(u2, "tank_pairs" ); // , 1);
		set_unit_value_ssc_double(u2, "cold_tank_Thtr" ); // , 250);
		set_unit_value_ssc_double(u2, "hot_tank_Thtr" ); // , 365);
		set_unit_value_ssc_double(u2, "tank_max_heat" ); // , 25);
		set_unit_value_ssc_double(u2, "T_field_in_des" ); // , 293);
		set_unit_value_ssc_double(u2, "T_field_out_des" ); // , 391);
		set_unit_value_ssc_double(u2, "q_pb_design" ); // , 294.118);
		set_unit_value_ssc_double(u2, "W_pb_design" ); // , 111);
		set_unit_value_ssc_double(u2, "cycle_max_frac" ); // , 1.05);
		set_unit_value_ssc_double(u2, "cycle_cutoff_frac" ); // , 0.25);
		set_unit_value_ssc_double(u2, "solarm" ); // , 2);
		set_unit_value_ssc_double(u2, "pb_pump_coef" ); // , 0.55);
		set_unit_value_ssc_double(u2, "tes_pump_coef" ); // , 0.15);
		set_unit_value_ssc_double(u2, "pb_fixed_par" ); // , 0.0055);
		set_unit_value_ssc_array(u2, "bop_array" ); // , [0,1,0.483,0.517,0]);
		set_unit_value_ssc_array(u2, "aux_array" ); // , [0.02273,1,0.483,0.517,0]);
		set_unit_value_ssc_double(u2, "T_startup" ); // , 300);
		set_unit_value_ssc_double(u2, "fossil_mode" ); // , 1);
		set_unit_value_ssc_double(u2, "fthr_ok" ); // , 1);
		set_unit_value_ssc_double(u2, "nSCA" ); // , 8);
		set_unit_value_ssc_double(u2, "I_bn_des" ); // , 950);
		set_unit_value_ssc_double(u2, "fc_on" ); // , 0);
		set_unit_value_ssc_double(u2, "q_sby_frac" ); // , 0.2);
		set_unit_value_ssc_double(u2, "t_standby_reset" ); // , 2);
		set_unit_value_ssc_double(u2, "tes_type" ); // , 1);
		set_unit_value_ssc_array(u2, "tslogic_a" ); // , [0,0,0,0,0,0,0,0,0]);
		set_unit_value_ssc_array(u2, "tslogic_b" ); // , [0,0,0,0,0,0,0,0,0]);
		set_unit_value_ssc_array(u2, "tslogic_c" ); // , [1.05,1,1,1,1,1,1,1,1]);
		set_unit_value_ssc_array(u2, "ffrac" ); // , [0,0,0,0,0,0,0,0,0]);
		set_unit_value_ssc_double(u2, "tc_fill" ); // , 7);
		set_unit_value_ssc_double(u2, "tc_void" ); // , 0.25);
		set_unit_value_ssc_double(u2, "t_dis_out_min" ); // , 500);
		set_unit_value_ssc_double(u2, "t_ch_out_max" ); // , 500);
		set_unit_value_ssc_double(u2, "nodes" ); // , 2000);
		set_unit_value_ssc_double(u2, "f_tc_cold" ); // , 2);
		//Connections to controller
		bConnected &= connect(weather, "beam", u2, "I_bn", 0);
		bConnected &= connect(weather, "tdry", u2, "T_amb", 0);
		bConnected &= connect(u1, "m_dot_avail", u2, "m_dot_field");
		bConnected &= connect(u3, "m_dot_htf_ref", u2, "m_dot_htf_ref");
		bConnected &= connect(u1, "T_sys_h", u2, "T_field_out");
		bConnected &= connect(u3, "T_htf_cold", u2, "T_pb_out");
		bConnected &= connect(u3, "m_dot_demand", u2, "m_pb_demand");
		bConnected &= connect(u1, "E_bal_startup", u2, "q_startup");
		//Set controller initial values
		set_unit_value_ssc_double(u2, "I_bn" );           // , 0.);
		set_unit_value_ssc_double(u2, "T_amb" );          // , 15.);
		set_unit_value_ssc_double(u2, "m_dot_field" );    // , 0.);
		set_unit_value_ssc_double(u2, "m_dot_htf_ref" );  // , 0.);
		set_unit_value_ssc_double(u2, "T_field_out" );    // , 391.);
		// "T_pb_out" is an output, so had to change the name of the SSC var used to initialize it
		set_unit_value( u2, "T_pb_out", as_double("T_pb_out_init") ); // , 293.);
		set_unit_value_ssc_double(u2, "m_pb_demand" );    // , 100000.);
		set_unit_value_ssc_double(u2, "q_startup" );      // , 0.);

		//Set Powerblock Parameters ===========================================
		set_unit_value_ssc_double(u3, "P_ref" ); // , 111);
		set_unit_value_ssc_double(u3, "eta_ref" ); // , 0.3774);
		set_unit_value_ssc_double(u3, "T_htf_hot_ref" ); // , 391);
		set_unit_value_ssc_double(u3, "T_htf_cold_ref" ); // , 293);
		set_unit_value_ssc_double(u3, "dT_cw_ref" ); // , 10);
		set_unit_value_ssc_double(u3, "T_amb_des" ); // , 20);
		set_unit_value_ssc_double(u3, "HTF" ); // , 21);
		set_unit_value_ssc_double(u3, "q_sby_frac" ); // , 0.2);
		set_unit_value_ssc_double(u3, "P_boil" ); // , 100);
		set_unit_value_ssc_double(u3, "CT" ); // , 1);
		set_unit_value_ssc_double(u3, "startup_time" ); // , 0.5);
		set_unit_value_ssc_double(u3, "startup_frac" ); // , 0.2);
		set_unit_value_ssc_double(u3, "tech_type" ); // , 2);
		set_unit_value_ssc_double(u3, "T_approach" ); // , 5);
		set_unit_value_ssc_double(u3, "T_ITD_des" ); // , 16);
		set_unit_value_ssc_double(u3, "P_cond_ratio" ); // , 1.0028);
		set_unit_value_ssc_double(u3, "pb_bd_frac" ); // , 0.02);
		set_unit_value_ssc_double(u3, "P_cond_min" ); // , 1.25);
		set_unit_value_ssc_double(u3, "n_pl_inc" ); // , 2);
		set_unit_value_ssc_array(u3, "F_wc" ); // , [0,0,0,0,0,0,0,0,0]);
		set_unit_value_ssc_double(u3, "mode" ); // , 2);	//Always set to 2 for type 251
		//Connect inputs
		bConnected &= connect(weather, "twet", u3, "T_wb", 0);
		bConnected &= connect(weather, "tdry", u3, "T_db", 0);
		bConnected &= connect(weather, "pres", u3, "P_amb", 0);
		bConnected &= connect(weather, "rhum", u3, "rh", 0);
		bConnected &= connect(u2, "T_pb_in", u3, "T_htf_hot");
		bConnected &= connect(u2, "m_dot_pb", u3, "m_dot_htf");
		bConnected &= connect(u2, "m_dot_pb", u3, "demand_var");
		bConnected &= connect(u2, "standby_control", u3, "standby_control");
		bConnected &= connect(u2, "TOU", u3, "TOU");
		//Set initial values
		set_unit_value_ssc_double(u3, "T_wb" ); // , 10.);
		set_unit_value_ssc_double(u3, "T_db" ); // , 15.);
		set_unit_value_ssc_double(u3, "P_amb" ); // , 1.);
		set_unit_value_ssc_double(u3, "rh" ); // , 0.25);
		set_unit_value_ssc_double(u3, "T_htf_hot" ); // , 391.0);
		// "m_dot_htf" is an output, so had to change the name of the SSC var used to initialize it
		set_unit_value( u3, "m_dot_htf", as_double("m_dot_htf_init") );// , 0.);
		set_unit_value_ssc_double(u3, "demand_var" ); // , 110.);
		set_unit_value_ssc_double(u3, "standby_control" ); // , 0);
		set_unit_value_ssc_double(u3, "TOU" ); // , 1);

		//Set enet calculator inputs and connect it to the parasitic values ===========================================
		set_unit_value_ssc_double(u4, "eta_lhv" ); // , 0.9);
		set_unit_value_ssc_double(u4, "eta_tes_htr" ); // , 0.98);
		set_unit_value_ssc_double(u4, "fp_mode" ); // , 1);
		bConnected &= connect(u3, "P_cycle", u4, "W_cycle_gross");
		bConnected &= connect(u3, "W_cool_par", u4, "W_par_heatrej");
		bConnected &= connect(u1, "W_dot_pump", u4, "W_par_sf_pump");
		bConnected &= connect(u2, "htf_pump_power", u4, "W_par_tes_pump");
		bConnected &= connect(u2, "bop_par", u4, "W_par_BOP");
		bConnected &= connect(u2, "fixed_par", u4, "W_par_fixed");
		bConnected &= connect(u1, "SCA_par_tot", u4, "W_par_tracking");
		bConnected &= connect(u2, "aux_par", u4, "W_par_aux_boiler");
		bConnected &= connect(u2, "tank_fp_par", u4, "Q_par_tes_fp");
		bConnected &= connect(u1, "E_fp_tot", u4, "Q_par_sf_fp");
		bConnected &= connect(u2, "q_aux_heat", u4, "Q_aux_backup");

		// check if all connections worked
		if ( !bConnected )
			throw exec_error( "tcstrough_physical", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );

		// Run simulation
		size_t hours = 8760;
		if (0 > simulate(3600, hours*3600, 3600) )
			throw exec_error( "tcstrough_physical", util::format("there was a problem simulating in tcstrough_physical.") );

		// get the outputs
		if (!set_all_output_arrays() )
			throw exec_error( "tcstrough_physical", util::format("there was a problem returning the results from the simulation.") );


		//set_output_array("i_SfTi",8760);
	}

};

DEFINE_TCS_MODULE_ENTRY( tcstrough_physical, "CSP model using the emperical trough TCS types.", 4 )
