#include "core.h"
#include "tckernel.h"

static var_info _cm_vtab_tcsmolten_salt[] = {
/*	EXAMPLE LINES FOR INPUTS
    { SSC_INPUT,        SSC_NUMBER,      "XXXXXXXXXXXXXX",    "Label",                                                            "",             "",            "sca",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "INTINTINTINT",      "Label",                                                            "",             "",            "parasitic",      "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "XXXXXXXXXXX",       "Number indicating the receiver type",                              "",             "",            "hce",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "XXXXXXXXXXX",       "Label",                                                            "",             "",            "tes",            "*",                       "",                      "" },
*/

//    VARTYPE           DATATYPE          NAME                 LABEL                                                              UNITS           META            GROUP            REQUIRED_IF                 CONSTRAINTS             UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "file_name",         "local weather file path",                                          "",             "",            "Weather",        "*",                       "LOCAL_FILE",            "" },
    { SSC_INPUT,        SSC_NUMBER,      "track_mode",        "Tracking mode",                                                    "",             "",            "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tilt",              "Tilt angle of surface/axis",                                       "",             "",            "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "azimuth",           "Azimuth angle of surface/axis",                                    "",             "",            "Weather",        "*",                       "",                      "" },


	// Heliostat field (type 221) parameters
    { SSC_INPUT,        SSC_MATRIX,      "eta_map",           "Field efficiency matrix",                                          "-",            "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_zen",             "Number of zenith angle data points in file",                       "-",            "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_azi",             "Number of azimuth angle data points in file",                      "-",            "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_hel",             "Number of heliostats in the field",                                "-",            "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_start",           "Electric work for starting up one heliostat",                      "kWe-hr",       "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "p_run",             "Electric power for tracking one heliostat",                        "kWe",          "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "v_wind_max",        "Maximum tolerable wind speed",                                     "m/s",          "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hel_stow_deploy",   "Heliostat field stow/deploy solar elevation angle",                "deg",          "",            "heliostat",      "*",                       "",                      "" },
	// Heliostat field (type 221) inputs
    { SSC_INPUT,        SSC_NUMBER,      "field_control",     "Field defocus control",                                            "",             "",            "heliostat",      "*",                       "",                      "" },


	// Receiver (type 222) parameters
    { SSC_INPUT,        SSC_NUMBER,      "N_panels",          "Number of individual panels on the receiver",                      "",             "",            "receiver",       "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "D_rec",             "The overall outer diameter of the receiver",                       "m",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "H_rec",             "The height of the receiver",                                       "m",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "THT",               "The height of the tower (hel. pivot to rec equator)",              "m",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_tube_out",        "The outer diameter of an individual receiver tube",                "mm",           "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "th_tube",           "The wall thickness of a single receiver tube",                     "mm",           "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mat_tube",          "The material name of the receiver tubes",                          "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_htf",           "The name of the HTF used in the receiver",                         "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "field_fl_props",    "User defined field fluid property data",                           "-",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Flow_type",         "A flag indicating which flow pattern is used",                     "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "epsilon",           "The emissivity of the receiver surface coating",                   "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hl_ffact",          "The heat loss factor (thermal loss fudge factor)",                 "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_hot_des",     "Hot HTF outlet temperature at design conditions",                  "C",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_cold_des",    "Cold HTF inlet temperature at design conditions",                  "C",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_rec_min",         "Minimum receiver mass flow rate turn down fraction",               "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Q_rec_des",         "Design-point receiver thermal power output",                       "MWt",          "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_su_delay",      "Fixed startup delay time for the receiver",                        "hr",           "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_qf_delay",      "Energy-based rcvr startup delay (fraction of rated thermal power)","",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htf_max",     "Maximum receiver mass flow rate",                                  "kg/hr",        "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "A_sf",              "Solar Field Area",                                                 "m^2",          "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "fluxmap_angles",    "Matrix containing zenith and azimuth angles for flux maps",        "-",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "fluxmap",           "Matrix containing flux map for various solar positions",           "-",            "",            "receiver",       "*",                       "",                      "" },

    // Receiver (type 222) inputs
    { SSC_INPUT,        SSC_NUMBER,      "azimuth_ini",       "Solar azimuth angle",                                              "deg",          "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "zenith_ini",        "Solar zenith angle",                                               "deg",          "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_salt_hot_target", "Desired HTF outlet temperature",                                   "C",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_salt_cold",       "Desired HTF inlet temperature",                                    "C",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_wind_10",         "Ambient wind velocity, ground level",                              "m/s",          "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_amb",             "Ambient atmospheric pressure",                                     "mbar",         "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_pump",          "Receiver HTF pump efficiency",                                     "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_dp",              "Ambient dew point temperature",                                    "C",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_bn",              "Direct (beam) normal radiation",                                   "W/m^2-K",      "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "field_eff",         "Heliostat field efficiency",                                       "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_db",              "Ambient dry bulb temperature",                                     "C",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "night_recirc",      "Flag to indicate night recirculation through the rec.",            "",             "",            "receiver",       "*",                       "INTEGER",               "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "hel_stow_deploy",   "Heliostat field stow/deploy solar angle",                          "deg",          "",            "receiver",       "*",                       "",                      "" },

    // Controller (type 251) parameters
    { SSC_INPUT,        SSC_NUMBER,      "field_fluid",       "Material number for the collector field",                          "-",            "",            "controller",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_MATRIX,      "field_fl_props",    "User defined field fluid property data",                           "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "store_fluid",       "Material number for storage fluid",                                "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "user_fluid",        "User defined fluid property data",                                 "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tshours",           "Equivalent full-load thermal storage hours",                       "hr",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_hx",             "1=yes, 0=no"                                                       "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dt_hot",            "Hot side HX approach temp",                                        "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dt_cold",           "Cold side HX approach temp",                                       "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hx_config",         "HX configuration",                                                 "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_max_aux",         "Max heat rate of auxiliary heater",                                "MWt",          "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_set_aux",         "Aux heater outlet temp set point",                                 "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_tank_hot_ini",    "Initial hot tank fluid volume",                                    "m3",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_hot_ini",    "Initial hot tank fluid temperature",                               "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_cold_ini",   "Initial cold tank fluid tmeperature",                              "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "vol_tank",          "Total tank volume, including unusable HTF at bottom",              "m3",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank",            "Total height of tank (height of HTF when tank is full",            "m",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank_min",        "Minimum allowable HTF height in storage tank",                     "m",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "u_tank",            "Loss coefficient from the tank",                                   "W/m2-K",       "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_pairs",        "Number of equivalent tank pairs",                                  "-",            "",            "controller",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_Thtr",    "Minimum allowable cold tank HTF temp",                             "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_Thtr",     "Minimum allowable hot tank HTF temp",                              "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_max_heat",     "Rated heater capacity for tank heating",                           "MW",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_field_in_des",    "Field design inlet temperature",                                   "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_field_out_des",   "Field design outlet temperature",                                  "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_pb_design",       "Design heat input to power block",                                 "MWt",          "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "W_pb_design",       "Rated plant capacity",                                             "MWe",          "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_max_frac",    "Maximum turbine over design operation fraction",                   "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_cutoff_frac", "Minimum turbine operation fraction before shutdown",               "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "solarm",            "Solar Multiple",                                                   "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_pump_coef",      "Pumping power to move 1kg of HTF through PB loop",                 "kW/kg",        "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_pump_coef",     "Pumping power to move 1kg of HTF through tes loop",                "kW/kg",        "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_fixed_par_cntl", "Fraction of rated gross power constantly consumed by controller",  "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "bop_array",         "Coefficients for balance of plant parasitics calcs",               "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "aux_array",         "Coefficients for auxiliary heater parasitics calcs",               "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_startup",         "Startup temperature",                                              "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fossil_mode",       "Fossil backup mode 1=Normal 2=Topping",                            "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fthr_ok",           "Does the defocus control allow partial defocusing",                "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nSCA",              "Number of SCAs in a single loop",                                  "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_bn_des",          "Design point irradiation value",                                   "W/m2",         "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fc_on",             "DNI forecasting enabled",                                          "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",        "Fraction of thermal power required for standby",                   "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_standby_reset",   "Maximum allowable time for PB standby operation",                  "hr",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "sf_type",           "Solar field type, 1 = trough, 2 = tower",                          "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_type",          "1=2-tank, 2=thermocline",                                          "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_a",         "Dispatch logic without solar",                                     "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_b",         "Dispatch logic with solar",                                        "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_c",         "Dispatch logic for turbine load fraction",                         "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "ffrac",             "Fossil dispatch logic",                                            "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tc_fill",           "Thermocline fill material",                                        "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tc_void",           "Thermocline void fraction",                                        "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_dis_out_min",     "Min allowable hot side outlet temp during discharge",              "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_ch_out_max",      "Max allowable cold side outlet temp during charge",                "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nodes",             "Nodes modeled in the flow path",                                   "-",            "",            "controller",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_tc_cold",         "0=entire tank is hot, 1=entire tank is cold",                      "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "TOU_schedule",      "Annual hourly time-of-use schedule",                               "-",            "",            "controller",     "*",                       "",                      "" },

    // Controller (type 251) inputs
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htf_ref",     "Reference HTF flow rate at design conditions",                     "kg/hr",        "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_pb_out",          "Fluid temperature from the power block",                           "C",            "",            "controller",     "*",                       "",                      "" },


    // Powerblock (type 224) parameters
    { SSC_INPUT,        SSC_NUMBER,      "P_ref",             "Reference output electric power at design condition",              "MW",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_ref",           "Reference conversion efficiency at design condition",              "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_hot_ref",     "Reference HTF inlet temperature at design",                        "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_cold_ref",    "Reference HTF outlet temperature at design",                       "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",         "Reference condenser cooling water inlet/outlet T diff",            "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",         "Reference ambient temperature at design point",                    "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "HTF",               "Integer flag identifying HTF in power block",                      "none",         "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",        "Fraction of thermal power required for standby mode",              "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil",            "Boiler operating pressure",                                        "bar",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                "Flag for using dry cooling or wet cooling system",                 "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_time",      "Time needed for power block startup",                              "hr",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",      "Fraction of design thermal power needed for startup",              "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tech_type",         "Flag indicating which coef. set to use. (1=tower,2=trough,3=user)","none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",        "Cooling tower approach temperature",                               "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",         "ITD at design for dry system",                                     "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",      "Condenser pressure ratio",                                         "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",        "Power block blowdown steam fraction ",                             "none",         "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_STRING,      "pb_input_file",     "Power block coefficient file name",                                "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",        "Minimum condenser pressure",                                       "inHg",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",          "Number of part-load increments for the heat rejection system",     "none",         "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",              "Fraction indicating wet cooling use for hybrid system",            "none",         "",            "powerblock",     "*",                       "",                      "" },

    // Powerblock (type 224) inputs
    { SSC_INPUT,        SSC_NUMBER,      "mode",              "Cycle part load control, from plant controller",                   "none",         "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_htf_hot",         "Hot HTF inlet temperature, from storage tank",                     "C",            "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "m_dot_htf",         "HTF mass flow rate",                                               "kg/hr",        "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_wb",              "Ambient wet bulb temperature",                                     "C",            "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "demand_var",        "Control signal indicating operational mode",                       "none",         "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "standby_control",   "Control signal indicating standby mode",                           "none",         "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_db",              "Ambient dry bulb temperature",                                     "C",            "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "P_amb",             "Ambient pressure",                                                 "atm",          "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "TOU",               "Current Time-of-use period",                                       "none",         "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "rh",                "Relative humidity of the ambient air",                             "none",         "",            "powerblock",     "*",                       "",                      "" },

	// Parasitics (type 228) parameters
    {SSC_INPUT,         SSC_NUMBER,      "P_storage_pump",    "Storage pump power, rated per MWt of storage use",                 "MWe/MWt",      "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "Piping_loss",       "Thermal loss per meter of piping",                                 "Wt/m",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "Piping_length",     "Total length of exposed piping",                                   "m",            "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "Design_power",      "Power production at design conditions",                            "MWe",          "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "recirc_htr_eff",    "Recirculation heater efficiency",                                  "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "design_eff",        "Power cycle efficiency at design",                                 "none",         "",            "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "night_recirc",      "Flag indicating whether night recirculation is allowed",           "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "pb_fixed_par",      "Fixed parasitic load - runs at all times",                         "MWe/MWcap",    "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par",           "Aux heater, boiler parasitic",                                     "MWe/MWcap",    "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_f",         "Aux heater, boiler parasitic - multiplying fraction",              "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_0",         "Aux heater, boiler parasitic - constant coefficient",              "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_1",         "Aux heater, boiler parasitic - linear coefficient",                "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_2",         "Aux heater, boiler parasitic - quadratic coefficient",             "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par",           "Balance of plant parasitic power fraction",                        "MWe/MWcap",    "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_f",         "Balance of plant parasitic power fraction - mult frac",            "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_0",         "Balance of plant parasitic power fraction - const coeff",          "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_1",         "Balance of plant parasitic power fraction - linear coeff",         "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_2",         "Balance of plant parasitic power fraction - quadratic coeff",      "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "storage_bypass",    "Flag indicating whether the hot salt pump always runs w/ PB",      "none",         "",            "parasitics",     "*",                       "",                      "" },
    // Parasitics (type 228) inputs
    {SSC_INPUT,         SSC_NUMBER,      "flow_from_storage", "Flow rate from storage",                                           "kg/hr",       "",             "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "P_cooling_tower", "Cooling tower parasitic power fraction",                            "MWe",         "",             "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "P_tower_pump",    "Reported tower pump power",                                         "MWe",         "",             "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "P_helio_track",   "Reported heliostat tracking power",                                 "MWe",         "",             "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "P_plant_output",  "Reported plant power output",                                       "MWe",         "",             "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "eta_cycle",       "Power cycle efficiency",                                            "none",        "",             "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "P_cold_tank",       "Cold tank heater parasitic power",                                 "MWe",         "",             "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "P_hot_tank",        "Hot tank heater parasitic power",                                  "MWe",         "",             "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "P_tower_conv",      "Reported tower convection loss",                                   "MWt",         "",             "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "P_tower_rad",       "Reported tower radiation loss",                                    "MWt",         "",             "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "recirc_source",     "Recirculation heater control",                                     "none",        "",             "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "ref_htf_flow",      "HTF flow rate through the power cycle at design",                  "kg/hr",       "",             "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "aux_power",         "Auxiliary heater thermal power output",                            "MWt",         "",             "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "P_htf_pump",        "HTF pumping power",                                                "MWe",         "",             "parasitics",     "*",                       "",                      "" },



    // OUTPUTS
	// The names of the output variables should match the parameter names for the TCS units in order to signal to the TCS kernel to store the values by timestep

	// VARTYPE          DATATYPE          NAME                 LABEL                                                                UNITS           META            GROUP            REQUIRED_IF                 CONSTRAINTS             UI_HINTS
    { SSC_OUTPUT,       SSC_ARRAY,       "P_out_net",        "Cycle power output",                                                "MWh",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },

    var_info_invalid };

class cm_tcsmolten_salt : public tcKernel
{
public:

	cm_tcsmolten_salt(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info( _cm_vtab_tcsmolten_salt );
		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above
	}

	void exec( ) throw( general_error )
	{
		bool debug_mode = true; (__DEBUG__ == 1);  // When compiled in VS debug mode, this will use the trnsys weather file; otherwise, it will attempt to open the file with name that was passed in
		//Add weather file reader unit
		int weather = 0;
		if(debug_mode) weather = add_unit("trnsys_weatherreader", "TRNSYS weather reader");
		else weather = add_unit("weatherreader", "TCS weather reader");

		// Add units
		int hel_field = add_unit("sam_mw_pt_type221");
		int receiver = add_unit("sam_mw_pt_type222");
		int controller = add_unit("sam_mw_trough_type251");
		int powerblock = add_unit("sam_mw_pt_type224");
		int parasitics = add_unit("sam_mw_pt_type228");

		double avg_temp=0, avg_wind_v=0;

		if(debug_mode)
		{
			set_unit_value( weather, "file_name", "C:/svn_NREL/main/ssc/tcs/typelib/TRNSYS_weather_outputs/daggett_trnsys_weather.out" );
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

			avg_temp = 10.3;
			avg_wind_v = 0;
		}
		else
		{
			//Set weatherreader parameters
			set_unit_value_ssc_string( weather, "file_name" );
			set_unit_value_ssc_double( weather, "track_mode" );
			set_unit_value_ssc_double( weather, "tilt" );
			set_unit_value_ssc_double( weather, "azimuth" );

			avg_temp = as_double("T_db");
			avg_wind_v = as_double("V_wind_10");
		}

		// Heliostat field (type 221) parameters
		set_unit_value_ssc_matrix( hel_field, "eta_map" );
		set_unit_value_ssc_double( hel_field, "n_zen" ); // 8
		set_unit_value_ssc_double( hel_field, "n_azi" ); // 13
		set_unit_value_ssc_double( hel_field, "n_hel" ); // 8929
		set_unit_value_ssc_double( hel_field, "q_start" );
		set_unit_value_ssc_double( hel_field, "p_run" );
		set_unit_value_ssc_double( hel_field, "v_wind_max" );
		set_unit_value_ssc_double( hel_field, "hel_stow_deploy" );
        set_unit_value_ssc_double( hel_field, "field_control" );
		// Set initial value for input from controller
		set_unit_value_ssc_double( controller, "defocus", as_double("field_control") );

		// Heliostat field (type 221) inputs
		bool bConnected = connect( weather, "wspd", hel_field, "vwind" );
		bConnected = connect( weather, "solzen", hel_field, "theta" );
		bConnected = connect( weather, "solazi", hel_field, "phi" );
		bConnected = connect( controller, "defocus", hel_field, "field_control" );

		// Receiver (type 222) parameters
		set_unit_value_ssc_double( receiver, "N_panels" );//, 20 );
		set_unit_value_ssc_double( receiver, "D_rec" );//, 17.67 );
		set_unit_value_ssc_double( receiver, "H_rec" );//, 20.41 );
		set_unit_value_ssc_double( receiver, "THT" );//, 203.33 );
		set_unit_value_ssc_double( receiver, "d_tube_out" );//, 40.0 );
		set_unit_value_ssc_double( receiver, "th_tube" );//, 1.25 );
		set_unit_value_ssc_double( receiver, "mat_tube" );//, 2 );
		set_unit_value_ssc_double( receiver, "rec_htf" );//, 17 );
		//set_unit_value_ssc_matrix( receiver, "field_fl_props" );//, {} );
		set_unit_value_ssc_double( receiver, "Flow_type" );//, 1 );
		set_unit_value_ssc_double( receiver, "epsilon" );//, 0.88 );
		set_unit_value_ssc_double( receiver, "hl_ffact" );//, 1 );
		set_unit_value_ssc_double( receiver, "T_htf_hot_des" );//, 574 );
		set_unit_value_ssc_double( receiver, "T_htf_cold_des" );//, 290 );
		set_unit_value_ssc_double( receiver, "f_rec_min" );//, 0.25 );
		set_unit_value_ssc_double( receiver, "Q_rec_des" );//, 669.903 );
		set_unit_value_ssc_double( receiver, "rec_su_delay" );//, 0.2 );
		set_unit_value_ssc_double( receiver, "rec_qf_delay" );//, 0.25 );
		set_unit_value_ssc_double( receiver, "m_dot_htf_max" );//, 6.764E6 );
		set_unit_value_ssc_double( receiver, "A_sf" );
		set_unit_value_ssc_matrix( receiver, "fluxmap_angles" );
		set_unit_value_ssc_matrix( receiver, "fluxmap" );

		// Set Receiver (type 222) inputs (initial values)
		set_unit_value_ssc_double( receiver, "azimuth", as_double("azimuth_ini") ); //, 174.309 );
		set_unit_value_ssc_double( receiver, "zenith", as_double("zenith_ini") ); //, 58.0268 );
		// reset below, this line doesn't do anything:  set_unit_value_ssc_double( receiver, "T_salt_hot_target" ); //, 574 ); 
		// reset below, this line doesn't do anything:  set_unit_value_ssc_double( receiver, "T_salt_cold" ); //, 289.874 );
		set_unit_value_ssc_double( receiver, "V_wind_10", avg_wind_v ); //, 0.0 );
		set_unit_value_ssc_double( receiver, "P_amb" ); //, 956.0 );
		// reset below, this line doesn't do anything:  set_unit_value_ssc_double( receiver, "eta_pump" ); //, 0.85 );
		set_unit_value_ssc_double( receiver, "T_dp" ); //, -5.65 );
		set_unit_value_ssc_double( receiver, "I_bn" ); //, 941.0 );
		// reset below, this line doesn't do anything.  set_unit_value_ssc_double( receiver, "field_eff" ); //, 0.535 );
		set_unit_value_ssc_double( receiver, "T_db", avg_temp ); //, 10.3 );
		// reset below, this line doesn't do anything.  set_unit_value_ssc_double( receiver, "night_recirc" ); //, 0 );
		// reset below, this line doesn't do anything.  set_unit_value_ssc_double( receiver, "hel_stow_deploy" ); //, 8 );

		set_unit_value_ssc_double( receiver, "T_salt_hot_target" ); //, 574.0 );
		set_unit_value_ssc_double( receiver, "eta_pump" ); //, 0.85 );
		set_unit_value_ssc_double( receiver, "night_recirc" ); //, 0 );
		set_unit_value_ssc_double( receiver, "hel_stow_deploy" ); //, 8 ); 
		// Set initial values for inputs generated from subsequently called types
		set_unit_value_ssc_double( receiver, "T_salt_cold" ); //, 290.0 );
		set_unit_value_ssc_double( receiver, "field_eff" ); //, 0.0 );

		// Connect the Receiver (type 222) inputs to other types
		bConnected = connect(weather, "solazi", receiver, "azimuth");
		bConnected = connect(weather, "solzen", receiver, "zenith");
		bConnected = connect(controller, "T_field_in", receiver, "T_salt_cold");
		bConnected = connect(weather, "wspd", receiver, "V_wind_10");
		bConnected = connect(weather, "pres", receiver, "P_amb");
		bConnected = connect(weather, "tdew", receiver, "T_dp");
		bConnected = connect(weather, "beam", receiver, "I_bn");
		bConnected = connect(hel_field, "eta_field", receiver, "field_eff");
		bConnected = connect(weather, "tdry", receiver, "T_db");

		// Set Controller (type 251) Parameters
		set_unit_value_ssc_double(controller, "field_fluid" ); //, 17); 
		set_unit_value_ssc_matrix(controller, "field_fl_props" ); //, [0]);
		set_unit_value_ssc_double(controller, "store_fluid" ); //, 17);
		set_unit_value_ssc_matrix(controller, "user_fluid" ); //, [0]);
		set_unit_value_ssc_double(controller, "tshours" ); //, 10);
		set_unit_value_ssc_double(controller, "is_hx" ); //, 0);
		set_unit_value_ssc_double(controller, "dt_hot" ); //, 0);
		set_unit_value_ssc_double(controller, "dt_cold" ); //, 0);
		set_unit_value_ssc_double(controller, "hx_config" ); //, 0);
		set_unit_value_ssc_double(controller, "q_max_aux" ); //, 115/0.412);
		set_unit_value_ssc_double(controller, "T_set_aux" ); //, 594);
		set_unit_value_ssc_double(controller, "V_tank_hot_ini" ); //, 3895.75);
		set_unit_value_ssc_double(controller, "T_tank_hot_ini" ); //, 574.0);
		set_unit_value_ssc_double(controller, "T_tank_cold_ini" ); //, 290.0);
		set_unit_value_ssc_double(controller, "vol_tank" ); //, 12985.8);
		set_unit_value_ssc_double(controller, "h_tank" ); //, 20.0);
		set_unit_value_ssc_double(controller, "h_tank_min" ); //, 1.0);
		set_unit_value_ssc_double(controller, "u_tank" ); //, 0.4);
		set_unit_value_ssc_double(controller, "tank_pairs" ); //, 1);
		set_unit_value_ssc_double(controller, "cold_tank_Thtr" ); //, 280.0);
		set_unit_value_ssc_double(controller, "hot_tank_Thtr" ); //, 500.);
		set_unit_value_ssc_double(controller, "tank_max_heat" ); //, 30);
		set_unit_value_ssc_double(controller, "T_field_in_des" ); //, 290);
		set_unit_value_ssc_double(controller, "T_field_out_des" ); //, 574);
		set_unit_value_ssc_double(controller, "q_pb_design" ); //, 115/0.412);
		set_unit_value_ssc_double(controller, "W_pb_design" ); //, 115);
		set_unit_value_ssc_double(controller, "cycle_max_frac" ); //, 1.05);
		set_unit_value_ssc_double(controller, "cycle_cutoff_frac" ); //, 0.25);
		set_unit_value_ssc_double(controller, "solarm" ); //, 669.903/(115/0.412) );
		set_unit_value_ssc_double(controller, "pb_pump_coef" ); //, 0.55);
		set_unit_value_ssc_double(controller, "tes_pump_coef" ); //, 0.15);
		set_unit_value_ssc_double(controller, "pb_fixed_par", as_double("pb_fixed_par_cntl") ); //, 0.0);
		set_unit_value_ssc_array(controller, "bop_array" ); //, [0.0,0.0,0.0,0.0,0.0]);
		set_unit_value_ssc_array(controller, "aux_array" ); //, [0.0,0.0,0.0,0.0,0.0]);
		set_unit_value_ssc_double(controller, "T_startup" ); //, 500);
		set_unit_value_ssc_double(controller, "fossil_mode" ); //, 1);
		set_unit_value_ssc_double(controller, "fthr_ok" ); //, 1);
		set_unit_value_ssc_double(controller, "nSCA" ); //, 1);
		set_unit_value_ssc_double(controller, "I_bn_des" ); //, 950);
		set_unit_value_ssc_double(controller, "fc_on" ); //, 0);
		set_unit_value_ssc_double(controller, "q_sby_frac" ); //, 0.2);
		set_unit_value_ssc_double(controller, "t_standby_reset" ); //, 0.2);
		set_unit_value_ssc_double(controller, "sf_type" ); //, 2);
		set_unit_value_ssc_double(controller, "tes_type" ); //, 1);
		set_unit_value_ssc_array(controller, "tslogic_a" ); //, [0,0,0,0,0,0,0,0,0]);
		set_unit_value_ssc_array(controller, "tslogic_b" ); //, [0,0,0,0,0,0,0,0,0]);
		set_unit_value_ssc_array(controller, "tslogic_c" ); //, [1,1,1,1,1,1,1,1,1]);	//Not sure if TOU schedule is synced, so make all variables independent of it
		set_unit_value_ssc_array(controller, "ffrac" ); //, [0,0,0,0,0,0,0,0,0]);
		set_unit_value_ssc_double(controller, "tc_fill" ); //, 8);
		set_unit_value_ssc_double(controller, "tc_void" ); //, 0.25);
		set_unit_value_ssc_double(controller, "t_dis_out_min" ); //, 500);
		set_unit_value_ssc_double(controller, "t_ch_out_max" ); //, 400);
		set_unit_value_ssc_double(controller, "nodes" ); //, 100);
		set_unit_value_ssc_double(controller, "f_tc_cold" ); //, 0.7);
		set_unit_value_ssc_array( controller, "TOU_schedule");

		// Set initial values for inputs generated from subsequently called types
		set_unit_value_ssc_double( controller, "m_dot_htf_ref" ); //, 1.0 );
		set_unit_value_ssc_double( controller, "T_pb_out" ); //, T_HTF_out_ref ); 

		// Connect Controller (type 251) inputs
		bConnected = connect( weather, "beam", controller, "I_bn" );
		bConnected = connect( receiver, "m_dot_salt_tot", controller, "m_dot_field" );
		bConnected = connect( powerblock, "m_dot_htf_ref", controller, "m_dot_htf_ref" );
		bConnected = connect( receiver, "T_salt_hot", controller, "T_field_out" );
		bConnected = connect( powerblock, "T_htf_cold", controller, "T_pb_out" );
		bConnected = connect( weather, "tdry", controller, "T_amb" );
		bConnected = connect( powerblock, "m_dot_demand", controller, "m_pb_demand" );	//This is input is not used by the controller
		bConnected = connect( receiver, "q_startup", controller, "q_startup" );		// This input is not used by the controller


		// Set Powerblock (type 224) Parameters
		set_unit_value_ssc_double(powerblock, "P_ref" ); //, 115);
		set_unit_value_ssc_double(powerblock, "eta_ref" ); //, 0.412);
		set_unit_value_ssc_double(powerblock, "T_htf_hot_ref" ); //, 574);
		set_unit_value_ssc_double(powerblock, "T_htf_cold_ref" ); //, 290);
		set_unit_value_ssc_double(powerblock, "dT_cw_ref" ); //, 10);
		set_unit_value_ssc_double(powerblock, "T_amb_des" ); //, 43.0);
		set_unit_value_ssc_double(powerblock, "HTF" ); //, 17);
		set_unit_value_ssc_double(powerblock, "q_sby_frac" ); //, 0.2);
		set_unit_value_ssc_double(powerblock, "P_boil" ); //, 100);
		set_unit_value_ssc_double(powerblock, "CT" ); //, 2);
		set_unit_value_ssc_double(powerblock, "startup_time" ); //, 0.5);
		set_unit_value_ssc_double(powerblock, "startup_frac" ); //, 0.5);
		set_unit_value_ssc_double(powerblock, "tech_type" ); //, 1);
		set_unit_value_ssc_double(powerblock, "T_approach" ); //, 5);
		set_unit_value_ssc_double(powerblock, "T_ITD_des" ); //, 16);
		set_unit_value_ssc_double(powerblock, "P_cond_ratio" ); //, 1.0028);
		set_unit_value_ssc_double(powerblock, "pb_bd_frac" ); //, 0.02);
		set_unit_value_ssc_double(powerblock, "P_cond_min" ); //, 2);
		set_unit_value_ssc_double(powerblock, "n_pl_inc" ); //, 8);
		set_unit_value_ssc_array(powerblock, "F_wc" ); //, [0,0,0,0,0,0,0,0,0]);

		// Set Powerblock (type 224) Inputs
		set_unit_value_ssc_double( powerblock, "mode" ); //, 2 );		//Always set to 2 for type 251
		//set_unit_value_ssc_double( powerblock, "demand_var" ); //, 110.0 );		//Don't need to set this?

		// Connect Powerblock (type 224) inputs
		bConnected = connect( controller, "T_pb_in", powerblock, "T_htf_hot" );
		bConnected = connect( controller, "m_dot_pb", powerblock, "m_dot_htf" );
		bConnected = connect( weather, "twet", powerblock, "T_wb" );
		bConnected = connect( controller, "standby_control", powerblock, "standby_control" );
		bConnected = connect( weather, "tdry", powerblock, "T_db" );
		bConnected = connect( weather, "pres", powerblock, "P_amb");
		bConnected = connect( weather, "rhum", powerblock, "rh" );


		// Set Parasitics (type 228) Parameters
		set_unit_value_ssc_double(parasitics, "P_storage_pump"); //P_storage_pump);
		set_unit_value_ssc_double(parasitics, "Piping_loss"); //piping_loss );
		set_unit_value_ssc_double(parasitics, "Piping_length"); //piping_length );
		set_unit_value_ssc_double(parasitics, "Design_power"); //P_cycle_design );
		set_unit_value_ssc_double(parasitics, "recirc_htr_eff"); //recirc_htr_eff );
		set_unit_value_ssc_double(parasitics, "design_eff"); //Eff_cycle_design );
		set_unit_value_ssc_double(parasitics, "night_recirc"); //night_recirc );
		set_unit_value_ssc_double(parasitics, "pb_fixed_par"); //pb_fixed_par );
		set_unit_value_ssc_double(parasitics, "aux_par"); //aux_par );
		set_unit_value_ssc_double(parasitics, "aux_par_f"); //aux_par_f );
		set_unit_value_ssc_double(parasitics, "aux_par_0"); //aux_par_0 );
		set_unit_value_ssc_double(parasitics, "aux_par_1"); //aux_par_1 );
		set_unit_value_ssc_double(parasitics, "aux_par_2"); //aux_par_2 );
		set_unit_value_ssc_double(parasitics, "bop_par"); //bop_par );
		set_unit_value_ssc_double(parasitics, "bop_par_f"); //bop_par_f );
		set_unit_value_ssc_double(parasitics, "bop_par_0"); //bop_par_0 );
		set_unit_value_ssc_double(parasitics, "bop_par_1"); //bop_par_1 );
		set_unit_value_ssc_double(parasitics, "bop_par_2"); //bop_par_2 );
		set_unit_value_ssc_double(parasitics, "storage_bypass"); //storage_bypass );

		// Set Parasitics (type 228) Inputs (Initial values?)
		set_unit_value_ssc_double(parasitics, "flow_from_storage", 0.0);
		set_unit_value_ssc_double(parasitics, "P_hot_tank", 0.0);	// "tank_fp_par" from controller contains both hot and cold tank htr values
		set_unit_value_ssc_double(parasitics, "recirc_source", 0.0);

		// Connect Parasitics (type 228) module to others
		bConnected = connect(powerblock, "W_cool_par", parasitics, "P_cooling_tower");
		bConnected = connect(receiver, "W_dot_pump", parasitics, "P_tower_pump");
		bConnected = connect(hel_field, "pparasi", parasitics, "P_helio_track");
		bConnected = connect(powerblock, "P_cycle", parasitics, "P_plant_output");
		bConnected = connect(powerblock, "eta", parasitics, "eta_cycle");
		bConnected = connect(controller, "tank_fp_par", parasitics, "P_cold_tank");
		bConnected = connect(receiver, "q_conv_sum", parasitics, "P_tower_conv");
		bConnected = connect(receiver, "q_rad_sum", parasitics, "P_tower_rad");
		bConnected = connect(powerblock, "m_dot_htf_ref", parasitics, "ref_htf_flow");
		bConnected = connect(controller, "q_aux_heat", parasitics, "aux_power");
		bConnected = connect(controller, "htf_pump_power", parasitics, "P_htf_pump");

		// check if all connections worked
		if ( !bConnected )
			throw exec_error( "tcsmolten_salt", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );

		// Run simulation
		size_t hours = 8760;
		if (0 > simulate(3600, hours*3600, 3600) )
			throw exec_error( "tcsmolten_salt", util::format("there was a problem simulating in the TCS molten salt model.") );

		// get the outputs
		if (!set_all_output_arrays() )
			throw exec_error( "tcsmolten_salt", util::format("there was a problem returning the results from the simulation.") );


		//set_output_array("i_SfTi",8760);
	}

};

DEFINE_TCS_MODULE_ENTRY( tcsmolten_salt, "CSP model using the motlen salt power tower TCS types.", 4 )
