#include "core.h"
#include "tckernel.h"

static var_info _cm_vtab_tcsdirect_steam[] = {
/*	EXAMPLE LINES FOR INPUTS
    { SSC_INPUT,        SSC_NUMBER,      "XXXXXXXXXXXXXX",    "Label",                                                          "",             "",            "sca",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "INTINTINTINT",      "Label",                                                          "",             "",            "parasitic",      "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "XXXXXXXXXXX",       "Number indicating the receiver type",                            "",             "",            "hce",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "XXXXXXXXXXX",       "Label",                                                          "",             "",            "tes",            "*",                       "",                      "" },
*/
	
//    VARTYPE           DATATYPE          NAME                 LABEL                                                            UNITS           META            GROUP            REQUIRED_IF                 CONSTRAINTS             UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "file_name",         "local weather file path",                                        "",             "",            "Weather",        "*",                       "LOCAL_FILE",            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "track_mode",        "Tracking mode",                                                  "",             "",            "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tilt",              "Tilt angle of surface/axis",                                     "",             "",            "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "azimuth",           "Azimuth angle of surface/axis",                                  "",             "",            "Weather",        "*",                       "",                      "" },

	// Heliostat field (type 221) parameters
    { SSC_INPUT,        SSC_MATRIX,      "eta_map",         "Field efficiency matrix",                                          "-",            "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_zen",           "Number of zenith angle data points in file",                       "-",            "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_azi",           "Number of azimuth angle data points in file",                      "-",            "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_hel",           "Number of heliostats in the field",                                "-",            "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_start",         "Electric work for starting up one heliostat",                      "kWe-hr",       "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "p_run",           "Electric power for tracking one heliostat",                        "kWe",          "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "v_wind_max",      "Maximum tolerable wind speed",                                     "m/s",          "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hel_stow_deploy", "Heliostat field stow/deploy solar elevation angle",                "deg",          "",            "heliostat",      "*",                       "",                      "" },
	// Heliostat field (type 221) inputs
    { SSC_INPUT,        SSC_NUMBER,      "field_control",   "Field defocus control",                                            "",             "",            "heliostat",      "*",                       "",                      "" },

	// Direct steam controller (type 265) parameters
    { SSC_INPUT,        SSC_NUMBER,      "fossil_mode",     "Fossil model: 1=Normal, 2=Supplemental",                           "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_pb_design",     "Heat rate into powerblock at design",                              "MW",           "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_aux_max",       "Maximum heat rate of auxiliary heater",                            "MW",           "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "lhv_eff",         "Aux Heater lower heating value efficiency",                        "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tower",         "Tower Height",                                                     "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_panels",        "Number of panels",                                                 "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "flowtype",        "Code for flow pattern through rec.",                               "-",            "",            "dsg_controller", "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_rec",           "Diameter of Receiver",                                             "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_rec_des",       "Design-point thermal power",                                       "MW",           "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_rec_min",       "Minimum receiver absorbed power fraction",                         "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_qf_delay",    "Receiver start-up delay fraction of thermal energy of receiver running at design for 1 hour", "-", "", "dsg_controller", "*",                  "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_su_delay",    "Receiver start-up delay time",                                     "hr",           "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_pb_cutoff",     "Cycle cut-off fraction",                                           "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_pb_sb",         "Cycle minimum standby fraction",                                   "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_standby_ini",   "Power block standby time",                                         "hr",           "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "x_b_target",      "Target boiler outlet quality",                                     "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_rec_pump",    "Feedwater pump efficiency",                                        "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_hp_in_des",     "Design HP Turbine Inlet Pressure",                                 "bar",          "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_hp_out_des",    "Design HP Turbine Outlet Pressure",                                "bar",          "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_mdotrh_des",    "Design reheat mass flow rate fraction",                            "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "p_cycle_design",  "Design Cycle Power",                                               "MW",           "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ct",              "Cooling Type",                                                     "-",            "",            "dsg_controller", "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",       "Design ambient temperature (power cycle)",                         "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",       "Reference condenser water dT",                                     "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",      "Approach temperature for wet cooling",                             "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",       "Approach temperature for dry cooling",                             "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hl_ffact",        "Heat Loss Fudge FACTor",                                           "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_boiler",        "Height of boiler",                                                 "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_t_boiler",      "O.D. of boiler tubes",                                             "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "th_t_boiler",     "Thickness of boiler tubes",                                        "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "emis_boiler",     "Emissivity of boiler tubes",                                       "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "abs_boiler",      "Absorptance of boiler tubes",                                      "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mat_boiler",      "Numerical code for tube material",                                 "-",            "",            "dsg_controller", "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "th_fin",          "Thickness of fin",                                                 "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "l_fin",           "Length of fin (distance between tubes)",                           "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "emis_fin",        "Emissivity of fin",                                                "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "abs_fin",         "Absorptance of fin",                                               "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mat_fin",         "Numerical code for fin material",                                  "-",            "",            "dsg_controller", "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_sh",            "Height of superheater",                                            "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_sh",            "O.D. of superheater tubes",                                        "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "th_sh",           "Thickness of superheater tubes",                                   "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "emis_sh",         "Emissivity of superheater tubes",                                  "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "abs_sh",          "Absorptance of superheater tubes",                                 "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mat_sh",          "Numerical code for superheater material",                          "-",            "",            "dsg_controller", "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_sh_out_des",    "Target superheater outlet temperature",                            "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_rh",            "Height of reheater",                                               "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_rh",            "O.D. of reheater tubes",                                           "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "th_rh",           "Thickness of reheater tubes",                                      "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "emis_rh",         "Emissivity of reheater tubes",                                     "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "abs_rh",          "Absorptance of reheater tubes",                                    "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mat_rh",          "Numerical code for reheater material",                             "-",            "",            "dsg_controller", "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_rh_out_des",    "Target reheater outlet temperature",                               "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_max_frac",  "Cycle maximum overdesign fraction",                                "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "A_sf",            "Solar field area",                                                 "m^2",          "",            "dsg_controller", "*",                       "",                      "" },
	//{ SSC_INPUT,        SSC_ARRAY,       "ffrac",           "Fossil dispatch logic",                                            "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "fluxmap_angles",  "Matrix containing zenith and azimuth angles for flux maps",        "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "fluxmap",         "Matrix containing flux map for various solar positions",           "-",            "",            "dsg_controller", "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "TOU_schedule",    "Annual hourly time-of-use schedule",                               "-",            "",            "dsg_controller", "*",                       "",                      "" },
	// Direct steam controller (type 265) inputs
	//{ SSC_INPUT,        SSC_NUMBER,      "azimuth",         "Solar azimuth",                                                    "deg",          "",            "dsg_controller", "*",                       "",                      "" },
	//{ SSC_INPUT,        SSC_NUMBER,      "zenith",          "Solar zenith",                                                     "deg",          "",            "dsg_controller", "*",                       "",                      "" },
	//{ SSC_INPUT,        SSC_NUMBER,      "DNI",             "Direct normal irradiance",                                         "W/m^2",        "",            "dsg_controller", "*",                       "",                      "" },
	//{ SSC_INPUT,        SSC_NUMBER,      "T_amb",           "Ambient dry bulb temperature",                                     "C",            "",            "dsg_controller", "*",                       "",                      "" },
	//{ SSC_INPUT,        SSC_NUMBER,      "v_wind_10",       "Wind speed at 10 m",                                               "m/s",          "",            "dsg_controller", "*",                       "",                      "" },
	//{ SSC_INPUT,        SSC_NUMBER,      "P_atm",           "Ambient Pressure",                                                 "atm",          "",            "dsg_controller", "*",                       "",                      "" },
	//{ SSC_INPUT,        SSC_NUMBER,      "T_dp",            "Dew point temperature",                                            "C",            "",            "dsg_controller", "*",                       "",                      "" },
	//{ SSC_INPUT,        SSC_NUMBER,      "field_eff",       "Heliostat field efficiency",                                       "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_b_in",          "Boiler inlet pressure",                                            "bar",          "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_mdot_rh",       "Reheat mass flow rate fraction",                                   "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_hp_out",        "HP turbine outlet pressure",                                       "bar",          "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_hp_out",        "HP turbine outlet temperature",                                    "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_rh_target",     "Target reheater outlet temp.",                                     "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_fw",            "Feedwater outlet temperature",                                     "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond",          "Condenser pressure",                                               "Pa",           "",            "dsg_controller", "*",                       "",                      "" },

	// Power block (type 234) parameters
    { SSC_INPUT,        SSC_NUMBER,      "P_ref",           "Reference output electric power at design condition",               "MW",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_ref",         "Reference conversion efficiency at design condition",               "none",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_hot_ref",       "Reference HTF inlet temperature at design",                         "C",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_cold_ref",      "Reference HTF outlet temperature at design",                        "C",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",       "Reference condenser cooling water inlet/outlet T diff",             "C",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",       "Reference ambient temperature at design point",                     "C",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",      "Fraction of thermal power required for standby mode",               "none",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil_des",      "Boiler operating pressure @ design",                                "bar",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_rh",           "Flag indicating whether reheat is used 0:no, 1:yes",                "none",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_rh_ref",        "Reheater operating pressure at design",                             "bar",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_rh_hot_ref",    "Reheater design outlet temperature",                                "C",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rh_frac_ref",     "Reheater flow fraction at design",                                  "none",        "",            "powerblock",     "*",                       "",                      "" },
//	{ SSC_INPUT,        SSC_NUMBER,      "CT",              "Flag for using dry cooling or wet cooling system",                  "none",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_time",    "Time needed for power block startup",                               "hr",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",    "Fraction of design thermal power needed for startup",               "none",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tech_type",       "Flag indicating which coef. set to use. (1=tower,2=trough,3=user)", "none",        "",            "powerblock",     "*",                       "",                      "" },
//	{ SSC_INPUT,        SSC_NUMBER,      "T_approach",      "Cooling tower approach temperature",                                "C",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",       "ITD at design for dry system",                                      "C",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",    "Condenser pressure ratio",                                          "none",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",      "Power block blowdown steam fraction ",                              "none",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",      "Minimum condenser pressure",                                        "inHg",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",        "Number of part-load increments for the heat rejection system",      "none",        "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",            "Fraction indicating wet cooling use for hybrid system",             "none",        "",            "powerblock",     "*",                       "",                      "" },
	// Power block (type 234) inputs
    { SSC_INPUT,        SSC_NUMBER,      "mode",            "Cycle part load control, from plant controller",                    "none",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_hot",           "Hot HTF inlet temperature, from storage tank",                      "C",           "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "m_dot_st",        "HTF mass flow rate",                                                "kg/hr",       "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_wb",            "Ambient wet bulb temperature",                                      "C",           "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "demand_var",      "Control signal indicating operational mode",                        "none",        "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "standby_control", "Control signal indicating standby mode",                            "none",        "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_db",            "Ambient dry bulb temperature",                                      "C",           "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "P_amb",           "Ambient pressure",                                                  "atm",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "TOU",             "Current Time-of-use period",                                        "none",        "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "relhum",          "Relative humidity of the ambient air",                              "none",        "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "f_recSU",         "Fraction powerblock can run due to receiver startup",               "none",        "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "dp_b",            "Pressure drop in boiler",                                           "Pa",          "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "dp_sh",           "Pressure drop in superheater",                                      "Pa",          "",            "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "dp_rh",           "Pressure drop in reheater",                                         "Pa",          "",            "powerblock",     "*",                       "",                      "" },

	// Parasitics (type 228) parameters
    {SSC_INPUT,         SSC_NUMBER,      "P_storage_pump",  "Storage pump power, rated per MWt of storage use",                  "MWe/MWt",     "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "Piping_loss",     "Thermal loss per meter of piping",                                  "Wt/m",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "Piping_length",   "Total length of exposed piping",                                    "m",           "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "Design_power",    "Power production at design conditions",                             "MWe",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "recirc_htr_eff",  "Recirculation heater efficiency",                                   "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "design_eff",      "Power cycle efficiency at design",                                  "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "night_recirc",    "Flag indicating whether night recirculation is allowed",            "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "pb_fixed_par",    "Fixed parasitic load - runs at all times",                          "MWe/MWcap",   "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par",         "Aux heater, boiler parasitic",                                      "MWe/MWcap",   "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_f",       "Aux heater, boiler parasitic - multiplying fraction",               "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_0",       "Aux heater, boiler parasitic - constant coefficient",               "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_1",       "Aux heater, boiler parasitic - linear coefficient",                 "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_2",       "Aux heater, boiler parasitic - quadratic coefficient",              "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par",         "Balance of plant parasitic power fraction",                         "MWe/MWcap",   "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_f",       "Balance of plant parasitic power fraction - mult frac",             "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_0",       "Balance of plant parasitic power fraction - const coeff",           "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_1",       "Balance of plant parasitic power fraction - linear coeff",          "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_2",       "Balance of plant parasitic power fraction - quadratic coeff",       "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "storage_bypass",  "Flag indicating whether the hot salt pump always runs w/ PB",       "none",        "",            "parasitics",     "*",                       "",                      "" },
    // Parasitics (type 228) inputs
    {SSC_INPUT,         SSC_NUMBER,      "flow_from_storage","Flow rate from storage",                                           "kg/hr",       "",            "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "P_cooling_tower", "Cooling tower parasitic power fraction",                            "MWe",         "",            "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "P_tower_pump",    "Reported tower pump power",                                         "MWe",         "",            "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "P_helio_track",   "Reported heliostat tracking power",                                 "MWe",         "",            "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "P_plant_output",  "Reported plant power output",                                       "MWe",         "",            "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "eta_cycle",       "Power cycle efficiency",                                            "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "P_cold_tank",     "Cold tank heater parasitic power",                                  "MWe",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "P_hot_tank",      "Hot tank heater parasitic power",                                   "MWe",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "P_tower_conv",    "Reported tower convection loss",                                    "MWt",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "P_tower_rad",     "Reported tower radiation loss",                                     "MWt",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "recirc_source",   "Recirculation heater control",                                      "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "ref_htf_flow",    "HTF flow rate through the power cycle at design",                   "kg/hr",       "",            "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "aux_power",       "Auxiliary heater thermal power output",                             "MWt",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "P_htf_pump",      "HTF pumping power",                                                 "MWe",         "",            "parasitics",     "*",                       "",                      "" },


// OUTPUTS
// The names of the output variables should match the parameter names for the TCS units in order to signal to the TCS kernel to store the values by timestep

//    VARTYPE           DATATYPE          NAME                 LABEL                                                            UNITS           META            GROUP            REQUIRED_IF                 CONSTRAINTS             UI_HINTS
    { SSC_OUTPUT,       SSC_ARRAY,       "P_cycle",         "Cycle power output",                                                "MWh",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
//    { SSC_OUTPUT,       SSC_ARRAY,       "Cycle_power",     "Cycle power output",                                                "MWh",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
/*

// weather file reader
    { SSC_OUTPUT,       SSC_ARRAY,       "month",             "Month",                                                          "",             "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solazi",            "Solar Azimuth",                                                  "deg",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solzen",            "Solar Zenith",                                                   "deg",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "beam",              "Beam normal irradiance",                                         "W/m2",         "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tdry",              "Dry bulb temperature",                                           "C",            "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "wspd",              "Wind Speed",                                                     "m/s",          "",            "weather",        "*",                       "LENGTH=8760",           "" },


    { SSC_OUTPUT,       SSC_ARRAY,       "XXXXXXXXX",         "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",       "deg",          "",            "type_???",       "*",                       "LENGTH=8760",           "" },
*/

	var_info_invalid };

class cm_tcsdirect_steam : public tcKernel
{
public:

	cm_tcsdirect_steam(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info( _cm_vtab_tcsdirect_steam );
		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above
	}

	void exec( ) throw( general_error )
	{
		bool debug_mode = (__DEBUG__ == 1);  // When compiled in VS debug mode, this will use the trnsys weather file; otherwise, it will attempt to open the file with name that was passed in
		//Add weather file reader unit
		int weather = 0;
		if(debug_mode) weather = add_unit("trnsys_weatherreader", "TRNSYS weather reader");
		else weather = add_unit("weatherreader", "TCS weather reader");

		// Add units
		int hel_field = add_unit("sam_mw_pt_type221");
		int dsg_controller = add_unit("sam_dsg_controller_type265");
		int powerblock = add_unit("sam_mw_type234");
		int parasitics = add_unit("sam_mw_pt_type228");

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
		}
		else
		{
			//Set weatherreader parameters
			set_unit_value_ssc_string( weather, "file_name" );
			set_unit_value_ssc_double( weather, "track_mode" );
			set_unit_value_ssc_double( weather, "tilt" );
			set_unit_value_ssc_double( weather, "azimuth" );
		}

		// Set heliostat field parameters
		set_unit_value_ssc_matrix( hel_field, "eta_map");//, eta_field_map );
		set_unit_value_ssc_double( hel_field, "n_zen");//, num_zen );
		set_unit_value_ssc_double( hel_field, "n_azi");//, num_azi );
		set_unit_value_ssc_double( hel_field, "n_hel");//, num_helio );
		set_unit_value_ssc_double( hel_field, "q_start");//, P_hel_start );
		set_unit_value_ssc_double( hel_field, "p_run");//, P_hel_track );
		set_unit_value_ssc_double( hel_field, "v_wind_max");//, wind_stow_speed );
		set_unit_value_ssc_double( hel_field, "hel_stow_deploy");//, hel_stow_deploy );
        set_unit_value_ssc_double( hel_field, "field_control" ); // INITIAL VALUE

		// Connect heliostat field inputs
		bool bConnected = connect( weather, "wspd", hel_field, "vwind" );
		bConnected = connect( weather, "solzen", hel_field, "theta" );
		bConnected = connect( weather, "solazi", hel_field, "phi" );

		//Set DSG Controller Parameters
		set_unit_value_ssc_double( dsg_controller, "fossil_mode"); //fossil_mode);
        set_unit_value_ssc_double( dsg_controller, "q_pb_design"); //, as_double("p_cycle_design")/as_double("Eff_cycle_design") );
		set_unit_value_ssc_double( dsg_controller, "q_aux_max"); //, as_double("p_cycle_design")/as_double("Eff_cycle_design")); //q_pb_design);
		set_unit_value_ssc_double( dsg_controller, "lhv_eff"); //LHV_eff);
		set_unit_value_ssc_double( dsg_controller, "h_tower"); //h_tower);
		set_unit_value_ssc_double( dsg_controller, "n_panels"); //num_panels);
		set_unit_value_ssc_double( dsg_controller, "flowtype"); //flow_pattern);
		set_unit_value_ssc_double( dsg_controller, "d_rec"); //d_rec);
		set_unit_value_ssc_double( dsg_controller, "q_rec_des"); //Q_rec_des);
		set_unit_value_ssc_double( dsg_controller, "f_rec_min"); //f_rec_min);
		set_unit_value_ssc_double( dsg_controller, "rec_qf_delay"); //rec_qf_delay);
		set_unit_value_ssc_double( dsg_controller, "rec_su_delay"); //rec_su_delay);
		set_unit_value_ssc_double( dsg_controller, "f_pb_cutoff"); //cycle_cutoff_frac);
		set_unit_value_ssc_double( dsg_controller, "f_pb_sb"); //F_standby);
		set_unit_value_ssc_double( dsg_controller, "t_standby_ini"); //T_standby);
		set_unit_value_ssc_double( dsg_controller, "x_b_target"); //x_b_target);
		set_unit_value_ssc_double( dsg_controller, "eta_rec_pump"); //eta_rec_pump);
		set_unit_value_ssc_double( dsg_controller, "P_hp_in_des"); //P_HP_in);
		set_unit_value_ssc_double( dsg_controller, "P_hp_out_des"); //P_HP_out);
		set_unit_value_ssc_double( dsg_controller, "f_mdotrh_des"); //rh_frac_ref);
		set_unit_value_ssc_double( dsg_controller, "p_cycle_design"); //P_cycle_design);
		set_unit_value_ssc_double( dsg_controller, "ct"); //Cool_type);
		set_unit_value_ssc_double( dsg_controller, "T_amb_des"); //T_amb_des);
		set_unit_value_ssc_double( dsg_controller, "dT_cw_ref"); //dT_cooling_ref);
		set_unit_value_ssc_double( dsg_controller, "T_approach"); //T_approach);
		set_unit_value_ssc_double( dsg_controller, "T_ITD_des"); //T_ITD_des);
		set_unit_value_ssc_double( dsg_controller, "hl_ffact"); //hl_ffact);
		set_unit_value_ssc_double( dsg_controller, "h_boiler"); //h_boiler);
		set_unit_value_ssc_double( dsg_controller, "d_t_boiler"); //d_boiler);
		set_unit_value_ssc_double( dsg_controller, "th_t_boiler"); //th_boiler);
		set_unit_value_ssc_double( dsg_controller, "emis_boiler"); //emis_boiler);
		set_unit_value_ssc_double( dsg_controller, "abs_boiler"); //abs_boiler);
		set_unit_value_ssc_double( dsg_controller, "mat_boiler"); //Mat_boiler);
		set_unit_value_ssc_double( dsg_controller, "th_fin"); //th_fin);
		set_unit_value_ssc_double( dsg_controller, "l_fin"); //L_fin);
		set_unit_value_ssc_double( dsg_controller, "emis_fin"); //emis_fin);
		set_unit_value_ssc_double( dsg_controller, "abs_fin"); //abs_fin);
		set_unit_value_ssc_double( dsg_controller, "mat_fin"); //Mat_fin);
		set_unit_value_ssc_double( dsg_controller, "h_sh"); //h_SH);
		set_unit_value_ssc_double( dsg_controller, "d_sh"); //d_SH);
		set_unit_value_ssc_double( dsg_controller, "th_sh"); //th_SH);
		set_unit_value_ssc_double( dsg_controller, "emis_sh"); //emis_SH);
		set_unit_value_ssc_double( dsg_controller, "abs_sh"); //abs_SH);
		set_unit_value_ssc_double( dsg_controller, "mat_sh"); //Mat_SH);
		set_unit_value_ssc_double( dsg_controller, "T_sh_out_des"); //T_SH_out_ref);
		set_unit_value_ssc_double( dsg_controller, "h_rh"); //h_RH);
		set_unit_value_ssc_double( dsg_controller, "d_rh"); //d_RH);
		set_unit_value_ssc_double( dsg_controller, "th_rh"); //th_RH);
		set_unit_value_ssc_double( dsg_controller, "emis_rh"); //emis_RH);
		set_unit_value_ssc_double( dsg_controller, "abs_rh"); //abs_RH);
		set_unit_value_ssc_double( dsg_controller, "mat_rh"); //Mat_RH);
		set_unit_value_ssc_double( dsg_controller, "T_rh_out_des"); //T_rh_out_ref);
		set_unit_value_ssc_double( dsg_controller, "cycle_max_frac"); //cycle_max_fraction);
		set_unit_value_ssc_double( dsg_controller, "A_sf");//, A_sf );
		set_unit_value_ssc_matrix( dsg_controller, "fluxmap_angles"); //arr_sol_pos);
		set_unit_value_ssc_matrix( dsg_controller, "fluxmap"); //arr_flux);
		set_unit_value_ssc_array( dsg_controller, "TOU_schedule");

		// initial values for dsg controller
		set_unit_value_ssc_double(dsg_controller, "P_b_in"); //P_HP_in);			// Initial value
		set_unit_value_ssc_double(dsg_controller, "f_mdot_rh"); //rh_frac_ref);		// Initial value
		set_unit_value_ssc_double(dsg_controller, "P_hp_out"); //P_HP_out);			// Initial value
		set_unit_value_ssc_double(dsg_controller, "T_hp_out"); //300);				// Initial value
		set_unit_value_ssc_double(dsg_controller, "T_rh_target"); //T_rh_out_ref);	// Initial value
		set_unit_value_ssc_double(dsg_controller, "T_fw"); //340);					// Initial value
		set_unit_value_ssc_double(dsg_controller, "P_cond"); //P_HP_out);			// Initial value


		// Connect DSG Controller Inputs
		bConnected = connect(weather, "solazi", dsg_controller, "azimuth");
		bConnected = connect(weather, "solzen", dsg_controller, "zenith");
		bConnected = connect(weather, "beam", dsg_controller, "DNI");
		bConnected = connect(weather, "tdry", dsg_controller, "T_amb");
		bConnected = connect(weather, "wspd", dsg_controller, "v_wind_10");
		bConnected = connect(weather, "pres", dsg_controller, "P_atm");
		bConnected = connect(weather, "tdew", dsg_controller, "T_dp");
		bConnected = connect(hel_field, "eta_field", dsg_controller, "field_eff");
		bConnected = connect(powerblock, "P_boiler_in", dsg_controller, "P_b_in");
		bConnected = connect(powerblock, "f_rh", dsg_controller, "f_mdot_rh");
		bConnected = connect(powerblock, "P_rh_in", dsg_controller, "P_hp_out");
		bConnected = connect(powerblock, "T_rh_in", dsg_controller, "T_hp_out");
		bConnected = connect(powerblock, "T_rh_out", dsg_controller, "T_rh_target");
		bConnected = connect(powerblock, "T_cold", dsg_controller, "T_fw");
		bConnected = connect(powerblock, "P_cond", dsg_controller, "P_cond");

		// Set Powerblock Parameters
		set_unit_value_ssc_double(powerblock, "P_ref"); //P_cycle_design);
		set_unit_value_ssc_double(powerblock, "eta_ref"); //Eff_cycle_design);
		set_unit_value_ssc_double(powerblock, "T_hot_ref"); //T_SH_out_ref);
		set_unit_value_ssc_double(powerblock, "T_cold_ref"); //-1.23);				// This value isn't used in DSG
		set_unit_value_ssc_double(powerblock, "dT_cw_ref"); //dT_cooling_ref);
		set_unit_value_ssc_double(powerblock, "T_amb_des"); //T_amb_des);
		set_unit_value_ssc_double(powerblock, "q_sby_frac"); //F_standby);
		set_unit_value_ssc_double(powerblock, "P_boil_des"); //P_HP_in);
		set_unit_value_ssc_double(powerblock, "is_rh"); //is_rh);
		set_unit_value_ssc_double(powerblock, "P_rh_ref"); //P_HP_out);
		set_unit_value_ssc_double(powerblock, "T_rh_hot_ref"); //T_rh_out_ref);
		set_unit_value_ssc_double(powerblock, "rh_frac_ref"); //rh_frac_ref);
		set_unit_value_ssc_double(powerblock, "CT"); //Cool_type);
		set_unit_value_ssc_double(powerblock, "startup_time"); //startup_time);
		set_unit_value_ssc_double(powerblock, "startup_frac"); //startup_frac);
		set_unit_value_ssc_double(powerblock, "tech_type"); //tech_type);
		set_unit_value_ssc_double(powerblock, "T_approach"); //T_approach);
		set_unit_value_ssc_double(powerblock, "T_ITD_des"); //T_ITD_des);
		set_unit_value_ssc_double(powerblock, "P_cond_ratio"); //P_cond_ratio);
		set_unit_value_ssc_double(powerblock, "pb_bd_frac"); //pb_bd_frac);
		set_unit_value_ssc_double(powerblock, "P_cond_min"); //min_cond_pres);
		set_unit_value_ssc_double(powerblock, "n_pl_inc"); //hr_pl_nlev);
		set_unit_value_ssc_array(powerblock, "F_wc"); //[HC_LOGIC0, HC_LOGIC1, HC_LOGIC2, HC_LOGIC3, HC_LOGIC4, HC_LOGIC5, HC_LOGIC6, HC_LOGIC7, HC_LOGIC8]);

		// Set Powerblock initial values
		set_unit_value_ssc_double(powerblock, "mode");//, 2);
		set_unit_value_ssc_double(powerblock, "T_hot"); //T_SH_out_ref);
		set_unit_value_ssc_double(powerblock, "TOU");//, 1);

		// Connect Powerblock Inputs (and initial values?)
		bConnected = connect(dsg_controller, "m_dot_toPB", powerblock, "m_dot_st");
		bConnected = connect(weather, "twet", powerblock, "T_wb");
		bConnected = connect(dsg_controller, "m_dot_toPB", powerblock, "m_dot_st");
		bConnected = connect(dsg_controller, "standby_control", powerblock, "standby_control");
		bConnected = connect(weather, "tdry", powerblock, "T_db");
		bConnected = connect(weather, "pres", powerblock, "P_amb");
		bConnected = connect(weather, "rhum", powerblock, "relhum");
		bConnected = connect(dsg_controller, "f_timestep", powerblock, "f_recSU");
		bConnected = connect(dsg_controller, "P_drop_b", powerblock, "dp_b");
		bConnected = connect(dsg_controller, "dP_sh", powerblock, "dp_sh");
		bConnected = connect(dsg_controller, "dP_rh", powerblock, "dp_rh");

		// Set Parasitics Parameters
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

		// Set Parasitics Inputs (Initial values?)
		set_unit_value_ssc_double(parasitics, "flow_from_storage", 0.0);
		set_unit_value_ssc_double(parasitics, "P_cold_tank", 0.0);
		set_unit_value_ssc_double(parasitics, "P_hot_tank", 0.0);
		set_unit_value_ssc_double(parasitics, "P_tower_conv", 0.0);
		set_unit_value_ssc_double(parasitics, "P_tower_rad", 0.0);
		set_unit_value_ssc_double(parasitics, "recirc_source", 0.0);
		set_unit_value_ssc_double(parasitics, "ref_htf_flow", 0.0);
		set_unit_value_ssc_double(parasitics, "P_htf_pump", 0.0);

		bConnected = connect(powerblock, "W_cool_par", parasitics, "P_cooling_tower");
		bConnected = connect(dsg_controller, "W_dot_boost", parasitics, "P_tower_pump");
		bConnected = connect(hel_field, "pparasi", parasitics, "P_helio_track");
		bConnected = connect(powerblock, "P_cycle", parasitics, "P_plant_output");
		bConnected = connect(powerblock, "eta", parasitics, "eta_cycle");
		bConnected = connect(dsg_controller, "q_aux", parasitics, "aux_power");

		// check if all connections worked
		if ( !bConnected )
			throw exec_error( "tcsdirect_steam", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );


		// Run simulation
		size_t hours = 8760;
		if (0 > simulate(3600, hours*3600, 3600) )
			throw exec_error( "tcsdirect_steam", util::format("there was a problem simulating in the TCS direct steam power tower model.") );

		// get the outputs
		if (!set_all_output_arrays() )
			throw exec_error( "tcsdirect_steam", util::format("there was a problem returning the results from the simulation.") );


		//set_output_array("i_SfTi",8760);
	}

};

DEFINE_TCS_MODULE_ENTRY( tcsdirect_steam, "CSP model using the direct steam power tower TCS types.", 4 )
