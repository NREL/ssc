#include <math.h>

#include "lib_wfhrly.h"
#include "cmod_trnbase.h"


#ifndef M_PI
#define M_PI 3.1415926
#endif

static var_info _cm_vtab_trntower[] = {
/*   VARTYPE           DATATYPE         NAME                                  LABEL                                UNITS      META                      GROUP          REQUIRED_IF                  CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "weather_file",                      "Weather data file (TM2,TM2,EPW)",   "",       "",                      "CSPTower",      "*",                         "",                         "" },

	{ SSC_INPUT,        SSC_NUMBER,      "solar_multiple",                    "Solar multiple",                    "",       "",                      "CSPTower",      "*",                         "POSITIVE",                 "" },

	{ SSC_INPUT,        SSC_MATRIX,      "heliostat_field",                   "Heliostat field matrix",            "",       "",                      "CSPTower",      "*",                         "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "span_angle",                        "Field span angle",                  "deg",    "",                      "CSPTower",      "*",                         "MIN=0,MAX=360",            "" },

	{ SSC_INPUT,        SSC_NUMBER,      "round_heliostats",                  "Use round heliostats",              "0/1",    "",                      "CSPTower",      "*",                         "BOOLEAN",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heliostat_width",                   "Heliostat width",                   "m",      "",                      "CSPTower",      "*",                         "MIN=0",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heliostat_height",                  "Heliostat height",                  "m",      "",                      "CSPTower",      "*",                         "MIN=0",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "field_availability",                "Heliostat field availability",      "%",      "",                      "CSPTower",      "*",                         "MIN=0,MAX=100",            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ratio_reflect_to_profile",          "Ratio reflected area to profile",   "frac",   "",                      "CSPTower",      "*",                         "MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "mirror_reflectivity",               "Mirror reflectivity",               "frac",   "",                      "CSPTower",      "*",                         "MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "max_hel_tower_dist_ratio",          "Max helio-to-tower distance ratio", "frac", "",                        "CSPTower",      "*",                         "POSITIVE",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "min_hel_tower_dist_ratio",          "Min helio-to-tower distance ratio", "frac", "",                        "CSPTower",      "*",                         "POSITIVE",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "image_error",                       "Image error",                       "rad",    "",                      "CSPTower",      "*",                         "MIN=0",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "coating_absorptivity",              "Coating absorptivity",              "frac",   "",                      "CSPTower",      "*",                         "MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "max_receiver_flux",                 "Max receiver flux",                 "kWt/m2", "",                      "CSPTower",      "*",                         "MIN=0",                    "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "receiver_type",                     "Receiver type",                     "",       "0=external,1=cavity",   "CSPTower",      "*",                         "MIN=0,MAX=1,INTEGER",      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cavity_aperture_width",             "Cavity aperture width",             "m",      "",                      "CSPTower",      "receiver_type=1",           "POSITIVE",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cavity_aperture_hw_ratio",          "Cavity height-to-width ratio",      "frac",   "",                      "CSPTower",      "receiver_type=1",           "POSITIVE",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cavity_lip_height_ratio",           "Cavity lip-to-height ratio",        "frac",   "",                      "CSPTower",      "receiver_type=1",           "POSITIVE",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cavity_panel_height",               "Cavity panel height",               "",       "",                      "CSPTower",       "*",                        "POSITIVE",                 "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "exter_height",                      "External receiver height",          "m",      "",                      "CSPTower",      "receiver_type=0",           "POSITIVE",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "exter_diameter",                    "External receiver diameter",        "m",      "",                      "CSPTower",      "receiver_type=0",           "POSITIVE"                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "exter_num_panels",                  "External receiver num panels",      "",       "",                      "CSPTower",      "receiver_type=0",           "POSITIVE,INTEGER",         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "exter_coating_emittance",           "External receiver emittance",       "frac",   "",                      "CSPTower",      "receiver_type=0",           "",                         "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "tower_height",                      "Tower height",                      "m",      "",                      "CSPTower",      "*",                         "POSITIVE",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "design_gross_output",               "Plant design gross output",         "MW",     "",                      "CSPTower",      "*",                         "POSITIVE",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cycle_eff",                         "Cycle efficiency",                  "frac",   "",                      "CSPTower",      "*",                         "MIN=0,MAX=1",              "" },

	{ SSC_INPUT,        SSC_STRING,      "optieff_datablob",                  "Optical efficiency data file",      "",       ""                       "CSPTower",      "*",                         "",                         "" },
	{ SSC_INPUT,        SSC_STRING,      "fluxmap_datablob",                  "Flux map data file",                "",       ""                       "CSPTower",      "*",                         "",                         "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "wind_stow_speed",                   "Wind stow speed",                   "m/s",    "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "stow_deploy_angle",                 "Stow deploy angle",                 "deg",    "",                      "CSPTower",       "*",                        "",                         "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "tube_outer_diameter",               "Tube outer diameter",               "mm",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tube_wall_thickness",               "Tube wall thickness",               "mm",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "flow_pattern",                      "Flow pattern",                      "0..8",   "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "req_htf_outlet_temp",               "Required HTF outlet temp",          "'C",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "max_temp_to_receiver",              "Max temp to receiver",              "'C",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "max_flow_to_receiver",              "Max flow rate to receiver",         "kg/s",   "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "enable_night_recirc",               "Enable night recirculation",        "0/1",    "",                      "CSPTower",       "*",                        "BOOLEAN",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "recirc_eff",                        "Recirculation efficiency",          "frac",   "",                      "CSPTower",       "*",                        "MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "htf_type",                          "HTF fluid type",                    "0..2",   "",                      "CSPTower",       "*",                        "INTEGER,MIN=0,MAX=2",      "" },
	{ SSC_INPUT,        SSC_MATRIX,      "custom_htf",                        "Custom HTF fluid properties",       "",       "",                      "CSPTower",       "htf_type=2",               "NCOLS=7",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "min_turndown_frac",                 "Min turndown fraction",             "frac",   "",                      "CSPTower",       "*",                        "MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "startup_delay_time",                "Startup delay time",                "hr",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "startup_delay_energy_frac",         "Startup delay energy fraction",     "frac",   "",                      "CSPTower",       "*",                        "MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heat_loss_f",                       "Heat loss factor",                  "",       "",                      "CSPTower",       "*",                        "",                         "" },
	
	// power block
	{ SSC_INPUT,        SSC_NUMBER,      "boiler_steam_pressure",             "Boiler steam pressure",             "bar",    "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cycle_eff",                         "Cycle efficiency",                  "frac",   "",                      "CSPTower",       "*",                        "MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "design_ambient_temp",               "Design ambient temp",               "'C",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "design_htf_inlet_temp",             "Design HTF inlet temp",             "'C",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "design_htf_outlet_temp",            "Design HTF outlet temp",            "'C",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "design_thermal_power",              "Design thermal power",              "MWt",    "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "max_over_design",                   "Max over design operation fraction","frac",   "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "min_load",                          "Minimum loading",                   "frac",   "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "min_temp_to_load",                  "Minimum temp to load",              "'C",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ref_condenser_dt",                  "Reference condenser delta-T",       "'C",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "standby_fraction",                  "Standby fraction",                  "frac",   "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "standby_period",                    "Standby period",                    "hr",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "startup_fraction",                  "Startup fraction",                  "frac",   "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "startup_time",                      "Startup time",                      "hr",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "lhv_eff",                           "Boiler lower heating value",        "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "blowdown_frac",                     "Blowdown fraction",                 "frac",   "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "approach_temp",                     "Approach temperature",              "'C",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "condenser_pressure_ratio",          "Condenser pressure ratio",          "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "condenser_type",                    "Condenser type",                    "0..2",   "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itd_design",                        "ITD at design",                     "'C",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "min_condenser_pressure",            "Min condenser pressure",            "inHg",   "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hr_pl_nlev",                        "Cooling system part load levels",   "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl1",                           "Hybrid cooling dispatch fraction 1","",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl2",                           "Hybrid cooling dispatch fraction 2","",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl3",                           "Hybrid cooling dispatch fraction 3","",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl4",                           "Hybrid cooling dispatch fraction 4","",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl5",                           "Hybrid cooling dispatch fraction 5","",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl6",                           "Hybrid cooling dispatch fraction 6","",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl7",                           "Hybrid cooling dispatch fraction 7","",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl8",                           "Hybrid cooling dispatch fraction 8","",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl9",                           "Hybrid cooling dispatch fraction 9","",       "",                      "CSPTower",       "*",                        "",                         "" },
	
	// storage
	{ SSC_INPUT,        SSC_NUMBER,      "full_load_ts_hours",                "Full load thermal storage hours",   "hr",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "parallel_tank_pairs",               "Number of parallel tank pairs",     "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tank_height",                       "Storage tank height",               "m",      "",                      "CSPTower",       "*",                        "MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heater_eff",                        "Storage tank heater efficiency",    "frac",   "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hot_heater_max_load",               "Hot tank heater max load",          "MWe",    "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hot_heater_set_temp",               "Hot tank heater set temp",          "'C",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cold_heater_max_load",              "Cold tank heater max load",         "MWe",    "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cold_heater_set_temp",              "Cold tank heater set temp",         "'C",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "init_hot_htf_temp",                 "Initial hot tank HTF temp",         "'C",     "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "init_cold_htf_temp",                "Initial cold tank HTF temp",        "'C",     "",                      "CSPTower",       "*",                        "",                         "" },	
	{ SSC_INPUT,        SSC_NUMBER,      "init_hot_htf_percent",              "Initial hot tank HTF percentage",   "%",      "",                      "CSPTower",       "*",                        "MIN=0,MAX=100",            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "wetted_loss_coeff",                 "Wetted loss coefficient",           "Wt/m2-K","",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dry_loss_coeff",                    "Dry loss coefficient",              "Wt/m2-K","",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "storage_bypass",                    "Enable storage bypass valve",       "0/1",    "",                      "CSPTower",       "*",                        "BOOLEAN",                         "" },
	
	// dispatch
	{ SSC_INPUT,        SSC_STRING,      "dispatch_weekday",                  "Weekday dispatch schedule",           "",       "",                      "CSPTower",       "*",                        "TOUSCHED",                 "" },
	{ SSC_INPUT,        SSC_STRING,      "dispatch_weekend",                  "Weekend dispatch schedule",           "",       "",                      "CSPTower",       "*",                        "TOUSCHED",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch1_fossil",                  "Dispatch Period 1 Fossil Fraction"    "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch1_nosolar",                 "Dispatch Period 1 No Solar Fraction", "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch1_solar",                   "Dispatch Period 1 Solar Fraction",    "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch1_turbout",                 "Dispatch Period 1 Turbine Fraction",  "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch2_fossil",                  "Dispatch Period 2 Fossil Fraction",   "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch2_nosolar",                 "Dispatch Period 2 No Solar Fraction", "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch2_solar",                   "Dispatch Period 2 Solar Fraction",    "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch2_turbout",                 "Dispatch Period 2 Turbine Fraction",  "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch3_fossil",                  "Dispatch Period 3 Fossil Fraction",   "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch3_nosolar",                 "Dispatch Period 3 No Solar Fraction", "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch3_solar",                   "Dispatch Period 3 Solar Fraction",    "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch3_turbout",                 "Dispatch Period 3 Turbine Fraction",  "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch4_fossil",                  "Dispatch Period 4 Fossil Fraction",   "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch4_nosolar",                 "Dispatch Period 4 No Solar Fraction", "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch4_solar",                   "Dispatch Period 4 Solar Fraction",    "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch4_turbout",                 "Dispatch Period 4 Turbine Fraction",  "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch5_fossil",                  "Dispatch Period 5 Fossil Fraction",   "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch5_nosolar",                 "Dispatch Period 5 No Solar Fraction", "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch5_solar",                   "Dispatch Period 5 Solar Fraction",    "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch5_turbout",                 "Dispatch Period 5 Turbine Fraction",  "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch6_fossil",                  "Dispatch Period 6 Fossil Fraction",   "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch6_nosolar",                 "Dispatch Period 6 No Solar Fraction", "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch6_solar",                   "Dispatch Period 6 Solar Fraction",    "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch6_turbout",                 "Dispatch Period 6 Turbine Fraction",  "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch7_fossil",                  "Dispatch Period 7 Fossil Fraction",   "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch7_nosolar",                 "Dispatch Period 7 No Solar Fraction", "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch7_solar",                   "Dispatch Period 7 Solar Fraction",    "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch7_turbout",                 "Dispatch Period 7 Turbine Fraction",  "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch8_fossil",                  "Dispatch Period 8 Fossil Fraction",   "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch8_nosolar",                 "Dispatch Period 8 No Solar Fraction", "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch8_solar",                   "Dispatch Period 8 Solar Fraction",    "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch8_turbout",                 "Dispatch Period 8 Turbine Fraction",  "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch9_fossil",                  "Dispatch Period 9 Fossil Fraction",   "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch9_nosolar",                 "Dispatch Period 9 No Solar Fraction", "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch9_solar",                   "Dispatch Period 9 Solar Fraction",    "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch9_turbout",                 "Dispatch Period 9 Turbine Fraction",  "",       "",                      "CSPTower",       "*",                        "",                         "" },

	// parastics
	{ SSC_INPUT,        SSC_NUMBER,      "heliostat_startup_energy",          "Heliostat startup energy",            "kWe-hr", "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heliostat_tracking_power",          "Heliostat tracking energy",           "kWe",    "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "piping_loss_coeff",                 "Piping loss coefficient",             "Wt/m",   "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_htf_pump_eff",                  "Receiver HTF pump efficiency",        "frac",   "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "storage_pump_power",                "Storage pump power",                  "MWe/MWt","",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "total_piping_length",               "Total piping length",                 "m",      "",                      "CSPTower",       "*",                        "MIN=0",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "aux_c0",                            "Aux heater/boiler parasitic coeff 0", "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "aux_c1",                            "Aux heater/boiler parasitic coeff 1", "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "aux_c2",                            "Aux heater/boiler parasitic coeff 2", "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "aux_pf",                            "Aux heater/boiler parasitic factor",  "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "aux_val",                           "Aux heater/boiler parasitic value",   "MWe/MWcap","",                    "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_c0",                            "Balance of plant parasitic coeff 0",  "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_c1",                            "Balance of plant parasitic coeff 1",  "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_c2",                            "Balance of plant parasitic coeff 2",  "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_pf",                            "Balance of plant parasitic factor",   "",       "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_val",                           "Balance of plant parasitic value",    "MWe/MWcap",""                     "CSPTower",       "*",                        "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pb_fixed_frac",                     "Fixed power consumption fraction",    "MWe/MWt","",                      "CSPTower",       "*",                        "",                         "" },
	
	
	// outputs
	{ SSC_OUTPUT,        SSC_ARRAY,       "dni",                              "DNI",                                  "kW/m2",  "",                      "CSPTower",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "windspd",                          "Wind speed",                           "m/s",    "",                      "CSPTower",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "ambtemp",                          "Ambient temp",                         "'C",     "",                      "CSPTower",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "sol_rad_inc_on_col",               "Incident solar radiation",             "kWh",    "",                      "CSPTower",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "thermal_energy_from_sf",           "Thermal energy from solar field",      "kWh",    "",                      "CSPTower",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "thermal_energy_to_powerblock",     "Thermal energy to power block",        "kWh",    "",                      "CSPTower",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "gross_electric_output",            "Gross electric output",                "kWh",    "",                      "CSPTower",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "e_net",                            "Net electric output",                  "kWh",    "",                      "CSPTower",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "water_flow",                       "Water flow",                           "m3/hr",  "",                      "CSPTower",       "*",                        "LENGTH=8760",                         "" },
	
	{ SSC_OUTPUT,        SSC_NUMBER,      "fuel_usage",                       "Annual Fuel Usage (kWht)",             "kWht",   "",                      "CSPTower",       "*",                        "",                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "gross_to_net_conversion_factor",   "Gross to Net Conv. Factor",            "frac",   "",                      "CSPTower",       "*",                        "",                         "" },
	
var_info_invalid };


class cm_trntower : public cm_trnbase
{
private:
public:
	cm_trntower() : cm_trnbase()
	{
		add_var_info( _cm_vtab_trntower );
	}
	
	virtual void pre_trnsys_call() throw( general_error )
	{
		std::string file;
		
		file = work_dir() + util::path_separator() + "touperiod.in";
		if (!write_tou_file( "dispatch_weekday", "dispatch_weekend", file.c_str()))
			throw general_error("failed to write tou dispatch input file: " + file);

		file = work_dir() + util::path_separator() + "fluidprop.dat";
		if ( 36 == translate_htf_type( as_integer("htf_type") )
			&& !write_htf_file( "custom_htf", file.c_str()))
			throw general_error("failed to write custom htf data file: " + file);
	}

	virtual void write_include_file( FILE *fp ) throw( general_error )
	{
		std::string annual_output_file = work_dir() + util::path_separator() + "tower.annual.out";
		std::string monthly_output_file = work_dir() + util::path_separator() + "tower.monthly.out";
		std::string hourly_output_file = work_dir() + util::path_separator() + "tower.hourly.out";
		std::string dview_output_file = data_file();
		std::string touperiod_file = work_dir() + util::path_separator() + "touperiod.in";
		std::string eff_array_file = work_dir() + util::path_separator() + "eff_array.dat";
		std::string flux_map_file = work_dir() + util::path_separator() + "flux_map.csv";
		std::string fluid_file = work_dir() + util::path_separator() + "fluidprop.dat";
		std::string warnings_file = work_dir() + util::path_separator() + "warnings.out";
		std::string pb_coeff_file = work_dir() + util::path_separator() + "pb_coeff.in";
		const char *weather_file = as_string("weather_file");


		wf_header hdr;
		if (!wf_read_header( weather_file, &hdr ))
			throw general_error("could not scan weather file header information: " + std::string(weather_file));


		FILE *fout;

		// clear the warnings file
		fout = fopen(warnings_file.c_str(),"w");
		if (fout) fclose(fout);

		// write the eff_array, array_view, and fluxmap files
	
		// ======= efficiency array file
		fout = fopen(eff_array_file.c_str(), "w");
		if (fout)
		{
			fputs( as_string("optieff_datablob"), fout);
			fclose(fout);
		}
		
		// ======= flux map file
		fout = fopen(flux_map_file.c_str(), "w");
		if (fout)
		{
			fputs( as_string("fluxmap_datablob"), fout);
			fclose(fout);
		}

		// ========= write actual include file

		// get field dimensions
		size_t nrad = 0, nazm = 0;
		as_matrix("heliostat_field", &nrad, &nazm);

		// write all include file lines

		// for now, create empty PB coeff file
		FILE *fpb = fopen(pb_coeff_file.c_str(), "w");
		if (fpb) fclose(fpb);

		fputs("\n", fp);
		fprintf(fp, "ASSIGN \"%s\" 30\n", weather_file);
		fprintf(fp, "ASSIGN \"%s\" 53\n", touperiod_file.c_str()); 
		fprintf(fp, "ASSIGN \"%s\" 10\n", eff_array_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 60\n", flux_map_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 58\n", annual_output_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 54\n", hourly_output_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 61\n", dview_output_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 57\n", monthly_output_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 95\n", fluid_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 96\n", warnings_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 65\n", pb_coeff_file.c_str());
		fputs("\n", fp);


		fprintf(fp, "CONSTANTS 2\n");
		fprintf(fp, "TRAKMODE=4\n");
		fprintf(fp, "WFType = %d\n", weather_file_type( weather_file ));

		fputs("\n", fp);
		fprintf(fp, "*********************** HELIOSTAT FIELD PAGE **********************\n");
		fprintf(fp, "CONSTANTS 9\n");
		fprintf(fp, "num_zen = %d\n", as_integer("optieff_zenith"));
		fprintf(fp, "num_azi = %d\n", as_integer("optieff_azimuth"));
		fprintf(fp, "num_helio = %lg\n", as_double("csp.pt.sf.num_heliostats"));
		fprintf(fp, "wind_stow_speed = %lg\n", as_double("wind_stow_speed"));
		fprintf(fp, "h_helio = %lg\n", as_double("heliostat_height"));
		fprintf(fp, "w_helio = %lg\n", as_double("heliostat_width"));
		fprintf(fp, "Hel_dens = %lg\n", as_double("ratio_reflect_to_profile"));
		fprintf(fp, "hel_stow_deploy = %lg\n", as_double("stow_deploy_angle"));
		fprintf(fp, "field_angle = %lg\n", as_double("field_span")/2.0);
		fputs("\n", fp);

		fprintf(fp, "*********************** RECEIVER/TOWER FIELD PAGE **********************\n");
		fprintf(fp, "CONSTANTS 26\n");
		fprintf(fp, "is_north = %d\n", as_integer("receiver_type"));
		fprintf(fp, "num_panels = %d\n", as_integer("exter_num_panels"));
		fprintf(fp, "d_rec = %lg\n", as_double("exter_diameter"));

		if (as_integer("receiver_type")==0)
		{
			fprintf(fp, "h_rec = %lg\n", as_double("exter_height")); // EXTERNAL
			fprintf(fp, "Rec_d_spec = 0.0\n");
			fprintf(fp, "H_lip = 0.0\n");
		}
		else 
		{
			fprintf(fp, "h_rec = %lg\n", as_double("cavity_panel_height")); // CAVITY
			fprintf(fp, "Rec_d_spec = %lg\n", as_double("cavity_aperture_width"));
			fprintf(fp, "H_lip = %lg\n", as_double("cavity_panel_height")*as_double("cavity_lip_height_ratio"));
		}

		fprintf(fp, "h_tower = %lg\n", as_double("tower_height"));
		fprintf(fp, "d_tube = %lg\n", as_double("tube_outer_diameter"));
		fprintf(fp, "th_tube = %lg\n", as_double("tube_wall_thickness"));
		fprintf(fp, "Material = 2\n"); // fixed to stainless steel
		fprintf(fp, "HTF = %d\n", translate_htf_type(as_integer("htf_type")));
		fprintf(fp, "flow_pattern = %d\n", as_integer("flow_pattern")+1);
		fprintf(fp, "HTF_rec_out = %lg\n", as_double("req_htf_outlet_temp"));
		fprintf(fp, "HTF_max_inlet = %lg\n", as_double("max_temp_to_receiver"));
		fprintf(fp, "Rec_HTF_max_flow = %lg\n", as_double("max_flow_to_receiver")*3600);
		fprintf(fp, "Rec_coating_abs = %lg\n", as_double("coating_absorptance"));
		fprintf(fp, "epsilon = %lg\n", as_double("exter_coating_emittance"));
		fprintf(fp, "night_recirc = %d\n", as_integer("enable_night_recirc"));
		fprintf(fp, "recirc_htr_eff = %lg\n", as_double("recirc_eff"));
		fprintf(fp, "nazm = %d\n", nazm);
		fprintf(fp, "nrad = %d\n", nrad);


		fprintf(fp, "Plant_lattitude = %lg\n", hdr.lat);
	
		fprintf(fp, "f_rec_min = %lg\n", as_double("min_turndown_frac"));
		fprintf(fp, "Q_rec_des = %lg\n", as_double("design_thermal_power"));
		fprintf(fp, "rec_su_delay = %lg\n", as_double("startup_delay_time"));
		fprintf(fp, "rec_qf_delay = %lg\n", as_double("startup_delay_energy_frac"));

		fputs("\n", fp);
		
		fprintf(fp, "****************Power Block page****************\n");
		fprintf(fp, "CONSTANTS 27\n");
		fprintf(fp, "tech_type = 1\n");
		fprintf(fp, "LU_pb = 65\n");
		fprintf(fp, "P_cycle_design = %lg\n", as_double("design_gross_output"));
		fprintf(fp, "Eff_cycle_design = %lg\n", as_double("cycle_eff"));
		fprintf(fp, "T_HTF_in_ref = %lg\n", as_double("design_htf_inlet_temp"));
		fprintf(fp, "T_HTF_out_ref = %lg\n", as_double("design_htf_outlet_temp"));
		fprintf(fp, "P_boiler = %lg\n", as_double("boiler_steam_pressure"));
		fprintf(fp, "Cycle_min_inlet_temp = %lg\n", as_double("min_temp_to_load"));
		fprintf(fp, "Turb_startup_t = %lg\n", as_double("startup_time"));
		fprintf(fp, "Turb_startup_f = %lg\n", as_double("startup_fraction"));
		fprintf(fp, "T_standby = %lg\n", as_double("standby_period"));
		fprintf(fp, "F_standby = %lg\n", as_double("standby_fraction"));
		fprintf(fp, "startup_time = %lg\n", as_double("startup_time"));
		fprintf(fp, "startup_frac = %lg\n", as_double("startup_fraction"));
		fprintf(fp, "hl_ffact = %lg\n", as_double("heat_loss_f"));
		fprintf(fp, "cycle_cutoff_frac = %lg\n", as_double("min_load"));
		fprintf(fp, "cycle_max_fraction = %lg\n", as_double("max_over_design"));
		fprintf(fp, "LHV_eff = %lg\n", as_double("lhv_eff"));

		fprintf(fp, "T_amb_des = %lg\n", as_double("design_ambient_temp"));
		fprintf(fp, "dT_cooling_ref = %lg\n", as_double("ref_condenser_dt"));
		fprintf(fp, "Cool_type = %d\n", as_integer("condenser_type")+1);
		fprintf(fp, "T_approach = %lg\n", as_double("approach_temp"));
		fprintf(fp, "T_ITD_des = %lg\n", as_double("itd_design"));
		fprintf(fp, "P_cond_ratio = %lg\n", as_double("condenser_pressure_ratio"));
		fprintf(fp, "pb_bd_frac = %lg\n", as_double("blowdown_frac"));
		fprintf(fp, "min_cond_pres = %lg\n", as_double("min_condenser_pressure"));
		fprintf(fp, "hr_pl_nlev = %d\n", as_integer("hr_pl_nlev"));

		fputs("\n", fp);


		fprintf(fp, "****************Thermal storage page****************\n");
		fprintf(fp, "CONSTANTS 20\n");
		fprintf(fp, "storage_bypass = %d\n", as_integer("storage_bypass"));
		fprintf(fp, "Is_two_tank = %d\n", as_integer("storage_type")+1);
		fprintf(fp, "V_tank_tot = %lg\n", as_double("storage_htf_vol"));
		fprintf(fp, "V_tank_min = %lg\n", as_double("min_fluid_vol"));
		fprintf(fp, "V_tank_max = %lg\n", as_double("max_fluid_vol"));
		fprintf(fp, "r_tank = %lg\n", as_double("tank_diameter")/2);
		double t_d = as_double("tank_diameter");
		fprintf(fp, "Circ_tank = %lg\n", M_PI*t_d);
		fprintf(fp, "A_c_tank = %lg\n", M_PI*pow(t_d/2,2));
		fprintf(fp, "h_tank_wetted = %lg\n", as_double("wetted_loss_coeff"));
		fprintf(fp, "h_tank_dry = %lg\n", as_double("dry_loss_coeff"));
		fprintf(fp, "T_store_hot_initial = %lg\n", as_double("init_hot_htf_temp"));
		fprintf(fp, "T_store_cold_initial = %lg\n", as_double("init_cold_htf_temp"));
		fprintf(fp, "V_store_hot_initial = %lg\n", as_double("init_hot_htf_vol"));
		fprintf(fp, "V_store_cold_initial = %lg\n", as_double("init_cold_htf_vol"));
		fprintf(fp, "tank_pairs = %d\n", as_integer("parallel_tank_pairs"));
		fprintf(fp, "T_htr_ctank= %lg\n", as_double("cold_heater_set_temp"));
		fprintf(fp, "T_htr_htank= %lg\n", as_double("hot_heater_set_temp"));
		fprintf(fp, "Q_max_ctank= %lg\n", as_double("cold_heater_max_load"));
		fprintf(fp, "Q_max_htank= %lg\n", as_double("hot_heater_max_load"));
		fprintf(fp, "eta_tank_htr= %lg\n", as_double("heater_eff"));

		fprintf(fp, "CONSTANTS 38\n");
		fprintf(fp, "TSHOURS = %lg\n", as_double("full_load_ts_hours"));
		fprintf(fp, "NUMTOU = 9\n");	
		fprintf(fp, "TSLOGICT = %lg\n", as_double("dispatch1_solar"));
		fprintf(fp, "TSLOGIC1 = %lg\n", as_double("dispatch1_nosolar"));
		fprintf(fp, "TSLOGIC2 = %lg\n", as_double("dispatch1_turbout"));
		fprintf(fp, "TSLOGIC3 = %lg\n", as_double("dispatch2_solar"));
		fprintf(fp, "TSLOGIC4 = %lg\n", as_double("dispatch2_nosolar"));
		fprintf(fp, "TSLOGIC5 = %lg\n", as_double("dispatch2_turbout"));
		fprintf(fp, "TSLOGIC6 = %lg\n", as_double("dispatch3_solar"));
		fprintf(fp, "TSLOGIC7 = %lg\n", as_double("dispatch3_nosolar"));
		fprintf(fp, "TSLOGIC8 = %lg\n", as_double("dispatch3_turbout"));
		fprintf(fp, "TSLOGIC9 = %lg\n", as_double("dispatch4_solar"));
		fprintf(fp, "TSLOGI10 = %lg\n", as_double("dispatch4_nosolar"));
		fprintf(fp, "TSLOGI11 = %lg\n", as_double("dispatch4_turbout"));	
		fprintf(fp, "TSLOGI12 = %lg\n", as_double("dispatch5_solar"));
		fprintf(fp, "TSLOGI13 = %lg\n", as_double("dispatch5_nosolar"));
		fprintf(fp, "TSLOGI14 = %lg\n", as_double("dispatch5_turbout"));
		fprintf(fp, "TSLOGI15 = %lg\n", as_double("dispatch6_solar"));
		fprintf(fp, "TSLOGI16 = %lg\n", as_double("dispatch6_nosolar"));
		fprintf(fp, "TSLOGI17 = %lg\n", as_double("dispatch6_turbout"));
		fprintf(fp, "TSLOGI18 = %lg\n", as_double("dispatch7_solar"));
		fprintf(fp, "TSLOGI19 = %lg\n", as_double("dispatch7_nosolar"));
		fprintf(fp, "TSLOGI20 = %lg\n", as_double("dispatch7_turbout"));
		fprintf(fp, "TSLOGI21 = %lg\n", as_double("dispatch8_solar"));
		fprintf(fp, "TSLOGI22 = %lg\n", as_double("dispatch8_nosolar"));
		fprintf(fp, "TSLOGI23 = %lg\n", as_double("dispatch8_turbout"));
		fprintf(fp, "TSLOGI24 = %lg\n", as_double("dispatch9_solar"));
		fprintf(fp, "TSLOGI25 = %lg\n", as_double("dispatch9_nosolar"));
		fprintf(fp, "TSLOGI26 = %lg\n", as_double("dispatch9_turbout"));
		fprintf(fp, "FOSSILFI = %lg\n", as_double("dispatch1_fossil"));
		fprintf(fp, "FOSSILF1 = %lg\n", as_double("dispatch2_fossil"));
		fprintf(fp, "FOSSILF2 = %lg\n", as_double("dispatch3_fossil"));
		fprintf(fp, "FOSSILF3 = %lg\n", as_double("dispatch4_fossil"));
		fprintf(fp, "FOSSILF4 = %lg\n", as_double("dispatch5_fossil"));
		fprintf(fp, "FOSSILF5 = %lg\n", as_double("dispatch6_fossil"));
		fprintf(fp, "FOSSILF6 = %lg\n", as_double("dispatch7_fossil"));
		fprintf(fp, "FOSSILF7 = %lg\n", as_double("dispatch8_fossil"));
		fprintf(fp, "FOSSILF8 = %lg\n", as_double("dispatch9_fossil"));
		fputs("\n", fp);

		fprintf(fp, "CONSTANTS 9\n");
		fprintf(fp, "HC_LOGIC0 = %lg\n", as_double("hc_ctl1"));
		fprintf(fp, "HC_LOGIC1 = %lg\n", as_double("hc_ctl2"));
		fprintf(fp, "HC_LOGIC2 = %lg\n", as_double("hc_ctl3"));
		fprintf(fp, "HC_LOGIC3 = %lg\n", as_double("hc_ctl4"));
		fprintf(fp, "HC_LOGIC4 = %lg\n", as_double("hc_ctl5"));
		fprintf(fp, "HC_LOGIC5 = %lg\n", as_double("hc_ctl6"));
		fprintf(fp, "HC_LOGIC6 = %lg\n", as_double("hc_ctl7"));
		fprintf(fp, "HC_LOGIC7 = %lg\n", as_double("hc_ctl8"));
		fprintf(fp, "HC_LOGIC8 = %lg\n", as_double("hc_ctl9"));

		fprintf(fp, "****************Parasitics page****************\n");
		fprintf(fp, "CONSTANTS 17\n");
		fprintf(fp, "P_hel_start = %lg\n", as_double("heliostat_startup_energy"));
		fprintf(fp, "P_hel_track = %lg\n", as_double("heliostat_tracking_power"));
		fprintf(fp, "eta_rec_pump = %lg\n", as_double("rec_htf_pump_eff"));
		fprintf(fp, "P_storage_pump = %lg\n", as_double("storage_pump_power"));
		fprintf(fp, "piping_loss = %lg\n", as_double("piping_loss_coeff"));
		fprintf(fp, "piping_length = %lg\n", as_double("total_piping_length"));

	
		fprintf(fp, "aux_par_0 = %lg\n", as_double("aux_c0"));
		fprintf(fp, "aux_par_1 = %lg\n", as_double("aux_c1"));
		fprintf(fp, "aux_par_2 = %lg\n", as_double("aux_c2"));
		fprintf(fp, "aux_par_f = %lg\n", as_double("aux_pf"));
		fprintf(fp, "aux_par = %lg\n", as_double("aux_val"));
		fprintf(fp, "bop_par_0 = %lg\n", as_double("bop_c0"));
		fprintf(fp, "bop_par_1 = %lg\n", as_double("bop_c1"));
		fprintf(fp, "bop_par_2 = %lg\n", as_double("bop_c2"));
		fprintf(fp, "bop_par_f = %lg\n", as_double("bop_pf"));
		fprintf(fp, "bop_par = %lg\n", as_double("bop_val"));
		fprintf(fp, "pb_fixed_par = %lg\n", as_double("pb_fixed_frac"));

	}

	virtual void process_outputs() throw( general_error )
	{
		update("Saving data...", 99.5);
		
		std::string hourly = work_dir() + util::path_separator() + "tower.hourly.out";

		output d;
		if (!d.read( hourly.c_str() )) throw general_error("could not read hourly output file: " + hourly);
	
		save_data(d, "DNI", "dni", 1.0, 8760);
		save_data(d, "V_wind", "windspd", 1.0, 8760);
		save_data(d, "Dry_bulb_temp", "ambtemp", 1.0, 8760);
		save_data(d, "Total_incident_power", "sol_rad_inc_on_col", 1000.0, 8760);
		save_data(d, "Power_from_receiver", "thermal_energy_from_sf", 1000.0, 8760);
		save_data(d, "Q_to_PB", "thermal_energy_to_powerblock", 1000.0, 8760);
		save_data(d, "Power_from_cycle_elec", "gross_electric_output", 1000.0, 8760);
		save_data(d, "Power_to_the_grid", "e_net", 1000.0, 8760);
		save_data(d, "water_makeup_flow", "water_flow", (ssc_number_t)(1/998.2), 8760); //Vol [m3] = mass [kg] * 1/998.2 [m3/kg]

		
		double net_total = 1.0, gross_total = 1.0;
		if (!accumulate_annual( d, "Power_from_cycle_elec", gross_total)
			|| ! accumulate_annual( d, "Power_to_the_grid", net_total)) 
			throw general_error("could not accumulate annual values for gross and net generation");

		std::string annual = work_dir() + util::path_separator() + "tower.annual.out";
		if (!d.read( annual.c_str() )) throw general_error("could not read annual output file: " + annual);
		
		save_data(d, "Fossil_energy", "fuel_usage", 1000.0, 1);
		assign("gross_to_net_conversion_factor", var_data( (ssc_number_t)( net_total / gross_total ) ));

		if (!util::remove_file( hourly.c_str() ))
			log("failed to delete hourly file: " + hourly);
	
	}

	virtual const char *deck_name() throw( general_error )
	{
		if (as_integer("receiver_type") == 1) return "csp_cavtower";
		else return "csp_exttower";
	}

	
	int translate_htf_type(int ui_number)
	{
		/*
	!    1.) Air
	!    2.) Stainless_AISI316
	!    3.) Water (liquid)
	!    4.) Steam
	!    5.) CO2
	!    6.) Salt (68% KCl, 32% MgCl2)
	!    7.) Salt (8% NaF, 92% NaBF4)
	!    8.) Salt (25% KF, 75% KBF4)
	!    9.) Salt (31% RbF, 69% RbBF4)
	!    10.) Salt (46.5% LiF, 11.5%NaF, 42%KF)
	!    11.) Salt (49% LiF, 29% NaF, 29% ZrF4)
	!    12.) Salt (58% KF, 42% ZrF4)
	!    13.) Salt (58% LiCl, 42% RbCl)
	!    14.) Salt (58% NaCl, 42% MgCl2)
	!    15.) Salt (59.5% LiCl, 40.5% KCl)
	!    16.) Salt (59.5% NaF, 40.5% ZrF4)
	!    17.) Salt (60% NaNO3, 40% KNO3)
	!    18.) Nitrate Salt**
	!    19.) Caloria HT 43**
	!    20.) Hitec XL**
	!    21.) Therminol VP-1**
	!    22.) Hitec**
	!    23.) Dowtherm Q**
	!    24.) Dowtherm RP**
	!    25.) Salt XL**
	!   .....
	!    36+) User specified (lookup tables)
	*/
		switch(ui_number)
		{
		case 0: return 17;
		case 1: return 10;
		case 2: return 36; // user defined fluid #1
		default: return -1;
		}
	}
};

DEFINE_MODULE_ENTRY( trntower, "Power tower (central receiver) simulator (TRNSYS)", 1 )