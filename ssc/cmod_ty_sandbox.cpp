#include "core.h"

// solarpilot header files
#include "AutoPilot_API.h"
#include "SolarField.h"
#include "IOUtil.h"
#include "csp_common.h"

// Can probably delete these headers later...
#include "csp_solver_util.h"
#include "csp_solver_pt_heliostatfield.h"
#include "csp_solver_mspt_receiver_222.h"

static bool ssc_mspt_solarpilot_callback(simulation_info *siminfo, void *data);

static var_info _cm_vtab_ty_sandbox[] = {
	/*   VARTYPE           DATATYPE         NAME                           LABEL                                UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "solar_resource_file",  "local weather file path",                                           "",             "",            "Weather",        "*",                       "LOCAL_FILE",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",      "Nameplate capacity",                                                "kW",           "",            "molten salt tower", "*",                    "",   "" },

    // TOU													     																	  
    { SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",     "12x24 Time of Use Values for week days",                            "",             "",            "tou_translator", "*",                       "",                     "" }, 
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",     "12x24 Time of Use Values for week end days",                        "",             "",            "tou_translator", "*",                       "",                     "" }, 

    { SSC_INPUT,        SSC_NUMBER,      "run_type",             "Run type",                                                          "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_width",          "Heliostat width",                                                   "m",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_height",         "Heliostat height",                                                  "m",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_optical_error",  "Heliostat optical error",                                           "rad",          "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_active_fraction","Heliostat active frac.",                                            "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "dens_mirror",          "Ratio of Reflective Area to Profile",                               "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_reflectance",    "Heliostat reflectance",                                             "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_absorptance",      "Receiver absorptance",                                              "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_height",           "Receiver height",                                                   "m",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_aspect",           "Receiver aspect ratio",                                             "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_hl_perm2",         "Receiver design heatloss",                                          "kW/m2",        "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "land_bound_type",      "Land boundary type",                                                "-",            "",            "heliostat",      "?=0",                     "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "land_max",             "Land max boundary",                                                 "-ORm",         "",            "heliostat",      "?=7.5",                   "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "land_min",             "Land min boundary",                                                 "-ORm",         "",            "heliostat",      "?=0.75",                  "",                     "" },
    { SSC_INPUT,        SSC_MATRIX,      "land_bound_table",     "Land boundary table",                                               "m",            "",            "heliostat",      "?",                       "",                     "" },
    { SSC_INPUT,        SSC_ARRAY,       "land_bound_list",      "Boundary table listing",                                            "-",            "",            "heliostat",      "?",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dni_des",              "Design-point DNI",                                                  "W/m2",         "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "p_start",              "Heliostat startup energy",                                          "kWe-hr",       "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "p_track",              "Heliostat tracking energy",                                         "kWe",          "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "hel_stow_deploy",      "Stow/deploy elevation",                                             "deg",          "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "v_wind_max",           "Max. wind velocity",                                                "m/s",          "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "interp_nug",           "Interpolation nugget",                                              "-",            "",            "heliostat",      "?=0",                     "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "interp_beta",          "Interpolation beta coef.",                                          "-",            "",            "heliostat",      "?=1.99",                  "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_flux_x",             "Flux map X resolution",                                             "-",            "",            "heliostat",      "?=12",                    "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_flux_y",             "Flux map Y resolution",                                             "-",            "",            "heliostat",      "?=1",                     "",                     "" },
    { SSC_INPUT,        SSC_MATRIX,      "helio_positions",      "Heliostat position table",                                          "m",            "",            "heliostat",      "run_type=1",              "",                     "" },
    { SSC_INPUT,        SSC_MATRIX,      "helio_aim_points",     "Heliostat aim point table",                                         "m",            "",            "heliostat",      "?",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "N_hel",                "Number of heliostats",                                              "-",            "",            "heliostat",      "?",                       "",                     "" },
    { SSC_INPUT,        SSC_MATRIX,      "eta_map",              "Field efficiency array",                                            "-",            "",            "heliostat",      "?",                       "",                     "" },
    { SSC_INPUT,        SSC_MATRIX,      "flux_positions",       "Flux map sun positions",                                            "deg",          "",            "heliostat",      "?",                       "",                     "" },
    { SSC_INPUT,        SSC_MATRIX,      "flux_maps",            "Flux map intensities",                                              "-",            "",            "heliostat",      "?",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c_atm_0",              "Attenuation coefficient 0",                                         "",             "",            "heliostat",      "?=0.006789",              "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c_atm_1",              "Attenuation coefficient 1",                                         "",             "",            "heliostat",      "?=0.1046",                "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c_atm_2",              "Attenuation coefficient 2",                                         "",             "",            "heliostat",      "?=-0.0107",               "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c_atm_3",              "Attenuation coefficient 3",                                         "",             "",            "heliostat",      "?=0.002845",              "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "n_facet_x",            "Number of heliostat facets - X",                                    "",             "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "n_facet_y",            "Number of heliostat facets - Y",                                    "",             "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "focus_type",           "Heliostat focus method",                                            "",             "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cant_type",            "Heliostat cant method",                                             "",             "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_flux_days",          "No. days in flux map lookup",                                       "",             "",            "heliostat",      "?=8",                     "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "delta_flux_hrs",       "Hourly frequency in flux map lookup",                               "",             "",            "heliostat",      "?=1",                     "",                     "" },
    
    
	{ SSC_INPUT,        SSC_NUMBER,      "h_tower",              "Tower height",                                                      "m",            "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "q_design",             "Receiver thermal design power",                                     "MW",           "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "calc_fluxmaps",        "Include fluxmap calculations",                                      "",             "",            "heliostat",      "?=1",                     "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tower_fixed_cost",     "Tower fixed cost",                                                  "$",            "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tower_exp",            "Tower cost scaling exponent",                                       "",             "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_ref_cost",         "Receiver reference cost",                                           "$",            "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_ref_area",         "Receiver reference area for cost scale",                            "",             "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_cost_exp",         "Receiver cost scaling exponent",                                    "",             "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "site_spec_cost",       "Site improvement cost",                                             "$/m2",         "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heliostat_spec_cost",  "Heliostat field cost",                                              "$/m2",         "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "plant_spec_cost",      "Power cycle specific cost",                                         "$/kWe",        "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_spec_cost",        "BOS specific cost",                                                 "$/kWe",        "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tes_spec_cost",        "Thermal energy storage cost",                                       "$/kWht",       "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "land_spec_cost",       "Total land area cost",                                              "$/acre",       "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "contingency_rate",     "Contingency for cost overrun",                                      "%",            "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sales_tax_rate",       "Sales tax rate",                                                    "%",            "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sales_tax_frac",       "Percent of cost to which sales tax applies",                        "%",            "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cost_sf_fixed",        "Solar field fixed cost",                                            "$",            "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fossil_spec_cost",     "Fossil system specific cost",                                       "$/kWe",        "",            "heliostat",      "*",                       "",                     "" },
																																																									      
    { SSC_INPUT,        SSC_NUMBER,      "is_optimize",          "Do SolarPILOT optimization",                                        "",             "",            "heliostat",       "?=0",                    "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "flux_max",             "Maximum allowable flux",                                            "",             "",            "heliostat",       "?=1000",                 "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_init_step",        "Optimization initial step size",                                    "",             "",            "heliostat",       "?=0.05",                 "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_max_iter",         "Max. number iteration steps",                                       "",             "",            "heliostat",       "?=200",                  "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_conv_tol",         "Optimization convergence tol",                                      "",             "",            "heliostat",       "?=0.001",                "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_algorithm",        "Optimization algorithm",                                            "",             "",            "heliostat",       "?=0",                    "",                     "" },

    //other costs needed for optimization update
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.per_acre",       "EPC cost per acre",                                       "$/acre",       "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.percent",        "EPC cost percent of direct",                              "",             "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.per_watt",       "EPC cost per watt",                                       "$/W",          "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.fixed",          "EPC fixed",                                               "$",            "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.per_acre",       "PLM cost per acre",                                       "$/acre",       "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.percent",        "PLM cost percent of direct",                              "",             "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.per_watt",       "PLM cost per watt",                                       "$/W",          "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.fixed",          "PLM fixed",                                               "$",            "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.sf.fixed_land_area",      "Fixed land area",                                         "acre",         "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.sf.land_overhead_factor", "Land overhead factor",                                    "",             "",            "heliostat",       "*",                      "",                     "" },
	//The total installed cost from the cost page																                      			      		         				    					      					      
    { SSC_INPUT,        SSC_NUMBER,      "total_installed_cost",           "Total installed cost",                                    "$",            "",            "heliostat",       "*",                      "",                     "" },

	// Which type of receiver model to use in the simulation     																	  
    { SSC_INPUT,        SSC_NUMBER,      "receiver_type",        "External=0, Cavity=1",                                              "",             "",            "receiver",       "*",                       "INTEGER",               "" },
															     																	  
	// Receiver (type 222) parameters						     																	  
    { SSC_INPUT,        SSC_NUMBER,      "N_panels",             "Number of individual panels on the receiver",                       "",             "",            "receiver",       "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "D_rec",                "The overall outer diameter of the receiver",                        "m",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "H_rec",                "The height of the receiver",                                        "m",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "THT",                  "The height of the tower (hel. pivot to rec equator)",               "m",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_tube_out",           "The outer diameter of an individual receiver tube",                 "mm",           "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "th_tube",              "The wall thickness of a single receiver tube",                      "mm",           "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mat_tube",             "The material name of the receiver tubes",                           "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_htf",              "The name of the HTF used in the receiver",                          "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "field_fl_props",       "User defined field fluid property data",                            "-",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Flow_type",            "A flag indicating which flow pattern is used",                      "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "epsilon",              "The emissivity of the receiver surface coating",                    "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hl_ffact",             "The heat loss factor (thermal loss fudge factor)",                  "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_hot_des",        "Hot HTF outlet temperature at design conditions",                   "C",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_cold_des",       "Cold HTF inlet temperature at design conditions",                   "C",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_rec_min",            "Minimum receiver mass flow rate turn down fraction",                "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Q_rec_des",            "Design-point receiver thermal power output",                        "MWt",          "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_su_delay",         "Fixed startup delay time for the receiver",                         "hr",           "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_qf_delay",         "Energy-based rcvr startup delay (fraction of rated thermal power)", "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htf_max",        "Maximum receiver mass flow rate",                                   "kg/hr",        "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "A_sf",                 "Solar Field Area",                                                  "m^2",          "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_salt_hot_target",    "Desired HTF outlet temperature",                                    "C",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_salt_cold",          "Desired HTF inlet temperature",                                     "C",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_pump",             "Receiver HTF pump efficiency",                                      "",             "",            "receiver",       "*",                       "",                      "" },
													     																	  
    // Cavity Receiver (type 232) specific parameters		     																	  
    { SSC_INPUT,        SSC_NUMBER,      "rec_d_spec",           "Receiver aperture width",                                           "m",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_rec_panel",          "Height of a receiver panel",                                        "m",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_lip",                "Height of upper lip of cavity",                                     "m",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tower",              "Total height of the solar tower",                                   "m",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_angle",            "Section of the cavity circle covered in panels",                    "deg",          "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "conv_model",           "Type of convection model (1=Clausing, 2=Siebers/Kraabel)",          "-",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htf_max",        "Maximum receiver mass flow rate",                                   "kg/hr",        "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "eps_wavelength",       "Matrix containing wavelengths, active & passive surface eps",       "-",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "conv_coupled",         "1=coupled, 2=uncoupled",                                            "-",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "conv_forced",          "1=forced (use wind), 0=natural",                                    "-",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_wind_meas",          "Height at which wind measurements are given",                       "m",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "conv_wind_dir",        "Wind direction dependent forced convection 1=on 0=off",             "-",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "deg_wind",             "Wind direction",                                                    "deg",          "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_htf",                "Average coolant pressure",                                          "bar",          "",            "cavity_receiver","*",                       "",                      "" },
															     																	  
															     																	  															     																	  
    // Controller (type 251) parameters						     																	  
    { SSC_INPUT,        SSC_NUMBER,      "field_fluid",          "Material number for the collector field",                           "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "store_fluid",          "Material number for storage fluid",                                 "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tshours",              "Equivalent full-load thermal storage hours",                        "hr",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_hx",                "1=yes, 0=no"                                                        "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dt_hot",               "Hot side HX approach temp",                                         "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dt_cold",              "Cold side HX approach temp",                                        "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hx_config",            "HX configuration",                                                  "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_max_aux",            "Max heat rate of auxiliary heater",                                 "MWt",          "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_set_aux",            "Aux heater outlet temp set point",                                  "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_tank_hot_ini",       "Initial hot tank fluid volume",                                     "m3",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_hot_ini",       "Initial hot tank fluid temperature",                                "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_cold_ini",      "Initial cold tank fluid tmeperature",                               "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "vol_tank",             "Total tank volume, including unusable HTF at bottom",               "m3",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank",               "Total height of tank (height of HTF when tank is full",             "m",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank_min",           "Minimum allowable HTF height in storage tank",                      "m",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "u_tank",               "Loss coefficient from the tank",                                    "W/m2-K",       "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_pairs",           "Number of equivalent tank pairs",                                   "-",            "",            "controller",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_Thtr",       "Minimum allowable cold tank HTF temp",                              "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_Thtr",        "Minimum allowable hot tank HTF temp",                               "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_max_heat",        "Rated heater capacity for tank heating",                            "MW",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_field_in_des",       "Field design inlet temperature",                                    "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_field_out_des",      "Field design outlet temperature",                                   "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_pb_design",          "Design heat input to power block",                                  "MWt",          "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "W_pb_design",          "Rated plant capacity",                                              "MWe",          "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_max_frac",       "Maximum turbine over design operation fraction",                    "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_cutoff_frac",    "Minimum turbine operation fraction before shutdown",                "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "solarm",               "Solar Multiple",                                                    "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_pump_coef",         "Pumping power to move 1kg of HTF through PB loop",                  "kW/kg",        "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_pump_coef",        "Pumping power to move 1kg of HTF through tes loop",                 "kW/kg",        "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_fixed_par_cntl",    "Fraction of rated gross power constantly consumed by controller",   "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "bop_array",            "Coefficients for balance of plant parasitics calcs",                "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "aux_array",            "Coefficients for auxiliary heater parasitics calcs",                "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_startup",            "Startup temperature",                                               "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fossil_mode",          "Fossil backup mode 1=Normal 2=Topping",                             "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fthr_ok",              "Does the defocus control allow partial defocusing",                 "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nSCA",                 "Number of SCAs in a single loop",                                   "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_bn_des",             "Design point irradiation value",                                    "W/m2",         "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fc_on",                "DNI forecasting enabled",                                           "-",            "",            "controller",     "?=0",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",           "Fraction of thermal power required for standby",                    "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_standby_reset",      "Maximum allowable time for PB standby operation",                   "hr",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "sf_type",              "Solar field type, 1 = trough, 2 = tower",                           "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_type",             "1=2-tank, 2=thermocline",                                           "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_a",            "Dispatch logic without solar",                                      "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_b",            "Dispatch logic with solar",                                         "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_c",            "Dispatch logic for turbine load fraction",                          "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "ffrac",                "Fossil dispatch logic",                                             "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tc_fill",              "Thermocline fill material",                                         "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tc_void",              "Thermocline void fraction",                                         "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_dis_out_min",        "Min allowable hot side outlet temp during discharge",               "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_ch_out_max",         "Max allowable cold side outlet temp during charge",                 "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nodes",                "Nodes modeled in the flow path",                                    "-",            "",            "controller",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_tc_cold",            "0=entire tank is hot, 1=entire tank is cold",                       "-",            "",            "controller",     "*",                       "",                      "" },
														     																	  
    // Controller (type 251) inputs							     																	  
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htf_ref",        "Reference HTF flow rate at design conditions",                      "kg/hr",        "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_pb_out",             "Fluid temperature from the power block",                            "C",            "",            "controller",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_lhv",              "Fossil fuel lower heating value - Thermal power generated per unit fuel",   "MW/MMBTU",     "",    "controller",     "*",                       "",                      "" },													     																	  
															     																	  
    // Powerblock (type 224) parameters						     																	  
    { SSC_INPUT,        SSC_NUMBER,      "P_ref",                "Reference output electric power at design condition",               "MW",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_ref",              "Reference conversion efficiency at design condition",               "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_hot_ref",        "Reference HTF inlet temperature at design",                         "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_cold_ref",       "Reference HTF outlet temperature at design",                        "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",            "Reference condenser cooling water inlet/outlet T diff",             "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",            "Reference ambient temperature at design point",                     "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "HTF",                  "Integer flag identifying HTF in power block",                       "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil",               "Boiler operating pressure",                                         "bar",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                   "Flag for using dry cooling or wet cooling system",                  "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_time",         "Time needed for power block startup",                               "hr",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",         "Fraction of design thermal power needed for startup",               "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tech_type",            "Flag indicating which coef. set to use. (1=tower,2=trough,3=user)", "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",           "Cooling tower approach temperature",                                "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",            "ITD at design for dry system",                                      "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",         "Condenser pressure ratio",                                          "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",           "Power block blowdown steam fraction ",                              "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",           "Minimum condenser pressure",                                        "inHg",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",             "Number of part-load increments for the heat rejection system",      "none",         "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",                 "Fraction indicating wet cooling use for hybrid system",             "none",         "",            "powerblock",     "*",                       "",                      "" },
															     																	  
    // Powerblock (type 224) inputs							     																	  
    { SSC_INPUT,        SSC_NUMBER,      "mode",                 "Cycle part load control, from plant controller",                    "none",         "",            "powerblock",     "*",                       "",                      "" },

	// sCO2 Powerblock (type 424) inputs
	{ SSC_INPUT,        SSC_NUMBER,      "pc_config",            "0: Steam Rankine (224), 1: sCO2 Recompression (424)",               "none",         "",            "powerblock",     "?=0",                       "INTEGER",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_c",                "Isentropic efficiency of compressor(s)",                            "none",         "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_t",                "Isentropic efficiency of turbine",							      "none",         "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "P_high_limit",         "Upper pressure limit in cycle",								      "MPa",          "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "deltaT_PHX",           "Design temperature difference in PHX",						      "C",	          "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fan_power_perc_net",   "% of net cycle output used for fan power at design",			      "%",	          "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "elev",                 "Site elevation",                                                    "m",            "",            "powerblock",     "*",                       "",                      "" },

	// Parasitics (type 228) parameters						     																	  
    {SSC_INPUT,         SSC_NUMBER,      "P_storage_pump",       "Storage pump power, rated per MWt of storage use",                  "MWe/MWt",      "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "Piping_loss",          "Thermal loss per meter of piping",                                  "Wt/m",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "Piping_length",        "Total length of exposed piping",                                    "m",            "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "csp.pt.par.piping_length_mult",     "Piping length multiplier",                             "",             "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "csp.pt.par.piping_length_const",    "Piping constant length",                               "m",            "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "Design_power",         "Power production at design conditions",                             "MWe",          "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "recirc_htr_eff",       "Recirculation heater efficiency",                                   "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "design_eff",           "Power cycle efficiency at design",                                  "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "pb_fixed_par",         "Fixed parasitic load - runs at all times",                          "MWe/MWcap",    "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par",              "Aux heater, boiler parasitic",                                      "MWe/MWcap",    "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_f",            "Aux heater, boiler parasitic - multiplying fraction",               "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_0",            "Aux heater, boiler parasitic - constant coefficient",               "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_1",            "Aux heater, boiler parasitic - linear coefficient",                 "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_2",            "Aux heater, boiler parasitic - quadratic coefficient",              "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par",              "Balance of plant parasitic power fraction",                         "MWe/MWcap",    "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_f",            "Balance of plant parasitic power fraction - mult frac",             "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_0",            "Balance of plant parasitic power fraction - const coeff",           "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_1",            "Balance of plant parasitic power fraction - linear coeff",          "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_2",            "Balance of plant parasitic power fraction - quadratic coeff",       "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "storage_bypass",       "Flag indicating whether the hot salt pump always runs w/ PB",       "none",         "",            "parasitics",     "*",                       "",                      "" },
    
	// Parasitics (type 228) inputs							     																	  
    {SSC_INPUT,         SSC_NUMBER,      "flow_from_storage",    "Flow rate from storage",                                            "kg/hr",       "",             "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "P_hot_tank",           "Hot tank heater parasitic power",                                   "MWe",         "",             "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "recirc_source",        "Recirculation heater control",                                      "none",        "",             "parasitics",     "*",                       "",                      "" },

	// Outputs here:

	var_info_invalid };

class cm_ty_sandbox : public compute_module
{
public:

	cm_ty_sandbox()
	{
		add_var_info(_cm_vtab_ty_sandbox);
	}

	bool relay_message(string &msg, double percent)
	{
		log(msg);
		return update(msg, percent);
	}

	void exec() throw(general_error)
	{
		std::string example_msg = "Does this work?";
		int example_type = C_csp_messages::NOTICE;

		C_csp_messages csp_messages;

		csp_messages.add_message(example_type, example_msg);

		int out_type = -1;
		std::string out_msg = "";
		while( csp_messages.get_message(&out_type, &out_msg))
		{
			double stophere = 1.23;
		}

		try
		{
			throw C_csp_exception("error message", "code location");
		}
		catch(C_csp_exception &csp_exception)
		{
			std::string error_message_out = csp_exception.m_error_message;
			std::string code_location_out = csp_exception.m_code_location;
		}

		C_pt_heliostatfield heliostatfield;

		heliostatfield.ms_params.m_run_type = (int) as_double("run_type");
		heliostatfield.ms_params.m_helio_width = as_double("helio_width");
		heliostatfield.ms_params.m_helio_height = as_double("helio_height");
		heliostatfield.ms_params.m_helio_optical_error = as_double("helio_optical_error");
		heliostatfield.ms_params.m_helio_active_fraction = as_double("helio_active_fraction");
		heliostatfield.ms_params.m_dens_mirror = as_double("dens_mirror");
		heliostatfield.ms_params.m_helio_reflectance = as_double("helio_reflectance");
		heliostatfield.ms_params.m_rec_absorptance = as_double("rec_absorptance");

		bool is_optimize = as_boolean("is_optimize");

		/*
		Any parameter that's dependent on the size of the solar field must be recalculated here
		if the optimization is happening within the cmod
		*/
		double H_rec, D_rec, rec_aspect, THT, A_sf;

		if( is_optimize )
		{
			//Run solarpilot right away to update values as needed
			solarpilot_invoke spi(this);
			spi.run();
			//AutoPilot_S *sapi = spi.GetSAPI();

			//Optimization iteration history
			vector<vector<double> > steps;
			vector<double> obj, flux;
			spi.opt.getOptimizationSimulationHistory(steps, obj, flux);
			int nr = steps.size();
			int nc = steps.front().size() + 2;
			ssc_number_t *ssc_hist = allocate("opt_history", nr, nc);
			for( size_t i = 0; i<nr; i++ ){

				for( size_t j = 0; j<steps.front().size(); j++ )
					ssc_hist[i*nc + j] = steps.at(i).at(j);
				ssc_hist[i*nc + nc - 2] = obj.at(i);
				ssc_hist[i*nc + nc - 1] = flux.at(i);

			}
		
			//receiver calculations
			H_rec = spi.recs.front().height;
			rec_aspect = spi.recs.front().aspect;
			THT = spi.layout.h_tower;
			//update heliostat position table
			nr = (int)spi.layout.heliostat_positions.size();
			ssc_number_t *ssc_hl = allocate("helio_positions", nr, 2);
			for( int i = 0; i<nr; i++ ){
				ssc_hl[i * 2] = (ssc_number_t)spi.layout.heliostat_positions.at(i).location.x;
				ssc_hl[i * 2 + 1] = (ssc_number_t)spi.layout.heliostat_positions.at(i).location.y;
			}

			A_sf = as_double("helio_height") * as_double("helio_width") * as_double("dens_mirror") * (double)nr;

			//update piping length for parasitic calculation
			double piping_length = THT * as_double("csp.pt.par.piping_length_mult") + as_double("csp.pt.par.piping_length_const");

			//update assignments for cost model
			assign("H_rec", var_data((ssc_number_t)H_rec));
			assign("rec_height", var_data((ssc_number_t)H_rec));
			assign("rec_aspect", var_data((ssc_number_t)rec_aspect));
			assign("D_rec", var_data((ssc_number_t)(H_rec / rec_aspect)));
			assign("THT", var_data((ssc_number_t)THT));
			assign("h_tower", var_data((ssc_number_t)THT));
			assign("A_sf", var_data((ssc_number_t)A_sf));
			assign("Piping_length", var_data((ssc_number_t)piping_length));
		
			//Update the total installed cost
			double total_direct_cost = 0.;
			double A_rec;
			switch( spi.recs.front().type )
			{
			case sp_receiver::TYPE::CYLINDRICAL:
			{
												   double h = spi.recs.front().height;
												   double d = h / spi.recs.front().aspect;
												   A_rec = h*d*3.1415926;
												   break;
			}
			case sp_receiver::TYPE::CAVITY:
			case sp_receiver::TYPE::FLAT:
				double h = spi.recs.front().height;
				double w = h / spi.recs.front().aspect;
				A_rec = h*w;
				break;
			}
			double receiver = as_double("rec_ref_cost")*pow(A_rec / as_double("rec_ref_area"), as_double("rec_cost_exp"));     //receiver cost

			//storage cost
			double storage = as_double("q_pb_design")*as_double("tshours")*as_double("tes_spec_cost")*1000.;

			//power block + BOP
			double P_ref = as_double("P_ref") * 1000.;  //kWe
			double power_block = P_ref * (as_double("plant_spec_cost") + as_double("bop_spec_cost")); //$/kWe --> $

			//site improvements
			double site_improvements = A_sf * as_double("site_spec_cost");

			//heliostats
			double heliostats = A_sf * as_double("heliostat_spec_cost");

			//fixed cost
			double cost_fixed = as_double("cost_sf_fixed");

			//fossil
			double fossil = P_ref * as_double("fossil_spec_cost");

			//tower cost
			double tower = as_double("tower_fixed_cost") * exp(as_double("tower_exp") * (THT + 0.5*(-H_rec + as_double("helio_height"))));

			//---- total direct cost -----
			total_direct_cost = (1. + as_double("contingency_rate") / 100.) * (
				site_improvements + heliostats + power_block +
				cost_fixed + storage + fossil + tower + receiver);
			//-----

			//land area
			double land_area = spi.layout.land_area * as_double("csp.pt.sf.land_overhead_factor") + as_double("csp.pt.sf.fixed_land_area");

			//EPC
			double cost_epc =
				as_double("csp.pt.cost.epc.per_acre") * land_area
				+ as_double("csp.pt.cost.epc.percent") * total_direct_cost / 100.
				+ P_ref * 1000. * as_double("csp.pt.cost.epc.per_watt")
				+ as_double("csp.pt.cost.epc.fixed");

			//PLM
			double cost_plm =
				as_double("csp.pt.cost.plm.per_acre") * land_area
				+ as_double("csp.pt.cost.plm.percent") * total_direct_cost / 100.
				+ P_ref * 1000. * as_double("csp.pt.cost.plm.per_watt")
				+ as_double("csp.pt.cost.plm.fixed");

			//sales tax
			//return ${csp.pt.cost.sales_tax.value}/100*${total_direct_cost}*${csp.pt.cost.sales_tax.percent}/100; };
			double cost_sales_tax = as_double("sales_tax_rate") / 100. * total_direct_cost * as_double("sales_tax_frac") / 100.;

			//----- indirect cost
			double total_indirect_cost = cost_epc + cost_plm + cost_sales_tax;

			//----- total installed cost!
			double total_installed_cost = total_direct_cost + total_indirect_cost;
			assign("total_installed_cost", var_data((ssc_number_t)total_installed_cost));

		}
		else
		{
			H_rec = as_double("H_rec");
			rec_aspect = as_double("rec_aspect");
			THT = as_double("THT");
			A_sf = as_double("A_sf");
		}

		D_rec = H_rec / rec_aspect;
		
		heliostatfield.ms_params.m_rec_height = as_double("rec_height");
		heliostatfield.ms_params.m_rec_aspect = as_double("rec_aspect");
		heliostatfield.ms_params.m_h_tower = as_double("h_tower");
		heliostatfield.ms_params.m_rec_hl_perm2 = as_double("rec_hl_perm2");
		heliostatfield.ms_params.m_q_design = as_double("Q_rec_des");
		heliostatfield.ms_params.m_dni_des = as_double("dni_des");
		heliostatfield.ms_params.m_weather_file = as_string("solar_resource_file");
		heliostatfield.ms_params.m_land_bound_type = (int) as_double("land_bound_type");
		heliostatfield.ms_params.m_land_max = as_double("land_max");
		heliostatfield.ms_params.m_land_min = as_double("land_min");
		heliostatfield.ms_params.m_p_start = as_double("p_start");
		heliostatfield.ms_params.m_p_track = as_double("p_track");
		heliostatfield.ms_params.m_hel_stow_deploy = as_double("hel_stow_deploy");
		heliostatfield.ms_params.m_v_wind_max = as_double("v_wind_max");
		heliostatfield.ms_params.m_n_flux_x = (int) as_double("n_flux_x");
		heliostatfield.ms_params.m_n_flux_y = (int) as_double("n_flux_y");
		heliostatfield.ms_params.m_c_atm_0 = as_double("c_atm_0");
		heliostatfield.ms_params.m_c_atm_1 = as_double("c_atm_1");
		heliostatfield.ms_params.m_c_atm_2 = as_double("c_atm_2");
		heliostatfield.ms_params.m_c_atm_3 = as_double("c_atm_3");
		heliostatfield.ms_params.m_n_facet_x = (int) as_double("n_facet_x");
		heliostatfield.ms_params.m_n_facet_y = (int) as_double("n_facet_y");
		heliostatfield.ms_params.m_focus_type = (int) as_double("focus_type");
		heliostatfield.ms_params.m_cant_type = (int) as_double("cant_type");
		heliostatfield.ms_params.m_n_flux_days = (int) as_double("n_flux_days");
		heliostatfield.ms_params.m_delta_flux_hrs = (int) as_double("delta_flux_hrs");

		int run_type = heliostatfield.ms_params.m_run_type;

		if( run_type == 1 )
		{
			heliostatfield.ms_params.m_helio_positions = as_matrix("helio_positions");
		}
		else if( run_type == 2 )
		{
			heliostatfield.ms_params.m_eta_map = as_matrix("eta_map");
			heliostatfield.ms_params.m_flux_positions = as_matrix("flux_positions");
			heliostatfield.ms_params.m_flux_maps = as_matrix("flux_maps");
		}
		else
		{
			string msg = util::format("SSC INPUT 'run_type' must be set to either 1 or 2. Its input value is %d", run_type);

			throw exec_error("MSPT CSP Solver", msg);
		}

		// Set callback information
		heliostatfield.mf_callback = ssc_mspt_solarpilot_callback;
		heliostatfield.m_cdata = (void*)this;

		// Try running pt heliostat init() call just for funsies
			// What happens when no callback to reference?
		heliostatfield.init();


		//// *********************************************************
		//// *********************************************************
		//// *********************************************************
		////      Now set Type 222 parameters
		//// *********************************************************
		//// *********************************************************
		//// *********************************************************
		C_mspt_receiver_222 receiver;
		receiver.m_n_panels = as_double("N_panels");
		receiver.m_d_rec = D_rec;
		receiver.m_h_rec = H_rec;
		receiver.m_h_tower = THT;
		receiver.m_od_tube = as_double("d_tube_out");
		receiver.m_th_tube = as_double("th_tube");
		receiver.m_mat_tube = as_double("mat_tube");
		receiver.m_field_fl = (int) as_double("rec_htf");
		receiver.m_field_fl_props = as_matrix("field_fl_props");
		receiver.m_flow_type = as_double("Flow_type");
		receiver.m_epsilon = as_double("epsilon");
		receiver.m_hl_ffact = as_double("hl_ffact");
		receiver.m_T_htf_hot_des = as_double("T_htf_hot_des");
		receiver.m_T_htf_cold_des = as_double("T_htf_cold_des");
		receiver.m_f_rec_min = as_double("f_rec_min");
		receiver.m_q_rec_des = as_double("Q_rec_des");
		receiver.m_rec_su_delay = as_double("rec_su_delay");
		receiver.m_rec_qf_delay = as_double("rec_qf_delay");
		receiver.m_m_dot_htf_max = as_double("m_dot_htf_max");
		receiver.m_A_sf = A_sf;
		receiver.m_n_flux_x = as_double("n_flux_x");
		receiver.m_n_flux_y = as_double("n_flux_y");

		// Optional ISCC stuff...
		receiver.init();



		//C_csp_weatherreader weather;

		//weather.set_csp_component_value_ssc_string("file_name", as_string("solar_resource_file"));
		//weather.set_csp_component_value_ssc_double("track_mode", 0.0);
		//weather.set_csp_component_value_ssc_double("tilt", 0.0);
		//weather.set_csp_component_value_ssc_double("azimuth", 0.0);

		////weather.init();

		//C_csp_no_storage thermal_storage;

		//C_csp_indirect_Rankine_224 power_cycle;

		//C_csp_solver csp_solver;

		//csp_solver.setup_technology_model(&weather, &solar_field, &thermal_storage, &power_cycle);

		//csp_solver.timeseries_simulation();

		//log("Solar Field Initialization was incredibly successful");

		//solar_field.set_csp_component_value_ssc_double("run_type", as_double("run_type"));
		//solar_field.set_csp_component_value_ssc_double("helio_width", as_double("helio_width"));
		//
		//size_t l_ffrac = 0;
		//
		//solar_field.set_csp_component_value_ssc_array("ffrac", as_array("ffrac", &l_ffrac), l_ffrac);
		//
		//size_t n_rows = 0;
		//size_t n_cols = 0;
		//ssc_number_t *p_matrix = as_matrix("field_fl_props", &n_rows, &n_cols);
		//
		//n_rows = 0;
		//n_cols = 0;
		//solar_field.set_csp_component_value_ssc_matrix("field_fl_props", as_matrix("field_fl_props", &n_rows, &n_cols), n_rows, n_cols);
		//
		//solar_field.set_csp_component_value_ssc_string("weather_file", as_string("solar_resource_file"));
		//
		
		//
		//double check_end = 1.23;



	}
};

static bool ssc_mspt_solarpilot_callback( simulation_info *siminfo, void *data )
{
	cm_ty_sandbox *cm = static_cast<cm_ty_sandbox*> (data);
	if( !cm )
		false;
	float simprogress = (float)siminfo->getCurrentSimulation() / (float)(max(siminfo->getTotalSimulationCount(), 1));

	return cm->relay_message(*siminfo->getSimulationNotices(), simprogress*100.0f);
}

DEFINE_MODULE_ENTRY(ty_sandbox, "trying out code", 1)
