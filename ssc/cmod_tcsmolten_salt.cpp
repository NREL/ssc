#include "core.h"

// for adjustment factors
#include "common.h"

// solarpilot header files
#include "AutoPilot_API.h"
#include "SolarField.h"
#include "IOUtil.h"
#include "csp_common.h"

// Can probably delete these headers later...
#include "csp_solver_util.h"
#include "csp_solver_core.h"
#include "csp_solver_pt_heliostatfield.h"
#include "csp_solver_mspt_receiver_222.h"
#include "csp_solver_mspt_collector_receiver.h"
#include "csp_solver_pc_Rankine_indirect_224.h"
#include "csp_solver_two_tank_tes.h"
#include "csp_solver_tou_block_schedules.h"


static bool ssc_mspt_solarpilot_callback(simulation_info *siminfo, void *data);

static bool ssc_mspt_sim_progress(void *data, double percent, C_csp_messages *csp_messages, float time_sec);

static var_info _cm_vtab_tcsmolten_salt[] = {
	/*   VARTYPE           DATATYPE         NAME                           LABEL                                UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "solar_resource_file",  "local weather file path",                                           "",             "",            "Weather",        "*",                       "LOCAL_FILE",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",      "Nameplate capacity",                                                "kWe",          "",            "molten salt tower", "*",                    "",   "" },

    // TOU													     																	  
    { SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",     "12x24 CSP operation Time-of-Use Weekday schedule",                  "-",             "",            "tou",            "*",                       "",                     "" }, 
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",     "12x24 CSP operation Time-of-Use Weekend schedule",                  "-",             "",            "tou",            "*",                       "",                     "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "is_dispatch",          "Allow dispatch optimization?",  /*TRUE=1*/                          "-",             "",            "tou",            "?=0"                      "",                     "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_horizon",         "Time horizon for dispatch optimization",                            "hour",         "",            "tou",            "?=48"                     "",                     "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_frequency",       "Frequency for dispatch optimization calculations",                  "hour",         "",            "tou",            "?=24"                     "",                     "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_max_iter",        "Max. no. dispatch optimization iterations",                         "-",             "",            "tou",            "?=10000"                  "",                     "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_timeout",         "Max. dispatch optimization solve duration",                         "s",            "",            "tou",            "?=5"                      "",                     "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_mip_gap",         "Dispatch optimization solution tolerance",                          "-",             "",            "tou",            "?=0.055"                  "",                     "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_spec_bb",         "Dispatch optimization B&B heuristic",                               "-",             "",            "tou",            "?=-1"                    "",                     "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_spec_presolve",   "Dispatch optimization presolve heuristic",                          "-",             "",            "tou",            "?=-1"                    "",                     "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_spec_scaling",    "Dispatch optimization scaling heuristic",                           "-",             "",            "tou",            "?=-1"                    "",                     "" }, 
    

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
    { SSC_INPUT,        SSC_NUMBER,      "eta_pump",             "Receiver HTF pump efficiency",                                      "",             "",            "receiver",       "*",                       "",                      "" },
													     																	  
    // Cavity Receiver (type 232) specific parameters		     																	  
    { SSC_INPUT,        SSC_NUMBER,      "rec_d_spec",           "Receiver aperture width",                                           "m",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_rec_panel",          "Height of a receiver panel",                                        "m",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_lip",                "Height of upper lip of cavity",                                     "m",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_angle",            "Section of the cavity circle covered in panels",                    "deg",          "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "conv_model",           "Type of convection model (1=Clausing, 2=Siebers/Kraabel)",          "-",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "eps_wavelength",       "Matrix containing wavelengths, active & passive surface eps",       "-",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "conv_coupled",         "1=coupled, 2=uncoupled",                                            "-",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "conv_forced",          "1=forced (use wind), 0=natural",                                    "-",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_wind_meas",          "Height at which wind measurements are given",                       "m",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "conv_wind_dir",        "Wind direction dependent forced convection 1=on 0=off",             "-",            "",            "cavity_receiver","*",                       "",                      "" },
															     																	  
															     																	  															     																	  
    // Controller (type 251) parameters						     																	  
    { SSC_INPUT,        SSC_NUMBER,      "field_fluid",          "Material number for the collector field",                           "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tshours",              "Equivalent full-load thermal storage hours",                        "hr",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_max_aux",            "Max heat rate of auxiliary heater",                                 "MWt",          "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_set_aux",            "Aux heater outlet temp set point",                                  "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "csp.pt.tes.init_hot_htf_percent", "Initial fraction of avail. vol that is hot",             "%",            "",            "controller",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "h_tank",               "Total height of tank (height of HTF when tank is full",             "m",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank_min",           "Minimum allowable HTF height in storage tank",                      "m",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "u_tank",               "Loss coefficient from the tank",                                    "W/m2-K",       "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_pairs",           "Number of equivalent tank pairs",                                   "-",            "",            "controller",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_Thtr",       "Minimum allowable cold tank HTF temp",                              "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_Thtr",        "Minimum allowable hot tank HTF temp",                               "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_max_heat",    "Rated heater capacity for hot tank heating",                        "MW",           "",            "controller",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cold_tank_max_heat",   "Rated heater capacity for cold tank heating",                       "MW",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_field_in_des",       "Field design inlet temperature",                                    "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_field_out_des",      "Field design outlet temperature",                                   "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_pb_design",          "Design heat input to power block",                                  "MWt",          "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "W_pb_design",          "Rated plant capacity",                                              "MWe",          "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_max_frac",       "Maximum turbine over design operation fraction",                    "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_cutoff_frac",    "Minimum turbine operation fraction before shutdown",                "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "solarm",               "Solar Multiple",                                                    "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_pump_coef",         "Pumping power to move 1kg of HTF through PB loop",                  "kW/kg",        "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_startup",            "Startup temperature",                                               "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fossil_mode",          "Fossil backup mode 1=Normal 2=Topping",                             "-",            "",            "controller",     "*",                       "",                      "" },
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
	{ SSC_INPUT,        SSC_NUMBER,      "eta_lhv",              "Fossil fuel lower heating value - Thermal power generated per unit fuel",   "MW/MMBTU",     "",    "controller",     "*",                       "",                      "" },													     																	  
															     																	  
    // Powerblock (type 224) parameters						     																	  
    { SSC_INPUT,        SSC_NUMBER,      "P_ref",                "Reference output electric power at design condition",               "MW",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_ref",              "Reference conversion efficiency at design condition",               "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_hot_ref",        "Reference HTF inlet temperature at design",                         "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_cold_ref",       "Reference HTF outlet temperature at design",                        "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",            "Reference condenser cooling water inlet/outlet T diff",             "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",            "Reference ambient temperature at design point",                     "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil",               "Boiler operating pressure",                                         "bar",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                   "Flag for using dry cooling or wet cooling system",                  "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_time",         "Time needed for power block startup",                               "hr",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",         "Fraction of design thermal power needed for startup",               "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",           "Cooling tower approach temperature",                                "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",            "ITD at design for dry system",                                      "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",         "Condenser pressure ratio",                                          "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",           "Power block blowdown steam fraction ",                              "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",           "Minimum condenser pressure",                                        "inHg",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",             "Number of part-load increments for the heat rejection system",      "none",         "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",                 "Fraction indicating wet cooling use for hybrid system",             "none",         "",            "powerblock",     "*",                       "",                      "" },
															     																	  
	// sCO2 Powerblock (type 424) inputs
	{ SSC_INPUT,        SSC_NUMBER,      "pc_config",            "0: Steam Rankine (224), 1: sCO2 Recompression (424)",               "none",         "",            "powerblock",     "?=0",                       "INTEGER",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_c",                "Isentropic efficiency of compressor(s)",                            "none",         "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_t",                "Isentropic efficiency of turbine",							      "none",         "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "P_high_limit",         "Upper pressure limit in cycle",								      "MPa",          "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "deltaT_PHX",           "Design temperature difference in PHX",						      "C",	          "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fan_power_perc_net",   "% of net cycle output used for fan power at design",			      "%",	          "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "elev",                 "Site elevation",                                                    "m",            "",            "powerblock",     "*",                       "",                      "" },

	// Parasitics (type 228) parameters						     																	  
    {SSC_INPUT,         SSC_NUMBER,      "piping_loss",          "Thermal loss per meter of piping",                                  "Wt/m",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "piping_length",        "Total length of exposed piping",                                    "m",            "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "piping_length_mult",   "Piping length multiplier",                                          "",             "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "piping_length_const",  "Piping constant length",                                            "m",            "",            "parasitics",     "*",                       "",                      "" },
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

	// Financial inputs
	{ SSC_INPUT,        SSC_MATRIX,      "dispatch_sched_weekday", "12x24 PPA pricing Weekday schedule",                              "",             "",            "tou",            "*",                       "",                      "" }, 
	{ SSC_INPUT,        SSC_MATRIX,      "dispatch_sched_weekend", "12x24 PPA pricing Weekend schedule",                              "",             "",            "tou",            "*",                       "",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor1",     "Dispatch payment factor 1",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor2",     "Dispatch payment factor 2",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor3",     "Dispatch payment factor 3",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor4",     "Dispatch payment factor 4",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor5",     "Dispatch payment factor 5",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor6",     "Dispatch payment factor 6",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor7",     "Dispatch payment factor 7",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor8",     "Dispatch payment factor 8",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor9",     "Dispatch payment factor 9",	                                      "",             "",            "tou",            "*",						  "",                      "" },



	// ****************************************************************************************************************************************
	// Outputs here:
	// ****************************************************************************************************************************************
		// Simulation outputs
	{ SSC_OUTPUT,       SSC_ARRAY,       "time_hr",              "Time at end of timestep",                                      "hr",           "",            "Solver",         "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "solzen",               "Resource Solar Zenith",                                        "deg",          "",            "weather",        "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "beam",                 "Resource Beam normal irradiance",                              "W/m2",         "",            "weather",        "*",                       "",           "" },
	
		// Collector-receiver outputs
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_field",            "Field optical efficiency",                                     "",             "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "defocus",              "Field optical focus fraction",                                 "",             "",            "Controller",     "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_therm",            "Rec. thermal efficiency",                                      "",             "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_thermal",            "Rec. thermal power to HTF less piping loss",                   "MWt",          "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_rec",            "Rec. mass flow rate",                                          "kg/hr",        "",            "CR",             "*",                       "",           "" },	
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_startup",            "Rec. startup thermal energy consumed",                         "MWt",          "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_rec_in",             "Rec. HTF inlet temperature",                                   "C",            "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_rec_out",            "Rec. HTF outlet temperature",                                  "C",            "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_piping_losses",      "Tower piping losses",                                          "MWt",          "",            "CR",             "*",                       "",           "" },
	
		// Power cycle outputs
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta",                  "PC efficiency: gross",                                         "",             "",            "PC",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_pb",		         "PC input energy",                                              "MWt",          "",            "PC",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_pc",             "PC HTF mass flow rate",                                        "kg/hr",        "",            "PC",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_pc_startup",         "PC startup thermal energy",                                    "MWht",       "",            "PC",             "*",                       "",           "" },	
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_startup",     "PC startup thermal power",                                     "MWt",          "",            "PC",             "*",                       "",           "" },	
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_cycle",              "PC electrical power output: gross",                            "MWe",          "",            "PC",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_pc_in",              "PC HTF inlet temperature",                                     "C",            "",            "PC",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_pc_out",             "PC HTF outlet temperature",                                    "C",            "",            "PC",             "*",                       "",           "" },
	
		// Thermal energy storage outputs
	{ SSC_OUTPUT,       SSC_ARRAY,       "tank_losses",          "TES thermal losses",                                           "MWt",          "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_heater",             "TES freeze protection power",                                  "MWe",          "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tes_hot",            "TES hot temperature",                                          "C",            "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tes_cold",           "TES cold temperature",                                         "C",            "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_dc_tes",             "TES discharge thermal power",                                  "MWt",          "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_ch_tes",             "TES charge thermal power",                                     "MWt",          "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "e_ch_tes",             "TES charge state",                                             "MWht",         "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_tes_dc",         "TES discharge mass flow rate",                                 "kg/hr",        "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_tes_ch",         "TES charge mass flow rate",                                    "kg/hr",        "",            "TES",            "*",                       "",           "" },
	
		// Parasitics outputs
	{ SSC_OUTPUT,       SSC_ARRAY,       "pparasi",              "Parasitic power heliostat drives",                             "MWe",          "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_tower_pump",         "Parasitic power receiver/tower HTF pump",                      "MWe",          "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "htf_pump_power",       "Parasitic power TES and Cycle HTF pump",                       "MWe",          "",            "PC-TES",         "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_cooling_tower_tot",  "Parasitic power condenser operation",                          "MWe",          "",            "PC",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_fixed",              "Parasitic power fixed load",                                   "MWe",          "",            "System",         "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_plant_balance_tot",  "Parasitic power generation-dependent load",                    "MWe",          "",            "System",         "*",                       "",           "" },
	
		// System outputs
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_out_net",            "Total electric power to grid",                                 "MWe",          "",            "System",         "*",                       "",           "" },
	
		// Controller outputs
	{ SSC_OUTPUT,       SSC_ARRAY,       "tou_value",            "CSP operating Time-of-use value",                              "",             "",            "Controller",    "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "pricing_mult",         "PPA price multiplier",                                         "",             "",            "Controller",    "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "n_op_modes",           "Operating modes in reporting timestep",                        "",             "",            "Solver",        "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "op_mode_1",            "1st operating mode",                                           "",             "",            "Solver",        "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "op_mode_2",            "2nd op. mode, if applicable",                                  "",             "",            "Solver",        "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "op_mode_3",            "3rd op. mode, if applicable",                                  "",             "",            "Solver",        "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_balance",        "Relative mass flow balance error",                             "",             "",            "Controller",     "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_balance",            "Relative energy balance error",                                "",             "",            "Controller",     "*",                       "",           "" },

    { SSC_OUTPUT,       SSC_ARRAY,       "disp_solve_state",     "Dispatch solver state",                                        "",             "",            "tou",            "*"                       "",            "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_solve_iter",      "Dispatch iterations count",                                    "",             "",            "tou",            "*"                       "",            "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_objective",       "Dispatch objective function value",                            "",             "",            "tou",            "*"                       "",            "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_obj_relax",       "Dispatch objective function - relaxed max",                    "",             "",            "tou",            "*"                       "",            "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_qsf_expected",    "Dispatch expected solar field available energy",               "MWt",          "",            "tou",            "*"                       "",            "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_qsfprod_expected","Dispatch expected solar field generation",                     "MWt",          "",            "tou",            "*"                       "",            "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_qsfsu_expected",  "Dispatch expected solar field startup enegy",                  "MWt",          "",            "tou",            "*"                       "",            "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_tes_expected",    "Dispatch expected TES charge level",                           "MWht",         "",            "tou",            "*"                       "",            "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_pceff_expected",  "Dispatch expected power cycle efficiency adj.",                "",             "",            "tou",            "*"                       "",            "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_thermeff_expected","Dispatch expected SF thermal efficiency adj.",                "",             "",            "tou",            "*"                       "",            "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_qpbsu_expected",  "Dispatch expected power cycle startup energy",                 "MWht",         "",            "tou",            "*"                       "",            "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_wpb_expected",    "Dispatch expected power generation",                           "MWe",          "",            "tou",            "*"                       "",            "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_rev_expected",    "Dispatch expected revenue factor",                             "",             "",            "tou",            "*"                       "",            "" }, 


			// These outputs correspond to the first csp-solver timestep in the reporting timestep.
			//     Subsequent csp-solver timesteps within the same reporting timestep are not tracked
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_sb",          "Thermal power for PC standby",                                 "MWt",          "",            "Controller",     "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_min",		 "Thermal power for PC min operation",		                     "MWt",			 "",            "Controller",	  "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_max",		 "Max thermal power to PC",						                 "MWt",			 "",            "Controller",	  "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_target",		 "Target thermal power to PC",							         "MWt",			 "",            "Controller",	  "*",                       "",           "" },	
	{ SSC_OUTPUT,       SSC_ARRAY,       "is_rec_su_allowed",	 "is receiver startup allowed",		                             "",			 "",            "Controller",	  "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "is_pc_su_allowed",	 "is power cycle startup allowed",	                             "",			 "",            "Controller",	  "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "is_pc_sb_allowed",	 "is power cycle standby allowed",	                             "",			 "",            "Controller",	  "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_dot_est_cr_su",		 "Estimate rec. startup thermal power",                          "MWt",			 "",            "Controller",	  "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_dot_est_cr_on",		 "Estimate rec. thermal power TO HTF",	                         "MWt",			 "",            "Controller",	  "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_dot_est_tes_dc",	 "Estimate max TES discharge thermal power",			         "MWt",			 "",            "Controller",	  "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_dot_est_tes_ch",	 "Estimate max TES charge thermal power",			             "MWt",			 "",            "Controller",	  "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "operating_modes_a",    "First 3 operating modes tried",                                "",             "",            "Solver",         "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "operating_modes_b",    "Next 3 operating modes tried",                                 "",             "",            "Solver",         "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "operating_modes_c",    "Final 3 operating modes tried",                                "",             "",            "Solver",         "*",                       "",           "" },
	


	// Annual single-value outputs
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",        "Annual total electric power to grid",                          "kWhe",       "",            "System",         "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_W_cycle_gross", "Electrical source - Power cycle gross output",                 "kWhe",       "",            "PC",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "conversion_factor",    "Gross to Net Conversion Factor",                               "%",            "",            "PostProcess",    "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "capacity_factor",      "Capacity factor",                                              "%",            "",            "PostProcess",    "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "kwh_per_kw",           "First year kWh/kW",                                            "kWh/kW",       "",            "",               "*",                       "",           "" },

	var_info_invalid };

class cm_tcsmolten_salt : public compute_module
{
public:

	cm_tcsmolten_salt()
	{
		add_var_info(_cm_vtab_tcsmolten_salt);
		add_var_info(vtab_adjustment_factors);
	} 

	bool relay_message(string &msg, double percent)
	{
		log(msg);
		return update(msg, percent);
	}

	void exec() throw(general_error)
	{

		// Logic to choose between steam and sco2 power cycle 
		bool is_steam_pc = true;
		int pb_tech_type = as_integer("pc_config");
		if( pb_tech_type == 1 )
		{
			pb_tech_type = 424;
			is_steam_pc = false;

			log("The sCO2 power cycle is not yet supported by the new CSP Solver and Dispatch Optimization models.\n", SSC_WARNING);

			return;
		}

		int tes_type = as_integer("tes_type");
		if( tes_type != 1 )
		{
			throw exec_error("MSPT CSP Solver", "Thermocline thermal energy storage is not yet supported by the new CSP Solver and Dispatch Optimization models.\n");
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
			double piping_length = THT * as_double("piping_length_mult") + as_double("piping_length_const");

			//update assignments for cost model
			assign("H_rec", var_data((ssc_number_t)H_rec));
			assign("rec_height", var_data((ssc_number_t)H_rec));
			assign("rec_aspect", var_data((ssc_number_t)rec_aspect));
			assign("D_rec", var_data((ssc_number_t)(H_rec / rec_aspect)));
			assign("THT", var_data((ssc_number_t)THT));
			assign("h_tower", var_data((ssc_number_t)THT));
			assign("A_sf", var_data((ssc_number_t)A_sf));
			assign("piping_length", var_data((ssc_number_t)piping_length));
		
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

		// Set parameters that were set with TCS defaults
		heliostatfield.ms_params.m_interp_nug = 0.0;
		heliostatfield.ms_params.m_interp_beta = 1.99;

		// Set callback information
		heliostatfield.mf_callback = ssc_mspt_solarpilot_callback;
		heliostatfield.m_cdata = (void*)this;

		// Try running pt heliostat init() call just for funsies
			// What happens when no callback to reference?
		//heliostatfield.init();


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

		// 8.10.2015 twn: add tower piping thermal losses to receiver performance
		receiver.m_pipe_loss_per_m = as_double("piping_loss");						//[Wt/m]
		receiver.m_pipe_length_add = as_double("piping_length_const");	//[m]
		receiver.m_pipe_length_mult = as_double("piping_length_mult");		//[-]

		receiver.m_n_flux_x = as_double("n_flux_x");
		receiver.m_n_flux_y = as_double("n_flux_y");

		receiver.m_T_salt_hot_target = as_double("T_salt_hot_target");
		receiver.m_eta_pump = as_double("eta_pump");
		receiver.m_night_recirc = 0;					// 8.15.15 twn: this is hardcoded for now - need to check that it is functioning correctly and reporting correct parasitics
		receiver.m_hel_stow_deploy = as_double("hel_stow_deploy");

		// Set parameters that were set with TCS defaults
		receiver.m_is_iscc = false;

		// Could add optional ISCC stuff...

		// Test mspt_receiver initialization
		//receiver.init();

		// Now try to instantiate mspt_collector_receiver
		C_csp_mspt_collector_receiver collector_receiver(heliostatfield, receiver);
		// Then try init() call here, which should call inits from both classes
		//collector_receiver.init();



		// Weather reader
		C_csp_weatherreader weather_reader;
		weather_reader.m_filename = as_string("solar_resource_file");
		weather_reader.m_trackmode = 0;
		weather_reader.m_tilt = 0.0;
		weather_reader.m_azimuth = 0.0;

		// Test weatherreader initialization
		//weather_reader.init();


		// Power cycle
		C_pc_Rankine_indirect_224 power_cycle;
		C_pc_Rankine_indirect_224::S_params *pc = &power_cycle.ms_params;
		pc->m_P_ref = as_double("P_ref");
		pc->m_eta_ref = as_double("eta_ref");
		pc->m_T_htf_hot_ref = as_double("T_htf_hot_ref");
		pc->m_T_htf_cold_ref = as_double("T_htf_cold_ref");
		pc->m_dT_cw_ref = as_double("dT_cw_ref");
		pc->m_T_amb_des = as_double("T_amb_des");
		pc->m_pc_fl = as_integer("rec_htf");					// power cycle HTF is same as receiver HTF
		pc->m_pc_fl_props = as_matrix("field_fl_props");
		pc->m_cycle_max_frac = as_double("cycle_max_frac");
		pc->m_cycle_cutoff_frac = as_double("cycle_cutoff_frac");
		pc->m_q_sby_frac = as_double("q_sby_frac");
		pc->m_P_boil = as_double("P_boil");
		pc->m_CT = as_integer("CT");
		pc->m_startup_time = as_double("startup_time");
		pc->m_startup_frac = as_double("startup_frac");
		pc->m_tech_type = 1;									// compute module is for MSPT, so hardcode tech type
		pc->m_T_approach = as_double("T_approach");
		pc->m_T_ITD_des = as_double("T_ITD_des");
		pc->m_P_cond_ratio = as_double("P_cond_ratio");
		pc->m_pb_bd_frac = as_double("pb_bd_frac");
		pc->m_P_cond_min = as_double("P_cond_min");
		pc->m_htf_pump_coef = as_double("pb_pump_coef");
		pc->m_n_pl_inc = as_integer("n_pl_inc");

		// ********************************************
		// Test U.D. power cycle
		// ********************************************
		pc->m_is_user_defined_pc = false;

			// Use Type224 Hardcoded values for T_HTF and m_dot
			// Can't switch between condenser pressure and T_amb...
					// (well, I guess we *could* by using the steam props...)
		pc->mc_T_htf_ind.resize(20,9);
		pc->mc_T_amb_ind.resize(3,9);
		pc->mc_m_dot_htf_ind.resize(20,9);
		pc->m_W_dot_cooling_des = pc->m_P_ref*0.02;		//[MW]
		pc->m_m_dot_water_des = 0.0;					//[kg/s]

		// Ok, so let's try assuming there is no dependence on T_amb
		pc->mc_T_amb_ind(0, 0) = -50.0;
		pc->mc_T_amb_ind(1, 0) = 20.0;
		pc->mc_T_amb_ind(2, 0) = 200.0;

		for( int i = 1; i < 9; i++ )
		{
			for( int j = 0; j < 3; j++ )
			{				
				pc->mc_T_amb_ind(j,i) = 1.0;
			}
		}

		// Also, let's hardcode all effects to 1.0, then redefine as necessary
		for( int i = 1; i < 9; i++ )
		{
			for( int j = 0; j < 20; j++ )
			{
				pc->mc_T_htf_ind(j, i) = 1.0;
				pc->mc_m_dot_htf_ind(j, i) = 1.0;
			}
		}

		// Now, let's tackle T_HTF
		// Need to re-dimensionalize the HTF temperature values...
		// From 'csp_solver_pc_Rankine_indirect_224.cpp': "double T_htf_hot_ND = (T_htf_hot - T_ref) / (T_htf_hot_ref - T_ref);"
		// So, what are T_ref and T_htf_hot_ref?, just run debug to figure this out... 
		//		T_ref = 584.15 K in default case
		//		T_htf_hot_ref = 847.15 K in default case
		double T_ref_table = 584.15;
		double T_htf_hot_ref_table = 847.15;
			// Temperature values, row 1
		double T_htf_hot_ND_table[20] = {0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316, 0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895, 0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474, 1.14737, 1.20000};

		// Get m_dot_ND values, row 7
		double m_dot_htf_ND_table[20] = { 0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842, 0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053, 0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263, 1.42632, 1.50000 };

		// Now get HTF temp main effect on Power, row 2
		double T_ME_on_Power_table[20] = {0.16759, 0.21750, 0.26932, 0.32275, 0.37743, 0.43300, 0.48910, 0.54545, 0.60181, 0.65815, 0.71431, 0.77018, 0.82541, 0.88019, 0.93444, 0.98886, 1.04378, 1.09890, 1.15425, 1.20982};
		// .. and Heat Input, row 3
		double T_ME_on_Heat_table[20] = {0.19656, 0.24969, 0.30325, 0.35710, 0.41106, 0.46497, 0.51869, 0.57215, 0.62529, 0.67822, 0.73091, 0.78333, 0.83526, 0.88694, 0.93838, 0.98960, 1.04065, 1.09154, 1.14230, 1.19294};

		// Now get m_dot_htf main effect on Power, row 8
		double m_dot_ME_on_Power_table[20] = {0.09403, 0.16542, 0.23861, 0.31328, 0.38901, 0.46540, 0.54203, 0.61849, 0.69437, 0.76928, 0.84282, 0.91458, 0.98470, 1.05517, 1.12536, 1.19531, 1.26502, 1.33450, 1.40376, 1.47282};
		// ... and Heat Input, row 9
		double m_dot_ME_on_Heat_table[20] = {0.10659, 0.18303, 0.25848, 0.33316, 0.40722, 0.48075, 0.55381, 0.62646, 0.69873, 0.77066, 0.84228, 0.91360, 0.98464, 1.05542, 1.12596, 1.19627, 1.26637, 1.33625, 1.40593, 1.47542};

		// So now we can calculate temp values in C...
		for( int i = 0; i < 20; i++ )
		{
			pc->mc_T_htf_ind(i,0) = T_htf_hot_ND_table[i] * (T_htf_hot_ref_table - T_ref_table) + T_ref_table - 273.15;
			pc->mc_m_dot_htf_ind(i,0) = m_dot_htf_ND_table[i];

			pc->mc_T_htf_ind(i,1) = T_ME_on_Power_table[i];
			pc->mc_T_htf_ind(i,3) = T_ME_on_Heat_table[i];

			pc->mc_m_dot_htf_ind(i,1) = m_dot_ME_on_Power_table[i];
			pc->mc_m_dot_htf_ind(i,3) = m_dot_ME_on_Heat_table[i];

			// Let's also say that cooling parasitics scale same as power...
			pc->mc_T_htf_ind(i,5) = T_ME_on_Power_table[i];
			pc->mc_m_dot_htf_ind(i,5) = m_dot_ME_on_Power_table[i];
		}


		//pc->mc_T_htf_ind(0,0) = 0.0;
		//pc->mc_T_htf_ind(1,0) = pc->m_T_htf_hot_ref;
		//pc->mc_T_htf_ind(2,0) = 1500.0;
		//
		//pc->mc_m_dot_htf_ind(0,0) = 0.0;
		//pc->mc_m_dot_htf_ind(1,0) = 1.0;
		//pc->mc_m_dot_htf_ind(2,0) = 1.5;
		
		// ********************************************
		// END Test U.D. power cycle
		// ********************************************



		size_t n_F_wc = -1;
		ssc_number_t *p_F_wc = as_array("F_wc", &n_F_wc);
		pc->m_F_wc.resize(n_F_wc, 0.0);
		for( int i = 0; i < n_F_wc; i++ )
			pc->m_F_wc[i] = (double) p_F_wc[i];

		// Test power cycle initialization
		//power_cycle.init();


		// Thermal energy storage 
		C_csp_two_tank_tes storage;
		C_csp_two_tank_tes::S_params *tes = &storage.ms_params;
		tes->m_field_fl = as_integer("field_fluid");
		tes->m_field_fl_props = as_matrix("field_fl_props");
		tes->m_tes_fl = as_integer("field_fluid");
		tes->m_tes_fl_props = as_matrix("field_fl_props");
		tes->m_is_hx = false;									// MSPT assumes direct storage, so no user input required here: hardcode = false
		tes->m_W_dot_pc_design = as_double("P_ref");		//[MWe]
		tes->m_eta_pc = as_double("eta_ref");				//[-]
		tes->m_solarm = as_double("solarm");
		tes->m_ts_hours = as_double("tshours");
		tes->m_h_tank = as_double("h_tank");
		tes->m_u_tank = as_double("u_tank");
		tes->m_tank_pairs = as_integer("tank_pairs");
		tes->m_hot_tank_Thtr = as_double("hot_tank_Thtr");
		tes->m_hot_tank_max_heat = as_double("hot_tank_max_heat");
		tes->m_cold_tank_Thtr = as_double("cold_tank_Thtr");
		tes->m_cold_tank_max_heat = as_double("cold_tank_max_heat");
		tes->m_dt_hot = 0.0;								// MSPT assumes direct storage, so no user input here: hardcode = 0.0
		tes->m_T_field_in_des = as_double("T_field_in_des");
		tes->m_T_field_out_des = as_double("T_field_out_des");
		tes->m_T_tank_hot_ini = as_double("T_field_out_des");
		tes->m_T_tank_cold_ini = as_double("T_field_in_des");
		tes->m_h_tank_min = as_double("h_tank_min");
		tes->m_f_V_hot_ini = as_double("csp.pt.tes.init_hot_htf_percent");
		tes->m_htf_pump_coef = as_double("pb_pump_coef");

		// TOU parameters
		C_csp_tou_block_schedules tou;
		C_csp_tou_block_schedules::S_params *tou_params = &tou.ms_params;
		tou_params->mc_csp_ops.mc_weekdays = as_matrix("weekday_schedule");
		tou_params->mc_csp_ops.mc_weekends = as_matrix("weekend_schedule");
		tou_params->mc_pricing.mc_weekdays = as_matrix("dispatch_sched_weekday");
		tou_params->mc_pricing.mc_weekends = as_matrix("dispatch_sched_weekend");
        tou.mc_dispatch_params.m_dispatch_optimize = as_boolean("is_dispatch");
        tou.mc_dispatch_params.m_optimize_frequency = as_integer("disp_frequency");
        tou.mc_dispatch_params.m_optimize_horizon = as_integer("disp_horizon");
        tou.mc_dispatch_params.m_max_iterations = as_integer("disp_max_iter");
        tou.mc_dispatch_params.m_solver_timeout = as_double("disp_timeout");
        tou.mc_dispatch_params.m_mip_gap = as_double("disp_mip_gap");
        tou.mc_dispatch_params.m_presolve_type = as_integer("disp_spec_presolve");
        tou.mc_dispatch_params.m_bb_type = as_integer("disp_spec_bb");
        tou.mc_dispatch_params.m_scaling_type = as_integer("disp_spec_scaling");
		tou.mc_dispatch_params.m_is_block_dispatch = ! tou.mc_dispatch_params.m_dispatch_optimize;      //mw
		tou.mc_dispatch_params.m_use_rule_1 = true;
		tou.mc_dispatch_params.m_standby_off_buffer = 2.0;
		tou.mc_dispatch_params.m_use_rule_2 = false;
		tou.mc_dispatch_params.m_q_dot_rec_des_mult = -1.23;
		tou.mc_dispatch_params.m_f_q_dot_pc_overwrite = -1.23;


		size_t n_f_turbine = -1;
		ssc_number_t *p_f_turbine = as_array("tslogic_c", &n_f_turbine);
		tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC].resize(n_f_turbine,0.0);
		//tou_params->mv_t_frac.resize(n_f_turbine, 0.0);
		for( int i = 0; i < n_F_wc; i++ )
			tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC][i] = (double)p_f_turbine[i];

		tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(9,0.0);
		tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][0] = as_double("dispatch_factor1");
		tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][1] = as_double("dispatch_factor2");
		tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][2] = as_double("dispatch_factor3");
		tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][3] = as_double("dispatch_factor4");
		tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][4] = as_double("dispatch_factor5");
		tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][5] = as_double("dispatch_factor6");
		tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][6] = as_double("dispatch_factor7");
		tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][7] = as_double("dispatch_factor8");
		tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][8] = as_double("dispatch_factor9");
		
		// System parameters
		C_csp_solver::S_csp_system_params system;
		system.m_pb_fixed_par = as_double("pb_fixed_par");
		system.m_bop_par = as_double("bop_par");
		system.m_bop_par_f = as_double("bop_par_f");
		system.m_bop_par_0 = as_double("bop_par_0");
		system.m_bop_par_1 = as_double("bop_par_1");
		system.m_bop_par_2 = as_double("bop_par_2");

		// Instantiate Solver
		C_csp_solver csp_solver(weather_reader, collector_receiver, power_cycle, storage, tou, system);


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
				log(out_msg);
			}

			log(csp_exception.m_error_message, SSC_ERROR, -1.0);

			return;
		}

		// Set up ssc output arrays
		// Set steps per hour
        double nhoursim = 8760.;          //[hr] Number of hours to simulate
		C_csp_solver::S_sim_setup sim_setup;
		sim_setup.m_sim_time_start = 0.0;			//[s] starting first hour of year
		sim_setup.m_sim_time_end = nhoursim*3600.; //8760.0*3600.0;	//[s] full year simulation

		int steps_per_hour = 1;		//[-]
		int n_steps_fixed = steps_per_hour * 8760;	//[-]
		sim_setup.m_report_step = 3600.0 / (double)steps_per_hour;	//[s]

		float **ptr_array = new float*[C_csp_solver::E_reported_outputs::N_END];
		float **post_proc_array = new float*[C_csp_solver::E_post_proc_outputs::N_END_POST_PROC];

		for( int i = 0; i < C_csp_solver::E_post_proc_outputs::N_END_POST_PROC; i++)
		{
			post_proc_array[i] = 0;
		}

		post_proc_array[C_csp_solver::E_post_proc_outputs::PC_Q_STARTUP] = allocate("q_pc_startup", n_steps_fixed);

		for( int i = 0; i < C_csp_solver::E_reported_outputs::N_END; i++ )
		{
			ptr_array[i] = 0;
		}

			// Simulation outputs
		ptr_array[C_csp_solver::E_reported_outputs::TIME_FINAL] = allocate("time_hr", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::SOLZEN] = allocate("solzen", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::BEAM] = allocate("beam", n_steps_fixed);

			// Collector-receiver outputs
		ptr_array[C_csp_solver::E_reported_outputs::CR_OPT_ETA] = allocate("eta_field", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::CR_DEFOCUS] = allocate("defocus", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::REC_ETA_THERMAL] = allocate("eta_therm", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::REC_Q_DOT] = allocate("Q_thermal", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::REC_M_DOT] = allocate("m_dot_rec", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::REC_Q_DOT_STARTUP] = allocate("q_startup", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::REC_T_IN] = allocate("T_rec_in", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::REC_T_OUT] = allocate("T_rec_out", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::CR_Q_DOT_PIPING_LOSS] = allocate("q_piping_losses", n_steps_fixed);

			// Power cycle outputs
		ptr_array[C_csp_solver::E_reported_outputs::PC_ETA_THERMAL] = allocate("eta", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::PC_Q_DOT] = allocate("q_pb", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::PC_M_DOT] = allocate("m_dot_pc", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::PC_Q_DOT_STARTUP] = allocate("q_dot_pc_startup", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::PC_W_DOT] = allocate("P_cycle", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::PC_T_IN] = allocate("T_pc_in", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::PC_T_OUT] = allocate("T_pc_out", n_steps_fixed);

			// Thermal energy storage outputs
		ptr_array[C_csp_solver::E_reported_outputs::TES_Q_DOT_LOSS] = allocate("tank_losses", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::TES_W_DOT_HEATER] = allocate("q_heater", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::TES_T_HOT] = allocate("T_tes_hot", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::TES_T_COLD] = allocate("T_tes_cold", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::TES_Q_DOT_DC] = allocate("q_dc_tes", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::TES_Q_DOT_CH] = allocate("q_ch_tes", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::TES_E_CH_STATE] = allocate("e_ch_tes", n_steps_fixed);
        ptr_array[C_csp_solver::E_reported_outputs::TES_M_DOT_DC] = allocate("m_dot_tes_dc", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::TES_M_DOT_CH] = allocate("m_dot_tes_ch", n_steps_fixed);

			// Parasitics outputs
		ptr_array[C_csp_solver::E_reported_outputs::COL_W_DOT_TRACK] = allocate("pparasi", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::CR_W_DOT_PUMP] = allocate("P_tower_pump", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::SYS_W_DOT_PUMP] = allocate("htf_pump_power", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::PC_W_DOT_COOLING] = allocate("P_cooling_tower_tot", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::SYS_W_DOT_FIXED] = allocate("P_fixed", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::SYS_W_DOT_BOP] = allocate("P_plant_balance_tot", n_steps_fixed);

			// System outputs
		ptr_array[C_csp_solver::E_reported_outputs::W_DOT_NET] = allocate("P_out_net", n_steps_fixed);
		
			// Controller outputs
		ptr_array[C_csp_solver::E_reported_outputs::TOU_PERIOD] = allocate("tou_value", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::PRICING_MULT] = allocate("pricing_mult", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::N_OP_MODES] = allocate("n_op_modes", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::OP_MODE_1] = allocate("op_mode_1", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::OP_MODE_2] = allocate("op_mode_2", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::OP_MODE_3] = allocate("op_mode_3", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::ERR_M_DOT] = allocate("m_dot_balance", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::ERR_Q_DOT] = allocate("q_balance", n_steps_fixed);


		ptr_array[C_csp_solver::E_reported_outputs::PC_Q_DOT_SB] = allocate("q_dot_pc_sb", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::PC_Q_DOT_MIN] = allocate("q_dot_pc_min", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::PC_Q_DOT_MAX] = allocate("q_dot_pc_max", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::PC_Q_DOT_TARGET] = allocate("q_dot_pc_target", n_steps_fixed);

		ptr_array[C_csp_solver::E_reported_outputs::CTRL_IS_REC_SU] = allocate("is_rec_su_allowed", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::CTRL_IS_PC_SU] = allocate("is_pc_su_allowed", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::CTRL_IS_PC_SB] = allocate("is_pc_sb_allowed", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::EST_Q_DOT_CR_SU] = allocate("q_dot_est_cr_su", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::EST_Q_DOT_CR_ON] = allocate("q_dot_est_cr_on", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::EST_Q_DOT_DC] = allocate("q_dot_est_tes_dc", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::EST_Q_DOT_CH] = allocate("q_dot_est_tes_ch", n_steps_fixed);

		ptr_array[C_csp_solver::E_reported_outputs::CTRL_OP_MODE_SEQ_A] = allocate("operating_modes_a", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::CTRL_OP_MODE_SEQ_B] = allocate("operating_modes_b", n_steps_fixed);
		ptr_array[C_csp_solver::E_reported_outputs::CTRL_OP_MODE_SEQ_C] = allocate("operating_modes_c", n_steps_fixed);

        ptr_array[C_csp_solver::E_reported_outputs::DISPATCH_SOLVE_STATE] = allocate("disp_solve_state", n_steps_fixed);
        ptr_array[C_csp_solver::E_reported_outputs::DISPATCH_SOLVE_ITER] = allocate("disp_solve_iter", n_steps_fixed);
        ptr_array[C_csp_solver::E_reported_outputs::DISPATCH_SOLVE_OBJ] = allocate("disp_objective", n_steps_fixed);
        ptr_array[C_csp_solver::E_reported_outputs::DISPATCH_SOLVE_OBJ_RELAX] = allocate("disp_obj_relax", n_steps_fixed);
        ptr_array[C_csp_solver::E_reported_outputs::DISPATCH_QSF_EXPECT] = allocate("disp_qsf_expected", n_steps_fixed);
        ptr_array[C_csp_solver::E_reported_outputs::DISPATCH_QSFPROD_EXPECT] = allocate("disp_qsfprod_expected", n_steps_fixed);
        ptr_array[C_csp_solver::E_reported_outputs::DISPATCH_QSFSU_EXPECT] = allocate("disp_qsfsu_expected", n_steps_fixed);
        ptr_array[C_csp_solver::E_reported_outputs::DISPATCH_TES_EXPECT] = allocate("disp_tes_expected", n_steps_fixed);
        ptr_array[C_csp_solver::E_reported_outputs::DISPATCH_PCEFF_EXPECT] = allocate("disp_pceff_expected", n_steps_fixed);
        ptr_array[C_csp_solver::E_reported_outputs::DISPATCH_SFEFF_EXPECT] = allocate("disp_thermeff_expected", n_steps_fixed);
        ptr_array[C_csp_solver::E_reported_outputs::DISPATCH_QPBSU_EXPECT] = allocate("disp_qpbsu_expected", n_steps_fixed);
        ptr_array[C_csp_solver::E_reported_outputs::DISPATCH_WPB_EXPECT] = allocate("disp_wpb_expected", n_steps_fixed);
        ptr_array[C_csp_solver::E_reported_outputs::DISPATCH_REV_EXPECT] = allocate("disp_rev_expected", n_steps_fixed);

		try
		{
			// Simulate !
			csp_solver.simulate(sim_setup, 
									ssc_mspt_sim_progress, (void*)this, 
									ptr_array,
									post_proc_array);
		}
		catch(C_csp_exception &csp_exception)
		{
			// Report warning before exiting with error
			while( csp_solver.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				log(out_msg);
			}

			log(csp_exception.m_error_message, SSC_WARNING);
			delete [] ptr_array;
			delete [] post_proc_array;

			return;
		}

		// ************************************
		// ************************************
		delete[] ptr_array;
		delete[] post_proc_array;
		// ************************************
		// ************************************

		// If no exception, then report messages
		while( csp_solver.mc_csp_messages.get_message(&out_type, &out_msg) )
		{
			log(out_msg);
		}

		size_t count;
		ssc_number_t *p_W_dot_net = as_array("P_out_net", &count);
		ssc_number_t *p_time_final_hr = as_array("time_hr", &count);

		// 'adjustment_factors' class stores factors in hourly array, so need to index as such
		adjustment_factors haf(this);
		if( !haf.setup() )
			throw exec_error("tcsmolten_salt", "failed to setup adjustment factors: " + haf.error());


		ssc_number_t *p_gen = allocate("gen", count);
		for( int i = 0; i < count; i++ )
		{
			size_t hour = ceil(p_time_final_hr[i]);
			p_gen[i] = p_W_dot_net[i] * 1.E3 * (ssc_number_t)haf(hour);			//[kWe]
		}

		accumulate_annual_for_year("gen", "annual_energy", sim_setup.m_report_step / 3600.0, steps_per_hour);
		
		accumulate_annual_for_year("P_cycle", "annual_W_cycle_gross", 1000.0*sim_setup.m_report_step / 3600.0, steps_per_hour);		//[kWe-hr]

		// Calculated Outputs
		ssc_number_t ae = as_number("annual_energy");
		ssc_number_t pg = as_number("annual_W_cycle_gross");
		ssc_number_t convfactor = (pg != 0) ? 100 * ae / pg : 0.0;
		assign("conversion_factor", convfactor);

		double kWh_per_kW = 0.0;
		double nameplate = as_double("system_capacity");
		if(nameplate > 0.0)
			kWh_per_kW = ae / nameplate;

		assign("capacity_factor", kWh_per_kW/87.6);
		assign("kwh_per_kw", kWh_per_kW);

		log("Everything was super successful, great job!");
		 
	}
};

static bool ssc_mspt_solarpilot_callback( simulation_info *siminfo, void *data )
{
	cm_tcsmolten_salt *cm = static_cast<cm_tcsmolten_salt*> (data);
	if( !cm )
		false;
	float simprogress = (float)siminfo->getCurrentSimulation() / (float)(max(siminfo->getTotalSimulationCount(), 1));

	return cm->relay_message(*siminfo->getSimulationNotices(), simprogress*100.0f);
}

static bool ssc_mspt_sim_progress( void *data, double percent, C_csp_messages *csp_msg, float time_sec )
{
	cm_tcsmolten_salt *cm = static_cast<cm_tcsmolten_salt*> (data);
	if( !cm )
		false;
	
    if(csp_msg != 0)
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

DEFINE_MODULE_ENTRY(tcsmolten_salt, "CSP molten salt power tower with hierarchical controller and dispatch optimization", 1)
