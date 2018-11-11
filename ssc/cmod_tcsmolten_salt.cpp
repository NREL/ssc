/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  ("Alliance") under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

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
#include "csp_solver_pt_sf_perf_interp.h"
#include "csp_solver_mspt_receiver_222.h"
#include "csp_solver_mspt_collector_receiver.h"
#include "csp_solver_pc_Rankine_indirect_224.h"
#include "csp_solver_pc_sco2.h"
#include "csp_solver_two_tank_tes.h"
#include "csp_solver_tou_block_schedules.h"

#include "csp_system_costs.h"

static var_info _cm_vtab_tcsmolten_salt[] = {
	/*   VARTYPE           DATATYPE         NAME                           LABEL                                                     UNITS            META           GROUP            REQUIRED_IF                 CONSTRAINTS         UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "solar_resource_file",  "local weather file path",                                           "",             "",            "Weather",        "?",                       "LOCAL_FILE",           "" },
	{ SSC_INPUT,        SSC_TABLE,       "solar_resource_data",  "solar resouce data in memory",									  "",			  "",			 "Weather",        "?",						  "",					  "" },

	{ SSC_INPUT,		SSC_NUMBER,		"ppa_multiplier_model",	 "PPA multiplier model",											  "0/1",  "0=diurnal,1=timestep","Time of Delivery","?=0",					  "INTEGER,MIN=0",		  "" },
	{ SSC_INPUT,		SSC_ARRAY,		"dispatch_factors_ts",	 "Dispatch payment factor array",									  "",			  "",			"Time of Delivery","ppa_multiplier_model=1",  "",					  "" },

	{ SSC_INPUT,        SSC_NUMBER,      "field_model_type",     "0=design field and tower/receiver geometry 1=design field 2=user field, calculate performance 3=user performance maps vs solar position", "", "", "heliostat", "*", "", "" },
	{ SSC_INPUT,        SSC_NUMBER,      "gross_net_conversion_factor", "Estimated gross to net conversion factor",                   "",             "",            "system_design",  "*",                       "",                     "" },

	{ SSC_INPUT,        SSC_NUMBER,      "helio_width",          "Heliostat width",                                                   "m",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_height",         "Heliostat height",                                                  "m",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_optical_error_mrad",  "Heliostat optical error",                                      "mrad",          "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_active_fraction","Heliostat active frac.",                                            "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "dens_mirror",          "Ratio of Reflective Area to Profile",                               "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_reflectance",    "Heliostat reflectance",                                             "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_absorptance",      "Receiver absorptance",                                              "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_hl_perm2",         "Receiver design heatloss",                                          "kW/m2",        "",            "heliostat",      "*",                       "",                     "" },
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
    { SSC_INPUT,        SSC_MATRIX,      "helio_aim_points",     "Heliostat aim point table",                                         "m",            "",            "heliostat",      "?",                       "",                     "" },
    { SSC_INPUT,        SSC_MATRIX,      "eta_map",              "Field efficiency array",                                            "-",            "",            "heliostat",      "?",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_map_aod_format",   "Use 3D AOD format field efficiency array"                           "-",            "",            "heliostat",      "?=0",                     "",                     "" },
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
    { SSC_INPUT,        SSC_NUMBER,      "water_usage_per_wash", "Water usage per wash",                                              "L/m2_aper",    "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "washing_frequency",    "Mirror washing frequency",                                          "none",         "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "check_max_flux",       "Check max flux at design point",                                    "",             "",            "heliostat",      "?=0",                     "",                     "" },
	
    
	{ SSC_INPUT,        SSC_NUMBER,      "sf_excess",            "Heliostat field multiple",                                          "",             "",            "heliostat",      "?=1.0",                   "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "calc_fluxmaps",        "Include fluxmap calculations",                                      "",             "",            "heliostat",      "?=0",                     "",                     "" },
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
																																																									      
    { SSC_INPUT,        SSC_NUMBER,      "flux_max",             "Maximum allowable flux",                                            "",             "",            "heliostat",       "?=1000",                 "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_init_step",        "Optimization initial step size",                                    "",             "",            "heliostat",       "?=0.05",                 "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_max_iter",         "Max. number iteration steps",                                       "",             "",            "heliostat",       "?=200",                  "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_conv_tol",         "Optimization convergence tol",                                      "",             "",            "heliostat",       "?=0.001",                "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_flux_penalty",     "Optimization flux overage penalty",                                 "",             "",            "heliostat",       "*",                      "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_algorithm",        "Optimization algorithm",                                            "",             "",            "heliostat",       "?=0",                    "",                     "" },

    //other costs needed for optimization update
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.per_acre",       "EPC cost per acre",                                       "$/acre",       "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.percent",        "EPC cost percent of direct",                              "%",            "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.per_watt",       "EPC cost per watt",                                       "$/W",          "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.fixed",          "EPC fixed",                                               "$",            "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.percent",        "PLM cost percent of direct",                              "%",            "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.per_watt",       "PLM cost per watt",                                       "$/W",          "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.fixed",          "PLM fixed",                                               "$",            "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.sf.fixed_land_area",      "Fixed land area",                                         "acre",         "",            "heliostat",       "*",                      "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.sf.land_overhead_factor", "Land overhead factor",                                    "",             "",            "heliostat",       "*",                      "",                     "" },

	



	// System Design
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_cold_des",       "Cold HTF inlet temperature at design conditions",                   "C",            "",            "system_design",  "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_hot_des",        "Hot HTF outlet temperature at design conditions",                   "C",            "",            "system_design",  "*",                       "",                      "" },
  	{ SSC_INPUT,        SSC_NUMBER,      "P_ref",                "Reference output electric power at design condition",               "MW",           "",            "system_design",  "*",                       "",                      "" },	
    { SSC_INPUT,        SSC_NUMBER,      "design_eff",           "Power cycle efficiency at design",                                  "none",         "",            "system_design",  "*",                       "",                      "" },    		
	{ SSC_INPUT,        SSC_NUMBER,      "tshours",              "Equivalent full-load thermal storage hours",                        "hr",           "",            "system_design",  "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "solarm",               "Solar Multiple",                                                    "-",            "",            "system_design",  "*",                       "",                      "" },
														     																	  
	// Receiver (type 222) parameters						     																	  
    { SSC_INPUT,        SSC_NUMBER,      "N_panels",             "Number of individual panels on the receiver",                       "",             "",            "receiver",       "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_tube_out",           "The outer diameter of an individual receiver tube",                 "mm",           "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "th_tube",              "The wall thickness of a single receiver tube",                      "mm",           "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mat_tube",             "2: Stainless AISI316",                                              "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_htf",              "17: Salt (60% NaNO3, 40% KNO3) 10: Salt (46.5% LiF 11.5% NaF 42% KF) 50: Lookup tables", "", "",   "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "field_fl_props",       "User defined field fluid property data",                            "-",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Flow_type",            "Flow pattern: see figure on SAM Receiver page",                     "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "crossover_shift",      "No. panels shift in receiver crossover position",                   "",             "",            "receiver",       "?=0",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "epsilon",              "The emissivity of the receiver surface coating",                    "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hl_ffact",             "The heat loss factor (thermal loss fudge factor)",                  "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_rec_min",            "Minimum receiver mass flow rate turn down fraction",                "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_su_delay",         "Fixed startup delay time for the receiver",                         "hr",           "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_qf_delay",         "Energy-based rcvr startup delay (fraction of rated thermal power)", "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "csp.pt.rec.max_oper_frac",  "Maximum receiver mass flow rate fraction",                     "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_pump",             "Receiver HTF pump efficiency",                                      "",             "",            "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "piping_loss",          "Thermal loss per meter of piping",                                  "Wt/m",         "",            "tower",          "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "piping_length_mult",   "Piping length multiplier",                                          "",             "",            "tower",          "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "piping_length_const",  "Piping constant length",                                            "m",            "",            "tower",          "*",                       "",                      "" },
													     																	  
	
	// TES parameters - general
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.tes.init_hot_htf_percent", "Initial fraction of avail. vol that is hot",             "%",            "",            "TES",            "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "h_tank",               "Total height of tank (height of HTF when tank is full",             "m",            "",            "TES",            "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cold_tank_max_heat",   "Rated heater capacity for cold tank heating",                       "MW",           "",            "TES",            "*",                       "",                      "" },    		
    { SSC_INPUT,        SSC_NUMBER,      "u_tank",               "Loss coefficient from the tank",                                    "W/m2-K",       "",            "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_pairs",           "Number of equivalent tank pairs",                                   "-",            "",            "TES",            "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_Thtr",       "Minimum allowable cold tank HTF temp",                              "C",            "",            "TES",            "*",                       "",                      "" },
		// TES parameters - 2 tank
	{ SSC_INPUT,        SSC_NUMBER,      "h_tank_min",           "Minimum allowable HTF height in storage tank",                      "m",            "",            "TES",      "*",              "",                      "" },	
	{ SSC_INPUT,        SSC_NUMBER,      "hot_tank_Thtr",        "Minimum allowable hot tank HTF temp",                               "C",            "",            "TES",      "*",              "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_max_heat",    "Rated heater capacity for hot tank heating",                        "MW",           "",            "TES",      "*",              "",                      "" },
				
		//RADIATIVE COOLING WITH COLD STORAGE
	{ SSC_INPUT,        SSC_NUMBER,      "h_ctes_tank_min",      "Minimum allowable water height in storage tank",					"m",            "",				"RADCOOL",		"?=0",						"",							"" },
	{ SSC_INPUT,        SSC_NUMBER,      "ctes_tshours",         "Equivalent full load storage hours",								"hr",           "",				"RADCOOL",      "?=0",						"",							"" },
	{ SSC_INPUT,        SSC_NUMBER,      "ctes_field_fl",        "Fluid in radiator field. 3=liquid water. Other = Glycol.",							"-",            "",				"RADCOOL",      "?=3",						"",							"" },
	{ SSC_INPUT,        SSC_NUMBER,      "h_ctes_tank",			 "Total height of cold storage tank when full",						"m",            "",				"RADCOOL",      "?=0",						"",							"" },
	{ SSC_INPUT,        SSC_NUMBER,      "u_ctes_tank",			 "Loss coefficient from cold storage tank",							"W/m2-K",       "",				"RADCOOL",      "?=0",						"",							"" },
	{ SSC_INPUT,        SSC_NUMBER,      "ctes_tankpairs",		 "Number of equivalent tank pairs",									"-",			"",				"RADCOOL",      "?=0",						"",							"" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_ctes_cold_design",	 "Design value of cooled water to power block",						"C",			"",				"RADCOOL",      "?=0",						"",							"" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_ctes_warm_design",   "Design value of warm water returning from power block",			"C",			"",				"RADCOOL",      "?=0",						"",							"" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_ctes_warm_ini",		 "Initial value of warm tank",										"C",			"",				"RADCOOL",      "?=0",						"",							"" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_ctes_cold_ini",		 "Initial value of cold tank",										"C",			"",				"RADCOOL",      "?=0",						"",							"" },
	{ SSC_INPUT,        SSC_NUMBER,      "f_ctes_warm_ini",		 "Initial fraction of avail. volume that is warm",					"-",			"",				"RADCOOL",      "?=0",						"",							"" },
	{ SSC_INPUT,        SSC_NUMBER,      "rad_multiplier",		 "Ratio of radiator field area to solar aperature area",			"-",           "",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "m_dot_radpanel",	     "Mass flow rate through single radiator panel",					"kg/sec",       "",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "n_rad_tubes",		     "Number of parallel tubes in single radiator panel",				"-",		    "",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "W_rad_tubes",		     "Center-to-center distance between tubes in radiator panel",		"m",			"",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "L_rad",			     "Length of radiator panel row",									"m",			"",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "th_rad_panel",		 "Thickness of radiator panel",										"m",			"",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "D_rad_tubes",			 "Inner diameter of tubes in radiator panel",						"m",			"",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "k_panel",				 "Thermal conductivity of radiator panel material",					"W/m-K",		"",            "RADCOOL",      "?=235",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "epsilon_radtop",		 "Emmissivity of top of radiator panel",							"-",			"",            "RADCOOL",      "?=.95",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "epsilon_radbot",		 "Emmissivity of top of radiator panel bottom (facing ground)",		"-",			"",            "RADCOOL",      "?=.07",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "epsilon_radgrnd",		 "Emmissivity of ground underneath radiator panel",					"-",			"",            "RADCOOL",      "?=.90",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "L_rad_sections",		 "Length of individual radiator panel",								"m",			"",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "epsilon_radHX",		 "Effectiveness of HX between radiative field and cold storage",	"-",			"",            "RADCOOL",      "?=.8",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "ctes_type",			 "Type of cold storage (2=two tank, 3= three node)",				"-",			"",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "helio_area_tot",		 "Heliostat total reflective area",									"-",			"",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "radiator_unitcost",	 "Cost of radiative panels",										"$/m^2",		"",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "radiator_installcost", "Installation cost of radiative panels",							"$/m^2",		"",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "radiator_fluidcost",	 "Cost of circulating fluid in radiative panels",					"$/L",			"",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "radfluid_vol_ratio",	 "Ratio of fluid in distribution to fluid in panels",				"-",			"",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "ctes_cost",			 "Cost of cold storage construction",								"$/L",			"",            "RADCOOL",      "?=0",						 "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "rad_pressuredrop", "Average pressure drop through a radiative panel & distribution",	"kPa",			"",            "RADCOOL",      "?=0",						 "",						"" },

    					     																	  
    // Power Cycle Inputs
	{ SSC_INPUT,        SSC_NUMBER,      "pc_config",            "0: Steam Rankine (224), 1: user defined, 2: sCO2 Recompression (424)", "-",         "",            "powerblock",     "?=0",                     "INTEGER",               "" },    
	{ SSC_INPUT,        SSC_NUMBER,      "pb_pump_coef",         "Pumping power to move 1kg of HTF through PB loop",                  "kW/kg",        "",            "powerblock",     "*",                       "",                      "" },    
    { SSC_INPUT,        SSC_NUMBER,      "startup_time",         "Time needed for power block startup",                               "hr",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",         "Fraction of design thermal power needed for startup",               "none",         "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cycle_max_frac",       "Maximum turbine over design operation fraction",                    "-",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_cutoff_frac",    "Minimum turbine operation fraction before shutdown",                "-",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",           "Fraction of thermal power required for standby",                    "-",            "",            "powerblock",     "*",                       "",                      "" },

	
		// Steam Rankine cycle
	{ SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",            "Reference condenser cooling water inlet/outlet T diff",             "C",            "",            "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",            "Reference ambient temperature at design point",                     "C",            "",            "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil",               "Boiler operating pressure",                                         "bar",          "",            "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                   "cooling tech type: 1=evaporative, 2=air, 3=hybrid",                 "-",            "",            "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",           "Cooling tower approach temperature",                                "C",            "",            "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",            "ITD at design for dry system",                                      "C",            "",            "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",         "Condenser pressure ratio",                                          "none",         "",            "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",           "Power block blowdown steam fraction ",                              "none",         "",            "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",           "Minimum condenser pressure",                                        "inHg",         "",            "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",             "Number of part-load increments for the heat rejection system",      "none",         "",            "powerblock",     "pc_config=0",             "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",                 "TOU array of fractions indicating wet cooling share for hybrid cooling", "-",       "",            "powerblock",     "pc_config=0",             "",                      "" },
   	{ SSC_INPUT,        SSC_NUMBER,      "tech_type",            "Turbine inlet pressure control 1: Fixed, 3: Sliding",               "-",            "",            "powerblock",     "pc_config=0",             "",                      "" },

		// User Defined cycle
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_amb_des",         "Ambient temperature at user-defined power cycle design point",                   "C",	    "",      "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_f_W_dot_cool_des",  "Percent of user-defined power cycle design gross output consumed by cooling",    "%",	    "",      "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_m_dot_water_cool_des", "Mass flow rate of water required at user-defined power cycle design point",   "kg/s",  "",      "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_htf_low",         "Low level HTF inlet temperature for T_amb parametric",                           "C",     "",      "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_htf_high",        "High level HTF inlet temperature for T_amb parametric",                          "C",		"",      "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_amb_low",         "Low level ambient temperature for HTF mass flow rate parametric",                "C",		"",      "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_amb_high",        "High level ambient temperature for HTF mass flow rate parametric",               "C",		"",      "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_m_dot_htf_low",     "Low level normalized HTF mass flow rate for T_HTF parametric",                   "-",	    "",      "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_m_dot_htf_high",    "High level normalized HTF mass flow rate for T_HTF parametric",                  "-",	    "",      "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_MATRIX,      "ud_T_htf_ind_od",      "Off design table of user-defined power cycle performance formed from parametric on T_htf_hot [C]", "", "", "user_defined_PC", "pc_config=1",    "",                      "" },
	{ SSC_INPUT,        SSC_MATRIX,      "ud_T_amb_ind_od",      "Off design table of user-defined power cycle performance formed from parametric on T_amb [C]",	 "", "", "user_defined_PC", "pc_config=1",    "",                      "" }, 
	{ SSC_INPUT,        SSC_MATRIX,      "ud_m_dot_htf_ind_od",  "Off design table of user-defined power cycle performance formed from parametric on m_dot_htf [ND]","", "", "user_defined_PC", "pc_config=1",    "",                      "" }, 
    { SSC_INPUT,        SSC_MATRIX,      "ud_ind_od",            "Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb", "", "", "user_defined_PC", "pc_config=1",     "",                      "" },
																     																	  
		// sCO2 Powerblock (type 424) inputs
	{ SSC_INPUT,        SSC_NUMBER,      "sco2_cycle_config",    "1 = recompression, 2 = partial cooling",                            "",             "",            "sco2_pc",     "pc_config=2",                "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_c",                "Isentropic efficiency of compressor(s)",                            "none",         "",            "sco2_pc",     "pc_config=2",                "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_t",                "Isentropic efficiency of turbine",							      "none",         "",            "sco2_pc",     "pc_config=2",                "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "recup_eff_max",        "Maximum recuperator effectiveness",                                 "none",         "",            "sco2_pc",     "pc_config=2",                "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "P_high_limit",         "Upper pressure limit in cycle",								      "MPa",          "",            "sco2_pc",     "pc_config=2",                "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "deltaT_PHX",           "Design temperature difference in PHX",						      "C",	          "",            "sco2_pc",     "pc_config=2",                "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fan_power_perc_net",   "% of net cycle output used for fan power at design",			      "%",	          "",            "sco2_pc",     "pc_config=2",                "",                      "" },	
	{ SSC_INPUT,        SSC_NUMBER,      "sco2_T_amb_des",       "Ambient temperature at design point",                                      "C",     "",            "sco2_pc",     "pc_config=2",                "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sco2_T_approach",      "Temperature difference between main compressor CO2 inlet and ambient air", "C",     "",            "sco2_pc",     "pc_config=2",                "",                      "" },
		// sCO2 Powerblock pre-process
	{ SSC_INPUT,        SSC_NUMBER,      "is_sco2_preprocess",       "Is sco2 off-design performance preprocessed? 1= yes",			                   "-",	                 "", "sco2_pc_pre",     "?=0",                        "",      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sco2ud_T_htf_cold_calc",   "HTF cold temperature from sCO2 cycle des, may be different than T_htf_cold_des", "C",                  "", "sco2_pc_pre",     "is_sco2_preprocess=1",       "",      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sco2ud_T_htf_low",         "Low level HTF inlet temperature for T_amb parametric",                           "C",                  "", "sco2_pc_pre",     "is_sco2_preprocess=1",       "",      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sco2ud_T_htf_high",        "High level HTF inlet temperature for T_amb parametric",                          "C",                  "", "sco2_pc_pre",     "is_sco2_preprocess=1",       "",      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sco2ud_T_amb_low",         "Low level ambient temperature for HTF mass flow rate parametric",                "C",		             "", "sco2_pc_pre",     "is_sco2_preprocess=1",       "",      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sco2ud_T_amb_high",        "High level ambient temperature for HTF mass flow rate parametric",               "C",		             "", "sco2_pc_pre",     "is_sco2_preprocess=1",       "",      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sco2ud_m_dot_htf_low",     "Low level normalized HTF mass flow rate for T_HTF parametric",                   "-",	                 "", "sco2_pc_pre",     "is_sco2_preprocess=1",       "",      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sco2ud_m_dot_htf_high",    "High level normalized HTF mass flow rate for T_HTF parametric",                  "-",	                 "", "sco2_pc_pre",     "is_sco2_preprocess=1",       "",      "" },
	{ SSC_INPUT,        SSC_MATRIX,      "sco2ud_T_htf_ind_od",      "Off design table of user-defined power cycle performance formed from parametric on T_htf_hot [C]", "", "", "sco2_pc_pre",     "is_sco2_preprocess=1",       "",      "" },
	{ SSC_INPUT,        SSC_MATRIX,      "sco2ud_T_amb_ind_od",      "Off design table of user-defined power cycle performance formed from parametric on T_amb [C]",	 "", "", "sco2_pc_pre",     "is_sco2_preprocess=1",       "",      "" }, 
	{ SSC_INPUT,        SSC_MATRIX,      "sco2ud_m_dot_htf_ind_od",  "Off design table of user-defined power cycle performance formed from parametric on m_dot_htf [ND]","", "", "sco2_pc_pre",     "is_sco2_preprocess=1",       "",      "" }, 

	{ SSC_INPUT,        SSC_NUMBER,      "_sco2_P_high_limit",       "Preprocess input: upper pressure limit",				         "MPa",	"", "sco2_pc_pre",   "is_sco2_preprocess=1",  "",   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "_sco2_P_ref",              "Preprocess input: gross power output",				         "MWe"	"", "sco2_pc_pre",   "is_sco2_preprocess=1",  "",   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "_sco2_T_amb_des",          "Preprocess input: design ambient temperature",		         "C",	"", "sco2_pc_pre",   "is_sco2_preprocess=1",  "",   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "_sco2_T_approach",         "Preprocess input: compressor approach temperature",	         "C",	"", "sco2_pc_pre",   "is_sco2_preprocess=1",  "",   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "_sco2_T_htf_hot_des",      "Preprocess input: HTF hot temperature",				         "C",	"", "sco2_pc_pre",   "is_sco2_preprocess=1",  "",   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "_sco2_deltaT_PHX",		 "Preprocess input: PHX approach temperature",			         "C",	"", "sco2_pc_pre",   "is_sco2_preprocess=1",  "",   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "_sco2_design_eff",		 "Preprocess input: cycle thermal efficiency",			         "",	"", "sco2_pc_pre",   "is_sco2_preprocess=1",  "",   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "_sco2_eta_c",				 "Preprocess input: compressor isentropic efficiency",	         "",	"", "sco2_pc_pre",   "is_sco2_preprocess=1",  "",   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "_sco2_eta_t",				 "Preprocess input: turbine isentropic efficiency",		         "",	"", "sco2_pc_pre",   "is_sco2_preprocess=1",  "",   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "_sco2_recup_eff_max",		 "Preprocess input: max recuperator effectiveness",              "",    "", "sco2_pc_pre",   "is_sco2_preprocess=1",  "",   "" },

	// System Control	
    { SSC_INPUT,        SSC_NUMBER,      "time_start",           "Simulation start time",                                             "s",            "",            "sys_ctrl",          "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "time_stop",            "Simulation stop time",                                              "s",            "",            "sys_ctrl",          "?=31536000",              "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "time_steps_per_hour",  "Number of simulation time steps per hour",                          "-",            "",            "sys_ctrl",          "?=-1",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "vacuum_arrays",        "Allocate arrays for only the required number of steps",             "-",            "",            "sys_ctrl",          "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_fixed_par",         "Fixed parasitic load - runs at all times",                          "MWe/MWcap",    "",            "sys_ctrl",          "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "aux_par",              "Aux heater, boiler parasitic",                                      "MWe/MWcap",    "",            "sys_ctrl",          "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "aux_par_f",            "Aux heater, boiler parasitic - multiplying fraction",               "none",         "",            "sys_ctrl",          "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "aux_par_0",            "Aux heater, boiler parasitic - constant coefficient",               "none",         "",            "sys_ctrl",          "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "aux_par_1",            "Aux heater, boiler parasitic - linear coefficient",                 "none",         "",            "sys_ctrl",          "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "aux_par_2",            "Aux heater, boiler parasitic - quadratic coefficient",              "none",         "",            "sys_ctrl",          "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "bop_par",              "Balance of plant parasitic power fraction",                         "MWe/MWcap",    "",            "sys_ctrl",          "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "bop_par_f",            "Balance of plant parasitic power fraction - mult frac",             "none",         "",            "sys_ctrl",          "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "bop_par_0",            "Balance of plant parasitic power fraction - const coeff",           "none",         "",            "sys_ctrl",          "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "bop_par_1",            "Balance of plant parasitic power fraction - linear coeff",          "none",         "",            "sys_ctrl",          "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "bop_par_2",            "Balance of plant parasitic power fraction - quadratic coeff",       "none",         "",            "sys_ctrl",          "*",                       "",                      "" },												     																	  
    { SSC_INPUT,        SSC_ARRAY,       "f_turb_tou_periods",   "Dispatch logic for turbine load fraction",                          "-",            "",            "sys_ctrl",          "*",                       "",                      "" },    
	{ SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",     "12x24 CSP operation Time-of-Use Weekday schedule",                  "-",            "",            "sys_ctrl",          "*",                       "",                      "" }, 
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",     "12x24 CSP operation Time-of-Use Weekend schedule",                  "-",            "",            "sys_ctrl",          "*",                       "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "is_dispatch",          "Allow dispatch optimization?",  /*TRUE=1*/                          "-",            "",            "sys_ctrl_disp_opt", "?=0",                     "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_horizon",         "Time horizon for dispatch optimization",                            "hour",         "",            "sys_ctrl_disp_opt", "is_dispatch=1",           "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_frequency",       "Frequency for dispatch optimization calculations",                  "hour",         "",            "sys_ctrl_disp_opt", "is_dispatch=1",           "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_steps_per_hour",  "Time steps per hour for dispatch optimization calculations",        "-",            "",            "sys_ctrl_disp_opt", "?=1",                     "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_max_iter",        "Max. no. dispatch optimization iterations",                         "-",            "",            "sys_ctrl_disp_opt", "is_dispatch=1",           "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_timeout",         "Max. dispatch optimization solve duration",                         "s",            "",            "sys_ctrl_disp_opt", "is_dispatch=1",           "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_mip_gap",         "Dispatch optimization solution tolerance",                          "-",            "",            "sys_ctrl_disp_opt", "is_dispatch=1",           "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_spec_bb",         "Dispatch optimization B&B heuristic",                               "-",            "",            "sys_ctrl_disp_opt", "?=-1",                    "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_reporting",       "Dispatch optimization reporting level",                             "-",            "",            "sys_ctrl_disp_opt", "?=-1",                    "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_spec_presolve",   "Dispatch optimization presolve heuristic",                          "-",            "",            "sys_ctrl_disp_opt", "?=-1",                    "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_spec_scaling",    "Dispatch optimization scaling heuristic",                           "-",            "",            "sys_ctrl_disp_opt", "?=-1",                    "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_time_weighting",  "Dispatch optimization future time discounting factor",              "-",            "",            "sys_ctrl_disp_opt", "?=0.99",                    "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "is_write_ampl_dat",    "Write AMPL data files for dispatch run",                            "-",            "",            "sys_ctrl_disp_opt", "?=0",                     "",                      "" }, 
    { SSC_INPUT,        SSC_STRING,      "ampl_data_dir",        "AMPL data file directory",                                          "-",            "",            "sys_ctrl_disp_opt", "?=''",                    "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "is_ampl_engine",       "Run dispatch optimization with external AMPL engine",               "-",            "",            "sys_ctrl_disp_opt", "?=0",                     "",                      "" }, 
    { SSC_INPUT,        SSC_STRING,      "ampl_exec_call",       "System command to run AMPL code",                                   "-",            "",            "sys_ctrl_disp_opt", "?='ampl sdk_solution.run'", "",                    "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_rsu_cost",        "Receiver startup cost",                                             "$",            "",            "sys_ctrl_disp_opt", "is_dispatch=1",           "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_csu_cost",        "Cycle startup cost",                                                "$",            "",            "sys_ctrl_disp_opt", "is_dispatch=1",           "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "disp_pen_delta_w",     "Dispatch cycle production change penalty",                          "$/kWe-change", "",            "sys_ctrl_disp_opt", "is_dispatch=1",           "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "q_rec_standby",        "Receiver standby energy consumption",                               "kWt",          "",            "sys_ctrl_disp_opt", "?=9e99",                  "",                      "" }, 
	{ SSC_INPUT,		SSC_NUMBER,		 "q_rec_heattrace",		 "Receiver heat trace energy consumption during startup",			  "kWe-hr",		  "",			 "sys_ctrl_disp_opt", "?=0.0",					 "",					  "" },
	{ SSC_INPUT,		SSC_NUMBER,		 "is_wlim_series",       "Use time-series net electricity generation limits",				  "",			  "",			 "sys_ctrl_disp_opt", "?=0",					 "",					  "" },
	{ SSC_INPUT,		SSC_ARRAY,		 "wlim_series",			 "Time series net electicity generation limits",					  "kWe",		  "",			 "sys_ctrl_disp_opt", "is_wlim_series=1",		 "",					  "" },


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
	{ SSC_INPUT,        SSC_NUMBER,      "is_dispatch_series",   "Use time-series dispatch factors",                                  "",             "",            "tou",            "?=0",						  "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dispatch_series",      "Time series dispatch factors",                                      "",             "",            "tou",            "",						  "",                      "" },

	// Inputs required for user defined SF performance
	{ SSC_INPUT,        SSC_NUMBER,      "A_sf_in",              "Solar Field Area",                                                 "m^2",           "",            "receiver",       "",                        "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "A_sf",                 "Solar Field Area",                                                 "m^2",           "",            "receiver",       "*",                       "",                      "" },



	// optimized outputs updated depending on run type 
	{ SSC_INOUT,        SSC_NUMBER,      "rec_height",           "Receiver height",                                                  "m",             "",            "heliostat",      "*",                       "",                      "" },
	{ SSC_INOUT,        SSC_NUMBER,      "D_rec",                "The overall outer diameter of the receiver",                       "m",             "",            "receiver",       "*",                       "",                      "" },
	{ SSC_INOUT,        SSC_NUMBER,      "h_tower",              "Tower height",                                                     "m",             "",            "heliostat",      "*",                       "",                      "" },
    { SSC_INOUT,        SSC_NUMBER,      "N_hel",                "Number of heliostats",                                             "-",             "",            "heliostat",      "",              "",                      "" },
	{ SSC_INOUT,        SSC_MATRIX,      "helio_positions",      "Heliostat position table",                                         "",              "",            "heliostat",      "*",              "",                      "COL_LABEL=XY_POSITION" },
	{ SSC_INOUT,        SSC_NUMBER,      "land_area_base",       "Base land area occupied by heliostats",                            "acre",          "",            "heliostat",      "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.total_land_area", "Total land area",                                           "acre",          "",            "system_costs",   "*",                       "",                      "" },


	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.site_improvements",	    "Site improvement cost",                  "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.heliostats",	            "Heliostat cost",                         "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.tower",	                "Tower cost",                             "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.receiver",	            "Receiver cost",                          "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.storage",	                "TES cost",                               "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.power_block",	            "Power cycle cost",                       "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,		SSC_NUMBER,		 "csp.pt.cost.rad_field",			    "Radiative field cost"					  "$",			  "",			 "system_costs",   "*",		   "",	"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		 "csp.pt.cost.rad_fluid",			    "Radiative fluid cost"					  "$",			  "",			 "system_costs",   "*",		   "",	"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		 "csp.pt.cost.rad_storage",			    "Cold storage cost"						  "$",			  "",			 "system_costs",   "*",		   "",	"" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.bop",	                    "BOP cost",                               "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.fossil",	                "Fossil backup cost",                     "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "ui_direct_subtotal",	                "Direct capital precontingency cost",     "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.contingency",	            "Contingency cost",                       "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "total_direct_cost",	                "Total direct cost",                      "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.epc.total",	            "EPC and owner cost",                     "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.plm.total",	            "Total land cost",                        "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.sales_tax.total",	        "Sales tax cost",                         "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "total_indirect_cost",	                "Total indirect cost",                    "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "total_installed_cost",	            "Total installed cost",                   "$",            "",            "system_costs",   "*",        "",  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "csp.pt.cost.installed_per_capacity",  "Estimated installed cost per cap",       "$",            "",            "system_costs",   "*",        "",  "" },

		// Construction financing inputs/outputs (SSC variable table from cmod_cb_construction_financing)
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_interest_rate1",            "Interest rate, loan 1",                       "%",    "",     "financial parameters",   "*",   "",  "" },   
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_interest_rate2",            "Interest rate, loan 2",                       "%",    "",     "financial parameters",   "*",   "",  "" },   
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_interest_rate3",            "Interest rate, loan 3",                       "%",    "",     "financial parameters",   "*",   "",  "" },   
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_interest_rate4",            "Interest rate, loan 4",                       "%",    "",     "financial parameters",   "*",   "",  "" },   
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_interest_rate5",            "Interest rate, loan 5",                       "%",    "",     "financial parameters",   "*",   "",  "" },   
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_months1",                   "Months prior to operation, loan 1",           "",     "",     "financial parameters",   "*",   "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_months2",                   "Months prior to operation, loan 2",           "",     "",     "financial parameters",   "*",   "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_months3",                   "Months prior to operation, loan 3",           "",     "",     "financial parameters",   "*",   "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_months4",                   "Months prior to operation, loan 4",           "",     "",     "financial parameters",   "*",   "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_months5",                   "Months prior to operation, loan 5",           "",     "",     "financial parameters",   "*",   "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_percent1",                  "Percent of tot. installed cost, loan 1",      "%",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_percent2",                  "Percent of tot. installed cost, loan 2",      "%",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_percent3",                  "Percent of tot. installed cost, loan 3",      "%",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_percent4",                  "Percent of tot. installed cost, loan 4",      "%",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_percent5",                  "Percent of tot. installed cost, loan 5",      "%",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_upfront_rate1",             "Upfront fee on principal, loan 1",            "%",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_upfront_rate2",             "Upfront fee on principal, loan 2",            "%",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_upfront_rate3",             "Upfront fee on principal, loan 3",            "%",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_upfront_rate4",             "Upfront fee on principal, loan 4",            "%",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_INPUT,  SSC_NUMBER,   "const_per_upfront_rate5",             "Upfront fee on principal, loan 5",            "%",    "",     "financial parameters",   "*",   "",  "" },
																												      
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_principal1",                "Principal, loan 1",                           "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_principal2",                "Principal, loan 2",                           "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_principal3",                "Principal, loan 3",                           "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_principal4",                "Principal, loan 4",                           "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_principal5",                "Principal, loan 5",                           "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_interest1",                 "Interest cost, loan 1",                       "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_interest2",                 "Interest cost, loan 2",                       "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_interest3",                 "Interest cost, loan 3",                       "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_interest4",                 "Interest cost, loan 4",                       "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_interest5",                 "Interest cost, loan 5",                       "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_total1",                    "Total financing cost, loan 1",                "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_total2",                    "Total financing cost, loan 2",                "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_total3",                    "Total financing cost, loan 3",                "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_total4",                    "Total financing cost, loan 4",                "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_total5",                    "Total financing cost, loan 5",                "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_percent_total",	           "Total percent of installed costs, all loans", "%",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_principal_total",           "Total principal, all loans",				  "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "const_per_interest_total",	           "Total interest costs, all loans",			  "$",    "",     "financial parameters",   "*",   "",  "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "construction_financing_cost",         "Total construction financing cost",           "$",    "",     "financial parameters",   "*",   "",  "" },



	// ****************************************************************************************************************************************
	// Outputs here:
	// ****************************************************************************************************************************************
		// Simulation outputs
	{ SSC_OUTPUT,       SSC_ARRAY,       "time_hr",              "Time at end of timestep",                                      "hr",           "",            "Solver",         "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "solzen",               "Resource Solar Zenith",                                        "deg",          "",            "weather",        "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "solaz",                "Resource Solar Azimuth",                                       "deg",          "",            "weather",        "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "beam",                 "Resource Beam normal irradiance",                              "W/m2",         "",            "weather",        "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tdry",                 "Resource Dry Bulb Temperature",                                "C",           "",            "weather",        "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "twet",                 "Resource Wet Bulb Temperature",                                "C",           "",            "weather",        "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "rh",                   "Resource Relative Humidity",                                   "%",           "",            "weather",        "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "wspd",                 "Resource Wind Velocity",                                       "m/s",         "",            "weather",        "*",                       "",           "" },
	
		// Collector-receiver outputs
			// Eventually want to make this INOUT, but will have to add 'eta_map' to UI...
    { SSC_OUTPUT,       SSC_MATRIX,      "eta_map_out",          "Solar field optical efficiencies",                             "",             "",            "heliostat",      "*",                       "",           "COL_LABEL=OPTICAL_EFFICIENCY,ROW_LABEL=NO_ROW_LABEL" },
    { SSC_OUTPUT,       SSC_MATRIX,      "flux_maps_out",        "Flux map intensities",                                         "",             "",            "heliostat",      "*",                       "",           "COL_LABEL=FLUX_MAPS,ROW_LABEL=NO_ROW_LABEL" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "q_sf_inc",             "Field incident thermal power",                                 "MWt",          "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_field",            "Field optical efficiency",                                     "",             "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "defocus",              "Field optical focus fraction",                                 "",             "",            "Controller",     "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sf_adjust_out",        "Field availability adjustment factor",                         "",             "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_dot_rec_inc",        "Rec. incident thermal power",                                  "MWt",          "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_therm",            "Rec. thermal efficiency",                                      "",             "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_thermal",            "Rec. thermal power to HTF less piping loss",                   "MWt",          "",            "CR",             "*",                       "",           "" },
			
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_rec",            "Rec. mass flow rate",                                          "kg/s",         "",            "CR",             "*",                       "",           "" },	
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_startup",            "Rec. startup thermal energy consumed",                         "MWt",          "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_rec_in",             "Rec. HTF inlet temperature",                                   "C",            "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_rec_out",            "Rec. HTF outlet temperature",                                  "C",            "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_piping_losses",      "Rec. header/tower piping losses",                              "MWt",          "",            "CR",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_thermal_loss",       "Rec. convection and emission losses",                          "MWt",          "",            "CR",             "*",                       "",           "" },
	
		// Power cycle outputs
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta",                  "PC efficiency: gross",                                         "",             "",            "PC",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_pb",		         "PC input energy",                                              "MWt",          "",            "PC",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_pc",             "PC HTF mass flow rate",                                        "kg/s",         "",            "PC",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_pc_startup",         "PC startup thermal energy",                                    "MWht",         "",            "PC",             "*",                       "",           "" },	
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_startup",     "PC startup thermal power",                                     "MWt",          "",            "PC",             "*",                       "",           "" },	
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_cycle",              "PC electrical power output: gross",                            "MWe",          "",            "PC",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_pc_in",              "PC HTF inlet temperature",                                     "C",            "",            "PC",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_pc_out",             "PC HTF outlet temperature",                                    "C",            "",            "PC",             "*",                       "",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_water_pc",       "PC water consumption: makeup + cooling",                       "kg/s",         "",            "PC",             "*",                       "",           "" },
	{ SSC_OUTPUT,		SSC_ARRAY,		 "T_cond_out",			 "PC condenser water outlet temperature",						 "C",			 "",			"PC",			  "*",						 "",		   "" },
	{ SSC_OUTPUT,		SSC_ARRAY,		 "T_cold",				 "Cold storage cold temperature",								 "C",			 "",			"PC",			  "*",						 "",		   "" },
	{ SSC_OUTPUT,		SSC_ARRAY,		 "m_cold",				 "Cold storage cold tank mass",									 "kg",			 "",			"PC",			  "*",						 "",		   "" },
	{ SSC_OUTPUT,		SSC_ARRAY,		 "m_warm",				 "Cold storage warm tank mass",									 "kg",			 "",			"PC",			  "*",						 "",		   "" },
	{ SSC_OUTPUT,		SSC_ARRAY,		 "T_warm",				 "Cold storage warm tank temperature",							 "C",			 "",			"PC",			  "*",						 "",		   "" },
	{ SSC_OUTPUT,		SSC_ARRAY,		 "T_rad_out",			 "Radiator outlet temperature",									 "C",			 "",			"PC",			  "*",						 "",		   "" },
	{ SSC_OUTPUT,		SSC_NUMBER,		 "A_radfield",			 "Radiator field surface area",									 "m^2",			 "",			"PC",			  "*",						 "",		   "" },
	{ SSC_OUTPUT,		SSC_ARRAY,		 "P_cond",				 "PC condensing presssure",										 "Pa",			 "",			"PC",			  "*",						 "",		   "" },
	{ SSC_OUTPUT,		SSC_ARRAY,		 "radcool_control",		 "Radiative cooling status code",								 "-",			 "",			"PC",			  "*",						 "",		   "" },


		// Thermal energy storage outputs
	{ SSC_OUTPUT,       SSC_ARRAY,       "tank_losses",          "TES thermal losses",                                           "MWt",          "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_heater",             "TES freeze protection power",                                  "MWe",          "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tes_hot",            "TES hot temperature",                                          "C",            "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tes_cold",           "TES cold temperature",                                         "C",            "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_dc_tes",             "TES discharge thermal power",                                  "MWt",          "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_ch_tes",             "TES charge thermal power",                                     "MWt",          "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "e_ch_tes",             "TES charge state",                                             "MWht",         "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_tes_dc",         "TES discharge mass flow rate",                                 "kg/s",         "",            "TES",            "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_tes_ch",         "TES charge mass flow rate",                                    "kg/s",         "",            "TES",            "*",                       "",           "" },
	
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
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_presolve_nconstr","Dispatch number of constraints in problem",                    "",             "",            "tou",            "*"                       "",            "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_presolve_nvar",   "Dispatch number of variables in problem",                      "",             "",            "tou",            "*"                       "",            "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_solve_time",      "Dispatch solver time",                                         "sec",          "",            "tou",            "*"                       "",            "" }, 


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
	

	{ SSC_OUTPUT,       SSC_ARRAY,       "gen",                  "Total electric power to grid w/ avail. derate",                                 "kWe",          "",            "System",         "*",                       "",           "" },
		
	                                                                                                                                                                          //"?=[[0,1,2][10,11,12]]",
	{ SSC_OUTPUT,       SSC_MATRIX,      "ud_T_htf_ind_od_out",  "T_htf_hot cycle off design",                                   "",             "",            "PC",             "?=[[0,1,2,3,4,5,6,7,8,9,10,11,12][0,1,2,3,4,5,6,7,8,9,10,11,12]]",             "",          "COL_LABEL=UDPC_T_HTF_HOT,ROW_LABEL=NO_ROW_LABEL" },
	{ SSC_OUTPUT,       SSC_MATRIX,      "ud_T_amb_ind_od_out",  "T_amb cycle off design",		                                 "",             "",            "PC",             "?=[[0,1,2,3,4,5,6,7,8,9,10,11,12][0,1,2,3,4,5,6,7,8,9,10,11,12]]",             "",          "COL_LABEL=UDPC_T_AMB,ROW_LABEL=NO_ROW_LABEL" },
	{ SSC_OUTPUT,       SSC_MATRIX,      "ud_m_dot_htf_ind_od_out", "m_dot_htf cycle off design",                                "",             "",            "PC",             "?=[[0,1,2,3,4,5,6,7,8,9,10,11,12][0,1,2,3,4,5,6,7,8,9,10,11,12]]",             "",          "COL_LABEL=UDPC_M_DOT_HTF,ROW_LABEL=NO_ROW_LABEL" },


	// Annual single-value outputs
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",        "Annual total electric power to grid",                          "kWhe",       "",            "System",         "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_W_cycle_gross", "Electrical source - Power cycle gross output",                 "kWhe",       "",            "PC",             "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_W_cooling_tower", "Total of condenser operation parasitics",				      "kWhe",       "",            "PC",             "*",                       "",           "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "conversion_factor",    "Gross to Net Conversion Factor",                               "%",            "",            "PostProcess",    "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "capacity_factor",      "Capacity factor",                                              "%",            "",            "PostProcess",    "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "kwh_per_kw",           "First year kWh/kW",                                            "kWh/kW",       "",            "",               "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_total_water_use","Total Annual Water Usage: cycle + mirror washing",            "m3",         "",            "PostProcess",    "*",                         "",           "" },

    { SSC_OUTPUT,       SSC_NUMBER,      "disp_objective_ann",  "Annual sum of dispatch objective func. value",                 "",            "",             "",               "*",                       "",           "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "disp_iter_ann",       "Annual sum of dispatch solver iterations",                     "",            "",             "",               "*",                       "",           "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "disp_presolve_nconstr_ann",  "Annual sum of dispatch problem constraint count",       "",            "",             "",               "*",                       "",           "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "disp_presolve_nvar_ann",  "Annual sum of dispatch problem variable count",            "",            "",             "",               "*",                       "",           "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "disp_solve_time_ann",  "Annual sum of dispatch solver time",                          "",            "",             "",               "*",                       "",           "" },


	var_info_invalid };

class cm_tcsmolten_salt : public compute_module
{
public:

	cm_tcsmolten_salt()
	{
		add_var_info(_cm_vtab_tcsmolten_salt);
		add_var_info(vtab_adjustment_factors);
        add_var_info(vtab_sf_adjustment_factors);
	} 

	bool relay_message(string &msg, double percent)
	{
		log(msg);
		return update(msg, (float)percent);
	}

	void exec() throw(general_error)
	{
		// Weather reader
		C_csp_weatherreader weather_reader;
		if (is_assigned("solar_resource_file")){
			weather_reader.m_weather_data_provider = make_shared<weatherfile>(as_string("solar_resource_file"));
			if (weather_reader.m_weather_data_provider->has_message()) log(weather_reader.m_weather_data_provider->message(), SSC_WARNING);
		}
		if (is_assigned("solar_resource_data")){
			weather_reader.m_weather_data_provider = make_shared<weatherdata>(lookup("solar_resource_data"));
			if (weather_reader.m_weather_data_provider->has_message()) log(weather_reader.m_weather_data_provider->message(), SSC_WARNING);
		}

		weather_reader.m_trackmode = 0;
		weather_reader.m_tilt = 0.0;
		weather_reader.m_azimuth = 0.0;
		// Initialize to get weather file info
		weather_reader.init();
		if (weather_reader.has_error()) throw exec_error("tcsmolten_salt", weather_reader.get_error());

		// Get info from the weather reader initialization
		double site_elevation = weather_reader.ms_solved_params.m_elev;		//[m]


		int tes_type = 1;

		int rec_type = var_receiver::REC_TYPE::EXTERNAL_CYLINDRICAL;
		switch (rec_type)
		{
			case var_receiver::REC_TYPE::EXTERNAL_CYLINDRICAL:
			{
				assign("rec_aspect", as_number("rec_height") / as_number("D_rec"));
				break;
			}
			case var_receiver::REC_TYPE::FLAT_PLATE:
				assign("rec_aspect", as_number("rec_height") / as_number("D_rec"));
				break;
		}

		assign("q_design", as_number("P_ref") / as_number("design_eff") * as_number("solarm"));

		// Set up "cmod_solarpilot.cpp" conversions as necessary
		assign("helio_optical_error", (ssc_number_t)(as_number("helio_optical_error_mrad")*1.E-3));

		// Set 'n_flux_x' and 'n_flux_y' here, for now
		assign("n_flux_y", 1);
		int n_rec_panels = as_integer("N_panels");
		assign("n_flux_x", (ssc_number_t)max(12, n_rec_panels));

		// Calculate system capacity instead of pass in
		double system_capacity = as_double("P_ref") * as_double("gross_net_conversion_factor") *1.E3;		//[kWe]

		// 'sf_model_type'
		// 0 = design field and tower/receiver geometry
		// 1 = design field
		// 2 = user field, calculate performance
		// 3 = user performance maps vs solar position
		int field_model_type = as_integer("field_model_type");

		// Run solarpilot right away to update values as needed
		solarpilot_invoke spi(this);
		util::matrix_t<double> mt_eta_map;
		util::matrix_t<double> mt_flux_maps;

		assign("is_optimize", 0);
		bool is_optimize = false;

		if (field_model_type == 0 || field_model_type == 1)	// Auto-design. Generate a new system (is_optimize = true) or field layout
		{
			if (field_model_type == 0)
			{
				assign("is_optimize", 1);
				is_optimize = true;
			}

			assign("calc_fluxmaps", 1);

			spi.run(weather_reader.m_weather_data_provider);

			if (is_optimize)
			{
				//Optimization iteration history
				vector<vector<double> > steps;
				vector<double> obj, flux;
				spi.getOptimizationSimulationHistory(steps, obj, flux);
				int nr = steps.size();
				if (nr > 0)
				{
					int nc = steps.front().size() + 2;
					ssc_number_t *ssc_hist = allocate("opt_history", nr, nc);
					for (int i = 0; i<nr; i++){

						for (size_t j = 0; j<steps.front().size(); j++)
							ssc_hist[i*nc + j] = (ssc_number_t)steps.at(i).at(j);
						ssc_hist[i*nc + nc - 2] = (ssc_number_t)obj.at(i);
						ssc_hist[i*nc + nc - 1] = (ssc_number_t)flux.at(i);
					}
				}
			}

			// receiver calculations
			double H_rec = spi.recs.front().rec_height.val;
			double rec_aspect = spi.recs.front().rec_aspect.Val();
			double THT = spi.sf.tht.val;

			int nr = (int)spi.layout.heliostat_positions.size();
			assign("N_hel", (ssc_number_t)nr);

			double A_sf = as_double("helio_height") * as_double("helio_width") * as_double("dens_mirror") * (double)nr;

			//update assignments for cost model
			assign("rec_height", var_data((ssc_number_t)H_rec));
			assign("rec_aspect", var_data((ssc_number_t)rec_aspect));
			assign("D_rec", var_data((ssc_number_t)(H_rec / rec_aspect)));
			assign("h_tower", var_data((ssc_number_t)THT));
			assign("A_sf", var_data((ssc_number_t)A_sf));

			double land_area_base = spi.land.land_area.Val();		//[acres] Land area occupied by heliostats
			assign("land_area_base", (ssc_number_t)land_area_base);

			ssc_number_t *ssc_hl = allocate("helio_positions", nr, 2);
			for (int i = 0; i<nr; i++)
			{
				ssc_hl[i * 2] = (ssc_number_t)spi.layout.heliostat_positions.at(i).location.x;
				ssc_hl[i * 2 + 1] = (ssc_number_t)spi.layout.heliostat_positions.at(i).location.y;
			}

			//collect the optical efficiency data and sun positions
			if (spi.fluxtab.zeniths.size() > 0 && spi.fluxtab.azimuths.size() > 0
				&& spi.fluxtab.efficiency.size() > 0)
			{
				size_t nvals = spi.fluxtab.efficiency.size();
				mt_eta_map.resize(nvals, 3);

				for (size_t i = 0; i<nvals; i++)
				{
					mt_eta_map(i, 0) = spi.fluxtab.azimuths[i] * 180. / CSP::pi;      //Convention is usually S=0, E<0, W>0 
					mt_eta_map(i, 1) = spi.fluxtab.zeniths[i] * 180. / CSP::pi;     //Provide zenith angle
					mt_eta_map(i, 2) = spi.fluxtab.efficiency[i];
				}
			}
			else
				throw exec_error("solarpilot", "failed to calculate a correct optical efficiency table");

			//collect the flux map data
			block_t<double> *flux_data = &spi.fluxtab.flux_surfaces.front().flux_data;  //there should be only one flux stack for SAM
			if (flux_data->ncols() > 0 && flux_data->nlayers() > 0)
			{

				int nflux_y = (int)flux_data->nrows();
				int nflux_x = (int)flux_data->ncols();

				mt_flux_maps.resize(nflux_y * flux_data->nlayers(), nflux_x);
				int cur_row = 0;

				for (size_t i = 0; i<flux_data->nlayers(); i++)
				{
					for (int j = 0; j<nflux_y; j++)
					{
						for (int k = 0; k<nflux_x; k++)
						{
							mt_flux_maps(cur_row, k) = flux_data->at(j, k, i);
							//fluxdata[cur_row * nflux_x + k] = (float)flux_data->at(j, k, i);
						}
						cur_row++;
					}
				}
			}
			else
				throw exec_error("solarpilot", "failed to calculate a correct flux map table");
		}
		else if (field_model_type == 2)
		{
			// only calculates a flux map, so need to "assign" 'helio_positions_in'
			util::matrix_t<double> helio_pos_temp = as_matrix("helio_positions");
			int n_h_rows = helio_pos_temp.nrows();
			ssc_number_t *p_helio_positions_in = allocate("helio_positions_in", n_h_rows, 2);
			for (int i = 0; i < n_h_rows; i++)
			{
				p_helio_positions_in[i * 2] = (ssc_number_t)helio_pos_temp(i, 0);
				p_helio_positions_in[i * 2 + 1] = (ssc_number_t)helio_pos_temp(i, 1);
			}
			assign("N_hel", (ssc_number_t)n_h_rows);
			// 'calc_fluxmaps' should be true
			assign("calc_fluxmaps", 1);

			spi.run(weather_reader.m_weather_data_provider);

			//collect the optical efficiency data and sun positions
			if (spi.fluxtab.zeniths.size() > 0 && spi.fluxtab.azimuths.size() > 0
				&& spi.fluxtab.efficiency.size() > 0)
			{
				size_t nvals = spi.fluxtab.efficiency.size();
				mt_eta_map.resize(nvals, 3);

				for (size_t i = 0; i<nvals; i++)
				{
					mt_eta_map(i, 0) = spi.fluxtab.azimuths[i] * 180. / CSP::pi;	//Convention is usually S=0, E<0, W>0 
					mt_eta_map(i, 1) = spi.fluxtab.zeniths[i] * 180. / CSP::pi;     //Provide zenith angle
					mt_eta_map(i, 2) = spi.fluxtab.efficiency[i];
				}
			}
			else
				throw exec_error("solarpilot", "failed to calculate a correct optical efficiency table");

			//collect the flux map data
			block_t<double> *flux_data = &spi.fluxtab.flux_surfaces.front().flux_data;  //there should be only one flux stack for SAM
			if (flux_data->ncols() > 0 && flux_data->nlayers() > 0)
			{

				int nflux_y = (int)flux_data->nrows();
				int nflux_x = (int)flux_data->ncols();

				mt_flux_maps.resize(nflux_y * flux_data->nlayers(), nflux_x);

				int cur_row = 0;

				for (size_t i = 0; i<flux_data->nlayers(); i++)
				{
					for (int j = 0; j<nflux_y; j++)
					{
						for (int k = 0; k<nflux_x; k++)
						{
							mt_flux_maps(cur_row, k) = flux_data->at(j, k, i);
							//fluxdata[cur_row * nflux_x + k] = (float)flux_data->at(j, k, i);
						}
						cur_row++;
					}
				}
			}
			else
				throw exec_error("solarpilot", "failed to calculate a correct flux map table");

			int nr = as_integer("N_hel");
			double A_sf = as_double("helio_height") * as_double("helio_width") * as_double("dens_mirror") * (double)nr;
			assign("A_sf", (ssc_number_t)A_sf);
		}
		else if (field_model_type == 3)
		{
			assign("calc_fluxmaps", 0);

			// The following optional inputs must be set here:
			assign("A_sf", as_number("A_sf_in"));
		}
		else
		{
			string msg = util::format("One field performance modeling option must be set to True");

			throw exec_error("MSPT CSP Solver", msg);
		}

		if( tes_type != 1 )
		{
			throw exec_error("MSPT CSP Solver", "Thermocline thermal energy storage is not yet supported by the new CSP Solver and Dispatch Optimization models.\n");
		}

        
        // Set steps per hour
		C_csp_solver::S_sim_setup sim_setup;
		sim_setup.m_sim_time_start = as_double("time_start");		//[s] time at beginning of first time step
		sim_setup.m_sim_time_end = as_double("time_stop");          //[s] time at end of last time step
        
        int steps_per_hour = (int)as_double("time_steps_per_hour");		//[-]

        //if the number of steps per hour is not provided (=-1), then assign it based on the weather file step
        if( steps_per_hour < 0 )
        {
            double sph_d = 3600. / weather_reader.m_weather_data_provider->step_sec();
            steps_per_hour = (int)( sph_d + 1.e-5 );
            if( (double)steps_per_hour != sph_d )
                throw spexception("The time step duration must be evenly divisible within an hour.");
        }

        int n_steps_fixed = steps_per_hour * 8760;	//[-]
        if( as_boolean("vacuum_arrays") )
        {
            n_steps_fixed = steps_per_hour * (int)( (sim_setup.m_sim_time_end - sim_setup.m_sim_time_start)/3600. );
        }
        //int n_steps_fixed = (int)( (sim_setup.m_sim_time_end - sim_setup.m_sim_time_start) * steps_per_hour / 3600. ) ; 
		sim_setup.m_report_step = 3600.0 / (double)steps_per_hour;	//[s]





		// ***********************************************
		// ***********************************************
		// Power cycle
		// ***********************************************
		// ***********************************************
		C_csp_power_cycle * p_csp_power_cycle;
		// Steam Rankine and User Defined power cycle classes
		C_pc_Rankine_indirect_224 rankine_pc;
		// sCO2 power cycle class
		C_pc_sco2 sco2_pc;

		// Logic to choose between steam and sco2 power cycle 
		int pb_tech_type = as_integer("pc_config");
		if (pb_tech_type == 0 || pb_tech_type == 1)
		{
			C_pc_Rankine_indirect_224::S_params *pc = &rankine_pc.ms_params;
			pc->m_P_ref = as_double("P_ref");
			pc->m_eta_ref = as_double("design_eff");
			pc->m_T_htf_hot_ref = as_double("T_htf_hot_des");
			pc->m_T_htf_cold_ref = as_double("T_htf_cold_des");
			pc->m_cycle_max_frac = as_double("cycle_max_frac");
			pc->m_cycle_cutoff_frac = as_double("cycle_cutoff_frac");
			pc->m_q_sby_frac = as_double("q_sby_frac");
			pc->m_startup_time = as_double("startup_time");
			pc->m_startup_frac = as_double("startup_frac");
			pc->m_htf_pump_coef = as_double("pb_pump_coef");
			pc->m_pc_fl = as_integer("rec_htf");							// power cycle HTF is same as receiver HTF
			pc->m_pc_fl_props = as_matrix("field_fl_props");

			if (pb_tech_type == 0)
			{
				pc->m_dT_cw_ref = as_double("dT_cw_ref");
				pc->m_T_amb_des = as_double("T_amb_des");
				pc->m_P_boil = as_double("P_boil");
				pc->m_CT = as_integer("CT");					// cooling tech type: 1=evaporative, 2=air, 3=hybrid	
				pc->m_tech_type = as_integer("tech_type");		// 1: Fixed, 3: Sliding
				if (!(pc->m_tech_type == 1 || pc->m_tech_type == 3 || pc->m_tech_type ==5 || pc->m_tech_type==6))
				{
					std::string tech_msg = util::format("tech_type must be either 1 (fixed pressure) or 3 (sliding). Input was %d."
						" Simulation proceeded with fixed pressure", pc->m_tech_type);
					pc->m_tech_type = 1;
				}
				pc->m_T_approach = as_double("T_approach");
				pc->m_T_ITD_des = as_double("T_ITD_des");
				pc->m_P_cond_ratio = as_double("P_cond_ratio");
				pc->m_pb_bd_frac = as_double("pb_bd_frac");
				pc->m_P_cond_min = as_double("P_cond_min");
				pc->m_n_pl_inc = as_integer("n_pl_inc");

				//parameters for radiative cooling with cold storage
				C_csp_cold_tes *two_tank = &rankine_pc.mc_two_tank_ctes;	//pointer for two tank
				C_csp_stratified_tes *stratified = &rankine_pc.mc_stratified_ctes; //pointer for stratified
			
				two_tank->ms_params.m_ctes_type = as_integer("ctes_type");
				stratified->ms_params.m_ctes_type = as_integer("ctes_type");

				if (two_tank->ms_params.m_ctes_type == 2)
				{
					two_tank->ms_params.m_h_tank_min = as_double("h_ctes_tank_min");
					two_tank->ms_params.m_ts_hours = as_double("ctes_tshours");
					two_tank->ms_params.m_h_tank = as_double("h_ctes_tank");
					two_tank->ms_params.m_u_tank = as_double("u_ctes_tank");
					two_tank->ms_params.m_tank_pairs = as_integer("ctes_tankpairs");
					two_tank->ms_params.m_T_field_in_des = as_double("T_ctes_cold_design");
					two_tank->ms_params.m_T_field_out_des = as_double("T_ctes_warm_design");
					two_tank->ms_params.m_T_tank_hot_ini = as_double("T_ctes_warm_ini");
					two_tank->ms_params.m_T_tank_cold_ini = as_double("T_ctes_cold_ini");
					two_tank->ms_params.m_f_V_hot_ini = as_double("f_ctes_warm_ini");
				}
				if (two_tank->ms_params.m_ctes_type >2)
				{
					stratified->ms_params.m_h_tank_min = 0;								//hardcode zero minimum height for stratified tanks.
					stratified->ms_params.m_ts_hours = as_double("ctes_tshours");
					stratified->ms_params.m_h_tank = as_double("h_ctes_tank");
					stratified->ms_params.m_u_tank = as_double("u_ctes_tank");
					stratified->ms_params.m_tank_pairs = as_integer("ctes_tankpairs");
					stratified->ms_params.m_T_field_in_des = as_double("T_ctes_cold_design");
					stratified->ms_params.m_T_field_out_des = as_double("T_ctes_warm_design");
					stratified->ms_params.m_T_tank_hot_ini = as_double("T_ctes_warm_ini");
					stratified->ms_params.m_T_tank_cold_ini = as_double("T_ctes_cold_ini");
					stratified->ms_params.m_f_V_hot_ini = as_double("f_ctes_warm_ini");
				}
				rankine_pc.mc_radiator.ms_params.m_field_fl = as_integer("ctes_field_fl");
				rankine_pc.mc_radiator.ms_params.RM = as_double("rad_multiplier");
				rankine_pc.mc_radiator.ms_params.Asolar_refl=as_double("helio_area_tot");
				rankine_pc.mc_radiator.ms_params.m_dot_panel = as_double("m_dot_radpanel");
				rankine_pc.mc_radiator.ms_params.n = as_integer("n_rad_tubes");
				rankine_pc.mc_radiator.ms_params.W = as_double("W_rad_tubes");
				rankine_pc.mc_radiator.ms_params.L = as_double("L_rad");
				rankine_pc.mc_radiator.ms_params.th = as_double("th_rad_panel");
				rankine_pc.mc_radiator.ms_params.D = as_double("D_rad_tubes");
				rankine_pc.mc_radiator.ms_params.k_panel = as_double("k_panel");
				rankine_pc.mc_radiator.ms_params.epsilon = as_double("epsilon_radtop");
				rankine_pc.mc_radiator.ms_params.epsilonb = as_double("epsilon_radbot");
				rankine_pc.mc_radiator.ms_params.epsilong = as_double("epsilon_radgrnd");
				rankine_pc.mc_radiator.ms_params.Lsec = as_double("L_rad_sections");
				rankine_pc.mc_radiator.ms_params.epsilon_HX = as_double("epsilon_radHX");
				rankine_pc.mc_radiator.ms_params.radfield_dp = as_double("rad_pressuredrop");

				size_t n_F_wc = 0;
				ssc_number_t *p_F_wc = as_array("F_wc", &n_F_wc);
				pc->m_F_wc.resize(n_F_wc, 0.0);
				for (size_t i = 0; i < n_F_wc; i++)
					pc->m_F_wc[i] = (double)p_F_wc[i];

				// Set User Defined cycle parameters to appropriate values
				pc->m_is_user_defined_pc = false;
				pc->m_W_dot_cooling_des = std::numeric_limits<double>::quiet_NaN();
			}
			else if (pb_tech_type == 1)
			{
				pc->m_is_user_defined_pc = true;

				// User-Defined Cycle Parameters
				pc->m_T_amb_des = as_double("ud_T_amb_des");	//[C]
				pc->m_W_dot_cooling_des = as_double("ud_f_W_dot_cool_des") / 100.0*as_double("P_ref");	//[MWe]
				pc->m_m_dot_water_des = as_double("ud_m_dot_water_cool_des");		//[kg/s]

				// Also need lower and upper levels for the 3 independent variables...
				pc->m_T_htf_low = as_double("ud_T_htf_low");			//[C]
				pc->m_T_htf_high = as_double("ud_T_htf_high");			//[C]
				pc->m_T_amb_low = as_double("ud_T_amb_low");			//[C]
				pc->m_T_amb_high = as_double("ud_T_amb_high");			//[C]
				pc->m_m_dot_htf_low = as_double("ud_m_dot_htf_low");	//[-]
				pc->m_m_dot_htf_high = as_double("ud_m_dot_htf_high");	//[-]

				// User-Defined Cycle Off-Design Tables 
				pc->mc_T_htf_ind = as_matrix("ud_T_htf_ind_od");
				pc->mc_T_amb_ind = as_matrix("ud_T_amb_ind_od");
				pc->mc_m_dot_htf_ind = as_matrix("ud_m_dot_htf_ind_od");
                pc->mc_combined_ind = as_matrix("ud_ind_od");
			}

			// Set pointer to parent class
			p_csp_power_cycle = &rankine_pc;
		}
		else if (pb_tech_type == 2)
		{
			int is_sco2_preprocess = as_integer("is_sco2_preprocess");

			if (is_sco2_preprocess == 1)
			{
				double _sco2_P_high_limit = as_double("_sco2_P_high_limit");   //[MPa]
				double P_high_limit = as_double("P_high_limit");			   //[MPa]
				if (!std::isfinite(_sco2_P_high_limit) || _sco2_P_high_limit != P_high_limit)
				{
					throw exec_error("tcsmolten_salt", util::format("The upper pressure limit used to generate"
						" the preprocessed sCO2 cycle data, %lg [MPa], is not equal to the input upper pressure limit %lg [MPa]",
						_sco2_P_high_limit, P_high_limit));
				}

				double _sco2_P_ref = as_double("_sco2_P_ref");
				double P_ref_input = as_double("P_ref");
				if (!std::isfinite(_sco2_P_ref) || _sco2_P_ref != P_ref_input)
				{
					throw exec_error("tcsmolten_salt", util::format("The cycle gross power used to generate"
						" the preprocessed sCO2 cycle data, %lg [MWe], is not equal to the input cycle gross power %lg [MWe]",
						_sco2_P_ref, P_ref_input));
				}

				double _sco2_T_amb_des = as_double("_sco2_T_amb_des");
				double T_amb_des_input = as_double("sco2_T_amb_des");
				if (!std::isfinite(_sco2_T_amb_des) || _sco2_T_amb_des != T_amb_des_input)
				{
					throw exec_error("tcsmolten_salt", util::format("The design ambient temperature used to generate"
						" the preprocessed sCO2 cycle data, %lg [C], is not equal to the input design ambient temperature %lg [C]",
						_sco2_T_amb_des, T_amb_des_input));
				}

				double _sco2_T_approach = as_double("_sco2_T_approach");
				double T_approach_input = as_double("sco2_T_approach");
				if (!std::isfinite(_sco2_T_approach) || _sco2_T_approach != T_approach_input)
				{
					throw exec_error("tcsmolten_salt", util::format("The compressor approach temperature used to generate"
						" the preprocessed sCO2 cycle data, %lg [C], is not equal to the input compressor approach temperature %lg [C]",
						_sco2_T_approach, T_approach_input));
				}

				double _sco2_T_htf_hot_des = as_double("_sco2_T_htf_hot_des");
				double T_htf_hot_des_input = as_double("T_htf_hot_des");
				if (!std::isfinite(_sco2_T_htf_hot_des) || _sco2_T_htf_hot_des != T_htf_hot_des_input)
				{
					throw exec_error("tcsmolten_salt", util::format("The HTF hot temperature uesd to generate"
						" the preprocessed sCO2 cycle data, %lg [C], is not equal to the input HTF hot temperature",
						_sco2_T_htf_hot_des, T_htf_hot_des_input));
				}

				double _sco2_deltaT_PHX = as_double("_sco2_deltaT_PHX");
				double deltaT_PHX_input = as_double("deltaT_PHX");
				if (!std::isfinite(_sco2_deltaT_PHX) || _sco2_deltaT_PHX != deltaT_PHX_input)
				{
					throw exec_error("tcsmolten_salt", util::format("The PHX approach temperature used to generate"
						" the preprocessed sCO2 cycle data, %lg [C], is not equal to the input PHX approach temperature",
						_sco2_deltaT_PHX, deltaT_PHX_input));
				}

				double _sco2_design_eff = as_double("_sco2_design_eff");
				double design_eff_input = as_double("design_eff");
				if (!std::isfinite(_sco2_design_eff) || _sco2_design_eff != design_eff_input)
				{
					throw exec_error("tcsmolten_salt", util::format("The thermal efficiency used to generate"
						" the preprocessed sCO2 cycle data, %lg, is not equal to the input thermal efficiency",
						_sco2_design_eff, design_eff_input));
				}

				double _sco2_eta_c = as_double("_sco2_eta_c");
				double eta_c_input = as_double("eta_c");
				if (!std::isfinite(_sco2_eta_c) || _sco2_eta_c != eta_c_input)
				{
					throw exec_error("tcsmolten_salt", util::format("The compressor isentropic efficiency used to generate"
						" the preprocessed sCO2 cycle data, %lg, is not equal to the input compressor isentropic efficiency",
						_sco2_eta_c, eta_c_input));
				}

				double _sco2_eta_t = as_double("_sco2_eta_t");
				double eta_t_input = as_double("eta_t");
				if (!std::isfinite(_sco2_eta_t) || _sco2_eta_t != eta_t_input)
				{
					throw exec_error("tcsmolten_salt", util::format("The turbine isentropic efficiency used to generate"
						" the preprocessed sCO2 cycle data, %lg, is not equal to the input turbine isentropic efficiency",
						_sco2_eta_t, eta_t_input));
				}

				double _sco2_recup_eff_max = as_double("_sco2_recup_eff_max");
				double recup_eff_max = as_double("recup_eff_max");
				if (!std::isfinite(_sco2_recup_eff_max) || _sco2_recup_eff_max != recup_eff_max)
				{
					throw exec_error("tcsmolten_salt", util::format("The max recuperator effectiveness used to generate"
						" the preprocessed sCO2 cycle data, %lg, is not equal to the input max recuperator effectiveness",
						_sco2_recup_eff_max, recup_eff_max));
				}

				// ****************************************************
				// Setup UDPC model
				// ****************************************************
				C_pc_Rankine_indirect_224::S_params *pc = &rankine_pc.ms_params;
				pc->m_P_ref = as_double("P_ref");					//[MWe]
				pc->m_eta_ref = as_double("design_eff");			//[-]
				pc->m_T_htf_hot_ref = as_double("T_htf_hot_des");	//[C]
				
				double T_htf_cold_calc = as_double("sco2ud_T_htf_cold_calc");		//[C]
				double T_rec_htf_cold = as_double("T_htf_cold_des");				//[C]
				if (T_htf_cold_calc != T_rec_htf_cold)
				{
					assign("T_htf_cold_des", T_htf_cold_calc);							//[C]
					log(util::format("\nThe user input receiver design HTF cold temperature, %lg [C], was reset"
						" to the calculated sCO2 cycle HTF cold return temperature, %lg [C]\n", T_rec_htf_cold, T_htf_cold_calc), SSC_WARNING);
				}
				pc->m_T_htf_cold_ref = T_htf_cold_calc;		//[C]
				
				pc->m_cycle_max_frac = as_double("cycle_max_frac");		//[-]
				
				double cycle_cutoff_frac_sys = as_double("cycle_cutoff_frac");
				double cycle_cutoff_frac_sco2 = as_double("sco2ud_m_dot_htf_low");
				if (cycle_cutoff_frac_sys < cycle_cutoff_frac_sco2)
				{
					log(util::format("The user input cutoff fraction, %lg, was reset to the minimum allowable cutoff fraction"
						" for this sCO2 cycle off-design model, %lg\n", cycle_cutoff_frac_sys, cycle_cutoff_frac_sco2), SSC_WARNING);
					cycle_cutoff_frac_sys = cycle_cutoff_frac_sco2;
					assign("cycle_cutoff_frac", cycle_cutoff_frac_sys);
				}
				pc->m_cycle_cutoff_frac = cycle_cutoff_frac_sys;		//[-]
				
				pc->m_q_sby_frac = as_double("q_sby_frac");				//[-]
				pc->m_startup_time = as_double("startup_time");			//[hr]
				pc->m_startup_frac = as_double("startup_frac");			//[-]
				pc->m_htf_pump_coef = as_double("pb_pump_coef");		//[kWe/kg/s]
				pc->m_pc_fl = as_integer("rec_htf");					//[-] power cycle HTF is same as receiver HTF
				pc->m_pc_fl_props = as_matrix("field_fl_props");		//[-]

				// User-Defined Cycle Parameters
				pc->m_is_user_defined_pc = true;

				// User-Defined Cycle Parameters
				pc->m_T_amb_des = as_double("sco2_T_amb_des");			//[C]
				pc->m_W_dot_cooling_des = as_double("fan_power_perc_net") / 100.0*as_double("P_ref");	//[MWe]
				pc->m_m_dot_water_des = 0.0;		//[kg/s]

				// Also need lower and upper levels for the 3 independent variables...
				pc->m_T_htf_low = as_double("sco2ud_T_htf_low");			//[C]
				pc->m_T_htf_high = as_double("sco2ud_T_htf_high");			//[C]
				pc->m_T_amb_low = as_double("sco2ud_T_amb_low");			//[C]
				pc->m_T_amb_high = as_double("sco2ud_T_amb_high");			//[C]
				pc->m_m_dot_htf_low = as_double("sco2ud_m_dot_htf_low");	//[-]
				pc->m_m_dot_htf_high = as_double("sco2ud_m_dot_htf_high");	//[-]

				// User-Defined Cycle Off-Design Tables 
				pc->mc_T_htf_ind = as_matrix("sco2ud_T_htf_ind_od");
				pc->mc_T_amb_ind = as_matrix("sco2ud_T_amb_ind_od");
				pc->mc_m_dot_htf_ind = as_matrix("sco2ud_m_dot_htf_ind_od");

				p_csp_power_cycle = &rankine_pc;
			}
			else
			{
				// ****************************************
				// C_sco2_recomp_csp::S_des_par  User Defined Parameters
				// ****************************************
				C_sco2_recomp_csp::S_des_par sco2_rc_csp_par;
				// System Design Parameters
				sco2_rc_csp_par.m_hot_fl_code = as_integer("rec_htf");					//[-]
				sco2_rc_csp_par.mc_hot_fl_props = as_matrix("field_fl_props");			//[-]
				sco2_rc_csp_par.m_T_htf_hot_in = as_double("T_htf_hot_des") + 273.15;	//[K] Design HTF hot temp to power cycle
				sco2_rc_csp_par.m_phx_dt_hot_approach = as_double("deltaT_PHX");		//[K/C]
				sco2_rc_csp_par.m_T_amb_des = as_double("sco2_T_amb_des") + 273.15;		//[K] Design ambient temp, convert from C
				sco2_rc_csp_par.m_dt_mc_approach = as_double("sco2_T_approach");		//[K/C]
				sco2_rc_csp_par.m_elevation = site_elevation;							//[m]
				sco2_rc_csp_par.m_W_dot_net = as_double("P_ref")*1.E3;					//[kWe]

				sco2_rc_csp_par.m_cycle_config = as_integer("sco2_cycle_config");		//[-] 1 = recompression, 2 = partial cooling

				// Hardcode for now that design method iterates on UA_recup_total to hit target etas
				sco2_rc_csp_par.m_design_method = 1;
				sco2_rc_csp_par.m_eta_thermal = as_double("design_eff");				//[-]
				
				// Hardcode that recompression cycle is ok
				sco2_rc_csp_par.m_is_recomp_ok = 1;

				sco2_rc_csp_par.m_P_high_limit = as_double("P_high_limit")*1.E3;	//[kPa]
				sco2_rc_csp_par.m_fixed_P_mc_out = false;
				// Hardcode don't fix pressure ratio
				sco2_rc_csp_par.m_fixed_PR_mc = false;				

				// ****************************************
				// ****************************************
				// C_sco2_recomp_csp::S_des_par  Hardcoded Parameters (for now...)
				// ****************************************
				// Cycle design parameters
				std::vector<double> DP_LT(2);
				/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
				DP_LT[0] = 0;
				DP_LT[1] = 0;
				/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
				std::vector<double> DP_HT(2);
				DP_HT[0] = 0;
				DP_HT[1] = 0;
				/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
				std::vector<double> DP_PC(2);
				DP_PC[0] = 0;
				DP_PC[1] = 0;
				/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
				std::vector<double> DP_PHX(2);
				DP_PHX[0] = 0;
				DP_PHX[1] = 0;
				sco2_rc_csp_par.m_DP_LT = DP_LT;
				sco2_rc_csp_par.m_DP_HT = DP_HT;
				sco2_rc_csp_par.m_DP_PC = DP_PC;
				sco2_rc_csp_par.m_DP_PHX = DP_PHX;
				sco2_rc_csp_par.m_N_sub_hxrs = 10;

				sco2_rc_csp_par.m_N_turbine = 3000.0;

				sco2_rc_csp_par.m_tol = 1.E-3;
				sco2_rc_csp_par.m_opt_tol = 1.E-3;
				
				// Cycle Design Parameters
				sco2_rc_csp_par.m_LT_eff_max = as_double("recup_eff_max");		//[-]
				sco2_rc_csp_par.m_HT_eff_max = as_double("recup_eff_max");		//[-]
				sco2_rc_csp_par.m_eta_mc = as_double("eta_c");					//[-]
				sco2_rc_csp_par.m_eta_rc = as_double("eta_c");					//[-]
				if (sco2_rc_csp_par.m_cycle_config == 2)
					sco2_rc_csp_par.m_eta_pc = as_double("eta_c");		   //[-]
				else
					sco2_rc_csp_par.m_eta_pc = sco2_rc_csp_par.m_eta_mc;
				sco2_rc_csp_par.m_eta_t = as_double("eta_t");					//[-]

				// PHX design parameters
				sco2_rc_csp_par.m_des_objective_type = 1;		//[-] Optimize design to maximize efficiency
				sco2_rc_csp_par.m_phx_dt_cold_approach = sco2_rc_csp_par.m_phx_dt_hot_approach;	//[K/C]

				// Air cooler parameters
				sco2_rc_csp_par.m_frac_fan_power = as_double("fan_power_perc_net") / 100.0;	//[-]
				sco2_rc_csp_par.m_deltaP_cooler_frac = 0.002;		//[-]

				sco2_pc.ms_params.ms_mc_sco2_recomp_params = sco2_rc_csp_par;

				bool is_preprocess_udpc = true;		// "is_preprocess_udpc"

				if (is_preprocess_udpc)
				{
					// For try/catch below
					int out_type = -1;
					std::string out_msg = "";

					//log("Calculating sCO2 design point...", SSC_WARNING);
					update("Calculating sCO2 design point...", 0.0);

					// Construction class and design system
					C_sco2_recomp_csp c_sco2_recomp_csp;

					// Pass through callback function and pointer
					c_sco2_recomp_csp.mf_callback_update = ssc_cmod_update;
					c_sco2_recomp_csp.mp_mf_update = (void*)(this);

					try
					{
						c_sco2_recomp_csp.design(sco2_rc_csp_par);
					}
					catch (C_csp_exception &csp_exception)
					{
						// Report warning before exiting with error
						while (c_sco2_recomp_csp.mc_messages.get_message(&out_type, &out_msg))
						{
							log(out_msg + "\n");
							log("\n");
						}

						throw exec_error("sco2_csp_system", csp_exception.m_error_message);
					}

					// Get sCO2 design outputs
					double T_htf_cold_calc = c_sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_T_h_out;		//[K]
					log("sCO2 design point calculations complete.", SSC_WARNING);
					double T_rec_htf_cold = as_double("T_htf_cold_des");			//[C]
					assign("T_htf_cold_des", T_htf_cold_calc - 273.15);				//[C]
					log(util::format("\nThe user input receiver design HTF cold temperature, %lg [C], was reset"
						" to the calculated sCO2 cycle HTF cold return temperature, %lg [C]\n", T_rec_htf_cold, T_htf_cold_calc - 273.15), SSC_WARNING);
					update("Preprocessing cycle off-design...", 0.0);

					// Get user-defined power cycle parameters
					// HTF temperature parametric
					double T_htf_hot_low = c_sco2_recomp_csp.get_design_par()->m_T_htf_hot_in - 273.15 - 20.0;	//[C]
					double T_htf_hot_high = c_sco2_recomp_csp.get_design_par()->m_T_htf_hot_in - 273.15 + 15.0;	//[C]
					int n_T_htf_hot_in = 5;				//[-]

					// Ambient temperature parametric
					double T_amb_low = 0.0;				//[C]
					double T_amb_high = std::max(sco2_rc_csp_par.m_T_amb_des - 273.15 + 5.0, 45.0);			//[C]
					int n_T_amb_in = 10;				//[-]

					// HTF mass flow rate parametric
					double cycle_f_min = as_double("cycle_cutoff_frac");		//[-]
					bool is_des_rc = c_sco2_recomp_csp.get_design_solved()->ms_rc_cycle_solved.m_is_rc;	//[-]
					double sco2_f_min = 0.5;
					std::string cycle_type = "recompression";
					if (!is_des_rc)
					{
						sco2_f_min = 0.7;
						cycle_type = "simple";
					}
					if (cycle_f_min < sco2_f_min)
					{
						log(util::format("The user input cutoff fraction, %lg, was reset to the minimum allowable cutoff fraction"
							" for this sCO2 %s cycle off-design model, %lg\n", cycle_f_min, cycle_type.c_str(), sco2_f_min), SSC_WARNING);
						update("Preprocessing cycle off-design...", 0.0);
						cycle_f_min = sco2_f_min;
						assign("cycle_cutoff_frac", cycle_f_min);
					}
					double m_dot_htf_ND_low = min(0.95, cycle_f_min);	//[-]
					// Design is always = 1.0, so high needs to be a value > 1.0
					double m_dot_htf_ND_high = 1.05;			// max(1.05, as_double("cycle_max_frac"));		// + 0.01;		//[-]
					int n_m_dot_htf_ND_in = 10;

					util::matrix_t<double> T_htf_parametrics, T_amb_parametrics, m_dot_htf_ND_parametrics;

					try
					{
						c_sco2_recomp_csp.generate_ud_pc_tables(T_htf_hot_low, T_htf_hot_high, n_T_htf_hot_in,
							T_amb_low, T_amb_high, n_T_amb_in,
							m_dot_htf_ND_low, m_dot_htf_ND_high, n_m_dot_htf_ND_in,
							T_htf_parametrics, T_amb_parametrics, m_dot_htf_ND_parametrics);
					}
					catch (C_csp_exception &csp_exception)
					{
						// Report warning before exiting with error
						while (c_sco2_recomp_csp.mc_messages.get_message(&out_type, &out_msg))
						{
							log(out_msg);
						}

						throw exec_error("sco2_csp_system", csp_exception.m_error_message);
					}

					int ncols = T_htf_parametrics.ncols();

					util::matrix_t<float> &p_udpc_T_htf_hot = allocate_matrix("ud_T_htf_ind_od_out", n_T_htf_hot_in, ncols);
					for (int i = 0; i < n_T_htf_hot_in; i++)
					{
						for (int j = 0; j < ncols; j++)
						{
							p_udpc_T_htf_hot(i, j) = (float)T_htf_parametrics(i, j);
						}
					}

					util::matrix_t<float> &p_udpc_T_amb = allocate_matrix("ud_T_amb_ind_od_out", n_T_amb_in, ncols);
					for (int i = 0; i < n_T_amb_in; i++)
					{
						for (int j = 0; j < ncols; j++)
						{
							p_udpc_T_amb(i, j) = (float)T_amb_parametrics(i, j);
						}
					}

					util::matrix_t<float> &p_udpc_m_dot_htf = allocate_matrix("ud_m_dot_htf_ind_od_out", n_m_dot_htf_ND_in, ncols);
					for (int i = 0; i < n_m_dot_htf_ND_in; i++)
					{
						for (int j = 0; j < ncols; j++)
						{
							p_udpc_m_dot_htf(i, j) = (float)m_dot_htf_ND_parametrics(i, j);
						}
					}

					log("sCO2 off-design performance calculations for lookup tables complete.", SSC_WARNING);
					update("sCO2 preprocess complete", 100.0);

					// ****************************************************
					// ****************************************************
					// Now, setup UDPC model
					// ****************************************************
					C_pc_Rankine_indirect_224::S_params *pc = &rankine_pc.ms_params;
					pc->m_P_ref = as_double("P_ref");
					pc->m_eta_ref = as_double("design_eff");
					pc->m_T_htf_hot_ref = as_double("T_htf_hot_des");
					pc->m_T_htf_cold_ref = c_sco2_recomp_csp.get_design_solved()->ms_phx_des_solved.m_T_h_out - 273.15;
					pc->m_cycle_max_frac = as_double("cycle_max_frac");
					pc->m_cycle_cutoff_frac = as_double("cycle_cutoff_frac");
					pc->m_q_sby_frac = as_double("q_sby_frac");
					pc->m_startup_time = as_double("startup_time");
					pc->m_startup_frac = as_double("startup_frac");
					pc->m_htf_pump_coef = as_double("pb_pump_coef");
					pc->m_pc_fl = as_integer("rec_htf");							// power cycle HTF is same as receiver HTF
					pc->m_pc_fl_props = as_matrix("field_fl_props");

					// User-Defined Cycle Parameters
					pc->m_is_user_defined_pc = true;

					pc->m_T_amb_des = c_sco2_recomp_csp.get_design_par()->m_T_amb_des - 273.15;	//[C]
					pc->m_W_dot_cooling_des = as_double("fan_power_perc_net") / 100.0*as_double("P_ref");	//[MWe]
					pc->m_m_dot_water_des = 0.0;		//[kg/s]

					// Also need lower and upper levels for the 3 independent variables...
					pc->m_T_htf_low = T_htf_hot_low;			//[C]
					pc->m_T_htf_high = T_htf_hot_high;			//[C]
					pc->m_T_amb_low = T_amb_low;				//[C]
					pc->m_T_amb_high = T_amb_high;				//[C]
					pc->m_m_dot_htf_low = m_dot_htf_ND_low;		//[-]
					pc->m_m_dot_htf_high = m_dot_htf_ND_high;	//[-]

					// User-Defined Cycle Off-Design Tables 
					pc->mc_T_htf_ind = T_htf_parametrics;
					pc->mc_T_amb_ind = T_amb_parametrics;
					pc->mc_m_dot_htf_ind = m_dot_htf_ND_parametrics;

					p_csp_power_cycle = &rankine_pc;
				}
				else
				{
					// ****************************************
					// ****************************************
					// C_sco2_recomp_csp::S_des_par   User Defined Parameters
					// ****************************************
					sco2_pc.ms_params.m_cycle_max_frac = as_double("cycle_max_frac");			//[-]
					sco2_pc.ms_params.m_cycle_cutoff_frac = as_double("cycle_cutoff_frac");		//[-]
					sco2_pc.ms_params.m_q_sby_frac = as_double("q_sby_frac");					//[-]
					sco2_pc.ms_params.m_startup_time = as_double("startup_time");				//[hr]
					sco2_pc.ms_params.m_startup_frac = as_double("startup_frac");				//[-]
					sco2_pc.ms_params.m_htf_pump_coef = as_double("pb_pump_coef");				//[kW/kg/s]

					p_csp_power_cycle = &sco2_pc;
				}
			}
		}
		else
		{
			std::string err_msg = util::format("The specified power cycle configuration, %d, does not exist. See SSC Input Table for help.\n", pb_tech_type);
			log(err_msg, SSC_WARNING);
			return;
		}

		// Set power cycle outputs common to all power cycle technologies
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_ETA_THERMAL, allocate("eta", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_Q_DOT_HTF, allocate("q_pb", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_M_DOT_HTF, allocate("m_dot_pc", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_Q_DOT_STARTUP, allocate("q_dot_pc_startup", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_W_DOT, allocate("P_cycle", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_T_HTF_IN, allocate("T_pc_in", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_T_HTF_OUT, allocate("T_pc_out", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_M_DOT_WATER, allocate("m_dot_water_pc", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_T_COND_OUT, allocate("T_cond_out", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_T_COLD, allocate("T_cold", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_M_COLD, allocate("m_cold", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_M_WARM, allocate("m_warm", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_T_WARM, allocate("T_warm", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_T_RADOUT, allocate("T_rad_out", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_P_COND, allocate("P_cond", n_steps_fixed), n_steps_fixed);
		p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_RADCOOL_CNTRL, allocate("radcool_control", n_steps_fixed), n_steps_fixed);


		//heliostat field class
		C_pt_sf_perf_interp heliostatfield;

		heliostatfield.ms_params.m_p_start = as_double("p_start");		//[kWe-hr] Heliostat startup energy
		heliostatfield.ms_params.m_p_track = as_double("p_track");		//[kWe] Heliostat tracking power
		heliostatfield.ms_params.m_hel_stow_deploy = as_double("hel_stow_deploy");	// N/A
		heliostatfield.ms_params.m_v_wind_max = as_double("v_wind_max");			// N/A
		heliostatfield.ms_params.m_n_flux_x = (int) as_double("n_flux_x");		// sp match
		heliostatfield.ms_params.m_n_flux_y = (int) as_double("n_flux_y");		// sp match

		if (field_model_type != 3)
		{
			heliostatfield.ms_params.m_eta_map = mt_eta_map;
			heliostatfield.ms_params.m_eta_map_aod_format = false;
			heliostatfield.ms_params.m_flux_maps = mt_flux_maps;
			heliostatfield.ms_params.m_N_hel = as_integer("N_hel");
			heliostatfield.ms_params.m_A_sf = as_double("A_sf");		//[m2]
		}
		else
		{
			heliostatfield.ms_params.m_eta_map = as_matrix("eta_map");
            heliostatfield.ms_params.m_eta_map_aod_format = as_boolean("eta_map_aod_format");
			heliostatfield.ms_params.m_flux_maps = as_matrix("flux_maps");
			heliostatfield.ms_params.m_N_hel = as_integer("N_hel");
			heliostatfield.ms_params.m_A_sf = as_double("A_sf");		//[m2]
		}



        //Load the solar field adjustment factors
        sf_adjustment_factors sf_haf(this);
		int n_steps_full = (int)weather_reader.m_weather_data_provider->nrecords(); //steps_per_hour * 8760;
		if (!sf_haf.setup(n_steps_full))
			throw exec_error("tcsmolten_salt", "failed to setup sf adjustment factors: " + sf_haf.error());
        //allocate array to pass to tcs
        heliostatfield.ms_params.m_sf_adjust.resize( sf_haf.size() );
        for( int i=0; i<sf_haf.size(); i++)     
            heliostatfield.ms_params.m_sf_adjust.at(i) = sf_haf(i);

		// Set callback information
		heliostatfield.mf_callback = ssc_cmod_solarpilot_callback;
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
		double H_rec = as_double("rec_height");
		double rec_aspect = as_double("rec_aspect");

		double D_rec = H_rec / rec_aspect;

		double A_rec = std::numeric_limits<double>::quiet_NaN();

		switch (rec_type)
		{
		case var_receiver::REC_TYPE::EXTERNAL_CYLINDRICAL:
		{
			A_rec = H_rec * D_rec * 3.1415926;
			break;
		}
		case var_receiver::REC_TYPE::FLAT_PLATE:
			A_rec = H_rec * D_rec;
			break;
		}

		C_mspt_receiver_222 receiver;
		receiver.m_n_panels = as_integer("N_panels");
		receiver.m_d_rec = D_rec;
		receiver.m_h_rec = H_rec;
		receiver.m_h_tower = as_double("h_tower");
		receiver.m_od_tube = as_double("d_tube_out");
		receiver.m_th_tube = as_double("th_tube");
		receiver.m_mat_tube = as_integer("mat_tube");
		receiver.m_field_fl = as_integer("rec_htf");
		receiver.m_field_fl_props = as_matrix("field_fl_props");
		receiver.m_flow_type = as_integer("Flow_type");
        receiver.m_crossover_shift = as_integer("crossover_shift");
		receiver.m_epsilon = as_double("epsilon");
		receiver.m_hl_ffact = as_double("hl_ffact");
		receiver.m_T_htf_hot_des = as_double("T_htf_hot_des");				//[C]
		receiver.m_T_htf_cold_des = as_double("T_htf_cold_des");			//[C]
		receiver.m_f_rec_min = as_double("f_rec_min");
		receiver.m_q_rec_des = as_double("P_ref")/as_double("design_eff")*as_double("solarm");
		receiver.m_rec_su_delay = as_double("rec_su_delay");
		receiver.m_rec_qf_delay = as_double("rec_qf_delay");
		receiver.m_m_dot_htf_max_frac = as_double("csp.pt.rec.max_oper_frac");
		receiver.m_A_sf = as_double("A_sf");

		// 8.10.2015 twn: add tower piping thermal losses to receiver performance
		receiver.m_pipe_loss_per_m = as_double("piping_loss");						//[Wt/m]
		receiver.m_pipe_length_add = as_double("piping_length_const");	//[m]
		receiver.m_pipe_length_mult = as_double("piping_length_mult");		//[-]

		receiver.m_n_flux_x = as_integer("n_flux_x");
		receiver.m_n_flux_y = as_integer("n_flux_y");

		receiver.m_T_salt_hot_target = as_double("T_htf_hot_des");
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

		// *******************************************************
		// *******************************************************
		// Set receiver outputs
		//float *p_q_thermal_copy = allocate("Q_thermal_123", n_steps_fixed);
		collector_receiver.mc_reported_outputs.assign(C_csp_mspt_collector_receiver::E_FIELD_Q_DOT_INC, allocate("q_sf_inc", n_steps_fixed), n_steps_fixed);
		collector_receiver.mc_reported_outputs.assign(C_csp_mspt_collector_receiver::E_FIELD_ETA_OPT, allocate("eta_field", n_steps_fixed), n_steps_fixed);
		collector_receiver.mc_reported_outputs.assign(C_csp_mspt_collector_receiver::E_FIELD_ADJUST, allocate("sf_adjust_out", n_steps_fixed), n_steps_fixed);

		collector_receiver.mc_reported_outputs.assign(C_csp_mspt_collector_receiver::E_Q_DOT_INC, allocate("q_dot_rec_inc", n_steps_fixed), n_steps_fixed);
		collector_receiver.mc_reported_outputs.assign(C_csp_mspt_collector_receiver::E_ETA_THERMAL, allocate("eta_therm", n_steps_fixed), n_steps_fixed);
		collector_receiver.mc_reported_outputs.assign(C_csp_mspt_collector_receiver::E_Q_DOT_THERMAL, allocate("Q_thermal", n_steps_fixed), n_steps_fixed);
		collector_receiver.mc_reported_outputs.assign(C_csp_mspt_collector_receiver::E_M_DOT_HTF, allocate("m_dot_rec", n_steps_fixed), n_steps_fixed);
		collector_receiver.mc_reported_outputs.assign(C_csp_mspt_collector_receiver::E_Q_DOT_STARTUP, allocate("q_startup", n_steps_fixed), n_steps_fixed);
		collector_receiver.mc_reported_outputs.assign(C_csp_mspt_collector_receiver::E_T_HTF_IN, allocate("T_rec_in", n_steps_fixed), n_steps_fixed);
		collector_receiver.mc_reported_outputs.assign(C_csp_mspt_collector_receiver::E_T_HTF_OUT, allocate("T_rec_out", n_steps_fixed), n_steps_fixed);
		collector_receiver.mc_reported_outputs.assign(C_csp_mspt_collector_receiver::E_Q_DOT_PIPE_LOSS, allocate("q_piping_losses", n_steps_fixed), n_steps_fixed);
		collector_receiver.mc_reported_outputs.assign(C_csp_mspt_collector_receiver::E_Q_DOT_LOSS, allocate("q_thermal_loss", n_steps_fixed), n_steps_fixed);


		// Thermal energy storage 
		C_csp_two_tank_tes storage;
		C_csp_two_tank_tes::S_params *tes = &storage.ms_params;
		tes->m_field_fl = as_integer("rec_htf");
		tes->m_field_fl_props = as_matrix("field_fl_props");
		tes->m_tes_fl = as_integer("rec_htf");
		tes->m_tes_fl_props = as_matrix("field_fl_props");
		tes->m_is_hx = false;									// MSPT assumes direct storage, so no user input required here: hardcode = false
		tes->m_W_dot_pc_design = as_double("P_ref");		//[MWe]
		tes->m_eta_pc = as_double("design_eff");				//[-]
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
		tes->m_T_field_in_des = as_double("T_htf_cold_des");
		tes->m_T_field_out_des = as_double("T_htf_hot_des");
		tes->m_T_tank_hot_ini = as_double("T_htf_hot_des");
		tes->m_T_tank_cold_ini = as_double("T_htf_cold_des");
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
        tou.mc_dispatch_params.m_is_write_ampl_dat = as_boolean("is_write_ampl_dat");
        tou.mc_dispatch_params.m_is_ampl_engine = as_boolean("is_ampl_engine");
        tou.mc_dispatch_params.m_ampl_data_dir = as_string("ampl_data_dir");
        tou.mc_dispatch_params.m_ampl_exec_call = as_string("ampl_exec_call");
		if( tou.mc_dispatch_params.m_dispatch_optimize )
		{
			tou.mc_dispatch_params.m_optimize_frequency = as_integer("disp_frequency");
            tou.mc_dispatch_params.m_disp_steps_per_hour = as_integer("disp_steps_per_hour");
			tou.mc_dispatch_params.m_optimize_horizon = as_integer("disp_horizon");
			tou.mc_dispatch_params.m_max_iterations = as_integer("disp_max_iter");
			tou.mc_dispatch_params.m_solver_timeout = as_double("disp_timeout");
			tou.mc_dispatch_params.m_mip_gap = as_double("disp_mip_gap");
			tou.mc_dispatch_params.m_presolve_type = as_integer("disp_spec_presolve");
			tou.mc_dispatch_params.m_bb_type = as_integer("disp_spec_bb");
			tou.mc_dispatch_params.m_disp_reporting = as_integer("disp_reporting");
			tou.mc_dispatch_params.m_scaling_type = as_integer("disp_spec_scaling");
			tou.mc_dispatch_params.m_disp_time_weighting = as_double("disp_time_weighting");
            tou.mc_dispatch_params.m_rsu_cost = as_double("disp_rsu_cost");
            tou.mc_dispatch_params.m_csu_cost = as_double("disp_csu_cost");
            tou.mc_dispatch_params.m_pen_delta_w = as_double("disp_pen_delta_w");
            tou.mc_dispatch_params.m_q_rec_standby = as_double("q_rec_standby");
			tou.mc_dispatch_params.m_w_rec_ht = as_double("q_rec_heattrace");

			if (as_boolean("is_wlim_series"))
			{
				size_t n_wlim_series = 0;
				ssc_number_t* wlim_series = as_array("wlim_series", &n_wlim_series);
				if (n_wlim_series != n_steps_full)
					throw exec_error("tcsmolten_salt", "Invalid net electricity generation limit series dimension. Matrix must have "+util::to_string(n_steps_full)+" rows.");
				for (int i = 0; i < n_steps_full; i++)
					tou.mc_dispatch_params.m_w_lim_full.at(i) = (double)wlim_series[i];
			}

	
		}
		tou.mc_dispatch_params.m_is_block_dispatch = ! tou.mc_dispatch_params.m_dispatch_optimize;      //mw
		tou.mc_dispatch_params.m_use_rule_1 = true;
		tou.mc_dispatch_params.m_standby_off_buffer = 2.0;
		tou.mc_dispatch_params.m_use_rule_2 = false;
		tou.mc_dispatch_params.m_q_dot_rec_des_mult = -1.23;
		tou.mc_dispatch_params.m_f_q_dot_pc_overwrite = -1.23;

        size_t n_f_turbine = 0;
		ssc_number_t *p_f_turbine = as_array("f_turb_tou_periods", &n_f_turbine);
		tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC].resize(n_f_turbine,0.0);
		//tou_params->mv_t_frac.resize(n_f_turbine, 0.0);
		for( size_t i = 0; i < n_f_turbine; i++ )
			tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC][i] = (double)p_f_turbine[i];

		bool is_timestep_input = (as_integer("ppa_multiplier_model") == 1);
		tou_params->mc_pricing.mv_is_diurnal = !(is_timestep_input);
		if (is_timestep_input)
		{
			size_t nmultipliers;
			ssc_number_t *multipliers = as_array("dispatch_factors_ts", &nmultipliers);
			tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(nmultipliers, 0.0);
			for (size_t ii = 0; ii < nmultipliers; ii++)
				tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][ii] = multipliers[ii];
		}
		else // standard diuranal input
		{
			tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(9, 0.0);
			tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][0] = as_double("dispatch_factor1");
			tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][1] = as_double("dispatch_factor2");
			tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][2] = as_double("dispatch_factor3");
			tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][3] = as_double("dispatch_factor4");
			tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][4] = as_double("dispatch_factor5");
			tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][5] = as_double("dispatch_factor6");
			tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][6] = as_double("dispatch_factor7");
			tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][7] = as_double("dispatch_factor8");
			tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][8] = as_double("dispatch_factor9");
		}

		// System parameters
		C_csp_solver::S_csp_system_params system;
		system.m_pb_fixed_par = as_double("pb_fixed_par");
		system.m_bop_par = as_double("bop_par");
		system.m_bop_par_f = as_double("bop_par_f");
		system.m_bop_par_0 = as_double("bop_par_0");
		system.m_bop_par_1 = as_double("bop_par_1");
		system.m_bop_par_2 = as_double("bop_par_2");

  		// Instantiate Solver		
		C_csp_solver csp_solver(weather_reader, 
						collector_receiver, 
						*p_csp_power_cycle, 
						storage, 
						tou, 
						system,
						ssc_cmod_update,
						(void*)(this));


		// Set solver reporting outputs
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TIME_FINAL, allocate("time_hr", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::ERR_M_DOT, allocate("m_dot_balance", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::ERR_Q_DOT, allocate("q_balance", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::N_OP_MODES, allocate("n_op_modes", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::OP_MODE_1, allocate("op_mode_1", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::OP_MODE_2, allocate("op_mode_2", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::OP_MODE_3, allocate("op_mode_3", n_steps_fixed), n_steps_fixed);


		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TOU_PERIOD, allocate("tou_value", n_steps_fixed), n_steps_fixed);            
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PRICING_MULT, allocate("pricing_mult", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PC_Q_DOT_SB, allocate("q_dot_pc_sb", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PC_Q_DOT_MIN, allocate("q_dot_pc_min", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PC_Q_DOT_TARGET, allocate("q_dot_pc_target", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PC_Q_DOT_MAX, allocate("q_dot_pc_max", n_steps_fixed), n_steps_fixed);
		
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_IS_REC_SU, allocate("is_rec_su_allowed", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_IS_PC_SU, allocate("is_pc_su_allowed", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_IS_PC_SB, allocate("is_pc_sb_allowed", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::EST_Q_DOT_CR_SU, allocate("q_dot_est_cr_su", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::EST_Q_DOT_CR_ON, allocate("q_dot_est_cr_on", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::EST_Q_DOT_DC, allocate("q_dot_est_tes_dc", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::EST_Q_DOT_CH, allocate("q_dot_est_tes_ch", n_steps_fixed), n_steps_fixed);
		
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_OP_MODE_SEQ_A, allocate("operating_modes_a", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_OP_MODE_SEQ_B, allocate("operating_modes_b", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_OP_MODE_SEQ_C, allocate("operating_modes_c", n_steps_fixed), n_steps_fixed);
		
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_STATE, allocate("disp_solve_state", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_ITER, allocate("disp_solve_iter", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_OBJ, allocate("disp_objective", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_OBJ_RELAX, allocate("disp_obj_relax", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_QSF_EXPECT, allocate("disp_qsf_expected", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_QSFPROD_EXPECT, allocate("disp_qsfprod_expected", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_QSFSU_EXPECT, allocate("disp_qsfsu_expected", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_TES_EXPECT, allocate("disp_tes_expected", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_PCEFF_EXPECT, allocate("disp_pceff_expected", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SFEFF_EXPECT, allocate("disp_thermeff_expected", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_QPBSU_EXPECT, allocate("disp_qpbsu_expected", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_WPB_EXPECT, allocate("disp_wpb_expected", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_REV_EXPECT, allocate("disp_rev_expected", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_PRES_NCONSTR, allocate("disp_presolve_nconstr", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_PRES_NVAR, allocate("disp_presolve_nvar", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_TIME, allocate("disp_solve_time", n_steps_fixed), n_steps_fixed);

		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::SOLZEN, allocate("solzen", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::SOLAZ, allocate("solaz", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::BEAM, allocate("beam", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TDRY, allocate("tdry", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TWET, allocate("twet", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::RH, allocate("RH", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::WSPD, allocate("wspd", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CR_DEFOCUS, allocate("defocus", n_steps_fixed), n_steps_fixed);

		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_Q_DOT_LOSS, allocate("tank_losses", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_W_DOT_HEATER, allocate("q_heater", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_T_HOT, allocate("T_tes_hot", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_T_COLD, allocate("T_tes_cold", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_Q_DOT_DC, allocate("q_dc_tes", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_Q_DOT_CH, allocate("q_ch_tes", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_E_CH_STATE, allocate("e_ch_tes", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_M_DOT_DC, allocate("m_dot_tes_dc", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_M_DOT_CH, allocate("m_dot_tes_ch", n_steps_fixed), n_steps_fixed);

		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::COL_W_DOT_TRACK, allocate("pparasi", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CR_W_DOT_PUMP, allocate("P_tower_pump", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::SYS_W_DOT_PUMP, allocate("htf_pump_power", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PC_W_DOT_COOLING, allocate("P_cooling_tower_tot", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::SYS_W_DOT_FIXED, allocate("P_fixed", n_steps_fixed), n_steps_fixed);
		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::SYS_W_DOT_BOP, allocate("P_plant_balance_tot", n_steps_fixed), n_steps_fixed);

		csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::W_DOT_NET, allocate("P_out_net", n_steps_fixed), n_steps_fixed);



		update("Initialize MSPT model...", 0.0);

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

			throw exec_error("tcsmolten_salt", csp_exception.m_error_message);
		}

		// If no exception, then report messages
		while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
		{
			log(out_msg, out_type);
		}


        //if the pricing schedule is provided as hourly, overwrite the tou schedule
        if( as_boolean("is_dispatch_series") )
        {
            size_t n_dispatch_series;
            ssc_number_t *dispatch_series = as_array("dispatch_series", &n_dispatch_series);

       //     if( n_dispatch_series != n_steps_fixed)
			    //throw exec_error("tcsmolten_salt", "Invalid dispatch pricing series dimension. Array length must match number of simulation time steps ("+my_to_string(n_steps_fixed)+").");
                
            //resize the m_hr_tou array
            if( tou_params->mc_pricing.m_hr_tou != 0 )
                delete [] tou_params->mc_pricing.m_hr_tou;
            tou_params->mc_pricing.m_hr_tou = new double[n_steps_fixed];
            //set the tou period as unique for each time step
            for(int i=0; i<n_steps_fixed; i++)
                tou_params->mc_pricing.m_hr_tou[i] = i+1;
            //allocate reported arrays
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(n_steps_fixed);
            for( int i=0; i<n_steps_fixed; i++)
                tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][i] = dispatch_series[i];
        }

		update("Begin timeseries simulation...", 0.0);

		try
		{
			// Simulate !
			csp_solver.Ssimulate(sim_setup);
		}
		catch(C_csp_exception &csp_exception)
		{
			// Report warning before exiting with error
			while( csp_solver.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				log(out_msg);
			}

			throw exec_error("tcsmolten_salt", csp_exception.m_error_message);
		}

		// If no exception, then report messages
		while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
		{
			log(out_msg, out_type);
		}

		// ******* Re-calculate system costs here ************
		C_mspt_system_costs sys_costs;

		sys_costs.ms_par.A_sf_refl = as_double("A_sf");
		sys_costs.ms_par.site_improv_spec_cost = as_double("site_spec_cost");
		sys_costs.ms_par.heliostat_spec_cost = as_double("heliostat_spec_cost");
		sys_costs.ms_par.heliostat_fixed_cost = as_double("cost_sf_fixed");

		sys_costs.ms_par.h_tower = as_double("h_tower");
		sys_costs.ms_par.h_rec = H_rec;
		sys_costs.ms_par.h_helio = as_double("helio_height");
		sys_costs.ms_par.tower_fixed_cost = as_double("tower_fixed_cost");
		sys_costs.ms_par.tower_cost_scaling_exp = as_double("tower_exp");

		sys_costs.ms_par.A_rec = A_rec;
		sys_costs.ms_par.rec_ref_cost = as_double("rec_ref_cost");
		sys_costs.ms_par.A_rec_ref = as_double("rec_ref_area");
		sys_costs.ms_par.rec_cost_scaling_exp = as_double("rec_cost_exp");

		sys_costs.ms_par.Q_storage = as_double("P_ref") / as_double("design_eff")*as_double("tshours");
		sys_costs.ms_par.tes_spec_cost = as_double("tes_spec_cost");

		sys_costs.ms_par.W_dot_design = as_double("P_ref");
		sys_costs.ms_par.power_cycle_spec_cost = as_double("plant_spec_cost");

		sys_costs.ms_par.radfield_area= rankine_pc.mc_radiator.ms_params.Afield;
		sys_costs.ms_par.radfield_vol = rankine_pc.mc_radiator.ms_params.D*rankine_pc.mc_radiator.ms_params.D / 4 * PI*rankine_pc.mc_radiator.ms_params.n*rankine_pc.mc_radiator.ms_params.Np*rankine_pc.mc_radiator.ms_params.L; //Calculate volume in radiator panel tubes = pi/4*d^2*L*n*Np
		if (rankine_pc.mc_two_tank_ctes.ms_params.m_ctes_type == 2) //If two tank
		{
			sys_costs.ms_par.coldstorage_vol = rankine_pc.mc_two_tank_ctes.get_physical_volume();
		}
		if (rankine_pc.mc_two_tank_ctes.ms_params.m_ctes_type > 2) //If stratified 
		{
			sys_costs.ms_par.coldstorage_vol = rankine_pc.mc_stratified_ctes.get_physical_volume();	

		}
		sys_costs.ms_par.rad_unitcost = as_double("radiator_unitcost");
		sys_costs.ms_par.rad_installcost = as_double("radiator_installcost");
		sys_costs.ms_par.rad_fluidcost = as_double("radiator_fluidcost");
		sys_costs.ms_par.rad_volmulti = as_double("radfluid_vol_ratio");
		sys_costs.ms_par.coldstorage_unitcost = as_double("ctes_cost");

		sys_costs.ms_par.bop_spec_cost = as_double("bop_spec_cost");

		sys_costs.ms_par.fossil_backup_spec_cost = as_double("fossil_spec_cost");

		sys_costs.ms_par.contingency_rate = as_double("contingency_rate");

		//land area
		sys_costs.ms_par.total_land_area = as_double("land_area_base") * as_double("csp.pt.sf.land_overhead_factor") + as_double("csp.pt.sf.fixed_land_area")+ sys_costs.ms_par.radfield_area/4046.86 /*acres/m^2*/ ;
		assign("csp.pt.cost.total_land_area", (ssc_number_t)sys_costs.ms_par.total_land_area);

		sys_costs.ms_par.plant_net_capacity = system_capacity / 1000.0;			//[MWe], convert from kWe
		sys_costs.ms_par.EPC_land_spec_cost = as_double("csp.pt.cost.epc.per_acre");
		sys_costs.ms_par.EPC_land_perc_direct_cost = as_double("csp.pt.cost.epc.percent");
		sys_costs.ms_par.EPC_land_per_power_cost = as_double("csp.pt.cost.epc.per_watt");
		sys_costs.ms_par.EPC_land_fixed_cost = as_double("csp.pt.cost.epc.fixed");
		sys_costs.ms_par.total_land_spec_cost = as_double("land_spec_cost");
		sys_costs.ms_par.total_land_perc_direct_cost = as_double("csp.pt.cost.plm.percent");
		sys_costs.ms_par.total_land_per_power_cost = as_double("csp.pt.cost.plm.per_watt");
		sys_costs.ms_par.total_land_fixed_cost = as_double("csp.pt.cost.plm.fixed");
		sys_costs.ms_par.sales_tax_basis = as_double("sales_tax_frac");
		sys_costs.ms_par.sales_tax_rate = as_double("sales_tax_rate");

		try
		{
			sys_costs.calculate_costs();
		}
		catch (C_csp_exception &)
		{
			throw exec_error("MSPT system costs", util::format("System cost calculations failed. Check that all inputs are properly defined"));
		}

		// 1.5.2016 twn: financial model needs an updated total_installed_cost, remaining are for reporting only
		assign("total_installed_cost", (ssc_number_t)sys_costs.ms_out.total_installed_cost);

		assign("csp.pt.cost.site_improvements", (ssc_number_t)sys_costs.ms_out.site_improvement_cost);
		assign("csp.pt.cost.heliostats", (ssc_number_t)sys_costs.ms_out.heliostat_cost);
		assign("csp.pt.cost.tower", (ssc_number_t)sys_costs.ms_out.tower_cost);
		assign("csp.pt.cost.receiver", (ssc_number_t)sys_costs.ms_out.receiver_cost);
		assign("csp.pt.cost.storage", (ssc_number_t)sys_costs.ms_out.tes_cost);
		assign("csp.pt.cost.power_block", (ssc_number_t)sys_costs.ms_out.power_cycle_cost);
		assign("csp.pt.cost.rad_field", (ssc_number_t)sys_costs.ms_out.rad_field_totcost);
		assign("csp.pt.cost.rad_fluid", (ssc_number_t)sys_costs.ms_out.rad_fluid_totcost);
		assign("csp.pt.cost.rad_storage", (ssc_number_t)sys_costs.ms_out.rad_storage_totcost);
		assign("csp.pt.cost.bop", (ssc_number_t)sys_costs.ms_out.bop_cost);
		assign("csp.pt.cost.fossil", (ssc_number_t)sys_costs.ms_out.fossil_backup_cost);
		assign("ui_direct_subtotal", (ssc_number_t)sys_costs.ms_out.direct_capital_precontingency_cost);
		assign("csp.pt.cost.contingency", (ssc_number_t)sys_costs.ms_out.contingency_cost);
		assign("total_direct_cost", (ssc_number_t)sys_costs.ms_out.total_direct_cost);
		assign("csp.pt.cost.epc.total", (ssc_number_t)sys_costs.ms_out.epc_and_owner_cost);
		assign("csp.pt.cost.plm.total", (ssc_number_t)sys_costs.ms_out.total_land_cost);
		assign("csp.pt.cost.sales_tax.total", (ssc_number_t)sys_costs.ms_out.sales_tax_cost);
		assign("total_indirect_cost", (ssc_number_t)sys_costs.ms_out.total_indirect_cost);
		assign("csp.pt.cost.installed_per_capacity", (ssc_number_t)sys_costs.ms_out.estimated_installed_cost_per_cap);

		// Update construction financing costs, specifically, update: "construction_financing_cost"
		double const_per_interest_rate1 = as_double("const_per_interest_rate1");
		double const_per_interest_rate2 = as_double("const_per_interest_rate2");
		double const_per_interest_rate3 = as_double("const_per_interest_rate3");
		double const_per_interest_rate4 = as_double("const_per_interest_rate4");
		double const_per_interest_rate5 = as_double("const_per_interest_rate5");
		double const_per_months1 = as_double("const_per_months1");
		double const_per_months2 = as_double("const_per_months2");
		double const_per_months3 = as_double("const_per_months3");
		double const_per_months4 = as_double("const_per_months4");
		double const_per_months5 = as_double("const_per_months5");
		double const_per_percent1 = as_double("const_per_percent1");
		double const_per_percent2 = as_double("const_per_percent2");
		double const_per_percent3 = as_double("const_per_percent3");
		double const_per_percent4 = as_double("const_per_percent4");
		double const_per_percent5 = as_double("const_per_percent5");
		double const_per_upfront_rate1 = as_double("const_per_upfront_rate1");
		double const_per_upfront_rate2 = as_double("const_per_upfront_rate2");
		double const_per_upfront_rate3 = as_double("const_per_upfront_rate3");
		double const_per_upfront_rate4 = as_double("const_per_upfront_rate4");
		double const_per_upfront_rate5 = as_double("const_per_upfront_rate5");

		double const_per_principal1, const_per_principal2, const_per_principal3, const_per_principal4, const_per_principal5;
		double const_per_interest1, const_per_interest2, const_per_interest3, const_per_interest4, const_per_interest5;
		double const_per_total1, const_per_total2, const_per_total3, const_per_total4, const_per_total5;
		double const_per_percent_total, const_per_principal_total, const_per_interest_total, construction_financing_cost;

		const_per_principal1 = const_per_principal2 = const_per_principal3 = const_per_principal4 = const_per_principal5 =
			const_per_interest1 = const_per_interest2 = const_per_interest3 = const_per_interest4 = const_per_interest5 =
			const_per_total1 = const_per_total2 = const_per_total3 = const_per_total4 = const_per_total5 =
			const_per_percent_total = const_per_principal_total = const_per_interest_total = construction_financing_cost =
			std::numeric_limits<double>::quiet_NaN();

		N_financial_parameters::construction_financing_total_cost(sys_costs.ms_out.total_installed_cost,
			const_per_interest_rate1, const_per_interest_rate2, const_per_interest_rate3, const_per_interest_rate4, const_per_interest_rate5,
			const_per_months1, const_per_months2, const_per_months3, const_per_months4, const_per_months5,
			const_per_percent1, const_per_percent2, const_per_percent3, const_per_percent4, const_per_percent5,
			const_per_upfront_rate1, const_per_upfront_rate2, const_per_upfront_rate3, const_per_upfront_rate4, const_per_upfront_rate5,
			const_per_principal1, const_per_principal2, const_per_principal3, const_per_principal4, const_per_principal5,
			const_per_interest1, const_per_interest2, const_per_interest3, const_per_interest4, const_per_interest5,
			const_per_total1, const_per_total2, const_per_total3, const_per_total4, const_per_total5,
			const_per_percent_total, const_per_principal_total, const_per_interest_total, construction_financing_cost);

		assign("const_per_principal1", (ssc_number_t)const_per_principal1);
		assign("const_per_principal2", (ssc_number_t)const_per_principal2);
		assign("const_per_principal3", (ssc_number_t)const_per_principal3);
		assign("const_per_principal4", (ssc_number_t)const_per_principal4);
		assign("const_per_principal5", (ssc_number_t)const_per_principal5);
		assign("const_per_interest1", (ssc_number_t)const_per_interest1);
		assign("const_per_interest2", (ssc_number_t)const_per_interest2);
		assign("const_per_interest3", (ssc_number_t)const_per_interest3);
		assign("const_per_interest4", (ssc_number_t)const_per_interest4);
		assign("const_per_interest5", (ssc_number_t)const_per_interest5);
		assign("const_per_total1", (ssc_number_t)const_per_total1);
		assign("const_per_total2", (ssc_number_t)const_per_total2);
		assign("const_per_total3", (ssc_number_t)const_per_total3);
		assign("const_per_total4", (ssc_number_t)const_per_total4);
		assign("const_per_total5", (ssc_number_t)const_per_total5);
		assign("const_per_percent_total", (ssc_number_t)const_per_percent_total);
		assign("const_per_principal_total", (ssc_number_t)const_per_principal_total);
		assign("const_per_interest_total", (ssc_number_t)const_per_interest_total);
		assign("construction_financing_cost", (ssc_number_t)construction_financing_cost);

		// Do unit post-processing here
		float *p_q_pc_startup = allocate("q_pc_startup", n_steps_fixed);
		size_t count_pc_su = 0;
		ssc_number_t *p_q_dot_pc_startup = as_array("q_dot_pc_startup", &count_pc_su);
		if( count_pc_su != n_steps_fixed )
		{
			log("q_dot_pc_startup array is a different length than 'n_steps_fixed'.", SSC_WARNING);
			return;
		}
		for( int i = 0; i < n_steps_fixed; i++ )
		{
			p_q_pc_startup[i] = (float)(p_q_dot_pc_startup[i] * (sim_setup.m_report_step / 3600.0));	//[MWh]
		}

		// Convert mass flow rates from [kg/hr] to [kg/s]
		size_t count_m_dot_pc, count_m_dot_rec, count_m_dot_water_pc, count_m_dot_tes_dc, count_m_dot_tes_ch;
		count_m_dot_pc = count_m_dot_rec = count_m_dot_water_pc = count_m_dot_tes_dc = count_m_dot_tes_ch = 0;
		ssc_number_t *p_m_dot_rec = as_array("m_dot_rec", &count_m_dot_rec);
		ssc_number_t *p_m_dot_pc = as_array("m_dot_pc", &count_m_dot_pc);
		ssc_number_t *p_m_dot_water_pc = as_array("m_dot_water_pc", &count_m_dot_water_pc);
		ssc_number_t *p_m_dot_tes_dc = as_array("m_dot_tes_dc", &count_m_dot_tes_dc);
		ssc_number_t *p_m_dot_tes_ch = as_array("m_dot_tes_ch", &count_m_dot_tes_ch);
		if (count_m_dot_rec != n_steps_fixed || count_m_dot_pc != n_steps_fixed || count_m_dot_water_pc != n_steps_fixed
			|| count_m_dot_tes_dc != n_steps_fixed || count_m_dot_tes_ch != n_steps_fixed)
		{
			log("At least one m_dot array is a different length than 'n_steps_fixed'.", SSC_WARNING);
			return;
		}
		for (int i = 0; i < n_steps_fixed; i++)
		{
			p_m_dot_rec[i] = (ssc_number_t)(p_m_dot_rec[i] / 3600.0);	//[kg/s] convert from kg/hr
			p_m_dot_pc[i] = (ssc_number_t)(p_m_dot_pc[i] / 3600.0);		//[kg/s] convert from kg/hr
			p_m_dot_water_pc[i] = (ssc_number_t)(p_m_dot_water_pc[i] / 3600.0);	//[kg/s] convert from kg/hr
			p_m_dot_tes_dc[i] = (ssc_number_t)(p_m_dot_tes_dc[i] / 3600.0);		//[kg/s] convert from kg/hr
			p_m_dot_tes_ch[i] = (ssc_number_t)(p_m_dot_tes_ch[i] / 3600.0);		//[kg/s] convert from kg/hr
		}		

		// Set output data from heliostat class
		int n_rows_eta_map = heliostatfield.ms_params.m_eta_map.nrows();
		ssc_number_t *eta_map_out = allocate("eta_map_out", n_rows_eta_map, 3);
		int n_rows_flux_maps = heliostatfield.ms_params.m_flux_maps.nrows();
		int n_cols_flux_maps = heliostatfield.ms_params.m_flux_maps.ncols() + 2;
		ssc_number_t *flux_maps_out = allocate("flux_maps_out", n_rows_eta_map, n_cols_flux_maps);

		if(n_rows_eta_map != n_rows_flux_maps)
		{
			log("The number of rows in the field efficiency and receiver flux map matrices are not equal. This is unexpected, and the flux maps may be inaccurate.");
		}

		double flux_scaling_mult = as_double("dni_des")*heliostatfield.ms_params.m_A_sf / 1000.0 /
			(CSP::pi*H_rec*
			H_rec / rec_aspect /
			double(heliostatfield.ms_params.m_n_flux_x));

		for( int i = 0; i < n_rows_eta_map; i++ )
		{
			flux_maps_out[n_cols_flux_maps*i] = eta_map_out[3 * i] = (ssc_number_t)heliostatfield.ms_params.m_eta_map(i, 0);		//[deg] Solar azimuth angle
			flux_maps_out[n_cols_flux_maps*i + 1] = eta_map_out[3 * i + 1] = (ssc_number_t)heliostatfield.ms_params.m_eta_map(i, 1);	//[deg] Solar zenith angle
			eta_map_out[3 * i + 2] = (ssc_number_t)heliostatfield.ms_params.m_eta_map(i, 2);							//[deg] Solar field optical efficiency
			for( int j = 2; j < n_cols_flux_maps; j++ )
			{
				flux_maps_out[n_cols_flux_maps*i + j] = (ssc_number_t)(heliostatfield.ms_params.m_flux_maps(i, j - 2)*heliostatfield.ms_params.m_eta_map(i, 2)*flux_scaling_mult);		//[kW/m^2]
			}
		}

		size_t count;
		ssc_number_t *p_W_dot_net = as_array("P_out_net", &count);
		ssc_number_t *p_time_final_hr = as_array("time_hr", &count);

		// 'adjustment_factors' class stores factors in hourly array, so need to index as such
		adjustment_factors haf(this, "adjust");
		if( !haf.setup() )
			throw exec_error("tcsmolten_salt", "failed to setup adjustment factors: " + haf.error());


		ssc_number_t *p_gen = allocate("gen", count);
		for( size_t i = 0; i < count; i++ )
		{
			size_t hour = (size_t)ceil(p_time_final_hr[i]);
			p_gen[i] = (ssc_number_t)(p_W_dot_net[i] * 1.E3 * haf(hour));			//[kWe]
		}

		accumulate_annual_for_year("gen", "annual_energy", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed/steps_per_hour);
		
		accumulate_annual_for_year("P_cycle", "annual_W_cycle_gross", 1000.0*sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed/steps_per_hour);		//[kWe-hr]
		accumulate_annual_for_year("P_cooling_tower_tot", "annual_W_cooling_tower", 1000.0*sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour);		//[kWe-hr]


        accumulate_annual_for_year("disp_objective", "disp_objective_ann", 1000.0*sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed/steps_per_hour);
        accumulate_annual_for_year("disp_solve_iter", "disp_iter_ann", 1000.0*sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed/steps_per_hour);
        accumulate_annual_for_year("disp_presolve_nconstr", "disp_presolve_nconstr_ann", sim_setup.m_report_step / 3600.0/ as_double("disp_frequency"), steps_per_hour, 1, n_steps_fixed/steps_per_hour);
        accumulate_annual_for_year("disp_presolve_nvar", "disp_presolve_nvar_ann", sim_setup.m_report_step / 3600.0/ as_double("disp_frequency"), steps_per_hour, 1, n_steps_fixed/steps_per_hour);
        accumulate_annual_for_year("disp_solve_time", "disp_solve_time_ann", sim_setup.m_report_step/3600. / as_double("disp_frequency"), steps_per_hour, 1, n_steps_fixed/steps_per_hour );

		// Calculated Outputs
			// First, sum power cycle water consumption timeseries outputs
		accumulate_annual_for_year("m_dot_water_pc", "annual_total_water_use", sim_setup.m_report_step / 1000.0, steps_per_hour, 1, n_steps_fixed/steps_per_hour); //[m^3], convert from kg
			// Then, add water usage from mirror cleaning
		ssc_number_t V_water_cycle = as_number("annual_total_water_use");
		double V_water_mirrors = as_double("water_usage_per_wash") / 1000.0*as_double("A_sf")*as_double("washing_frequency");
		assign("annual_total_water_use", (ssc_number_t)(V_water_cycle + V_water_mirrors));

		ssc_number_t ae = as_number("annual_energy");
		ssc_number_t pg = as_number("annual_W_cycle_gross");
		ssc_number_t convfactor = (pg != 0) ? 100 * ae / pg : (ssc_number_t)0.0;
		assign("conversion_factor", convfactor);

		double kWh_per_kW = 0.0;
		double nameplate = system_capacity;		//[kWe]
		if(nameplate > 0.0)
			kWh_per_kW = ae / nameplate;

		assign("capacity_factor", (ssc_number_t)(kWh_per_kW / ((double)n_steps_fixed / (double)steps_per_hour)*100.));
		assign("kwh_per_kw", (ssc_number_t)kWh_per_kW);
		 
		//Single value outputs from radiative cooling system
		double A_radfield = rankine_pc.mc_radiator.ms_params.Afield;
		assign("A_radfield", (ssc_number_t)A_radfield);
	}
};

DEFINE_MODULE_ENTRY(tcsmolten_salt, "CSP molten salt power tower with hierarchical controller and dispatch optimization", 1)
