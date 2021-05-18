/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "core.h"

#include "common.h"
#include "csp_common.h"
#include "csp_solver_core.h"
#include "csp_solver_cr_electric_resistance.h"
#include "csp_solver_pc_Rankine_indirect_224.h"
#include "csp_solver_tou_block_schedules.h"

#include "csp_system_costs.h"

static var_info _cm_vtab_etes_electric_resistance[] = {

    // Resource Data
    { SSC_INPUT,  SSC_STRING, "solar_resource_file",           "Local weather file path",                                        "",             "",                                  "Solar Resource",                    "?",                                                                "LOCAL_FILE",    ""},

    // Simulation Parametes
    { SSC_INPUT,  SSC_NUMBER, "time_start",                    "Simulation start time",                                          "s",            "",                                  "System Control",                           "?=0",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "time_stop",                     "Simulation stop time",                                           "s",            "",                                  "System Control",                           "?=31536000",                                                       "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "time_steps_per_hour",           "Number of simulation time steps per hour",                       "",             "",                                  "System Control",                           "?=-1",                                                             "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "vacuum_arrays",                 "Allocate arrays for only the required number of steps",          "",             "",                                  "System Control",                           "?=0",                                                              "",              ""},


    // System 
    { SSC_INPUT,  SSC_NUMBER, "T_htf_cold_des",                "Cold HTF inlet temperature at design conditions",                "C",            "",                                  "System Design",                            "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "T_htf_hot_des",                 "Hot HTF outlet temperature at design conditions",                "C",            "",                                  "System Design",                            "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "P_ref",                         "Reference output electric power at design condition",            "MW",           "",                                  "System Design",                            "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "design_eff",                    "Power cycle efficiency at design",                               "none",         "",                                  "System Design",                            "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "tshours",                       "Equivalent full-load thermal storage hours",                     "hr",           "",                                  "System Design",                            "*",                                                                "",              ""},
/*new*/    { SSC_INPUT,  SSC_NUMBER, "heater_mult",                   "Heater multiple relative to design cycle thermal power",         "-",            "",                                  "System Design",                            "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "gross_net_conversion_factor",   "Estimated gross to net conversion factor",                       "",             "",                                  "System Design",                            "*",                                                                "",              ""},


    // Power Cycle
        // General
    { SSC_INPUT,  SSC_NUMBER, "pc_config",                     "PC configuration 0=Steam Rankine, 1=user defined",               "",             "",                                  "Power Cycle",                              "?=0",                                                              "INTEGER",       ""},
    { SSC_INPUT,  SSC_NUMBER, "pb_pump_coef",                  "Pumping power to move 1kg of HTF through PB loop",               "kW/kg",        "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "startup_time",                  "Time needed for power block startup",                            "hr",           "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "startup_frac",                  "Fraction of design thermal power needed for startup",            "none",         "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "cycle_max_frac",                "Maximum turbine over design operation fraction",                 "",             "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "cycle_cutoff_frac",             "Minimum turbine operation fraction before shutdown",             "",             "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "q_sby_frac",                    "Fraction of thermal power required for standby",                 "",             "",                                  "Power Cycle",                              "*",                                                                "",              ""},
        // Steam Rankine
    { SSC_INPUT,  SSC_NUMBER, "dT_cw_ref",                     "Reference condenser cooling water inlet/outlet temperature difference",  "C",    "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "T_amb_des",                     "Reference ambient temperature at design point",                          "C",    "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "P_boil",                        "Boiler operating pressure",                                              "bar",  "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "CT",                            "Condensor type: 1=evaporative, 2=air, 3=hybrid",                         "",     "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "T_approach",                    "Cooling tower approach temperature",                                     "C",    "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "T_ITD_des",                     "ITD at design for dry system",                                           "C",    "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "P_cond_ratio",                  "Condenser pressure ratio",                                               "",     "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "pb_bd_frac",                    "Power block blowdown steam fraction",                                    "",     "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "P_cond_min",                    "Minimum condenser pressure",                                             "inHg", "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "n_pl_inc",                      "Number of part-load increments for the heat rejection system",           "none", "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "INTEGER",       ""},
    { SSC_INPUT,  SSC_ARRAY,  "F_wc",                          "TOU array of fractions indicating wet cooling share for hybrid cooling", "",     "",                                  "System Control",                           "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "tech_type",                     "Turbine inlet pressure control 1=Fixed, 3=Sliding",                      "",     "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
        // User Defined cycle
    { SSC_INPUT,  SSC_NUMBER, "ud_f_W_dot_cool_des",           "Percent of user-defined power cycle design gross output consumed by cooling",                     "%",      "",       "User Defined Power Cycle",                 "pc_config=1",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "ud_m_dot_water_cool_des",       "Mass flow rate of water required at user-defined power cycle design point",                       "kg/s",   "",       "User Defined Power Cycle",                 "pc_config=1",                                                      "",              ""},
    { SSC_INPUT,  SSC_MATRIX, "ud_ind_od",                     "Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb", "",       "",       "User Defined Power Cycle",                 "pc_config=1",                                                      "",              ""},


    // TES
        // Performance
/*new*/    { SSC_INPUT,  SSC_NUMBER, "tes_fl_code",                   "Receiver HTF, 17=Salt (60% NaNO3, 40% KNO3) 10=Salt (46.5% LiF 11.5% NaF 42% KF) 50=Lookup tables", "", "",           "Thermal Storage",                                      "*",                                                                "",              ""},
/*new*/    { SSC_INPUT,  SSC_MATRIX, "ud_tes_fl_props",               "User-defined TES fluid property data",                           "-",            "",                                  "Thermal Storage",                                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "csp.pt.tes.init_hot_htf_percent", "Initial fraction of available volume that is hot",              "%",            "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "h_tank",                        "Total height of tank (height of HTF when tank is full)",        "m",            "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "cold_tank_max_heat",            "Rated heater capacity for cold tank heating",                   "MW",           "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "u_tank",                        "Loss coefficient from the tank",                                "W/m2-K",       "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "tank_pairs",                    "Number of equivalent tank pairs",                               "",             "",                                  "Thermal Storage",                          "*",                                                                "INTEGER",       ""},
    { SSC_INPUT,  SSC_NUMBER, "cold_tank_Thtr",                "Minimum allowable cold tank HTF temperature",                   "C",            "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "h_tank_min",                    "Minimum allowable HTF height in storage tank",                  "m",            "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "hot_tank_Thtr",                 "Minimum allowable hot tank HTF temperature",                    "C",            "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "hot_tank_max_heat",             "Rated heater capacity for hot tank heating",                    "MW",           "",                                  "Thermal Storage",                          "*",                                                                "",              ""},


    // Heater ??


    // System control
    { SSC_INPUT,  SSC_NUMBER, "is_dispatch",                   "Allow dispatch optimization?",                                  "",             "",                                  "System Control",                           "?=0",                                                              "",              ""},


    // System performance
    { SSC_INPUT,  SSC_NUMBER, "pb_fixed_par",                  "Fixed parasitic load that don't generate heat - runs at all times","MWe/MWcap", "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par",                       "Balance of plant parasitic power fraction",                        "MWe/MWcap", "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par_f",                     "Balance of plant parasitic power fraction - mult frac",            "",          "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par_0",                     "Balance of plant parasitic power fraction - const coeff",          "",          "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par_1",                     "Balance of plant parasitic power fraction - linear coeff",         "",          "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par_2",                     "Balance of plant parasitic power fraction - quadratic coeff",      "",          "",                                  "System Control",                           "*",                                                                "",              ""},


    // Pricing schedules and multipliers
    { SSC_INPUT,  SSC_NUMBER, "ppa_multiplier_model",          "PPA multiplier model",                                          "0/1",          "0=diurnal,1=timestep",              "Time of Delivery Factors",                 "?=0",                                                              "INTEGER,MIN=0", ""},
    { SSC_INPUT,  SSC_ARRAY,  "dispatch_factors_ts",           "Dispatch payment factor array",                                 "",             "",                                  "Time of Delivery Factors",                 "ppa_multiplier_model=1",                                           "",              ""},
    { SSC_INPUT,  SSC_MATRIX, "dispatch_sched_weekday",        "PPA pricing weekday schedule, 12x24",                           "",             "",                                  "Time of Delivery Factors",                 "?=[[1]]",                                                          "",              ""},
    { SSC_INPUT,  SSC_MATRIX, "dispatch_sched_weekend",        "PPA pricing weekend schedule, 12x24",                           "",             "",                                  "Time of Delivery Factors",                 "?=[[1]]",                                                          "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor1",              "Dispatch payment factor 1",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor2",              "Dispatch payment factor 2",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor3",              "Dispatch payment factor 3",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor4",              "Dispatch payment factor 4",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor5",              "Dispatch payment factor 5",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor6",              "Dispatch payment factor 6",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor7",              "Dispatch payment factor 7",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor8",              "Dispatch payment factor 8",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor9",              "Dispatch payment factor 9",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},


    // System Costs
    { SSC_INPUT,  SSC_NUMBER, "cycle_spec_cost",               "Power cycle specific cost",                                     "$/kWe",        "",                                  "System Cost",                              "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "tes_spec_cost",                 "Thermal energy storage specific cost",                          "$/kWht",       "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "heater_spec_cost",              "Heater specific cost",                                          "$/kWht",       "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "bop_spec_cost",                 "Balance of plant specific cost",                                "$/kWe",        "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "contingency_rate",              "Contingency for cost overrun",                                  "%",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "sales_tax_frac",                "Percent of cost to which sales tax applies",                    "%",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "epc_cost_perc_of_direct",       "EPC cost percent of direct",                                    "%",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "epc_cost_per_watt",             "EPC cost per watt",                                             "$/W",          "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "epc_cost_fixed",                "EPC fixed",                                                     "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "land_cost_perc_of_direct",      "Land cost percent of direct",                                   "%",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "land_cost_per_watt",            "Land cost per watt",                                            "$/W",          "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "land_cost_fixed",               "Land fixed",                                                    "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },


    // Financial Parameters
    { SSC_INPUT,  SSC_NUMBER, "sales_tax_rate",                "Sales tax rate",                                                "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },


        // Construction financing inputs/outputs (SSC variable table from cmod_cb_construction_financing)
    { SSC_INPUT,  SSC_NUMBER, "const_per_interest_rate1",      "Interest rate, loan 1",                                         "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_interest_rate2",      "Interest rate, loan 2",                                         "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_interest_rate3",      "Interest rate, loan 3",                                         "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_interest_rate4",      "Interest rate, loan 4",                                         "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_interest_rate5",      "Interest rate, loan 5",                                         "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_months1",             "Months prior to operation, loan 1",                             "",             "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_months2",             "Months prior to operation, loan 2",                             "",             "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_months3",             "Months prior to operation, loan 3",                             "",             "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_months4",             "Months prior to operation, loan 4",                             "",             "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_months5",             "Months prior to operation, loan 5",                             "",             "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_percent1",            "Percent of total installed cost, loan 1",                       "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_percent2",            "Percent of total installed cost, loan 2",                       "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_percent3",            "Percent of total installed cost, loan 3",                       "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_percent4",            "Percent of total installed cost, loan 4",                       "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_percent5",            "Percent of total installed cost, loan 5",                       "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_upfront_rate1",       "Upfront fee on principal, loan 1",                              "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_upfront_rate2",       "Upfront fee on principal, loan 2",                              "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_upfront_rate3",       "Upfront fee on principal, loan 3",                              "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_upfront_rate4",       "Upfront fee on principal, loan 4",                              "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "const_per_upfront_rate5",       "Upfront fee on principal, loan 5",                              "%",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },



    // ****************************************************************************************************************************************
    // Outputs here:
    // ****************************************************************************************************************************************
        // Simulation outputs
    { SSC_OUTPUT, SSC_ARRAY,  "time_hr",                       "Time at end of timestep",                                       "hr",           "",                                  "",                                         "*",                                                                "",              ""},

        // Heater outputs
    { SSC_OUTPUT, SSC_ARRAY,  "W_dot_heater",                  "Heater electricity consumption",                                "MWe",          "",                                  "",                                         "*",                                                                "",              ""},
    { SSC_OUTPUT, SSC_ARRAY,  "q_dot_heater_to_htf",           "Heater thermal power to HTF",                                   "MWt",          "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_ARRAY,  "q_dot_heater_startup",          "Heater thermal power consumed during startup",                  "MWt",          "",                                  "",                                         "*",                                                                "",              "" },

        // TES outputs
    { SSC_OUTPUT, SSC_ARRAY,  "q_dot_dc_tes",                  "TES discharge thermal power",                                   "MWt",          "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_ARRAY,  "q_dot_ch_tes",                  "TES charge thermal power",                                      "MWt",          "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_ARRAY,  "e_ch_tes",                      "TES charge state",                                              "MWht",         "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_ARRAY,  "q_dot_tes_losses",              "TES thermal losses",                                            "MWt",          "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_ARRAY,  "q_dot_tes_heater",              "TES freeze protection power",                                   "MWe",          "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_ARRAY,  "T_tes_hot",                     "TES hot temperature",                                           "C",            "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_ARRAY,  "T_tes_cold",                    "TES cold temperature",                                          "C",            "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_ARRAY,  "mass_tes_cold",                 "TES cold tank mass (end)",                                      "kg",           "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_ARRAY,  "mass_tes_hot",                  "TES hot tank mass (end)",                                       "kg",           "",                                  "",                                         "*",                                                                "",              "" },

        // Cycle outputs
    //{ SSC_OUTPUT, SSC_ARRAY,  "eta_cycle",                     "PC efficiency (no cooling parasitics)",                         "",             "",               "powerblock",     "*",                       "",                      "" },
    { SSC_OUTPUT, SSC_ARRAY,  "q_dot_cycle",                   "PC thermal power",                                              "MWt",          "",               "powerblock",     "*",                       "",                      "" },
    { SSC_OUTPUT, SSC_ARRAY,  "W_dot_cycle_gross",             "PC electrical power gross (no cooling parasitics)",             "MWe",          "",               "powerblock",     "*",                       "",                      "" },
    { SSC_OUTPUT, SSC_ARRAY,  "q_dot_cycle_startup",           "PC startup thermal power",                                      "MWt",          "",                                  "",                                         "*",                                                                "",              "" },


        // System outputs
    { SSC_OUTPUT, SSC_ARRAY,  "W_dot_out_net",                 "Total electric power to grid",                                  "MWe",          "",                                  "",                                         "*",                                                                "",              ""},

    { SSC_OUTPUT, SSC_ARRAY,  "gen",                           "Total electric power to grid with available derate",            "kWe",          "",                                  "",                                         "*",                                                                "",              "" },


        // Annual single-value outputs
    { SSC_OUTPUT, SSC_NUMBER, "annual_energy",                 "Annual total electric power to grid",                           "kWhe",         "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "annual_energy_full_availability","Annual total electric power to grid w/ full availability",     "MWhe",         "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "annual_E_heater",               "Annual heater electric energy consumption",                     "MWhe",         "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "annual_Q_heater_to_htf",        "Annual heater thermal power to HTF",                            "MWhe",         "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "annual_Q_heater_startup",       "Annual heater thermal energy consumed by startup",              "MWht",         "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "annual_E_tes_heater",           "Annual TES freeze heater electric energy consumption",          "MWhe",         "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "annual_Q_tes_losses",           "Annual TES thermal energy lost to ambient",                     "MWht",         "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "annual_E_cycle_gross",          "Annual cycle gross electric energy generation",                 "MWhe",         "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "annual_Q_cycle_thermal_in",     "Annual cycle thermal energy input",                             "MWht",         "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "annual_Q_cycle_thermal_startup","Annual cycle thermal energy consumed by startup",               "MWht",         "",                                  "",                                         "*",                                                                "",              "" },

        // Calculated costs - should these be INOUT?
    { SSC_OUTPUT, SSC_NUMBER, "system_capacity",               "System capacity",                                               "kWe",          "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "total_installed_cost",          "Total installed cost",                                          "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "construction_financing_cost",   "Total construction financing cost",                             "$",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },


    var_info_invalid };

class cm_etes_electric_resistance : public compute_module
{
public:

    cm_etes_electric_resistance()
    {
        add_var_info(_cm_vtab_etes_electric_resistance);
        add_var_info(vtab_adjustment_factors);
    }

    void exec() override
    {
        // *****************************************************
        // System Design Parameters
        double T_htf_cold_des = as_double("T_htf_cold_des");    //[C]
        double T_htf_hot_des = as_double("T_htf_hot_des");      //[C]
        double W_dot_cycle_des = as_double("P_ref");            //[MWe]
        double eta_cycle = as_double("design_eff");             //[-]
        double tshours = as_double("tshours");                  //[-]
        double heater_mult = as_double("heater_mult");          //[-]
        double gross_net_conversion_factor = as_double("gross_net_conversion_factor");

        // TES parameters
        int tes_fl_code = as_integer("tes_fl_code");
        util::matrix_t<double> ud_tes_fl_props = as_matrix("ud_tes_fl_props");

        // System Design Calcs
        double q_dot_pc_des = W_dot_cycle_des / eta_cycle;      //[MWt]
        double Q_tes = q_dot_pc_des * tshours;                  //[MWt-hr]
        double q_dot_heater_des = q_dot_pc_des * heater_mult;   //[MWt]
        double system_capacity = W_dot_cycle_des * gross_net_conversion_factor * 1.E-3; //[kWe]
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Weather reader
        C_csp_weatherreader weather_reader;
        weather_reader.m_weather_data_provider = make_shared<weatherfile>(as_string("solar_resource_file"));
        if (weather_reader.m_weather_data_provider->has_message()) log(weather_reader.m_weather_data_provider->message(), SSC_WARNING);

        weather_reader.m_trackmode = 0;
        weather_reader.m_tilt = 0.0;
        weather_reader.m_azimuth = 0.0;
        // Initialize to get weather file info
        weather_reader.init();
        if (weather_reader.has_error()) throw exec_error("tcsmolten_salt", weather_reader.get_error());
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Simulation setup
        C_csp_solver::S_sim_setup sim_setup;
        sim_setup.m_sim_time_start = as_double("time_start");       //[s] time at beginning of first time step
        sim_setup.m_sim_time_end = as_double("time_stop");          //[s] time at end of last time step

        int steps_per_hour = (int)as_double("time_steps_per_hour");     //[-]

        //if the number of steps per hour is not provided (=-1), then assign it based on the weather file step
        if (steps_per_hour < 0)
        {
            double sph_d = 3600. / weather_reader.m_weather_data_provider->step_sec();
            steps_per_hour = (int)(sph_d + 1.e-5);
            if ((double)steps_per_hour != sph_d)
                throw spexception("The time step duration must be evenly divisible within an hour.");
        }

        size_t n_steps_fixed = (size_t)steps_per_hour * 8760;   //[-]
        if (as_boolean("vacuum_arrays"))
        {
            n_steps_fixed = steps_per_hour * (size_t)((sim_setup.m_sim_time_end - sim_setup.m_sim_time_start) / 3600.);
        }
        sim_setup.m_report_step = 3600.0 / (double)steps_per_hour;  //[s]
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Power cycle
        // Steam Rankine and User Defined power cycle classes
        C_pc_Rankine_indirect_224 rankine_pc;

        int pb_tech_type = as_integer("pc_config");
        if (pb_tech_type == 0 || pb_tech_type == 1)
        {
            C_pc_Rankine_indirect_224::S_params* pc = &rankine_pc.ms_params;
            pc->m_P_ref = W_dot_cycle_des;              //[MWe]
            pc->m_eta_ref = eta_cycle;                  //[-]
            pc->m_T_htf_hot_ref = T_htf_hot_des;        //[C]
            pc->m_T_htf_cold_ref = T_htf_cold_des;      //[C]
            pc->m_cycle_max_frac = as_double("cycle_max_frac");
            pc->m_cycle_cutoff_frac = as_double("cycle_cutoff_frac");
            pc->m_q_sby_frac = as_double("q_sby_frac");
            pc->m_startup_time = as_double("startup_time");
            pc->m_startup_frac = as_double("startup_frac");
            pc->m_htf_pump_coef = as_double("pb_pump_coef");
            pc->m_pc_fl = tes_fl_code;      
            pc->m_pc_fl_props = ud_tes_fl_props;

            if (pb_tech_type == 0)
            {
                pc->m_dT_cw_ref = as_double("dT_cw_ref");
                pc->m_T_amb_des = as_double("T_amb_des");
                pc->m_P_boil = as_double("P_boil");
                pc->m_CT = as_integer("CT");                    // cooling tech type: 1=evaporative, 2=air, 3=hybrid
                if (pc->m_CT > 3) {
                    std::string err_msg = util::format("The specified power cycle cooling tech type, %d, does not exist"
                        " for the ETES electric resistance heating model. Choose from 1) evaporative, 2) air, or 3) hyrbid\n", pb_tech_type);
                    log(err_msg, SSC_WARNING);
                    return;
                }
                pc->m_tech_type = as_integer("tech_type");      // 1: Fixed, 3: Sliding
                if (pc->m_tech_type == 2) { pc->m_tech_type = 1; }; // changing fixed pressure for the trough to fixed pressure for the tower

                if (!(pc->m_tech_type == 1 || pc->m_tech_type == 3 || pc->m_tech_type == 5 || pc->m_tech_type == 6 || pc->m_tech_type == 7 || pc->m_tech_type == 8))
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

                size_t n_F_wc = 0;
                ssc_number_t* p_F_wc = as_array("F_wc", &n_F_wc);
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
                pc->m_W_dot_cooling_des = as_double("ud_f_W_dot_cool_des") / 100.0 * as_double("P_ref");  //[MWe]
                pc->m_m_dot_water_des = as_double("ud_m_dot_water_cool_des");       //[kg/s]

                // User-Defined Cycle Off-Design Tables 
                pc->mc_combined_ind = as_matrix("ud_ind_od");
            }
        }
        else
        {
            std::string err_msg = util::format("The specified power cycle configuration, %d, does not exist. See SSC Input Table for help.\n", pb_tech_type);
            log(err_msg, SSC_WARNING);
            return;
        }

        // Set cycle cmod outputs
        rankine_pc.assign(C_pc_Rankine_indirect_224::E_W_DOT, allocate("W_dot_cycle_gross", n_steps_fixed), n_steps_fixed);
        rankine_pc.assign(C_pc_Rankine_indirect_224::E_Q_DOT_HTF, allocate("q_dot_cycle", n_steps_fixed), n_steps_fixed);
        rankine_pc.assign(C_pc_Rankine_indirect_224::E_Q_DOT_STARTUP, allocate("q_dot_cycle_startup", n_steps_fixed), n_steps_fixed);


        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Electric resistance heater
        // Construct electric resistance heater class
        double f_q_dot_des_allowable_su = 1.0;  //[-]
        double hrs_startup_at_max_rate = 0.25;  //[hr]
        C_csp_cr_electric_resistance c_electric_resistance(T_htf_cold_des, T_htf_hot_des, q_dot_heater_des,
            f_q_dot_des_allowable_su, hrs_startup_at_max_rate,
            tes_fl_code, ud_tes_fl_props);

        // Test init()
        //C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs;
        //C_csp_collector_receiver::S_csp_cr_solved_params solved_params;
        //c_electric_resistance.init(init_inputs, solved_params);

        // Set heater cmod outputs
        c_electric_resistance.mc_reported_outputs.assign(C_csp_cr_electric_resistance::E_W_DOT_HEATER, allocate("W_dot_heater", n_steps_fixed), n_steps_fixed);
        c_electric_resistance.mc_reported_outputs.assign(C_csp_cr_electric_resistance::E_Q_DOT_HTF, allocate("q_dot_heater_to_htf", n_steps_fixed), n_steps_fixed);
        c_electric_resistance.mc_reported_outputs.assign(C_csp_cr_electric_resistance::E_Q_DOT_STARTUP, allocate("q_dot_heater_startup", n_steps_fixed), n_steps_fixed);

        // *****************************************************
        // *****************************************************


        // *****************************************************
        // TES
        C_csp_two_tank_tes storage;
        C_csp_two_tank_tes::S_params* tes = &storage.ms_params;
        tes->m_field_fl = tes_fl_code;
        tes->m_field_fl_props = ud_tes_fl_props;
        tes->m_tes_fl = tes_fl_code;
        tes->m_tes_fl_props = ud_tes_fl_props;
        tes->m_is_hx = false;                       // ETES assumes direct storage, so no user input required here: hardcode = false
        tes->m_W_dot_pc_design = W_dot_cycle_des;   //[MWe]
        tes->m_eta_pc = eta_cycle;                  //[-]
        tes->m_solarm = heater_mult;                //[-]
        tes->m_ts_hours = tshours;                  //[hr]
        tes->m_h_tank = as_double("h_tank");
        tes->m_u_tank = as_double("u_tank");
        tes->m_tank_pairs = as_integer("tank_pairs");
        tes->m_hot_tank_Thtr = as_double("hot_tank_Thtr");
        tes->m_hot_tank_max_heat = as_double("hot_tank_max_heat");
        tes->m_cold_tank_Thtr = as_double("cold_tank_Thtr");
        tes->m_cold_tank_max_heat = as_double("cold_tank_max_heat");
        tes->m_dt_hot = 0.0;                        // MSPT assumes direct storage, so no user input here: hardcode = 0.0
        tes->m_T_field_in_des = T_htf_cold_des;     //[C]
        tes->m_T_field_out_des = T_htf_hot_des;     //[C]
        tes->m_T_tank_hot_ini = T_htf_hot_des;      //[C]
        tes->m_T_tank_cold_ini = T_htf_cold_des;    //[C]
        tes->m_h_tank_min = as_double("h_tank_min");
        tes->m_f_V_hot_ini = as_double("csp.pt.tes.init_hot_htf_percent");
        tes->m_htf_pump_coef = as_double("pb_pump_coef");
        tes->tanks_in_parallel = false;     //[-] False: Field HTF always goes to TES. PC HTF always comes from TES. ETES should not simultaneously operate heater and cycle
        tes->V_tes_des = 1.85;              //[m/s]
        tes->calc_design_pipe_vals = false; // for now, to get 'tanks_in_parallel' to work

        // Set TES cmod outputs
        storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_Q_DOT_LOSS, allocate("q_dot_tes_losses", n_steps_fixed), n_steps_fixed);
        storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_W_DOT_HEATER, allocate("q_dot_tes_heater", n_steps_fixed), n_steps_fixed);
        storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_TES_T_HOT, allocate("T_tes_hot", n_steps_fixed), n_steps_fixed);
        storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_TES_T_COLD, allocate("T_tes_cold", n_steps_fixed), n_steps_fixed);
        storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_MASS_COLD_TANK, allocate("mass_tes_cold", n_steps_fixed), n_steps_fixed);
        storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_MASS_HOT_TANK, allocate("mass_tes_hot", n_steps_fixed), n_steps_fixed);

        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Pricing and operation schedules
        C_csp_tou_block_schedules tou;
        C_csp_tou_block_schedules::S_params* tou_params = &tou.ms_params;

        // Still need to define mc_csp_ops blocks and fractions although we're not using them
        tou_params->mc_csp_ops.mc_weekdays.resize_fill(12, 24, 1.0);
        tou_params->mc_csp_ops.mc_weekends.resize_fill(12, 24, 1.0);
        tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC].resize(2, std::numeric_limits<double>::quiet_NaN());

        tou_params->mc_pricing.mc_weekdays = as_matrix("dispatch_sched_weekday");
        if (tou_params->mc_pricing.mc_weekdays.ncells() == 1) { tou_params->mc_pricing.mc_weekdays.resize_fill(12, 24, 1.); };
        tou_params->mc_pricing.mc_weekends = as_matrix("dispatch_sched_weekend");
        if (tou_params->mc_pricing.mc_weekends.ncells() == 1) { tou_params->mc_pricing.mc_weekends.resize_fill(12, 24, 1.); };
        tou.mc_dispatch_params.m_dispatch_optimize = as_boolean("is_dispatch");

        tou.mc_dispatch_params.m_is_tod_pc_target_also_pc_max = true;
        tou.mc_dispatch_params.m_is_block_dispatch = false;
        tou.mc_dispatch_params.m_is_arbitrage_policy = !tou.mc_dispatch_params.m_dispatch_optimize;
        tou.mc_dispatch_params.m_use_rule_1 = false;
        tou.mc_dispatch_params.m_standby_off_buffer = 2.0;          //[hr] Applies if m_use_rule_1 is true
        tou.mc_dispatch_params.m_use_rule_2 = false;
        tou.mc_dispatch_params.m_q_dot_rec_des_mult = -1.23;        //[-] Applies if m_use_rule_2 is true
        tou.mc_dispatch_params.m_f_q_dot_pc_overwrite = -1.23;      //[-] Applies if m_use_rule_2 is true

        bool is_timestep_input = (as_integer("ppa_multiplier_model") == 1);
        tou_params->mc_pricing.mv_is_diurnal = !(is_timestep_input);
        if (is_timestep_input)
        {
            size_t nmultipliers;
            ssc_number_t* multipliers = as_array("dispatch_factors_ts", &nmultipliers);
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
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // System performance
        C_csp_solver::S_csp_system_params system;
        system.m_pb_fixed_par = as_double("pb_fixed_par");
        system.m_bop_par = as_double("bop_par");
        system.m_bop_par_f = as_double("bop_par_f");
        system.m_bop_par_0 = as_double("bop_par_0");
        system.m_bop_par_1 = as_double("bop_par_1");
        system.m_bop_par_2 = as_double("bop_par_2");

        // *****************************************************
        // *****************************************************

        // *****************************************************
        // Construct System Simulation
        C_csp_solver csp_solver(weather_reader,
            c_electric_resistance,
            rankine_pc,
            storage,
            tou,
            system,
            ssc_cmod_update,
            (void*)(this));

        // Set system cmod outputs
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TIME_FINAL, allocate("time_hr", n_steps_fixed), n_steps_fixed);

        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::W_DOT_NET, allocate("W_dot_out_net", n_steps_fixed), n_steps_fixed);

            // TES
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_Q_DOT_DC, allocate("q_dot_dc_tes", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_Q_DOT_CH, allocate("q_dot_ch_tes", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_E_CH_STATE, allocate("e_ch_tes", n_steps_fixed), n_steps_fixed);


        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Initialize
        update("Initialize ETES electric resistance heating model...", 0.0);

        int out_type = -1;
        std::string out_msg = "";
        try
        {
            // Initialize Solver
            csp_solver.init();
        }
        catch (C_csp_exception& csp_exception)
        {
            // Report warning before exiting with error
            while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
            {
                log(out_msg, out_type);
            }

            throw exec_error("etes_electric_resistance", csp_exception.m_error_message);
        }

        // If no exception, then report messages
        while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
        {
            log(out_msg, out_type);
        }
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Run timeseries simulation
        update("Begin timeseries simulation...", 0.0);

        try
        {
            // Simulate !
            csp_solver.Ssimulate(sim_setup);
        }
        catch (C_csp_exception& csp_exception)
        {
            // Report warning before exiting with error
            while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
            {
                log(out_msg);
            }

            throw exec_error("etes_electric_resistance", csp_exception.m_error_message);
        }

        // If no exception, then report messages
        while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
        {
            log(out_msg, out_type);
        }
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Post-process
        size_t count;
        ssc_number_t* p_W_dot_net = as_array("W_dot_out_net", &count);
        ssc_number_t* p_time_final_hr = as_array("time_hr", &count);

        // 'adjustment_factors' class stores factors in hourly array, so need to index as such
        adjustment_factors haf(this, "adjust");
        if (!haf.setup())
            throw exec_error("tcsmolten_salt", "failed to setup adjustment factors: " + haf.error());

        ssc_number_t* p_gen = allocate("gen", count);
        for (size_t i = 0; i < count; i++)
        {
            size_t hour = (size_t)ceil(p_time_final_hr[i]);
            p_gen[i] = (ssc_number_t)(p_W_dot_net[i] * 1.E3 * haf(hour));           //[kWe]
        }
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Report simulation metrics
        ssc_number_t* p_annual_energy_dist_time = gen_heatmap(this, steps_per_hour);

        accumulate_annual_for_year("gen", "annual_energy", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour);
        accumulate_annual_for_year("W_dot_out_net", "annual_energy_full_availability", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour);
        accumulate_annual_for_year("W_dot_heater", "annual_E_heater", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour); //[MWhe]
        accumulate_annual_for_year("q_dot_heater_to_htf", "annual_Q_heater_to_htf", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour); //[MWhe]
        accumulate_annual_for_year("q_dot_heater_startup", "annual_Q_heater_startup", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour); //[MWht]
        accumulate_annual_for_year("q_dot_tes_heater", "annual_E_tes_heater", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour); //[MWht]
        accumulate_annual_for_year("q_dot_tes_losses", "annual_Q_tes_losses", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour); //[MWht]
        accumulate_annual_for_year("W_dot_cycle_gross", "annual_E_cycle_gross", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour); //[MWhe]
        accumulate_annual_for_year("q_dot_cycle", "annual_Q_cycle_thermal_in", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour); //[MWht]
        accumulate_annual_for_year("q_dot_cycle_startup", "annual_Q_cycle_thermal_startup", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour); //[MWht]

        
        // *****************************************************
        // Calculate annual metrics
        double E_heater = as_double("annual_E_heater");
        double Q_heater_to_htf = as_double("annual_Q_heater_to_htf");
        double Q_tes_heater = as_double("annual_E_tes_heater");
        double Q_tes_losses = as_double("annual_Q_tes_losses");
        double Q_cycle_thermal_in = as_double("annual_Q_cycle_thermal_in");

        double q_balance_rel = (Q_heater_to_htf + Q_tes_heater - Q_tes_losses - Q_cycle_thermal_in) / Q_cycle_thermal_in;


        // *****************************************************
        // Calculate system costs
        double tes_spec_cost = as_double("tes_spec_cost");
        double power_cycle_spec_cost = as_double("cycle_spec_cost");
        double heater_spec_cost = as_double("heater_spec_cost");
        double bop_spec_cost = as_double("bop_spec_cost");
        double contingency_rate = as_double("contingency_rate");

        double plant_net_capacity = system_capacity / 1000.0;         //[MWe], convert from kWe
        double EPC_perc_direct_cost = as_double("epc_cost_perc_of_direct");
        double EPC_per_power_cost = as_double("epc_cost_per_watt");
        double EPC_fixed_cost = as_double("epc_cost_fixed");
        double total_land_perc_direct_cost = as_double("land_cost_perc_of_direct");
        double total_land_per_power_cost = as_double("land_cost_per_watt");
        double total_land_fixed_cost = as_double("land_cost_fixed");
        double sales_tax_basis = as_double("sales_tax_frac");
        double sales_tax_rate = as_double("sales_tax_rate");

        // Cost model outputs
        double tes_cost, power_cycle_cost, heater_cost, bop_cost, fossil_backup_cost,
            direct_capital_precontingency_cost, contingency_cost, total_direct_cost, total_land_cost,
            epc_and_owner_cost, sales_tax_cost, total_indirect_cost, total_installed_cost, estimated_installed_cost_per_cap;
        tes_cost = power_cycle_cost = heater_cost = bop_cost = fossil_backup_cost =
            direct_capital_precontingency_cost = contingency_cost = total_direct_cost = total_land_cost =
            epc_and_owner_cost = sales_tax_cost = total_indirect_cost = total_installed_cost = estimated_installed_cost_per_cap = std::numeric_limits<double>::quiet_NaN();

        N_mspt::calculate_etes_costs(Q_tes, tes_spec_cost, W_dot_cycle_des, power_cycle_spec_cost,
            q_dot_heater_des, heater_spec_cost, bop_spec_cost, contingency_rate,
            plant_net_capacity, EPC_perc_direct_cost, EPC_per_power_cost, EPC_fixed_cost,
            total_land_perc_direct_cost, total_land_per_power_cost, total_land_fixed_cost,
            sales_tax_basis, sales_tax_rate,
            tes_cost, power_cycle_cost, heater_cost, bop_cost, direct_capital_precontingency_cost,
            contingency_cost, total_direct_cost, total_land_cost, epc_and_owner_cost,
            sales_tax_cost, total_indirect_cost, total_installed_cost, estimated_installed_cost_per_cap);

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

        N_financial_parameters::construction_financing_total_cost(total_installed_cost,
            const_per_interest_rate1, const_per_interest_rate2, const_per_interest_rate3, const_per_interest_rate4, const_per_interest_rate5,
            const_per_months1, const_per_months2, const_per_months3, const_per_months4, const_per_months5,
            const_per_percent1, const_per_percent2, const_per_percent3, const_per_percent4, const_per_percent5,
            const_per_upfront_rate1, const_per_upfront_rate2, const_per_upfront_rate3, const_per_upfront_rate4, const_per_upfront_rate5,
            const_per_principal1, const_per_principal2, const_per_principal3, const_per_principal4, const_per_principal5,
            const_per_interest1, const_per_interest2, const_per_interest3, const_per_interest4, const_per_interest5,
            const_per_total1, const_per_total2, const_per_total3, const_per_total4, const_per_total5,
            const_per_percent_total, const_per_principal_total, const_per_interest_total, construction_financing_cost);

        // Assign cmod variables required by downstream models
        assign("system_capacity", system_capacity);     //[kWe]
        assign("total_installed_cost", (ssc_number_t)total_installed_cost);                 //[$]
        assign("construction_financing_cost", (ssc_number_t)construction_financing_cost);   //[$]

    }
};

DEFINE_MODULE_ENTRY(etes_electric_resistance, "Electric resistance heater charging TES from grid, discharge with power cycle", 1)
