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
#include "csp_solver_core.h"

#include "csp_solver_pc_ptes.h"
#include "csp_solver_cr_heat_pump.h"
#include "csp_solver_two_tank_tes.h"
#include "csp_solver_tou_block_schedules.h"
#include "etes_dispatch.h"

#include "csp_system_costs.h"

static var_info _cm_vtab_etes_ptes[] = {

    // Resource Data
    { SSC_INPUT,  SSC_STRING, "solar_resource_file",           "Local weather file path",                                        "",             "",                                  "Solar Resource",                           "?",                                                                "LOCAL_FILE",    ""},

    // Simulation Parameters
    { SSC_INPUT,  SSC_NUMBER, "is_dispatch",                   "Allow dispatch optimization?",                                   "",             "",                                  "System Control",                           "?=0",                                                              "",               ""},
    { SSC_INPUT,  SSC_NUMBER, "sim_type",                      "1 (default): timeseries, 2: design only",                        "",             "",                                  "System Control",                           "?=1",                                                              "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "etes_financial_model",          "",                                                               "1-8",          "",                                  "Financial Model",                          "?=1",                                                              "INTEGER,MIN=0", ""},
    { SSC_INPUT,  SSC_NUMBER, "time_start",                    "Simulation start time",                                          "s",            "",                                  "System Control",                           "?=0",                                                              "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "time_stop",                     "Simulation stop time",                                           "s",            "",                                  "System Control",                           "?=31536000",                                                       "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "time_steps_per_hour",           "Number of simulation time steps per hour",                       "",             "",                                  "System Control",                           "?=-1",                                                             "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "vacuum_arrays",                 "Allocate arrays for only the required number of steps",          "",             "",                                  "System Control",                           "?=0",                                                              "",              "SIMULATION_PARAMETER"},


    // HTFs
    { SSC_INPUT,  SSC_NUMBER, "hot_htf_code",                  "Hot HTF code - see htf_props.h for list",                        "",             "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_MATRIX, "ud_hot_htf_props",              "User-defined Hot HTF fluid property data",                       "-",            "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "cold_htf_code",                 "Cold HTF code - see htf_props.h for list",                       "",             "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_MATRIX, "ud_cold_htf_props",             "User-defined Cold HTF fluid property data",                      "-",            "",                                  "Thermal Storage",                          "*",                                                                "",              ""},


    // Heat Pump
    { SSC_INPUT,  SSC_NUMBER, "f_q_dot_des_allowable_su",      "Fraction of design power allowed during startup",                "-",            "",                                  "Heater",                                   "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "hrs_startup_at_max_rate",       "Duration of startup at max startup power",                       "hr",           "",                                  "Heater",                                   "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "f_q_dot_heater_min",            "Minimum allowable heater output as fraction of design",          "",             "",                                  "Heater",                                   "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "heat_pump_HT_HTF_pump_coef",    "High temp HX pumping power to move 1 kg/s",                      "kW/kg/s",      "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "heat_pump_CT_HTF_pump_coef",    "Cold temp HX pumping power to move 1 kg/s",                      "kW/kg/s",      "",                                  "Power Cycle",                              "*",                                                                "",              ""},


    // Power Cycle
        // General
    { SSC_INPUT,  SSC_NUMBER, "pb_pump_coef",                  "COLD TES pumping power to move 1kg of HTF through PB loop",      "kW/kg/s",      "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "CT_pb_pump_coef",               "COLD TES pumping power to move 1kg of HTF through PB loop",      "kW/kg/s",      "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "startup_time",                  "Time needed for power block startup",                            "hr",           "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "startup_frac",                  "Fraction of design thermal power needed for startup",            "none",         "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "cycle_max_frac",                "Maximum turbine over design operation fraction",                 "",             "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "cycle_cutoff_frac",             "Minimum turbine operation fraction before shutdown",             "",             "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "q_sby_frac",                    "Fraction of thermal power required for standby",                 "",             "",                                  "Power Cycle",                              "*",                                                                "",              ""},


    // High Temp Two-Tank TES
    { SSC_INPUT,  SSC_NUMBER, "tes_init_hot_htf_percent",      "HOT TES Initial fraction of available volume that is hot",       "%",            "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "h_tank",                        "HOT TES Total height of tank (height of HTF when tank is full)", "m",            "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "cold_tank_max_heat",            "HOT TES Rated heater capacity for cold tank heating",            "MW",           "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "u_tank",                        "HOT TES Loss coefficient from the tank",                         "W/m2-K",       "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "tank_pairs",                    "HOT TES Number of equivalent tank pairs",                        "",             "",                                  "Hot Thermal Storage",                      "*",                                                                "INTEGER",       ""},
    { SSC_INPUT,  SSC_NUMBER, "cold_tank_Thtr",                "HOT TES Minimum allowable cold tank HTF temperature",            "C",            "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "h_tank_min",                    "HOT TES Minimum allowable HTF height in storage tank",           "m",            "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "hot_tank_Thtr",                 "HOT TES Minimum allowable hot tank HTF temperature",             "C",            "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "hot_tank_max_heat",             "HOT TES Rated heater capacity for hot tank heating",             "MW",           "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},


    // COLD Temp Two-Tank TES
    { SSC_INPUT,  SSC_NUMBER, "CT_tes_init_hot_htf_percent",   "COLD TES Initial fraction of available volume that is hot",      "%",            "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "CT_h_tank",                     "COLD TES Total height of tank (height of HTF when tank is full)","m",            "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "CT_u_tank",                     "COLD TES Loss coefficient from the tank",                        "W/m2-K",       "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "CT_tank_pairs",                 "COLD TES Number of equivalent tank pairs",                       "",             "",                                  "Cold Thermal Storage",                     "*",                                                                "INTEGER",       ""},
    { SSC_INPUT,  SSC_NUMBER, "CT_h_tank_min",                 "COLD TES Minimum allowable HTF height in storage tank",          "m",            "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},
    //{ SSC_INPUT,  SSC_NUMBER, "CT_cold_tank_max_heat",         "COLD TES Rated heater capacity for cold tank heating",           "MW",           "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},
    //{ SSC_INPUT,  SSC_NUMBER, "CT_cold_tank_Thtr",             "COLD TES Minimum allowable cold tank HTF temperature",           "C",            "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},
    //{ SSC_INPUT,  SSC_NUMBER, "CT_hot_tank_Thtr",              "COLD TES Minimum allowable hot tank HTF temperature",            "C",            "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},
    //{ SSC_INPUT,  SSC_NUMBER, "CT_hot_tank_max_heat",          "COLD TES Rated heater capacity for hot tank heating",            "MW",           "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},


    // Pricing schedules and multipliers
    { SSC_INPUT,  SSC_NUMBER, "ppa_multiplier_model",          "PPA multiplier model",                                          "0/1",          "0=diurnal,1=timestep",              "Time of Delivery Factors",                 "?=0",                                                              "INTEGER,MIN=0", "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_ARRAY,  "dispatch_factors_ts",           "Dispatch payment factor timeseries array",                      "",             "",                                  "Time of Delivery Factors",                 "ppa_multiplier_model=1&etes_financial_model<5&is_dispatch=1&sim_type=1",      "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_MATRIX, "dispatch_sched_weekday",        "PPA pricing weekday schedule, 12x24",                           "",             "",                                  "Time of Delivery Factors",                 "ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1",      "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_MATRIX, "dispatch_sched_weekend",        "PPA pricing weekend schedule, 12x24",                           "",             "",                                  "Time of Delivery Factors",                 "ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1",      "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor1",              "Dispatch payment factor 1",                                     "",             "",                                  "Time of Delivery Factors",                 "ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1",      "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor2",              "Dispatch payment factor 2",                                     "",             "",                                  "Time of Delivery Factors",                 "ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1",      "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor3",              "Dispatch payment factor 3",                                     "",             "",                                  "Time of Delivery Factors",                 "ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1",      "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor4",              "Dispatch payment factor 4",                                     "",             "",                                  "Time of Delivery Factors",                 "ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1",      "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor5",              "Dispatch payment factor 5",                                     "",             "",                                  "Time of Delivery Factors",                 "ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1",      "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor6",              "Dispatch payment factor 6",                                     "",             "",                                  "Time of Delivery Factors",                 "ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1",      "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor7",              "Dispatch payment factor 7",                                     "",             "",                                  "Time of Delivery Factors",                 "ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1",      "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor8",              "Dispatch payment factor 8",                                     "",             "",                                  "Time of Delivery Factors",                 "ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1",      "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor9",              "Dispatch payment factor 9",                                     "",             "",                                  "Time of Delivery Factors",                 "ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1",      "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_ARRAY,  "ppa_price_input",			   "PPA prices - yearly",			                                "$/kWh",	    "",	                                 "Revenue",			                         "ppa_multiplier_model=0&etes_financial_model<5&is_dispatch=1&sim_type=1",      "",      	     "SIMULATION_PARAMETER"},


    // System performance
    { SSC_INPUT,  SSC_NUMBER, "pb_fixed_par",                  "Fixed parasitic load that don't generate heat - runs at all times","MWe/MWcap", "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par",                       "Balance of plant parasitic power fraction",                        "MWe/MWcap", "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par_f",                     "Balance of plant parasitic power fraction - mult frac",            "",          "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par_0",                     "Balance of plant parasitic power fraction - const coeff",          "",          "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par_1",                     "Balance of plant parasitic power fraction - linear coeff",         "",          "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par_2",                     "Balance of plant parasitic power fraction - quadratic coeff",      "",          "",                                  "System Control",                           "*",                                                                "",              ""},

    // System costs
    { SSC_INPUT,  SSC_NUMBER, "cycle_spec_cost",               "Power cycle specific cost",                                     "$/kWe",        "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "tes_spec_cost",                 "Hot Temp thermal energy storage specific cost",                 "$/kWht",       "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "CT_tes_spec_cost",              "Cold Temp thermal energy storage specific cost",                "$/kWht",       "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_INPUT,  SSC_NUMBER, "heat_pump_spec_cost",           "Heater pump specific cost",                                     "$/kWht",       "",                                  "System Costs",                             "*",                                                                "",              "" },
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
        // Design Outputs here:
        // ****************************************************************************************************************************************
            // System
    { SSC_OUTPUT, SSC_NUMBER, "system_capacity",                 "System capacity (discharge)",                         "kWe",          "",                                  "System Design Calc",             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "nameplate",                       "Nameplate capacity (discharge)",                      "MWe",          "",                                  "System Design Calc",             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "rte_thermo",                      "Round-trip efficiency of working fluid cycles",       "MWe",          "",                                  "System Design Calc",             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "rte_net",                         "Net round-trip efficiency considering all parasitics","MWe",          "",                                  "System Design Calc",             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "charge_capacity",                 "Total electricity consumption at design-point charge","MWe",          "",                                  "System Design Calc",             "*",                                                                "",              "" },
                                                                 
            // Heater                                            
    { SSC_OUTPUT, SSC_NUMBER, "W_dot_in_thermo_charge_des",      "Heat pump power into working fluid",                  "MWe",          "",                                  "Heat Pump",                      "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "q_dot_hot_out_charge_des",        "Heat pump heat output",                               "MWt",          "",                                  "Heat Pump",                      "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "q_dot_cold_in_charge_des",        "Heat pump heat input",                                "MWt",          "",                                  "Heat Pump",                      "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "W_dot_elec_parasitic_charge_des", "Heat pump parasitic power",                           "MWe",          "",                                  "Heat Pump",                      "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "W_dot_in_charge_net_des",         "Heat pump total power consumption",                   "MWe",          "",                                  "Heat Pump",                      "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "COP_net_des",                     "Heat pump net COP",                                   "MWe",          "",                                  "Heat Pump",                      "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "m_dot_HT_htf_charge_des",         "Heat pump HT HTF mass flow rate",                     "kg/s",         "",                                  "Heat Pump",                      "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "W_dot_HT_htf_pump_charge_des",    "Heat pump HT HTF pump power",                         "MWe",          "",                                  "Heat Pump",                      "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "m_dot_CT_htf_charge_des",         "Heat pump CT HTF mass flow rate",                     "kg/s",         "",                                  "Heat Pump",                      "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "W_dot_CT_htf_pump_charge_des",    "Heat pump CT HTF pump power",                         "MWe",          "",                                  "Heat Pump",                      "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "E_heater_su_des",                 "Heat pump startup energy",                            "MWt-hr",       "",                                  "Heat Pump",                      "*",                                                                "",              "" },

            // Cycle
    { SSC_OUTPUT, SSC_NUMBER, "q_dot_hot_in_gen_des",            "Cycle heat input",                                    "MWt",          "",                                  "Cycle",                          "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "q_dot_cold_out_thermo_gen_des",   "Cycle total heat rejection",                          "MWt",          "",                                  "Cycle",                          "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "W_dot_elec_parasitic_gen_des",    "Cycle parasitic power",                               "MWe",          "",                                  "Cycle",                          "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "W_dot_gen_net_des",               "Cycle net power generation",                          "MWe",          "",                                  "Cycle",                          "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "eta_net_gen_des",                 "Cycle net efficiency",                                "-",            "",                                  "Cycle",                          "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "q_dot_cold_to_CTES_gen_des",      "Cycle heat to cold TES",                              "MWt",          "",                                  "Cycle",                          "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "q_dot_cold_to_surroundings_gen_des", "Cycle heat to surroundings",                       "MWt",          "",                                  "Cycle",                          "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "m_dot_HT_htf_gen_des",            "Cycle HT HTF mass flow rate",                         "kg/s",         "",                                  "Cycle",                          "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "W_dot_HT_htf_pump_gen_des",       "Cycle HT HTF pump power",                             "MWe",          "",                                  "Cycle",                          "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "m_dot_CT_htf_gen_des",            "Cycle CT HTF mass flow rate",                         "kg/s",         "",                                  "Cycle",                          "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "W_dot_CT_htf_pump_gen_des",       "Cycle CT HTF pump power",                             "MWe",          "",                                  "Cycle",                          "*",                                                                "",              "" },

            // Hot TES
    { SSC_OUTPUT, SSC_NUMBER, "Q_tes_des",                       "TES design capacity",                                 "MWt-hr",       "",                                  "TES Design Calc",                "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "V_tes_htf_avail",                 "Volume of TES HTF available for heat transfer",       "m3",           "",                                  "TES Design Calc",                "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "V_tes_htf_total",                 "Total TES HTF volume",                                "m3",           "",                                  "TES Design Calc",                "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "d_tank_tes",                      "Diameter of TES tank",                                "m",            "",                                  "TES Design Calc",                "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "q_dot_loss_tes_des",              "TES thermal loss at design",                          "MWt",          "",                                  "TES Design Calc",                "*",                                                                "",              "" },

            // Cold TES
    { SSC_OUTPUT, SSC_NUMBER, "Q_CT_tes_des",                    "TES design capacity",                                 "MWt-hr",       "",                                  "TES Design Calc",                "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "V_CT_tes_htf_avail",              "Volume of TES HTF available for heat transfer",       "m3",           "",                                  "TES Design Calc",                "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "V_CT_tes_htf_total",              "Total TES HTF volume",                                "m3",           "",                                  "TES Design Calc",                "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "d_CT_tank_tes",                   "Diameter of TES tank",                                "m",            "",                                  "TES Design Calc",                "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "q_dot_loss_CT_tes_des",           "TES thermal loss at design",                          "MWt",          "",                                  "TES Design Calc",                "*",                                                                "",              "" },

            // Balance of Plant
    { SSC_OUTPUT, SSC_NUMBER, "W_dot_bop_design",                "BOP parasitics at design",                            "MWe",          "",                                  "Balance of Plant",               "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "W_dot_fixed",                     "Fixed parasitic at design",                           "MWe",          "",                                  "Balance of Plant",               "*",                                                                "",              "" },

            // Costs
    { SSC_OUTPUT, SSC_NUMBER, "heater_cost_calc",                "Heater cost",                             "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "tes_cost_calc",                   "TES cost",                                "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "CT_tes_cost_calc",                "Cold TES cost",                           "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "bop_cost_calc",                   "BOP cost",                                "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "cycle_cost_calc",                 "Cycle cost",                              "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "direct_subtotal_cost_calc",       "Direct subtotal cost",                    "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "contingency_cost_calc",           "Contingency cost",                        "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "total_direct_cost_calc",          "Total direct cost",                       "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "epc_cost_calc",                   "EPC cost",                                "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "land_cost_calc",                  "Land cost",                               "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "sales_tax_cost_calc",             "Sales tax cost",                          "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "total_indirect_cost_calc",        "Total indirect cost",                     "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "installed_per_cap_cost_calc",     "Installed cost per capacity",             "$/kWe",        "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "total_installed_cost",            "Total installed cost",                    "$",            "",                                  "System Costs",                             "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "construction_financing_cost",     "Total construction financing cost",       "$",            "",                                  "Financial Parameters",                     "*",                                                                "",              "" },

    // ****************************************************************************************************************************************
    // Timeseries Simulation Outputs here:
    // ****************************************************************************************************************************************
            // Simulation outputs


            // ETES settings for financial model
    { SSC_OUTPUT, SSC_NUMBER, "ppa_soln_mode",                 "PPA solution mode",                                             "0/1",          "0 = solve ppa,1 = specify ppa",     "Revenue",                                  "sim_type=1",                                                                "INTEGER,MIN = 0,MAX = 1", "" },
    { SSC_OUTPUT, SSC_NUMBER, "flip_target_percent",		   "After-tax IRR target",		                                    "%",	        "",					                 "Revenue",                                  "sim_type=1",					                                              "MIN=0,MAX=100",     	     "" },



    var_info_invalid };

class cm_etes_ptes : public compute_module
{
public:
    cm_etes_ptes()
    {
        add_var_info(_cm_vtab_etes_ptes);
    }

    void exec() override
    {
        //FILE* fp = fopen("etes_cmod_to_lk.lk", "w");
        //write_cmod_to_lk_script(fp, m_vartab);

        // First, check sim type
        int sim_type = as_integer("sim_type");
        if (sim_type != 1 && sim_type != 2) {
            std::string sim_type_msg = util::format("sim_type input was %d. It must be 1 (timeseries) or 2 (design only)", sim_type);

            throw exec_error("etes_ptes", sim_type_msg);
        }

        // System design calcs
        // Need more information than typical because cycle and heat pump design are related
        // And we want all component calcs to live in component methods/helpers
        double heater_mult = 1.0;               //[-]
        double tshours = 10.0;      //[-]

        // Define generation mechanical, electrical, and thermal power
        // Need to break out thermodynamic cycle so that net output, heat input, heat output, and efficiency are consistent
        // Important because: 1) important to capture exact heat rejection for CT storage and net efficiency includes electrical parasitics that don't apply to cycle working fluid
        double W_dot_gen_thermo = 100.0;    //[MWe]
        double f_elec_consume_vs_gen = 0.1; //[-] Fraction of thermo generation that cycle uses for parasitics (motors, generators, cooling)
        double eta_therm_mech = 0.5;        //[-]
        double W_dot_gen_net, W_dot_gen_elec_parasitic, q_dot_hot_in_gen, q_dot_cold_out_gen, eta_gen_net;
        W_dot_gen_net = W_dot_gen_elec_parasitic = q_dot_hot_in_gen = q_dot_cold_out_gen = eta_gen_net = std::numeric_limits<double>::quiet_NaN();
        pc_ptes_helpers::design_calcs__no_ctes(W_dot_gen_thermo, f_elec_consume_vs_gen,
            eta_therm_mech,
            W_dot_gen_net, W_dot_gen_elec_parasitic,
            q_dot_hot_in_gen, q_dot_cold_out_gen, eta_gen_net);

        // Define heat pump power/heat flows
            // Design parameters
        double COP_heat_charge_therm = 1.5;     //[-]
        double f_elec_consume_vs_W_dot_thermo = 0.05;   //[-]
            // Calculate heat and power 
        double q_dot_hot_out_charge = q_dot_hot_in_gen*heater_mult; //[MWt]
            // Call heat pump high-level design method
        double W_dot_in_charge_thermo, q_dot_cold_in_charge, W_dot_in_charge_elec, W_dot_charge_net, COP_heat_charge_net;
        W_dot_in_charge_thermo = q_dot_cold_in_charge = W_dot_in_charge_elec = W_dot_charge_net = COP_heat_charge_net = std::numeric_limits<double>::quiet_NaN();
        heat_pump_helpers::design_calcs(q_dot_hot_out_charge, COP_heat_charge_therm, f_elec_consume_vs_W_dot_thermo,
            W_dot_in_charge_thermo, q_dot_cold_in_charge, W_dot_in_charge_elec, W_dot_charge_net, COP_heat_charge_net);

        // Ratio to heat pump q_dot_cold to q_dot_hot
        // Power cycle model uses this to fix ratios of CT and HT TES mass flow rates
        // So that CT and HT availability moves together
        // (could also use COP but thought this value was more general)
        double fixed__q_dot_cold__to__q_dot_warm = q_dot_cold_in_charge / q_dot_hot_out_charge;     //[-]
        // Now can solve cycle split between CT TES and rejection to ambient
        double q_dot_cold_out_gen_to_CTES = std::numeric_limits<double>::quiet_NaN();       //[MWt]
        double q_dot_cold_out_gen_to_surr = std::numeric_limits<double>::quiet_NaN();       //[MWt]
        pc_ptes_helpers::design_calcs__q_dot_ctes(q_dot_hot_in_gen, fixed__q_dot_cold__to__q_dot_warm, q_dot_cold_out_gen,
                                    q_dot_cold_out_gen_to_CTES, q_dot_cold_out_gen_to_surr);

        // Calculate some RTE terms
        // Probably want some "true" net RTE that includes all parasitics
        // ... e.g. these don't include pumping power or BOP & fixed system parasitics
        double RTE_therm = eta_therm_mech * COP_heat_charge_therm;      //[-]
        double RTE_cycles_net = eta_gen_net * COP_heat_charge_net;      //[-]

        // *****************************************************
        // *****************************************************
        // --- Either ----
        // Define temperatures - define TES temps using CYCLE working fluid and approach temps
            // High Temp two-tank - charging
        //double T_HT_hot_charge = 565.0;     //[C]
        //double T_HT_cold_charge = 310.0;    //[C]
        //double dT_HX_HT_charge = 5.0;       //[C] assume counterflow CR = 1
        //    // Calculate Hot Temp TES temps
        //double T_HT_hot_TES = T_HT_hot_charge - dT_HX_HT_charge;   //[C]
        //double T_HT_cold_TES = T_HT_cold_charge - dT_HX_HT_charge; //[C]
        //
        //    // Cold Temp two-tank - charging
        //double T_CT_cold_charge = -50.0;    //[C]
        //double T_CT_hot_charge = 50.0;      //[C]
        //double dT_HX_CT_charge = 5.0;       //[C] assume counterflow CR = 1
        //    // Calculate Cold Temp TES temps
        //double T_CT_cold_TES = T_CT_cold_charge + dT_HX_CT_charge;  //[C]
        //double T_CT_hot_TES = T_CT_hot_charge + dT_HX_HT_charge;    //[C]
        //
        //    // Generation temperatures
        //double dT_HX_HT_gen = 5.0;  //[C] assume counterflow CR = 1
        //double T_HT_hot_gen = T_HT_hot_TES - dT_HX_HT_gen;      //[C]
        //double T_HT_cold_gen = T_HT_cold_TES - dT_HX_HT_gen;    //[C]
        //
        //double dT_HX_CT_gen = 5.0;  //[C] assume counterflow CR = 1
        //double T_CT_hot_gen = T_CT_hot_TES + dT_HX_CT_gen;      //[C]
        //double T_CT_cold_gen = T_CT_cold_TES + dT_HX_CT_gen;    //[C]
        // *****************************************************
        // *****************************************************
        // --- OR ----
        // Define TES temps only. Assume direct storage. Heat pump and cycle model off-design uses HTF temps
        double T_HT_hot_TES = 560.0;    //[C]
        double T_HT_cold_TES = 305.0;   //[C]
        double T_CT_cold_TES = -45.0;   //[C]
        double T_CT_hot_TES = 55.0;     //[C]
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
        if (weather_reader.has_error()) throw exec_error("etes_ptes", weather_reader.get_error());
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
                throw exec_error("etes_ptes", "The time step duration must be evenly divisible within an hour.");
        }

        size_t n_steps_fixed = (size_t)steps_per_hour * 8760;   //[-]
        if (as_boolean("vacuum_arrays"))
        {
            n_steps_fixed = steps_per_hour * (size_t)((sim_setup.m_sim_time_end - sim_setup.m_sim_time_start) / 3600.);
        }
        sim_setup.m_report_step = 3600.0 / (double)steps_per_hour;  //[s]
        // *****************************************************
        // *****************************************************

        // Get HTF inputs here
        int HT_htf_code = as_integer("hot_htf_code");
        util::matrix_t<double> ud_HT_htf_props = as_matrix("ud_hot_htf_props");
        int CT_htf_code = as_integer("cold_htf_code");
        util::matrix_t<double> ud_CT_htf_props = as_matrix("ud_cold_htf_props");

        // *****************************************************
        // Power cycle
        double cycle_max_frac = as_double("cycle_max_frac");        //[-]
        double cycle_cutoff_frac = as_double("cycle_cutoff_frac");  //[-]
        double q_sby_frac = as_double("q_sby_frac");                //[-]
        double HT_htf_pump_coef = as_double("pb_pump_coef");        //[kW/kg/s]
        double CT_htf_pump_coef = as_double("CT_pb_pump_coef");     //[kW/kg/s]
        double startup_time = as_double("startup_time");            //[hr]
        double startup_frac = as_double("startup_frac");            //[-]

        C_pc_ptes c_pc(W_dot_gen_thermo, eta_therm_mech,
            f_elec_consume_vs_gen, fixed__q_dot_cold__to__q_dot_warm,
            T_HT_hot_TES, T_HT_cold_TES, T_CT_cold_TES, T_CT_hot_TES,
            cycle_max_frac, cycle_cutoff_frac, q_sby_frac,
            startup_time, startup_frac,
            HT_htf_pump_coef, CT_htf_pump_coef,
            HT_htf_code, ud_HT_htf_props,
            CT_htf_code, ud_CT_htf_props);

        C_csp_power_cycle::S_solved_params pc_solved_params;
        
        try {
            c_pc.init(pc_solved_params);
        }
        catch (C_csp_exception& csp_exception) {

            int out_type = -1;
            std::string out_msg = "";
            // Report warning before exiting with error
            while (c_pc.mc_csp_messages.get_message(&out_type, &out_msg))
            {
                log(out_msg, out_type);
            }

            throw exec_error("etes_electric_resistance", csp_exception.m_error_message);
        }
        // **********************************************************
        // **********************************************************

        // **********************************************************
        // Heat pump
        double f_q_dot_des_allowable_su = as_double("f_q_dot_des_allowable_su");    //[-] fraction of design power allowed during startup
        double hrs_startup_at_max_rate = as_double("hrs_startup_at_max_rate");      //[hr] duration of startup at max startup power
        double f_heater_min = as_double("f_q_dot_heater_min");                      //[-] minimum allowable heater output as fraction of design

        double heat_pump_HT_htf_pump_coef = as_double("heat_pump_HT_HTF_pump_coef");           //[kW/kg/s]
        double heat_pump_CT_htf_pump_coef = as_double("heat_pump_CT_HTF_pump_coef");           //[kW/kg/s]

        C_csp_cr_heat_pump c_heat_pump(COP_heat_charge_therm, q_dot_hot_out_charge,
            f_elec_consume_vs_W_dot_thermo,
            T_HT_hot_TES, T_HT_cold_TES, T_CT_cold_TES, T_CT_hot_TES,
            f_heater_min, f_q_dot_des_allowable_su, hrs_startup_at_max_rate,
            heat_pump_HT_htf_pump_coef, heat_pump_CT_htf_pump_coef,
            HT_htf_code, ud_HT_htf_props,
            CT_htf_code, ud_CT_htf_props);

        // **********************************************************
        // **********************************************************

        // **********************************************************
        // High temp TES
         C_csp_two_tank_tes c_HT_TES;
        {
            C_csp_two_tank_tes::S_params* tes = &c_HT_TES.ms_params;
            tes->m_field_fl = HT_htf_code;
            tes->m_field_fl_props = ud_HT_htf_props;
            tes->m_tes_fl = HT_htf_code;
            tes->m_tes_fl_props = ud_HT_htf_props;
            tes->m_W_dot_pc_design = W_dot_gen_thermo;  //[MWe]
            tes->m_eta_pc = eta_therm_mech;             //[-]
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
            tes->m_T_field_in_des = T_HT_cold_TES;      //[C]
            tes->m_T_field_out_des = T_HT_hot_TES;      //[C]
            tes->m_T_tank_hot_ini = T_HT_hot_TES;       //[C]
            tes->m_T_tank_cold_ini = T_HT_cold_TES;     //[C]
            tes->m_h_tank_min = as_double("h_tank_min");
            tes->m_f_V_hot_ini = as_double("tes_init_hot_htf_percent");
            tes->m_htf_pump_coef = 0.0;         //[kW/kg/s] No htf pump losses in direct TES
            tes->tanks_in_parallel = false;     //[-] False: Field HTF always goes to TES. PC HTF always comes from TES. ETES should not simultaneously operate heater and cycle
            tes->V_tes_des = 1.85;              //[m/s]
            tes->calc_design_pipe_vals = false; // for now, to get 'tanks_in_parallel' to work
        }

        // **********************************************************
        // **********************************************************

        // **********************************************************
        // Cold temp TES
        std::shared_ptr<C_csp_two_tank_tes> c_CT_TES(new C_csp_two_tank_tes());
        {
            C_csp_two_tank_tes::S_params* ctes = &c_CT_TES->ms_params;
            ctes->m_field_fl = CT_htf_code;
            ctes->m_field_fl_props = ud_CT_htf_props;
            ctes->m_tes_fl = CT_htf_code;
            ctes->m_tes_fl_props = ud_CT_htf_props;
            // Power/effiency relationship doesn't hold for cold tank, so just fake it with power = heat and eta = 1
            ctes->m_W_dot_pc_design = q_dot_cold_in_charge;     //[MWt]
            ctes->m_eta_pc = 1.0;                               //[-]
                // *************************************************
            ctes->m_solarm = heater_mult;                //[-]
            ctes->m_ts_hours = tshours;                  //[hr]
            ctes->m_h_tank = as_double("CT_h_tank");
            ctes->m_u_tank = as_double("CT_u_tank");
            ctes->m_tank_pairs = as_integer("CT_tank_pairs");
            // If CTES is colder than ambient, then heaters aren't going to do much?
            ctes->m_hot_tank_Thtr = -200.0;         //[C]
            ctes->m_hot_tank_max_heat = 0.0;        //[MWt]
            ctes->m_cold_tank_Thtr = -200.0;        //[C]
            ctes->m_cold_tank_max_heat = 0.0;       //[MWt]
                // so do we want these inputs from cmod?
                // do we need a tank cooler? Close enough to ambient (we expect?) to have minor heat loss?
                // ctes->m_hot_tank_Thtr = as_double("CT_hot_tank_Thtr");
                // ctes->m_hot_tank_max_heat = as_double("CT_hot_tank_max_heat");
                // ctes->m_cold_tank_Thtr = as_double("CT_cold_tank_Thtr");
                // ctes->m_cold_tank_max_heat = as_double("CT_cold_tank_max_heat");
            // ********************************************************************
            ctes->m_dt_hot = 0.0;                        // MSPT assumes direct storage, so no user input here: hardcode = 0.0
            ctes->m_T_field_in_des = T_CT_cold_TES;      //[C]
            ctes->m_T_field_out_des = T_CT_hot_TES;      //[C]
            ctes->m_T_tank_hot_ini = T_CT_hot_TES;       //[C]
            ctes->m_T_tank_cold_ini = T_CT_cold_TES;     //[C]
            ctes->m_h_tank_min = as_double("CT_h_tank_min");
            ctes->m_f_V_hot_ini = as_double("CT_tes_init_hot_htf_percent");
            ctes->m_htf_pump_coef = 0.0;            //[kW/kg/s] No htf pump losses in direct TES
            ctes->tanks_in_parallel = false;        //[-] False: Field HTF always goes to TES. PC HTF always comes from TES. ETES should not simultaneously operate heater and cycle
            ctes->V_tes_des = 1.85;                 //[m/s]
            ctes->calc_design_pipe_vals = false;    // for now, to get 'tanks_in_parallel' to work
        }

        // **********************************************************
        // **********************************************************

        // **********************************************************
        // Pricing and operation schedules
        C_csp_tou_block_schedules tou;
        C_csp_tou_block_schedules::S_params* tou_params = &tou.ms_params;

        // Still need to define mc_csp_ops blocks and fractions although we're not using them
        tou_params->mc_csp_ops.mc_weekdays.resize_fill(12, 24, 1.0);
        tou_params->mc_csp_ops.mc_weekends.resize_fill(12, 24, 1.0);
        tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC].resize(2, std::numeric_limits<double>::quiet_NaN());

        tou.mc_dispatch_params.m_is_tod_pc_target_also_pc_max = true;
        tou.mc_dispatch_params.m_is_block_dispatch = false;
        tou.mc_dispatch_params.m_is_arbitrage_policy = !as_boolean("is_dispatch");
        tou.mc_dispatch_params.m_use_rule_1 = false;
        tou.mc_dispatch_params.m_standby_off_buffer = 2.0;          //[hr] Applies if m_use_rule_1 is true
        tou.mc_dispatch_params.m_use_rule_2 = false;
        tou.mc_dispatch_params.m_q_dot_rec_des_mult = -1.23;        //[-] Applies if m_use_rule_2 is true
        tou.mc_dispatch_params.m_f_q_dot_pc_overwrite = -1.23;      //[-] Applies if m_use_rule_2 is true

        int etes_financial_model = as_integer("etes_financial_model");
        bool is_dispatch = as_boolean("is_dispatch");

        double ppa_price_year1 = std::numeric_limits<double>::quiet_NaN();
        if (sim_type == 1) {    // if sim_type = 2, skip this until ui call back is ironed out
            if (etes_financial_model > 0 && etes_financial_model < 5) { // Single Owner financial models

                // Get first year base ppa price
                size_t count_ppa_price_input;
                ssc_number_t* ppa_price_input_array = as_array("ppa_price_input", &count_ppa_price_input);
                ppa_price_year1 = (double)ppa_price_input_array[0];  // [$/kWh]

                // Time-of-Delivery factors by time step:
                int ppa_mult_model = as_integer("ppa_multiplier_model");
                if (ppa_mult_model == 1) {   // use dispatch_ts input

                    tou_params->mc_pricing.mv_is_diurnal = false;

                    if (is_assigned("dispatch_factors_ts") || is_dispatch) {
                        size_t nmultipliers;
                        ssc_number_t* multipliers = as_array("dispatch_factors_ts", &nmultipliers);
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(nmultipliers, 0.0);
                        for (size_t ii = 0; ii < nmultipliers; ii++)
                            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][ii] = multipliers[ii];
                    }
                    else { // if no dispatch optimization, don't need an input pricing schedule
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(n_steps_fixed, -1.0);
                    }
                }
                else if (ppa_mult_model == 0) { // standard diuranal input

                    tou_params->mc_pricing.mv_is_diurnal = true;

                    bool are_all_assigned = is_assigned("dispatch_sched_weekday") || is_assigned("dispatch_sched_weekend")
                        || is_assigned("dispatch_factor1") || is_assigned("dispatch_factor2") || is_assigned("dispatch_factor3")
                        || is_assigned("dispatch_factor4") || is_assigned("dispatch_factor5") || is_assigned("dispatch_factor6")
                        || is_assigned("dispatch_factor7") || is_assigned("dispatch_factor8") || is_assigned("dispatch_factor9");

                    if (are_all_assigned || is_dispatch) {
                        tou_params->mc_pricing.mc_weekdays = as_matrix("dispatch_sched_weekday");
                        if (tou_params->mc_pricing.mc_weekdays.ncells() == 1) { tou_params->mc_pricing.mc_weekdays.resize_fill(12, 24, 1.); };
                        tou_params->mc_pricing.mc_weekends = as_matrix("dispatch_sched_weekend");
                        if (tou_params->mc_pricing.mc_weekends.ncells() == 1) { tou_params->mc_pricing.mc_weekends.resize_fill(12, 24, 1.); };


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
                    else {
                        tou_params->mc_pricing.mc_weekdays.resize_fill(12, 24, 1.);
                        tou_params->mc_pricing.mc_weekends.resize_fill(12, 24, 1.);
                        tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(9, -1.0);
                    }
                }
            }
            else {
                throw exec_error("etes_electric_resistsance", "etes_financial_model must be 1, 2, 3, or 4");
            }
        }
        else if (sim_type == 2) { // if sim_type = 2, skip this until ui call back is ironed out
            tou_params->mc_pricing.mv_is_diurnal = false;

            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(n_steps_fixed, -1.0);
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
        // System dispatch
        etes_dispatch_opt dispatch;

        if (as_boolean("is_dispatch")) {
            dispatch.solver_params.set_user_inputs(as_boolean("is_dispatch"), as_integer("disp_steps_per_hour"), as_integer("disp_frequency"), as_integer("disp_horizon"),
                as_integer("disp_max_iter"), as_double("disp_mip_gap"), as_double("disp_timeout"),
                as_integer("disp_spec_presolve"), as_integer("disp_spec_bb"), as_integer("disp_spec_scaling"), as_integer("disp_reporting"),
                false, false, "", "");
            dispatch.params.set_user_params(as_double("disp_time_weighting"), as_double("disp_csu_cost")*W_dot_gen_thermo, as_double("disp_pen_delta_w"),
                as_double("disp_hsu_cost")*q_dot_hot_out_charge, as_double("disp_down_time_min"), as_double("disp_up_time_min"), ppa_price_year1);
        }
        else {
            dispatch.solver_params.dispatch_optimize = false;
        }

        // *****************************************************
        // *****************************************************

        // *****************************************************
        // Construct System Simulation
        C_csp_solver csp_solver(weather_reader,
            c_heat_pump,
            c_pc,
            c_HT_TES,
            tou,        // TODO: can we refactor tou to a dispatch struct?
            dispatch,
            system,
            NULL,
            c_CT_TES,
            ssc_cmod_update,
            (void*)(this));

        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Initialize
        update("Initialize ETES PTES model...", 0.0);

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
        // System design is complete, get design parameters from component models as necessary

            // Cycle
        double W_dot_net_gen_calc;      //[MWe]
        double q_dot_hot_in_gen_calc;   //[MWt]
        double q_dot_cold_out_thermo_gen_calc;  //[MWt]
        double W_dot_elec_parasitic_gen_calc;   //[MWe]
        double eta_net_gen_calc;        //[MWe]
        double q_dot_cold_to_CTES_calc; //[MWt]
        double q_dot_cold_to_surr_calc; //[MWt]

        double m_dot_HT_htf_gen_calc;   //[kg/s]
        double cp_HT_htf_gen_calc;      //[kJ/kg-K]
        double W_dot_HT_htf_pump_gen_calc;  //[MWe]
        double m_dot_CT_htf_gen_calc;   //[kg/s] to CT TES
        double cp_CT_htf_gen_calc;      //[kJ/kg-K]
        double W_dot_CT_htf_pump_gen_calc;   //[MWe]

        c_pc.get_design_parameters(W_dot_net_gen_calc, q_dot_hot_in_gen_calc,
                        q_dot_cold_out_thermo_gen_calc, W_dot_elec_parasitic_gen_calc,
                        eta_net_gen_calc, q_dot_cold_to_CTES_calc, q_dot_cold_to_surr_calc,
                        m_dot_HT_htf_gen_calc, cp_HT_htf_gen_calc, W_dot_HT_htf_pump_gen_calc,
                        m_dot_CT_htf_gen_calc, cp_CT_htf_gen_calc, W_dot_CT_htf_pump_gen_calc);

            // Heat Pump
        double W_dot_in_thermo_charge_calc;        //[MWe] power into cycle working fluid. does not consider electric parasitics (e.g. cooling fan, motor inefficiencies, etc.)
        double q_dot_cold_in_charge_calc;   //[MWt]
        double W_dot_elec_parasitic_charge_calc;    //[MWe]
        double W_dot_in_net_charge_calc;    //[MWe]
        double COP_net_calc;                //[-]

        // cp should be same as cycle?
        // but mass flow rates should be a bit different
        //  ... hot should scale w/ heater_mult and cold should also include the rte effect
        double m_dot_HT_htf_charge_calc;    //[kg/s]
        double cp_HT_htf_charge_calc;       //[kJ/kg-K]
        double W_dot_HT_htf_pump_charge_calc;   //[MWe]
        double m_dot_CT_htf_charge_calc;    //[kg/s]
        double cp_CT_htf_charge_calc;       //[kJ/kg-K]
        double W_dot_CT_htf_pump_charge_calc;   //[MWe]

        double E_su_charge_calc;            //[MWt-hr]

        c_heat_pump.get_design_parameters(W_dot_in_thermo_charge_calc, q_dot_cold_in_charge_calc,
                        W_dot_elec_parasitic_charge_calc, W_dot_in_net_charge_calc,
                        COP_net_calc,
                        m_dot_HT_htf_charge_calc, cp_HT_htf_charge_calc, W_dot_HT_htf_pump_charge_calc,
                        m_dot_CT_htf_charge_calc, cp_CT_htf_charge_calc, W_dot_CT_htf_pump_charge_calc,
                        E_su_charge_calc);

            // HT TES
        double V_tes_htf_avail_calc /*m3*/, V_tes_htf_total_calc /*m3*/,
            d_tank_calc /*m*/, q_dot_loss_tes_des_calc /*MWt*/, dens_store_htf_at_T_ave_calc /*kg/m3*/,
            Q_tes_des_calc /*MWt-hr*/;

        c_HT_TES.get_design_parameters(V_tes_htf_avail_calc, V_tes_htf_total_calc,
            d_tank_calc, q_dot_loss_tes_des_calc, dens_store_htf_at_T_ave_calc, Q_tes_des_calc);

            // CT TES
        double CT_V_tes_htf_avail_calc /*m3*/, CT_V_tes_htf_total_calc /*m3*/,
            CT_d_tank_calc /*m*/, CT_q_dot_loss_tes_des_calc /*MWt*/, CT_dens_store_htf_at_T_ave_calc /*kg/m3*/,
            CT_Q_tes_des_calc /*MWt-hr*/;

        c_CT_TES->get_design_parameters(CT_V_tes_htf_avail_calc, CT_V_tes_htf_total_calc,
            CT_d_tank_calc, CT_q_dot_loss_tes_des_calc, CT_dens_store_htf_at_T_ave_calc,
            CT_Q_tes_des_calc);

            // System
        double W_dot_bop_design, W_dot_fixed_parasitic_design;    //[MWe]
        csp_solver.get_design_parameters(W_dot_bop_design, W_dot_fixed_parasitic_design);

            // Calculate net system *generation* capacity including HTF pumps and system parasitics
            //     use 'gen_thermo' instead of 'gen_net' so we can see all parasitics together
        double plant_net_capacity = W_dot_gen_thermo - W_dot_gen_elec_parasitic -
                                W_dot_HT_htf_pump_gen_calc - W_dot_CT_htf_pump_gen_calc -
                                W_dot_bop_design - W_dot_fixed_parasitic_design;    //[MWe]
        double system_capacity = plant_net_capacity*1.E3;         //[kWe], convert from MWe

            // Calculate net system *charging* metrics
            // *** Currently model does NOT calculate BOP during charging
            //       should consider adding this for PTES ***
        double plant_charging_power_in = W_dot_in_thermo_charge_calc + W_dot_elec_parasitic_charge_calc +
                                W_dot_HT_htf_pump_charge_calc + W_dot_CT_htf_pump_charge_calc +
                                W_dot_fixed_parasitic_design;   //[MWe]

            // Calculate net RTE
        double RTE_net = plant_net_capacity / (plant_charging_power_in / heater_mult);      //[-]

        // *****************************************************
        // System design is complete, so calculate final design outputs like cost, capacity, etc.
        double HT_tes_spec_cost = as_double("tes_spec_cost");           //[$/kWh]
        double CT_tes_spec_cost = as_double("CT_tes_spec_cost");        //[$/kWh]
        double power_cycle_spec_cost = as_double("cycle_spec_cost");    //[$/kWe]
        double heat_pump_spec_cost = as_double("heat_pump_spec_cost");  //[$/kWt]
        double bop_spec_cost = as_double("bop_spec_cost");              //[$/kWe]
        double contingency_rate = as_double("contingency_rate");        //[%]

        double EPC_perc_direct_cost = as_double("epc_cost_perc_of_direct");
        double EPC_per_power_cost = as_double("epc_cost_per_watt");
        double EPC_fixed_cost = as_double("epc_cost_fixed");
        double total_land_perc_direct_cost = as_double("land_cost_perc_of_direct");
        double total_land_per_power_cost = as_double("land_cost_per_watt");
        double total_land_fixed_cost = as_double("land_cost_fixed");
        double sales_tax_basis = as_double("sales_tax_frac");
        double sales_tax_rate = as_double("sales_tax_rate");

        // Cost model outputs
        double tes_cost, CT_tes_cost, power_cycle_cost, heater_cost, bop_cost, fossil_backup_cost,
            direct_capital_precontingency_cost, contingency_cost, total_direct_cost, total_land_cost,
            epc_and_owner_cost, sales_tax_cost, total_indirect_cost, total_installed_cost, estimated_installed_cost_per_cap;
        tes_cost = CT_tes_cost = power_cycle_cost = heater_cost = bop_cost = fossil_backup_cost =
            direct_capital_precontingency_cost = contingency_cost = total_direct_cost = total_land_cost =
            epc_and_owner_cost = sales_tax_cost = total_indirect_cost = total_installed_cost = estimated_installed_cost_per_cap = std::numeric_limits<double>::quiet_NaN();

        N_mspt::calculate_etes_costs(Q_tes_des_calc, HT_tes_spec_cost, CT_Q_tes_des_calc, CT_tes_spec_cost,
            W_dot_in_charge_thermo, power_cycle_spec_cost,
            q_dot_hot_out_charge, heat_pump_spec_cost, bop_spec_cost, contingency_rate,
            plant_net_capacity, EPC_perc_direct_cost, EPC_per_power_cost, EPC_fixed_cost,
            total_land_perc_direct_cost, total_land_per_power_cost, total_land_fixed_cost,
            sales_tax_basis, sales_tax_rate,
            tes_cost, CT_tes_cost, power_cycle_cost, heater_cost, bop_cost, direct_capital_precontingency_cost,
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

        // Assign cmod variables that are required by downstream models or represent design-point
            // System
        assign("system_capacity", (ssc_number_t)system_capacity);           //[kWe] Discharge capacity
        assign("nameplate", (ssc_number_t)plant_net_capacity);              //[MWe] Discharge capacity
        assign("rte_thermo", (ssc_number_t)RTE_therm);                      //[-] Round-trip efficiency of working fluid cycles
        assign("rte_net", (ssc_number_t)RTE_net);                           //[-] Round-trip efficiency considering all parasitics
        assign("charge_capacity", (ssc_number_t)plant_charging_power_in);   //[MWe] Total electricity consumption at design-point charge

            // Heater
        assign("W_dot_in_thermo_charge_des", W_dot_in_thermo_charge_calc);  //[MWe]
        assign("q_dot_hot_out_charge_des", q_dot_hot_in_gen_calc);          //[MWt]
        assign("q_dot_cold_in_charge_des", q_dot_cold_in_charge_calc);      //[MWt]
        assign("W_dot_elec_parasitic_charge_des", W_dot_elec_parasitic_charge_calc);    //[MWe]
        assign("W_dot_in_charge_net_des", W_dot_in_net_charge_calc);        //[MWe]
        assign("COP_net_des", COP_net_calc);                                //[-]
        assign("m_dot_HT_htf_charge_des", m_dot_HT_htf_charge_calc);        //[kg/s]
        assign("W_dot_HT_htf_pump_charge_des", W_dot_HT_htf_pump_charge_calc);     //[MWe]
        assign("m_dot_CT_htf_charge_des", m_dot_CT_htf_charge_calc);                //[kg/s]
        assign("W_dot_CT_htf_pump_charge_des", W_dot_CT_htf_pump_charge_calc);     //[MWe]
        assign("E_heater_su_des", E_su_charge_calc);                        //[MWt-hr]

            // Cycle
        assign("q_dot_hot_in_gen_des", q_dot_hot_in_gen_calc);                      //[MWt]
        assign("q_dot_cold_out_thermo_gen_des", q_dot_cold_out_thermo_gen_calc);    //[MWt]
        assign("W_dot_elec_parasitic_gen_des", W_dot_elec_parasitic_gen_calc);      //[MWe]
        assign("W_dot_gen_net_des", W_dot_net_gen_calc);                            //[MWe]
        assign("eta_net_gen_des", eta_net_gen_calc);                                //[-]
        assign("q_dot_cold_to_CTES_gen_des", q_dot_cold_to_CTES_calc);              //[MWt]
        assign("q_dot_cold_to_surroundings_gen_des", q_dot_cold_to_surr_calc);      //[MWt]
        assign("m_dot_HT_htf_gen_des", m_dot_HT_htf_gen_calc);                      //[kg/s]
        assign("W_dot_HT_htf_pump_gen_des", W_dot_HT_htf_pump_gen_calc);            //[MWe]
        assign("m_dot_CT_htf_gen_des", m_dot_CT_htf_gen_calc);                      //[kg/s]
        assign("W_dot_CT_htf_pump_gen_des", W_dot_CT_htf_pump_gen_calc);            //[MWe]

            // Hot TES
        assign("Q_tes_des", Q_tes_des_calc);                //[MWt-hr]
        assign("V_tes_htf_avail", V_tes_htf_avail_calc);    //[m3]
        assign("V_tes_htf_total", V_tes_htf_total_calc);    //[m3]
        assign("d_tank_tes", d_tank_calc);                  //[m]
        assign("q_dot_loss_tes_des", q_dot_loss_tes_des_calc);  //[MWt]

            // Cold TES
        assign("Q_CT_tes_des", CT_Q_tes_des_calc);              //[MWt-hr]
        assign("V_CT_tes_htf_avail", CT_V_tes_htf_avail_calc);  //[m3]
        assign("V_CT_tes_htf_total", CT_V_tes_htf_total_calc);  //[m3]
        assign("d_CT_tank_tes", CT_d_tank_calc);                //[m]
        assign("q_dot_loss_CT_tes_des", CT_q_dot_loss_tes_des_calc);    //[MWt]

            // Balance of Plant
        assign("W_dot_bop_design", W_dot_bop_design);           //[MWe]
        assign("W_dot_fixed", W_dot_fixed_parasitic_design);    //[MWe]

            // Costs
        assign("heater_cost_calc", heater_cost);        //[$]
        assign("tes_cost_calc", tes_cost);              //[$]
        assign("CT_tes_cost_calc", CT_tes_cost);        //[$]
        assign("bop_cost_calc", bop_cost);              //[$]
        assign("cycle_cost_calc", power_cycle_cost);    //[$]
        assign("direct_subtotal_cost_calc", direct_capital_precontingency_cost);    //[$]
        assign("contingency_cost_calc", contingency_cost);  //[$]
        assign("total_direct_cost_calc", total_direct_cost);//[$]
        assign("epc_cost_calc", epc_and_owner_cost);        //[$]
        assign("land_cost_calc", total_land_cost);          //[$]
        assign("sales_tax_cost_calc", sales_tax_cost);      //[$]
        assign("total_indirect_cost_calc", total_indirect_cost);    //[$]
        assign("installed_per_cap_cost_calc", (ssc_number_t)(total_installed_cost / system_capacity));  //[$/kWe]
        assign("total_installed_cost", (ssc_number_t)total_installed_cost);                 //[$]
        assign("construction_financing_cost", (ssc_number_t)construction_financing_cost);   //[$]

            // Financial
        assign("ppa_soln_mode", 1);         // Only allow dispatch model to use fixed ppa mode so dispatch model knows absolute prices
        assign("flip_target_percent", 0.0); //[%] fixed ppa mode shouldn't use this input, so set it to a value that, if used, will give weird results

        // *****************************************************
        // *****************************************************

        // *****************************************************
        // If calling cmod to run design only, return here
        if (as_double("sim_type") != 1) {
            return;
        }
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Run timeseries simulation
        update("Begin timeseries simulation...", 0.0);


        return;
    }
};

DEFINE_MODULE_ENTRY(etes_ptes, "Heat pump charging two two-tank TES from grid, discharge with power cycle", 1)