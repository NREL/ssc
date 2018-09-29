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

#include <math.h>

#include "common.h"
#include "core.h"
#include "lib_util.h"
#include "cmod_battery.h"

var_info vtab_battery_inputs[] = {
	/*   VARTYPE           DATATYPE         NAME                                            LABEL                                                   UNITS      META                   GROUP           REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	// simulation inputs - required only if lifetime analysis
	{ SSC_INPUT,        SSC_NUMBER,      "system_use_lifetime_output",                 "PV lifetime simulation",                                  "0/1",     "",                     "",             "?=0",                        "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,      "analysis_period",                            "Lifetime analysis period",                                "years",   "",                     "",             "system_use_lifetime_output=1",   "",                               "" },

	// configuration inputs
	{ SSC_INPUT,        SSC_NUMBER,      "batt_chem",                                  "Battery chemistry",                                       "",        "0=LeadAcid,1=LiIon",   "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inverter_model",                             "Inverter model specifier",                                "",        "0=cec,1=datasheet,2=partload,3=coefficientgenerator,4=generic","","", "INTEGER,MIN=0,MAX=4",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inverter_count",                             "Number of inverters",                                     "",        "",                     "pvsamv1"        "",                            "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_eff_cec",                            "Inverter Sandia CEC Efficiency",                          "%",       "",                     "pvsamv1",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_paco",                               "Inverter Sandia Maximum AC Power",                        "Wac",     "",                     "pvsamv1",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_ds_eff",                                 "Inverter Datasheet Efficiency",                           "%",       "",                     "pvsamv1",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_ds_paco",                                "Inverter Datasheet Maximum AC Power",                     "Wac",     "",                     "pvsamv1",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_pd_eff",                                 "Inverter Partload Efficiency",                            "%",       "",                     "pvsamv1",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_pd_paco",                                "Inverter Partload Maximum AC Power",                      "Wac",     "",                     "pvsamv1",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_cec_cg_eff_cec",                         "Inverter Coefficient Generator CEC Efficiency",           "%",       "",                     "pvsamv1",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_cec_cg_paco",                            "Inverter Coefficient Generator Max AC Power",             "Wac",       "",                   "pvsamv1",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_ac_or_dc",                              "Battery interconnection (AC or DC)",                      "dc=0,ac=1",  "",                  "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_dc_dc_efficiency",                      "PV DC to battery DC efficiency",                          "",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dcoptimizer_loss",                           "PV loss in DC/DC w/MPPT conversion",                      "",        "",                     "pvsamv1",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_dc_ac_efficiency",                      "Battery DC to AC efficiency",                             "",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_ac_dc_efficiency",                      "Inverter AC to battery DC efficiency",                    "",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_meter_position",                        "Position of battery relative to electric meter",          "",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "batt_losses",                                "Battery system losses at each timestep",                  "kW",       "",                     "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,       "batt_losses_charging",                       "Battery system losses when charging",                     "kW",       "",                     "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,       "batt_losses_discharging",                    "Battery system losses when discharging",                  "kW",       "",                     "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,       "batt_losses_idle",                           "Battery system losses when idle",                         "kW",       "",                     "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_loss_choice",                           "Loss power input option",                                 "0/1",      "",                     "Battery",       "?=0",                        "",                             "" },

	// Current and capacity battery inputs
	{ SSC_INPUT,        SSC_NUMBER,      "batt_current_choice",                        "Limit cells by current or power",                         "",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_computed_strings",                      "Number of strings of cells",                              "",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_computed_series",                       "Number of cells in series",                               "",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_computed_bank_capacity",                "Computed bank capacity",                                  "kWh",     "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_current_charge_max",                    "Maximum charge current",                                  "A",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_current_discharge_max",                 "Maximum discharge current",                               "A",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_power_charge_max",                      "Maximum charge power",                                    "kW",       "",                    "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_power_discharge_max",                   "Maximum discharge power",                                 "kW",       "",                    "Battery",       "",                           "",                              "" },


	// Voltage discharge curve
	{ SSC_INPUT,        SSC_NUMBER,      "batt_voltage_choice",                        "Battery voltage input option",                            "0/1",      "",                    "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Vfull",                                 "Fully charged cell voltage",                              "V",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Vexp",                                  "Cell voltage at end of exponential zone",                 "V",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Vnom",                                  "Cell voltage at end of nominal zone",                     "V",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Vnom_default",                          "Default nominal cell voltage",                            "V",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Qfull",                                 "Fully charged cell capacity",                             "Ah",      "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Qfull_flow",                            "Fully charged flow battery capacity",                     "Ah",      "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Qexp",                                  "Cell capacity at end of exponential zone",                "Ah",      "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_Qnom",                                  "Cell capacity at end of nominal zone",                    "Ah",      "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_C_rate",                                "Rate at which voltage vs. capacity curve input",          "",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_resistance",                            "Internal resistance",                                     "Ohm",     "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,		SSC_MATRIX,      "batt_voltage_matrix",                        "Battery voltage vs. depth-of-discharge",                 "",         "",                     "Battery",       "",                           "",                             "" },

	// lead-acid inputs
	{ SSC_INPUT,		SSC_NUMBER,		"LeadAcid_q20_computed",	                   "Capacity at 20-hour discharge rate",                     "Ah",       "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,		SSC_NUMBER,		"LeadAcid_q10_computed",	                   "Capacity at 10-hour discharge rate",                     "Ah",       "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,		SSC_NUMBER,		"LeadAcid_qn_computed",	                       "Capacity at discharge rate for n-hour rate",             "Ah",       "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,		SSC_NUMBER,		"LeadAcid_tn",	                               "Time to discharge",                                      "h",        "",                     "Battery",       "",                           "",                             "" },

	// charge limits and priority inputs
	{ SSC_INPUT,        SSC_NUMBER,      "batt_initial_SOC",		                   "Initial state-of-charge",                                 "%",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_minimum_SOC",		                   "Minimum allowed state-of-charge",                         "%",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_maximum_SOC",                           "Maximum allowed state-of-charge",                         "%",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_minimum_modetime",                      "Minimum time at charge state",                            "min",     "",                     "Battery",       "",                           "",                              "" },

	// lifetime inputs
	{ SSC_INPUT,		SSC_MATRIX,     "batt_lifetime_matrix",                        "Cycles vs capacity at different depths-of-discharge",    "",         "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_calendar_choice",                        "Calendar life degradation input option",                 "0/1/2",    "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_MATRIX,     "batt_calendar_lifetime_matrix",               "Days vs capacity",                                       "",         "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_calendar_q0",                            "Calendar life model initial capacity cofficient",        "",         "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_calendar_a",                             "Calendar life model coefficient",                        "1/sqrt(day)","",                   "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_calendar_b",                             "Calendar life model coefficient",                        "K",        "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_calendar_c",                             "Calendar life model coefficient",                        "K",        "",                     "Battery",       "",                           "",                             "" },

	// replacement inputs
	{ SSC_INPUT,        SSC_NUMBER,     "batt_replacement_capacity",                   "Capacity degradation at which to replace battery",       "%",        "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_replacement_option",                     "Enable battery replacement?",                             "0=none,1=capacity based,2=user schedule", "", "Battery", "?=0",                  "INTEGER,MIN=0,MAX=2",          "" },
	{ SSC_INPUT,        SSC_ARRAY,      "batt_replacement_schedule",                   "Battery bank replacements per year (user specified)",     "number/year","",                  "Battery",      "batt_replacement_option=2",   "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_replacement_cost",                       "Cost to replace battery per kWh",                        "$/kWh",    "",                     "Battery",       "",                           "",                             "" },

	// thermal inputs
	{ SSC_INPUT,        SSC_NUMBER,     "batt_mass",                                   "Battery mass",                                           "kg",       "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_length",                                 "Battery length",                                         "m",        "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_width",                                  "Battery width",                                          "m",        "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_height",                                 "Battery height",                                         "m",        "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_Cp",                                     "Battery specific heat capacity",                         "J/KgK",    "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_h_to_ambient",                           "Heat transfer between battery and environment",          "W/m2K",    "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "T_room",                                      "Temperature of storage room",                            "C",        "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_MATRIX,     "cap_vs_temp",                                 "Effective capacity as function of temperature",          "C,%",      "",                     "Battery",       "",                           "",                             "" },

	// storage dispatch
	{ SSC_INPUT,        SSC_ARRAY,      "dispatch_manual_charge",                      "Periods 1-6 charging allowed?",                          "",         "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,      "dispatch_manual_discharge",                   "Periods 1-6 discharging allowed?",                       "",         "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,      "dispatch_manual_gridcharge",                  "Periods 1-6 grid charging allowed?",                     "",         "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,      "dispatch_manual_percent_discharge",           "Periods 1-6 discharge percent",                          "%",        "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,      "dispatch_manual_percent_gridcharge",          "Periods 1-6 gridcharge percent",                         "%",        "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_MATRIX,     "dispatch_manual_sched",                       "Battery dispatch schedule for weekday",                  "",         "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_MATRIX,     "dispatch_manual_sched_weekend",               "Battery dispatch schedule for weekend",                  "",         "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,      "batt_target_power",                           "Grid target power for every time step",                  "kW",       "",                     "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,      "batt_target_power_monthly",                   "Grid target power on monthly basis",                     "kW",       "",                     "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_target_choice",                          "Target power input option",                              "0/1",      "",                     "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,      "batt_custom_dispatch",                        "Custom battery power for every time step",               "kW",       "",                     "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_dispatch_choice",                        "Battery dispatch algorithm",                             "0/1/2/3/4", "",                    "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_pv_choice",                              "Prioritize PV usage for load or battery",                "0/1",      "",                     "Battery",       "?=0",                        "",                             "" },
	{ SSC_INPUT,        SSC_ARRAY,      "batt_pv_clipping_forecast",                   "PV clipping forecast",                                   "kW",       "",                     "Battery",       "en_batt=1&batt_meter_position=1&batt_dispatch_choice=2",  "",          "" },
	{ SSC_INPUT,        SSC_ARRAY,      "batt_pv_dc_forecast",                         "PV dc power forecast",                                   "kW",       "",                     "Battery",       "en_batt=1&batt_meter_position=1&batt_dispatch_choice=2",  "",          "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_dispatch_auto_can_gridcharge",           "Grid charging allowed for automated dispatch?",          "kW",       "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_dispatch_auto_can_charge",               "PV charging allowed for automated dispatch?",            "kW",       "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_dispatch_auto_can_clipcharge",           "Battery can charge from clipped PV for automated dispatch?", "kW",   "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_auto_gridcharge_max_daily",              "Allowed grid charging percent per day for automated dispatch","kW",  "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_look_ahead_hours",                       "Hours to look ahead in automated dispatch",              "hours",    "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_dispatch_update_frequency_hours",        "Frequency to update the look-ahead dispatch",            "hours",    "",                     "Battery",       "",                           "",                             "" },

	//  cycle cost inputs
	{ SSC_INPUT,        SSC_NUMBER,     "batt_cycle_cost_choice",                      "Use SAM model for cycle costs or input custom",           "0/1",     "",                     "Battery",       "",                           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "batt_cycle_cost",                             "Input battery cycle costs",                               "$/cycle-kWh","",                  "Battery",       "",                           "",                             "" },

	// Utility rate inputs
	{ SSC_INPUT,        SSC_MATRIX,     "ur_ec_sched_weekday",                         "Energy charge weekday schedule",                          "",        "12 x 24 matrix",         "",              "en_batt=1&batt_meter_position=1&batt_dispatch_choice=2",  "",          "" },
	{ SSC_INPUT,        SSC_MATRIX,     "ur_ec_sched_weekend",                         "Energy charge weekend schedule",                          "",        "12 x 24 matrix",         "",              "en_batt=1&batt_meter_position=1&batt_dispatch_choice=2",  "",          "" },
	{ SSC_INPUT,        SSC_MATRIX,     "ur_ec_tou_mat",                               "Energy rates table",                                      "",        "",                       "",              "en_batt=1&batt_meter_position=1&batt_dispatch_choice=2",  "",          "" },

	// PPA financial inputs
	{ SSC_INPUT,        SSC_NUMBER,     "ppa_price_input",		                        "PPA Price Input",	                                        "",      "",                  "Time of Delivery", "en_batt=1&batt_meter_position=1&batt_dispatch_choice=2"   "",          "" },
	{ SSC_INPUT,        SSC_ARRAY,      "dispatch_tod_factors",		                    "TOD factors for periods 1-9",	                            "",      "",                  "Time of Delivery", "en_batt=1&batt_meter_position=1&batt_dispatch_choice=2"   "",          "" },
	{ SSC_INPUT,        SSC_MATRIX,     "dispatch_sched_weekday",                       "Diurnal weekday TOD periods",                              "1..9",  "12 x 24 matrix",    "Time of Delivery", "en_batt=1&batt_meter_position=1&batt_dispatch_choice=2",  "",          "" },
	{ SSC_INPUT,        SSC_MATRIX,     "dispatch_sched_weekend",                       "Diurnal weekend TOD periods",                              "1..9",  "12 x 24 matrix",    "Time of Delivery", "en_batt=1&batt_meter_position=1&batt_dispatch_choice=2",  "",          "" },

	var_info_invalid
};

var_info vtab_battery_outputs[] = {
	// Capacity, Voltage, Charge outputs
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_q0",                                    "Battery total charge",                                   "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_q1",                                    "Battery available charge",                               "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_q2",                                    "Battery bound charge",                                   "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_SOC",                                   "Battery state of charge",                                "%",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_DOD",                                   "Battery cycle depth of discharge",                       "%",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_qmaxI",                                 "Battery maximum capacity at current",                    "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_qmax",                                  "Battery maximum charge with degradation",                "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_qmax_thermal",                          "Battery maximum charge at temperature",                  "Ah",       "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_I",                                     "Battery current",                                        "A",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_voltage_cell",                          "Battery cell voltage",                                   "V",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_voltage",                               "Battery voltage",	                                     "V",        "",                     "Battery",       "",                           "",                              "" },
																		               
	// Lifecycle related outputs											             
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_cycles",                                "Battery number of cycles",                               "",         "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_temperature",                           "Battery temperature",                                    "C",        "",                     "Battery",       "",                           "",                              "" }, 
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_capacity_percent",                      "Battery capacity percent for lifetime",                  "%",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_capacity_thermal_percent",              "Battery capacity percent for temperature",               "%",        "",                     "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_bank_replacement",                      "Battery bank replacements per year",                     "number/year", "",                  "Battery",       "",                           "",                              "" },
																			          
	// Power outputs at native timestep												        
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_power",                                 "Electricity to/from battery",                           "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "grid_power",                                 "Electricity to/from grid",                              "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "pv_to_load",                                 "Electricity to load from PV",                           "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_to_load",                               "Electricity to load from battery",                      "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "grid_to_load",                               "Electricity to load from grid",                         "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "pv_to_batt",                                 "Electricity to battery from PV",                        "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "grid_to_batt",                               "Electricity to battery from grid",                      "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "pv_to_grid",                                 "Electricity to grid from PV",                           "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_to_grid",                               "Electricity to grid from battery",                      "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_conversion_loss",                       "Electricity loss in battery power electronics",         "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_system_loss",                           "Electricity loss from battery ancillary equipment",     "kW",      "",                       "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "grid_power_target",                          "Electricity grid power target for automated dispatch","kW","",                               "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_power_target",                          "Electricity battery power target for automated dispatch","kW","",                            "Battery",       "",                           "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_cost_to_cycle",                         "Computed cost to cycle",                                "$/cycle", "",                       "Battery",       "",                           "",                              "" },


	// monthly outputs
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_pv_to_load",                         "Energy to load from PV",                                "kWh",      "",                      "Battery",       "",                          "LENGTH=12",                     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_batt_to_load",                       "Energy to load from battery",                           "kWh",      "",                      "Battery",       "",                          "LENGTH=12",                     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_grid_to_load",                       "Energy to load from grid",                              "kWh",      "",                      "Battery",       "",                          "LENGTH=12",                     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_pv_to_grid",                         "Energy to grid from PV",                                "kWh",      "",                      "Battery",       "",                          "LENGTH=12",                     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_batt_to_grid",                       "Energy to grid from battery",                           "kWh",      "",                      "Battery",       "",                          "LENGTH=12",                     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_pv_to_batt",                         "Energy to battery from PV",                             "kWh",      "",                      "Battery",       "",                          "LENGTH=12",                     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "monthly_grid_to_batt",                       "Energy to battery from grid",                           "kWh",      "",                      "Battery",       "",                          "LENGTH=12",                     "" },

	// annual metrics													          
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_annual_charge_from_pv",                 "Battery annual energy charged from PV",                 "kWh",      "",                      "Battery",       "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_annual_charge_from_grid",               "Battery annual energy charged from grid",               "kWh",      "",                      "Battery",       "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_annual_charge_energy",                  "Battery annual energy charged",                         "kWh",      "",                      "Battery",       "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_annual_discharge_energy",               "Battery annual energy discharged",                      "kWh",      "",                      "Battery",       "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_annual_energy_loss",                    "Battery annual energy loss",                            "kWh",      "",                      "Battery",       "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "batt_annual_energy_system_loss",             "Battery annual system energy loss",                     "kWh",      "",                      "Battery",       "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "annual_export_to_grid_energy",               "Annual energy exported to grid",                        "kWh",      "",                      "Battery",       "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "annual_import_to_grid_energy",               "Annual energy imported from grid",                      "kWh",      "",                      "Battery",       "",                           "",                               "" },
	
	// single value metrics
	{ SSC_OUTPUT,        SSC_NUMBER,     "average_battery_conversion_efficiency",      "Battery average cycle conversion efficiency",           "%",        "",                      "Annual",        "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "average_battery_roundtrip_efficiency",       "Battery average roundtrip efficiency",                  "%",        "",                      "Annual",        "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "batt_pv_charge_percent",                     "Battery percent energy charged from PV",                "%",        "",                      "Annual",        "",                           "",                               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "batt_bank_installed_capacity",               "Battery bank installed capacity",                       "kWh",      "",                      "Annual",        "",                           "",                               "" },

	// test matrix output
	{ SSC_OUTPUT,        SSC_MATRIX,     "batt_dispatch_sched",                        "Battery dispatch schedule",                              "",        "",                     "Battery",       "",                           "",                               "ROW_LABEL=MONTHS,COL_LABEL=HOURS_OF_DAY"  },
	

var_info_invalid };

battstor::battstor(compute_module &cm, bool setup_model, size_t nrec, double dt_hr, batt_variables *batt_vars_in)
{
	make_vars = false;

	// time quantities
	nyears = 1;
	_dt_hour = dt_hr;
	step_per_hour = static_cast<size_t>(1. / _dt_hour);
	initialize_time(0, 0, 0);

	// battery variables
	if (batt_vars_in == 0)
	{
		make_vars = true;
		batt_vars = new batt_variables();
		batt_vars->en_batt = cm.as_boolean("en_batt");
		if (batt_vars->en_batt)
		{
			// Financial Parameters
			batt_vars->analysis_period = cm.as_integer("analysis_period");

			// Lifetime simulation
			batt_vars->system_use_lifetime_output = cm.as_boolean("system_use_lifetime_output");

			if (batt_vars->system_use_lifetime_output) {
				nyears = batt_vars->analysis_period;
			}

			// Chemistry
			batt_vars->batt_chem = cm.as_integer("batt_chem");

			// Lead acid settings
			if (batt_vars->batt_chem == battery_t::LEAD_ACID)
			{
				batt_vars->LeadAcid_q10_computed = cm.as_double("LeadAcid_q10_computed");
				batt_vars->LeadAcid_q20_computed = cm.as_double("LeadAcid_q20_computed");
				batt_vars->LeadAcid_qn_computed = cm.as_double("LeadAcid_qn_computed");
				batt_vars->LeadAcid_tn = cm.as_double("LeadAcid_tn");
			}

			// Battery bank sizing
			batt_vars->batt_computed_series = cm.as_integer("batt_computed_series");
			batt_vars->batt_computed_strings = cm.as_integer("batt_computed_strings");
			batt_vars->batt_kwh = cm.as_double("batt_computed_bank_capacity");

			// Voltage properties
			batt_vars->batt_voltage_choice = cm.as_integer("batt_voltage_choice");
			batt_vars->batt_Vnom_default = cm.as_double("batt_Vnom_default");
			batt_vars->batt_Vfull = cm.as_double("batt_Vfull");
			batt_vars->batt_Vexp = cm.as_double("batt_Vexp");
			batt_vars->batt_Vnom = cm.as_double("batt_Vnom");
			batt_vars->batt_Qfull_flow = cm.as_double("batt_Qfull_flow");
			batt_vars->batt_Qfull = cm.as_double("batt_Qfull");
			batt_vars->batt_Qexp = cm.as_double("batt_Qexp");
			batt_vars->batt_Qnom = cm.as_double("batt_Qnom");
			batt_vars->batt_C_rate = cm.as_double("batt_C_rate");
			batt_vars->batt_resistance = cm.as_double("batt_resistance");

			// Current and capacity
			batt_vars->batt_current_choice = cm.as_integer("batt_current_choice");
			batt_vars->batt_current_charge_max = cm.as_double("batt_current_charge_max");
			batt_vars->batt_current_discharge_max = cm.as_double("batt_current_discharge_max");
			batt_vars->batt_power_charge_max = cm.as_double("batt_power_charge_max");
			batt_vars->batt_power_discharge_max = cm.as_double("batt_power_discharge_max");

			// Power converters and topology
			batt_vars->batt_topology = cm.as_integer("batt_ac_or_dc");
			batt_vars->batt_ac_dc_efficiency = cm.as_double("batt_ac_dc_efficiency");
			batt_vars->batt_dc_ac_efficiency = cm.as_double("batt_dc_ac_efficiency");
			batt_vars->batt_dc_dc_bms_efficiency = cm.as_double("batt_dc_dc_efficiency");

			if (cm.is_assigned("dcoptimizer_loss")) {
				batt_vars->pv_dc_dc_mppt_efficiency = 100. - cm.as_double("dcoptimizer_loss");
			}
			else {
				batt_vars->pv_dc_dc_mppt_efficiency = 100;
			}

			// Ancillary equipment losses
			batt_vars->batt_loss_choice = cm.as_integer("batt_loss_choice");
			batt_vars->batt_losses_charging = cm.as_vector_double("batt_losses_charging");
			batt_vars->batt_losses_discharging = cm.as_vector_double("batt_losses_discharging");
			batt_vars->batt_losses_idle = cm.as_vector_double("batt_losses_idle");
			batt_vars->batt_losses = cm.as_vector_double("batt_losses");

			// Charge limits and priority
			batt_vars->batt_initial_SOC = cm.as_double("batt_initial_SOC");
			batt_vars->batt_maximum_SOC = cm.as_double("batt_maximum_soc");
			batt_vars->batt_minimum_SOC = cm.as_double("batt_minimum_soc");
			batt_vars->batt_minimum_modetime = cm.as_double("batt_minimum_modetime");

			// Storage dispatch controllers
			batt_vars->batt_dispatch = cm.as_integer("batt_dispatch_choice");
			batt_vars->batt_meter_position = cm.as_integer("batt_meter_position");

			// Front of meter
			if (batt_vars->batt_meter_position == dispatch_t::FRONT)
			{
				batt_vars->pv_clipping_forecast = cm.as_vector_double("batt_pv_clipping_forecast");
				batt_vars->pv_dc_power_forecast = cm.as_vector_double("batt_pv_dc_forecast");
				double ppa_price = cm.as_double("ppa_price_input");
				batt_vars->ppa_factors = cm.as_vector_double("dispatch_tod_factors");
				for (size_t i = 0; i != batt_vars->ppa_factors.size(); i++)
					batt_vars->ppa_factors[i] *= ppa_price;
				batt_vars->ppa_weekday_schedule = cm.as_matrix_unsigned_long("dispatch_sched_weekday");
				batt_vars->ppa_weekend_schedule = cm.as_matrix_unsigned_long("dispatch_sched_weekend");

				// For automated front of meter with electricity rates
				batt_vars->ec_rate_defined = false;
				if (cm.is_assigned("ur_ec_tou_mat"))
				{
					batt_vars->ec_weekday_schedule = cm.as_matrix_unsigned_long("ur_ec_sched_weekday");
					batt_vars->ec_weekend_schedule = cm.as_matrix_unsigned_long("ur_ec_sched_weekend");
					batt_vars->ec_tou_matrix = cm.as_matrix("ur_ec_tou_mat");
					batt_vars->ec_rate_defined = true;
				}
			
	
				batt_vars->batt_dispatch_auto_can_charge = cm.as_boolean("batt_dispatch_auto_can_charge");
				batt_vars->batt_dispatch_auto_can_clipcharge = cm.as_boolean("batt_dispatch_auto_can_clipcharge");
				batt_vars->batt_dispatch_auto_can_gridcharge = cm.as_boolean("batt_dispatch_auto_can_gridcharge");

				batt_vars->batt_cycle_cost_choice = cm.as_integer("batt_cycle_cost_choice");
				batt_vars->batt_cycle_cost = cm.as_double("batt_cycle_cost");

				if (batt_vars->batt_dispatch == dispatch_t::FOM_LOOK_AHEAD || 
					batt_vars->batt_dispatch == dispatch_t::FOM_FORECAST || 
					batt_vars->batt_dispatch == dispatch_t::FOM_LOOK_BEHIND)
				{
					batt_vars->batt_look_ahead_hours = cm.as_unsigned_long("batt_look_ahead_hours");
					batt_vars->batt_dispatch_update_frequency_hours = cm.as_double("batt_dispatch_update_frequency_hours");
				}
				else if (batt_vars->batt_dispatch == dispatch_t::FOM_CUSTOM_DISPATCH)
				{
					batt_vars->batt_custom_dispatch = cm.as_vector_double("batt_custom_dispatch");
				}
			}
			// Automated behind-the-meter
			else
			{
				if (batt_vars->batt_dispatch == dispatch_t::MAINTAIN_TARGET)
				{
					batt_vars->batt_target_choice = cm.as_integer("batt_target_choice");
					batt_vars->target_power_monthly = cm.as_vector_double("batt_target_power_monthly");
					batt_vars->target_power = cm.as_vector_double("batt_target_power");

					if (batt_vars->batt_target_choice == dispatch_automatic_behind_the_meter_t::TARGET_SINGLE_MONTHLY)
					{
						target_power_monthly = batt_vars->target_power_monthly;
						target_power.clear();
						target_power.reserve(8760 * step_per_hour);
						for (size_t month = 0; month != 12; month++)
						{
							double target = target_power_monthly[month];
							for (size_t h = 0; h != util::hours_in_month(month + 1); h++)
							{
									for (size_t s = 0; s != step_per_hour; s++)
										target_power.push_back(target);
							}
						}
						
					}
					else
						target_power = batt_vars->target_power;

					if (target_power.size() != nrec)
						throw compute_module::exec_error("battery", "invalid number of target powers, must be equal to number of records in weather file");

					// extend target power to lifetime internally
					for (size_t y = 1; y < nyears; y++){
						for (size_t i = 0; i < nrec; i++) {
							target_power.push_back(target_power[i]);
						}
					}
					batt_vars->target_power = target_power;
					
				}
				else if (batt_vars->batt_dispatch == dispatch_t::CUSTOM_DISPATCH)
				{
					batt_vars->batt_custom_dispatch = cm.as_vector_double("batt_custom_dispatch");
				}

				batt_vars->batt_dispatch_auto_can_gridcharge = cm.as_boolean("batt_dispatch_auto_can_gridcharge");
				batt_vars->batt_dispatch_auto_can_charge = cm.as_boolean("batt_dispatch_auto_can_charge");
			}

			// Manual dispatch
			if ((batt_vars->batt_meter_position == dispatch_t::FRONT && batt_vars->batt_dispatch == dispatch_t::FOM_MANUAL ) ||
				(batt_vars->batt_meter_position == dispatch_t::BEHIND && batt_vars->batt_dispatch == dispatch_t::MANUAL))
			{
				batt_vars->batt_can_charge = cm.as_vector_bool("dispatch_manual_charge");
				batt_vars->batt_can_discharge = cm.as_vector_bool("dispatch_manual_discharge");
				batt_vars->batt_can_gridcharge = cm.as_vector_bool("dispatch_manual_gridcharge");
				batt_vars->batt_discharge_percent = cm.as_vector_double("dispatch_manual_percent_discharge");
				batt_vars->batt_gridcharge_percent = cm.as_vector_double("dispatch_manual_percent_gridcharge");
				batt_vars->batt_discharge_schedule_weekday = cm.as_matrix_unsigned_long("dispatch_manual_sched");
				batt_vars->batt_discharge_schedule_weekend = cm.as_matrix_unsigned_long("dispatch_manual_sched_weekend");
			}

			// Common to automated methods
			batt_vars->batt_dispatch_auto_can_gridcharge = cm.as_boolean("batt_dispatch_auto_can_gridcharge");
			batt_vars->batt_dispatch_auto_can_charge = true;
			batt_vars->batt_dispatch_auto_can_clipcharge = true;

			if (cm.is_assigned("batt_dispatch_auto_can_charge")) {
				batt_vars->batt_dispatch_auto_can_charge = cm.as_boolean("batt_dispatch_auto_can_charge");
			}
			if (cm.is_assigned("batt_dispatch_auto_can_clipcharge")) {
				batt_vars->batt_dispatch_auto_can_clipcharge = cm.as_boolean("batt_dispatch_auto_can_clipcharge");
			}

			// Battery bank replacement
			batt_vars->batt_cost_per_kwh = cm.as_double("batt_replacement_cost");
			batt_vars->batt_replacement_option = cm.as_integer("batt_replacement_option");
			batt_vars->batt_replacement_capacity = cm.as_double("batt_replacement_capacity");
			
			if (batt_vars->batt_replacement_option == battery_t::REPLACE_BY_SCHEDULE)
				batt_vars->batt_replacement_schedule = cm.as_vector_integer("batt_replacement_schedule");

			// Battery lifetime
			batt_vars->batt_calendar_choice = cm.as_integer("batt_calendar_choice");
			batt_vars->batt_lifetime_matrix = cm.as_matrix("batt_lifetime_matrix");
			batt_vars->batt_calendar_lifetime_matrix = cm.as_matrix("batt_calendar_lifetime_matrix");
			batt_vars->batt_voltage_matrix = cm.as_matrix("batt_voltage_matrix");
			batt_vars->batt_calendar_q0 = cm.as_double("batt_calendar_q0");
			batt_vars->batt_calendar_a = cm.as_double("batt_calendar_a");
			batt_vars->batt_calendar_b = cm.as_double("batt_calendar_b");
			batt_vars->batt_calendar_c = cm.as_double("batt_calendar_c");

			// Thermal behavior
			batt_vars->cap_vs_temp = cm.as_matrix("cap_vs_temp");
			batt_vars->batt_mass = cm.as_double("batt_mass");
			batt_vars->batt_length = cm.as_double("batt_length");
			batt_vars->batt_width = cm.as_double("batt_width");
			batt_vars->batt_height = cm.as_double("batt_height");
			batt_vars->batt_Cp = cm.as_double("batt_Cp");
			batt_vars->batt_h_to_ambient = cm.as_double("batt_h_to_ambient");
			batt_vars->T_room = cm.as_double("T_room");

			// Inverter settings
			if (cm.is_assigned("inverter_model"))
			{
				batt_vars->inverter_model = cm.as_integer("inverter_model");
				batt_vars->inverter_count = cm.as_integer("inverter_count");

				if (batt_vars->inverter_model == SharedInverter::SANDIA_INVERTER)
				{
					batt_vars->inverter_efficiency = cm.as_double("inv_snl_eff_cec");
					batt_vars->inverter_paco = batt_vars->inverter_count * cm.as_double("inv_snl_paco") * util::watt_to_kilowatt;
				}
				else if (batt_vars->inverter_model == SharedInverter::DATASHEET_INVERTER)
				{
					batt_vars->inverter_efficiency = cm.as_double("inv_ds_eff");
					batt_vars->inverter_paco = batt_vars->inverter_count * cm.as_double("inv_ds_paco") * util::watt_to_kilowatt;

				}
				else if (batt_vars->inverter_model == SharedInverter::PARTLOAD_INVERTER)
				{
					batt_vars->inverter_efficiency = cm.as_double("inv_pd_eff");
					batt_vars->inverter_paco = batt_vars->inverter_count * cm.as_double("inv_pd_paco") * util::watt_to_kilowatt;
				}
				else if (batt_vars->inverter_model == SharedInverter::COEFFICIENT_GENERATOR)
				{
					batt_vars->inverter_efficiency = cm.as_double("inv_cec_cg_eff_cec");
					batt_vars->inverter_paco = batt_vars->inverter_count * cm.as_double("inv_cec_cg_paco") * util::watt_to_kilowatt;
				}
			}
			else
			{
				batt_vars->inverter_model = SharedInverter::NONE;
				batt_vars->inverter_count = 1;
				batt_vars->inverter_efficiency = batt_vars->batt_ac_dc_efficiency;
				batt_vars->inverter_paco = batt_vars->batt_kw;
			}
		}
	}
	else
		batt_vars = batt_vars_in;

	// component models
	voltage_model = 0;
	lifetime_model = 0;
	lifetime_cycle_model = 0;
	lifetime_calendar_model = 0;
	thermal_model = 0;
	battery_model = 0;
	capacity_model = 0;
	dispatch_model = 0;
	losses_model = 0;
	charge_control = 0;
	battery_metrics = 0;

	// outputs
	outTotalCharge = 0;
	outAvailableCharge = 0;
	outBoundCharge = 0;
	outMaxChargeAtCurrent = 0;
	outMaxChargeThermal = 0;
	outMaxCharge = 0;
	outSOC = 0;
	outDOD = 0;
	outCurrent = 0;
	outCellVoltage = 0;
	outBatteryVoltage = 0;
	outCapacityPercent = 0;
	outCycles = 0;
	outBatteryBankReplacement = 0;
	outBatteryTemperature = 0;
	outCapacityThermalPercent = 0;
	outBatteryPower = 0;
	outGridPower = 0;
	outPVToLoad = 0;
	outBatteryToLoad = 0;
	outGridToLoad = 0;
	outGridPowerTarget = 0;
	outPVToBatt = 0;
	outGridToBatt = 0;
	outPVToGrid = 0;
	outBatteryToGrid = 0;
	outBatteryConversionPowerLoss = 0;
	outBatterySystemLoss = 0;
	outAverageCycleEfficiency = 0;
	outPVChargePercent = 0;
	outAnnualPVChargeEnergy = 0;
	outAnnualGridChargeEnergy = 0;
	outAnnualChargeEnergy = 0;
	outAnnualDischargeEnergy = 0;
	outAnnualGridImportEnergy = 0;
	outAnnualGridExportEnergy = 0;
	outCostToCycle = 0;

	en = setup_model;
	if (!en) return;

	if (!batt_vars->system_use_lifetime_output){
		if (batt_vars->batt_replacement_option > 0)
			cm.log("Replacements are enabled without running lifetime simulation, please run over lifetime to consider battery replacements", SSC_WARNING);
	}
	total_steps = nyears * 8760 * step_per_hour;
	chem = batt_vars->batt_chem;

	util::matrix_t<double>  batt_voltage_matrix = batt_vars->batt_voltage_matrix;
	if (batt_vars->batt_voltage_choice == voltage_t::VOLTAGE_TABLE)
	{
		if (batt_voltage_matrix.nrows() < 2 || batt_voltage_matrix.ncols() != 2)
			throw compute_module::exec_error("battery", "Battery lifetime matrix must have 2 columns and at least 2 rows");
	}
	util::matrix_t<double>  batt_lifetime_matrix = batt_vars->batt_lifetime_matrix;
	if (batt_lifetime_matrix.nrows() < 3 || batt_lifetime_matrix.ncols() != 3)
		throw compute_module::exec_error("battery", "Battery lifetime matrix must have three columns and at least three rows");

	util::matrix_t<double>  batt_calendar_lifetime_matrix = batt_vars->batt_calendar_lifetime_matrix;
	if (batt_vars->batt_calendar_choice == lifetime_calendar_t::CALENDAR_LOSS_TABLE && (batt_calendar_lifetime_matrix.nrows() < 2 || batt_calendar_lifetime_matrix.ncols() != 2))
		throw compute_module::exec_error("battery", "Battery calendar lifetime matrix must have 2 columns and at least 2 rows");

	/* **********************************************************************
	Initialize outputs
	********************************************************************** */

	// non-lifetime outputs
	if (nyears <= 1)
	{
		// only allocate if lead-acid
		if (chem == 0)
		{
			outAvailableCharge = cm.allocate("batt_q1", nrec*nyears);
			outBoundCharge = cm.allocate("batt_q2", nrec*nyears);
		}
		outCellVoltage = cm.allocate("batt_voltage_cell", nrec*nyears);
		outMaxCharge = cm.allocate("batt_qmax", nrec*nyears);
		outMaxChargeThermal = cm.allocate("batt_qmax_thermal", nrec*nyears);
		outBatteryTemperature = cm.allocate("batt_temperature", nrec*nyears);
		outCapacityThermalPercent = cm.allocate("batt_capacity_thermal_percent", nrec*nyears);
	}
	outCurrent = cm.allocate("batt_I", nrec*nyears);
	outBatteryVoltage = cm.allocate("batt_voltage", nrec*nyears);
	outTotalCharge = cm.allocate("batt_q0", nrec*nyears);
	outCycles = cm.allocate("batt_cycles", nrec*nyears);
	outSOC = cm.allocate("batt_SOC", nrec*nyears);
	outDOD = cm.allocate("batt_DOD", nrec*nyears);
	outCapacityPercent = cm.allocate("batt_capacity_percent", nrec*nyears);
	outBatteryPower = cm.allocate("batt_power", nrec*nyears);
	outGridPower = cm.allocate("grid_power", nrec*nyears); // Net grid energy required.  Positive indicates putting energy on grid.  Negative indicates pulling off grid
	outGenPower = cm.allocate("pv_batt_gen", nrec*nyears);
	outPVToGrid = cm.allocate("pv_to_grid", nrec*nyears);

	if (batt_vars->batt_meter_position == dispatch_t::BEHIND)
	{
		outPVToLoad = cm.allocate("pv_to_load", nrec*nyears);
		outBatteryToLoad = cm.allocate("batt_to_load", nrec*nyears);
		outGridToLoad = cm.allocate("grid_to_load", nrec*nyears);

		if (batt_vars->batt_dispatch != dispatch_t::MANUAL)
		{
			outGridPowerTarget = cm.allocate("grid_power_target", nrec*nyears);
			outBattPowerTarget = cm.allocate("batt_power_target", nrec*nyears);
		}
	}
	else if (batt_vars->batt_meter_position == dispatch_t::FRONT)
	{
		outBatteryToGrid = cm.allocate("batt_to_grid", nrec*nyears);

		if (batt_vars->batt_dispatch != dispatch_t::FOM_MANUAL)
			outCostToCycle = cm.allocate("batt_cost_to_cycle", nrec*nyears);
	}
	outPVToBatt = cm.allocate("pv_to_batt", nrec*nyears);
	outGridToBatt = cm.allocate("grid_to_batt", nrec*nyears);
	outBatteryConversionPowerLoss = cm.allocate("batt_conversion_loss", nrec*nyears);
	outBatterySystemLoss = cm.allocate("batt_system_loss", nrec*nyears);

	// annual outputs
	size_t annual_size = nyears + 1;
	if (nyears == 1){ annual_size = 1; };

	outBatteryBankReplacement = cm.allocate("batt_bank_replacement", annual_size);
	outAnnualChargeEnergy = cm.allocate("batt_annual_charge_energy", annual_size);
	outAnnualDischargeEnergy = cm.allocate("batt_annual_discharge_energy", annual_size);
	outAnnualGridImportEnergy = cm.allocate("annual_import_to_grid_energy", annual_size);
	outAnnualGridExportEnergy = cm.allocate("annual_export_to_grid_energy", annual_size);
	outAnnualEnergySystemLoss = cm.allocate("batt_annual_energy_system_loss", annual_size);
	outAnnualEnergyLoss = cm.allocate("batt_annual_energy_loss", annual_size);
	outAnnualPVChargeEnergy = cm.allocate("batt_annual_charge_from_pv", annual_size);
	outAnnualGridChargeEnergy = cm.allocate("batt_annual_charge_from_grid", annual_size);

	outBatteryBankReplacement[0] = 0;
	outAnnualChargeEnergy[0] = 0;
	outAnnualDischargeEnergy[0] = 0;
	outAnnualGridImportEnergy[0] = 0;
	outAnnualGridExportEnergy[0] = 0;
	outAnnualEnergyLoss[0] = 0;

	// model initialization
	if ((chem == battery_t::LEAD_ACID || chem == battery_t::LITHIUM_ION) &&  batt_vars->batt_voltage_choice == voltage_t::VOLTAGE_MODEL)
		voltage_model = new voltage_dynamic_t(batt_vars->batt_computed_series, batt_vars->batt_computed_strings, batt_vars->batt_Vnom_default, batt_vars->batt_Vfull, batt_vars->batt_Vexp,
		batt_vars->batt_Vnom, batt_vars->batt_Qfull, batt_vars->batt_Qexp, batt_vars->batt_Qnom, batt_vars->batt_C_rate, batt_vars->batt_resistance);
	else if ((chem == battery_t::VANADIUM_REDOX) && batt_vars->batt_voltage_choice == voltage_t::VOLTAGE_MODEL)
		voltage_model = new voltage_vanadium_redox_t(batt_vars->batt_computed_series, batt_vars->batt_computed_strings, batt_vars->batt_Vnom_default, batt_vars->batt_resistance);
	else
		voltage_model = new voltage_table_t(batt_vars->batt_computed_series, batt_vars->batt_computed_strings, batt_vars->batt_Vnom_default, batt_vars->batt_voltage_matrix, batt_vars->batt_resistance);

	lifetime_cycle_model = new  lifetime_cycle_t(batt_lifetime_matrix);
	lifetime_calendar_model = new lifetime_calendar_t(batt_vars->batt_calendar_choice, batt_calendar_lifetime_matrix, _dt_hour, 
		(float)batt_vars->batt_calendar_q0, (float)batt_vars->batt_calendar_a, (float)batt_vars->batt_calendar_b, (float)batt_vars->batt_calendar_c);
	lifetime_model = new lifetime_t(lifetime_cycle_model, lifetime_calendar_model, batt_vars->batt_replacement_option, batt_vars->batt_replacement_capacity);

	util::matrix_t<double> cap_vs_temp = batt_vars->cap_vs_temp;
	if (cap_vs_temp.nrows() < 2 || cap_vs_temp.ncols() != 2)
		throw compute_module::exec_error("battery", "capacity vs temperature matrix must have two columns and at least two rows");

	thermal_model = new thermal_t(
		batt_vars->batt_mass, // [kg]
		batt_vars->batt_length, // [m]
		batt_vars->batt_width, // [m]
		batt_vars->batt_height, // [m]
		batt_vars->batt_Cp, // [J/kgK]
		batt_vars->batt_h_to_ambient, // W/m2K
		273.15 + batt_vars->T_room, // K
		cap_vs_temp);


	battery_model = new battery_t(
		dt_hr,
		chem);

	if (chem == battery_t::LEAD_ACID)
	{
		capacity_model = new capacity_kibam_t(
			batt_vars->LeadAcid_q20_computed,
			batt_vars->LeadAcid_tn,
			batt_vars->LeadAcid_qn_computed,
			batt_vars->LeadAcid_q10_computed,
			batt_vars->batt_initial_SOC,
			batt_vars->batt_maximum_SOC,
			batt_vars->batt_minimum_SOC);
	}
	else if (chem == battery_t::LITHIUM_ION)
	{
		capacity_model = new capacity_lithium_ion_t(
			batt_vars->batt_Qfull*batt_vars->batt_computed_strings, batt_vars->batt_initial_SOC, batt_vars->batt_maximum_SOC, batt_vars->batt_minimum_SOC);
	}
	// for now assume Flow Batteries responds quickly, like Lithium-ion, but with an independent capacity/power
	else if (chem == battery_t::VANADIUM_REDOX || chem == battery_t::IRON_FLOW)
	{
		capacity_model = new capacity_lithium_ion_t(
			batt_vars->batt_Qfull_flow, batt_vars->batt_initial_SOC, batt_vars->batt_maximum_SOC, batt_vars->batt_minimum_SOC);
	}

	// accumulate monthly losses
	double_vec batt_charging_losses;
	double_vec batt_discharging_losses;
	double_vec batt_idle_losses;
	double_vec batt_system_losses;

	if (batt_vars->batt_loss_choice == losses_t::MONTHLY)
	{
		double_vec charging_loss = batt_vars->batt_losses_charging;
		double_vec discharging_loss = batt_vars->batt_losses_discharging;
		double_vec idling_loss = batt_vars->batt_losses_idle;

		double charge_loss = charging_loss[0];
		double discharge_loss = discharging_loss[0];
		double idle_loss = idling_loss[0];


		for (int m = 0; m != 12; m++)
		{
			if (charging_loss.size() > 1)
				charge_loss = charging_loss[m];
			if (discharging_loss.size() > 1)
				discharge_loss = discharging_loss[m];
			if (idling_loss.size() > 1)
				idle_loss = idling_loss[m];

			for (int d = 0; d != util::days_in_month((int)m); d++)
			{
				for (int h = 0; h != 24; h++)
				{
					for (size_t s = 0; s != step_per_hour; s++)
					{
						batt_charging_losses.push_back(charge_loss);
						batt_discharging_losses.push_back(discharge_loss);
						batt_idle_losses.push_back(idle_loss);
						batt_system_losses.push_back(0.);
					}
				}
			}
		}
	}

	losses_model = new losses_t(
		lifetime_model,
		thermal_model,
		capacity_model,
		batt_vars->batt_loss_choice,
		batt_charging_losses,
		batt_discharging_losses,
		batt_idle_losses,
		batt_system_losses);

	battery_model->initialize(capacity_model, voltage_model, lifetime_model, thermal_model, losses_model);
	battery_metrics = new battery_metrics_t(dt_hr);

	/*! Process the dispatch options and create the appropriate model */
	if ((batt_vars->batt_meter_position == dispatch_t::BEHIND && batt_vars->batt_dispatch == dispatch_t::MANUAL) ||
		(batt_vars->batt_meter_position == dispatch_t::FRONT && batt_vars->batt_dispatch == dispatch_t::FOM_MANUAL))
	{
		/*! Generic manual dispatch model inputs */
		if (batt_vars->batt_can_charge.size() != 6 || batt_vars->batt_can_discharge.size() != 6 || batt_vars->batt_can_gridcharge.size() != 6)
			throw compute_module::exec_error("battery", "invalid manual dispatch control vector lengths");

		if (batt_vars->batt_discharge_schedule_weekday.nrows() != 12 || batt_vars->batt_discharge_schedule_weekday.ncols() != 24)
			throw compute_module::exec_error("battery", "invalid manual dispatch schedule matrix dimensions, must be 12 x 24");

		if (batt_vars->batt_discharge_schedule_weekend.nrows() != 12 || batt_vars->batt_discharge_schedule_weekend.ncols() != 24)
			throw compute_module::exec_error("battery", "invalid weekend manual dispatch schedule matrix dimensions, must be 12 x 24");

		size_t discharge_index = 0;
		size_t gridcharge_index = 0;
		for (size_t i = 0; i < batt_vars->batt_can_discharge.size(); i++)
		{
			if (batt_vars->batt_can_discharge[i])
				dm_percent_discharge[i + 1] = batt_vars->batt_discharge_percent[discharge_index++];
			
			if (batt_vars->batt_can_gridcharge[i])
				dm_percent_gridcharge[i + 1] = batt_vars->batt_gridcharge_percent[gridcharge_index++];
		}
		// Manual Dispatch Model
		if ((batt_vars->batt_meter_position == dispatch_t::BEHIND && batt_vars->batt_dispatch == dispatch_t::MANUAL) || 
			(batt_vars->batt_meter_position == dispatch_t::FRONT && batt_vars->batt_dispatch == dispatch_t::FOM_MANUAL))
		{
			dispatch_model = new dispatch_manual_t(battery_model, dt_hr, batt_vars->batt_minimum_SOC, batt_vars->batt_maximum_SOC,
				batt_vars->batt_current_choice,
				batt_vars->batt_current_charge_max, batt_vars->batt_current_discharge_max,
				batt_vars->batt_power_charge_max, batt_vars->batt_power_discharge_max,
				batt_vars->batt_minimum_modetime,
				batt_vars->batt_dispatch, batt_vars->batt_meter_position,
				batt_vars->batt_discharge_schedule_weekday, batt_vars->batt_discharge_schedule_weekend,
				batt_vars->batt_can_charge, batt_vars->batt_can_discharge, batt_vars->batt_can_gridcharge, dm_percent_discharge, dm_percent_gridcharge);
		}
	}
	/*! Front of meter automated DC-connected dispatch */
	else if (batt_vars->batt_meter_position == dispatch_t::FRONT) 
	{
		double efficiencyCombined = batt_vars->batt_dc_dc_bms_efficiency * 0.01 * batt_vars->inverter_efficiency;

		// Create UtilityRate object only if utility rate is defined
		utilityRate = NULL;
		if (batt_vars->ec_rate_defined) {
			utilityRate = new UtilityRate(batt_vars->ec_weekday_schedule, batt_vars->ec_weekend_schedule, batt_vars->ec_tou_matrix);
		}
		dispatch_model = new dispatch_automatic_front_of_meter_t(battery_model, dt_hr, batt_vars->batt_minimum_SOC, batt_vars->batt_maximum_SOC,
			batt_vars->batt_current_choice, batt_vars->batt_current_charge_max, batt_vars->batt_current_discharge_max,
			batt_vars->batt_power_charge_max, batt_vars->batt_power_discharge_max, batt_vars->batt_minimum_modetime,
			batt_vars->batt_dispatch, batt_vars->batt_meter_position,
			nyears, batt_vars->batt_look_ahead_hours, batt_vars->batt_dispatch_update_frequency_hours,
			batt_vars->batt_dispatch_auto_can_charge, batt_vars->batt_dispatch_auto_can_clipcharge, batt_vars->batt_dispatch_auto_can_gridcharge,
			batt_vars->inverter_paco, batt_vars->batt_cost_per_kwh,
			batt_vars->batt_cycle_cost_choice, batt_vars->batt_cycle_cost,
			batt_vars->ppa_factors, batt_vars->ppa_weekday_schedule, batt_vars->ppa_weekend_schedule, utilityRate,
			batt_vars->batt_dc_dc_bms_efficiency, efficiencyCombined , efficiencyCombined);

		if (batt_vars->batt_dispatch == dispatch_t::CUSTOM_DISPATCH)
		{
			if (dispatch_automatic_front_of_meter_t * dispatch_fom = dynamic_cast<dispatch_automatic_front_of_meter_t*>(dispatch_model))
			{
				if (batt_vars->batt_custom_dispatch.size() != 8760 * step_per_hour) {
					throw compute_module::exec_error("battery", "invalid custom dispatch, must be 8760 * steps_per_hour");
				}
				dispatch_fom->set_custom_dispatch(batt_vars->batt_custom_dispatch);
			}
		}
		
	}
	/*! Behind-the-meter automated dispatch for peak shaving */
	else
	{			
		dispatch_model = new dispatch_automatic_behind_the_meter_t(battery_model, dt_hr, batt_vars->batt_minimum_SOC, batt_vars->batt_maximum_SOC,
			batt_vars->batt_current_choice, batt_vars->batt_current_charge_max, batt_vars->batt_current_discharge_max,
			batt_vars->batt_power_charge_max, batt_vars->batt_power_discharge_max, batt_vars->batt_minimum_modetime,
			batt_vars->batt_dispatch, batt_vars->batt_meter_position, nyears,
			batt_vars->batt_look_ahead_hours, batt_vars->batt_dispatch_update_frequency_hours,
			batt_vars->batt_dispatch_auto_can_charge, batt_vars->batt_dispatch_auto_can_clipcharge, batt_vars->batt_dispatch_auto_can_gridcharge
			);
		if (batt_vars->batt_dispatch == dispatch_t::CUSTOM_DISPATCH)
		{
			if (dispatch_automatic_behind_the_meter_t * dispatch_btm = dynamic_cast<dispatch_automatic_behind_the_meter_t*>(dispatch_model))
			{
				if (batt_vars->batt_custom_dispatch.size() != 8760 * step_per_hour) {
					throw compute_module::exec_error("battery", "invalid custom dispatch, must be 8760 * steps_per_hour");
				}
				dispatch_btm->set_custom_dispatch(batt_vars->batt_custom_dispatch);
			}
		}
	}

	if (batt_vars->batt_topology == ChargeController::AC_CONNECTED) {
		charge_control = new ACBatteryController(dispatch_model, battery_metrics, batt_vars->batt_ac_dc_efficiency, batt_vars->batt_dc_ac_efficiency);
	}
	else if (batt_vars->batt_topology == ChargeController::DC_CONNECTED) {
		charge_control = new DCBatteryController(dispatch_model, battery_metrics, batt_vars->batt_dc_dc_bms_efficiency);
	}

	parse_configuration();
}

void battstor::parse_configuration()
{
	int batt_dispatch = batt_vars->batt_dispatch;
	int batt_meter_position = batt_vars->batt_meter_position;

	// parse configuration
	if (dynamic_cast<dispatch_automatic_t*>(dispatch_model))
	{
		prediction_index = 0;
		if (batt_meter_position == dispatch_t::BEHIND)
		{
			if (batt_dispatch == dispatch_t::LOOK_AHEAD || batt_dispatch == dispatch_t::MAINTAIN_TARGET)
			{
				look_ahead = true;
				if (batt_dispatch == dispatch_t::MAINTAIN_TARGET)
					input_target = true;
			}
			else if (batt_dispatch == dispatch_t::CUSTOM_DISPATCH)
			{
				input_custom_dispatch = true;
			}
			else
				look_behind = true;
		}
		else if (batt_meter_position == dispatch_t::FRONT)
		{
			if (batt_dispatch == dispatch_t::FOM_LOOK_AHEAD) {
				look_ahead = true;
			}
			else if (batt_dispatch == dispatch_t::FOM_LOOK_BEHIND) {
				look_behind = true;
			}
			else if (batt_dispatch == dispatch_t::FOM_FORECAST) {
				input_forecast = true;
			}
			else if (batt_dispatch == dispatch_t::FOM_CUSTOM_DISPATCH) {
				input_custom_dispatch = true;
			}
		}
	}
	else
		manual_dispatch = true;
}

void battstor::initialize_automated_dispatch(std::vector<ssc_number_t> pv, std::vector<ssc_number_t> load, std::vector<ssc_number_t> cliploss)
{
	if (dynamic_cast<dispatch_automatic_t*>(dispatch_model))
	{
		// automatic look ahead or behind
		size_t nrec = nyears * 8760 * step_per_hour;
		if (!input_custom_dispatch)
		{

			// look ahead
			if (look_ahead)
			{
				if (pv.size() != 0)
				{
					for (size_t idx = 0; idx != nrec; idx++) {
						pv_prediction.push_back(pv[idx]);
					}
			
				}
				if (load.size() != 0) 
				{
					for (size_t idx = 0; idx != nrec; idx++) {
						load_prediction.push_back(load[idx]);
					}
				}
				if (cliploss.size() != 0) 
				{
					for (size_t idx = 0; idx != nrec; idx++) {
						cliploss_prediction.push_back(cliploss[idx]);
					}
				}
			}
			else if (look_behind)
			{
				// day one is zeros
				for (size_t idx = 0; idx != 24 * step_per_hour; idx++)
				{
					pv_prediction.push_back(0);
					load_prediction.push_back(0);
					cliploss_prediction.push_back(0);
				}

				if (pv.size() != 0)
				{
					for (size_t idx = 0; idx != nrec - 24 * step_per_hour; idx++){
						pv_prediction.push_back(pv[idx]);
					}
				}
				if (load.size() !=0 )
				{
					for (size_t idx = 0; idx != nrec - 24 * step_per_hour; idx++) {
						load_prediction.push_back(load[idx]);
					}
				}
				if (cliploss.size() !=0 )
				{
					for (size_t idx = 0; idx != nrec - 24 * step_per_hour; idx++) {
						cliploss_prediction.push_back(cliploss[idx]);
					}
				}
			}
			else if (input_forecast)
			{
				if (pv.size() != 0)
				{
					for (size_t idx = 0; idx != nrec; idx++) {
						pv_prediction.push_back(pv[idx]);
					}
				}
				if (cliploss.size() != 0)
				{
					for (size_t idx = 0; idx != nrec; idx++) {
						cliploss_prediction.push_back(cliploss[idx]);
					}
				}
			}
			// Input checking
			if (pv.size() == 0)
			{

				for (size_t idx = 0; idx != nrec; idx++)				{
					pv_prediction.push_back(0.);
				}
			}
			if (load.size() == 0)
			{
				for (size_t idx = 0; idx != nrec; idx++) {
					load_prediction.push_back(0.);
				}
			}
			if (cliploss.size() == 0)
			{
				for (size_t idx = 0; idx != nrec; idx++) {
					cliploss_prediction.push_back(0.);
				}
			}

			if (dispatch_automatic_behind_the_meter_t * automatic_dispatch_btm = dynamic_cast<dispatch_automatic_behind_the_meter_t*>(dispatch_model))
			{
				automatic_dispatch_btm->update_pv_data(pv_prediction);
				automatic_dispatch_btm->update_load_data(load_prediction);

				if (input_target)
					automatic_dispatch_btm->set_target_power(target_power);
			}
			else if (dispatch_automatic_front_of_meter_t * automatic_dispatch_fom = dynamic_cast<dispatch_automatic_front_of_meter_t*>(dispatch_model))
			{
				automatic_dispatch_fom->update_pv_data(pv_prediction);
				automatic_dispatch_fom->update_cliploss_data(cliploss_prediction);
			}
		}
	}
	
}
battstor::~battstor()
{
	if( voltage_model ) delete voltage_model;
	if( lifetime_cycle_model ) delete lifetime_cycle_model;
	if (lifetime_calendar_model) delete lifetime_calendar_model;
	if( thermal_model ) delete thermal_model;
	if( battery_model ) delete battery_model;
	if (battery_metrics) delete battery_metrics;
	if( capacity_model ) delete capacity_model;
	if (losses_model) delete losses_model;
	if( dispatch_model ) delete dispatch_model;
	if (charge_control) delete charge_control;
	if (make_vars) delete batt_vars;
}

void battstor::check_replacement_schedule()
{
	if (batt_vars->batt_replacement_option == battery_t::REPLACE_BY_SCHEDULE)
	{
		// don't allow replacement on first hour of first year
		if (hour == 0 && year == 0)
			return;

		bool replace = false;
		if (year < batt_vars->batt_replacement_schedule.size())
		{
			int num_repl = batt_vars->batt_replacement_schedule[year];
			for (int j_repl = 0; j_repl < num_repl; j_repl++)
			{
				if ((hour == (j_repl * 8760 / num_repl)) && step == 0)
				{
					replace = true;
					break;
				}
			}
		}
		if (replace)
			force_replacement();
	}
}
void battstor::force_replacement()
{
	lifetime_model->force_replacement();
	battery_model->runLifetimeModel(0);
}

void battstor::initialize_time(size_t year_in, size_t hour_of_year, size_t step_of_hour)
{
	step = step_of_hour;
	hour = hour_of_year;
	year = year_in;
	index = (year * 8760 + hour) * step_per_hour + step;
	year_index = (hour * step_per_hour) + step; 
	step_per_year = 8760 * step_per_hour;
}
void battstor::advance(compute_module &cm, double P_pv, double V_pv, double P_load, double P_pv_clipped )
{
	charge_control->run(year, hour, step, year_index, P_pv, V_pv, P_load, P_pv_clipped);
	outputs_fixed(cm);
	outputs_topology_dependent(cm);
	metrics(cm);
}
void battstor::setSharedInverter(SharedInverter * sharedInverter)
{
	if (DCBatteryController * tmp = dynamic_cast<DCBatteryController *>(charge_control))
		tmp->setSharedInverter(sharedInverter);
}
void battstor::outputs_fixed(compute_module &cm)
{
	if (index == total_steps - 1)
		process_messages(cm);

	// non-lifetime outputs
	if (nyears <= 1)
	{
		// Capacity Output with Losses Applied
		if (capacity_kibam_t * kibam = dynamic_cast<capacity_kibam_t*>(capacity_model))
		{
			outAvailableCharge[index] = (ssc_number_t)(kibam->q1());
			outBoundCharge[index] = (ssc_number_t)(kibam->q2());
		}
		outCellVoltage[index] = (ssc_number_t)(voltage_model->cell_voltage());
		outMaxCharge[index] = (ssc_number_t)(capacity_model->qmax());
		outMaxChargeThermal[index] = (ssc_number_t)(capacity_model->qmax_thermal());
	
		outBatteryTemperature[index] = (ssc_number_t)(thermal_model->T_battery() - 273.15);
		outCapacityThermalPercent[index] = (ssc_number_t)(thermal_model->capacity_percent());
	}

	// Lifetime outputs
	outTotalCharge[index] = (ssc_number_t)(capacity_model->q0());
	outCurrent[index] = (ssc_number_t)(capacity_model->I());
	outBatteryVoltage[index] = (ssc_number_t)(voltage_model->battery_voltage());

	outCycles[index] = (ssc_number_t)(lifetime_cycle_model->cycles_elapsed());
	outSOC[index] = (ssc_number_t)(capacity_model->SOC());
	outDOD[index] = (ssc_number_t)(lifetime_cycle_model->cycle_range());
	outCapacityPercent[index] = (ssc_number_t)(lifetime_model->capacity_percent());
}

void battstor::outputs_topology_dependent(compute_module &)
{
	// Power output (all Powers in kWac)
	outBatteryPower[index] = (ssc_number_t)(dispatch_model->power_tofrom_battery());
	outGridPower[index] = (ssc_number_t)(dispatch_model->power_tofrom_grid());
	outGenPower[index] = (ssc_number_t)(dispatch_model->power_gen());
	outPVToBatt[index] = (ssc_number_t)(dispatch_model->power_pv_to_batt());
	outGridToBatt[index] = (ssc_number_t)(dispatch_model->power_grid_to_batt());
	outBatteryConversionPowerLoss[index] = (ssc_number_t)(dispatch_model->power_conversion_loss());
	outBatterySystemLoss[index] = (ssc_number_t)(dispatch_model->power_system_loss());
	outPVToGrid[index] = (ssc_number_t)(dispatch_model->power_pv_to_grid());

	if (batt_vars->batt_meter_position == dispatch_t::BEHIND)
	{
		outPVToLoad[index] = (ssc_number_t)(dispatch_model->power_pv_to_load());
		outBatteryToLoad[index] = (ssc_number_t)(dispatch_model->power_battery_to_load());
		outGridToLoad[index] = (ssc_number_t)(dispatch_model->power_grid_to_load());

		if (batt_vars->batt_dispatch != dispatch_t::MANUAL)
		{
			outGridPowerTarget[index] = (ssc_number_t)(dispatch_model->power_grid_target());
			outBattPowerTarget[index] = (ssc_number_t)(dispatch_model->power_batt_target());
		}

	}
	else if (batt_vars->batt_meter_position == dispatch_t::FRONT)
	{
		outBatteryToGrid[index] = (ssc_number_t)(dispatch_model->power_battery_to_grid());

		if (batt_vars->batt_dispatch != dispatch_t::FOM_MANUAL)
			outCostToCycle[index] = (ssc_number_t)(dispatch_model->cost_to_cycle());
	}
}

void battstor::metrics(compute_module &)
{
	size_t annual_index;
	nyears > 1 ? annual_index = year + 1 : annual_index = 0;
	outBatteryBankReplacement[annual_index] = (ssc_number_t)(lifetime_model->replacements());

	if ((hour == 8759) && (step == step_per_hour - 1))
	{
		lifetime_model->reset_replacements();
		outAnnualGridImportEnergy[annual_index] = (ssc_number_t)(battery_metrics->energy_grid_import_annual());
		outAnnualGridExportEnergy[annual_index] = (ssc_number_t)(battery_metrics->energy_grid_export_annual());
		outAnnualPVChargeEnergy[annual_index] = (ssc_number_t)(battery_metrics->energy_pv_charge_annual());
		outAnnualGridChargeEnergy[annual_index] = (ssc_number_t)(battery_metrics->energy_grid_charge_annual());
		outAnnualChargeEnergy[annual_index] = (ssc_number_t)(battery_metrics->energy_charge_annual());
		outAnnualDischargeEnergy[annual_index] = (ssc_number_t)(battery_metrics->energy_discharge_annual());
		outAnnualEnergyLoss[annual_index] = (ssc_number_t)(battery_metrics->energy_loss_annual());
		outAnnualEnergySystemLoss[annual_index] = (ssc_number_t)(battery_metrics->energy_system_loss_annual());
		battery_metrics->new_year();
	}
	 
	// Average battery conversion efficiency
	outAverageCycleEfficiency = (ssc_number_t)battery_metrics->average_battery_conversion_efficiency();
	if (outAverageCycleEfficiency > 100)
		outAverageCycleEfficiency = 100;
	else if (outAverageCycleEfficiency < 0)
		outAverageCycleEfficiency = 0;

	// Average battery roundtrip efficiency
	outAverageRoundtripEfficiency = (ssc_number_t)battery_metrics->average_battery_roundtrip_efficiency();
	if (outAverageRoundtripEfficiency > 100)
		outAverageRoundtripEfficiency = 100;
	else if (outAverageRoundtripEfficiency < 0)
		outAverageRoundtripEfficiency = 0;

	// PV charge ratio
	outPVChargePercent = (ssc_number_t)battery_metrics->pv_charge_percent();
	if (outPVChargePercent > 100)
		outPVChargePercent = 100;
	else if (outPVChargePercent < 0)
		outPVChargePercent = 0;
}

// function needed to correctly calculate P_grid to to additional losses in P_gen post battery like wiring, curtailment, availablity
void battstor::update_grid_power(compute_module &, double P_gen_ac, double P_load_ac, size_t index_replace)
{
	double P_grid = P_gen_ac - P_load_ac;
	outGridPower[index_replace] = (ssc_number_t)(P_grid);
}

void battstor::calculate_monthly_and_annual_outputs( compute_module &cm )
{
	// single value metrics
	cm.assign("average_battery_conversion_efficiency", var_data( (ssc_number_t) outAverageCycleEfficiency ));
	cm.assign("average_battery_roundtrip_efficiency", var_data((ssc_number_t)outAverageRoundtripEfficiency));
	cm.assign("batt_pv_charge_percent", var_data((ssc_number_t)outPVChargePercent));
	cm.assign("batt_bank_installed_capacity", (ssc_number_t)batt_vars->batt_kwh);

	// monthly outputs
	cm.accumulate_monthly_for_year("pv_to_batt", "monthly_pv_to_batt", _dt_hour, step_per_hour);
	cm.accumulate_monthly_for_year("grid_to_batt", "monthly_grid_to_batt", _dt_hour, step_per_hour);
	cm.accumulate_monthly_for_year("pv_to_grid", "monthly_pv_to_grid", _dt_hour, step_per_hour);

	if (batt_vars->batt_meter_position == dispatch_t::BEHIND)
	{
		cm.accumulate_monthly_for_year("pv_to_load", "monthly_pv_to_load", _dt_hour, step_per_hour);
		cm.accumulate_monthly_for_year("batt_to_load", "monthly_batt_to_load", _dt_hour, step_per_hour);
		cm.accumulate_monthly_for_year("grid_to_load", "monthly_grid_to_load", _dt_hour, step_per_hour);
	}
	else if (batt_vars->batt_meter_position == dispatch_t::FRONT)
	{
		cm.accumulate_monthly_for_year("batt_to_grid", "monthly_batt_to_grid", _dt_hour, step_per_hour);
	}
}
void battstor::process_messages(compute_module &cm) 
{
	message dispatch_messages = dispatch_model->get_messages();
	message thermal_messages = thermal_model->get_messages();

	for (int i = 0; i != (int)dispatch_messages.total_message_count(); i++)
		cm.log(dispatch_messages.construct_log_count_string(i), SSC_NOTICE);
	for (int i = 0; i != (int)thermal_messages.total_message_count(); i++)
		cm.log(thermal_messages.construct_log_count_string(i), SSC_NOTICE);
}

///////////////////////////////////////////////////
static var_info _cm_vtab_battery[] = {
	/*   VARTYPE           DATATYPE         NAME                                            LABEL                                                   UNITS      META                           GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "en_batt",                                    "Enable battery storage model",                            "0/1",        "",                     "Battery",                      "?=0",                    "",                               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "gen",										   "System power generated",                                  "kW",         "",                     "",                             "",                       "",                               "" },
	{ SSC_INPUT,		SSC_ARRAY,	     "load",			                           "Electricity load (year 1)",                               "kW",	        "",				        "",                             "",	                      "",	                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_replacement_option",                    "Enable battery replacement?",                              "0=none,1=capacity based,2=user schedule", "", "Battery",             "?=0",                    "INTEGER,MIN=0,MAX=2",           "" },
	{ SSC_INOUT,        SSC_NUMBER,      "capacity_factor",                            "Capacity factor",                                         "%",          "",                     "",                             "?=0",                    "",                               "" },
	{ SSC_INOUT,        SSC_NUMBER,      "annual_energy",                              "Annual Energy",                                           "kWh",        "",                     "Battery",                      "?=0",                    "",                               "" },
																																																											      
	// other variables come from battstor common table
	var_info_invalid };

class cm_battery : public compute_module
{
public:

	cm_battery()
	{
		add_var_info(_cm_vtab_battery);
		add_var_info( vtab_battery_inputs);
		add_var_info(vtab_battery_outputs);
	}

	void exec() throw(general_error)
	{
		if (as_boolean("en_batt"))
		{
			// Parse "Gen input"
			std::vector<ssc_number_t> power_input = as_vector_ssc_number_t("gen");
			size_t nrec = power_input.size();
			battstor batt(*this, true, nrec, static_cast<double>(8760. / nrec));
			ssc_number_t * p_gen = allocate("gen", nrec * batt.nyears);

			// Parse "Load input"
			std::vector<ssc_number_t> power_load;
			if (batt.batt_vars->batt_meter_position == dispatch_t::BEHIND)
			{
				power_load = as_vector_ssc_number_t("load");
				batt.initialize_automated_dispatch(power_input, power_load);
			}
			else
			{
				for (int i = 0; i != power_input.size(); i++)
					power_load.push_back(0);
			}

			// Prepare annual outputs
			double capacity_factor_in = 0.;
			double annual_energy_in = 0.;
			double nameplate_in = 0.;

			if (is_assigned("capacity_factor") && is_assigned("annual_energy")) {
				capacity_factor_in = as_double("capacity_factor");
				annual_energy_in = as_double("annual_energy");
				nameplate_in = (annual_energy_in / (capacity_factor_in * 0.01)) / 8760.;
			}
	
			// Error checking
			if (power_input.size() != power_load.size())
				throw exec_error("battery", "Load and PV power do not match weatherfile length");

			
			if (batt.step_per_hour > 60 || batt.total_steps != power_input.size() * batt.nyears)
				throw exec_error("battery", util::format("invalid number of data records (%u): must be an integer multiple of 8760", batt.total_steps));

			// Battery cannot be run in DC-connected mode for generic system.  
			// We don't have detailed inverter voltage info or clipping info (if PV)
			if (batt.batt_vars->batt_topology == ChargeController::DC_CONNECTED) {
				batt.batt_vars->batt_topology = ChargeController::AC_CONNECTED;
				throw exec_error("battery", "Generic System must be AC connected to battery");
			}

			/* *********************************************************************************************
			Run Simulation
			*********************************************************************************************** */
			double annual_energy = 0;
			int lifetime_idx = 0;
			for (size_t year = 0; year != batt.nyears; year++)
			{
				int year_idx = 0; 
				for (size_t hour = 0; hour < 8760; hour++)
				{
					for (size_t jj = 0; jj < batt.step_per_hour; jj++)
					{
	
						batt.initialize_time(year, hour, jj);
						batt.check_replacement_schedule();
						batt.advance(*this, power_input[year_idx], power_load[year_idx]);
						p_gen[lifetime_idx] = batt.outGenPower[lifetime_idx];
						annual_energy += p_gen[lifetime_idx] * batt._dt_hour;
						lifetime_idx++;
						year_idx++;
					}
				}
			}
			batt.calculate_monthly_and_annual_outputs(*this);

			// update capacity factor and annual energy
			assign("capacity_factor", var_data(static_cast<ssc_number_t>(annual_energy * 100.0 / (nameplate_in * 8760.))));
			assign("annual_energy", var_data(static_cast<ssc_number_t>(annual_energy)));
		}
		else
			assign("average_battery_roundtrip_efficiency", var_data((ssc_number_t)0.));
	}
};

DEFINE_MODULE_ENTRY(battery, "Battery storage standalone model .", 10)
