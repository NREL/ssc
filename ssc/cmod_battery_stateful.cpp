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

#include <lib_battery_capacity.h>
#include "common.h"
#include "vartab.h"
#include "core.h"
#include "cmod_battery_stateful.h"

var_info vtab_battery_stateful_inputs[] = {
        /*   VARTYPE           DATATYPE         NAME                                            LABEL                                                   UNITS      META                   GROUP           REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
        { SSC_INPUT,        SSC_NUMBER,      "control_mode",                               "Control using current (0) or power (1)",                  "hr",      "",   "Controls",       "*",                           "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "dt_hr",                                      "Time step in hours",                                      "hr",      "",   "Controls",       "*",                           "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "input_current",                              "Current at which to run battery",                         "A",       "",   "Controls",       "control_mode=0",              "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "input_power",                                "Power at which to run battery",                           "kW",      "",   "Controls",       "control_mode=1",              "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "run_sequentially",                           "True turns off reading state from data at start of step", "0/1",     "",   "Controls",       "?=0",                         "",                              "" },

        // capacity
        { SSC_INPUT,        SSC_NUMBER,      "chem",                                       "Lead Acid (0), Li Ion (1), Vanadium Redox (2), Iron Flow (3)","0/1/2/3","",   "BatteryCell",       "*",                           "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "initial_SOC",		                          "Initial state-of-charge",                                 "%",       "",                     "BatteryCell",       "*",                           "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "minimum_SOC",		                          "Minimum allowed state-of-charge",                         "%",       "",                     "BatteryCell",       "*",                           "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "maximum_SOC",                                "Maximum allowed state-of-charge",                         "%",       "",                     "BatteryCell",       "*",                           "",                              "" },
        { SSC_INPUT,		SSC_NUMBER,		 "leadacid_q20",	                              "Capacity at 20-hour discharge rate",                      "Ah",       "",                     "BatteryCell",       "chem=0",                           "",                             "" },
        { SSC_INPUT,		SSC_NUMBER,		 "leadacid_q10",	                              "Capacity at 10-hour discharge rate",                      "Ah",       "",                     "BatteryCell",       "chem=0",                           "",                             "" },
        { SSC_INPUT,		SSC_NUMBER,		 "leadacid_qn",	                              "Capacity at discharge rate for n-hour rate",              "Ah",       "",                     "BatteryCell",       "chem=0",                           "",                             "" },
        { SSC_INPUT,		SSC_NUMBER,		 "leadacid_tn",	                              "Hours to discharge for qn rate",                          "h",        "",                     "BatteryCell",       "chem=0",                           "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "num_strings",                                "Number of strings of cells",                              "",        "",                     "BatterySystem",       "*",                           "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "num_cells_series",                           "Number of cells in series",                               "",        "",                     "BatterySystem",       "*",                           "",                              "" },

        // Voltage discharge curve
        { SSC_INPUT,        SSC_NUMBER,      "voltage_choice",                             "Battery voltage input option",                            "0/1",     "0=Model,1=Table",      "BatteryCell",       "?=0",                        "",                             "" },
        { SSC_INPUT,		SSC_MATRIX,      "voltage_matrix",                             "Table with depth-of-discharge % and Voltage as columns",  "[[%, V]]","",                     "BatteryCell",       "voltage_choice=1",      "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "Vnom_default",                               "Default nominal cell voltage",                            "V",       "",                     "BatteryCell",       "*",                          "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "resistance",                                 "Internal resistance",                                     "Ohm",     "",                     "BatteryCell",       "*",                           "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "Vfull",                                      "Fully charged cell voltage",                              "V",       "",                     "BatteryCell",       "voltage_choice=0&chem~2", "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "Vexp",                                       "Cell voltage at end of exponential zone",                 "V",       "",                     "BatteryCell",       "voltage_choice=0&chem~2",  "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "Vnom",                                       "Cell voltage at end of nominal zone",                     "V",       "",                     "BatteryCell",       "voltage_choice=0&chem~2", "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "Qfull",                                      "Fully charged cell capacity",                             "Ah",      "",                     "BatteryCell",       "voltage_choice=0&chem~2", "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "Qexp",                                       "Cell capacity at end of exponential zone",                "Ah",      "",                     "BatteryCell",       "voltage_choice=0&chem~2", "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "Qnom",                                       "Cell capacity at end of nominal zone",                    "Ah",      "",                     "BatteryCell",       "voltage_choice=0&chem~2", "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "C_rate",                                     "Rate at which voltage vs. capacity curve input",          "",        "",                     "BatteryCell",       "voltage_choice=0&chem~2", "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "Qfull_flow",                                 "Fully charged flow battery capacity",                     "Ah",      "",                     "BatteryCell",       "voltage_choice=0&chem=3", "",                              "" },

        // thermal inputs
        { SSC_INPUT,        SSC_NUMBER,      "mass",                                       "Battery mass",                                            "kg",       "",                     "BatterySystem",       "*",                           "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "surface_area",                               "Battery surface area",                                    "m^2",      "",                     "BatterySystem",       "*",                           "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "Cp",                                         "Battery specific heat capacity",                          "J/KgK",    "",                     "BatteryCell",       "*",                           "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "h",                                          "Heat transfer between battery and environment",           "W/m2K",    "",                     "BatteryCell",       "*",                           "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "T_room_init",                                "Temperature of storage room",                             "C",        "",                     "BatteryCell",       "*",                           "",                             "" },
        { SSC_INPUT,        SSC_MATRIX,      "cap_vs_temp",                                "Table with Temperature and Capacity % as columns",        "[[C,%]]",  "",                     "BatteryCell",       "*",                           "",                             "" },

        // lifetime inputs
        { SSC_INPUT,		SSC_MATRIX,      "cycling_matrix",                             "Table with DOD %, Cycle #, and Capacity % columns",       "[[%, #, %]]","",                     "BatteryCell",       "*",                           "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "calendar_choice",                            "Calendar life degradation input option",                  "0/1/2",    "0=None,1=LithiomIonModel,2=InputLossTable",  "BatteryCell",       "*",       "",                             "" },
        { SSC_INPUT,        SSC_MATRIX,      "calendar_matrix",                            "Table with Day # and Capacity % columns",                 "[[#, %]]", "",                     "BatteryCell",       "calendar_choice=2",        "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "calendar_q0",                                "Calendar life model initial capacity cofficient",         "",         "",                     "BatteryCell",       "calendar_choice=1",        "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "calendar_a",                                 "Calendar life model coefficient",                         "1/sqrt(day)","",                   "BatteryCell",       "calendar_choice=1",        "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "calendar_b",                                 "Calendar life model coefficient",                         "K",        "",                     "BatteryCell",       "calendar_choice=1",        "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "calendar_c",                                 "Calendar life model coefficient",                         "K",        "",                     "BatteryCell",       "calendar_choice=1",        "",                             "" },

        // losses
        { SSC_INPUT,        SSC_NUMBER,      "loss_choice",                                "Loss power input option",                                 "0/1",        "0=Monthly,1=TimeSeries", "BatterySystem",       "?=0",                        "",                             "" },
        { SSC_INPUT,        SSC_ARRAY,       "monthly_charge_loss",                        "Battery system losses when charging",                     "[kW]",       "",                     "BatterySystem",       "?=0",                        "",                             "" },
        { SSC_INPUT,        SSC_ARRAY,       "monthly_discharge_loss",                     "Battery system losses when discharging",                  "[kW]",       "",                     "BatterySystem",       "?=0",                        "",                             "" },
        { SSC_INPUT,        SSC_ARRAY,       "monthly_idle_loss",                          "Battery system losses when idle",                         "[kW]",       "",                     "BatterySystem",       "?=0",                        "",                             "" },
        { SSC_INPUT,        SSC_ARRAY,       "schedule_loss",                              "Battery system losses at each timestep",                  "[kW]",       "",                     "BatterySystem",       "?=0",                        "",                             "" },

        // replacement inputs
        { SSC_INPUT,        SSC_NUMBER,      "replacement_option",                         "Replacements: none (1), by capacity (1), or schedule (2)", "0=none,1=capacity limit,2=yearly schedule", "", "BatterySystem", "?=0",                  "INTEGER,MIN=0,MAX=2",          "" },
        { SSC_INPUT,        SSC_NUMBER,      "replacement_capacity",                       "Capacity degradation at which to replace battery",       "%",        "",                     "BatterySystem",       "replacement_option=1",                           "",                             "" },
        { SSC_INPUT,        SSC_ARRAY,       "replacement_schedule",                       "Battery bank number of replacements in each year",       "[number/year]","length <= analysis_period",                  "BatterySystem",      "replacement_option=2",   "",                             "" },
        { SSC_INPUT,        SSC_ARRAY,       "replacement_schedule_percent",               "Percentage of battery capacity to replace in each year", "[%/year]","length <= analysis_period",                  "BatterySystem",      "replacement_option=2",   "",                             "" },
        var_info_invalid
};

var_info vtab_battery_state[] = {
        // battery pack
        { SSC_INOUT,        SSC_NUMBER,     "last_idx",                  "Last index (lifetime)",                                    "",          "",                     "BatteryState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "I",                         "Current",                                                  "A",         "",                     "BatteryState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "V",                         "Voltage",                                                  "V",         "",                     "BatteryState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "P",                         "Power",                                                    "kW",        "",                     "BatteryState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "Q",                         "Capacity",                                                 "Ah",        "",                     "BatteryState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "Q_max",                     "Max Capacity",                                             "Ah",        "",                     "BatteryState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "P_dischargeable",           "Estimated max dischargeable power",                        "kW",        "",                     "BatteryState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "P_chargeable",              "Estimated max chargeable power ",                          "kW",        "",                     "BatteryState",       "",                           "",                               ""  },

        // capacity
        { SSC_INOUT,        SSC_NUMBER,     "q0",                        "Cell capacity at timestep",                                "Ah",        "",                     "CapacityState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "qmax_lifetime",             "Maximum possible cell capacity",                           "Ah",        "",                     "CapacityState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "qmax_thermal",              "Maximum cell capacity adjusted for temperature effects",   "Ah",        "",                     "CapacityState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "cell_current",              "Cell current",                                             "A",         "",                     "CapacityState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "I_loss",                    "Lifetime and thermal losses",                              "A",         "",                     "CapacityState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "SOC",                       "State of Charge",                                          "%",         "",                     "CapacityState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "DOD",                       "Depth of Discharge percent",                               "%",         "",                     "CapacityState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "DOD_prev",                  "Depth of Discharge percent of last time step",             "%",         "",                     "CapacityState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "charge_mode",               "Charge (0), Idle (1), Discharge (2)",                      "0/1/2",     "",                     "CapacityState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "prev_charge",               "Charge mode of last time step",                            "0/1/2",     "",                     "CapacityState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "chargeChange",              "Whether Charge mode changed since last step",              "0/1",       "",                     "CapacityState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "q1_0",                      "Lead acid - Cell charge available",                        "Ah",        "",                     "CapacityState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "q2_0",                      "Lead acid - Cell charge bound",                            "Ah",        "",                     "CapacityState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "qn",                        "Lead acid - Cell capacity at n-hr discharge rate",         "Ah",        "",                     "CapacityState",       "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "q2",                        "Lead acid - Cell capacity at 10-hr discharge rate",        "Ah",        "",                     "CapacityState",       "",                           "",                               ""  },

        // voltage
        { SSC_INOUT,        SSC_NUMBER,     "cell_voltage",              "Cell voltage",                                             "V",         "",                     "VoltageState",        "",                           "",                               ""  },

        // thermal
        { SSC_INOUT,        SSC_NUMBER,     "q_relative_thermal",        "Relative capacity due to thermal effects",                 "Ah",        "",                     "ThermalState",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "T_batt",                    "Battery temperature averaged over time step",              "C",         "",                     "ThermalState",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "T_room",                    "Room temperature",                                         "C",         "",                     "ThermalState",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "T_batt_prev",               "Battery temperature at end of last time step",             "C",         "",                     "ThermalState",        "",                           "",                               ""  },

        // lifetime
        { SSC_INOUT,        SSC_NUMBER,     "q_relative",                "Overall relative capacity due to lifetime effects",        "Ah",        "",                     "LifetimeState",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "q_relative_cycle",          "Relative capacity due to cycling effects",                 "%",         "",                     "LifetimeState",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "n_cycles",                  "Number of cycles",                                         "",          "",                     "LifetimeState",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "range",                     "Cycle range",                                              "%",         "",                     "LifetimeState",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "average_range",             "Average cycle range",                                      "%",         "",                     "LifetimeState",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "rainflow_Xlt",              "Rainflow range of second to last half cycle",              "%",         "",                     "LifetimeState",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "rainflow_Ylt",              "Rainflow range of last half cycle",                        "%",         "",                     "LifetimeState",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "rainflow_jlt",              "Rainflow number of turning points",                        "",          "",                     "LifetimeState",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_ARRAY,      "rainflow_peaks",            "Rainflow peaks of DOD",                                    "[%]",       "",                     "LifetimeState",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "q_relative_calendar",       "Relative capacity due to calendar effects",                "%",         "",                     "LifetimeState",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "day_age_of_battery",        "Day age of battery",                                       "day",       "",                     "LifetimeState",        "",                           "",                               ""  },
        { SSC_INOUT,        SSC_NUMBER,     "dq_relative_calendar_old",  "Change in capacity of last time step",                     "%",         "",                     "LifetimeState",        "",                           "",                               ""  },

        // losses
        { SSC_INOUT,        SSC_NUMBER,     "loss_percent",              "Power loss percent",                                       "%",         "",                     "LossesState",          "",                           "",                               ""  },

        // replacements
        { SSC_INOUT,        SSC_NUMBER,     "n_replacements",            "Number of replacements at current year",                   "",         "",                      "ReplacementState",     "",                           "",                               ""  },
        { SSC_INOUT,        SSC_ARRAY,      "indices_replaced",          "Lifetime indices of replacement occurrences",              "",         "",                      "ReplacementState",     "",                           "",                               ""  },

        var_info_invalid };

void write_battery_state(const battery_state& state, var_table* vt) {
    vt->assign_match_case("last_idx", (int)state.last_idx);
    vt->assign_match_case("I", state.I);
    vt->assign_match_case("V", state.V);
    vt->assign_match_case("P", state.P);
    vt->assign_match_case("Q", state.Q);
    vt->assign_match_case("Q_max", state.Q_max);
    vt->assign_match_case("P_dischargeable", state.P_dischargeable);
    vt->assign_match_case("P_chargeable", state.P_chargeable);

    auto cap = state.capacity;
    vt->assign_match_case("q0", cap->q0);
    vt->assign_match_case("qmax_lifetime", cap->qmax_lifetime);
    vt->assign_match_case("qmax_thermal", cap->qmax_thermal);
    vt->assign_match_case("cell_current", cap->cell_current);
    vt->assign_match_case("I_loss", cap->I_loss);
    vt->assign_match_case("SOC", cap->SOC);
    vt->assign_match_case("DOD", cap->DOD);
    vt->assign_match_case("DOD_prev", cap->DOD_prev);
    vt->assign_match_case("charge_mode", cap->charge_mode);
    vt->assign_match_case("prev_charge", cap->prev_charge);
    vt->assign_match_case("chargeChange", cap->chargeChange);
    vt->assign_match_case("q1_0", cap->leadacid.q1_0);
    vt->assign_match_case("q2_0", cap->leadacid.q2_0);
    vt->assign_match_case("qn", cap->leadacid.q1);
    vt->assign_match_case("q2", cap->leadacid.q2);

    vt->assign_match_case("cell_voltage", state.voltage->cell_voltage);

    auto thermal = state.thermal;
    vt->assign_match_case("q_relative_thermal", thermal->q_relative_thermal);
    vt->assign_match_case("T_batt", thermal->T_batt);
    vt->assign_match_case("T_room", thermal->T_room);
    vt->assign_match_case("T_batt_prev", thermal->T_batt_prev);

    auto lifetime = state.lifetime;
    vt->assign_match_case("q_relative", lifetime->q_relative);
    vt->assign_match_case("q_relative_cycle", lifetime->cycle->q_relative_cycle);
    vt->assign_match_case("n_cycles", lifetime->cycle->n_cycles);
    vt->assign_match_case("range", lifetime->cycle->range);
    vt->assign_match_case("average_range", lifetime->cycle->average_range);
    vt->assign_match_case("rainflow_Xlt", lifetime->cycle->rainflow_Xlt);
    vt->assign_match_case("rainflow_Ylt", lifetime->cycle->rainflow_Ylt);
    vt->assign_match_case("rainflow_jlt", lifetime->cycle->rainflow_jlt);
    vt->assign_match_case("rainflow_peaks", lifetime->cycle->rainflow_peaks);
    vt->assign_match_case("q_relative_calendar", lifetime->calendar->q_relative_calendar);
    vt->assign_match_case("rainflow_Ylt", lifetime->calendar->day_age_of_battery);
    vt->assign_match_case("rainflow_Ylt", lifetime->calendar->dq_relative_calendar_old);

    vt->assign_match_case("loss_percent", state.losses->loss_percent);

    vt->assign_match_case("n_replacements", state.replacement->n_replacements);
    vt->assign_match_case( "indices_replaced", state.replacement->indices_replaced);
}

void read_battery_state(battery_state& state, var_table* vt) {
    vt_get_uint(vt, "last_idx", &state.last_idx);
    vt_get_number(vt, "I", &state.I);
    vt_get_number(vt, "V", &state.V);
    vt_get_number(vt, "P", &state.P);
    vt_get_number(vt, "Q", &state.Q);
    vt_get_number(vt, "Q_max", &state.Q_max);
    vt_get_number(vt, "P_dischargeable", &state.P_dischargeable);
    vt_get_number(vt, "P_chargeable", &state.P_chargeable);

    auto cap = state.capacity;
    vt_get_number(vt, "q0", &cap->q0);
    vt_get_number(vt, "qmax_lifetime", &cap->qmax_lifetime);
    vt_get_number(vt, "qmax_thermal", &cap->qmax_thermal);
    vt_get_number(vt, "cell_current", &cap->cell_current);
    vt_get_number(vt, "I_loss", &cap->I_loss);
    vt_get_number(vt, "SOC", &cap->SOC);
    vt_get_number(vt, "DOD", &cap->DOD);
    vt_get_number(vt, "DOD_prev", &cap->DOD_prev);
    vt_get_int(vt, "charge_mode", &cap->charge_mode);
    vt_get_int(vt, "prev_charge", &cap->prev_charge);
    vt_get_bool(vt, "chargeChange", &cap->chargeChange);
    vt_get_number(vt, "q1_0", &cap->leadacid.q1_0);
    vt_get_number(vt, "q2_0", &cap->leadacid.q2_0);
    vt_get_number(vt, "qn", &cap->leadacid.q1);
    vt_get_number(vt, "q2", &cap->leadacid.q2);

    vt_get_number(vt, "cell_voltage", &state.voltage->cell_voltage);

    auto thermal = state.thermal;
    vt_get_number(vt, "q_relative_thermal", &thermal->q_relative_thermal);
    vt_get_number(vt, "T_batt", &thermal->T_batt);
    vt_get_number(vt, "T_room", &thermal->T_room);
    vt_get_number(vt, "T_batt_prev", &thermal->T_batt_prev);

    auto lifetime = state.lifetime;
    vt_get_number(vt, "q_relative", &lifetime->q_relative);
    vt_get_number(vt, "q_relative_cycle", &lifetime->cycle->q_relative_cycle);
    vt_get_int(vt, "n_cycles", &lifetime->cycle->n_cycles);
    vt_get_number(vt, "range", &lifetime->cycle->range);
    vt_get_number(vt, "average_range", &lifetime->cycle->average_range);
    vt_get_number(vt, "rainflow_Xlt", &lifetime->cycle->rainflow_Xlt);
    vt_get_number(vt, "rainflow_Ylt", &lifetime->cycle->rainflow_Ylt);
    vt_get_int(vt, "rainflow_jlt", &lifetime->cycle->rainflow_jlt);
    vt_get_array_vec(vt, "rainflow_peaks", lifetime->cycle->rainflow_peaks);
    vt_get_number(vt, "q_relative_calendar", &lifetime->calendar->q_relative_calendar);
    vt_get_int(vt, "rainflow_Ylt", &lifetime->calendar->day_age_of_battery);
    vt_get_number(vt, "rainflow_Ylt", &lifetime->calendar->dq_relative_calendar_old);

    vt_get_number(vt, "loss_percent", &state.losses->loss_percent);

    vt_get_int(vt, "n_replacements", &state.replacement->n_replacements);
    vt_get_array_vec(vt, "indices_replaced", state.replacement->indices_replaced);
}

std::shared_ptr<battery_params> create_battery_params(var_table *vt, double dt_hr) {
    auto params = std::make_shared<battery_params>();
    int chem;
    vt_get_int(vt, "chem", &chem);
    params->chem = static_cast<battery_params::CHEM>(chem);

    // voltage
    auto voltage = params->voltage;
    int choice;
    vt_get_int(vt, "voltage_choice", &choice);
    voltage->voltage_choice = static_cast<voltage_params::MODE>(choice);
    voltage->dt_hr = dt_hr;
    vt_get_int(vt, "num_cells_series", &voltage->num_cells_series);
    vt_get_int(vt, "num_strings", &voltage->num_strings);
    vt_get_number(vt, "Vnom_default", &voltage->Vnom_default);
    vt_get_number(vt, "resistance", &voltage->resistance);

    if (voltage->voltage_choice == voltage_params::TABLE || params->chem == battery_params::IRON_FLOW) {
        vt_get_matrix_vec(vt, "voltage_matrix", voltage->voltage_table);
    }
    else {
        if (params->chem == battery_params::LEAD_ACID  || params->chem == battery_params::LITHIUM_ION) {
            vt_get_number(vt, "Vfull", &voltage->dynamic.Vfull);
            vt_get_number(vt, "Vexp", &voltage->dynamic.Vexp);
            vt_get_number(vt, "Qfull", &voltage->dynamic.Qfull);
            vt_get_number(vt, "Qexp", &voltage->dynamic.Qexp);
            vt_get_number(vt, "Qnom", &voltage->dynamic.Qnom);
            vt_get_number(vt, "C_rate", &voltage->dynamic.C_rate);
        }
    }

    // capacity
    auto capacity = params->capacity;
    vt_get_number(vt, "initial_SOC", &capacity->initial_SOC);
    vt_get_number(vt, "maximum_soc", &capacity->maximum_SOC);
    vt_get_number(vt, "minimum_soc", &capacity->minimum_SOC);
    params->dt_hour = dt_hr;
    if (params->chem == battery_params::LEAD_ACID) {
        vt_get_number(vt, "leadacid_tn", &capacity->leadacid.tn);
        vt_get_number(vt, "leadacid_qn", &capacity->leadacid.qn);
        vt_get_number(vt, "leadacid_q10", &capacity->leadacid.q10);
        vt_get_number(vt, "leadacid_q20", &capacity->leadacid.q20);
    }
    else if (chem == battery_params::LITHIUM_ION)
    {
        capacity->qmax_init = voltage->dynamic.Qfull * voltage->num_strings;
    }
    else if (chem == battery_params::VANADIUM_REDOX || chem == battery_params::IRON_FLOW)
    {
        capacity->qmax_init = voltage->dynamic.Qfull;
    }

    // lifetime
    auto lifetime = params->lifetime;
    vt_get_int(vt, "calendar_choice", &choice);
    lifetime->calendar_choice = static_cast<lifetime_params::CALENDAR_CHOICE>(choice);
    lifetime->dt_hour = dt_hr;
    vt_get_matrix(vt, "cycling_matrix", lifetime->cycling_matrix);
    if (lifetime->calendar_choice == lifetime_params::CALENDAR_CHOICE::MODEL) {
        vt_get_number(vt, "calendar_q0", &lifetime->calendar_q0);
        vt_get_number(vt, "calendar_a", &lifetime->calendar_a);
        vt_get_number(vt, "calendar_b", &lifetime->calendar_b);
        vt_get_number(vt, "calendar_c", &lifetime->calendar_c);
    }
    else if (lifetime->calendar_choice == lifetime_params::CALENDAR_CHOICE::TABLE) {
        vt_get_matrix(vt, "calendar_matrix", lifetime->calendar_matrix);
    }

    // thermal
    auto thermal = params->thermal;
    thermal->dt_hour = dt_hr;
    thermal->option = thermal_params::VALUE;
    thermal->resistance = params->voltage->resistance;
    vt_get_number(vt, "mass", &thermal->mass);
    vt_get_number(vt, "surface_area", &thermal->surface_area);
    vt_get_number(vt, "Cp", &thermal->Cp);
    vt_get_number(vt, "h", &thermal->h);
    vt_get_number(vt, "T_room_init", &thermal->T_room_init);
    vt_get_matrix(vt, "cap_vs_temp", thermal->cap_vs_temp);

    // losses
    auto losses = params->losses;
    vt_get_int(vt, "loss_choice", &choice);
    losses->loss_choice = static_cast<losses_params::OPTIONS>(choice);
    if (losses->loss_choice == losses_params::MONTHLY) {
        vt_get_array_vec(vt, "monthly_charge_loss", losses->monthly_charge_loss);
        vt_get_array_vec(vt, "monthly_discharge_loss", losses->monthly_discharge_loss);
        vt_get_array_vec(vt, "monthly_idle_loss", losses->monthly_idle_loss);
    }
    else if (losses->loss_choice == losses_params::SCHEDULE) {
        vt_get_array_vec(vt, "schedule_loss", losses->schedule_loss);
    }

    // replacements
    auto replacements = params->replacement;
    vt_get_int(vt, "replacement_option", &choice);
    replacements->replacement_option = static_cast<replacement_params::OPTIONS>(choice);
    if (replacements->replacement_option == replacement_params::SCHEDULE) {
        vt_get_array_vec(vt, "replacement_schedule", replacements->replacement_schedule);
        vt_get_array_vec(vt, "replacement_schedule_percent", replacements->replacement_schedule_percent);
    }
    else if (replacements->replacement_option == replacement_params::CAPACITY_PERCENT) {
        vt_get_number(vt, "replacement_capacity", &replacements->replacement_capacity);
    }

    return params;
}

cm_battery_stateful::cm_battery_stateful():
        dt_hour(0),
        control_mode(0){
    add_var_info(vtab_battery_stateful_inputs);
    add_var_info(vtab_battery_state);
}

cm_battery_stateful::cm_battery_stateful(var_table* vt) :
        cm_battery_stateful() {
    m_vartab = vt;
    try {
        if (!compute_module::verify("precheck input", SSC_INPUT))
            throw exec_error("battery_stateful", log(0)->text);
        dt_hour = as_number("dt_hr");
        control_mode = as_number("control_mode");
        params = create_battery_params(m_vartab, dt_hour);
        battery = std::unique_ptr<battery_t>(new battery_t(params));
        write_battery_state(battery->get_state(), m_vartab);
    }
    catch (std::exception& e) {
        throw runtime_error(e.what());
    }
}

void cm_battery_stateful::exec() {
    if (!battery)
        throw exec_error("battery_stateful", "Battery model must be initialized first.");

    if (as_boolean("run_sequentially")) {
        battery_state state;
        try {
            read_battery_state(state, m_vartab);
            battery->set_state(state);
        }
        catch (std::exception& e) {
            std::string err = "battery_stateful error: Could not read state. ";
            err += e.what();
            throw runtime_error(err);
        }
    }

    if (static_cast<MODE>(as_integer("control_mode")) == MODE::CURRENT) {
        double I = as_number("input_current");
        battery->runCurrent(I);
    }
    else {
        double P = as_number("input_power");
        battery->runPower(P);
    }
    write_battery_state(battery->get_state(), m_vartab);
}

DEFINE_STATEFUL_MODULE_ENTRY(battery_stateful, "Battery management system model with state", 1)
