/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#include <string>
#include "common.h"
#include "vartab.h"
#include "lib_weatherfile.h"
#include "lib_time.h"
#include "lib_resilience.h"
#include "lib_util.h"

var_info vtab_standard_financial[] = {
{ SSC_INPUT,SSC_NUMBER  , "analysis_period"                      , "Analyis period"                                                 , "years"                                  , ""                                      , "Financial Parameters" , "?=30"           , "INTEGER,MIN=0,MAX=50"  , ""},
{ SSC_INPUT, SSC_ARRAY  , "federal_tax_rate"                     , "Federal income tax rate"                                        , "%"                                      , ""                                      , "Financial Parameters" , "*"              , ""                      , ""},
{ SSC_INPUT, SSC_ARRAY  , "state_tax_rate"                       , "State income tax rate"                                          , "%"                                      , ""                                      , "Financial Parameters" , "*"              , ""                      , ""},

{ SSC_OUTPUT, SSC_ARRAY , "cf_federal_tax_frac"                  , "Federal income tax rate"                                        , "frac"                                   , ""                                      , "Financial Parameters" , "*"              , "LENGTH_EQUAL=cf_length", ""},
{ SSC_OUTPUT, SSC_ARRAY , "cf_state_tax_frac"                    , "State income tax rate"                                          , "frac"                                   , ""                                      , "Financial Parameters" , "*"              , "LENGTH_EQUAL=cf_length", ""},
{ SSC_OUTPUT, SSC_ARRAY , "cf_effective_tax_frac"                , "Effective income tax rate"                                      , "frac"                                   , ""                                      , "Financial Parameters" , "*"              , "LENGTH_EQUAL=cf_length", ""},


{ SSC_INPUT, SSC_NUMBER , "property_tax_rate"                    , "Property tax rate"                                              , "%"                                      , ""                                      , "Financial Parameters" , "?=0.0"          , "MIN=0,MAX=100"         , ""},
{ SSC_INPUT,SSC_NUMBER  , "prop_tax_cost_assessed_percent"       , "Percent of pre-financing costs assessed"                        , "%"                                      , ""                                      , "Financial Parameters" , "?=95"           , "MIN=0,MAX=100"         , ""},
{ SSC_INPUT,SSC_NUMBER  , "prop_tax_assessed_decline"            , "Assessed value annual decline"                                  , "%"                                      , ""                                      , "Financial Parameters" , "?=5"            , "MIN=0,MAX=100"         , ""},
{ SSC_INPUT,SSC_NUMBER  , "real_discount_rate"                   , "Real discount rate"                                             , "%"                                      , ""                                      , "Financial Parameters" , "*"              , "MIN=-99"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "inflation_rate"                       , "Inflation rate"                                                 , "%"                                      , ""                                      , "Financial Parameters" , "*"              , "MIN=-99"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "insurance_rate"                       , "Insurance rate"                                                 , "%"                                      , ""                                      , "Financial Parameters" , "?=0.0"          , "MIN=0,MAX=100"         , ""},

{ SSC_INPUT,SSC_NUMBER  , "system_capacity"                      , "System nameplate capacity"                                      , "kW"                                     , ""                                      , "Financial Parameters" , "*"              , "POSITIVE"              , ""},
{ SSC_INPUT,SSC_NUMBER  , "system_heat_rate"                     , "System heat rate"                                               , "MMBTus/MWh"                             , ""                                      , "Financial Parameters" , "?=0.0"          , "MIN=0"                 , ""},
var_info_invalid };

var_info vtab_battery_replacement_cost[] = {
{ SSC_INPUT, SSC_NUMBER , "en_batt"                              , "Enable battery storage model"                                   , "0/1"                                    , ""                                      , "BatterySystem"              , "?=0"            , ""                      , ""},
{ SSC_INPUT, SSC_NUMBER , "en_standalone_batt"                   , "Enable standalone battery storage model"                                   , "0/1"                                    , ""                                      , "BatterySystem"              , "?=0"            , ""                      , ""},
{ SSC_INPUT, SSC_NUMBER , "en_wave_batt"                   , "Enable standalone battery storage model"                                   , "0/1"                                    , ""                                      , "BatterySystem"              , "?=0"            , ""                      , ""},
{ SSC_INOUT, SSC_ARRAY  , "batt_bank_replacement"                , "Battery bank replacements per year"                             , "number/year"                            , ""                                      , "BatterySystem"              , ""               , ""                      , ""},
{ SSC_INPUT, SSC_ARRAY  , "batt_replacement_schedule_percent"    , "Percentage of battery capacity to replace in each year"         , "%"                                      , "length <= analysis_period"             , "BatterySystem"              , ""               , ""                      , ""},
{ SSC_INPUT, SSC_NUMBER , "batt_replacement_option"              , "Enable battery replacement?"                                    , "0=none,1=capacity based,2=user schedule", ""                                      , "BatterySystem"              , "?=0"            , "INTEGER,MIN=0,MAX=2"   , ""},
{ SSC_INPUT, SSC_NUMBER , "battery_per_kWh"                      , "Battery cost"                                                   , "$/kWh"                                  , ""                                      , "BatterySystem"              , "?=0.0"          , ""                      , ""},
{ SSC_INPUT, SSC_NUMBER , "batt_computed_bank_capacity"          , "Battery bank capacity"                                          , "kWh"                                    , ""                                      , "BatterySystem"              , "?=0.0"          , ""                      , ""},
{ SSC_OUTPUT, SSC_ARRAY , "cf_battery_replacement_cost"          , "Battery replacement cost"                                       , "$"                                      , ""                                      , "Cash Flow"            , ""              , ""                      , ""},
{ SSC_OUTPUT, SSC_ARRAY , "cf_battery_replacement_cost_schedule" , "Battery replacement cost schedule"                              , "$"                                  , ""                                      , "Cash Flow"            , ""              , ""                      , ""},
var_info_invalid };

var_info vtab_financial_grid[] = {

/*   VARTYPE           DATATYPE         NAME                                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS	*/
{ SSC_INPUT,        SSC_ARRAY,      "grid_curtailment_price",                           "Curtailment price",                                  "$/kWh",  "",                      "GridLimits",      "?=0",                   "",          "" },
{ SSC_INPUT,        SSC_NUMBER,      "grid_curtailment_price_esc",                           "Curtailment price escalation",                                  "%",  "",           "GridLimits",      "?=0",                   "",          "" },
{ SSC_INPUT,        SSC_NUMBER,      "annual_energy_pre_curtailment_ac", "Annual Energy AC pre-curtailment (year 1)",                   "kWh",        "",                   "System Output",               "?=0",                     "",                              "" },

var_info_invalid };

var_info vtab_fuelcell_replacement_cost[] = {
{ SSC_INPUT, SSC_ARRAY  , "fuelcell_replacement"                 , "Fuel cell replacements per year"                                , "number/year"                            , ""                                      , "Fuel Cell"            , ""               , ""                      , ""},
{ SSC_INPUT, SSC_ARRAY  , "fuelcell_replacement_schedule"        , "Fuel cell replacements per year (user specified)"               , "number/year"                            , ""                                      , "Fuel Cell"            , ""               , ""                      , ""},
{ SSC_INPUT, SSC_NUMBER , "en_fuelcell"                          , "Enable fuel cell storage model"                                 , "0/1"                                    , ""                                      , "Fuel Cell"            , "?=0"            , ""                      , ""},
{ SSC_INPUT, SSC_NUMBER , "fuelcell_replacement_option"          , "Enable fuel cell replacement?"                                  , "0=none,1=capacity based,2=user schedule", ""                                      , "Fuel Cell"            , ""            , "INTEGER,MIN=0,MAX=2"   , ""},
{ SSC_INPUT, SSC_NUMBER , "fuelcell_per_kWh"                     , "Fuel cell cost"                                                 , "$/kWh"                                  , ""                                      , "Fuel Cell"            , "?=0.0"          , ""                      , ""},
{ SSC_INPUT, SSC_NUMBER , "fuelcell_computed_bank_capacity"      , "Fuel cell capacity"                                             , "kWh"                                    , ""                                      , "Fuel Cell"            , "?=0.0"          , ""                      , ""},
{ SSC_OUTPUT, SSC_ARRAY , "cf_fuelcell_replacement_cost"         , "Fuel cell replacement cost"                                     , "$"                                      , ""                                      , "Cash Flow"            , ""              , ""                      , ""},
{ SSC_OUTPUT, SSC_ARRAY , "cf_fuelcell_replacement_cost_schedule", "Fuel cell replacement cost schedule"                            , "$/kW"                                   , ""                                      , "Cash Flow"            , ""              , ""                      , ""},
var_info_invalid };

var_info vtab_standard_loan[] = {
{ SSC_INPUT,SSC_NUMBER  , "loan_term"                            , "Loan term"                                                      , "years"                                  , ""                                      , "Financial Parameters" , "?=0"            , "INTEGER,MIN=0,MAX=50"  , ""},
{ SSC_INPUT,SSC_NUMBER  , "loan_rate"                            , "Loan rate"                                                      , "%"                                      , ""                                      , "Financial Parameters" , "?=0"            , "MIN=0,MAX=100"         , ""},
{ SSC_INPUT,SSC_NUMBER  , "debt_fraction"                        , "Debt percentage"                                                , "%"                                      , ""                                      , "Financial Parameters" , "?=0"            , "MIN=0,MAX=100"         , ""},
var_info_invalid };


var_info vtab_oandm_heat[] = {
    /*   VARTYPE           DATATYPE         NAME                             LABEL                                UNITS      META                 GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

    { SSC_INPUT,        SSC_ARRAY,       "om_fixed",                     "Fixed O&M annual amount",           "$/year",  "",                  "System Costs",            "?=0.0",                 "",                                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "om_fixed_escal",               "Fixed O&M escalation",              "%/year",  "",                  "System Costs",            "?=0.0",                 "",                                         "" },
    { SSC_INPUT,        SSC_ARRAY,       "om_production_heat",                "Production-based O&M amount",       "$/MWht",   "",                  "System Costs",            "?=0.0",                 "",                                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "om_production_escal",          "Production-based O&M escalation",   "%/year",  "",                  "System Costs",            "?=0.0",                 "",                                         "" },
    { SSC_INPUT,        SSC_ARRAY,       "om_capacity_heat",                  "Capacity-based O&M amount",         "$/kWt-yr", "",                  "System Costs",            "?=0.0",                 "",                                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "om_capacity_escal",            "Capacity-based O&M escalation",     "%/year",  "",                  "System Costs",            "?=0.0",                 "",                                         "" },
    { SSC_INPUT,        SSC_ARRAY,		 "om_fuel_cost",                 "Fuel cost",                         "$/MMBtu", "generic_system,fuelcell,tcslinearfresnel,tcstroughempirical,tcsgenericsolar,fresnelphysical",                  "System Costs",            "?=0.0",                 "",                                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "om_fuel_cost_escal",           "Fuel cost escalation",              "%/year",  "generic_system,fuelcell,tcslinearfresnel,tcstroughempirical,tcsgenericsolar,fresnelphysical",                  "System Costs",            "?=0.0",                 "",                                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "annual_fuel_usage",            "Fuel usage (yr 1)",                 "kWht",    "generic_system,fuelcell,tcslinearfresnel,tcstroughempirical,tcsgenericsolar,fresnelphysical",                  "System Costs",            "?=0",                     "MIN=0",                                         "" },
    { SSC_INPUT,        SSC_ARRAY,       "annual_fuel_usage_lifetime",   "Fuel usage (lifetime)",             "kWht",    "generic_system,fuelcell,tcslinearfresnel,tcstroughempirical,tcsgenericsolar,fresnelphysical",                  "System Costs",            "",                     "",                                         "" },

    { SSC_INPUT,        SSC_ARRAY,		 "om_elec_price_for_heat_techs",       "Electricity price for purchases in heat model",                "$/kWh",   "", "System Costs",  "?=0.0",           "",                                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "om_elec_price_for_heat_techs_escal", "Escalation for electricity price for purchases in heat model", "%/year",  "", "System Costs",  "?=0.0",           "",                                         "" },


    // replacements
    { SSC_INPUT,SSC_ARRAY   , "om_batt_replacement_cost"                 , "Replacement cost 1"               , "$/kWh"   , "battery"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_ARRAY   , "om_fuelcell_replacement_cost"                 , "Replacement cost 2"            , "$/kW", "fuelcell"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_NUMBER  , "om_replacement_cost_escal"            , "Replacement cost escalation"           , "%/year", "battery,fuelcell"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},

    // optional fuel o and m for Biopower - usage can be in any unit and cost is in $ per usage unit
    { SSC_INPUT,SSC_NUMBER  , "om_opt_fuel_1_usage"                  , "Biomass feedstock usage"            , "unit"    , "biomass"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_ARRAY   , "om_opt_fuel_1_cost"                   , "Biomass feedstock cost"           , "$/unit"    , "biomass"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_NUMBER  , "om_opt_fuel_1_cost_escal"             , "Biomass feedstock cost escalation" , "%/year"   , "biomass"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_NUMBER  , "om_opt_fuel_2_usage"                  , "Coal feedstock usage"               , "unit"    , "biomass"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_ARRAY   , "om_opt_fuel_2_cost"                   , "Coal feedstock cost"                , "$/unit"  , "biomass"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_NUMBER  , "om_opt_fuel_2_cost_escal"             , "Coal feedstock cost escalation"     , "%/year"  , "biomass"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},

    // optional additional base o and m types
    { SSC_INPUT,SSC_NUMBER  , "add_om_num_types"                     , "Number of O and M types" , "" , "battery,fuelcell"                                      , "System Costs"         , "?=0"            , "INTEGER,MIN=0,MAX=2"   , ""},
    { SSC_INPUT,SSC_NUMBER  , "om_batt_nameplate"               , "Battery capacity for System Costs values"                       , "kW", "battery"                                      , "System Costs"         , "?=0"            , ""                      , ""},
    { SSC_INPUT,SSC_ARRAY   , "om_production1_values"                , "Battery production for System Costs values"                     , "kWh", "battery"                                      , "System Costs"         , "?=0"            , ""                      , ""},

    { SSC_INPUT,SSC_ARRAY   , "om_batt_fixed_cost"                            , "Battery fixed System Costs annual amount"                       , "$/year",    "battery",   "System Costs"         , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_ARRAY   , "om_batt_variable_cost"                       , "Battery production-based System Costs amount"                   , "$/MWh", "battery"    , "System Costs"         , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_ARRAY   , "om_batt_capacity_cost"                         , "Battery capacity-based System Costs amount"                     , "$/kWcap", "battery"    , "System Costs"         , "?=0.0"          , ""                      , ""},

    { SSC_INPUT,SSC_NUMBER  , "om_fuelcell_nameplate"               , "Fuel cell capacity for System Costs values"                     , "kW", "fuelcell"                                      , "System Costs"         , "?=0"            , ""                      , ""},
    { SSC_INPUT,SSC_ARRAY   , "om_production2_values"                , "Fuel cell production for System Costs values"                   , "kWh", "fuelcell"                                      , "System Costs"         , "?=0"            , ""                      , ""},

    { SSC_INPUT,SSC_ARRAY   , "om_fuelcell_fixed_cost"                            , "Fuel cell fixed System Costs annual amount"                     , "$/year", "fuelcell" , "System Costs"         , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_ARRAY   , "om_fuelcell_variable_cost"                       , "Fuel cell production-based System Costs amount"                 , "$/MWh", "fuelcell"    , "System Costs"         , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_ARRAY   , "om_fuelcell_capacity_cost"                         , "Fuel cell capacity-based System Costs amount"                   , "$/kWcap", "fuelcell"    , "System Costs"         , "?=0.0"          , ""                      , ""},

    // optional land lease
    { SSC_INPUT,        SSC_NUMBER,     "land_area",                      "Total land area",	                                                "acres",                "",                        "Land Lease",            "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,      "om_land_lease",	                    "Land lease cost",	                                                "$/acre",               "",                        "Land Lease",            "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "om_land_lease_escal",                  "Land lease cost escalation",	                                    "%/yr",                 "",                        "Land Lease",            "?=0",					   "",                              "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "cf_land_lease_expense",                "Land lease expense",                                       "$",                    "",                         "Land Lease",            "", "LENGTH_EQUAL=cf_length", "" },

var_info_invalid };

// meta should be either a blocklist (!gen,!gen) or an allowlist. Cannot do both blocked and allowed gens
var_info vtab_oandm[] = {
/*   VARTYPE           DATATYPE         NAME                             LABEL                                UNITS      META                 GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

{ SSC_INPUT,        SSC_ARRAY,       "om_fixed",                     "Fixed O&M annual amount",           "$/year",  "!battery,!fuelcell",                  "System Costs",            "?=0.0",                 "",                                         "" },
{ SSC_INPUT,        SSC_NUMBER,      "om_fixed_escal",               "Fixed O&M escalation",              "%/year",  "",                  "System Costs",            "?=0.0",                 "",                                         "" },
{ SSC_INPUT,        SSC_ARRAY,       "om_production",                "Production-based O&M amount",       "$/MWh",   "!battery,!fuelcell",                  "System Costs",            "?=0.0",                 "",                                         "" },
{ SSC_INPUT,        SSC_NUMBER,      "om_production_escal",          "Production-based O&M escalation",   "%/year",  "",                  "System Costs",            "?=0.0",                 "",                                         "" },
{ SSC_INPUT,        SSC_ARRAY,       "om_capacity",                  "Capacity-based O&M amount",         "$/kWcap", "!battery,!fuelcell",                  "System Costs",            "?=0.0",                 "",                                         "" },
{ SSC_INPUT,        SSC_NUMBER,      "om_capacity_escal",            "Capacity-based O&M escalation",     "%/year",  "",                  "System Costs",            "?=0.0",                 "",                                         "" },
{ SSC_INPUT,        SSC_ARRAY,		 "om_fuel_cost",                 "Fuel cost",                         "$/MMBtu", "custom_generation,fuelcell,tcslinearfresnel,tcstroughempirical,tcsgenericsolar,fresnelphysical",                  "System Costs",            "?=0.0",                 "",                                         "" },
{ SSC_INPUT,        SSC_NUMBER,      "om_fuel_cost_escal",           "Fuel cost escalation",              "%/year",  "custom_generation,fuelcell,tcslinearfresnel,tcstroughempirical,tcsgenericsolar,fresnelphysical",                  "System Costs",            "?=0.0",                 "",                                         "" },
{ SSC_INPUT,        SSC_NUMBER,      "annual_fuel_usage",            "Fuel usage (yr 1)",                 "kWht",    "custom_generation,fuelcell,tcslinearfresnel,tcstroughempirical,tcsgenericsolar,fresnelphysical",                  "System Costs",            "?=0",                     "MIN=0",                                         "" },
{ SSC_INPUT,        SSC_ARRAY,       "annual_fuel_usage_lifetime",   "Fuel usage (lifetime)",             "kWht",    "custom_generation,fuelcell,tcslinearfresnel,tcstroughempirical,tcsgenericsolar,fresnelphysical",                  "System Costs",            "",                     "",                                         "" },

// replacements
{ SSC_INPUT,SSC_ARRAY   , "om_batt_replacement_cost"                 , "Replacement cost 1"               , "$/kWh"   , "battery"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_ARRAY   , "om_fuelcell_replacement_cost"                 , "Replacement cost 2"            , "$/kW", "fuelcell"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "om_replacement_cost_escal"            , "Replacement cost escalation"           , "%/year", "battery,fuelcell"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},

// optional fuel o and m for Biopower - usage can be in any unit and cost is in $ per usage unit
{ SSC_INPUT,SSC_NUMBER  , "om_opt_fuel_1_usage"                  , "Biomass feedstock usage"            , "unit"    , "biomass"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_ARRAY   , "om_opt_fuel_1_cost"                   , "Biomass feedstock cost"           , "$/unit"    , "biomass"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "om_opt_fuel_1_cost_escal"             , "Biomass feedstock cost escalation" , "%/year"   , "biomass"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "om_opt_fuel_2_usage"                  , "Coal feedstock usage"               , "unit"    , "biomass"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_ARRAY   , "om_opt_fuel_2_cost"                   , "Coal feedstock cost"                , "$/unit"  , "biomass"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "om_opt_fuel_2_cost_escal"             , "Coal feedstock cost escalation"     , "%/year"  , "biomass"                                      , "System Costs"         , "?=0.0"          , ""                      , ""},

// optional additional base o and m types
{ SSC_INPUT,SSC_NUMBER  , "add_om_num_types"                     , "Number of O and M types" , "" , "battery,fuelcell"                                      , "System Costs"         , "?=0"            , "INTEGER,MIN=0,MAX=2"   , ""},
{ SSC_INPUT,SSC_NUMBER  , "om_batt_nameplate"               , "Battery capacity for System Costs values"                       , "kW", "battery"                                      , "System Costs"         , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_ARRAY   , "om_production1_values"                , "Battery production for System Costs values"                     , "kWh", "battery"                                      , "System Costs"         , "?=0"            , ""                      , ""},

{ SSC_INPUT,SSC_ARRAY   , "om_batt_fixed_cost"                            , "Battery fixed System Costs annual amount"                       , "$/year",    "battery",   "System Costs"         , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_ARRAY   , "om_batt_variable_cost"                       , "Battery production-based System Costs amount"                   , "$/MWh", "battery"    , "System Costs"         , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_ARRAY   , "om_batt_capacity_cost"                         , "Battery capacity-based System Costs amount"                     , "$/kWcap", "battery"    , "System Costs"         , "?=0.0"          , ""                      , ""},

{ SSC_INPUT,SSC_NUMBER  , "om_fuelcell_nameplate"               , "Fuel cell capacity for System Costs values"                     , "kW", "fuelcell"                                      , "System Costs"         , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_ARRAY   , "om_production2_values"                , "Fuel cell production for System Costs values"                   , "kWh", "fuelcell"                                      , "System Costs"         , "?=0"            , ""                      , ""},

{ SSC_INPUT,SSC_ARRAY   , "om_fuelcell_fixed_cost"                            , "Fuel cell fixed System Costs annual amount"                     , "$/year", "fuelcell" , "System Costs"         , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_ARRAY   , "om_fuelcell_variable_cost"                       , "Fuel cell production-based System Costs amount"                 , "$/MWh", "fuelcell"    , "System Costs"         , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_ARRAY   , "om_fuelcell_capacity_cost"                         , "Fuel cell capacity-based System Costs amount"                   , "$/kWcap", "fuelcell"    , "System Costs"         , "?=0.0"          , ""                      , ""},

// optional land lease
{ SSC_INPUT,        SSC_NUMBER,     "land_area",                      "Total land area",	                                                "acres",                "",                        "Land Lease",            "?=0",					   "",                              "" },
{ SSC_INPUT,        SSC_ARRAY,      "om_land_lease",	                    "Land lease cost",	                                                "$/acre",               "",                        "Land Lease",            "?=0",					   "",                              "" },
{ SSC_INPUT,        SSC_NUMBER,     "om_land_lease_escal",                  "Land lease cost escalation",	                                    "%/yr",                 "",                        "Land Lease",            "?=0",					   "",                              "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_land_lease_expense",                "Land lease expense",                                       "$",                    "",                         "Land Lease",            "", "LENGTH_EQUAL=cf_length", "" },

var_info_invalid };

var_info vtab_equip_reserve[] = {
{ SSC_INPUT,         SSC_NUMBER,    "reserves_interest",                      "Interest on reserves",				                           "%",	 "",					  "Financial Parameters",             "?=1.75",                     "MIN=0,MAX=100",      			"" },

/* replacement reserve on top of regular o and m */
{ SSC_INPUT,        SSC_NUMBER,     "equip1_reserve_cost",                    "Major equipment reserve 1 cost",	                               "$/W",	        "",				  "Financial Parameters",             "?=0.25",               "MIN=0",                         "" },
{ SSC_INPUT,        SSC_NUMBER,     "equip1_reserve_freq",                    "Major equipment reserve 1 frequency",	                       "years",	 "",			  "Financial Parameters",             "?=12",               "INTEGER,MIN=0",                         "" },
{ SSC_INPUT,        SSC_NUMBER,     "equip2_reserve_cost",                    "Major equipment reserve 2 cost",	                               "$/W",	 "",				  "Financial Parameters",             "?=0",               "MIN=0",                         "" },
{ SSC_INPUT,        SSC_NUMBER,     "equip2_reserve_freq",                    "Major equipment reserve 2 frequency",	                       "years",	 "",			  "Financial Parameters",             "?=15",               "INTEGER,MIN=0",                         "" },
{ SSC_INPUT,        SSC_NUMBER,     "equip3_reserve_cost",                    "Major equipment reserve 3 cost",	                               "$/W",	 "",				  "Financial Parameters",             "?=0",               "MIN=0",                         "" },
{ SSC_INPUT,        SSC_NUMBER,     "equip3_reserve_freq",                    "Major equipment reserve 3 frequency",	                       "years",	 "",			  "Financial Parameters",             "?=20",               "INTEGER,MIN=0",                         "" },

/* major equipment depreciation schedules - can extend to three different schedu  les */
{ SSC_INPUT,        SSC_NUMBER,     "equip_reserve_depr_sta",                 "Major equipment reserve state depreciation",	                   "",	 "0=5yr MACRS,1=15yr MACRS,2=5yr SL,3=15yr SL, 4=20yr SL,5=39yr SL,6=Custom",  "Financial Parameters", "?=0",   "INTEGER,MIN=0,MAX=6",  "" },
{ SSC_INPUT,        SSC_NUMBER,     "equip_reserve_depr_fed",                 "Major equipment reserve federal depreciation",	               "",	 "0=5yr MACRS,1=15yr MACRS,2=5yr SL,3=15yr SL, 4=20yr SL,5=39yr SL,6=Custom",  "Financial Parameters", "?=0",   "INTEGER,MIN=0,MAX=6",  "" },

var_info_invalid
};

var_info vtab_depreciation[] = {
{ SSC_INPUT,SSC_NUMBER  , "depr_fed_type"                        , "Federal depreciation type"                                      , ""                                       , "0=none,1=macrs_half_year,2=sl,3=custom", "Depreciation"         , "?=0"            , "INTEGER,MIN=0,MAX=3"   , ""},
{ SSC_INPUT,SSC_NUMBER  , "depr_fed_sl_years"                    , "Federal depreciation straight-line Years"                       , "years"                                  , ""                                      , "Depreciation"         , "depr_fed_type=2", "INTEGER,POSITIVE"      , "SKIP_CONSTRAINT_CHECK"},
{ SSC_INPUT,SSC_ARRAY   , "depr_fed_custom"                      , "Federal custom depreciation"                                    , "%/year"                                 , ""                                      , "Depreciation"         , "depr_fed_type=3", ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "depr_sta_type"                        , "State depreciation type"                                        , ""                                       , "0=none,1=macrs_half_year,2=sl,3=custom", "Depreciation"         , "?=0"            , "INTEGER,MIN=0,MAX=3"   , ""},
{ SSC_INPUT,SSC_NUMBER  , "depr_sta_sl_years"                    , "State depreciation straight-line years"                         , "years"                                  , ""                                      , "Depreciation"         , "depr_sta_type=2", "INTEGER,POSITIVE"      , "SKIP_CONSTRAINT_CHECK"},
{ SSC_INPUT,SSC_ARRAY   , "depr_sta_custom"                      , "State custom depreciation"                                      , "%/year"                                 , ""                                      , "Depreciation"         , "depr_sta_type=3", ""                      , ""},
var_info_invalid };

var_info vtab_depreciation_inputs[] = {
/* depreciation allocation */
{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_macrs_5_percent",		      "5-yr MACRS depreciation federal and state allocation",	"%", "",	  "Depreciation",             "?=89",					  "MIN=0,MAX=100",     			        "" },
{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_macrs_15_percent",		      "15-yr MACRS depreciation federal and state allocation",	"%", "",  "Depreciation",             "?=1.5",					  "MIN=0,MAX=100",     			        "" },
{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_5_percent",		          "5-yr straight line depreciation federal and state allocation",	"%", "",  "Depreciation",             "?=0",						  "MIN=0,MAX=100",     			        "" },
{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_15_percent",		          "15-yr straight line depreciation federal and state allocation","%", "",  "Depreciation",             "?=3",						  "MIN=0,MAX=100",     			        "" },
{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_20_percent",		          "20-yr straight line depreciation federal and state allocation","%", "",  "Depreciation",             "?=3",						  "MIN=0,MAX=100",     			        "" },
{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_39_percent",		          "39-yr straight line depreciation federal and state allocation","%", "",  "Depreciation",             "?=0.5",					  "MIN=0,MAX=100",     			        "" },
{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_custom_percent",	          "Custom depreciation federal and state allocation","%", "",  "Depreciation",             "?=0",					  "MIN=0,MAX=100",     			        "" },
{ SSC_INPUT,        SSC_ARRAY,      "depr_custom_schedule",		              "Custom depreciation schedule",	"%",   "",                      "Depreciation",             "*",						   "",                              "" },
/* bonus depreciation */
{ SSC_INPUT,        SSC_NUMBER,     "depr_bonus_sta",			              "State bonus depreciation",			"%",	 "",					  "Depreciation",             "?=0",						  "MIN=0,MAX=100",     			        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_macrs_5",                 "State bonus depreciation 5-yr MACRS","0/1", "",                      "Depreciation",			 "?=1",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_macrs_15",                "State bonus depreciation 15-yr MACRS","0/1","",                     "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_sl_5",                    "State bonus depreciation 5-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_sl_15",                   "State bonus depreciation 15-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_sl_20",                   "State bonus depreciation 20-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_sl_39",                   "State bonus depreciation 39-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_custom",                  "State bonus depreciation custom","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },

{ SSC_INPUT,        SSC_NUMBER,     "depr_bonus_fed",			              "Federal bonus depreciation",			"%",	 "",					  "Depreciation",             "?=0",						  "MIN=0,MAX=100",     			        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_macrs_5",                 "Federal bonus depreciation 5-yr MACRS","0/1", "",                      "Depreciation",			 "?=1",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_macrs_15",                "Federal bonus depreciation 15-yr MACRS","0/1","",                     "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_sl_5",                    "Federal bonus depreciation 5-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_sl_15",                   "Federal bonus depreciation 15-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_sl_20",                   "Federal bonus depreciation 20-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_sl_39",                   "Federal bonus depreciation 39-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_custom",                  "Federal bonus depreciation custom","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
/* ITC depreciation */
{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_macrs_5",                   "State ITC depreciation 5-yr MACRS","0/1", "",                      "Depreciation",			 "?=1",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_macrs_15",                  "State ITC depreciation 15-yr MACRS","0/1","",                     "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_sl_5",                      "State ITC depreciation 5-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_sl_15",                     "State ITC depreciation 15-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_sl_20",                     "State ITC depreciation 20-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_sl_39",                     "State ITC depreciation 39-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_custom",                    "State ITC depreciation custom","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },

{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_macrs_5",                   "Federal ITC depreciation 5-yr MACRS","0/1", "",                      "Depreciation",			 "?=1",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_macrs_15",                  "Federal ITC depreciation 15-yr MACRS","0/1","",                     "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_sl_5",                      "Federal ITC depreciation 5-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_sl_15",                     "Federal ITC depreciation 15-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_sl_20",                     "Federal ITC depreciation 20-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_sl_39",                     "Federal ITC depreciation 39-yr straight line","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_custom",                    "Federal ITC depreciation custom","0/1","",                  "Depreciation",			 "?=0",                       "BOOLEAN",                        "" },
var_info_invalid
};

var_info vtab_depreciation_outputs[] = {
/* state depreciation and tax */
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_stadepr_macrs_5",                     "State depreciation from 5-yr MACRS",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_stadepr_macrs_15",                    "State depreciation from 15-yr MACRS",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_stadepr_sl_5",                        "State depreciation from 5-yr straight line",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_stadepr_sl_15",                       "State depreciation from 15-yr straight line",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_stadepr_sl_20",                       "State depreciation from 20-yr straight line",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_stadepr_sl_39",                       "State depreciation from 39-yr straight line",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_stadepr_custom",                      "State depreciation from custom",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_stadepr_me1",                         "State depreciation from major equipment 1",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_stadepr_me2",                         "State depreciation from major equipment 2",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_stadepr_me3",                         "State depreciation from major equipment 3",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_stadepr_total",                       "Total state tax depreciation",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_statax_income_prior_incentives",      "State taxable income without incentives",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_statax_taxable_incentives",           "State taxable incentives",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_statax_income_with_incentives",       "State taxable income",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_statax",				              "State tax benefit (liability)",                   "$",            "",                      "Cash Flow State Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

/* federal depreciation and tax */
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_feddepr_macrs_5",                     "Federal depreciation from 5-yr MACRS",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_feddepr_macrs_15",                    "Federal depreciation from 15-yr MACRS",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_feddepr_sl_5",                        "Federal depreciation from 5-yr straight line",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_feddepr_sl_15",                       "Federal depreciation from 15-yr straight line",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_feddepr_sl_20",                       "Federal depreciation from 20-yr straight line",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_feddepr_sl_39",                       "Federal depreciation from 39-yr straight line",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_feddepr_custom",                      "Federal depreciation from custom",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_feddepr_me1",                         "Federal depreciation from major equipment 1",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_feddepr_me2",                         "Federal depreciation from major equipment 2",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_feddepr_me3",                         "Federal depreciation from major equipment 3",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_feddepr_total",                       "Total federal tax depreciation",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_fedtax_income_prior_incentives",      "Federal taxable income without incentives",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_fedtax_taxable_incentives",           "Federal taxable incentives",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_fedtax_income_with_incentives",       "Federal taxable income",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_fedtax",				              "Federal tax benefit (liability)",                   "$",            "",                      "Cash Flow Federal Income Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

var_info_invalid
};

var_info vtab_tax_credits_heat[] = {
{ SSC_INPUT,SSC_ARRAY   , "ptc_fed_amount_heat_btu"                       , "Federal PTC amount"                                             , "$/MMBtu"                                  , ""                                      , "Tax Credit Incentives", "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_ARRAY   , "ptc_sta_amount_heat_btu"                       , "State PTC amount"                                             , "$/MMBtu"                                  , ""                                      , "Tax Credit Incentives", "?=0"            , ""                      , ""},
var_info_invalid
};


var_info vtab_tax_credits[] = {
{ SSC_INPUT,SSC_ARRAY  , "itc_fed_amount"                       , "Federal amount-based ITC amount"                                , "$"                                      , ""                                      , "Tax Credit Incentives", "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "itc_fed_amount_deprbas_fed"           , "Federal amount-based ITC reduces federal depreciation basis"    , "0/1"                                    , ""                                      , "Tax Credit Incentives", "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "itc_fed_amount_deprbas_sta"           , "Federal amount-based ITC reduces state depreciation basis"      , "0/1"                                    , ""                                      , "Tax Credit Incentives", "?=1"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_ARRAY  , "itc_sta_amount"                       , "State amount-based ITC amount"                                  , "$"                                      , ""                                      , "Tax Credit Incentives", "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "itc_sta_amount_deprbas_fed"           , "State amount-based ITC reduces federal depreciation basis"      , "0/1"                                    , ""                                      , "Tax Credit Incentives", "?=0"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "itc_sta_amount_deprbas_sta"           , "State amount-based ITC reduces state depreciation basis"        , "0/1"                                    , ""                                      , "Tax Credit Incentives", "?=0"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_ARRAY  , "itc_fed_percent"                      , "Federal percentage-based ITC percent"                           , "%"                                      , ""                                      , "Tax Credit Incentives", "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_ARRAY  , "itc_fed_percent_maxvalue"             , "Federal percentage-based ITC maximum value"                     , "$"                                      , ""                                      , "Tax Credit Incentives", "?=1e99"         , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "itc_fed_percent_deprbas_fed"          , "Federal percentage-based ITC reduces federal depreciation basis", "0/1"                                    , ""                                      , "Tax Credit Incentives", "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "itc_fed_percent_deprbas_sta"          , "Federal percentage-based ITC reduces state depreciation basis"  , "0/1"                                    , ""                                      , "Tax Credit Incentives", "?=1"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_ARRAY  , "itc_sta_percent"                      , "State percentage-based ITC percent"                             , "%"                                      , ""                                      , "Tax Credit Incentives", "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_ARRAY  , "itc_sta_percent_maxvalue"             , "State percentage-based ITC maximum Value"                       , "$"                                      , ""                                      , "Tax Credit Incentives", "?=1e99"         , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "itc_sta_percent_deprbas_fed"          , "State percentage-based ITC reduces federal depreciation basis"  , "0/1"                                    , ""                                      , "Tax Credit Incentives", "?=0"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "itc_sta_percent_deprbas_sta"          , "State percentage-based ITC reduces state depreciation basis"    , "0/1"                                    , ""                                      , "Tax Credit Incentives", "?=0"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_ARRAY   , "ptc_fed_amount"                       , "Federal PTC amount"                                             , "$/kWh"                                  , ""                                      , "Tax Credit Incentives", "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ptc_fed_term"                         , "Federal PTC term"                                               , "years"                                  , ""                                      , "Tax Credit Incentives", "?=10"           , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ptc_fed_escal"                        , "Federal PTC escalation"                                         , "%/year"                                 , ""                                      , "Tax Credit Incentives", "?=0"            , ""                      , ""},

{ SSC_INPUT,SSC_ARRAY   , "ptc_sta_amount"                       , "State PTC amount"                                               , "$/kWh"                                  , ""                                      , "Tax Credit Incentives", "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ptc_sta_term"                         , "State PTC term"                                                 , "years"                                  , ""                                      , "Tax Credit Incentives", "?=10"           , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ptc_sta_escal"                        , "State PTC escalation"                                           , "%/year"                                 , ""                                      , "Tax Credit Incentives", "?=0"            , ""                      , ""},
var_info_invalid };

var_info vtab_payment_incentives_heat[] = {
    { SSC_INPUT,SSC_NUMBER  , "cbi_fed_amount_heat_btu"                       , "Federal CBI amount"                                             , "$/(Btu/hr))"                                 , ""                                      , "Payment Incentives"   , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_NUMBER  , "cbi_sta_amount_heat_btu"                       , "State CBI amount"                                             , "$/(Btu/hr))"                                 , ""                                      , "Payment Incentives"   , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_NUMBER  , "cbi_uti_amount_heat_btu"                       , "Utility CBI amount"                                             , "$/(Btu/hr))"                                 , ""                                      , "Payment Incentives"   , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_NUMBER  , "cbi_oth_amount_heat_btu"                       , "Other CBI amount"                                             , "$/(Btu/hr))"                                 , ""                                      , "Payment Incentives"   , "?=0.0"          , ""                      , ""},
    { SSC_INPUT,SSC_ARRAY   , "pbi_fed_amount_heat_btu"                       , "Federal PBI amount"                                             , "$/MMBtu"                                  , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
    { SSC_INPUT,SSC_ARRAY   , "pbi_sta_amount_heat_btu"                       , "State PBI amount"                                             , "$/MMBtu"                                  , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
    { SSC_INPUT,SSC_ARRAY   , "pbi_uti_amount_heat_btu"                       , "Utility PBI amount"                                             , "$/MMBtu"                                  , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
    { SSC_INPUT,SSC_ARRAY   , "pbi_oth_amount_heat_btu"                       , "Other PBI amount"                                             , "$/MMBtu"                                  , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},

var_info_invalid };


var_info vtab_payment_incentives[] = {
{ SSC_INPUT,SSC_NUMBER  , "ibi_fed_amount"                       , "Federal amount-based IBI amount"                                , "$"                                      , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_fed_amount_tax_fed"               , "Federal amount-based IBI federal taxable"                       , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_fed_amount_tax_sta"               , "Federal amount-based IBI state taxable"                         , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_fed_amount_deprbas_fed"           , "Federal amount-based IBI reduces federal depreciation basis"    , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_fed_amount_deprbas_sta"           , "Federal amount-based IBI reduces state depreciation basis"      , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_NUMBER  , "ibi_sta_amount"                       , "State amount-based IBI amount"                                  , "$"                                      , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_sta_amount_tax_fed"               , "State amount-based IBI federal taxable"                         , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_sta_amount_tax_sta"               , "State amount-based IBI state taxable"                           , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_sta_amount_deprbas_fed"           , "State amount-based IBI reduces federal depreciation basis"      , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_sta_amount_deprbas_sta"           , "State amount-based IBI reduces state depreciation basis"        , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_NUMBER  , "ibi_uti_amount"                       , "Utility amount-based IBI amount"                                , "$"                                      , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_uti_amount_tax_fed"               , "Utility amount-based IBI federal taxable"                       , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_uti_amount_tax_sta"               , "Utility amount-based IBI state taxable"                         , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_uti_amount_deprbas_fed"           , "Utility amount-based IBI reduces federal depreciation basis"    , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_uti_amount_deprbas_sta"           , "Utility amount-based IBI reduces state depreciation basis"      , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_NUMBER  , "ibi_oth_amount"                       , "Other amount-based IBI amount"                                  , "$"                                      , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_oth_amount_tax_fed"               , "Other amount-based IBI federal taxable"                         , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_oth_amount_tax_sta"               , "Other amount-based IBI state taxable"                           , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_oth_amount_deprbas_fed"           , "Other amount-based IBI reduces federal depreciation basis"      , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_oth_amount_deprbas_sta"           , "Other amount-based IBI reduces state depreciation basis"        , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_NUMBER  , "ibi_fed_percent"                      , "Federal percentage-based IBI percent"                           , "%"                                      , ""                                      , "Payment Incentives"   , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_fed_percent_maxvalue"             , "Federal percentage-based IBI maximum value"                     , "$"                                      , ""                                      , "Payment Incentives"   , "?=1e99"         , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_fed_percent_tax_fed"              , "Federal percentage-based IBI federal taxable"                   , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_fed_percent_tax_sta"              , "Federal percentage-based IBI state taxable"                     , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_fed_percent_deprbas_fed"          , "Federal percentage-based IBI reduces federal depreciation basis", "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_fed_percent_deprbas_sta"          , "Federal percentage-based IBI reduces state depreciation basis"  , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_NUMBER  , "ibi_sta_percent"                      , "State percentage-based IBI percent"                             , "%"                                      , ""                                      , "Payment Incentives"   , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_sta_percent_maxvalue"             , "State percentage-based IBI maximum value"                       , "$"                                      , ""                                      , "Payment Incentives"   , "?=1e99"         , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_sta_percent_tax_fed"              , "State percentage-based IBI federal taxable"                     , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_sta_percent_tax_sta"              , "State percentage-based IBI state taxable"                       , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_sta_percent_deprbas_fed"          , "State percentage-based IBI reduces federal depreciation basis"  , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_sta_percent_deprbas_sta"          , "State percentage-based IBI reduces state depreciation basis"    , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_NUMBER  , "ibi_uti_percent"                      , "Utility percentage-based IBI percent"                           , "%"                                      , ""                                      , "Payment Incentives"   , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_uti_percent_maxvalue"             , "Utility percentage-based IBI maximum value"                     , "$"                                      , ""                                      , "Payment Incentives"   , "?=1e99"         , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_uti_percent_tax_fed"              , "Utility percentage-based IBI federal taxable"                   , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_uti_percent_tax_sta"              , "Utility percentage-based IBI state taxable"                     , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_uti_percent_deprbas_fed"          , "Utility percentage-based IBI reduces federal depreciation basis", "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_uti_percent_deprbas_sta"          , "Utility percentage-based IBI reduces state depreciation basis"  , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_NUMBER  , "ibi_oth_percent"                      , "Other percentage-based IBI percent"                             , "%"                                      , ""                                      , "Payment Incentives"   , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_oth_percent_maxvalue"             , "Other percentage-based IBI maximum value"                       , "$"                                      , ""                                      , "Payment Incentives"   , "?=1e99"         , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_oth_percent_tax_fed"              , "Other percentage-based IBI federal taxable"                     , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_oth_percent_tax_sta"              , "Other percentage-based IBI state taxable"                       , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_oth_percent_deprbas_fed"          , "Other percentage-based IBI reduces federal depreciation basis"  , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "ibi_oth_percent_deprbas_sta"          , "Other percentage-based IBI reduces state depreciation basis"    , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_NUMBER  , "cbi_fed_amount"                       , "Federal CBI amount"                                             , "$/Watt"                                 , ""                                      , "Payment Incentives"   , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_fed_maxvalue"                     , "Federal CBI maximum"                                            , "$"                                      , ""                                      , "Payment Incentives"   , "?=1e99"         , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_fed_tax_fed"                      , "Federal CBI federal taxable"                                    , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_fed_tax_sta"                      , "Federal CBI state taxable"                                      , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_fed_deprbas_fed"                  , "Federal CBI reduces federal depreciation basis"                 , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_fed_deprbas_sta"                  , "Federal CBI reduces state depreciation basis"                   , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_NUMBER  , "cbi_sta_amount"                       , "State CBI amount"                                               , "$/Watt"                                 , ""                                      , "Payment Incentives"   , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_sta_maxvalue"                     , "State CBI maximum"                                              , "$"                                      , ""                                      , "Payment Incentives"   , "?=1e99"         , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_sta_tax_fed"                      , "State CBI federal taxable"                                      , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_sta_tax_sta"                      , "State CBI state taxable"                                        , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_sta_deprbas_fed"                  , "State CBI reduces federal depreciation basis"                   , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_sta_deprbas_sta"                  , "State CBI reduces state depreciation basis"                     , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_NUMBER  , "cbi_uti_amount"                       , "Utility CBI amount"                                             , "$/Watt"                                 , ""                                      , "Payment Incentives"   , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_uti_maxvalue"                     , "Utility CBI maximum"                                            , "$"                                      , ""                                      , "Payment Incentives"   , "?=1e99"         , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_uti_tax_fed"                      , "Utility CBI federal taxable"                                    , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_uti_tax_sta"                      , "Utility CBI state taxable"                                      , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_uti_deprbas_fed"                  , "Utility CBI reduces federal depreciation basis"                 , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_uti_deprbas_sta"                  , "Utility CBI reduces state depreciation basis"                   , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_NUMBER  , "cbi_oth_amount"                       , "Other CBI amount"                                               , "$/Watt"                                 , ""                                      , "Payment Incentives"   , "?=0.0"          , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_oth_maxvalue"                     , "Other CBI maximum"                                              , "$"                                      , ""                                      , "Payment Incentives"   , "?=1e99"         , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_oth_tax_fed"                      , "Other CBI federal taxable"                                      , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_oth_tax_sta"                      , "Other CBI state taxable"                                        , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_oth_deprbas_fed"                  , "Other CBI reduces federal depreciation basis"                   , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "cbi_oth_deprbas_sta"                  , "Other CBI reduces state depreciation basis"                     , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=0"            , "BOOLEAN"               , ""},


{ SSC_INPUT,SSC_ARRAY   , "pbi_fed_amount"                       , "Federal PBI amount"                                             , "$/kWh"                                  , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_fed_term"                         , "Federal PBI term"                                               , "years"                                  , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_fed_escal"                        , "Federal PBI escalation"                                         , "%"                                      , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_fed_tax_fed"                      , "Federal PBI federal taxable"                                    , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_fed_tax_sta"                      , "Federal PBI state taxable"                                      , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_ARRAY   , "pbi_sta_amount"                       , "State PBI amount"                                               , "$/kWh"                                  , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_sta_term"                         , "State PBI term"                                                 , "years"                                  , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_sta_escal"                        , "State PBI escalation"                                           , "%"                                      , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_sta_tax_fed"                      , "State PBI federal taxable"                                      , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_sta_tax_sta"                      , "State PBI state taxable"                                        , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_ARRAY   , "pbi_uti_amount"                       , "Utility PBI amount"                                             , "$/kWh"                                  , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_uti_term"                         , "Utility PBI term"                                               , "years"                                  , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_uti_escal"                        , "Utility PBI escalation"                                         , "%"                                      , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_uti_tax_fed"                      , "Utility PBI federal taxable"                                    , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_uti_tax_sta"                      , "Utility PBI state taxable"                                      , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},

{ SSC_INPUT,SSC_ARRAY   , "pbi_oth_amount"                       , "Other PBI amount"                                               , "$/kWh"                                  , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_oth_term"                         , "Other PBI term"                                                 , "years"                                  , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_oth_escal"                        , "Other PBI escalation"                                           , "%"                                      , ""                                      , "Payment Incentives"   , "?=0"            , ""                      , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_oth_tax_fed"                      , "Other PBI federal taxable"                                      , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},
{ SSC_INPUT,SSC_NUMBER  , "pbi_oth_tax_sta"                      , "Other PBI state taxable"                                        , "0/1"                                    , ""                                      , "Payment Incentives"   , "?=1"            , "BOOLEAN"               , ""},

// outputs
{ SSC_OUTPUT,       SSC_NUMBER,     "cbi_total_fed",                          "Federal CBI income",         "$",            "",                      "Cash Flow Incentives",      "*",                     "",                                      "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "cbi_total_sta",                          "State CBI income",           "$",            "",                      "Cash Flow Incentives",      "*",                     "",                                      "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "cbi_total_oth",                          "Other CBI income",           "$",            "",                      "Cash Flow Incentives",      "*",                     "",                                      "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "cbi_total_uti",                          "Utility CBI income",         "$",            "",                      "Cash Flow Incentives",      "*",                     "",                                      "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "cbi_total",                              "Total CBI income",           "$",            "",                      "Cash Flow Incentives",      "*",                     "",                                      "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "cbi_statax_total",                       "State taxable CBI income",   "$",            "",                      "Cash Flow Incentives",      "",                     "",                                      "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "cbi_fedtax_total",                       "Federal taxable CBI income", "$",            "",                      "Cash Flow Incentives",      "",                     "",                                      "" },

{ SSC_OUTPUT,       SSC_NUMBER,     "ibi_total_fed",                          "Federal IBI income",         "$",            "",                      "Cash Flow Incentives",      "*",                     "",                                      "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "ibi_total_sta",                          "State IBI income",           "$",            "",                      "Cash Flow Incentives",      "*",                     "",                                      "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "ibi_total_oth",                          "Other IBI income",           "$",            "",                      "Cash Flow Incentives",      "*",                     "",                                      "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "ibi_total_uti",                          "Utility IBI income",         "$",            "",                      "Cash Flow Incentives",      "*",                     "",                                      "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "ibi_total",                              "Total IBI income",           "$",            "",                      "Cash Flow Incentives",      "*",                     "",                                      "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "ibi_statax_total",                       "State taxable IBI income",   "$",            "",                      "Cash Flow Incentives",      "",                     "",                                      "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "ibi_fedtax_total",                       "Federal taxable IBI income", "$",            "",                      "Cash Flow Incentives",      "",                     "",                                      "" },

{ SSC_OUTPUT,       SSC_ARRAY,      "cf_pbi_total_fed",                       "Federal PBI income",         "$",            "",                      "Cash Flow Incentives",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_pbi_total_sta",                       "State PBI income",           "$",            "",                      "Cash Flow Incentives",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_pbi_total_oth",                       "Other PBI income",           "$",            "",                      "Cash Flow Incentives",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_pbi_total_uti",                       "Utility PBI income",         "$",            "",                      "Cash Flow Incentives",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_pbi_total",                           "Total PBI income",           "$",            "",                      "Cash Flow Incentives",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_pbi_statax_total",                    "State taxable PBI income",   "$",            "",                      "Cash Flow Incentives",      "",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_pbi_fedtax_total",                    "Federal taxable PBI income", "$",            "",                      "Cash Flow Incentives",      "",                     "LENGTH_EQUAL=cf_length",                "" },

{ SSC_OUTPUT,       SSC_NUMBER,     "itc_total_fed",                          "Federal ITC income",         "$",            "",                      "Cash Flow Incentives",      "*",                     "",                                      "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "itc_total_sta",                          "State ITC income",           "$",            "",                      "Cash Flow Incentives",      "*",                     "",                                      "" },
{ SSC_OUTPUT,        SSC_NUMBER,    "itc_total",							  "Total ITC income",           "$",            "",                      "Cash Flow Incentives",      "*",                     "",                "" },

{ SSC_OUTPUT,       SSC_ARRAY,      "cf_ptc_fed",                             "Federal PTC income",                 "$",            "",                      "Cash Flow Incentives",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_ptc_sta",                             "State PTC income",                   "$",            "",                      "Cash Flow Incentives",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
// SAM 1038 - make required after other compute module updated - see ssc 910
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_itc_fed_amount",                     "Federal ITC amount income",                 "$",            "",                      "Cash Flow Incentives",      "",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_itc_sta_amount",                     "State ITC amount income",                   "$",            "",                      "Cash Flow Incentives",      "",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_itc_fed_percent_amount",             "Federal ITC percent income",                 "$",            "",                      "Cash Flow Incentives",      "",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_itc_sta_percent_amount",             "State ITC percent income",                   "$",            "",                      "Cash Flow Incentives",      "",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_itc_fed",                            "Federal ITC total income",                 "$",            "",                      "Cash Flow Incentives",      "",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT,       SSC_ARRAY,      "cf_itc_sta",                            "State ITC total income",                   "$",            "",                      "Cash Flow Incentives",      "",                     "LENGTH_EQUAL=cf_length",                "" },
{ SSC_OUTPUT, 	SSC_ARRAY, 	    "cf_itc_total", 	                         "Total ITC income", 	            "$", 	            "", 	                "Cash Flow Incentives", 	        "", 	"LENGTH_EQUAL=cf_length", 	"" },

var_info_invalid };

var_info vtab_ppa_inout[] = {
{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_mode",                          "PPA solution mode",                              "0/1",   "0=solve ppa,1=specify ppa", "Revenue",         "?=0",                     "INTEGER,MIN=0,MAX=1",            "" },
{ SSC_INPUT,        SSC_NUMBER,     "ppa_soln_tolerance",                     "PPA solution tolerance",                         "",                 "", "Revenue", "?=1e-5", "", "" },
{ SSC_INPUT,        SSC_NUMBER,     "ppa_soln_min",                           "PPA solution minimum ppa",                       "cents/kWh",        "", "Revenue", "?=0", "", "" },
{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_max",                           "PPA solution maximum ppa",                       "cents/kWh",        "", "Revenue",         "?=100",                     "",            "" },
{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_max_iterations",                "PPA solution maximum number of iterations",      "",                 "", "Revenue",         "?=100",                     "INTEGER,MIN=1",            "" },

{ SSC_INPUT,        SSC_ARRAY,      "ppa_price_input",			              "PPA price in first year input",			            "$/kWh",	        "",	"Revenue",			 "*",         "",      			"" },
{ SSC_INPUT,        SSC_NUMBER,     "ppa_escalation",                         "PPA escalation rate",                            "%/year",           "", "Revenue", "?=0", "", "" },

{ SSC_OUTPUT,       SSC_NUMBER,     "lppa_real",                              "LPPA Levelized PPA price real",                         "cents/kWh",               "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "lppa_nom",                               "LPPA Levelized PPA price nominal",                      "cents/kWh",               "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "ppa",                                    "PPA price in Year 1",                        "cents/kWh",               "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "ppa_escalation",                         "PPA price escalation",                      "%/year",              "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "npv_ppa_revenue",                        "Present value of PPA revenue",              "$",                   "", "Metrics", "*", "", "" },

var_info_invalid };


var_info vtab_ppa_inout_heat[] = {
{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_mode",                          "PPA solution mode",                              "0/1",   "0=solve ppa,1=specify ppa", "Revenue",         "?=0",                     "INTEGER,MIN=0,MAX=1",            "" },
{ SSC_INPUT,        SSC_NUMBER,     "ppa_soln_tolerance",                     "PPA solution tolerance",                         "",                 "", "Revenue", "?=1e-5", "", "" },
{ SSC_INPUT,        SSC_NUMBER,     "ppa_soln_min",                           "PPA solution minimum ppa",                       "cents/kWht",        "", "Revenue", "?=0", "", "" },
{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_max",                           "PPA solution maximum ppa",                       "cents/kWht",        "", "Revenue",         "?=100",                     "",            "" },
{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_max_iterations",                "PPA solution maximum number of iterations",      "",                 "", "Revenue",         "?=100",                     "INTEGER,MIN=1",            "" },

{ SSC_INPUT,        SSC_ARRAY,      "ppa_price_input",			              "PPA price in first year input",			            "$/kWht",	        "",	"Revenue",			 "*",         "",      			"" },
{ SSC_INPUT,        SSC_NUMBER,     "ppa_escalation",                         "PPA escalation rate",                            "%/year",           "", "Revenue", "?=0", "", "" },

{ SSC_OUTPUT,       SSC_NUMBER,     "lppa_real",                              "LPPA Levelized PPA price real",                         "cents/kWht",               "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "lppa_nom",                               "LPPA Levelized PPA price nominal",                      "cents/kWht",               "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "ppa",                                    "PPA price in Year 1",                        "cents/kWht",               "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "ppa_escalation",                         "PPA price escalation",                      "%/year",              "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "npv_ppa_revenue",                        "Present value of PPA revenue",              "$",                   "", "Metrics", "*", "", "" },

var_info_invalid };



var_info vtab_financial_metrics[] = {
//	{ SSC_OUTPUT,       SSC_NUMBER,     "first_year_energy_net",                  "Annual energy",                             "kWh",                 "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "debt_fraction",                          "Debt percent",                             "%", "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "flip_target_year",                       "Target year to meet IRR",                   "",                    "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "flip_target_irr",                        "IRR target",                                "%",                   "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "flip_actual_year",                       "Year target IRR was achieved",              "year",                    "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "flip_actual_irr",                        "IRR in target year",                        "%",                   "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "lcoe_real",                              "LCOE Levelized cost of energy real",                               "cents/kWh",               "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "lcoe_nom",                               "LCOE Levelized cost of energy nominal",                            "cents/kWh",               "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "lcos_real",                              "LCOS Levelized cost of storage real",                               "cents/kWh",               "", "Metrics", "?", "", "" },
//{ SSC_OUTPUT,       SSC_NUMBER,     "lcos_nom",                               "LCOS Levelized cost of storage nominal",                            "cents/kWh",               "", "Metrics", "?", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "npv_energy_nom",                         "Present value of annual energy nominal",     "kWh",                 "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "npv_energy_real",                        "Present value of annual energy real",     "kWh",                 "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "present_value_oandm",                    "Present value of O&M",				       "$",                   "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "present_value_oandm_nonfuel",            "Present value of non-fuel O&M",         "$",                   "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "present_value_fuel",                     "Present value of fuel O&M",             "$",                   "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "present_value_insandproptax",            "Present value of insurance and prop tax",   "$",                   "", "Metrics", "*", "", "" },

{ SSC_OUTPUT,       SSC_NUMBER,     "lcoptc_fed_real",                        "Levelized federal PTC real",              "cents/kWh",                   "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "lcoptc_fed_nom",                         "Levelized federal PTC nominal",           "cents/kWh",                   "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "lcoptc_sta_real",                        "Levelized state PTC real",                "cents/kWh",                   "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "lcoptc_sta_nom",                         "Levelized state PTC nominal",             "cents/kWh",                   "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "wacc",                                   "WACC Weighted average cost of capital",   "$",                   "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "effective_tax_rate",                     "Effective tax rate",                        "%",                   "", "Metrics", "*", "", "" },
{ SSC_OUTPUT,       SSC_NUMBER,     "analysis_period_irr",                    "IRR at end of analysis period",             "%",                   "", "Metrics", "*", "", "" },
var_info_invalid
};



var_info vtab_financial_metrics_heat[] = {
    //	{ SSC_OUTPUT,       SSC_NUMBER,     "first_year_energy_net",                  "Annual energy",                             "kWh",                 "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "debt_fraction",                          "Debt percent",                             "%", "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "flip_target_year",                       "Target year to meet IRR",                   "",                    "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "flip_target_irr",                        "IRR target",                                "%",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "flip_actual_year",                       "Year target IRR was achieved",              "year",                    "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "flip_actual_irr",                        "IRR in target year",                        "%",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "lcoe_real",                              "LCOH Levelized cost of heat real",                               "cents/kWht",               "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "lcoe_nom",                               "LCOH Levelized cost of heat nominal",                            "cents/kWht",               "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "lcos_real",                              "LCOS Levelized cost of storage real",                               "cents/kWht",               "", "Metrics", "?", "", "" },
    //{ SSC_OUTPUT,       SSC_NUMBER,     "lcos_nom",                               "LCOS Levelized cost of storage nominal",                            "cents/kWh",               "", "Metrics", "?", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "npv_energy_nom",                         "Present value of annual energy nominal",     "kWht",                 "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "npv_energy_real",                        "Present value of annual energy real",     "kWht",                 "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "present_value_oandm",                    "Present value of O&M",				       "$",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "present_value_oandm_nonfuel",            "Present value of non-fuel O&M",         "$",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "present_value_fuel",                     "Present value of fuel O&M",             "$",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "present_value_insandproptax",            "Present value of insurance and prop tax",   "$",                   "", "Metrics", "*", "", "" },

    { SSC_OUTPUT,       SSC_NUMBER,     "lcoptc_fed_real",                        "Levelized federal PTC real",              "cents/kWht",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "lcoptc_fed_nom",                         "Levelized federal PTC nominal",           "cents/kWht",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "lcoptc_sta_real",                        "Levelized state PTC real",                "cents/kWht",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "lcoptc_sta_nom",                         "Levelized state PTC nominal",             "cents/kWht",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "wacc",                                   "WACC Weighted average cost of capital",   "$",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "effective_tax_rate",                     "Effective tax rate",                        "%",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "analysis_period_irr",                    "IRR at end of analysis period",             "%",                   "", "Metrics", "*", "", "" },
    var_info_invalid
};



var_info vtab_debt[] = {
/* term financing */
{ SSC_INPUT,        SSC_NUMBER,     "term_tenor",                             "Term financing period",				                            "years", "",				      "Financial Parameters",             "?=10",					"INTEGER,MIN=0",      			"" },
{ SSC_INPUT,        SSC_NUMBER,     "term_int_rate",                          "Term financing interest rate",		                            "%",	 "",					  "Financial Parameters",             "?=8.5",                   "MIN=0,MAX=100",      			"" },
{ SSC_INPUT,        SSC_NUMBER,     "dscr",						              "Debt service coverage ratio",		                            "",	     "",				      "Financial Parameters",             "?=1.5",					"MIN=0",      			        "" },
{ SSC_INPUT,        SSC_NUMBER,     "dscr_limit_debt_fraction",				  "Limit debt fraction",		                            "0/1",	     "",				      "Financial Parameters",             "?=0",					"BOOLEAN",      			        "" },
{ SSC_INPUT,        SSC_NUMBER,     "dscr_maximum_debt_fraction",			  "Maximum debt fraction",		                            "%",	     "",				      "Financial Parameters",             "?=100",					"POSITIVE",      			        "" },
{ SSC_INPUT,        SSC_NUMBER,     "dscr_reserve_months",		              "Debt service reserve account",		                            "months P&I","",			      "Financial Parameters",             "?=6",					    "MIN=0",      			        "" },
/* Debt fraction input option */
{ SSC_INPUT, SSC_NUMBER, "debt_percent", "Debt percent", "%", "", "Financial Parameters", "?=50", "MIN=0,MAX=100", "" },
{ SSC_INPUT, SSC_NUMBER, "debt_option", "Debt option", "0/1", "0=debt percent,1=dscr", "Financial Parameters", "?=1", "INTEGER,MIN=0,MAX=1", "" },
{ SSC_INPUT, SSC_NUMBER, "payment_option", "Debt repayment option", "0/1", "0=Equal payments (standard amortization),1=Fixed principal declining interest", "Financial Parameters", "?=0", "INTEGER,MIN=0,MAX=1", "" },
/* Capital Cost */
{ SSC_INPUT,        SSC_NUMBER,     "cost_debt_closing",		              "Debt closing cost",                                              "$",                 "",       "Financial Parameters",        "?=250000",         "MIN=0",              "" },
{ SSC_INPUT,        SSC_NUMBER,     "cost_debt_fee",		                  "Debt closing fee (% of total debt amount)",                      "%",                 "",       "Financial Parameters",        "?=1.5",            "MIN=0",              "" },
{ SSC_INPUT, SSC_NUMBER, "months_working_reserve", "Working capital reserve months of operating costs", "months", "", "Financial Parameters", "?=6", "MIN=0", "" },
{ SSC_INPUT, SSC_NUMBER, "months_receivables_reserve", "Receivables reserve months of PPA revenue", "months", "", "Financial Parameters", "?=0", "MIN=0", "" },
{ SSC_INPUT, SSC_NUMBER, "cost_other_financing", "Other financing cost", "$", "", "Financial Parameters", "?=150000", "MIN=0", "" },
/* Equity Structure */
{ SSC_INPUT,        SSC_NUMBER,     "flip_target_percent",			          "After-tax IRR target",		"%",	 "",					  "Revenue",             "?=11",					  "MIN=0,MAX=100",     			        "" },
{ SSC_INPUT,        SSC_NUMBER,     "flip_target_year",		                  "IRR target year",				"Year",		 "",					  "Revenue",             "?=11",					  "MIN=1",     			        "" },
/* PBI for debt service TODO - other yearly incentives */
{ SSC_INPUT,        SSC_NUMBER,     "pbi_fed_for_ds",                         "Federal PBI available for debt service",     "0/1",      "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
{ SSC_INPUT,        SSC_NUMBER,     "pbi_sta_for_ds",                         "State PBI available for debt service",     "0/1",      "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
{ SSC_INPUT,        SSC_NUMBER,     "pbi_uti_for_ds",                         "Utility PBI available for debt service",     "0/1",      "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
{ SSC_INPUT,        SSC_NUMBER,     "pbi_oth_for_ds",                         "Other PBI available for debt service",     "0/1",      "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
var_info_invalid
};

var_info vtab_adjustment_factors[] = {
{ SSC_INPUT,SSC_NUMBER  , "adjust_constant"                      , "Constant loss adjustment"                                       , "%",
"'adjust' and 'constant' separated by _ instead of : after SAM 2022.12.21",                                      "Adjustment Factors"   , "?=0"              , "MAX=100"               , ""},
{ SSC_INPUT, SSC_NUMBER,     "adjust_en_timeindex"        , "Enable lifetime adjustment factors",     "0/1",
"'adjust' and 'en_timeindex' separated by _ instead of : after SAM 2022.12.21",                      "Adjustment Factors",      "?=0",                       "BOOLEAN",                                         "" },
{ SSC_INPUT, SSC_NUMBER,     "adjust_en_periods"        , "Enable period-based adjustment factors",     "0/1",
"'adjust' and 'en_periods' separated by _ instead of : after SAM 2022.12.21",                      "Adjustment Factors",      "?=0",                       "BOOLEAN",                                         "" },
/* removed from UI but still used in scripts
    { SSC_INPUT, SSC_NUMBER,     "adjust_en_hourly"        , "Enable hourly-based adjustment factors",     "0/1",
"'adjust' and 'en_hourly' separated by _ instead of : after SAM 2022.12.21",                      "Adjustment Factors",      "?=0",                       "BOOLEAN",                                         "" },
{ SSC_INPUT,SSC_ARRAY   , "adjust_hourly"                        , "Hourly adjustment factors"                                      , "%",
"'adjust' and 'timeindex' separated by _ instead of : after SAM 2022.12.21"                                      , "Adjustment Factors"   , "adjust_en_hourly=1"              , "LENGTH=8760"           , ""},
*/
{ SSC_INPUT,SSC_ARRAY   , "adjust_timeindex"                        , "Lifetime adjustment factors"                                      , "%",
"'adjust' and 'timeindex' separated by _ instead of : after SAM 2022.12.21"                                      , "Adjustment Factors"   , "adjust_en_timeindex=1"              , ""           , ""},
{ SSC_INPUT,SSC_MATRIX  , "adjust_periods"                       , "Period-based adjustment factors"                                , "%",
"Syntax: n x 3 matrix [ start, end, loss ]; Version upgrade: 'adjust' and 'periods' separated by _ instead of : after SAM 2022.12.21"     , "Adjustment Factors"   , "adjust_en_periods=1"              , "COLS=3"                , ""},
var_info_invalid };

var_info vtab_dc_adjustment_factors[] = {
{ SSC_INPUT,SSC_NUMBER  , "dc_adjust_constant"                   , "DC Constant loss adjustment"                                    , "%",
"'dc_adjust' and 'constant' separated by _ instead of : after SAM 2022.12.21"                                      , "Adjustment Factors"   , "?=0"               , "MAX=100"               , ""},
{ SSC_INPUT, SSC_NUMBER,     "dc_adjust_en_timeindex"        , "Enable lifetime adjustment factors",     "0/1",      "",                      "Adjustment Factors",      "?=0",                       "BOOLEAN",                                         "" },
{ SSC_INPUT, SSC_NUMBER,     "dc_adjust_en_periods"        , "Enable period-based adjustment factors",     "0/1",      "",                      "Adjustment Factors",      "?=0",                       "BOOLEAN",                                         "" },
/* removed from UI but still used in scripts
{ SSC_INPUT, SSC_NUMBER,     "dc_adjust_en_hourly"        , "Enable hourly-based adjustment factors",     "0/1",
"'dc_adjust' and 'en_hourly' separated by _ instead of : after SAM 2022.12.21",                      "Adjustment Factors",      "?=0",                       "BOOLEAN",                                         "" },
{ SSC_INPUT,SSC_ARRAY   , "dc_adjust_hourly"                        , "DC Hourly adjustment factors"                                      , "%",
"'dc_adjust' and 'timeindex' separated by _ instead of : after SAM 2022.12.21"                                      , "Adjustment Factors"   , "dc_adjust_en_hourly=1"              , "LENGTH=8760"           , ""},
*/
{ SSC_INPUT,SSC_ARRAY   , "dc_adjust_timeindex"                        , "DC Lifetime Adjustment Factors"                                      , "%"                                      , ""                                      , "Adjustment Factors"   , "dc_adjust_en_timeindex=1"              , ""           , ""},
{ SSC_INPUT,SSC_MATRIX  , "dc_adjust_periods"                    , "DC Period-based Adjustment Factors"                             , "%"                                      , "n x 3 matrix [ start, end, loss ]"     , "Adjustment Factors"   , "dc_adjust_en_periods=1"               , "COLS=3"                , ""},
var_info_invalid };

var_info vtab_sf_adjustment_factors[] = {
{ SSC_INPUT,SSC_NUMBER  , "sf_adjust_constant"                   , "SF Constant loss adjustment"                                    , "%",
"'sf_adjust' and 'constant' separated by _ instead of : after SAM 2022.12.21"                                      , "Adjustment Factors"   , "?=0"              , "MAX=100"               , ""},
{ SSC_INPUT, SSC_NUMBER,     "sf_adjust_en_timeindex"        , "Enable lifetime adjustment factors",     "0/1",      "",                      "Adjustment Factors",      "?=0",                       "BOOLEAN",                                         "" },
{ SSC_INPUT, SSC_NUMBER,     "sf_adjust_en_periods"        , "Enable period-based adjustment factors",     "0/1",      "",                      "Adjustment Factors",      "?=0",                       "BOOLEAN",                                         "" },
/* removed from UI but still used in scripts
{ SSC_INPUT, SSC_NUMBER,     "sf_adjust_en_hourly"        , "Enable hourly-based adjustment factors",     "0/1",
"'adjust' and 'en_hourly' separated by _ instead of : after SAM 2022.12.21",                      "Adjustment Factors",      "?=0",                       "BOOLEAN",                                         "" },
{ SSC_INPUT,SSC_ARRAY   , "sf_adjust_hourly"                        , "SF Hourly adjustment factors"                                      , "%",
"'adjust' and 'timeindex' separated by _ instead of : after SAM 2022.12.21"                                      , "Adjustment Factors"   , "sf_adjust_en_hourly=1"              , "LENGTH=8760"           , ""},
*/
{ SSC_INPUT,SSC_ARRAY   , "sf_adjust_timeindex"                        , "SF Lifetime Adjustment Factors"                                      , "%"                                      , ""                                      , "Adjustment Factors"   , "sf_adjust_en_timeindex=1"              , ""           , ""},
{ SSC_INPUT,SSC_MATRIX  , "sf_adjust_periods"                    , "SF Period-based Adjustment Factors"                             , "%"                                      , "n x 3 matrix [ start, end, loss ]"     , "Adjustment Factors"   , "sf_adjust_en_periods=1"              , "COLS=3"                , ""},
var_info_invalid };

var_info vtab_batt_adjustment_factors[] = {
{ SSC_INPUT,SSC_NUMBER  , "batt_adjust_constant"                   , "Battery Constant loss adjustment",        "%", "",                "Adjustment Factors", "?=0"               , "MAX=100"               , ""},
{ SSC_INPUT, SSC_NUMBER,     "batt_adjust_en_timeindex"        , "Enable battery lifetime adjustment factors",     "0/1",      "",                      "Adjustment Factors",      "?=0",                       "BOOLEAN",                                         "" },
{ SSC_INPUT, SSC_NUMBER,     "batt_adjust_en_periods"        , "Enable battery period-based adjustment factors",     "0/1",      "",                      "Adjustment Factors",      "?=0",                       "BOOLEAN",                                         "" },
{ SSC_INPUT,SSC_ARRAY   , "batt_adjust_timeindex"                        , "Battery Lifetime Adjustment Factors"                                      , "%"                                      , ""                                      , "Adjustment Factors"   , "batt_adjust_en_timeindex=1"              , ""           , ""},
{ SSC_INPUT,SSC_MATRIX  , "batt_adjust_periods"                    , "Battery Period-based Adjustment Factors"                             , "%"                                      , "n x 3 matrix [ start, end, loss ]"     , "Adjustment Factors"   , "batt_adjust_en_periods=1"               , "COLS=3"                , ""},
{ SSC_OUTPUT, SSC_ARRAY,  "batt_availability_loss",                  "Battery availability loss",                             "%", "",    "Time Series",                 "",                     "",                   "" },

var_info_invalid };

var_info vtab_financial_capacity_payments[] = {

	/*   VARTYPE           DATATYPE         NAME                                    LABEL                                             UNITS        META                               GROUP                    REQUIRED_IF           CONSTRAINTS               UI_HINTS	*/
		{ SSC_INPUT,        SSC_NUMBER,      "cp_capacity_payment_esc",             "Capacity payment escalation",                      "%/year",    "",                               "Capacity Payments",      "*",                   "",                      "" },
		{ SSC_INPUT,        SSC_NUMBER,      "cp_capacity_payment_type",            "Capacity payment type",                            "",          "0=Energy basis,1=Fixed amount",  "Capacity Payments",      "*",                   "INTEGER,MIN=0,MAX=1",   "" },
		{ SSC_INPUT,        SSC_ARRAY,       "cp_capacity_payment_amount",          "Capacity payment amount",                          "$ or $/MW", "",                               "Capacity Payments",      "*",                   "",                      "" },
		{ SSC_INPUT,        SSC_ARRAY,       "cp_capacity_credit_percent",          "Capacity credit (eligible portion of nameplate)",  "%",         "",                               "Capacity Payments",      "cp_capacity_payment_type=0",   "",                      "" },
		{ SSC_INPUT,        SSC_NUMBER,      "cp_system_nameplate",                 "System nameplate",                                 "MW",        "",                               "Capacity Payments",      "cp_capacity_payment_type=0",   "MIN=0",                 "" },
		{ SSC_INPUT,        SSC_NUMBER,      "cp_battery_nameplate",                "Battery nameplate",                                "MW",        "",                               "Capacity Payments",      "cp_capacity_payment_type=0",   "MIN=0",                 "" },

var_info_invalid };


var_info vtab_grid_curtailment[] = {
	/*   VARTYPE           DATATYPE         NAME                               LABEL                                       UNITS     META                                     GROUP                 REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

		{ SSC_INPUT,        SSC_ARRAY,       "grid_curtailment",              "Grid curtailment as energy delivery limit (first year)",              "MW",    "",                                     "GridLimits",      "?",                     "",                "" },
        { SSC_INPUT,        SSC_NUMBER,      "enable_interconnection_limit",      "Enable grid interconnection limit",     "0/1",     "Enable a grid interconnection limit",   "GridLimits",        "","",                           "" },
        { SSC_INPUT,        SSC_NUMBER,      "grid_interconnection_limit_kwac",   "Grid interconnection limit",            "kWac",    "",                                      "GridLimits",        "","",                           "" },

    var_info_invalid };


var_info vtab_technology_outputs[] = {
	// instantaneous power at each timestep - consistent with sun position
{ SSC_OUTPUT, SSC_ARRAY , "gen",                                   "System power generated",                        "kW",         "", "Time Series",      "*",     "",    ""},
{ SSC_OUTPUT, SSC_MATRIX, "annual_energy_distribution_time",	   "Annual energy production as function of time",	"kW",		  "", "Heatmaps",		  "",	   "",	  ""},

    var_info_invalid };

ssc_number_t* gen_heatmap(compute_module* cm, double step_per_hour, bool heat) {
    if (!cm)
        return 0;
    size_t count = (size_t)(8760 * step_per_hour);
 //   size_t imonth = 0;
    size_t iday = 0;
    size_t hour;
    size_t count_gen;
    ssc_number_t* p_gen = NULL;
    if (heat)
        p_gen = cm->as_array("gen_heat", &count_gen);
    else
        p_gen = cm->as_array("gen", &count_gen);

    ssc_number_t* p_annual_energy_dist_time = cm->allocate("annual_energy_distribution_time", 25, 366);
    for (size_t i = 0; i < count; i++) {
        hour = (size_t)fmod(floor(double(i) / step_per_hour), 24);
        iday = (size_t)floor((double(i) / step_per_hour) / 24) ;
        for (size_t d = 0; d < 366; d++) {
            for (size_t h = 0; h < 25; h++) {
                if (i == 0) {
                    p_annual_energy_dist_time[h * 366] = (ssc_number_t)(h - 1);
                    p_annual_energy_dist_time[d] = (ssc_number_t)d;
                }
                if (iday == d && hour == (h - 1) && d != 365) {
                    p_annual_energy_dist_time[h * 366 + d + 1] += p_gen[i] * 1 / step_per_hour;
                    break;
                }
            }
        }
    }
    p_annual_energy_dist_time[0] = 0;
    return p_annual_energy_dist_time;


}

var_info vtab_p50p90[] = {
        { SSC_INPUT, SSC_NUMBER ,  "total_uncert"                 , "Total uncertainty in energy production as percent of annual energy", "%"                                   , ""                                      , "Uncertainty"          , ""              , "MIN=0,MAX=100"         , ""},
        { SSC_OUTPUT, SSC_NUMBER , "annual_energy_p75"            , "Annual energy with 75% probability of exceedance"                  , "kWh"                                 , ""                                      , "Uncertainty"          , ""              , ""                      , ""},
        { SSC_OUTPUT, SSC_NUMBER , "annual_energy_p90"            , "Annual energy with 90% probability of exceedance"                  , "kWh"                                 , ""                                      , "Uncertainty"          , ""              , ""                      , ""},
        { SSC_OUTPUT, SSC_NUMBER , "annual_energy_p95"            , "Annual energy with 95% probability of exceedance"                  , "kWh"                                 , ""                                      , "Uncertainty"          , ""              , ""                      , ""},
        var_info_invalid };

bool calculate_p50p90(compute_module *cm){
    if (!cm->is_assigned("total_uncert") || !cm->is_assigned("annual_energy"))
        return false;
    double aep = cm->as_double("annual_energy");
    double uncert = cm->as_double("total_uncert")/100.;
    cm->assign("annual_energy_p75", aep * (-0.67 * uncert + 1));
    cm->assign("annual_energy_p90", aep * (-1.28 * uncert + 1));
    cm->assign("annual_energy_p95", aep * (-1.64 * uncert + 1));
	return true;
}

/* the conditions on inputs will have to be expanded and abstracted to handle all technologies with dispatchable storage */
var_info vtab_forecast_price_signal[] = {
	// model selected PPA or Merchant Plant based
	{ SSC_INPUT,        SSC_NUMBER,     "forecast_price_signal_model",					"Forecast price signal model selected",   "0/1",   "0=PPA based,1=Merchant Plant",    "Price Signal",  "?=0",	"INTEGER,MIN=0,MAX=1",      "" },

	// PPA financial inputs
	{ SSC_INPUT,        SSC_ARRAY,      "ppa_price_input",		                        "PPA Price Input",	                                        "$/kWh",      "",               "Price Signal", "forecast_price_signal_model=0&en_batt=1&batt_meter_position=1"   "",          "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ppa_multiplier_model",                         "PPA multiplier model",                                    "0/1",    "0=diurnal,1=timestep","Price Signal", "forecast_price_signal_model=0&en_batt=1&batt_meter_position=1",                                                  "INTEGER,MIN=0", "" },
    { SSC_INPUT,        SSC_NUMBER,      "ppa_escalation",		                        "PPA escalation rate",	                                    "%/year",      "",            "Price Signal", "forecast_price_signal_model=0&en_batt=1&batt_meter_position=1"   "",          "" },
    { SSC_INPUT,        SSC_ARRAY,      "dispatch_factors_ts",                          "Dispatch payment factor time step",                        "",      "",                  "Price Signal", "forecast_price_signal_model=0&en_batt=1&batt_meter_position=1&ppa_multiplier_model=1", "", "" },
	{ SSC_INPUT,        SSC_ARRAY,      "dispatch_tod_factors",		                    "TOD factors for periods 1-9",	                            "",      "",                  "Price Signal", "en_batt=1&batt_meter_position=1&forecast_price_signal_model=0&ppa_multiplier_model=0"   "",          "" },
	{ SSC_INPUT,        SSC_MATRIX,     "dispatch_sched_weekday",                       "Diurnal weekday TOD periods",                              "1..9",  "12 x 24 matrix",    "Price Signal", "en_batt=1&batt_meter_position=1&forecast_price_signal_model=0&ppa_multiplier_model=0",  "",          "" },
	{ SSC_INPUT,        SSC_MATRIX,     "dispatch_sched_weekend",                       "Diurnal weekend TOD periods",                              "1..9",  "12 x 24 matrix",    "Price Signal", "en_batt=1&batt_meter_position=1&forecast_price_signal_model=0&ppa_multiplier_model=0",  "",          "" },
// Merchant plant inputs
	{ SSC_INPUT,        SSC_NUMBER,     "mp_enable_energy_market_revenue",				"Enable energy market revenue",   "0/1",   "0=false,1=true",    "Price Signal",  "en_batt=1&batt_meter_position=1&forecast_price_signal_model=1",	"INTEGER,MIN=0,MAX=1",      "" },
	{ SSC_INPUT,		SSC_MATRIX,		"mp_energy_market_revenue",						"Energy market revenue input", " [MW, $/MW]", "", "Price Signal","en_batt=1&batt_meter_position=1&forecast_price_signal_model=1",  ""},
	{ SSC_INPUT,        SSC_NUMBER,     "mp_enable_ancserv1",							"Enable ancillary services 1 revenue",   "0/1",   "",    "Price Signal",  "forecast_price_signal_model=1",	"INTEGER,MIN=0,MAX=1",      "" },
	{ SSC_INPUT,		SSC_MATRIX,		"mp_ancserv1_revenue",							"Ancillary services 1 revenue input", " [MW, $/MW]", "", "Price Signal", "en_batt=1&batt_meter_position=1&forecast_price_signal_model=1", "" },
	{ SSC_INPUT,        SSC_NUMBER,     "mp_enable_ancserv2",							"Enable ancillary services 2 revenue",   "0/1",   "",    "Price Signal",  "forecast_price_signal_model=1",	"INTEGER,MIN=0,MAX=1",      "" },
	{ SSC_INPUT,		SSC_MATRIX,		"mp_ancserv2_revenue",							"Ancillary services 2 revenue input", " [MW, $/MW]", "", "Price Signal", "en_batt=1&batt_meter_position=1&forecast_price_signal_model=1", ""},
	{ SSC_INPUT,        SSC_NUMBER,     "mp_enable_ancserv3",							"Enable ancillary services 3 revenue",   "0/1",   "",    "Price Signal",  "forecast_price_signal_model=1",	"INTEGER,MIN=0,MAX=1",      "" },
	{ SSC_INPUT,		SSC_MATRIX,		"mp_ancserv3_revenue",							"Ancillary services 3 revenue input", " [MW, $/MW]", "","Price Signal", "en_batt=1&batt_meter_position=1&forecast_price_signal_model=1", "" },
	{ SSC_INPUT,        SSC_NUMBER,     "mp_enable_ancserv4",							"Enable ancillary services 4 revenue",   "0/1",   "",    "Price Signal",  "forecast_price_signal_model=1",	"INTEGER,MIN=0,MAX=1",      "" },
	{ SSC_INPUT,		SSC_MATRIX,		"mp_ancserv4_revenue",							"Ancillary services 4 revenue input", " [MW, $/MW]", "","Price Signal", "en_batt=1&batt_meter_position=1&forecast_price_signal_model=1", ""},

    // percent generation matrices (1-column)
    { SSC_INPUT, SSC_MATRIX, "mp_energy_market_revenue_single", "Energy market revenue input", "", "Lifetime x 1 [Price($/MWh)]","Revenue", "forecast_price_signal_model=1&mp_enable_market_percent_gen=1", "", "" },
    { SSC_INPUT, SSC_MATRIX, "mp_ancserv1_revenue_single", "Ancillary services 1 revenue input", "", "Lifetime x 1[Price($/MWh)]","Revenue", "forecast_price_signal_model=1&mp_enable_ancserv1_percent_gen=1", "", "" },
    { SSC_INPUT, SSC_MATRIX, "mp_ancserv2_revenue_single", "Ancillary services 2 revenue input", "", "Lifetime x 1[Price($/MWh)]","Revenue", "forecast_price_signal_model=1&mp_enable_ancserv2_percent_gen=1", "", "" },
    { SSC_INPUT, SSC_MATRIX, "mp_ancserv3_revenue_single", "Ancillary services 3 revenue input", "", "Lifetime x 1[Price($/MWh)]","Revenue", "forecast_price_signal_model=1&mp_enable_ancserv3_percent_gen=1", "", "" },
    { SSC_INPUT, SSC_MATRIX, "mp_ancserv4_revenue_single", "Ancillary services 4 revenue input", "", "Lifetime x 1[Price($/MWh)]","Revenue", "forecast_price_signal_model=1&mp_enable_ancserv4_percent_gen=1", "", "" },

    // percent generation variables
    { SSC_INPUT,        SSC_NUMBER,     "mp_enable_market_percent_gen",		      "Enable percent demand cleared capacity option for market revenue",   "0/1",   "",    "Revenue",  "forecast_price_signal_model=1",	"INTEGER,MIN=0,MAX=1",      "" },
    { SSC_INPUT,        SSC_NUMBER,     "mp_enable_ancserv1_percent_gen",		      "Enable percent demand cleared capacity option for ancillary service 1",   "0/1",   "",    "Revenue",  "forecast_price_signal_model=1",	"INTEGER,MIN=0,MAX=1",      "" },
    { SSC_INPUT,        SSC_NUMBER,     "mp_enable_ancserv2_percent_gen",		      "Enable percent demand cleared capacity option for ancillary service 2",   "0/1",   "",    "Revenue",  "forecast_price_signal_model=1",	"INTEGER,MIN=0,MAX=1",      "" },
    { SSC_INPUT,        SSC_NUMBER,     "mp_enable_ancserv3_percent_gen",		      "Enable percent demand cleared capacity option for ancillary service 3",   "0/1",   "",    "Revenue",  "forecast_price_signal_model=1",	"INTEGER,MIN=0,MAX=1",      "" },
    { SSC_INPUT,        SSC_NUMBER,     "mp_enable_ancserv4_percent_gen",		      "Enable percent demand cleared capacity option for ancillary service 4",   "0/1",   "",    "Revenue",  "forecast_price_signal_model=1",	"INTEGER,MIN=0,MAX=1",      "" },
    { SSC_INPUT,        SSC_NUMBER,     "mp_market_percent_gen",                 "Percent of demand to copy to cleared capacity array",       "%","", "Revenue", "forecast_price_signal_model=1&mp_enable_market_percent_gen=1", "MIN=0,MAX=100", "" },
    { SSC_INPUT,        SSC_NUMBER,     "mp_ancserv1_percent_gen",               "Percent of demand to copy to cleared capacity array",       "%","", "Revenue", "forecast_price_signal_model=1&mp_enable_ancserv1_percent_gen=1", "MIN=0,MAX=100", "" },
    { SSC_INPUT,        SSC_NUMBER,     "mp_ancserv2_percent_gen",               "Percent of demand to copy to cleared capacity array",       "%","", "Revenue", "forecast_price_signal_model=1&mp_enable_ancserv2_percent_gen=1", "MIN=0,MAX=100", "" },
    { SSC_INPUT,        SSC_NUMBER,     "mp_ancserv3_percent_gen",               "Percent of demand to copy to cleared capacity array",       "%","", "Revenue", "forecast_price_signal_model=1&mp_enable_ancserv3_percent_gen=1", "MIN=0,MAX=100", "" },
    { SSC_INPUT,        SSC_NUMBER,     "mp_ancserv4_percent_gen",               "Percent of demand to copy to cleared capacity array",       "%","", "Revenue", "forecast_price_signal_model=1&mp_enable_ancserv4_percent_gen=1", "MIN=0,MAX=100", "" },

var_info_invalid };

forecast_price_signal::forecast_price_signal(var_table *vt)
	: vartab(vt)
{
}

bool forecast_price_signal::setup(size_t step_per_hour)
{
    size_t nsteps = 8760 * step_per_hour;;

	int forecast_price_signal_model = vartab->as_integer("forecast_price_signal_model");
    size_t nyears = vartab->as_unsigned_long("analysis_period");

	if (forecast_price_signal_model == 1)
	{
		// merchant plant additional revenue streams
		// enabled/disabled booleans
		bool en_mp_energy_market = (vartab->as_integer("mp_enable_energy_market_revenue") == 1);
		bool en_mp_ancserv1 = (vartab->as_integer("mp_enable_ancserv1") == 1);
		bool en_mp_ancserv2 = (vartab->as_integer("mp_enable_ancserv2") == 1);
		bool en_mp_ancserv3 = (vartab->as_integer("mp_enable_ancserv3") == 1);
		bool en_mp_ancserv4 = (vartab->as_integer("mp_enable_ancserv4") == 1);

        int mp_enable_market_percent_gen = vartab->as_integer("mp_enable_market_percent_gen");
        int mp_enable_ancserv1_percent_gen = vartab->as_integer("mp_enable_ancserv1_percent_gen");
        int mp_enable_ancserv2_percent_gen = vartab->as_integer("mp_enable_ancserv2_percent_gen");
        int mp_enable_ancserv3_percent_gen = vartab->as_integer("mp_enable_ancserv3_percent_gen");
        int mp_enable_ancserv4_percent_gen = vartab->as_integer("mp_enable_ancserv4_percent_gen");

        double mp_market_percent_gen = 0;
        if (vartab->is_assigned("mp_market_percent_gen") && mp_enable_market_percent_gen)
            mp_market_percent_gen = vartab->as_number("mp_market_percent_gen") / 100.0;
        double ancserv1_percent_gen = 0;
        if (vartab->is_assigned("mp_ancserv1_percent_gen") && mp_enable_ancserv1_percent_gen)
            ancserv1_percent_gen = vartab->as_number("mp_ancserv1_percent_gen") / 100.0;
        double ancserv2_percent_gen = 0;
        if (vartab->is_assigned("mp_ancserv2_percent_gen") && mp_enable_ancserv2_percent_gen)
            ancserv2_percent_gen = vartab->as_number("mp_ancserv2_percent_gen") / 100.0;
        double ancserv3_percent_gen = 0;
        if (vartab->is_assigned("mp_ancserv3_percent_gen") && mp_enable_ancserv3_percent_gen)
            ancserv3_percent_gen = vartab->as_number("mp_ancserv3_percent_gen") / 100.0;
        double ancserv4_percent_gen = 0;
        if (vartab->is_assigned("mp_ancserv4_percent_gen") && mp_enable_ancserv4_percent_gen)
            ancserv4_percent_gen = vartab->as_number("mp_ancserv4_percent_gen") / 100.0;

		// cleared capacity and price columns
		// assume cleared capacities valid and use as generation for forecasting - will verify after generation in the financial models.
		size_t nrows, ncols;
		util::matrix_t<double> mp_energy_market_revenue_mat(1, 2, 0.0);
		if (en_mp_energy_market)
		{
            bool percent_gen = mp_enable_market_percent_gen > 0.5;
			ssc_number_t *mp_energy_market_revenue_in = vartab->as_matrix("mp_energy_market_revenue" + std::string((percent_gen) ? "_single" : ""), &nrows, &ncols);
			if (ncols != 2 && !percent_gen)
			{
				m_error = util::format("The energy market revenue table must have 2 columns. Instead it has %d columns.", (int)ncols);
				return false;
			}
            else if (percent_gen && ncols != 1) {
                m_error = util::format("The energy market revenue table must have 1 column. Instead it has %d columns.", (int)ncols);
                return false;
            }
			mp_energy_market_revenue_mat.resize(nrows, ncols);
			mp_energy_market_revenue_mat.assign(mp_energy_market_revenue_in, nrows, ncols);
		}

		util::matrix_t<double> mp_ancserv_1_revenue_mat(1, 2, 0.0);
		if (en_mp_ancserv1)
		{
            bool percent_gen = mp_enable_ancserv1_percent_gen > 0.5;
			ssc_number_t *mp_ancserv1_revenue_in = vartab->as_matrix("mp_ancserv1_revenue" + std::string((percent_gen) ? "_single" : ""), &nrows, &ncols);
			if (ncols != 2 && !percent_gen)
			{
				m_error = util::format("The ancillary services revenue 1 table must have 2 columns. Instead it has %d columns.", (int)ncols);
				return false;
			}
            else if (percent_gen && ncols != 1) {
                m_error = util::format("The  ancillary services revenue 1 table must have 1 column. Instead it has %d columns.", (int)ncols);
                return false;
            }
			mp_ancserv_1_revenue_mat.resize(nrows, ncols);
			mp_ancserv_1_revenue_mat.assign(mp_ancserv1_revenue_in, nrows, ncols);
		}

		util::matrix_t<double> mp_ancserv_2_revenue_mat(1, 2, 0.0);
		if (en_mp_ancserv2)
		{
            bool percent_gen = mp_enable_ancserv2_percent_gen > 0.5;

			ssc_number_t *mp_ancserv2_revenue_in = vartab->as_matrix("mp_ancserv2_revenue" + std::string((percent_gen) ? "_single" : ""), &nrows, &ncols);
			if (ncols != 2 && !percent_gen)
			{
				m_error = util::format("The ancillary services revenue 2 table must have 2 columns. Instead it has %d columns.", (int)ncols);
				return false;
			}
            else if (percent_gen && ncols != 1) {
                m_error = util::format("The  ancillary services revenue 2 table must have 1 column. Instead it has %d columns.", (int)ncols);
                return false;
            }
			mp_ancserv_2_revenue_mat.resize(nrows, ncols);
			mp_ancserv_2_revenue_mat.assign(mp_ancserv2_revenue_in, nrows, ncols);
		}

		util::matrix_t<double> mp_ancserv_3_revenue_mat(1, 2, 0.0);
		if (en_mp_ancserv3)
		{
            bool percent_gen = mp_enable_ancserv3_percent_gen > 0.5;

			ssc_number_t *mp_ancserv3_revenue_in = vartab->as_matrix("mp_ancserv3_revenue" + std::string((percent_gen) ? "_single" : ""), &nrows, &ncols);
			if (ncols != 2 && !percent_gen)
			{
				m_error = util::format("The ancillary services revenue 3 table must have 2 columns. Instead it has %d columns.", (int)ncols);
				return false;
			}
            else if (percent_gen && ncols != 1) {
                m_error = util::format("The  ancillary services revenue 3 table must have 1 column. Instead it has %d columns.", (int)ncols);
                return false;
            }
			mp_ancserv_3_revenue_mat.resize(nrows, ncols);
			mp_ancserv_3_revenue_mat.assign(mp_ancserv3_revenue_in, nrows, ncols);
		}

		util::matrix_t<double> mp_ancserv_4_revenue_mat(1, 2, 0.0);
		if (en_mp_ancserv4)
		{
            bool percent_gen = mp_enable_ancserv4_percent_gen > 0.5;

			ssc_number_t *mp_ancserv4_revenue_in = vartab->as_matrix("mp_ancserv4_revenue" + std::string((percent_gen) ? "_single" : ""), &nrows, &ncols);
			if (ncols != 2 && !percent_gen)
			{
				m_error = util::format("The ancillary services revenue 4 table must have 2 columns. Instead it has %d columns.", (int)ncols);
				return false;
			}
            else if (percent_gen && ncols != 1) {
                m_error = util::format("The  ancillary services revenue 4 table must have 1 column. Instead it has %d columns.", (int)ncols);
                return false;
            }
			mp_ancserv_4_revenue_mat.resize(nrows, ncols);
			mp_ancserv_4_revenue_mat.assign(mp_ancserv4_revenue_in, nrows, ncols);
		}

        if ( nsteps > 8760 * nyears) step_per_hour = nsteps / (8760 * nyears);
        if (step_per_hour < 1 || step_per_hour > 60 || step_per_hour * 8760 != nsteps)
        {
            m_error = util::format("The requested number of timesteps must be a multiple of 8760. Instead requested timesteps is %d.", (int)nsteps);
            return false;
        }
        m_forecast_price.reserve(nsteps * nyears);
        m_cleared_capacity.reserve(nsteps * nyears);
        for (size_t i = 0; i < nsteps * nyears; i++) {
            m_forecast_price.push_back(0.0);
            m_cleared_capacity.push_back(0.0);
        }

		// calculate revenue and consolidate to m_forecast_price
		std::vector<double> as_revenue;
		std::vector<double> as_revenue_extrapolated_em(nsteps,0.0);
		std::vector<double> as_revenue_extrapolated_ancserv_1(nsteps,0.0);
		std::vector<double> as_revenue_extrapolated_ancserv_2(nsteps,0.0);
		std::vector<double> as_revenue_extrapolated_ancserv_3(nsteps,0.0);
		std::vector<double> as_revenue_extrapolated_ancserv_4(nsteps,0.0);
        std::vector<double> as_capacity;
        std::vector<double> as_capacity_extrapolated_em(nsteps, 0.0);
        std::vector<double> as_capacity_extrapolated_ancserv_1(nsteps, 0.0);
        std::vector<double> as_capacity_extrapolated_ancserv_2(nsteps, 0.0);
        std::vector<double> as_capacity_extrapolated_ancserv_3(nsteps, 0.0);
        std::vector<double> as_capacity_extrapolated_ancserv_4(nsteps, 0.0);

        // Users can mix and match percent of generation and cleared capacity. Determine what percent is available for cleared capacity calcs
        cleared_capacity_percent = 1 - (mp_market_percent_gen + ancserv1_percent_gen + ancserv2_percent_gen
            + ancserv3_percent_gen + ancserv4_percent_gen);

        for (size_t y = 0; y < nyears; y++) {
            size_t forecast_start = y * nsteps;
            size_t forecast_end = (y + 1) * nsteps;
            // First: adapt all merchant plant timeseries to the simulation timestep
            size_t n_marketrevenue_per_year = mp_energy_market_revenue_mat.nrows() / (size_t)nyears;
            as_revenue.clear();
            as_revenue.reserve(n_marketrevenue_per_year);
            as_capacity.clear();
            as_capacity.reserve(n_marketrevenue_per_year);
            if (n_marketrevenue_per_year == 0) {
                if (mp_energy_market_revenue_mat.nrows() == 1) {
                    if (mp_enable_market_percent_gen) {
                        as_revenue.push_back(mp_energy_market_revenue_mat.at(0, 0) / 1000.0);
                    }
                    else {
                        as_capacity.push_back(mp_energy_market_revenue_mat.at(0, 0) * 1000.0);
                        as_revenue.push_back(mp_energy_market_revenue_mat.at(0, 1) / 1000.0);
                    }
                }
            }
            else {
                for (size_t j = y * n_marketrevenue_per_year; j < (y + 1) * n_marketrevenue_per_year; j++) {
                    as_revenue.push_back(mp_energy_market_revenue_mat.at(j, 1 - mp_enable_market_percent_gen) / 1000.0);
                    if (!mp_enable_market_percent_gen) {
                        as_capacity.push_back(mp_energy_market_revenue_mat.at(j, 0) * 1000.0);
                    }
                }
            }

            as_revenue_extrapolated_em = extrapolate_timeseries(as_revenue, step_per_hour);
            as_capacity_extrapolated_em = extrapolate_timeseries(as_capacity, step_per_hour);
            
            size_t n_ancserv_1_revenue_per_year = mp_ancserv_1_revenue_mat.nrows() / (size_t)nyears;
            as_revenue.clear();
            as_revenue.reserve(n_ancserv_1_revenue_per_year);
            as_capacity.clear();
            as_capacity.reserve(n_ancserv_1_revenue_per_year);
            if (n_ancserv_1_revenue_per_year == 0) {
                if (mp_ancserv_1_revenue_mat.nrows() == 1) {
                    if (mp_enable_ancserv1_percent_gen) {
                        as_revenue.push_back(mp_ancserv_1_revenue_mat.at(0, 0) / 1000.0);
                    }
                    else {
                        as_capacity.push_back(mp_ancserv_1_revenue_mat.at(0, 0) * 1000.0);
                        as_revenue.push_back(mp_ancserv_1_revenue_mat.at(0, 1) / 1000.0);
                    }
                }
            }
            else {
                for (size_t j = y * n_ancserv_1_revenue_per_year; j < (y + 1) * n_ancserv_1_revenue_per_year; j++) {
                    as_revenue.push_back(mp_ancserv_1_revenue_mat.at(j, 1 - mp_enable_ancserv1_percent_gen) / 1000.0);
                    if (!mp_enable_ancserv1_percent_gen) {
                        as_capacity.push_back(mp_ancserv_1_revenue_mat.at(j, 0) * 1000.0);
                    }
                }
            }
            as_revenue_extrapolated_ancserv_1 = extrapolate_timeseries(as_revenue, step_per_hour);
            as_capacity_extrapolated_ancserv_1 = extrapolate_timeseries(as_capacity, step_per_hour);
            
            size_t n_ancserv_2_revenue_per_year = mp_ancserv_2_revenue_mat.nrows() / (size_t)nyears;
            as_revenue.clear();
            as_revenue.reserve(n_ancserv_2_revenue_per_year);
            as_capacity.clear();
            as_capacity.reserve(n_ancserv_2_revenue_per_year);
            if (n_ancserv_2_revenue_per_year == 0) {
                if (mp_ancserv_2_revenue_mat.nrows() == 1) {
                    if (mp_enable_ancserv2_percent_gen) {
                        as_revenue.push_back(mp_ancserv_2_revenue_mat.at(0, 0) / 1000.0);
                    }
                    else {
                        as_capacity.push_back(mp_ancserv_2_revenue_mat.at(0, 0) * 1000.0);
                        as_revenue.push_back(mp_ancserv_2_revenue_mat.at(0, 1) / 1000.0);
                    }
                }
            }
            else {
                for (size_t j = y * n_ancserv_2_revenue_per_year; j < (y + 1) * n_ancserv_2_revenue_per_year; j++) {
                    as_revenue.push_back(mp_ancserv_2_revenue_mat.at(j, 1 - mp_enable_ancserv2_percent_gen) / 1000.0);
                    if (!mp_enable_ancserv2_percent_gen) {
                        as_capacity.push_back(mp_ancserv_2_revenue_mat.at(j, 0) * 1000.0);
                    }
                }
            }
            as_revenue_extrapolated_ancserv_2 = extrapolate_timeseries(as_revenue, step_per_hour);
            as_capacity_extrapolated_ancserv_2 = extrapolate_timeseries(as_capacity, step_per_hour);
            
            size_t n_ancserv_3_revenue_per_year = mp_ancserv_3_revenue_mat.nrows() / (size_t)nyears;
            as_revenue.clear();
            as_revenue.reserve(n_ancserv_3_revenue_per_year);
            as_capacity.clear();
            as_capacity.reserve(n_ancserv_3_revenue_per_year);
            if (n_ancserv_3_revenue_per_year == 0) {
                if (mp_ancserv_3_revenue_mat.nrows() == 1) {
                    if (mp_enable_ancserv3_percent_gen) {
                        as_revenue.push_back(mp_ancserv_3_revenue_mat.at(0, 0) / 1000.0);
                    }
                    else {
                        as_capacity.push_back(mp_ancserv_3_revenue_mat.at(0, 0) * 1000.0);
                        as_revenue.push_back(mp_ancserv_3_revenue_mat.at(0, 1) / 1000.0);
                    }
                }
            }
            else {
                for (size_t j = y * n_ancserv_3_revenue_per_year; j < (y + 1) * n_ancserv_3_revenue_per_year; j++) {
                    as_revenue.push_back(mp_ancserv_3_revenue_mat.at(j, 1 - mp_enable_ancserv3_percent_gen) / 1000.0);
                    if (!mp_enable_ancserv3_percent_gen) {
                        as_capacity.push_back(mp_ancserv_3_revenue_mat.at(j, 0) * 1000.0);
                    }
                }
            }
            as_revenue_extrapolated_ancserv_3 = extrapolate_timeseries(as_revenue, step_per_hour);
            as_capacity_extrapolated_ancserv_3 = extrapolate_timeseries(as_capacity, step_per_hour);
            
            size_t n_ancserv_4_revenue_per_year = mp_ancserv_4_revenue_mat.nrows() / (size_t)nyears;
            as_revenue.clear();
            as_revenue.reserve(n_ancserv_4_revenue_per_year);
            as_capacity.clear();
            as_capacity.reserve(n_ancserv_4_revenue_per_year);
            if (n_ancserv_4_revenue_per_year == 0) {
                if (mp_ancserv_4_revenue_mat.nrows() == 1) {
                    if (mp_enable_ancserv4_percent_gen) {
                        as_revenue.push_back(mp_ancserv_4_revenue_mat.at(0, 0) / 1000.0);
                    }
                    else {
                        as_capacity.push_back(mp_ancserv_4_revenue_mat.at(0, 0) * 1000.0);
                        as_revenue.push_back(mp_ancserv_4_revenue_mat.at(0, 1) / 1000.0);
                    }
                }
            }
            else {
                for (size_t j = y * n_ancserv_4_revenue_per_year; j < (y + 1) * n_ancserv_4_revenue_per_year; j++) {
                    as_revenue.push_back(mp_ancserv_4_revenue_mat.at(j, 1 - mp_enable_ancserv4_percent_gen) / 1000.0);
                    if (!mp_enable_ancserv4_percent_gen) {
                        as_capacity.push_back(mp_ancserv_4_revenue_mat.at(j, 0) * 1000.0);
                    }
                }
            }
            as_revenue_extrapolated_ancserv_4 = extrapolate_timeseries(as_revenue, step_per_hour);
            as_capacity_extrapolated_ancserv_4 = extrapolate_timeseries(as_capacity, step_per_hour);

            // Then: sum weighted average prices for timeseries use
            for (size_t j = 0; j < nsteps; j++) {
                m_cleared_capacity[j + forecast_start] = as_capacity_extrapolated_em[j] + as_capacity_extrapolated_ancserv_1[j]
                    + as_capacity_extrapolated_ancserv_2[j] + as_capacity_extrapolated_ancserv_3[j] + as_capacity_extrapolated_ancserv_4[j];

                if (mp_enable_market_percent_gen) {
                    m_forecast_price[j + forecast_start] += as_revenue_extrapolated_em[j] * mp_market_percent_gen;
                }
                else if (m_cleared_capacity[j + forecast_start] > 0) {
                    m_forecast_price[j + forecast_start] += as_capacity_extrapolated_em[j] / m_cleared_capacity[j + forecast_start] * cleared_capacity_percent;
                }

                if (mp_enable_ancserv1_percent_gen) {
                    m_forecast_price[j + forecast_start] += as_revenue_extrapolated_ancserv_1[j] * ancserv1_percent_gen;
                }
                else if (m_cleared_capacity[j + forecast_start] > 0) {
                    m_forecast_price[j + forecast_start] += as_capacity_extrapolated_ancserv_1[j] / m_cleared_capacity[j + forecast_start] * cleared_capacity_percent;
                }

                if (mp_enable_ancserv2_percent_gen) {
                    m_forecast_price[j + forecast_start] += as_revenue_extrapolated_ancserv_2[j] * ancserv2_percent_gen;
                }
                else if (m_cleared_capacity[j + forecast_start] > 0) {
                    m_forecast_price[j + forecast_start] += as_capacity_extrapolated_ancserv_2[j] / m_cleared_capacity[j + forecast_start] * cleared_capacity_percent;
                }

                if (mp_enable_ancserv3_percent_gen) {
                    m_forecast_price[j + forecast_start] += as_revenue_extrapolated_ancserv_3[j] * ancserv3_percent_gen;
                }
                else if (m_cleared_capacity[j + forecast_start] > 0) {
                    m_forecast_price[j + forecast_start] += as_capacity_extrapolated_ancserv_3[j] / m_cleared_capacity[j + forecast_start] * cleared_capacity_percent;
                }

                if (mp_enable_ancserv4_percent_gen) {
                    m_forecast_price[j + forecast_start] += as_revenue_extrapolated_ancserv_4[j] * ancserv4_percent_gen;
                }
                else if (m_cleared_capacity[j + forecast_start] > 0) {
                    m_forecast_price[j + forecast_start] += as_capacity_extrapolated_ancserv_4[j] / m_cleared_capacity[j + forecast_start] * cleared_capacity_percent;
                }

            }
        }

        if (cleared_capacity_percent < 1e-7) {
            forecast_type = dispatch_t::PRICE_ONLY;
        }
        else if (cleared_capacity_percent > (1 - 1e-7)) {
            forecast_type = dispatch_t::CAPACITY_ONLY;
        }
        else {
            forecast_type = dispatch_t::PRICE_AND_CAPACITY;
        }

	}
	else
	{
		int ppa_multiplier_mode = vartab->as_integer("ppa_multiplier_model");
		size_t count_ppa_price_input;
		ssc_number_t* ppa_price = vartab->as_array("ppa_price_input", &count_ppa_price_input);
		if (count_ppa_price_input < 1)
		{
			m_error = util::format("The ppa price array needs at least one entry. Input had less than one input.");
			return false;
		}

        double ppa_escalation = vartab->as_double("ppa_escalation") * 0.01;
        double ppa = ppa_price[0];

        m_forecast_price.reserve(nsteps* nyears);
        for (size_t i = 0; i < nsteps * nyears; i++)
            m_forecast_price.push_back(0.0);
        forecast_type = dispatch_t::PRICE_ONLY;
        cleared_capacity_percent = 0.0;
        m_cleared_capacity.clear();

        std::vector<double> as_revenue;

        for (size_t y = 0; y < nyears; y++) {
            size_t forecast_start = y * nsteps;
            size_t forecast_end = (y + 1) * nsteps;

            as_revenue.clear();
            as_revenue.reserve(nsteps);

            if (count_ppa_price_input > 1) {
                if (y < (int)count_ppa_price_input) {
                    ppa_price[y];
                }
                else {
                    ppa = 0; // Match convention in single owner 
                }
            }
            else {
                ppa = ppa_price[0] * pow(1 + ppa_escalation, y);
            }

            if (ppa_multiplier_mode == 0)
            {
                as_revenue = flatten_diurnal(
                    vartab->as_matrix_unsigned_long("dispatch_sched_weekday"),
                    vartab->as_matrix_unsigned_long("dispatch_sched_weekend"),
                    step_per_hour,
                    vartab->as_vector_double("dispatch_tod_factors"), ppa);
            }
            else
            { // assumption on size - check that is requested size.
                std::vector<double> factors = vartab->as_vector_double("dispatch_factors_ts");
                as_revenue = extrapolate_timeseries(factors, step_per_hour, ppa);
            }

            std::transform(m_forecast_price.begin() + forecast_start, m_forecast_price.begin() + forecast_end, as_revenue.begin(), m_forecast_price.begin() + forecast_start, std::plus<double>());

        }
	}

	return true;
}

ssc_number_t forecast_price_signal::operator()(size_t time)
{ // dollar value at timestep "time"
	if (time < m_forecast_price.size()) return m_forecast_price[time];
	else return 0.0;
}

var_info vtab_resilience_outputs[] = {
{ SSC_OUTPUT, SSC_ARRAY  , "resilience_hrs"     , "Hours of autonomy during grid outage at each timestep"            , "hr"  , ""                                                         , "Resilience" , ""  , ""             , ""},
{ SSC_OUTPUT, SSC_NUMBER , "resilience_hrs_min" , "Hours of autonomy during grid outage minimum"                     , "hr"  , ""                                                         , "Resilience" , ""  , "MIN=0"        , ""},
{ SSC_OUTPUT, SSC_NUMBER , "resilience_hrs_max" , "Hours of autonomy during grid outage maximum"                     , "hr"  , ""                                                         , "Resilience" , ""  , "MIN=0"        , ""},
{ SSC_OUTPUT, SSC_NUMBER , "resilience_hrs_avg" , "Hours of autonomy during grid outage average"                     , "hr"  , ""                                                         , "Resilience" , ""  , "MIN=0"        , ""},
{ SSC_OUTPUT, SSC_ARRAY  , "outage_durations"   , "Hours of autonomy during grid outage hour list from min to max"   , "hr"  , "Hours from resilience_hrs_min to resilience_hrs_max"      , "Resilience" , ""  , ""             , ""},
{ SSC_OUTPUT, SSC_ARRAY  , "pdf_of_surviving"   , "Hours of autonomy during grid outage probabilities"               , ""    , "Hours from resilience_hrs_min to resilience_hrs_max"      , "Resilience" , ""  , ""  , ""},
{ SSC_OUTPUT, SSC_ARRAY  , "cdf_of_surviving"   , "Hours of autonomy during grid outage cumulative probabilities"    , ""    , "Prob surviving at least x hrs; hrs from min to max"       , "Resilience" , ""  , ""  , ""},
{ SSC_OUTPUT, SSC_ARRAY  , "survival_function"  , "Hours of autonomy during grid outage survival function"           , ""    , "Prob surviving greater than x hours; hrs from min to max" , "Resilience" , ""  , ""  , ""},
{ SSC_OUTPUT, SSC_NUMBER , "avg_critical_load"  , "Hours of autonomy during grid outage critical load met"           , "kWh" , ""                                                         , "Resilience" , ""  , "MIN=0"        , ""},
        var_info_invalid
};

void calculate_resilience_outputs(compute_module *cm, std::unique_ptr<resilience_runner> &resilience)
{
	if (!cm || !resilience)
		return;
	double avg_hours_survived = resilience->compute_metrics();
	auto outage_durations = resilience->get_outage_duration_hrs();
	cm->assign("resilience_hrs", resilience->get_hours_survived());
	cm->assign("resilience_hrs_min", (int) outage_durations[0]);
	cm->assign("resilience_hrs_max", (int) outage_durations.back());
	cm->assign("resilience_hrs_avg", avg_hours_survived);
	cm->assign("outage_durations", outage_durations);
	cm->assign("pdf_of_surviving", resilience->get_probs_of_surviving());
	cm->assign("cdf_of_surviving", resilience->get_cdf_of_surviving());
	cm->assign("survival_function", resilience->get_survival_function());
	cm->assign("avg_critical_load", resilience->get_avg_crit_load_kwh());
}

// financial inputs required for technologies in cmod_hybrid, array is populated in ssc_module_hybridize
var_info vtab_oandm_hybrid[sizeof(vtab_oandm) / sizeof(var_info)] = {var_info_invalid};

var_info vtab_hybrid_tech_inputs[] = {
    /*   VARTYPE           DATATYPE         NAME                           LABEL                            UNITS     META                      GROUP           REQUIRED_IF      CONSTRAINTS     UI_HINTS*/
    { SSC_INPUT,   SSC_NUMBER,     "total_installed_cost",         "Total installed cost",                  "$",       "",                  "HybridCosts",            "*",                 "",             "" },
    { SSC_INPUT,   SSC_ARRAY,      "degradation",                  "Annual AC degradation",                 "%",       "",                  "HybridCosts",            "?=0.0",                   "",             "" },
var_info_invalid };

// for o and m cost outputs calculated in cmod_hybrid
var_info vtab_hybrid_tech_om_outputs[] = {
    /*   VARTYPE           DATATYPE         NAME                           LABEL                UNITS     META                      GROUP           REQUIRED_IF      CONSTRAINTS     UI_HINTS*/
//    { SSC_INPUT,  SSC_NUMBER,     "is_hybrid",              "hybrid configuration",      "0/1", "0=singletech,1=hybrid",    "HybridCosts",       "?=0",      "",             "" },
    { SSC_OUTPUT,   SSC_ARRAY,      "cf_om_production",                     "production O&M costs",      "$",   "",             "HybridCosts",       "",         "",             "" },
    { SSC_OUTPUT,   SSC_ARRAY,      "cf_om_capacity",                       "capacity O&M costs",        "$",   "",             "HybridCosts",       "",         "",             "" },
    { SSC_OUTPUT,   SSC_ARRAY,      "cf_om_fixed",                          "fixed O&M costs",           "$",   "",             "HybridCosts",       "",         "",             "" },
    { SSC_OUTPUT,   SSC_ARRAY,      "cf_om_land_lease",                     "land lease O&M costs",      "$",   "",             "HybridCosts",       "",         "",             "" },
    { SSC_OUTPUT,   SSC_ARRAY,      "cf_om_fuel_cost",                      "fossil fuel O&M costs",     "$",   "",             "HybridCosts",       "",         "",             "" },
    { SSC_OUTPUT,   SSC_ARRAY,      "cf_battery_replacement_cost_schedule", "replacement O&M costs",     "$",   "",             "HybridCosts",       "",         "",             "" },
    { SSC_OUTPUT,   SSC_ARRAY,      "cf_fuelcell_replacement_cost_schedule", "replacement O&M costs",    "$",   "",             "HybridCosts",       "",         "",             "" },
    { SSC_OUTPUT,   SSC_ARRAY,      "cf_energy_net",                        "annual energy",             "kWh", "",             "HybridCosts",       "",         "",             "" },
var_info_invalid };

// for o and m cost outputs calculated in cmod_hybrid and added to operating expenses
var_info vtab_hybrid_fin_om[] = {
    /*   VARTYPE           DATATYPE         NAME                           LABEL                UNITS     META                      GROUP           REQUIRED_IF      CONSTRAINTS     UI_HINTS*/
    { SSC_INPUT,          SSC_NUMBER,     "is_hybrid",              "hybrid configuration",      "0/1", "0=singletech,1=hybrid",    "HybridFin",       "?=0",      "",             "" },
    { SSC_INPUT,          SSC_ARRAY,      "cf_hybrid_om_sum",       "Hybrid O&M costs",          "$",   "",                         "HybridFin",       "",         "",             "" },
    { SSC_INPUT,          SSC_ARRAY,      "monthly_energy",         "Monthly AC energy in Year 1",            "kWh", "",                         "Monthly",         "",         "LENGTH = 12",  "" },

var_info_invalid };




var_info vtab_utility_rate_common[] = {
/*   VARTYPE           DATATYPE         NAME                        LABEL                                          UNITS      META                     GROUP                      REQUIRED_IF         CONSTRAINTS               UI_HINTS	*/

    { SSC_INPUT,        SSC_NUMBER,     "en_electricity_rates",     "Optionally enable/disable electricity_rate",  "years",    "",                    "Electricity Rates",         "",                 "INTEGER,MIN=0,MAX=1",          "SIMULATION_PARAMETER" }, // Required for battery to use retail rates

    // 24.04.04 twn added to access setup()
    { SSC_INPUT, SSC_NUMBER, "inflation_rate", "Inflation rate", "%", "", "Lifetime", "", "MIN=-99", "SIMULATION_PARAMETER" },

    // -----------------------------


    { SSC_INPUT,        SSC_ARRAY,      "rate_escalation",          "Annual electricity rate escalation",          "%/year",   "",                     "Electricity Rates",        "?=0",              "",                             "SIMULATION_PARAMETER" },
                                                                                                                   
    { SSC_INPUT,        SSC_NUMBER,     "ur_metering_option",       "Metering options",                            "0=net energy metering,1=net energy metering with $ credits,2=net billing,3=net billing with carryover to next month,4=buy all - sell all", // continued on next row
                                                                                                                                "Net metering monthly excess","Electricity Rates",  "?=0",              "INTEGER,MIN=0,MAX=4",          "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,     "ur_nm_yearend_sell_rate",  "Net metering true-up credit sell rate", "$/kWh",    "",                     "Electricity Rates",        "?=0.0",            "",                             "" },
    { SSC_INPUT,        SSC_NUMBER,     "ur_nm_credit_month",       "Month of year end payout (true-up)",    "mn",       "",                     "Electricity Rates",        "?=11",             "INTEGER,MIN=0,MAX=11",         "" },
    { SSC_INPUT,        SSC_NUMBER,     "ur_nm_credit_rollover",    "Apply net metering true-up credits to future bills", "0/1", "0=disable,1=enable",           "Electricity Rates",        "?=0",              "INTEGER,MIN=0,MAX=1",          "" },
    { SSC_INPUT,        SSC_NUMBER,     "ur_monthly_fixed_charge",  "Monthly fixed charge",                 "$",        "",                     "Electricity Rates",        "?=0.0",            "",                             "" },
    { SSC_INPUT,        SSC_NUMBER,     "ur_nb_credit_expire",       "Credit is lost upon end of year        ", "0/1",  "0=disable,1=enable",     "Electricity Rates",        "?=0",              "INTEGER,MIN=0,MAX=1",          "" },
    { SSC_INPUT,        SSC_NUMBER,     "ur_nb_apply_credit_current_month", "Apply earned credits to balance before rolling over excess        ", "0/1",  "0=disable,1=enable",     "Electricity Rates",        "?=0",              "INTEGER,MIN=0,MAX=1",          "" },

    // optional input that allows sell rates to be overridden with buy rates - defaults to not override
    { SSC_INPUT,        SSC_NUMBER,     "ur_sell_eq_buy",           "Set sell rate equal to buy rate",              "0/1",     "Optional override",    "Electricity Rates",        "?=0",              "BOOLEAN",                      "SIMULATION_PARAMETER" },
                                                                                                                    
                                                                                                                    
    // urdb minimums                                                                                                
    { SSC_INPUT,        SSC_NUMBER,     "ur_monthly_min_charge",    "Monthly minimum charge",                       "$",       "",                     "Electricity Rates",        "?=0.0",            "",                             "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,     "ur_annual_min_charge",     "Annual minimum charge",                        "$",       "",                     "Electricity Rates",        "?=0.0",            "",                             "SIMULATION_PARAMETER" },
                                                                                                                    
    // time step rates                                                                                              
    { SSC_INPUT,        SSC_NUMBER,     "ur_en_ts_sell_rate",       "Enable time step sell rates",                  "0/1",     "0=disable,1=enable",   "Electricity Rates",        "?=0",              "BOOLEAN",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_ARRAY,      "ur_ts_sell_rate",          "Time step sell rates",                         "$/kWh",   "",                     "Electricity Rates",        "",                 "",                             "SIMULATION_PARAMETER" },
    // add separately to UI                                                                                         
    { SSC_INPUT,        SSC_NUMBER,     "ur_en_ts_buy_rate",        "Enable time step buy rates",                   "0/1",     "0=disable,1=enable",   "Electricity Rates",        "?=0",              "BOOLEAN",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_ARRAY,      "ur_ts_buy_rate",           "Time step buy rates",                          "$/kWh",   "",                     "Electricity Rates",        "",                 "",                             "SIMULATION_PARAMETER" },

    // Energy Charge Inputs
    { SSC_INPUT,        SSC_MATRIX,     "ur_ec_sched_weekday",      "Energy charge weekday schedule",  "Periods defined in ur_ec_tou_mat",   "12x24",  "Electricity Rates",        "",                 "",                             "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_MATRIX,     "ur_ec_sched_weekend",      "Energy charge weekend schedule",  "Periods defined in ur_ec_tou_mat",   "12x24",  "Electricity Rates",        "",                 "",                             "SIMULATION_PARAMETER" },

    // ur_ec_tou_mat has 6 columns period, tier, max usage, max usage units, buy rate, sell rate
    // replaces 12(P)*6(T)*(max usage+buy+sell) = 216 single inputs
    { SSC_INPUT,        SSC_MATRIX,     "ur_ec_tou_mat",            "Energy rates table",              "col 0=period no, col 1=tier no, col 2=max usage, col 3=max usage units (0=kWh, 1=kWh/kW, 2=kWh daily, 3=kWh/kW daily), col 4=buy rate ($/kWh), col 5=sell rate ($/kWh)",
                                                                                                                               "nx6",                  "Electricity Rates",        "",                 "",                             "SIMULATION_PARAMETER" },

    // Demand Charge Inputs
    { SSC_INPUT,        SSC_NUMBER,     "ur_dc_enable",             "Enable demand charge",                         "0/1",      "0=disable,1=enable",   "Electricity Rates",       "?=0",              "BOOLEAN",                      "SIMULATION_PARAMETER" },
    // TOU demand charge
    { SSC_INPUT,        SSC_MATRIX,     "ur_dc_sched_weekday",      "Demand charge weekday schedule",  "Periods defined in ur_dc_tou_mat",   "12x24",   "Electricity Rates",       "",                 "",                             "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_MATRIX,     "ur_dc_sched_weekend",      "Demand charge weekend schedule",  "Periods defined in ur_dc_tou_mat",   "12x24",   "Electricity Rates",       "",                 "",                             "SIMULATION_PARAMETER" },

    // ur_dc_tou_mat has 4 columns period, tier, peak demand (kW), demand charge
    // replaces 12(P)*6(T)*(peak+charge) = 144 single inputs
    { SSC_INPUT,        SSC_MATRIX,     "ur_dc_tou_mat",            "Demand rates (TOU) table",        "col 0=period no, col 1=tier no, col 2=tier peak (kW), col 3=charge ($/kW)",
                                                                                                                               "nx4",                   "Electricity Rates",       "ur_dc_enable=1",   "",                             "SIMULATION_PARAMETER" },

    // flat demand charge
    // ur_dc_tou_flat has 4 columns month, tier, peak demand (kW), demand charge
    // replaces 12(P)*6(T)*(peak+charge) = 144 single inputs
    { SSC_INPUT,        SSC_MATRIX,     "ur_dc_flat_mat",           "Demand rates (flat) table",       "col 0=month, col 1=tier no, col 2=tier peak (kW), col 3=charge ($/kW)",
                                                                                                                               "nx4",                    "Electricity Rates",      "ur_dc_enable=1",   "",                             "SIMULATION_PARAMETER" },

    // Ratcheting demand charges
    { SSC_INPUT,        SSC_NUMBER,     "ur_enable_billing_demand",               "Enable billing demand ratchets",                                               "0/1", "0=disable,1=enable", "Electricity Rates",  "?=0",                        "INTEGER,MIN=0,MAX=1",  "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,     "ur_billing_demand_minimum",              "Minimum billing demand",                                                       "kW",  "",                   "Electricity Rates",  "ur_enable_billing_demand=1", "",                     "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_NUMBER,     "ur_billing_demand_lookback_period",      "Billing demand lookback period",                                               "mn",  "",                   "Electricity Rates",  "ur_enable_billing_demand=1", "INTEGER,MIN=0,MAX=12", "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_MATRIX,     "ur_billing_demand_lookback_percentages", "Billing demand lookback percentages by month and consider actual peak demand", "%",   "12x2",               "Electricity Rates",  "ur_enable_billing_demand=1", "",                     "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_MATRIX,     "ur_dc_billing_demand_periods",           "Billing demand applicability to a given demand charge time of use period",     "",    "",                   "Electricity Rates",  "ur_enable_billing_demand=1", "",                     "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_ARRAY,      "ur_yearzero_usage_peaks",                "Peak usage by month for year zero",                                            "kW",  "12",                 "Electricity Rates",  "ur_enable_billing_demand=1", "",                     "SIMULATION_PARAMETER" },


    var_info_invalid
};

adjustment_factors::adjustment_factors(var_table* vt, const std::string &prefix )
: m_vt(vt), m_prefix(prefix)
{
}

//adjustment factors changed from derates to percentages jmf 1/9/15
bool adjustment_factors::setup(int nsteps, int analysis_period) //nsteps is set to 8760 in this declaration function in common.h
{
    ssc_number_t f = m_vt->as_number(m_prefix + "_constant");
    f = 1.0 - f / 100.0; //convert from percentage to factor
	m_factors.resize( nsteps * analysis_period, f);

    if (m_vt->is_assigned(m_prefix + "_en_hourly")) {
        if (m_vt->as_boolean(m_prefix + "_en_hourly")) {
            size_t n;
            ssc_number_t* p = m_vt->as_array(m_prefix + "_hourly", &n);
            if (p != 0 && n == 8760)
            {
                for (int i = 0; i < 8760; i++)
                    m_factors[i] *= (1.0 - p[i] / 100.0); //convert from percentages to factors
            }
            else {
                m_error = util::format("Adjustment factor hourly loss array length (%d) does not equal length of weather file (%d) ", (int)n, (int)nsteps);
            }
        }
    }
    if (m_vt->as_boolean(m_prefix +  "_en_timeindex"))
    {
        size_t n;
        double steps_per_hour = nsteps / 8760.0;
        int month = 0;
        int day = 0;
        int week = 0;
        ssc_number_t* p = m_vt->as_array(m_prefix + "_timeindex", &n);
        if (p != 0) {
            if (n == 1) {
                for (int a = 0; a < analysis_period; a++) {
                    for (int i = 0; i < nsteps; i++)
                        m_factors[nsteps * a + i] *= (1.0 - p[0]/100.0); //input as factors not percentage
                }
            }
            else if (n == (size_t)(nsteps * analysis_period)) { //Hourly or subhourly- must match weather file resolution
                for (int a = 0; a < analysis_period; a++) {
                    for (int i = 0; i < nsteps; i++)
                        m_factors[nsteps * a + i] *= (1.0 - p[a*nsteps + i]/100.0); //convert from percentages to factors
                }
            }
            else if ((n % 8760 == 0 || n % 2920 == 0) && n != (size_t)(nsteps * analysis_period)) // give a helpful error for timestep mismatch
            {
                m_error = util::format("Adjustment factor number of time series loss time steps (%d) must be the same as the number of simulation time steps (%d) unless losses were defined as daily, weekly, monthly, annual, or single values.",(int)n,(int)nsteps*(int)analysis_period);
            }
            else if (n == (size_t)( 12 * analysis_period)) { //Monthly 
                for (int a = 0; a < analysis_period; a++) {
                    for (int i = 0; i < nsteps; i++) {
                        month = util::month_of(int(i / steps_per_hour))-1;
                        m_factors[nsteps*a + i] *= (1.0 - p[a * 12 + month]/100.0); //input as factors not percentage
                    }

                }
            }
            else if (n == (size_t)( 365 * analysis_period)) { //Daily
                for (int a = 0; a < analysis_period; a++) {
                    for (int i = 0; i < nsteps; i++) {
                        day = util::day_of_year(int(i / steps_per_hour));
                        m_factors[nsteps*a + i] *= (1.0 - p[a * 365 + day]/100.0); //input as factors not percentage
                    }

                }
            }
            else if (n == (size_t)( 52 * analysis_period)) { //Weekly
                for (int a = 0; a < analysis_period; a++) {
                    for (int i = 0; i < nsteps; i++) {
                        week = util::week_of(int(i / steps_per_hour));
                        m_factors[nsteps*a + i] *= (1.0 - p[a * 52 + week]/100.0); //input as factors not percentage
                    }

                }
            }
            else if (n == (size_t)analysis_period) { //Annual
                for (int a = 0; a < analysis_period; a++) {
                    for (int i = 0; i < nsteps; i++)
                        m_factors[nsteps * a + i] *= (1.0 - p[a]/100.0); //input as factors not percentage
                }
            }
            else {
                m_error = util::format("Adjustment factor number of lifetime availability loss values (%d) must be time series, daily, weekly, monthly, annual or single value.", (int)n);
            }
        }
    }
    if (m_vt->as_boolean(m_prefix + "_en_periods"))
    {
		size_t nr, nc;
        ssc_number_t* mat = m_vt->as_matrix(m_prefix + "_periods", &nr, &nc);
        double ts_mult = nsteps / 8760.0;
		if ( mat != 0 && nc == 3 )
		{
			for( size_t r=0;r<nr;r++ )
			{
				int start = (int) round(mat[ nc*r ] * ts_mult);
				int end = (int) round(mat[ nc*r + 1 ] * ts_mult);
				ssc_number_t factor =  mat[ nc*r + 2 ];

				if ( start < 0 || start >= nsteps || end < start )
				{
					m_error = util::format( "Adjustment factor period %d is invalid ( start: %d, end %d )", (int)r, start, end );
					continue;
				}

				if ( end >= nsteps ) end = nsteps-1;

                for (int a = 0; a < analysis_period; a++) {
				    for( int i=start;i<=end;i++ )
					    m_factors[a * nsteps + i] *= (1.0 - factor/100.0); //convert from percentages to factors
                }
			}
		}
	}

	return m_error.length() == 0;
}

ssc_number_t adjustment_factors::operator()( size_t time )
{
	if ( time < m_factors.size() ) return m_factors[time];
	else return 0.0;
}

size_t adjustment_factors::size()
{
    return m_factors.size();
}


shading_factor_calculator::shading_factor_calculator()
{
    m_steps_per_hour = 1;
    m_string_option = -1;
    m_enTimestep = false;
	m_enAzAlt = false;
	m_enMxH = false;
    m_enTimestep = false;
	m_diffFactor = 1.0;
	m_beam_shade_factor = 1.0;
	m_dc_shade_factor = 1.0;
    m_string_option = -1;
    m_steps_per_hour = 1;
}

bool shading_factor_calculator::setup( compute_module *cm, const std::string &prefix )
{
	bool ok = true;
	m_diffFactor = 1.0;
	m_string_option = -1;// 0=shading db, 1=average, 2=max, 3=min, -1 not enabled.
	// Sara 1/25/16 - shading database derate applied to dc only
	// shading loss applied to beam if not from shading database
	m_beam_shade_factor = 1.0;
	m_dc_shade_factor = 1.0;


	m_steps_per_hour = 1;
    size_t nrecs = 8760;
    m_beamFactors.resize_fill(nrecs, 1, 1.0);

    if (cm->is_assigned(prefix + "shading_en_string_option") && cm->as_boolean(prefix + "shading_en_string_option"))
        m_string_option = cm->as_integer(prefix + "shading_string_option");

    m_enTimestep = false;
    if (cm->is_assigned(prefix + "shading_en_timestep") && cm->as_boolean(prefix + "shading_en_timestep"))
    {
        size_t nrows, ncols;
        ssc_number_t* mat = cm->as_matrix(prefix + "shading_timestep", &nrows, &ncols);

        if (nrows % 8760 == 0)
        {
            nrecs = nrows;
            m_beamFactors.resize_fill(nrows, ncols, 1.0);
            if (m_string_option == 0) // use percent shaded to lookup in database
            {
                for (size_t r = 0; r < nrows; r++)
                    for (size_t c = 0; c < ncols; c++)
                        m_beamFactors.at(r, c) = mat[r * ncols + c]; //entered in % shaded
            }
            else if (m_string_option == 1) // use average of all strings in column zero
            {
                for (size_t r = 0; r < nrows; r++)
                {
                    double sum_percent_shaded = 0;
                    for (size_t c = 0; c < ncols; c++)
                    {
                        sum_percent_shaded += mat[r * ncols + c];//entered in % shaded
                    }
                    sum_percent_shaded /= ncols;
                    //cm->log(util::format("hour %d avg percent beam factor %lg",
                    //	r, sum_percent_shaded),
                    //	SSC_WARNING);
                    m_beamFactors.at(r, 0) = 1.0 - sum_percent_shaded / 100;
                }
            }
            else if (m_string_option == 2) // use max of all strings in column zero
            {
                for (size_t r = 0; r < nrows; r++)
                {
                    double max_percent_shaded = 0;
                    for (size_t c = 0; c < ncols; c++)
                    {
                        if (mat[r * ncols + c] > max_percent_shaded)
                            max_percent_shaded = mat[r * ncols + c];//entered in % shaded
                    }
                    //cm->log(util::format("hour %d max percent beam factor %lg",
                    //	r, max_percent_shaded),
                    //	SSC_WARNING);

                    m_beamFactors.at(r, 0) = 1.0 - max_percent_shaded / 100;
                }
            }
            else if (m_string_option == 3) // use min of all strings in column zero
            {
                for (size_t r = 0; r < nrows; r++)
                {
                    double min_percent_shaded = 100;
                    for (size_t c = 0; c < ncols; c++)
                    {
                        if (mat[r * ncols + c] < min_percent_shaded)
                            min_percent_shaded = mat[r * ncols + c];//entered in % shaded
                    }
                    //cm->log(util::format("hour %d min percent beam factor %lg",
                    //	r, min_percent_shaded),
                    //	SSC_WARNING);
                    m_beamFactors.at(r, 0) = 1.0 - min_percent_shaded / 100;
                }
            }
            else // use unshaded factors to apply to beam ( column zero only is used)
            {
                for (size_t r = 0; r < nrows; r++)
                    for (size_t c = 0; c < ncols; c++)
                        m_beamFactors.at(r, c) = 1 - mat[r * ncols + c] / 100; //all other entries must be converted from % to factor unshaded for beam
            }
            m_steps_per_hour = (int)nrows / 8760;
            m_enTimestep = true;
        }
        else
        {
            ok = false;
            m_errors.push_back("hourly shading beam losses must be multiple of 8760 values");
        }
    }

    // initialize other shading inputs
    m_enMxH = false;
    if (cm->is_assigned(prefix + "shading_en_mxh") && cm->as_boolean(prefix + "shading_en_mxh"))
    {
        m_mxhFactors.resize_fill(nrecs, 1, 1.0);
        size_t nrows, ncols;
        ssc_number_t* mat = cm->as_matrix(prefix + "shading_mxh", &nrows, &ncols);
        if (nrows != 12 || ncols != 24)
        {
            ok = false;
            m_errors.push_back("month x hour shading losses must have 12 rows and 24 columns");
        }
        else
        {
            int c = 0;
            for (int m = 0; m < 12; m++)
                for (size_t d = 0; d < util::nday[m]; d++)
                    for (int h = 0; h < 24; h++)
                        for (int jj = 0; jj < m_steps_per_hour; jj++)
                            m_mxhFactors.at(c++, 0) = 1 - mat[m * ncols + h] / 100;

        }
        m_enMxH = true;
    }

    m_enAzAlt = false;
    if (cm->is_assigned(prefix + "shading_en_azal") && cm->as_boolean(prefix + "shading_en_azal"))
    {
        size_t nrows, ncols;
        ssc_number_t* mat = cm->as_matrix(prefix + "shading_azal", &nrows, &ncols);
        if (nrows < 3 || ncols < 3)
        {
            ok = false;
            m_errors.push_back("azimuth x altitude shading losses must have at least 3 rows and 3 columns");
        }

        m_azaltvals.resize_fill(nrows, ncols, 1.0);
        for (size_t r = 0; r < nrows; r++)
        {
            for (size_t c = 0; c < ncols; c++)
            {
                if (r == 0 || c == 0)
                    m_azaltvals.at(r, c) = mat[r * ncols + c]; //first row and column contain azimuth by altitude values
                else
                    m_azaltvals.at(r, c) = 1 - mat[r * ncols + c] / 100; //all other entries must be converted from % to factor
            }
        }
        m_enAzAlt = true;
    }


    if (cm->is_assigned(prefix + "shading_en_diff") && cm->as_boolean(prefix + "shading_en_diff"))
        m_diffFactor = 1 - cm->as_double(prefix + "shading_diff") / 100;

    return ok;
}

std::string shading_factor_calculator::get_error(size_t i)
{
	if( i < m_errors.size() ) return m_errors[i];
	else return std::string("");
}


bool shading_factor_calculator::use_shade_db()
{
	return (m_enTimestep && (m_string_option == 0)); // determine which fbeam function to call
}

size_t shading_factor_calculator::get_row_index_for_input(size_t hour_of_year, size_t minute)
{
	// this function needs to handle different simulation timesteps and timeseries shading input timesteps
	// therefore, start the row index by multiplying the hour of the year by the number of timesteps per hour in the timeseries shading input
	size_t ndx = hour_of_year * (size_t)m_steps_per_hour;

	// then figure out how many row indices to add for the subhourly timeseries entries based on the minute stamp
	// for example, minute 30 in a half-hour weather file will calculate: floor(30 / (60/2)) = 1 and will add one to the row index, which is correct
	ndx += (size_t)floor((int)minute / (60 / (m_steps_per_hour)));

	return ndx;
}


bool shading_factor_calculator::fbeam(size_t hour_of_year, double minute, double solalt, double solazi)
{
    // apply beam Shade Loss Tables
    bool ok = false;
	double factor = 1.0;
	size_t irow = get_row_index_for_input(hour_of_year, (size_t)minute);
	if (irow < m_beamFactors.nrows() && irow >= 0)
	{
		factor = m_beamFactors.at(irow, 0);                                 // beam shading losses by time step
        if (m_enMxH && (irow < m_mxhFactors.nrows())) {
            factor *= m_mxhFactors(irow, 0);                                // month-by-hour beam shading losses
        }
        if (m_enAzAlt) {
            factor *= util::bilinear(solalt, solazi, m_azaltvals);          // azimuth-by-altitude beam shading losses
        }

		m_beam_shade_factor = factor;

		ok = true;
	}
	return ok;
}


bool shading_factor_calculator::fbeam_shade_db(ShadeDB8_mpp * p_shadedb, size_t hour, double minute, double solalt, double solazi, double gpoa, double dpoa, double pv_cell_temp, int mods_per_str, double str_vmp_stc, double mppt_lo, double mppt_hi)
{
    // apply 3D Shade Calculator results
    bool ok = false;
	double dc_factor = 1.0;
	double beam_factor = 1.0;
	size_t irow = get_row_index_for_input(hour, (size_t)minute);
	if (irow < m_beamFactors.nrows())
	{
		std::vector<double> shad_fracs;
        for (size_t icol = 0; icol < m_beamFactors.ncols(); icol++) {
            shad_fracs.push_back(m_beamFactors.at(irow, icol));
        }
		dc_factor = 1.0 - p_shadedb->get_shade_loss(gpoa, dpoa, shad_fracs, true, pv_cell_temp, mods_per_str, str_vmp_stc, mppt_lo, mppt_hi);
        if (m_enMxH && (irow < m_mxhFactors.nrows())) {
            beam_factor *= m_mxhFactors(irow, 0);                           // month-by-hour beam shading losses
        }
        if (m_enAzAlt) {
            beam_factor *= util::bilinear(solalt, solazi, m_azaltvals);     // azimuth-by-altitude beam shading losses
        }

		m_dc_shade_factor = dc_factor;
		m_beam_shade_factor = beam_factor;

		ok = true;
	}
	return ok;
}


double shading_factor_calculator::fdiff()
{
	return m_diffFactor;
}




double shading_factor_calculator::beam_shade_factor()
{
	// Sara 1/25/16 - shading database derate applied to dc only
	// shading loss applied to beam if not from shading database
	return (m_beam_shade_factor);
}

double shading_factor_calculator::dc_shade_factor()
{
	// Sara 1/25/16 - shading database derate applied to dc only
	// shading loss applied to beam if not from shading database
	return (m_dc_shade_factor);
}

//this function checks the weather data to make sure that it is a single, continuous year with an even timestep starting jan 1 and ending dec 31
//HOWEVER, the year value may vary (i.e., TMY) so only checking month, day, hour, minute timesteps, not actual year vector
bool weatherdata::check_continuous_single_year(bool leapyear)
{
    //first, set up some timestep variables
	int ts_per_hour = 0; //determine the number of timesteps in each hour
	if (leapyear)
		ts_per_hour = (int)(m_nRecords / 8784);
	else
		ts_per_hour = (int)(m_nRecords / 8760);
	double ts_min = 60. / ts_per_hour; //determine the number of minutes of each timestep

    //next, check if the data has leap day (feb 29). need to do this because some tools pass in 8760 data that contains feb 29 and not dec 31
    bool has_leapday = false;
    int leapDayNoon = 1429 * ts_per_hour; //look for the index of noon on leap day. noon on leap day is hour 1429 of the year
    if (this->m_data[leapDayNoon]->month == 2 && this->m_data[leapDayNoon]->day == 29) //check noon on what would be feb 29 if it's in the data
        has_leapday = true;

    //last, go through each index in order and make sure that the timestamps all correspond to a single, serially complete year with even timesteps
	int idx = 0; // index to keep track of where we are in the timestamp vectors
	for (int m = 1; m <= 12; m++)
	{
		int daymax = util::days_in_month(m - 1);
		if (m == 2 && has_leapday) daymax = 29; //make sure to account for leap day in Feb if it's in the data
        if (m == 12 && has_leapday && !leapyear) daymax = 30; //if the data is 8760 but has Feb 29, then it won't have Dec 31
		for (int d = 1; d <= daymax; d++)
		{
			for (int h = 0; h < 24; h++)
			{
				double min = this->m_data[idx]->minute;
				for (int tsph = 0; tsph < ts_per_hour; tsph++)
				{
                    //first check that the index isn't out of bounds
                    if (idx > (int)m_nRecords - 1)
                        return false;
                    //if any of the month, day, hour, or minute don't line up with what we've calculated, then it doesn't fit our criteria for a continuous year
					if (this->m_data[idx]->month != m || this->m_data[idx]->day != d || this->m_data[idx]->hour != h
					    || this->m_data[idx]->minute != min)
						return false;
					else
						idx++;
                    min += ts_min;
				}
			}
		}
	}
	return true;
}

weatherdata::weatherdata( var_data *data_table )
{
	m_startSec = m_stepSec = m_nRecords = 0;
	m_index = 0;
	m_ok = true;

	if ( data_table->type != SSC_TABLE )
	{
		m_message = "solar data must be an SSC table variable with fields: "
			"(numbers): lat, lon, tz, elev, "
			"(arrays): year, month, day, hour, minute, gh, dn, df, poa, wspd, wdir, tdry, twet, tdew, rhum, pres, snow, alb, aod";
		return;
	}

	m_hdr.lat = get_number( data_table, "lat" );
    if (std::isnan(m_hdr.lat)) {
        m_message = "missing latitude: could not find lat";
        m_ok = false;
    }
	m_hdr.lon = get_number( data_table, "lon" );
    if (std::isnan(m_hdr.lon)) {
        m_message = "missing longitude: could not find lon";
        m_ok = false;
    }
	m_hdr.tz = get_number( data_table, "tz" );
    if (std::isnan(m_hdr.tz)) {
        m_message = "missing time zone: could not find tz";
        m_ok = false;
    }
    
	m_hdr.elev = get_number( data_table, "elev" );
    //Handle missing elevation in performance model, not weather file handling
    
	// make sure two types of irradiance are provided
	size_t nrec = 0;
	int n_irr = 0;
	if (var_data *value = data_table->table.lookup("df"))
	{
		if (value->type == SSC_ARRAY){
			nrec = value->num.length();
			n_irr++;
		}
	}
	if (var_data *value = data_table->table.lookup("dn"))
	{
		if (value->type == SSC_ARRAY){
			nrec = value->num.length();
			n_irr++;
		}
	}
	if (var_data *value = data_table->table.lookup("gh"))
	{
		if (value->type == SSC_ARRAY){
			nrec = value->num.length();
			n_irr++;
		}
	}
	if (nrec == 0 || n_irr < 2) //poa required if two other types of irradiance are not defined
	{
		if (var_data *value = data_table->table.lookup("poa")) //if poa is supplied, use it to specify nrec
		{
			if (value->type == SSC_ARRAY) {
				nrec = value->num.length();
				n_irr++;
			}
		}
		else //otherwise, not enough irradiance components are specified
		{
			m_message = "missing irradiance: could not find gh, dn, df, or poa";
			m_ok = false;
			return;
		}
	}

	// check that all vectors are of same length as irradiance vectors (which were used to set nrec)
	vec year = get_vector( data_table, "year", &nrec);
	vec month = get_vector( data_table, "month", &nrec);
	vec day = get_vector( data_table, "day", &nrec);
	vec hour = get_vector( data_table, "hour", &nrec);
	vec minute = get_vector( data_table, "minute", &nrec);
	vec gh = get_vector( data_table, "gh", &nrec );
	vec dn = get_vector( data_table, "dn", &nrec );
	vec df = get_vector( data_table, "df", &nrec );
	vec poa = get_vector(data_table, "poa", &nrec);
	vec wspd = get_vector( data_table, "wspd", &nrec );
	vec wdir = get_vector( data_table, "wdir", &nrec );
	vec tdry = get_vector( data_table, "tdry", &nrec );
	vec twet = get_vector( data_table, "twet", &nrec );
	vec tdew = get_vector( data_table, "tdew", &nrec );
	vec rhum = get_vector( data_table, "rhum", &nrec );
	vec pres = get_vector( data_table, "pres", &nrec );
	vec snow = get_vector( data_table, "snow", &nrec );
	vec alb = get_vector( data_table, "alb", &nrec );
	vec aod = get_vector( data_table, "aod", &nrec );
	if (m_ok == false){
		return; //m_message is set in get_vector function, so doesn't need to be set here
	}

	// new: minute column required for weather data
	if (!has_data_column(weather_data_provider::MINUTE))
	{
		m_message = "minute column required for weather data input";
		m_ok = false;
		return;
	}

	m_nRecords = nrec;

	if ( nrec > 0)
	{
		m_data.resize( nrec );
		for( size_t i=0;i<nrec;i++ )
		{
			weather_record *r = new weather_record;

			if ( i < year.len ) r->year = (int)year.p[i];
			if ( i < month.len ) r->month = (int)month.p[i];
			if ( i < day.len ) r->day = (int)day.p[i];
			if ( i < hour.len ) r->hour = (int)hour.p[i];
			if ( i < minute.len ) r->minute = minute.p[i];

			// minute column must go from 0-59, NOT 1-60!
			if (minute.p[i] > 60)
			{
				m_message = "minute column must contain integers from 0-59";
				m_ok = false;
				return;
			}

			r->gh = r->dn = r->df = r->poa = r->wspd = r->wdir = r->tdry = r->twet = r->tdew
				= r->rhum = r->pres = r->snow = r->alb = r->aod = std::numeric_limits<double>::quiet_NaN();
			if ( i < gh.len ) r->gh = gh.p[i];
			if ( i < dn.len ) r->dn = dn.p[i];
			if ( i < df.len ) r->df = df.p[i];
			if (i < poa.len ) r->poa = poa.p[i];

			if ( i < wspd.len ) r->wspd = wspd.p[i];
			if ( i < wdir.len ) r->wdir = wdir.p[i];

			if ( i < tdry.len ) r->tdry = tdry.p[i];
			if ( i < twet.len ) r->twet = twet.p[i];
			else{
				// calculate twet using calc_twet if tdry & rh & pres are available
				if ((i < tdry.len) && (i < rhum.len) && (i < pres.len)){
					r->twet = (float)calc_twet(tdry.p[i], rhum.p[i], pres.p[i]);
				}
			}
			if ( i < tdew.len ) r->tdew = tdew.p[i];
			else{
				// calculate tdew using wiki_dew_calc if tdry & rh are available
				if ((i < tdry.len) && (i < rhum.len)){
					r->tdew = (float)wiki_dew_calc(tdry.p[i], rhum.p[i]);
				}
			}

			if ( i < rhum.len ) r->rhum = rhum.p[i];
			if ( i < pres.len ) r->pres = pres.p[i];

			if ( i < snow.len ) r->snow = snow.p[i];
			if ( i < alb.len ) r->alb = alb.p[i];
			if ( i < aod.len ) r->aod = aod.p[i];

			m_data[i] = r;
		}

        start_hours_at_0();
    }

	// estimate time step and check for continuous year
	size_t nmult = 0;
	bool is_leap_year = false;
	// Check if the weather file contains a leap day
	// if so, correct the number of nrecords
	if (m_nRecords % 8784 == 0)
	{
		m_nRecords = m_nRecords / 8784 * 8760;
		is_leap_year = true;
	}
	if (m_nRecords % 8760 == 0)
	{
		if (check_continuous_single_year(is_leap_year))
		{
			nmult = nrec / 8760;
			m_stepSec = 3600 / nmult;
			m_startSec = m_stepSec / 2;
		}
		else
			m_continuousYear = false;
	}
	else
	{
		m_continuousYear = false;
	}
}

weatherdata::~weatherdata()
{
    if (m_data.size() > 0) {
        for (size_t i = 0; i< m_data.size(); i++)
            delete m_data[i];
    }
    m_data.clear();
}


int weatherdata::name_to_id( const char *name )
{
	std::string n( util::lower_case( name ) );

	if ( n == "year" ) return YEAR;
	if ( n == "month" ) return MONTH;
	if ( n == "day" ) return DAY;
	if ( n == "hour" ) return HOUR;
	if ( n == "minute" ) return MINUTE;
	if ( n == "gh" ) return GHI;
	if ( n == "dn" ) return DNI;
	if ( n == "df" ) return DHI;
	if ( n == "poa" ) return POA;
	if ( n == "wspd" ) return WSPD;
	if ( n == "wdir" ) return WDIR;
	if ( n == "tdry" ) return TDRY;
	if ( n == "twet" ) return TWET;
	if ( n == "tdew" ) return TDEW;
	if ( n == "rhum" ) return RH;
	if ( n == "pres" ) return PRES;
	if ( n == "snow" ) return SNOW;
	if ( n == "alb" ) return ALB;
	if ( n == "aod" ) return AOD;

	return -1;
}

weatherdata::vec weatherdata::get_vector( var_data *v, const char *name, size_t *len )
{
	vec x;
	x.p = 0;
	x.len = 0;
	if ( var_data *value = v->table.lookup( name ) )
	{
		if ( value->type == SSC_ARRAY )
		{
			x.len = value->num.length();
			x.p = value->num.data();
			if (len && *len != x.len) {
				std::string name_s(name);
				m_message = name_s + " number of entries doesn't match with other fields";
				m_ok = false;
			}
			size_t id = name_to_id(name);
			if ( !has_data_column( id ) )
			  {
			    m_columns.push_back( id );
			  }
		}
	}

	return x;
}

ssc_number_t weatherdata::get_number( var_data *v, const char *name )
{
	if ( var_data *value = v->table.lookup( name ) )
	{
		if ( value->type == SSC_NUMBER )
			return value->num;
	}

	return std::numeric_limits<ssc_number_t>::quiet_NaN();
}

void weatherdata::start_hours_at_0() {
    std::vector<int> hours;
    for (weather_record *i : m_data)
        hours.push_back(i->hour);
    double max_hr = *std::max_element(hours.begin(), hours.end());
    double min_hr = *std::min_element(hours.begin(), hours.end());
    if (max_hr - min_hr != 23)
        m_message = "Weather data range was not (0-23) or (1-24)";
    else if (max_hr == 24)
        for (weather_record *i : m_data) i->hour -= 1;
}

void weatherdata::set_counter_to(size_t cur_index){
	if (cur_index < m_data.size()) {
		m_index = cur_index;
	}
}

bool weatherdata::read( weather_record *r )
{
	if (m_index < m_data.size())
	{
		*r = *m_data[m_index++];
		return true;
	}
	else
		return false;
}

bool weatherdata::read_average(weather_record *r, std::vector<int> &, size_t &)
{
	// finish per bool weatherfile::read_average(weather_record *r, std::vector<int> &cols, size_t &num_timesteps)
	if (m_index < m_data.size())
	{
		*r = *m_data[m_index++];
		return true;
	}
	else
		return false;

}



bool weatherdata::has_data_column( size_t id )
{
	return std::find( m_columns.begin(), m_columns.end(), id ) != m_columns.end();
}

bool ssc_cmod_update(std::string &log_msg, std::string &progress_msg, void *data, double progress, int log_type)
{
	compute_module *cm = static_cast<compute_module*> (data);
	if (!cm)
		return false;

	if (log_msg != "")
		cm->log(log_msg, log_type);

	return cm->update(progress_msg, (float)progress);
}

scalefactors::scalefactors(var_table* v)
{
    vt = v;
}

// TODO: add a boolean for factoring in inflation when adding new financial models
// Assumes "name" is an input array of percentages with length that may be zero (invalid), 1 (value), >1 (schedule that may or may not be the same length of analysis period)
std::vector<double> scalefactors::get_factors(const char* name)
{
    size_t nyears = 1;
    if (vt->is_assigned("analysis_period"))
    {
        nyears = vt->as_integer("analysis_period");
    }
    std::vector<double> scale_factors(nyears,1.0);
    if (vt->is_assigned(name)) {
        size_t count, i;
        ssc_number_t* parr = vt->as_array(name, &count);
        if (count < 1) {
            for (i = 0; i < nyears; i++)
                scale_factors[i] = (ssc_number_t)1.0;
        }
        else if (count < 2) {
            for (i = 0; i < nyears; i++)
                scale_factors[i] = (ssc_number_t)pow((double)(1 + parr[0] * 0.01), (double)i);
        }
        else {
            if (count < nyears)
            {
                std::ostringstream ss;
                ss << "Expected length of " << name << " to be " << nyears << " found " << count << " entries";
                throw general_error(ss.str());
            }
            for (i = 0; i < nyears; i++)
                scale_factors[i] = (ssc_number_t)(1 + parr[i] * 0.01);
        }
    }
    return scale_factors;
}

void prepend_to_output(compute_module* cm, std::string var_name, size_t count, ssc_number_t value) {
    size_t orig_count = 0;
    if (cm->is_assigned(var_name)) {
        ssc_number_t* arr = cm->as_array(var_name, &orig_count);
        arr = cm->resize_array(var_name, count);
        if (count > orig_count) {
            size_t diff = count - orig_count;
            for (int i = (int)orig_count - 1; i >= 0; i--) {
                arr[i + diff] = arr[i];
            }
            for (int i = 0; i < (int)diff; i++) {
                arr[i] = value;
            }
        }
    }
}
