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

#include "common_financial.h"
#include "lib_financial.h"
using namespace libfin;
#include <sstream>

#ifndef WIN32
#include <float.h>
#endif

static var_info _cm_vtab_communitysolar[] = {

/*   VARTYPE           DATATYPE         NAME                                      LABEL                                                            UNITS              META                      GROUP                       REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

// 3 additional variables for PPA Buy rate
// optional output from battery model
	{ SSC_INPUT,        SSC_NUMBER,      "en_batt",                                    "Enable battery storage model",                            "0/1",     "",                     "BatterySystem",       "?=0",                                 "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "en_electricity_rates",                       "Enable electricity rates for grid purchase",              "0/1",     "",                     "Electricity Rates",       "?=0",                                 "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_meter_position",                        "Position of battery relative to electric meter",          "",        "",                     "BatterySystem",       "",                           "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "revenue_gen",                                "Electricity to grid",                                     "kW",      "",                       "System Output",       "",                           "",                              "" },
//    { SSC_OUTPUT,       SSC_ARRAY,       "gen_purchases",                              "Electricity from grid",                                    "kW",      "",                       "System Output",       "",                           "",                              "" },


	{ SSC_INPUT,        SSC_ARRAY,       "gen",                                         "Net power to or from the grid",                            "kW",       "",                    "System Output", "*", "", "" },
    { SSC_INPUT,        SSC_ARRAY,      "gen_without_battery",                          "Electricity to or from the renewable system, without the battery", "kW", "",                     "System Output", "", "", "" },

	{ SSC_INPUT,        SSC_ARRAY, "degradation", "Annual energy degradation", "", "", "System Output", "*", "", "" },
	{ SSC_INPUT,        SSC_NUMBER,     "system_capacity",			              "System nameplate capacity",		                               "kW",                "",                        "System Output",             "*",					   "MIN=1e-3",                      "" },
    
	/* PPA Buy Rate values (PVWatts has no nighttime losses, so electricity purchase is always zero. Note no PPA community solar. Will need to address if we make CS available with pvsamv1, storage, etc.*/
	{ SSC_INPUT, SSC_ARRAY, "utility_bill_w_sys", "Electricity bill with system", "$", "", "Utility Bill", "", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "cf_utility_bill", "Electricity purchase", "$", "", "", "", "LENGTH_EQUAL=cf_length", "" },


	/*loan moratorium from Sara for India Documentation\India\Loan Moratorum
	assumptions:
	1) moratorium period begins at beginning of loan term 
	2) moratorium affects principal payment and not interest
	3) loan term remains the same
	4) payments increase after moratorium period
	*/
	{ SSC_INPUT, SSC_NUMBER, "loan_moratorium", "Loan moratorium period", "years", "", "Financial Parameters", "?=0", "INTEGER,MIN=0", "" },

/* Recapitalization */                                                            														           
	{ SSC_INOUT,        SSC_NUMBER,     "system_use_recapitalization",	          "Recapitalization expenses",	                                   "0/1",               "0=None,1=Recapitalize",   "System Costs",          "?=0",					   "INTEGER,MIN=0",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,     "system_recapitalization_cost",	          "Recapitalization cost",	                                       "$",                 "",                        "System Costs",          "?=0",					   "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "system_recapitalization_escalation",     "Recapitalization escalation (above inflation)",	               "%",	                "",					       "System Costs",          "?=0",                     "MIN=0,MAX=100",      		    "" },
	{ SSC_INPUT,        SSC_ARRAY,      "system_lifetime_recapitalize",		      "Recapitalization boolean",	                                   "",                  "",                        "System Costs",          "?=0",					   "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_recapitalization",	                  "Recapitalization operating expense",	                           "$",                 "",                        "Recapitalization",          "*",					   "LENGTH_EQUAL=cf_length",        "" },
                                                                                  															       
/* Dispatch */                                                                    															       
	{ SSC_INPUT,        SSC_NUMBER,     "system_use_lifetime_output",		      "Lifetime hourly system outputs",	                               "0/1",                         "0=hourly first year,1=hourly lifetime",                      "Lifetime",             "*",						   "INTEGER,MIN=0",                 "" },


    // community solar

    // subscriber share
    { SSC_INPUT,        SSC_ARRAY,     "subscriber1_share",	          "Subscriber 1 share",	                                                    "%",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber1_growth",	          "Subscriber 1 growth",	                                                "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,     "subscriber2_share",	          "Subscriber 2 share",	                                                    "%",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber2_growth",	          "Subscriber 2 growth",	                                                "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,     "subscriber3_share",	          "Subscriber 3 share",	                                                    "%",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber3_growth",	          "Subscriber 3 growth",	                                                "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,     "subscriber4_share",	          "Subscriber 4 share",	                                                    "%",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber4_growth",	          "Subscriber 4 growth",	                                                "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },

    // bill credit rate
    { SSC_INPUT,        SSC_ARRAY,      "subscriber1_bill_credit_rate",	      "Subscriber 1 bill credit rate",	                                            "$/kWh",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber1_bill_credit_rate_escal",  "Subscriber 1 bill credit rate escalation",	                                                "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,      "subscriber2_bill_credit_rate",	      "Subscriber 2 bill credit rate",	                                            "$/kWh",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber2_bill_credit_rate_escal",  "Subscriber 2 bill credit rate escalation",	                                                "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,      "subscriber3_bill_credit_rate",	      "Subscriber 3 bill credit rate",	                                            "$/kWh",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber3_bill_credit_rate_escal",  "Subscriber 3 bill credit rate escalation",	                                                "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,      "subscriber4_bill_credit_rate",	      "Subscriber 4 bill credit rate",	                                            "$/kWh",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber4_bill_credit_rate_escal",  "Subscriber 4 bill credit rate escalation",	                                                "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },

    // subscription revenue
    { SSC_INPUT,        SSC_NUMBER,     "subscriber1_payment_upfront",	          "Subscriber 1 payment up-front",                                    "$",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber2_payment_upfront",	          "Subscriber 2 payment up-front",                                    "$",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber3_payment_upfront",	          "Subscriber 3 payment up-front",                                    "$",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber4_payment_upfront",	          "Subscriber 4 payment up-front",                                    "$",                 "",                        "Community Solar",          "?=0",					   "",                              "" },

    { SSC_INPUT,        SSC_ARRAY,     "subscriber1_payment_generation",	      "Subscriber 1 payment generation",                                 "$/kWh",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,    "subscriber1_payment_generation_escal",    "Subscriber 1 payment generation escalation",	                     "%/yr",                  "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,     "subscriber2_payment_generation",	      "Subscriber 2 payment generation",                                 "$/kWh",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,    "subscriber2_payment_generation_escal",    "Subscriber 1 payment generation escalation",	                     "%/yr",                  "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,     "subscriber3_payment_generation",	      "Subscriber 3 payment generation",                                 "$/kWh",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,    "subscriber3_payment_generation_escal",    "Subscriber 1 payment generation escalation",	                     "%/yr",                  "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,     "subscriber4_payment_generation",	      "Subscriber 4 payment generation",                                 "$/kWh",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,    "subscriber4_payment_generation_escal",    "Subscriber 1 payment generation escalation",	                     "%/yr",                  "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,     "unsubscribed_payment_generation",	      "Unsubscribed generation rate",                                    "$/kWh",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,    "unsubscribed_payment_generation_escal",    "Unsubscribed generation escalation",	                          "%/yr",                  "",                        "Community Solar",          "?=0",					   "",                              "" },

    { SSC_INPUT,        SSC_ARRAY,      "subscriber1_payment_annual",	      "Subscriber 1 payment annual",	                                        "$/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber1_payment_annual_escal",   "Subscriber 1 payment annual escalation",	                                "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,      "subscriber2_payment_annual",	      "Subscriber 2 payment annual",	                                        "$/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber2_payment_annual_escal",   "Subscriber 2 payment annual escalation",	                                "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,      "subscriber3_payment_annual",	      "Subscriber 3 payment annual",	                                        "$/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber3_payment_annual_escal",   "Subscriber 3 payment annual escalation",	                                "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,      "subscriber4_payment_annual",	      "Subscriber 4 payment annual",	                                        "$/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "subscriber4_payment_annual_escal",   "Subscriber 4 payment annual escalation",	                                "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },

    // costs - upfront and recurring
    { SSC_INPUT,        SSC_NUMBER,     "cs_cost_upfront",	                   "Up-front fixed cost",                                       "$",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "cs_cost_upfront_per_capacity",	       "Up-front cost by capacity",                                 "$/kW",              "",                        "Community Solar",          "?=0",					   "",                              "" },

    { SSC_INPUT,        SSC_ARRAY,      "cs_cost_recurring_fixed",	            "Recurring annual fixed cost",	                                            "$/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "cs_cost_recurring_fixed_escal",        "Recurring annual fixed cost escalation",	                                "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,      "cs_cost_recurring_capacity",	        "Recurring annual cost by capacity",	                                    "$/kW-yr",              "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "cs_cost_recurring_capacity_escal",     "Recurring annual cost by capacity escalation",	                            "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_ARRAY,      "cs_cost_recurring_generation",	        "Recurring annual cost by generation",	                                    "$/kWh",                "",                        "Community Solar",          "?=0",					   "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,     "cs_cost_recurring_generation_escal",   "Recurring annual cost by generation escalation",                           "%/yr",                 "",                        "Community Solar",          "?=0",					   "",                              "" },


/* PPA revenue not applicable to community solar, may need to be restored later
	// dispatch update TODO - remove SO output label below after consildated with CSP
	{ SSC_INPUT, SSC_NUMBER, "ppa_multiplier_model", "PPA multiplier model", "0/1", "0=diurnal,1=timestep", "Revenue", "?=0", "INTEGER,MIN=0", "" },
	{ SSC_INPUT, SSC_ARRAY, "dispatch_factors_ts", "Dispatch payment factor array", "", "", "Revenue", "ppa_multiplier_model=1", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "ppa_multipliers", "TOD factors", "", "", "Revenue", "*", "", "" },
	   
	{ SSC_INPUT,        SSC_NUMBER,     "dispatch_factor1",		                  "TOD factor for period 1",	                                   "",   "",                          "Revenue",             "ppa_multiplier_model=0",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,     "dispatch_factor2",		                  "TOD factor for period 2",	                                   "",   "",                          "Revenue",             "ppa_multiplier_model=0",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,     "dispatch_factor3",		                  "TOD factor for period 3",	                                   "",   "",                          "Revenue",             "ppa_multiplier_model=0",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,     "dispatch_factor4",		                  "TOD factor for period 4",	                                   "",   "",                          "Revenue",             "ppa_multiplier_model=0",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,     "dispatch_factor5",		                  "TOD factor for period 5",	                                   "",   "",                          "Revenue",             "ppa_multiplier_model=0",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,     "dispatch_factor6",		                  "TOD factor for period 6",	                                   "",   "",                          "Revenue",             "ppa_multiplier_model=0",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,     "dispatch_factor7",		                  "TOD factor for period 7",	                                   "",   "",                          "Revenue",             "ppa_multiplier_model=0",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,     "dispatch_factor8",		                  "TOD factor for period 8",	                                   "",   "",                          "Revenue",             "ppa_multiplier_model=0",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,     "dispatch_factor9",		                  "TOD factor for period 9",	                                   "",   "",                          "Revenue",             "ppa_multiplier_model=0",						   "",                 "" },
	{ SSC_INPUT,        SSC_MATRIX,     "dispatch_sched_weekday",                 "Diurnal weekday TOD periods",                                   "1..9", "12 x 24 matrix",    "Revenue", "ppa_multiplier_model=0", "", "" },
	{ SSC_INPUT,        SSC_MATRIX,     "dispatch_sched_weekend",                 "Diurnal weekend TOD periods",                                   "1..9", "12 x 24 matrix",    "Revenue", "ppa_multiplier_model=0", "", "" },
                                                                                  																   
                                                                                  																   
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_energy_sales_jan",                     "Energy produced by year in January",                      "kWh", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_revenue_jan",                        "PPA revenue by year for January",                            "$", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_energy_sales_feb",                     "Energy produced by year in February",                     "kWh", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_revenue_feb",                        "PPA revenue by year for February",                           "$", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_energy_sales_mar",                     "Energy produced by year in March",                        "kWh", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_revenue_mar",                        "PPA revenue by year for March",                              "$", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_energy_sales_apr",                     "Energy produced by year in April",                        "kWh", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_revenue_apr",                        "PPA revenue by year for April",                              "$", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_energy_sales_may",                     "Energy produced by year in May",                          "kWh", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_revenue_may",                        "PPA revenue by year for May",                                "$", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_energy_sales_jun",                     "Energy produced by year in June",                         "kWh", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_revenue_jun",                        "PPA revenue by year for June",                               "$", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_energy_sales_jul",                     "Energy produced by year in July",                         "kWh", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_revenue_jul",                        "PPA revenue by year for July",                               "$", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_energy_sales_aug",                     "Energy produced by year in August",                       "kWh", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_revenue_aug",                        "PPA revenue by year for August",                             "$", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_energy_sales_sep",                     "Energy produced by year in September",                    "kWh", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_revenue_sep",                        "PPA revenue by year for September",                          "$", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_energy_sales_oct",                     "Energy produced by year in October",                      "kWh", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_revenue_oct",                        "PPA revenue by year for October",                            "$", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_energy_sales_nov",                     "Energy produced by year in November",                     "kWh", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_revenue_nov",                        "PPA revenue by year for November",                           "$", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_energy_sales_dec",                     "Energy produced by year in December",                     "kWh", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cf_revenue_dec",                        "PPA revenue by year for December",                           "$", "", "Cash Flow Revenue by Month and TOD Period", "*", "LENGTH_EQUAL=cf_length", "" },
                                                                                  
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_dispatch1",	              "Energy produced by year in TOD period 1",	               "kWh",   "",  "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_dispatch1",		                  "PPA revenue by year for TOD period 1",	                   "$",   "",      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_dispatch2",	                  "Energy produced by year in TOD period 2",	               "kWh",   "",  "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_dispatch2",		                  "PPA revenue by year for TOD period 2",	                   "$",   "",      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_dispatch3",	                  "Energy produced by year in TOD period 3",	               "kWh",   "",  "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_dispatch3",		                  "PPA revenue by year for TOD period 3",	                   "$",   "",      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_dispatch4",	                  "Energy produced by year in TOD period 4",	               "kWh",   "",  "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_dispatch4",		                  "PPA revenue by year for TOD period 4",	                   "$",   "",      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_dispatch5",	                  "Energy produced by year in TOD period 5",	               "kWh",   "",  "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_dispatch5",		                  "PPA revenue by year for TOD period 5",	                   "$",   "",      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_dispatch6",	                  "Energy produced by year in TOD period 6",	               "kWh",   "",  "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_dispatch6",		                  "PPA revenue by year for TOD period 6",	                   "$",   "",      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_dispatch7",	                  "Energy produced by year in TOD period 7",	               "kWh",   "",  "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_dispatch7",		                  "PPA revenue by year for TOD period 7",	                   "$",   "",      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_dispatch8",	                  "Energy produced by year in TOD period 8",	               "kWh",   "",  "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_dispatch8",		                  "PPA revenue by year for TOD period 8",	                   "$",   "",      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_dispatch9",	                  "Energy produced by year in TOD period 9",	               "kWh",   "",  "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_dispatch9",		                  "PPA revenue by year for TOD period 9",	                   "$",   "",      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "LENGTH_EQUAL=cf_length",                 "" },
                                                                                  
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_revenue_dispatch1",            "PPA revenue in Year 1 TOD period 1",            "$",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT, SSC_NUMBER, "firstyear_revenue_dispatch2", "PPA revenue from in Year 1 TOD period 2", "$", "", "Cash Flow Revenue by Month and TOD Period", "ppa_multiplier_model=0", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "firstyear_revenue_dispatch3", "PPA revenue from in Year 1 TOD period 3", "$", "", "Cash Flow Revenue by Month and TOD Period", "ppa_multiplier_model=0", "", "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_revenue_dispatch4",            "PPA revenue in Year 1 TOD period 4",            "$",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_revenue_dispatch5",            "PPA revenue in Year 1 TOD period 5",            "$",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_revenue_dispatch6",            "PPA revenue in Year 1 TOD period 6",            "$",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_revenue_dispatch7",            "PPA revenue in Year 1 TOD period 7",            "$",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_revenue_dispatch8",            "PPA revenue in Year 1 TOD period 8",            "$",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_revenue_dispatch9",            "PPA revenue in Year 1 TOD period 9",            "$",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
                                                                                                                                                   
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_dispatch1",             "Energy produced in Year 1 TOD period 1",             "kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_dispatch2",             "Energy produced in Year 1 TOD period 2",             "kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_dispatch3",             "Energy produced in Year 1 TOD period 3",             "kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_dispatch4",             "Energy produced in Year 1 TOD period 4",             "kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_dispatch5",             "Energy produced in Year 1 TOD period 5",             "kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_dispatch6",             "Energy produced in Year 1 TOD period 6",             "kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_dispatch7",             "Energy produced in Year 1 TOD period 7",             "kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_dispatch8",             "Energy produced in Year 1 TOD period 8",             "kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_dispatch9",             "Energy produced in Year 1 TOD period 9",             "kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
                                                                                  
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_price1",                "Power price in Year 1 TOD period 1",                      "cents/kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_price2",                "Power price in Year 1 TOD period 2",                      "cents/kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_price3",                "Power price in Year 1 TOD period 3",                      "cents/kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_price4",                "Power price in Year 1 TOD period 4",                      "cents/kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_price5",                "Power price in Year 1 TOD period 5",                      "cents/kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_price6",                "Power price in Year 1 TOD period 6",                      "cents/kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_price7",                "Power price in Year 1 TOD period 7",                      "cents/kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_price8",                "Power price in Year 1 TOD period 8",                      "cents/kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,    "firstyear_energy_price9",                "Power price in Year 1 TOD period 9",                      "cents/kWh",             "",                      "Cash Flow Revenue by Month and TOD Period",      "ppa_multiplier_model=0",                       "",                                  "" },
                                                                                  
// first year monthly output for each TOD period                                  
//	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_monthly_firstyear",		      "PPA revenue in Year 1 by month",	"",   "",      "Cash Flow Revenue by Month and TOD Period",             "*",				   "",                 "" },
//	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_monthly_firstyear",	      "Energy produced in Year 1 by month",	"",   "",          "Cash Flow Revenue by Month and TOD Period",             "*",				   "",                 "" },
                                                                                 
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_monthly_firstyear_TOD1",      "PPA revenue in Year 1 by month for TOD period 1",  "$",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_monthly_firstyear_TOD1",   "Energy produced in Year 1 by month for TOD period 1",   "kWh",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_monthly_firstyear_TOD2",      "PPA revenue in Year 1 by month for TOD period 2",  "$",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_monthly_firstyear_TOD2",   "Energy produced in Year 1 by month for TOD period 2",   "kWh",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_monthly_firstyear_TOD3",      "PPA revenue in Year 1 by month for TOD period 3",  "$",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_monthly_firstyear_TOD3",   "Energy produced in Year 1 by month for TOD period 3",   "kWh",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_monthly_firstyear_TOD4",      "PPA revenue in Year 1 by month for TOD period 4",  "$",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_monthly_firstyear_TOD4",   "Energy produced in Year 1 by month for TOD period 4",   "kWh",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_monthly_firstyear_TOD5",      "PPA revenue in Year 1 by month for TOD period 5",  "$",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_monthly_firstyear_TOD5",   "Energy produced in Year 1 by month for TOD period 5",   "kWh",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_monthly_firstyear_TOD6",      "PPA revenue in Year 1 by month for TOD period 6",  "$",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_monthly_firstyear_TOD6",   "Energy produced in Year 1 by month for TOD period 6",   "kWh",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_monthly_firstyear_TOD7",      "PPA revenue in Year 1 by month for TOD period 7",  "$",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_monthly_firstyear_TOD7",   "Energy produced in Year 1 by month for TOD period 7",   "kWh",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_monthly_firstyear_TOD8",      "PPA revenue in Year 1 by month for TOD period 8",  "$",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_monthly_firstyear_TOD8",   "Energy produced in Year 1 by month for TOD period 8",   "kWh",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_revenue_monthly_firstyear_TOD9",      "PPA revenue in Year 1 by month for TOD period 9",  "$",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_sales_monthly_firstyear_TOD9",   "Energy produced in Year 1 by month for TOD period 9",   "kWh",   "",                      "Cash Flow Revenue by Month and TOD Period",             "ppa_multiplier_model=0",				   "",                 "" },
  */                                                                                
/* inputs in model not currently in M 11/15/10 */                             
	{ SSC_INPUT,         SSC_NUMBER,    "total_installed_cost",                   "Installed cost",                                                "$",     "",					  "System Costs",			 "*",                         "",                             "" },

/* salvage value */	                                                          
	{ SSC_INPUT,        SSC_NUMBER,     "salvage_percentage",                     "Net pre-tax cash salvage value",	                               "%",	 "",					  "Financial Parameters",             "?=10",                     "MIN=0,MAX=100",      			"" },
    //{ SSC_INPUT,        SSC_NUMBER,     "batt_salvage_percentage",                     "Net pre-tax cash battery salvage value",	                               "%",	 "",					  "Financial Parameters",             "?=0",                     "MIN=0,MAX=100",      			"" },

/* market specific inputs - leveraged partnership flip */                     

/* construction period */                                                     
	{ SSC_INPUT,       SSC_NUMBER,      "construction_financing_cost",	          "Construction financing total",	                                "$",	 "",					  "Financial Parameters",			 "*",                         "",                             "" },

/* intermediate outputs */
	{ SSC_OUTPUT,       SSC_NUMBER,     "cost_debt_upfront",                      "Debt up-front fee",          "$",   "",					  "Intermediate Costs",			 "?=0",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "cost_financing",                         "Total financing cost",          "$",   "",					  "Intermediate Costs",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "cost_prefinancing",                      "Total installed cost",          "$",   "",					  "Intermediate Costs",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "cost_installed",                         "Net capital cost",                   "$",     "",					  "Intermediate Costs",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "cost_installedperwatt",                  "Net capital cost per watt",          "$/W",   "",					  "Intermediate Costs",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "nominal_discount_rate",                  "Nominal discount rate",            "%",     "",					  "Intermediate Costs",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "prop_tax_assessed_value",                "Assessed value of property for tax purposes","$", "",				  "Intermediate Costs",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "salvage_value",			              "Net pre-tax cash salvage value",	"$",	 "",					  "Intermediate Costs",			 "*",                         "",                             "" },
	                                                                              
	{ SSC_OUTPUT,       SSC_NUMBER,     "depr_alloc_none_percent",		          "Non-depreciable federal and state allocation",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "depr_alloc_none",		                  "Non-depreciable federal and state allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "depr_alloc_total",		                  "Total depreciation federal and state allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "pre_depr_alloc_basis",		              "Depreciable basis prior to allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "pre_itc_qual_basis",		              "ITC basis prior to qualification",	"$", "",	  "Tax Credits",             "*",					  "",     			        "" },

// state itc table                                                                
/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_macrs_5",		      "5-yr MACRS state percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_macrs_5",		              "5-yr MACRS depreciation federal and state allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_ibi_reduc_macrs_5",		  "5-yr MACRS state IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_cbi_reduc_macrs_5",		  "5-yr MACRS state CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_prior_itc_macrs_5",		  "5-yr MACRS state depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_sta_qual_macrs_5",		              "5-yr MACRS depreciation state ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_qual_macrs_5",		  "5-yr MACRS state percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_amount_macrs_5",	  "5-yr MACRS depreciation ITC basis from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_percent_macrs_5",		  "5-yr MACRS depreciation ITC basis disallowance from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_fixed_amount_macrs_5",		  "5-yr MACRS depreciation ITC basis from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_fixed_macrs_5",		  "5-yr MACRS depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_sta_reduction_macrs_5",  "5-yr MACRS state basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_fed_reduction_macrs_5",  "5-yr MACRS state basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_after_itc_macrs_5",		  "5-yr MACRS state depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_first_year_bonus_macrs_5",	  "5-yr MACRS state first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_macrs_5",		              "5-yr MACRS state depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
                                                                                  
/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_macrs_15",		      "15-yr MACRS state percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_macrs_15",		              "15-yr MACRS depreciation federal and state allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_ibi_reduc_macrs_15",		  "15-yr MACRS state IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_cbi_reduc_macrs_15",		  "15-yr MACRS state CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_prior_itc_macrs_15",		  "15-yr MACRS state depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_sta_qual_macrs_15",		          "15-yr MACRS depreciation state ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_qual_macrs_15",	  "15-yr MACRS state percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_amount_macrs_15",	  "15-yr MACRS depreciation ITC basis from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_percent_macrs_15",	  "15-yr MACRS depreciation ITC basis disallowance from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_fixed_amount_macrs_15",	  "15-yr MACRS depreciation ITC basis from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_fixed_macrs_15",		  "15-yr MACRS depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_sta_reduction_macrs_15", "15-yr MACRS state basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_fed_reduction_macrs_15", "15-yr MACRS state basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_after_itc_macrs_15",		  "15-yr MACRS state depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_first_year_bonus_macrs_15",  "15-yr MACRS state first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_macrs_15",		              "15-yr MACRS state depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_sl_5",		          "5-yr straight line state percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_sl_5",		                  "5-yr straight line depreciation federal and state allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_ibi_reduc_sl_5",		      "5-yr straight line state IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_cbi_reduc_sl_5",		      "5-yr straight line state CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_prior_itc_sl_5",		      "5-yr straight line state depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_sta_qual_sl_5",		              "5-yr straight line depreciation state ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_qual_sl_5",		  "5-yr straight line state percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_amount_sl_5",		  "5-yr straight line depreciation ITC basis from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_percent_sl_5",		  "5-yr straight line depreciation ITC basis disallowance from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_fixed_amount_sl_5",		  "5-yr straight line depreciation ITC basis from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_fixed_sl_5",		      "5-yr straight line depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_sta_reduction_sl_5",	  "5-yr straight line state basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_fed_reduction_sl_5",	  "5-yr straight line state basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_after_itc_sl_5",		      "5-yr straight line state depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_first_year_bonus_sl_5",	  "5-yr straight line state first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_sl_5",		                  "5-yr straight line state depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_sl_15",		      "15-yr straight line state percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_sl_15",		                  "15-yr straight line depreciation federal and state allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_ibi_reduc_sl_15",		      "15-yr straight line state IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_cbi_reduc_sl_15",		      "15-yr straight line state CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_prior_itc_sl_15",		      "15-yr straight line state depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_sta_qual_sl_15",		              "15-yr straight line depreciation state ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_qual_sl_15",		  "15-yr straight line state percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_amount_sl_15",		  "15-yr straight line depreciation ITC basis from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_percent_sl_15",		  "15-yr straight line depreciation ITC basis disallowance from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_fixed_amount_sl_15",		  "15-yr straight line depreciation ITC basis from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_fixed_sl_15",		      "15-yr straight line depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_sta_reduction_sl_15",	  "15-yr straight line state basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_fed_reduction_sl_15",	  "15-yr straight line state basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_after_itc_sl_15",		      "15-yr straight line state depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_first_year_bonus_sl_15",	  "15-yr straight line state first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_sl_15",		              "15-yr straight line state depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_sl_20",		      "20-yr straight line state percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_sl_20",		                  "20-yr straight line depreciation federal and state allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_ibi_reduc_sl_20",		      "20-yr straight line state IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_cbi_reduc_sl_20",		      "20-yr straight line state CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_prior_itc_sl_20",		      "20-yr straight line state depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_sta_qual_sl_20",		              "20-yr straight line depreciation state ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_qual_sl_20",		  "20-yr straight line state percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_amount_sl_20",		  "20-yr straight line depreciation ITC basis from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_percent_sl_20",		  "20-yr straight line depreciation ITC basis disallowance from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_fixed_amount_sl_20",		  "20-yr straight line depreciation ITC basis from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_fixed_sl_20",		      "20-yr straight line depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_sta_reduction_sl_20",	  "20-yr straight line state basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_fed_reduction_sl_20",	  "20-yr straight line state basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_after_itc_sl_20",		      "20-yr straight line state depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_first_year_bonus_sl_20",	  "20-yr straight line state first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_sl_20",		              "20-yr straight line state depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_sl_39",		      "39-yr straight line state percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_sl_39",		                  "39-yr straight line depreciation federal and state allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_ibi_reduc_sl_39",		      "39-yr straight line state IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_cbi_reduc_sl_39",		      "39-yr straight line state CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_prior_itc_sl_39",		      "39-yr straight line state depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_sta_qual_sl_39",		              "39-yr straight line depreciation state ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_qual_sl_39",		  "39-yr straight line state percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_amount_sl_39",		  "39-yr straight line depreciation ITC basis from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_percent_sl_39",		  "39-yr straight line depreciation ITC basis disallowance from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_fixed_amount_sl_39",		  "39-yr straight line depreciation ITC basis from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_fixed_sl_39",		      "39-yr straight line depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_sta_reduction_sl_39",	  "39-yr straight line state basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_fed_reduction_sl_39",	  "39-yr straight line state basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_after_itc_sl_39",		      "39-yr straight line state depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_first_year_bonus_sl_39",	  "39-yr straight line state first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_sl_39",		              "39-yr straight line state depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_custom",		      "Custom straight line state percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_custom",		              "Custom straight line depreciation federal and state allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_ibi_reduc_custom",		      "Custom straight line state IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_cbi_reduc_custom",		      "Custom straight line state CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_prior_itc_custom",		      "Custom straight line state depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_sta_qual_custom",		              "Custom straight line depreciation state ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_qual_custom",		  "Custom straight line state percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_amount_custom",	  "Custom straight line depreciation ITC basis from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_percent_custom",		  "Custom straight line depreciation ITC basis disallowance from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_fixed_amount_custom",		  "Custom straight line depreciation ITC basis from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_fixed_custom",		  "Custom straight line depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_sta_reduction_custom",	  "Custom straight line state basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_fed_reduction_custom",	  "Custom straight line state basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_after_itc_custom",		      "Custom straight line state depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_first_year_bonus_custom",	  "Custom straight line state first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_custom",		              "Custom straight line state depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_total",		      "Total state percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_total",		                  "Total depreciation federal and state allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_ibi_reduc_total",		      "Total state IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_cbi_reduc_total",		      "Total state CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_prior_itc_total",		      "Total state depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_sta_qual_total",		              "Total state ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_qual_total",		  "Total state percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_percent_amount_total",		  "Total depreciation ITC basis from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_percent_total",		  "Total depreciation ITC basis disallowance from state percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_fixed_amount_total",		  "Total depreciation ITC basis from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_sta_fixed_total",		      "Total depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_sta_reduction_total",	  "Total state basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_itc_fed_reduction_total",	  "Total state basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_after_itc_total",		      "Total state depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_first_year_bonus_total",	  "Total state first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_stabas_total",		              "Total state depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_sta_percent_total",		          "State ITC percent total",	"$", "",	  "Tax Credits",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_sta_fixed_total",		              "State ITC fixed total",	"$", "",	  "Tax Credits",             "*",					  "",     			        "" },

// federal itc table
/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_macrs_5",		      "5-yr MACRS federal percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_macrs_5",		              "5-yr MACRS depreciation federal and federal allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_ibi_reduc_macrs_5",		  "5-yr MACRS federal IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_cbi_reduc_macrs_5",		  "5-yr MACRS federal CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_prior_itc_macrs_5",		  "5-yr MACRS federal depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_fed_qual_macrs_5",		              "5-yr MACRS depreciation federal ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_qual_macrs_5",		  "5-yr MACRS federal percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_amount_macrs_5",	  "5-yr MACRS depreciation ITC basis from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_percent_macrs_5",		  "5-yr MACRS depreciation ITC basis disallowance from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_fixed_amount_macrs_5",		  "5-yr MACRS depreciation ITC basis from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_fixed_macrs_5",		  "5-yr MACRS depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_macrs_5",  "5-yr MACRS federal basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_macrs_5",  "5-yr MACRS federal basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_after_itc_macrs_5",		  "5-yr MACRS federal depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_first_year_bonus_macrs_5",	  "5-yr MACRS federal first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_macrs_5",		              "5-yr MACRS federal depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_macrs_15",		      "15-yr MACRS federal percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_macrs_15",		              "15-yr MACRS depreciation federal and federal allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_ibi_reduc_macrs_15",		  "15-yr MACRS federal IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_cbi_reduc_macrs_15",		  "15-yr MACRS federal CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_prior_itc_macrs_15",		  "15-yr MACRS federal depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_fed_qual_macrs_15",		          "15-yr MACRS depreciation federal ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_qual_macrs_15",	  "15-yr MACRS federal percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_amount_macrs_15",	  "15-yr MACRS depreciation ITC basis from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_percent_macrs_15",	  "15-yr MACRS depreciation ITC basis disallowance from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_fixed_amount_macrs_15",	  "15-yr MACRS depreciation ITC basis from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_fixed_macrs_15",		  "15-yr MACRS depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_macrs_15", "15-yr MACRS federal basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_macrs_15", "15-yr MACRS federal basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_after_itc_macrs_15",		  "15-yr MACRS federal depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_first_year_bonus_macrs_15",  "15-yr MACRS federal first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_macrs_15",		              "15-yr MACRS federal depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_sl_5",		          "5-yr straight line federal percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_sl_5",		                  "5-yr straight line depreciation federal and federal allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_ibi_reduc_sl_5",		      "5-yr straight line federal IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_cbi_reduc_sl_5",		      "5-yr straight line federal CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_prior_itc_sl_5",		      "5-yr straight line federal depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_fed_qual_sl_5",		              "5-yr straight line depreciation federal ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_qual_sl_5",		  "5-yr straight line federal percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_amount_sl_5",		  "5-yr straight line depreciation ITC basis from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_percent_sl_5",		  "5-yr straight line depreciation ITC basis disallowance from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_fixed_amount_sl_5",		  "5-yr straight line depreciation ITC basis from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_fixed_sl_5",		      "5-yr straight line depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_sl_5",	  "5-yr straight line federal basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_sl_5",	  "5-yr straight line federal basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_after_itc_sl_5",		      "5-yr straight line federal depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_first_year_bonus_sl_5",	  "5-yr straight line federal first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_sl_5",		                  "5-yr straight line federal depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_sl_15",		      "15-yr straight line federal percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_sl_15",		                  "15-yr straight line depreciation federal and federal allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_ibi_reduc_sl_15",		      "15-yr straight line federal IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_cbi_reduc_sl_15",		      "15-yr straight line federal CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_prior_itc_sl_15",		      "15-yr straight line federal depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_fed_qual_sl_15",		              "15-yr straight line depreciation federal ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_qual_sl_15",		  "15-yr straight line federal percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_amount_sl_15",		  "15-yr straight line depreciation ITC basis from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_percent_sl_15",		  "15-yr straight line depreciation ITC basis disallowance from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_fixed_amount_sl_15",		  "15-yr straight line depreciation ITC basis from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_fixed_sl_15",		      "15-yr straight line depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_sl_15",	  "15-yr straight line federal basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_sl_15",    "15-yr straight line federal basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_after_itc_sl_15",            "15-yr straight line federal depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_first_year_bonus_sl_15",     "15-yr straight line federal first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_sl_15",                      "15-yr straight line federal depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_sl_20",              "20-yr straight line federal percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_sl_20",                       "20-yr straight line depreciation federal and federal allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_ibi_reduc_sl_20",		      "20-yr straight line federal IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_cbi_reduc_sl_20",		      "20-yr straight line federal CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_prior_itc_sl_20",		      "20-yr straight line federal depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_fed_qual_sl_20",		              "20-yr straight line depreciation federal ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_qual_sl_20",		  "20-yr straight line federal percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_amount_sl_20",		  "20-yr straight line depreciation ITC basis from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_percent_sl_20",		  "20-yr straight line depreciation ITC basis disallowance from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_fixed_amount_sl_20",		  "20-yr straight line depreciation ITC basis from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_fixed_sl_20",		      "20-yr straight line depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_sl_20",	  "20-yr straight line federal basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_sl_20",    "20-yr straight line federal basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_after_itc_sl_20",            "20-yr straight line federal depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_first_year_bonus_sl_20",     "20-yr straight line federal first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_sl_20",                      "20-yr straight line federal depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_sl_39",              "39-yr straight line federal percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_sl_39",                       "39-yr straight line depreciation federal and federal allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_ibi_reduc_sl_39",            "39-yr straight line federal IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_cbi_reduc_sl_39",            "39-yr straight line federal CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_prior_itc_sl_39",            "39-yr straight line federal depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_fed_qual_sl_39",                     "39-yr straight line depreciation federal ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_qual_sl_39",		  "39-yr straight line federal percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_amount_sl_39",		  "39-yr straight line depreciation ITC basis from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_percent_sl_39",		  "39-yr straight line depreciation ITC basis disallowance from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_fixed_amount_sl_39",		  "39-yr straight line depreciation ITC basis from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_fixed_sl_39",		      "39-yr straight line depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_sl_39",	  "39-yr straight line federal basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_sl_39",	  "39-yr straight line federal basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_after_itc_sl_39",		      "39-yr straight line federal depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_first_year_bonus_sl_39",     "39-yr straight line federal first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_sl_39",                      "39-yr straight line federal depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_custom",  		      "Custom straight line federal percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_custom",		              "Custom straight line depreciation federal and federal allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_ibi_reduc_custom",		      "Custom straight line federal IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_cbi_reduc_custom",		      "Custom straight line federal CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_prior_itc_custom",		      "Custom straight line federal depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_fed_qual_custom",		              "Custom straight line depreciation federal ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_qual_custom",		  "Custom straight line federal percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_amount_custom",	  "Custom straight line depreciation ITC basis from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_percent_custom",		  "Custom straight line depreciation ITC basis disallowance from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_fixed_amount_custom",		  "Custom straight line depreciation ITC basis from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_fixed_custom",		  "Custom straight line depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_custom",	  "Custom straight line federal basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_custom",	  "Custom straight line federal basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_after_itc_custom",		      "Custom straight line federal depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_first_year_bonus_custom",	  "Custom straight line federal first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_custom",		              "Custom straight line federal depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/*1*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_total",		      "Total federal percent of total depreciable basis",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_alloc_total",		                  "Total depreciation federal and federal allocation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*2*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_ibi_reduc_total",		      "Total federal IBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*3*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_cbi_reduc_total",		      "Total federal CBI reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*4*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_prior_itc_total",		      "Total federal depreciation basis prior ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_fed_qual_total",		              "Total federal ITC adj qualifying costs",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*5*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_qual_total",		  "Total federal percent of qualifying costs",	"%", "",	  "Depreciation",             "*",					  "",     			        "" },
/*6*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_percent_amount_total",		  "Total depreciation ITC basis from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_percent_total",		  "Total depreciation ITC basis disallowance from federal percentage",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*7*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_fixed_amount_total",		  "Total depreciation ITC basis from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_disallow_fed_fixed_total",		      "Total depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*8*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_total",	  "Total federal basis state ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*9*/ { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_total",	  "Total federal basis federal ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*10*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_after_itc_total",		      "Total federal depreciation basis after ITC reduction",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
/*11*/{ SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_first_year_bonus_total",	  "Total federal first year bonus depreciation",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "depr_fedbas_total",		              "Total federal depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

      { SSC_OUTPUT,     SSC_NUMBER,     "itc_fed_percent_total",		          "Federal ITC percent total",	"$", "",	  "Tax Credits",             "*",					  "",     			        "" },
	  { SSC_OUTPUT,     SSC_NUMBER,     "itc_fed_fixed_total",		              "Federal ITC fixed total",	"$", "",	  "Tax Credits",             "*",					  "",     			        "" },

/* depreciation bases method - added with version 4.1    0=5-yrMacrs, 1=proportional */
	{ SSC_INPUT,        SSC_NUMBER,      "depr_stabas_method",                    "Method of state depreciation reduction",     "",      "0=5yr MACRS,1=Proportional",                      "Depreciation",      "?=0",                       "INTEGER,MIN=0,MAX=1",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "depr_fedbas_method",                    "Method of federal depreciation reduction",     "",      "0=5yr MACRS,1=Proportional",                      "Depreciation",      "?=0",                       "INTEGER,MIN=0,MAX=1",                                         "" },

/* State depreciation table */
	{ SSC_OUTPUT,       SSC_NUMBER,     "depr_stabas_total",		              "Total state depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/* Federal depreciation table */
	{ SSC_OUTPUT,       SSC_NUMBER,     "depr_fedbas_macrs_5",		              "5-yr MACRS federal depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "depr_fedbas_macrs_15",		              "15-yr MACRS federal depreciation basis",	"$", "",  "Depreciation",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "depr_fedbas_sl_5",		                  "5-yr straight line federal depreciation basis",	"$", "",  "Depreciation",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "depr_fedbas_sl_15",		              "15-yr straight line federal depreciation basis","$", "",  "Depreciation",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "depr_fedbas_sl_20",		              "20-yr straight line federal depreciation basis","$", "",  "Depreciation",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "depr_fedbas_sl_39",		              "39-yr straight line federal depreciation basis","$", "",  "Depreciation",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "depr_fedbas_custom",		              "Custom federal depreciation basis","$", "",  "Depreciation",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "depr_fedbas_total",		              "Total federal depreciation basis",	"$", "",	  "Depreciation",             "*",					  "",     			        "" },

/* State taxes */
	/* intermediate outputs for validation */
	{ SSC_OUTPUT,       SSC_NUMBER,     "cash_for_debt_service",                  "Cash available for debt service (CAFDS)",   "$",     "",					  "Debt Sizing",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "pv_cafds",                               "Present value of CAFDS","$", "",				  "Debt Sizing",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "size_of_debt",			                  "Size of debt",	"$",	 "",					  "Debt Sizing",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "size_of_equity",			              "Equity",	"$",	 "",					  "Debt Sizing",			 "*",                         "",                             "" },

/* model outputs */
	{ SSC_OUTPUT,       SSC_NUMBER,     "cf_length",                              "Number of periods in cashflow",      "",             "",                      "Metrics",      "*",                       "INTEGER",                                  "" },
    // No PPA for community solar, placeholder for implementing it later
    //  { SSC_OUTPUT,       SSC_NUMBER,     "ppa_price",			                  "PPA price in first year",			"cents/kWh",	"",				   "Metrics",			  "*",                         "",      					   "" },
/* Production - input as energy_net above */

/* Partial Income Statement: Project */	
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_energy_net",                          "Electricity to grid net",                 "kWh",      "",                      "Cash Flow Electricity",             "*",                      "LENGTH_EQUAL=cf_length",                             "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "cf_energy_sales",                        "Electricity to grid",                    "kWh",      "",                      "Cash Flow Electricity",             "*",                      "LENGTH_EQUAL=cf_length",                             "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "cf_energy_purchases",                    "Electricity from grid",                  "kWh",      "",                      "Cash Flow Electricity",             "*",                      "LENGTH_EQUAL=cf_length",                             "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "cf_energy_without_battery",              "Electricity produced without the battery or curtailment", "kWh",      "",       "Cash Flow Electricity",             "",                       "LENGTH_EQUAL=cf_length",                             "" },
    /* PPA revenue not applicable to community solar, may need to be restored later
    { SSC_OUTPUT,       SSC_ARRAY,      "cf_ppa_price",                           "PPA price",                     "cents/kWh",      "",                      "Cash Flow Revenues",             "*",                      "LENGTH_EQUAL=cf_length",                             "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "cf_energy_value",                        "PPA revenue",                     "$",      "",                      "Cash Flow Revenues",             "*",                      "LENGTH_EQUAL=cf_length",                             "" },
    */
    // for fuel cell only, restore if community solar available for fuel cell configs
    //{ SSC_OUTPUT,       SSC_ARRAY,      "cf_thermal_value",                       "Thermal revenue",                     "$",      "",                      "Cash Flow Revenues",             "*",                      "LENGTH_EQUAL=cf_length",                             "" },
    
    { SSC_OUTPUT,       SSC_ARRAY,      "cf_om_fixed_expense",       "O&M fixed expense",                  "$",            "",                      "Cash Flow Expenses",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_om_production_expense",  "O&M production-based expense",       "$",            "",                      "Cash Flow Expenses",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_om_capacity_expense",    "O&M capacity-based expense",         "$",            "",                      "Cash Flow Expenses",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
    { SSC_OUTPUT,        SSC_ARRAY,     "cf_om_fixed1_expense",      "O&M battery fixed expense",                  "$",            "",              "Cash Flow Expenses",      "",                     "LENGTH_EQUAL=cf_length",                "" },
    { SSC_OUTPUT,        SSC_ARRAY,     "cf_om_production1_expense", "O&M battery production-based expense",       "$",            "",              "Cash Flow Expenses",      "",                     "LENGTH_EQUAL=cf_length",                "" },
    { SSC_OUTPUT,        SSC_ARRAY,     "cf_om_capacity1_expense",   "O&M battery capacity-based expense",         "$",            "",              "Cash Flow Expenses",      "",                     "LENGTH_EQUAL=cf_length",                "" },
    { SSC_OUTPUT,        SSC_ARRAY,     "cf_om_fixed2_expense",      "O&M fuel cell fixed expense",                  "$",            "",            "Cash Flow Expenses",      "",                     "LENGTH_EQUAL=cf_length",                "" },
    { SSC_OUTPUT,        SSC_ARRAY,     "cf_om_production2_expense", "O&M fuel cell production-based expense",       "$",            "",            "Cash Flow Expenses",      "",                     "LENGTH_EQUAL=cf_length",                "" },
    { SSC_OUTPUT,        SSC_ARRAY,     "cf_om_capacity2_expense",   "O&M fuel cell capacity-based expense",         "$",            "",            "Cash Flow Expenses",      "",                     "LENGTH_EQUAL=cf_length",                "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "cf_om_fuel_expense",        "Fuel expense",                   "$",            "",                          "Cash Flow Expenses",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_om_opt_fuel_1_expense",  "Feedstock biomass expense",                   "$",            "",             "Cash Flow Expenses",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_om_opt_fuel_2_expense",  "Feedstock coal expense",                   "$",            "",                "Cash Flow Expenses",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_property_tax_assessed_value",         "Property tax net assessed value", "$",            "",                      "Cash Flow Expenses",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_property_tax_expense",                "Property tax expense",               "$",            "",                      "Cash Flow Expenses",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_insurance_expense",                   "Insurance expense",                  "$",            "",                      "Cash Flow Expenses",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_operating_expenses",                  "Total operating expenses",            "$",            "",                      "Cash Flow Expenses",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_net_salvage_value",                   "Salvage value",            "$",            "",                      "Cash Flow Revenues",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_total_revenue",                       "Total revenue",            "$",            "",                      "Cash Flow Revenues",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_ebitda",                              "EBITDA",       "$",            "",                      "Cash Flow Expenses",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_reserve_debtservice",                 "Reserves debt service balance",       "$",            "",                      "Cash Flow Reserves",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT, SSC_ARRAY, "cf_reserve_om", "Reserves working capital balance ", "$", "", "Cash Flow Reserves", "*", "LENGTH_EQUAL=cf_length", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "cf_reserve_receivables", "Reserves receivables balance", "$", "", "Cash Flow Reserves", "*", "LENGTH_EQUAL=cf_length", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "cf_reserve_equip1", "Reserves major equipment 1 balance", "$", "", "Cash Flow Reserves", "*", "LENGTH_EQUAL=cf_length", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_reserve_equip2",                      "Reserves major equipment 2 balance",       "$",            "",                      "Cash Flow Reserves",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_reserve_equip3",                      "Reserves major equipment 3 balance",       "$",            "",                      "Cash Flow Reserves",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_reserve_total",                       "Reserves total reserves balance",       "$",            "",                      "Cash Flow Reserves",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_reserve_interest",                    "Interest earned on reserves",       "$",            "",                      "Cash Flow Reserves",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_funding_debtservice",                 "Reserves debt service funding",       "$",            "",                      "Cash Flow Reserves",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT, SSC_ARRAY, "cf_funding_om", "Reserves working capital funding", "$", "", "Cash Flow Reserves", "*", "LENGTH_EQUAL=cf_length", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "cf_funding_receivables", "Reserves receivables funding", "$", "", "Cash Flow Reserves", "*", "LENGTH_EQUAL=cf_length", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "cf_funding_equip1", "Reserves major equipment 1 funding", "$", "", "Cash Flow Reserves", "*", "LENGTH_EQUAL=cf_length", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_funding_equip2",                      "Reserves major equipment 2 funding",       "$",            "",                      "Cash Flow Reserves",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_funding_equip3",                      "Reserves major equipment 3 funding",       "$",            "",                      "Cash Flow Reserves",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_disbursement_debtservice",            "Reserves debt service disbursement ",       "$",            "",                      "Cash Flow Reserves",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT, SSC_ARRAY, "cf_disbursement_om", "Reserves working capital disbursement", "$", "", "Cash Flow Reserves", "*", "LENGTH_EQUAL=cf_length", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "cf_disbursement_receivables", "Reserves receivables disbursement", "$", "", "Cash Flow Reserves", "*", "LENGTH_EQUAL=cf_length", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "cf_disbursement_equip1", "Reserves major equipment 1 disbursement", "$", "", "Cash Flow Reserves", "*", "LENGTH_EQUAL=cf_length", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_disbursement_equip2",                 "Reserves major equipment 2 disbursement",       "$",            "",                      "Cash Flow Reserves",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_disbursement_equip3",                 "Reserves major equipment 3 disbursement",       "$",            "",                      "Cash Flow Reserves",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
		
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_cash_for_ds",                         "Cash available for debt service (CAFDS)",                       "$",            "",                      "Cash Flow Debt Sizing",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_pv_interest_factor",                  "Present value interest factor for CAFDS",             "",            "",                      "Cash Flow Debt Repayment",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_pv_cash_for_ds",                      "Present value of CAFDS",                       "$",            "",                      "Cash Flow Debt Sizing",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_debt_size",                           "Size of debt",                       "$",            "",                      "Cash Flow Debt Sizing",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_debt_balance",                        "Debt balance",                       "$",            "",                      "Cash Flow Debt Repayment",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_debt_payment_interest",               "Debt interest payment",                   "$",            "",                      "Cash Flow Debt Repayment",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_debt_payment_principal",              "Debt principal payment",                  "$",            "",                      "Cash Flow Debt Repayment",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_debt_payment_total",                  "Debt total payment",             "$",            "",                      "Cash Flow Debt Repayment",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	// Project cash flow

	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_operating_activities",        "Cash flow from operating activities",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,       SSC_NUMBER,     "purchase_of_property",	                  "Purchase of property",	"$",	 "",					  "Cash Flow Total and Returns",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_dsra",                        "Reserve (increase)/decrease debt service ",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT, SSC_ARRAY, "cf_project_wcra", "Reserve (increase)/decrease working capital", "$", "", "Cash Flow Total and Returns", "*", "LENGTH_EQUAL=cf_length", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "cf_project_receivablesra", "Reserve (increase)/decrease receivables", "$", "", "Cash Flow Total and Returns", "*", "LENGTH_EQUAL=cf_length", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "cf_project_me1ra", "Reserve (increase)/decrease major equipment 1", "$", "", "Cash Flow Total and Returns", "*", "LENGTH_EQUAL=cf_length", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_me2ra",                       "Reserve (increase)/decrease major equipment 2",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_me3ra",                       "Reserve (increase)/decrease major equipment 3",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_ra",                          "Reserve (increase)/decrease total reserve account",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_me1cs",                       "Reserve capital spending major equipment 1",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_me2cs",                       "Reserve capital spending major equipment 2",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_me3cs",                       "Reserve capital spending major equipment 3",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_mecs",                        "Reserve capital spending major equipment total",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_investing_activities",        "Cash flow from investing activities",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,       SSC_NUMBER,     "issuance_of_equity",	                  "Issuance of equity",	"$",	 "",					  "Cash Flow Total and Returns",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_financing_activities",        "Cash flow from financing activities",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_pretax_cashflow",                     "Total pre-tax cash flow",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

// Project returns
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_return_pretax",               "Total pre-tax returns",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_return_pretax_irr",           "Pre-tax cumulative IRR",  "%", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_return_pretax_npv",           "Pre-tax cumulative NPV",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_return_aftertax_cash",        "Total after-tax cash returns",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_return_aftertax",             "Total after-tax returns",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "cf_project_return_aftertax_irr",         "After-tax cumulative IRR",  "%", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "cf_project_return_aftertax_max_irr",     "After-tax project maximum IRR",  "%", "",                      "Cash Flow After Tax",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
    { SSC_OUTPUT,       SSC_ARRAY,      "cf_project_return_aftertax_npv",         "After-tax cumulative NPV",  "$", "",                      "Cash Flow Total and Returns",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	// metrics table
    { SSC_OUTPUT,       SSC_NUMBER,     "project_return_aftertax_irr",            "IRR Internal rate of return",       "%",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "project_return_aftertax_npv",            "NPV Net present value",             "$",                   "", "Metrics", "*", "", "" },

    { SSC_OUTPUT, SSC_ARRAY, "cf_annual_costs", "Annual costs", "$", "", "LCOE calculations", "*", "LENGTH_EQUAL=cf_length", "" },

    { SSC_OUTPUT, SSC_NUMBER, "npv_annual_costs", "Present value of annual costs", "$", "", "LCOE calculations", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "adjusted_installed_cost", "Initial cost less cash incentives", "$", "", "", "*", "", "" },

	{ SSC_OUTPUT, SSC_NUMBER, "min_dscr", "Minimum DSCR", "", "", "DSCR", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "cf_pretax_dscr", "DSCR (pre-tax)", "", "", "DSCR", "*", "LENGTH_EQUAL=cf_length", "" },

	{ SSC_INPUT,        SSC_ARRAY,       "system_pre_curtailment_kwac",     "System power before grid curtailment",  "kW",       "System generation" "",                 "System Output",                        "",                              "" },

    { SSC_OUTPUT, SSC_ARRAY, "cf_energy_curtailed", "Electricity curtailed", "kWh", "", "Cash Flow Electricity", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_curtailment_value", "Curtailment payment revenue", "$", "", "Cash Flow Revenues", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_capacity_payment", "Capacity payment revenue", "$", "", "Cash Flow Revenues", "*", "LENGTH_EQUAL=cf_length", "" },

	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_curtailment_revenue",                        "Present value of curtailment payment revenue",              "$",                   "", "Metrics", "*", "", "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_capacity_revenue",                        "Present value of capacity payment revenue",              "$",                   "", "Metrics", "*", "", "" },
		// only count toward revenue if user selected
	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_fed_pbi_income",                        "Present value of federal PBI income",              "$",                   "", "Metrics", "*", "", "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_sta_pbi_income",                        "Present value of state PBI income",              "$",                   "", "Metrics", "*", "", "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_uti_pbi_income",                        "Present value of utility PBI income",              "$",                   "", "Metrics", "*", "", "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_oth_pbi_income",                        "Present value of other PBI income",              "$",                   "", "Metrics", "*", "", "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_salvage_value",                        "Present value of salvage value",              "$",                   "", "Metrics", "*", "", "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_thermal_value",                        "Present value of thermal value",              "$",                   "", "Metrics", "*", "", "" },

        // community solar specific outputs
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber1_share_fraction", "Subscriber 1 Share of system capacity", "", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber2_share_fraction", "Subscriber 2 Share of system capacity", "", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber3_share_fraction", "Subscriber 3 Share of system capacity", "", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber4_share_fraction", "Subscriber 4 Share of system capacity", "", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_unsubscribed_share_fraction", "Unsubscribed share of system capacity", "", "", "", "*", "LENGTH_EQUAL=cf_length", "" },

    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber1_bill_credit_rate", "Subscriber 1 Bill credit rate", "$/kWh", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber2_bill_credit_rate", "Subscriber 2 Bill credit rate", "$/kWh", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber3_bill_credit_rate", "Subscriber 3 Bill credit rate", "$/kWh", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber4_bill_credit_rate", "Subscriber 4 Bill credit rate", "$/kWh", "", "", "*", "LENGTH_EQUAL=cf_length", "" },

    /* these are already accounted for by cf_community_solar_ outputs below, so no need to duplicate TO DO ok to delete?
    { SSC_OUTPUT, SSC_ARRAY, "cf_recurring_fixed", "Recurring fixed cost", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_recurring_capacity", "Recurring cost by capacity", "$/kW-yr", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_recurring_generation", "Recurring cost by generation", "$/kWh", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    */

    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber1_generation_payment", "Subscriber 1 Generation rate", "$/kWh", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber2_generation_payment", "Subscriber 2 Generation rate", "$/kWh", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber3_generation_payment", "Subscriber 3 Generation rate", "$/kWh", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber4_generation_payment", "Subscriber 4 Generation rate", "$/kWh", "", "", "*", "LENGTH_EQUAL=cf_length", "" },

    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber1_share_of_generation", "Subscriber 1 Share of generation", "kWh", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber2_share_of_generation", "Subscriber 2 Share of generation", "kWh", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber3_share_of_generation", "Subscriber 3 Share of generation", "kWh", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber4_share_of_generation", "Subscriber 4 Share of generation", "kWh", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_unsubscribed_share_of_generation", "Unsubscribed share of generation", "kWh", "", "", "*", "LENGTH_EQUAL=cf_length", "" },

    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber1_revenue_generation", "Revenue from Subscriber 1 generation payments", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber2_revenue_generation", "Revenue from Subscriber 2 generation payments", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber3_revenue_generation", "Revenue from Subscriber 3 generation payments", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber4_revenue_generation", "Revenue from Subscriber 4 generation", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },

    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber1_revenue_upfront", "Revenue from Subscriber 1 up-front payments", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber2_revenue_upfront", "Revenue from Subscriber 2 up-front payments", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber3_revenue_upfront", "Revenue from Subscriber 3 up-front payments", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber4_revenue_upfront", "Revenue from Subscriber 4 up-front payments", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },

    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber1_revenue_annual_payment", "Revenue from Subscriber 1 annual payments", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber2_revenue_annual_payment", "Revenue from Subscriber 2 annual payments", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber3_revenue_annual_payment", "Revenue from Subscriber 3 annual payments", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber4_revenue_annual_payment", "Revenue from Subscriber 4 annual payments", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },

    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber1_bill_credit_amount", "Bill credit for Subscriber 1 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber2_bill_credit_amount", "Bill credit for Subscriber 2 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber3_bill_credit_amount", "Bill credit for Subscriber 3 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber4_bill_credit_amount", "Bill credit for Subscriber 4 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },

    { SSC_OUTPUT, SSC_ARRAY, "cf_community_solar_subscriber1_revenue", "Revenue from Subscriber 1 total", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_community_solar_subscriber2_revenue", "Revenue from Subscriber 2 total", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_community_solar_subscriber3_revenue", "Revenue from Subscriber 3 total", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_community_solar_subscriber4_revenue", "Revenue from Subscriber 4 total", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_community_solar_unsubscribed_revenue", "Revenue from unsubscribed generation", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },

    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber1_cost_of_participation", "Cost of participation for Subscriber 1 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber2_cost_of_participation", "Cost of participation for Subscriber 2 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber3_cost_of_participation", "Cost of participation for Subscriber 3 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber4_cost_of_participation", "Cost of participation for Subscriber 4 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },

    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber1_net_benefit", "Net benefit for Subscriber 1 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber2_net_benefit", "Net benefit for Subscriber 2 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber3_net_benefit", "Net benefit for Subscriber 3 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber4_net_benefit", "Net benefit for Subscriber 4 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },

    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber1_net_benefit_cumulative", "Cumulative net benefit for Subscriber 1 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber2_net_benefit_cumulative", "Cumulative net benefit for Subscriber 2 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber3_net_benefit_cumulative", "Cumulative net benefit for Subscriber 3 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_subscriber4_net_benefit_cumulative", "Cumulative net benefit for Subscriber 4 class", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },

    { SSC_OUTPUT, SSC_ARRAY, "cf_community_solar_upfront", "Community solar total up-front fixed cost", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_community_solar_upfront_per_capacity", "Community solar total up-front cost by capacity", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_community_solar_recurring_fixed", "Community solar recurring total fixed cost", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_community_solar_recurring_capacity", "Community solar total recurring cost by capacity", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },
    { SSC_OUTPUT, SSC_ARRAY, "cf_community_solar_recurring_generation", "Community solar total recurring cost by generation", "$", "", "", "*", "LENGTH_EQUAL=cf_length", "" },

    { SSC_OUTPUT,       SSC_NUMBER,     "community_solar_upfront_cost",   "Community solar total up-front cost",              "$",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "community_solar_upfront_revenue",   "Revenue from total up-front payments",              "$",                   "", "Metrics", "*", "", "" },

    { SSC_OUTPUT,       SSC_NUMBER,     "subscriber1_npv",            "Subscriber 1 NPV Net present value",             "$",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "subscriber2_npv",            "Subscriber 2 NPV Net present value",             "$",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "subscriber3_npv",            "Subscriber 3 NPV Net present value",             "$",                   "", "Metrics", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,     "subscriber4_npv",            "Subscriber 4 NPV Net present value",             "$",                   "", "Metrics", "*", "", "" },


var_info_invalid };

extern var_info
//	vtab_ppa_inout[],
	vtab_standard_financial[],
	vtab_oandm[],
    vtab_equip_reserve[],
	vtab_tax_credits[],
	vtab_depreciation_inputs[],
    vtab_depreciation_outputs[],
	vtab_payment_incentives[],
	vtab_debt[],
    vtab_financial_metrics[],
//	vtab_financial_capacity_payments[],
	vtab_financial_grid[],
	vtab_fuelcell_replacement_cost[],
    vtab_update_tech_outputs[],
    vtab_lcos_inputs[],
	vtab_battery_replacement_cost[];

enum {
	CF_energy_net,
	CF_energy_curtailed,
	CF_energy_value,
	CF_thermal_value,
	CF_curtailment_value,
	CF_capacity_payment,
	//CF_ppa_price,

	CF_om_fixed_expense,
	CF_om_production_expense,
	CF_om_capacity_expense,
	CF_om_fixed1_expense,
	CF_om_production1_expense,
	CF_om_capacity1_expense,
	CF_om_fixed2_expense,
	CF_om_production2_expense,
	CF_om_capacity2_expense,
	CF_om_fuel_expense,

	CF_om_opt_fuel_2_expense,
	CF_om_opt_fuel_1_expense,

    CF_land_lease_expense,

	CF_federal_tax_frac,
	CF_state_tax_frac,
	CF_effective_tax_frac,

	CF_property_tax_assessed_value,
	CF_property_tax_expense,
	CF_insurance_expense,
	CF_operating_expenses,
	CF_net_salvage_value,
	CF_total_revenue,
	CF_ebitda,

	CF_reserve_debtservice,
	CF_funding_debtservice,
	CF_disbursement_debtservice,
	CF_reserve_om,
	CF_funding_om,
	CF_disbursement_om,
	CF_reserve_receivables,
	CF_funding_receivables,
	CF_disbursement_receivables,
	CF_reserve_equip1,
	CF_funding_equip1,
	CF_disbursement_equip1,
	CF_reserve_equip2,
	CF_funding_equip2,
	CF_disbursement_equip2,
	CF_reserve_equip3,
	CF_funding_equip3,
	CF_disbursement_equip3,
	CF_reserve_total,
	CF_reserve_interest,


	// Project cash flow
	CF_project_operating_activities,
	CF_project_dsra,
	CF_project_wcra,
	CF_project_receivablesra,
	CF_project_me1ra,
	CF_project_me2ra,
	CF_project_me3ra,
	CF_project_ra,
	CF_project_me1cs,
	CF_project_me2cs,
	CF_project_me3cs,
	CF_project_mecs,
	CF_project_investing_activities,
	CF_project_financing_activities,
	CF_pretax_cashflow,


	// Project returns
	CF_project_return_pretax,
	CF_project_return_pretax_irr,
	CF_project_return_pretax_npv,
	CF_project_return_aftertax_cash,
	CF_project_return_aftertax_itc,
	CF_project_return_aftertax_ptc,
	CF_project_return_aftertax_tax,
	CF_project_return_aftertax,
	CF_project_return_aftertax_irr,
	CF_project_return_aftertax_max_irr,
	CF_project_return_aftertax_npv,


	CF_pv_interest_factor,
	CF_cash_for_ds,
	CF_pv_cash_for_ds,
	CF_debt_size,

	CF_debt_balance,
	CF_debt_payment_interest,
	CF_debt_payment_principal,
	CF_debt_payment_total,
	
	CF_pbi_fed,
	CF_pbi_sta,
	CF_pbi_uti,
	CF_pbi_oth,
	CF_pbi_total,
	CF_pbi_statax_total,
	CF_pbi_fedtax_total,

	CF_ptc_fed,
	CF_ptc_sta,
	CF_aftertax_ptc,
	
	CF_macrs_5_frac,
	CF_macrs_15_frac,
	CF_sl_5_frac,
	CF_sl_15_frac,
	CF_sl_20_frac,
	CF_sl_39_frac,
	CF_custom_frac,

	CF_stadepr_macrs_5,
	CF_stadepr_macrs_15,
	CF_stadepr_sl_5,
	CF_stadepr_sl_15,
	CF_stadepr_sl_20,
	CF_stadepr_sl_39,
	CF_stadepr_custom,
	CF_stadepr_me1,
	CF_stadepr_me2,
	CF_stadepr_me3,
	CF_stadepr_total,
	CF_statax_income_prior_incentives,
	CF_statax_taxable_incentives,
	CF_statax_income_with_incentives,
	CF_statax,

	CF_feddepr_macrs_5,
	CF_feddepr_macrs_15,
	CF_feddepr_sl_5,
	CF_feddepr_sl_15,
	CF_feddepr_sl_20,
	CF_feddepr_sl_39,
	CF_feddepr_custom,
	CF_feddepr_me1,
	CF_feddepr_me2,
	CF_feddepr_me3,
	CF_feddepr_total,
	CF_fedtax_income_prior_incentives,
	CF_fedtax_taxable_incentives,
	CF_fedtax_income_with_incentives,
	CF_fedtax,

	CF_me1depr_total,
	CF_me2depr_total,
	CF_me3depr_total,

	CF_sta_depr_sched,
	CF_sta_depreciation,
	CF_sta_incentive_income_less_deductions,
	CF_sta_taxable_income_less_deductions,
//	CF_sta_tax_savings,
	
	CF_fed_depr_sched,
	CF_fed_depreciation,
	CF_fed_incentive_income_less_deductions,
	CF_fed_taxable_income_less_deductions,
//	CF_fed_tax_savings,

	CF_degradation,

	CF_Recapitalization,
	CF_Recapitalization_boolean,

	CF_Annual_Costs,
	CF_pretax_dscr,

	CF_battery_replacement_cost_schedule,
	CF_battery_replacement_cost,

	CF_fuelcell_replacement_cost_schedule,
	CF_fuelcell_replacement_cost,

	CF_utility_bill,

    CF_energy_sales,
    CF_energy_purchases,

    CF_energy_without_battery,

	CF_battery_discharged,
    CF_fuelcell_discharged,

    CF_energy_charged_grid,
    CF_energy_charged_pv,
    CF_energy_discharged,
    CF_charging_cost_pv,
    CF_charging_cost_grid,
    CF_charging_cost_grid_month,
    CF_om_cost_lcos,
    CF_salvage_cost_lcos,
    CF_investment_cost_lcos,
    CF_annual_cost_lcos,
    CF_util_escal_rate,

// Community Solar specific
    CF_subscriber1_share_fraction,
    CF_subscriber2_share_fraction,
    CF_subscriber3_share_fraction,
    CF_subscriber4_share_fraction,
    CF_unsubscribed_share_fraction,

    CF_subscriber1_bill_credit_rate,
    CF_subscriber2_bill_credit_rate,
    CF_subscriber3_bill_credit_rate,
    CF_subscriber4_bill_credit_rate,

    CF_recurring_fixed,
    CF_recurring_capacity,
    CF_recurring_generation,

    CF_subscriber1_generation_payment,
    CF_subscriber2_generation_payment,
    CF_subscriber3_generation_payment,
    CF_subscriber4_generation_payment,
    CF_unsubscribed_generation_payment,

    CF_subscriber1_share_of_generation,
    CF_subscriber2_share_of_generation,
    CF_subscriber3_share_of_generation,
    CF_subscriber4_share_of_generation,
    CF_unsubscribed_share_of_generation,

    CF_subscriber1_revenue_generation,
    CF_subscriber2_revenue_generation,
    CF_subscriber3_revenue_generation,
    CF_subscriber4_revenue_generation,
    CF_unsubscribed_revenue_generation,

    CF_subscriber1_revenue_upfront,
    CF_subscriber2_revenue_upfront,
    CF_subscriber3_revenue_upfront,
    CF_subscriber4_revenue_upfront,

    CF_subscriber1_revenue_annual_payment,
    CF_subscriber2_revenue_annual_payment,
    CF_subscriber3_revenue_annual_payment,
    CF_subscriber4_revenue_annual_payment,

    CF_subscriber1_bill_credit_amount,
    CF_subscriber2_bill_credit_amount,
    CF_subscriber3_bill_credit_amount,
    CF_subscriber4_bill_credit_amount,

    CF_subscriber1_cost_of_participation,
    CF_subscriber2_cost_of_participation,
    CF_subscriber3_cost_of_participation,
    CF_subscriber4_cost_of_participation,

    CF_subscriber1_net_benefit,
    CF_subscriber2_net_benefit,
    CF_subscriber3_net_benefit,
    CF_subscriber4_net_benefit,

    CF_subscriber1_net_benefit_cumulative,
    CF_subscriber2_net_benefit_cumulative,
    CF_subscriber3_net_benefit_cumulative,
    CF_subscriber4_net_benefit_cumulative,

    CF_community_solar_subscriber1_revenue,
    CF_community_solar_subscriber2_revenue,
    CF_community_solar_subscriber3_revenue,
    CF_community_solar_subscriber4_revenue,
    CF_community_solar_unsubscribed_revenue,

    CF_community_solar_upfront,
    CF_community_solar_upfront_per_capacity,
    CF_community_solar_recurring_fixed,
    CF_community_solar_recurring_capacity,
    CF_community_solar_recurring_generation,

    // SAM 1038
    CF_itc_fed_amount,
    CF_itc_fed_percent_fraction,
    CF_itc_fed_percent_amount,
    CF_itc_fed_percent_maxvalue,
    CF_itc_fed,
    CF_itc_sta_amount,
    CF_itc_sta_percent_fraction,
    CF_itc_sta_percent_amount,
    CF_itc_sta_percent_maxvalue,
    CF_itc_sta,
    CF_itc_total,


    CF_max,
 };



class cm_communitysolar : public compute_module
{
private:
	util::matrix_t<double> cf;
    util::matrix_t<double> cf_lcos;
//	dispatch_calculations m_disp_calcs;
	hourly_energy_calculation hourly_energy_calcs;


public:
	cm_communitysolar()
	{
//        add_var_info(vtab_ppa_inout );
        add_var_info( vtab_standard_financial );
		add_var_info( vtab_oandm );
		add_var_info( vtab_equip_reserve );
		add_var_info( vtab_tax_credits );
		add_var_info(vtab_depreciation_inputs );
        add_var_info(vtab_depreciation_outputs );
        add_var_info( vtab_payment_incentives );
		add_var_info( vtab_debt );
		add_var_info( vtab_financial_metrics );
		add_var_info( _cm_vtab_communitysolar );
		add_var_info(vtab_battery_replacement_cost);
		add_var_info(vtab_fuelcell_replacement_cost);
//		add_var_info(vtab_financial_capacity_payments);
		add_var_info(vtab_financial_grid);
        add_var_info(vtab_lcos_inputs);
        add_var_info(vtab_update_tech_outputs);
    }

	void exec( )
	{
		int i = 0;

		// cash flow initialization
		int nyears = as_integer("analysis_period");
		cf.resize_fill(CF_max, nyears + 1, 0.0);
        cf_lcos.resize_fill(CF_max, nyears + 1, 0.0);
		// assign inputs
		double inflation_rate = as_double("inflation_rate")*0.01;
        //double ppa_escalation = 0.0;// as_double("ppa_escalation") * 0.01;
		double disc_real = as_double("real_discount_rate")*0.01;
//		double federal_tax_rate = as_double("federal_tax_rate")*0.01;
//		double state_tax_rate = as_double("state_tax_rate")*0.01;
		size_t count;
		ssc_number_t* arrp;
		arrp = as_array("federal_tax_rate", &count);
		if (count > 0)
		{
			if (count == 1) // single value input
			{
				for (i = 0; i < nyears; i++)
					cf.at(CF_federal_tax_frac, i + 1) = arrp[0] * 0.01;
			}
			else // schedule
			{
				for (i = 0; i < nyears && i < (int)count; i++)
					cf.at(CF_federal_tax_frac, i + 1) = arrp[i] * 0.01;
			}
		}
		arrp = as_array("state_tax_rate", &count);
		if (count > 0)
		{
			if (count == 1) // single value input
			{
				for (i = 0; i < nyears; i++)
					cf.at(CF_state_tax_frac, i + 1) = arrp[0] * 0.01;
			}
			else // schedule
			{
				for (i = 0; i < nyears && i < (int)count; i++)
					cf.at(CF_state_tax_frac, i + 1) = arrp[i] * 0.01;
			}
		}
		for (i = 0; i <= nyears; i++)
			cf.at(CF_effective_tax_frac, i) = cf.at(CF_state_tax_frac, i) +
			(1.0 - cf.at(CF_state_tax_frac, i))*cf.at(CF_federal_tax_frac, i);

		if (is_assigned("annual_thermal_value"))
		{
			arrp = as_array("annual_thermal_value", &count);
			i = 0;
			while (i < nyears && i < (int)count)
			{
				cf.at(CF_thermal_value, i + 1) = (double)arrp[i+1];
				i++;
			}
		}

		double nom_discount_rate = (1+inflation_rate)*(1+disc_real)-1;

		// In conjunction with SAM - take installed costs and salestax costs (for deducting if necessary)
		double cost_prefinancing = as_double("total_installed_cost");

		// use named range names for variables whenever possible
		double nameplate = as_double("system_capacity");
		double year1_fuel_use = as_double("annual_fuel_usage"); // kWht
		std::vector<double> fuel_use;
		if ((as_integer("system_use_lifetime_output") == 1) && is_assigned("annual_fuel_usage_lifetime")) {
			fuel_use = as_vector_double("annual_fuel_usage_lifetime");
			if (fuel_use.size() != (size_t)(nyears )) {
				throw exec_error("communitysolar", util::format("fuel usage years (%d) not equal to analysis period years (%d).", (int)fuel_use.size(), nyears));
			}
		}
		else {
			for (size_t y = 0; y < (size_t)(nyears); y++) {
				fuel_use.push_back(year1_fuel_use);
			}
		}

	
		double assessed_frac = as_double("prop_tax_cost_assessed_percent")*0.01;
		double salvage_value_frac = as_double("salvage_percentage")*0.01;
		double salvage_value = salvage_value_frac * cost_prefinancing;

		double cost_debt_closing = as_double("cost_debt_closing");
		double cost_debt_fee_frac = as_double("cost_debt_fee")*0.01;
		double cost_other_financing = as_double("cost_other_financing");
		double cost_debt_upfront;


		double constr_total_financing = as_double("construction_financing_cost");

        // Community Solar overrride since in separate vtab
		//int ppa_mode = as_integer("ppa_soln_mode");
        int ppa_mode = 1; // specify ppa price input

		bool constant_dscr_mode = (as_integer("debt_option")==1);
		bool constant_principal = (as_integer("payment_option") == 1);
		//		log(util::format("debt option=%d and constant dscr mode=%s.",
//			as_integer("debt_option"), (constant_dscr_mode ? "true":"false")),
//			SSC_WARNING);


		// general financial expenses and incentives - stdlib?
		// precompute share from annual schedules or value+escalation
        escal_or_annual(CF_om_fixed_expense, nyears, "om_fixed", inflation_rate, 1.0, false, as_double("om_fixed_escal") * 0.01);
		escal_or_annual( CF_om_production_expense, nyears, "om_production", inflation_rate, 0.001, false, as_double("om_production_escal")*0.01 );  
		escal_or_annual( CF_om_capacity_expense, nyears, "om_capacity", inflation_rate, 1.0, false, as_double("om_capacity_escal")*0.01 );  
		escal_or_annual( CF_om_fuel_expense, nyears, "om_fuel_cost", inflation_rate, as_double("system_heat_rate")*0.001, false, as_double("om_fuel_cost_escal")*0.01 );
		
		escal_or_annual( CF_om_opt_fuel_1_expense, nyears, "om_opt_fuel_1_cost", inflation_rate, 1.0, false, as_double("om_opt_fuel_1_cost_escal")*0.01 );  
		escal_or_annual( CF_om_opt_fuel_2_expense, nyears, "om_opt_fuel_2_cost", inflation_rate, 1.0, false, as_double("om_opt_fuel_2_cost_escal")*0.01 );  



		double om_opt_fuel_1_usage = as_double("om_opt_fuel_1_usage");
		double om_opt_fuel_2_usage = as_double("om_opt_fuel_2_usage");

		// additional o and m sub types (e.g. batteries and fuel cells)
		int add_om_num_types = as_integer("add_om_num_types");
		ssc_number_t nameplate1 = 0;
		ssc_number_t nameplate2 = 0;
        std::vector<double> battery_discharged(nyears,0);
        std::vector<double> fuelcell_discharged(nyears,0);

        //throw exec_error("communitysolar", "Checkpoint 1");
		if (add_om_num_types > 0) //PV Battery
		{
			escal_or_annual(CF_om_fixed1_expense, nyears, "om_batt_fixed_cost", inflation_rate, 1.0, false, as_double("om_fixed_escal")*0.01);
			escal_or_annual(CF_om_production1_expense, nyears, "om_batt_variable_cost", inflation_rate, 0.001, false, as_double("om_production_escal")*0.01); //$/MWh
			escal_or_annual(CF_om_capacity1_expense, nyears, "om_batt_capacity_cost", inflation_rate, 1.0, false, as_double("om_capacity_escal")*0.01);
			nameplate1 = as_number("om_batt_nameplate");
            if (as_integer("en_batt") == 1 || as_integer("en_standalone_batt") == 1)
                battery_discharged = as_vector_double("batt_annual_discharge_energy");
		}
        if (battery_discharged.size() == 1) { // ssc #992
            double first_val = battery_discharged[0];
            battery_discharged.resize(nyears, first_val);
        }
        if (battery_discharged.size() != nyears)
            throw exec_error("communitysolar", util::format("battery_discharged size (%d) incorrect",(int)battery_discharged.size()));

		if (add_om_num_types > 1) // PV Battery Fuel Cell
		{
			escal_or_annual(CF_om_fixed2_expense, nyears, "om_fuelcell_fixed_cost", inflation_rate, 1.0, false, as_double("om_fixed_escal")*0.01);
			escal_or_annual(CF_om_production2_expense, nyears, "om_fuelcell_variable_cost", inflation_rate, 0.001, false, as_double("om_production_escal")*0.01);
			escal_or_annual(CF_om_capacity2_expense, nyears, "om_fuelcell_capacity_cost", inflation_rate, 1.0, false, as_double("om_capacity_escal")*0.01);
			nameplate2 = as_number("om_fuelcell_nameplate");
            fuelcell_discharged = as_vector_double("fuelcell_annual_energy_discharged");
		}
        if (fuelcell_discharged.size()== 1) { // ssc #992
            double first_val = fuelcell_discharged[0];
            fuelcell_discharged.resize(nyears, first_val);
         }
        if (fuelcell_discharged.size() != nyears)
            throw exec_error("communitysolar", util::format("fuelcell_discharged size (%d) incorrect",(int)fuelcell_discharged.size()));


        // battery cost - replacement from lifetime analysis
        if ((as_integer("en_batt") == 1 || as_integer("en_standalone_batt") == 1) && (as_integer("batt_replacement_option") > 0))
        {
            ssc_number_t* batt_rep = 0;
            std::vector<ssc_number_t> replacement_percent;

            batt_rep = as_array("batt_bank_replacement", &count); // replacements per year calculated

            // replace at capacity percent
            if (as_integer("batt_replacement_option") == 1) {

                for (i = 0; i < (int)count; i++) {
                    replacement_percent.push_back(100);
                }
            }
            else {// user specified
                replacement_percent = as_vector_ssc_number_t("batt_replacement_schedule_percent");
            }
            double batt_cap = as_double("batt_computed_bank_capacity");
            // updated 10/17/15 per 10/14/15 meeting
            escal_or_annual(CF_battery_replacement_cost_schedule, nyears, "om_batt_replacement_cost", inflation_rate, batt_cap, false, as_double("om_replacement_cost_escal") * 0.01);

            for (i = 0; i < nyears && i < (int)count; i++) {
                // batt_rep and the cash flow sheets are 1 indexed, replacement_percent is zero indexed
                cf.at(CF_battery_replacement_cost, i + 1) = batt_rep[i+1] * replacement_percent[i] * 0.01 *
                    cf.at(CF_battery_replacement_cost_schedule, i + 1);
            }
        }
        else
        {
            double batt_cap = as_double("batt_computed_bank_capacity");
            // updated 10/17/15 per 10/14/15 meeting
            escal_or_annual(CF_battery_replacement_cost_schedule, nyears, "om_batt_replacement_cost", inflation_rate, batt_cap, false, as_double("om_replacement_cost_escal") * 0.01);
        }
		

		// fuelcell cost - replacement from lifetime analysis
		if (is_assigned("fuelcell_replacement_option") && (as_integer("fuelcell_replacement_option") > 0))
		{
			ssc_number_t *fuelcell_rep = 0;
			if (as_integer("fuelcell_replacement_option") == 1)
				fuelcell_rep = as_array("fuelcell_replacement", &count); // replacements per year calculated
			else // user specified
				fuelcell_rep = as_array("fuelcell_replacement_schedule", &count); // replacements per year user-defined
			escal_or_annual(CF_fuelcell_replacement_cost_schedule, nyears, "om_fuelcell_replacement_cost", inflation_rate, nameplate2, false, as_double("om_replacement_cost_escal")*0.01);

			for (i = 0; i < nyears && i < (int)count; i++) {
				cf.at(CF_fuelcell_replacement_cost, i + 1) = fuelcell_rep[i] *
					cf.at(CF_fuelcell_replacement_cost_schedule, i + 1);
			}
		}






		if (is_assigned("utility_bill_w_sys"))
		{ 
			size_t ub_count;
			ssc_number_t* ub_arr;
			ub_arr = as_array("utility_bill_w_sys", &ub_count);
			if (ub_count != (size_t)(nyears+1))
				throw exec_error("communitysolar", util::format("utility bill years (%d) not equal to analysis period years (%d).", (int)ub_count, nyears));

			for ( i = 0; i <= nyears; i++)
				cf.at(CF_utility_bill, i) = ub_arr[i];
			save_cf(CF_utility_bill, nyears, "cf_utility_bill");
		}
        else
        {
            for (i = 0; i <= nyears; i++)
                cf.at(CF_utility_bill, i) = 0;
            save_cf(CF_utility_bill, nyears, "cf_utility_bill");
        }




		// initialize energy and revenue
		// initialize energy
		// differs from samsim - accumulate hourly energy
		//double first_year_energy = as_double("energy_net");
		double first_year_energy = 0.0;
        double first_year_sales = 0.0;
        double first_year_purchases = 0.0;


		// degradation
		// degradation starts in year 2 for single value degradation - no degradation in year 1 - degradation =1.0
		// lifetime degradation applied in technology compute modules
		if (as_integer("system_use_lifetime_output") == 1)
		{
			for (i = 1; i <= nyears; i++) cf.at(CF_degradation, i) = 1.0;
		}
		else
		{
			size_t count_degrad = 0;
			ssc_number_t *degrad = 0;
			degrad = as_array("degradation", &count_degrad);

			if (count_degrad == 1)
			{
				for (i = 1; i <= nyears; i++) cf.at(CF_degradation, i) = pow((1.0 - degrad[0] / 100.0), i - 1);
			}
			else if (count_degrad > 0)
			{
				for (i = 0; i < nyears && i < (int)count_degrad; i++) cf.at(CF_degradation, i + 1) = (1.0 - degrad[i] / 100.0);
			}
		}



		hourly_energy_calcs.calculate(this);

		// dispatch
		if (as_integer("system_use_lifetime_output") == 1)
		{
			// hourly_enet includes all curtailment, availability
			for (size_t y = 1; y <= (size_t)nyears; y++)
			{
				for (size_t h = 0; h<8760; h++)
				{
					cf.at(CF_energy_net, y) += hourly_energy_calcs.hourly_energy()[(y - 1) * 8760 + h] * cf.at(CF_degradation, y);
					cf.at(CF_energy_sales, y) += hourly_energy_calcs.hourly_sales()[(y - 1) * 8760 + h] * cf.at(CF_degradation, y);
					cf.at(CF_energy_purchases, y) += hourly_energy_calcs.hourly_purchases()[(y - 1) * 8760 + h] * cf.at(CF_degradation, y);
				}
			}
		}
		else
		{
            for (i = 0; i < 8760; i++) {
                first_year_energy += hourly_energy_calcs.hourly_energy()[i]; // sum up hourly kWh to get total annual kWh first year production includes first year curtailment, availability
                first_year_sales += hourly_energy_calcs.hourly_sales()[i];
                first_year_purchases += hourly_energy_calcs.hourly_purchases()[i];
            }
			cf.at(CF_energy_net, 1) = first_year_energy;
            cf.at(CF_energy_sales, 1) = first_year_sales;
            cf.at(CF_energy_purchases, 1) = first_year_purchases;
            for (i = 1; i <= nyears; i++) {
                cf.at(CF_energy_net, i) = first_year_energy * cf.at(CF_degradation, i);
                cf.at(CF_energy_sales, i) = first_year_sales * cf.at(CF_degradation, i);
                cf.at(CF_energy_purchases, i) = first_year_purchases * cf.at(CF_degradation, i);
            }
                
		}

        if (is_assigned("gen_without_battery") && as_vector_double("gen_without_battery").size() > 0)
        {
            ssc_number_t first_year_energy_without_battery = 0.0;
            if (as_integer("system_use_lifetime_output") == 1)
            {
                // hourly_enet includes all curtailment, availability
                for (size_t y = 1; y <= (size_t)nyears; y++)
                {
                    for (size_t h = 0; h < 8760; h++)
                    {
                        cf.at(CF_energy_without_battery, y) += hourly_energy_calcs.hourly_energy_without_battery()[(y - 1) * 8760 + h] * cf.at(CF_degradation, y);
                    }
                }
            }
            else
            {
                for (i = 0; i < 8760; i++) {
                    first_year_energy_without_battery += hourly_energy_calcs.hourly_energy_without_battery()[i];
                }
                cf.at(CF_energy_without_battery, 1) = first_year_energy_without_battery;
                for (i = 1; i <= nyears; i++) {
                    cf.at(CF_energy_without_battery, i) = first_year_energy_without_battery * cf.at(CF_degradation, i);
                }

            }
        }

		// curtailed energy and revenue
		ssc_number_t pre_curtailement_year1_energy = as_number("annual_energy_pre_curtailment_ac");
		size_t count_curtailment_price;
		ssc_number_t *grid_curtailment_price = as_array("grid_curtailment_price", &count_curtailment_price);
		ssc_number_t grid_curtailment_price_esc = as_number("grid_curtailment_price_esc") * 0.01;
		// does not work with degraded energy production escal_or_annual(CF_curtailment_value, nyears, "grid_curtailment_price", 0.0, pre_curtailement_year1_energy, false, as_double("grid_curtailment_price_esc")*0.01);
		// use "system_pre_curtailment_kwac" input to determine energy curtailed for lifetime output
		if (as_integer("system_use_lifetime_output") == 1)
		{
			size_t count_pre_curtailment_kwac;
			ssc_number_t *system_pre_curtailment_kwac = as_array("system_pre_curtailment_kwac", &count_pre_curtailment_kwac);
			size_t num_rec_pre_curtailment_kwac_per_year = count_pre_curtailment_kwac / nyears;
			// hourly_enet includes all curtailment, availability
			for (size_t y = 1; y <= (size_t)nyears; y++)
			{
				cf.at(CF_energy_curtailed, y) = 0.0;
				for (size_t h = 0; h < num_rec_pre_curtailment_kwac_per_year; h++)
				{
					// add steps per hour
					cf.at(CF_energy_curtailed, y) += system_pre_curtailment_kwac[h + (y - 1)*num_rec_pre_curtailment_kwac_per_year]*(8760.0/(ssc_number_t)num_rec_pre_curtailment_kwac_per_year);
				}
				cf.at(CF_energy_curtailed, y) -= cf.at(CF_energy_net, y);
			}
		}
		else
		{
			for (size_t y = 1; y <= (size_t)nyears; y++)
				cf.at(CF_energy_curtailed, y) = pre_curtailement_year1_energy * cf.at(CF_degradation, y) - cf.at(CF_energy_net, y);
		}

		for (size_t y = 1; y <= (size_t)nyears; y++)
		{
			// for fom calculations - gen is reduced by batt from grid in compute module but updated in hourly_energy_calcs.calculate(this); above
			if (cf.at(CF_energy_curtailed, y) < 0) cf.at(CF_energy_curtailed, y) = 0; // TODO - address upstream possibly
			if (count_curtailment_price == 1)
				cf.at(CF_curtailment_value, y) = cf.at(CF_energy_curtailed, y) * grid_curtailment_price[0] * pow(1 + grid_curtailment_price_esc, y - 1);
			else if (y <= count_curtailment_price)// schedule
				cf.at(CF_curtailment_value, y) = cf.at(CF_energy_curtailed, y) * grid_curtailment_price[y - 1];
			else
				cf.at(CF_curtailment_value, y) = 0.0;
		}

        /*
		// capacity payment
		int cp_payment_type = as_integer("cp_capacity_payment_type");
		if (cp_payment_type < 0 || cp_payment_type > 1)
			throw exec_error("communitysolar", util::format("Invalid capacity payment type (%d).", cp_payment_type));
		
		size_t count_cp_payment_amount;
		ssc_number_t *cp_payment_amount = as_array("cp_capacity_payment_amount", &count_cp_payment_amount);
		ssc_number_t cp_payment_esc = as_number("cp_capacity_payment_esc") *0.01;
		if (count_cp_payment_amount == 1)
		{
			for (size_t y = 1; y <= (size_t)nyears; y++)
				cf.at(CF_capacity_payment, y) = cp_payment_amount[0] * pow(1 + cp_payment_esc, y - 1);
		}
		else
		{
			for (size_t y = 1; y <= (size_t)nyears; y++)
			{
				if (y <= count_cp_payment_amount)
					cf.at(CF_capacity_payment, y) = cp_payment_amount[y - 1];
				else
					cf.at(CF_capacity_payment, y) = 0.0;
			}
		}
		if (cp_payment_type == 0)  // capacity based payment ($/MW)
		{
			// use system nameplate
			ssc_number_t cp_nameplate = as_number("cp_system_nameplate");
			size_t count_cp_capacity_credit_percent = 0;
			ssc_number_t *cp_capacity_credit_percent = as_array("cp_capacity_credit_percent", &count_cp_capacity_credit_percent);
			if (count_cp_capacity_credit_percent == 1)
			{
				for (size_t y = 1; y <= (size_t)nyears; y++)
					cf.at(CF_capacity_payment, y) *= cp_capacity_credit_percent[0]*0.01 * cp_nameplate;
			}
			else
			{
				for (size_t y = 1; y <= (size_t)nyears; y++)
				{
					if (y <= count_cp_capacity_credit_percent)
						cf.at(CF_capacity_payment, y) *= cp_capacity_credit_percent[y - 1] * 0.01 * cp_nameplate;
					else
						cf.at(CF_capacity_payment, y) = 0.0;
				}
			}
		}
        */







		first_year_energy = cf.at(CF_energy_net, 1);
		

		std::vector<double> degrade_cf;
		for (i = 0; i <= nyears; i++)
		{
			degrade_cf.push_back(cf.at(CF_degradation, i));
		}
		// end of energy and dispatch initialization

       

		for (i=1;i<=nyears;i++)
		{
            if (is_assigned("gen_without_battery")) {
                // TODO: this does not include curtailment, but CF_energy_net does. Which should be used for VOM?
                cf.at(CF_om_production_expense, i) *= cf.at(CF_energy_without_battery, i);
            }
            else {
                cf.at(CF_om_production_expense, i) *= cf.at(CF_energy_sales, i);
            }
			cf.at(CF_om_capacity_expense, i) *= nameplate;
			cf.at(CF_om_capacity1_expense, i) *= nameplate1;
			cf.at(CF_om_capacity2_expense, i) *= nameplate2;
			cf.at(CF_om_fuel_expense,i) *= fuel_use[i-1];

            //Battery Production OM Costs
            cf.at(CF_om_production1_expense, i) *= battery_discharged[i - 1]; //$/MWh * 0.001 MWh/kWh * kWh = $
            cf.at(CF_om_production2_expense, i) *= fuelcell_discharged[i-1];

			cf.at(CF_om_opt_fuel_1_expense,i) *= om_opt_fuel_1_usage;
			cf.at(CF_om_opt_fuel_2_expense,i) *= om_opt_fuel_2_usage;
		}

        // community solar calculations
        // community solar -  subscriber fraction
        escal_or_annual(CF_subscriber1_share_fraction, nyears, "subscriber1_share", 0.0, 0.01, false, as_double("subscriber1_growth") * 0.01); // entered as percentage and growth rate without inflation
        escal_or_annual(CF_subscriber2_share_fraction, nyears, "subscriber2_share", 0.0, 0.01, false, as_double("subscriber2_growth") * 0.01); // entered as percentage and growth rate without inflation
        escal_or_annual(CF_subscriber3_share_fraction, nyears, "subscriber3_share", 0.0, 0.01, false, as_double("subscriber3_growth") * 0.01); // entered as percentage and growth rate without inflation
        escal_or_annual(CF_subscriber4_share_fraction, nyears, "subscriber4_share", 0.0, 0.01, false, as_double("subscriber4_growth") * 0.01); // entered as percentage and growth rate without inflation

        // Automatically cap the subscriber shares at the values that cause the total share to reach 100%. SAM would report a simulation message to explain the adjustment.
        // for schedules, throw simulation error if sum greater than one
        // for single value and growth rate FOR ALL FOUR subscriber classes, check values and cap as follows:
        size_t sub1_cnt, sub2_cnt, sub3_cnt, sub4_cnt;
        ssc_number_t* sub1 = as_array("subscriber1_share", &sub1_cnt);
        ssc_number_t* sub2 = as_array("subscriber2_share", &sub2_cnt);
        ssc_number_t* sub3 = as_array("subscriber3_share", &sub3_cnt);
        ssc_number_t* sub4 = as_array("subscriber4_share", &sub4_cnt);
        // check that all are entered as growth rates
        bool all_sub_growth_rate = ((sub1_cnt == 1)&& (sub2_cnt == 1)&& (sub3_cnt == 1)&& (sub4_cnt == 1));

        if (all_sub_growth_rate) {
            double prev_sum, prev_sub1, prev_sub2, prev_sub3, prev_sub4;
            double sum, sub1, sub2, sub3, sub4;
            for (size_t i = 0; i <= nyears; i++) {
                sub1 = cf.at(CF_subscriber1_share_fraction, i);
                sub2 = cf.at(CF_subscriber2_share_fraction, i);
                sub3 = cf.at(CF_subscriber3_share_fraction, i);
                sub4 = cf.at(CF_subscriber4_share_fraction, i);
                // check all subscribers for negative fractions and reset to zero
                if (sub1 < 0.0) {
                    log(util::format("Subscriber 1 share fraction was negative (%g) and reset to zero for year %d.", sub1, (int(i)), SSC_NOTICE, (float)i));
                    sub1 = 0.0;
                }
                if (sub2 < 0.0) {
                    log(util::format("Subscriber 2 share fraction was negative (%g) and reset to zero for year %d.", sub2, (int(i)), SSC_NOTICE, (float)i));
                    sub2 = 0.0;
                }
                if (sub3 < 0.0) {
                    log(util::format("Subscriber 3 share fraction was negative (%g) and reset to zero for year %d.", sub3, (int(i)), SSC_NOTICE, (float)i));
                    sub3 = 0.0;
                }
                if (sub4 < 0.0) {
                    log(util::format("Subscriber 4 share fraction was negative (%g) and reset to zero for year %d.", sub4, (int(i)), SSC_NOTICE, (float)i));
                    sub4 = 0.0;
                }
                double sum = sub1 + sub2 + sub3 + sub4;
                if (sum < 0.0) // this should not happen, all are set to >=0
                    throw exec_error("communitysolar", util::format("Total subscribed fraction for year (%d) is %g (less than zero).", (int)i, sum));
                else if (sum > 1.0) { // adjust based on previous values
                    if (prev_sum >= 1)
                        log(util::format("Total subscription fraction exceeds 1 in Year %d. Subscriber shares adjusted so that total subscription rate is 100 percent. See notices for details.", int(i)), SSC_WARNING);
                    if (prev_sum < 1.0) {
                        double additional_share = sum - 1.0; // divide up amongst changed values
                        if (i > 0) additional_share = cf.at(CF_unsubscribed_share_fraction, i - 1);
                        double newsub;
                        if (sub1 != prev_sub1) {
                            newsub = (sub1-prev_sub1) / (sum - prev_sum) * additional_share;
                            log(util::format("Subscriber 1 share fraction was %g and reset to %g for year %d", sub1, prev_sub1 + newsub, (int(i)), SSC_NOTICE, (float)i));
                            sub1 = prev_sub1 + newsub;
                        }
                        if (sub2 != prev_sub2) {
                            newsub = (sub2 - prev_sub2) / (sum - prev_sum) * additional_share;
                            log(util::format("Subscriber 2 share fraction was %g and reset to %g for year %d", sub2, prev_sub2 + newsub, (int(i)), SSC_NOTICE, (float)i));
                            sub2 = prev_sub2 + newsub;
                        }
                        if (sub3 != prev_sub3) {
                            newsub = (sub3 - prev_sub3) / (sum - prev_sum) * additional_share;
                            log(util::format("Subscriber 3 share fraction was %g and reset to %g for year %d", sub3, prev_sub3 + newsub, (int(i)), SSC_NOTICE, (float)i));
                            sub3 = prev_sub3 + newsub;
                        }
                        if (sub4 != prev_sub4) {
                            newsub = (sub4 - prev_sub4) / (sum - prev_sum) * additional_share;
                            log(util::format("Subscriber 4 share fraction was %g and reset to %g for year %d", sub4, prev_sub4 + newsub, (int(i)), SSC_NOTICE, (float)i));
                            sub4 = prev_sub4 + newsub;
                        }
                    }
                    else { // prev_sum = 1 and reset as necessary
                        if (sub1 != prev_sub1) {
                            log(util::format("Subscriber 1 share fraction was %g and reset to %g for year %d", sub1, prev_sub1, (int(i)), SSC_NOTICE, (float)i));
                            sub1 = prev_sub1;
                        }
                        if (sub2 != prev_sub2) {
                            log(util::format("Subscriber 2 share fraction was %g and reset to %g for year %d", sub2, prev_sub2, (int(i)), SSC_NOTICE, (float)i));
                            sub2 = prev_sub2;
                        }
                        if (sub3 != prev_sub3) {
                            log(util::format("Subscriber 3 share fraction was %g and reset to %g for year %d", sub3, prev_sub3, (int(i)), SSC_NOTICE, (float)i));
                            sub3 = prev_sub3;
                        }
                        if (sub4 != prev_sub4) {
                            log(util::format("Subscriber 4 share fraction was %g and reset to %g for year %d", sub4, prev_sub4, (int(i)), SSC_NOTICE, (float)i));
                            sub4 = prev_sub4;
                        }
                    }
                    //sum = sub1 + sub2 + sub3 + sub4; // check that is one
                    sum = 1.0;
                }
                cf.at(CF_subscriber1_share_fraction, i) = sub1;
                cf.at(CF_subscriber2_share_fraction, i) = sub2;
                cf.at(CF_subscriber3_share_fraction, i) = sub3;
                cf.at(CF_subscriber4_share_fraction, i) = sub4;
                cf.at(CF_unsubscribed_share_fraction, i) = 1.0 - sum;
                prev_sum = sum;
                prev_sub1 = sub1;
                prev_sub2 = sub2;
                prev_sub3 = sub3;
                prev_sub4 = sub4;
            }
        }
        else { // at least one subscriber share entered as a schedule
            for (size_t i = 0; i <= nyears; i++) {
                double sum = cf.at(CF_subscriber1_share_fraction, i) + cf.at(CF_subscriber2_share_fraction, i) + cf.at(CF_subscriber3_share_fraction, i) + cf.at(CF_subscriber4_share_fraction, i);
                if (sum < 0.0) 
                    throw exec_error("communitysolar", util::format("Total subscribed fraction for year (%d) is %g (less than zero).", (int)i, sum));
                else if (sum > 1.0)
                    throw exec_error("communitysolar", util::format("Total subscribed fraction for year (%d) is %g (greater than one).", (int)i, sum));
            }
        }

        // community solar - bill credit portion (escalation above inflation)
        escal_or_annual(CF_subscriber1_bill_credit_rate, nyears, "subscriber1_bill_credit_rate", inflation_rate, 1.0, false, as_double("subscriber1_bill_credit_rate_escal") * 0.01); 
        escal_or_annual(CF_subscriber2_bill_credit_rate, nyears, "subscriber2_bill_credit_rate", inflation_rate, 1.0, false, as_double("subscriber2_bill_credit_rate_escal") * 0.01); 
        escal_or_annual(CF_subscriber3_bill_credit_rate, nyears, "subscriber3_bill_credit_rate", inflation_rate, 1.0, false, as_double("subscriber3_bill_credit_rate_escal") * 0.01); 
        escal_or_annual(CF_subscriber4_bill_credit_rate, nyears, "subscriber4_bill_credit_rate", inflation_rate, 1.0, false, as_double("subscriber4_bill_credit_rate_escal") * 0.01); 

        // community solar - revenue - annual values
        escal_or_annual(CF_subscriber1_revenue_annual_payment, nyears, "subscriber1_payment_annual", inflation_rate, 1.0, false, as_double("subscriber1_payment_annual_escal") * 0.01);
        escal_or_annual(CF_subscriber2_revenue_annual_payment, nyears, "subscriber2_payment_annual", inflation_rate, 1.0, false, as_double("subscriber2_payment_annual_escal") * 0.01);
        escal_or_annual(CF_subscriber3_revenue_annual_payment, nyears, "subscriber3_payment_annual", inflation_rate, 1.0, false, as_double("subscriber3_payment_annual_escal") * 0.01);
        escal_or_annual(CF_subscriber4_revenue_annual_payment, nyears, "subscriber4_payment_annual", inflation_rate, 1.0, false, as_double("subscriber4_payment_annual_escal") * 0.01);

        // community solar - revenue - generation values
        escal_or_annual(CF_subscriber1_generation_payment, nyears, "subscriber1_payment_generation", inflation_rate, 1.0, false, as_double("subscriber1_payment_generation_escal") * 0.01);
        escal_or_annual(CF_subscriber2_generation_payment, nyears, "subscriber2_payment_generation", inflation_rate, 1.0, false, as_double("subscriber2_payment_generation_escal") * 0.01);
        escal_or_annual(CF_subscriber3_generation_payment, nyears, "subscriber3_payment_generation", inflation_rate, 1.0, false, as_double("subscriber3_payment_generation_escal") * 0.01);
        escal_or_annual(CF_subscriber4_generation_payment, nyears, "subscriber4_payment_generation", inflation_rate, 1.0, false, as_double("subscriber4_payment_generation_escal") * 0.01);
        escal_or_annual(CF_unsubscribed_generation_payment, nyears, "unsubscribed_payment_generation", inflation_rate, 1.0, false, as_double("unsubscribed_payment_generation_escal") * 0.01);

        // community solar - up front costs
        // TO DO these are Year zero values so shouldn't be arrays for cash flow
        cf.at(CF_community_solar_upfront, 0) = as_double("cs_cost_upfront");
        cf.at(CF_community_solar_upfront_per_capacity, 0) = as_double("cs_cost_upfront_per_capacity") * nameplate;

        // community solar - recurring cost inputs
        escal_or_annual(CF_recurring_fixed, nyears, "cs_cost_recurring_fixed", inflation_rate, 1.0, false, as_double("cs_cost_recurring_fixed_escal") * 0.01); 
        escal_or_annual(CF_recurring_capacity, nyears, "cs_cost_recurring_capacity", inflation_rate, 1.0, false, as_double("cs_cost_recurring_capacity_escal") * 0.01);
        escal_or_annual(CF_recurring_generation, nyears, "cs_cost_recurring_generation", inflation_rate, 0.001, false, as_double("cs_cost_recurring_generation_escal") * 0.01); // $/MWh scaling to $/kWh

        // community solar - revenue
        cf.at(CF_subscriber1_revenue_upfront, 0) = as_double("subscriber1_payment_upfront");
        cf.at(CF_subscriber2_revenue_upfront, 0) = as_double("subscriber2_payment_upfront");
        cf.at(CF_subscriber3_revenue_upfront, 0) = as_double("subscriber3_payment_upfront");
        cf.at(CF_subscriber4_revenue_upfront, 0) = as_double("subscriber4_payment_upfront");

        cf.at(CF_subscriber1_bill_credit_amount, 0) = 0.0;
        cf.at(CF_subscriber2_bill_credit_amount, 0) = 0.0;
        cf.at(CF_subscriber3_bill_credit_amount, 0) = 0.0;
        cf.at(CF_subscriber4_bill_credit_amount, 0) = 0.0;

         for (size_t i = 0; i <= nyears; i++) {

            // revenue to system owner from subscriber payments
            cf.at(CF_subscriber1_share_of_generation, i) = cf.at(CF_subscriber1_share_fraction, i) * cf.at(CF_energy_sales, i);
            cf.at(CF_subscriber2_share_of_generation, i) = cf.at(CF_subscriber2_share_fraction, i) * cf.at(CF_energy_sales, i);
            cf.at(CF_subscriber3_share_of_generation, i) = cf.at(CF_subscriber3_share_fraction, i) * cf.at(CF_energy_sales, i);
            cf.at(CF_subscriber4_share_of_generation, i) = cf.at(CF_subscriber4_share_fraction, i) * cf.at(CF_energy_sales, i);
            cf.at(CF_unsubscribed_share_of_generation, i) = cf.at(CF_unsubscribed_share_fraction, i) * cf.at(CF_energy_sales, i);

            cf.at(CF_subscriber1_revenue_generation, i) = cf.at(CF_subscriber1_share_of_generation, i) * cf.at(CF_subscriber1_generation_payment, i);
            cf.at(CF_subscriber2_revenue_generation, i) = cf.at(CF_subscriber2_share_of_generation, i) * cf.at(CF_subscriber2_generation_payment, i);
            cf.at(CF_subscriber3_revenue_generation, i) = cf.at(CF_subscriber3_share_of_generation, i) * cf.at(CF_subscriber3_generation_payment, i);
            cf.at(CF_subscriber4_revenue_generation, i) = cf.at(CF_subscriber4_share_of_generation, i) * cf.at(CF_subscriber4_generation_payment, i);
            cf.at(CF_unsubscribed_revenue_generation, i) = cf.at(CF_unsubscribed_share_of_generation, i) * cf.at(CF_unsubscribed_generation_payment, i);

            /* TO DO Upfront revenue treated in investment activities like IBI, so do not include in revenue cash flow unless we hear otherwise from user feedback
            cf.at(CF_community_solar_subscriber1_revenue, i) = cf.at(CF_subscriber1_revenue_upfront, i) + cf.at(CF_subscriber1_revenue_generation, i) + cf.at(CF_subscriber1_revenue_annual_payment, i);
            cf.at(CF_community_solar_subscriber2_revenue, i) = cf.at(CF_subscriber2_revenue_upfront, i) + cf.at(CF_subscriber2_revenue_generation, i) + cf.at(CF_subscriber2_revenue_annual_payment, i);
            cf.at(CF_community_solar_subscriber3_revenue, i) = cf.at(CF_subscriber3_revenue_upfront, i) + cf.at(CF_subscriber3_revenue_generation, i) + cf.at(CF_subscriber3_revenue_annual_payment, i);
            cf.at(CF_community_solar_subscriber4_revenue, i) = cf.at(CF_subscriber4_revenue_upfront, i) + cf.at(CF_subscriber4_revenue_generation, i) + cf.at(CF_subscriber4_revenue_annual_payment, i);
            cf.at(CF_community_solar_unsubscribed_revenue, i) =  cf.at(CF_unsubscribed_revenue_generation, i) ;
            */
            cf.at(CF_community_solar_subscriber1_revenue, i) = cf.at(CF_subscriber1_revenue_generation, i) + cf.at(CF_subscriber1_revenue_annual_payment, i);
            cf.at(CF_community_solar_subscriber2_revenue, i) = cf.at(CF_subscriber2_revenue_generation, i) + cf.at(CF_subscriber2_revenue_annual_payment, i);
            cf.at(CF_community_solar_subscriber3_revenue, i) = cf.at(CF_subscriber3_revenue_generation, i) + cf.at(CF_subscriber3_revenue_annual_payment, i);
            cf.at(CF_community_solar_subscriber4_revenue, i) = cf.at(CF_subscriber4_revenue_generation, i) + cf.at(CF_subscriber4_revenue_annual_payment, i);
            cf.at(CF_community_solar_unsubscribed_revenue, i) = cf.at(CF_unsubscribed_revenue_generation, i);

            // subscriber bill credits
            cf.at(CF_subscriber1_bill_credit_amount, i) = cf.at(CF_subscriber1_share_of_generation, i) * cf.at(CF_subscriber1_bill_credit_rate, i);
            cf.at(CF_subscriber2_bill_credit_amount, i) = cf.at(CF_subscriber2_share_of_generation, i) * cf.at(CF_subscriber2_bill_credit_rate, i);
            cf.at(CF_subscriber3_bill_credit_amount, i) = cf.at(CF_subscriber3_share_of_generation, i) * cf.at(CF_subscriber3_bill_credit_rate, i);
            cf.at(CF_subscriber4_bill_credit_amount, i) = cf.at(CF_subscriber4_share_of_generation, i) * cf.at(CF_subscriber4_bill_credit_rate, i);

            // subscriber cost of participation
            cf.at(CF_subscriber1_cost_of_participation, i) = cf.at(CF_community_solar_subscriber1_revenue, i);
            cf.at(CF_subscriber2_cost_of_participation, i) = cf.at(CF_community_solar_subscriber2_revenue, i);
            cf.at(CF_subscriber3_cost_of_participation, i) = cf.at(CF_community_solar_subscriber3_revenue, i);
            cf.at(CF_subscriber4_cost_of_participation, i) = cf.at(CF_community_solar_subscriber4_revenue, i);

            // subscriber net benefit
            cf.at(CF_subscriber1_net_benefit, i) = cf.at(CF_subscriber1_bill_credit_amount, i) - cf.at(CF_subscriber1_cost_of_participation, i);
            cf.at(CF_subscriber2_net_benefit, i) = cf.at(CF_subscriber2_bill_credit_amount, i) - cf.at(CF_subscriber2_cost_of_participation, i);
            cf.at(CF_subscriber3_net_benefit, i) = cf.at(CF_subscriber3_bill_credit_amount, i) - cf.at(CF_subscriber3_cost_of_participation, i);
            cf.at(CF_subscriber4_net_benefit, i) = cf.at(CF_subscriber4_bill_credit_amount, i) - cf.at(CF_subscriber4_cost_of_participation, i);

            if (i == 0)
            {
                cf.at(CF_subscriber1_net_benefit_cumulative, i) = cf.at(CF_subscriber1_net_benefit, i);
                cf.at(CF_subscriber2_net_benefit_cumulative, i) = cf.at(CF_subscriber2_net_benefit, i);
                cf.at(CF_subscriber3_net_benefit_cumulative, i) = cf.at(CF_subscriber3_net_benefit, i);
                cf.at(CF_subscriber4_net_benefit_cumulative, i) = cf.at(CF_subscriber4_net_benefit, i);
            }
            else
            {
                cf.at(CF_subscriber1_net_benefit_cumulative, i) = cf.at(CF_subscriber1_net_benefit_cumulative, i - 1) + cf.at(CF_subscriber1_net_benefit, i);
                cf.at(CF_subscriber2_net_benefit_cumulative, i) = cf.at(CF_subscriber2_net_benefit_cumulative, i - 1) + cf.at(CF_subscriber2_net_benefit, i);
                cf.at(CF_subscriber3_net_benefit_cumulative, i) = cf.at(CF_subscriber3_net_benefit_cumulative, i - 1) + cf.at(CF_subscriber3_net_benefit, i);
                cf.at(CF_subscriber4_net_benefit_cumulative, i) = cf.at(CF_subscriber4_net_benefit_cumulative, i - 1) + cf.at(CF_subscriber4_net_benefit, i);
            }

            // operating expenses
            cf.at(CF_recurring_generation, i) *= cf.at(CF_energy_sales, i);
            cf.at(CF_recurring_capacity, i) *= nameplate;

            // twice??
            cf.at(CF_community_solar_recurring_fixed, i) = cf.at(CF_recurring_fixed, i);
            cf.at(CF_community_solar_recurring_capacity, i) = cf.at(CF_recurring_capacity, i);
            cf.at(CF_community_solar_recurring_generation, i) = cf.at(CF_recurring_generation, i);
        }

        double cs_upfront_cost = cf.at(CF_community_solar_upfront, 0) + cf.at(CF_community_solar_upfront_per_capacity, 0);
        double cs_upfront_revenue = cf.at(CF_subscriber1_revenue_upfront, 0) + cf.at(CF_subscriber2_revenue_upfront, 0) + cf.at(CF_subscriber3_revenue_upfront, 0) + cf.at(CF_subscriber4_revenue_upfront, 0);

        // land lease - general for all financial models in the future
        ssc_number_t total_land_area = as_double("land_area");
        escal_or_annual(CF_land_lease_expense, nyears, "om_land_lease", inflation_rate, total_land_area, false, as_double("om_land_lease_escal") * 0.01);

		size_t count_ppa_price_input = 0;
        // Community Solar override of ppa_price_input since in separate common vtab
//		ssc_number_t* ppa_price_input = as_array("ppa_price_input", &count_ppa_price_input);
        ssc_number_t ppa_price_input[1] = { 0.0 };

		double ppa = 0;
		if (count_ppa_price_input > 0) ppa = ppa_price_input[0] * 100.0;
//		double ppa = as_double("ppa_price_input")*100.0; // either initial guess for ppa_mode=1 or final ppa for ppa_mode=0
		if (ppa_mode == 0) ppa = 0; // initial guess for target irr mode

		double property_tax_assessed_value = cost_prefinancing * as_double("prop_tax_cost_assessed_percent") * 0.01;
		double property_tax_decline_percentage = as_double("prop_tax_assessed_decline");
		double property_tax_rate = as_double("property_tax_rate")*0.01;
		double insurance_rate = as_double("insurance_rate")*0.01;
		double months_working_reserve_frac = as_double("months_working_reserve") / 12.0;
		double months_receivables_reserve_frac = as_double("months_receivables_reserve") / 12.0;
		double equip1_reserve_cost = as_double("equip1_reserve_cost");
		int equip1_reserve_freq = as_integer("equip1_reserve_freq");
		double equip2_reserve_cost = as_double("equip2_reserve_cost");
		int equip2_reserve_freq = as_integer("equip2_reserve_freq");
		double equip3_reserve_cost = as_double("equip3_reserve_cost");
		int equip3_reserve_freq = as_integer("equip3_reserve_freq");

		//  calculate debt for constant dscr mode
		int term_tenor = as_integer("term_tenor");
		int loan_moratorium = as_integer("loan_moratorium");
		double term_int_rate = as_double("term_int_rate")*0.01;
		double dscr = as_double("dscr");
		double dscr_reserve_months = as_double("dscr_reserve_months");
        bool dscr_limit_debt_fraction = as_boolean("dscr_limit_debt_fraction");
        double dscr_maximum_debt_fraction = as_double("dscr_maximum_debt_fraction") * 0.01;
		double cash_for_debt_service=0;
		double pv_cafds=0;
		double size_of_debt=0;


		// pre calculate reserves
		int i_equip1=1;
		int i_equip2=1;
		int i_equip3=1;

		for (i=1; i<=nyears; i++)
		{			
			// reserves calculations
			// major equipment 1 reserve
			if ( (i <= (i_equip1 * equip1_reserve_freq)) && ((i_equip1 * equip1_reserve_freq) <= nyears) ) // note will not enter if equip_reequip1_reserve_freq=0
			{
				cf.at(CF_funding_equip1,i) = equip1_reserve_cost * nameplate*1000 * pow( 1 + inflation_rate, i_equip1 * equip1_reserve_freq-1 ) / equip1_reserve_freq;
			}
			if (i == (i_equip1 * equip1_reserve_freq))
			{
				cf.at(CF_disbursement_equip1,i) =  -equip1_reserve_cost * nameplate*1000 * pow( 1 + inflation_rate, i-1 );
				i_equip1++;
			}
			cf.at(CF_reserve_equip1,i) = cf.at(CF_funding_equip1,i) + cf.at(CF_disbursement_equip1,i) + cf.at(CF_reserve_equip1,i-1);
			// major equipment 2 reserve
			if ( (i <= (i_equip2 * equip2_reserve_freq)) && ((i_equip2 * equip2_reserve_freq) <= nyears) ) // note will not enter if equip_reequip2_reserve_freq=0
			{
				cf.at(CF_funding_equip2,i) = equip2_reserve_cost * nameplate*1000 * pow( 1 + inflation_rate, i_equip2 * equip2_reserve_freq-1 ) / equip2_reserve_freq;
			}
			if (i == (i_equip2 * equip2_reserve_freq))
			{
				cf.at(CF_disbursement_equip2,i) =  -equip2_reserve_cost * nameplate*1000 * pow( 1 + inflation_rate, i-1 );
				i_equip2++;
			}
			cf.at(CF_reserve_equip2,i) = cf.at(CF_funding_equip2,i) + cf.at(CF_disbursement_equip2,i) + cf.at(CF_reserve_equip2,i-1);;
			// major equipment 3 reserve
			if ( (i <= (i_equip3 * equip3_reserve_freq)) && ((i_equip3 * equip3_reserve_freq) <= nyears) ) // note will not enter if equip_reequip3_reserve_freq=0
			{
				cf.at(CF_funding_equip3,i) = equip3_reserve_cost * nameplate*1000 * pow( 1 + inflation_rate, i_equip3 * equip3_reserve_freq-1 ) / equip3_reserve_freq;
			}
			if (i == (i_equip3 * equip3_reserve_freq))
			{
				cf.at(CF_disbursement_equip3,i) =  -equip3_reserve_cost * nameplate*1000 * pow( 1 + inflation_rate, i-1 );
				i_equip3++;
			}
			cf.at(CF_reserve_equip3,i) = cf.at(CF_funding_equip3,i) + cf.at(CF_disbursement_equip3,i) + cf.at(CF_reserve_equip3,i-1);
		}

		depreciation_sched_5_year_macrs_half_year(CF_macrs_5_frac,nyears);
		depreciation_sched_15_year_macrs_half_year(CF_macrs_15_frac,nyears);
		depreciation_sched_5_year_straight_line_half_year(CF_sl_5_frac,nyears);
		depreciation_sched_15_year_straight_line_half_year(CF_sl_15_frac,nyears);
		depreciation_sched_20_year_straight_line_half_year(CF_sl_20_frac,nyears);
		depreciation_sched_39_year_straight_line_half_year(CF_sl_39_frac,nyears);
		depreciation_sched_custom(CF_custom_frac,nyears,"depr_custom_schedule");

		int feddepr_me1=CF_macrs_5_frac + as_integer("equip_reserve_depr_fed");
		int feddepr_me2=CF_macrs_5_frac + as_integer("equip_reserve_depr_fed");
		int feddepr_me3=CF_macrs_5_frac + as_integer("equip_reserve_depr_fed");

		int stadepr_me1=CF_macrs_5_frac + as_integer("equip_reserve_depr_sta");
		int stadepr_me2=CF_macrs_5_frac + as_integer("equip_reserve_depr_sta");
		int stadepr_me3=CF_macrs_5_frac + as_integer("equip_reserve_depr_sta");


		for (i=1;i<=nyears;i++)
		{
			if ((equip1_reserve_freq != 0) && (i%equip1_reserve_freq == 0))
			{
				major_equipment_depreciation(CF_disbursement_equip1,feddepr_me1,i,nyears,CF_feddepr_me1);
				major_equipment_depreciation(CF_disbursement_equip1,stadepr_me1,i,nyears,CF_stadepr_me1);
			}
			if ((equip2_reserve_freq != 0) && (i%equip2_reserve_freq == 0))
			{
				major_equipment_depreciation(CF_disbursement_equip2,feddepr_me2,i,nyears,CF_feddepr_me2);
				major_equipment_depreciation(CF_disbursement_equip2,stadepr_me2,i,nyears,CF_stadepr_me2);
			}
			if ((equip3_reserve_freq != 0) && (i%equip3_reserve_freq == 0))
			{
				major_equipment_depreciation(CF_disbursement_equip3,feddepr_me3,i,nyears,CF_feddepr_me3);
				major_equipment_depreciation(CF_disbursement_equip3,stadepr_me3,i,nyears,CF_stadepr_me3);
			}
		}

		double recapitalization_cost = as_double("system_recapitalization_cost");
		double recapitalization_escalation = 0.01*as_double("system_recapitalization_escalation");
		if (as_integer("system_use_recapitalization"))
		{
			size_t recap_boolean_count;
			ssc_number_t *recap_boolean = 0;
			recap_boolean = as_array("system_lifetime_recapitalize", &recap_boolean_count);

			if (recap_boolean_count > 0)
			{
				for (i=0;i<nyears && i<(int)recap_boolean_count;i++) cf.at(CF_Recapitalization_boolean,i+1) = recap_boolean[i];
			}
		}


		for (i=1; i<=nyears; i++)
		{			
		// Project partial income statement			

			double decline_percent = 100 - (i-1)*property_tax_decline_percentage;
			cf.at(CF_property_tax_assessed_value,i) = (decline_percent > 0) ? property_tax_assessed_value * decline_percent * 0.01:0.0;
			cf.at(CF_property_tax_expense,i) = cf.at(CF_property_tax_assessed_value,i) * property_tax_rate;
			cf.at(CF_insurance_expense,i) = cost_prefinancing * insurance_rate * pow( 1 + inflation_rate, i-1 );

			if (as_integer("system_use_recapitalization"))
			{
				cf.at(CF_Recapitalization,i) = cf.at(CF_Recapitalization_boolean,i) * recapitalization_cost
					 *  pow((1 + inflation_rate + recapitalization_escalation ), i-1 );
			}
			 
			cf.at(CF_operating_expenses,i) = 
				+cf.at(CF_om_fixed_expense, i)
				+ cf.at(CF_om_production_expense, i)
				+ cf.at(CF_om_capacity_expense, i)
				+ cf.at(CF_om_fixed1_expense, i)
				+ cf.at(CF_om_production1_expense, i)
				+ cf.at(CF_om_capacity1_expense, i)
				+ cf.at(CF_om_fixed2_expense, i)
				+ cf.at(CF_om_production2_expense, i)
				+ cf.at(CF_om_capacity2_expense, i)
				+ cf.at(CF_om_fuel_expense,i)
				+ cf.at(CF_om_opt_fuel_1_expense,i)
				+ cf.at(CF_om_opt_fuel_2_expense,i)
				+ cf.at(CF_property_tax_expense,i)
				+ cf.at(CF_insurance_expense,i)
				+ cf.at(CF_battery_replacement_cost,i)
				+ cf.at(CF_fuelcell_replacement_cost, i)
				+ cf.at(CF_utility_bill,i)
                + cf.at(CF_recurring_fixed, i)
                + cf.at(CF_recurring_capacity, i)
                + cf.at(CF_recurring_generation, i)
                + cf.at(CF_land_lease_expense, i)
                + cf.at(CF_Recapitalization,i);
		}

		// salvage value
		cf.at(CF_net_salvage_value,nyears) = salvage_value;

		// o and m reserve
		if (nyears>0) 
		{
			cf.at(CF_reserve_om,0) = months_working_reserve_frac * cf.at(CF_operating_expenses,1) ;
			cf.at(CF_funding_om,0) = cf.at(CF_reserve_om,0);
			for (i=1; i<nyears; i++)
			{
				cf.at(CF_reserve_om,i) = months_working_reserve_frac * cf.at(CF_operating_expenses,i+1) ;
				cf.at(CF_funding_om,i) = cf.at(CF_reserve_om,i) - cf.at(CF_reserve_om,i-1);
			}
			cf.at(CF_disbursement_om, nyears) = -cf.at(CF_reserve_om,nyears-1);
		}

		for (i=0;i<=nyears; i++)
		{
			cf.at(CF_project_wcra,i) = -cf.at(CF_funding_om,i) - cf.at(CF_disbursement_om,i);
			cf.at(CF_project_me1ra,i) = -cf.at(CF_funding_equip1,i) - cf.at(CF_disbursement_equip1,i);
			cf.at(CF_project_me2ra,i) = -cf.at(CF_funding_equip2,i) - cf.at(CF_disbursement_equip2,i);
			cf.at(CF_project_me3ra,i) = -cf.at(CF_funding_equip3,i) - cf.at(CF_disbursement_equip3,i);
		} 

		// interest on reserves
		double reserves_interest = as_double("reserves_interest")*0.01;

		double issuance_of_equity;

		// ibi fixed
		double ibi_fed_amount = as_double("ibi_fed_amount");
		double ibi_sta_amount = as_double("ibi_sta_amount");
		double ibi_uti_amount = as_double("ibi_uti_amount");
		double ibi_oth_amount = as_double("ibi_oth_amount");

		// ibi percent
		double ibi_fed_per = as_double("ibi_fed_percent")*0.01*cost_prefinancing;
		if (ibi_fed_per > as_double("ibi_fed_percent_maxvalue")) ibi_fed_per = as_double("ibi_fed_percent_maxvalue"); 
		double ibi_sta_per = as_double("ibi_sta_percent")*0.01*cost_prefinancing;
		if (ibi_sta_per > as_double("ibi_sta_percent_maxvalue")) ibi_sta_per = as_double("ibi_sta_percent_maxvalue"); 
		double ibi_uti_per = as_double("ibi_uti_percent")*0.01*cost_prefinancing;
		if (ibi_uti_per > as_double("ibi_uti_percent_maxvalue")) ibi_uti_per = as_double("ibi_uti_percent_maxvalue"); 
		double ibi_oth_per = as_double("ibi_oth_percent")*0.01*cost_prefinancing;
		if (ibi_oth_per > as_double("ibi_oth_percent_maxvalue")) ibi_oth_per = as_double("ibi_oth_percent_maxvalue"); 

        // SAM 1038
         // itc fixed
        double itc_fed_amount = 0.0;
        double_vec vitc_fed_amount = as_vector_double("itc_fed_amount");
        for (size_t k = 0; k < vitc_fed_amount.size() && k < nyears; k++) {
            cf.at(CF_itc_fed_amount, k + 1) = vitc_fed_amount[k];
            itc_fed_amount += vitc_fed_amount[k];
        }

        double itc_sta_amount = 0.0;
        double_vec vitc_sta_amount = as_vector_double("itc_sta_amount");
        for (size_t k = 0; k < vitc_sta_amount.size() && k < nyears; k++) {
            cf.at(CF_itc_sta_amount, k + 1) = vitc_sta_amount[k];
            itc_sta_amount += vitc_sta_amount[k];
        }

        // itc percent - max value used for comparison to qualifying costs
        double_vec vitc_fed_frac = as_vector_double("itc_fed_percent");
        for (size_t k = 0; k < vitc_fed_frac.size(); k++)
            cf.at(CF_itc_fed_percent_fraction, k + 1) = vitc_fed_frac[k] * 0.01;
        double itc_fed_per;
        double_vec vitc_sta_frac = as_vector_double("itc_sta_percent");
        for (size_t k = 0; k < vitc_sta_frac.size(); k++)
            cf.at(CF_itc_sta_percent_fraction, k + 1) = vitc_sta_frac[k] * 0.01;
        double itc_sta_per;

        double_vec itc_sta_percent_maxvalue = as_vector_double("itc_sta_percent_maxvalue");
        if (itc_sta_percent_maxvalue.size() == 1) {
            for (size_t k = 0; k < nyears; k++)
                cf.at(CF_itc_sta_percent_maxvalue, k + 1) = itc_sta_percent_maxvalue[0];
        }
        else {
            for (size_t k = 0; k < itc_sta_percent_maxvalue.size() && k < nyears; k++)
                cf.at(CF_itc_sta_percent_maxvalue, k + 1) = itc_sta_percent_maxvalue[k];
        }

        double_vec itc_fed_percent_maxvalue = as_vector_double("itc_fed_percent_maxvalue");
        if (itc_fed_percent_maxvalue.size() == 1) {
            for (size_t k = 0; k < nyears; k++)
                cf.at(CF_itc_fed_percent_maxvalue, k + 1) = itc_fed_percent_maxvalue[0];
        }
        else {
            for (size_t k = 0; k < itc_fed_percent_maxvalue.size() && k < nyears; k++)
                cf.at(CF_itc_fed_percent_maxvalue, k + 1) = itc_fed_percent_maxvalue[k];
        }

		// cbi
		double cbi_fed_amount = 1000.0*nameplate*as_double("cbi_fed_amount");
		if (cbi_fed_amount > as_double("cbi_fed_maxvalue")) cbi_fed_amount = as_double("cbi_fed_maxvalue"); 
		double cbi_sta_amount = 1000.0*nameplate*as_double("cbi_sta_amount");
		if (cbi_sta_amount > as_double("cbi_sta_maxvalue")) cbi_sta_amount = as_double("cbi_sta_maxvalue"); 
		double cbi_uti_amount = 1000.0*nameplate*as_double("cbi_uti_amount");
		if (cbi_uti_amount > as_double("cbi_uti_maxvalue")) cbi_uti_amount = as_double("cbi_uti_maxvalue"); 
		double cbi_oth_amount = 1000.0*nameplate*as_double("cbi_oth_amount");
		if (cbi_oth_amount > as_double("cbi_oth_maxvalue")) cbi_oth_amount = as_double("cbi_oth_maxvalue"); 
		
		// precompute pbi
		compute_production_incentive( CF_pbi_fed, nyears, "pbi_fed_amount", "pbi_fed_term", "pbi_fed_escal" );
		compute_production_incentive( CF_pbi_sta, nyears, "pbi_sta_amount", "pbi_sta_term", "pbi_sta_escal" );
		compute_production_incentive( CF_pbi_uti, nyears, "pbi_uti_amount", "pbi_uti_term", "pbi_uti_escal" );
		compute_production_incentive( CF_pbi_oth, nyears, "pbi_oth_amount", "pbi_oth_term", "pbi_oth_escal" );

		// precompute ptc
		compute_production_incentive_IRS_2010_37( CF_ptc_sta, nyears, "ptc_sta_amount", "ptc_sta_term", "ptc_sta_escal" );
		compute_production_incentive_IRS_2010_37( CF_ptc_fed, nyears, "ptc_fed_amount", "ptc_fed_term", "ptc_fed_escal" );

		for (i=0;i<=nyears; i++)
		{
			cf.at(CF_pbi_total,i) = cf.at(CF_pbi_fed,i) + cf.at(CF_pbi_sta,i) + cf.at(CF_pbi_uti,i) + cf.at(CF_pbi_oth,i);
			cf.at(CF_aftertax_ptc,i) = cf.at(CF_ptc_fed,i) + cf.at(CF_ptc_sta,i);
		}

		double cbi_total = cbi_fed_amount + cbi_sta_amount +cbi_uti_amount + cbi_oth_amount;
		double ibi_total = ibi_fed_amount + ibi_sta_amount +ibi_uti_amount + ibi_oth_amount + ibi_fed_per + ibi_sta_per +ibi_uti_per + ibi_oth_per;

		double cbi_statax_total = 
			( as_boolean("cbi_fed_tax_sta") ? cbi_fed_amount : 0 ) +
			( as_boolean("cbi_sta_tax_sta") ? cbi_sta_amount : 0 ) +
			( as_boolean("cbi_uti_tax_sta") ? cbi_uti_amount : 0 ) +
			( as_boolean("cbi_oth_tax_sta") ? cbi_oth_amount : 0 );
		double ibi_statax_total = 
			( as_boolean("ibi_fed_amount_tax_sta") ? ibi_fed_amount : 0 ) +
			( as_boolean("ibi_fed_percent_tax_sta") ? ibi_fed_per : 0 ) +
			( as_boolean("ibi_sta_amount_tax_sta") ? ibi_sta_amount : 0 ) +
			( as_boolean("ibi_sta_percent_tax_sta") ? ibi_sta_per : 0 ) +
			( as_boolean("ibi_uti_amount_tax_sta") ? ibi_uti_amount : 0 ) +
			( as_boolean("ibi_uti_percent_tax_sta") ? ibi_uti_per : 0 ) +
			( as_boolean("ibi_oth_amount_tax_sta") ? ibi_oth_amount : 0 ) +
			( as_boolean("ibi_oth_percent_tax_sta") ? ibi_oth_per : 0 );

		double cbi_fedtax_total = 
			( as_boolean("cbi_fed_tax_fed") ? cbi_fed_amount : 0 ) +
			( as_boolean("cbi_sta_tax_fed") ? cbi_sta_amount : 0 ) +
			( as_boolean("cbi_uti_tax_fed") ? cbi_uti_amount : 0 ) +
			( as_boolean("cbi_oth_tax_fed") ? cbi_oth_amount : 0 );
		double ibi_fedtax_total = 
			( as_boolean("ibi_fed_amount_tax_fed") ? ibi_fed_amount : 0 ) +
			( as_boolean("ibi_fed_percent_tax_fed") ? ibi_fed_per : 0 ) +
			( as_boolean("ibi_sta_amount_tax_fed") ? ibi_sta_amount : 0 ) +
			( as_boolean("ibi_sta_percent_tax_fed") ? ibi_sta_per : 0 ) +
			( as_boolean("ibi_uti_amount_tax_fed") ? ibi_uti_amount : 0 ) +
			( as_boolean("ibi_uti_percent_tax_fed") ? ibi_uti_per : 0 ) +
			( as_boolean("ibi_oth_amount_tax_fed") ? ibi_oth_amount : 0 ) +
			( as_boolean("ibi_oth_percent_tax_fed") ? ibi_oth_per : 0 );


		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_pbi_statax_total,i) =
				(( as_boolean("pbi_fed_tax_sta") && (!as_boolean("pbi_fed_for_ds"))) ? cf.at(CF_pbi_fed,i) : 0 ) +
				(( as_boolean("pbi_sta_tax_sta") && (!as_boolean("pbi_sta_for_ds"))) ? cf.at(CF_pbi_sta,i) : 0 ) +
				(( as_boolean("pbi_uti_tax_sta") && (!as_boolean("pbi_uti_for_ds"))) ? cf.at(CF_pbi_uti,i) : 0 ) +
				(( as_boolean("pbi_oth_tax_sta") && (!as_boolean("pbi_oth_for_ds"))) ? cf.at(CF_pbi_oth,i) : 0 ) ;

			cf.at(CF_pbi_fedtax_total,i) =
				(( as_boolean("pbi_fed_tax_fed") && (!as_boolean("pbi_fed_for_ds"))) ? cf.at(CF_pbi_fed,i) : 0 ) +
				(( as_boolean("pbi_sta_tax_fed") && (!as_boolean("pbi_sta_for_ds"))) ? cf.at(CF_pbi_sta,i) : 0 ) +
				(( as_boolean("pbi_uti_tax_fed") && (!as_boolean("pbi_uti_for_ds"))) ? cf.at(CF_pbi_uti,i) : 0 ) +
				(( as_boolean("pbi_oth_tax_fed") && (!as_boolean("pbi_oth_for_ds"))) ? cf.at(CF_pbi_oth,i) : 0 ) ;
		}
		// 5/1/11
		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_statax_taxable_incentives,i) = cf.at(CF_pbi_statax_total,i);
			cf.at(CF_fedtax_taxable_incentives,i) = cf.at(CF_pbi_fedtax_total,i);
		}
		cf.at(CF_statax_taxable_incentives,1) += cbi_statax_total + ibi_statax_total;
		cf.at(CF_fedtax_taxable_incentives,1) += cbi_fedtax_total + ibi_fedtax_total;

		double cost_financing;

		double cost_installed;

        double pre_depr_alloc_basis; // Total costs that could qualify for depreciation before allocations
        double pre_itc_qual_basis; // Total costs that could qualify for ITC before allocations

		double depr_alloc_macrs_5_frac = as_double("depr_alloc_macrs_5_percent") * 0.01;
		double depr_alloc_macrs_15_frac = as_double("depr_alloc_macrs_15_percent") * 0.01;
		double depr_alloc_sl_5_frac = as_double("depr_alloc_sl_5_percent") * 0.01;
		double depr_alloc_sl_15_frac = as_double("depr_alloc_sl_15_percent") * 0.01;
		double depr_alloc_sl_20_frac = as_double("depr_alloc_sl_20_percent") * 0.01;
		double depr_alloc_sl_39_frac = as_double("depr_alloc_sl_39_percent") * 0.01;
		double depr_alloc_custom_frac = as_double("depr_alloc_custom_percent") * 0.01;
		double depr_alloc_total_frac = depr_alloc_macrs_5_frac + depr_alloc_macrs_15_frac +	
			depr_alloc_sl_5_frac + depr_alloc_sl_15_frac +	depr_alloc_sl_20_frac +	depr_alloc_sl_39_frac + depr_alloc_custom_frac;
		// TODO - place check that depr_alloc_total_frac <= 1 and <>0
		double depr_alloc_none_frac = 1.0 - depr_alloc_total_frac;
		// TODO - place check that depr_alloc_none_frac >= 0

		
		// redistribute fractions to only depreciable allocations
		if (depr_alloc_total_frac > 0) // and <=1
		{
			depr_alloc_macrs_5_frac /= depr_alloc_total_frac;
			depr_alloc_macrs_15_frac /= depr_alloc_total_frac;
			depr_alloc_sl_5_frac /= depr_alloc_total_frac;
			depr_alloc_sl_15_frac /= depr_alloc_total_frac;
			depr_alloc_sl_20_frac /= depr_alloc_total_frac;
			depr_alloc_sl_39_frac /= depr_alloc_total_frac;
			depr_alloc_custom_frac /= depr_alloc_total_frac;
		}
		
		double depr_stabas_macrs_5_frac;
		double depr_stabas_macrs_15_frac;
		double depr_stabas_sl_5_frac;
		double depr_stabas_sl_15_frac;
		double depr_stabas_sl_20_frac;
		double depr_stabas_sl_39_frac;
		double depr_stabas_custom_frac;

		if (as_integer("depr_stabas_method")==0) 
		{
			depr_stabas_macrs_5_frac = 1.0;
			depr_stabas_macrs_15_frac = 0.0;
			depr_stabas_sl_5_frac = 0.0;
			depr_stabas_sl_15_frac = 0.0;
			depr_stabas_sl_20_frac = 0.0;
			depr_stabas_sl_39_frac = 0.0;
			depr_stabas_custom_frac = 0.0;
		}
		else
		{
			depr_stabas_macrs_5_frac = depr_alloc_macrs_5_frac;
			depr_stabas_macrs_15_frac = depr_alloc_macrs_15_frac;
			depr_stabas_sl_5_frac = depr_alloc_sl_5_frac;
			depr_stabas_sl_15_frac = depr_alloc_sl_15_frac;
			depr_stabas_sl_20_frac = depr_alloc_sl_20_frac;
			depr_stabas_sl_39_frac = depr_alloc_sl_39_frac;
			depr_stabas_custom_frac = depr_alloc_custom_frac;
		}

		double depr_fedbas_macrs_5_frac;
		double depr_fedbas_macrs_15_frac;
		double depr_fedbas_sl_5_frac;
		double depr_fedbas_sl_15_frac;
		double depr_fedbas_sl_20_frac;
		double depr_fedbas_sl_39_frac;
		double depr_fedbas_custom_frac;

		if (as_integer("depr_fedbas_method")==0) 
		{
			depr_fedbas_macrs_5_frac = 1.0;
			depr_fedbas_macrs_15_frac = 0.0;
			depr_fedbas_sl_5_frac = 0.0;
			depr_fedbas_sl_15_frac = 0.0;
			depr_fedbas_sl_20_frac = 0.0;
			depr_fedbas_sl_39_frac = 0.0;
			depr_fedbas_custom_frac = 0.0;
		}
		else
		{
			depr_fedbas_macrs_5_frac = depr_alloc_macrs_5_frac;
			depr_fedbas_macrs_15_frac = depr_alloc_macrs_15_frac;
			depr_fedbas_sl_5_frac = depr_alloc_sl_5_frac;
			depr_fedbas_sl_15_frac = depr_alloc_sl_15_frac;
			depr_fedbas_sl_20_frac = depr_alloc_sl_20_frac;
			depr_fedbas_sl_39_frac = depr_alloc_sl_39_frac;
			depr_fedbas_custom_frac = depr_alloc_custom_frac;
		}

		double depr_alloc_macrs_5;
		double depr_alloc_macrs_15;
		double depr_alloc_sl_5;
		double depr_alloc_sl_15;
		double depr_alloc_sl_20;
		double depr_alloc_sl_39;
		double depr_alloc_custom;
		double depr_alloc_none;
		double depr_alloc_total;

		double itc_sta_qual_macrs_5_frac = ( as_boolean("depr_itc_sta_macrs_5")  ? 1: 0 ) ;
		double itc_sta_qual_macrs_15_frac = ( as_boolean("depr_itc_sta_macrs_15")  ? 1: 0 ) ;
		double itc_sta_qual_sl_5_frac = ( as_boolean("depr_itc_sta_sl_5")  ? 1: 0 ) ;
		double itc_sta_qual_sl_15_frac = ( as_boolean("depr_itc_sta_sl_15")   ? 1: 0 ) ;
		double itc_sta_qual_sl_20_frac = ( as_boolean("depr_itc_sta_sl_20")  ? 1: 0 ) ;
		double itc_sta_qual_sl_39_frac = ( as_boolean("depr_itc_sta_sl_39")  ? 1: 0 ) ;
		double itc_sta_qual_custom_frac = ( as_boolean("depr_itc_sta_custom")  ? 1: 0 ) ;

		double itc_sta_qual_total;

		double itc_sta_qual_macrs_5;
		double itc_sta_qual_macrs_15;
		double itc_sta_qual_sl_5;
		double itc_sta_qual_sl_15;
		double itc_sta_qual_sl_20;
		double itc_sta_qual_sl_39;
		double itc_sta_qual_custom;

		double itc_sta_disallow_factor = 0.5;

		double itc_disallow_sta_percent_macrs_5;
		double itc_disallow_sta_percent_macrs_15;
		double itc_disallow_sta_percent_sl_5;
		double itc_disallow_sta_percent_sl_15;
		double itc_disallow_sta_percent_sl_20;
		double itc_disallow_sta_percent_sl_39;
		double itc_disallow_sta_percent_custom;

		double itc_disallow_sta_fixed_macrs_5 = (itc_sta_disallow_factor*itc_sta_qual_macrs_5_frac * itc_sta_amount);
		double itc_disallow_sta_fixed_macrs_15 = (itc_sta_disallow_factor*itc_sta_qual_macrs_15_frac * itc_sta_amount);
		double itc_disallow_sta_fixed_sl_5 = (itc_sta_disallow_factor*itc_sta_qual_sl_5_frac * itc_sta_amount);
		double itc_disallow_sta_fixed_sl_15 = (itc_sta_disallow_factor*itc_sta_qual_sl_15_frac * itc_sta_amount);
		double itc_disallow_sta_fixed_sl_20 = (itc_sta_disallow_factor*itc_sta_qual_sl_20_frac * itc_sta_amount);
		double itc_disallow_sta_fixed_sl_39 = (itc_sta_disallow_factor*itc_sta_qual_sl_39_frac * itc_sta_amount);
		double itc_disallow_sta_fixed_custom = (itc_sta_disallow_factor*itc_sta_qual_custom_frac * itc_sta_amount);

		double itc_fed_qual_macrs_5_frac = ( as_boolean("depr_itc_fed_macrs_5")  ?1: 0 ) ;
		double itc_fed_qual_macrs_15_frac = ( as_boolean("depr_itc_fed_macrs_15")  ? 1: 0 ) ;
		double itc_fed_qual_sl_5_frac = ( as_boolean("depr_itc_fed_sl_5")  ? 1: 0 ) ;
		double itc_fed_qual_sl_15_frac = ( as_boolean("depr_itc_fed_sl_15")   ? 1: 0 ) ;
		double itc_fed_qual_sl_20_frac = ( as_boolean("depr_itc_fed_sl_20")  ? 1: 0 ) ;
		double itc_fed_qual_sl_39_frac = ( as_boolean("depr_itc_fed_sl_39")  ? 1: 0 ) ;
		double itc_fed_qual_custom_frac = ( as_boolean("depr_itc_fed_custom")  ? 1: 0 ) ;

		double itc_fed_qual_total;

		double itc_fed_qual_macrs_5;
		double itc_fed_qual_macrs_15;
		double itc_fed_qual_sl_5;
		double itc_fed_qual_sl_15;
		double itc_fed_qual_sl_20;
		double itc_fed_qual_sl_39;
		double itc_fed_qual_custom;

		double itc_fed_disallow_factor = 0.5;

		double itc_disallow_fed_percent_macrs_5;
		double itc_disallow_fed_percent_macrs_15;
		double itc_disallow_fed_percent_sl_5;
		double itc_disallow_fed_percent_sl_15;
		double itc_disallow_fed_percent_sl_20;
		double itc_disallow_fed_percent_sl_39;
		double itc_disallow_fed_percent_custom;

		double itc_disallow_fed_fixed_macrs_5 = (itc_fed_disallow_factor*itc_fed_qual_macrs_5_frac * itc_fed_amount);
		double itc_disallow_fed_fixed_macrs_15 = (itc_fed_disallow_factor*itc_fed_qual_macrs_15_frac * itc_fed_amount);
		double itc_disallow_fed_fixed_sl_5 = (itc_fed_disallow_factor*itc_fed_qual_sl_5_frac * itc_fed_amount);
		double itc_disallow_fed_fixed_sl_15 = (itc_fed_disallow_factor*itc_fed_qual_sl_15_frac * itc_fed_amount);
		double itc_disallow_fed_fixed_sl_20 = (itc_fed_disallow_factor*itc_fed_qual_sl_20_frac * itc_fed_amount);
		double itc_disallow_fed_fixed_sl_39 = (itc_fed_disallow_factor*itc_fed_qual_sl_39_frac * itc_fed_amount);
		double itc_disallow_fed_fixed_custom = (itc_fed_disallow_factor*itc_fed_qual_custom_frac * itc_fed_amount);



// Depreciation
// State depreciation
		double depr_sta_reduction_ibi =  (
			( as_boolean("ibi_fed_amount_deprbas_sta")  ? ibi_fed_amount : 0 ) +
			( as_boolean("ibi_fed_percent_deprbas_sta")  ? ibi_fed_per : 0 ) +
			( as_boolean("ibi_sta_amount_deprbas_sta")  ? ibi_sta_amount : 0 ) +
			( as_boolean("ibi_sta_percent_deprbas_sta")  ? ibi_sta_per : 0 ) +
			( as_boolean("ibi_uti_amount_deprbas_sta")  ? ibi_uti_amount : 0 ) +
			( as_boolean("ibi_uti_percent_deprbas_sta")  ? ibi_uti_per : 0 ) +
			( as_boolean("ibi_oth_amount_deprbas_sta")  ? ibi_oth_amount : 0 ) +
			( as_boolean("ibi_oth_percent_deprbas_sta")  ? ibi_oth_per : 0 )
			);

		double depr_sta_reduction_cbi =  (
			( as_boolean("cbi_fed_deprbas_sta")  ? cbi_fed_amount : 0 ) +
			( as_boolean("cbi_sta_deprbas_sta")  ? cbi_sta_amount : 0 ) +
			( as_boolean("cbi_uti_deprbas_sta")  ? cbi_uti_amount : 0 ) +
			( as_boolean("cbi_oth_deprbas_sta")  ? cbi_oth_amount : 0 ) 
			);
		
		double depr_sta_reduction = depr_sta_reduction_ibi + depr_sta_reduction_cbi;

		double depr_stabas_macrs_5;
		double depr_stabas_macrs_15;
		double depr_stabas_sl_5;
		double depr_stabas_sl_15;
		double depr_stabas_sl_20;
		double depr_stabas_sl_39;
		double depr_stabas_custom;

		// ITC reduction
		double itc_fed_percent_deprbas_sta = as_boolean("itc_fed_percent_deprbas_sta") ? 1.0 : 0.0;
		double itc_fed_amount_deprbas_sta = as_boolean("itc_fed_amount_deprbas_sta") ? 1.0 : 0.0;
		double itc_sta_percent_deprbas_sta = as_boolean("itc_sta_percent_deprbas_sta") ? 1.0 : 0.0;
		double itc_sta_amount_deprbas_sta = as_boolean("itc_sta_amount_deprbas_sta") ? 1.0 : 0.0;


		// Bonus depreciation
		double depr_stabas_macrs_5_bonus_frac = ( as_boolean("depr_bonus_sta_macrs_5") ? as_double("depr_bonus_sta")*0.01 : 0 );
		double depr_stabas_macrs_15_bonus_frac = ( as_boolean("depr_bonus_sta_macrs_15") ? as_double("depr_bonus_sta")*0.01 : 0 );
		double depr_stabas_sl_5_bonus_frac = ( as_boolean("depr_bonus_sta_sl_5") ? as_double("depr_bonus_sta")*0.01 : 0 );
		double depr_stabas_sl_15_bonus_frac = ( as_boolean("depr_bonus_sta_sl_15") ? as_double("depr_bonus_sta")*0.01 : 0 );
		double depr_stabas_sl_20_bonus_frac = ( as_boolean("depr_bonus_sta_sl_20") ? as_double("depr_bonus_sta")*0.01 : 0 );
		double depr_stabas_sl_39_bonus_frac = ( as_boolean("depr_bonus_sta_sl_39") ? as_double("depr_bonus_sta")*0.01 : 0 );
		double depr_stabas_custom_bonus_frac = ( as_boolean("depr_bonus_sta_custom") ? as_double("depr_bonus_sta")*0.01 : 0 );

		double depr_stabas_macrs_5_bonus;
		double depr_stabas_macrs_15_bonus;
		double depr_stabas_sl_5_bonus;
		double depr_stabas_sl_15_bonus;
		double depr_stabas_sl_20_bonus;
		double depr_stabas_sl_39_bonus;
		double depr_stabas_custom_bonus;

		double depr_stabas_total;

		// Federal depreciation
			double depr_fed_reduction_ibi =  (
			( as_boolean("ibi_fed_amount_deprbas_fed")  ? ibi_fed_amount : 0 ) +
			( as_boolean("ibi_fed_percent_deprbas_fed")  ? ibi_fed_per : 0 ) +
			( as_boolean("ibi_sta_amount_deprbas_fed")  ? ibi_sta_amount : 0 ) +
			( as_boolean("ibi_sta_percent_deprbas_fed")  ? ibi_sta_per : 0 ) +
			( as_boolean("ibi_uti_amount_deprbas_fed")  ? ibi_uti_amount : 0 ) +
			( as_boolean("ibi_uti_percent_deprbas_fed")  ? ibi_uti_per : 0 ) +
			( as_boolean("ibi_oth_amount_deprbas_fed")  ? ibi_oth_amount : 0 ) +
			( as_boolean("ibi_oth_percent_deprbas_fed")  ? ibi_oth_per : 0 )
			);

		double depr_fed_reduction_cbi =  (
			( as_boolean("cbi_fed_deprbas_fed")  ? cbi_fed_amount : 0 ) +
			( as_boolean("cbi_sta_deprbas_fed")  ? cbi_sta_amount : 0 ) +
			( as_boolean("cbi_uti_deprbas_fed")  ? cbi_uti_amount : 0 ) +
			( as_boolean("cbi_oth_deprbas_fed")  ? cbi_oth_amount : 0 ) 
			);
		
		double depr_fed_reduction = depr_fed_reduction_ibi + depr_fed_reduction_cbi;

		double depr_fedbas_macrs_5;
		double depr_fedbas_macrs_15;
		double depr_fedbas_sl_5;
		double depr_fedbas_sl_15;
		double depr_fedbas_sl_20;
		double depr_fedbas_sl_39;
		double depr_fedbas_custom;

		// ITC reduction
		double itc_fed_percent_deprbas_fed = as_boolean("itc_fed_percent_deprbas_fed") ? 1.0 : 0.0;
		double itc_fed_amount_deprbas_fed = as_boolean("itc_fed_amount_deprbas_fed") ? 1.0 : 0.0;
		double itc_sta_percent_deprbas_fed = as_boolean("itc_sta_percent_deprbas_fed") ? 1.0 : 0.0;
		double itc_sta_amount_deprbas_fed = as_boolean("itc_sta_amount_deprbas_fed") ? 1.0 : 0.0;

		// Bonus depreciation
		double depr_fedbas_macrs_5_bonus_frac = ( as_boolean("depr_bonus_fed_macrs_5") ? as_double("depr_bonus_fed")*0.01 : 0 );
		double depr_fedbas_macrs_15_bonus_frac = ( as_boolean("depr_bonus_fed_macrs_15") ? as_double("depr_bonus_fed")*0.01 : 0 );
		double depr_fedbas_sl_5_bonus_frac = ( as_boolean("depr_bonus_fed_sl_5") ? as_double("depr_bonus_fed")*0.01 : 0 );
		double depr_fedbas_sl_15_bonus_frac = ( as_boolean("depr_bonus_fed_sl_15") ? as_double("depr_bonus_fed")*0.01 : 0 );
		double depr_fedbas_sl_20_bonus_frac = ( as_boolean("depr_bonus_fed_sl_20") ? as_double("depr_bonus_fed")*0.01 : 0 );
		double depr_fedbas_sl_39_bonus_frac = ( as_boolean("depr_bonus_fed_sl_39") ? as_double("depr_bonus_fed")*0.01 : 0 );
		double depr_fedbas_custom_bonus_frac = ( as_boolean("depr_bonus_fed_custom") ? as_double("depr_bonus_fed")*0.01 : 0 );

		double depr_fedbas_macrs_5_bonus;
		double depr_fedbas_macrs_15_bonus;
		double depr_fedbas_sl_5_bonus;
		double depr_fedbas_sl_15_bonus;
		double depr_fedbas_sl_20_bonus;
		double depr_fedbas_sl_39_bonus;
		double depr_fedbas_custom_bonus;

		double depr_fedbas_total;


		double pbi_fed_for_ds_frac = as_boolean("pbi_fed_for_ds") ? 1.0 : 0.0;
		double pbi_sta_for_ds_frac = as_boolean("pbi_sta_for_ds") ? 1.0 : 0.0;
		double pbi_uti_for_ds_frac = as_boolean("pbi_uti_for_ds") ? 1.0 : 0.0;
		double pbi_oth_for_ds_frac = as_boolean("pbi_oth_for_ds") ? 1.0 : 0.0;

		
		//		if (ppa_mode == 0) // iterate to meet flip target by varying ppa price
        double ppa_soln_tolerance = 0;// as_double("ppa_soln_tolerance");
        int ppa_soln_max_iteations = 1;// as_integer("ppa_soln_max_iterations");
		double flip_target_percent = as_double("flip_target_percent") ;
        int flip_target_year = nyears; //  as_integer("flip_target_year");
		// check for accessing off of the end of cashflow matrix
		if (flip_target_year > nyears) flip_target_year = nyears;
		int flip_year=-1;
		double purchase_of_property;
		bool solved=true;
        double ppa_min = 0;// as_double("ppa_soln_min");
        double ppa_max = 1000;// as_double("ppa_soln_max");
		int its=0;
		double irr_weighting_factor = DBL_MAX;
		bool irr_is_minimally_met = false;
		bool irr_greater_than_target = false;
		double w0=1.0;
		double w1=1.0;
		double x0=ppa_min;
		double x1=ppa_max;
		double ppa_coarse_interval=10; // 10 cents/kWh
		bool ppa_interval_found=false;
		bool ppa_too_large=false;
		bool ppa_interval_reset=true;
		// 12/14/12 - address issue from Eric Lantz - ppa solution when target mode and ppa < 0
		double ppa_old=ppa;


		// debt fraction input
		if (!constant_dscr_mode)
		{
			double debt_frac = as_double("debt_percent")*0.01;

			cost_installed = 
				cost_prefinancing 
				+ constr_total_financing
				+ cost_debt_closing 
				+ cost_other_financing
				+ cf.at(CF_reserve_debtservice, 0) // initially zero - based on p&i
				+ cf.at(CF_reserve_om, 0)
                + cs_upfront_cost
                - cs_upfront_revenue
				- ibi_fed_amount 
				- ibi_sta_amount
				- ibi_uti_amount
				- ibi_oth_amount
				- ibi_fed_per
				- ibi_sta_per
				- ibi_uti_per
				- ibi_oth_per
				- cbi_fed_amount
				- cbi_sta_amount
				- cbi_uti_amount
				- cbi_oth_amount;
			cost_installed += debt_frac *cost_installed*cost_debt_fee_frac; // approximate up front fee
			double loan_amount = debt_frac * cost_installed;

			int i_repeat = 0;
			double old_ds_reserve = 0, new_ds_reserve = 0;

			// first year principal payment based on loan moratorium
			ssc_number_t first_principal_payment = 0;
            ssc_number_t first_principal_payment_batt = 0;
			do
			{
				// first iteration - calculate debt reserve account based on initial installed cost
				old_ds_reserve = new_ds_reserve;
				// debt service reserve
				if (loan_moratorium < 1)
				{
					if (constant_principal)
					{
						if ((term_tenor - loan_moratorium) > 0)
							first_principal_payment = (ssc_number_t)loan_amount / (ssc_number_t)(term_tenor - loan_moratorium);
					}
					else
					{
						first_principal_payment = (ssc_number_t)-ppmt(term_int_rate,       // Rate
							1,           // Period
							(term_tenor - loan_moratorium),   // Number periods
							loan_amount, // Present Value
							0,           // future Value
							0);         // cash flow at end of period
					}
				}
				else
					first_principal_payment = 0;
				cf.at(CF_debt_payment_principal, 1) = first_principal_payment;
				cf.at(CF_debt_payment_interest, 1) = loan_amount * term_int_rate;
				cf.at(CF_reserve_debtservice, 0) = dscr_reserve_months / 12.0 * (cf.at(CF_debt_payment_principal, 1) + cf.at(CF_debt_payment_interest, 1));
				cf.at(CF_funding_debtservice, 0) = cf.at(CF_reserve_debtservice, 0);
				new_ds_reserve = cf.at(CF_reserve_debtservice, 0);

				// update installed cost with approximate debt reserve account for year 0
				cost_installed =
					cost_prefinancing
					+ constr_total_financing
					+ cost_debt_closing
					+ cost_other_financing
					+ cf.at(CF_reserve_debtservice, 0) // initially zero - based on p&i
					+ cf.at(CF_reserve_om, 0)
                    + cs_upfront_cost
                    - cs_upfront_revenue
					- ibi_fed_amount
					- ibi_sta_amount
					- ibi_uti_amount
					- ibi_oth_amount
					- ibi_fed_per
					- ibi_sta_per
					- ibi_uti_per
					- ibi_oth_per
					- cbi_fed_amount
					- cbi_sta_amount
					- cbi_uti_amount
					- cbi_oth_amount;
				cost_debt_upfront = debt_frac * cost_installed * cost_debt_fee_frac; // for cash flow output
				cost_installed += debt_frac *cost_installed*cost_debt_fee_frac;
				loan_amount = debt_frac * cost_installed;
				i_repeat++;
			} while ((std::abs(new_ds_reserve - old_ds_reserve) > 1e-3) && (i_repeat < 10));

			if (term_tenor == 0) loan_amount = 0;
//			log(util::format("loan amount =%lg, debt fraction=%lg, adj installed cost=%lg", loan_amount, debt_frac, adjusted_installed_cost), SSC_WARNING);
			for ( i = 1; i <= nyears; i++)
			{
				if (i == 1)
				{
					first_principal_payment = 0;
					cf.at(CF_debt_balance, i - 1) = loan_amount;
					cf.at(CF_debt_payment_interest, i) = loan_amount * term_int_rate;
					if (i > loan_moratorium)
					{
						if (constant_principal)
						{
							if ((term_tenor - loan_moratorium) > 0)	
								first_principal_payment = (ssc_number_t)loan_amount / (ssc_number_t)(term_tenor - loan_moratorium);
						}
						else
						{
							first_principal_payment = (ssc_number_t)-ppmt(term_int_rate,       // Rate
								i,           // Period
								(term_tenor - loan_moratorium),   // Number periods
								loan_amount, // Present Value
								0,           // future Value
								0);         // cash flow at end of period
						}
					}
					cf.at(CF_debt_payment_principal, 1) = first_principal_payment;
					cf.at(CF_debt_balance, i) = cf.at(CF_debt_balance, i - 1) - cf.at(CF_debt_payment_principal, i);
// update reserve account
					cf.at(CF_debt_payment_interest, i) = loan_amount * term_int_rate;
					cf.at(CF_reserve_debtservice, i-1) = dscr_reserve_months / 12.0 * (cf.at(CF_debt_payment_principal, i) + cf.at(CF_debt_payment_interest, i));
					cf.at(CF_funding_debtservice, i-1) = cf.at(CF_reserve_debtservice, i-1);
				}
				else // i > 1 
				{
					if (i <= term_tenor)
					{
						cf.at(CF_debt_payment_interest, i) = term_int_rate * cf.at(CF_debt_balance, i - 1);
						if (i > loan_moratorium)
						{
							if (constant_principal)
							{
								if ((term_tenor - loan_moratorium) > 0) 
									cf.at(CF_debt_payment_principal, i) = loan_amount / (term_tenor - loan_moratorium);
							}
							else
							{
								if (term_int_rate != 0.0)
								{
									cf.at(CF_debt_payment_principal, i) = term_int_rate * loan_amount / (1 - pow((1 + term_int_rate), -(term_tenor - loan_moratorium)))
										- cf.at(CF_debt_payment_interest, i);
								}
								else
								{
									cf.at(CF_debt_payment_principal, i) = loan_amount / (term_tenor - loan_moratorium) - cf.at(CF_debt_payment_interest, i);
								}
							}
						}
						else
						{
								cf.at(CF_debt_payment_principal, i) = 0;
						}

						// debt service reserve
						cf.at(CF_reserve_debtservice, i - 1) = dscr_reserve_months / 12.0 *		(cf.at(CF_debt_payment_principal, i) + cf.at(CF_debt_payment_interest, i));
						cf.at(CF_funding_debtservice, i - 1) = cf.at(CF_reserve_debtservice, i - 1);
						cf.at(CF_funding_debtservice, i - 1) -= cf.at(CF_reserve_debtservice, i	 - 2);
						if (i == term_tenor) cf.at(CF_disbursement_debtservice, i) = 0 - cf.at		(CF_reserve_debtservice, i - 1);

					}
					cf.at(CF_debt_balance, i) = cf.at(CF_debt_balance, i - 1) - cf.at(CF_debt_payment_principal, i);

				}

				cf.at(CF_debt_payment_total, i) = cf.at(CF_debt_payment_principal, i) + cf.at(CF_debt_payment_interest, i);
				cf.at(CF_debt_size, i) = cf.at(CF_debt_payment_principal, i);
//				log(util::format("year=%d, debt balance =%lg, debt interest=%lg, debt principal=%lg, total debt payment=%lg, debt size=%lg", i, cf.at(CF_debt_balance, i), cf.at(CF_debt_payment_interest, i), cf.at(CF_debt_payment_principal, i), cf.at(CF_debt_payment_total, i), cf.at(CF_debt_size, i)), SSC_WARNING);

				size_of_debt += cf.at(CF_debt_size, i);
			}
			cf.at(CF_debt_balance, 0) = loan_amount;
//			log(util::format("size of debt=%lg.", size_of_debt), SSC_WARNING);

		}

//		log(util::format("before loop  - size of debt =%lg .",	size_of_debt),	SSC_WARNING);




/***************** begin iterative solution *********************************************************************/

	do
	{

		flip_year=-1;
		cash_for_debt_service=0;
		pv_cafds=0;
		if (constant_dscr_mode)	size_of_debt=0;
		if (ppa_interval_found)	ppa = (w0*x1+w1*x0)/(w0 + w1);

		// debt pre calculation
		for (i=1; i<=nyears; i++)
		{			
		// Project partial income statement			
			// energy_value = DHF Total PPA Revenue (cents/kWh)
            /* No PPA for community solar, placeholder for implementing it later
			if ((ppa_mode == 1) && (count_ppa_price_input > 1))
			{
				if (i <= (int)count_ppa_price_input)
					cf.at(CF_ppa_price, i) = ppa_price_input[i - 1] * 100.0; // $/kWh to cents/kWh
				else
					cf.at(CF_ppa_price, i) = 0;
			}
			else
				cf.at(CF_ppa_price, i) = ppa * pow(1 + ppa_escalation, i - 1); // ppa_mode==0 or single value 
            */

//			log(util::format("year %d : energy value =%lg", i, m_disp_calcs.tod_energy_value(i)), SSC_WARNING);
			// total revenue
			cf.at(CF_total_revenue,i) = cf.at(CF_energy_value,i) +
                cf.at(CF_community_solar_subscriber1_revenue, i) +
                cf.at(CF_community_solar_subscriber2_revenue, i) +
                cf.at(CF_community_solar_subscriber3_revenue, i) +
                cf.at(CF_community_solar_subscriber4_revenue, i) +
                cf.at(CF_community_solar_unsubscribed_revenue, i) +
                // cf.at(CF_thermal_value,i) +
				cf.at(CF_curtailment_value, i) +
				cf.at(CF_capacity_payment, i) +
				pbi_fed_for_ds_frac * cf.at(CF_pbi_fed,i) +
				pbi_sta_for_ds_frac * cf.at(CF_pbi_sta,i) +
				pbi_uti_for_ds_frac * cf.at(CF_pbi_uti,i) +
				pbi_oth_for_ds_frac * cf.at(CF_pbi_oth,i) +
				cf.at(CF_net_salvage_value,i);

			cf.at(CF_ebitda,i) = cf.at(CF_total_revenue,i) - cf.at(CF_operating_expenses,i);
		
	
		} // end of debt precalculation.

		// receivables precalculation need future energy value so outside previous loop
        // restore cf_thermal_value if Community Storage for fuel cell implemented, and capacity/curtailment payments if additional revenue implemented
		if (nyears>0)
		{
			cf.at(CF_reserve_receivables, 0) = months_receivables_reserve_frac * (cf.at(CF_community_solar_subscriber1_revenue, 1) + cf.at(CF_community_solar_subscriber2_revenue, 1) + cf.at(CF_community_solar_subscriber3_revenue, 1) + cf.at(CF_community_solar_subscriber4_revenue, 1) /*+ cf.at(CF_thermal_value, 1) + cf.at(CF_curtailment_value, 1) + cf.at(CF_capacity_payment, 1)*/);
			cf.at(CF_funding_receivables, 0) = cf.at(CF_reserve_receivables, 0);
			for (i = 1; i<nyears; i++)
			{
				cf.at(CF_reserve_receivables, i) = months_receivables_reserve_frac * (cf.at(CF_community_solar_subscriber1_revenue, i + 1) + cf.at(CF_community_solar_subscriber2_revenue, i + 1) + cf.at(CF_community_solar_subscriber3_revenue, i + 1) + cf.at(CF_community_solar_subscriber4_revenue, i + 1) /* + cf.at(CF_thermal_value, i + 1) + cf.at(CF_curtailment_value, i + 1) + cf.at(CF_capacity_payment, i + 1)*/);
				cf.at(CF_funding_receivables, i) = cf.at(CF_reserve_receivables, i) - cf.at(CF_reserve_receivables, i - 1);
			}
			cf.at(CF_disbursement_receivables, nyears) = -cf.at(CF_reserve_receivables, nyears - 1);
		}
		for (i = 0; i <= nyears; i++)
		{
			cf.at(CF_project_receivablesra, i) = -cf.at(CF_funding_receivables, i) - cf.at(CF_disbursement_receivables, i);
			// include receivables.
			if (i <= term_tenor)
			{
				cf.at(CF_cash_for_ds, i) = cf.at(CF_ebitda, i) - cf.at(CF_funding_equip1, i) - cf.at(CF_funding_equip2, i) - cf.at(CF_funding_equip3, i) - cf.at(CF_funding_receivables, i);
				cash_for_debt_service += cf.at(CF_cash_for_ds, i);
				if (i <= 1)
					cf.at(CF_pv_interest_factor, i) = 1.0 / (1.0 + term_int_rate);
				else
					cf.at(CF_pv_interest_factor, i) = cf.at(CF_pv_interest_factor, i - 1) / (1.0 + term_int_rate);
				cf.at(CF_pv_cash_for_ds, i) = cf.at(CF_pv_interest_factor, i) * cf.at(CF_cash_for_ds, i);
				pv_cafds += cf.at(CF_pv_cash_for_ds, i);
				if (constant_dscr_mode)
				{
					if (dscr != 0) cf.at(CF_debt_size, i) = cf.at(CF_pv_cash_for_ds, i) / dscr;
					size_of_debt += cf.at(CF_debt_size, i);
				}
			}
		}

        /* Github issue 550 update dscr if necessary with limit on maximum debt fraction */
        if (constant_dscr_mode && dscr_limit_debt_fraction) {
            // Initial estimate of these costs for subsequent calculations
            cost_financing =
                cost_debt_closing +
                cost_debt_fee_frac * size_of_debt +
                cost_other_financing +
                cf.at(CF_reserve_debtservice, 0) +
                constr_total_financing +
                cf.at(CF_reserve_om, 0) +
                cf.at(CF_reserve_receivables, 0);

            // Community Solar adjustment for up-front revenue and costs
            cost_installed =
                cost_prefinancing
                + cost_financing
                + cs_upfront_cost
                - cs_upfront_revenue
                - ibi_fed_amount
                - ibi_sta_amount
                - ibi_uti_amount
                - ibi_oth_amount
                - ibi_fed_per
                - ibi_sta_per
                - ibi_uti_per
                - ibi_oth_per
                - cbi_fed_amount
                - cbi_sta_amount
                - cbi_uti_amount
                - cbi_oth_amount;

            if ((std::abs(size_of_debt) > (cost_installed * dscr_maximum_debt_fraction)) || (size_of_debt < 0)) {
                if ((cost_installed > 0) && (dscr_maximum_debt_fraction > 0)) {

                    dscr = size_of_debt / (cost_installed * dscr_maximum_debt_fraction) * dscr;
                    // recalculate debt size with constrained dscr
                    size_of_debt = 0.0;
                    for (i = 0; i <= nyears; i++) {
                        if (dscr > 0)
                            cf.at(CF_debt_size, i) = cf.at(CF_pv_cash_for_ds, i) / dscr;
                        else
                            cf.at(CF_debt_size, i) = 0.0; // default behavior of initialization of cash flow line items
                        size_of_debt += cf.at(CF_debt_size, i);
                    }

                }
            }
        }

		/*
		// DSCR calculations
		for (i = 0; i <= nyears; i++)
		{
		if (cf.at(CF_debt_payment_total, i) == 0.0) cf.at(CF_pretax_dscr, i) = 0; //cf.at(CF_pretax_dscr, i) = std::numeric_limits<double>::quiet_NaN();
		else cf.at(CF_pretax_dscr, i) = cf.at(CF_cash_for_ds, i) / cf.at(CF_debt_payment_total, i);
		}

		*/
		if (constant_dscr_mode)
		{
			cf.at(CF_debt_balance, 0) = size_of_debt;

			for (i = 1; ((i <= nyears) && (i <= term_tenor)); i++)
			{
				cf.at(CF_debt_payment_interest, i) = cf.at(CF_debt_balance, i - 1) * term_int_rate;
					if (dscr != 0)
						cf.at(CF_debt_payment_total, i) = cf.at(CF_cash_for_ds, i) / dscr;
				else
					cf.at(CF_debt_payment_total, i) = cf.at(CF_debt_payment_interest, i);
				cf.at(CF_debt_payment_principal, i) = cf.at(CF_debt_payment_total, i) - cf.at(CF_debt_payment_interest, i);
				cf.at(CF_debt_balance, i) = cf.at(CF_debt_balance, i - 1) - cf.at(CF_debt_payment_principal, i);
			}


			// debt service reserve
			for (i = 1; ((i <= nyears) && (i <= term_tenor)); i++)
			{
				cf.at(CF_reserve_debtservice, i - 1) = dscr_reserve_months / 12.0 * (cf.at(CF_debt_payment_principal, i) + cf.at(CF_debt_payment_interest, i));
				cf.at(CF_funding_debtservice, i - 1) = cf.at(CF_reserve_debtservice, i - 1);
				if (i > 1) cf.at(CF_funding_debtservice, i - 1) -= cf.at(CF_reserve_debtservice, i - 2);
				if (i == term_tenor) cf.at(CF_disbursement_debtservice, i) = 0 - cf.at(CF_reserve_debtservice, i - 1);
			}
		}

		// total reserves
		for (i=0; i<=nyears; i++)
			cf.at(CF_reserve_total,i) = 
				cf.at(CF_reserve_debtservice,i) +
				cf.at(CF_reserve_om, i) +
				cf.at(CF_reserve_receivables, i) +
				cf.at(CF_reserve_equip1, i) +
				cf.at(CF_reserve_equip2,i) +
				cf.at(CF_reserve_equip3,i);
		for (i=1; i<=nyears; i++)
			cf.at(CF_reserve_interest,i) = reserves_interest * cf.at(CF_reserve_total,i-1);

		cost_financing =
			cost_debt_closing +
			cost_debt_fee_frac * size_of_debt +
			cost_other_financing +
			cf.at(CF_reserve_debtservice, 0) +
			constr_total_financing +
			cf.at(CF_reserve_om, 0) +
			cf.at(CF_reserve_receivables, 0);

		cost_debt_upfront = cost_debt_fee_frac * size_of_debt; // cpg added this to make cash flow consistent with single_owner.xlsx

        // Community Solar adjustment for up-front revenue and costs
		cost_installed =
            cost_prefinancing
            + cost_financing
            + cs_upfront_cost
            - cs_upfront_revenue
			- ibi_fed_amount
			- ibi_sta_amount
			- ibi_uti_amount
			- ibi_oth_amount
			- ibi_fed_per
			- ibi_sta_per
			- ibi_uti_per
			- ibi_oth_per
			- cbi_fed_amount
			- cbi_sta_amount
			- cbi_uti_amount
			- cbi_oth_amount;

        // Installed costs and construction costs, developer fees, and legal fees can be claimed in the basis, but reserves and financing fees cannot
        // See https://github.com/NREL/SAM/issues/1803 and linked issues for more details
        pre_depr_alloc_basis = cost_prefinancing
            + cost_other_financing
            + constr_total_financing;
        // Basis reductions are handled in depr_fed_reduction and depr_sta_reduction

        // Under 2024 law these are understood to be the same, keep seperate variables for reporting out
        pre_itc_qual_basis = pre_depr_alloc_basis;
			
		depr_alloc_total = depr_alloc_total_frac * pre_depr_alloc_basis;
		depr_alloc_macrs_5 = depr_alloc_macrs_5_frac * depr_alloc_total;
		depr_alloc_macrs_15 = depr_alloc_macrs_15_frac * depr_alloc_total;
		depr_alloc_sl_5 = depr_alloc_sl_5_frac * depr_alloc_total;
		depr_alloc_sl_15 = depr_alloc_sl_15_frac * depr_alloc_total;
		depr_alloc_sl_20 = depr_alloc_sl_20_frac * depr_alloc_total;
		depr_alloc_sl_39 = depr_alloc_sl_39_frac * depr_alloc_total;
		depr_alloc_custom = depr_alloc_custom_frac * depr_alloc_total;
		depr_alloc_none = depr_alloc_none_frac * depr_alloc_total;

		itc_sta_qual_macrs_5 = itc_sta_qual_macrs_5_frac * ( depr_alloc_macrs_5 - depr_stabas_macrs_5_frac * depr_sta_reduction);
		itc_sta_qual_macrs_15 = itc_sta_qual_macrs_15_frac * ( depr_alloc_macrs_15 - depr_stabas_macrs_15_frac * depr_sta_reduction);
		itc_sta_qual_sl_5 = itc_sta_qual_sl_5_frac * ( depr_alloc_sl_5 - depr_stabas_sl_5_frac * depr_sta_reduction);
		itc_sta_qual_sl_15 = itc_sta_qual_sl_15_frac * ( depr_alloc_sl_15 - depr_stabas_sl_15_frac * depr_sta_reduction);
		itc_sta_qual_sl_20 = itc_sta_qual_sl_20_frac * ( depr_alloc_sl_20 - depr_stabas_sl_20_frac * depr_sta_reduction);
		itc_sta_qual_sl_39 = itc_sta_qual_sl_39_frac * ( depr_alloc_sl_39 - depr_stabas_sl_39_frac * depr_sta_reduction);
		itc_sta_qual_custom = itc_sta_qual_custom_frac * ( depr_alloc_custom - depr_stabas_custom_frac * depr_sta_reduction);

		itc_sta_qual_total = itc_sta_qual_macrs_5 + itc_sta_qual_macrs_15 + itc_sta_qual_sl_5 +itc_sta_qual_sl_15 +itc_sta_qual_sl_20 + itc_sta_qual_sl_39 + itc_sta_qual_custom;

        itc_sta_per = 0.0;
        for (size_t k = 0; k <= nyears; k++) {
            cf.at(CF_itc_sta_percent_amount, k) = min(cf.at(CF_itc_sta_percent_maxvalue, k), cf.at(CF_itc_sta_percent_fraction, k) * itc_sta_qual_total);
            itc_sta_per += cf.at(CF_itc_sta_percent_amount, k);
        }

		if (itc_sta_qual_total > 0)
		{
			itc_disallow_sta_percent_macrs_5 = itc_sta_qual_macrs_5_frac * (itc_sta_disallow_factor * itc_sta_qual_macrs_5 / itc_sta_qual_total * itc_sta_per);
			itc_disallow_sta_percent_macrs_15 = itc_sta_qual_macrs_15_frac * (itc_sta_disallow_factor * itc_sta_qual_macrs_15 / itc_sta_qual_total * itc_sta_per);
			itc_disallow_sta_percent_sl_5 = itc_sta_qual_sl_5_frac * (itc_sta_disallow_factor * itc_sta_qual_sl_5 / itc_sta_qual_total * itc_sta_per);
			itc_disallow_sta_percent_sl_15 = itc_sta_qual_sl_15_frac * (itc_sta_disallow_factor * itc_sta_qual_sl_15 / itc_sta_qual_total * itc_sta_per);
			itc_disallow_sta_percent_sl_20 = itc_sta_qual_sl_20_frac * (itc_sta_disallow_factor * itc_sta_qual_sl_20 / itc_sta_qual_total * itc_sta_per);
			itc_disallow_sta_percent_sl_39 = itc_sta_qual_sl_39_frac * (itc_sta_disallow_factor * itc_sta_qual_sl_39 / itc_sta_qual_total * itc_sta_per);
			itc_disallow_sta_percent_custom = itc_sta_qual_custom_frac * (itc_sta_disallow_factor * itc_sta_qual_custom / itc_sta_qual_total * itc_sta_per);

			itc_disallow_sta_fixed_macrs_5 = itc_sta_qual_macrs_5_frac * (itc_sta_disallow_factor * itc_sta_qual_macrs_5 / itc_sta_qual_total * itc_sta_amount);
			itc_disallow_sta_fixed_macrs_15 = itc_sta_qual_macrs_15_frac * (itc_sta_disallow_factor * itc_sta_qual_macrs_15 / itc_sta_qual_total * itc_sta_amount);
			itc_disallow_sta_fixed_sl_5 = itc_sta_qual_sl_5_frac * (itc_sta_disallow_factor * itc_sta_qual_sl_5 / itc_sta_qual_total * itc_sta_amount);
			itc_disallow_sta_fixed_sl_15 = itc_sta_qual_sl_15_frac * (itc_sta_disallow_factor * itc_sta_qual_sl_15 / itc_sta_qual_total * itc_sta_amount);
			itc_disallow_sta_fixed_sl_20 = itc_sta_qual_sl_20_frac * (itc_sta_disallow_factor * itc_sta_qual_sl_20 / itc_sta_qual_total * itc_sta_amount);
			itc_disallow_sta_fixed_sl_39 = itc_sta_qual_sl_39_frac * (itc_sta_disallow_factor * itc_sta_qual_sl_39 / itc_sta_qual_total * itc_sta_amount);
			itc_disallow_sta_fixed_custom = itc_sta_qual_custom_frac * (itc_sta_disallow_factor * itc_sta_qual_custom / itc_sta_qual_total * itc_sta_amount);
		}
		else
		{
			itc_disallow_sta_percent_macrs_5 = 0;
			itc_disallow_sta_percent_macrs_15 = 0;
			itc_disallow_sta_percent_sl_5 = 0;
			itc_disallow_sta_percent_sl_15 = 0;
			itc_disallow_sta_percent_sl_20 = 0;
			itc_disallow_sta_percent_sl_39 = 0;
			itc_disallow_sta_percent_custom = 0;

			itc_disallow_sta_fixed_macrs_5 = 0;
			itc_disallow_sta_fixed_macrs_15 = 0;
			itc_disallow_sta_fixed_sl_5 = 0;
			itc_disallow_sta_fixed_sl_15 = 0;
			itc_disallow_sta_fixed_sl_20 = 0;
			itc_disallow_sta_fixed_sl_39 = 0;
			itc_disallow_sta_fixed_custom = 0;
		}

		itc_fed_qual_macrs_5 = itc_fed_qual_macrs_5_frac * ( depr_alloc_macrs_5 - depr_fedbas_macrs_5_frac * depr_fed_reduction);
		itc_fed_qual_macrs_15 = itc_fed_qual_macrs_15_frac * ( depr_alloc_macrs_15 - depr_fedbas_macrs_15_frac * depr_fed_reduction);
		itc_fed_qual_sl_5 = itc_fed_qual_sl_5_frac * ( depr_alloc_sl_5 - depr_fedbas_sl_5_frac * depr_fed_reduction);
		itc_fed_qual_sl_15 = itc_fed_qual_sl_15_frac * ( depr_alloc_sl_15 - depr_fedbas_sl_15_frac * depr_fed_reduction);
		itc_fed_qual_sl_20 = itc_fed_qual_sl_20_frac * ( depr_alloc_sl_20 - depr_fedbas_sl_20_frac * depr_fed_reduction);
		itc_fed_qual_sl_39 = itc_fed_qual_sl_39_frac * ( depr_alloc_sl_39 - depr_fedbas_sl_39_frac * depr_fed_reduction);
		itc_fed_qual_custom = itc_fed_qual_custom_frac * ( depr_alloc_custom - depr_fedbas_custom_frac * depr_fed_reduction);

		itc_fed_qual_total = itc_fed_qual_macrs_5 + itc_fed_qual_macrs_15 + itc_fed_qual_sl_5 +itc_fed_qual_sl_15 +itc_fed_qual_sl_20 + itc_fed_qual_sl_39 + itc_fed_qual_custom;

        itc_fed_per = 0.0;
        for (size_t k = 0; k <= nyears; k++) {
            cf.at(CF_itc_fed_percent_amount, k) = min(cf.at(CF_itc_fed_percent_maxvalue, k), cf.at(CF_itc_fed_percent_fraction, k) * itc_fed_qual_total);
            itc_fed_per += cf.at(CF_itc_fed_percent_amount, k);
        }

		if (itc_fed_qual_total > 0)
		{
			itc_disallow_fed_percent_macrs_5 = itc_fed_qual_macrs_5_frac * (itc_fed_disallow_factor * itc_fed_qual_macrs_5 / itc_fed_qual_total * itc_fed_per);
			itc_disallow_fed_percent_macrs_15 = itc_fed_qual_macrs_15_frac * (itc_fed_disallow_factor * itc_fed_qual_macrs_15 / itc_fed_qual_total * itc_fed_per);
			itc_disallow_fed_percent_sl_5 = itc_fed_qual_sl_5_frac * (itc_fed_disallow_factor * itc_fed_qual_sl_5 / itc_fed_qual_total * itc_fed_per);
			itc_disallow_fed_percent_sl_15 = itc_fed_qual_sl_15_frac * (itc_fed_disallow_factor * itc_fed_qual_sl_15 / itc_fed_qual_total * itc_fed_per);
			itc_disallow_fed_percent_sl_20 = itc_fed_qual_sl_20_frac * (itc_fed_disallow_factor * itc_fed_qual_sl_20 / itc_fed_qual_total * itc_fed_per);
			itc_disallow_fed_percent_sl_39 = itc_fed_qual_sl_39_frac * (itc_fed_disallow_factor * itc_fed_qual_sl_39 / itc_fed_qual_total * itc_fed_per);
			itc_disallow_fed_percent_custom = itc_fed_qual_custom_frac * (itc_fed_disallow_factor * itc_fed_qual_custom / itc_fed_qual_total * itc_fed_per);

			itc_disallow_fed_fixed_macrs_5 = itc_fed_qual_macrs_5_frac * (itc_fed_disallow_factor * itc_fed_qual_macrs_5 / itc_fed_qual_total * itc_fed_amount);
			itc_disallow_fed_fixed_macrs_15 = itc_fed_qual_macrs_15_frac * (itc_fed_disallow_factor * itc_fed_qual_macrs_15 / itc_fed_qual_total * itc_fed_amount);
			itc_disallow_fed_fixed_sl_5 = itc_fed_qual_sl_5_frac * (itc_fed_disallow_factor * itc_fed_qual_sl_5 / itc_fed_qual_total * itc_fed_amount);
			itc_disallow_fed_fixed_sl_15 = itc_fed_qual_sl_15_frac * (itc_fed_disallow_factor * itc_fed_qual_sl_15 / itc_fed_qual_total * itc_fed_amount);
			itc_disallow_fed_fixed_sl_20 = itc_fed_qual_sl_20_frac * (itc_fed_disallow_factor * itc_fed_qual_sl_20 / itc_fed_qual_total * itc_fed_amount);
			itc_disallow_fed_fixed_sl_39 = itc_fed_qual_sl_39_frac * (itc_fed_disallow_factor * itc_fed_qual_sl_39 / itc_fed_qual_total * itc_fed_amount);
			itc_disallow_fed_fixed_custom = itc_fed_qual_custom_frac * (itc_fed_disallow_factor * itc_fed_qual_custom / itc_fed_qual_total * itc_fed_amount);
		}
		else
		{
			itc_disallow_fed_percent_macrs_5 = 0;
			itc_disallow_fed_percent_macrs_15 = 0;
			itc_disallow_fed_percent_sl_5 = 0;
			itc_disallow_fed_percent_sl_15 = 0;
			itc_disallow_fed_percent_sl_20 = 0;
			itc_disallow_fed_percent_sl_39 = 0;
			itc_disallow_fed_percent_custom = 0;

			itc_disallow_fed_fixed_macrs_5 = 0;
			itc_disallow_fed_fixed_macrs_15 = 0;
			itc_disallow_fed_fixed_sl_5 = 0;
			itc_disallow_fed_fixed_sl_15 = 0;
			itc_disallow_fed_fixed_sl_20 = 0;
			itc_disallow_fed_fixed_sl_39 = 0;
			itc_disallow_fed_fixed_custom = 0;
		}

        // SAM 1038
        for (size_t k = 0; k <= nyears; k++) {
            cf.at(CF_itc_fed, k) = cf.at(CF_itc_fed_amount, k) + cf.at(CF_itc_fed_percent_amount, k);
            cf.at(CF_itc_sta, k) = cf.at(CF_itc_sta_amount, k) + cf.at(CF_itc_sta_percent_amount, k);
            cf.at(CF_itc_total, k) = cf.at(CF_itc_fed, k) + cf.at(CF_itc_sta, k);
        }


// Depreciation
// State depreciation
		depr_stabas_macrs_5 = depr_alloc_macrs_5 - depr_stabas_macrs_5_frac * depr_sta_reduction;
		depr_stabas_macrs_15 = depr_alloc_macrs_15 - depr_stabas_macrs_15_frac * depr_sta_reduction;
		depr_stabas_sl_5 = depr_alloc_sl_5 - depr_stabas_sl_5_frac * depr_sta_reduction;
		depr_stabas_sl_15 = depr_alloc_sl_15 - depr_stabas_sl_15_frac * depr_sta_reduction;
		depr_stabas_sl_20 = depr_alloc_sl_20 - depr_stabas_sl_20_frac * depr_sta_reduction;
		depr_stabas_sl_39 = depr_alloc_sl_39 - depr_stabas_sl_39_frac * depr_sta_reduction;
		depr_stabas_custom = depr_alloc_custom - depr_stabas_custom_frac * depr_sta_reduction;

		// ITC reduction
		depr_stabas_macrs_5 -= (itc_fed_percent_deprbas_sta * itc_disallow_fed_percent_macrs_5 +
								itc_fed_amount_deprbas_sta * itc_disallow_fed_fixed_macrs_5 +
								itc_sta_percent_deprbas_sta * itc_disallow_sta_percent_macrs_5 +
								itc_sta_amount_deprbas_sta * itc_disallow_sta_fixed_macrs_5 );

		depr_stabas_macrs_15 -= (itc_fed_percent_deprbas_sta * itc_disallow_fed_percent_macrs_15 +
								itc_fed_amount_deprbas_sta * itc_disallow_fed_fixed_macrs_15 +
								itc_sta_percent_deprbas_sta * itc_disallow_sta_percent_macrs_15 +
								itc_sta_amount_deprbas_sta * itc_disallow_sta_fixed_macrs_15 );

		depr_stabas_sl_5 -= (itc_fed_percent_deprbas_sta * itc_disallow_fed_percent_sl_5 +
								itc_fed_amount_deprbas_sta * itc_disallow_fed_fixed_sl_5 +
								itc_sta_percent_deprbas_sta * itc_disallow_sta_percent_sl_5 +
								itc_sta_amount_deprbas_sta * itc_disallow_sta_fixed_sl_5 );

		depr_stabas_sl_15 -= (itc_fed_percent_deprbas_sta * itc_disallow_fed_percent_sl_15 +
								itc_fed_amount_deprbas_sta * itc_disallow_fed_fixed_sl_15 +
								itc_sta_percent_deprbas_sta * itc_disallow_sta_percent_sl_15 +
								itc_sta_amount_deprbas_sta * itc_disallow_sta_fixed_sl_15 );

		depr_stabas_sl_20 -= (itc_fed_percent_deprbas_sta * itc_disallow_fed_percent_sl_20 +
								itc_fed_amount_deprbas_sta * itc_disallow_fed_fixed_sl_20 +
								itc_sta_percent_deprbas_sta * itc_disallow_sta_percent_sl_20 +
								itc_sta_amount_deprbas_sta * itc_disallow_sta_fixed_sl_20 );

		depr_stabas_sl_39 -= (itc_fed_percent_deprbas_sta * itc_disallow_fed_percent_sl_39 +
								itc_fed_amount_deprbas_sta * itc_disallow_fed_fixed_sl_39 +
								itc_sta_percent_deprbas_sta * itc_disallow_sta_percent_sl_39 +
								itc_sta_amount_deprbas_sta * itc_disallow_sta_fixed_sl_39 );

		depr_stabas_custom -= (itc_fed_percent_deprbas_sta * itc_disallow_fed_percent_custom +
								itc_fed_amount_deprbas_sta * itc_disallow_fed_fixed_custom +
								itc_sta_percent_deprbas_sta * itc_disallow_sta_percent_custom +
								itc_sta_amount_deprbas_sta * itc_disallow_sta_fixed_custom );

		// Bonus depreciation
		depr_stabas_macrs_5_bonus = depr_stabas_macrs_5_bonus_frac * depr_stabas_macrs_5;
		depr_stabas_macrs_15_bonus = depr_stabas_macrs_15_bonus_frac * depr_stabas_macrs_15;
		depr_stabas_sl_5_bonus = depr_stabas_sl_5_bonus_frac * depr_stabas_sl_5;
		depr_stabas_sl_15_bonus = depr_stabas_sl_15_bonus_frac * depr_stabas_sl_15;
		depr_stabas_sl_20_bonus = depr_stabas_sl_20_bonus_frac * depr_stabas_sl_20;
		depr_stabas_sl_39_bonus = depr_stabas_sl_39_bonus_frac * depr_stabas_sl_39;
		depr_stabas_custom_bonus = depr_stabas_custom_bonus_frac * depr_stabas_custom;

		depr_stabas_macrs_5 -= depr_stabas_macrs_5_bonus;
		depr_stabas_macrs_15 -= depr_stabas_macrs_15_bonus;
		depr_stabas_sl_5 -= depr_stabas_sl_5_bonus;
		depr_stabas_sl_15 -= depr_stabas_sl_15_bonus;
		depr_stabas_sl_20 -= depr_stabas_sl_20_bonus;
		depr_stabas_sl_39 -= depr_stabas_sl_39_bonus;
		depr_stabas_custom -= depr_stabas_custom_bonus;
		
		depr_stabas_total = depr_stabas_macrs_5 + depr_stabas_macrs_15 + depr_stabas_sl_5 + depr_stabas_sl_15 + depr_stabas_sl_20 + depr_stabas_sl_39 + depr_stabas_custom;

		// Federal depreciation
		depr_fedbas_macrs_5 = depr_alloc_macrs_5 - depr_fedbas_macrs_5_frac * depr_fed_reduction;
		depr_fedbas_macrs_15 = depr_alloc_macrs_15 - depr_fedbas_macrs_15_frac * depr_fed_reduction;
		depr_fedbas_sl_5 = depr_alloc_sl_5 - depr_fedbas_sl_5_frac * depr_fed_reduction;
		depr_fedbas_sl_15 = depr_alloc_sl_15 - depr_fedbas_sl_15_frac * depr_fed_reduction;
		depr_fedbas_sl_20 = depr_alloc_sl_20 - depr_fedbas_sl_20_frac * depr_fed_reduction;
		depr_fedbas_sl_39 = depr_alloc_sl_39 - depr_fedbas_sl_39_frac * depr_fed_reduction;
		depr_fedbas_custom = depr_alloc_custom - depr_fedbas_custom_frac * depr_fed_reduction;

		// ITC reduction
		depr_fedbas_macrs_5 -= (itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_macrs_5 +
								itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_macrs_5 +
								itc_sta_percent_deprbas_fed * itc_disallow_sta_percent_macrs_5 +
								itc_sta_amount_deprbas_fed * itc_disallow_sta_fixed_macrs_5 );

		depr_fedbas_macrs_15 -= (itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_macrs_15 +
								itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_macrs_15 +
								itc_sta_percent_deprbas_fed * itc_disallow_sta_percent_macrs_15 +
								itc_sta_amount_deprbas_fed * itc_disallow_sta_fixed_macrs_15 );

		depr_fedbas_sl_5 -= (itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_sl_5 +
								itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_sl_5 +
								itc_sta_percent_deprbas_fed * itc_disallow_sta_percent_sl_5 +
								itc_sta_amount_deprbas_fed * itc_disallow_sta_fixed_sl_5 );

		depr_fedbas_sl_15 -= (itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_sl_15 +
								itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_sl_15 +
								itc_sta_percent_deprbas_fed * itc_disallow_sta_percent_sl_15 +
								itc_sta_amount_deprbas_fed * itc_disallow_sta_fixed_sl_15 );

		depr_fedbas_sl_20 -= (itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_sl_20 +
								itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_sl_20 +
								itc_sta_percent_deprbas_fed * itc_disallow_sta_percent_sl_20 +
								itc_sta_amount_deprbas_fed * itc_disallow_sta_fixed_sl_20 );

		depr_fedbas_sl_39 -= (itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_sl_39 +
								itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_sl_39 +
								itc_sta_percent_deprbas_fed * itc_disallow_sta_percent_sl_39 +
								itc_sta_amount_deprbas_fed * itc_disallow_sta_fixed_sl_39 );

		depr_fedbas_custom -= (itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_custom +
								itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_custom +
								itc_sta_percent_deprbas_fed * itc_disallow_sta_percent_custom +
								itc_sta_amount_deprbas_fed * itc_disallow_sta_fixed_custom );

		// Bonus depreciation
		depr_fedbas_macrs_5_bonus = depr_fedbas_macrs_5_bonus_frac * depr_fedbas_macrs_5;
		depr_fedbas_macrs_15_bonus = depr_fedbas_macrs_15_bonus_frac * depr_fedbas_macrs_15;
		depr_fedbas_sl_5_bonus = depr_fedbas_sl_5_bonus_frac * depr_fedbas_sl_5;
		depr_fedbas_sl_15_bonus = depr_fedbas_sl_15_bonus_frac * depr_fedbas_sl_15;
		depr_fedbas_sl_20_bonus = depr_fedbas_sl_20_bonus_frac * depr_fedbas_sl_20;
		depr_fedbas_sl_39_bonus = depr_fedbas_sl_39_bonus_frac * depr_fedbas_sl_39;
		depr_fedbas_custom_bonus = depr_fedbas_custom_bonus_frac * depr_fedbas_custom;

		depr_fedbas_macrs_5 -= depr_fedbas_macrs_5_bonus;
		depr_fedbas_macrs_15 -= depr_fedbas_macrs_15_bonus;
		depr_fedbas_sl_5 -= depr_fedbas_sl_5_bonus;
		depr_fedbas_sl_15 -= depr_fedbas_sl_15_bonus;
		depr_fedbas_sl_20 -= depr_fedbas_sl_20_bonus;
		depr_fedbas_sl_39 -= depr_fedbas_sl_39_bonus;
		depr_fedbas_custom -= depr_fedbas_custom_bonus;
		
		depr_fedbas_total = depr_fedbas_macrs_5 + depr_fedbas_macrs_15 + depr_fedbas_sl_5 + depr_fedbas_sl_15 + depr_fedbas_sl_20 + depr_fedbas_sl_39 + depr_fedbas_custom;

		purchase_of_property = -cost_installed + cf.at(CF_reserve_debtservice, 0) + cf.at(CF_reserve_om, 0) + cf.at(CF_reserve_receivables, 0);
//		issuance_of_equity = cost_installed - (size_of_debt + ibi_total + cbi_total);
		issuance_of_equity = cost_installed - size_of_debt;

		for (i=0; i<=nyears; i++)
		{
			//			cf.at(CF_project_operating_activities,i) = cf.at(CF_ebitda,i) + cf.at(CF_pbi_total,i) + cf.at(CF_reserve_interest,i) - cf.at(CF_debt_payment_interest,i);
			cf.at(CF_project_operating_activities,i) = cf.at(CF_ebitda,i) + cf.at(CF_reserve_interest,i) - cf.at(CF_debt_payment_interest,i) +
				(1.0 - pbi_fed_for_ds_frac) * cf.at(CF_pbi_fed,i) +
				(1-0 - pbi_sta_for_ds_frac) * cf.at(CF_pbi_sta,i) +
				(1-0 - pbi_uti_for_ds_frac) * cf.at(CF_pbi_uti,i) +
				(1-0 - pbi_oth_for_ds_frac) * cf.at(CF_pbi_oth,i);
			cf.at(CF_project_dsra,i) = -cf.at(CF_funding_debtservice,i) - cf.at(CF_disbursement_debtservice,i);
			cf.at(CF_project_ra,i) =
				cf.at(CF_project_dsra,i) +
				cf.at(CF_project_wcra, i) +
				cf.at(CF_project_receivablesra, i) +
				cf.at(CF_project_me1ra, i) +
				cf.at(CF_project_me2ra,i) +
				cf.at(CF_project_me3ra,i);
			cf.at(CF_project_me1cs,i) = cf.at(CF_disbursement_equip1,i);
			cf.at(CF_project_me2cs,i) = cf.at(CF_disbursement_equip2,i);
			cf.at(CF_project_me3cs,i) = cf.at(CF_disbursement_equip3,i);
			cf.at(CF_project_mecs,i) =
				cf.at(CF_project_me1cs,i) +
				cf.at(CF_project_me2cs,i) +
				cf.at(CF_project_me3cs,i);
			cf.at(CF_project_investing_activities,i) = cf.at(CF_project_ra,i) + cf.at(CF_project_mecs,i);
			if (i==0) cf.at(CF_project_investing_activities,i) += purchase_of_property;

			cf.at(CF_project_financing_activities,i) = -cf.at(CF_debt_payment_principal,i);
            if (i == 0) cf.at(CF_project_financing_activities, i) += issuance_of_equity + size_of_debt;

			cf.at(CF_pretax_cashflow,i) = cf.at(CF_project_operating_activities,i) + cf.at(CF_project_investing_activities,i) + cf.at(CF_project_financing_activities,i);

			cf.at(CF_project_return_pretax,i) = cf.at(CF_pretax_cashflow,i);
			if (i==0) cf.at(CF_project_return_pretax,i) -= (issuance_of_equity); 

			cf.at(CF_project_return_pretax_irr,i) = irr(CF_project_return_pretax,i)*100.0;
			cf.at(CF_project_return_pretax_npv,i) = npv(CF_project_return_pretax,i,nom_discount_rate) +  cf.at(CF_project_return_pretax,0) ;

			cf.at(CF_project_return_aftertax_cash,i) = cf.at(CF_project_return_pretax,i);
		}


		cf.at(CF_project_return_aftertax,0) = cf.at(CF_project_return_aftertax_cash,0);
		cf.at(CF_project_return_aftertax_irr,0) = irr(CF_project_return_aftertax_tax,0)*100.0;
		cf.at(CF_project_return_aftertax_max_irr,0) = cf.at(CF_project_return_aftertax_irr,0);
		cf.at(CF_project_return_aftertax_npv,0) = cf.at(CF_project_return_aftertax,0) ;


		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_stadepr_macrs_5,i) = cf.at(CF_macrs_5_frac,i) * depr_stabas_macrs_5;
			cf.at(CF_stadepr_macrs_15,i) = cf.at(CF_macrs_15_frac,i) * depr_stabas_macrs_15;
			cf.at(CF_stadepr_sl_5,i) = cf.at(CF_sl_5_frac,i) * depr_stabas_sl_5;
			cf.at(CF_stadepr_sl_15,i) = cf.at(CF_sl_15_frac,i) * depr_stabas_sl_15;
			cf.at(CF_stadepr_sl_20,i) = cf.at(CF_sl_20_frac,i) * depr_stabas_sl_20;
			cf.at(CF_stadepr_sl_39,i) = cf.at(CF_sl_39_frac,i) * depr_stabas_sl_39;
			cf.at(CF_stadepr_custom,i) = cf.at(CF_custom_frac,i) * depr_stabas_custom;


			cf.at(CF_stadepr_total,i)=
				cf.at(CF_stadepr_macrs_5,i)+
				cf.at(CF_stadepr_macrs_15,i)+
				cf.at(CF_stadepr_sl_5,i)+
				cf.at(CF_stadepr_sl_15,i)+
				cf.at(CF_stadepr_sl_20,i)+
				cf.at(CF_stadepr_sl_39,i)+
				cf.at(CF_stadepr_custom,i)+
				cf.at(CF_stadepr_me1,i)+
				cf.at(CF_stadepr_me2,i)+
				cf.at(CF_stadepr_me3,i);

			if (i==1) cf.at(CF_stadepr_total,i) += ( depr_stabas_macrs_5_bonus +depr_stabas_macrs_15_bonus + depr_stabas_sl_5_bonus + depr_stabas_sl_15_bonus + depr_stabas_sl_20_bonus + depr_stabas_sl_39_bonus + depr_stabas_custom_bonus);
			cf.at(CF_statax_income_prior_incentives,i)=
				cf.at(CF_ebitda,i) + 
				cf.at(CF_reserve_interest,i) -
				cf.at(CF_debt_payment_interest,i) -
				cf.at(CF_stadepr_total,i);



			// pbi in ebitda - so remove if non-taxable
			// 5/1/11
			cf.at(CF_statax_income_with_incentives,i) = cf.at(CF_statax_income_prior_incentives,i) + cf.at(CF_statax_taxable_incentives,i);
			cf.at(CF_statax, i) = -cf.at(CF_state_tax_frac, i) * cf.at(CF_statax_income_with_incentives, i);

// federal 
			cf.at(CF_feddepr_macrs_5,i) = cf.at(CF_macrs_5_frac,i) * depr_fedbas_macrs_5;
			cf.at(CF_feddepr_macrs_15,i) = cf.at(CF_macrs_15_frac,i) * depr_fedbas_macrs_15;
			cf.at(CF_feddepr_sl_5,i) = cf.at(CF_sl_5_frac,i) * depr_fedbas_sl_5;
			cf.at(CF_feddepr_sl_15,i) = cf.at(CF_sl_15_frac,i) * depr_fedbas_sl_15;
			cf.at(CF_feddepr_sl_20,i) = cf.at(CF_sl_20_frac,i) * depr_fedbas_sl_20;
			cf.at(CF_feddepr_sl_39,i) = cf.at(CF_sl_39_frac,i) * depr_fedbas_sl_39;
			cf.at(CF_feddepr_custom,i) = cf.at(CF_custom_frac,i) * depr_fedbas_custom;
			cf.at(CF_feddepr_total,i)=
				cf.at(CF_feddepr_macrs_5,i)+
				cf.at(CF_feddepr_macrs_15,i)+
				cf.at(CF_feddepr_sl_5,i)+
				cf.at(CF_feddepr_sl_15,i)+
				cf.at(CF_feddepr_sl_20,i)+
				cf.at(CF_feddepr_sl_39,i)+
				cf.at(CF_feddepr_custom,i)+
				cf.at(CF_feddepr_me1,i)+
				cf.at(CF_feddepr_me2,i)+
				cf.at(CF_feddepr_me3,i);
			if (i==1) cf.at(CF_feddepr_total,i) += ( depr_fedbas_macrs_5_bonus +depr_fedbas_macrs_15_bonus + depr_fedbas_sl_5_bonus + depr_fedbas_sl_15_bonus + depr_fedbas_sl_20_bonus + depr_fedbas_sl_39_bonus + depr_fedbas_custom_bonus);
			cf.at(CF_fedtax_income_prior_incentives,i)=
				cf.at(CF_ebitda,i) + 
				cf.at(CF_reserve_interest,i) -
				cf.at(CF_debt_payment_interest,i) -
				cf.at(CF_feddepr_total,i) +
				cf.at(CF_statax,i) +
				cf.at(CF_ptc_sta,i) +
                cf.at(CF_itc_sta, i);
            //	SAM 1038		if (i==1) cf.at(CF_fedtax_income_prior_incentives,i) += itc_sta_total;


			// pbi in ebitda - so remove if non-taxable
			// 5/1/11
			cf.at(CF_fedtax_income_with_incentives,i) = cf.at(CF_fedtax_income_prior_incentives,i) + cf.at(CF_fedtax_taxable_incentives,i);
			cf.at(CF_fedtax, i) = -cf.at(CF_federal_tax_frac, i) * cf.at(CF_fedtax_income_with_incentives, i);

			cf.at(CF_project_return_aftertax,i) = 
				cf.at(CF_project_return_aftertax_cash,i) +
				cf.at(CF_ptc_fed,i) + cf.at(CF_ptc_sta,i) +
				cf.at(CF_statax,i) + cf.at(CF_fedtax,i) + cf.at(CF_itc_total, i);
            //	SAM 1038		if (i==1) cf.at(CF_project_return_aftertax,i) += itc_total;


			cf.at(CF_project_return_aftertax_irr,i) = irr(CF_project_return_aftertax,i)*100.0;
			cf.at(CF_project_return_aftertax_max_irr,i) = max(cf.at(CF_project_return_aftertax_max_irr,i-1),cf.at(CF_project_return_aftertax_irr,i));
			cf.at(CF_project_return_aftertax_npv,i) = npv(CF_project_return_aftertax,i,nom_discount_rate) +  cf.at(CF_project_return_aftertax,0) ;

			if (flip_year <=0) 
			{
				double residual = std::abs(cf.at(CF_project_return_aftertax_irr, i) - flip_target_percent) / 100.0; // solver checks fractions and not percentages
				if ( ( cf.at(CF_project_return_aftertax_max_irr,i-1) < flip_target_percent ) &&  (   residual  < ppa_soln_tolerance ) 	) 
				{
					flip_year = i;
					cf.at(CF_project_return_aftertax_max_irr,i)=flip_target_percent; //within tolerance so pre-flip and post-flip percentages applied correctly
				}
				else if ((cf.at(CF_project_return_aftertax_max_irr, i - 1) < flip_target_percent) && (cf.at(CF_project_return_aftertax_max_irr, i) >= flip_target_percent)) flip_year = i;
			}


		}
		cf.at(CF_project_return_aftertax_npv,0) = cf.at(CF_project_return_aftertax,0) ;

		// 12/14/12 - address issue from Eric Lantz - ppa solution when target mode and ppa < 0
		ppa_old = ppa;

		if (ppa_mode == 0)
		{
		// 12/14/12 - address issue from Eric Lantz - ppa solution when target mode and ppa < 0
			double resid_denom = max(flip_target_percent,1);
		// 12/14/12 - address issue from Eric Lantz - ppa solution when target mode and ppa < 0
			double ppa_denom = max(x0, x1);
			if (ppa_denom <= ppa_soln_tolerance) ppa_denom = 1;
			double residual = cf.at(CF_project_return_aftertax_irr, flip_target_year) - flip_target_percent;
			solved = ((std::abs( residual )/resid_denom < ppa_soln_tolerance ) || (std::abs(x0-x1)/ppa_denom < ppa_soln_tolerance) );
//			solved = (( fabs( residual ) < ppa_soln_tolerance ) );
				double flip_frac = flip_target_percent/100.0;
				double itnpv_target = npv(CF_project_return_aftertax,flip_target_year,flip_frac) +  cf.at(CF_project_return_aftertax,0) ;
//				double itnpv_target_delta = npv(CF_project_return_aftertax,flip_target_year,flip_frac+0.001) +  cf.at(CF_project_return_aftertax,0) ;
			//	double itnpv_actual = npv(CF_project_return_aftertax,flip_target_year,cf.at(CF_project_return_aftertax_irr, flip_target_year)) +  cf.at(CF_project_return_aftertax,0) ;
			//	double itnpv_actual_delta = npv(CF_project_return_aftertax,flip_target_year,cf.at(CF_project_return_aftertax_irr, flip_target_year)+0.001) +  cf.at(CF_project_return_aftertax,0) ;
			if (!solved)
			{
//				double flip_frac = flip_target_percent/100.0;
//				double itnpv_target = npv(CF_project_return_aftertax,flip_target_year,flip_frac) +  cf.at(CF_project_return_aftertax,0) ;
				irr_weighting_factor = std::abs(itnpv_target);
				irr_is_minimally_met = ((irr_weighting_factor < ppa_soln_tolerance));
				irr_greater_than_target = (( itnpv_target >= 0.0) || irr_is_minimally_met );
				if (ppa_interval_found)
				{// reset interval
				
					if (irr_greater_than_target) // too large
					{
			// set endpoint of weighted interval x0<x1
						x1 = ppa;
						w1 = irr_weighting_factor;
					}
					else // too small
					{
			// set endpoint of weighted interval x0<x1
						x0 = ppa;
						w0 = irr_weighting_factor;
					}

				}
				else
				{ // find solution interval [x0,x1]
					if (ppa_interval_reset) 
					{
							if (irr_greater_than_target) ppa_too_large=true;
							ppa_interval_reset=false;
					}
					if (ppa_too_large) // too large
					{
						if (irr_greater_than_target)
						{
							x0 = ppa;
							w0 = irr_weighting_factor;
							ppa = x0-ppa_coarse_interval;
						}
						else
						{
						  x1 = x0;
						  w1 = w0;
						  x0 = ppa;
						  w0 = irr_weighting_factor;
						  ppa_interval_found=true;
						}
					}
					else
					{
						if (!irr_greater_than_target)
						{
							x1 = ppa;
							w1 = irr_weighting_factor;
							ppa = x1+ppa_coarse_interval;
						}
						else
						{
						  x0 = x1;
						  w0 = w1;
						  x1 = ppa;
						  w1 = irr_weighting_factor;
						  ppa_interval_found=true;
						}
					}
					// for initial guess of zero
					if (std::abs(x0-x1)<ppa_soln_tolerance) x0 = x1-2*ppa_soln_tolerance;
				}
					//std::stringstream outm;
					//outm << "iteration=" << its  << ", irr=" << cf.at(CF_project_return_aftertax_irr, flip_target_year)  << ", npvtarget=" << itnpv_target  << ", npvtarget_delta=" << itnpv_target_delta  
					//	//  << ", npvactual=" << itnpv_actual  << ", npvactual_delta=" << itnpv_target_delta  
					//	<< ", residual=" << residual << ", ppa=" << ppa << ", x0=" << x0 << ", x1=" << x1 <<  ",w0=" << w0 << ", w1=" << w1 << ", ppamax-ppamin=" << x1-x0;
					//log( outm.str() );
			}
		}
		its++;

	}	// target tax investor return in target year
	while (!solved && !irr_is_minimally_met  && (its < ppa_soln_max_iteations) && (ppa >= 0) );


		// 12/14/12 - address issue from Eric Lantz - ppa solution when target mode and ppa < 0
	if (ppa < 0) ppa = ppa_old;	

/***************** end iterative solution *********************************************************************/

//	log(util::format("after loop  - size of debt =%lg .", size_of_debt), SSC_WARNING);

   

	// NPV of revenue components for stacked bar chart
	/*
		{ SSC_OUTPUT,       SSC_NUMBER,     "npv_curtailment_revenue",                        "Present value of curtailment payment revenue",              "$",                   "", "Metrics", "*", "", "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_capacity_revenue",                        "Present value of capacity payment revenue",              "$",                   "", "Metrics", "*", "", "" },
		// only count toward revenue if user selected
	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_fed_pbi_income",                        "Present value of federal PBI income",              "$",                   "", "Metrics", "*", "", "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_sta_pbi_income",                        "Present value of state PBI income",              "$",                   "", "Metrics", "*", "", "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_uti_pbi_income",                        "Present value of utility PBI income",              "$",                   "", "Metrics", "*", "", "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_oth_pbi_income",                        "Present value of other PBI income",              "$",                   "", "Metrics", "*", "", "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_salvage_value",                        "Present value of salvage value",              "$",                   "", "Metrics", "*", "", "" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "npv_thermal_value",                        "Present value of thermal value",              "$",                   "", "Metrics", "*", "", "" },

	*/
	assign("npv_curtailment_revenue", var_data((ssc_number_t)npv(CF_curtailment_value, nyears, nom_discount_rate)));
	assign("npv_capacity_revenue", var_data((ssc_number_t)npv(CF_capacity_payment, nyears, nom_discount_rate)));
	assign("npv_fed_pbi_income", var_data((ssc_number_t)npv(CF_pbi_fed, nyears, nom_discount_rate)));
	assign("npv_sta_pbi_income", var_data((ssc_number_t)npv(CF_pbi_sta, nyears, nom_discount_rate)));
	assign("npv_uti_pbi_income", var_data((ssc_number_t)npv(CF_pbi_uti, nyears, nom_discount_rate)));
	assign("npv_oth_pbi_income", var_data((ssc_number_t)npv(CF_pbi_oth, nyears, nom_discount_rate)));
	assign("npv_salvage_value", var_data((ssc_number_t)npv(CF_net_salvage_value, nyears, nom_discount_rate)));
	assign("npv_thermal_value", var_data((ssc_number_t)npv(CF_thermal_value, nyears, nom_discount_rate)));



    double npv_energy_nom = npv(CF_energy_sales, nyears, nom_discount_rate);
    double npv_energy_real = npv(CF_energy_sales, nyears, disc_real);

    // LPPA - change form total revenue to PPA revenue 7/19/15 consistent with DHF v4.4
	// fixed price PPA - LPPA independent of salvage value per 7/16/15 meeting
	// Thermal value not included in LPPA calculation but in total revenue.
    /*	double npv_ppa_revenue = npv(CF_energy_value, nyears, nom_discount_rate);
    //	double npv_ppa_revenue = npv(CF_total_revenue, nyears, nom_discount_rate);
    double npv_ppa_revenue = npv(CF_energy_value, nyears, nom_discount_rate);
    //	double npv_ppa_revenue = npv(CF_total_revenue, nyears, nom_discount_rate);
    double lppa_nom = 0;
	if (npv_energy_nom != 0) lppa_nom = npv_ppa_revenue / npv_energy_nom * 100.0;
	double lppa_real = 0;
    if (npv_energy_real != 0) lppa_real = npv_ppa_revenue / npv_energy_real * 100.0;

    // update LCOE calculations 
	double lcoe_nom = lppa_nom;
	double lcoe_real = lppa_real;
     */

    double lcoe_nom = 0.0;
    double lcoe_real = 0.0;

	// from single_owner.xlsm
	cf.at(CF_Annual_Costs, 0) = -issuance_of_equity;
	for (i = 1; i <= nyears; i++)
	{
		cf.at(CF_Annual_Costs, i) =
			cf.at(CF_pbi_total, i)
			+ cf.at(CF_statax, i)
			+ cf.at(CF_fedtax, i)
			- cf.at(CF_debt_payment_interest, i)
			- cf.at(CF_debt_payment_principal, i)
			- cf.at(CF_operating_expenses, i)
			// incentives (cbi and ibi in installed cost and itc in year 1 below
			// TODO - check PBI
			+ cf.at(CF_ptc_fed, i)
			+ cf.at(CF_ptc_sta, i)
			// reserve accounts
			- cf.at(CF_funding_equip1, i)
			- cf.at(CF_funding_equip2, i)
			- cf.at(CF_funding_equip3, i)
			- cf.at(CF_funding_om, i)
			- cf.at(CF_funding_receivables, i)
			- cf.at(CF_funding_debtservice, i)
			+ cf.at(CF_reserve_interest, i)
			- cf.at(CF_disbursement_debtservice, i) // note sign is negative for positive disbursement
			- cf.at(CF_disbursement_om, i) // note sign is negative for positive disbursement
			+ cf.at(CF_net_salvage_value, i)// benefit to cost reduction so that project revenue based on PPA revenue and not total revenue per 7/16/15 meeting
            + cf.at(CF_itc_total, i); // SAM 1038
    }
    // year 1 add total ITC (net benefit) so that project return = project revenue - project cost
    //if (nyears >= 1) cf.at(CF_Annual_Costs, 1) += itc_total;



	double npv_annual_costs = -(npv(CF_Annual_Costs, nyears, nom_discount_rate)
		+ cf.at(CF_Annual_Costs, 0));
	if (npv_energy_nom != 0) lcoe_nom = npv_annual_costs / npv_energy_nom * 100.0;
	if (npv_energy_real != 0) lcoe_real = npv_annual_costs / npv_energy_real * 100.0;

	assign("npv_annual_costs", var_data((ssc_number_t)npv_annual_costs));
	save_cf(CF_Annual_Costs, nyears, "cf_annual_costs");

    ///////////////////////////////////////////////////////////////////////
    //LCOS Calculations
    if (is_assigned("battery_total_cost_lcos") && as_double("battery_total_cost_lcos") != 0) {
        for (int y = 0; y <= nyears; y++) {
            cf_lcos.at(0, y) = cf.at(CF_battery_replacement_cost, y);
            cf_lcos.at(1, y) = cf.at(CF_battery_replacement_cost_schedule, y);
            //cf_lcos.at(2, y) = cf.at(CF_ppa_price, y);
            cf_lcos.at(6, y) = cf.at(CF_om_fixed1_expense, y); //Fixed OM Battery cost
            cf_lcos.at(7, y) = cf.at(CF_om_production1_expense, y); //Produciton OM Battery cost
            cf_lcos.at(8, y) = cf.at(CF_om_capacity1_expense, y); //Capacity OM Battery Cost
        }
        int grid_charging_cost_version = 1;
        ssc_number_t* tod_multipliers;
        size_t* n_tod_multipliers = 0;
        lcos_calc(this, cf_lcos, nyears, nom_discount_rate, inflation_rate, lcoe_real, cost_prefinancing, disc_real, grid_charging_cost_version);
    }
    /////////////////////////////////////////////////////////////////////////////////////////

	// DSCR calculations
	for (i = 0; i <= nyears; i++)
	{
		if (cf.at(CF_debt_payment_total, i) == 0.0) cf.at(CF_pretax_dscr, i) = 0; //cf.at(CF_pretax_dscr, i) = std::numeric_limits<double>::quiet_NaN();
		else cf.at(CF_pretax_dscr, i) = cf.at(CF_cash_for_ds, i) / cf.at(CF_debt_payment_total, i);
	}
	double min_dscr = min_cashflow_value(CF_pretax_dscr, nyears);
	assign("min_dscr", var_data((ssc_number_t)min_dscr));
	save_cf(CF_pretax_dscr, nyears, "cf_pretax_dscr");



	double npv_fed_ptc = npv(CF_ptc_fed,nyears,nom_discount_rate);
	double npv_sta_ptc = npv(CF_ptc_sta,nyears,nom_discount_rate);

//	double effective_tax_rate = state_tax_rate + (1.0-state_tax_rate)*federal_tax_rate;
	npv_fed_ptc /= (1.0 - cf.at(CF_effective_tax_frac, 1));
	npv_sta_ptc /= (1.0 - cf.at(CF_effective_tax_frac, 1));


	double lcoptc_fed_nom=0.0;
	if (npv_energy_nom != 0) lcoptc_fed_nom = npv_fed_ptc / npv_energy_nom * 100.0;
	double lcoptc_fed_real=0.0;
	if (npv_energy_real != 0) lcoptc_fed_real = npv_fed_ptc / npv_energy_real * 100.0;

	double lcoptc_sta_nom=0.0;
	if (npv_energy_nom != 0) lcoptc_sta_nom = npv_sta_ptc / npv_energy_nom * 100.0;
	double lcoptc_sta_real=0.0;
	if (npv_energy_real != 0) lcoptc_sta_real = npv_sta_ptc / npv_energy_real * 100.0;

	assign("lcoptc_fed_nom", var_data((ssc_number_t) lcoptc_fed_nom));
	assign("lcoptc_fed_real", var_data((ssc_number_t) lcoptc_fed_real));
	assign("lcoptc_sta_nom", var_data((ssc_number_t) lcoptc_sta_nom));
	assign("lcoptc_sta_real", var_data((ssc_number_t) lcoptc_sta_real));

	double analysis_period_irr = 0.0;
	analysis_period_irr = cf.at(CF_project_return_aftertax_irr, nyears)/100.0; //fraction for calculations

	double debt_fraction = 0.0;
//	double size_of_equity = cost_installed - ibi_total - cbi_total - size_of_debt;
	double size_of_equity = cost_installed - size_of_debt;
	//cpg same as issuance_of_equity 
	//	if (cost_installed > 0) debt_fraction = size_of_debt / cost_installed;
	if ((size_of_debt + size_of_equity) > 0)
		debt_fraction = size_of_debt / (size_of_debt + size_of_equity);

   


	double wacc = 0.0;
	wacc = (1.0 - debt_fraction)*analysis_period_irr + debt_fraction*term_int_rate*(1.0 - cf.at(CF_effective_tax_frac, 1));

	// percentages
	debt_fraction *= 100.0;
	wacc *= 100.0;
//	effective_tax_rate *= 100.0;
	analysis_period_irr *= 100.0;


	assign("debt_fraction", var_data((ssc_number_t) debt_fraction ));
	assign("wacc", var_data( (ssc_number_t) wacc));
	assign("effective_tax_rate", var_data((ssc_number_t)(cf.at(CF_effective_tax_frac, 1)*100.0)));
	assign("analysis_period_irr", var_data( (ssc_number_t) analysis_period_irr));

    // community solar
    flip_target_percent = analysis_period_irr;
    flip_target_year = nyears;
    assign("flip_target_year", var_data((ssc_number_t)flip_target_year));
    assign("flip_target_irr", var_data((ssc_number_t)flip_target_percent));

    // Paul 1/27/15 - update for ppa specified and IRR year requested
    if (ppa_mode == 1) flip_year = flip_target_year;

    double actual_flip_irr = std::numeric_limits<double>::quiet_NaN();
    if (flip_year > -1)
    {
        actual_flip_irr = cf.at(CF_project_return_aftertax_irr, flip_target_year);
        assign("flip_actual_year", var_data((ssc_number_t)flip_year));
    }
    else
    {
        assign("flip_actual_year", var_data((ssc_number_t)actual_flip_irr));
    }
    assign("flip_actual_irr", var_data((ssc_number_t)actual_flip_irr));

	//assign("npv_ppa_revenue", var_data( (ssc_number_t) npv_ppa_revenue));
	assign("npv_energy_nom", var_data( (ssc_number_t) npv_energy_nom));
	assign("npv_energy_real", var_data( (ssc_number_t) npv_energy_real));

		assign( "cf_length", var_data( (ssc_number_t) nyears+1 ));

		assign( "salvage_value", var_data((ssc_number_t)salvage_value));

		assign( "prop_tax_assessed_value", var_data((ssc_number_t)( assessed_frac * cost_prefinancing )));

		assign("adjusted_installed_cost", var_data((ssc_number_t)(cost_installed - cbi_total - ibi_total)));
		assign("cost_installed", var_data((ssc_number_t)cost_installed));

		assign( "cost_prefinancing", var_data((ssc_number_t) cost_prefinancing ) );
		//assign( "cost_prefinancingperwatt", var_data((ssc_number_t)( cost_prefinancing / nameplate / 1000.0 ) ));
        assign("community_solar_upfront_cost", var_data((ssc_number_t)cs_upfront_cost));
        assign("community_solar_upfront_revenue", var_data((ssc_number_t)cs_upfront_revenue));

		assign( "nominal_discount_rate", var_data((ssc_number_t)nom_discount_rate ) );


		assign( "depr_fedbas_macrs_5", var_data((ssc_number_t) depr_fedbas_macrs_5 ) );
		assign( "depr_fedbas_macrs_15", var_data((ssc_number_t) depr_fedbas_macrs_15 ) );
		assign( "depr_fedbas_sl_5", var_data((ssc_number_t) depr_fedbas_sl_5 ) );
		assign( "depr_fedbas_sl_15", var_data((ssc_number_t) depr_fedbas_sl_15 ) );
		assign( "depr_fedbas_sl_20", var_data((ssc_number_t) depr_fedbas_sl_20 ) );
		assign( "depr_fedbas_sl_39", var_data((ssc_number_t) depr_fedbas_sl_39 ) );
		assign( "depr_fedbas_custom", var_data((ssc_number_t) depr_fedbas_custom ) );
		assign( "depr_fedbas_total", var_data((ssc_number_t) depr_fedbas_total ) );


		assign("cost_financing", var_data((ssc_number_t) cost_financing));
		assign("cost_debt_upfront", var_data((ssc_number_t) cost_debt_upfront));


		assign( "size_of_equity", var_data((ssc_number_t) size_of_equity) );
		assign( "cost_installedperwatt", var_data((ssc_number_t)( cost_installed / nameplate / 1000.0 ) ));

		// metric costs
		//advanced_financing_cost adv(this);
		//adv.compute_cost(cost_installed, size_of_equity, size_of_debt, cbi_total, ibi_total);



 		assign( "itc_fed_qual_macrs_5", var_data((ssc_number_t) itc_fed_qual_macrs_5 ) );
		assign( "itc_fed_qual_macrs_15", var_data((ssc_number_t) itc_fed_qual_macrs_15 ) );
		assign( "itc_fed_qual_sl_5", var_data((ssc_number_t) itc_fed_qual_sl_5 ) );
		assign( "itc_fed_qual_sl_15", var_data((ssc_number_t) itc_fed_qual_sl_15 ) );
		assign( "itc_fed_qual_sl_20", var_data((ssc_number_t) itc_fed_qual_sl_20 ) );
		assign( "itc_fed_qual_sl_39", var_data((ssc_number_t) itc_fed_qual_sl_39 ) );
		assign( "itc_fed_qual_custom", var_data((ssc_number_t) itc_fed_qual_custom ) );

		assign( "itc_disallow_fed_percent_macrs_5", var_data((ssc_number_t) itc_disallow_fed_percent_macrs_5 ) );
		assign( "itc_disallow_fed_percent_macrs_15", var_data((ssc_number_t) itc_disallow_fed_percent_macrs_15 ) );
		assign( "itc_disallow_fed_percent_sl_5", var_data((ssc_number_t) itc_disallow_fed_percent_sl_5 ) );
		assign( "itc_disallow_fed_percent_sl_15", var_data((ssc_number_t) itc_disallow_fed_percent_sl_15 ) );
		assign( "itc_disallow_fed_percent_sl_20", var_data((ssc_number_t) itc_disallow_fed_percent_sl_20 ) );
		assign( "itc_disallow_fed_percent_sl_39", var_data((ssc_number_t) itc_disallow_fed_percent_sl_39 ) );
		assign( "itc_disallow_fed_percent_custom", var_data((ssc_number_t) itc_disallow_fed_percent_custom ) );

		assign( "itc_disallow_fed_fixed_macrs_5", var_data((ssc_number_t) itc_disallow_fed_fixed_macrs_5 ) );
		assign( "itc_disallow_fed_fixed_macrs_15", var_data((ssc_number_t) itc_disallow_fed_fixed_macrs_15 ) );
		assign( "itc_disallow_fed_fixed_sl_5", var_data((ssc_number_t) itc_disallow_fed_fixed_sl_5 ) );
		assign( "itc_disallow_fed_fixed_sl_15", var_data((ssc_number_t) itc_disallow_fed_fixed_sl_15 ) );
		assign( "itc_disallow_fed_fixed_sl_20", var_data((ssc_number_t) itc_disallow_fed_fixed_sl_20 ) );
		assign( "itc_disallow_fed_fixed_sl_39", var_data((ssc_number_t) itc_disallow_fed_fixed_sl_39 ) );
		assign( "itc_disallow_fed_fixed_custom", var_data((ssc_number_t) itc_disallow_fed_fixed_custom ) );

		assign( "itc_fed_qual_total", var_data((ssc_number_t) itc_fed_qual_total ) );
		assign( "itc_fed_percent_total", var_data((ssc_number_t) itc_fed_per ) );
		assign( "itc_fed_fixed_total", var_data((ssc_number_t) itc_fed_amount ) );
	
	

		// output variable and cashflow line item assignments

		assign("issuance_of_equity", var_data((ssc_number_t) issuance_of_equity));
		assign("purchase_of_property", var_data((ssc_number_t) purchase_of_property));
		assign("cash_for_debt_service", var_data((ssc_number_t) cash_for_debt_service));
		assign("pv_cafds", var_data((ssc_number_t) pv_cafds));
		assign("size_of_debt", var_data((ssc_number_t) size_of_debt));
		
		//assign("ppa_price", var_data((ssc_number_t) ppa));
		assign("target_return_flip_year", var_data((ssc_number_t) flip_year));

		assign("ibi_total_fed", var_data((ssc_number_t) (ibi_fed_amount+ibi_fed_per)));
		assign("ibi_total_sta", var_data((ssc_number_t) (ibi_sta_amount+ibi_sta_per)));
		assign("ibi_total_oth", var_data((ssc_number_t) (ibi_oth_amount+ibi_oth_per)));
		assign("ibi_total_uti", var_data((ssc_number_t) (ibi_uti_amount+ibi_uti_per)));
		assign("ibi_total", var_data((ssc_number_t) ibi_total));
	assign("ibi_fedtax_total", var_data((ssc_number_t) ibi_fedtax_total));
	assign("ibi_statax_total", var_data((ssc_number_t) ibi_statax_total));
		assign("cbi_total", var_data((ssc_number_t) cbi_total));
	assign("cbi_fedtax_total", var_data((ssc_number_t) cbi_fedtax_total));
	assign("cbi_statax_total", var_data((ssc_number_t) cbi_statax_total));
		assign("cbi_total_fed", var_data((ssc_number_t) cbi_fed_amount));
		assign("cbi_total_sta", var_data((ssc_number_t) cbi_sta_amount));
		assign("cbi_total_oth", var_data((ssc_number_t) cbi_oth_amount));
		assign("cbi_total_uti", var_data((ssc_number_t) cbi_uti_amount));

        // SAM 1038
        double itc_fed_total = 0.0;
        double itc_sta_total = 0.0;
        double itc_total = 0.0;

        for (size_t k = 0; k <= nyears; k++) {
            itc_fed_total += cf.at(CF_itc_fed, k);
            itc_sta_total += cf.at(CF_itc_sta, k);
            itc_total += cf.at(CF_itc_total, k);
        }

        assign("itc_total_fed", var_data((ssc_number_t) itc_fed_total));
		assign("itc_total_sta", var_data((ssc_number_t) itc_sta_total));
		assign("itc_total", var_data((ssc_number_t) itc_total));

//		assign("first_year_energy_net", var_data((ssc_number_t) cf.at(CF_energy_net,1)));

		assign("lcoe_nom", var_data((ssc_number_t)lcoe_nom));
		assign("lcoe_real", var_data((ssc_number_t)lcoe_real));
		/* No PPA for community solar, keep here to restore later
        assign("lppa_nom", var_data((ssc_number_t)lppa_nom));
		assign("lppa_real", var_data((ssc_number_t)lppa_real));
		assign("ppa_price", var_data((ssc_number_t)ppa));
		assign("ppa_escalation", var_data((ssc_number_t) (ppa_escalation *100.0) ));
		assign("ppa", var_data((ssc_number_t) ppa));
        */

		assign("issuance_of_equity", var_data((ssc_number_t) issuance_of_equity));
		

		assign("project_return_aftertax_irr", var_data((ssc_number_t)  (irr(CF_project_return_aftertax,nyears)*100.0)));
		assign("project_return_aftertax_npv", var_data((ssc_number_t)  (npv(CF_project_return_aftertax,nyears,nom_discount_rate) +  cf.at(CF_project_return_aftertax,0)) ));


		// cash flow line items
		save_cf(CF_federal_tax_frac, nyears, "cf_federal_tax_frac");
		save_cf(CF_state_tax_frac, nyears, "cf_state_tax_frac");
		save_cf(CF_effective_tax_frac, nyears, "cf_effective_tax_frac");

		
		save_cf( CF_statax_taxable_incentives, nyears, "cf_statax_taxable_incentives" );
		save_cf( CF_statax_income_with_incentives, nyears, "cf_statax_income_with_incentives" );
		save_cf( CF_statax, nyears, "cf_statax" );
		save_cf( CF_fedtax_taxable_incentives, nyears, "cf_fedtax_taxable_incentives" );
		save_cf( CF_fedtax_income_with_incentives, nyears, "cf_fedtax_income_with_incentives" );
		save_cf( CF_fedtax, nyears, "cf_fedtax" );

		save_cf( CF_stadepr_macrs_5, nyears, "cf_stadepr_macrs_5" );
		save_cf( CF_stadepr_macrs_15, nyears, "cf_stadepr_macrs_15" );
		save_cf( CF_stadepr_sl_5, nyears, "cf_stadepr_sl_5" );
		save_cf( CF_stadepr_sl_15, nyears, "cf_stadepr_sl_15" );
		save_cf( CF_stadepr_sl_20, nyears, "cf_stadepr_sl_20" );
		save_cf( CF_stadepr_sl_39, nyears, "cf_stadepr_sl_39" );
		save_cf( CF_stadepr_custom, nyears, "cf_stadepr_custom" );
		save_cf( CF_stadepr_me1, nyears, "cf_stadepr_me1" );
		save_cf( CF_stadepr_me2, nyears, "cf_stadepr_me2" );
		save_cf( CF_stadepr_me3, nyears, "cf_stadepr_me3" );
		save_cf( CF_stadepr_total, nyears, "cf_stadepr_total" );
		save_cf( CF_statax_income_prior_incentives, nyears, "cf_statax_income_prior_incentives" );

		save_cf( CF_feddepr_macrs_5, nyears, "cf_feddepr_macrs_5" );
		save_cf( CF_feddepr_macrs_15, nyears, "cf_feddepr_macrs_15" );
		save_cf( CF_feddepr_sl_5, nyears, "cf_feddepr_sl_5" );
		save_cf( CF_feddepr_sl_15, nyears, "cf_feddepr_sl_15" );
		save_cf( CF_feddepr_sl_20, nyears, "cf_feddepr_sl_20" );
		save_cf( CF_feddepr_sl_39, nyears, "cf_feddepr_sl_39" );
		save_cf( CF_feddepr_custom, nyears, "cf_feddepr_custom" );
		save_cf( CF_feddepr_me1, nyears, "cf_feddepr_me1" );
		save_cf( CF_feddepr_me2, nyears, "cf_feddepr_me2" );
		save_cf( CF_feddepr_me3, nyears, "cf_feddepr_me3" );
		save_cf( CF_feddepr_total, nyears, "cf_feddepr_total" );
		save_cf( CF_fedtax_income_prior_incentives, nyears, "cf_fedtax_income_prior_incentives" );

	save_cf( CF_pbi_fed, nyears, "cf_pbi_total_fed");
	save_cf( CF_pbi_sta, nyears, "cf_pbi_total_sta");
	save_cf( CF_pbi_oth, nyears, "cf_pbi_total_oth");
	save_cf( CF_pbi_uti, nyears, "cf_pbi_total_uti");
	save_cf( CF_pbi_total, nyears, "cf_pbi_total" );
	save_cf( CF_pbi_statax_total, nyears, "cf_pbi_statax_total" );
	save_cf( CF_pbi_fedtax_total, nyears, "cf_pbi_fedtax_total" );

		save_cf( CF_ptc_fed, nyears, "cf_ptc_fed" );
		save_cf( CF_ptc_sta, nyears, "cf_ptc_sta" );

		save_cf( CF_project_return_aftertax_cash, nyears, "cf_project_return_aftertax_cash" );
		save_cf( CF_project_return_aftertax, nyears, "cf_project_return_aftertax" );
		save_cf( CF_project_return_aftertax_irr, nyears, "cf_project_return_aftertax_irr" );
		save_cf( CF_project_return_aftertax_max_irr, nyears, "cf_project_return_aftertax_max_irr" );
		save_cf( CF_project_return_aftertax_npv, nyears, "cf_project_return_aftertax_npv" );
		save_cf( CF_project_return_pretax, nyears, "cf_project_return_pretax" );
		save_cf( CF_project_return_pretax_irr, nyears, "cf_project_return_pretax_irr" );
		save_cf( CF_project_return_pretax_npv, nyears, "cf_project_return_pretax_npv" );

		save_cf( CF_project_financing_activities, nyears, "cf_project_financing_activities" );
		save_cf( CF_pretax_cashflow, nyears, "cf_pretax_cashflow" );

		save_cf( CF_project_dsra, nyears, "cf_project_dsra" );
		save_cf(CF_project_wcra, nyears, "cf_project_wcra");
		save_cf(CF_project_receivablesra, nyears, "cf_project_receivablesra");
		save_cf(CF_project_me1ra, nyears, "cf_project_me1ra");
		save_cf( CF_project_me2ra, nyears, "cf_project_me2ra" );
		save_cf( CF_project_me3ra, nyears, "cf_project_me3ra" );
		save_cf( CF_project_ra, nyears, "cf_project_ra" );
		save_cf( CF_project_me1cs, nyears, "cf_project_me1cs" );
		save_cf( CF_project_me2cs, nyears, "cf_project_me2cs" );
		save_cf( CF_project_me3cs, nyears, "cf_project_me3cs" );
		save_cf( CF_project_mecs, nyears, "cf_project_mecs" );
		save_cf( CF_project_investing_activities, nyears, "cf_project_investing_activities" );

		save_cf( CF_pv_interest_factor, nyears, "cf_pv_interest_factor" );
		save_cf( CF_cash_for_ds, nyears, "cf_cash_for_ds" );
		save_cf( CF_pv_cash_for_ds, nyears, "cf_pv_cash_for_ds" );
		save_cf( CF_debt_size, nyears, "cf_debt_size" );			
		save_cf( CF_project_operating_activities, nyears, "cf_project_operating_activities" );

		save_cf( CF_debt_payment_total, nyears, "cf_debt_payment_total" );
		save_cf( CF_debt_payment_interest, nyears, "cf_debt_payment_interest" );
		save_cf( CF_debt_payment_principal, nyears, "cf_debt_payment_principal" );
		save_cf( CF_debt_balance, nyears, "cf_debt_balance" );


		save_cf(CF_energy_value, nyears, "cf_energy_value");
		save_cf(CF_thermal_value, nyears, "cf_thermal_value");
		save_cf(CF_curtailment_value, nyears, "cf_curtailment_value");
		save_cf(CF_capacity_payment, nyears, "cf_capacity_payment");
		save_cf(CF_energy_curtailed, nyears, "cf_energy_curtailed");
		//save_cf( CF_ppa_price, nyears, "cf_ppa_price" );
		save_cf( CF_om_fixed_expense, nyears, "cf_om_fixed_expense" );
		save_cf( CF_om_production_expense, nyears, "cf_om_production_expense" );
		save_cf( CF_om_capacity_expense, nyears, "cf_om_capacity_expense" );
        if (add_om_num_types > 0) {
            save_cf(CF_om_fixed1_expense, nyears, "cf_om_fixed1_expense");
            save_cf(CF_om_production1_expense, nyears, "cf_om_production1_expense");
            save_cf(CF_om_capacity1_expense, nyears, "cf_om_capacity1_expense");
        }
        if (add_om_num_types > 1) {
            save_cf(CF_om_fixed2_expense, nyears, "cf_om_fixed2_expense");
            save_cf(CF_om_production2_expense, nyears, "cf_om_production2_expense");
            save_cf(CF_om_capacity2_expense, nyears, "cf_om_capacity2_expense");
        }


		save_cf( CF_om_fuel_expense, nyears, "cf_om_fuel_expense" );
		save_cf( CF_om_opt_fuel_1_expense, nyears, "cf_om_opt_fuel_1_expense" );
		save_cf( CF_om_opt_fuel_2_expense, nyears, "cf_om_opt_fuel_2_expense" );
        save_cf(CF_land_lease_expense, nyears, "cf_land_lease_expense");
		save_cf( CF_property_tax_assessed_value, nyears, "cf_property_tax_assessed_value" );
		save_cf( CF_property_tax_expense, nyears, "cf_property_tax_expense" );
		save_cf( CF_insurance_expense, nyears, "cf_insurance_expense" );
        if (as_integer("en_batt") == 1 || as_integer("en_standalone_batt") == 1) {
            save_cf(CF_battery_replacement_cost, nyears, "cf_battery_replacement_cost");
            save_cf(CF_battery_replacement_cost_schedule, nyears, "cf_battery_replacement_cost_schedule");
        }
        if (is_assigned("fuelcell_replacement_option")) {
            save_cf(CF_fuelcell_replacement_cost, nyears, "cf_fuelcell_replacement_cost");
            save_cf(CF_fuelcell_replacement_cost_schedule, nyears, "cf_fuelcell_replacement_cost_schedule");
        }
		save_cf( CF_operating_expenses, nyears, "cf_operating_expenses" );
		save_cf( CF_ebitda, nyears, "cf_ebitda" );
		save_cf( CF_net_salvage_value, nyears, "cf_net_salvage_value" );
		save_cf( CF_total_revenue, nyears, "cf_total_revenue" );

		save_cf( CF_energy_net, nyears, "cf_energy_net" );
		save_cf( CF_energy_sales, nyears, "cf_energy_sales" );
		save_cf( CF_energy_purchases, nyears, "cf_energy_purchases" );
        if (is_assigned("gen_without_battery")) {
            save_cf(CF_energy_without_battery, nyears, "cf_energy_without_battery");
        }

		save_cf( CF_reserve_debtservice, nyears, "cf_reserve_debtservice" );
		save_cf(CF_reserve_om, nyears, "cf_reserve_om");
		save_cf(CF_reserve_receivables, nyears, "cf_reserve_receivables");
		save_cf(CF_reserve_equip1, nyears, "cf_reserve_equip1");
		save_cf( CF_reserve_equip2, nyears, "cf_reserve_equip2" );
		save_cf( CF_reserve_equip3, nyears, "cf_reserve_equip3" );

		save_cf( CF_funding_debtservice, nyears, "cf_funding_debtservice" );
		save_cf(CF_funding_om, nyears, "cf_funding_om");
		save_cf(CF_funding_receivables, nyears, "cf_funding_receivables");
		save_cf(CF_funding_equip1, nyears, "cf_funding_equip1");
		save_cf( CF_funding_equip2, nyears, "cf_funding_equip2" );
		save_cf( CF_funding_equip3, nyears, "cf_funding_equip3" );

		save_cf( CF_disbursement_debtservice, nyears, "cf_disbursement_debtservice" );
		save_cf(CF_disbursement_om, nyears, "cf_disbursement_om");
		save_cf(CF_disbursement_receivables, nyears, "cf_disbursement_receivables");
		save_cf(CF_disbursement_equip1, nyears, "cf_disbursement_equip1");
		save_cf( CF_disbursement_equip2, nyears, "cf_disbursement_equip2" );
		save_cf( CF_disbursement_equip3, nyears, "cf_disbursement_equip3" );

		save_cf( CF_reserve_total, nyears, "cf_reserve_total" );
		save_cf( CF_reserve_interest, nyears, "cf_reserve_interest" );

		save_cf(CF_Recapitalization, nyears, "cf_recapitalization");

        // community solar cashflow outputs
        save_cf(CF_subscriber1_share_fraction, nyears, "cf_subscriber1_share_fraction");
        save_cf(CF_subscriber2_share_fraction, nyears, "cf_subscriber2_share_fraction");
        save_cf(CF_subscriber3_share_fraction, nyears, "cf_subscriber3_share_fraction");
        save_cf(CF_subscriber4_share_fraction, nyears, "cf_subscriber4_share_fraction");
        save_cf(CF_unsubscribed_share_fraction, nyears, "cf_unsubscribed_share_fraction");
        
        save_cf(CF_subscriber1_bill_credit_rate, nyears, "cf_subscriber1_bill_credit_rate");
        save_cf(CF_subscriber2_bill_credit_rate, nyears, "cf_subscriber2_bill_credit_rate");
        save_cf(CF_subscriber3_bill_credit_rate, nyears, "cf_subscriber3_bill_credit_rate");
        save_cf(CF_subscriber4_bill_credit_rate, nyears, "cf_subscriber4_bill_credit_rate");

        save_cf(CF_recurring_fixed, nyears, "cf_recurring_fixed");
        save_cf(CF_recurring_capacity, nyears, "cf_recurring_capacity");
        save_cf(CF_recurring_generation, nyears, "cf_recurring_generation");

        save_cf(CF_subscriber1_generation_payment, nyears, "cf_subscriber1_generation_payment");
        save_cf(CF_subscriber2_generation_payment, nyears, "cf_subscriber2_generation_payment");
        save_cf(CF_subscriber3_generation_payment, nyears, "cf_subscriber3_generation_payment");
        save_cf(CF_subscriber4_generation_payment, nyears, "cf_subscriber4_generation_payment");

        save_cf(CF_subscriber1_share_of_generation, nyears, "cf_subscriber1_share_of_generation");
        save_cf(CF_subscriber2_share_of_generation, nyears, "cf_subscriber2_share_of_generation");
        save_cf(CF_subscriber3_share_of_generation, nyears, "cf_subscriber3_share_of_generation");
        save_cf(CF_subscriber4_share_of_generation, nyears, "cf_subscriber4_share_of_generation");
        save_cf(CF_unsubscribed_share_of_generation, nyears, "cf_unsubscribed_share_of_generation");

        save_cf(CF_subscriber1_revenue_generation, nyears, "cf_subscriber1_revenue_generation");
        save_cf(CF_subscriber2_revenue_generation, nyears, "cf_subscriber2_revenue_generation");
        save_cf(CF_subscriber3_revenue_generation, nyears, "cf_subscriber3_revenue_generation");
        save_cf(CF_subscriber4_revenue_generation, nyears, "cf_subscriber4_revenue_generation");
        save_cf(CF_unsubscribed_revenue_generation, nyears, "cf_unsubscribed_revenue_generation");

        save_cf(CF_subscriber1_revenue_upfront, nyears, "cf_subscriber1_revenue_upfront");
        save_cf(CF_subscriber2_revenue_upfront, nyears, "cf_subscriber2_revenue_upfront");
        save_cf(CF_subscriber3_revenue_upfront, nyears, "cf_subscriber3_revenue_upfront");
        save_cf(CF_subscriber4_revenue_upfront, nyears, "cf_subscriber4_revenue_upfront");

        save_cf(CF_subscriber1_revenue_annual_payment, nyears, "cf_subscriber1_revenue_annual_payment");
        save_cf(CF_subscriber2_revenue_annual_payment, nyears, "cf_subscriber2_revenue_annual_payment");
        save_cf(CF_subscriber3_revenue_annual_payment, nyears, "cf_subscriber3_revenue_annual_payment");
        save_cf(CF_subscriber4_revenue_annual_payment, nyears, "cf_subscriber4_revenue_annual_payment");

        save_cf(CF_subscriber1_bill_credit_amount, nyears, "cf_subscriber1_bill_credit_amount");
        save_cf(CF_subscriber2_bill_credit_amount, nyears, "cf_subscriber2_bill_credit_amount");
        save_cf(CF_subscriber3_bill_credit_amount, nyears, "cf_subscriber3_bill_credit_amount");
        save_cf(CF_subscriber4_bill_credit_amount, nyears, "cf_subscriber4_bill_credit_amount");

        save_cf(CF_community_solar_subscriber1_revenue, nyears, "cf_community_solar_subscriber1_revenue");
        save_cf(CF_community_solar_subscriber2_revenue, nyears, "cf_community_solar_subscriber2_revenue");
        save_cf(CF_community_solar_subscriber3_revenue, nyears, "cf_community_solar_subscriber3_revenue");
        save_cf(CF_community_solar_subscriber4_revenue, nyears, "cf_community_solar_subscriber4_revenue");
        save_cf(CF_community_solar_unsubscribed_revenue, nyears, "cf_community_solar_unsubscribed_revenue");

        save_cf(CF_subscriber1_cost_of_participation, nyears, "cf_subscriber1_cost_of_participation");
        save_cf(CF_subscriber2_cost_of_participation, nyears, "cf_subscriber2_cost_of_participation");
        save_cf(CF_subscriber3_cost_of_participation, nyears, "cf_subscriber3_cost_of_participation");
        save_cf(CF_subscriber4_cost_of_participation, nyears, "cf_subscriber4_cost_of_participation");

        save_cf(CF_subscriber1_net_benefit, nyears, "cf_subscriber1_net_benefit");
        save_cf(CF_subscriber2_net_benefit, nyears, "cf_subscriber2_net_benefit");
        save_cf(CF_subscriber3_net_benefit, nyears, "cf_subscriber3_net_benefit");
        save_cf(CF_subscriber4_net_benefit, nyears, "cf_subscriber4_net_benefit");

        save_cf(CF_subscriber1_net_benefit_cumulative, nyears, "cf_subscriber1_net_benefit_cumulative");
        save_cf(CF_subscriber2_net_benefit_cumulative, nyears, "cf_subscriber2_net_benefit_cumulative");
        save_cf(CF_subscriber3_net_benefit_cumulative, nyears, "cf_subscriber3_net_benefit_cumulative");
        save_cf(CF_subscriber4_net_benefit_cumulative, nyears, "cf_subscriber4_net_benefit_cumulative");

        save_cf(CF_community_solar_upfront, nyears, "cf_community_solar_upfront");
        save_cf(CF_community_solar_upfront_per_capacity, nyears, "cf_community_solar_upfront_per_capacity");
        save_cf(CF_community_solar_recurring_fixed, nyears, "cf_community_solar_recurring_fixed");
        save_cf(CF_community_solar_recurring_capacity, nyears, "cf_community_solar_recurring_capacity");
        save_cf(CF_community_solar_recurring_generation, nyears, "cf_community_solar_recurring_generation");

        // community solar metrics
        assign("subscriber1_npv", var_data((ssc_number_t)(npv(CF_subscriber1_net_benefit, nyears, nom_discount_rate) + cf.at(CF_subscriber1_net_benefit, 0))));
        assign("subscriber2_npv", var_data((ssc_number_t)(npv(CF_subscriber2_net_benefit, nyears, nom_discount_rate) + cf.at(CF_subscriber2_net_benefit, 0))));
        assign("subscriber3_npv", var_data((ssc_number_t)(npv(CF_subscriber3_net_benefit, nyears, nom_discount_rate) + cf.at(CF_subscriber3_net_benefit, 0))));
        assign("subscriber4_npv", var_data((ssc_number_t)(npv(CF_subscriber4_net_benefit, nyears, nom_discount_rate) + cf.at(CF_subscriber4_net_benefit, 0))));

		// dispatch
        /* Community solar not available with storage, need to address price signal dispatch with no PPA if we enable with storage
        std::vector<double> ppa_cf;
		for (i = 0; i <= nyears; i++)
		{
			ppa_cf.push_back(cf.at(CF_ppa_price, i));
		}
		//m_disp_calcs.compute_outputs(ppa_cf);
        */

        // Intermediate tax credit/depreciation variables
        assign("pre_depr_alloc_basis", var_data((ssc_number_t)pre_depr_alloc_basis));
        assign("pre_itc_qual_basis", var_data((ssc_number_t)pre_itc_qual_basis));

		// State ITC/depreciation table
		assign("depr_stabas_percent_macrs_5", var_data((ssc_number_t)  (depr_stabas_macrs_5_frac*100.0)));
		assign( "depr_alloc_macrs_5", var_data((ssc_number_t) depr_alloc_macrs_5 ) );
		double depr_stabas_ibi_reduc_macrs_5 = depr_stabas_macrs_5_frac * depr_sta_reduction_ibi;
		double depr_stabas_cbi_reduc_macrs_5 = depr_stabas_macrs_5_frac * depr_sta_reduction_cbi;
		assign( "depr_stabas_ibi_reduc_macrs_5", var_data((ssc_number_t) depr_stabas_ibi_reduc_macrs_5 ) );
		assign( "depr_stabas_cbi_reduc_macrs_5", var_data((ssc_number_t) depr_stabas_cbi_reduc_macrs_5 ) );
 		assign( "depr_stabas_prior_itc_macrs_5", var_data((ssc_number_t) ( depr_alloc_macrs_5 - depr_stabas_ibi_reduc_macrs_5 - depr_stabas_cbi_reduc_macrs_5)) );
 		assign( "itc_sta_qual_macrs_5", var_data((ssc_number_t) itc_sta_qual_macrs_5 ) );
		double depr_stabas_percent_qual_macrs_5 = (itc_sta_qual_total > 0)? 100.0 * itc_sta_qual_macrs_5 /  itc_sta_qual_total:0.0;
 		assign( "depr_stabas_percent_qual_macrs_5", var_data((ssc_number_t) depr_stabas_percent_qual_macrs_5) );
 		assign( "depr_stabas_percent_amount_macrs_5", var_data((ssc_number_t) (depr_stabas_percent_qual_macrs_5/100.0 * itc_sta_per)) );
		assign( "itc_disallow_sta_percent_macrs_5", var_data((ssc_number_t) itc_disallow_sta_percent_macrs_5 ) );
 		assign( "depr_stabas_fixed_amount_macrs_5", var_data((ssc_number_t) (depr_stabas_percent_qual_macrs_5/100.0 * itc_sta_amount)) );
		assign( "itc_disallow_sta_fixed_macrs_5", var_data((ssc_number_t) itc_disallow_sta_fixed_macrs_5 ) );
		double depr_stabas_itc_sta_reduction_macrs_5 = itc_sta_percent_deprbas_sta * itc_disallow_sta_percent_macrs_5 + itc_sta_amount_deprbas_sta * itc_disallow_sta_fixed_macrs_5;
		double depr_stabas_itc_fed_reduction_macrs_5 = itc_fed_percent_deprbas_sta * itc_disallow_fed_percent_macrs_5 + itc_fed_amount_deprbas_sta * itc_disallow_fed_fixed_macrs_5;
		assign( "depr_stabas_itc_sta_reduction_macrs_5", var_data((ssc_number_t) depr_stabas_itc_sta_reduction_macrs_5 ) );
		assign( "depr_stabas_itc_fed_reduction_macrs_5", var_data((ssc_number_t) depr_stabas_itc_fed_reduction_macrs_5 ) );
		assign( "depr_stabas_after_itc_macrs_5", var_data((ssc_number_t) (depr_stabas_macrs_5 + depr_stabas_macrs_5_bonus) ) );
		assign( "depr_stabas_first_year_bonus_macrs_5", var_data((ssc_number_t) depr_stabas_macrs_5_bonus ) );
		assign( "depr_stabas_macrs_5", var_data((ssc_number_t) depr_stabas_macrs_5 ) );

		assign("depr_stabas_percent_macrs_15", var_data((ssc_number_t)  (depr_stabas_macrs_15_frac*100.0)));
		assign( "depr_alloc_macrs_15", var_data((ssc_number_t) depr_alloc_macrs_15 ) );
		double depr_stabas_ibi_reduc_macrs_15 = depr_stabas_macrs_15_frac * depr_sta_reduction_ibi;
		double depr_stabas_cbi_reduc_macrs_15 = depr_stabas_macrs_15_frac * depr_sta_reduction_cbi;
		assign( "depr_stabas_ibi_reduc_macrs_15", var_data((ssc_number_t) depr_stabas_ibi_reduc_macrs_15 ) );
		assign( "depr_stabas_cbi_reduc_macrs_15", var_data((ssc_number_t) depr_stabas_cbi_reduc_macrs_15 ) );
 		assign( "depr_stabas_prior_itc_macrs_15", var_data((ssc_number_t) ( depr_alloc_macrs_15 - depr_stabas_ibi_reduc_macrs_15 - depr_stabas_cbi_reduc_macrs_15)) );
 		assign( "itc_sta_qual_macrs_15", var_data((ssc_number_t) itc_sta_qual_macrs_15 ) );
		double depr_stabas_percent_qual_macrs_15 = (itc_sta_qual_total > 0)? 100.0 * itc_sta_qual_macrs_15 /  itc_sta_qual_total:0.0;
 		assign( "depr_stabas_percent_qual_macrs_15", var_data((ssc_number_t) depr_stabas_percent_qual_macrs_15) );
 		assign( "depr_stabas_percent_amount_macrs_15", var_data((ssc_number_t) (depr_stabas_percent_qual_macrs_15/100.0 * itc_sta_per)) );
		assign( "itc_disallow_sta_percent_macrs_15", var_data((ssc_number_t) itc_disallow_sta_percent_macrs_15 ) );
 		assign( "depr_stabas_fixed_amount_macrs_15", var_data((ssc_number_t) (depr_stabas_percent_qual_macrs_15/100.0 * itc_sta_amount)) );
		assign( "itc_disallow_sta_fixed_macrs_15", var_data((ssc_number_t) itc_disallow_sta_fixed_macrs_15 ) );
		double depr_stabas_itc_sta_reduction_macrs_15 = itc_sta_percent_deprbas_sta * itc_disallow_sta_percent_macrs_15 + itc_sta_amount_deprbas_sta * itc_disallow_sta_fixed_macrs_15;
		double depr_stabas_itc_fed_reduction_macrs_15 = itc_fed_percent_deprbas_sta * itc_disallow_fed_percent_macrs_15 + itc_fed_amount_deprbas_sta * itc_disallow_fed_fixed_macrs_15;
		assign( "depr_stabas_itc_sta_reduction_macrs_15", var_data((ssc_number_t) depr_stabas_itc_sta_reduction_macrs_15 ) );
		assign( "depr_stabas_itc_fed_reduction_macrs_15", var_data((ssc_number_t) depr_stabas_itc_fed_reduction_macrs_15 ) );
		assign( "depr_stabas_after_itc_macrs_15", var_data((ssc_number_t) (depr_stabas_macrs_15 + depr_stabas_macrs_15_bonus) ) );
		assign( "depr_stabas_first_year_bonus_macrs_15", var_data((ssc_number_t) depr_stabas_macrs_15_bonus ) );
		assign( "depr_stabas_macrs_15", var_data((ssc_number_t) depr_stabas_macrs_15 ) );

		assign("depr_stabas_percent_sl_5", var_data((ssc_number_t)  (depr_stabas_sl_5_frac*100.0)));
		assign( "depr_alloc_sl_5", var_data((ssc_number_t) depr_alloc_sl_5 ) );
		double depr_stabas_ibi_reduc_sl_5 = depr_stabas_sl_5_frac * depr_sta_reduction_ibi;
		double depr_stabas_cbi_reduc_sl_5 = depr_stabas_sl_5_frac * depr_sta_reduction_cbi;
		assign( "depr_stabas_ibi_reduc_sl_5", var_data((ssc_number_t) depr_stabas_ibi_reduc_sl_5 ) );
		assign( "depr_stabas_cbi_reduc_sl_5", var_data((ssc_number_t) depr_stabas_cbi_reduc_sl_5 ) );
 		assign( "depr_stabas_prior_itc_sl_5", var_data((ssc_number_t) ( depr_alloc_sl_5 - depr_stabas_ibi_reduc_sl_5 - depr_stabas_cbi_reduc_sl_5)) );
 		assign( "itc_sta_qual_sl_5", var_data((ssc_number_t) itc_sta_qual_sl_5 ) );
		double depr_stabas_percent_qual_sl_5 = (itc_sta_qual_total > 0)? 100.0 * itc_sta_qual_sl_5 /  itc_sta_qual_total:0.0;
 		assign( "depr_stabas_percent_qual_sl_5", var_data((ssc_number_t) depr_stabas_percent_qual_sl_5) );
 		assign( "depr_stabas_percent_amount_sl_5", var_data((ssc_number_t) (depr_stabas_percent_qual_sl_5/100.0 * itc_sta_per)) );
		assign( "itc_disallow_sta_percent_sl_5", var_data((ssc_number_t) itc_disallow_sta_percent_sl_5 ) );
 		assign( "depr_stabas_fixed_amount_sl_5", var_data((ssc_number_t) (depr_stabas_percent_qual_sl_5/100.0 * itc_sta_amount)) );
		assign( "itc_disallow_sta_fixed_sl_5", var_data((ssc_number_t) itc_disallow_sta_fixed_sl_5 ) );
		double depr_stabas_itc_sta_reduction_sl_5 = itc_sta_percent_deprbas_sta * itc_disallow_sta_percent_sl_5 + itc_sta_amount_deprbas_sta * itc_disallow_sta_fixed_sl_5;
		double depr_stabas_itc_fed_reduction_sl_5 = itc_fed_percent_deprbas_sta * itc_disallow_fed_percent_sl_5 + itc_fed_amount_deprbas_sta * itc_disallow_fed_fixed_sl_5;
		assign( "depr_stabas_itc_sta_reduction_sl_5", var_data((ssc_number_t) depr_stabas_itc_sta_reduction_sl_5 ) );
		assign( "depr_stabas_itc_fed_reduction_sl_5", var_data((ssc_number_t) depr_stabas_itc_fed_reduction_sl_5 ) );
		assign( "depr_stabas_after_itc_sl_5", var_data((ssc_number_t) (depr_stabas_sl_5 + depr_stabas_sl_5_bonus) ) );
		assign( "depr_stabas_first_year_bonus_sl_5", var_data((ssc_number_t) depr_stabas_sl_5_bonus ) );
		assign( "depr_stabas_sl_5", var_data((ssc_number_t) depr_stabas_sl_5 ) );

		assign("depr_stabas_percent_sl_15", var_data((ssc_number_t)  (depr_stabas_sl_15_frac*100.0)));
		assign( "depr_alloc_sl_15", var_data((ssc_number_t) depr_alloc_sl_15 ) );
		double depr_stabas_ibi_reduc_sl_15 = depr_stabas_sl_15_frac * depr_sta_reduction_ibi;
		double depr_stabas_cbi_reduc_sl_15 = depr_stabas_sl_15_frac * depr_sta_reduction_cbi;
		assign( "depr_stabas_ibi_reduc_sl_15", var_data((ssc_number_t) depr_stabas_ibi_reduc_sl_15 ) );
		assign( "depr_stabas_cbi_reduc_sl_15", var_data((ssc_number_t) depr_stabas_cbi_reduc_sl_15 ) );
 		assign( "depr_stabas_prior_itc_sl_15", var_data((ssc_number_t) ( depr_alloc_sl_15 - depr_stabas_ibi_reduc_sl_15 - depr_stabas_cbi_reduc_sl_15)) );
 		assign( "itc_sta_qual_sl_15", var_data((ssc_number_t) itc_sta_qual_sl_15 ) );
		double depr_stabas_percent_qual_sl_15 = (itc_sta_qual_total > 0)? 100.0 * itc_sta_qual_sl_15 /  itc_sta_qual_total:0.0;
 		assign( "depr_stabas_percent_qual_sl_15", var_data((ssc_number_t) depr_stabas_percent_qual_sl_15) );
 		assign( "depr_stabas_percent_amount_sl_15", var_data((ssc_number_t) (depr_stabas_percent_qual_sl_15/100.0 * itc_sta_per)) );
		assign( "itc_disallow_sta_percent_sl_15", var_data((ssc_number_t) itc_disallow_sta_percent_sl_15 ) );
 		assign( "depr_stabas_fixed_amount_sl_15", var_data((ssc_number_t) (depr_stabas_percent_qual_sl_15/100.0 * itc_sta_amount)) );
		assign( "itc_disallow_sta_fixed_sl_15", var_data((ssc_number_t) itc_disallow_sta_fixed_sl_15 ) );
		double depr_stabas_itc_sta_reduction_sl_15 = itc_sta_percent_deprbas_sta * itc_disallow_sta_percent_sl_15 + itc_sta_amount_deprbas_sta * itc_disallow_sta_fixed_sl_15;
		double depr_stabas_itc_fed_reduction_sl_15 = itc_fed_percent_deprbas_sta * itc_disallow_fed_percent_sl_15 + itc_fed_amount_deprbas_sta * itc_disallow_fed_fixed_sl_15;
		assign( "depr_stabas_itc_sta_reduction_sl_15", var_data((ssc_number_t) depr_stabas_itc_sta_reduction_sl_15 ) );
		assign( "depr_stabas_itc_fed_reduction_sl_15", var_data((ssc_number_t) depr_stabas_itc_fed_reduction_sl_15 ) );
		assign( "depr_stabas_after_itc_sl_15", var_data((ssc_number_t) (depr_stabas_sl_15 + depr_stabas_sl_15_bonus) ) );
		assign( "depr_stabas_first_year_bonus_sl_15", var_data((ssc_number_t) depr_stabas_sl_15_bonus ) );
		assign( "depr_stabas_sl_15", var_data((ssc_number_t) depr_stabas_sl_15 ) );

		assign("depr_stabas_percent_sl_20", var_data((ssc_number_t)  (depr_stabas_sl_20_frac*100.0)));
		assign( "depr_alloc_sl_20", var_data((ssc_number_t) depr_alloc_sl_20 ) );
		double depr_stabas_ibi_reduc_sl_20 = depr_stabas_sl_20_frac * depr_sta_reduction_ibi;
		double depr_stabas_cbi_reduc_sl_20 = depr_stabas_sl_20_frac * depr_sta_reduction_cbi;
		assign( "depr_stabas_ibi_reduc_sl_20", var_data((ssc_number_t) depr_stabas_ibi_reduc_sl_20 ) );
		assign( "depr_stabas_cbi_reduc_sl_20", var_data((ssc_number_t) depr_stabas_cbi_reduc_sl_20 ) );
 		assign( "depr_stabas_prior_itc_sl_20", var_data((ssc_number_t) ( depr_alloc_sl_20 - depr_stabas_ibi_reduc_sl_20 - depr_stabas_cbi_reduc_sl_20)) );
 		assign( "itc_sta_qual_sl_20", var_data((ssc_number_t) itc_sta_qual_sl_20 ) );
		double depr_stabas_percent_qual_sl_20 = (itc_sta_qual_total > 0)? 100.0 * itc_sta_qual_sl_20 /  itc_sta_qual_total:0.0;
 		assign( "depr_stabas_percent_qual_sl_20", var_data((ssc_number_t) depr_stabas_percent_qual_sl_20) );
 		assign( "depr_stabas_percent_amount_sl_20", var_data((ssc_number_t) (depr_stabas_percent_qual_sl_20/100.0 * itc_sta_per)) );
		assign( "itc_disallow_sta_percent_sl_20", var_data((ssc_number_t) itc_disallow_sta_percent_sl_20 ) );
 		assign( "depr_stabas_fixed_amount_sl_20", var_data((ssc_number_t) (depr_stabas_percent_qual_sl_20/100.0 * itc_sta_amount)) );
		assign( "itc_disallow_sta_fixed_sl_20", var_data((ssc_number_t) itc_disallow_sta_fixed_sl_20 ) );
		double depr_stabas_itc_sta_reduction_sl_20 = itc_sta_percent_deprbas_sta * itc_disallow_sta_percent_sl_20 + itc_sta_amount_deprbas_sta * itc_disallow_sta_fixed_sl_20;
		double depr_stabas_itc_fed_reduction_sl_20 = itc_fed_percent_deprbas_sta * itc_disallow_fed_percent_sl_20 + itc_fed_amount_deprbas_sta * itc_disallow_fed_fixed_sl_20;
		assign( "depr_stabas_itc_sta_reduction_sl_20", var_data((ssc_number_t) depr_stabas_itc_sta_reduction_sl_20 ) );
		assign( "depr_stabas_itc_fed_reduction_sl_20", var_data((ssc_number_t) depr_stabas_itc_fed_reduction_sl_20 ) );
		assign( "depr_stabas_after_itc_sl_20", var_data((ssc_number_t) (depr_stabas_sl_20 + depr_stabas_sl_20_bonus) ) );
		assign( "depr_stabas_first_year_bonus_sl_20", var_data((ssc_number_t) depr_stabas_sl_20_bonus ) );
		assign( "depr_stabas_sl_20", var_data((ssc_number_t) depr_stabas_sl_20 ) );

		assign("depr_stabas_percent_sl_39", var_data((ssc_number_t)  (depr_stabas_sl_39_frac*100.0)));
		assign( "depr_alloc_sl_39", var_data((ssc_number_t) depr_alloc_sl_39 ) );
		double depr_stabas_ibi_reduc_sl_39 = depr_stabas_sl_39_frac * depr_sta_reduction_ibi;
		double depr_stabas_cbi_reduc_sl_39 = depr_stabas_sl_39_frac * depr_sta_reduction_cbi;
		assign( "depr_stabas_ibi_reduc_sl_39", var_data((ssc_number_t) depr_stabas_ibi_reduc_sl_39 ) );
		assign( "depr_stabas_cbi_reduc_sl_39", var_data((ssc_number_t) depr_stabas_cbi_reduc_sl_39 ) );
 		assign( "depr_stabas_prior_itc_sl_39", var_data((ssc_number_t) ( depr_alloc_sl_39 - depr_stabas_ibi_reduc_sl_39 - depr_stabas_cbi_reduc_sl_39)) );
 		assign( "itc_sta_qual_sl_39", var_data((ssc_number_t) itc_sta_qual_sl_39 ) );
		double depr_stabas_percent_qual_sl_39 = (itc_sta_qual_total > 0)? 100.0 * itc_sta_qual_sl_39 /  itc_sta_qual_total:0.0;
 		assign( "depr_stabas_percent_qual_sl_39", var_data((ssc_number_t) depr_stabas_percent_qual_sl_39) );
 		assign( "depr_stabas_percent_amount_sl_39", var_data((ssc_number_t) (depr_stabas_percent_qual_sl_39/100.0 * itc_sta_per)) );
		assign( "itc_disallow_sta_percent_sl_39", var_data((ssc_number_t) itc_disallow_sta_percent_sl_39 ) );
 		assign( "depr_stabas_fixed_amount_sl_39", var_data((ssc_number_t) (depr_stabas_percent_qual_sl_39/100.0 * itc_sta_amount)) );
		assign( "itc_disallow_sta_fixed_sl_39", var_data((ssc_number_t) itc_disallow_sta_fixed_sl_39 ) );
		double depr_stabas_itc_sta_reduction_sl_39 = itc_sta_percent_deprbas_sta * itc_disallow_sta_percent_sl_39 + itc_sta_amount_deprbas_sta * itc_disallow_sta_fixed_sl_39;
		double depr_stabas_itc_fed_reduction_sl_39 = itc_fed_percent_deprbas_sta * itc_disallow_fed_percent_sl_39 + itc_fed_amount_deprbas_sta * itc_disallow_fed_fixed_sl_39;
		assign( "depr_stabas_itc_sta_reduction_sl_39", var_data((ssc_number_t) depr_stabas_itc_sta_reduction_sl_39 ) );
		assign( "depr_stabas_itc_fed_reduction_sl_39", var_data((ssc_number_t) depr_stabas_itc_fed_reduction_sl_39 ) );
		assign( "depr_stabas_after_itc_sl_39", var_data((ssc_number_t) (depr_stabas_sl_39 + depr_stabas_sl_39_bonus) ) );
		assign( "depr_stabas_first_year_bonus_sl_39", var_data((ssc_number_t) depr_stabas_sl_39_bonus ) );
		assign( "depr_stabas_sl_39", var_data((ssc_number_t) depr_stabas_sl_39 ) );

		assign("depr_stabas_percent_custom", var_data((ssc_number_t)  (depr_stabas_custom_frac*100.0)));
		assign( "depr_alloc_custom", var_data((ssc_number_t) depr_alloc_custom ) );
		double depr_stabas_ibi_reduc_custom = depr_stabas_custom_frac * depr_sta_reduction_ibi;
		double depr_stabas_cbi_reduc_custom = depr_stabas_custom_frac * depr_sta_reduction_cbi;
		assign( "depr_stabas_ibi_reduc_custom", var_data((ssc_number_t) depr_stabas_ibi_reduc_custom ) );
		assign( "depr_stabas_cbi_reduc_custom", var_data((ssc_number_t) depr_stabas_cbi_reduc_custom ) );
 		assign( "depr_stabas_prior_itc_custom", var_data((ssc_number_t) ( depr_alloc_custom - depr_stabas_ibi_reduc_custom - depr_stabas_cbi_reduc_custom)) );
 		assign( "itc_sta_qual_custom", var_data((ssc_number_t) itc_sta_qual_custom ) );
		double depr_stabas_percent_qual_custom = (itc_sta_qual_total > 0)? 100.0 * itc_sta_qual_custom /  itc_sta_qual_total:0.0;
 		assign( "depr_stabas_percent_qual_custom", var_data((ssc_number_t) depr_stabas_percent_qual_custom) );
 		assign( "depr_stabas_percent_amount_custom", var_data((ssc_number_t) (depr_stabas_percent_qual_custom/100.0 * itc_sta_per)) );
		assign( "itc_disallow_sta_percent_custom", var_data((ssc_number_t) itc_disallow_sta_percent_custom ) );
 		assign( "depr_stabas_fixed_amount_custom", var_data((ssc_number_t) (depr_stabas_percent_qual_custom/100.0 * itc_sta_amount)) );
		assign( "itc_disallow_sta_fixed_custom", var_data((ssc_number_t) itc_disallow_sta_fixed_custom ) );
		double depr_stabas_itc_sta_reduction_custom = itc_sta_percent_deprbas_sta * itc_disallow_sta_percent_custom + itc_sta_amount_deprbas_sta * itc_disallow_sta_fixed_custom;
		double depr_stabas_itc_fed_reduction_custom = itc_fed_percent_deprbas_sta * itc_disallow_fed_percent_custom + itc_fed_amount_deprbas_sta * itc_disallow_fed_fixed_custom;
		assign( "depr_stabas_itc_sta_reduction_custom", var_data((ssc_number_t) depr_stabas_itc_sta_reduction_custom ) );
		assign( "depr_stabas_itc_fed_reduction_custom", var_data((ssc_number_t) depr_stabas_itc_fed_reduction_custom ) );
		assign( "depr_stabas_after_itc_custom", var_data((ssc_number_t) (depr_stabas_custom + depr_stabas_custom_bonus) ) );
		assign( "depr_stabas_first_year_bonus_custom", var_data((ssc_number_t) depr_stabas_custom_bonus ) );
		assign( "depr_stabas_custom", var_data((ssc_number_t) depr_stabas_custom ) );

		assign("depr_stabas_percent_total", var_data((ssc_number_t)  (100.0*(depr_stabas_macrs_5_frac+depr_stabas_macrs_15_frac+depr_stabas_sl_5_frac+depr_stabas_sl_15_frac+depr_stabas_sl_20_frac+depr_stabas_sl_39_frac+depr_stabas_custom_frac))));
		assign( "depr_alloc_total", var_data((ssc_number_t) depr_alloc_total ) );
		assign( "depr_stabas_ibi_reduc_total", var_data((ssc_number_t) depr_sta_reduction_ibi ) );
		assign( "depr_stabas_cbi_reduc_total", var_data((ssc_number_t) depr_sta_reduction_cbi ) );
 		assign( "depr_stabas_prior_itc_total", var_data((ssc_number_t) ( depr_alloc_total - depr_sta_reduction_ibi - depr_sta_reduction_cbi)) );
 		assign( "itc_sta_qual_total", var_data((ssc_number_t) itc_sta_qual_total ) );
 		assign( "depr_stabas_percent_qual_total", var_data((ssc_number_t) 100.0) );
 		assign( "depr_stabas_percent_amount_total", var_data((ssc_number_t) itc_sta_per) );
		assign( "itc_disallow_sta_percent_total", var_data((ssc_number_t) (itc_disallow_sta_percent_macrs_5 + itc_disallow_sta_percent_macrs_15 + itc_disallow_sta_percent_sl_5 + itc_disallow_sta_percent_sl_15 + itc_disallow_sta_percent_sl_20 + itc_disallow_sta_percent_sl_39 + itc_disallow_sta_percent_custom) ) );
 		assign( "depr_stabas_fixed_amount_total", var_data((ssc_number_t) itc_sta_amount) );
		assign( "itc_disallow_sta_fixed_total", var_data((ssc_number_t) (itc_disallow_sta_fixed_macrs_5 + itc_disallow_sta_fixed_macrs_15 + itc_disallow_sta_fixed_sl_5 + itc_disallow_sta_fixed_sl_15 + itc_disallow_sta_fixed_sl_20 + itc_disallow_sta_fixed_sl_39 + itc_disallow_sta_fixed_custom) ) );
		double depr_stabas_itc_sta_reduction_total = depr_stabas_itc_sta_reduction_macrs_5 + depr_stabas_itc_sta_reduction_macrs_15 + depr_stabas_itc_sta_reduction_sl_5 + depr_stabas_itc_sta_reduction_sl_15 + depr_stabas_itc_sta_reduction_sl_20 + depr_stabas_itc_sta_reduction_sl_39 + depr_stabas_itc_sta_reduction_custom;
		assign( "depr_stabas_itc_sta_reduction_total", var_data((ssc_number_t) depr_stabas_itc_sta_reduction_total ) );
		double depr_stabas_itc_fed_reduction_total = depr_stabas_itc_fed_reduction_macrs_5 + depr_stabas_itc_fed_reduction_macrs_15 + depr_stabas_itc_fed_reduction_sl_5 + depr_stabas_itc_fed_reduction_sl_15 + depr_stabas_itc_fed_reduction_sl_20 + depr_stabas_itc_fed_reduction_sl_39 + depr_stabas_itc_fed_reduction_custom;
		assign( "depr_stabas_itc_fed_reduction_total", var_data((ssc_number_t) depr_stabas_itc_fed_reduction_total ) );
		double depr_stabas_first_year_bonus_total = depr_stabas_macrs_5_bonus+depr_stabas_macrs_15_bonus+depr_stabas_sl_5_bonus+depr_stabas_sl_15_bonus+depr_stabas_sl_20_bonus+depr_stabas_sl_39_bonus+depr_stabas_custom_bonus;
		assign( "depr_stabas_after_itc_total", var_data((ssc_number_t) (depr_stabas_total + depr_stabas_first_year_bonus_total) ) );
		assign( "depr_stabas_first_year_bonus_total", var_data((ssc_number_t) depr_stabas_first_year_bonus_total ) );
		assign( "depr_stabas_total", var_data((ssc_number_t) depr_stabas_total ) );

	
		assign( "itc_sta_percent_total", var_data((ssc_number_t) itc_sta_per ) );
		assign( "itc_sta_fixed_total", var_data((ssc_number_t) itc_sta_amount ) );



		// Federal ITC/depreciation table
		assign("depr_fedbas_percent_macrs_5", var_data((ssc_number_t)  (depr_fedbas_macrs_5_frac*100.0)));
		assign( "depr_alloc_macrs_5", var_data((ssc_number_t) depr_alloc_macrs_5 ) );
		double depr_fedbas_ibi_reduc_macrs_5 = depr_fedbas_macrs_5_frac * depr_fed_reduction_ibi;
		double depr_fedbas_cbi_reduc_macrs_5 = depr_fedbas_macrs_5_frac * depr_fed_reduction_cbi;
		assign( "depr_fedbas_ibi_reduc_macrs_5", var_data((ssc_number_t) depr_fedbas_ibi_reduc_macrs_5 ) );
		assign( "depr_fedbas_cbi_reduc_macrs_5", var_data((ssc_number_t) depr_fedbas_cbi_reduc_macrs_5 ) );
 		assign( "depr_fedbas_prior_itc_macrs_5", var_data((ssc_number_t) ( depr_alloc_macrs_5 - depr_fedbas_ibi_reduc_macrs_5 - depr_fedbas_cbi_reduc_macrs_5)) );
 		assign( "itc_fed_qual_macrs_5", var_data((ssc_number_t) itc_fed_qual_macrs_5 ) );
		double depr_fedbas_percent_qual_macrs_5 = (itc_fed_qual_total > 0)? 100.0 * itc_fed_qual_macrs_5 /  itc_fed_qual_total:0.0;
 		assign( "depr_fedbas_percent_qual_macrs_5", var_data((ssc_number_t) depr_fedbas_percent_qual_macrs_5) );
 		assign( "depr_fedbas_percent_amount_macrs_5", var_data((ssc_number_t) (depr_fedbas_percent_qual_macrs_5/100.0 * itc_fed_per)) );
		assign( "itc_disallow_fed_percent_macrs_5", var_data((ssc_number_t) itc_disallow_fed_percent_macrs_5 ) );
 		assign( "depr_fedbas_fixed_amount_macrs_5", var_data((ssc_number_t) (depr_fedbas_percent_qual_macrs_5/100.0 * itc_fed_amount)) );
		assign( "itc_disallow_fed_fixed_macrs_5", var_data((ssc_number_t) itc_disallow_fed_fixed_macrs_5 ) );
		double depr_fedbas_itc_sta_reduction_macrs_5 = itc_sta_percent_deprbas_fed * itc_disallow_sta_percent_macrs_5 + itc_sta_amount_deprbas_fed * itc_disallow_sta_fixed_macrs_5;
		double depr_fedbas_itc_fed_reduction_macrs_5 = itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_macrs_5 + itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_macrs_5;
		assign( "depr_fedbas_itc_sta_reduction_macrs_5", var_data((ssc_number_t) depr_fedbas_itc_sta_reduction_macrs_5 ) );
		assign( "depr_fedbas_itc_fed_reduction_macrs_5", var_data((ssc_number_t) depr_fedbas_itc_fed_reduction_macrs_5 ) );
		assign( "depr_fedbas_after_itc_macrs_5", var_data((ssc_number_t) (depr_fedbas_macrs_5 + depr_fedbas_macrs_5_bonus) ) );
		assign( "depr_fedbas_first_year_bonus_macrs_5", var_data((ssc_number_t) depr_fedbas_macrs_5_bonus ) );
		assign( "depr_fedbas_macrs_5", var_data((ssc_number_t) depr_fedbas_macrs_5 ) );

		assign("depr_fedbas_percent_macrs_15", var_data((ssc_number_t)  (depr_fedbas_macrs_15_frac*100.0)));
		assign( "depr_alloc_macrs_15", var_data((ssc_number_t) depr_alloc_macrs_15 ) );
		double depr_fedbas_ibi_reduc_macrs_15 = depr_fedbas_macrs_15_frac * depr_fed_reduction_ibi;
		double depr_fedbas_cbi_reduc_macrs_15 = depr_fedbas_macrs_15_frac * depr_fed_reduction_cbi;
		assign( "depr_fedbas_ibi_reduc_macrs_15", var_data((ssc_number_t) depr_fedbas_ibi_reduc_macrs_15 ) );
		assign( "depr_fedbas_cbi_reduc_macrs_15", var_data((ssc_number_t) depr_fedbas_cbi_reduc_macrs_15 ) );
 		assign( "depr_fedbas_prior_itc_macrs_15", var_data((ssc_number_t) ( depr_alloc_macrs_15 - depr_fedbas_ibi_reduc_macrs_15 - depr_fedbas_cbi_reduc_macrs_15)) );
 		assign( "itc_fed_qual_macrs_15", var_data((ssc_number_t) itc_fed_qual_macrs_15 ) );
		double depr_fedbas_percent_qual_macrs_15 = (itc_fed_qual_total > 0)? 100.0 * itc_fed_qual_macrs_15 /  itc_fed_qual_total:0.0;
 		assign( "depr_fedbas_percent_qual_macrs_15", var_data((ssc_number_t) depr_fedbas_percent_qual_macrs_15) );
 		assign( "depr_fedbas_percent_amount_macrs_15", var_data((ssc_number_t) (depr_fedbas_percent_qual_macrs_15/100.0 * itc_fed_per)) );
		assign( "itc_disallow_fed_percent_macrs_15", var_data((ssc_number_t) itc_disallow_fed_percent_macrs_15 ) );
 		assign( "depr_fedbas_fixed_amount_macrs_15", var_data((ssc_number_t) (depr_fedbas_percent_qual_macrs_15/100.0 * itc_fed_amount)) );
		assign( "itc_disallow_fed_fixed_macrs_15", var_data((ssc_number_t) itc_disallow_fed_fixed_macrs_15 ) );
		double depr_fedbas_itc_sta_reduction_macrs_15 = itc_sta_percent_deprbas_fed * itc_disallow_sta_percent_macrs_15 + itc_sta_amount_deprbas_fed * itc_disallow_sta_fixed_macrs_15;
		double depr_fedbas_itc_fed_reduction_macrs_15 = itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_macrs_15 + itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_macrs_15;
		assign( "depr_fedbas_itc_sta_reduction_macrs_15", var_data((ssc_number_t) depr_fedbas_itc_sta_reduction_macrs_15 ) );
		assign( "depr_fedbas_itc_fed_reduction_macrs_15", var_data((ssc_number_t) depr_fedbas_itc_fed_reduction_macrs_15 ) );
		assign( "depr_fedbas_after_itc_macrs_15", var_data((ssc_number_t) (depr_fedbas_macrs_15 + depr_fedbas_macrs_15_bonus) ) );
		assign( "depr_fedbas_first_year_bonus_macrs_15", var_data((ssc_number_t) depr_fedbas_macrs_15_bonus ) );
		assign( "depr_fedbas_macrs_15", var_data((ssc_number_t) depr_fedbas_macrs_15 ) );

		assign("depr_fedbas_percent_sl_5", var_data((ssc_number_t)  (depr_fedbas_sl_5_frac*100.0)));
		assign( "depr_alloc_sl_5", var_data((ssc_number_t) depr_alloc_sl_5 ) );
		double depr_fedbas_ibi_reduc_sl_5 = depr_fedbas_sl_5_frac * depr_fed_reduction_ibi;
		double depr_fedbas_cbi_reduc_sl_5 = depr_fedbas_sl_5_frac * depr_fed_reduction_cbi;
		assign( "depr_fedbas_ibi_reduc_sl_5", var_data((ssc_number_t) depr_fedbas_ibi_reduc_sl_5 ) );
		assign( "depr_fedbas_cbi_reduc_sl_5", var_data((ssc_number_t) depr_fedbas_cbi_reduc_sl_5 ) );
 		assign( "depr_fedbas_prior_itc_sl_5", var_data((ssc_number_t) ( depr_alloc_sl_5 - depr_fedbas_ibi_reduc_sl_5 - depr_fedbas_cbi_reduc_sl_5)) );
 		assign( "itc_fed_qual_sl_5", var_data((ssc_number_t) itc_fed_qual_sl_5 ) );
		double depr_fedbas_percent_qual_sl_5 = (itc_fed_qual_total > 0)? 100.0 * itc_fed_qual_sl_5 /  itc_fed_qual_total:0.0;
 		assign( "depr_fedbas_percent_qual_sl_5", var_data((ssc_number_t) depr_fedbas_percent_qual_sl_5) );
 		assign( "depr_fedbas_percent_amount_sl_5", var_data((ssc_number_t) (depr_fedbas_percent_qual_sl_5/100.0 * itc_fed_per)) );
		assign( "itc_disallow_fed_percent_sl_5", var_data((ssc_number_t) itc_disallow_fed_percent_sl_5 ) );
 		assign( "depr_fedbas_fixed_amount_sl_5", var_data((ssc_number_t) (depr_fedbas_percent_qual_sl_5/100.0 * itc_fed_amount)) );
		assign( "itc_disallow_fed_fixed_sl_5", var_data((ssc_number_t) itc_disallow_fed_fixed_sl_5 ) );
		double depr_fedbas_itc_sta_reduction_sl_5 = itc_sta_percent_deprbas_fed * itc_disallow_sta_percent_sl_5 + itc_sta_amount_deprbas_fed * itc_disallow_sta_fixed_sl_5;
		double depr_fedbas_itc_fed_reduction_sl_5 = itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_sl_5 + itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_sl_5;
		assign( "depr_fedbas_itc_sta_reduction_sl_5", var_data((ssc_number_t) depr_fedbas_itc_sta_reduction_sl_5 ) );
		assign( "depr_fedbas_itc_fed_reduction_sl_5", var_data((ssc_number_t) depr_fedbas_itc_fed_reduction_sl_5 ) );
		assign( "depr_fedbas_after_itc_sl_5", var_data((ssc_number_t) (depr_fedbas_sl_5 + depr_fedbas_sl_5_bonus) ) );
		assign( "depr_fedbas_first_year_bonus_sl_5", var_data((ssc_number_t) depr_fedbas_sl_5_bonus ) );
		assign( "depr_fedbas_sl_5", var_data((ssc_number_t) depr_fedbas_sl_5 ) );

		assign("depr_fedbas_percent_sl_15", var_data((ssc_number_t)  (depr_fedbas_sl_15_frac*100.0)));
		assign( "depr_alloc_sl_15", var_data((ssc_number_t) depr_alloc_sl_15 ) );
		double depr_fedbas_ibi_reduc_sl_15 = depr_fedbas_sl_15_frac * depr_fed_reduction_ibi;
		double depr_fedbas_cbi_reduc_sl_15 = depr_fedbas_sl_15_frac * depr_fed_reduction_cbi;
		assign( "depr_fedbas_ibi_reduc_sl_15", var_data((ssc_number_t) depr_fedbas_ibi_reduc_sl_15 ) );
		assign( "depr_fedbas_cbi_reduc_sl_15", var_data((ssc_number_t) depr_fedbas_cbi_reduc_sl_15 ) );
 		assign( "depr_fedbas_prior_itc_sl_15", var_data((ssc_number_t) ( depr_alloc_sl_15 - depr_fedbas_ibi_reduc_sl_15 - depr_fedbas_cbi_reduc_sl_15)) );
 		assign( "itc_fed_qual_sl_15", var_data((ssc_number_t) itc_fed_qual_sl_15 ) );
		double depr_fedbas_percent_qual_sl_15 = (itc_fed_qual_total > 0)? 100.0 * itc_fed_qual_sl_15 /  itc_fed_qual_total:0.0;
 		assign( "depr_fedbas_percent_qual_sl_15", var_data((ssc_number_t) depr_fedbas_percent_qual_sl_15) );
 		assign( "depr_fedbas_percent_amount_sl_15", var_data((ssc_number_t) (depr_fedbas_percent_qual_sl_15/100.0 * itc_fed_per)) );
		assign( "itc_disallow_fed_percent_sl_15", var_data((ssc_number_t) itc_disallow_fed_percent_sl_15 ) );
 		assign( "depr_fedbas_fixed_amount_sl_15", var_data((ssc_number_t) (depr_fedbas_percent_qual_sl_15/100.0 * itc_fed_amount)) );
		assign( "itc_disallow_fed_fixed_sl_15", var_data((ssc_number_t) itc_disallow_fed_fixed_sl_15 ) );
		double depr_fedbas_itc_sta_reduction_sl_15 = itc_sta_percent_deprbas_fed * itc_disallow_sta_percent_sl_15 + itc_sta_amount_deprbas_fed * itc_disallow_sta_fixed_sl_15;
		double depr_fedbas_itc_fed_reduction_sl_15 = itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_sl_15 + itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_sl_15;
		assign( "depr_fedbas_itc_sta_reduction_sl_15", var_data((ssc_number_t) depr_fedbas_itc_sta_reduction_sl_15 ) );
		assign( "depr_fedbas_itc_fed_reduction_sl_15", var_data((ssc_number_t) depr_fedbas_itc_fed_reduction_sl_15 ) );
		assign( "depr_fedbas_after_itc_sl_15", var_data((ssc_number_t) (depr_fedbas_sl_15 + depr_fedbas_sl_15_bonus) ) );
		assign( "depr_fedbas_first_year_bonus_sl_15", var_data((ssc_number_t) depr_fedbas_sl_15_bonus ) );
		assign( "depr_fedbas_sl_15", var_data((ssc_number_t) depr_fedbas_sl_15 ) );

		assign("depr_fedbas_percent_sl_20", var_data((ssc_number_t)  (depr_fedbas_sl_20_frac*100.0)));
		assign( "depr_alloc_sl_20", var_data((ssc_number_t) depr_alloc_sl_20 ) );
		double depr_fedbas_ibi_reduc_sl_20 = depr_fedbas_sl_20_frac * depr_fed_reduction_ibi;
		double depr_fedbas_cbi_reduc_sl_20 = depr_fedbas_sl_20_frac * depr_fed_reduction_cbi;
		assign( "depr_fedbas_ibi_reduc_sl_20", var_data((ssc_number_t) depr_fedbas_ibi_reduc_sl_20 ) );
		assign( "depr_fedbas_cbi_reduc_sl_20", var_data((ssc_number_t) depr_fedbas_cbi_reduc_sl_20 ) );
 		assign( "depr_fedbas_prior_itc_sl_20", var_data((ssc_number_t) ( depr_alloc_sl_20 - depr_fedbas_ibi_reduc_sl_20 - depr_fedbas_cbi_reduc_sl_20)) );
 		assign( "itc_fed_qual_sl_20", var_data((ssc_number_t) itc_fed_qual_sl_20 ) );
		double depr_fedbas_percent_qual_sl_20 = (itc_fed_qual_total > 0)? 100.0 * itc_fed_qual_sl_20 /  itc_fed_qual_total:0.0;
 		assign( "depr_fedbas_percent_qual_sl_20", var_data((ssc_number_t) depr_fedbas_percent_qual_sl_20) );
 		assign( "depr_fedbas_percent_amount_sl_20", var_data((ssc_number_t) (depr_fedbas_percent_qual_sl_20/100.0 * itc_fed_per)) );
		assign( "itc_disallow_fed_percent_sl_20", var_data((ssc_number_t) itc_disallow_fed_percent_sl_20 ) );
 		assign( "depr_fedbas_fixed_amount_sl_20", var_data((ssc_number_t) (depr_fedbas_percent_qual_sl_20/100.0 * itc_fed_amount)) );
		assign( "itc_disallow_fed_fixed_sl_20", var_data((ssc_number_t) itc_disallow_fed_fixed_sl_20 ) );
		double depr_fedbas_itc_sta_reduction_sl_20 = itc_sta_percent_deprbas_fed * itc_disallow_sta_percent_sl_20 + itc_sta_amount_deprbas_fed * itc_disallow_sta_fixed_sl_20;
		double depr_fedbas_itc_fed_reduction_sl_20 = itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_sl_20 + itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_sl_20;
		assign( "depr_fedbas_itc_sta_reduction_sl_20", var_data((ssc_number_t) depr_fedbas_itc_sta_reduction_sl_20 ) );
		assign( "depr_fedbas_itc_fed_reduction_sl_20", var_data((ssc_number_t) depr_fedbas_itc_fed_reduction_sl_20 ) );
		assign( "depr_fedbas_after_itc_sl_20", var_data((ssc_number_t) (depr_fedbas_sl_20 + depr_fedbas_sl_20_bonus) ) );
		assign( "depr_fedbas_first_year_bonus_sl_20", var_data((ssc_number_t) depr_fedbas_sl_20_bonus ) );
		assign( "depr_fedbas_sl_20", var_data((ssc_number_t) depr_fedbas_sl_20 ) );

		assign("depr_fedbas_percent_sl_39", var_data((ssc_number_t)  (depr_fedbas_sl_39_frac*100.0)));
		assign( "depr_alloc_sl_39", var_data((ssc_number_t) depr_alloc_sl_39 ) );
		double depr_fedbas_ibi_reduc_sl_39 = depr_fedbas_sl_39_frac * depr_fed_reduction_ibi;
		double depr_fedbas_cbi_reduc_sl_39 = depr_fedbas_sl_39_frac * depr_fed_reduction_cbi;
		assign( "depr_fedbas_ibi_reduc_sl_39", var_data((ssc_number_t) depr_fedbas_ibi_reduc_sl_39 ) );
		assign( "depr_fedbas_cbi_reduc_sl_39", var_data((ssc_number_t) depr_fedbas_cbi_reduc_sl_39 ) );
 		assign( "depr_fedbas_prior_itc_sl_39", var_data((ssc_number_t) ( depr_alloc_sl_39 - depr_fedbas_ibi_reduc_sl_39 - depr_fedbas_cbi_reduc_sl_39)) );
 		assign( "itc_fed_qual_sl_39", var_data((ssc_number_t) itc_fed_qual_sl_39 ) );
		double depr_fedbas_percent_qual_sl_39 = (itc_fed_qual_total > 0)? 100.0 * itc_fed_qual_sl_39 /  itc_fed_qual_total:0.0;
 		assign( "depr_fedbas_percent_qual_sl_39", var_data((ssc_number_t) depr_fedbas_percent_qual_sl_39) );
 		assign( "depr_fedbas_percent_amount_sl_39", var_data((ssc_number_t) (depr_fedbas_percent_qual_sl_39/100.0 * itc_fed_per)) );
		assign( "itc_disallow_fed_percent_sl_39", var_data((ssc_number_t) itc_disallow_fed_percent_sl_39 ) );
 		assign( "depr_fedbas_fixed_amount_sl_39", var_data((ssc_number_t) (depr_fedbas_percent_qual_sl_39/100.0 * itc_fed_amount)) );
		assign( "itc_disallow_fed_fixed_sl_39", var_data((ssc_number_t) itc_disallow_fed_fixed_sl_39 ) );
		double depr_fedbas_itc_sta_reduction_sl_39 = itc_sta_percent_deprbas_fed * itc_disallow_sta_percent_sl_39 + itc_sta_amount_deprbas_fed * itc_disallow_sta_fixed_sl_39;
		double depr_fedbas_itc_fed_reduction_sl_39 = itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_sl_39 + itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_sl_39;
		assign( "depr_fedbas_itc_sta_reduction_sl_39", var_data((ssc_number_t) depr_fedbas_itc_sta_reduction_sl_39 ) );
		assign( "depr_fedbas_itc_fed_reduction_sl_39", var_data((ssc_number_t) depr_fedbas_itc_fed_reduction_sl_39 ) );
		assign( "depr_fedbas_after_itc_sl_39", var_data((ssc_number_t) (depr_fedbas_sl_39 + depr_fedbas_sl_39_bonus) ) );
		assign( "depr_fedbas_first_year_bonus_sl_39", var_data((ssc_number_t) depr_fedbas_sl_39_bonus ) );
		assign( "depr_fedbas_sl_39", var_data((ssc_number_t) depr_fedbas_sl_39 ) );

		assign("depr_fedbas_percent_custom", var_data((ssc_number_t)  (depr_fedbas_custom_frac*100.0)));
		assign( "depr_alloc_custom", var_data((ssc_number_t) depr_alloc_custom ) );
		double depr_fedbas_ibi_reduc_custom = depr_fedbas_custom_frac * depr_fed_reduction_ibi;
		double depr_fedbas_cbi_reduc_custom = depr_fedbas_custom_frac * depr_fed_reduction_cbi;
		assign( "depr_fedbas_ibi_reduc_custom", var_data((ssc_number_t) depr_fedbas_ibi_reduc_custom ) );
		assign( "depr_fedbas_cbi_reduc_custom", var_data((ssc_number_t) depr_fedbas_cbi_reduc_custom ) );
 		assign( "depr_fedbas_prior_itc_custom", var_data((ssc_number_t) ( depr_alloc_custom - depr_fedbas_ibi_reduc_custom - depr_fedbas_cbi_reduc_custom)) );
 		assign( "itc_fed_qual_custom", var_data((ssc_number_t) itc_fed_qual_custom ) );
		double depr_fedbas_percent_qual_custom = (itc_fed_qual_total > 0)? 100.0 * itc_fed_qual_custom /  itc_fed_qual_total:0.0;
 		assign( "depr_fedbas_percent_qual_custom", var_data((ssc_number_t) depr_fedbas_percent_qual_custom) );
 		assign( "depr_fedbas_percent_amount_custom", var_data((ssc_number_t) (depr_fedbas_percent_qual_custom/100.0 * itc_fed_per)) );
		assign( "itc_disallow_fed_percent_custom", var_data((ssc_number_t) itc_disallow_fed_percent_custom ) );
 		assign( "depr_fedbas_fixed_amount_custom", var_data((ssc_number_t) (depr_fedbas_percent_qual_custom/100.0 * itc_fed_amount)) );
		assign( "itc_disallow_fed_fixed_custom", var_data((ssc_number_t) itc_disallow_fed_fixed_custom ) );
		double depr_fedbas_itc_sta_reduction_custom = itc_sta_percent_deprbas_fed * itc_disallow_sta_percent_custom + itc_sta_amount_deprbas_fed * itc_disallow_sta_fixed_custom;
		double depr_fedbas_itc_fed_reduction_custom = itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_custom + itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_custom;
		assign( "depr_fedbas_itc_sta_reduction_custom", var_data((ssc_number_t) depr_fedbas_itc_sta_reduction_custom ) );
		assign( "depr_fedbas_itc_fed_reduction_custom", var_data((ssc_number_t) depr_fedbas_itc_fed_reduction_custom ) );
		assign( "depr_fedbas_after_itc_custom", var_data((ssc_number_t) (depr_fedbas_custom + depr_fedbas_custom_bonus) ) );
		assign( "depr_fedbas_first_year_bonus_custom", var_data((ssc_number_t) depr_fedbas_custom_bonus ) );
		assign( "depr_fedbas_custom", var_data((ssc_number_t) depr_fedbas_custom ) );


		assign("depr_fedbas_percent_total", var_data((ssc_number_t)  (100.0*(depr_fedbas_macrs_5_frac+depr_fedbas_macrs_15_frac+depr_fedbas_sl_5_frac+depr_fedbas_sl_15_frac+depr_fedbas_sl_20_frac+depr_fedbas_sl_39_frac+depr_fedbas_custom_frac))));
		assign( "depr_alloc_total", var_data((ssc_number_t) depr_alloc_total ) );
		assign( "depr_fedbas_ibi_reduc_total", var_data((ssc_number_t) depr_fed_reduction_ibi ) );
		assign( "depr_fedbas_cbi_reduc_total", var_data((ssc_number_t) depr_fed_reduction_cbi ) );
 		assign( "depr_fedbas_prior_itc_total", var_data((ssc_number_t) ( depr_alloc_total - depr_fed_reduction_ibi - depr_fed_reduction_cbi)) );
 		assign( "itc_sta_qual_total", var_data((ssc_number_t) itc_sta_qual_total ) );
 		assign( "depr_fedbas_percent_qual_total", var_data((ssc_number_t) 100.0) );
 		assign( "depr_fedbas_percent_amount_total", var_data((ssc_number_t) itc_fed_per) );
		assign( "itc_disallow_fed_percent_total", var_data((ssc_number_t) (itc_disallow_fed_percent_macrs_5 + itc_disallow_fed_percent_macrs_15 + itc_disallow_fed_percent_sl_5 + itc_disallow_fed_percent_sl_15 + itc_disallow_fed_percent_sl_20 + itc_disallow_fed_percent_sl_39 + itc_disallow_fed_percent_custom) ) );
 		assign( "depr_fedbas_fixed_amount_total", var_data((ssc_number_t) itc_fed_amount) );
		assign( "itc_disallow_fed_fixed_total", var_data((ssc_number_t) (itc_disallow_fed_fixed_macrs_5 + itc_disallow_fed_fixed_macrs_15 + itc_disallow_fed_fixed_sl_5 + itc_disallow_fed_fixed_sl_15 + itc_disallow_fed_fixed_sl_20 + itc_disallow_fed_fixed_sl_39 + itc_disallow_fed_fixed_custom) ) );
		double depr_fedbas_itc_sta_reduction_total = depr_fedbas_itc_sta_reduction_macrs_5 + depr_fedbas_itc_sta_reduction_macrs_15 + depr_fedbas_itc_sta_reduction_sl_5 + depr_fedbas_itc_sta_reduction_sl_15 + depr_fedbas_itc_sta_reduction_sl_20 + depr_fedbas_itc_sta_reduction_sl_39 + depr_fedbas_itc_sta_reduction_custom;
		assign( "depr_fedbas_itc_sta_reduction_total", var_data((ssc_number_t) depr_fedbas_itc_sta_reduction_total ) );
		double depr_fedbas_itc_fed_reduction_total = depr_fedbas_itc_fed_reduction_macrs_5 + depr_fedbas_itc_fed_reduction_macrs_15 + depr_fedbas_itc_fed_reduction_sl_5 + depr_fedbas_itc_fed_reduction_sl_15 + depr_fedbas_itc_fed_reduction_sl_20 + depr_fedbas_itc_fed_reduction_sl_39 + depr_fedbas_itc_fed_reduction_custom;
		assign( "depr_fedbas_itc_fed_reduction_total", var_data((ssc_number_t) depr_fedbas_itc_fed_reduction_total ) );
		double depr_fedbas_first_year_bonus_total = depr_fedbas_macrs_5_bonus+depr_fedbas_macrs_15_bonus+depr_fedbas_sl_5_bonus+depr_fedbas_sl_15_bonus+depr_fedbas_sl_20_bonus+depr_fedbas_sl_39_bonus+depr_fedbas_custom_bonus;
		assign( "depr_fedbas_after_itc_total", var_data((ssc_number_t) (depr_fedbas_total + depr_fedbas_first_year_bonus_total) ) );
		assign( "depr_fedbas_first_year_bonus_total", var_data((ssc_number_t) depr_fedbas_first_year_bonus_total ) );
		assign( "depr_fedbas_total", var_data((ssc_number_t) depr_fedbas_total ) );

		assign( "depr_alloc_none_percent", var_data((ssc_number_t) (depr_alloc_none_frac*100.0) ) );
		assign( "depr_alloc_none", var_data((ssc_number_t) depr_alloc_none ) );
		assign( "depr_alloc_total", var_data((ssc_number_t) depr_alloc_total ) );
		// Project cash flow

	// for cost stacked bars
		//npv(CF_energy_value, nyears, nom_discount_rate)
		// present value of o and m value - note - present value is distributive - sum of pv = pv of sum
		double pvAnnualOandM = npv(CF_om_fixed_expense, nyears, nom_discount_rate);
		double pvFixedOandM = npv(CF_om_capacity_expense, nyears, nom_discount_rate);
		double pvVariableOandM = npv(CF_om_production_expense, nyears, nom_discount_rate);
		double pvFuelOandM = npv(CF_om_fuel_expense, nyears, nom_discount_rate);
		double pvOptFuel1OandM = npv(CF_om_opt_fuel_1_expense, nyears, nom_discount_rate);
		double pvOptFuel2OandM = npv(CF_om_opt_fuel_2_expense, nyears, nom_discount_rate);
	//	double pvWaterOandM = NetPresentValue(sv[svNominalDiscountRate], cf[cfAnnualWaterCost], analysis_period);

		assign( "present_value_oandm",  var_data((ssc_number_t)(pvAnnualOandM + pvFixedOandM + pvVariableOandM + pvFuelOandM))); // + pvWaterOandM);

		assign( "present_value_oandm_nonfuel", var_data((ssc_number_t)(pvAnnualOandM + pvFixedOandM + pvVariableOandM)));
		assign( "present_value_fuel", var_data((ssc_number_t)(pvFuelOandM + pvOptFuel1OandM + pvOptFuel2OandM)));

		// present value of insurance and property tax
		double pvInsurance = npv(CF_insurance_expense, nyears, nom_discount_rate);
		double pvPropertyTax = npv(CF_property_tax_expense, nyears, nom_discount_rate);

		assign( "present_value_insandproptax", var_data((ssc_number_t)(pvInsurance + pvPropertyTax)));

        // check financial metric outputs per SAM issue 551
        ssc_number_t irr_metric_end = irr(CF_project_return_aftertax, nyears) * 100.0;
        ssc_number_t irr_metric_flip_year = actual_flip_irr;
        ssc_number_t npv_metric = npv(CF_project_return_aftertax, nyears, nom_discount_rate) + cf.at(CF_project_return_aftertax, 0);

        check_financial_metrics cfm;
        cfm.check_irr(this, irr_metric_end);
        cfm.check_irr_flip(this, irr_metric_flip_year);
        cfm.check_npv(this, npv_metric);
        cfm.check_debt_percentage(this, debt_fraction);

        // SAM 1038
        save_cf(CF_itc_fed_amount, nyears, "cf_itc_fed_amount");
        save_cf(CF_itc_fed_percent_amount, nyears, "cf_itc_fed_percent_amount");
        save_cf(CF_itc_fed, nyears, "cf_itc_fed");
        save_cf(CF_itc_sta_amount, nyears, "cf_itc_sta_amount");
        save_cf(CF_itc_sta_percent_amount, nyears, "cf_itc_sta_percent_amount");
        save_cf(CF_itc_sta, nyears, "cf_itc_sta");
        save_cf(CF_itc_total, nyears, "cf_itc_total");


	}


	// std lib
	void major_equipment_depreciation( int cf_equipment_expenditure, int cf_depr_sched, int expenditure_year, int analysis_period, int cf_equipment_depreciation )
	{
		// depreciate equipment cost in expenditure_year according to depr_sched schedule subject to cutoff by analysis_period
		if ( (expenditure_year > 0 ) && (expenditure_year <= analysis_period))
		{
			// sign convention from v3 model
			double depreciable_basis = -cf.at(cf_equipment_expenditure, expenditure_year);
			for (int i=expenditure_year; i<=analysis_period; i++)
			{
				cf.at(cf_equipment_depreciation,i) += depreciable_basis * cf.at(cf_depr_sched,i-expenditure_year+1);
			}

		}
	}

	// std lib
	void depreciation_sched_5_year_macrs_half_year( int cf_line, int nyears )
	{
		for (int i=1; i<=nyears; i++)
		{
			double factor = 0.0;
			switch(i)
			{
			case 1: factor = 0.2000; break;
			case 2: factor = 0.3200; break;
			case 3: factor = 0.1920; break;
			case 4: factor = 0.1152; break;
			case 5: factor = 0.1152; break;
			case 6: factor = 0.0576; break;
			default: factor = 0.0; break;
			}
			cf.at(cf_line, i) = factor;
		}
	}
	// std lib
	void depreciation_sched_15_year_macrs_half_year( int cf_line, int nyears )
	{
		for (int i=1; i<=nyears; i++)
		{
			double factor = 0.0;
			switch(i)
			{
			case 1: factor = 0.0500; break;
			case 2: factor = 0.0950; break;
			case 3: factor = 0.0855; break;
			case 4: factor = 0.0770; break;
			case 5: factor = 0.0693; break;
			case 6: factor = 0.0623; break;
			case 7: factor = 0.0590; break;
			case 8: factor = 0.0590; break;
			case 9: factor = 0.0591; break;
			case 10: factor = 0.0590; break;
			case 11: factor = 0.0591; break;
			case 12: factor = 0.0590; break;
			case 13: factor = 0.0591; break;
			case 14: factor = 0.0590; break;
			case 15: factor = 0.0591; break;
			case 16: factor = 0.0295; break;
			default: factor = 0.0; break;
			}
			cf.at(cf_line, i) = factor;
		}
	}
	// std lib
	void depreciation_sched_5_year_straight_line_half_year( int cf_line, int nyears )
	{
		for (int i=1; i<=nyears; i++)
		{
			double factor = 0.0;
			switch(i)
			{
			case 1: factor = 0.1000; break;
			case 2: factor = 0.2000; break;
			case 3: factor = 0.2000; break;
			case 4: factor = 0.2000; break;
			case 5: factor = 0.2000; break;
			case 6: factor = 0.1000; break;
			default: factor = 0.0; break;
			}
			cf.at(cf_line, i) = factor;
		}
	}
	// std lib
	void depreciation_sched_15_year_straight_line_half_year( int cf_line, int nyears )
	{
		for (int i=1; i<=nyears; i++)
		{
			double factor = 0.0;
			switch(i)
			{
			case 1: factor = 0.0333; break;
			case 2: factor = 0.0667; break;
			case 3: factor = 0.0667; break;
			case 4: factor = 0.0667; break;
			case 5: factor = 0.0667; break;
			case 6: factor = 0.0667; break;
			case 7: factor = 0.0667; break;
			case 8: factor = 0.0666; break;
			case 9: factor = 0.0667; break;
			case 10: factor = 0.0666; break;
			case 11: factor = 0.0667; break;
			case 12: factor = 0.0666; break;
			case 13: factor = 0.0667; break;
			case 14: factor = 0.0666; break;
			case 15: factor = 0.0667; break;
			case 16: factor = 0.0333; break;

			default: factor = 0.0; break;
			}
			cf.at(cf_line, i) = factor;
		}
	}
	// std lib
	void depreciation_sched_20_year_straight_line_half_year( int cf_line, int nyears )
	{
		for (int i=1; i<=nyears; i++)
		{
			double factor = 0.0;
			switch(i)
			{
			case 1: factor = 0.0250; break;
			case 2: factor = 0.0500; break;
			case 3: factor = 0.0500; break;
			case 4: factor = 0.0500; break;
			case 5: factor = 0.0500; break;
			case 6: factor = 0.0500; break;
			case 7: factor = 0.0500; break;
			case 8: factor = 0.0500; break;
			case 9: factor = 0.0500; break;
			case 10: factor = 0.0500; break;
			case 11: factor = 0.0500; break;
			case 12: factor = 0.0500; break;
			case 13: factor = 0.0500; break;
			case 14: factor = 0.0500; break;
			case 15: factor = 0.0500; break;
			case 16: factor = 0.0500; break;
			case 17: factor = 0.0500; break;
			case 18: factor = 0.0500; break;
			case 19: factor = 0.0500; break;
			case 20: factor = 0.0500; break;
			case 21: factor = 0.0250; break;
			default: factor = 0.0; break;
			}
			cf.at(cf_line, i) = factor;
		}
	}
	// std lib
	void depreciation_sched_39_year_straight_line_half_year( int cf_line, int nyears )
	{
		for (int i=1; i<=nyears; i++)
		{
			double factor = 0.0;
			double base=2.56410256410256e-2;
			switch(i)
			{
			case 1: factor = 0.5*base; break;
			case 2: factor = base; break;
			case 3: factor = base; break;
			case 4: factor = base; break;
			case 5: factor = base; break;
			case 6: factor = base; break;
			case 7: factor = base; break;
			case 8: factor = base; break;
			case 9: factor = base; break;
			case 10: factor = base; break;
			case 11: factor = base; break;
			case 12: factor = base; break;
			case 13: factor = base; break;
			case 14: factor = base; break;
			case 15: factor = base; break;
			case 16: factor = base; break;
			case 17: factor = base; break;
			case 18: factor = base; break;
			case 19: factor = base; break;
			case 20: factor = base; break;
			case 21: factor = base; break;
			case 22: factor = base; break;
			case 23: factor = base; break;
			case 24: factor = base; break;
			case 25: factor = base; break;
			case 26: factor = base; break;
			case 27: factor = base; break;
			case 28: factor = base; break;
			case 29: factor = base; break;
			case 30: factor = base; break;
			case 31: factor = base; break;
			case 32: factor = base; break;
			case 33: factor = base; break;
			case 34: factor = base; break;
			case 35: factor = base; break;
			case 36: factor = base; break;
			case 37: factor = base; break;
			case 38: factor = base; break;
			case 39: factor = base; break;
			case 40: factor = 0.5*base; break;
			default: factor = 0.0; break;
			}
			cf.at(cf_line, i) = factor;
		}
	}


	void depreciation_sched_custom(int cf_line, int nyears, const std::string &custom)
	{
		// computes custom percentage schedule 100%
		// if customValue is array then annual schedule of percent
		// if customValue is a single value then that value is used up to 100%
		int i;
		size_t count = 0;
		ssc_number_t *parr = as_array(custom, &count);
		for (i = 1; i<=nyears; i++)
		{
			cf.at(cf_line,i) = 0;
		}

		if (count ==1) // single value
		{
			cf.at(cf_line,1) = parr[0]/100.0;
		}
		else // annual schedule
		{// note schedules begin at year 1 (index 0)
			int scheduleDuration = ((int)count > nyears)? nyears : (int)count;
			for (i = 1; i<=scheduleDuration; i++)
			{
				cf.at(cf_line,i) = parr[i-1] / 100.0; // percentage to factor
			}
		}
	}



	// std lib
	void save_cf(int cf_line, int nyears, const std::string &name)
	{
		ssc_number_t *arrp = allocate( name, nyears+1 );
		for (int i=0;i<=nyears;i++)
			arrp[i] = (ssc_number_t)cf.at(cf_line, i);
	}

	void escal_or_annual( int cf_line, int nyears, const std::string &variable, 
			double inflation_rate, double scale, bool as_rate=true, double escal = 0.0)
	{
		size_t count;
		ssc_number_t *arrp = as_array(variable, &count);

		if (as_rate)
		{
			if (count == 1)
			{
				escal = inflation_rate + scale*arrp[0];
				for (int i=0; i < nyears; i++)
					cf.at(cf_line, i+1) = pow( 1+escal, i );
			}
			else
			{
				for (int i=0; i < nyears && i < (int)count; i++)
					cf.at(cf_line, i+1) = 1 + arrp[i]*scale;
			}
		}
		else
		{
			if (count == 1)
			{
				for (int i=0;i<nyears;i++)
					cf.at(cf_line, i+1) = arrp[0]*scale*pow( 1+escal+inflation_rate, i );
			}
			else
			{
				for (int i=0;i<nyears && i<(int)count;i++)
					cf.at(cf_line, i+1) = arrp[i]*scale;
			}
		}
	}

		void compute_production_incentive( int cf_line, int nyears, const std::string &s_val, const std::string &s_term, const std::string &s_escal )
	{
		size_t len = 0;
		ssc_number_t *parr = as_array(s_val, &len);
		int term = as_integer(s_term);
		double escal = as_double(s_escal)/100.0;

		if (len == 1)
		{
			for (int i=1;i<=nyears;i++)
				cf.at(cf_line, i) = (i <= term) ? parr[0] * cf.at(CF_energy_sales,i) * pow(1 + escal, i-1) : 0.0;
		}
		else
		{
			for (int i=1;i<=nyears && i <= (int)len;i++)
				cf.at(cf_line, i) = parr[i-1]*cf.at(CF_energy_sales,i);
		}
	}

		void compute_production_incentive_IRS_2010_37( int cf_line, int nyears, const std::string &s_val, const std::string &s_term, const std::string &s_escal )
	{
		// rounding based on IRS document and emails 2/24/2011
		size_t len = 0;
		ssc_number_t *parr = as_array(s_val, &len);
		int term = as_integer(s_term);
		double escal = as_double(s_escal)/100.0;

		if (len == 1)
		{
			for (int i=1;i<=nyears;i++)
				cf.at(cf_line, i) = (i <= term) ? cf.at(CF_energy_sales,i) / 1000.0 * round_irs(1000.0 * parr[0] * pow(1 + escal, i-1)) : 0.0;
		}
		else
		{
			for (int i=1;i<=nyears && i <= (int)len;i++)
				cf.at(cf_line, i) = parr[i-1]*cf.at(CF_energy_sales,i);
		}
	}

	void single_or_schedule( int cf_line, int nyears, double scale, const std::string &name )
	{
		size_t len = 0;
		ssc_number_t *p = as_array(name, &len);
		for (int i=1;i<=(int)len && i <= nyears;i++)
			cf.at(cf_line, i) = scale*p[i-1];
	}
	
	void single_or_schedule_check_max( int cf_line, int nyears, double scale, const std::string &name, const std::string &maxvar )
	{
		double max = as_double(maxvar);
		size_t len = 0;
		ssc_number_t *p = as_array(name, &len);
		for (int i=1;i<=(int)len && i <= nyears;i++)
			cf.at(cf_line, i) = min( scale*p[i-1], max );
	}

	double npv( int cf_line, int nyears, double rate )
	{		
		//if (rate == -1.0) throw general_error("cannot calculate NPV with discount rate equal to -1.0");
		double rr = 1.0;
		if (rate != -1.0) rr = 1.0/(1.0+rate);
		double result = 0;
		for (int i=nyears;i>0;i--)
			result = rr * result + cf.at(cf_line,i);

		return result*rr;
	}

/* ported from http://code.google.com/p/irr-newtonraphson-calculator/ */
	bool is_valid_iter_bound(double estimated_return_rate)
	{
		return estimated_return_rate != -1 && (estimated_return_rate < std::numeric_limits<int>::max()) && (estimated_return_rate > std::numeric_limits<int>::min());
	}

	double irr_poly_sum(double estimated_return_rate, int cf_line, int count)
	{
		double sum_of_polynomial = 0;
		if (is_valid_iter_bound(estimated_return_rate))
		{
			for (int j = 0; j <= count ; j++)
			{
				double val = (pow((1 + estimated_return_rate), j));
				if (val != 0.0)
					sum_of_polynomial += cf.at(cf_line,j)/val;
				else
					break;
			}
		}
		return sum_of_polynomial;
	}

	double irr_derivative_sum(double estimated_return_rate,int cf_line, int count)
	{
		double sum_of_derivative = 0;
		if (is_valid_iter_bound(estimated_return_rate))
			for (int i = 1; i <= count ; i++)
			{
				sum_of_derivative += cf.at(cf_line,i)*(i)/pow((1 + estimated_return_rate), i+1);
			}
		return sum_of_derivative*-1;
	}

	double irr_scale_factor( int cf_unscaled, int count)
	{
		// scale to max value for better irr convergence
		if (count<1) return 1.0;
		int i=0;
		double max= std::abs(cf.at(cf_unscaled,0));
		for (i=0;i<=count;i++) 
			if (std::abs(cf.at(cf_unscaled,i))> max) max =fabs(cf.at(cf_unscaled,i));
		return (max>0 ? max:1);
	}

	bool is_valid_irr( int cf_line, int count, double residual, double tolerance, int number_of_iterations, int max_iterations, double calculated_irr, double scale_factor )
	{
		double npv_of_irr = npv(cf_line,count,calculated_irr)+cf.at(cf_line,0);
		double npv_of_irr_plus_delta = npv(cf_line,count,calculated_irr+0.001)+cf.at(cf_line,0);
		bool is_valid = ( (number_of_iterations<max_iterations) && (std::abs(residual)<tolerance) && (npv_of_irr>npv_of_irr_plus_delta) && (std::abs(npv_of_irr/scale_factor)<tolerance) );
				//if (!is_valid)
				//{
				//std::stringstream outm;
				//outm <<  "cf_line=" << cf_line << "count=" << count << "residual=" << residual << "number_of_iterations=" << number_of_iterations << "calculated_irr=" << calculated_irr
				//	<< "npv of irr=" << npv_of_irr << "npv of irr plus delta=" << npv_of_irr_plus_delta;
				//log( outm.str() );
				//}
		return is_valid;
	}

	double irr( int cf_line, int count, double initial_guess=-2, double tolerance=1e-6, int max_iterations=100 )
	{
		int number_of_iterations=0;
//		double calculated_irr = 0;
		double calculated_irr = std::numeric_limits<double>::quiet_NaN();
//		double calculated_irr = -999;


		if (count < 1) 
			return calculated_irr;

		// only possible for first value negative
		if ( (cf.at(cf_line,0) <= 0))
		{
			// initial guess from http://zainco.blogspot.com/2008/08/internal-rate-of-return-using-newton.html
			if ((initial_guess < -1) && (count > 1))// second order
			{
				if (cf.at(cf_line,0) !=0) 
				{
					double b = 2.0+ cf.at(cf_line,1)/cf.at(cf_line,0);
					double c = 1.0+cf.at(cf_line,1)/cf.at(cf_line,0)+cf.at(cf_line,2)/cf.at(cf_line,0);
					initial_guess = -0.5*b - 0.5*sqrt(b*b-4.0*c);
					if ((initial_guess <= 0) || (initial_guess >= 1)) initial_guess = -0.5*b + 0.5*sqrt(b*b-4.0*c);
				}
			}
			else if (initial_guess < 0) // first order
			{
				if (cf.at(cf_line,0) !=0) initial_guess = -(1.0 + cf.at(cf_line,1)/cf.at(cf_line,0));
			}

			double scale_factor = irr_scale_factor(cf_line,count);
			double residual=DBL_MAX;

			calculated_irr = irr_calc(cf_line,count,initial_guess,tolerance,max_iterations,scale_factor,number_of_iterations,residual);

			if (!is_valid_irr(cf_line,count,residual,tolerance,number_of_iterations,max_iterations,calculated_irr,scale_factor)) // try 0.1 as initial guess
			{
				initial_guess=0.1;
				number_of_iterations=0;
				residual=0;
				calculated_irr = irr_calc(cf_line,count,initial_guess,tolerance,max_iterations,scale_factor,number_of_iterations,residual);
			}

			if (!is_valid_irr(cf_line,count,residual,tolerance,number_of_iterations,max_iterations,calculated_irr,scale_factor)) // try -0.1 as initial guess
			{
				initial_guess=-0.1;
				number_of_iterations=0;
				residual=0;
				calculated_irr = irr_calc(cf_line,count,initial_guess,tolerance,max_iterations,scale_factor,number_of_iterations,residual);
			}
			if (!is_valid_irr(cf_line,count,residual,tolerance,number_of_iterations,max_iterations,calculated_irr,scale_factor)) // try 0 as initial guess
			{
				initial_guess=0;
				number_of_iterations=0;
				residual=0;
				calculated_irr = irr_calc(cf_line,count,initial_guess,tolerance,max_iterations,scale_factor,number_of_iterations,residual);
			}

			if (!is_valid_irr(cf_line,count,residual,tolerance,number_of_iterations,max_iterations,calculated_irr,scale_factor)) // try 0.1 as initial guess
			{
//				calculated_irr = 0.0; // did not converge
				calculated_irr = std::numeric_limits<double>::quiet_NaN(); // did not converge
//				double calculated_irr = -999;
			}

		}
		return calculated_irr;
	}


	double irr_calc( int cf_line, int count, double initial_guess, double tolerance, int max_iterations, double scale_factor, int &number_of_iterations, double &residual )
	{
//		double calculated_irr = 0;
		double calculated_irr = std::numeric_limits<double>::quiet_NaN();
//		double calculated_irr = -999;
		double deriv_sum = irr_derivative_sum(initial_guess, cf_line, count);
		if (deriv_sum != 0.0)
			calculated_irr = initial_guess - irr_poly_sum(initial_guess,cf_line,count)/deriv_sum;
		else
			return initial_guess;

		number_of_iterations++;


		residual = irr_poly_sum(calculated_irr,cf_line,count) / scale_factor;

		while (!(std::abs(residual) <= tolerance) && (number_of_iterations < max_iterations))
		{
			deriv_sum = irr_derivative_sum(initial_guess,cf_line,count);
			if (deriv_sum != 0.0)
				calculated_irr = calculated_irr - irr_poly_sum(calculated_irr,cf_line,count)/deriv_sum;
			else
				break;

			number_of_iterations++;
			residual = irr_poly_sum(calculated_irr,cf_line,count) / scale_factor;
		}
		return calculated_irr;
	}


	double min(double a, double b)
	{ // handle NaN
		if ((a != a) || (b != b))
			return 0;
		else
			return (a < b) ? a : b;
	}

	double max(double a, double b)
	{ // handle NaN
		if ((a != a) || (b != b))
			return 0;
		else
			return (a > b) ? a : b;
	}

	double min_cashflow_value(int cf_line, int nyears)
	{
		// check for NaN
		bool is_nan = true;
		for (int i = 1; i <= nyears; i++)
			is_nan &= std::isnan(cf.at(cf_line, i));
		if (is_nan) return std::numeric_limits<double>::quiet_NaN();

		double min_value = DBL_MAX;
		for (int i = 1; i <= nyears; i++)
			if ((cf.at(cf_line, i)<min_value) && (cf.at(cf_line, i) != 0)) min_value = cf.at(cf_line, i);
		return min_value;
	}


};




DEFINE_MODULE_ENTRY( communitysolar, "Comunity Solar Financial Model_", 1 );


