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


#include "cmod_utilityrate5.h"

#include "core.h"
#include "lib_utility_rate_equations.h"
#include "common.h"
#include <algorithm>
#include <sstream>

static var_info vtab_utility_rate5[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                                           UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,     "analysis_period",           "Number of years in analysis",                   "years",  "",                      "Lifetime",             "*",                         "INTEGER,POSITIVE",              "" },

	{ SSC_INPUT, SSC_NUMBER, "system_use_lifetime_output", "Lifetime hourly system outputs", "0/1", "0=hourly first year,1=hourly lifetime", "Lifetime", "*", "INTEGER,MIN=0,MAX=1", "" },

	{ SSC_INPUT, SSC_NUMBER, "TOU_demand_single_peak", "Use single monthly peak for TOU demand charge", "0/1", "0=use TOU peak,1=use flat peak", "Electricity Rates", "?=0", "INTEGER,MIN=0,MAX=1", "" },

	// First year or lifetime hourly or subhourly
	// load and gen expected to be > 0
	// grid positive if system generation > load, negative otherwise
	{ SSC_INPUT, SSC_ARRAY, "gen", "System power generated", "kW", "", "System Output", "*", "", "" },

	// input from user as kW and output as kW
	{ SSC_INOUT, SSC_ARRAY, "load", "Electricity load (year 1)", "kW", "", "Load", "", "", "" },

    // Optional input, only provided by battery cases
    { SSC_INPUT, SSC_ARRAY, "grid_outage", "Grid outage in this time step", "0/1", "0=GridAvailable,1=GridUnavailable,Length=load", "Load",    "",                       "",                               "" },

	//  output as kWh - same as load (kW) for hourly simulations
	{ SSC_OUTPUT, SSC_ARRAY, "bill_load", "Bill load (year 1)", "kWh", "", "Load", "*", "", "" },

    { SSC_INPUT, SSC_NUMBER, "inflation_rate", "Inflation rate", "%", "", "Lifetime", "*", "MIN=-99", "" },
	{ SSC_INPUT, SSC_ARRAY, "degradation", "Annual energy degradation", "%", "", "System Output", "system_use_lifetime_output=0", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "load_escalation", "Annual load escalation", "%/year", "", "Load", "?=0", "", "" },

	// outputs
    { SSC_OUTPUT,       SSC_ARRAY,      "annual_energy_value",             "Energy value in each year",     "$",    "",                      "Annual",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "annual_electric_load",            "Electricity load total in each year",  "kWh",    "",                      "Annual",             "*",                         "",   "" },


	// year 1 values for metrics
	{ SSC_OUTPUT, SSC_NUMBER, "utility_bill_w_sys_year1",    "Electricity bill with system (year 1)",    "$/yr", "",    "Financial Metrics", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "utility_bill_wo_sys_year1", "Electricity bill without system (year 1)", "$/yr", "",    "Financial Metrics", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "savings_year1",                  "Electricity bill savings with system (year 1)",             "$/yr",    "", "Financial Metrics", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "year1_electric_load",            "Electricity load total (year 1)",                "kWh/yr",  "", "Financial Metrics", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_e_tofromgrid", "Electricity to/from grid (year 1 hourly)", "kWh", "", "Time Series", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_e_togrid", "Electricity to grid (year 1 hourly)", "kWh", "", "Time Series", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_e_fromgrid", "Electricity from grid (year 1 hourly)", "kWh", "", "Time Series", "*", "", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_system_to_load",    "Electricity from system to load (year 1 hourly)",     "kWh", "",                      "",             "*",                         "",                   "" },

// lifetime load (optional for lifetime analysis)
	{ SSC_OUTPUT, SSC_ARRAY, "lifetime_load", "Lifetime electricity load", "kW", "", "Time Series", "?", "", "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_tofromgrid",         "Electricity to/from grid peak (year 1 hourly)", "kW",  "",                      "Time Series",             "*",                         "",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_system_to_load",         "Electricity peak from system to load (year 1 hourly)", "kW",  "",                      "Time Series",             "*",                         "",                   "" },


	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_salespurchases_with_system",     "Electricity sales/purchases with system (year 1 hourly)",    "$", "",          "Time Series",             "*",                         "",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_salespurchases_without_system",  "Electricity sales/purchases without system (year 1 hourly)", "$", "",          "Time Series",             "*",                         "",                   "" },

	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_ec_with_system", "Energy charge with system (year 1 hourly)", "$", "", "Time Series", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_ec_without_system", "Energy charge without system (year 1 hourly)", "$", "", "Time Series", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_dc_with_system", "Incremental demand charge with system (year 1 hourly)", "$", "", "Time Series", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_dc_without_system", "Incremental demand charge without system (year 1 hourly)", "$", "", "Time Series", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_ec_tou_schedule", "TOU period for energy charges (year 1 hourly)", "", "", "Time Series", "*", "", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_dc_tou_schedule",       "TOU period for demand charges (year 1 hourly)", "", "", "Time Series", "*", "", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_dc_peak_per_period",    "Electricity peak from grid per TOU period (year 1 hourly)",        "kW", "", "Time Series", "*", "", "" },


	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_fixed_with_system", "Fixed monthly charge with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_fixed_without_system", "Fixed monthly charge without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_minimum_with_system", "Minimum charge with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_minimum_without_system", "Minimum charge without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_dc_fixed_with_system", "Demand charge (flat) with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_dc_tou_with_system", "Demand charge (TOU) with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_ec_charge_with_system", "Energy charge with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_dc_fixed_without_system",   "Demand charge (flat) without system", "$/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_dc_tou_without_system",     "Demand charge (TOU) without system",   "$/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_ec_charge_without_system", "Energy charge without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },

	// monthly outputs from Sean 7/29/13 "Net Metering Accounting.xlsx" updates from Paul and Sean 8/9/13 and 8/12/13
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_load", "Electricity load", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_peak_w_system", "Demand peak with system", "kW/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_peak_wo_system", "Demand peak without system", "kW/mo", "", "Monthly", "*", "LENGTH=12", "" },

// TODO - remove after testing
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_use_w_system", "Electricity use with system", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_use_wo_system", "Electricity use without system", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_electricity_to_grid",    "Electricity to/from grid",           "kWh/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
    { SSC_OUTPUT, SSC_ARRAY, "year1_monthly_cumulative_excess_generation", "Net metering cumulative credit for annual true-up", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },
    { SSC_OUTPUT, SSC_ARRAY, "year1_monthly_utility_bill_w_sys", "Electricity bill with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_utility_bill_wo_sys", "Electricity bill without system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },


	// convert annual outputs from Arrays to Matrices years x months
	{ SSC_OUTPUT, SSC_MATRIX, "utility_bill_w_sys_ym", "Electricity bill with system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "utility_bill_wo_sys_ym", "Electricity bill without system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },

	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_fixed_ym", "Fixed monthly charge with system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_fixed_ym", "Fixed monthly charge without system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },

	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_minimum_ym", "Minimum charge with system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_minimum_ym", "Minimum charge without system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },

	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_dc_fixed_ym", "Demand charge with system (flat)", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_dc_tou_ym", "Demand charge with system (TOU)", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_dc_fixed_ym", "Demand charge without system (flat)", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_dc_tou_ym", "Demand charge without system (TOU)", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },

	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_ym", "Energy charge with system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
    { SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_ym", "Energy charge without system", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },

	// annual sums
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_w_sys", "Electricity bill with system", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "utility_bill_wo_sys", "Electricity bill without system", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_fixed", "Fixed monthly charge with system", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_fixed", "Fixed monthly charge without system", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_minimum", "Minimum charge with system", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_minimum", "Minimum charge without system", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_dc_fixed", "Demand charge with system (flat)", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_dc_tou", "Demand charge with system (TOU)", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed", "Demand charge without system (flat)", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou", "Demand charge without system (TOU)", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec", "Energy charge with system", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec", "Energy charge without system", "$", "", "Charges by Month", "*", "", "" },

    // Updated based on https://github.com/NREL/SAM/issues/372
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_gross_ym", "Energy charge with system before credits", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "nm_dollars_applied_ym", "Net metering credit", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
	{ SSC_OUTPUT, SSC_MATRIX, "excess_kwhs_earned_ym", "Excess generation", "kWh", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
    { SSC_OUTPUT, SSC_MATRIX, "net_billing_credits_ym", "Net billing credit", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
    { SSC_OUTPUT, SSC_MATRIX, "two_meter_sales_ym",     "Buy all sell all electricity sales to grid", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },
    { SSC_OUTPUT, SSC_MATRIX, "true_up_credits_ym",     "Net annual true-up payments", "$", "", "Charges by Month", "*", "", "COL_LABEL=MONTHS,FORMAT_SPEC=CURRENCY,GROUP=UR_AM" },

	// Updated based on https://github.com/NREL/SAM/issues/372
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_ec_charge_gross_with_system", "Energy charge with system before credits", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_nm_dollars_applied", "Net metering credit", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_excess_kwhs_earned", "Excess generation", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },
    { SSC_OUTPUT, SSC_ARRAY, "year1_net_billing_credits", "Net billing credit", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
    { SSC_OUTPUT, SSC_ARRAY, "year1_two_meter_sales",  "Buy all sell all electricity sales to grid", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
    { SSC_OUTPUT, SSC_ARRAY, "year1_true_up_credits",  "Net annual true-up payments", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },

    // Outputs for billing demand calculations
    { SSC_OUTPUT, SSC_MATRIX, "billing_demand_w_sys_ym",     "Billing demand with system", "kW", "", "Charges by Month", "", "", "COL_LABEL=MONTHS,GROUP=UR_AM" },
    { SSC_OUTPUT, SSC_ARRAY, "year1_billing_demand_w_sys",  "Billing demand with system", "kW", "", "Monthly", "", "LENGTH=12", "" },
    { SSC_OUTPUT, SSC_MATRIX, "billing_demand_wo_sys_ym",     "Billing demand without system", "kW", "", "Charges by Month", "", "", "COL_LABEL=MONTHS,GROUP=UR_AM" },
    { SSC_OUTPUT, SSC_ARRAY, "year1_billing_demand_wo_sys",  "Billing demand without system", "kW", "", "Monthly", "", "LENGTH=12", "" },


// for Pablo at IRENA 8/8/15
// first year outputs only per email from Paul 8/9/15

// energy charge wo system
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_jan_tp", "Energy charge without system Jan", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_feb_tp", "Energy charge without system Feb", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_mar_tp", "Energy charge without system Mar", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_apr_tp", "Energy charge without system Apr", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_may_tp", "Energy charge without system May", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_jun_tp", "Energy charge without system Jun", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_jul_tp", "Energy charge without system Jul", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_aug_tp", "Energy charge without system Aug", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_sep_tp", "Energy charge without system Sep", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_oct_tp", "Energy charge without system Oct", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_nov_tp", "Energy charge without system Nov", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_wo_sys_ec_dec_tp", "Energy charge without system Dec", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },

	// energy use wo system
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_jan_tp", "Electricity usage without system Jan", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_feb_tp", "Electricity usage without system Feb", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_mar_tp", "Electricity usage without system Mar", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_apr_tp", "Electricity usage without system Apr", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_may_tp", "Electricity usage without system May", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_jun_tp", "Electricity usage without system Jun", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_jul_tp", "Electricity usage without system Jul", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_aug_tp", "Electricity usage without system Aug", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_sep_tp", "Electricity usage without system Sep", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_oct_tp", "Electricity usage without system Oct", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_nov_tp", "Electricity usage without system Nov", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_wo_sys_ec_dec_tp", "Electricity usage without system Dec", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },

	// energy charge w system
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_jan_tp", "Energy charge with system Jan", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_feb_tp", "Energy charge with system Feb", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_mar_tp", "Energy charge with system Mar", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_apr_tp", "Energy charge with system Apr", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_may_tp", "Energy charge with system May", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_jun_tp", "Energy charge with system Jun", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_jul_tp", "Energy charge with system Jul", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_aug_tp", "Energy charge with system Aug", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_sep_tp", "Energy charge with system Sep", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_oct_tp", "Energy charge with system Oct", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_nov_tp", "Energy charge with system Nov", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "charge_w_sys_ec_dec_tp", "Energy charge with system Dec", "$", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },

	// energy use w system
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_jan_tp", "Electricity usage with system Jan", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_feb_tp", "Electricity usage with system Feb", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_mar_tp", "Electricity usage with system Mar", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_apr_tp", "Electricity usage with system Apr", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_may_tp", "Electricity usage with system May", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_jun_tp", "Electricity usage with system Jun", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_jul_tp", "Electricity usage with system Jul", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_aug_tp", "Electricity usage with system Aug", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_sep_tp", "Electricity usage with system Sep", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_oct_tp", "Electricity usage with system Oct", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_nov_tp", "Electricity usage with system Nov", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "energy_w_sys_ec_dec_tp", "Electricity usage with system Dec", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },

	// energy surplus w system
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_jan_tp", "Electricity exports with system Jan", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_feb_tp", "Electricity exports with system Feb", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_mar_tp", "Electricity exports with system Mar", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_apr_tp", "Electricity exports with system Apr", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_may_tp", "Electricity exports with system May", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_jun_tp", "Electricity exports with system Jun", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_jul_tp", "Electricity exports with system Jul", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_aug_tp", "Electricity exports with system Aug", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_sep_tp", "Electricity exports with system Sep", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_oct_tp", "Electricity exports with system Oct", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_nov_tp", "Electricity exports with system Nov", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },
	{ SSC_OUTPUT, SSC_MATRIX, "surplus_w_sys_ec_dec_tp", "Electricity exports with system Dec", "kWh", "", "Charges by Month", "*", "", "ROW_LABEL=UR_PERIODNUMS,COL_LABEL=UR_TIERNUMS,FORMAT_SPEC=CURRENCY,GROUP=UR_MTP" },

		// monthly peak demand per period
	{ SSC_OUTPUT, SSC_MATRIX, "monthly_tou_demand_peak_w_sys", "Demand peak with system", "kW", "", "Charges by Month", "", "", "ROW_LABEL=MONTHS,COL_LABEL=UR_MONTH_TOU_DEMAND,FORMAT_SPEC=CURRENCY,GROUP=UR_DMP" },
	{ SSC_OUTPUT, SSC_MATRIX, "monthly_tou_demand_peak_wo_sys", "Demand peak without system", "kW", "", "Charges by Month", "", "", "ROW_LABEL=MONTHS,COL_LABEL=UR_MONTH_TOU_DEMAND,FORMAT_SPEC=CURRENCY,GROUP=UR_DMP" },
	{ SSC_OUTPUT, SSC_MATRIX, "monthly_tou_demand_charge_w_sys", "Demand peak charge with system", "$", "", "Charges by Month", "", "", "ROW_LABEL=MONTHS,COL_LABEL=UR_MONTH_TOU_DEMAND,FORMAT_SPEC=CURRENCY,GROUP=UR_DMP" },
	{ SSC_OUTPUT, SSC_MATRIX, "monthly_tou_demand_charge_wo_sys", "Demand peak charge without system", "$", "", "Charges by Month", "", "", "ROW_LABEL=MONTHS,COL_LABEL=UR_MONTH_TOU_DEMAND,FORMAT_SPEC=CURRENCY,GROUP=UR_DMP" },


	var_info_invalid };

void rate_setup::setup(var_table* vt, int num_recs_yearly, size_t nyears, rate_data& rate, std::string cm_name) {
    rate.dc_enabled = vt->as_boolean("ur_dc_enable");
    rate.en_ts_buy_rate = vt->as_boolean("ur_en_ts_buy_rate");
    rate.en_ts_sell_rate = vt->as_boolean("ur_en_ts_sell_rate");

    size_t cnt = 0; size_t nrows, ncols, i;
    ssc_number_t* parr = 0;
    ssc_number_t* ts_sr = NULL; ssc_number_t* ts_br = NULL;

    rate.init(num_recs_yearly);

    double inflation_rate = vt->as_double("inflation_rate") * 0.01;

    // compute utility rate out-years escalation multipliers
    std::vector<ssc_number_t> rate_scale(nyears);
    parr = vt->as_array("rate_escalation", &cnt);
    if (cnt == 1)
    {
        for (i = 0; i < nyears; i++)
            rate_scale[i] = (ssc_number_t)pow((double)(inflation_rate + 1 + parr[0] * 0.01), (double)i);
    }
    else if (cnt < nyears)
    {
        throw exec_error("utilityrate5", "rate_escalation must have 1 entry or length equal to analysis_period");
    }
    else
    {
        for (i = 0; i < nyears; i++)
            rate_scale[i] = (ssc_number_t)(1 + parr[i] * 0.01);
    }
    rate.rate_scale = rate_scale;

    if (rate.en_ts_buy_rate)
    {
        if (!vt->is_assigned("ur_ts_buy_rate"))
        {
            throw exec_error("utilityrate5", util::format("Error in ur_ts_buy_rate. Time step buy rate enabled but no time step buy rates specified."));
        }
        else
        { // hourly or sub hourly loads for single year
            ts_br = vt->as_array("ur_ts_buy_rate", &cnt);
            size_t ts_step_per_hour = cnt / 8760;
            if (ts_step_per_hour < 1 || ts_step_per_hour > 60 || ts_step_per_hour * 8760 != cnt)
                throw exec_error("utilityrate5", util::format("number of buy rate records (%d) must be equal to number of gen records (%d) or 8760 for each year", (int)cnt, (int)rate.m_num_rec_yearly));
        }
    }

    if (rate.en_ts_sell_rate) {
        if (!vt->is_assigned("ur_ts_sell_rate"))
        {
            throw exec_error(cm_name, util::format("Error in ur_ts_sell_rate. Time step sell rate enabled but no time step sell rates specified."));
        }
        else
        { // hourly or sub hourly loads for single year

            ts_sr = vt->as_array("ur_ts_sell_rate", &cnt);
            size_t ts_step_per_hour = cnt / 8760;
            if (ts_step_per_hour < 1 || ts_step_per_hour > 60 || ts_step_per_hour * 8760 != cnt)
                throw exec_error(cm_name, util::format("invalid number of sell rate records (%d): must be an integer multiple of 8760", (int)cnt));
        }
    }

    rate.setup_time_series(cnt, ts_sr, ts_br);

    // Energy charges are always enabled
    ssc_number_t* ec_weekday = vt->as_matrix("ur_ec_sched_weekday", &nrows, &ncols);
    if (nrows != 12 || ncols != 24)
    {
        std::ostringstream ss;
        ss << "The weekday TOU matrix for energy rates should have 12 rows and 24 columns. Instead it has " << nrows << " rows and " << ncols << " columns.";
        throw exec_error(cm_name, ss.str());
    }
    ssc_number_t* ec_weekend = vt->as_matrix("ur_ec_sched_weekend", &nrows, &ncols);
    if (nrows != 12 || ncols != 24)
    {
        std::ostringstream ss;
        ss << "The weekend TOU matrix for energy rates should have 12 rows and 24 columns. Instead it has " << nrows << " rows and " << ncols << " columns.";
        throw exec_error(cm_name, ss.str());
    }

    // 6 columns period, tier, max usage, max usage units, buy, sell
    ssc_number_t* ec_tou_in = vt->as_matrix("ur_ec_tou_mat", &nrows, &ncols);
    if (ncols != 6)
    {
        std::ostringstream ss;
        ss << "The energy rate table must have 6 columns. Instead it has " << ncols << " columns.";
        throw exec_error(cm_name, ss.str());
    }
    size_t tou_rows = nrows;

    bool sell_eq_buy = vt->as_boolean("ur_sell_eq_buy");

    rate.setup_energy_rates(ec_weekday, ec_weekend, tou_rows, ec_tou_in, sell_eq_buy);

    ssc_number_t* dc_weekday = NULL; ssc_number_t* dc_weekend = NULL; ssc_number_t* dc_tou_in = NULL; ssc_number_t* dc_flat_in = NULL;
    size_t dc_tier_rows = 0; size_t dc_flat_rows = 0;

    if (rate.dc_enabled)
    {
        dc_weekday = vt->as_matrix("ur_dc_sched_weekday", &nrows, &ncols);
        if (nrows != 12 || ncols != 24)
        {
            std::ostringstream ss;
            ss << "The weekday TOU matrix for demand rates should have 12 rows and 24 columns. Instead it has " << nrows << " rows and " << ncols << " columns.";
            throw exec_error(cm_name, ss.str());
        }
        dc_weekend = vt->as_matrix("ur_dc_sched_weekend", &nrows, &ncols);
        if (nrows != 12 || ncols != 24)
        {
            std::ostringstream ss;
            ss << "The weekend TOU matrix for demand rates should have 12 rows and 24 columns. Instead it has " << nrows << " rows and " << ncols << " columns.";
            throw exec_error(cm_name, ss.str());
        }

        dc_tou_in = vt->as_matrix("ur_dc_tou_mat", &nrows, &ncols);
        if (ncols != 4)
        {
            std::ostringstream ss;
            ss << "The demand rate table for TOU periods, 'ur_dc_tou_mat', must have 4 columns. Instead, it has " << ncols << "columns.";
            throw exec_error(cm_name, ss.str());
        }
        dc_tier_rows = nrows;

        // 4 columns month, tier, max usage, charge
        dc_flat_in = vt->as_matrix("ur_dc_flat_mat", &nrows, &ncols);
        if (ncols != 4)
        {
            std::ostringstream ss;
            ss << "The demand rate table, 'ur_dc_flat_mat', by month must have 4 columns. Instead it has " << ncols << " columns";
            throw exec_error(cm_name, ss.str());
        }
        dc_flat_rows = nrows;
        rate.setup_demand_charges(dc_weekday, dc_weekend, dc_tier_rows, dc_tou_in, dc_flat_rows, dc_flat_in);
    }

    int metering_option = vt->as_integer("ur_metering_option");
    rate.enable_nm = (metering_option == 0 || metering_option == 1);
    rate.nm_credits_w_rollover = (vt->as_integer("ur_metering_option") == 0);
    rate.net_metering_credit_month = (int)vt->as_number("ur_nm_credit_month");
    rate.nb_credit_expire = vt->as_boolean("ur_nb_credit_expire");
    rate.nb_apply_credit_current_month = vt->as_boolean("ur_nb_apply_credit_current_month");
    rate.nm_credit_sell_rate = vt->as_number("ur_nm_yearend_sell_rate");

    ssc_number_t* ratchet_matrix = NULL; ssc_number_t* bd_tou_matrix = NULL;
    bool ratchets_enabled = vt->as_boolean("ur_enable_billing_demand");
    if (ratchets_enabled) {
        rate.en_billing_demand_lookback = ratchets_enabled;
        rate.bd_minimum = vt->as_number("ur_billing_demand_minimum");
        rate.bd_lookback_months = vt->as_integer("ur_billing_demand_lookback_period");

        ratchet_matrix = vt->as_matrix("ur_billing_demand_lookback_percentages", &nrows, &ncols);
        if (nrows != 12 || ncols != 2)
        {
            std::ostringstream ss;
            ss << "The ur_billing_demand_lookback_percentages matrix should have 12 rows and 2 columns. Instead it has " << nrows << " rows and " << ncols << " columns.";
            throw exec_error(cm_name, ss.str());
        }

        bd_tou_matrix = vt->as_matrix("ur_dc_billing_demand_periods", &nrows, &ncols);
        if (ncols != 2) {
            std::ostringstream ss;
            ss << "The ur_dc_billing_demand_periods matrix should have 2 columns. Instead it has " << ncols << " columns.";
            throw exec_error(cm_name, ss.str());
        }
        else if (rate.dc_enabled && nrows != rate.m_dc_tou_periods.size()) {
            std::ostringstream ss;
            ss << "The ur_dc_billing_demand_periods matrix should have " << rate.m_dc_tou_periods.size() << " rows, to match the number of TOU periods. Instead it has " << nrows << " rows.";
            throw exec_error(cm_name, ss.str());
        }

        bool error = rate.setup_ratcheting_demand(ratchet_matrix, bd_tou_matrix);

        if (error) {
            std::ostringstream ss;
            ss << "ur_dc_billing_demand_periods should have at least one period where billing demand calculations are enabled.";
            throw exec_error(cm_name, ss.str());
        }
    }


    rate.init_energy_rates_all_months(false); // TODO: update if rate forecast needs to support two meter
};

class cm_utilityrate5 : public compute_module
{
private:
	rate_data rate;

	size_t m_num_rec_yearly;

public:
	cm_utilityrate5() :
		rate(),
		m_num_rec_yearly(8760) // will be overwritten
	{
		add_var_info( vtab_utility_rate5 );
        add_var_info( vtab_utility_rate_common );
	}

	void exec( )
	{
		// if not assigned, we assume electricity rates are enabled
		if (is_assigned("en_electricity_rates")) {
			if (!as_boolean("en_electricity_rates")) {
				remove_var_info(vtab_utility_rate5);
				return;
			}
		}

		ssc_number_t *parr = 0;
		size_t count, i, j;

		size_t nyears = (size_t)as_integer("analysis_period");

        scalefactors scale_calculator(m_vartab);

		// compute annual system output degradation multipliers
		std::vector<ssc_number_t> sys_scale(nyears);

		// degradation
		// degradation starts in year 2 for single value degradation - no degradation in year 1 - degradation =1.0
		// lifetime degradation applied in technology compute modules
		if (as_integer("system_use_lifetime_output") == 1)
		{
			for (i = 0; i<nyears; i++)
				sys_scale[i] = 1.0;
		}
		else
		{
			parr = as_array("degradation", &count);
			if (count == 1)
			{
				for (i = 0; i<nyears; i++)
					sys_scale[i] = (ssc_number_t)pow((double)(1 - parr[0] * 0.01), (double)i);
			}
			else
			{
				for (i = 0; i<nyears && i<count; i++)
					sys_scale[i] = (ssc_number_t)(1.0 - parr[i] * 0.01);
			}
		}



		// compute load (electric demand) annual escalation multipliers
        std::vector<ssc_number_t> load_scale = scale_calculator.get_factors("load_escalation");

		/* Update all e_sys and e_load values based on new inputs
		grid = gen -load where gen = sys + batt
		1. scale load and system value to hourly values as necessary
		2. use (kWh) e_sys[i] = sum((grid+load) * timestep ) over the hour for each hour i
		3. use (kW)  p_sys[i] = max( grid+load) over the hour for each hour i
		3. use (kWh) e_load[i] = sum(load * timestep ) over the hour for each hour i
		4. use (kW)  p_load[i] = max(load) over the hour for each hour i
		5. After above assignment, proceed as before with same outputs
		*/
		ssc_number_t *pload = NULL, *pgen;
		size_t nrec_load = 0, nrec_gen = 0, step_per_hour_gen=1, step_per_hour_load=1;
		bool bload=false;
		pgen = as_array("gen", &nrec_gen);
		// for lifetime analysis
		size_t nrec_gen_per_year = nrec_gen;
		if (as_integer("system_use_lifetime_output") == 1)
			nrec_gen_per_year = nrec_gen / nyears;
		step_per_hour_gen = nrec_gen_per_year / 8760;
		if (step_per_hour_gen < 1 || step_per_hour_gen > 60 || step_per_hour_gen * 8760 != nrec_gen_per_year)
			throw exec_error("utilityrate5", util::format("invalid number of gen records (%d): must be an integer multiple of 8760", (int)nrec_gen_per_year));
		ssc_number_t ts_hour_gen = 1.0f / step_per_hour_gen;
		m_num_rec_yearly = nrec_gen_per_year;

		if (is_assigned("load"))
		{ // hourly or sub hourly loads for single year
			bload = true;
			pload = as_array("load", &nrec_load);
			step_per_hour_load = nrec_load / 8760;
			if (step_per_hour_load < 1 || step_per_hour_load > 60 || step_per_hour_load * 8760 != nrec_load)
				throw exec_error("utilityrate5", util::format("invalid number of load records (%d): must be an integer multiple of 8760", (int)nrec_load));
			if ((nrec_load != m_num_rec_yearly) && (nrec_load != 8760))
				throw exec_error("utilityrate5", util::format("number of load records (%d) must be equal to number of gen records (%d) or 8760 for each year", (int)nrec_load, (int)m_num_rec_yearly));
		}
//		ssc_number_t ts_hour_load = 1.0f / step_per_hour_load;

        std::vector<bool> grid_outage(m_num_rec_yearly, false);
        if (is_assigned("grid_outage")) {
            grid_outage = as_vector_bool("grid_outage");
        }
        if (grid_outage.size() != m_num_rec_yearly) {
            // throw error - causing sam issue 1676
            // set to first value or false
            bool bVal = false;
            if (grid_outage.size() > 0)
                bVal = grid_outage[0];
            grid_outage.resize(m_num_rec_yearly, bVal);
        }

		// prepare timestep arrays for load and grid values
		std::vector<ssc_number_t>
			e_sys_cy(m_num_rec_yearly), p_sys_cy(m_num_rec_yearly),
			p_load(m_num_rec_yearly), // to handle no load, or num load != num gen
			e_grid_cy(m_num_rec_yearly), p_grid_cy(m_num_rec_yearly),
			e_load_cy(m_num_rec_yearly), p_load_cy(m_num_rec_yearly); // current year load (accounts for escal)



		// assign timestep values for utility rate calculations
		size_t idx = 0;
		ssc_number_t ts_load = 0;
		ssc_number_t year1_elec_load = 0;

		//load - fill out to number of generation records per year
		// handle cases
		// 1. if no load
		// 2. if load has 8760 and gen has more records
		// 3. if number records same for load and gen
		idx = 0;
		for (i = 0; i < 8760; i++)
		{
			for (size_t ii = 0; ii < step_per_hour_gen; ii++)
			{
				size_t ndx = i*step_per_hour_gen + ii;
				ts_load = (bload ? ((idx < nrec_load) ? pload[idx] : 0) : 0);
				year1_elec_load += ts_load;
				// sign correction for utility rate calculations
				p_load[ndx] = -ts_load;
				if (step_per_hour_gen == step_per_hour_load)
					idx++;
				else if (ii == (step_per_hour_gen - 1))
					idx++;
			}
		}

		assign("year1_electric_load", year1_elec_load* ts_hour_gen);


		/* allocate intermediate data arrays */
		std::vector<ssc_number_t> revenue_w_sys(m_num_rec_yearly), revenue_wo_sys(m_num_rec_yearly),
			payment(m_num_rec_yearly), income(m_num_rec_yearly),
			demand_charge_w_sys(m_num_rec_yearly), energy_charge_w_sys(m_num_rec_yearly), energy_charge_gross_w_sys(m_num_rec_yearly),
			demand_charge_wo_sys(m_num_rec_yearly), energy_charge_wo_sys(m_num_rec_yearly),
			ec_tou_sched(m_num_rec_yearly), dc_tou_sched(m_num_rec_yearly), load(m_num_rec_yearly),
			e_tofromgrid(m_num_rec_yearly), p_tofromgrid(m_num_rec_yearly), salespurchases(m_num_rec_yearly);
        std::vector<ssc_number_t> monthly_revenue_w_sys(12), monthly_revenue_wo_sys(12),
            monthly_fixed_charges(12), monthly_minimum_charges(12),
            monthly_ec_charges(12),
            monthly_ec_charges_gross(12),
            monthly_nm_dollars_applied(12),
            monthly_excess_dollars_earned(12),
            monthly_excess_kwhs_earned(12),
            monthly_net_billing_credits(12),
            monthly_ec_rates(12),
            monthly_salespurchases(12),
            monthly_load(12), monthly_system_generation(12), monthly_elec_to_grid(12),
            monthly_elec_needed_from_grid(12),
            monthly_cumulative_excess_energy_w_sys(12), monthly_cumulative_excess_dollars_w_sys(12),
            monthly_cumulative_excess_energy_wo_sys(12), monthly_cumulative_excess_dollars_wo_sys(12),
            monthly_bill(12), monthly_test(12),
            monthly_two_meter_sales(12),
            monthly_peak_wo_sys(12), monthly_peak_w_sys(12), // can't re-use these due to their role in billing demand
            monthly_true_up_credits(12), // Realistically only one true-up month will be non-zero, but track them all for monthly outputs
            monthly_billing_demand_peaks_wo_sys(12), monthly_billing_demand_peaks_w_sys(12); // Peaks used for billing demand calculations - sometimes does not include dc_flat_peak

		/* allocate outputs */
		ssc_number_t *annual_net_revenue = allocate("annual_energy_value", nyears+1);
		ssc_number_t *annual_electric_load = allocate("annual_electric_load", nyears+1);
		ssc_number_t *energy_net = allocate("scaled_annual_energy", nyears+1);
		ssc_number_t *annual_revenue_w_sys = allocate("revenue_with_system", nyears+1);
		ssc_number_t *annual_revenue_wo_sys = allocate("revenue_without_system", nyears+1);

		// matrices
		ssc_number_t *utility_bill_w_sys_ym = allocate("utility_bill_w_sys_ym", nyears + 1, 12);
		ssc_number_t *utility_bill_wo_sys_ym = allocate("utility_bill_wo_sys_ym", nyears + 1, 12);
		ssc_number_t *ch_w_sys_dc_fixed_ym = allocate("charge_w_sys_dc_fixed_ym", nyears + 1, 12);
		ssc_number_t *ch_w_sys_dc_tou_ym = allocate("charge_w_sys_dc_tou_ym", nyears + 1, 12);
		ssc_number_t *ch_w_sys_ec_ym = allocate("charge_w_sys_ec_ym", nyears + 1, 12);

		ssc_number_t *ch_w_sys_ec_gross_ym = allocate("charge_w_sys_ec_gross_ym", nyears + 1, 12);
		ssc_number_t *nm_dollars_applied_ym = allocate("nm_dollars_applied_ym", nyears + 1, 12);
		ssc_number_t *excess_kwhs_earned_ym = allocate("excess_kwhs_earned_ym", nyears + 1, 12);
        ssc_number_t* net_billing_credits_ym = allocate("net_billing_credits_ym", nyears + 1, 12);
        ssc_number_t* two_meter_sales_ym = allocate("two_meter_sales_ym", nyears + 1, 12);
        ssc_number_t* true_up_credits_ym = allocate("true_up_credits_ym", nyears + 1, 12);

		ssc_number_t *ch_wo_sys_dc_fixed_ym = allocate("charge_wo_sys_dc_fixed_ym", nyears + 1, 12);
		ssc_number_t *ch_wo_sys_dc_tou_ym = allocate("charge_wo_sys_dc_tou_ym", nyears + 1, 12);
		ssc_number_t *ch_wo_sys_ec_ym = allocate("charge_wo_sys_ec_ym", nyears + 1, 12);
		ssc_number_t *ch_w_sys_fixed_ym = allocate("charge_w_sys_fixed_ym", nyears + 1, 12);
		ssc_number_t *ch_wo_sys_fixed_ym = allocate("charge_wo_sys_fixed_ym", nyears + 1, 12);
		ssc_number_t *ch_w_sys_minimum_ym = allocate("charge_w_sys_minimum_ym", nyears + 1, 12);
		ssc_number_t *ch_wo_sys_minimum_ym = allocate("charge_wo_sys_minimum_ym", nyears + 1, 12);


		// annual sums
		ssc_number_t *utility_bill_w_sys = allocate("utility_bill_w_sys", nyears + 1);
		ssc_number_t *utility_bill_wo_sys = allocate("utility_bill_wo_sys", nyears + 1);
		ssc_number_t *ch_w_sys_dc_fixed = allocate("charge_w_sys_dc_fixed", nyears + 1);
		ssc_number_t *ch_w_sys_dc_tou = allocate("charge_w_sys_dc_tou", nyears + 1);
		ssc_number_t *ch_w_sys_ec = allocate("charge_w_sys_ec", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed = allocate("charge_wo_sys_dc_fixed", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou = allocate("charge_wo_sys_dc_tou", nyears + 1);
		ssc_number_t *ch_wo_sys_ec = allocate("charge_wo_sys_ec", nyears + 1);
		ssc_number_t *ch_w_sys_fixed = allocate("charge_w_sys_fixed", nyears + 1);
		ssc_number_t *ch_wo_sys_fixed = allocate("charge_wo_sys_fixed", nyears + 1);
		ssc_number_t *ch_w_sys_minimum = allocate("charge_w_sys_minimum", nyears + 1);
		ssc_number_t *ch_wo_sys_minimum = allocate("charge_wo_sys_minimum", nyears + 1);


		// Enphase outputs requested - see emails 2/12/16- first year system to grid and from grid
		ssc_number_t *year1_hourly_e_togrid = allocate("year1_hourly_e_togrid", m_num_rec_yearly);
		ssc_number_t *year1_hourly_e_fromgrid = allocate("year1_hourly_e_fromgrid", m_num_rec_yearly);



		// IRENA outputs array of tier values
		// reverse to tiers columns and periods are rows based on IRENA desired output.
		// tiers and periods determined by input matrices

		rate_setup::setup(m_vartab, (int)m_num_rec_yearly, (int)nyears, rate, "utilityrate5");

        // Check units are intialized properly
        // This code provides error cecking for rate.init_energy_rates handling of kWh/kW units, but must be done here due to SSC logging
        bool log_msg = false;
        int e_month = -1;
        int e_period = -1;
        int e_tier = -1;
        for (int m = 0; m < 12; m++) {
            ur_month month = rate.m_month[m];
            int units = month.ec_tou_units.at(0, 0);
            if (units == 0 || units == 2) {
                for (i = 0; i < month.ec_periods.size(); i++)
                {
                    int period = month.ec_periods[i] - 1;
                    std::vector<int> tiers = month.ec_periods_tiers[period];
                    for (j = 0; j < tiers.size(); j++)
                    {
                        units = month.ec_tou_units.at(i, j);
                        if (units == 1 || units == 3) {
                            log_msg = true;
                            e_period = period;
                            e_tier = (int) j;
                            break;
                        }
                    }

                    if (log_msg) {
                        break;
                    }
                }
            }
            if (log_msg) {
                e_month = m;
                break;
            }
        }

        if (log_msg) {
            std::ostringstream ss;
            ss << "Period " << e_period << " tier " << e_tier << " of month " << e_month << " has units of kWh/kW or kWh/kW daily. ";
            ss << "If these units are used, they must appear in the first tier of the first period, and all periods must have the same tier schedule. Please double check the rate schedule.";
            log(ss.str(), SSC_WARNING);
        }

		size_t jan_rows = rate.m_month[0].ec_charge.nrows() + 2;
		size_t jan_cols = rate.m_month[0].ec_charge.ncols() + 2;
		size_t feb_rows = rate.m_month[1].ec_charge.nrows() + 2;
		size_t feb_cols = rate.m_month[1].ec_charge.ncols() + 2;
		size_t mar_rows = rate.m_month[2].ec_charge.nrows() + 2;
		size_t mar_cols = rate.m_month[2].ec_charge.ncols() + 2;
		size_t apr_rows = rate.m_month[3].ec_charge.nrows() + 2;
		size_t apr_cols = rate.m_month[3].ec_charge.ncols() + 2;
		size_t may_rows = rate.m_month[4].ec_charge.nrows() + 2;
		size_t may_cols = rate.m_month[4].ec_charge.ncols() + 2;
		size_t jun_rows = rate.m_month[5].ec_charge.nrows() + 2;
		size_t jun_cols = rate.m_month[5].ec_charge.ncols() + 2;
		size_t jul_rows = rate.m_month[6].ec_charge.nrows() + 2;
		size_t jul_cols = rate.m_month[6].ec_charge.ncols() + 2;
		size_t aug_rows = rate.m_month[7].ec_charge.nrows() + 2;
		size_t aug_cols = rate.m_month[7].ec_charge.ncols() + 2;
		size_t sep_rows = rate.m_month[8].ec_charge.nrows() + 2;
		size_t sep_cols = rate.m_month[8].ec_charge.ncols() + 2;
		size_t oct_rows = rate.m_month[9].ec_charge.nrows() + 2;
		size_t oct_cols = rate.m_month[9].ec_charge.ncols() + 2;
		size_t nov_rows = rate.m_month[10].ec_charge.nrows() + 2;
		size_t nov_cols = rate.m_month[10].ec_charge.ncols() + 2;
		size_t dec_rows = rate.m_month[11].ec_charge.nrows() + 2;
		size_t dec_cols = rate.m_month[11].ec_charge.ncols() + 2;


		// note that ec_charge and not ec_energy_use have the correct dimensions after setup
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_jan_tp = allocate_matrix("charge_wo_sys_ec_jan_tp", jan_rows, jan_cols);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_feb_tp = allocate_matrix("charge_wo_sys_ec_feb_tp", feb_rows, feb_cols);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_mar_tp = allocate_matrix("charge_wo_sys_ec_mar_tp", mar_rows, mar_cols);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_apr_tp = allocate_matrix("charge_wo_sys_ec_apr_tp", apr_rows, apr_cols);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_may_tp = allocate_matrix("charge_wo_sys_ec_may_tp", may_rows, may_cols);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_jun_tp = allocate_matrix("charge_wo_sys_ec_jun_tp", jun_rows, jun_cols);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_jul_tp = allocate_matrix("charge_wo_sys_ec_jul_tp", jul_rows, jul_cols);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_aug_tp = allocate_matrix("charge_wo_sys_ec_aug_tp", aug_rows, aug_cols);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_sep_tp = allocate_matrix("charge_wo_sys_ec_sep_tp", sep_rows, sep_cols);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_oct_tp = allocate_matrix("charge_wo_sys_ec_oct_tp", oct_rows, oct_cols);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_nov_tp = allocate_matrix("charge_wo_sys_ec_nov_tp", nov_rows, nov_cols);
		util::matrix_t<ssc_number_t> &charge_wo_sys_ec_dec_tp = allocate_matrix("charge_wo_sys_ec_dec_tp", dec_rows, dec_cols);


		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_jan_tp = allocate_matrix("energy_wo_sys_ec_jan_tp", jan_rows, jan_cols);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_feb_tp = allocate_matrix("energy_wo_sys_ec_feb_tp", feb_rows, feb_cols);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_mar_tp = allocate_matrix("energy_wo_sys_ec_mar_tp", mar_rows, mar_cols);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_apr_tp = allocate_matrix("energy_wo_sys_ec_apr_tp", apr_rows, apr_cols);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_may_tp = allocate_matrix("energy_wo_sys_ec_may_tp", may_rows, may_cols);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_jun_tp = allocate_matrix("energy_wo_sys_ec_jun_tp", jun_rows, jun_cols);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_jul_tp = allocate_matrix("energy_wo_sys_ec_jul_tp", jul_rows, jul_cols);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_aug_tp = allocate_matrix("energy_wo_sys_ec_aug_tp", aug_rows, aug_cols);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_sep_tp = allocate_matrix("energy_wo_sys_ec_sep_tp", sep_rows, sep_cols);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_oct_tp = allocate_matrix("energy_wo_sys_ec_oct_tp", oct_rows, oct_cols);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_nov_tp = allocate_matrix("energy_wo_sys_ec_nov_tp", nov_rows, nov_cols);
		util::matrix_t<ssc_number_t> &energy_wo_sys_ec_dec_tp = allocate_matrix("energy_wo_sys_ec_dec_tp", dec_rows, dec_cols);

// no longer output but still updated internally in ur_update_ec_monthly
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_jan_tp = allocate_matrix("surplus_wo_sys_ec_jan_tp", jan_rows, jan_cols);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_feb_tp = allocate_matrix("surplus_wo_sys_ec_feb_tp", feb_rows, feb_cols);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_mar_tp = allocate_matrix("surplus_wo_sys_ec_mar_tp", mar_rows, mar_cols);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_apr_tp = allocate_matrix("surplus_wo_sys_ec_apr_tp", apr_rows, apr_cols);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_may_tp = allocate_matrix("surplus_wo_sys_ec_may_tp", may_rows, may_cols);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_jun_tp = allocate_matrix("surplus_wo_sys_ec_jun_tp", jun_rows, jun_cols);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_jul_tp = allocate_matrix("surplus_wo_sys_ec_jul_tp", jul_rows, jul_cols);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_aug_tp = allocate_matrix("surplus_wo_sys_ec_aug_tp", aug_rows, aug_cols);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_sep_tp = allocate_matrix("surplus_wo_sys_ec_sep_tp", sep_rows, sep_cols);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_oct_tp = allocate_matrix("surplus_wo_sys_ec_oct_tp", oct_rows, oct_cols);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_nov_tp = allocate_matrix("surplus_wo_sys_ec_nov_tp", nov_rows, nov_cols);
		util::matrix_t<ssc_number_t> &surplus_wo_sys_ec_dec_tp = allocate_matrix("surplus_wo_sys_ec_dec_tp", dec_rows, dec_cols);




		util::matrix_t<ssc_number_t> &charge_w_sys_ec_jan_tp = allocate_matrix("charge_w_sys_ec_jan_tp", jan_rows, jan_cols);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_feb_tp = allocate_matrix("charge_w_sys_ec_feb_tp", feb_rows, feb_cols);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_mar_tp = allocate_matrix("charge_w_sys_ec_mar_tp", mar_rows, mar_cols);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_apr_tp = allocate_matrix("charge_w_sys_ec_apr_tp", apr_rows, apr_cols);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_may_tp = allocate_matrix("charge_w_sys_ec_may_tp", may_rows, may_cols);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_jun_tp = allocate_matrix("charge_w_sys_ec_jun_tp", jun_rows, jun_cols);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_jul_tp = allocate_matrix("charge_w_sys_ec_jul_tp", jul_rows, jul_cols);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_aug_tp = allocate_matrix("charge_w_sys_ec_aug_tp", aug_rows, aug_cols);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_sep_tp = allocate_matrix("charge_w_sys_ec_sep_tp", sep_rows, sep_cols);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_oct_tp = allocate_matrix("charge_w_sys_ec_oct_tp", oct_rows, oct_cols);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_nov_tp = allocate_matrix("charge_w_sys_ec_nov_tp", nov_rows, nov_cols);
		util::matrix_t<ssc_number_t> &charge_w_sys_ec_dec_tp = allocate_matrix("charge_w_sys_ec_dec_tp", dec_rows, dec_cols);


		util::matrix_t<ssc_number_t> &energy_w_sys_ec_jan_tp = allocate_matrix("energy_w_sys_ec_jan_tp", jan_rows, jan_cols);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_feb_tp = allocate_matrix("energy_w_sys_ec_feb_tp", feb_rows, feb_cols);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_mar_tp = allocate_matrix("energy_w_sys_ec_mar_tp", mar_rows, mar_cols);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_apr_tp = allocate_matrix("energy_w_sys_ec_apr_tp", apr_rows, apr_cols);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_may_tp = allocate_matrix("energy_w_sys_ec_may_tp", may_rows, may_cols);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_jun_tp = allocate_matrix("energy_w_sys_ec_jun_tp", jun_rows, jun_cols);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_jul_tp = allocate_matrix("energy_w_sys_ec_jul_tp", jul_rows, jul_cols);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_aug_tp = allocate_matrix("energy_w_sys_ec_aug_tp", aug_rows, aug_cols);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_sep_tp = allocate_matrix("energy_w_sys_ec_sep_tp", sep_rows, sep_cols);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_oct_tp = allocate_matrix("energy_w_sys_ec_oct_tp", oct_rows, oct_cols);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_nov_tp = allocate_matrix("energy_w_sys_ec_nov_tp", nov_rows, nov_cols);
		util::matrix_t<ssc_number_t> &energy_w_sys_ec_dec_tp = allocate_matrix("energy_w_sys_ec_dec_tp", dec_rows, dec_cols);


		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_jan_tp = allocate_matrix("surplus_w_sys_ec_jan_tp", jan_rows, jan_cols);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_feb_tp = allocate_matrix("surplus_w_sys_ec_feb_tp", feb_rows, feb_cols);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_mar_tp = allocate_matrix("surplus_w_sys_ec_mar_tp", mar_rows, mar_cols);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_apr_tp = allocate_matrix("surplus_w_sys_ec_apr_tp", apr_rows, apr_cols);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_may_tp = allocate_matrix("surplus_w_sys_ec_may_tp", may_rows, may_cols);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_jun_tp = allocate_matrix("surplus_w_sys_ec_jun_tp", jun_rows, jun_cols);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_jul_tp = allocate_matrix("surplus_w_sys_ec_jul_tp", jul_rows, jul_cols);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_aug_tp = allocate_matrix("surplus_w_sys_ec_aug_tp", aug_rows, aug_cols);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_sep_tp = allocate_matrix("surplus_w_sys_ec_sep_tp", sep_rows, sep_cols);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_oct_tp = allocate_matrix("surplus_w_sys_ec_oct_tp", oct_rows, oct_cols);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_nov_tp = allocate_matrix("surplus_w_sys_ec_nov_tp", nov_rows, nov_cols);
		util::matrix_t<ssc_number_t> &surplus_w_sys_ec_dec_tp = allocate_matrix("surplus_w_sys_ec_dec_tp", dec_rows, dec_cols);

		size_t tou_periods = rate.m_dc_tou_periods.size();
        util::matrix_t<ssc_number_t> &monthly_tou_demand_peak_w_sys = allocate_matrix("monthly_tou_demand_peak_w_sys", 13, tou_periods);
        util::matrix_t<ssc_number_t> &monthly_tou_demand_peak_wo_sys = allocate_matrix("monthly_tou_demand_peak_wo_sys", 13, tou_periods);
        util::matrix_t<ssc_number_t> &monthly_tou_demand_charge_w_sys = allocate_matrix("monthly_tou_demand_charge_w_sys", 13, tou_periods);
        util::matrix_t<ssc_number_t> &monthly_tou_demand_charge_wo_sys = allocate_matrix("monthly_tou_demand_charge_wo_sys", 13, tou_periods);

		// lifetime hourly load
        ssc_number_t* lifetime_load;
        if (as_integer("system_use_lifetime_output") == 1) 
		    lifetime_load = allocate("lifetime_load", nrec_gen);

		/*
		0=Single meter with monthly rollover credits in kWh
		1=Single meter with monthly rollover credits in $
		2=Single meter with no monthly rollover credits (Net Billing)
		3=Single meter with monthly rollover credits in $ (Net Billing $)
		4=Two meters with all generation sold and all load purchaseded
		*/
		int metering_option = as_integer("ur_metering_option");
		bool two_meter = (metering_option == 4 );
		bool timestep_reconciliation = (metering_option == 2 || metering_option == 3 || metering_option == 4);

		bool time_series_rates = as_boolean("ur_en_ts_sell_rate") || as_boolean("ur_en_ts_buy_rate");
		if (time_series_rates && !timestep_reconciliation)
		{
			throw exec_error("utilityrate5", "Time series rates are not compatible with net metering. Please disable time series rates or change to net billing / buy all - sell all");
		}

        bool dc_enabled = as_boolean("ur_dc_enable");
        rate.uses_billing_demand = rate.has_kwh_per_kw_rate() || dc_enabled;
        ssc_number_t* billing_demand_w_sys_ym = NULL;
        ssc_number_t* billing_demand_wo_sys_ym = NULL;
        if (rate.uses_billing_demand) {
            billing_demand_w_sys_ym = allocate("billing_demand_w_sys_ym", nyears + 1, 12);
            billing_demand_wo_sys_ym = allocate("billing_demand_wo_sys_ym", nyears + 1, 12);
        }

        // If billing demand ratchets - populate monthly_peaks with what the user specified as year zero:
        ssc_number_t* year_zero_peaks = NULL;
        size_t nrows;
        bool ratchets_enabled = as_boolean("ur_enable_billing_demand");
        if (ratchets_enabled) {

            if (!rate.uses_billing_demand) {
                std::ostringstream ss;
                ss << "The option ur_enable_billing_demand is only relevant when the energy rates have kWh/kW or kWh/kW daily units, please add those units to your rates structure or set ur_enable_billing_demand to false";
                throw exec_error("utilityrate5", ss.str());
            }

            year_zero_peaks = as_array("ur_yearzero_usage_peaks", &nrows);

            if (nrows != 12) {
                std::ostringstream ss;
                ss << "The ur_yearzero_usage_peaks array should have 12 rows. Instead it has " << nrows << " rows.";
                throw exec_error("utilityrate5", ss.str());
            }

            for (i = 0; i < nrows; i++) {
                monthly_billing_demand_peaks_wo_sys[i] = year_zero_peaks[i];
                monthly_billing_demand_peaks_w_sys[i] = year_zero_peaks[i];
            }
        }



		ur_month last_month_wo_sys;
		ssc_number_t last_excess_energy_wo_sys = 0;
		ssc_number_t last_excess_dollars_wo_sys = 0;
        ur_month last_month_w_sys;
        ssc_number_t last_excess_energy_w_sys = 0;
        ssc_number_t last_excess_dollars_w_sys = 0;

		idx = 0;
		for (i=0;i<nyears;i++)
		{
 			if (i > 0) {
				last_month_w_sys = rate.m_month[11];
				last_excess_energy_w_sys = monthly_cumulative_excess_energy_w_sys[11];
				last_excess_dollars_w_sys = monthly_cumulative_excess_dollars_w_sys[11];

                // last_month_wo_sys set below
                last_excess_energy_wo_sys = monthly_cumulative_excess_energy_wo_sys[11];
                last_excess_dollars_wo_sys = monthly_cumulative_excess_dollars_wo_sys[11];
			}
			for (j = 0; j<m_num_rec_yearly; j++)
			{
				/* for future implementation for lifetime loads
				// update e_load and p_load per year if lifetime output
				// lifetime load values? sell values
				if ((as_integer("system_use_lifetime_output") == 1) && (idx < nrec_load))
				{
					e_load[j] = p_load[j] = 0.0;
					for (size_t ii = 0; (ii < step_per_hour_load) && (idx < nrec_load); ii++)
					{
						ts_load = (bload ? pload[idx] : 0);
						e_load[i] += ts_load * ts_hour_load;
						p_load[i] = ((ts_load > p_load[i]) ? ts_load : p_load[i]);
						idx++;
					}
					lifetime_hourly_load[i*8760 + j] = e_load[i];
					// sign correction for utility rate calculations
					e_load[i] = -e_load[i];
					p_load[i] = -p_load[i];
				}
				*/
				// apply load escalation appropriate for current year
//				e_load_cy[j] = e_load[j] * load_scale[i];
//				p_load_cy[j] = p_load[j] * load_scale[i];
				e_load_cy[j] = p_load[j] * load_scale[i] * ts_hour_gen;
				p_load_cy[j] = p_load[j] * load_scale[i];

                if (grid_outage[j]) {
                    // update e_sys per year if lifetime output
                    if ((as_integer("system_use_lifetime_output") == 1) && (idx < nrec_gen))
                    {
                        e_sys_cy[j] = 0.0;
                        p_sys_cy[j] = 0.0;
                        // until lifetime load fully implemented
                        // report lifetime load in kW and not kWh
                        lifetime_load[idx] = 0.0;
                        idx++;
                    }
                    else
                    {
                        e_sys_cy[j] = 0.0;
                        p_sys_cy[j] = 0.0;
                    }
                }
                else {
                    // update e_sys per year if lifetime output
                    if ((as_integer("system_use_lifetime_output") == 1) && (idx < nrec_gen))
                    {
                        //					e_sys[j] = p_sys[j] = 0.0;
                        //					ts_power = (idx < nrec_gen) ? pgen[idx] : 0;
                        //					e_sys[j] = ts_power * ts_hour_gen;
                        //					p_sys[j] = ((ts_power > p_sys[j]) ? ts_power : p_sys[j]);
                        e_sys_cy[j] = pgen[idx] * ts_hour_gen;
                        p_sys_cy[j] = pgen[idx];
                        // until lifetime load fully implemented
                        // report lifetime load in kW and not kWh
                        lifetime_load[idx] = -e_load_cy[j] / ts_hour_gen;
                        idx++;
                    }
                    else
                    {
                        e_sys_cy[j] = pgen[j] * ts_hour_gen;
                        p_sys_cy[j] = pgen[j];
                    }
                }
                e_sys_cy[j] *= sys_scale[i];
                p_sys_cy[j] *= sys_scale[i];
                // calculate e_grid value (e_sys + e_load)
//				e_sys_cy[j] = e_sys[j] * sys_scale[i];
//				p_sys_cy[j] = p_sys[j] * sys_scale[i];
                // note: load is assumed to have negative sign
                e_grid_cy[j] = e_sys_cy[j] + e_load_cy[j];
                p_grid_cy[j] = p_sys_cy[j] + p_load_cy[j];
            }


			// now calculate revenue without solar system (using load only)
			if (timestep_reconciliation)
			{
				ur_calc_timestep(&e_load_cy[0], &p_load_cy[0],
					&revenue_wo_sys[0], &payment[0], &income[0], &demand_charge_wo_sys[0], &energy_charge_wo_sys[0],
					&monthly_fixed_charges[0], &monthly_minimum_charges[0],
					&monthly_ec_charges[0],
					&monthly_ec_charges_gross[0],
					&monthly_excess_dollars_earned[0],
					&monthly_excess_kwhs_earned[0],
                    &monthly_net_billing_credits[0],
					&rate.dc_hourly_peak[0], &monthly_cumulative_excess_energy_wo_sys[0],
					&monthly_cumulative_excess_dollars_wo_sys[0], &monthly_bill[0],
                    &monthly_two_meter_sales[0],
                    &monthly_true_up_credits[0],
                    &monthly_billing_demand_peaks_wo_sys[0],
                    rate.rate_scale[i], i,
					last_excess_dollars_wo_sys);
			}
			else
			{
				ur_calc(&e_load_cy[0], &p_load_cy[0],
					&revenue_wo_sys[0], &payment[0], &income[0], &demand_charge_wo_sys[0], &energy_charge_wo_sys[0],
					&monthly_fixed_charges[0], &monthly_minimum_charges[0],
					&monthly_ec_charges[0],
					&monthly_ec_charges_gross[0],
					&monthly_excess_dollars_earned[0],
					&monthly_nm_dollars_applied[0],
					&monthly_excess_kwhs_earned[0],
					&rate.dc_hourly_peak[0], &monthly_cumulative_excess_energy_wo_sys[0],
					&monthly_cumulative_excess_dollars_wo_sys[0], &monthly_bill[0],
                    &monthly_true_up_credits[0],
                    &monthly_billing_demand_peaks_wo_sys[0],
                    rate.rate_scale[i], i,
					&last_month_wo_sys, last_excess_energy_wo_sys, last_excess_dollars_wo_sys);
			}

            last_month_wo_sys = ur_month(rate.m_month[11]); // Deep copy now so it's available next y

			for (j = 0; j < 12; j++)
			{
				utility_bill_wo_sys_ym[(i + 1) * 12 + j] = monthly_bill[j];
				ch_wo_sys_dc_fixed_ym[(i + 1) * 12 + j] = rate.monthly_dc_fixed[j];
				ch_wo_sys_dc_tou_ym[(i + 1) * 12 + j] = rate.monthly_dc_tou[j];
				ch_wo_sys_ec_ym[(i + 1) * 12 + j] = monthly_ec_charges[j];
				//ch_wo_sys_ec_flat_ym[(i + 1) * 12 + j] = monthly_ec_flat_charges[j];
				ch_wo_sys_fixed_ym[(i + 1) * 12 + j] = monthly_fixed_charges[j];
				ch_wo_sys_minimum_ym[(i + 1) * 12 + j] = monthly_minimum_charges[j];

                if (rate.uses_billing_demand) {
                    billing_demand_wo_sys_ym[(i + 1) * 12 + j] = rate.billing_demand[j];
                }
                if (rate.en_billing_demand_lookback) {
                    double monthly_peak_demand = 0.0;
                    std::vector<int> dc_periods = rate.m_month[j].dc_periods;
                    if (dc_periods.size() == 0) {
                        monthly_peak_demand = rate.m_month[j].dc_flat_peak;
                    }
                    else {
                        for (size_t p = 0; p < dc_periods.size(); p++) {
                            double tou_demand = rate.m_month[j].dc_tou_peak[p];
                            if (rate.bd_tou_periods.at(dc_periods[p]) && tou_demand > monthly_peak_demand) {
                                monthly_peak_demand = tou_demand;
                            }
                        }
                    }
                    monthly_billing_demand_peaks_wo_sys[j] = monthly_peak_demand;
                }

				utility_bill_wo_sys[i + 1] += monthly_bill[j];
				ch_wo_sys_dc_fixed[i + 1] += rate.monthly_dc_fixed[j];
				ch_wo_sys_dc_tou[i + 1] += rate.monthly_dc_tou[j];
				ch_wo_sys_ec[i + 1] += monthly_ec_charges[j];
				//ch_wo_sys_ec_flat[i + 1] += monthly_ec_flat_charges[j];
				ch_wo_sys_fixed[i + 1] += monthly_fixed_charges[j];
				ch_wo_sys_minimum[i + 1] += monthly_minimum_charges[j];
			}


			if (i == 0)
			{
				ur_update_ec_monthly(0, charge_wo_sys_ec_jan_tp, energy_wo_sys_ec_jan_tp, surplus_wo_sys_ec_jan_tp);
				ur_update_ec_monthly(1, charge_wo_sys_ec_feb_tp, energy_wo_sys_ec_feb_tp, surplus_wo_sys_ec_feb_tp);
				ur_update_ec_monthly(2, charge_wo_sys_ec_mar_tp, energy_wo_sys_ec_mar_tp, surplus_wo_sys_ec_mar_tp);
				ur_update_ec_monthly(3, charge_wo_sys_ec_apr_tp, energy_wo_sys_ec_apr_tp, surplus_wo_sys_ec_apr_tp);
				ur_update_ec_monthly(4, charge_wo_sys_ec_may_tp, energy_wo_sys_ec_may_tp, surplus_wo_sys_ec_may_tp);
				ur_update_ec_monthly(5, charge_wo_sys_ec_jun_tp, energy_wo_sys_ec_jun_tp, surplus_wo_sys_ec_jun_tp);
				ur_update_ec_monthly(6, charge_wo_sys_ec_jul_tp, energy_wo_sys_ec_jul_tp, surplus_wo_sys_ec_jul_tp);
				ur_update_ec_monthly(7, charge_wo_sys_ec_aug_tp, energy_wo_sys_ec_aug_tp, surplus_wo_sys_ec_aug_tp);
				ur_update_ec_monthly(8, charge_wo_sys_ec_sep_tp, energy_wo_sys_ec_sep_tp, surplus_wo_sys_ec_sep_tp);
				ur_update_ec_monthly(9, charge_wo_sys_ec_oct_tp, energy_wo_sys_ec_oct_tp, surplus_wo_sys_ec_oct_tp);
				ur_update_ec_monthly(10, charge_wo_sys_ec_nov_tp, energy_wo_sys_ec_nov_tp, surplus_wo_sys_ec_nov_tp);
				ur_update_ec_monthly(11, charge_wo_sys_ec_dec_tp, energy_wo_sys_ec_dec_tp, surplus_wo_sys_ec_dec_tp);

                    // demand peak without system
                for (int irow = 0; irow <= 12; irow++)
                {
                    for (int icol = 0; icol < (int)rate.m_dc_tou_periods.size(); icol++)
                    {
                        if (irow == 0)
                        {
                            monthly_tou_demand_peak_wo_sys.at(0, icol) = (float)rate.m_dc_tou_periods[icol];
                            monthly_tou_demand_charge_wo_sys.at(0, icol) = (float)rate.m_dc_tou_periods[icol];
                        }
                        else
                        {
                            int ndx = -1;
                            int period = rate.m_dc_tou_periods[icol];
                            std::vector<int>::iterator result = std::find(rate.m_month[irow - 1].dc_periods.begin(), rate.m_month[irow - 1].dc_periods.end(), period);
                            if (result == rate.m_month[irow - 1].dc_periods.end())
                            {
                                monthly_tou_demand_peak_wo_sys.at(irow, icol) = 0;
                                monthly_tou_demand_charge_wo_sys.at(irow, icol) = 0;
                            }
                            else
                            {
                                ndx = (int)(result - rate.m_month[irow - 1].dc_periods.begin());
                                if (ndx > -1 && ndx < (int)rate.m_month[irow - 1].dc_tou_peak.size())
                                    monthly_tou_demand_peak_wo_sys.at(irow, icol) = rate.m_month[irow - 1].dc_tou_peak[ndx];
                                if (ndx > -1 && ndx < (int)rate.m_month[irow - 1].dc_tou_charge.size())
                                    monthly_tou_demand_charge_wo_sys.at(irow, icol) = (float)rate.m_month[irow - 1].dc_tou_charge[ndx];

                            }
                        }
                    }
                }


				assign("year1_hourly_dc_without_system", var_data(&demand_charge_wo_sys[0], m_num_rec_yearly));
				assign("year1_hourly_ec_without_system", var_data(&energy_charge_wo_sys[0], m_num_rec_yearly));

				assign("year1_monthly_dc_fixed_without_system", var_data(&rate.monthly_dc_fixed[0], 12));
				assign( "year1_monthly_dc_tou_without_system", var_data(&rate.monthly_dc_tou[0], 12) );
				assign("year1_monthly_ec_charge_without_system", var_data(&monthly_ec_charges[0], 12));

                if (rate.uses_billing_demand) {
                    assign("year1_billing_demand_wo_sys", var_data(&rate.billing_demand[0], 12));
                }

				// sign reversal based on 9/5/13 meeting, reverse again 9/6/13
				for (int ii = 0; ii<(int)m_num_rec_yearly; ii++)
				{
					salespurchases[ii] = revenue_wo_sys[ii];
				}

				int c = 0;

				for (int m=0;m<12;m++)
				{
					monthly_salespurchases[m] = 0;
					for (size_t d=0;d<util::nday[m];d++)
					{
						for(int h=0;h<24;h++)
						{
							for (size_t s = 0; s < step_per_hour_gen; s++)
							{
								monthly_salespurchases[m] += salespurchases[c];
								c++;
							}
						}
					}
				}
				assign("year1_hourly_salespurchases_without_system", var_data(&salespurchases[0], (int)m_num_rec_yearly));
				assign("year1_monthly_utility_bill_wo_sys", var_data(&monthly_bill[0], 12));
				assign("year1_monthly_fixed_without_system", var_data(&monthly_fixed_charges[0], 12));
				assign("year1_monthly_minimum_without_system", var_data(&monthly_minimum_charges[0], 12));

				// testing energy use
				for (int ii = 0; ii < 12; ii++)
				{
					monthly_test[ii] = -rate.m_month[ii].energy_net;
                    monthly_peak_wo_sys[ii] = rate.m_month[ii].dc_flat_peak;
				}
				assign("year1_monthly_peak_wo_system", var_data(&monthly_peak_wo_sys[0], 12));
				assign("year1_monthly_use_wo_system", var_data(&monthly_test[0], 12));
			}

// with system

			if (timestep_reconciliation)
			{
				if (two_meter)
				{
					ur_calc_timestep(&e_sys_cy[0], &p_sys_cy[0],
						&revenue_w_sys[0], &payment[0], &income[0],
						&demand_charge_w_sys[0], &energy_charge_w_sys[0],
						&monthly_fixed_charges[0], &monthly_minimum_charges[0],
						&monthly_ec_charges[0],
						&monthly_ec_charges_gross[0],
						&monthly_excess_dollars_earned[0],
						&monthly_excess_kwhs_earned[0],
                        &monthly_net_billing_credits[0],
						&rate.dc_hourly_peak[0], &monthly_cumulative_excess_energy_w_sys[0], &monthly_cumulative_excess_dollars_w_sys[0], &monthly_bill[0],
                        &monthly_two_meter_sales[0], &monthly_true_up_credits[0],
                        &monthly_billing_demand_peaks_w_sys[0],
                        rate.rate_scale[i],
						i, last_excess_dollars_w_sys, false, false, true);
				}
				else
				{
					ur_calc_timestep(&e_grid_cy[0], &p_grid_cy[0],
						&revenue_w_sys[0], &payment[0], &income[0],
						&demand_charge_w_sys[0], &energy_charge_w_sys[0],
						&monthly_fixed_charges[0], &monthly_minimum_charges[0],
						&monthly_ec_charges[0],
						&monthly_ec_charges_gross[0],
						&monthly_excess_dollars_earned[0],
						&monthly_excess_kwhs_earned[0],
                        &monthly_net_billing_credits[0],
						&rate.dc_hourly_peak[0], &monthly_cumulative_excess_energy_w_sys[0], &monthly_cumulative_excess_dollars_w_sys[0],
						&monthly_bill[0], &monthly_two_meter_sales[0],
                        &monthly_true_up_credits[0],
                        &monthly_billing_demand_peaks_w_sys[0],
                        rate.rate_scale[i], i, last_excess_dollars_w_sys);
				}
			}
			else // monthly reconciliation per 2015.6.30 release
			{
				// Two-meter always hits the timestep_reconciliation path, so ur_calc will never be called for buy all / sell all
				// calculate revenue with solar system (using net grid energy & maxpower)
				ur_calc(&e_grid_cy[0], &p_grid_cy[0],
					&revenue_w_sys[0], &payment[0], &income[0],
					&demand_charge_w_sys[0], &energy_charge_w_sys[0],
					&monthly_fixed_charges[0], &monthly_minimum_charges[0],
					&monthly_ec_charges[0],
					&monthly_ec_charges_gross[0],
					&monthly_excess_dollars_earned[0],
					&monthly_nm_dollars_applied[0],
					&monthly_excess_kwhs_earned[0],
					&rate.dc_hourly_peak[0], &monthly_cumulative_excess_energy_w_sys[0], &monthly_cumulative_excess_dollars_w_sys[0],
					&monthly_bill[0], &monthly_true_up_credits[0],
                    &monthly_billing_demand_peaks_w_sys[0],
                    rate.rate_scale[i], i,
					&last_month_w_sys, last_excess_energy_w_sys, last_excess_dollars_w_sys);
			}
			if (two_meter)
			{
				// TODO - remove annual_revenue and just use annual bill
				// Two meters - adjust output accordingly
				for (j = 0; j < m_num_rec_yearly; j++)
				{
					revenue_w_sys[j] += revenue_wo_sys[j]; // watch sign
					annual_revenue_w_sys[i + 1] += revenue_w_sys[j] - revenue_wo_sys[j];
					energy_charge_w_sys[j] += energy_charge_wo_sys[j];
					energy_charge_gross_w_sys[j] += energy_charge_wo_sys[j];
					demand_charge_w_sys[j] += demand_charge_wo_sys[j];
				}
				// adjust monthly outputs as sum of both meters = system meter + load meter

				for (j = 0; j < 12; j++)
				{
					rate.monthly_dc_fixed[j] += ch_wo_sys_dc_fixed_ym[(i + 1) * 12 + j];
                    rate.monthly_dc_tou[j] += ch_wo_sys_dc_tou_ym[(i + 1) * 12 + j];
					monthly_ec_charges[j] += ch_wo_sys_ec_ym[(i + 1) * 12 + j];
					monthly_ec_charges_gross[j] += ch_wo_sys_ec_ym[(i + 1) * 12 + j];
					monthly_fixed_charges[j] += ch_wo_sys_fixed_ym[(i + 1) * 12 + j];
					monthly_minimum_charges[j] += ch_wo_sys_minimum_ym[(i + 1) * 12 + j];
					monthly_bill[j] += utility_bill_wo_sys_ym[(i + 1) * 12 + j];
				}

				if (i == 0)
				{

					// for each month add the wo system charge and energy
					// note that first row contains tier num and first column contains period numbers in the charge_wo_sys_ec and energy_wo_sys_ec matrices
                    // for github ssc issue 539, check that the kWh are not maxed out before adding to same tier
					for (int m = 0; m < (int)rate.m_month.size(); m++)
					{
						ur_month& curr_month = rate.m_month[m];
						for (size_t ir = 0; ir < curr_month.ec_charge.nrows(); ir++)
						{
                            ssc_number_t energy_above_ub_total = 0;
                            ssc_number_t prev_upper_bound = 0;
                            for (size_t  ic = 0; ic < curr_month.ec_charge.ncols(); ic++)
							{
                                ssc_number_t charge_adj = 0;
                                ssc_number_t energy_adj = 0;
                                switch (m)
								{
								case 0:
									charge_adj = charge_wo_sys_ec_jan_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_jan_tp.at(ir + 1, ic + 1);
									break;
								case 1:
									charge_adj = charge_wo_sys_ec_feb_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_feb_tp.at(ir + 1, ic + 1);
									break;
								case 2:
									charge_adj = charge_wo_sys_ec_mar_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_mar_tp.at(ir + 1, ic + 1);
									break;
								case 3:
									charge_adj = charge_wo_sys_ec_apr_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_apr_tp.at(ir + 1, ic + 1);
									break;
								case 4:
									charge_adj = charge_wo_sys_ec_may_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_may_tp.at(ir + 1, ic + 1);
									break;
								case 5:
									charge_adj = charge_wo_sys_ec_jun_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_jun_tp.at(ir + 1, ic + 1);
									break;
								case 6:
									charge_adj = charge_wo_sys_ec_jul_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_jul_tp.at(ir + 1, ic + 1);
									break;
								case 7:
									charge_adj = charge_wo_sys_ec_aug_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_aug_tp.at(ir + 1, ic + 1);
									break;
								case 8:
									charge_adj = charge_wo_sys_ec_sep_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_sep_tp.at(ir + 1, ic + 1);
									break;
								case 9:
									charge_adj = charge_wo_sys_ec_oct_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_oct_tp.at(ir + 1, ic + 1);
									break;
								case 10:
									charge_adj = charge_wo_sys_ec_nov_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_nov_tp.at(ir + 1, ic + 1);
									break;
								case 11:
									charge_adj = charge_wo_sys_ec_dec_tp.at(ir + 1, ic + 1);
									energy_adj = energy_wo_sys_ec_dec_tp.at(ir + 1, ic + 1);
									break;
                                default: // should never reach here!
                                    energy_adj = 0;
                                    charge_adj = 0;
								}
                                curr_month.ec_charge.at(ir, ic) += charge_adj;
                                curr_month.ec_energy_use.at(ir, ic) += energy_adj;
                                ssc_number_t energy_above_ub = curr_month.ec_energy_use.at(ir, ic) + prev_upper_bound - curr_month.ec_tou_ub(ir, ic);
                                if (energy_above_ub > 0) {
                                    curr_month.ec_energy_use.at(ir, ic) = curr_month.ec_tou_ub(ir, ic) - prev_upper_bound;
                                    curr_month.ec_charge.at(ir, ic) = curr_month.ec_energy_use.at(ir, ic) * curr_month.ec_tou_br(ir, ic);
                                    energy_above_ub_total += energy_above_ub;
                                }
                                else if (energy_above_ub_total > 0) {
                                    ssc_number_t additional_energy_for_tier = energy_above_ub_total + energy_above_ub;
                                    if (additional_energy_for_tier > 0) {
                                        curr_month.ec_energy_use.at(ir, ic) -= energy_above_ub; // subtracting a negative number
                                        curr_month.ec_charge.at(ir, ic) -= energy_above_ub * curr_month.ec_tou_br(ir, ic);
                                        energy_above_ub_total = additional_energy_for_tier;
                                    }
                                    else { // apply all energy_above_ub_total to this tier
                                        curr_month.ec_energy_use.at(ir, ic) += energy_above_ub_total;
                                        curr_month.ec_charge.at(ir, ic) += energy_above_ub_total * curr_month.ec_tou_br(ir, ic);
                                        energy_above_ub_total = 0;
                                    }
                                }
                                prev_upper_bound = curr_month.ec_tou_ub(ir, ic);
                            }
						}
					}
				} // i==0 (first year matrix outputs)
			} // two meter metering option

			if (i == 0)
			{
				ur_update_ec_monthly(0, charge_w_sys_ec_jan_tp, energy_w_sys_ec_jan_tp, surplus_w_sys_ec_jan_tp);
				ur_update_ec_monthly(1, charge_w_sys_ec_feb_tp, energy_w_sys_ec_feb_tp, surplus_w_sys_ec_feb_tp);
				ur_update_ec_monthly(2, charge_w_sys_ec_mar_tp, energy_w_sys_ec_mar_tp, surplus_w_sys_ec_mar_tp);
				ur_update_ec_monthly(3, charge_w_sys_ec_apr_tp, energy_w_sys_ec_apr_tp, surplus_w_sys_ec_apr_tp);
				ur_update_ec_monthly(4, charge_w_sys_ec_may_tp, energy_w_sys_ec_may_tp, surplus_w_sys_ec_may_tp);
				ur_update_ec_monthly(5, charge_w_sys_ec_jun_tp, energy_w_sys_ec_jun_tp, surplus_w_sys_ec_jun_tp);
				ur_update_ec_monthly(6, charge_w_sys_ec_jul_tp, energy_w_sys_ec_jul_tp, surplus_w_sys_ec_jul_tp);
				ur_update_ec_monthly(7, charge_w_sys_ec_aug_tp, energy_w_sys_ec_aug_tp, surplus_w_sys_ec_aug_tp);
				ur_update_ec_monthly(8, charge_w_sys_ec_sep_tp, energy_w_sys_ec_sep_tp, surplus_w_sys_ec_sep_tp);
				ur_update_ec_monthly(9, charge_w_sys_ec_oct_tp, energy_w_sys_ec_oct_tp, surplus_w_sys_ec_oct_tp);
				ur_update_ec_monthly(10, charge_w_sys_ec_nov_tp, energy_w_sys_ec_nov_tp, surplus_w_sys_ec_nov_tp);
				ur_update_ec_monthly(11, charge_w_sys_ec_dec_tp, energy_w_sys_ec_dec_tp, surplus_w_sys_ec_dec_tp);

				// demand peak with system
                for (int irow = 0; irow <= 12; irow++)
                {
                    for (int icol = 0; icol < (int)rate.m_dc_tou_periods.size(); icol++)
                    {
                        if (irow == 0)
                        {
                            monthly_tou_demand_peak_w_sys.at(0, icol) = (float)rate.m_dc_tou_periods[icol];
                            monthly_tou_demand_charge_w_sys.at(0, icol) = (float)rate.m_dc_tou_periods[icol];
                        }
                        else
                        {
                            ur_month& curr_month = rate.m_month[irow - 1];
                            int ndx = -1;
                            int period = rate.m_dc_tou_periods[icol];
                            std::vector<int>::iterator result = std::find(curr_month.dc_periods.begin(), curr_month.dc_periods.end(), period);
                            if (result == curr_month.dc_periods.end())
                            {
                                monthly_tou_demand_peak_w_sys.at(irow, icol) = 0;
                                monthly_tou_demand_charge_w_sys.at(irow, icol) = 0;
                            }
                            else
                            {
                                ndx = (int)(result - curr_month.dc_periods.begin());
                                if (ndx > -1 && ndx < (int)curr_month.dc_tou_peak.size())
                                    monthly_tou_demand_peak_w_sys.at(irow, icol) = curr_month.dc_tou_peak[ndx];
                                if (ndx > -1 && ndx < (int)curr_month.dc_tou_charge.size())
                                    monthly_tou_demand_charge_w_sys.at(irow, icol) = (float)curr_month.dc_tou_charge[ndx];
                            }
                        }
                    }
                }
				assign("year1_hourly_dc_with_system", var_data(&demand_charge_w_sys[0], (int)m_num_rec_yearly));
				assign("year1_hourly_ec_with_system", var_data(&energy_charge_w_sys[0], (int)m_num_rec_yearly));
				assign("year1_hourly_dc_peak_per_period", var_data(&rate.dc_hourly_peak[0], (int)m_num_rec_yearly));

				// sign reversal based on 9/5/13 meeting reverse again 9/6/13
				for (int ii = 0; ii<(int)m_num_rec_yearly; ii++)
				{
					ec_tou_sched[ii] = (ssc_number_t)rate.m_ec_tou_sched[ii];
					dc_tou_sched[ii] = (ssc_number_t)rate.m_dc_tou_sched[ii];
					load[ii] = -e_load_cy[ii];
					e_tofromgrid[ii] = e_grid_cy[ii];

                    if (fabs(e_tofromgrid[ii]) < powerflow_tolerance) { // powerflow_tolerance is defined globally in shared/lib_battery_powerflow.h, set to 0.000005 (Watts or Watt-hrs)
                        e_tofromgrid[ii] = 0.0;
                    }
					if (e_tofromgrid[ii] > 0)
					{
						year1_hourly_e_togrid[ii] = e_tofromgrid[ii];
						year1_hourly_e_fromgrid[ii] = 0.0;
					}
					else
					{
						year1_hourly_e_togrid[ii] = 0.0;
						year1_hourly_e_fromgrid[ii] = -e_tofromgrid[ii];
					}
					p_tofromgrid[ii] = std::fabs(p_grid_cy[ii]) > powerflow_tolerance ? p_grid_cy[ii] : 0.0;
					salespurchases[ii] = revenue_w_sys[ii];
				}
				assign("year1_hourly_ec_tou_schedule", var_data(&ec_tou_sched[0], (int)m_num_rec_yearly));
				assign("year1_hourly_dc_tou_schedule", var_data(&dc_tou_sched[0], (int)m_num_rec_yearly));
				// monthly outputs - Paul and Sean 7/29/13 - updated 8/9/13 and 8/12/13 and 9/10/13
				monthly_outputs(&e_load_cy[0], &e_sys_cy[0], &e_grid_cy[0], &salespurchases[0],
					&monthly_load[0], &monthly_system_generation[0], &monthly_elec_to_grid[0],
					&monthly_elec_needed_from_grid[0],
					&monthly_salespurchases[0]);

				assign("year1_hourly_e_tofromgrid", var_data(&e_tofromgrid[0], (int)m_num_rec_yearly));
				assign("year1_hourly_p_tofromgrid", var_data(&p_tofromgrid[0], (int)m_num_rec_yearly));
				assign("bill_load", var_data(&load[0], (int)m_num_rec_yearly));
				assign("year1_hourly_salespurchases_with_system", var_data(&salespurchases[0], (int)m_num_rec_yearly));
				assign("year1_monthly_load", var_data(&monthly_load[0], 12));
				assign("year1_monthly_system_generation", var_data(&monthly_system_generation[0], 12));
				assign("year1_monthly_electricity_to_grid", var_data(&monthly_elec_to_grid[0], 12));
				assign("year1_monthly_electricity_needed_from_grid", var_data(&monthly_elec_needed_from_grid[0], 12));

				assign("year1_monthly_cumulative_excess_generation", var_data(&monthly_cumulative_excess_energy_w_sys[0], 12));
				assign("year1_monthly_utility_bill_w_sys", var_data(&monthly_bill[0], 12));

				// output and demand per Paul's email 9/10/10
				// positive demand indicates system does not produce enough electricity to meet load
				// zero if the system produces more than the demand
				std::vector<ssc_number_t> output(m_num_rec_yearly), edemand(m_num_rec_yearly), pdemand(m_num_rec_yearly), e_sys_to_grid(m_num_rec_yearly), e_sys_to_load(m_num_rec_yearly), p_sys_to_load(m_num_rec_yearly);
				for (j = 0; j<m_num_rec_yearly; j++)
				{
					output[j] = e_sys_cy[j];
					edemand[j] = e_grid_cy[j] < 0.0 ? -e_grid_cy[j] : (ssc_number_t)0.0;
					pdemand[j] = p_grid_cy[j] < 0.0 ? -p_grid_cy[j] : (ssc_number_t)0.0;

					ssc_number_t sys_e_net = output[j] + e_load_cy[j];// loads are assumed negative
					e_sys_to_grid[j] = sys_e_net > 0 ? sys_e_net : (ssc_number_t)0.0;
					e_sys_to_load[j] = sys_e_net > 0 ? -e_load_cy[j] : output[j];

//					ssc_number_t sys_p_net = output[j] + p_load[j];// loads are assumed negative
//					p_sys_to_load[j] = sys_p_net > 0 ? -p_load[j] : output[j];
					ssc_number_t sys_p_net = output[j] + p_load_cy[j];// loads are assumed negative
					p_sys_to_load[j] = sys_p_net > 0 ? -p_load_cy[j] : output[j];
				}

				assign("year1_hourly_system_output", var_data(&output[0], (int)m_num_rec_yearly));
				assign("year1_hourly_e_demand", var_data(&edemand[0], (int)m_num_rec_yearly));
				assign("year1_hourly_p_demand", var_data(&pdemand[0], (int)m_num_rec_yearly));

				assign("year1_hourly_system_to_load", var_data(&e_sys_to_load[0], (int)m_num_rec_yearly));
				assign("year1_hourly_p_system_to_load", var_data(&p_sys_to_load[0], (int)m_num_rec_yearly));

				assign("year1_monthly_fixed_with_system", var_data(&monthly_fixed_charges[0], 12));
				assign("year1_monthly_minimum_with_system", var_data(&monthly_minimum_charges[0], 12));
				assign("year1_monthly_dc_fixed_with_system", var_data(&rate.monthly_dc_fixed[0], 12));
				assign("year1_monthly_dc_tou_with_system", var_data(&rate.monthly_dc_tou[0], 12));
				assign("year1_monthly_ec_charge_with_system", var_data(&monthly_ec_charges[0], 12));
				assign("year1_monthly_ec_charge_gross_with_system", var_data(&monthly_ec_charges_gross[0], 12));
				assign("year1_nm_dollars_applied", var_data(&monthly_nm_dollars_applied[0], 12));
				assign("year1_net_billing_credits", var_data(&monthly_net_billing_credits[0], 12));
				assign("year1_excess_kwhs_earned", var_data(&monthly_excess_kwhs_earned[0], 12));
                assign("year1_two_meter_sales", var_data(&monthly_two_meter_sales[0], 12));
                assign("year1_true_up_credits", var_data(&monthly_true_up_credits[0], 12));
				// peak demand and testing energy use
				for (int ii = 0; ii < 12; ii++)
				{
                    monthly_peak_w_sys[ii] = rate.m_month[ii].dc_flat_peak;
					monthly_test[ii] = -rate.m_month[ii].energy_net;
				}
				assign("year1_monthly_peak_w_system", var_data(&monthly_peak_w_sys[0], 12));
				assign("year1_monthly_use_w_system", var_data(&monthly_test[0], 12));

                if (rate.uses_billing_demand) {
                    assign("year1_billing_demand_w_sys", var_data(&rate.billing_demand[0], 12));
                }

			}

			// determine net-revenue benefit due to solar for year 'i'

			annual_net_revenue[i+1] = 0.0;
			annual_electric_load[i + 1] = 0.0;
			energy_net[i + 1] = 0.0;
			annual_revenue_w_sys[i + 1] = 0.0;
			annual_revenue_wo_sys[i + 1] = 0.0;

			for (j = 0; j<m_num_rec_yearly; j++)
			{
				energy_net[i + 1] +=  e_sys_cy[j];
				annual_net_revenue[i + 1] += revenue_w_sys[j] - revenue_wo_sys[j];
				annual_electric_load[i + 1] += -e_load_cy[j];
				annual_revenue_w_sys[i + 1] += revenue_w_sys[j];
				annual_revenue_wo_sys[i + 1] += revenue_wo_sys[j];
			}

			for (j = 0; j < 12; j++)
			{
				utility_bill_w_sys_ym[(i+1)*12 + j] = monthly_bill[j];
				ch_w_sys_dc_fixed_ym[(i + 1) * 12 + j] = rate.monthly_dc_fixed[j];
				ch_w_sys_dc_tou_ym[(i + 1) * 12 + j] = rate.monthly_dc_tou[j];
				ch_w_sys_ec_ym[(i + 1) * 12 + j] = monthly_ec_charges[j];

				ch_w_sys_ec_gross_ym[(i + 1) * 12 + j] = monthly_ec_charges_gross[j];
				nm_dollars_applied_ym[(i + 1) * 12 + j] = monthly_nm_dollars_applied[j];
				excess_kwhs_earned_ym[(i + 1) * 12 + j] = monthly_excess_kwhs_earned[j];
                net_billing_credits_ym[(i + 1) * 12 + j] = monthly_net_billing_credits[j];
                two_meter_sales_ym[(i + 1) * 12 + j] = monthly_two_meter_sales[j];
                true_up_credits_ym[(i + 1) * 12 + j] = monthly_true_up_credits[j];

				ch_w_sys_fixed_ym[(i + 1) * 12 + j] = monthly_fixed_charges[j];
				ch_w_sys_minimum_ym[(i + 1) * 12 + j] = monthly_minimum_charges[j];

                if (rate.uses_billing_demand) {
                    billing_demand_w_sys_ym[(i + 1) * 12 + j] = rate.billing_demand[j];
                }
                if (rate.en_billing_demand_lookback) {
                    double monthly_peak_demand = 0.0;
                    std::vector<int> dc_periods = rate.m_month[j].dc_periods;
                    if (dc_periods.size() == 0) {
                        monthly_peak_demand = rate.m_month[j].dc_flat_peak;
                    }
                    else {
                        for (size_t p = 0; p < dc_periods.size(); p++) {
                            double tou_demand = rate.m_month[j].dc_tou_peak[p];
                            if (rate.bd_tou_periods.at(dc_periods[p]) && tou_demand > monthly_peak_demand) {
                                monthly_peak_demand = tou_demand;
                            }
                        }
                    }
                    monthly_billing_demand_peaks_w_sys[j] = monthly_peak_demand;
                }

				utility_bill_w_sys[i + 1] += monthly_bill[j];
				ch_w_sys_dc_fixed[i + 1] += rate.monthly_dc_fixed[j];
				ch_w_sys_dc_tou[i + 1] += rate.monthly_dc_tou[j];
				ch_w_sys_ec[i + 1] += monthly_ec_charges[j];
				ch_w_sys_fixed[i + 1] += monthly_fixed_charges[j];
				ch_w_sys_minimum[i + 1] += monthly_minimum_charges[j];

			}


		}

        assign("utility_bill_w_sys_year1", utility_bill_w_sys[1]);
        assign("utility_bill_wo_sys_year1", utility_bill_wo_sys[1]);
        assign("savings_year1", utility_bill_wo_sys[1] - utility_bill_w_sys[1]);

        if (!dc_enabled) {
            unassign("monthly_tou_demand_peak_w_sys");
            unassign("monthly_tou_demand_peak_wo_sys");
            unassign("monthly_tou_demand_charge_w_sys");
            unassign("monthly_tou_demand_charge_wo_sys");
        }


	}

	void monthly_outputs(ssc_number_t *e_load, ssc_number_t *e_sys, ssc_number_t *e_grid, ssc_number_t *salespurchases, ssc_number_t monthly_load[12], ssc_number_t monthly_generation[12], ssc_number_t monthly_elec_to_grid[12], ssc_number_t monthly_elec_needed_from_grid[12], ssc_number_t monthly_salespurchases[12])
	{
		// calculate the monthly net energy and monthly hours
		int m,h,s;
		size_t d;
		ssc_number_t energy_use[12]; // 12 months
		int c=0;

		size_t steps_per_hour = m_num_rec_yearly / 8760;
		for (m=0;m<12;m++)
		{
			energy_use[m] = 0;
			monthly_load[m] = 0;
			monthly_generation[m] = 0;
			monthly_elec_to_grid[m] = 0;
			monthly_salespurchases[m] = 0;
			for (d=0;d<util::nday[m];d++)
			{
				for(h=0;h<24;h++)
				{
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
					{
						// net energy use per month
						energy_use[m] += e_grid[c];
						// Sean's sign convention
						monthly_load[m] -= e_load[c];
						monthly_generation[m] += e_sys[c]; // does not include first year sys_scale
						monthly_elec_to_grid[m] += e_grid[c];
						// 9/10/13 update from Paul
						monthly_salespurchases[m] += salespurchases[c];
						c++;
					}
				}
			}
		}
		//

		for (m=0;m<12;m++)
		{
			if (monthly_elec_to_grid[m] > 0)
				monthly_elec_needed_from_grid[m] = monthly_elec_to_grid[m];
			else
				monthly_elec_needed_from_grid[m]=0;
		}
	}

	void ur_calc( ssc_number_t *e_in, ssc_number_t *p_in,
		ssc_number_t *revenue, ssc_number_t *payment, ssc_number_t *income,
		ssc_number_t *demand_charge, ssc_number_t *energy_charge,
		ssc_number_t monthly_fixed_charges[12], ssc_number_t monthly_minimum_charges[12],
		ssc_number_t monthly_ec_charges[12],
		ssc_number_t monthly_ec_charges_gross[12],
		ssc_number_t excess_dollars_earned[12],
		ssc_number_t excess_dollars_applied[12],
		ssc_number_t excess_kwhs_earned[12],
		ssc_number_t *dc_hourly_peak, ssc_number_t monthly_cumulative_excess_energy[12],
		ssc_number_t monthly_cumulative_excess_dollars[12], ssc_number_t monthly_bill[12],
        ssc_number_t monthly_true_up_credits[12],
        ssc_number_t prev_monthly_peaks[12],
		ssc_number_t rate_esc, size_t year, ur_month* prev_dec, ssc_number_t prev_excess_energy, ssc_number_t prev_excess_dollars, bool include_fixed=true, bool include_min=true, bool gen_only=false)

	{
		int i;

		for (i=0;i<(int)m_num_rec_yearly;i++)
			revenue[i] = payment[i] = income[i] = demand_charge[i] = dc_hourly_peak[i] = energy_charge[i] = 0.0;

		for (i=0;i<12;i++)
		{
			monthly_fixed_charges[i] = monthly_minimum_charges[i]
				//= monthly_ec_flat_charges[i]
				= rate.monthly_dc_fixed[i] = rate.monthly_dc_tou[i]
				= monthly_ec_charges[i]
				= monthly_ec_charges_gross[i]
				= excess_dollars_earned[i]
				= excess_dollars_applied[i]
				= excess_kwhs_earned[i]
				= monthly_cumulative_excess_energy[i]
				= monthly_cumulative_excess_dollars[i]
				= monthly_bill[i]
                = monthly_true_up_credits[i] = 0.0;
		}
		// initialize all montly values

		/*
		0=Single meter with monthly rollover credits in kWh
		1=Single meter with monthly rollover credits in $
		2=Single meter with no monthly rollover credits (Net Billing)
		3=Two meters with all generation sold and all load purchaseded
		4=Single meter with monthly rollover credits in $ (Net Billing $)
		*/
		int metering_option = as_integer("ur_metering_option");
		bool enable_nm = (metering_option == 0 || metering_option == 1);

		bool ec_enabled = true; // per 2/25/16 meeting
		bool dc_enabled = as_boolean("ur_dc_enable");

		bool excess_monthly_dollars = (as_integer("ur_metering_option") == 1);

		rate.tou_demand_single_peak = (as_integer("TOU_demand_single_peak") == 1);

		int net_metering_credit_month = (int)as_number("ur_nm_credit_month");
		bool rollover_credit = as_boolean("ur_nm_credit_rollover");

		size_t steps_per_hour = m_num_rec_yearly / 8760;
		// calculate the monthly net energy and monthly hours
		int m, h, s, period, tier;
		size_t d;
		int c = 0;
        ssc_number_t prev_demand_charge = 0.0;
        ssc_number_t curr_demand_charge = 0.0;

        if (rate.uses_billing_demand) {
            if (rate.en_billing_demand_lookback) {
                rate.setup_prev_demand(prev_monthly_peaks);
            }
        }

		for (m = 0; m < (int)rate.m_month.size(); m++)
		{
			ur_month& curr_month = rate.m_month[m];
            curr_month.reset();
            prev_demand_charge = 0.0;
            
            if (dc_enabled || rate.uses_billing_demand) {
                rate.init_dc_peak_vectors(m);
                if (rate.en_billing_demand_lookback) {
                    rate.billing_demand[m] = rate.get_billing_demand(m);
                }
                else {
                    rate.billing_demand[m] = 0.0;
                }
            }

			for (d = 0; d < util::nday[m]; d++)
			{
				for (h = 0; h < 24; h++)
				{
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
					{
						curr_month.update_net_and_peak(e_in[c], p_in[c], c);
                        // set peak per period - no tier accumulation
                        rate.find_dc_tou_peak(m, p_in[c], c);

                        if (rate.en_billing_demand_lookback) {
                            rate.billing_demand[m] = rate.get_billing_demand(m);
                        }
                        else {
                            rate.billing_demand[m] = curr_month.dc_flat_peak;
                        }

                        if (dc_enabled) {
                            curr_demand_charge = rate.get_demand_charge(m, year);
                            demand_charge[c] = curr_demand_charge - prev_demand_charge;
                            prev_demand_charge = curr_demand_charge;
                        }
						c++;
					}
				}
			}
            if (dc_enabled) {
                rate.set_demand_peak_hours(m);
            }
		}

		// monthly cumulative excess energy (positive = excess energy, negative = excess load)
		if (enable_nm && !excess_monthly_dollars)
		{
			ssc_number_t prev_value = 0;
			for (m = 0; m < 12; m++)
			{
				ur_month& curr_month = rate.m_month[m];
				if (m == net_metering_credit_month + 1) {
					monthly_cumulative_excess_energy[m] = ((curr_month.energy_net) > 0) ? (curr_month.energy_net) : 0;
				}
				else if (m == 0 && year > 0 && net_metering_credit_month != 11) {
					monthly_cumulative_excess_energy[m] = prev_excess_energy;
				}
				else {
					prev_value = (m > 0) ? monthly_cumulative_excess_energy[m - 1] : 0;
					monthly_cumulative_excess_energy[m] = ((prev_value + curr_month.energy_net) > 0) ? (prev_value + curr_month.energy_net) : 0;
				}
			}
		}

		// excess earned
		for (m = 0; m < 12; m++)
		{
            // Excess generation (kWh) should remain zero for buy all / sell all
            if (rate.m_month[m].energy_net > 0 && !gen_only)
				excess_kwhs_earned[m] = rate.m_month[m].energy_net;
		}


		// adjust net energy if net metering with monthly rollover
		if (enable_nm && !excess_monthly_dollars)
		{
			if (net_metering_credit_month != 11 && year > 0) {
				if (prev_excess_energy < 0) {
					rate.m_month[0].energy_net += prev_excess_energy;
				}
			}
			for (m = 1; m < (int)rate.m_month.size(); m++)
			{
				if (rate.m_month[m].energy_net < 0 && m != net_metering_credit_month + 1)
				{
					rate.m_month[m].energy_net += monthly_cumulative_excess_energy[m - 1];
				}
			}
		}

		if (ec_enabled)
		{
			// calculate the monthly net energy per tier and period based on units
			rate.init_energy_rates_all_months(gen_only);
			c = 0;
			for (m = 0; m < (int)rate.m_month.size(); m++)
			{
				ur_month& curr_month = rate.m_month[m];

                std::vector<double> composite_buy_rate = rate.get_composite_tou_buy_rate(m, year, curr_month.energy_net);
                std::vector<double> composite_sell_rate = rate.get_composite_tou_sell_rate(m, year, curr_month.energy_net);

                bool skip_rollover = (m == 0 && year == 0) || (m == net_metering_credit_month + 1) || (m == 0 && net_metering_credit_month == 11);

                // rollover energy from correct period - matching time of day
                if (!skip_rollover && enable_nm && !excess_monthly_dollars)
                {
                    ur_month& prev_month = (m == 0) ? *prev_dec : rate.m_month[m - 1];
                    int errorCode = rate.transfer_surplus(curr_month, prev_month);
                    if (errorCode > 0)
                    {
                        int errorMonth = m;
                        if (errorCode > 200)
                        {
                            errorMonth = m - 1;
                        }
                        std::ostringstream ss;
                        ss << "year:" << year << "utilityrate5: Unable to determine period for energy charge rollover: Period " << errorCode % 100 << " does not exist for 12 am, 6 am, 12 pm or 6 pm in the current month, which is " << util::schedule_int_to_month(errorMonth) << ".";
                        log(ss.str(), SSC_NOTICE);
                    }
                }

                ssc_number_t prev_cost = 0;
				for (d = 0; d < util::nday[m]; d++)
				{
					for (h = 0; h < 24; h++)
					{
						for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
						{
							rate.sort_energy_to_periods(m, e_in[c], c);
                            ssc_number_t curr_cost = rate.getEnergyChargeNetMetering(m, composite_buy_rate, composite_sell_rate);
                            energy_charge[c] = curr_cost - prev_cost;
                            prev_cost = curr_cost;
							c++;
						}
					}
				}

                rate.compute_surplus(curr_month);

				// now ditribute across tier boundaries - upper bounds equally across periods
				// 3/5/16 prorate based on total net per period / total net
				// look at total net distributed among tiers

				ssc_number_t num_per = (ssc_number_t)curr_month.ec_energy_use.nrows();
				ssc_number_t tot_energy = 0;
				for (size_t ir = 0; ir < num_per; ir++)
					tot_energy += curr_month.ec_energy_use.at(ir, 0);
				if (tot_energy > 0)
				{
					for (size_t ir = 0; ir < num_per; ir++)
					{
						bool done = false;
						ssc_number_t per_energy = curr_month.ec_energy_use.at(ir, 0);
						for (size_t ic = 0; ic < curr_month.ec_tou_ub.ncols() && !done; ic++)
						{
							ssc_number_t ub_tier = curr_month.ec_tou_ub.at(ir, ic);
							if (per_energy > 0)
							{
								if (tot_energy > ub_tier)
								{
									curr_month.ec_energy_use.at(ir, ic) = (per_energy/tot_energy) * ub_tier;
									if (ic > 0)
										curr_month.ec_energy_use.at(ir, ic) -= (per_energy / tot_energy) * curr_month.ec_tou_ub.at(ir, ic - 1);
								}
								else
								{
									curr_month.ec_energy_use.at(ir, ic) = (per_energy / tot_energy) * tot_energy;
									if (ic > 0)
										curr_month.ec_energy_use.at(ir, ic) -= (per_energy / tot_energy)* curr_month.ec_tou_ub.at(ir, ic - 1);
									done=true;
								}
							}
						}
					}
				}

				// repeat for surplus
				tot_energy = 0;
				for (size_t ir = 0; ir < num_per; ir++)
					tot_energy += curr_month.ec_energy_surplus.at(ir, 0);
				if (tot_energy > 0)
				{
					for (size_t ir = 0; ir < num_per; ir++)
					{
						bool done = false;
						ssc_number_t per_energy = curr_month.ec_energy_surplus.at(ir, 0);
						for (size_t ic = 0; ic < curr_month.ec_tou_ub.ncols() && !done; ic++)
						{
							ssc_number_t ub_tier = curr_month.ec_tou_ub.at(0, ic);
							if (per_energy > 0)
							{
								if (tot_energy > ub_tier)
								{
									curr_month.ec_energy_surplus.at(ir, ic) = (per_energy / tot_energy) * ub_tier;
									if (ic > 0)
										curr_month.ec_energy_surplus.at(ir, ic) -= (per_energy / tot_energy) * curr_month.ec_tou_ub.at(ir, ic - 1);
								}
								else
								{
									curr_month.ec_energy_surplus.at(ir, ic) = (per_energy / tot_energy) * tot_energy;
									if (ic > 0)
										curr_month.ec_energy_surplus.at(ir, ic) -= (per_energy / tot_energy)* curr_month.ec_tou_ub.at(ir, ic - 1);
									done = true;
								}
							}
						}
					}
				}

			} // end month
		}

// main loop
		c = 0;
		// process one month at a time
		for (m = 0; m < (int)rate.m_month.size(); m++)
		{
			ur_month& curr_month = rate.m_month[m];
			if (curr_month.hours_per_month <= 0) break;
			for (d = 0; d<util::nday[m]; d++)
			{
				for (h = 0; h<24; h++)
				{
					// energy charge
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
					{
						if (d == util::nday[m] - 1 && h == 23 && s == (int)(steps_per_hour-1) )
						{
							if (ec_enabled)
							{
								// energy use and surplus distributed correctly above.
								// so calculate for all and not based on monthly net
								// addresses issue if net > 0 but one period net < 0
								ssc_number_t credit_amt = 0;
								for (period = 0; period < (int)curr_month.ec_tou_sr.nrows(); period++)
								{
									for (tier = 0; tier < (int)curr_month.ec_tou_sr.ncols(); tier++)
									{
										ssc_number_t cr = curr_month.ec_energy_surplus.at(period, tier) * curr_month.ec_tou_sr.at(period, tier) * rate_esc;

										if (!enable_nm)
										{
											credit_amt += cr;
											curr_month.ec_charge.at(period, tier) = -cr;
										}
										else if (excess_monthly_dollars)
											monthly_cumulative_excess_dollars[m] += cr;

									}
								}
								monthly_ec_charges[m] -= credit_amt;

								ssc_number_t charge_amt = 0;
								for (period = 0; period < (int)curr_month.ec_tou_br.nrows(); period++)
								{
									for (tier = 0; tier < (int)curr_month.ec_tou_br.ncols(); tier++)
									{
										ssc_number_t ch = curr_month.ec_energy_use.at(period, tier) * curr_month.ec_tou_br.at(period, tier) * rate_esc;
										curr_month.ec_charge.at(period, tier) = ch;
										charge_amt += ch;
									}
								}
								monthly_ec_charges[m] += charge_amt;

								// monthly rollover with year end sell at reduced rate
								if (enable_nm)
								{
									payment[c] += monthly_ec_charges[m];

								}
								else // non-net metering - no rollover
								{
									if (curr_month.energy_net < 0) // must buy from grid
										payment[c] += monthly_ec_charges[m];
									else // surplus - sell to grid
										income[c] -= monthly_ec_charges[m]; // charge is negative for income!
								}

								// end of energy charge

							}


							if (dc_enabled)
							{
								ssc_number_t charge = rate.get_demand_charge(m, year);
								payment[c] += charge; // apply to last hour of the month
							}

						} // end of if end of month
						c++;
					} // end of steps per hour loop
				}  // h loop
			} // d loop

			// Calculate monthly bill (before minimums and fixed charges) and excess kwhs and rollover
//			monthly_bill[m] = payment[c - 1] - income[c - 1];
			monthly_bill[m] = monthly_ec_charges[m] + rate.monthly_dc_fixed[m] + rate.monthly_dc_tou[m];

			monthly_ec_charges_gross[m] = monthly_ec_charges[m];
			excess_dollars_earned[m] = monthly_cumulative_excess_dollars[m];
			ssc_number_t dollars_applied = 0;
			if (enable_nm)
			{
                if (rollover_credit && m == net_metering_credit_month)
                {
                    ssc_number_t year_end_dollars = monthly_cumulative_excess_energy[m] * as_number("ur_nm_yearend_sell_rate") * rate_esc;
                    payment[c - 1] -= year_end_dollars;
                    monthly_ec_charges[m] -= year_end_dollars;
                    // Don't apply this to monthly_true_up_credits, since the dollars aren't recieved this month when rollover_credit is true
                    dollars_applied += year_end_dollars;
                }

				// apply previous month rollover kwhs
				if (m > 0 && (m != (net_metering_credit_month + 1) || rollover_credit))
				{
//					monthly_bill[m] -= monthly_cumulative_excess_dollars[m - 1];
					payment[c - 1] -= monthly_cumulative_excess_dollars[m-1];
					monthly_ec_charges[m] -= monthly_cumulative_excess_dollars[m - 1];
					dollars_applied += monthly_cumulative_excess_dollars[m - 1];
				}
				else if (m == 0 && year > 0 && (net_metering_credit_month != 11 || rollover_credit)) {
					payment[c - 1] -= prev_excess_dollars;
					monthly_ec_charges[m] -= prev_excess_dollars;
					dollars_applied += prev_excess_dollars;
				}

				if (monthly_ec_charges[m] < 0)
				{
					if (excess_monthly_dollars || rollover_credit)
					{
						monthly_cumulative_excess_dollars[m] -= monthly_ec_charges[m];
					}

					payment[c - 1] -= monthly_ec_charges[m];; // keep demand charges
					monthly_ec_charges[m] = 0;
				}
				else // apply current month rollover and adjust
				{
//					monthly_bill[m] -= monthly_cumulative_excess_dollars[m];
					monthly_ec_charges[m] -= monthly_cumulative_excess_dollars[m];
					//					if (monthly_bill[m] < 0)
					if (monthly_ec_charges[m] < 0)
					{
						payment[c - 1] -= monthly_cumulative_excess_dollars[m] + monthly_ec_charges[m];
						if (excess_monthly_dollars || rollover_credit)
						{
							dollars_applied += monthly_cumulative_excess_dollars[m] + monthly_ec_charges[m];
                            monthly_cumulative_excess_dollars[m] = -monthly_ec_charges[m];
						}
						//						monthly_bill[m] = 0;
						monthly_ec_charges[m] = 0;
					}
					else
					{
						dollars_applied += monthly_cumulative_excess_dollars[m];
						payment[c - 1] -= monthly_cumulative_excess_dollars[m];
						monthly_cumulative_excess_dollars[m] = 0;
					}
				}
			}
			if (monthly_ec_charges_gross[m] < dollars_applied) dollars_applied = monthly_ec_charges_gross[m];
			excess_dollars_applied[m] = dollars_applied;
			monthly_bill[m] = monthly_ec_charges[m] + rate.monthly_dc_fixed[m] + rate.monthly_dc_tou[m];

		} // end of month m (m loop)

		// TODO: check two meter excess


		// Assumption that fixed and minimum charges independent of rollovers kWh or $
		// process monthly fixed charges
		// compute revenue ( = income - payment ) and monthly bill ( = payment - income) and apply fixed and minimum charges
		c = 0;
		ssc_number_t mon_bill = 0, ann_bill = 0;
		ssc_number_t ann_min_charge = as_number("ur_annual_min_charge")*rate_esc;
		ssc_number_t mon_min_charge = as_number("ur_monthly_min_charge")*rate_esc;
		ssc_number_t mon_fixed = as_number("ur_monthly_fixed_charge")*rate_esc;

		// process one month at a time
		for (m = 0; m < 12; m++)
		{
			for (d = 0; d < util::nday[m]; d++)
			{
				for (h = 0; h < 24; h++)
				{
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
					{
						if (d == util::nday[m] - 1 && h == 23 && s == (int)(steps_per_hour - 1))
						{
							// apply fixed first
							if (include_fixed)
							{
								payment[c] += mon_fixed;
								monthly_fixed_charges[m] += mon_fixed;
							}
							mon_bill = payment[c] - income[c];
							if (mon_bill < 0) mon_bill = 0; // for calculating min charge when monthly surplus.
							// apply monthly minimum
							if (include_min)
							{
								if (mon_bill < mon_min_charge)
								{
									monthly_minimum_charges[m] += mon_min_charge - mon_bill;
									payment[c] += mon_min_charge - mon_bill;
								}
							}
							ann_bill += mon_bill;
							if (m == 11) // Example: PG&E assesses monthly bills 12 months after interconnection, so this assumes the system comes online Jan 1st
							{
								// apply annual minimum
								if (include_min)
								{
									if (ann_bill < ann_min_charge)
									{
										monthly_minimum_charges[m] += ann_min_charge - ann_bill;
										payment[c] += ann_min_charge - ann_bill;
									}
								}
							}

							if (m == net_metering_credit_month)
							{
								// apply annual rollovers AFTER minimum calculations
								if (enable_nm)
								{
									// monthly rollover with year end sell at reduced rate
									if (!excess_monthly_dollars && (monthly_cumulative_excess_energy[m] > 0) && !rollover_credit)
									{
										ssc_number_t year_end_dollars = monthly_cumulative_excess_energy[m] * as_number("ur_nm_yearend_sell_rate")*rate_esc;
										monthly_cumulative_excess_dollars[m] = year_end_dollars;
										excess_dollars_earned[m] += year_end_dollars;
                                        monthly_true_up_credits[m] += year_end_dollars;
										income[c] += year_end_dollars;
									}
									else if (excess_monthly_dollars && (monthly_cumulative_excess_dollars[m] > 0))
									{
										income[c] += monthly_cumulative_excess_dollars[m];
                                        monthly_true_up_credits[m] = monthly_cumulative_excess_dollars[m];
										// ? net metering energy?
									}
								}
							}
							revenue[c] = income[c] - payment[c];
							monthly_bill[m] = -revenue[c];
						}
						c++;
					}
				}
			}
		}

	}

	// updated to timestep for net billing
	void ur_calc_timestep(ssc_number_t *e_in, ssc_number_t *p_in,
		ssc_number_t *revenue, ssc_number_t *payment, ssc_number_t *income,
		ssc_number_t *demand_charge, ssc_number_t *energy_charge,
		ssc_number_t monthly_fixed_charges[12], ssc_number_t monthly_minimum_charges[12],
		ssc_number_t monthly_ec_charges[12],
		ssc_number_t monthly_ec_charges_gross[12],
		ssc_number_t excess_dollars_earned[12],
		ssc_number_t excess_kwhs_earned[12],
        ssc_number_t net_billing_credits[12],
		ssc_number_t *dc_hourly_peak, ssc_number_t monthly_cumulative_excess_energy[12],
		ssc_number_t monthly_cumulative_excess_dollars[12], ssc_number_t monthly_bill[12],
        ssc_number_t monthly_two_meter_sales[12], ssc_number_t monthly_true_up_credits[12],
        ssc_number_t prev_monthly_peaks[12],
		ssc_number_t rate_esc, size_t year, ssc_number_t prev_excess_dollars, bool include_fixed = true, bool include_min = true, bool gen_only = false)

	{
		int i;
		for (i = 0; i<(int)m_num_rec_yearly; i++)
			revenue[i] = payment[i] = income[i] = demand_charge[i] = dc_hourly_peak[i] = energy_charge[i] = 0.0;

		for (i = 0; i<12; i++)
		{
			monthly_fixed_charges[i] = monthly_minimum_charges[i]
				//= monthly_ec_flat_charges[i]
				= rate.monthly_dc_fixed[i] = rate.monthly_dc_tou[i]
				= monthly_ec_charges[i]
				= monthly_ec_charges_gross[i]
				= excess_dollars_earned[i]
				= excess_kwhs_earned[i]
                = net_billing_credits[i]
				= monthly_cumulative_excess_energy[i]
				= monthly_cumulative_excess_dollars[i]
				= monthly_bill[i]
                = monthly_two_meter_sales[i]
                = monthly_true_up_credits[i] = 0.0;
		}

		// 0=hourly (match with 2015.1.30 release, 1=monthly (most common unit in URDB), 2=daily (used for PG&E baseline rates). Currently hidden in UI and set to zero
//		int ur_ec_hourly_acc_period = as_integer("ur_ec_hourly_acc_period");
		int ur_ec_hourly_acc_period = 1; // monthly per 2/25/16 meeting
		// single meter so single net accumulation
		ssc_number_t daily_surplus_energy;
		ssc_number_t monthly_surplus_energy;
        ssc_number_t step_surplus_energy; // For tier rollovers
		ssc_number_t daily_deficit_energy;
		ssc_number_t monthly_deficit_energy;
        ssc_number_t step_deficit_energy; // For tier rollovers
        ssc_number_t e_upper; // For tier computation

		bool ec_enabled = true; // per 2/25/16 meeting
		bool dc_enabled = as_boolean("ur_dc_enable");

		/*
		0=Single meter with monthly rollover credits in kWh
		1=Single meter with monthly rollover credits in $
		2=Single meter with no monthly rollover credits (Net Billing)
		3=Single meter with monthly rollover credits in $ (Net Billing $)
		4=Two meters with all generation sold and all load purchaseded
		*/
		//int metering_option = as_integer("ur_metering_option");
		bool excess_monthly_dollars = (as_integer("ur_metering_option") == 3);
		int excess_dollars_credit_month = (int)as_number("ur_nm_credit_month");
        // Do our credits expire at the end of year
        bool nb_credit_expire = as_boolean("ur_nb_credit_expire"); 
        // Do we wish to apply our credits in the month they are earned
        bool nb_apply_credit_current_month = as_boolean("ur_nb_apply_credit_current_month"); 

        rate.tou_demand_single_peak = (as_integer("TOU_demand_single_peak") == 1);


		size_t steps_per_hour = m_num_rec_yearly / 8760;


		// calculate the monthly net energy and monthly hours
        int m, h, s;
        size_t surplus_tier = 0, deficit_tier = 0;
		size_t d;
		size_t c = 0;

        ssc_number_t prev_demand_charge = 0.0;
        ssc_number_t curr_demand_charge = 0.0;

        if (rate.uses_billing_demand) {
            if (rate.en_billing_demand_lookback) {
                rate.setup_prev_demand(prev_monthly_peaks);
            }
        }

        for (m = 0; m < (int)rate.m_month.size(); m++)
        {
            ur_month& curr_month = rate.m_month[m];
            curr_month.reset();
            prev_demand_charge = 0.0;

            if (dc_enabled || rate.uses_billing_demand) {
                rate.init_dc_peak_vectors(m);
                if (rate.en_billing_demand_lookback) {
                    rate.billing_demand[m] = rate.get_billing_demand(m);
                }
                else {
                    rate.billing_demand[m] = 0.0;
                }
            }

            for (d = 0; d < util::nday[m]; d++)
            {
                for (h = 0; h < 24; h++)
                {
                    for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
                    {
                        curr_month.update_net_and_peak(e_in[c], p_in[c], c);

                        // set peak per period - no tier accumulation
                        rate.find_dc_tou_peak(m, p_in[c], c);

                        if (rate.en_billing_demand_lookback) {
                            rate.billing_demand[m] = rate.get_billing_demand(m);
                        }
                        else {
                            rate.billing_demand[m] = curr_month.dc_flat_peak;
                        }

                        if (dc_enabled) {
                            curr_demand_charge = rate.get_demand_charge(m, year);
                            demand_charge[c] = curr_demand_charge - prev_demand_charge;
                            prev_demand_charge = curr_demand_charge;
                        }
                        c++;
                    }
                }
            }
            if (dc_enabled) {
                rate.set_demand_peak_hours(m);
            }
        }

		// excess earned
		for (m = 0; m < 12; m++)
		{
            // Excess generation (kWh) should remain zero for buy all / sell all
			if (rate.m_month[m].energy_net > 0 && !gen_only)
				excess_kwhs_earned[m] = rate.m_month[m].energy_net;
		}

		if (ec_enabled)
		{
			rate.init_energy_rates_all_months(gen_only);
		}

// main loop
		c = 0; // hourly count
		// process one timestep at a time
		for (m = 0; m < 12; m++) // for each month
		{
			ur_month& curr_month = rate.m_month[m]; 
			monthly_surplus_energy = 0;                 
			monthly_deficit_energy = 0;
            if (ur_ec_hourly_acc_period == 1) {
                surplus_tier = 0;
                deficit_tier = 0;
            }

			for (d = 0; d<util::nday[m]; d++) // for each day
			{
				daily_surplus_energy = 0;
				daily_deficit_energy = 0;
                if (ur_ec_hourly_acc_period == 2) { // This will never happen because we set it to 1 above
                    surplus_tier = 0;
                    deficit_tier = 0;
                }
				for (h = 0; h<24; h++) // for each hour
				{
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++) // as far as I can tell given an hour usage profile, etc, this should be a single iteration
					{
						// energy charge
						if (ec_enabled)         // Also always true, "energy charge enabled", 
						{
							int row = rate.get_tou_row(c, m);

                            step_surplus_energy = 0.0;
                            step_deficit_energy = 0.0;

							if (e_in[c] >= 0.0) // If we have money earned
							{ // calculate income or credit
                                e_upper = curr_month.ec_tou_ub.at(row, surplus_tier); // Have to check this each step to swap between surplus and deficit
								monthly_surplus_energy += e_in[c];
								daily_surplus_energy += e_in[c];

								// base period charge on units specified
								ssc_number_t energy_surplus = e_in[c];              
								ssc_number_t cumulative_energy = e_in[c];
								if (ur_ec_hourly_acc_period == 1)                   // Always 1
									cumulative_energy = monthly_surplus_energy;
								else if (ur_ec_hourly_acc_period == 2)              // Never taken
									cumulative_energy = daily_surplus_energy;


								// cumulative energy used to determine tier for credit of entire surplus amount

                                // check for vlid tier structure per https://github.com/NREL/SAM/issues/1834
                                size_t max_tier = curr_month.ec_tou_ub.ncols() - 1;
                                if (cumulative_energy > curr_month.ec_tou_ub.at(row, max_tier)) {
                                    std::ostringstream ss;
                                    ss << "cumulative energy  " << cumulative_energy << "kWh exceeds maximum usage specified for all tiers for period " << row;
                                    throw exec_error("utilityrate5", ss.str());
                                }


								ssc_number_t credit_amt = 0;

                                ssc_number_t tier_credit = 0.0, sr = 0.0, tier_energy = 0.0;
                                // time step sell rates
                                bool use_ec_table_sell_rates = true;
                                if (as_boolean("ur_en_ts_sell_rate")) {
                                    if (c < rate.m_ec_ts_sell_rate.size()) {
                                        tier_energy = energy_surplus;
                                        sr = rate.m_ec_ts_sell_rate[c];
                                        tier_credit = tier_energy * sr * rate_esc;
                                        curr_month.ec_energy_surplus.at(row, surplus_tier) += (ssc_number_t)tier_energy;
                                        use_ec_table_sell_rates = false;
                                    }
                                }

                                // Fall back to TOU rates if m_ec_ts_sell_rate.size() is too small
                                if (use_ec_table_sell_rates) {         // So AFAICT this is to make sure we don't compute both time step and TOU. Maybe better as an else?
                                    if (cumulative_energy <= e_upper) { // If we are within the max usage for the tier, then it's a simple multiply
                                        tier_energy = energy_surplus;   // of our usage with that rate  and the escalator
                                        sr = curr_month.ec_tou_sr.at(row, surplus_tier);

                                        tier_credit = tier_energy * sr * rate_esc;
                                        curr_month.ec_energy_surplus.at(row, surplus_tier) += (ssc_number_t)tier_energy;
                                    }
                                    else {  // Otherwise, we need to handle that the electricity is being sold at different rates
                                        bool break_tier_loop = false;
                                        while (cumulative_energy > e_upper) {
                                            step_surplus_energy = energy_surplus - (cumulative_energy - e_upper); // Subtract amount above the tier to find amount in this tier

                                            ssc_number_t sr_base = curr_month.ec_tou_sr.at(row, surplus_tier);
                                            tier_credit += step_surplus_energy * sr_base * rate_esc;
                                            curr_month.ec_energy_surplus.at(row, surplus_tier) += (ssc_number_t)step_surplus_energy;

                                            ssc_number_t upper_tier_energy = energy_surplus - step_surplus_energy;

                                            surplus_tier++;
                                            if (surplus_tier >= (int)curr_month.ec_tou_ub.ncols()) {
                                                surplus_tier = (int)curr_month.ec_tou_ub.ncols() - 1;
                                                break_tier_loop = true;
                                            }

                                            ssc_number_t e_upper_new = curr_month.ec_tou_ub.at(row, surplus_tier);

                                            // Only run on last iteration of loop
                                            if (cumulative_energy <= e_upper_new) {
                                                sr = curr_month.ec_tou_sr.at(row, surplus_tier);
                                                tier_credit += upper_tier_energy * sr * rate_esc;
                                                curr_month.ec_energy_surplus.at(row, surplus_tier) += (ssc_number_t)upper_tier_energy;
                                            }

                                            energy_surplus -= step_surplus_energy;
                                            e_upper = e_upper_new;

                                            if (break_tier_loop)
                                                break;
                                        }
                                    }
                                    
                                }

								credit_amt = tier_credit;


								if (excess_monthly_dollars) // NB with carryover to next month
								{
									monthly_cumulative_excess_dollars[m] += credit_amt;
								}
								else
								{
									curr_month.ec_charge.at(row, surplus_tier) -= (ssc_number_t)tier_credit;
									//								price[c] += (ssc_number_t)credit_amt;
									monthly_ec_charges[m] -= (ssc_number_t)credit_amt;
									income[c] = (ssc_number_t)credit_amt;
									energy_charge[c] = -(ssc_number_t)credit_amt;
                                    if (!gen_only) {
                                        net_billing_credits[m] += credit_amt;
                                    }
                                    else {
                                        monthly_two_meter_sales[m] += credit_amt;
                                    }
								}
							}
							else // Same calc but with spending money
							{ // calculate payment or charge
                                e_upper = curr_month.ec_tou_ub.at(row, deficit_tier); // Have to check this each step to swap between surplus and deficit
								monthly_deficit_energy -= e_in[c];          
								daily_deficit_energy -= e_in[c];
								double charge_amt = 0;
								double energy_deficit = -e_in[c];
								// base period charge on units specified
								double cumulative_deficit = -e_in[c];
								if (ur_ec_hourly_acc_period == 1)
									cumulative_deficit = monthly_deficit_energy;
								else if (ur_ec_hourly_acc_period == 2)
									cumulative_deficit = daily_deficit_energy;

                                ssc_number_t tier_charge = 0.0, br = 0.0, tier_energy = 0.0;
                                bool use_ec_table_buy_rates = true;
                                // time step sell rates
                                if (as_boolean("ur_en_ts_buy_rate")) {
                                    if (c < rate.m_ec_ts_buy_rate.size()) {
                                        tier_energy = energy_deficit;
                                        br = rate.m_ec_ts_buy_rate[c];
                                        tier_charge = tier_energy * br * rate_esc;
                                        charge_amt = tier_energy * br * rate_esc;
                                        curr_month.ec_energy_use.at(row, deficit_tier) += (ssc_number_t)tier_energy;
                                        curr_month.ec_charge.at(row, deficit_tier) += (ssc_number_t)charge_amt;
                                        use_ec_table_buy_rates = false;
                                    }
                                }

                                // Fall back to TOU rates if m_ec_ts_buy_rate.size() is too small
                                if (use_ec_table_buy_rates) {
                                    if (cumulative_deficit <= e_upper) {
                                        tier_energy = energy_deficit;
                                        br = curr_month.ec_tou_br.at(row, deficit_tier);

                                        charge_amt = tier_energy * br * rate_esc;
                                        curr_month.ec_energy_use.at(row, deficit_tier) += (ssc_number_t)tier_energy;
                                        curr_month.ec_charge.at(row, deficit_tier) += (ssc_number_t)charge_amt;
                                    }
                                    else {
                                        bool break_tier_loop = false;
                                        while (cumulative_deficit > e_upper) {
                                            step_deficit_energy = energy_deficit - (cumulative_deficit - e_upper); // Subtract amount above the tier to find amount in this tier

                                            ssc_number_t br_base = curr_month.ec_tou_br.at(row, deficit_tier);
                                            tier_charge += step_deficit_energy * br_base * rate_esc;
                                            curr_month.ec_energy_use.at(row, deficit_tier) += (ssc_number_t)step_deficit_energy;

                                            ssc_number_t upper_tier_energy = energy_deficit - step_deficit_energy;

                                            deficit_tier++;
                                            if (deficit_tier >= (int)curr_month.ec_tou_ub.ncols()) {
                                                deficit_tier = (int)curr_month.ec_tou_ub.ncols() - 1;
                                                break_tier_loop = true;
                                            }

                                            ssc_number_t e_upper_new = curr_month.ec_tou_ub.at(row, deficit_tier);

                                            // Only run on last iteration of loop
                                            if (cumulative_deficit <= e_upper_new) {
                                                br = curr_month.ec_tou_br.at(row, deficit_tier);
                                                tier_charge += upper_tier_energy * br * rate_esc;
                                                curr_month.ec_energy_use.at(row, deficit_tier) += (ssc_number_t)upper_tier_energy;
                                            }

                                            energy_deficit -= step_deficit_energy;
                                            e_upper = e_upper_new;

                                            if (break_tier_loop)
                                                break;
                                        }
                                        curr_month.ec_charge.at(row, deficit_tier) += (ssc_number_t)tier_charge;
                                        charge_amt += tier_charge;
                                    }
                                    
                                }

								payment[c] = (ssc_number_t)charge_amt;
								monthly_ec_charges[m] += (ssc_number_t)charge_amt;
                                if (!excess_monthly_dollars)
                                {
                                    // Runs for net billing (option 2) and buy all / sell all (option 4)
                                    monthly_ec_charges_gross[m] += (ssc_number_t)charge_amt;
                                }
//								price[c] += (ssc_number_t)charge_amt;
								energy_charge[c] = (ssc_number_t)charge_amt;
							}
						}
						// end of energy charge


						// demand charge - end of month only
						if (d == util::nday[m] - 1 && h == 23 && s == (int)(steps_per_hour - 1))
						{

							if (dc_enabled)
							{
								ssc_number_t charge = rate.get_demand_charge(m, year);
								payment[c] += charge; // apply to last hour of the month

							} // if demand charges enabled (dc_enabled)
						}	// end of demand charges at end of month

						c++;
					} // steps per hour loop
				}  // h loop
			} // d loop

			// Calculate monthly bill (before minimums and fixed charges) and excess kwhs and rollover

			monthly_bill[m] = monthly_ec_charges[m] + rate.monthly_dc_fixed[m] + rate.monthly_dc_tou[m]; // This code seems unnecessary, it is repeated verbatim below

			excess_dollars_earned[m] = monthly_cumulative_excess_dollars[m];


			ssc_number_t dollars_applied = 0;
			// apply previous month rollover kwhs
			if (excess_monthly_dollars) // If in net billing with carrover to next month
			{
                // Overwrite this to include the current month's charges
                monthly_ec_charges_gross[m] = monthly_ec_charges[m];
				if (m == 0 && excess_dollars_credit_month != 11) { // If start of the year and the rollover month not december
					monthly_ec_charges[m] -= prev_excess_dollars;   // prev_excess dollars is the value of the credit coming in from last year
					payment[c - 1] -= prev_excess_dollars;
					dollars_applied += prev_excess_dollars;
				}
				else if (m > 0 && m != excess_dollars_credit_month + 1) // If during the middle of the year and the rollover month not the prior month
				{
					monthly_ec_charges[m] -= monthly_cumulative_excess_dollars[m - 1];
					payment[c - 1] -= monthly_cumulative_excess_dollars[m - 1];
					dollars_applied += monthly_cumulative_excess_dollars[m - 1];
				}
                if (nb_apply_credit_current_month) {
                    monthly_ec_charges[m] -= monthly_cumulative_excess_dollars[m];
                    payment[c - 1] -= monthly_cumulative_excess_dollars[m];
                    dollars_applied += monthly_cumulative_excess_dollars[m];
                    monthly_cumulative_excess_dollars[m] = 0;
                }
                // Rollover credits at end of true-up period are applied after annual minimums below this section

                if (monthly_ec_charges[m] < 0)  // If this brings the monthly energy charges negative, then
                {
                    payment[c - 1] -= monthly_ec_charges[m]; // Add the magnitude of the negative amount to the last hour of the month
                    dollars_applied += monthly_ec_charges[m];// Subtract the magnitude of the negative amount from the dollars applied
                    monthly_cumulative_excess_dollars[m] -= monthly_ec_charges[m]; //add in the magnitude of the negative amount to the cumulative excess for this month
					monthly_ec_charges[m] = 0;
				}
                // I'm pretty sure this branch can't be taken. 
                if (monthly_ec_charges_gross[m] < dollars_applied) dollars_applied = monthly_ec_charges_gross[m];
                net_billing_credits[m] = dollars_applied;
			}

			monthly_bill[m] = monthly_ec_charges[m] + rate.monthly_dc_fixed[m] + rate.monthly_dc_tou[m];
		} // end of month m (m loop)


		// Assumption that fixed and minimum charges independent of rollovers kWh or $
		// process monthly fixed charges
		// compute revenue ( = income - payment ) and monthly bill ( = payment - income) and apply fixed and minimum charges
		c = 0;
		ssc_number_t mon_bill = 0, ann_bill = 0;
		ssc_number_t ann_min_charge = as_number("ur_annual_min_charge")*rate_esc;
		ssc_number_t mon_min_charge = as_number("ur_monthly_min_charge")*rate_esc;
		ssc_number_t mon_fixed = as_number("ur_monthly_fixed_charge")*rate_esc;

		// process one month at a time
		for (m = 0; m < 12; m++)
		{
			for (d = 0; d < util::nday[m]; d++)
			{
				for (h = 0; h < 24; h++)
				{
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
					{
						if (d == util::nday[m] - 1 && h == 23 && s == (int)(steps_per_hour - 1))
						{
							// apply fixed first
							if (include_fixed)
							{
								payment[c] += mon_fixed;
								monthly_fixed_charges[m] += mon_fixed;
							}
							mon_bill = monthly_bill[m] + monthly_fixed_charges[m];
							if (mon_bill < 0) mon_bill = 0; // for calculating min charge with monthly surplus
							// apply monthly minimum
							if (include_min)
							{
								if (mon_bill < mon_min_charge)
								{
									monthly_minimum_charges[m] += mon_min_charge - mon_bill;
									payment[c] += mon_min_charge - mon_bill;
								}
							}
							ann_bill += mon_bill;
							if (m == 11) // Example: PG&E assesses monthly bills 12 months after interconnection, so this assumes the system comes online Jan 1st
							{
								// apply annual minimum
								if (include_min)
								{
									if (ann_bill < ann_min_charge)
									{
										monthly_minimum_charges[m] += ann_min_charge - ann_bill;
										payment[c] += ann_min_charge - ann_bill;
									}
								}
							}
                            if (!nb_credit_expire)
                            {
                                if (m == excess_dollars_credit_month)
                                {
                                    // apply annual rollovers AFTER minimum calculations
                                    if (excess_monthly_dollars && (monthly_cumulative_excess_dollars[m] > 0))
                                    {
                                        income[c] += monthly_cumulative_excess_dollars[m];
                                        monthly_bill[m] -= monthly_cumulative_excess_dollars[m];
                                        monthly_true_up_credits[m] = monthly_cumulative_excess_dollars[m];
                                    }
                                }
                            }
							monthly_bill[m] += monthly_fixed_charges[m] + monthly_minimum_charges[m];
						}
						revenue[c] = income[c] - payment[c];
						c++;
					}
				}
			}
		}

	}


	void ur_update_ec_monthly(int month, util::matrix_t<double>& charge, util::matrix_t<double>& energy, util::matrix_t<double>& surplus)

	{
		if (month < 0 || month > (int)rate.m_month.size()) {
			std::ostringstream ss;
			ss << "ur_update_ec_monthly month not found for Month " << month;
			throw exec_error("utilityrate5", ss.str());
		}
		ur_month& curr_month = rate.m_month[month];
		charge.resize_fill(curr_month.ec_charge.nrows() + 2, curr_month.ec_charge.ncols() + 2, 0);
		energy.resize_fill(curr_month.ec_energy_use.nrows() + 2, curr_month.ec_energy_use.ncols() + 2, 0);
		surplus.resize_fill(curr_month.ec_energy_surplus.nrows() + 2, curr_month.ec_energy_surplus.ncols() + 2, 0);
		// output with tier column headings and period row labels
		// and totals for rows and columns.
		int ndx = -1;
		int period = 0;
		if ((curr_month.ec_periods.size() > 0) && (rate.m_ec_periods.size() > 0))
		{
			// note that all monthly periods have same tiers (checked in setup)
			period = curr_month.ec_periods[0];
			std::vector<int>::iterator result = std::find(rate.m_ec_periods.begin(), rate.m_ec_periods.end(), period);
			if (result == rate.m_ec_periods.end())
			{
				std::ostringstream ss;
				ss << "Energy rate Period " << period << " not found.";
				throw exec_error("utilityrate5", ss.str());
			}
			ndx = (int)(result - rate.m_ec_periods.begin());
		}
		if (ndx > -1)
		{
			for (int ic = 0; ic < (int)curr_month.ec_charge.ncols(); ic++)
			{
				charge.at(0, ic + 1) = (float)curr_month.ec_periods_tiers[ndx][ic];
				energy.at(0, ic + 1) = (float)curr_month.ec_periods_tiers[ndx][ic];
				surplus.at(0, ic + 1) = (float)curr_month.ec_periods_tiers[ndx][ic];
			}
			for (int ir = 0; ir < (int)curr_month.ec_charge.nrows(); ir++)
			{
				charge.at(ir + 1, 0) = (float)curr_month.ec_periods[ir];
				energy.at(ir + 1, 0) = (float)curr_month.ec_periods[ir];
				surplus.at(ir + 1, 0) = (float)curr_month.ec_periods[ir];
			}
			ssc_number_t c_total = 0;
			ssc_number_t e_total = 0;
			ssc_number_t s_total = 0;
			for (int ir = 0; ir < (int)curr_month.ec_charge.nrows(); ir++)
			{
				ssc_number_t c_row_total = 0;
				ssc_number_t e_row_total = 0;
				ssc_number_t s_row_total = 0;
				for (int ic = 0; ic <(int)curr_month.ec_charge.ncols(); ic++)
				{
					charge.at(ir + 1, ic + 1) = curr_month.ec_charge.at(ir, ic);
					c_row_total += curr_month.ec_charge.at(ir, ic);
					energy.at(ir + 1, ic + 1) = curr_month.ec_energy_use.at(ir, ic);
					e_row_total += curr_month.ec_energy_use.at(ir, ic);
					surplus.at(ir + 1, ic + 1) = curr_month.ec_energy_surplus.at(ir, ic);
					s_row_total += curr_month.ec_energy_surplus.at(ir, ic);
				}
				charge.at(ir + 1, curr_month.ec_charge.ncols() + 1) = c_row_total;
				energy.at(ir + 1, curr_month.ec_charge.ncols() + 1) = e_row_total;
				surplus.at(ir + 1, curr_month.ec_charge.ncols() + 1) = s_row_total;
				c_total += c_row_total;
				e_total += e_row_total;
				s_total += s_row_total;
			}
			for (int ic = 0; ic < (int)curr_month.ec_charge.ncols(); ic++)
			{
				ssc_number_t c_col_total = 0;
				ssc_number_t e_col_total = 0;
				ssc_number_t s_col_total = 0;
				for (int ir = 0; ir < (int)curr_month.ec_charge.nrows(); ir++)
				{
					c_col_total += curr_month.ec_charge.at(ir, ic);
					e_col_total += curr_month.ec_energy_use.at(ir, ic);
					s_col_total += curr_month.ec_energy_surplus.at(ir, ic);
				}
				charge.at(curr_month.ec_charge.nrows() + 1, ic + 1) = c_col_total;
				energy.at(curr_month.ec_energy_use.nrows() + 1, ic + 1) = e_col_total;
				surplus.at(curr_month.ec_energy_surplus.nrows() + 1, ic + 1) = s_col_total;
			}
			charge.at(curr_month.ec_charge.nrows() + 1, curr_month.ec_charge.ncols() + 1) = c_total;
			energy.at(curr_month.ec_energy_use.nrows() + 1, curr_month.ec_energy_use.ncols() + 1) = e_total;
			surplus.at(curr_month.ec_energy_surplus.nrows() + 1, curr_month.ec_energy_surplus.ncols() + 1) = s_total;
		}

	}


};

DEFINE_MODULE_ENTRY(utilityrate5, "Electricity bill calculator based on OpenEI Version 8", 1);
