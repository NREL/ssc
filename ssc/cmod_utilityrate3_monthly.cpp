#include "core.h"
#include <sstream>



static var_info vtab_utility_rate3_monthly[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                                           UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,     "analysis_period",           "Number of years in analysis",                   "years",  "",                      "",             "*",                         "INTEGER,POSITIVE",              "" },

	{ SSC_INPUT, SSC_NUMBER, "system_use_lifetime_output", "Lifetime hourly system outputs", "0/1", "0=hourly first year,1=hourly lifetime", "", "*", "INTEGER,MIN=0,MAX=1", "" },

	/* change to load, grid and gen per 4/9/15 meeting
	{ SSC_INPUT,        SSC_ARRAY,      "hourly_energy",            "Energy at grid with system",                "kWh",    "",                      "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_INPUT,        SSC_ARRAY,      "p_with_system",            "Max power at grid with system",                 "kW",     "",                      "",             "?",                         "LENGTH=8760",                   "" },
	{ SSC_INPUT, SSC_ARRAY, "e_load", "Energy at grid without system (load only)", "kWh", "", "", "?", "LENGTH=8760", "" },
	{ SSC_INPUT, SSC_ARRAY, "p_load", "Max power at grid without system (load only)", "kW", "", "", "?", "LENGTH=8760", "" },
	*/
	// First year hourly or subhourly
	// load and gen expected to be > 0
	// grid positive if system generation > load, negative otherwise
	{ SSC_INPUT, SSC_ARRAY, "gen", "System power generated", "kW", "", "Time Series", "*", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "load", "Electric load", "kW", "", "Time Series", "*", "", "" },
	// optional
//	{ SSC_INPUT, SSC_ARRAY, "grid", "System power delivered to grid", "kW", "", "Time Series", "", "", "" },

// 4/16/15 meeting update
// load can be subhourly but hourly_grid is hourly from performance models.
//	{ SSC_INPUT, SSC_ARRAY, "load", "Electric load", "kW", "", "Time Series", "*", "", "" },
//	{ SSC_INPUT, SSC_ARRAY, "hourly_grid", "Net grid power", "kW", "", "Time Series", "*", "LENGTH=8760", "" },

	{ SSC_INPUT, SSC_NUMBER, "inflation_rate", "Inflation rate", "%", "", "Financials", "*", "MIN=0,MAX=100", "" },

	{ SSC_INPUT, SSC_ARRAY, "degradation", "Annual energy degradation", "%", "", "AnnualOutput", "*", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "load_escalation", "Annual load escalation", "%/year", "", "", "?=0", "", "" },
	{ SSC_INPUT,        SSC_ARRAY,      "rate_escalation",          "Annual utility rate escalation",  "%/year", "",                      "",             "?=0",                       "",                              "" },
	
	{ SSC_INPUT, SSC_NUMBER, "ur_enable_net_metering", "Enable net metering", "0/1", "Enforce net metering", "", "?=1", "BOOLEAN", "" },
	{ SSC_INPUT, SSC_NUMBER, "ur_sell_eq_buy", "Set sell rate equal to buy rate", "0/1", "Retail and wholesale rates", "", "?=1", "BOOLEAN", "" },
	{ SSC_INPUT, SSC_NUMBER, "ur_excess_monthly_energy_or_dollars", "Net metering handling of monthly excess", "0=Rollover energy,1=Rollover dollars", "Net metering monthly excess", "", "?=0", "INTEGER", "" },

	{ SSC_INPUT, SSC_NUMBER, "ur_nm_yearend_sell_rate", "Year end sell rate", "$/kWh", "", "", "?=0.0", "", "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_monthly_fixed_charge",  "Monthly fixed charge",            "$",      "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_flat_buy_rate",         "Flat rate (buy)",                 "$/kWh",  "",                      "",             "*",                         "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_flat_sell_rate",        "Flat rate (sell)",                "$/kWh",  "",                      "",             "?=0.0",                     "",      "" },

	// urdb minimums
	{ SSC_INPUT, SSC_NUMBER, "ur_monthly_min_charge", "Monthly minimum charge", "$", "", "", "?=0.0", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "ur_annual_min_charge", "Annual minimum charge", "$", "", "", "?=0.0", "", "" },



	// Energy Charge Inputs
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_enable",            "Enable energy charge",        "0/1",    "",                      "",             "?=0",                       "BOOLEAN",                       "" },

	{ SSC_INPUT, SSC_MATRIX, "ur_ec_sched_weekday", "Energy Charge Weekday Schedule", "", "12x24", "", "ur_ec_enable=1", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "ur_ec_sched_weekend", "Energy Charge Weekend Schedule", "", "12x24", "", "ur_ec_enable=1", "", "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t1_br",       "Period 1 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t1_sr",       "Period 1 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t1_ub",       "Period 1 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t2_br",       "Period 1 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t2_sr",       "Period 1 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t2_ub",       "Period 1 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t3_br",       "Period 1 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t3_sr",       "Period 1 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t3_ub",       "Period 1 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t4_br",       "Period 1 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t4_sr",       "Period 1 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t4_ub",       "Period 1 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t5_br",       "Period 1 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t5_sr",       "Period 1 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t5_ub",       "Period 1 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t6_br",       "Period 1 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t6_sr",       "Period 1 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p1_t6_ub",       "Period 1 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t1_br",       "Period 2 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t1_sr",       "Period 2 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t1_ub",       "Period 2 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t2_br",       "Period 2 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t2_sr",       "Period 2 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t2_ub",       "Period 2 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t3_br",       "Period 2 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t3_sr",       "Period 2 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t3_ub",       "Period 2 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t4_br",       "Period 2 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t4_sr",       "Period 2 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t4_ub",       "Period 2 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t5_br",       "Period 2 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t5_sr",       "Period 2 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t5_ub",       "Period 2 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t6_br",       "Period 2 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t6_sr",       "Period 2 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p2_t6_ub",       "Period 2 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t1_br",       "Period 3 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t1_sr",       "Period 3 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t1_ub",       "Period 3 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t2_br",       "Period 3 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t2_sr",       "Period 3 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t2_ub",       "Period 3 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t3_br",       "Period 3 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t3_sr",       "Period 3 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t3_ub",       "Period 3 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t4_br",       "Period 3 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t4_sr",       "Period 3 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t4_ub",       "Period 3 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t5_br",       "Period 3 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t5_sr",       "Period 3 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t5_ub",       "Period 3 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t6_br",       "Period 3 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t6_sr",       "Period 3 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p3_t6_ub",       "Period 3 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t1_br",       "Period 4 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t1_sr",       "Period 4 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t1_ub",       "Period 4 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t2_br",       "Period 4 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t2_sr",       "Period 4 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t2_ub",       "Period 4 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t3_br",       "Period 4 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t3_sr",       "Period 4 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t3_ub",       "Period 4 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t4_br",       "Period 4 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t4_sr",       "Period 4 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t4_ub",       "Period 4 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t5_br",       "Period 4 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t5_sr",       "Period 4 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t5_ub",       "Period 4 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t6_br",       "Period 4 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t6_sr",       "Period 4 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p4_t6_ub",       "Period 4 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t1_br",       "Period 5 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t1_sr",       "Period 5 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t1_ub",       "Period 5 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t2_br",       "Period 5 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t2_sr",       "Period 5 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t2_ub",       "Period 5 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t3_br",       "Period 5 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t3_sr",       "Period 5 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t3_ub",       "Period 5 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t4_br",       "Period 5 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t4_sr",       "Period 5 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t4_ub",       "Period 5 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t5_br",       "Period 5 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t5_sr",       "Period 5 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t5_ub",       "Period 5 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t6_br",       "Period 5 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t6_sr",       "Period 5 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p5_t6_ub",       "Period 5 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t1_br",       "Period 6 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t1_sr",       "Period 6 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t1_ub",       "Period 6 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t2_br",       "Period 6 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t2_sr",       "Period 6 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t2_ub",       "Period 6 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t3_br",       "Period 6 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t3_sr",       "Period 6 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t3_ub",       "Period 6 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t4_br",       "Period 6 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t4_sr",       "Period 6 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t4_ub",       "Period 6 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t5_br",       "Period 6 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t5_sr",       "Period 6 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t5_ub",       "Period 6 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t6_br",       "Period 6 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t6_sr",       "Period 6 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p6_t6_ub",       "Period 6 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t1_br",       "Period 7 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t1_sr",       "Period 7 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t1_ub",       "Period 7 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t2_br",       "Period 7 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t2_sr",       "Period 7 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t2_ub",       "Period 7 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t3_br",       "Period 7 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t3_sr",       "Period 7 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t3_ub",       "Period 7 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t4_br",       "Period 7 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t4_sr",       "Period 7 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t4_ub",       "Period 7 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t5_br",       "Period 7 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t5_sr",       "Period 7 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t5_ub",       "Period 7 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t6_br",       "Period 7 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t6_sr",       "Period 7 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p7_t6_ub",       "Period 7 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t1_br",       "Period 8 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t1_sr",       "Period 8 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t1_ub",       "Period 8 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t2_br",       "Period 8 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t2_sr",       "Period 8 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t2_ub",       "Period 8 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t3_br",       "Period 8 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t3_sr",       "Period 8 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t3_ub",       "Period 8 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t4_br",       "Period 8 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t4_sr",       "Period 8 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t4_ub",       "Period 8 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t5_br",       "Period 8 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t5_sr",       "Period 8 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t5_ub",       "Period 8 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t6_br",       "Period 8 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t6_sr",       "Period 8 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p8_t6_ub",       "Period 8 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t1_br",       "Period 9 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t1_sr",       "Period 9 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t1_ub",       "Period 9 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t2_br",       "Period 9 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t2_sr",       "Period 9 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t2_ub",       "Period 9 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t3_br",       "Period 9 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t3_sr",       "Period 9 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t3_ub",       "Period 9 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t4_br",       "Period 9 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t4_sr",       "Period 9 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t4_ub",       "Period 9 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t5_br",       "Period 9 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t5_sr",       "Period 9 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t5_ub",       "Period 9 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t6_br",       "Period 9 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t6_sr",       "Period 9 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p9_t6_ub",       "Period 9 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t1_br",       "Period 10 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t1_sr",       "Period 10 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t1_ub",       "Period 10 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t2_br",       "Period 10 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t2_sr",       "Period 10 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t2_ub",       "Period 10 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t3_br",       "Period 10 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t3_sr",       "Period 10 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t3_ub",       "Period 10 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t4_br",       "Period 10 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t4_sr",       "Period 10 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t4_ub",       "Period 10 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t5_br",       "Period 10 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t5_sr",       "Period 10 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t5_ub",       "Period 10 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t6_br",       "Period 10 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t6_sr",       "Period 10 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p10_t6_ub",       "Period 10 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t1_br",       "Period 11 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t1_sr",       "Period 11 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t1_ub",       "Period 11 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t2_br",       "Period 11 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t2_sr",       "Period 11 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t2_ub",       "Period 11 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t3_br",       "Period 11 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t3_sr",       "Period 11 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t3_ub",       "Period 11 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t4_br",       "Period 11 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t4_sr",       "Period 11 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t4_ub",       "Period 11 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t5_br",       "Period 11 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t5_sr",       "Period 11 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t5_ub",       "Period 11 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t6_br",       "Period 11 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t6_sr",       "Period 11 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p11_t6_ub",       "Period 11 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t1_br",       "Period 12 Tier 1 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t1_sr",       "Period 12 Tier 1 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t1_ub",       "Period 12 Tier 1 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t2_br",       "Period 12 Tier 2 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t2_sr",       "Period 12 Tier 2 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t2_ub",       "Period 12 Tier 2 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t3_br",       "Period 12 Tier 3 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t3_sr",       "Period 12 Tier 3 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t3_ub",       "Period 12 Tier 3 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t4_br",       "Period 12 Tier 4 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t4_sr",       "Period 12 Tier 4 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t4_ub",       "Period 12 Tier 4 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t5_br",       "Period 12 Tier 5 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t5_sr",       "Period 12 Tier 5 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t5_ub",       "Period 12 Tier 5 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t6_br",       "Period 12 Tier 6 Energy Buy Rate",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t6_sr",       "Period 12 Tier 6 Energy Sell Rate",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_ec_p12_t6_ub",       "Period 12 Tier 6 Maximum Energy Usage",         "kWh",  "",                      "",             "?=0.0",                     "",                              "" },



	// Demand Charge Inputs
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_enable",            "Enable Demand Charge",        "0/1",    "",                      "",             "?=0",                       "BOOLEAN",                       "" },

//	{ SSC_INPUT, SSC_MATRIX, "ur_dc_sched_weekday", "Demend Charge Weekday Schedule", "", "12x24", "", "ur_dc_enable=1", "", "" },
//	{ SSC_INPUT, SSC_MATRIX, "ur_dc_sched_weekend", "Demend Charge Weekend Schedule", "", "12x24", "", "ur_dc_enable=1", "", "" },
// optional input for flat monthly demand charge per email from Mike Gleason 1/16/15
	{ SSC_INPUT, SSC_MATRIX, "ur_dc_sched_weekday", "Demend Charge Weekday Schedule", "", "12x24", "", "", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "ur_dc_sched_weekend", "Demend Charge Weekend Schedule", "", "12x24", "", "", "", "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t1_dc",       "Period 1 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t1_ub",       "Period 1 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t2_dc",       "Period 1 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t2_ub",       "Period 1 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t3_dc",       "Period 1 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t3_ub",       "Period 1 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t4_dc",       "Period 1 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t4_ub",       "Period 1 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t5_dc",       "Period 1 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t5_ub",       "Period 1 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t6_dc",       "Period 1 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1_t6_ub",       "Period 1 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t1_dc",       "Period 2 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t1_ub",       "Period 2 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t2_dc",       "Period 2 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t2_ub",       "Period 2 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t3_dc",       "Period 2 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t3_ub",       "Period 2 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t4_dc",       "Period 2 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t4_ub",       "Period 2 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t5_dc",       "Period 2 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t5_ub",       "Period 2 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t6_dc",       "Period 2 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2_t6_ub",       "Period 2 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t1_dc",       "Period 3 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t1_ub",       "Period 3 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t2_dc",       "Period 3 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t2_ub",       "Period 3 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t3_dc",       "Period 3 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t3_ub",       "Period 3 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t4_dc",       "Period 3 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t4_ub",       "Period 3 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t5_dc",       "Period 3 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t5_ub",       "Period 3 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t6_dc",       "Period 3 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3_t6_ub",       "Period 3 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t1_dc",       "Period 4 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t1_ub",       "Period 4 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t2_dc",       "Period 4 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t2_ub",       "Period 4 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t3_dc",       "Period 4 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t3_ub",       "Period 4 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t4_dc",       "Period 4 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t4_ub",       "Period 4 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t5_dc",       "Period 4 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t5_ub",       "Period 4 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t6_dc",       "Period 4 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4_t6_ub",       "Period 4 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t1_dc",       "Period 5 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t1_ub",       "Period 5 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t2_dc",       "Period 5 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t2_ub",       "Period 5 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t3_dc",       "Period 5 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t3_ub",       "Period 5 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t4_dc",       "Period 5 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t4_ub",       "Period 5 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t5_dc",       "Period 5 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t5_ub",       "Period 5 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t6_dc",       "Period 5 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5_t6_ub",       "Period 5 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t1_dc",       "Period 6 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t1_ub",       "Period 6 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t2_dc",       "Period 6 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t2_ub",       "Period 6 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t3_dc",       "Period 6 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t3_ub",       "Period 6 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t4_dc",       "Period 6 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t4_ub",       "Period 6 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t5_dc",       "Period 6 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t5_ub",       "Period 6 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t6_dc",       "Period 6 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6_t6_ub",       "Period 6 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t1_dc",       "Period 7 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t1_ub",       "Period 7 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t2_dc",       "Period 7 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t2_ub",       "Period 7 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t3_dc",       "Period 7 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t3_ub",       "Period 7 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t4_dc",       "Period 7 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t4_ub",       "Period 7 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t5_dc",       "Period 7 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t5_ub",       "Period 7 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t6_dc",       "Period 7 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7_t6_ub",       "Period 7 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t1_dc",       "Period 8 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t1_ub",       "Period 8 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t2_dc",       "Period 8 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t2_ub",       "Period 8 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t3_dc",       "Period 8 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t3_ub",       "Period 8 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t4_dc",       "Period 8 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t4_ub",       "Period 8 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t5_dc",       "Period 8 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t5_ub",       "Period 8 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t6_dc",       "Period 8 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8_t6_ub",       "Period 8 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t1_dc",       "Period 9 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t1_ub",       "Period 9 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t2_dc",       "Period 9 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t2_ub",       "Period 9 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t3_dc",       "Period 9 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t3_ub",       "Period 9 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t4_dc",       "Period 9 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t4_ub",       "Period 9 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t5_dc",       "Period 9 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t5_ub",       "Period 9 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t6_dc",       "Period 9 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9_t6_ub",       "Period 9 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t1_dc",       "Period 10 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t1_ub",       "Period 10 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t2_dc",       "Period 10 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t2_ub",       "Period 10 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t3_dc",       "Period 10 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t3_ub",       "Period 10 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t4_dc",       "Period 10 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t4_ub",       "Period 10 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t5_dc",       "Period 10 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t5_ub",       "Period 10 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t6_dc",       "Period 10 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p10_t6_ub",       "Period 10 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t1_dc",       "Period 11 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t1_ub",       "Period 11 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t2_dc",       "Period 11 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t2_ub",       "Period 11 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t3_dc",       "Period 11 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t3_ub",       "Period 11 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t4_dc",       "Period 11 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t4_ub",       "Period 11 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t5_dc",       "Period 11 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t5_ub",       "Period 11 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t6_dc",       "Period 11 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p11_t6_ub",       "Period 11 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t1_dc",       "Period 12 Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t1_ub",       "Period 12 Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t2_dc",       "Period 12 Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t2_ub",       "Period 12 Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t3_dc",       "Period 12 Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t3_ub",       "Period 12 Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t4_dc",       "Period 12 Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t4_ub",       "Period 12 Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t5_dc",       "Period 12 Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t5_ub",       "Period 12 Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t6_dc",       "Period 12 Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p12_t6_ub",       "Period 12 Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t1_dc",       "January Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t1_ub",       "January Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t2_dc",       "January Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t2_ub",       "January Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t3_dc",       "January Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t3_ub",       "January Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t4_dc",       "January Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t4_ub",       "January Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t5_dc",       "January Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t5_ub",       "January Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t6_dc",       "January Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jan_t6_ub",       "January Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t1_dc",       "February Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t1_ub",       "February Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t2_dc",       "February Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t2_ub",       "February Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t3_dc",       "February Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t3_ub",       "February Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t4_dc",       "February Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t4_ub",       "February Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t5_dc",       "February Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t5_ub",       "February Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t6_dc",       "February Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_feb_t6_ub",       "February Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t1_dc",       "March Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t1_ub",       "March Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t2_dc",       "March Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t2_ub",       "March Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t3_dc",       "March Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t3_ub",       "March Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t4_dc",       "March Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t4_ub",       "March Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t5_dc",       "March Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t5_ub",       "March Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t6_dc",       "March Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_mar_t6_ub",       "March Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t1_dc",       "April Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t1_ub",       "April Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t2_dc",       "April Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t2_ub",       "April Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t3_dc",       "April Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t3_ub",       "April Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t4_dc",       "April Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t4_ub",       "April Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t5_dc",       "April Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t5_ub",       "April Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t6_dc",       "April Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_apr_t6_ub",       "April Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t1_dc",       "May Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t1_ub",       "May Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t2_dc",       "May Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t2_ub",       "May Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t3_dc",       "May Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t3_ub",       "May Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t4_dc",       "May Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t4_ub",       "May Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t5_dc",       "May Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t5_ub",       "May Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t6_dc",       "May Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_may_t6_ub",       "May Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t1_dc",       "June Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t1_ub",       "June Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t2_dc",       "June Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t2_ub",       "June Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t3_dc",       "June Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t3_ub",       "June Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t4_dc",       "June Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t4_ub",       "June Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t5_dc",       "June Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t5_ub",       "June Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t6_dc",       "June Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jun_t6_ub",       "June Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t1_dc",       "July Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t1_ub",       "July Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t2_dc",       "July Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t2_ub",       "July Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t3_dc",       "July Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t3_ub",       "July Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t4_dc",       "July Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t4_ub",       "July Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t5_dc",       "July Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t5_ub",       "July Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t6_dc",       "July Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_jul_t6_ub",       "July Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t1_dc",       "August Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t1_ub",       "August Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t2_dc",       "August Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t2_ub",       "August Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t3_dc",       "August Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t3_ub",       "August Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t4_dc",       "August Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t4_ub",       "August Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t5_dc",       "August Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t5_ub",       "August Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t6_dc",       "August Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_aug_t6_ub",       "August Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t1_dc",       "September Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t1_ub",       "September Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t2_dc",       "September Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t2_ub",       "September Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t3_dc",       "September Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t3_ub",       "September Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t4_dc",       "September Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t4_ub",       "September Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t5_dc",       "September Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t5_ub",       "September Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t6_dc",       "September Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_sep_t6_ub",       "September Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t1_dc",       "October Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t1_ub",       "October Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t2_dc",       "October Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t2_ub",       "October Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t3_dc",       "October Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t3_ub",       "October Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t4_dc",       "October Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t4_ub",       "October Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t5_dc",       "October Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t5_ub",       "October Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t6_dc",       "October Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_oct_t6_ub",       "October Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t1_dc",       "November Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t1_ub",       "November Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t2_dc",       "November Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t2_ub",       "November Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t3_dc",       "November Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t3_ub",       "November Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t4_dc",       "November Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t4_ub",       "November Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t5_dc",       "November Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t5_ub",       "November Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t6_dc",       "November Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_nov_t6_ub",       "November Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t1_dc",       "December Tier 1 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t1_ub",       "December Tier 1 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t2_dc",       "December Tier 2 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t2_ub",       "December Tier 2 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t3_dc",       "December Tier 3 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t3_ub",       "December Tier 3 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t4_dc",       "December Tier 4 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t4_ub",       "December Tier 4 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t5_dc",       "December Tier 5 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t5_ub",       "December Tier 5 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t6_dc",       "December Tier 6 Demand Charge",         "$/kW",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_dec_t6_ub",       "December Tier 6 Peak Demand",         "kW",  "",                      "",             "?=0.0",                     "",                              "" },


	
	// outputs
//	{ SSC_OUTPUT,       SSC_ARRAY,      "energy_value",             "Energy value in each year",     "$",    "",                      "",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "annual_energy_value",             "Energy value in each year",     "$",    "",                      "Annual",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "annual_electric_load",            "Total electric load in each year",  "kWh",    "",                      "Annual",             "*",                         "",   "" },

	// use output from annualoutput not scaled output from here
	//	{ SSC_OUTPUT,       SSC_ARRAY,      "energy_net",               "Energy in each year",           "kW",   "",                      "",             "*",                         "",   "" },


		// outputs from Paul, Nate and Sean 9/9/13
//	{ SSC_OUTPUT,       SSC_ARRAY,      "revenue_with_system",      "Total revenue with system",         "$",    "",                      "",             "*",                         "",   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "revenue_without_system",   "Total revenue without system",      "$",    "",                      "",             "*",                         "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY, "elec_cost_with_system",    "Electricity cost with system",    "$/yr", "", "Annual", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "elec_cost_without_system", "Electricity cost without system", "$/yr", "", "Annual", "*", "", "" },

	// year 1 values for metrics
	{ SSC_OUTPUT, SSC_NUMBER, "elec_cost_with_system_year1",    "Electricity cost with system",    "$/yr", "",    "Financial Metrics", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "elec_cost_without_system_year1", "Electricity cost without system", "$/yr", "",    "Financial Metrics", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "savings_year1",                  "Electricity savings",             "$/yr",    "", "Financial Metrics", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "year1_electric_load",            "Electricity load",                "kWh/yr",  "", "Financial Metrics", "*", "", "" },



//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_e_grid",         "Year 1 electricity to/from grid",       "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_e_tofromgrid",         "Electricity to/from grid",       "kWh", "",                      "Time Series",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_system_output",  "Year 1 hourly electricity from system",     "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_e_demand",       "Year 1 hourly electricity from grid",     "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
	
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_system_to_grid",    "Year 1 hourly electricity to grid",     "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_system_to_load",    "Year 1 hourly system electricity to load",     "kWh", "",                      "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_hourly_load", "Electricity load (first year)", "kWh", "", "Time Series", "*", "LENGTH=8760", "" },

// lifetime load (optional for lifetime analysis)
	{ SSC_OUTPUT, SSC_ARRAY, "lifetime_load", "Electricity load", "kW", "", "Time Series", "system_use_lifetime_output=1", "", "" },

//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_grid",         "Year 1 subhourly peak to/from grid", "kW",  "",                      "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_tofromgrid",         "Electricity to/from grid peak", "kW",  "",                      "Time Series",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_demand",       "Year 1 subhourly peak from grid", "kW",  "",                      "",             "*",                         "LENGTH=8760",                   "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_p_system_to_load",         "Electricity peak load met by system", "kW",  "",                      "Time Series",             "*",                         "LENGTH=8760",                   "" },
	

//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_revenue_with_system",     "Year 1 hourly sales/purchases with sytem",    "$", "",          "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_salespurchases_with_system",     "Electricity sales/purchases with sytem",    "$", "",          "Time Series",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_payment_with_system",     "Year 1 hourly electricity purchases with system",    "$", "",          "",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_income_with_system",      "Year 1 hourly electricity sales with system",     "$", "",          "",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_price_with_system",       "Year 1 hourly energy charge with system",      "$", "",          "",             "*",                         "LENGTH=8760",                   "" },
	
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_revenue_without_system",  "Year 1 hourly sales/purchases without sytem", "$", "",          "",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_salespurchases_without_system",  "Electricity sales/purchases without sytem", "$", "",          "Time Series",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_payment_without_system",  "Year 1 hourly electricity purchases without system", "$", "",          "",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_income_without_system",   "Year 1 hourly electricity sales without system",  "$", "",          "",             "*",                         "LENGTH=8760",                   "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_price_without_system",    "Year 1 hourly energy charge without system",   "$", "",          "",             "*",                         "LENGTH=8760",                   "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_dc_with_system",        "Electricity demand charge with system",      "$/kW", "",          "Time Series",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_dc_without_system",     "Electricity demand charge without system",      "$/kW", "",          "Time Series",             "*",                         "LENGTH=8760",                   "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_ec_tou_schedule",       "Electricity TOU period for energy charges", "", "",          "Time Series",             "*",                         "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_dc_tou_schedule",       "Electricity TOU period for demand charges", "", "", "Time Series", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_hourly_dc_peak_per_period",    "Electricity from grid peak per TOU period",        "kW", "", "Time Series", "*", "LENGTH=8760", "" },


	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_dc_fixed_with_system",      "Electricity demand charge (Fixed) with system",    "$/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_dc_tou_with_system",        "Electricity demand charge (TOU) with system",      "$/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_ec_charge_with_system",     "Electricity energy charge with system",            "$/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_ec_rate_with_system",       "Year 1 monthly energy rate with system",              "$/kWh", "", "",          "*",                         "LENGTH=12",                     "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_dc_fixed_without_system",   "Electricity demand charge (Fixed) without system", "$/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_dc_tou_without_system",     "Electricity demand charge (TOU) without system",   "$/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_ec_charge_without_system",  "Electricity energy charge without system",         "$/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_ec_rate_without_system",    "Year 1 monthly energy rate without system",           "$/kWh", "", "",          "*",                         "LENGTH=12",                     "" },


	// monthly outputs from Sean 7/29/13 "Net Metering Accounting.xlsx" updates from Paul and Sean 8/9/13 and 8/12/13
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_load",    "Electricity load",           "kWh/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_system_generation",    "Year 1 monthly system generation",           "kWh", "", "",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_electricity_to_grid",    "Electricity to/from grid",           "kWh/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_electricity_needed_from_grid",    "Electricity needed from grid",           "kWh", "", "",          "*",                         "LENGTH=12",                     "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_cumulative_excess_generation", "Electricity net metering credit", "kWh/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_cumulative_excess_dollars", "Dollar net metering credit", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "year1_monthly_salespurchases", "Electricity sales/purchases with system", "$/mo", "", "Monthly", "*", "LENGTH=12", "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "year1_monthly_salespurchases_wo_sys",    "Electricity sales/purchases without system",           "$/mo", "", "Monthly",          "*",                         "LENGTH=12",                     "" },




	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_jan",      "Demand charge with system (fixed) in Jan",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_feb",      "Demand charge with system (fixed) in Feb",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_mar",      "Demand charge with system (fixed) in Mar",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_apr",      "Demand charge with system (fixed) in Apr",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_may",      "Demand charge with system (fixed) in May",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_jun",      "Demand charge with system (fixed) in Jun",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_jul",      "Demand charge with system (fixed) in Jul",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_aug",      "Demand charge with system (fixed) in Aug",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_sep",      "Demand charge with system (fixed) in Sep",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_oct",      "Demand charge with system (fixed) in Oct",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_fixed_nov",      "Demand charge with system (fixed) in Nov",    "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_dc_fixed_dec", "Demand charge with system (fixed) in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_dc_fixed", "Demand charge with system (fixed)", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_jan",        "Demand charge with system (TOU) in Jan",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_feb",        "Demand charge with system (TOU) in Feb",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_mar",        "Demand charge with system (TOU) in Mar",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_apr",        "Demand charge with system (TOU) in Apr",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_may",        "Demand charge with system (TOU) in May",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_jun",        "Demand charge with system (TOU) in Jun",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_jul",        "Demand charge with system (TOU) in Jul",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_aug",        "Demand charge with system (TOU) in Aug",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_sep",        "Demand charge with system (TOU) in Sep",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_oct",        "Demand charge with system (TOU) in Oct",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_dc_tou_nov",        "Demand charge with system (TOU) in Nov",      "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_dc_tou_dec", "Demand charge with system (TOU) in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_dc_tou", "Demand charge with system (TOU)", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_jan",            "Energy charge with system in Jan",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_feb",            "Energy charge with system in Feb",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_mar",            "Energy charge with system in Mar",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_apr",            "Energy charge with system in Apr",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_may",            "Energy charge with system in May",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_jun",            "Energy charge with system in Jun",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_jul",            "Energy charge with system in Jul",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_aug",            "Energy charge with system in Aug",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_sep",            "Energy charge with system in Sep",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_oct",            "Energy charge with system in Oct",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "charge_w_sys_ec_nov",            "Energy charge with system in Nov",       "$",      "",                      "Charges by Month",             "*",                         "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec_dec", "Energy charge with system in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_w_sys_ec", "Energy charge with system", "$", "", "Charges by Month", "*", "", "" },



	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_jan", "Demand charge without system (fixed) in Jan", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_feb", "Demand charge without system (fixed) in Feb", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_mar", "Demand charge without system (fixed) in Mar", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_apr", "Demand charge without system (fixed) in Apr", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_may", "Demand charge without system (fixed) in May", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_jun", "Demand charge without system (fixed) in Jun", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_jul", "Demand charge without system (fixed) in Jul", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_aug", "Demand charge without system (fixed) in Aug", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_sep", "Demand charge without system (fixed) in Sep", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_oct", "Demand charge without system (fixed) in Oct", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_nov", "Demand charge without system (fixed) in Nov", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed_dec", "Demand charge without system (fixed) in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_fixed", "Demand charge without system (fixed)", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_jan", "Demand charge without system (TOU) in Jan", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_feb", "Demand charge without system (TOU) in Feb", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_mar", "Demand charge without system (TOU) in Mar", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_apr", "Demand charge without system (TOU) in Apr", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_may", "Demand charge without system (TOU) in May", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_jun", "Demand charge without system (TOU) in Jun", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_jul", "Demand charge without system (TOU) in Jul", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_aug", "Demand charge without system (TOU) in Aug", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_sep", "Demand charge without system (TOU) in Sep", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_oct", "Demand charge without system (TOU) in Oct", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_nov", "Demand charge without system (TOU) in Nov", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou_dec", "Demand charge without system (TOU) in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_dc_tou", "Demand charge without system (TOU)", "$", "", "Charges by Month", "*", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jan", "Energy charge without system in Jan", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_feb", "Energy charge without system in Feb", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_mar", "Energy charge without system in Mar", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_apr", "Energy charge without system in Apr", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_may", "Energy charge without system in May", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jun", "Energy charge without system in Jun", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_jul", "Energy charge without system in Jul", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_aug", "Energy charge without system in Aug", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_sep", "Energy charge without system in Sep", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_oct", "Energy charge without system in Oct", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_nov", "Energy charge without system in Nov", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec_dec", "Energy charge without system in Dec", "$", "", "Charges by Month", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "charge_wo_sys_ec", "Energy charge without system", "$", "", "Charges by Month", "*", "", "" },

	var_info_invalid };




class cm_utilityrate3_monthly : public compute_module
{
private:
public:
	cm_utilityrate3_monthly()
	{
		add_var_info( vtab_utility_rate3_monthly );
	}

	void exec( ) throw( general_error )
	{
		ssc_number_t *parr = 0;
		size_t count, i, j; 

		size_t nyears = (size_t)as_integer("analysis_period");
		double inflation_rate = as_double("inflation_rate")*0.01;

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
		std::vector<ssc_number_t> load_scale(nyears);
		parr = as_array("load_escalation", &count);
		if (count == 1)
		{
			for (i=0;i<nyears;i++)
				load_scale[i] = (ssc_number_t)pow( (double)(1+parr[0]*0.01), (double)i );
		}
		else
		{
			for (i=0;i<nyears;i++)
				load_scale[i] = (ssc_number_t)(1 + parr[i]*0.01);
		}

		// compute utility rate out-years escalation multipliers
		std::vector<ssc_number_t> rate_scale(nyears);
		parr = as_array("rate_escalation", &count);
		if (count == 1)
		{
			for (i=0;i<nyears;i++)
				rate_scale[i] = (ssc_number_t)pow( (double)(inflation_rate+1+parr[0]*0.01), (double)i );
		}
		else
		{
			for (i=0;i<nyears;i++)
				rate_scale[i] = (ssc_number_t)(1 + parr[i]*0.01);
		}


		// prepare 8760 arrays for load and grid values
		std::vector<ssc_number_t> e_sys(8760), p_sys(8760), 
			e_load(8760), p_load(8760),
			e_grid(8760), p_grid(8760),
			e_load_cy(8760), p_load_cy(8760); // current year load (accounts for escal)
		



		/* Update all e_sys and e_load values based on new inputs
		grid = gen -load where gen = sys + batt
		1. scale load and system value to hourly values as necessary
		2. use (kWh) e_sys[i] = sum((grid+load) * timestep ) over the hour for each hour i
		3. use (kW)  p_sys[i] = max( grid+load) over the hour for each hour i
		3. use (kWh) e_load[i] = sum(load * timestep ) over the hour for each hour i
		4. use (kW)  p_load[i] = max(load) over the hour for each hour i
		5. After above assignment, proceed as before with same outputs
		*/
		ssc_number_t *pload, *pgen;
		size_t nrec_load = 0, nrec_gen = 0, step_per_hour_gen=1, step_per_hour_load=1;
		bool bload=false;
		pgen = as_array("gen", &nrec_gen);
		// for lifetime analysis
		size_t nrec_gen_per_year = nrec_gen;
		if (as_integer("system_use_lifetime_output") == 1)
			nrec_gen_per_year = nrec_gen / nyears;
		step_per_hour_gen = nrec_gen_per_year / 8760;
		if (step_per_hour_gen < 1 || step_per_hour_gen > 60 || step_per_hour_gen * 8760 != nrec_gen_per_year)
			throw exec_error("utilityrate3", util::format("invalid number of gen records (%d): must be an integer multiple of 8760", (int)nrec_gen_per_year));
		ssc_number_t ts_hour_gen = 1.0f / step_per_hour_gen;


		if (is_assigned("load"))
		{ // hourly or sub hourly loads for single yer
			bload = true;
			pload = as_array("load", &nrec_load);
			step_per_hour_load = nrec_load / 8760;
			if (step_per_hour_load < 1 || step_per_hour_load > 60 || step_per_hour_load * 8760 != nrec_load)
				throw exec_error("utilityrate3", util::format("invalid number of load records (%d): must be an integer multiple of 8760", (int)nrec_load));
//			if (nrec_load != nrec_gen)
//				throw exec_error("utilityrate3", util::format("number of load records (%d) must be equal to number of gen records (%d)", (int)nrec_load, (int)nrec_gen));
		}
		ssc_number_t ts_hour_load = 1.0f / step_per_hour_load;


		// assign hourly values for utility rate calculations
		size_t idx = 0;
		ssc_number_t ts_power = 0;
		ssc_number_t ts_load = 0;
		ssc_number_t year1_elec_load = 0;
		for (i = 0; i < 8760; i++)
		{
			e_sys[i] = p_sys[i] = e_grid[i] = p_grid[i] = e_load[i] = p_load[i] = e_load_cy[i] = p_load_cy[i] = 0.0;
			for (size_t ii = 0; ii < step_per_hour_gen; ii++)
			{
				ts_power = pgen[idx];
				e_sys[i] += ts_power * ts_hour_gen;
				p_sys[i] = ((ts_power > p_sys[i]) ? ts_power : p_sys[i]);
				idx++;
			}
		}
		//load
		idx = 0;
		for (i = 0; i < 8760; i++)
		{
			for (size_t ii = 0; ii < step_per_hour_load; ii++)
			{
				ts_load = (bload ? pload[idx] : 0);
				e_load[i] += ts_load * ts_hour_load;
				p_load[i] = ((ts_load > p_load[i]) ? ts_load : p_load[i]);
				idx++;
			}
			year1_elec_load += e_load[i];
			// sign correction for utility rate calculations
			e_load[i] = -e_load[i];
			p_load[i] = -p_load[i];
		}

		assign("year1_electric_load", year1_elec_load);

		/*
		parr = as_array("hourly_energy", &count);
		for (i=0;i<8760;i++)
		{
			e_sys[i] = p_sys[i] = parr[i]; // by default p_sys = e_sys (since it's hourly)
			// others are 0.0
			e_grid[i] = p_grid[i] = e_load[i] = p_load[i] = e_load_cy[i] = p_load_cy[i] = 0.0;
		}

		if (is_assigned("p_with_system"))
		{
			parr = as_array("p_with_system", &count);
			if (count != 8760) throw general_error("p_with_system must have 8760 values");
			for (i=0;i<8760;i++)
				p_sys[i] = parr[i];
		}

		if (is_assigned("e_load"))
			{
			parr = as_array("e_load", &count);
			if (count != 8760) throw general_error("e_load must have 8760 values");
			for (i=0;i<8760;i++)
			{
				e_load[i] = -parr[i]; // input sign change 9/12/14
				p_load[i] = -parr[i]; // by default p_load = e_load
			}
		}

		//accumulate annual load for outputs
		double year1_elec_load = accumulate_annual("e_load", "year1_electric_load");

		if (is_assigned("p_load"))
			{
			parr = as_array("p_load", &count);
			if (count != 8760) throw general_error("p_load must have 8760 values");
			for (i=0;i<8760;i++)
				p_load[i] = -parr[i]; // input sign change 9/12/14
		}

		*/


		/* allocate intermediate data arrays */
		std::vector<ssc_number_t> revenue_w_sys(8760), revenue_wo_sys(8760),
			payment(8760), income(8760), price(8760), demand_charge(8760), 
			ec_tou_sched(8760), dc_tou_sched(8760), load(8760), dc_hourly_peak(8760),
			e_tofromgrid(8760), p_tofromgrid(8760),	salespurchases(8760);
		std::vector<ssc_number_t> monthly_revenue_w_sys(12), monthly_revenue_wo_sys(12),
			monthly_fixed_charges(12),
			monthly_dc_fixed(12), monthly_dc_tou(12),
			monthly_ec_charges(12), monthly_ec_rates(12), 
			monthly_salespurchases(12),
			monthly_load(12), monthly_system_generation(12), monthly_elec_to_grid(12), 
			monthly_elec_needed_from_grid(12), 
			monthly_cumulative_excess_energy(12), monthly_cumulative_excess_dollars(12), monthly_bill(12);

		/* allocate outputs */		
		ssc_number_t *annual_net_revenue = allocate("annual_energy_value", nyears+1);
		ssc_number_t *annual_electric_load = allocate("annual_electric_load", nyears+1);
		ssc_number_t *energy_net = allocate("scaled_annual_energy", nyears+1);
		ssc_number_t *annual_revenue_w_sys = allocate("revenue_with_system", nyears+1);
		ssc_number_t *annual_revenue_wo_sys = allocate("revenue_without_system", nyears+1);
		ssc_number_t *annual_elec_cost_w_sys = allocate("elec_cost_with_system", nyears+1);
		ssc_number_t *annual_elec_cost_wo_sys = allocate("elec_cost_without_system", nyears+1);

		ssc_number_t *ch_w_sys_dc_fixed_jan = allocate("charge_w_sys_dc_fixed_jan", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_feb = allocate("charge_w_sys_dc_fixed_feb", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_mar = allocate("charge_w_sys_dc_fixed_mar", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_apr = allocate("charge_w_sys_dc_fixed_apr", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_may = allocate("charge_w_sys_dc_fixed_may", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_jun = allocate("charge_w_sys_dc_fixed_jun", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_jul = allocate("charge_w_sys_dc_fixed_jul", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_aug = allocate("charge_w_sys_dc_fixed_aug", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_sep = allocate("charge_w_sys_dc_fixed_sep", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_oct = allocate("charge_w_sys_dc_fixed_oct", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_nov = allocate("charge_w_sys_dc_fixed_nov", nyears+1 );
		ssc_number_t *ch_w_sys_dc_fixed_dec = allocate("charge_w_sys_dc_fixed_dec", nyears + 1);
		ssc_number_t *ch_w_sys_dc_fixed = allocate("charge_w_sys_dc_fixed", nyears + 1);

		ssc_number_t *ch_w_sys_dc_tou_jan = allocate("charge_w_sys_dc_tou_jan", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_feb = allocate("charge_w_sys_dc_tou_feb", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_mar = allocate("charge_w_sys_dc_tou_mar", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_apr = allocate("charge_w_sys_dc_tou_apr", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_may = allocate("charge_w_sys_dc_tou_may", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_jun = allocate("charge_w_sys_dc_tou_jun", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_jul = allocate("charge_w_sys_dc_tou_jul", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_aug = allocate("charge_w_sys_dc_tou_aug", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_sep = allocate("charge_w_sys_dc_tou_sep", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_oct = allocate("charge_w_sys_dc_tou_oct", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_nov = allocate("charge_w_sys_dc_tou_nov", nyears+1 );
		ssc_number_t *ch_w_sys_dc_tou_dec = allocate("charge_w_sys_dc_tou_dec", nyears + 1);
		ssc_number_t *ch_w_sys_dc_tou = allocate("charge_w_sys_dc_tou", nyears + 1);

		ssc_number_t *ch_w_sys_ec_jan = allocate("charge_w_sys_ec_jan", nyears+1 );
		ssc_number_t *ch_w_sys_ec_feb = allocate("charge_w_sys_ec_feb", nyears+1 );
		ssc_number_t *ch_w_sys_ec_mar = allocate("charge_w_sys_ec_mar", nyears+1 );
		ssc_number_t *ch_w_sys_ec_apr = allocate("charge_w_sys_ec_apr", nyears+1 );
		ssc_number_t *ch_w_sys_ec_may = allocate("charge_w_sys_ec_may", nyears+1 );
		ssc_number_t *ch_w_sys_ec_jun = allocate("charge_w_sys_ec_jun", nyears+1 );
		ssc_number_t *ch_w_sys_ec_jul = allocate("charge_w_sys_ec_jul", nyears+1 );
		ssc_number_t *ch_w_sys_ec_aug = allocate("charge_w_sys_ec_aug", nyears+1 );
		ssc_number_t *ch_w_sys_ec_sep = allocate("charge_w_sys_ec_sep", nyears+1 );
		ssc_number_t *ch_w_sys_ec_oct = allocate("charge_w_sys_ec_oct", nyears+1 );
		ssc_number_t *ch_w_sys_ec_nov = allocate("charge_w_sys_ec_nov", nyears+1 );
		ssc_number_t *ch_w_sys_ec_dec = allocate("charge_w_sys_ec_dec", nyears + 1);
		ssc_number_t *ch_w_sys_ec = allocate("charge_w_sys_ec", nyears + 1);

		ssc_number_t *ch_wo_sys_dc_fixed_jan = allocate("charge_wo_sys_dc_fixed_jan", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_feb = allocate("charge_wo_sys_dc_fixed_feb", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_mar = allocate("charge_wo_sys_dc_fixed_mar", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_apr = allocate("charge_wo_sys_dc_fixed_apr", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_may = allocate("charge_wo_sys_dc_fixed_may", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_jun = allocate("charge_wo_sys_dc_fixed_jun", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_jul = allocate("charge_wo_sys_dc_fixed_jul", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_aug = allocate("charge_wo_sys_dc_fixed_aug", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_sep = allocate("charge_wo_sys_dc_fixed_sep", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_oct = allocate("charge_wo_sys_dc_fixed_oct", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_nov = allocate("charge_wo_sys_dc_fixed_nov", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed_dec = allocate("charge_wo_sys_dc_fixed_dec", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_fixed = allocate("charge_wo_sys_dc_fixed", nyears + 1);

		ssc_number_t *ch_wo_sys_dc_tou_jan = allocate("charge_wo_sys_dc_tou_jan", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_feb = allocate("charge_wo_sys_dc_tou_feb", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_mar = allocate("charge_wo_sys_dc_tou_mar", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_apr = allocate("charge_wo_sys_dc_tou_apr", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_may = allocate("charge_wo_sys_dc_tou_may", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_jun = allocate("charge_wo_sys_dc_tou_jun", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_jul = allocate("charge_wo_sys_dc_tou_jul", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_aug = allocate("charge_wo_sys_dc_tou_aug", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_sep = allocate("charge_wo_sys_dc_tou_sep", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_oct = allocate("charge_wo_sys_dc_tou_oct", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_nov = allocate("charge_wo_sys_dc_tou_nov", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou_dec = allocate("charge_wo_sys_dc_tou_dec", nyears + 1);
		ssc_number_t *ch_wo_sys_dc_tou = allocate("charge_wo_sys_dc_tou", nyears + 1);

		ssc_number_t *ch_wo_sys_ec_jan = allocate("charge_wo_sys_ec_jan", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_feb = allocate("charge_wo_sys_ec_feb", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_mar = allocate("charge_wo_sys_ec_mar", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_apr = allocate("charge_wo_sys_ec_apr", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_may = allocate("charge_wo_sys_ec_may", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_jun = allocate("charge_wo_sys_ec_jun", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_jul = allocate("charge_wo_sys_ec_jul", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_aug = allocate("charge_wo_sys_ec_aug", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_sep = allocate("charge_wo_sys_ec_sep", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_oct = allocate("charge_wo_sys_ec_oct", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_nov = allocate("charge_wo_sys_ec_nov", nyears + 1);
		ssc_number_t *ch_wo_sys_ec_dec = allocate("charge_wo_sys_ec_dec", nyears + 1);
		ssc_number_t *ch_wo_sys_ec = allocate("charge_wo_sys_ec", nyears + 1);


		// lifetime hourly load
		ssc_number_t *lifetime_hourly_load = allocate("lifetime_load", nrec_gen);

		idx = 0;
		for (i=0;i<nyears;i++)
		{
			for (j=0;j<8760;j++)
			{
				/* for future implementation for lifetime loads
				// update e_load and p_load per year if lifetime output
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
				e_load_cy[j] = e_load[j] * load_scale[i];
				p_load_cy[j] = p_load[j] * load_scale[i];

				
				// update e_sys per year if lifetime output
				if ((as_integer("system_use_lifetime_output") == 1) )
				{
					e_sys[j] = p_sys[j] = 0.0;
					for (size_t ii = 0; (ii < step_per_hour_gen); ii++)
					{
						ts_power = pgen[idx];
						e_sys[j] += ts_power * ts_hour_gen;
						p_sys[j] = ((ts_power > p_sys[j]) ? ts_power : p_sys[j]);
						// until lifetime load fully implemented
						lifetime_hourly_load[idx] = -e_load_cy[j]*ts_hour_gen;
						idx++;
					}
				}

				// calculate e_grid value (e_sys + e_load)
				// note: load is assumed to have negative sign
				e_grid[j] = e_sys[j] * sys_scale[i] + e_load_cy[j];
				p_grid[j] = p_sys[j]*sys_scale[i] + p_load_cy[j];
			}

			// now recalculate revenue without solar system (using load only)
			ur_calc( &e_load_cy[0], &p_load_cy[0],
				&revenue_wo_sys[0], &payment[0], &income[0], &price[0], &demand_charge[0],
				&monthly_fixed_charges[0],
				&monthly_dc_fixed[0], &monthly_dc_tou[0],
				&monthly_ec_charges[0], &monthly_ec_rates[0], &ec_tou_sched[0], &dc_tou_sched[0], &dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0]);


			ch_wo_sys_dc_fixed_jan[i + 1] = monthly_dc_fixed[0] * rate_scale[i];
			ch_wo_sys_dc_fixed_feb[i + 1] = monthly_dc_fixed[1] * rate_scale[i];
			ch_wo_sys_dc_fixed_mar[i + 1] = monthly_dc_fixed[2] * rate_scale[i];
			ch_wo_sys_dc_fixed_apr[i + 1] = monthly_dc_fixed[3] * rate_scale[i];
			ch_wo_sys_dc_fixed_may[i + 1] = monthly_dc_fixed[4] * rate_scale[i];
			ch_wo_sys_dc_fixed_jun[i + 1] = monthly_dc_fixed[5] * rate_scale[i];
			ch_wo_sys_dc_fixed_jul[i + 1] = monthly_dc_fixed[6] * rate_scale[i];
			ch_wo_sys_dc_fixed_aug[i + 1] = monthly_dc_fixed[7] * rate_scale[i];
			ch_wo_sys_dc_fixed_sep[i + 1] = monthly_dc_fixed[8] * rate_scale[i];
			ch_wo_sys_dc_fixed_oct[i + 1] = monthly_dc_fixed[9] * rate_scale[i];
			ch_wo_sys_dc_fixed_nov[i + 1] = monthly_dc_fixed[10] * rate_scale[i];
			ch_wo_sys_dc_fixed_dec[i + 1] = monthly_dc_fixed[11] * rate_scale[i];
			ch_wo_sys_dc_fixed[i + 1] = ch_wo_sys_dc_fixed_jan[i + 1]
				+ ch_wo_sys_dc_fixed_feb[i + 1]
				+ ch_wo_sys_dc_fixed_mar[i + 1]
				+ ch_wo_sys_dc_fixed_apr[i + 1]
				+ ch_wo_sys_dc_fixed_may[i + 1]
				+ ch_wo_sys_dc_fixed_jun[i + 1]
				+ ch_wo_sys_dc_fixed_jul[i + 1]
				+ ch_wo_sys_dc_fixed_aug[i + 1]
				+ ch_wo_sys_dc_fixed_sep[i + 1]
				+ ch_wo_sys_dc_fixed_oct[i + 1]
				+ ch_wo_sys_dc_fixed_nov[i + 1]
				+ ch_wo_sys_dc_fixed_dec[i + 1];


			ch_wo_sys_dc_tou_jan[i + 1] = monthly_dc_tou[0] * rate_scale[i];
			ch_wo_sys_dc_tou_feb[i + 1] = monthly_dc_tou[1] * rate_scale[i];
			ch_wo_sys_dc_tou_mar[i + 1] = monthly_dc_tou[2] * rate_scale[i];
			ch_wo_sys_dc_tou_apr[i + 1] = monthly_dc_tou[3] * rate_scale[i];
			ch_wo_sys_dc_tou_may[i + 1] = monthly_dc_tou[4] * rate_scale[i];
			ch_wo_sys_dc_tou_jun[i + 1] = monthly_dc_tou[5] * rate_scale[i];
			ch_wo_sys_dc_tou_jul[i + 1] = monthly_dc_tou[6] * rate_scale[i];
			ch_wo_sys_dc_tou_aug[i + 1] = monthly_dc_tou[7] * rate_scale[i];
			ch_wo_sys_dc_tou_sep[i + 1] = monthly_dc_tou[8] * rate_scale[i];
			ch_wo_sys_dc_tou_oct[i + 1] = monthly_dc_tou[9] * rate_scale[i];
			ch_wo_sys_dc_tou_nov[i + 1] = monthly_dc_tou[10] * rate_scale[i];
			ch_wo_sys_dc_tou_dec[i + 1] = monthly_dc_tou[11] * rate_scale[i];
			ch_wo_sys_dc_tou[i + 1] = ch_wo_sys_dc_tou_jan[i + 1]
				+ ch_wo_sys_dc_tou_feb[i + 1]
				+ ch_wo_sys_dc_tou_mar[i + 1]
				+ ch_wo_sys_dc_tou_apr[i + 1]
				+ ch_wo_sys_dc_tou_may[i + 1]
				+ ch_wo_sys_dc_tou_jun[i + 1]
				+ ch_wo_sys_dc_tou_jul[i + 1]
				+ ch_wo_sys_dc_tou_aug[i + 1]
				+ ch_wo_sys_dc_tou_sep[i + 1]
				+ ch_wo_sys_dc_tou_oct[i + 1]
				+ ch_wo_sys_dc_tou_nov[i + 1]
				+ ch_wo_sys_dc_tou_dec[i + 1];

			ch_wo_sys_ec_jan[i + 1] = monthly_ec_charges[0] * rate_scale[i];
			ch_wo_sys_ec_feb[i + 1] = monthly_ec_charges[1] * rate_scale[i];
			ch_wo_sys_ec_mar[i + 1] = monthly_ec_charges[2] * rate_scale[i];
			ch_wo_sys_ec_apr[i + 1] = monthly_ec_charges[3] * rate_scale[i];
			ch_wo_sys_ec_may[i + 1] = monthly_ec_charges[4] * rate_scale[i];
			ch_wo_sys_ec_jun[i + 1] = monthly_ec_charges[5] * rate_scale[i];
			ch_wo_sys_ec_jul[i + 1] = monthly_ec_charges[6] * rate_scale[i];
			ch_wo_sys_ec_aug[i + 1] = monthly_ec_charges[7] * rate_scale[i];
			ch_wo_sys_ec_sep[i + 1] = monthly_ec_charges[8] * rate_scale[i];
			ch_wo_sys_ec_oct[i + 1] = monthly_ec_charges[9] * rate_scale[i];
			ch_wo_sys_ec_nov[i + 1] = monthly_ec_charges[10] * rate_scale[i];
			ch_wo_sys_ec_dec[i + 1] = monthly_ec_charges[11] * rate_scale[i];
			ch_wo_sys_ec[i + 1] = ch_wo_sys_ec_jan[i + 1]
				+ ch_wo_sys_ec_feb[i + 1]
				+ ch_wo_sys_ec_mar[i + 1]
				+ ch_wo_sys_ec_apr[i + 1]
				+ ch_wo_sys_ec_may[i + 1]
				+ ch_wo_sys_ec_jun[i + 1]
				+ ch_wo_sys_ec_jul[i + 1]
				+ ch_wo_sys_ec_aug[i + 1]
				+ ch_wo_sys_ec_sep[i + 1]
				+ ch_wo_sys_ec_oct[i + 1]
				+ ch_wo_sys_ec_nov[i + 1]
				+ ch_wo_sys_ec_dec[i + 1];



			if (i == 0)
			{
				//assign( "year1_hourly_revenue_without_system", var_data( &revenue_wo_sys[0], 8760 ) );
				//assign( "year1_hourly_payment_without_system", var_data( &payment[0], 8760 ) );
				//assign( "year1_hourly_income_without_system", var_data( &income[0], 8760 ) );
				//assign( "year1_hourly_price_without_system", var_data( &price[0], 8760 ) );
				assign( "year1_hourly_dc_without_system", var_data( &demand_charge[0], 8760 ) );
							
				assign( "year1_monthly_dc_fixed_without_system", var_data(&monthly_dc_fixed[0], 12) );
				assign( "year1_monthly_dc_tou_without_system", var_data(&monthly_dc_tou[0], 12) );
				assign( "year1_monthly_ec_charge_without_system", var_data(&monthly_ec_charges[0], 12) );
				//assign( "year1_monthly_ec_rate_without_system", var_data(&monthly_ec_rates[0], 12) );

				// sign reversal based on 9/5/13 meeting, reverse again 9/6/13
				for (int ii=0;ii<8760;ii++) 
				{
					salespurchases[ii] = revenue_wo_sys[ii];
				}
				int c = 0;
				for (int m=0;m<12;m++)
				{
					monthly_salespurchases[m] = 0;
					for (int d=0;d<util::nday[m];d++)
					{
						for(int h=0;h<24;h++)
						{
							monthly_salespurchases[m] += salespurchases[c];
							c++;
						}
					}
				}
				assign( "year1_hourly_salespurchases_without_system", var_data( &salespurchases[0], 8760 ) );
				assign( "year1_monthly_salespurchases_wo_sys", var_data(&monthly_salespurchases[0], 12) );
			}

			// calculate revenue with solar system (using net grid energy & maxpower)
			ur_calc(&e_grid[0], &p_grid[0],
				&revenue_w_sys[0], &payment[0], &income[0], &price[0], &demand_charge[0],
				&monthly_fixed_charges[0],
				&monthly_dc_fixed[0], &monthly_dc_tou[0],
				&monthly_ec_charges[0], &monthly_ec_rates[0], &ec_tou_sched[0], &dc_tou_sched[0], &dc_hourly_peak[0], &monthly_cumulative_excess_energy[0], &monthly_cumulative_excess_dollars[0], &monthly_bill[0]);

			if (i == 0)
			{
				//assign( "year1_hourly_revenue_with_system", var_data( &revenue_w_sys[0], 8760 ) );
				//assign( "year1_hourly_payment_with_system", var_data( &payment[0], 8760 ) );
				//assign( "year1_hourly_income_with_system", var_data( &income[0], 8760 ) );
				//assign( "year1_hourly_price_with_system", var_data( &price[0], 8760 ) );
				assign("year1_hourly_dc_with_system", var_data(&demand_charge[0], 8760));
				//				assign( "year1_hourly_e_grid", var_data( &e_grid[0], 8760 ) );
				//				assign( "year1_hourly_p_grid", var_data( &p_grid[0], 8760 ) );
				assign("year1_hourly_ec_tou_schedule", var_data(&ec_tou_sched[0], 8760));
				assign("year1_hourly_dc_tou_schedule", var_data(&dc_tou_sched[0], 8760));
				assign("year1_hourly_dc_peak_per_period", var_data(&dc_hourly_peak[0], 8760));

				// sign reversal based on 9/5/13 meeting reverse again 9/6/13
				for (int ii = 0; ii<8760; ii++)
				{
					load[ii] = -e_load[ii];
					e_tofromgrid[ii] = e_grid[ii];
					p_tofromgrid[ii] = p_grid[ii];
					salespurchases[ii] = revenue_w_sys[ii];
				}
				// monthly outputs - Paul and Sean 7/29/13 - updated 8/9/13 and 8/12/13 and 9/10/13
				monthly_outputs(&e_load[0], &e_sys[0], &e_grid[0], &salespurchases[0],
					&monthly_load[0], &monthly_system_generation[0], &monthly_elec_to_grid[0],
					&monthly_elec_needed_from_grid[0], &monthly_cumulative_excess_energy[0],
					&monthly_salespurchases[0]);

				assign("year1_hourly_e_tofromgrid", var_data(&e_tofromgrid[0], 8760));
				assign("year1_hourly_p_tofromgrid", var_data(&p_tofromgrid[0], 8760));
				assign("year1_hourly_load", var_data(&load[0], 8760));
				assign("year1_hourly_salespurchases_with_system", var_data(&salespurchases[0], 8760));
				assign("year1_monthly_load", var_data(&monthly_load[0], 12));
				assign("year1_monthly_system_generation", var_data(&monthly_system_generation[0], 12));
				assign("year1_monthly_electricity_to_grid", var_data(&monthly_elec_to_grid[0], 12));
				assign("year1_monthly_electricity_needed_from_grid", var_data(&monthly_elec_needed_from_grid[0], 12));

				assign("year1_monthly_cumulative_excess_generation", var_data(&monthly_cumulative_excess_energy[0], 12));
				assign("year1_monthly_salespurchases", var_data(&monthly_salespurchases[0], 12));

				// output and demand per Paul's email 9/10/10
				// positive demand indicates system does not produce enough electricity to meet load
				// zero if the system produces more than the demand
				std::vector<ssc_number_t> output(8760), edemand(8760), pdemand(8760), e_sys_to_grid(8760), e_sys_to_load(8760), p_sys_to_load(8760);
				for (j = 0; j<8760; j++)
				{
					output[j] = e_sys[j] * sys_scale[i];
					edemand[j] = e_grid[j] < 0.0 ? -e_grid[j] : (ssc_number_t)0.0;
					pdemand[j] = p_grid[j] < 0.0 ? -p_grid[j] : (ssc_number_t)0.0;

					ssc_number_t sys_e_net = output[j] + e_load[j];// loads are assumed negative
					e_sys_to_grid[j] = sys_e_net > 0 ? sys_e_net : (ssc_number_t)0.0;
					e_sys_to_load[j] = sys_e_net > 0 ? -e_load[j] : output[j];

					ssc_number_t sys_p_net = output[j] + p_load[j];// loads are assumed negative
					p_sys_to_load[j] = sys_p_net > 0 ? -p_load[j] : output[j];
				}

				assign("year1_hourly_system_output", var_data(&output[0], 8760));
				assign("year1_hourly_e_demand", var_data(&edemand[0], 8760));
				assign("year1_hourly_p_demand", var_data(&pdemand[0], 8760));

				assign("year1_hourly_system_to_grid", var_data(&e_sys_to_grid[0], 8760));
				assign("year1_hourly_system_to_load", var_data(&e_sys_to_load[0], 8760));
				assign("year1_hourly_p_system_to_load", var_data(&p_sys_to_load[0], 8760));

				assign("year1_monthly_dc_fixed_with_system", var_data(&monthly_dc_fixed[0], 12));
				assign("year1_monthly_dc_tou_with_system", var_data(&monthly_dc_tou[0], 12));
				assign("year1_monthly_ec_charge_with_system", var_data(&monthly_ec_charges[0], 12));
				//assign( "year1_monthly_ec_rate_with_system", var_data(&monthly_ec_rates[0], 12) );
			}

			// determine net-revenue benefit due to solar for year 'i'
			
			annual_net_revenue[i+1] = 0.0;
			annual_electric_load[i + 1] = 0.0;
			energy_net[i + 1] = 0.0;
			annual_revenue_w_sys[i + 1] = 0.0;
			annual_revenue_wo_sys[i + 1] = 0.0;

			for(j=0;j<8760;j++)
			{
				energy_net[i + 1] +=  e_sys[j]*sys_scale[i];
				annual_net_revenue[i + 1] += revenue_w_sys[j] - revenue_wo_sys[j];
				annual_electric_load[i + 1] += -e_load_cy[j];
				annual_revenue_w_sys[i + 1] += revenue_w_sys[j];
				annual_revenue_wo_sys[i + 1] += revenue_wo_sys[j];
			}

			annual_net_revenue[i + 1] *= rate_scale[i];
			annual_revenue_w_sys[i + 1] *= rate_scale[i];
			annual_revenue_wo_sys[i + 1] *= rate_scale[i];

			//Outputs from Paul, Nate and Sean 9/9/13
			annual_elec_cost_w_sys[i + 1] = -annual_revenue_w_sys[i+1];
			annual_elec_cost_wo_sys[i + 1] = -annual_revenue_wo_sys[i+1];


			ch_w_sys_dc_fixed_jan[i + 1] = monthly_dc_fixed[0] * rate_scale[i];
			ch_w_sys_dc_fixed_feb[i + 1] = monthly_dc_fixed[1] * rate_scale[i];
			ch_w_sys_dc_fixed_mar[i + 1] = monthly_dc_fixed[2] * rate_scale[i];
			ch_w_sys_dc_fixed_apr[i + 1] = monthly_dc_fixed[3] * rate_scale[i];
			ch_w_sys_dc_fixed_may[i + 1] = monthly_dc_fixed[4] * rate_scale[i];
			ch_w_sys_dc_fixed_jun[i + 1] = monthly_dc_fixed[5] * rate_scale[i];
			ch_w_sys_dc_fixed_jul[i + 1] = monthly_dc_fixed[6] * rate_scale[i];
			ch_w_sys_dc_fixed_aug[i + 1] = monthly_dc_fixed[7] * rate_scale[i];
			ch_w_sys_dc_fixed_sep[i + 1] = monthly_dc_fixed[8] * rate_scale[i];
			ch_w_sys_dc_fixed_oct[i + 1] = monthly_dc_fixed[9] * rate_scale[i];
			ch_w_sys_dc_fixed_nov[i + 1] = monthly_dc_fixed[10] * rate_scale[i];
			ch_w_sys_dc_fixed_dec[i + 1] = monthly_dc_fixed[11] * rate_scale[i];
			ch_w_sys_dc_fixed[i + 1] = ch_w_sys_dc_fixed_jan[i + 1]
				+ ch_w_sys_dc_fixed_feb[i + 1]
				+ ch_w_sys_dc_fixed_mar[i + 1]
				+ ch_w_sys_dc_fixed_apr[i + 1]
				+ ch_w_sys_dc_fixed_may[i + 1]
				+ ch_w_sys_dc_fixed_jun[i + 1]
				+ ch_w_sys_dc_fixed_jul[i + 1]
				+ ch_w_sys_dc_fixed_aug[i + 1]
				+ ch_w_sys_dc_fixed_sep[i + 1]
				+ ch_w_sys_dc_fixed_oct[i + 1]
				+ ch_w_sys_dc_fixed_nov[i + 1]
				+ ch_w_sys_dc_fixed_dec[i + 1];

		
			ch_w_sys_dc_tou_jan[i + 1] = monthly_dc_tou[0] * rate_scale[i];
			ch_w_sys_dc_tou_feb[i + 1] = monthly_dc_tou[1] * rate_scale[i];
			ch_w_sys_dc_tou_mar[i + 1] = monthly_dc_tou[2] * rate_scale[i];
			ch_w_sys_dc_tou_apr[i + 1] = monthly_dc_tou[3] * rate_scale[i];
			ch_w_sys_dc_tou_may[i + 1] = monthly_dc_tou[4] * rate_scale[i];
			ch_w_sys_dc_tou_jun[i + 1] = monthly_dc_tou[5] * rate_scale[i];
			ch_w_sys_dc_tou_jul[i + 1] = monthly_dc_tou[6] * rate_scale[i];
			ch_w_sys_dc_tou_aug[i + 1] = monthly_dc_tou[7] * rate_scale[i];
			ch_w_sys_dc_tou_sep[i + 1] = monthly_dc_tou[8] * rate_scale[i];
			ch_w_sys_dc_tou_oct[i + 1] = monthly_dc_tou[9] * rate_scale[i];
			ch_w_sys_dc_tou_nov[i + 1] = monthly_dc_tou[10] * rate_scale[i];
			ch_w_sys_dc_tou_dec[i + 1] = monthly_dc_tou[11] * rate_scale[i];
			ch_w_sys_dc_tou[i + 1] = ch_w_sys_dc_tou_jan[i + 1]
				+ ch_w_sys_dc_tou_feb[i + 1]
				+ ch_w_sys_dc_tou_mar[i + 1]
				+ ch_w_sys_dc_tou_apr[i + 1]
				+ ch_w_sys_dc_tou_may[i + 1]
				+ ch_w_sys_dc_tou_jun[i + 1]
				+ ch_w_sys_dc_tou_jul[i + 1]
				+ ch_w_sys_dc_tou_aug[i + 1]
				+ ch_w_sys_dc_tou_sep[i + 1]
				+ ch_w_sys_dc_tou_oct[i + 1]
				+ ch_w_sys_dc_tou_nov[i + 1]
				+ ch_w_sys_dc_tou_dec[i + 1];

			ch_w_sys_ec_jan[i + 1] = monthly_ec_charges[0] * rate_scale[i];
			ch_w_sys_ec_feb[i + 1] = monthly_ec_charges[1] * rate_scale[i];
			ch_w_sys_ec_mar[i + 1] = monthly_ec_charges[2] * rate_scale[i];
			ch_w_sys_ec_apr[i + 1] = monthly_ec_charges[3] * rate_scale[i];
			ch_w_sys_ec_may[i + 1] = monthly_ec_charges[4] * rate_scale[i];
			ch_w_sys_ec_jun[i + 1] = monthly_ec_charges[5] * rate_scale[i];
			ch_w_sys_ec_jul[i + 1] = monthly_ec_charges[6] * rate_scale[i];
			ch_w_sys_ec_aug[i + 1] = monthly_ec_charges[7] * rate_scale[i];
			ch_w_sys_ec_sep[i + 1] = monthly_ec_charges[8] * rate_scale[i];
			ch_w_sys_ec_oct[i + 1] = monthly_ec_charges[9] * rate_scale[i];
			ch_w_sys_ec_nov[i + 1] = monthly_ec_charges[10] * rate_scale[i];
			ch_w_sys_ec_dec[i + 1] = monthly_ec_charges[11] * rate_scale[i];			
			ch_w_sys_ec[i + 1] = ch_w_sys_ec_jan[i + 1]
				+ ch_w_sys_ec_feb[i + 1]
				+ ch_w_sys_ec_mar[i + 1]
				+ ch_w_sys_ec_apr[i + 1]
				+ ch_w_sys_ec_may[i + 1]
				+ ch_w_sys_ec_jun[i + 1]
				+ ch_w_sys_ec_jul[i + 1]
				+ ch_w_sys_ec_aug[i + 1]
				+ ch_w_sys_ec_sep[i + 1]
				+ ch_w_sys_ec_oct[i + 1]
				+ ch_w_sys_ec_nov[i + 1]
				+ ch_w_sys_ec_dec[i + 1];
		}

		assign("elec_cost_with_system_year1", annual_elec_cost_w_sys[1]);
		assign("elec_cost_without_system_year1", annual_elec_cost_wo_sys[1]);
		assign("savings_year1", annual_elec_cost_wo_sys[1] - annual_elec_cost_w_sys[1]);
	}

	void monthly_outputs( ssc_number_t e_load[8760], ssc_number_t e_sys[8760], ssc_number_t e_grid[8760], ssc_number_t salespurchases[8760], ssc_number_t monthly_load[12], ssc_number_t monthly_generation[12], ssc_number_t monthly_elec_to_grid[12], ssc_number_t monthly_elec_needed_from_grid[12], ssc_number_t monthly_cumulative_excess[12], ssc_number_t monthly_salespurchases[12])
	{
		// calculate the monthly net energy and monthly hours
		int m,d,h;
		ssc_number_t energy_use[12]; // 12 months
		int c=0;
//		bool sell_eq_buy = as_boolean("ur_sell_eq_buy");
		bool enable_nm = as_boolean("ur_enable_net_metering");


		for (m=0;m<12;m++)
		{
			energy_use[m] = 0;
			monthly_load[m] = 0;
			monthly_generation[m] = 0;
			monthly_elec_to_grid[m] = 0;
			monthly_cumulative_excess[m] = 0;
			monthly_salespurchases[m] = 0;
			for (d=0;d<util::nday[m];d++)
			{
				for(h=0;h<24;h++)
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
		//
		
		for (m=0;m<12;m++)
		{
			if (monthly_elec_to_grid[m] > 0)
				monthly_elec_needed_from_grid[m] = monthly_elec_to_grid[m];
			else
				monthly_elec_needed_from_grid[m]=0;
		}
	}


	void ur_calc( ssc_number_t e_in[8760], ssc_number_t p_in[8760],

		ssc_number_t revenue[8760], ssc_number_t payment[8760], ssc_number_t income[8760], 
		ssc_number_t price[8760], ssc_number_t demand_charge[8760], 
		ssc_number_t monthly_fixed_charges[12],
		ssc_number_t monthly_dc_fixed[12], ssc_number_t monthly_dc_tou[12],
		ssc_number_t monthly_ec_charges[12], ssc_number_t monthly_ec_rates[12],
		ssc_number_t ec_tou_sched[8760], ssc_number_t dc_tou_sched[8760], ssc_number_t dc_hourly_peak[8760], ssc_number_t monthly_cumulative_excess_energy[12], ssc_number_t monthly_cumulative_excess_dollars[12], ssc_number_t monthly_bill[12]) throw(general_error)
	{
		int i;

		for (i=0;i<8760;i++)
			revenue[i] = payment[i] = income[i] = price[i] = demand_charge[i] = dc_hourly_peak[i] = 0.0;

		for (i=0;i<12;i++)
		{
			monthly_fixed_charges[i] 
				= monthly_dc_fixed[i] = monthly_dc_tou[i] 
				= monthly_ec_charges[i] = monthly_ec_rates[i] 
				= monthly_cumulative_excess_energy[i] 
				= monthly_cumulative_excess_dollars[i] 
				= monthly_bill[i] = 0.0;
		}
		// initialize all montly values
		ssc_number_t buy = as_number("ur_flat_buy_rate");
		ssc_number_t sell = as_number("ur_flat_sell_rate");

		bool sell_eq_buy = as_boolean("ur_sell_eq_buy");
		bool enable_nm = as_boolean("ur_enable_net_metering");
		bool ec_enabled = as_boolean("ur_ec_enable");
		bool dc_enabled = as_boolean("ur_ec_enable");

		bool excess_monthly_dollars = (as_integer("ur_excess_monthly_energy_or_dollars") == 1);
		bool apply_excess_to_flat_rate = !ec_enabled;

		if (sell_eq_buy)
			sell = buy;

		// calculate the monthly net energy and monthly hours
		int m, d, h;
		ssc_number_t monthly_energy_net[12]; // 12 months
		// calculate the monthly net energy per mont
		int hours_per_month[12];
		int c = 0;
		for (m = 0; m < 12; m++)
		{
			monthly_energy_net[m] = 0;
			hours_per_month[m] = 0;
			for (d = 0; d < util::nday[m]; d++)
			{
				for (h = 0; h < 24; h++)
				{
					// net energy use per month
					monthly_energy_net[m] += e_in[c];
					// hours per period per month
					hours_per_month[m]++;
					c++;
				}
			}
		}
		if (enable_nm && !excess_monthly_dollars)
		{
			// monthly cumulative excess energy (positive = excess energy, negative = excess load)
			ssc_number_t prev_value = 0;
			for (m = 0; m < 12; m++)
			{
				prev_value = (m > 0) ? monthly_cumulative_excess_energy[m - 1] : 0;
				monthly_cumulative_excess_energy[m] = ((prev_value + monthly_energy_net[m]) > 0) ? (prev_value + monthly_energy_net[m]) : 0;
			}
		}



// TODO schedules can be initialized outside of ur_calc once!
		// 12 periods with 6 tiers each rates 3rd index = 0 = buy and 1=sell
		ssc_number_t ec_rates[12][6][2];
		ssc_number_t ec_energy_ub[12][6];



		size_t nrows, ncols;
		ssc_number_t *ec_weekday = as_matrix("ur_ec_sched_weekday", &nrows, &ncols);
		if (nrows != 12 || ncols != 24)
		{
			std::ostringstream ss;
			ss << "energy charge weekday schedule must be 12x24, input is " << nrows << "x" << ncols;
			throw exec_error("utilityrate3", ss.str());
		}
		ssc_number_t *ec_weekend = as_matrix("ur_ec_sched_weekend", &nrows, &ncols);
		if (nrows != 12 || ncols != 24)
		{
			std::ostringstream ss;
			ss << "energy charge weekend schedule must be 12x24, input is " << nrows << "x" << ncols;
			throw exec_error("utilityrate3", ss.str());
		}
		util::matrix_t<float> ec_schedwkday(12, 24);
		ec_schedwkday.assign(ec_weekday, nrows, ncols);
		util::matrix_t<float> ec_schedwkend(12, 24);
		ec_schedwkend.assign(ec_weekend, nrows, ncols);

		int ec_tod[8760];

		if (!util::translate_schedule(ec_tod, ec_schedwkday, ec_schedwkend, 1, 12))
			throw general_error("could not translate weekday and weekend schedules for energy charges");

		for (int i = 0; i<8760; i++) ec_tou_sched[i] = (ssc_number_t)(ec_tod[i]);

		// tiered rates for all 6 tiers in each of the 12 periods

		for (int period = 0; period<12; period++)
		{
			std::string str_period = util::to_string(period + 1);

			for (int tier = 0; tier<6; tier++)
			{
				std::string str_tier = util::to_string(tier + 1);

				ec_rates[period][tier][0] = as_number("ur_ec_p" + str_period + "_t" + str_tier + "_br");
				ec_rates[period][tier][1] = sell_eq_buy ? ec_rates[period][tier][0] : as_number("ur_ec_p" + str_period + "_t" + str_tier + "_sr");
				ec_energy_ub[period][tier] = as_number("ur_ec_p" + str_period + "_t" + str_tier + "_ub");
			}
		}




		// calculate the monthly net energy per period
		int period, tier;
		ssc_number_t ec_monthly_energy_net[12][12]; // 12 months, 12 periods
		int ec_hours_per_month_per_period[12][12];
		c = 0;
		for (m = 0; m<12; m++)
		{
			for (period = 0; period<12; period++)
			{
				ec_monthly_energy_net[m][period] = 0;
				ec_hours_per_month_per_period[m][period] = 0;
			}

			for (d = 0; d<util::nday[m]; d++)
			{
				for (h = 0; h<24; h++)
				{
					int todp = ec_tod[c] - 1;
					// net energy use per period per month
					ec_monthly_energy_net[m][todp] += e_in[c];
					// hours per period per month
					ec_hours_per_month_per_period[m][todp]++;
					c++;
				}
			}
		}



// fixed demand charge initialization
		ssc_number_t dc_fixed_charges[12][6];
		ssc_number_t dc_fixed_energy_ub[12][6];

		for (m = 0; m<12; m++)
		{
			for (tier = 0; tier<6; tier++)
			{
				std::string str_tier = util::to_string(tier + 1);
				dc_fixed_charges[m][tier] = as_number("ur_dc_" + util::schedule_int_to_month(m) + "_t" + str_tier + "_dc");
				dc_fixed_energy_ub[m][tier] = as_number("ur_dc_" + util::schedule_int_to_month(m) + "_t" + str_tier + "_ub");
			}
		}


// monthly fixed demand charge peaks
		ssc_number_t monthly_peak[12];  // peak usage for the month (negative value)
		ssc_number_t peak_demand = 0;
		ssc_number_t charge = 0;
		int peak_hour[12];
		c = 0;
		for (m = 0; m < 12; m++)
		{
			monthly_peak[m] = 0;
			peak_hour[m] = 0;
			for (d = 0; d < util::nday[m]; d++)
			{
				for (h = 0; h < 24; h++)
				{
					if (p_in[c] < 0 && p_in[c] < monthly_peak[m])
					{
						monthly_peak[m] = p_in[c];
						peak_hour[m] = c;
					}
					c++;
				}
			}
		}

// demand charge schedules
		ssc_number_t *dc_weekday;
		ssc_number_t *dc_weekend;
		// initialize to diurnal all 1 if only flat monthly demand charge specified per Mike Gleason 1/16/15
		util::matrix_t<float> dc_schedwkday(12, 24, 1);
		util::matrix_t<float> dc_schedwkend(12, 24, 1);
		if (is_assigned("ur_dc_sched_weekday"))
		{
			dc_weekday = as_matrix("ur_dc_sched_weekday", &nrows, &ncols);
			if (nrows != 12 || ncols != 24)
			{
				std::ostringstream ss;
				ss << "demand charge weekday schedule must be 12x24, input is " << nrows << "x" << ncols;
				throw exec_error("utilityrate3", ss.str());
			}
			dc_schedwkday.assign(dc_weekday, nrows, ncols);
		}
		if (is_assigned("ur_dc_sched_weekend"))
		{
			dc_weekend = as_matrix("ur_dc_sched_weekend", &nrows, &ncols);
			if (nrows != 12 || ncols != 24)
			{
				std::ostringstream ss;
				ss << "demand charge weekend schedule must be 12x24, input is " << nrows << "x" << ncols;
				throw exec_error("utilityrate3", ss.str());
			}
			dc_schedwkend.assign(dc_weekend, nrows, ncols);
		}



		int dc_tod[8760];
		if (!util::translate_schedule(dc_tod, dc_schedwkday, dc_schedwkend, 1, 12))
			throw general_error("could not translate weekday and weekend schedules for demand charge time-of-use rate");

		for (int i = 0; i<8760; i++) dc_tou_sched[i] = (ssc_number_t)(dc_tod[i]);


		// extract rate info
		ssc_number_t dc_charges[12][6];
		ssc_number_t dc_energy_ub[12][6];

		for (period = 0; period<12; period++)
		{
			std::string str_period = util::to_string(period + 1);
			for (tier = 0; tier<6; tier++)
			{
				std::string str_tier = util::to_string(tier + 1);
				dc_charges[period][tier] = as_number("ur_dc_p" + str_period + "_t" + str_tier + "_dc");
				dc_energy_ub[period][tier] = as_number("ur_dc_p" + str_period + "_t" + str_tier + "_ub");
			}
		}

		ssc_number_t monthly_period_peak[12][12];  // peak usage for period for the month (negative value)
		int peak_period_hour[12][12];
		c = 0;
		for (m = 0; m < 12; m++)
		{
			for (i = 0; i < 12; i++) // TOU periods
			{
				monthly_period_peak[m][i] = 0;
				peak_period_hour[m][i] = 0;
			}

			for (d = 0; d < util::nday[m]; d++)
			{
				for (h = 0; h < 24; h++)
				{
					int todp = dc_tod[c] - 1;
					if (p_in[c] < 0 && p_in[c] < monthly_period_peak[m][todp])
					{
						monthly_period_peak[m][todp] = p_in[c];
						peak_period_hour[m][todp] = c;
					}
				}
			}
		}




		// process one month at a time
		for (m = 0; m < 12; m++)
		{
// flat rate
			if (hours_per_month[m] <= 0) break;
			for (d = 0; d<util::nday[m]; d++)
			{
				for (h = 0; h<24; h++)
				{
					if (d == util::nday[m] - 1 && h == 23)
					{
						if (enable_nm)
						{
							// monthly rollover with year end sell at reduced rate
							if (!excess_monthly_dollars && (monthly_cumulative_excess_energy[m] == 0)) // buy from grid
							{
								if ((m > 0) && apply_excess_to_flat_rate)
									payment[c] += -(monthly_energy_net[m] + monthly_cumulative_excess_energy[m - 1]) * buy;
								else
									payment[c] += -monthly_energy_net[m] * buy;
							}
							else if (excess_monthly_dollars) // may need to buy from grid
							{
								if (m > 0)
								{
									if (monthly_energy_net[m] < 0) // may need to buy from grid
									{
										ssc_number_t energy_cost = -monthly_energy_net[m] * buy;
										ssc_number_t excess_dollars = -energy_cost;
										if (apply_excess_to_flat_rate)
											excess_dollars += monthly_cumulative_excess_dollars[m - 1];
										if (excess_dollars < 0) // buy from grid
											payment[c] += -excess_dollars;
//										else // update excess dollars
//											monthly_cumulative_excess_dollars[m] = excess_dollars;
									}
//									else // update excess dollars
//										monthly_cumulative_excess_dollars[m] = monthly_energy_net[m] * sell + monthly_cumulative_excess_dollars[m - 1];
								}
								else
								{
									if (monthly_energy_net[m] < 0) // must buy from grid
										payment[c] += -monthly_energy_net[m] * buy;
//									else // update excess dollars
//										monthly_cumulative_excess_dollars[m] = monthly_energy_net[m] * sell;
								}
							}
						}
						else // no net metering - so no rollover.
						{
							if (monthly_energy_net[m] < 0) // must buy from grid
								payment[c] += -monthly_energy_net[m] * buy;
							else
								income[c] += monthly_energy_net[m] * sell;
						}

// Price ?

// end of flat rate

						if (ec_enabled)
						{
							// energy charge
							ssc_number_t monthly_energy = 0;
							for (period = 0; period<12; period++)
							{
								//				charge[m][period]=0;
								//				credit[m][period]=0;

								if (ec_monthly_energy_net[m][period] >= 0.0)
								{ // calculate income or credit
									ssc_number_t credit_amt = 0;
									ssc_number_t energy_surplus = ec_monthly_energy_net[m][period];
									tier = 0;
									while (tier < 6)
									{
										// add up the charge amount for this block
										ssc_number_t e_upper = ec_energy_ub[period][tier];
										ssc_number_t e_lower = tier > 0 ? ec_energy_ub[period][tier - 1] : (ssc_number_t)0.0;

										if (energy_surplus > e_upper)
											credit_amt += (e_upper - e_lower)*ec_rates[period][tier][1];
										else
											credit_amt += (energy_surplus - e_lower)*ec_rates[period][tier][1];

										if (energy_surplus < e_upper)
											break;
										tier++;
									}
									//					credit[m][period] = credit_amt;
									monthly_ec_charges[m] -= credit_amt;
									monthly_energy += energy_surplus;
								}
								else
								{ // calculate payment or charge
									ssc_number_t charge_amt = 0;
									ssc_number_t energy_deficit = -ec_monthly_energy_net[m][period];

									// update 9/1/14 based on feedback from Peter Jeavons 8/28/14
									if (enable_nm && (m>0) && !excess_monthly_dollars && !apply_excess_to_flat_rate) // reduce energy used to calculate charge
										energy_deficit -= fabs(monthly_cumulative_excess_energy[m - 1] * ec_monthly_energy_net[m][period] / monthly_energy_net[m]);

									tier = 0;
									while (tier < 6)
									{
										// add up the charge amount for this block
										ssc_number_t e_upper = ec_energy_ub[period][tier];
										ssc_number_t e_lower = tier > 0 ? ec_energy_ub[period][tier - 1] : (ssc_number_t)0.0;

										if (energy_deficit > e_upper)
											charge_amt += (e_upper - e_lower)*ec_rates[period][tier][0];
										else
											charge_amt += (energy_deficit - e_lower)*ec_rates[period][tier][0];

										if (energy_deficit < e_upper)
											break;
										tier++;
									}
									//					charge[m][period] = charge_amt;
									monthly_ec_charges[m] += charge_amt;
									monthly_energy -= energy_deficit;
								}
								//monthly_energy += energy_net[m][period];
							}
							if (enable_nm && (m > 0) && excess_monthly_dollars && !apply_excess_to_flat_rate)
							{
								monthly_ec_charges[m] -= monthly_cumulative_excess_dollars[m - 1];
								// excess dollar for the month
								//							cumulative_excess_dollars[m] = (ec_charge[m] < 0) ? -ec_charge[m] : 0;
							}
							monthly_ec_rates[m] = monthly_energy != 0 ? monthly_ec_charges[m] / monthly_energy : (ssc_number_t)0.0;


							// monthly rollover with year end sell at reduced rate
							if (enable_nm)
							{
								if (apply_excess_to_flat_rate)
									payment[c] += monthly_ec_charges[m];
								else
								{
									if (!excess_monthly_dollars && (monthly_cumulative_excess_energy[m] == 0)) // buy from grid
										payment[c] += monthly_ec_charges[m];
									else if (excess_monthly_dollars)
										// && (monthly_cumulative_excess_dollars[m] == 0)) // buy from grid
										payment[c] += monthly_ec_charges[m];
								}
							}
							else // non-net metering - no rollover 
							{
								if (monthly_energy_net[m] < 0) // must buy from grid
									payment[c] += monthly_ec_charges[m];
								else // surplus - sell to grid
									income[c] -= monthly_ec_charges[m]; // charge is negative for income!
							}

							// Price ?

							// end of energy charge

						}


						if (dc_enabled)
						{
							// fixed demand charge

							// compute charge based on tier structure for the month
							tier = 0;
							charge = 0;
							peak_demand = -monthly_peak[m]; // energy demands are negative.
							while (tier < 6)
							{
								// add up the charge amount for this block
								ssc_number_t e_upper = dc_fixed_energy_ub[m][tier];
								ssc_number_t e_lower = tier > 0 ? dc_fixed_energy_ub[m][tier - 1] : (ssc_number_t)0.0;

								if (peak_demand > e_upper)
									charge += (e_upper - e_lower)*dc_fixed_charges[m][tier];
								else
									charge += (peak_demand - e_lower)*dc_fixed_charges[m][tier];

								if (peak_demand < e_upper)
									break;
								tier++;
							}

							monthly_dc_fixed[m] = charge;
							payment[c] += monthly_dc_fixed[m];
							demand_charge[c] = charge;
							dc_hourly_peak[peak_hour[m]] = peak_demand;


							// end of fixed demand charge


							// TOU demand charge
							charge = 0;
							peak_demand = 0;
							int peak_hour = 0;
							for (period = 0; period<12; period++)
							{
								tier = 0;
								peak_demand = -monthly_period_peak[m][period];
								peak_hour = peak_period_hour[m][period];
								while (tier < 6)
								{
									// add up the charge amount for this block
									ssc_number_t e_upper = dc_energy_ub[period][tier];
									ssc_number_t e_lower = tier > 0 ? dc_energy_ub[period][tier - 1] : (ssc_number_t)0.0;

									if (peak_demand > e_upper)
										charge += (e_upper - e_lower)*dc_charges[period][tier];
									else
										charge += (peak_demand - e_lower)*dc_charges[period][tier];

									if (peak_demand < e_upper)
										break;

									tier++;
								}
								dc_hourly_peak[peak_hour] = peak_demand;
							}
							// add to payments
							monthly_dc_tou[m] = charge;
							payment[c] += monthly_dc_tou[m]; // apply to last hour of the month
							demand_charge[c] += charge; // add TOU charge to hourly demand charge

							// end of TOU demand charge
						}

					} // end of if end of month
					c++;
				}  // h loop
			} // d loop



		} // end of month m (m loop)

		// TODO calculate monthly bill and excess dollars and rollover if m>0


		/*
		// process basic flat rate - V3 handled in energy charge
		process_flat_rate( e_in, payment, income, price );

		// process demand charges
		if (as_boolean("ur_dc_enable"))
			process_demand_charge( p_in, payment, demand_charge, monthly_dc_fixed, monthly_dc_tou, dc_tou_sched, dc_hourly_peak );

		// process energy charges
		if (as_boolean("ur_ec_enable"))
			process_energy_charge( e_in, payment, income, price, monthly_ec_charges, monthly_ec_rates, ec_tou_sched );
			*/




		// Assumption that fixed and minimum charges independent of rollovers kWh or $
		// process monthly fixed charges
		process_monthly_charge(payment, monthly_fixed_charges);
		// process min charges
		process_monthly_min(payment, monthly_fixed_charges);
		process_annual_min(payment, monthly_fixed_charges);

		if (enable_nm)
		{
			// monthly rollover with year end sell at reduced rate
			if (!excess_monthly_dollars && (monthly_cumulative_excess_energy[11] > 0))
				income[8759] += monthly_cumulative_excess_energy[11] * as_number("ur_nm_yearend_sell_rate");
			else if (excess_monthly_dollars && (monthly_cumulative_excess_dollars[11] > 0))
				income[8759] += monthly_cumulative_excess_dollars[11];

		}

		// compute revenue ( = income - payment )
		for (i=0;i<8760;i++)
			revenue[i] = income[i] - payment[i];

	}

	void process_annual_min(ssc_number_t payment[8760], ssc_number_t charges[12])
	{
		int m, d, h, c = 0;
		ssc_number_t annual_charge = 0;

		ssc_number_t min_charge = as_number("ur_annual_min_charge");
		for (m = 0; m<12; m++)
			for (d = 0; d<util::nday[m]; d++)
				for (h = 0; h<24; h++)
					annual_charge += payment[c];
		// check against min charge
		if (annual_charge < min_charge)
		{
			// if less then apply charge to last month and last hour
			ssc_number_t add_annual_charge = min_charge - annual_charge;
			charges[11] += add_annual_charge;
			payment[8759] += add_annual_charge;
		}

	}

	void process_monthly_min(ssc_number_t payment[8760], ssc_number_t charges[12])
	{
		int m, d, h, c;
		ssc_number_t monthly_charge[12];

		ssc_number_t min_charge = as_number("ur_monthly_min_charge");
		c = 0;
		for (m = 0; m<12; m++)
		{
			monthly_charge[m] = 0;
			for (d = 0; d<util::nday[m]; d++)
			{
				for (h = 0; h<24; h++)
				{
					monthly_charge[m] += payment[c];
					c++;
				}
			}
		}
		// check each month against minimum charge
		c = 0;
		for (m = 0; m < 12; m++)
		{
			ssc_number_t add_monthly_charge = 0;
			// if less then add difference to end of month
			if (monthly_charge[m] < min_charge)
			{
				add_monthly_charge = min_charge - monthly_charge[m];
			}
			for (d = 0; d<util::nday[m]; d++)
			{
				for (h = 0; h<24; h++)
				{
					if (d == util::nday[m] - 1 && h == 23)
					{
						charges[m] += add_monthly_charge;
						payment[c] += add_monthly_charge;
					}
					c++;
				}
			}
		}
	}


	void process_monthly_charge(ssc_number_t payment[8760], ssc_number_t charges[12])
	{
		int m, d, h, c;

		ssc_number_t fixed = as_number("ur_monthly_fixed_charge");
		c = 0;
		for (m = 0; m<12; m++)
		{
			for (d = 0; d<util::nday[m]; d++)
			{
				for (h = 0; h<24; h++)
				{
					if (d == util::nday[m] - 1 && h == 23)
					{
						charges[m] = fixed;
						payment[c] += fixed;
					}
					c++;
				}
			}
		}
	}



	void process_flat_rate( ssc_number_t e[8760],
			ssc_number_t payment[8760],
			ssc_number_t income[8760],
			ssc_number_t price[8760],
			ssc_number_t cumulative_excess_energy[12],
			ssc_number_t cumulative_excess_dollars[12])
	{
		ssc_number_t buy = as_number("ur_flat_buy_rate");
		ssc_number_t sell = as_number("ur_flat_sell_rate");

		bool sell_eq_buy = as_boolean("ur_sell_eq_buy");
		bool enable_nm = as_boolean("ur_enable_net_metering");
		bool excess_monthly_dollars = (as_integer("ur_excess_monthly_energy_or_dollars") == 1);

		if (sell_eq_buy)
			sell = buy;

		// calculate the monthly net energy and monthly hours
		int m,d,h;
		ssc_number_t energy_use[12]; // 12 months
//		ssc_number_t cumulative_excess_energy[12]; // 12 months for year end reconciliation
//		ssc_number_t cumulative_excess_dollars[12]; // 12 months for year end reconciliation
		// calculate the monthly net energy per period
		int hours[12];
		int c=0;
		for (m=0;m<12;m++)
		{
			energy_use[m] = 0;
//			cumulative_excess_energy[m] = 0;
			hours[m] = 0;
			for (d=0;d<util::nday[m];d++)
			{
				for(h=0;h<24;h++)
				{
					// net energy use per month
					energy_use[m] += e[c];
					// hours per period per month
					hours[m]++;
					c++;
				}
			}
		}


		// monthly cumulative excess energy (positive = excess energy, negative = excess load)
		ssc_number_t prev_value = 0;
		for (m=0;m<12;m++)
		{
			prev_value = ( m > 0 ) ? cumulative_excess_energy[m-1] : 0;
			cumulative_excess_energy[m]=( (prev_value+energy_use[m]) > 0) ? (prev_value+energy_use[m]) : 0;
		}


		// back out hourly values based on monthly reconciliation
		c=0;
		for (m=0;m<12;m++)
		{
			if (hours[m] <= 0) break;
			for (d=0;d<util::nday[m];d++)
			{
				for(h=0;h<24;h++)
				{
					if (d==util::nday[m]-1 && h==23)
					{
						if (enable_nm)
						{
							// monthly rollover with year end sell at reduced rate
							if (!excess_monthly_dollars &&(cumulative_excess_energy[m] == 0)) // buy from grid
							{
								if (m > 0)
									payment[c] += -(energy_use[m] + cumulative_excess_energy[m - 1]) * buy;
								else
									payment[c] += -energy_use[m] * buy;
							}
							else if (excess_monthly_dollars) // may need to buy from grid
							{
								if (m > 0)
								{
									if (energy_use[m] < 0) // may need to buy from grid
									{
										ssc_number_t energy_cost = -energy_use[m] * buy;
										ssc_number_t excess_dollars = cumulative_excess_dollars[m - 1] - energy_cost;
										if (excess_dollars < 0) // buy from grid
											payment[c] += -excess_dollars;
										else // update excess dollars
											cumulative_excess_dollars[m] = excess_dollars;
									}
									else // update excess dollars
										cumulative_excess_dollars[m] = energy_use[m] * sell + cumulative_excess_dollars[m-1];
								}
								else
								{
									if (energy_use[m] < 0) // must buy from grid
										payment[c] += -energy_use[m] * buy;
									else // update excess dollars
										cumulative_excess_dollars[m] = energy_use[m] * sell;
								}
							}
						}
						else // no net metering - so no rollover.
						{
							if (energy_use[m] < 0) // must buy from grid
								payment[c] += -energy_use[m] * buy;
							else
								income[c] += energy_use[m] * sell;
						}
					}
					c++;
				}
			}
		}


		// calculate hourly energy charge regardless of scenario - email from Paul 7/29/13
		for (int i=0;i<8760;i++)
		{
			if (e[i] < 0) // must buy from grid
				price[i] += buy;
			else
				price[i] += sell;
		}


	}
	

	void process_energy_charge( ssc_number_t e[8760],
			ssc_number_t payment[8760],
			ssc_number_t income[8760],
			ssc_number_t price[8760],
			ssc_number_t ec_charge[12],
			ssc_number_t ec_rate[12],
			ssc_number_t ec_tou_sched[8760],
			ssc_number_t cumulative_excess_energy[12],
			ssc_number_t cumulative_excess_dollars[12])
	{
		// 12 periods with 6 tiers each rates 3rd index = 0 = buy and 1=sell
		ssc_number_t rates[12][6][2]; 
		ssc_number_t energy_ub[12][6];



		size_t nrows, ncols;
		ssc_number_t *dc_weekday = as_matrix("ur_ec_sched_weekday", &nrows, &ncols);
		if (nrows != 12 || ncols != 24)
		{
			std::ostringstream ss;
			ss << "energy charge weekday schedule must be 12x24, input is " << nrows << "x" << ncols;
			throw exec_error("utilityrate3", ss.str());
		}
		ssc_number_t *dc_weekend = as_matrix("ur_ec_sched_weekend", &nrows, &ncols);
		if (nrows != 12 || ncols != 24)
		{
			std::ostringstream ss;
			ss << "energy charge weekend schedule must be 12x24, input is " << nrows << "x" << ncols;
			throw exec_error("utilityrate3", ss.str());
		}
		util::matrix_t<float> schedwkday(12,24);
		schedwkday.assign(dc_weekday, nrows, ncols);
		util::matrix_t<float> schedwkend(12, 24);
		schedwkend.assign(dc_weekend, nrows, ncols);

		int tod[8760];

		if (!util::translate_schedule( tod, schedwkday, schedwkend, 1, 12))
			throw general_error("could not translate weekday and weekend schedules for energy charges");

		for (int i=0;i<8760; i++) ec_tou_sched[i] = (ssc_number_t)(tod[i]);

		bool sell_eq_buy = as_boolean("ur_sell_eq_buy");
		bool enable_nm = as_boolean("ur_enable_net_metering");
		bool excess_monthly_dollars = (as_integer("ur_excess_monthly_energy_or_dollars") == 1);

		// tiered rates for all 6 tiers in each of the 12 periods

		for (int period=0;period<12;period++)
		{
			std::string str_period = util::to_string( period+1 );

			for (int tier=0; tier<6; tier++)
			{
				std::string str_tier = util::to_string( tier+1 );

				rates[period][tier][0] = as_number("ur_ec_p" + str_period + "_t" + str_tier + "_br");
				rates[period][tier][1] = sell_eq_buy ? rates[period][tier][0] : as_number("ur_ec_p" + str_period + "_t" + str_tier + "_sr");
				energy_ub[period][tier] = as_number("ur_ec_p" + str_period + "_t" + str_tier + "_ub");
			}
		}




		ssc_number_t energy_use[12]; // 12 months
//		ssc_number_t cumulative_excess_energy[12]; // 12 months for year end reconciliation
		// handle monthly excess dollars
//		ssc_number_t cumulative_excess_dollars[12]; // 12 months for year end reconciliation
		// calculate the monthly net energy per period
		int m,d,h,period,tier;
		ssc_number_t energy_net[12][12]; // 12 months, 12 periods
		int hours[12][12];
		int hours_per_month[12];
		int c=0;
		for (m=0;m<12;m++)
		{
			energy_use[m]=0.0;
			hours_per_month[m] = 0;
//			cumulative_excess_energy[m] = 0.0;
			for (period=0;period<12;period++) 
			{
				energy_net[m][period] = 0;
				hours[m][period] = 0;
			}

			for (d=0;d<util::nday[m];d++)
			{
				for(h=0;h<24;h++)
				{
					int todp = tod[c]-1;
					// net energy use per period per month
					energy_net[m][todp] += e[c];
					// hours per period per month
					hours[m][todp]++;
					hours_per_month[m]++;
					// net energy per month
					energy_use[m] += e[c];
					c++;
				}
			}
		}
		/*
		// monthly cumulative excess energy (positive = excess energy, negative = excess load)
		ssc_number_t prev_value = 0;
		cumulative_excess_energy[0] = 0.0;
		for (m=1;m<12;m++)
		{
			prev_value = cumulative_excess_energy[m-1];
			cumulative_excess_energy[m]=( (prev_value+energy_use[m]) > 0) ? (prev_value+energy_use[m]) : 0;
		}
		*/


		// go through each period and tier and calculate per SAM_V2_RATES.docx - Nathan Clark 3/6/13
		// reconcile on monthly basis
//		ssc_number_t charge[12][12];
//		ssc_number_t credit[12][12];
		c=0;
		for (m=0;m<12;m++)
		{
			cumulative_excess_dollars[m] = 0;
			ssc_number_t monthly_energy = 0;
			for (period=0;period<12;period++)
			{
//				charge[m][period]=0;
//				credit[m][period]=0;

				if (energy_net[m][period] >= 0.0)
				{ // calculate income or credit
					ssc_number_t credit_amt = 0;
					ssc_number_t energy_surplus = energy_net[m][period];
					tier=0;
					while (tier<6)
					{
						// add up the charge amount for this block
						ssc_number_t e_upper = energy_ub[period][tier];
						ssc_number_t e_lower = tier > 0 ? energy_ub[period][tier-1] : (ssc_number_t)0.0;

						if (energy_surplus > e_upper)
							credit_amt += (e_upper-e_lower)*rates[period][tier][1];
						else
							credit_amt += (energy_surplus-e_lower)*rates[period][tier][1];
	
						if ( energy_surplus < e_upper )
							break;
						tier++;
			 		}
//					credit[m][period] = credit_amt;
					ec_charge[m] -= credit_amt;
					monthly_energy += energy_surplus;
				}
				else
				{ // calculate payment or charge
					ssc_number_t charge_amt = 0;
					ssc_number_t energy_deficit = -energy_net[m][period];

					// update 9/1/14 based on feedback from Peter Jeavons 8/28/14
					if (enable_nm && (m>0) && !excess_monthly_dollars) // reduce energy used to calculate charge
						energy_deficit -= fabs(cumulative_excess_energy[m - 1] * energy_net[m][period]/energy_use[m]);

					tier=0;
					while (tier<6)
					{
						// add up the charge amount for this block
						ssc_number_t e_upper = energy_ub[period][tier];
						ssc_number_t e_lower = tier > 0 ? energy_ub[period][tier-1] : (ssc_number_t)0.0;

						if (energy_deficit > e_upper)
							charge_amt += (e_upper-e_lower)*rates[period][tier][0];
						else
							charge_amt += (energy_deficit-e_lower)*rates[period][tier][0];
	
						if ( energy_deficit < e_upper )
							break;
						tier++;
					}
//					charge[m][period] = charge_amt;
					ec_charge[m] += charge_amt;
					monthly_energy -= energy_deficit;
				}
				//monthly_energy += energy_net[m][period];
			}
			if (enable_nm && (m > 0) && excess_monthly_dollars)
			{
				ec_charge[m] -= cumulative_excess_dollars[m - 1];
				// excess dollar for the month
				cumulative_excess_dollars[m] = (ec_charge[m] < 0) ? -ec_charge[m] : 0;
			}
			ec_rate[m] = monthly_energy != 0 ? ec_charge[m] / monthly_energy : (ssc_number_t)0.0;
		}



		c=0;
		for (m=0;m<12;m++)
		{
			for (d=0;d<util::nday[m];d++)
			{
				for(h=0;h<24;h++)
				{
					if (d==util::nday[m]-1 && h==23)
					{
					// monthly rollover with year end sell at reduced rate
						if (enable_nm)
						{
							if (!excess_monthly_dollars && (cumulative_excess_energy[m] == 0)) // buy from grid
								payment[c] += ec_charge[m];
							else if (excess_monthly_dollars && (cumulative_excess_dollars[m]==0)) // buy from grid
								payment[c] += ec_charge[m];
						}
						else // non-net metering - no rollover 
						{
							if (energy_use[m] < 0) // must buy from grid
								payment[c] += ec_charge[m];
							else // surplus - sell to grid
								income[c] -= ec_charge[m]; // charge is negative for income!
						}
					}
					c++;
				}
			}
		}

		//TODO - test
		if (enable_nm)
		{
			// monthly rollover with year end sell at reduced rate
/*			if (!excess_monthly_dollars && (cumulative_excess_energy[11] > 0))
				income[8759] += cumulative_excess_energy[11] * as_number("ur_nm_yearend_sell_rate");
			else */ if (excess_monthly_dollars && (cumulative_excess_dollars[11] > 0))
				income[8759] += cumulative_excess_dollars[11];

		}

		// calculate energy charge for both scenarios hour by hour basis
		for (int i=0;i<8760;i++)
		{
			int period = tod[i]-1;
			if (e[i] >= 0.0)
			{ // calculate income or credit
				ssc_number_t credit_amt = 0;
				ssc_number_t energy_surplus = e[i];
				tier=0;
				while (tier<6)
				{
					// add up the charge amount for this block
					ssc_number_t e_upper = energy_ub[period][tier];
					ssc_number_t e_lower = tier > 0 ? energy_ub[period][tier-1] : (ssc_number_t)0.0;

					if (energy_surplus > e_upper)
						credit_amt += (e_upper-e_lower)*rates[period][tier][1];
					else
						credit_amt += (energy_surplus-e_lower)*rates[period][tier][1];
	
					if ( energy_surplus < e_upper )
						break;
					tier++;
				}
				price[i] += credit_amt;
			}
			else
			{ // calculate payment or charge
				ssc_number_t charge_amt = 0;
				ssc_number_t energy_deficit = -e[i];
				tier=0;
				while (tier<6)
				{
					// add up the charge amount for this block
					ssc_number_t e_upper = energy_ub[period][tier];
					ssc_number_t e_lower = tier > 0 ? energy_ub[period][tier-1] : (ssc_number_t)0.0;

					if (energy_deficit > e_upper)
						charge_amt += (e_upper-e_lower)*rates[period][tier][0];
					else
						charge_amt += (energy_deficit-e_lower)*rates[period][tier][0];
	
					if ( energy_deficit < e_upper )
						break;
					tier++;
				}
				price[i] += charge_amt;
			}

		}
	}




	void process_demand_charge( ssc_number_t p[8760],
			ssc_number_t payment[8760],
			ssc_number_t demand_charge[8760],
			ssc_number_t dc_fixed[12],
			ssc_number_t dc_tou[12],
			ssc_number_t dc_tou_sched[8760], 
			ssc_number_t dc_hourly_peak[8760])
	{
		int i,m,d,h,c,tier;

		// 12 months for fixed demand charges, 12 periods for TOU demand charges. Each with 6 tiers.
		ssc_number_t charges[12][6]; 
		ssc_number_t energy_ub[12][6];



		for (m=0;m<12;m++)
		{
			for (tier=0; tier<6; tier++)
			{
				std::string str_tier = util::to_string( tier+1 );
				charges[m][tier] = as_number("ur_dc_" + util::schedule_int_to_month(m) + "_t" + str_tier + "_dc");
				energy_ub[m][tier] = as_number("ur_dc_" + util::schedule_int_to_month(m) + "_t" + str_tier + "_ub");
			}
		}


		
		// compute fixed monthly demand charges - add to last hour of the month (payments)
		c=0; // hourly count
		for (m=0;m<12;m++)
		{
			ssc_number_t charge = 0.0;
			ssc_number_t mpeak = 0.0;  // peak usage for the month (negative value)
			ssc_number_t peak_demand = 0;
			int peak_hour = 0;
			for (d = 0; d<util::nday[m]; d++)
			{
				for (h=0;h<24;h++)
				{
					if (p[c] < 0 && p[c] < mpeak)
					{
						mpeak = p[c];
						peak_hour = c;
					}

					if (d==util::nday[m]-1 && h==23)
					{
						// compute charge based on tier structure for the month
						tier=0;
						peak_demand = -mpeak; // energy demands are negative.
						while (tier<6)
						{
							// add up the charge amount for this block
							ssc_number_t e_upper = energy_ub[m][tier];
							ssc_number_t e_lower = tier > 0 ? energy_ub[m][tier-1] : (ssc_number_t)0.0;

							if (peak_demand > e_upper)
								charge += (e_upper-e_lower)*charges[m][tier];
							else
								charge += (peak_demand-e_lower)*charges[m][tier];
	
							if ( peak_demand < e_upper )
								break;
							tier++;
						}

						dc_fixed[m] = charge;
						payment[c] += dc_fixed[m];
						demand_charge[c] = charge;
						dc_hourly_peak[peak_hour] = peak_demand;
					}

					c++;
				}
			}
		}


		// compute time-of-use based demand charge
		// for each month:
		// 1. find peak demand in each period (1-12)
		// 2. multiply each period's peak demand by period price and add to payment for that month

		// extract schedules
		size_t nrows, ncols;
		ssc_number_t *dc_weekday;
		ssc_number_t *dc_weekend;
		// initialize to diurnal all 1 if only flat monthly demand charge specified per Mike Gleason 1/16/15
		util::matrix_t<float> schedwkday(12, 24,1);
		util::matrix_t<float> schedwkend(12, 24,1);
		if (is_assigned("ur_dc_sched_weekday"))
		{
			dc_weekday = as_matrix("ur_dc_sched_weekday", &nrows, &ncols);
			if (nrows != 12 || ncols != 24)
			{
				std::ostringstream ss;
				ss << "demand charge weekday schedule must be 12x24, input is " << nrows << "x" << ncols;
				throw exec_error("utilityrate3", ss.str());
			}
			schedwkday.assign(dc_weekday, nrows, ncols);
		}
		if (is_assigned("ur_dc_sched_weekend"))
		{
			dc_weekend = as_matrix("ur_dc_sched_weekend", &nrows, &ncols);
			if (nrows != 12 || ncols != 24)
			{
				std::ostringstream ss;
				ss << "demand charge weekend schedule must be 12x24, input is " << nrows << "x" << ncols;
				throw exec_error("utilityrate3", ss.str());
			}
			schedwkend.assign(dc_weekend, nrows, ncols);
		}



		int tod[8760];
		if (!util::translate_schedule( tod, schedwkday, schedwkend, 1, 12))
			throw general_error("could not translate weekday and weekend schedules for demand charge time-of-use rate");

		for (int i=0;i<8760; i++) dc_tou_sched[i] = (ssc_number_t)(tod[i]);


		// extract rate info

		int period;
		for (period=0;period<12;period++)
		{
			std::string str_period = util::to_string( period+1 );
			for (tier=0; tier<6; tier++)
			{
				std::string str_tier = util::to_string( tier+1 );
				charges[period][tier] = as_number("ur_dc_p" + str_period + "_t" + str_tier + "_dc");
				energy_ub[period][tier] = as_number("ur_dc_p" + str_period + "_t" + str_tier + "_ub");
			}
		}



		// find peak demand per month for each of the twelve periods
		ssc_number_t ppeaks[12]; // period peak demand 
		int phpeaks[12]; // period peak demand hour
		c = 0;
		for (m=0;m<12;m++)
		{
			for (i = 0; i < 12; i++)
			{
				ppeaks[i] = 0; // reset each month
				phpeaks[i] = 0; // reset each month
			}

			for (d=0;d<util::nday[m];d++)
			{
				for(h=0;h<24;h++)
				{
					int todp = tod[c]-1;
					if (p[c] < 0 && p[c] < ppeaks[todp])
					{
						ppeaks[todp] = p[c];
						phpeaks[todp] = c;
					}

					if (d==util::nday[m]-1 && h==23)
					{
						// sum up all peak demand charges at end of month
						ssc_number_t charge=0;
						ssc_number_t peak_demand=0;
						int peak_hour = 0;
						for (period=0;period<12;period++)
						{
							tier=0;
							peak_demand=-ppeaks[period];
							peak_hour = phpeaks[period];
							while (tier<6)
							{
								// add up the charge amount for this block
								ssc_number_t e_upper = energy_ub[period][tier];
								ssc_number_t e_lower = tier > 0 ? energy_ub[period][tier-1] : (ssc_number_t)0.0;

								if (peak_demand > e_upper)
									charge += (e_upper-e_lower)*charges[period][tier];
								else
									charge += (peak_demand-e_lower)*charges[period][tier];
	
								if ( peak_demand < e_upper )
									break;

								tier++;
							}
							dc_hourly_peak[peak_hour] = peak_demand;
						}
						// add to payments
						dc_tou[m] = charge;
						payment[c] += dc_tou[m]; // apply to last hour of the month
						demand_charge[c] += charge; // add TOU charge to hourly demand charge
					}

					c++;
				}
			}
		}

	}



};

DEFINE_MODULE_ENTRY( utilityrate3_monthly, "Complex utility rate structure net revenue calculator OpenEI Version 3", 1 );


