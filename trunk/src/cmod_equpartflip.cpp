#include "lib_financial.h"
#include "core.h"
#include <sstream>

/*
Very close to cmod_levpartflip

5 inputs not used 
1. term_tenor
2. term_int_rate
3. dscr
4. dscr_reserve_months
5. cost_debt_closing

2 inputs added
1. sponsor_cap_recovery_method
2. sponsor_cap_recovery_time
*/


static var_info _cm_vtab_equpartflip[] = {


/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_ARRAY,      "energy_net",				"Annual energy produced by system",	"kWh",   "",                      "DHF",             "*",						   "",                              "" },
/* constraint is > 0 */
	{ SSC_INPUT,        SSC_NUMBER,     "system_capacity",			"System nameplate capacity",		"kW",    "",                      "DHF",             "*",						   "MIN=1e-3",                         "" },

/* costs - to be updated based on meetings with DHF */
	{ SSC_INPUT,        SSC_NUMBER,     "cost_gen_equip",           "Generation equiptment cost",		"$",	 "",					  "DHF",             "?=24000000",              "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "cost_bop",					"Balance of plant cost",			"$",	 "",					  "DHF",             "?=8000000",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "cost_network",             "Network upgrade cost",				"$",	 "",					  "DHF",             "?=3500000",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "percent_contingency",      "Contingency percent",				"%",	 "",					  "DHF",             "?=1",                       "MIN=0,MAX=100",		        	"" },

	{ SSC_INPUT,        SSC_NUMBER,     "cost_developer",           "Developer cost & fees",		    "$",	 "",					  "DHF",             "?=2000000",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "cost_land_improve",        "Land improvements cost",		    "$",	 "",					  "DHF",             "?=200000",                 "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "cost_other",               "Other cost",						"$",	 "",					  "DHF",             "?=75000",                  "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "percent_taxable",          "Taxable cost",				        "%",	 "",					  "DHF",             "?=100",                     "MIN=0,MAX=100",      			"" },

/* inputs in DHF model not currently in SAM 11/15/10 */
	{ SSC_INPUT,        SSC_NUMBER,     "reserves_interest",        "Interest on reserves",				"%",	 "",					  "DHF",             "?=1.75",                     "MIN=0,MAX=100",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "prop_tax_cost_assessed_percent",   "Percent of pre-financing costs assessed","%","",			  "DHF",			 "?=95",                     "MIN=0,MAX=100",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "prop_tax_assessed_decline","Assessed value annual decline",	"%",	 "",					  "DHF",             "?=5",                     "MIN=0,MAX=100",      			"" },

/* DHF replacement reserve on top of regular o and m */
	{ SSC_INPUT,        SSC_NUMBER,     "equip1_reserve_cost",      "Major equipment reserve1 cost",	"$/Wdc",	 "",				  "DHF",             "?=0.25",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "equip1_reserve_freq",      "Major equipment reserve1 frequency",	"years",	 "",			  "DHF",             "?=12",               "INTEGER,MIN=0",                         "" },

	{ SSC_INPUT,        SSC_NUMBER,     "equip2_reserve_cost",      "Major equipment reserve2 cost",	"$/Wdc",	 "",				  "DHF",             "?=0",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "equip2_reserve_freq",      "Major equipment reserve2 frequency",	"years",	 "",			  "DHF",             "?=15",               "INTEGER,MIN=0",                         "" },

	{ SSC_INPUT,        SSC_NUMBER,     "equip3_reserve_cost",      "Major equipment reserve3 cost",	"$/Wdc",	 "",				  "DHF",             "?=0",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "equip3_reserve_freq",      "Major equipment reserve3 frequency",	"years",	 "",			  "DHF",             "?=20",               "INTEGER,MIN=0",                         "" },

/* major equipment depreciation schedules - can extend to three different schedules */
	{ SSC_INPUT,        SSC_NUMBER,     "equip_reserve_depr_sta",   "Major equipment reserve state depreciation",	"",	 "0=5yr MACRS,1=15yr MACRS,2=5yr SL,3=15yr SL, 4=20yr SL,5=39yr SL",  "DHF", "?=0",   "INTEGER,MIN=0,MAX=5",  "" },
	{ SSC_INPUT,        SSC_NUMBER,     "equip_reserve_depr_fed",   "Major equipment reserve federal depreciation",	"",	 "0=5yr MACRS,1=15yr MACRS,2=5yr SL,3=15yr SL, 4=20yr SL,5=39yr SL",  "DHF", "?=0",   "INTEGER,MIN=0,MAX=5",  "" },

/* DHF salvage value */
	{ SSC_INPUT,        SSC_NUMBER,     "salvage_percentage",          "Net pre-tax cash salvage value",	"%",	 "",					  "DHF",             "?=10",                     "MIN=0,MAX=100",      			"" },
/* DHF market specific inputs - leveraged partnership flip */
	{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_mode",            "PPA solution mode",                "0/1",   "0=specify ppa,1=solve ppa", "DHF",         "?=0",                     "INTEGER,MIN=0,MAX=1",            "" },
	{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_tolerance",            "PPA solution tolerance",                "",   "", "DHF",         "?=1e-3",                     "",            "" },
	{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_min",            "PPA solution minimum ppa",                "cents/kWh",   "", "DHF",         "?=0",                     "",            "" },
	{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_max",            "PPA solution maximum ppa",                "cents/kWh",   "", "DHF",         "?=100",                     "",            "" },
	{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_max_iterations",            "PPA solution maximum number of iterations",                "",   "", "DHF",         "?=100",                     "INTEGER,MIN=1",            "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ppa_price_input",			"Initial year PPA price",			"cents/kWh",	 "",			  "DHF",			 "?=10",         "",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "ppa_escalation",           "PPA escalation",					"%",	 "",					  "DHF",             "?=0",                     "MIN=0,MAX=100",      			"" },
/* DHF construction period */
	{ SSC_INPUT,        SSC_NUMBER,     "constr_months",            "Construction period",				"months", "",				      "DHF",             "?=10",					"INTEGER,MIN=0",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "constr_int_rate",          "Construction interest rate",		"%",	 "",					  "DHF",             "?=4",                     "MIN=0,MAX=100",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "constr_upfront_percent",  "Construction up-front fee",    	"%",	 "",					  "DHF",             "?=1",                     "MIN=0,MAX=100",      			"" },

/* DHF term financing 
	{ SSC_INPUT,        SSC_NUMBER,     "term_tenor",               "Term financing tenor",				"years", "",				      "DHF",             "?=10",					"INTEGER,MIN=0",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "term_int_rate",            "Term financing interest rate",		"%",	 "",					  "DHF",             "?=8.5",                   "MIN=0,MAX=100",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "dscr",						"Debt service coverage ratio",		"",	     "",				      "DHF",             "?=1.5",					"MIN=0",      			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "dscr_reserve_months",		"Debt service reserve account",		"months P&I","",			      "DHF",             "?=6",					    "INTEGER,MIN=0",      			        "" },
*/
/* DHF Capital Cost */
	{ SSC_INPUT,        SSC_NUMBER,     "cost_dev_fee_percent",		"Development fee (% pre-financing cost)","%",	 "",					  "DHF",             "?=3",					    "MIN=0,MAX=100",      			        "" },
//	{ SSC_INPUT,        SSC_NUMBER,     "cost_debt_closing",		"Debt closing cost",				"$",	 "",					  "DHF",             "?=250000",					    "MIN=0",      			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "cost_equity_closing",		"Equity closing cost",				"$",	 "",					  "DHF",             "?=100000",					    "MIN=0",      			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "cost_working_reserve",		"Q&M/Working capital reserve",		"$",	 "",					  "DHF",             "?=150000",					    "MIN=0",      			        "" },
/* DHF Equity Structure */
	{ SSC_INPUT,        SSC_NUMBER,     "tax_investor_equity_percent",		"Tax investor equity",				"%",	 "",					  "DHF",             "?=98",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "tax_investor_preflip_cash_percent",		"Tax investor pre-flip cash ",		"%",	 "",  "DHF",             "?=98",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "tax_investor_postflip_cash_percent",	"Tax investor post-flip cash ",	"%",	 "",  "DHF",             "?=15",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "tax_investor_preflip_tax_percent",		"Tax investor pre-flip tax benefit ",		"%",	 "",  "DHF",             "?=98",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "tax_investor_postflip_tax_percent",	"Tax investor post-flip tax benefit ",	"%",	 "",  "DHF",             "?=15",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "flip_target_percent",			"After-tax flip/return target",		"%",	 "",					  "DHF",             "?=11",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "flip_target_year",		"Return target year",				"",		 "",					  "DHF",             "?=11",					  "MIN=1",     			        "" },
/* DHF depreciation allocation */
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_macrs_5_percent",		"5-yr MACRS depreciation federal and state allocation",	"%", "",	  "DHF",             "?=89",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_macrs_15_percent",		"15-yr MACRS depreciation federal and state allocation",	"%", "",  "DHF",             "?=1.5",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_5_percent",		"5-yr straight line depreciation federal and state allocation",	"%", "",  "DHF",             "?=0",						  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_15_percent",		"15-yr straight line depreciation federal and state allocation","%", "",  "DHF",             "?=3",						  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_20_percent",		"20-yr straight line depreciation federal and state allocation","%", "",  "DHF",             "?=3",						  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_39_percent",		"39-yr straight line depreciation federal and state allocation","%", "",  "DHF",             "?=0.5",					  "MIN=0,MAX=100",     			        "" },
/* DHF bonus depreciation */
	{ SSC_INPUT,        SSC_NUMBER,     "depr_bonus_sta",			"State bonus depreciation",			"%",	 "",					  "DHF",             "?=0",						  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_macrs_5",   "State bonus depreciation 5-yr MACRS","0/1", "",                      "DHF",			 "?=1",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_macrs_15",   "State bonus depreciation 15-yr MACRS","0/1","",                     "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_sl_5",   "State bonus depreciation 5-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_sl_15",   "State bonus depreciation 15-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_sl_20",   "State bonus depreciation 20-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_sl_39",   "State bonus depreciation 39-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },

	{ SSC_INPUT,        SSC_NUMBER,     "depr_bonus_fed",			"Federal bonus depreciation",			"%",	 "",					  "DHF",             "?=0",						  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_macrs_5",   "Federal bonus depreciation 5-yr MACRS","0/1", "",                      "DHF",			 "?=1",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_macrs_15",   "Federal bonus depreciation 15-yr MACRS","0/1","",                     "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_sl_5",   "Federal bonus depreciation 5-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_sl_15",   "Federal bonus depreciation 15-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_sl_20",   "Federal bonus depreciation 20-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_sl_39",   "Federal bonus depreciation 39-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
/* DHF ITC depreciation */
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_macrs_5",   "State itc depreciation 5-yr MACRS","0/1", "",                      "DHF",			 "?=1",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_macrs_15",   "State itc depreciation 15-yr MACRS","0/1","",                     "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_sl_5",   "State itc depreciation 5-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_sl_15",   "State itc depreciation 15-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_sl_20",   "State itc depreciation 20-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_sl_39",   "State itc depreciation 39-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },

	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_macrs_5",   "Federal itc depreciation 5-yr MACRS","0/1", "",                      "DHF",			 "?=1",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_macrs_15",   "Federal itc depreciation 15-yr MACRS","0/1","",                     "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_sl_5",   "Federal itc depreciation 5-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_sl_15",   "Federal itc depreciation 15-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_sl_20",   "Federal itc depreciation 20-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_sl_39",   "Federal itc depreciation 39-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },

/* PBI for debt service TODO - other yearly incentives */
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_fed_for_ds",    "Federal PBI available for debt service",     "0/1",      "",                      "DHF",      "?=0",                       "INTEGER,MIN=0,MAX=1",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_sta_for_ds",    "State PBI available for debt service",     "0/1",      "",                      "DHF",      "?=0",                       "INTEGER,MIN=0,MAX=1",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_uti_for_ds",    "Utility PBI available for debt service",     "0/1",      "",                      "DHF",      "?=0",                       "INTEGER,MIN=0,MAX=1",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_oth_for_ds",    "Other PBI available for debt service",     "0/1",      "",                      "DHF",      "?=0",                       "INTEGER,MIN=0,MAX=1",                                         "" },

/* intermediate outputs */
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_contingency",        "Contingency cost",                 "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_hard",               "Hard cost",                        "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_soft",               "Soft cost",                        "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_salestax",           "Sales tax",                        "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_prefinancing",          "Installed cost",                   "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_prefinancingperwatt",   "Installed cost per watt",          "$/W",   "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_installed",          "Installed cost",                   "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_installedperwatt",   "Installed cost per watt",          "$/W",   "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "nominal_discount_rate",   "Nominal discount rate",            "%",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "prop_tax_assessed_value", "Assessed value of property for tax purposes","$", "",				  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "salvage_value",			"Net pre-tax cash salvage value",	"$",	 "",					  "DHF",			 "*",                         "",                             "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_none_percent",		"Non-depreciable federal and state allocation",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_none",		"Non-depreciable federal and state allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_macrs_5",		"5-yr MACRS depreciation federal and state allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_macrs_15",		"15-yr MACRS depreciation federal and state allocation",	"$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_sl_5",		"5-yr straight line depreciation federal and state allocation",	"$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_sl_15",		"15-yr straight line depreciation federal and state allocation","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_sl_20",		"20-yr straight line depreciation federal and state allocation","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_sl_39",		"39-yr straight line depreciation federal and state allocation","$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_total",		"Total depreciation federal and state allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },

// disallowment factors - hardcoded to 0.5 in DHF model
	{ SSC_INPUT,        SSC_NUMBER,     "itc_sta_disallow_factor",		"State ITC basis disallowment factor",	"$", "",	  "DHF",             "?=0.5",					  "",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "itc_fed_disallow_factor",		"Federal ITC basis disallowment factor",	"$", "",	  "DHF",             "?=0.5",					  "",     			        "" },

// state itc table
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_macrs_5",		"5-yr MACRS depreciation state ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_macrs_15",		"15-yr MACRS depreciation state ITC adj qualifying costs",	"$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_sl_5",		"5-yr straight line depreciation state ITC adj qualifying costs",	"$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_sl_15",		"15-yr straight line depreciation state ITC adj qualifying costs","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_sl_20",		"20-yr straight line depreciation state ITC adj qualifying costs","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_sl_39",		"39-yr straight line depreciation state ITC adj qualifying costs","$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_total",		"Total state ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_percent_macrs_5",		"5-yr MACRS depreciation ITC basis disallowance from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_percent_macrs_15",		"15-yr MACRS depreciation ITC basis disallowance from state percentage",	"$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_percent_sl_5",		"5-yr straight line depreciation ITC basis disallowance from state percentage",	"$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_percent_sl_15",		"15-yr straight line depreciation ITC basis disallowance from state percentage","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_percent_sl_20",		"20-yr straight line depreciation ITC basis disallowance from state percentage","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_percent_sl_39",		"39-yr straight line depreciation ITC basis disallowance from state percentage","$", "",  "DHF",             "*",					  "",     			        "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_percent_total",		"State ITC percent total",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_fixed_total",		"State ITC fixed total",	"$", "",	  "DHF",             "*",					  "",     			        "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_reduction",		"State ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_fixed_macrs_5",		"5-yr MACRS depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_fixed_macrs_15",		"15-yr MACRS depreciation ITC basis disallowance from state fixed amount",	"$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_fixed_sl_5",		"5-yr straight line depreciation ITC basis disallowance from state fixed amount",	"$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_fixed_sl_15",		"15-yr straight line depreciation ITC basis disallowance from state fixed amount","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_fixed_sl_20",		"20-yr straight line depreciation ITC basis disallowance from state fixed amount","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_fixed_sl_39",		"39-yr straight line depreciation ITC basis disallowance from state fixed amount","$", "",  "DHF",             "*",					  "",     			        "" },


// federal itc table
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_macrs_5",		"5-yr MACRS depreciation federal ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_macrs_15",		"15-yr MACRS depreciation federal ITC adj qualifying costs",	"$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_sl_5",		"5-yr straight line depreciation federal ITC adj qualifying costs",	"$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_sl_15",		"15-yr straight line depreciation federal ITC adj qualifying costs","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_sl_20",		"20-yr straight line depreciation federal ITC adj qualifying costs","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_sl_39",		"39-yr straight line depreciation federal ITC adj qualifying costs","$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_total",		"Total federal ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_percent_macrs_5",		"5-yr MACRS depreciation ITC basis disallowance from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_percent_macrs_15",		"15-yr MACRS depreciation ITC basis disallowance from federal percentage",	"$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_percent_sl_5",		"5-yr straight line depreciation ITC basis disallowance from federal percentage",	"$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_percent_sl_15",		"15-yr straight line depreciation ITC basis disallowance from federal percentage","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_percent_sl_20",		"20-yr straight line depreciation ITC basis disallowance from federal percentage","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_percent_sl_39",		"39-yr straight line depreciation ITC basis disallowance from federal percentage","$", "",  "DHF",             "*",					  "",     			        "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_percent_total",		"federal ITC percent total",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_fixed_total",		"federal ITC fixed total",	"$", "",	  "DHF",             "*",					  "",     			        "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_reduction",		"federal ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_fixed_macrs_5",		"5-yr MACRS depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_fixed_macrs_15",		"15-yr MACRS depreciation ITC basis disallowance from federal fixed amount",	"$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_fixed_sl_5",		"5-yr straight line depreciation ITC basis disallowance from federal fixed amount",	"$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_fixed_sl_15",		"15-yr straight line depreciation ITC basis disallowance from federal fixed amount","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_fixed_sl_20",		"20-yr straight line depreciation ITC basis disallowance from federal fixed amount","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_fixed_sl_39",		"39-yr straight line depreciation ITC basis disallowance from federal fixed amount","$", "",  "DHF",             "*",					  "",     			        "" },


/* depreciation bases method - added with version 4.1    0=5-yrMacrs, 1=proportional */
	{ SSC_INPUT,        SSC_NUMBER,      "depr_stabas_method",    "Method of state depreciation reduction",     "",      "0=5yr MACRS,1=Proportional",                      "DHF",      "?=0",                       "INTEGER,MIN=0,MAX=1",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "depr_fedbas_method",    "Method of federal depreciation reduction",     "",      "0=5yr MACRS,1=Proportional",                      "DHF",      "?=0",                       "INTEGER,MIN=0,MAX=1",                                         "" },

/* Sponsor recovery method    0=Time, 1=Full Capital Recovery */
	{ SSC_INPUT,        SSC_NUMBER,      "depr_stabas_method",    "Sponsor Capital Recovery",     "",      "0=Time, 1=Full Capital Recovery",                      "DHF",      "?=0",                       "INTEGER,MIN=0,MAX=1",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "depr_fedbas_method",    "Duration (in years)",     "years",      "",                      "DHF",      "?=3",                       "INTEGER",                                         "" },


/* State depreciation table */
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_macrs_5",		"5-yr MACRS state depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_macrs_15",		"15-yr MACRS state depreciation basis",	"$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_sl_5",		"5-yr straight line state depreciation basis",	"$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_sl_15",		"15-yr straight line state depreciation basis","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_sl_20",		"20-yr straight line state depreciation basis","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_sl_39",		"39-yr straight line state depreciation basis","$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_total",		"Total state depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/* Federal depreciation table */
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_macrs_5",		"5-yr MACRS federal depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_macrs_15",		"15-yr MACRS federal depreciation basis",	"$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_sl_5",		"5-yr straight line federal depreciation basis",	"$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_sl_15",		"15-yr straight line federal depreciation basis","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_sl_20",		"20-yr straight line federal depreciation basis","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_sl_39",		"39-yr straight line federal depreciation basis","$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_total",		"Total federal depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },


/* State taxes */

	/* intermediate outputs for validation */
	{ SSC_OUTPUT,       SSC_NUMBER,      "cash_for_debt_service",   "Cash avaialble for debt service",   "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "pv_cafds", "Present value of cash avaialble for debt service","$", "",				  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "size_of_debt",			"Size of debt",	"$",	 "",					  "DHF",			 "*",                         "",                             "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "constr_interest",			"Interest during construction",	"$",	 "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "constr_upfront_fee",		"Construction up-front fee",	"$",	 "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "constr_total_financing",	"Construction financing total",	"$",	 "",					  "DHF",			 "*",                         "",                             "" },


/* model outputs */
	{ SSC_OUTPUT,        SSC_NUMBER,     "cf_length",                "Number of periods in cashflow",      "",             "",                      "DHF",      "*",                       "INTEGER",                                  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "ppa_price",			    "Initial year PPA price",			"cents/kWh",	"",				   "DHF",			  "*",                         "",      					   "" },
/* Production - input as energy_net above */

/* Partial Income Statement: Project */
	{ SSC_OUTPUT,        SSC_ARRAY,       "cf_energy_net",            "Net Energy",                     "kWh",      "",                      "DHF",             "*",                      "LENGTH_EQUAL=cf_length",                             "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "cf_ppa_price",            "PPA price",                     "cents/kWh",      "",                      "DHF",             "*",                      "LENGTH_EQUAL=cf_length",                             "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "cf_energy_value",         "Total PPA revenue",                     "$",      "",                      "DHF",             "*",                      "LENGTH_EQUAL=cf_length",                             "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_om_fixed_expense",      "O&M Fixed expense",                  "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_om_production_expense", "O&M Production-based expense",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_om_capacity_expense",   "O&M Capacity-based expense",         "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_om_fuel_expense",       "O&M Fuel expense",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "CF_property_tax_assesed_value","Property tax net assesed value", "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_property_tax_expense",  "Property tax expense",               "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_insurance_expense",     "Insurance expense",                  "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_operating_expenses",    "Total operating expense",            "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_total_revenue",    "Total revenue",            "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_ebitda",    "EBITDA (cash available for debt service)",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_reserve_debtservice",    "Debt service reserve",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_reserve_om",    "O and M reserve",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_reserve_equip1",    "Major equipment reserve 1",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_reserve_equip2",    "Major equipment reserve 2",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_reserve_equip3",    "Major equipment reserve 3",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_reserve_total",    "Total reserve",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_reserve_interest", "Interest on reserves",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_funding_debtservice",    "Debt service funding",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_funding_om",    "O and M funding",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_funding_equip1",    "Major equipment funding 1",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_funding_equip2",    "Major equipment funding 2",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_funding_equip3",    "Major equipment funding 3",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_disbursement_debtservice",    "Debt service disbursement",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_disbursement_om",    "O and M disbursement",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_disbursement_equip1",    "Major equipment disbursement 1",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_disbursement_equip2",    "Major equipment disbursement 2",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_disbursement_equip3",    "Major equipment disbursement 3",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_cash_for_ds",     "Cash for debt service",                       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_pv_cash_for_ds",     "Present value of cash for debt service",                       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_debt_size",          "Debt balance",                       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },


	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_debt_balance",          "Debt balance",                       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_debt_payment_interest", "Interest payment",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_debt_payment_principal","Principal payment",                  "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_debt_payment_total",    "Total P&I debt payment",             "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	// Project cash flow

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_operating_activities",    "Cash flow from operating activities",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "purchase_of_property",	"Purchase of property cost",	"$",	 "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_dsra",    "(Increase)/Decrease in debt service reserve account",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_wcra",    "(Increase)/Decrease in working captial reserve account",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_me1ra",    "(Increase)/Decrease in major equipment reserve account 1",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_me2ra",    "(Increase)/Decrease in major equipment reserve account 2",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_me3ra",    "(Increase)/Decrease in major equipment reserve account 3",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_ra",    "(Increase)/Decrease in reserve accounts",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_me1cs",    "Major equipment 1 capital spending",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_me2cs",    "Major equipment 2 capital spending",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_me3cs",    "Major equipment 3 capital spending",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_mecs",    "Major equipment capital spending",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_investing_activities",    "Cash flow from investing activities",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "issuance_of_equity",	"Issuance of equity",	"$",	 "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_financing_activities",    "Cash flow from financing activities",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_pretax_cashflow",    "Pre-tax cash flow",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

// Project returns
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_return_pretax",    "Pre-tax project returns",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_return_pretax_irr",    "Pre-tax project cumulative IRR",  "%", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_return_pretax_npv",    "Pre-tax project cumulative NPV",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_return_aftertax_cash",    "After-tax project returns cash total",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_return_aftertax",    "After-tax project returns",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_return_aftertax_irr",    "After-tax project cumulative IRR",  "%", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_return_aftertax_npv",    "After-tax project cumulative NPV",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_ibi_total",             "Total IBI incentive income",         "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_cbi_total",             "Total CBI incentive income",         "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_pbi_total",             "Total PBI incentive income",         "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_ptc_fed",               "Federal PTC income",                 "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_ptc_sta",               "State PTC income",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_itc_fed_total",         "Federal ITC income",                 "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_itc_sta_total",         "State ITC income",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_itc_fed_amt",         "Federal ITC income from fixed amount",                 "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_itc_sta_amt",         "State ITC income from fixed amount",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_itc_fed_per",         "Federal ITC income from percentage",                 "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_itc_sta_per",         "State ITC income from percentage",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

/* state depreciation and tax */
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_macrs_5",         "State depreciation from 5-yr MACRS",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_macrs_15",         "State depreciation from 15-yr MACRS",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_sl_5",         "State depreciation from 5-yr straight line",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_sl_15",         "State depreciation from 15-yr straight line",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_sl_20",         "State depreciation from 20-yr straight line",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_sl_39",         "State depreciation from 39-yr straight line",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_me1",         "State depreciation from major equipment 1",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_me2",         "State depreciation from major equipment 2",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_me3",         "State depreciation from major equipment 3",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_total",         "Total state tax depreciation",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_statax_income_prior_incentives", "State tax income prior incentives",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_statax_taxable_incentives", "State taxable incentives",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_statax_income_with_incentives", "State tax income with incentives",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_statax",				"State tax benefit/(liability)",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },


/* federal depreciation and tax */
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_macrs_5",         "Federal depreciation from 5-yr MACRS",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_macrs_15",         "Federal depreciation from 15-yr MACRS",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_sl_5",         "Federal depreciation from 5-yr straight line",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_sl_15",         "Federal depreciation from 15-yr straight line",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_sl_20",         "Federal depreciation from 20-yr straight line",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_sl_39",         "Federal depreciation from 39-yr straight line",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_me1",         "Federal depreciation from major equipment 1",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_me2",         "Federal depreciation from major equipment 2",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_me3",         "Federal depreciation from major equipment 3",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_total",         "Total federal tax depreciation",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_fedtax_income_prior_incentives", "Federal tax income prior incentives",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_fedtax_taxable_incentives", "Federal taxable incentives",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_fedtax_income_with_incentives", "Federal tax income with incentives",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_fedtax",				"Federal tax benefit/(liability)",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "lcoe_real",                "Real LCOE",                          "cents/kWh",    "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "lcoe_nom",                 "Nominal LCOE",                       "cents/kWh",    "",                      "DHF",      "*",                       "",                                         "" },

	// tax investor
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_tax_investor_aftertax_cash",    "After-tax tax investor cash returns",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_tax_investor_aftertax_itc",    "After-tax tax investor itc returns",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_tax_investor_aftertax_ptc",    "After-tax tax investor ptc returns",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_tax_investor_aftertax_tax",    "After-tax tax investor tax returns",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_tax_investor_aftertax",        "After-tax tax investor returns",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_tax_investor_aftertax_irr",    "After-tax tax investor cumulative IRR",  "%", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_tax_investor_aftertax_npv",    "After-tax tax investor cumulative NPV",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_tax_investor_aftertax_max_irr",    "After-tax tax investor maximum IRR",  "%", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "target_return_flip_year",       "Year Flip Target/Year Hurdle Rate Reached",                       "",    "",                      "DHF",      "*",                       "INTEGER",     "" },


	// Sponsor
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_sponsor_pretax_equity",    "Pre-tax sponsor equity investment",  "$", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_sponsor_pretax_development",    "Pre-tax sponsor development fee",  "$", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sponsor_pretax",    "Pre-tax sponsor total",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sponsor_pretax_irr",    "Pre-tax sponsor cumulative IRR",  "%", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sponsor_pretax_npv",    "Pre-tax sponsor cumulative NPV",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_sponsor_pretax_irr",    "Pre-tax sponsor IRR",  "%", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_sponsor_pretax_npv",    "Pre-tax sponsor NPV",  "$", "",                      "DHF",      "*",                     "",                "" },

	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_sponsor_aftertax_equity",    "After-tax sponsor equity investment",  "$", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_sponsor_aftertax_development",    "After-tax sponsor development fee",  "$", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sponsor_aftertax_cash",    "After-tax sponsor cash returns",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sponsor_aftertax",    "After-tax sponsor total",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sponsor_aftertax_itc",    "After-tax sponsor itc returns",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sponsor_aftertax_ptc",    "After-tax sponsor ptc returns",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sponsor_aftertax_tax",    "After-tax sponsor tax returns",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sponsor_aftertax_irr",    "After-tax sponsor cumulative IRR",  "%", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sponsor_aftertax_npv",    "After-tax sponsor cumulative NPV",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_sponsor_aftertax_irr",    "After-tax sponsor IRR",  "%", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_sponsor_aftertax_npv",    "After-tax sponsor NPV",  "$", "",                      "DHF",      "*",                     "",                "" },


var_info_invalid };

extern var_info
	vtab_standard_financial[],
	vtab_standard_loan[],
	vtab_oandm[],
	vtab_depreciation[],
	vtab_utility_rate[],
	vtab_tax_credits[],
	vtab_payment_incentives[];

enum {
	CF_energy_net,
	CF_energy_value,
	CF_ppa_price,
	CF_ppa_revenue,

	CF_om_fixed_expense,
	CF_om_production_expense,
	CF_om_capacity_expense,
	CF_om_fuel_expense,
	CF_property_tax_assesed_value,
	CF_property_tax_expense,
	CF_insurance_expense,
	CF_operating_expenses,
	CF_total_revenue,
	CF_ebitda,

	CF_reserve_debtservice,
	CF_funding_debtservice,
	CF_disbursement_debtservice,
	CF_reserve_om,
	CF_funding_om,
	CF_disbursement_om,
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
	CF_project_return_aftertax,
	CF_project_return_aftertax_irr,
	CF_project_return_aftertax_npv,


	// tax investor returns
	CF_tax_investor_aftertax_cash,
	CF_tax_investor_aftertax_itc,
	CF_tax_investor_aftertax_ptc,
	CF_tax_investor_aftertax_tax,
	CF_tax_investor_aftertax,
	CF_tax_investor_aftertax_irr,
	CF_tax_investor_aftertax_max_irr,
	CF_tax_investor_aftertax_npv,

	// sponsor returns
	CF_sponsor_pretax,
	CF_sponsor_pretax_irr,
	CF_sponsor_pretax_npv,
	CF_sponsor_aftertax_cash,
	CF_sponsor_aftertax,
	CF_sponsor_aftertax_itc,
	CF_sponsor_aftertax_ptc,
	CF_sponsor_aftertax_tax,
	CF_sponsor_aftertax_irr,
	CF_sponsor_aftertax_npv,


	CF_deductible_expenses,

	CF_pv_interest_factor,
	CF_cash_for_ds,
	CF_pv_cash_for_ds,
	CF_debt_size,

	CF_debt_balance,
	CF_debt_payment_interest,
	CF_debt_payment_principal,
	CF_debt_payment_total,

	CF_ibi_fed_amt,
	CF_ibi_sta_amt,
	CF_ibi_uti_amt,
	CF_ibi_oth_amt,
	CF_ibi_fed_per,
	CF_ibi_sta_per,
	CF_ibi_uti_per,
	CF_ibi_oth_per,
	CF_ibi_total,

	CF_cbi_fed,
	CF_cbi_sta,
	CF_cbi_uti,
	CF_cbi_oth,
	CF_cbi_total,

	CF_pbi_fed,
	CF_pbi_sta,
	CF_pbi_uti,
	CF_pbi_oth,
	CF_pbi_total,

	CF_ptc_fed,
	CF_ptc_sta,

	CF_itc_fed_amt,
	CF_itc_fed_per,
	CF_itc_fed_total,

	CF_itc_sta_amt,
	CF_itc_sta_per,
	CF_itc_sta_total,

	CF_itc_total,

	CF_macrs_5_frac,
	CF_macrs_15_frac,
	CF_sl_5_frac,
	CF_sl_15_frac,
	CF_sl_20_frac,
	CF_sl_39_frac,

	CF_stadepr_macrs_5,
	CF_stadepr_macrs_15,
	CF_stadepr_sl_5,
	CF_stadepr_sl_15,
	CF_stadepr_sl_20,
	CF_stadepr_sl_39,
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
	CF_sta_tax_savings,

	CF_fed_depr_sched,
	CF_fed_depreciation,
	CF_fed_incentive_income_less_deductions,
	CF_fed_taxable_income_less_deductions,
	CF_fed_tax_savings,

	CF_sta_and_fed_tax_savings,
	CF_after_tax_net_equity_cost_flow,
	CF_after_tax_cash_flow,

	CF_payback_with_expenses,
	CF_cumulative_payback_with_expenses,

	CF_payback_without_expenses,
	CF_cumulative_payback_without_expenses,

	CF_max };



class cm_equpartflip : public compute_module
{
private:
	util::matrix_t<double> cf;

public:
	cm_equpartflip()
	{
		add_var_info( vtab_standard_financial );
		add_var_info( vtab_standard_loan );
		add_var_info( vtab_oandm );
		add_var_info( vtab_depreciation );
		add_var_info( vtab_tax_credits );
		add_var_info( vtab_payment_incentives );

		add_var_info( _cm_vtab_equpartflip );
	}

	void exec( ) throw( general_error )
	{

		// cash flow initialization
		int nyears = as_integer("analysis_years");
		cf.resize_fill( CF_max, nyears+1, 0.0 );

		// assign inputs
		double inflation_rate = as_double("inflation_rate")*0.01;
		double ppa_escalation = as_double("ppa_escalation")*0.01;
		double disc_real = as_double("real_discount_rate")*0.01;
		double federal_tax_rate = as_double("federal_tax_rate")*0.01;
		double state_tax_rate = as_double("state_tax_rate")*0.01;

		double nom_discount_rate = (1+inflation_rate)*(1+disc_real)-1;

		double gen = as_double("cost_gen_equip");
		double bop = as_double("cost_bop");
		double net = as_double("cost_network");
		double cont = as_double("percent_contingency")*0.01;

		double cost_hard = gen + bop + net;
		double cost_cont = cost_hard * cont;

		double dev = as_double("cost_developer");
		double land = as_double("cost_land_improve");
		double other = as_double("cost_other");
		double cost_taxable = as_double("percent_taxable") / 100.0;
		double sales_tax_rate = as_double("sales_tax_rate") / 100.0;

		double cost_soft = dev + land + other;
		double cost_salestax = (cost_hard + cost_cont + cost_soft) * cost_taxable * sales_tax_rate;

		// use DHF named range names for variables whenever possible
		double cost_prefinancing = cost_soft + cost_salestax + cost_hard + cost_cont;
		double nameplate = as_double("system_capacity");

		double assessed_frac = as_double("prop_tax_cost_assessed_percent")*0.01;
		double salvage_value_frac = as_double("salvage_percentage")*0.01;
		double salvage_value = salvage_value_frac * cost_prefinancing;

		double cost_debt_closing = as_double("cost_debt_closing");
		double cost_equity_closing = as_double("cost_equity_closing");
		double cost_dev_fee_percent = as_double("cost_dev_fee_percent")*0.01;

		int constr_months = as_integer("constr_months");
		double constr_int_rate = as_double("constr_int_rate")*0.01;
		double constr_upfront_percent = as_double("constr_upfront_percent")*0.01;

		double constr_interest = cost_prefinancing * (constr_months/2.0) * (constr_int_rate/12.0);
		double constr_upfront_fee = cost_prefinancing * constr_upfront_percent;
		double constr_total_financing = constr_upfront_fee + constr_interest;

		int ppa_mode = as_integer("ppa_soln_mode");

		// general financial expenses and incentives - stdlib?
		// precompute expenses from annual schedules or value+escalation
		escal_or_annual( CF_om_fixed_expense, nyears, "om_fixed", inflation_rate, 1.0, false, as_double("om_fixed_escal")*0.01 );
		escal_or_annual( CF_om_production_expense, nyears, "om_production", inflation_rate, 0.001, false, as_double("om_production_escal")*0.01 );
		escal_or_annual( CF_om_capacity_expense, nyears, "om_capacity", inflation_rate, 1.0, false, as_double("om_capacity_escal")*0.01 );
		escal_or_annual( CF_om_fuel_expense, nyears, "om_fuel_cost", inflation_rate, as_double("system_heat_rate")*0.001, false, as_double("om_fuel_cost_escal")*0.01 );

		// initialize energy
		size_t count = 0;
		ssc_number_t *arrp = 0;

		arrp = as_array("energy_net", &count);
		int i=0;
		while ( i < nyears && i < (int)count )
		{
			cf.at(CF_energy_net, i+1) = (double) arrp[i];
			cf.at(CF_om_production_expense,i+1) *= cf.at(CF_energy_net,i+1);
			cf.at(CF_om_capacity_expense,i+1) *= nameplate;

			i++;
		}

		// precompute ibi
		single_or_schedule( CF_ibi_fed_amt, nyears, 1.0, "ibi_fed_amount" );
		single_or_schedule( CF_ibi_sta_amt, nyears, 1.0, "ibi_sta_amount" );
		single_or_schedule( CF_ibi_uti_amt, nyears, 1.0, "ibi_uti_amount" );
		single_or_schedule( CF_ibi_oth_amt, nyears, 1.0, "ibi_oth_amount" );


		// precompute cbi
		single_or_schedule_check_max( CF_cbi_fed, nyears, 1000*nameplate, "cbi_fed_amount", "cbi_fed_maxvalue");
		single_or_schedule_check_max( CF_cbi_sta, nyears, 1000*nameplate, "cbi_sta_amount", "cbi_sta_maxvalue");
		single_or_schedule_check_max( CF_cbi_uti, nyears, 1000*nameplate, "cbi_uti_amount", "cbi_uti_maxvalue");
		single_or_schedule_check_max( CF_cbi_oth, nyears, 1000*nameplate, "cbi_oth_amount", "cbi_oth_maxvalue");

		// precompute pbi
		compute_production_incentive( CF_pbi_fed, nyears, "pbi_fed_amount", "pbi_fed_term", "pbi_fed_escal" );
		compute_production_incentive( CF_pbi_sta, nyears, "pbi_sta_amount", "pbi_sta_term", "pbi_sta_escal" );
		compute_production_incentive( CF_pbi_uti, nyears, "pbi_uti_amount", "pbi_uti_term", "pbi_uti_escal" );
		compute_production_incentive( CF_pbi_oth, nyears, "pbi_oth_amount", "pbi_oth_term", "pbi_oth_escal" );

		// precompute ptc
		compute_production_incentive( CF_ptc_sta, nyears, "ptc_sta_amount", "ptc_sta_term", "ptc_sta_escal" );
		compute_production_incentive( CF_ptc_fed, nyears, "ptc_fed_amount", "ptc_fed_term", "ptc_fed_escal" );

		double ppa = as_double("ppa_price_input"); // either initial guess for ppa_mode=1 or final ppa for pp_mode=0

		double property_tax_assessed_value = cost_prefinancing * as_double("prop_tax_cost_assessed_percent") * 0.01;
		double property_tax_decline_percentage = as_double("prop_tax_assessed_decline");
		double property_tax_rate = as_double("property_tax_rate")*0.01;
		double insurance_rate = as_double("insurance_rate")*0.01;
		double cost_working_reserve = as_double("cost_working_reserve");
		double equip1_reserve_cost = as_double("equip1_reserve_cost");
		int equip1_reserve_freq = as_integer("equip1_reserve_freq");
		double equip2_reserve_cost = as_double("equip2_reserve_cost");
		int equip2_reserve_freq = as_integer("equip2_reserve_freq");
		double equip3_reserve_cost = as_double("equip3_reserve_cost");
		int equip3_reserve_freq = as_integer("equip3_reserve_freq");

		//  calculate debt
		int term_tenor = as_integer("term_tenor");
		double term_int_rate = as_double("term_int_rate")*0.01;
		double dscr = as_double("dscr");
		int dscr_reserve_months = as_integer("dscr_reserve_months");
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

		int feddepr_me1=CF_macrs_5_frac + as_integer("equip_reserve_depr_fed");
		int feddepr_me2=CF_macrs_5_frac + as_integer("equip_reserve_depr_fed");
		int feddepr_me3=CF_macrs_5_frac + as_integer("equip_reserve_depr_fed");

		int stadepr_me1=CF_macrs_5_frac + as_integer("equip_reserve_depr_sta");
		int stadepr_me2=CF_macrs_5_frac + as_integer("equip_reserve_depr_sta");
		int stadepr_me3=CF_macrs_5_frac + as_integer("equip_reserve_depr_sta");


		for (i=1;i<=nyears;i++)
		{
			if (i%equip1_reserve_freq == 0)
			{
				major_equipment_depreciation(CF_disbursement_equip1,feddepr_me1,i,nyears,CF_feddepr_me1);
				major_equipment_depreciation(CF_disbursement_equip1,stadepr_me1,i,nyears,CF_stadepr_me1);
			}
			if (i%equip2_reserve_freq == 0)
			{
				major_equipment_depreciation(CF_disbursement_equip2,feddepr_me2,i,nyears,CF_feddepr_me2);
				major_equipment_depreciation(CF_disbursement_equip2,stadepr_me2,i,nyears,CF_stadepr_me2);
			}
			if (i%equip3_reserve_freq == 0)
			{
				major_equipment_depreciation(CF_disbursement_equip3,feddepr_me3,i,nyears,CF_feddepr_me3);
				major_equipment_depreciation(CF_disbursement_equip3,stadepr_me3,i,nyears,CF_stadepr_me3);
			}
		}

		for (i=1; i<=nyears; i++)
		{
		// Project partial income statement

			double decline_percent = 100 - (i-1)*property_tax_decline_percentage;
			cf.at(CF_property_tax_assesed_value,i) = (decline_percent > 0) ? property_tax_assessed_value * decline_percent * 0.01:0.0;
			cf.at(CF_property_tax_expense,i) = cf.at(CF_property_tax_assesed_value,i) * property_tax_rate;
			cf.at(CF_insurance_expense,i) = cost_prefinancing * insurance_rate * pow( 1 + inflation_rate, i-1 );

			cf.at(CF_operating_expenses,i) =
				+ cf.at(CF_om_fixed_expense,i)
				+ cf.at(CF_om_production_expense,i)
				+ cf.at(CF_om_capacity_expense,i)
				+ cf.at(CF_om_fuel_expense,i)
				+ cf.at(CF_property_tax_expense,i)
				+ cf.at(CF_insurance_expense,i);
		}

		// o and m reserve
		cf.at(CF_funding_om,0) = cost_working_reserve;
		cf.at(CF_disbursement_om,nyears) = -cost_working_reserve;
		cf.at(CF_reserve_om,0) = cost_working_reserve;
		for (i=1; i<=nyears; i++)
			cf.at(CF_reserve_om,i) = cf.at(CF_reserve_om,i-1)+cf.at(CF_funding_om,i)+cf.at(CF_disbursement_om,i);

		for (i=0;i<=nyears; i++)
		{
			cf.at(CF_project_wcra,i) = -cf.at(CF_funding_om,i) - cf.at(CF_disbursement_om,i);
			cf.at(CF_project_me1ra,i) = -cf.at(CF_funding_equip1,i) - cf.at(CF_disbursement_equip1,i);
			cf.at(CF_project_me2ra,i) = -cf.at(CF_funding_equip2,i) - cf.at(CF_disbursement_equip2,i);
			cf.at(CF_project_me3ra,i) = -cf.at(CF_funding_equip3,i) - cf.at(CF_disbursement_equip3,i);
		}

		// interest on reserves
		double reserves_interest = as_double("reserves_interest")*0.01;

		double tax_investor_equity_frac = as_double("tax_investor_equity_percent") * 0.01;
		double tax_investor_preflip_cash_frac = as_double("tax_investor_preflip_cash_percent") * 0.01;
		double tax_investor_postflip_cash_frac = as_double("tax_investor_postflip_cash_percent") * 0.01;
		double tax_investor_preflip_tax_frac = as_double("tax_investor_preflip_tax_percent") * 0.01;
		double tax_investor_postflip_tax_frac = as_double("tax_investor_postflip_tax_percent") * 0.01;
		double sponsor_pretax_development_fee = cost_dev_fee_percent * cost_prefinancing;

		double issuance_of_equity;
		double equity_tax_investor;
		double sponsor_pretax_equity_investment;

		// precompute ibi
		single_or_schedule_check_max( CF_ibi_fed_per, nyears, 0.01*cost_prefinancing, "ibi_fed_percent", "ibi_fed_percent_maxvalue" );
		single_or_schedule_check_max( CF_ibi_sta_per, nyears, 0.01*cost_prefinancing, "ibi_sta_percent", "ibi_sta_percent_maxvalue" );
		single_or_schedule_check_max( CF_ibi_uti_per, nyears, 0.01*cost_prefinancing, "ibi_uti_percent", "ibi_uti_percent_maxvalue" );
		single_or_schedule_check_max( CF_ibi_oth_per, nyears, 0.01*cost_prefinancing, "ibi_oth_percent", "ibi_oth_percent_maxvalue" );
		// precompute itc
		single_or_schedule( CF_itc_fed_amt, nyears, 1.0, "itc_fed_amount" );
		single_or_schedule( CF_itc_sta_amt, nyears, 1.0, "itc_sta_amount" );
		single_or_schedule_check_max( CF_itc_fed_per, nyears, 0.01, "itc_fed_percent", "itc_fed_percent_maxvalue");
		single_or_schedule_check_max( CF_itc_sta_per, nyears, 0.01, "itc_sta_percent", "itc_sta_percent_maxvalue");

		for (i=0;i<=nyears;i++)
		{
			cf.at(CF_cbi_total,i) = cf.at(CF_cbi_fed,i) + cf.at(CF_cbi_sta,i) + cf.at(CF_cbi_uti,i) + cf.at(CF_cbi_oth,i);
			cf.at(CF_ibi_total,i) = cf.at(CF_ibi_fed_amt,i) + cf.at(CF_ibi_sta_amt,i) + cf.at(CF_ibi_uti_amt,i) + cf.at(CF_ibi_oth_amt,i) + cf.at(CF_ibi_fed_per,i) + cf.at(CF_ibi_sta_per,i) + cf.at(CF_ibi_uti_per,i) + cf.at(CF_ibi_oth_per,i);

			cf.at(CF_itc_fed_total,i) = cf.at(CF_itc_fed_amt,i) + cf.at(CF_itc_fed_per,i);
			cf.at(CF_itc_sta_total,i) = cf.at(CF_itc_sta_amt,i) + cf.at(CF_itc_sta_per,i);
			cf.at(CF_itc_total,i) = cf.at(CF_itc_fed_total,i) + cf.at(CF_itc_sta_total,i);
		}

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_statax_taxable_incentives,i) =
				( as_boolean("ibi_fed_amount_tax_sta") ? cf.at(CF_ibi_fed_amt,i) : 0 ) +
				( as_boolean("ibi_fed_percent_tax_sta") ? cf.at(CF_ibi_fed_per,i) : 0 ) +
				( as_boolean("ibi_sta_amount_tax_sta") ? cf.at(CF_ibi_sta_amt,i) : 0 ) +
				( as_boolean("ibi_sta_percent_tax_sta") ? cf.at(CF_ibi_sta_per,i) : 0 ) +
				( as_boolean("ibi_uti_amount_tax_sta") ? cf.at(CF_ibi_uti_amt,i) : 0 ) +
				( as_boolean("ibi_uti_percent_tax_sta") ? cf.at(CF_ibi_uti_per,i) : 0 ) +
				( as_boolean("ibi_oth_amount_tax_sta") ? cf.at(CF_ibi_oth_amt,i) : 0 ) +
				( as_boolean("ibi_oth_percent_tax_sta") ? cf.at(CF_ibi_oth_per,i) : 0 ) +
				( as_boolean("cbi_fed_tax_sta") ? cf.at(CF_cbi_fed,i) : 0 ) +
				( as_boolean("cbi_sta_tax_sta") ? cf.at(CF_cbi_sta,i) : 0 ) +
				( as_boolean("cbi_uti_tax_sta") ? cf.at(CF_cbi_uti,i) : 0 ) +
				( as_boolean("cbi_oth_tax_sta") ? cf.at(CF_cbi_oth,i) : 0 ) +
				( (as_boolean("pbi_fed_tax_sta") && (!as_boolean("pbi_fed_for_ds")) ) ? cf.at(CF_pbi_fed,i) : 0 ) +
				( (as_boolean("pbi_sta_tax_sta") && (!as_boolean("pbi_sta_for_ds")) ) ? cf.at(CF_pbi_sta,i) : 0 ) +
				( (as_boolean("pbi_uti_tax_sta") && (!as_boolean("pbi_uti_for_ds")) ) ? cf.at(CF_pbi_uti,i) : 0 ) +
				( (as_boolean("pbi_oth_tax_sta") && (!as_boolean("pbi_oth_for_ds")) ) ? cf.at(CF_pbi_oth,i) : 0 ) ;

			cf.at(CF_fedtax_taxable_incentives,i) =
				( as_boolean("ibi_fed_amount_tax_fed") ? cf.at(CF_ibi_fed_amt,i) : 0 ) +
				( as_boolean("ibi_fed_percent_tax_fed") ? cf.at(CF_ibi_fed_per,i) : 0 ) +
				( as_boolean("ibi_sta_amount_tax_fed") ? cf.at(CF_ibi_sta_amt,i) : 0 ) +
				( as_boolean("ibi_sta_percent_tax_fed") ? cf.at(CF_ibi_sta_per,i) : 0 ) +
				( as_boolean("ibi_uti_amount_tax_fed") ? cf.at(CF_ibi_uti_amt,i) : 0 ) +
				( as_boolean("ibi_uti_percent_tax_fed") ? cf.at(CF_ibi_uti_per,i) : 0 ) +
				( as_boolean("ibi_oth_amount_tax_fed") ? cf.at(CF_ibi_oth_amt,i) : 0 ) +
				( as_boolean("ibi_oth_percent_tax_fed") ? cf.at(CF_ibi_oth_per,i) : 0 ) +
				( as_boolean("cbi_fed_tax_fed") ? cf.at(CF_cbi_fed,i) : 0 ) +
				( as_boolean("cbi_sta_tax_fed") ? cf.at(CF_cbi_sta,i) : 0 ) +
				( as_boolean("cbi_uti_tax_fed") ? cf.at(CF_cbi_uti,i) : 0 ) +
				( as_boolean("cbi_oth_tax_fed") ? cf.at(CF_cbi_oth,i) : 0 ) +
				( (as_boolean("pbi_fed_tax_fed") && (!as_boolean("pbi_fed_for_ds")) ) ? cf.at(CF_pbi_fed,i) : 0 ) +
				( (as_boolean("pbi_sta_tax_fed") && (!as_boolean("pbi_sta_for_ds")) ) ? cf.at(CF_pbi_sta,i) : 0 ) +
				( (as_boolean("pbi_uti_tax_fed") && (!as_boolean("pbi_uti_for_ds")) ) ? cf.at(CF_pbi_uti,i) : 0 ) +
				( (as_boolean("pbi_oth_tax_fed") && (!as_boolean("pbi_oth_for_ds")) ) ? cf.at(CF_pbi_oth,i) : 0 ) ;
		}

		double cost_financing;

		double cost_installed;

		double depr_alloc_macrs_5_frac = as_double("depr_alloc_macrs_5_percent") * 0.01;
		double depr_alloc_macrs_15_frac = as_double("depr_alloc_macrs_15_percent") * 0.01;
		double depr_alloc_sl_5_frac = as_double("depr_alloc_sl_5_percent") * 0.01;
		double depr_alloc_sl_15_frac = as_double("depr_alloc_sl_15_percent") * 0.01;
		double depr_alloc_sl_20_frac = as_double("depr_alloc_sl_20_percent") * 0.01;
		double depr_alloc_sl_39_frac = as_double("depr_alloc_sl_39_percent") * 0.01;
		double depr_alloc_total_frac = depr_alloc_macrs_5_frac + depr_alloc_macrs_15_frac +
			depr_alloc_sl_5_frac + depr_alloc_sl_15_frac +	depr_alloc_sl_20_frac +	depr_alloc_sl_39_frac;
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
		}

		double depr_stabas_macrs_5_frac;
		double depr_stabas_macrs_15_frac;
		double depr_stabas_sl_5_frac;
		double depr_stabas_sl_15_frac;
		double depr_stabas_sl_20_frac;
		double depr_stabas_sl_39_frac;

		if (as_integer("depr_stabas_method")==0)
		{
			depr_stabas_macrs_5_frac = 1.0;
			depr_stabas_macrs_15_frac = 0.0;
			depr_stabas_sl_5_frac = 0.0;
			depr_stabas_sl_15_frac = 0.0;
			depr_stabas_sl_20_frac = 0.0;
			depr_stabas_sl_39_frac = 0.0;
		}
		else
		{
			depr_stabas_macrs_5_frac = depr_alloc_macrs_5_frac;
			depr_stabas_macrs_15_frac = depr_alloc_macrs_15_frac;
			depr_stabas_sl_5_frac = depr_alloc_sl_5_frac;
			depr_stabas_sl_15_frac = depr_alloc_sl_15_frac;
			depr_stabas_sl_20_frac = depr_alloc_sl_20_frac;
			depr_stabas_sl_39_frac = depr_alloc_sl_39_frac;
		}

		double depr_fedbas_macrs_5_frac;
		double depr_fedbas_macrs_15_frac;
		double depr_fedbas_sl_5_frac;
		double depr_fedbas_sl_15_frac;
		double depr_fedbas_sl_20_frac;
		double depr_fedbas_sl_39_frac;

		if (as_integer("depr_fedbas_method")==0)
		{
			depr_fedbas_macrs_5_frac = 1.0;
			depr_fedbas_macrs_15_frac = 0.0;
			depr_fedbas_sl_5_frac = 0.0;
			depr_fedbas_sl_15_frac = 0.0;
			depr_fedbas_sl_20_frac = 0.0;
			depr_fedbas_sl_39_frac = 0.0;
		}
		else
		{
			depr_fedbas_macrs_5_frac = depr_alloc_macrs_5_frac;
			depr_fedbas_macrs_15_frac = depr_alloc_macrs_15_frac;
			depr_fedbas_sl_5_frac = depr_alloc_sl_5_frac;
			depr_fedbas_sl_15_frac = depr_alloc_sl_15_frac;
			depr_fedbas_sl_20_frac = depr_alloc_sl_20_frac;
			depr_fedbas_sl_39_frac = depr_alloc_sl_39_frac;
		}

		double depr_alloc_macrs_5;
		double depr_alloc_macrs_15;
		double depr_alloc_sl_5;
		double depr_alloc_sl_15;
		double depr_alloc_sl_20;
		double depr_alloc_sl_39;
		double depr_alloc_none;
		double depr_alloc_total;

//State ITC
		double itc_sta_reduction =  (
			( as_boolean("ibi_fed_amount_itcbas_sta")  ? npv( CF_ibi_fed_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_fed_percent_itcbas_sta")  ? npv( CF_ibi_fed_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_fed_itcbas_sta")  ? npv( CF_cbi_fed, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_sta_amount_itcbas_sta")  ? npv( CF_ibi_sta_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_sta_percent_itcbas_sta")  ? npv( CF_ibi_sta_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_sta_itcbas_sta")  ? npv( CF_cbi_sta, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_uti_amount_itcbas_sta")  ? npv( CF_ibi_uti_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_uti_percent_itcbas_sta")  ? npv( CF_ibi_uti_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_uti_itcbas_sta")  ? npv( CF_cbi_uti, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_oth_amount_itcbas_sta")  ? npv( CF_ibi_oth_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_oth_percent_itcbas_sta")  ? npv( CF_ibi_oth_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_oth_itcbas_sta")  ? npv( CF_cbi_oth, nyears, nom_discount_rate ) : 0 )
			);
// escalate back to year 1 for itc reduction
		itc_sta_reduction *=  (1.0 + nom_discount_rate);

		double itc_sta_qual_macrs_5_frac = ( as_boolean("depr_itc_sta_macrs_5")  ? depr_stabas_macrs_5_frac: 0 ) ;
		double itc_sta_qual_macrs_15_frac = ( as_boolean("depr_itc_sta_macrs_15")  ? depr_stabas_macrs_15_frac: 0 ) ;
		double itc_sta_qual_sl_5_frac = ( as_boolean("depr_itc_sta_sl_5")  ? depr_stabas_sl_5_frac: 0 ) ;
		double itc_sta_qual_sl_15_frac = ( as_boolean("depr_itc_sta_sl_15")   ? depr_stabas_sl_15_frac: 0 ) ;
		double itc_sta_qual_sl_20_frac = ( as_boolean("depr_itc_sta_sl_20")  ? depr_stabas_sl_20_frac: 0 ) ;
		double itc_sta_qual_sl_39_frac = ( as_boolean("depr_itc_sta_sl_39")  ? depr_stabas_sl_39_frac: 0 ) ;

		double itc_sta_qual_total;

		double itc_sta_qual_macrs_5;
		double itc_sta_qual_macrs_15;
		double itc_sta_qual_sl_5;
		double itc_sta_qual_sl_15;
		double itc_sta_qual_sl_20;
		double itc_sta_qual_sl_39;

		double itc_sta_percent_total = npv(CF_itc_sta_per,nyears,nom_discount_rate);
		double itc_sta_fixed_total = npv(CF_itc_sta_amt,nyears,nom_discount_rate);
// escalate back to year 1 for itc reduction
		itc_sta_percent_total *= (1 + nom_discount_rate);
		itc_sta_fixed_total *= (1 + nom_discount_rate);

		double itc_sta_percent_maxvalue = as_double("itc_sta_percent_maxvalue");

		double itc_sta_disallow_factor = as_double("itc_sta_disallow_factor");

		double itc_disallow_sta_percent_macrs_5;
		double itc_disallow_sta_percent_macrs_15;
		double itc_disallow_sta_percent_sl_5;
		double itc_disallow_sta_percent_sl_15;
		double itc_disallow_sta_percent_sl_20;
		double itc_disallow_sta_percent_sl_39;

		double itc_disallow_sta_fixed_macrs_5 = (itc_sta_disallow_factor*itc_sta_qual_macrs_5_frac * itc_sta_fixed_total);
		double itc_disallow_sta_fixed_macrs_15 = (itc_sta_disallow_factor*itc_sta_qual_macrs_15_frac * itc_sta_fixed_total);
		double itc_disallow_sta_fixed_sl_5 = (itc_sta_disallow_factor*itc_sta_qual_sl_5_frac * itc_sta_fixed_total);
		double itc_disallow_sta_fixed_sl_15 = (itc_sta_disallow_factor*itc_sta_qual_sl_15_frac * itc_sta_fixed_total);
		double itc_disallow_sta_fixed_sl_20 = (itc_sta_disallow_factor*itc_sta_qual_sl_20_frac * itc_sta_fixed_total);
		double itc_disallow_sta_fixed_sl_39 = (itc_sta_disallow_factor*itc_sta_qual_sl_39_frac * itc_sta_fixed_total);

//Federal ITC

		double itc_fed_reduction =  (
			( as_boolean("ibi_fed_amount_itcbas_fed")  ? npv( CF_ibi_fed_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_fed_percent_itcbas_fed")  ? npv( CF_ibi_fed_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_fed_itcbas_fed")  ? npv( CF_cbi_fed, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_sta_amount_itcbas_fed")  ? npv( CF_ibi_sta_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_sta_percent_itcbas_fed")  ? npv( CF_ibi_sta_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_sta_itcbas_fed")  ? npv( CF_cbi_sta, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_uti_amount_itcbas_fed")  ? npv( CF_ibi_uti_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_uti_percent_itcbas_fed")  ? npv( CF_ibi_uti_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_uti_itcbas_fed")  ? npv( CF_cbi_uti, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_oth_amount_itcbas_fed")  ? npv( CF_ibi_oth_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_oth_percent_itcbas_fed")  ? npv( CF_ibi_oth_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_oth_itcbas_fed")  ? npv( CF_cbi_oth, nyears, nom_discount_rate ) : 0 )
			);
// escalate back to year 1 for itc reduction
		itc_fed_reduction *=  (1.0 + nom_discount_rate);

		double itc_fed_qual_macrs_5_frac = ( as_boolean("depr_itc_fed_macrs_5")  ? depr_fedbas_macrs_5_frac: 0 ) ;
		double itc_fed_qual_macrs_15_frac = ( as_boolean("depr_itc_fed_macrs_15")  ? depr_fedbas_macrs_15_frac: 0 ) ;
		double itc_fed_qual_sl_5_frac = ( as_boolean("depr_itc_fed_sl_5")  ? depr_fedbas_sl_5_frac: 0 ) ;
		double itc_fed_qual_sl_15_frac = ( as_boolean("depr_itc_fed_sl_15")   ? depr_fedbas_sl_15_frac: 0 ) ;
		double itc_fed_qual_sl_20_frac = ( as_boolean("depr_itc_fed_sl_20")  ? depr_fedbas_sl_20_frac: 0 ) ;
		double itc_fed_qual_sl_39_frac = ( as_boolean("depr_itc_fed_sl_39")  ? depr_fedbas_sl_39_frac: 0 ) ;

		double itc_fed_qual_total;

		double itc_fed_qual_macrs_5;
		double itc_fed_qual_macrs_15;
		double itc_fed_qual_sl_5;
		double itc_fed_qual_sl_15;
		double itc_fed_qual_sl_20;
		double itc_fed_qual_sl_39;

		double itc_fed_percent_total = npv(CF_itc_fed_per,nyears,nom_discount_rate);
		double itc_fed_fixed_total = npv(CF_itc_fed_amt,nyears,nom_discount_rate);
// escalate back to year 1 for itc reduction
		itc_fed_percent_total *= (1 + nom_discount_rate);
		itc_fed_fixed_total *= (1 + nom_discount_rate);

		double itc_fed_percent_maxvalue = as_double("itc_fed_percent_maxvalue");

		double itc_fed_disallow_factor = as_double("itc_fed_disallow_factor");

		double itc_disallow_fed_percent_macrs_5;
		double itc_disallow_fed_percent_macrs_15;
		double itc_disallow_fed_percent_sl_5;
		double itc_disallow_fed_percent_sl_15;
		double itc_disallow_fed_percent_sl_20;
		double itc_disallow_fed_percent_sl_39;

		double itc_disallow_fed_fixed_macrs_5 = (itc_fed_disallow_factor*itc_fed_qual_macrs_5_frac * itc_fed_fixed_total);
		double itc_disallow_fed_fixed_macrs_15 = (itc_fed_disallow_factor*itc_fed_qual_macrs_15_frac * itc_fed_fixed_total);
		double itc_disallow_fed_fixed_sl_5 = (itc_fed_disallow_factor*itc_fed_qual_sl_5_frac * itc_fed_fixed_total);
		double itc_disallow_fed_fixed_sl_15 = (itc_fed_disallow_factor*itc_fed_qual_sl_15_frac * itc_fed_fixed_total);
		double itc_disallow_fed_fixed_sl_20 = (itc_fed_disallow_factor*itc_fed_qual_sl_20_frac * itc_fed_fixed_total);
		double itc_disallow_fed_fixed_sl_39 = (itc_fed_disallow_factor*itc_fed_qual_sl_39_frac * itc_fed_fixed_total);



// Depreciation
// State depreciation
		double depr_sta_reduction =  (
			( as_boolean("ibi_fed_amount_deprbas_sta")  ? npv( CF_ibi_fed_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_fed_percent_deprbas_sta")  ? npv( CF_ibi_fed_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_fed_deprbas_sta")  ? npv( CF_cbi_fed, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_sta_amount_deprbas_sta")  ? npv( CF_ibi_sta_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_sta_percent_deprbas_sta")  ? npv( CF_ibi_sta_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_sta_deprbas_sta")  ? npv( CF_cbi_sta, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_uti_amount_deprbas_sta")  ? npv( CF_ibi_uti_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_uti_percent_deprbas_sta")  ? npv( CF_ibi_uti_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_uti_deprbas_sta")  ? npv( CF_cbi_uti, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_oth_amount_deprbas_sta")  ? npv( CF_ibi_oth_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_oth_percent_deprbas_sta")  ? npv( CF_ibi_oth_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_oth_deprbas_sta")  ? npv( CF_cbi_oth, nyears, nom_discount_rate ) : 0 )
			);
// escalate back to year 1 for itc reduction
		depr_sta_reduction *=  (1.0 + nom_discount_rate);

		double depr_stabas_macrs_5;
		double depr_stabas_macrs_15;
		double depr_stabas_sl_5;
		double depr_stabas_sl_15;
		double depr_stabas_sl_20;
		double depr_stabas_sl_39;

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

		double depr_stabas_macrs_5_bonus;
		double depr_stabas_macrs_15_bonus;
		double depr_stabas_sl_5_bonus;
		double depr_stabas_sl_15_bonus;
		double depr_stabas_sl_20_bonus;
		double depr_stabas_sl_39_bonus;

		double depr_stabas_total;

		// Federal depreciation

		double depr_fed_reduction =  (
			( as_boolean("ibi_fed_amount_deprbas_fed")  ? npv( CF_ibi_fed_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_fed_percent_deprbas_fed")  ? npv( CF_ibi_fed_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_fed_deprbas_fed")  ? npv( CF_cbi_fed, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_sta_amount_deprbas_fed")  ? npv( CF_ibi_sta_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_sta_percent_deprbas_fed")  ? npv( CF_ibi_sta_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_sta_deprbas_fed")  ? npv( CF_cbi_sta, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_uti_amount_deprbas_fed")  ? npv( CF_ibi_uti_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_uti_percent_deprbas_fed")  ? npv( CF_ibi_uti_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_uti_deprbas_fed")  ? npv( CF_cbi_uti, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_oth_amount_deprbas_fed")  ? npv( CF_ibi_oth_amt, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("ibi_oth_percent_deprbas_fed")  ? npv( CF_ibi_oth_per, nyears, nom_discount_rate ) : 0 ) +
			( as_boolean("cbi_oth_deprbas_fed")  ? npv( CF_cbi_oth, nyears, nom_discount_rate ) : 0 )
			);
// escalate back to year 1 for itc reduction
		depr_fed_reduction *=  (1.0 + nom_discount_rate);

		double depr_fedbas_macrs_5;
		double depr_fedbas_macrs_15;
		double depr_fedbas_sl_5;
		double depr_fedbas_sl_15;
		double depr_fedbas_sl_20;
		double depr_fedbas_sl_39;

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

		double depr_fedbas_macrs_5_bonus;
		double depr_fedbas_macrs_15_bonus;
		double depr_fedbas_sl_5_bonus;
		double depr_fedbas_sl_15_bonus;
		double depr_fedbas_sl_20_bonus;
		double depr_fedbas_sl_39_bonus;

		double depr_fedbas_total;


		double pbi_fed_for_ds_frac = as_boolean("pbi_fed_for_ds") ? 1.0 : 0.0;
		double pbi_sta_for_ds_frac = as_boolean("pbi_sta_for_ds") ? 1.0 : 0.0;
		double pbi_uti_for_ds_frac = as_boolean("pbi_uti_for_ds") ? 1.0 : 0.0;
		double pbi_oth_for_ds_frac = as_boolean("pbi_oth_for_ds") ? 1.0 : 0.0;


		//		if (ppa_mode == 1) // iterate to meet flip target by varying ppa price
		double ppa_soln_tolerance = as_double("ppa_soln_tolerance");
		int ppa_soln_max_iteations = as_integer("ppa_soln_max_iterations");
		double flip_target_percent = as_double("flip_target_percent") ;
		int flip_target_year = as_integer("flip_target_year");
		int flip_year=-1;
		double purchase_of_property;
		bool solved=true;
		double ppa_min=as_double("ppa_soln_min");
		double ppa_max=as_double("ppa_soln_max");
		int its=0;

/***************** begin iterative solution *********************************************************************/

	do
	{

		cash_for_debt_service=0;
		pv_cafds=0;
		size_of_debt=0;

		// debt pre calculation
		for (i=1; i<=nyears; i++)
		{
		// Project partial income statement
			// energy_value = DHF Total PPA Revenue
			cf.at(CF_ppa_price,i) = ppa * pow( 1 + ppa_escalation, i-1 ); // ppa_mode==0
			cf.at(CF_energy_value,i) = cf.at(CF_energy_net,i) * cf.at(CF_ppa_price,i) /100.0;
			// PBI
			// total revenue
			cf.at(CF_total_revenue,i) = cf.at(CF_energy_value,i) * (1.0 +
				pbi_fed_for_ds_frac * cf.at(CF_pbi_fed,i) +
				pbi_sta_for_ds_frac * cf.at(CF_pbi_sta,i) +
				pbi_uti_for_ds_frac * cf.at(CF_pbi_uti,i) +
				pbi_oth_for_ds_frac * cf.at(CF_pbi_oth,i) );
			// salvage value
			if (i==nyears) cf.at(CF_total_revenue,nyears) += salvage_value;
			// compute expenses

			cf.at(CF_ebitda,i) = cf.at(CF_total_revenue,i) - cf.at(CF_operating_expenses,i);

			// term financing
			if (i<=term_tenor)
			{
				cf.at(CF_cash_for_ds,i) = cf.at(CF_ebitda,i) - cf.at(CF_funding_equip1,i) - cf.at(CF_funding_equip2,i) - cf.at(CF_funding_equip3,i);
				cash_for_debt_service += cf.at(CF_cash_for_ds,i);
				if (i==1)
					cf.at(CF_pv_interest_factor,i) = 1.0/(1.0+term_int_rate);
				else
					cf.at(CF_pv_interest_factor,i) = cf.at(CF_pv_interest_factor,i-1)/(1.0+term_int_rate);
				cf.at(CF_pv_cash_for_ds,i) = cf.at(CF_pv_interest_factor,i) * cf.at(CF_cash_for_ds,i);
				pv_cafds += cf.at(CF_pv_cash_for_ds,i);
				if (dscr!=0) cf.at(CF_debt_size,i) = cf.at(CF_pv_cash_for_ds,i) / dscr;
				size_of_debt += cf.at(CF_debt_size,i);
			}
		}

		cf.at(CF_debt_balance,0) = size_of_debt;

		for (i=1; ((i<=nyears) && (i<=term_tenor)); i++)
		{
			if(dscr!=0) cf.at(CF_debt_payment_total,i) = cf.at(CF_cash_for_ds,i) / dscr;
			cf.at(CF_debt_payment_interest,i) = cf.at(CF_debt_balance,i-1) * term_int_rate;
			cf.at(CF_debt_payment_principal,i) = cf.at(CF_debt_payment_total,i) - cf.at(CF_debt_payment_interest,i);
			cf.at(CF_debt_balance,i) = cf.at(CF_debt_balance,i-1) - cf.at(CF_debt_payment_principal,i);
		}

		// debt service reserve
		for (i=1; ((i<=nyears) && (i<=term_tenor)); i++)
		{
			cf.at(CF_reserve_debtservice,i-1) = dscr_reserve_months/12.0 * (cf.at(CF_debt_payment_principal,i) + cf.at(CF_debt_payment_interest,i));
			cf.at(CF_funding_debtservice,i-1) = cf.at(CF_reserve_debtservice,i-1);
			if (i>1) cf.at(CF_funding_debtservice,i-1) -= cf.at(CF_reserve_debtservice,i-2);
			if (i==term_tenor) cf.at(CF_disbursement_debtservice,i)=0-cf.at(CF_reserve_debtservice,i-1);
		}

		// total reserves
		for (i=0; i<=nyears; i++)
			cf.at(CF_reserve_total,i) =
				cf.at(CF_reserve_debtservice,i) +
				cf.at(CF_reserve_om,i) +
				cf.at(CF_reserve_equip1,i) +
				cf.at(CF_reserve_equip2,i) +
				cf.at(CF_reserve_equip3,i);
		for (i=1; i<=nyears; i++)
			cf.at(CF_reserve_interest,i) = reserves_interest * cf.at(CF_reserve_total,i-1);

		cost_financing =
			cost_dev_fee_percent * cost_prefinancing +
			cost_equity_closing +
			cost_debt_closing +
			cf.at(CF_reserve_debtservice,0) +
			constr_total_financing +
			cost_working_reserve;

		cost_installed = cost_prefinancing + cost_financing;

		depr_alloc_total = depr_alloc_total_frac * cost_installed;
		depr_alloc_macrs_5 = depr_alloc_macrs_5_frac * depr_alloc_total;
		depr_alloc_macrs_15 = depr_alloc_macrs_15_frac * depr_alloc_total;
		depr_alloc_sl_5 = depr_alloc_sl_5_frac * depr_alloc_total;
		depr_alloc_sl_15 = depr_alloc_sl_15_frac * depr_alloc_total;
		depr_alloc_sl_20 = depr_alloc_sl_20_frac * depr_alloc_total;
		depr_alloc_sl_39 = depr_alloc_sl_39_frac * depr_alloc_total;
		depr_alloc_none = depr_alloc_none_frac * depr_alloc_total;

		itc_sta_qual_macrs_5 = itc_sta_qual_macrs_5_frac * (depr_alloc_total - itc_sta_reduction);
		itc_sta_qual_macrs_15 = itc_sta_qual_macrs_15_frac * (depr_alloc_total - itc_sta_reduction);
		itc_sta_qual_sl_5 = itc_sta_qual_sl_5_frac * (depr_alloc_total - itc_sta_reduction);
		itc_sta_qual_sl_15 = itc_sta_qual_sl_15_frac * (depr_alloc_total - itc_sta_reduction);
		itc_sta_qual_sl_20 = itc_sta_qual_sl_20_frac * (depr_alloc_total - itc_sta_reduction);
		itc_sta_qual_sl_39 = itc_sta_qual_sl_39_frac * (depr_alloc_total - itc_sta_reduction);

		itc_sta_qual_total = itc_sta_qual_macrs_5 + itc_sta_qual_macrs_15 + itc_sta_qual_sl_5 +itc_sta_qual_sl_15 +itc_sta_qual_sl_20 + itc_sta_qual_sl_39;

		itc_sta_percent_total = min(itc_sta_percent_maxvalue,itc_sta_percent_total*itc_sta_qual_total);

		itc_disallow_sta_percent_macrs_5 = (itc_sta_disallow_factor*itc_sta_qual_macrs_5_frac * itc_sta_percent_total);
		itc_disallow_sta_percent_macrs_15 = (itc_sta_disallow_factor*itc_sta_qual_macrs_15_frac * itc_sta_percent_total);
		itc_disallow_sta_percent_sl_5 = (itc_sta_disallow_factor*itc_sta_qual_sl_5_frac * itc_sta_percent_total);
		itc_disallow_sta_percent_sl_15 = (itc_sta_disallow_factor*itc_sta_qual_sl_15_frac * itc_sta_percent_total);
		itc_disallow_sta_percent_sl_20 = (itc_sta_disallow_factor*itc_sta_qual_sl_20_frac * itc_sta_percent_total);
		itc_disallow_sta_percent_sl_39 = (itc_sta_disallow_factor*itc_sta_qual_sl_39_frac * itc_sta_percent_total);

		itc_fed_qual_macrs_5 = itc_fed_qual_macrs_5_frac * (depr_alloc_total - itc_fed_reduction);
		itc_fed_qual_macrs_15 = itc_fed_qual_macrs_15_frac * (depr_alloc_total - itc_fed_reduction);
		itc_fed_qual_sl_5 = itc_fed_qual_sl_5_frac * (depr_alloc_total - itc_fed_reduction);
		itc_fed_qual_sl_15 = itc_fed_qual_sl_15_frac * (depr_alloc_total - itc_fed_reduction);
		itc_fed_qual_sl_20 = itc_fed_qual_sl_20_frac * (depr_alloc_total - itc_fed_reduction);
		itc_fed_qual_sl_39 = itc_fed_qual_sl_39_frac * (depr_alloc_total - itc_fed_reduction);

		itc_fed_qual_total = itc_fed_qual_macrs_5 + itc_fed_qual_macrs_15 + itc_fed_qual_sl_5 +itc_fed_qual_sl_15 +itc_fed_qual_sl_20 + itc_fed_qual_sl_39;

		itc_fed_percent_total = min(itc_fed_percent_maxvalue,itc_fed_percent_total*itc_fed_qual_total);

		itc_disallow_fed_percent_macrs_5 = (itc_fed_disallow_factor*itc_fed_qual_macrs_5_frac * itc_fed_percent_total);
		itc_disallow_fed_percent_macrs_15 = (itc_fed_disallow_factor*itc_fed_qual_macrs_15_frac * itc_fed_percent_total);
		itc_disallow_fed_percent_sl_5 = (itc_fed_disallow_factor*itc_fed_qual_sl_5_frac * itc_fed_percent_total);
		itc_disallow_fed_percent_sl_15 = (itc_fed_disallow_factor*itc_fed_qual_sl_15_frac * itc_fed_percent_total);
		itc_disallow_fed_percent_sl_20 = (itc_fed_disallow_factor*itc_fed_qual_sl_20_frac * itc_fed_percent_total);
		itc_disallow_fed_percent_sl_39 = (itc_fed_disallow_factor*itc_fed_qual_sl_39_frac * itc_fed_percent_total);


// Depreciaiton
// State depreciation
		depr_stabas_macrs_5 = depr_alloc_macrs_5 - depr_stabas_macrs_5_frac * depr_sta_reduction;
		depr_stabas_macrs_15 = depr_alloc_macrs_15 - depr_stabas_macrs_15_frac * depr_sta_reduction;
		depr_stabas_sl_5 = depr_alloc_sl_5 - depr_stabas_sl_5_frac * depr_sta_reduction;
		depr_stabas_sl_15 = depr_alloc_sl_15 - depr_stabas_sl_15_frac * depr_sta_reduction;
		depr_stabas_sl_20 = depr_alloc_sl_20 - depr_stabas_sl_20_frac * depr_sta_reduction;
		depr_stabas_sl_39 = depr_alloc_sl_39 - depr_stabas_sl_39_frac * depr_sta_reduction;

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

		// Bonus depreciation
		depr_stabas_macrs_5_bonus = depr_stabas_macrs_5_bonus_frac * depr_stabas_macrs_5;
		depr_stabas_macrs_15_bonus = depr_stabas_macrs_15_bonus_frac * depr_stabas_macrs_15;
		depr_stabas_sl_5_bonus = depr_stabas_sl_5_bonus_frac * depr_stabas_sl_5;
		depr_stabas_sl_15_bonus = depr_stabas_sl_15_bonus_frac * depr_stabas_sl_15;
		depr_stabas_sl_20_bonus = depr_stabas_sl_20_bonus_frac * depr_stabas_sl_20;
		depr_stabas_sl_39_bonus = depr_stabas_sl_39_bonus_frac * depr_stabas_sl_39;

		depr_stabas_macrs_5 -= depr_stabas_macrs_5_bonus;
		depr_stabas_macrs_15 -= depr_stabas_macrs_15_bonus;
		depr_stabas_sl_5 -= depr_stabas_sl_5_bonus;
		depr_stabas_sl_15 -= depr_stabas_sl_15_bonus;
		depr_stabas_sl_20 -= depr_stabas_sl_20_bonus;
		depr_stabas_sl_39 -= depr_stabas_sl_39_bonus;

		depr_stabas_total = depr_stabas_macrs_5 + depr_stabas_macrs_15 + depr_stabas_sl_5 + depr_stabas_sl_15 + depr_stabas_sl_20 + depr_stabas_sl_39;

		// Federal depreciation
		depr_fedbas_macrs_5 = depr_alloc_macrs_5 - depr_fedbas_macrs_5_frac * depr_fed_reduction;
		depr_fedbas_macrs_15 = depr_alloc_macrs_15 - depr_fedbas_macrs_15_frac * depr_fed_reduction;
		depr_fedbas_sl_5 = depr_alloc_sl_5 - depr_fedbas_sl_5_frac * depr_fed_reduction;
		depr_fedbas_sl_15 = depr_alloc_sl_15 - depr_fedbas_sl_15_frac * depr_fed_reduction;
		depr_fedbas_sl_20 = depr_alloc_sl_20 - depr_fedbas_sl_20_frac * depr_fed_reduction;
		depr_fedbas_sl_39 = depr_alloc_sl_39 - depr_fedbas_sl_39_frac * depr_fed_reduction;

		// ITC reduction
		depr_fedbas_macrs_5 -= (itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_macrs_5 +
								itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_macrs_5 +
								itc_sta_percent_deprbas_fed * itc_disallow_fed_percent_macrs_5 +
								itc_sta_amount_deprbas_fed * itc_disallow_fed_fixed_macrs_5 );

		depr_fedbas_macrs_15 -= (itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_macrs_15 +
								itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_macrs_15 +
								itc_sta_percent_deprbas_fed * itc_disallow_fed_percent_macrs_15 +
								itc_sta_amount_deprbas_fed * itc_disallow_fed_fixed_macrs_15 );

		depr_fedbas_sl_5 -= (itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_sl_5 +
								itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_sl_5 +
								itc_sta_percent_deprbas_fed * itc_disallow_fed_percent_sl_5 +
								itc_sta_amount_deprbas_fed * itc_disallow_fed_fixed_sl_5 );

		depr_fedbas_sl_15 -= (itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_sl_15 +
								itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_sl_15 +
								itc_sta_percent_deprbas_fed * itc_disallow_fed_percent_sl_15 +
								itc_sta_amount_deprbas_fed * itc_disallow_fed_fixed_sl_15 );

		depr_fedbas_sl_20 -= (itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_sl_20 +
								itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_sl_20 +
								itc_sta_percent_deprbas_fed * itc_disallow_fed_percent_sl_20 +
								itc_sta_amount_deprbas_fed * itc_disallow_fed_fixed_sl_20 );

		depr_fedbas_sl_39 -= (itc_fed_percent_deprbas_fed * itc_disallow_fed_percent_sl_39 +
								itc_fed_amount_deprbas_fed * itc_disallow_fed_fixed_sl_39 +
								itc_sta_percent_deprbas_fed * itc_disallow_fed_percent_sl_39 +
								itc_sta_amount_deprbas_fed * itc_disallow_fed_fixed_sl_39 );

		// Bonus depreciation
		depr_fedbas_macrs_5_bonus = depr_fedbas_macrs_5_bonus_frac * depr_fedbas_macrs_5;
		depr_fedbas_macrs_15_bonus = depr_fedbas_macrs_15_bonus_frac * depr_fedbas_macrs_15;
		depr_fedbas_sl_5_bonus = depr_fedbas_sl_5_bonus_frac * depr_fedbas_sl_5;
		depr_fedbas_sl_15_bonus = depr_fedbas_sl_15_bonus_frac * depr_fedbas_sl_15;
		depr_fedbas_sl_20_bonus = depr_fedbas_sl_20_bonus_frac * depr_fedbas_sl_20;
		depr_fedbas_sl_39_bonus = depr_fedbas_sl_39_bonus_frac * depr_fedbas_sl_39;

		depr_fedbas_macrs_5 -= depr_fedbas_macrs_5_bonus;
		depr_fedbas_macrs_15 -= depr_fedbas_macrs_15_bonus;
		depr_fedbas_sl_5 -= depr_fedbas_sl_5_bonus;
		depr_fedbas_sl_15 -= depr_fedbas_sl_15_bonus;
		depr_fedbas_sl_20 -= depr_fedbas_sl_20_bonus;
		depr_fedbas_sl_39 -= depr_fedbas_sl_39_bonus;

		depr_fedbas_total = depr_fedbas_macrs_5 + depr_fedbas_macrs_15 + depr_fedbas_sl_5 + depr_fedbas_sl_15 + depr_fedbas_sl_20 + depr_fedbas_sl_39;

		purchase_of_property = -cost_installed + cf.at(CF_reserve_debtservice,0) + cost_working_reserve;
		// TODO - check with incentives - need total ibi and cbi - can use npv at year 1 - note that itc at year 1 but cbi and ibi at year 0
		issuance_of_equity = cost_installed - (size_of_debt + cf.at(CF_ibi_total,1) + cf.at(CF_cbi_total,1));

		equity_tax_investor = tax_investor_equity_frac * issuance_of_equity;

		sponsor_pretax_equity_investment = -issuance_of_equity * (1.0 - tax_investor_equity_frac);

		for (i=0; i<=nyears; i++)
		{
			cf.at(CF_project_operating_activities,i) = cf.at(CF_ebitda,i) + cf.at(CF_pbi_total,i) + cf.at(CF_reserve_interest,i) - cf.at(CF_debt_payment_interest,i);
			cf.at(CF_project_dsra,i) = -cf.at(CF_funding_debtservice,i) - cf.at(CF_disbursement_debtservice,i);
			cf.at(CF_project_ra,i) =
				cf.at(CF_project_dsra,i) +
				cf.at(CF_project_wcra,i) +
				cf.at(CF_project_me1ra,i) +
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

			// TODO - address out IBI CBI at year 1 and DHF has at year 0
//			cf.at(CF_project_financing_activities,i) = cf.at(CF_ibi_total,i) + cf.at(CF_cbi_total,i) - cf.at(CF_debt_payment_principal,i);
			cf.at(CF_project_financing_activities,i) = -cf.at(CF_debt_payment_principal,i);
			if (i==0) cf.at(CF_project_financing_activities,i) += issuance_of_equity + size_of_debt + + cf.at(CF_ibi_total,1) + cf.at(CF_cbi_total,1);

			cf.at(CF_pretax_cashflow,i) = cf.at(CF_project_operating_activities,i) + cf.at(CF_project_investing_activities,i) + cf.at(CF_project_financing_activities,i);

			cf.at(CF_project_return_pretax,i) = cf.at(CF_pretax_cashflow,i);
			if (i==0) cf.at(CF_project_return_pretax,i) -= (issuance_of_equity);

			cf.at(CF_project_return_pretax_irr,i) = irr(CF_project_return_pretax,i)*100.0;
			cf.at(CF_project_return_pretax_npv,i) = npv(CF_project_return_pretax,i,nom_discount_rate) +  cf.at(CF_project_return_pretax,0) ;

			cf.at(CF_project_return_aftertax_cash,i) = cf.at(CF_project_return_pretax,i);
		}


		cf.at(CF_project_return_aftertax,0) = cf.at(CF_project_return_aftertax_cash,0);

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_stadepr_macrs_5,i) = cf.at(CF_macrs_5_frac,i) * depr_stabas_macrs_5;
			cf.at(CF_stadepr_macrs_15,i) = cf.at(CF_macrs_15_frac,i) * depr_stabas_macrs_15;
			cf.at(CF_stadepr_sl_5,i) = cf.at(CF_sl_5_frac,i) * depr_stabas_sl_5;
			cf.at(CF_stadepr_sl_15,i) = cf.at(CF_sl_15_frac,i) * depr_stabas_sl_15;
			cf.at(CF_stadepr_sl_20,i) = cf.at(CF_sl_20_frac,i) * depr_stabas_sl_20;
			cf.at(CF_stadepr_sl_39,i) = cf.at(CF_sl_39_frac,i) * depr_stabas_sl_39;


			cf.at(CF_stadepr_total,i)=
				cf.at(CF_stadepr_macrs_5,i)+
				cf.at(CF_stadepr_macrs_15,i)+
				cf.at(CF_stadepr_sl_5,i)+
				cf.at(CF_stadepr_sl_15,i)+
				cf.at(CF_stadepr_sl_20,i)+
				cf.at(CF_stadepr_sl_39,i)+
				cf.at(CF_stadepr_me1,i)+
				cf.at(CF_stadepr_me2,i)+
				cf.at(CF_stadepr_me3,i);

			if (i==1) cf.at(CF_stadepr_total,i) += ( depr_stabas_macrs_5_bonus +depr_stabas_macrs_15_bonus + depr_stabas_sl_5_bonus + depr_stabas_sl_15_bonus + depr_stabas_sl_20_bonus + depr_stabas_sl_39_bonus);
			cf.at(CF_statax_income_prior_incentives,i)=
				cf.at(CF_ebitda,i) +
				cf.at(CF_reserve_interest,i) -
				cf.at(CF_debt_payment_interest,i) -
				cf.at(CF_stadepr_total,i);



			cf.at(CF_statax_income_with_incentives,i) = cf.at(CF_statax_income_prior_incentives,i) + cf.at(CF_statax_taxable_incentives,i);
			cf.at(CF_statax,i) = -state_tax_rate * cf.at(CF_statax_income_with_incentives,i);

// federal
			cf.at(CF_feddepr_macrs_5,i) = cf.at(CF_macrs_5_frac,i) * depr_fedbas_macrs_5;
			cf.at(CF_feddepr_macrs_15,i) = cf.at(CF_macrs_15_frac,i) * depr_fedbas_macrs_15;
			cf.at(CF_feddepr_sl_5,i) = cf.at(CF_sl_5_frac,i) * depr_fedbas_sl_5;
			cf.at(CF_feddepr_sl_15,i) = cf.at(CF_sl_15_frac,i) * depr_fedbas_sl_15;
			cf.at(CF_feddepr_sl_20,i) = cf.at(CF_sl_20_frac,i) * depr_fedbas_sl_20;
			cf.at(CF_feddepr_sl_39,i) = cf.at(CF_sl_39_frac,i) * depr_fedbas_sl_39;
			cf.at(CF_feddepr_total,i)=
				cf.at(CF_feddepr_macrs_5,i)+
				cf.at(CF_feddepr_macrs_15,i)+
				cf.at(CF_feddepr_sl_5,i)+
				cf.at(CF_feddepr_sl_15,i)+
				cf.at(CF_feddepr_sl_20,i)+
				cf.at(CF_feddepr_sl_39,i)+
				cf.at(CF_feddepr_me1,i)+
				cf.at(CF_feddepr_me2,i)+
				cf.at(CF_feddepr_me3,i);
			if (i==1) cf.at(CF_feddepr_total,i) += ( depr_fedbas_macrs_5_bonus +depr_fedbas_macrs_15_bonus + depr_fedbas_sl_5_bonus + depr_fedbas_sl_15_bonus + depr_fedbas_sl_20_bonus + depr_fedbas_sl_39_bonus);
			cf.at(CF_fedtax_income_prior_incentives,i)=
				cf.at(CF_ebitda,i) +
				cf.at(CF_reserve_interest,i) -
				cf.at(CF_debt_payment_interest,i) -
				cf.at(CF_feddepr_total,i) +
				cf.at(CF_statax,i) +
				cf.at(CF_itc_sta_total,i) +
				cf.at(CF_ptc_sta,i);


			cf.at(CF_fedtax_income_with_incentives,i) = cf.at(CF_fedtax_income_prior_incentives,i) + cf.at(CF_fedtax_taxable_incentives,i);
			cf.at(CF_fedtax,i) = -federal_tax_rate * cf.at(CF_fedtax_income_with_incentives,i);

			cf.at(CF_cbi_total,i) = cf.at(CF_cbi_fed,i) + cf.at(CF_cbi_sta,i) + cf.at(CF_cbi_uti,i) + cf.at(CF_cbi_oth,i);
			cf.at(CF_ibi_total,i) = cf.at(CF_ibi_fed_amt,i) + cf.at(CF_ibi_sta_amt,i) + cf.at(CF_ibi_uti_amt,i) + cf.at(CF_ibi_oth_amt,i) + cf.at(CF_ibi_fed_per,i) + cf.at(CF_ibi_sta_per,i) + cf.at(CF_ibi_uti_per,i) + cf.at(CF_ibi_oth_per,i);

			cf.at(CF_itc_fed_total,i) = cf.at(CF_itc_fed_amt,i) + cf.at(CF_itc_fed_per,i)*itc_fed_qual_total;
			cf.at(CF_itc_sta_total,i) = cf.at(CF_itc_sta_amt,i) + cf.at(CF_itc_sta_per,i)*itc_sta_qual_total;
			cf.at(CF_itc_total,i) = cf.at(CF_itc_fed_total,i) + cf.at(CF_itc_sta_total,i);

			cf.at(CF_project_return_aftertax,i) =
				cf.at(CF_project_return_aftertax_cash,i) +
				cf.at(CF_itc_total,i) +
				cf.at(CF_ptc_fed,i) + cf.at(CF_ptc_sta,i) +
				cf.at(CF_statax,i) + cf.at(CF_fedtax,i);

			cf.at(CF_project_return_aftertax_irr,i) = irr(CF_project_return_aftertax,i,0.05)*100.0;
			cf.at(CF_project_return_aftertax_npv,i) = npv(CF_project_return_aftertax,i,nom_discount_rate) +  cf.at(CF_project_return_aftertax,0) ;

		}
		cf.at(CF_project_return_aftertax_npv,0) = cf.at(CF_project_return_aftertax,0) ;

// partner returns

		cf.at(CF_tax_investor_aftertax_cash,0) = -equity_tax_investor;
		cf.at(CF_tax_investor_aftertax_itc,0) = 0;
		cf.at(CF_tax_investor_aftertax_ptc,0) = 0;
		cf.at(CF_tax_investor_aftertax_tax,0) = 0;
		cf.at(CF_tax_investor_aftertax,0) =
			cf.at(CF_tax_investor_aftertax_cash,0) +
			cf.at(CF_tax_investor_aftertax_itc,0) +
			cf.at(CF_tax_investor_aftertax_ptc,0) +
			cf.at(CF_tax_investor_aftertax_tax,0);

		cf.at(CF_tax_investor_aftertax_irr,0) = irr(CF_tax_investor_aftertax_tax,0)*100.0;
		cf.at(CF_tax_investor_aftertax_max_irr,0) = cf.at(CF_tax_investor_aftertax_irr,0);
		cf.at(CF_tax_investor_aftertax_npv,0) = cf.at(CF_tax_investor_aftertax,0) ;


		cf.at(CF_sponsor_aftertax_cash,0) = sponsor_pretax_equity_investment + sponsor_pretax_development_fee;
		cf.at(CF_sponsor_aftertax,0) = cf.at(CF_sponsor_aftertax_cash,0);
		cf.at(CF_sponsor_pretax_irr,0) = irr(CF_sponsor_aftertax_tax,0)*100.0;
		cf.at(CF_sponsor_pretax_npv,0) = cf.at(CF_sponsor_aftertax,0) ;
		cf.at(CF_sponsor_aftertax_irr,0) = irr(CF_sponsor_aftertax_tax,0)*100.0;
		cf.at(CF_sponsor_aftertax_npv,0) = cf.at(CF_sponsor_aftertax,0) ;

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_tax_investor_aftertax_cash,i) = ((cf.at(CF_tax_investor_aftertax_max_irr,i-1) < flip_target_percent) ?
						tax_investor_preflip_cash_frac : tax_investor_postflip_cash_frac) * cf.at(CF_project_return_aftertax_cash,i);
			cf.at(CF_tax_investor_aftertax_itc,i) =  ((cf.at(CF_tax_investor_aftertax_max_irr,i-1) < flip_target_percent) ?
						tax_investor_preflip_tax_frac : tax_investor_postflip_tax_frac) * cf.at(CF_itc_total,i);
			cf.at(CF_tax_investor_aftertax_ptc,i) =  ((cf.at(CF_tax_investor_aftertax_max_irr,i-1) < flip_target_percent) ?
						tax_investor_preflip_tax_frac : tax_investor_postflip_tax_frac) * (cf.at(CF_ptc_fed,i) + cf.at(CF_ptc_sta,i));
			cf.at(CF_tax_investor_aftertax_tax,i) =  ((cf.at(CF_tax_investor_aftertax_max_irr,i-1) < flip_target_percent) ?
						tax_investor_preflip_tax_frac : tax_investor_postflip_tax_frac) * (cf.at(CF_statax,i) + cf.at(CF_fedtax,i));
			cf.at(CF_tax_investor_aftertax,i) =
				cf.at(CF_tax_investor_aftertax_cash,i) +
				cf.at(CF_tax_investor_aftertax_itc,i) +
				cf.at(CF_tax_investor_aftertax_ptc,i) +
				cf.at(CF_tax_investor_aftertax_tax,i);
			cf.at(CF_tax_investor_aftertax_irr,i) = irr(CF_tax_investor_aftertax,i)*100.0;
			cf.at(CF_tax_investor_aftertax_max_irr,i) = max(cf.at(CF_tax_investor_aftertax_max_irr,i-1),cf.at(CF_tax_investor_aftertax_irr,i));
			cf.at(CF_tax_investor_aftertax_npv,i) = npv(CF_tax_investor_aftertax,i,nom_discount_rate) +  cf.at(CF_tax_investor_aftertax,0) ;

			if (flip_year <=0)
				if ( ( cf.at(CF_tax_investor_aftertax_max_irr,i-1) < flip_target_percent ) && ( cf.at(CF_tax_investor_aftertax_max_irr,i) >= flip_target_percent ) ) flip_year=i;


			cf.at(CF_sponsor_aftertax_cash,i) = cf.at(CF_project_return_aftertax_cash,i) - cf.at(CF_tax_investor_aftertax_cash,i);
			cf.at(CF_sponsor_pretax,i) = cf.at(CF_sponsor_aftertax_cash,i);
			cf.at(CF_sponsor_aftertax_itc,i) = cf.at(CF_itc_total,i) - cf.at(CF_tax_investor_aftertax_itc,i);
			cf.at(CF_sponsor_aftertax_ptc,i) = (cf.at(CF_ptc_fed,i) + cf.at(CF_ptc_sta,i)) - cf.at(CF_tax_investor_aftertax_ptc,i);
			cf.at(CF_sponsor_aftertax_tax,i) = (cf.at(CF_statax,i) + cf.at(CF_fedtax,i)) - cf.at(CF_tax_investor_aftertax_tax,i);
			cf.at(CF_sponsor_aftertax,i) =
				cf.at(CF_sponsor_aftertax_cash,i) +
				cf.at(CF_sponsor_aftertax_itc,i) +
				cf.at(CF_sponsor_aftertax_ptc,i) +
				cf.at(CF_sponsor_aftertax_tax,i);
			// year 1 development fee tax
			if (i==1) cf.at(CF_sponsor_aftertax,i) -= sponsor_pretax_development_fee * (state_tax_rate + federal_tax_rate * (1.0 - state_tax_rate));
			cf.at(CF_sponsor_pretax_irr,i) = irr(CF_sponsor_pretax,i)*100.0;
			cf.at(CF_sponsor_pretax_npv,i) = npv(CF_sponsor_pretax,i,nom_discount_rate) +  cf.at(CF_sponsor_pretax,0) ;
			cf.at(CF_sponsor_aftertax_irr,i) = irr(CF_sponsor_aftertax,i)*100.0;
			cf.at(CF_sponsor_aftertax_npv,i) = npv(CF_sponsor_aftertax,i,nom_discount_rate) +  cf.at(CF_sponsor_aftertax,0) ;

		}

		if (ppa_mode == 1)
		{
			double residual = cf.at(CF_tax_investor_aftertax_irr, flip_target_year) - flip_target_percent;
			solved = (( fabs( residual ) < ppa_soln_tolerance ) || ( fabs(ppa_min-ppa_max) < ppa_soln_tolerance) );
			if (!solved)
			{
				double itnpv = cf.at(CF_tax_investor_aftertax_npv, flip_target_year);
				if (cf.at(CF_tax_investor_aftertax_irr, flip_target_year) > 0) // use residual
				{
					if (residual < 0)
						ppa_min = ppa;
					else
						ppa_max = ppa;
				}
				else // use npv
				{
					if (itnpv < 0)
						ppa_min = ppa;
					else
						ppa_max = ppa;
				}
				ppa = 0.5 * (ppa_min + ppa_max);
				std::stringstream outm;
				outm << "iteration=" << its << ", npv=" << itnpv  << ", residual=" << residual << ", ppa=" << ppa << ", ppa_min=" << ppa_min << ", ppamax=" << ppa_max << ", ppamax-ppamin=" << ppa_max-ppa_min;
				log( outm.str() );
			}
		}
		its++;

	}	// target tax investor return in target year
	while ( !solved  && (its < ppa_soln_max_iteations) );

/***************** end iterative solution *********************************************************************/

		// LCOE
		double npv_revenue = npv(CF_energy_value,nyears,nom_discount_rate);
		double lcoe_nom = npv_revenue / npv(CF_energy_net,nyears,nom_discount_rate) * 100.0;
		double lcoe_real = npv_revenue / npv(CF_energy_net,nyears,disc_real) * 100.0;

		assign( "cf_length", var_data( (ssc_number_t) nyears+1 ));

		assign( "salvage_value", var_data((ssc_number_t)salvage_value));

		assign( "prop_tax_assessed_value", var_data((ssc_number_t)( assessed_frac * cost_prefinancing )));

		assign( "cost_prefinancing", var_data((ssc_number_t) cost_prefinancing ) );
		assign( "cost_prefinancingperwatt", var_data((ssc_number_t)( cost_prefinancing / nameplate / 1000.0 ) ));

		assign( "cost_prefinancing", var_data((ssc_number_t) cost_prefinancing ) );
		assign( "cost_prefinancingperwatt", var_data((ssc_number_t)( cost_prefinancing / nameplate / 1000.0 ) ));

		assign( "cost_contingency", var_data((ssc_number_t) cost_cont ) );
		assign( "cost_hard", var_data( (ssc_number_t)(cost_hard + cost_cont)) );
		assign( "cost_salestax", var_data((ssc_number_t)cost_salestax ) );
		assign( "cost_soft", var_data((ssc_number_t) (cost_soft + cost_salestax) ) );
		assign( "nominal_discount_rate", var_data((ssc_number_t)nom_discount_rate ) );

		assign( "depr_stabas_macrs_5", var_data((ssc_number_t) depr_stabas_macrs_5 ) );
		assign( "depr_stabas_macrs_15", var_data((ssc_number_t) depr_stabas_macrs_15 ) );
		assign( "depr_stabas_sl_5", var_data((ssc_number_t) depr_stabas_sl_5 ) );
		assign( "depr_stabas_sl_15", var_data((ssc_number_t) depr_stabas_sl_15 ) );
		assign( "depr_stabas_sl_20", var_data((ssc_number_t) depr_stabas_sl_20 ) );
		assign( "depr_stabas_sl_39", var_data((ssc_number_t) depr_stabas_sl_39 ) );
		assign( "depr_stabas_total", var_data((ssc_number_t) depr_stabas_total ) );

		assign( "depr_fedbas_macrs_5", var_data((ssc_number_t) depr_fedbas_macrs_5 ) );
		assign( "depr_fedbas_macrs_15", var_data((ssc_number_t) depr_fedbas_macrs_15 ) );
		assign( "depr_fedbas_sl_5", var_data((ssc_number_t) depr_fedbas_sl_5 ) );
		assign( "depr_fedbas_sl_15", var_data((ssc_number_t) depr_fedbas_sl_15 ) );
		assign( "depr_fedbas_sl_20", var_data((ssc_number_t) depr_fedbas_sl_20 ) );
		assign( "depr_fedbas_sl_39", var_data((ssc_number_t) depr_fedbas_sl_39 ) );
		assign( "depr_fedbas_total", var_data((ssc_number_t) depr_fedbas_total ) );


		assign("lcoe_nom", var_data((ssc_number_t) lcoe_nom));
		assign("lcoe_real", var_data((ssc_number_t) lcoe_real));

		assign("cost_financing", var_data((ssc_number_t) cost_financing));

		assign( "cost_installed", var_data((ssc_number_t) cost_installed ) );
		assign( "cost_installedperwatt", var_data((ssc_number_t)( cost_installed / nameplate / 1000.0 ) ));

		assign( "itc_fed_reduction", var_data((ssc_number_t) itc_fed_reduction ) );
 		assign( "itc_fed_qual_macrs_5", var_data((ssc_number_t) itc_fed_qual_macrs_5 ) );
		assign( "itc_fed_qual_macrs_15", var_data((ssc_number_t) itc_fed_qual_macrs_15 ) );
		assign( "itc_fed_qual_sl_5", var_data((ssc_number_t) itc_fed_qual_sl_5 ) );
		assign( "itc_fed_qual_sl_15", var_data((ssc_number_t) itc_fed_qual_sl_15 ) );
		assign( "itc_fed_qual_sl_20", var_data((ssc_number_t) itc_fed_qual_sl_20 ) );
		assign( "itc_fed_qual_sl_39", var_data((ssc_number_t) itc_fed_qual_sl_39 ) );

		assign( "itc_disallow_fed_percent_macrs_5", var_data((ssc_number_t) itc_disallow_fed_percent_macrs_5 ) );
		assign( "itc_disallow_fed_percent_macrs_15", var_data((ssc_number_t) itc_disallow_fed_percent_macrs_15 ) );
		assign( "itc_disallow_fed_percent_sl_5", var_data((ssc_number_t) itc_disallow_fed_percent_sl_5 ) );
		assign( "itc_disallow_fed_percent_sl_15", var_data((ssc_number_t) itc_disallow_fed_percent_sl_15 ) );
		assign( "itc_disallow_fed_percent_sl_20", var_data((ssc_number_t) itc_disallow_fed_percent_sl_20 ) );
		assign( "itc_disallow_fed_percent_sl_39", var_data((ssc_number_t) itc_disallow_fed_percent_sl_39 ) );

		assign( "itc_disallow_fed_fixed_macrs_5", var_data((ssc_number_t) itc_disallow_fed_fixed_macrs_5 ) );
		assign( "itc_disallow_fed_fixed_macrs_15", var_data((ssc_number_t) itc_disallow_fed_fixed_macrs_15 ) );
		assign( "itc_disallow_fed_fixed_sl_5", var_data((ssc_number_t) itc_disallow_fed_fixed_sl_5 ) );
		assign( "itc_disallow_fed_fixed_sl_15", var_data((ssc_number_t) itc_disallow_fed_fixed_sl_15 ) );
		assign( "itc_disallow_fed_fixed_sl_20", var_data((ssc_number_t) itc_disallow_fed_fixed_sl_20 ) );
		assign( "itc_disallow_fed_fixed_sl_39", var_data((ssc_number_t) itc_disallow_fed_fixed_sl_39 ) );

		assign( "itc_fed_qual_total", var_data((ssc_number_t) itc_fed_qual_total ) );
		assign( "itc_fed_percent_total", var_data((ssc_number_t) itc_fed_percent_total ) );
		assign( "itc_fed_fixed_total", var_data((ssc_number_t) itc_fed_fixed_total ) );


	    assign("sv_sponsor_pretax_equity", var_data((ssc_number_t) sponsor_pretax_equity_investment));
		assign("sv_sponsor_pretax_development", var_data((ssc_number_t) sponsor_pretax_development_fee));
		assign("sv_sponsor_aftertax_equity", var_data((ssc_number_t) sponsor_pretax_equity_investment));
		assign("sv_sponsor_aftertax_development", var_data((ssc_number_t) sponsor_pretax_development_fee));

		assign( "depr_alloc_macrs_5", var_data((ssc_number_t) depr_alloc_macrs_5 ) );
		assign( "depr_alloc_macrs_15", var_data((ssc_number_t) depr_alloc_macrs_15 ) );
		assign( "depr_alloc_sl_5", var_data((ssc_number_t) depr_alloc_sl_5 ) );
		assign( "depr_alloc_sl_15", var_data((ssc_number_t) depr_alloc_sl_15 ) );
		assign( "depr_alloc_sl_20", var_data((ssc_number_t) depr_alloc_sl_20 ) );
		assign( "depr_alloc_sl_39", var_data((ssc_number_t) depr_alloc_sl_39 ) );

		assign( "depr_alloc_none_percent", var_data((ssc_number_t) (depr_alloc_none_frac*100.0) ) );
		assign( "depr_alloc_none", var_data((ssc_number_t) depr_alloc_none ) );
		assign( "depr_alloc_total", var_data((ssc_number_t) depr_alloc_total ) );
		// Project cash flow
		assign("constr_interest", var_data((ssc_number_t) constr_interest));
		assign("constr_upfront_fee", var_data((ssc_number_t) constr_upfront_fee));
		assign("constr_total_financing", var_data((ssc_number_t) constr_total_financing));

 		assign( "itc_sta_reduction", var_data((ssc_number_t) itc_sta_reduction ) );
 		assign( "itc_sta_qual_macrs_5", var_data((ssc_number_t) itc_sta_qual_macrs_5 ) );
		assign( "itc_sta_qual_macrs_15", var_data((ssc_number_t) itc_sta_qual_macrs_15 ) );
		assign( "itc_sta_qual_sl_5", var_data((ssc_number_t) itc_sta_qual_sl_5 ) );
		assign( "itc_sta_qual_sl_15", var_data((ssc_number_t) itc_sta_qual_sl_15 ) );
		assign( "itc_sta_qual_sl_20", var_data((ssc_number_t) itc_sta_qual_sl_20 ) );
		assign( "itc_sta_qual_sl_39", var_data((ssc_number_t) itc_sta_qual_sl_39 ) );

		assign( "itc_disallow_sta_percent_macrs_5", var_data((ssc_number_t) itc_disallow_sta_percent_macrs_5 ) );
		assign( "itc_disallow_sta_percent_macrs_15", var_data((ssc_number_t) itc_disallow_sta_percent_macrs_15 ) );
		assign( "itc_disallow_sta_percent_sl_5", var_data((ssc_number_t) itc_disallow_sta_percent_sl_5 ) );
		assign( "itc_disallow_sta_percent_sl_15", var_data((ssc_number_t) itc_disallow_sta_percent_sl_15 ) );
		assign( "itc_disallow_sta_percent_sl_20", var_data((ssc_number_t) itc_disallow_sta_percent_sl_20 ) );
		assign( "itc_disallow_sta_percent_sl_39", var_data((ssc_number_t) itc_disallow_sta_percent_sl_39 ) );

		assign( "itc_disallow_sta_fixed_macrs_5", var_data((ssc_number_t) itc_disallow_sta_fixed_macrs_5 ) );
		assign( "itc_disallow_sta_fixed_macrs_15", var_data((ssc_number_t) itc_disallow_sta_fixed_macrs_15 ) );
		assign( "itc_disallow_sta_fixed_sl_5", var_data((ssc_number_t) itc_disallow_sta_fixed_sl_5 ) );
		assign( "itc_disallow_sta_fixed_sl_15", var_data((ssc_number_t) itc_disallow_sta_fixed_sl_15 ) );
		assign( "itc_disallow_sta_fixed_sl_20", var_data((ssc_number_t) itc_disallow_sta_fixed_sl_20 ) );
		assign( "itc_disallow_sta_fixed_sl_39", var_data((ssc_number_t) itc_disallow_sta_fixed_sl_39 ) );

		assign( "itc_sta_qual_total", var_data((ssc_number_t) itc_sta_qual_total ) );
		assign( "itc_sta_percent_total", var_data((ssc_number_t) itc_sta_percent_total ) );
		assign( "itc_sta_fixed_total", var_data((ssc_number_t) itc_sta_fixed_total ) );



		// output variable and cashflow line item assignments

		assign("issuance_of_equity", var_data((ssc_number_t) issuance_of_equity));
		assign("purchase_of_property", var_data((ssc_number_t) purchase_of_property));
		assign("cash_for_debt_service", var_data((ssc_number_t) cash_for_debt_service));
		assign("pv_cafds", var_data((ssc_number_t) pv_cafds));
		assign("size_of_debt", var_data((ssc_number_t) size_of_debt));

		assign("ppa_price", var_data((ssc_number_t) ppa));
		assign("target_return_flip_year", var_data((ssc_number_t) flip_year));

		assign("sv_sponsor_pretax_irr", var_data((ssc_number_t)  (irr(CF_sponsor_pretax,nyears)*100.0)));
		assign("sv_sponsor_pretax_npv", var_data((ssc_number_t)  (npv(CF_sponsor_pretax,nyears,nom_discount_rate) +  cf.at(CF_sponsor_pretax,0)) ));
		assign("sv_sponsor_aftertax_irr", var_data((ssc_number_t)  (irr(CF_sponsor_aftertax,nyears)*100.0)));
		assign("sv_sponsor_aftertax_npv", var_data((ssc_number_t)  (npv(CF_sponsor_aftertax,nyears,nom_discount_rate) +  cf.at(CF_sponsor_aftertax,0)) ));


		// cash flow line items

		save_cf( CF_sponsor_pretax, nyears, "cf_sponsor_pretax" );
		save_cf( CF_sponsor_pretax_irr, nyears, "cf_sponsor_pretax_irr" );
		save_cf( CF_sponsor_pretax_npv, nyears, "cf_sponsor_pretax_npv" );
		save_cf( CF_sponsor_aftertax_cash, nyears, "cf_sponsor_aftertax_cash" );
		save_cf( CF_sponsor_aftertax, nyears, "cf_sponsor_aftertax" );
		save_cf( CF_sponsor_aftertax_itc, nyears, "cf_sponsor_aftertax_itc" );
		save_cf( CF_sponsor_aftertax_ptc, nyears, "cf_sponsor_aftertax_ptc" );
		save_cf( CF_sponsor_aftertax_tax, nyears, "cf_sponsor_aftertax_tax" );
		save_cf( CF_sponsor_aftertax_irr, nyears, "cf_sponsor_aftertax_irr" );
		save_cf( CF_sponsor_aftertax_npv, nyears, "cf_sponsor_aftertax_npv" );

		save_cf( CF_tax_investor_aftertax_cash, nyears, "cf_tax_investor_aftertax_cash" );
		save_cf( CF_tax_investor_aftertax_itc, nyears, "cf_tax_investor_aftertax_itc" );
		save_cf( CF_tax_investor_aftertax_ptc, nyears, "cf_tax_investor_aftertax_ptc" );
		save_cf( CF_tax_investor_aftertax_tax, nyears, "cf_tax_investor_aftertax_tax" );
		save_cf( CF_tax_investor_aftertax, nyears, "cf_tax_investor_aftertax" );
		save_cf( CF_tax_investor_aftertax_irr, nyears, "cf_tax_investor_aftertax_irr" );
		save_cf( CF_tax_investor_aftertax_npv, nyears, "cf_tax_investor_aftertax_npv" );
		save_cf( CF_tax_investor_aftertax_max_irr, nyears, "cf_tax_investor_aftertax_max_irr" );

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
		save_cf( CF_feddepr_me1, nyears, "cf_feddepr_me1" );
		save_cf( CF_feddepr_me2, nyears, "cf_feddepr_me2" );
		save_cf( CF_feddepr_me3, nyears, "cf_feddepr_me3" );
		save_cf( CF_feddepr_total, nyears, "cf_feddepr_total" );
		save_cf( CF_fedtax_income_prior_incentives, nyears, "cf_fedtax_income_prior_incentives" );

		save_cf( CF_ibi_total, nyears, "cf_ibi_total" );
		save_cf( CF_cbi_total, nyears, "cf_cbi_total" );
		save_cf( CF_pbi_total, nyears, "cf_pbi_total" );
		save_cf( CF_ptc_fed, nyears, "cf_ptc_fed" );
		save_cf( CF_ptc_sta, nyears, "cf_ptc_sta" );
		save_cf( CF_itc_fed_total, nyears, "cf_itc_fed_total" );
		save_cf( CF_itc_sta_total, nyears, "cf_itc_sta_total" );

		save_cf( CF_project_return_aftertax_cash, nyears, "cf_project_return_aftertax_cash" );
		save_cf( CF_project_return_aftertax, nyears, "cf_project_return_aftertax" );
		save_cf( CF_project_return_aftertax_irr, nyears, "cf_project_return_aftertax_irr" );
		save_cf( CF_project_return_aftertax_npv, nyears, "cf_project_return_aftertax_npv" );
		save_cf( CF_project_return_pretax, nyears, "cf_project_return_pretax" );
		save_cf( CF_project_return_pretax_irr, nyears, "cf_project_return_pretax_irr" );
		save_cf( CF_project_return_pretax_npv, nyears, "cf_project_return_pretax_npv" );

		save_cf( CF_project_financing_activities, nyears, "cf_project_financing_activities" );
		save_cf( CF_pretax_cashflow, nyears, "cf_pretax_cashflow" );

		save_cf( CF_project_dsra, nyears, "cf_project_dsra" );
		save_cf( CF_project_wcra, nyears, "cf_project_wcra" );
		save_cf( CF_project_me1ra, nyears, "cf_project_me1ra" );
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

		save_cf( CF_itc_sta_amt, nyears, "cf_itc_sta_amt" );
		save_cf( CF_itc_sta_per, nyears, "cf_itc_sta_per" );
		save_cf( CF_itc_fed_amt, nyears, "cf_itc_fed_amt" );
		save_cf( CF_itc_fed_per, nyears, "cf_itc_fed_per" );

		save_cf( CF_debt_payment_total, nyears, "cf_debt_payment_total" );
		save_cf( CF_debt_payment_interest, nyears, "cf_debt_payment_interest" );
		save_cf( CF_debt_payment_principal, nyears, "cf_debt_payment_principal" );
		save_cf( CF_debt_balance, nyears, "cf_debt_balance" );

		save_cf( CF_energy_value, nyears, "cf_energy_value" );
		save_cf( CF_ppa_price, nyears, "cf_ppa_price" );
		save_cf( CF_om_fixed_expense, nyears, "cf_om_fixed_expense" );
		save_cf( CF_om_production_expense, nyears, "cf_om_production_expense" );
		save_cf( CF_om_capacity_expense, nyears, "cf_om_capacity_expense" );
		save_cf( CF_om_fuel_expense, nyears, "cf_om_fuel_expense" );
		save_cf( CF_property_tax_assesed_value, nyears, "cf_property_tax_assesed_value" );
		save_cf( CF_property_tax_expense, nyears, "cf_property_tax_expense" );
		save_cf( CF_insurance_expense, nyears, "cf_insurance_expense" );
		save_cf( CF_operating_expenses, nyears, "cf_operating_expenses" );
		save_cf( CF_ebitda, nyears, "cf_ebitda" );
		save_cf( CF_total_revenue, nyears, "cf_total_revenue" );
		save_cf( CF_energy_net, nyears, "cf_energy_net" );
		save_cf( CF_reserve_debtservice, nyears, "cf_reserve_debtservice" );
		save_cf( CF_reserve_om, nyears, "cf_reserve_om" );
		save_cf( CF_reserve_equip1, nyears, "cf_reserve_equip1" );
		save_cf( CF_reserve_equip2, nyears, "cf_reserve_equip2" );
		save_cf( CF_reserve_equip3, nyears, "cf_reserve_equip3" );

		save_cf( CF_funding_debtservice, nyears, "cf_funding_debtservice" );
		save_cf( CF_funding_om, nyears, "cf_funding_om" );
		save_cf( CF_funding_equip1, nyears, "cf_funding_equip1" );
		save_cf( CF_funding_equip2, nyears, "cf_funding_equip2" );
		save_cf( CF_funding_equip3, nyears, "cf_funding_equip3" );

		save_cf( CF_disbursement_debtservice, nyears, "cf_disbursement_debtservice" );
		save_cf( CF_disbursement_om, nyears, "cf_disbursement_om" );
		save_cf( CF_disbursement_equip1, nyears, "cf_disbursement_equip1" );
		save_cf( CF_disbursement_equip2, nyears, "cf_disbursement_equip2" );
		save_cf( CF_disbursement_equip3, nyears, "cf_disbursement_equip3" );

		save_cf( CF_reserve_total, nyears, "cf_reserve_total" );
		save_cf( CF_reserve_interest, nyears, "cf_reserve_interest" );
	}


	// std lib
	void major_equipment_depreciation( int cf_equipment_expenditure, int cf_depr_sched, int expenditure_year, int analysis_period, int cf_equipment_depreciation )
	{
		// depreciate equipment cost in expenditure_year according to depr_sched schedule subject to cutoff by analysis_period
		if ( (expenditure_year > 0 ) && (expenditure_year <= analysis_period))
		{
			// sign convention from DHF v3 model
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
				cf.at(cf_line, i) = (i <= term) ? parr[0] * cf.at(CF_energy_net,i) * pow(1 + escal, i-1) : 0.0;
		}
		else
		{
			for (int i=1;i<=nyears && i <= (int)len;i++)
				cf.at(cf_line, i) = parr[i-1]*cf.at(CF_energy_net,i);
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

	double taxable_incentive_income(int year, const std::string &fed_or_sta)
	{
		double ti = 0.0;

		if ( as_boolean("ibi_fed_amount_tax_"+fed_or_sta) ) ti += cf.at( CF_ibi_fed_amt, year );
		if ( as_boolean("ibi_sta_amount_tax_"+fed_or_sta) ) ti += cf.at( CF_ibi_sta_amt, year );
		if ( as_boolean("ibi_uti_amount_tax_"+fed_or_sta) ) ti += cf.at( CF_ibi_uti_amt, year );
		if ( as_boolean("ibi_oth_amount_tax_"+fed_or_sta) ) ti += cf.at( CF_ibi_oth_amt, year );

		if ( as_boolean("ibi_fed_percent_tax_"+fed_or_sta) ) ti += cf.at( CF_ibi_fed_per, year );
		if ( as_boolean("ibi_sta_percent_tax_"+fed_or_sta) ) ti += cf.at( CF_ibi_sta_per, year );
		if ( as_boolean("ibi_uti_percent_tax_"+fed_or_sta) ) ti += cf.at( CF_ibi_uti_per, year );
		if ( as_boolean("ibi_oth_percent_tax_"+fed_or_sta) ) ti += cf.at( CF_ibi_oth_per, year );

		if ( as_boolean("cbi_fed_tax_"+fed_or_sta) ) ti += cf.at( CF_cbi_fed, year );
		if ( as_boolean("cbi_sta_tax_"+fed_or_sta) ) ti += cf.at( CF_cbi_sta, year );
		if ( as_boolean("cbi_uti_tax_"+fed_or_sta) ) ti += cf.at( CF_cbi_uti, year );
		if ( as_boolean("cbi_oth_tax_"+fed_or_sta) ) ti += cf.at( CF_cbi_oth, year );

		if ( as_boolean("pbi_fed_tax_"+fed_or_sta) ) ti += cf.at( CF_pbi_fed, year );
		if ( as_boolean("pbi_sta_tax_"+fed_or_sta) ) ti += cf.at( CF_pbi_sta, year );
		if ( as_boolean("pbi_uti_tax_"+fed_or_sta) ) ti += cf.at( CF_pbi_uti, year );
		if ( as_boolean("pbi_oth_tax_"+fed_or_sta) ) ti += cf.at( CF_pbi_oth, year );

		return ti;
	}

	double npv( int cf_line, int nyears, double rate ) throw ( general_error )
	{
		if (rate <= -1.0) throw general_error("cannot calculate NPV with discount rate less or equal to -1.0");

		double rr = 1/(1+rate);
		double result = 0;
		for (int i=nyears;i>0;i--)
			result = rr * result + cf.at(cf_line,i);

		return result*rr;
	}

/* ported from http://code.google.com/p/irr-newtonraphson-calculator/ */
	bool is_valid_iter_bound(double estimated_return_rate)
	{
		return estimated_return_rate != -1 && (estimated_return_rate < INT_MAX) && (estimated_return_rate > INT_MIN);
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

	double irr( int cf_line, int count, double initial_guess=-1 )
	{
		// from Excel
//		int max_iterations=20;
//		double tolerance =1e-5;
//		initial_guess = 0.1;
		// from various cases
		int max_iterations=200;
		double tolerance =1e-6;

		int number_of_iterations=0;
		double calculated_irr=0;


		if (count < 1)
			return calculated_irr;

		// initial guess from http://zainco.blogspot.com/2008/08/internal-rate-of-return-using-newton.html
		if (initial_guess < 0)
		{
			if (cf.at(cf_line,0) !=0) initial_guess = -(1.0 + cf.at(cf_line,1)/cf.at(cf_line,0));
			if ((initial_guess <= 0) || (initial_guess >= 1)) initial_guess = 0.1;
		}

		if ( (cf.at(cf_line,0) <= 0))
		{
			double deriv_sum = irr_derivative_sum(initial_guess,cf_line,count);
			if (deriv_sum != 0.0)
				calculated_irr = initial_guess - irr_poly_sum(initial_guess,cf_line,count)/deriv_sum;
			else
				return initial_guess;

			number_of_iterations++;
			while (!(fabs(irr_poly_sum(calculated_irr,cf_line,count)) <= tolerance) && (number_of_iterations < max_iterations))
			{
				deriv_sum = irr_derivative_sum(initial_guess,cf_line,count);
				if (deriv_sum != 0.0)
					calculated_irr = calculated_irr - irr_poly_sum(calculated_irr,cf_line,count)/deriv_sum;
				else
					break;

				number_of_iterations++;
			}
		}
		// TODO - check cases for which bounded irr -50% to 200% are all greater than zero or less than zero - no irr
		// should be NA with explanation
		if (number_of_iterations >= max_iterations) calculated_irr = 0;
		return calculated_irr;
	}


	double min( double a, double b )
	{
		return (a < b) ? a : b;
	}

	double max( double a, double b )
	{
		return (a > b) ? a : b;
	}

};




DEFINE_MODULE_ENTRY( equpartflip, "DHF All Equity Partnership Flip Financial Model_", 1 );


