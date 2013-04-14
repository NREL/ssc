#include "lib_financial.h"
using namespace libfin;
#include "core.h"
#include <sstream>

#ifndef WIN32
#include <float.h>
#endif

static var_info _cm_vtab_singleowner[] = {


/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
//	{ SSC_INPUT,        SSC_NUMBER,      "energy_net",				"Annual energy produced by system",	"kWh",   "",                      "DHF",             "*",						   "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,      "energy_net_hourly",	"Hourly energy produced by the system",	"kWh",   "",                      "DHF",             "*",						   "",                 "" },
	{ SSC_INPUT,        SSC_ARRAY,      "energy_availability",		"Annual energy availability",	"",   "",                      "DHF",             "*",						   "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,      "energy_degradation",		"Annual energy degradation",	"",   "",                      "DHF",             "*",						   "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "system_capacity",			"System nameplate capacity",		"kW",    "",                      "DHF",             "*",						   "MIN=1e-3",                         "" },

/* Recapitalization */
	{ SSC_INPUT,        SSC_NUMBER,      "system_use_recapitalization",		"Recapitalization expenses",	"0/1",   "0=None,1=Recapitalize",                      "DHF",             "?=0",						   "INTEGER,MIN=0",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_recapitalization_cost",	"Recapitalization cost",	"$",   "",                      "DHF",             "?=0",						   "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "system_recapitalization_escalation", "Recapitalization escalation (above inflation)",					"%",	 "",					  "DHF",             "?=0",                     "MIN=0,MAX=100",      			"" },
	{ SSC_INPUT,        SSC_ARRAY,      "system_lifetime_recapitalize",		"Recapitalization boolean",	"",   "",                      "DHF",             "?=0",						   "",                              "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_recapitalization",	"Recapitalization operating expense",	"$",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },

/* Dispatch */
	{ SSC_INPUT,        SSC_NUMBER,      "system_use_lifetime_output",		"Lifetime hourly system outputs",	"0/1",   "0=hourly first year,1=hourly lifetime",                      "DHF",             "*",						   "INTEGER,MIN=0",                 "" },
	{ SSC_INPUT,        SSC_ARRAY,      "energy_net_hourly",	"Hourly energy produced by the system",	"%",   "",                      "DHF",             "*",						   "",                 "" },
	{ SSC_INPUT,        SSC_ARRAY,      "dispatch_hourly",		"Hourly dispatch schedule for the system (1-9)",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor1",		"Dispatch payment factor 1",	"",   "",                      "DHF",             "*",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor2",		"Dispatch payment factor 2",	"",   "",                      "DHF",             "*",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor3",		"Dispatch payment factor 3",	"",   "",                      "DHF",             "*",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor4",		"Dispatch payment factor 4",	"",   "",                      "DHF",             "*",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor5",		"Dispatch payment factor 5",	"",   "",                      "DHF",             "*",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor6",		"Dispatch payment factor 6",	"",   "",                      "DHF",             "*",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor7",		"Dispatch payment factor 7",	"",   "",                      "DHF",             "*",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor8",		"Dispatch payment factor 8",	"",   "",                      "DHF",             "*",						   "",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dispatch_factor9",		"Dispatch payment factor 9",	"",   "",                      "DHF",             "*",						   "",                 "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_jan",	"Energy produced by the system in January",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_jan",		"Revenue from the system in January",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_feb",	"Energy produced by the system in February",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_feb",		"Revenue from the system in February",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_mar",	"Energy produced by the system in March",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_mar",		"Revenue from the system in March",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_apr",	"Energy produced by the system in April",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_apr",		"Revenue from the system in April",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_may",	"Energy produced by the system in May",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_may",		"Revenue from the system in May",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_jun",	"Energy produced by the system in June",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_jun",		"Revenue from the system in June",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_jul",	"Energy produced by the system in July",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_jul",		"Revenue from the system in July",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_aug",	"Energy produced by the system in August",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_aug",		"Revenue from the system in August",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_sep",	"Energy produced by the system in September",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_sep",		"Revenue from the system in September",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_oct",	"Energy produced by the system in October",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_oct",		"Revenue from the system in October",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_nov",	"Energy produced by the system in November",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_nov",		"Revenue from the system in November",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_dec",	"Energy produced by the system in December",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_dec",		"Revenue from the system in December",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_dispatch1",	"Energy produced by the system in dispatch period 1",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_dispatch1",		"Revenue from the system in dispatch period 1",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_dispatch2",	"Energy produced by the system in dispatch period 2",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_dispatch2",		"Revenue from the system in dispatch period 2",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_dispatch3",	"Energy produced by the system in dispatch period 3",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_dispatch3",		"Revenue from the system in dispatch period 3",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_dispatch4",	"Energy produced by the system in dispatch period 4",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_dispatch4",		"Revenue from the system in dispatch period 4",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_dispatch5",	"Energy produced by the system in dispatch period 5",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_dispatch5",		"Revenue from the system in dispatch period 5",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_dispatch6",	"Energy produced by the system in dispatch period 6",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_dispatch6",		"Revenue from the system in dispatch period 6",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_dispatch7",	"Energy produced by the system in dispatch period 7",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_dispatch7",		"Revenue from the system in dispatch period 7",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_dispatch8",	"Energy produced by the system in dispatch period 8",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_dispatch8",		"Revenue from the system in dispatch period 8",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_dispatch9",	"Energy produced by the system in dispatch period 9",	"",   "",                      "DHF",             "*",						   "LENGTH_EQUAL=cf_length",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_dispatch9",		"Revenue from the system in dispatch period 9",	"",   "",                      "DHF",             "*",				   "LENGTH_EQUAL=cf_length",                 "" },


	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_revenue_dispatch1",                "First year revenue from the system in dispatch period 1",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_revenue_dispatch2",                "First year revenue from the system in dispatch period 2",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_revenue_dispatch3",                "First year revenue from the system in dispatch period 3",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_revenue_dispatch4",                "First year revenue from the system in dispatch period 4",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_revenue_dispatch5",                "First year revenue from the system in dispatch period 5",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_revenue_dispatch6",                "First year revenue from the system in dispatch period 6",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_revenue_dispatch7",                "First year revenue from the system in dispatch period 7",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_revenue_dispatch8",                "First year revenue from the system in dispatch period 8",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_revenue_dispatch9",                "First year revenue from the system in dispatch period 9",      "",             "",                      "DHF",      "*",                       "",                                  "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_dispatch1",                "First year energy from the system in dispatch period 1",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_dispatch2",                "First year energy from the system in dispatch period 2",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_dispatch3",                "First year energy from the system in dispatch period 3",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_dispatch4",                "First year energy from the system in dispatch period 4",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_dispatch5",                "First year energy from the system in dispatch period 5",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_dispatch6",                "First year energy from the system in dispatch period 6",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_dispatch7",                "First year energy from the system in dispatch period 7",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_dispatch8",                "First year energy from the system in dispatch period 8",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_dispatch9",                "First year energy from the system in dispatch period 9",      "",             "",                      "DHF",      "*",                       "",                                  "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_price1",                "First year energy price dispatch period 1",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_price2",                "First year energy price dispatch period 2",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_price3",                "First year energy price dispatch period 3",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_price4",                "First year energy price dispatch period 4",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_price5",                "First year energy price dispatch period 5",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_price6",                "First year energy price dispatch period 6",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_price7",                "First year energy price dispatch period 7",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_price8",                "First year energy price dispatch period 8",      "",             "",                      "DHF",      "*",                       "",                                  "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "firstyear_energy_price9",                "First year energy price dispatch period 9",      "",             "",                      "DHF",      "*",                       "",                                  "" },


// first year monthly output for each TOD period
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_monthly_firstyear",		"First year revenue from the system by month",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_monthly_firstyear",		"First year energy from the system by month",	"",   "",                      "DHF",             "*",				   "",                 "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_monthly_firstyear_TOD1",		"First year revenue from the system by month for TOD1",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_monthly_firstyear_TOD1",		"First year energy from the system by month for TOD1",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_monthly_firstyear_TOD2",		"First year revenue from the system by month for TOD2",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_monthly_firstyear_TOD2",		"First year energy from the system by month for TOD2",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_monthly_firstyear_TOD3",		"First year revenue from the system by month for TOD3",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_monthly_firstyear_TOD3",		"First year energy from the system by month for TOD3",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_monthly_firstyear_TOD4",		"First year revenue from the system by month for TOD4",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_monthly_firstyear_TOD4",		"First year energy from the system by month for TOD4",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_monthly_firstyear_TOD5",		"First year revenue from the system by month for TOD5",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_monthly_firstyear_TOD5",		"First year energy from the system by month for TOD5",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_monthly_firstyear_TOD6",		"First year revenue from the system by month for TOD6",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_monthly_firstyear_TOD6",		"First year energy from the system by month for TOD6",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_monthly_firstyear_TOD7",		"First year revenue from the system by month for TOD7",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_monthly_firstyear_TOD7",		"First year energy from the system by month for TOD7",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_monthly_firstyear_TOD8",		"First year revenue from the system by month for TOD8",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_monthly_firstyear_TOD8",		"First year energy from the system by month for TOD8",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_revenue_monthly_firstyear_TOD9",		"First year revenue from the system by month for TOD9",	"",   "",                      "DHF",             "*",				   "",                 "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_energy_net_monthly_firstyear_TOD9",		"First year energy from the system by month for TOD9",	"",   "",                      "DHF",             "*",				   "",                 "" },








/* inputs in DHF model not currently in SAM 11/15/10 */
	{ SSC_INPUT,       SSC_NUMBER,      "cost_salestax",              "Sales tax",                        "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_INPUT,       SSC_NUMBER,      "cost_prefinancing",          "Installed cost",                   "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "reserves_interest",        "Interest on reserves",				"%",	 "",					  "DHF",             "?=1.75",                     "MIN=0,MAX=100",      			"" },

/* DHF replacement reserve on top of regular o and m */
	{ SSC_INPUT,        SSC_NUMBER,     "equip1_reserve_cost",      "Major equipment reserve1 cost",	"$/Wdc",	 "",				  "DHF",             "?=0.25",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "equip1_reserve_freq",      "Major equipment reserve1 frequency",	"years",	 "",			  "DHF",             "?=12",               "INTEGER,MIN=0",                         "" },

	{ SSC_INPUT,        SSC_NUMBER,     "equip2_reserve_cost",      "Major equipment reserve2 cost",	"$/Wdc",	 "",				  "DHF",             "?=0",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "equip2_reserve_freq",      "Major equipment reserve2 frequency",	"years",	 "",			  "DHF",             "?=15",               "INTEGER,MIN=0",                         "" },

	{ SSC_INPUT,        SSC_NUMBER,     "equip3_reserve_cost",      "Major equipment reserve3 cost",	"$/Wdc",	 "",				  "DHF",             "?=0",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "equip3_reserve_freq",      "Major equipment reserve3 frequency",	"years",	 "",			  "DHF",             "?=20",               "INTEGER,MIN=0",                         "" },

/* major equipment depreciation schedules - can extend to three different schedules */
	{ SSC_INPUT,        SSC_NUMBER,     "equip_reserve_depr_sta",   "Major equipment reserve state depreciation",	"",	 "0=5yr MACRS,1=15yr MACRS,2=5yr SL,3=15yr SL, 4=20yr SL,5=39yr SL,6=Custom",  "DHF", "?=0",   "INTEGER,MIN=0,MAX=6",  "" },
	{ SSC_INPUT,        SSC_NUMBER,     "equip_reserve_depr_fed",   "Major equipment reserve federal depreciation",	"",	 "0=5yr MACRS,1=15yr MACRS,2=5yr SL,3=15yr SL, 4=20yr SL,5=39yr SL,6=Custom",  "DHF", "?=0",   "INTEGER,MIN=0,MAX=6",  "" },

/* DHF salvage value */	
	{ SSC_INPUT,        SSC_NUMBER,     "salvage_percentage",          "Net pre-tax cash salvage value",	"%",	 "",					  "DHF",             "?=10",                     "MIN=0,MAX=100",      			"" },
/* DHF market specific inputs - leveraged partnership flip */
	{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_mode",            "PPA solution mode",                "0/1",   "0=solve ppa,1=specify ppa", "DHF",         "?=0",                     "INTEGER,MIN=0,MAX=1",            "" },
	{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_tolerance",            "PPA solution tolerance",                "",   "", "DHF",         "?=1e-3",                     "",            "" },
	{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_min",            "PPA solution minimum ppa",                "cents/kWh",   "", "DHF",         "?=0",                     "",            "" },
	{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_max",            "PPA solution maximum ppa",                "cents/kWh",   "", "DHF",         "?=100",                     "",            "" },
	{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_max_iterations",            "PPA solution maximum number of iterations",                "",   "", "DHF",         "?=100",                     "INTEGER,MIN=1",            "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ppa_price_input",			"Initial year PPA price",			"$/kWh",	 "",			  "DHF",			 "?=10",         "",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "ppa_escalation",           "PPA escalation",					"%",	 "",					  "DHF",             "?=0",                     "MIN=0,MAX=100",      			"" },

/* DHF construction period */
	{ SSC_INPUT,       SSC_NUMBER,      "constr_total_financing",	"Construction financing total",	"$",	 "",					  "DHF",			 "*",                         "",                             "" },

/* DHF term financing */
	{ SSC_INPUT,        SSC_NUMBER,     "term_tenor",               "Term financing tenor",				"years", "",				      "DHF",             "?=10",					"INTEGER,MIN=0",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "term_int_rate",            "Term financing interest rate",		"%",	 "",					  "DHF",             "?=8.5",                   "MIN=0,MAX=100",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "dscr",						"Debt service coverage ratio",		"",	     "",				      "DHF",             "?=1.5",					"MIN=0",      			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "dscr_reserve_months",		"Debt service reserve account",		"months P&I","",			      "DHF",             "?=6",					    "INTEGER,MIN=0",      			        "" },
/* DHF Capital Cost */
	{ SSC_INPUT,        SSC_NUMBER,     "cost_debt_closing",		"Debt closing cost",				"$",	 "",					  "DHF",             "?=250000",					    "MIN=0",      			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "cost_debt_fee",		"Debt closing fee (% of total debt amount)",				"%",	 "",					  "DHF",             "?=1.5",					    "MIN=0",      			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "months_working_reserve",		"Working capital reserve monthst of o and m",		"months",	 "",					  "DHF",             "?=6",					    "INTEGER,MIN=0",      			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "cost_other_financing",		"",		"$",	 "Other Financing Cost",					  "DHF",             "?=150000",					    "MIN=0",      			        "" },
/* DHF Equity Structure */
	{ SSC_INPUT,        SSC_NUMBER,     "flip_target_percent",			"After-tax flip/return target",		"%",	 "",					  "DHF",             "?=11",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "flip_target_year",		"Return target year",				"",		 "",					  "DHF",             "?=11",					  "MIN=1",     			        "" },
/* DHF depreciation allocation */
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_macrs_5_percent",		"5-yr MACRS depreciation federal and state allocation",	"%", "",	  "DHF",             "?=89",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_macrs_15_percent",		"15-yr MACRS depreciation federal and state allocation",	"%", "",  "DHF",             "?=1.5",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_5_percent",		"5-yr straight line depreciation federal and state allocation",	"%", "",  "DHF",             "?=0",						  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_15_percent",		"15-yr straight line depreciation federal and state allocation","%", "",  "DHF",             "?=3",						  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_20_percent",		"20-yr straight line depreciation federal and state allocation","%", "",  "DHF",             "?=3",						  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_39_percent",		"39-yr straight line depreciation federal and state allocation","%", "",  "DHF",             "?=0.5",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_custom_percent",		"Custom depreciation federal and state allocation","%", "",  "DHF",             "?=0",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_ARRAY,      "depr_custom_schedule",		"Custom depreciation schedule",	"%",   "",                      "DHF",             "*",						   "",                              "" },
/* DHF bonus depreciation */
	{ SSC_INPUT,        SSC_NUMBER,     "depr_bonus_sta",			"State bonus depreciation",			"%",	 "",					  "DHF",             "?=0",						  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_macrs_5",   "State bonus depreciation 5-yr MACRS","0/1", "",                      "DHF",			 "?=1",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_macrs_15",   "State bonus depreciation 15-yr MACRS","0/1","",                     "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_sl_5",   "State bonus depreciation 5-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_sl_15",   "State bonus depreciation 15-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_sl_20",   "State bonus depreciation 20-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_sl_39",   "State bonus depreciation 39-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_sta_custom",   "State bonus depreciation custom","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },

	{ SSC_INPUT,        SSC_NUMBER,     "depr_bonus_fed",			"Federal bonus depreciation",			"%",	 "",					  "DHF",             "?=0",						  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_macrs_5",   "Federal bonus depreciation 5-yr MACRS","0/1", "",                      "DHF",			 "?=1",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_macrs_15",   "Federal bonus depreciation 15-yr MACRS","0/1","",                     "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_sl_5",   "Federal bonus depreciation 5-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_sl_15",   "Federal bonus depreciation 15-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_sl_20",   "Federal bonus depreciation 20-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_sl_39",   "Federal bonus depreciation 39-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_bonus_fed_custom",   "Federal bonus depreciation custom","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
/* DHF ITC depreciation */
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_macrs_5",   "State itc depreciation 5-yr MACRS","0/1", "",                      "DHF",			 "?=1",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_macrs_15",   "State itc depreciation 15-yr MACRS","0/1","",                     "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_sl_5",   "State itc depreciation 5-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_sl_15",   "State itc depreciation 15-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_sl_20",   "State itc depreciation 20-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_sl_39",   "State itc depreciation 39-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_sta_custom",   "State itc depreciation custom","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },

	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_macrs_5",   "Federal itc depreciation 5-yr MACRS","0/1", "",                      "DHF",			 "?=1",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_macrs_15",   "Federal itc depreciation 15-yr MACRS","0/1","",                     "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_sl_5",   "Federal itc depreciation 5-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_sl_15",   "Federal itc depreciation 15-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_sl_20",   "Federal itc depreciation 20-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_sl_39",   "Federal itc depreciation 39-yr straight line","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },
	{ SSC_INPUT,        SSC_NUMBER,		"depr_itc_fed_custom",   "Federal itc depreciation custom","0/1","",                  "DHF",			 "?=0",                       "BOOLEAN",                        "" },

/* PBI for debt service TODO - other yearly incentives */
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_fed_for_ds",    "Federal PBI available for debt service",     "0/1",      "",                      "DHF",      "?=0",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_sta_for_ds",    "State PBI available for debt service",     "0/1",      "",                      "DHF",      "?=0",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_uti_for_ds",    "Utility PBI available for debt service",     "0/1",      "",                      "DHF",      "?=0",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_oth_for_ds",    "Other PBI available for debt service",     "0/1",      "",                      "DHF",      "?=0",                       "BOOLEAN",                                         "" },

/* intermediate outputs */
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_financing",   "Financing Cost",          "$",   "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_prefinancingperwatt",   "Installed cost per watt",          "$/W",   "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_installed",          "Total project cost",                   "",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_installedperwatt",   "Installed cost per watt",          "$/W",   "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "nominal_discount_rate",   "Nominal discount rate",            "%",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "prop_tax_assessed_value", "Assessed value of property for tax purposes","$", "",				  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "salvage_value",			"Net pre-tax cash salvage value",	"$",	 "",					  "DHF",			 "*",                         "",                             "" },
	
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_none_percent",		"Non-depreciable federal and state allocation",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_none",		"Non-depreciable federal and state allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_total",		"Total depreciation federal and state allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },

// state itc table
/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_macrs_5",		"5-yr MACRS state percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_macrs_5",		"5-yr MACRS depreciation federal and state allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_ibi_reduc_macrs_5",		"5-yr MACRS state ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_cbi_reduc_macrs_5",		"5-yr MACRS state cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_prior_itc_macrs_5",		"5-yr MACRS state depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_macrs_5",		"5-yr MACRS depreciation state ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_qual_macrs_5",		"5-yr MACRS state percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_amount_macrs_5",		"5-yr MACRS depreciation ITC basis from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_percent_macrs_5",		"5-yr MACRS depreciation ITC basis disallowance from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_fixed_amount_macrs_5",		"5-yr MACRS depreciation ITC basis from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_fixed_macrs_5",		"5-yr MACRS depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_sta_reduction_macrs_5",		"5-yr MACRS state basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_fed_reduction_macrs_5",		"5-yr MACRS state basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_after_itc_macrs_5",		"5-yr MACRS state depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_first_year_bonus_macrs_5",		"5-yr MACRS state first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_macrs_5",		"5-yr MACRS state depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_macrs_15",		"15-yr MACRS state percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_macrs_15",		"15-yr MACRS depreciation federal and state allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_ibi_reduc_macrs_15",		"15-yr MACRS state ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_cbi_reduc_macrs_15",		"15-yr MACRS state cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_prior_itc_macrs_15",		"15-yr MACRS state depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_macrs_15",		"15-yr MACRS depreciation state ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_qual_macrs_15",		"15-yr MACRS state percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_amount_macrs_15",		"15-yr MACRS depreciation ITC basis from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_percent_macrs_15",		"15-yr MACRS depreciation ITC basis disallowance from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_fixed_amount_macrs_15",		"15-yr MACRS depreciation ITC basis from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_fixed_macrs_15",		"15-yr MACRS depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_sta_reduction_macrs_15",		"15-yr MACRS state basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_fed_reduction_macrs_15",		"15-yr MACRS state basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_after_itc_macrs_15",		"15-yr MACRS state depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_first_year_bonus_macrs_15",		"15-yr MACRS state first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_macrs_15",		"15-yr MACRS state depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_sl_5",		"5-yr straight line state percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_sl_5",		"5-yr straight line depreciation federal and state allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_ibi_reduc_sl_5",		"5-yr straight line state ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_cbi_reduc_sl_5",		"5-yr straight line state cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_prior_itc_sl_5",		"5-yr straight line state depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_sl_5",		"5-yr straight line depreciation state ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_qual_sl_5",		"5-yr straight line state percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_amount_sl_5",		"5-yr straight line depreciation ITC basis from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_percent_sl_5",		"5-yr straight line depreciation ITC basis disallowance from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_fixed_amount_sl_5",		"5-yr straight line depreciation ITC basis from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_fixed_sl_5",		"5-yr straight line depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_sta_reduction_sl_5",		"5-yr straight line state basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_fed_reduction_sl_5",		"5-yr straight line state basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_after_itc_sl_5",		"5-yr straight line state depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_first_year_bonus_sl_5",		"5-yr straight line state first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_sl_5",		"5-yr straight line state depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_sl_15",		"15-yr straight line state percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_sl_15",		"15-yr straight line depreciation federal and state allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_ibi_reduc_sl_15",		"15-yr straight line state ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_cbi_reduc_sl_15",		"15-yr straight line state cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_prior_itc_sl_15",		"15-yr straight line state depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_sl_15",		"15-yr straight line depreciation state ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_qual_sl_15",		"15-yr straight line state percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_amount_sl_15",		"15-yr straight line depreciation ITC basis from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_percent_sl_15",		"15-yr straight line depreciation ITC basis disallowance from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_fixed_amount_sl_15",		"15-yr straight line depreciation ITC basis from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_fixed_sl_15",		"15-yr straight line depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_sta_reduction_sl_15",		"15-yr straight line state basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_fed_reduction_sl_15",		"15-yr straight line state basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_after_itc_sl_15",		"15-yr straight line state depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_first_year_bonus_sl_15",		"15-yr straight line state first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_sl_15",		"15-yr straight line state depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_sl_20",		"20-yr straight line state percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_sl_20",		"20-yr straight line depreciation federal and state allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_ibi_reduc_sl_20",		"20-yr straight line state ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_cbi_reduc_sl_20",		"20-yr straight line state cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_prior_itc_sl_20",		"20-yr straight line state depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_sl_20",		"20-yr straight line depreciation state ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_qual_sl_20",		"20-yr straight line state percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_amount_sl_20",		"20-yr straight line depreciation ITC basis from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_percent_sl_20",		"20-yr straight line depreciation ITC basis disallowance from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_fixed_amount_sl_20",		"20-yr straight line depreciation ITC basis from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_fixed_sl_20",		"20-yr straight line depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_sta_reduction_sl_20",		"20-yr straight line state basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_fed_reduction_sl_20",		"20-yr straight line state basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_after_itc_sl_20",		"20-yr straight line state depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_first_year_bonus_sl_20",		"20-yr straight line state first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_sl_20",		"20-yr straight line state depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_sl_39",		"39-yr straight line state percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_sl_39",		"39-yr straight line depreciation federal and state allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_ibi_reduc_sl_39",		"39-yr straight line state ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_cbi_reduc_sl_39",		"39-yr straight line state cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_prior_itc_sl_39",		"39-yr straight line state depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_sl_39",		"39-yr straight line depreciation state ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_qual_sl_39",		"39-yr straight line state percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_amount_sl_39",		"39-yr straight line depreciation ITC basis from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_percent_sl_39",		"39-yr straight line depreciation ITC basis disallowance from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_fixed_amount_sl_39",		"39-yr straight line depreciation ITC basis from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_fixed_sl_39",		"39-yr straight line depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_sta_reduction_sl_39",		"39-yr straight line state basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_fed_reduction_sl_39",		"39-yr straight line state basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_after_itc_sl_39",		"39-yr straight line state depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_first_year_bonus_sl_39",		"39-yr straight line state first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_sl_39",		"39-yr straight line state depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_custom",		"Custom straight line state percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_custom",		"Custom straight line depreciation federal and state allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_ibi_reduc_custom",		"Custom straight line state ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_cbi_reduc_custom",		"Custom straight line state cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_prior_itc_custom",		"Custom straight line state depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_custom",		"Custom straight line depreciation state ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_qual_custom",		"Custom straight line state percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_amount_custom",		"Custom straight line depreciation ITC basis from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_percent_custom",		"Custom straight line depreciation ITC basis disallowance from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_fixed_amount_custom",		"Custom straight line depreciation ITC basis from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_fixed_custom",		"Custom straight line depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_sta_reduction_custom",		"Custom straight line state basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_fed_reduction_custom",		"Custom straight line state basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_after_itc_custom",		"Custom straight line state depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_first_year_bonus_custom",		"Custom straight line state first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_custom",		"Custom straight line state depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },


/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_total",		"Total state percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_total",		"Total depreciation federal and state allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_ibi_reduc_total",		"Total state ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_cbi_reduc_total",		"Total state cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_prior_itc_total",		"Total state depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_qual_total",		"Total state ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_qual_total",		"Total state percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_percent_amount_total",		"Total depreciation ITC basis from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_percent_total",		"Total depreciation ITC basis disallowance from state percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_fixed_amount_total",		"Total depreciation ITC basis from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_sta_fixed_total",		"Total depreciation ITC basis disallowance from state fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_sta_reduction_total",		"Total state basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_itc_fed_reduction_total",		"Total state basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_after_itc_total",		"Total state depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_first_year_bonus_total",		"Total state first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_total",		"Total state depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_percent_total",		"State ITC percent total",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_sta_fixed_total",		"State ITC fixed total",	"$", "",	  "DHF",             "*",					  "",     			        "" },

// federal itc table
/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_macrs_5",		"5-yr MACRS federal percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_macrs_5",		"5-yr MACRS depreciation federal and federal allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_ibi_reduc_macrs_5",		"5-yr MACRS federal ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_cbi_reduc_macrs_5",		"5-yr MACRS federal cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_prior_itc_macrs_5",		"5-yr MACRS federal depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_macrs_5",		"5-yr MACRS depreciation federal ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_qual_macrs_5",		"5-yr MACRS federal percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_amount_macrs_5",		"5-yr MACRS depreciation ITC basis from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_percent_macrs_5",		"5-yr MACRS depreciation ITC basis disallowance from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_fixed_amount_macrs_5",		"5-yr MACRS depreciation ITC basis from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_fixed_macrs_5",		"5-yr MACRS depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_macrs_5",		"5-yr MACRS federal basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_macrs_5",		"5-yr MACRS federal basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_after_itc_macrs_5",		"5-yr MACRS federal depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_first_year_bonus_macrs_5",		"5-yr MACRS federal first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_macrs_5",		"5-yr MACRS federal depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_macrs_15",		"15-yr MACRS federal percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_macrs_15",		"15-yr MACRS depreciation federal and federal allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_ibi_reduc_macrs_15",		"15-yr MACRS federal ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_cbi_reduc_macrs_15",		"15-yr MACRS federal cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_prior_itc_macrs_15",		"15-yr MACRS federal depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_macrs_15",		"15-yr MACRS depreciation federal ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_qual_macrs_15",		"15-yr MACRS federal percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_amount_macrs_15",		"15-yr MACRS depreciation ITC basis from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_percent_macrs_15",		"15-yr MACRS depreciation ITC basis disallowance from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_fixed_amount_macrs_15",		"15-yr MACRS depreciation ITC basis from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_fixed_macrs_15",		"15-yr MACRS depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_macrs_15",		"15-yr MACRS federal basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_macrs_15",		"15-yr MACRS federal basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_after_itc_macrs_15",		"15-yr MACRS federal depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_first_year_bonus_macrs_15",		"15-yr MACRS federal first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_macrs_15",		"15-yr MACRS federal depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_sl_5",		"5-yr straight line federal percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_sl_5",		"5-yr straight line depreciation federal and federal allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_ibi_reduc_sl_5",		"5-yr straight line federal ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_cbi_reduc_sl_5",		"5-yr straight line federal cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_prior_itc_sl_5",		"5-yr straight line federal depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_sl_5",		"5-yr straight line depreciation federal ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_qual_sl_5",		"5-yr straight line federal percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_amount_sl_5",		"5-yr straight line depreciation ITC basis from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_percent_sl_5",		"5-yr straight line depreciation ITC basis disallowance from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_fixed_amount_sl_5",		"5-yr straight line depreciation ITC basis from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_fixed_sl_5",		"5-yr straight line depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_sl_5",		"5-yr straight line federal basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_sl_5",		"5-yr straight line federal basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_after_itc_sl_5",		"5-yr straight line federal depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_first_year_bonus_sl_5",		"5-yr straight line federal first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_sl_5",		"5-yr straight line federal depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_sl_15",		"15-yr straight line federal percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_sl_15",		"15-yr straight line depreciation federal and federal allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_ibi_reduc_sl_15",		"15-yr straight line federal ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_cbi_reduc_sl_15",		"15-yr straight line federal cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_prior_itc_sl_15",		"15-yr straight line federal depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_sl_15",		"15-yr straight line depreciation federal ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_qual_sl_15",		"15-yr straight line federal percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_amount_sl_15",		"15-yr straight line depreciation ITC basis from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_percent_sl_15",		"15-yr straight line depreciation ITC basis disallowance from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_fixed_amount_sl_15",		"15-yr straight line depreciation ITC basis from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_fixed_sl_15",		"15-yr straight line depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_sl_15",		"15-yr straight line federal basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_sl_15",		"15-yr straight line federal basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_after_itc_sl_15",		"15-yr straight line federal depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_first_year_bonus_sl_15",		"15-yr straight line federal first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_sl_15",		"15-yr straight line federal depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_sl_20",		"20-yr straight line federal percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_sl_20",		"20-yr straight line depreciation federal and federal allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_ibi_reduc_sl_20",		"20-yr straight line federal ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_cbi_reduc_sl_20",		"20-yr straight line federal cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_prior_itc_sl_20",		"20-yr straight line federal depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_sl_20",		"20-yr straight line depreciation federal ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_qual_sl_20",		"20-yr straight line federal percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_amount_sl_20",		"20-yr straight line depreciation ITC basis from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_percent_sl_20",		"20-yr straight line depreciation ITC basis disallowance from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_fixed_amount_sl_20",		"20-yr straight line depreciation ITC basis from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_fixed_sl_20",		"20-yr straight line depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_sl_20",		"20-yr straight line federal basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_sl_20",		"20-yr straight line federal basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_after_itc_sl_20",		"20-yr straight line federal depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_first_year_bonus_sl_20",		"20-yr straight line federal first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_sl_20",		"20-yr straight line federal depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_sl_39",		"39-yr straight line federal percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_sl_39",		"39-yr straight line depreciation federal and federal allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_ibi_reduc_sl_39",		"39-yr straight line federal ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_cbi_reduc_sl_39",		"39-yr straight line federal cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_prior_itc_sl_39",		"39-yr straight line federal depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_sl_39",		"39-yr straight line depreciation federal ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_qual_sl_39",		"39-yr straight line federal percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_amount_sl_39",		"39-yr straight line depreciation ITC basis from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_percent_sl_39",		"39-yr straight line depreciation ITC basis disallowance from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_fixed_amount_sl_39",		"39-yr straight line depreciation ITC basis from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_fixed_sl_39",		"39-yr straight line depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_sl_39",		"39-yr straight line federal basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_sl_39",		"39-yr straight line federal basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_after_itc_sl_39",		"39-yr straight line federal depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_first_year_bonus_sl_39",		"39-yr straight line federal first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_sl_39",		"39-yr straight line federal depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_custom",		"Custom straight line federal percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_custom",		"Custom straight line depreciation federal and federal allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_ibi_reduc_custom",		"Custom straight line federal ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_cbi_reduc_custom",		"Custom straight line federal cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_prior_itc_custom",		"Custom straight line federal depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_custom",		"Custom straight line depreciation federal ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_qual_custom",		"Custom straight line federal percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_amount_custom",		"Custom straight line depreciation ITC basis from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_percent_custom",		"Custom straight line depreciation ITC basis disallowance from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_fixed_amount_custom",		"Custom straight line depreciation ITC basis from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_fixed_custom",		"Custom straight line depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_custom",		"Custom straight line federal basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_custom",		"Custom straight line federal basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_after_itc_custom",		"Custom straight line federal depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_first_year_bonus_custom",		"Custom straight line federal first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_custom",		"Custom straight line federal depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/*1*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_total",		"Total federal percent of total depreciable basis",	"%", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_alloc_total",		"Total depreciation federal and federal allocation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*2*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_ibi_reduc_total",		"Total federal ibi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*3*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_cbi_reduc_total",		"Total federal cbi reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*4*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_prior_itc_total",		"Total federal depreciation basis prior ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_qual_total",		"Total federal ITC adj qualifying costs",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*5*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_qual_total",		"Total federal percent of qualifying costs",	"%", "",	  "DHF",             "*",					  "",     			        "" },
/*6*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_percent_amount_total",		"Total depreciation ITC basis from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_percent_total",		"Total depreciation ITC basis disallowance from federal percentage",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*7*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_fixed_amount_total",		"Total depreciation ITC basis from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_disallow_fed_fixed_total",		"Total depreciation ITC basis disallowance from federal fixed amount",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*8*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_sta_reduction_total",		"Total federal basis state ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*9*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_itc_fed_reduction_total",		"Total federal basis federal ITC reduciton",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*10*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_after_itc_total",		"Total federal depreciation basis after ITC reduction",	"$", "",	  "DHF",             "*",					  "",     			        "" },
/*11*/	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_first_year_bonus_total",		"Total federal first year bonus depreciation",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_total",		"Total federal depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },


	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_percent_total",		"federal ITC percent total",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "itc_fed_fixed_total",		"federal ITC fixed total",	"$", "",	  "DHF",             "*",					  "",     			        "" },



/* depreciation bases method - added with version 4.1    0=5-yrMacrs, 1=proportional */
	{ SSC_INPUT,        SSC_NUMBER,      "depr_stabas_method",    "Method of state depreciation reduction",     "",      "0=5yr MACRS,1=Proportional",                      "DHF",      "?=0",                       "INTEGER,MIN=0,MAX=1",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "depr_fedbas_method",    "Method of federal depreciation reduction",     "",      "0=5yr MACRS,1=Proportional",                      "DHF",      "?=0",                       "INTEGER,MIN=0,MAX=1",                                         "" },

/* State depreciation table */
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_stabas_total",		"Total state depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },

/* Federal depreciation table */
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_macrs_5",		"5-yr MACRS federal depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_macrs_15",		"15-yr MACRS federal depreciation basis",	"$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_sl_5",		"5-yr straight line federal depreciation basis",	"$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_sl_15",		"15-yr straight line federal depreciation basis","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_sl_20",		"20-yr straight line federal depreciation basis","$", "",  "DHF",             "*",						  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_sl_39",		"39-yr straight line federal depreciation basis","$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_custom",		"Custom federal depreciation basis","$", "",  "DHF",             "*",					  "",     			        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "depr_fedbas_total",		"Total federal depreciation basis",	"$", "",	  "DHF",             "*",					  "",     			        "" },


/* State taxes */

	/* intermediate outputs for validation */
	{ SSC_OUTPUT,       SSC_NUMBER,      "cash_for_debt_service",   "Cash avaialble for debt service",   "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "pv_cafds", "Present value of cash avaialble for debt service","$", "",				  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "size_of_debt",			"Total debt",	"",	 "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "size_of_equity",			"Total equity",	"",	 "",					  "DHF",			 "*",                         "",                             "" },

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

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_om_opt_fuel_1_expense",       "O&M Optional Fuel 1 expense",                   "$",            "",                      "Cashloan",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_om_opt_fuel_2_expense",       "O&M Optional Fuel 2 expense",                   "$",            "",                      "Cashloan",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_property_tax_assessed_value","Property tax net assessed value", "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_property_tax_expense",  "Property tax expense",               "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_insurance_expense",     "Insurance expense",                  "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_operating_expenses",    "Total operating expense",            "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_net_salvage_value",    "Salvage value",            "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_total_revenue",    "Total revenue",            "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_ebitda",    "EBITDA",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_reserve_debtservice",    "Debt service reserve",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_reserve_om",    "Working capital reserve",       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
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
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_wcra",    "(Increase)/Decrease in working capital reserve account",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
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
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_return_aftertax_max_irr",    "After-tax project maximum IRR",  "%", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_project_return_aftertax_npv",    "After-tax project cumulative NPV",  "$", "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

		
	{ SSC_OUTPUT,        SSC_NUMBER,      "cbi_total_fed",             "Total federal CBI incentive income",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "cbi_total_sta",             "Total state CBI incentive income",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "cbi_total_oth",             "Total other CBI incentive income",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "cbi_total_uti",             "Total utility CBI incentive income",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "cbi_total",             "Total CBI incentive income",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "cbi_statax_total",             "Total state taxable CBI incentive income",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "cbi_fedtax_total",             "Total federal taxable CBI incentive income",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "ibi_total_fed",             "Total federal IBI incentive income",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "ibi_total_sta",             "Total state IBI incentive income",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "ibi_total_oth",             "Total other IBI incentive income",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "ibi_total_uti",             "Total utility IBI incentive income",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "ibi_total",             "Total IBI incentive income",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "ibi_statax_total",             "Total state taxable IBI incentive income",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "ibi_fedtax_total",             "Total federal taxable IBI incentive income",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "itc_total_fed",             "Total federal ITC ",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "itc_total_sta",             "Total state ITC ",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "itc_total",             "Total ITC ",         "$",            "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_pbi_total_fed",             "Total federal PBI incentive income",         "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_pbi_total_sta",             "Total state PBI incentive income",         "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_pbi_total_oth",             "Total other PBI incentive income",         "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_pbi_total_uti",             "Total utility PBI incentive income",         "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_pbi_total",             "Total PBI incentive income",         "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_pbi_statax_total",             "Total state taxable PBI incentive income",         "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_pbi_fedtax_total",             "Total federal taxable PBI incentive income",         "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_ptc_fed",               "Federal PTC income",                 "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_ptc_sta",               "State PTC income",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },


/* state depreciation and tax */
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_macrs_5",         "State depreciation from 5-yr MACRS",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_macrs_15",         "State depreciation from 15-yr MACRS",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_sl_5",         "State depreciation from 5-yr straight line",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_sl_15",         "State depreciation from 15-yr straight line",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_sl_20",         "State depreciation from 20-yr straight line",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_sl_39",         "State depreciation from 39-yr straight line",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_stadepr_custom",         "State depreciation from custom",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
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
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_custom",         "Federal depreciation from custom",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_me1",         "Federal depreciation from major equipment 1",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_me2",         "Federal depreciation from major equipment 2",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_me3",         "Federal depreciation from major equipment 3",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_feddepr_total",         "Total federal tax depreciation",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_fedtax_income_prior_incentives", "Federal tax income prior incentives",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_fedtax_taxable_incentives", "Federal taxable incentives",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_fedtax_income_with_incentives", "Federal tax income with incentives",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_fedtax",				"Federal tax benefit/(liability)",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },


	// metrics table 
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_first_year_energy_net",    "Net Annual Energy",  "", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_capacity_factor",    "Capacity factor",  "", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_kwh_per_kw",    "First year kWh/kW",  "", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_debt_fraction",    "Debt fraction",  "", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_flip_target_year",    "IRR target year",  "", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_flip_target_irr",    "IRR target",  "", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_flip_actual_year",    "IRR actual year",  "", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_flip_actual_irr",    "IRR in target year",  "", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "sv_lcoe_real",                "Real LCOE",                          "",    "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "sv_lcoe_nom",                 "Nominal LCOE",                       "",    "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "sv_first_year_ppa",                 "PPA price",                       "",    "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "sv_ppa_escalation",                 "PPA price escalation",                       "",    "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_project_return_aftertax_irr",    "After-tax IRR",  "", "",                      "DHF",      "*",                     "",                "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "sv_project_return_aftertax_npv",    "After-tax NPV",  "", "",                      "DHF",      "*",                     "",                "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "sv_npv_ppa_revenue",                "NPV of PPA revenue",                          "",    "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "sv_npv_energy_nom",                "NPV of net annual energy (nominal)",                          "",    "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "sv_npv_energy_real",                "NPV of net annual energy (real)",                          "",    "",                      "DHF",      "*",                       "",                                         "" },


	{ SSC_OUTPUT,        SSC_NUMBER,     "present_value_oandm",                      "Present value of O and M",				   "$",            "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "present_value_oandm_nonfuel",              "Present value of non-fuel O and M",				   "$",            "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "present_value_fuel",                      "Present value of fuel O and M",				   "$",            "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "present_value_insandproptax",                      "Present value of Insurance and Prop Tax",				   "$",            "",                      "DHF",      "*",                       "",                                         "" },



	{ SSC_OUTPUT,        SSC_NUMBER,     "sv_lcoptc_fed_real",                "Levelized Federal PTC (real)",                          "",    "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "sv_lcoptc_fed_nom",                 "Levelized Federal PTC (nominal)",                       "",    "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "sv_lcoptc_sta_real",                "Levelized State PTC (real)",                          "",    "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "sv_lcoptc_sta_nom",                 "Levelized State PTC (nominal)",                       "",    "",                      "DHF",      "*",                       "",                                         "" },

	{ SSC_OUTPUT,        SSC_NUMBER,     "sv_wacc",                "Weighted Average Cost of Capital (WACC)",                          "",    "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "sv_effective_tax_rate",                 "Effective Tax Rate",                       "",    "",                      "DHF",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "sv_analysis_period_irr",                "Analysis Period IRR",                          "",    "",                      "DHF",      "*",                       "",                                         "" },





var_info_invalid };

extern var_info
	vtab_standard_financial[],
	vtab_oandm[],
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

	CF_om_opt_fuel_2_expense,
	CF_om_opt_fuel_1_expense,

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
	CF_sta_tax_savings,
	
	CF_fed_depr_sched,
	CF_fed_depreciation,
	CF_fed_incentive_income_less_deductions,
	CF_fed_taxable_income_less_deductions,
	CF_fed_tax_savings,

	// Dispatch
	CF_TOD1Energy,
	CF_TOD2Energy,
	CF_TOD3Energy,
	CF_TOD4Energy,
	CF_TOD5Energy,
	CF_TOD6Energy,
	CF_TOD7Energy,
	CF_TOD8Energy,
	CF_TOD9Energy,

	CF_TODJanEnergy,
	CF_TODFebEnergy,
	CF_TODMarEnergy,
	CF_TODAprEnergy,
	CF_TODMayEnergy,
	CF_TODJunEnergy,
	CF_TODJulEnergy,
	CF_TODAugEnergy,
	CF_TODSepEnergy,
	CF_TODOctEnergy,
	CF_TODNovEnergy,
	CF_TODDecEnergy,

	CF_TOD1JanEnergy,
	CF_TOD1FebEnergy,
	CF_TOD1MarEnergy,
	CF_TOD1AprEnergy,
	CF_TOD1MayEnergy,
	CF_TOD1JunEnergy,
	CF_TOD1JulEnergy,
	CF_TOD1AugEnergy,
	CF_TOD1SepEnergy,
	CF_TOD1OctEnergy,
	CF_TOD1NovEnergy,
	CF_TOD1DecEnergy,

	CF_TOD2JanEnergy,
	CF_TOD2FebEnergy,
	CF_TOD2MarEnergy,
	CF_TOD2AprEnergy,
	CF_TOD2MayEnergy,
	CF_TOD2JunEnergy,
	CF_TOD2JulEnergy,
	CF_TOD2AugEnergy,
	CF_TOD2SepEnergy,
	CF_TOD2OctEnergy,
	CF_TOD2NovEnergy,
	CF_TOD2DecEnergy,

	CF_TOD3JanEnergy,
	CF_TOD3FebEnergy,
	CF_TOD3MarEnergy,
	CF_TOD3AprEnergy,
	CF_TOD3MayEnergy,
	CF_TOD3JunEnergy,
	CF_TOD3JulEnergy,
	CF_TOD3AugEnergy,
	CF_TOD3SepEnergy,
	CF_TOD3OctEnergy,
	CF_TOD3NovEnergy,
	CF_TOD3DecEnergy,

	CF_TOD4JanEnergy,
	CF_TOD4FebEnergy,
	CF_TOD4MarEnergy,
	CF_TOD4AprEnergy,
	CF_TOD4MayEnergy,
	CF_TOD4JunEnergy,
	CF_TOD4JulEnergy,
	CF_TOD4AugEnergy,
	CF_TOD4SepEnergy,
	CF_TOD4OctEnergy,
	CF_TOD4NovEnergy,
	CF_TOD4DecEnergy,

	CF_TOD5JanEnergy,
	CF_TOD5FebEnergy,
	CF_TOD5MarEnergy,
	CF_TOD5AprEnergy,
	CF_TOD5MayEnergy,
	CF_TOD5JunEnergy,
	CF_TOD5JulEnergy,
	CF_TOD5AugEnergy,
	CF_TOD5SepEnergy,
	CF_TOD5OctEnergy,
	CF_TOD5NovEnergy,
	CF_TOD5DecEnergy,

	CF_TOD6JanEnergy,
	CF_TOD6FebEnergy,
	CF_TOD6MarEnergy,
	CF_TOD6AprEnergy,
	CF_TOD6MayEnergy,
	CF_TOD6JunEnergy,
	CF_TOD6JulEnergy,
	CF_TOD6AugEnergy,
	CF_TOD6SepEnergy,
	CF_TOD6OctEnergy,
	CF_TOD6NovEnergy,
	CF_TOD6DecEnergy,

	CF_TOD7JanEnergy,
	CF_TOD7FebEnergy,
	CF_TOD7MarEnergy,
	CF_TOD7AprEnergy,
	CF_TOD7MayEnergy,
	CF_TOD7JunEnergy,
	CF_TOD7JulEnergy,
	CF_TOD7AugEnergy,
	CF_TOD7SepEnergy,
	CF_TOD7OctEnergy,
	CF_TOD7NovEnergy,
	CF_TOD7DecEnergy,

	CF_TOD8JanEnergy,
	CF_TOD8FebEnergy,
	CF_TOD8MarEnergy,
	CF_TOD8AprEnergy,
	CF_TOD8MayEnergy,
	CF_TOD8JunEnergy,
	CF_TOD8JulEnergy,
	CF_TOD8AugEnergy,
	CF_TOD8SepEnergy,
	CF_TOD8OctEnergy,
	CF_TOD8NovEnergy,
	CF_TOD8DecEnergy,

	CF_TOD9JanEnergy,
	CF_TOD9FebEnergy,
	CF_TOD9MarEnergy,
	CF_TOD9AprEnergy,
	CF_TOD9MayEnergy,
	CF_TOD9JunEnergy,
	CF_TOD9JulEnergy,
	CF_TOD9AugEnergy,
	CF_TOD9SepEnergy,
	CF_TOD9OctEnergy,
	CF_TOD9NovEnergy,
	CF_TOD9DecEnergy,

	CF_TOD1Revenue,
	CF_TOD2Revenue,
	CF_TOD3Revenue,
	CF_TOD4Revenue,
	CF_TOD5Revenue,
	CF_TOD6Revenue,
	CF_TOD7Revenue,
	CF_TOD8Revenue,
	CF_TOD9Revenue,

	CF_TODJanRevenue,
	CF_TODFebRevenue,
	CF_TODMarRevenue,
	CF_TODAprRevenue,
	CF_TODMayRevenue,
	CF_TODJunRevenue,
	CF_TODJulRevenue,
	CF_TODAugRevenue,
	CF_TODSepRevenue,
	CF_TODOctRevenue,
	CF_TODNovRevenue,
	CF_TODDecRevenue,

	CF_revenue_monthly_firstyear,	
	CF_energy_net_monthly_firstyear,	
	CF_revenue_monthly_firstyear_TOD1,	
	CF_energy_net_monthly_firstyear_TOD1,	
	CF_revenue_monthly_firstyear_TOD2,	
	CF_energy_net_monthly_firstyear_TOD2,	
	CF_revenue_monthly_firstyear_TOD3,	
	CF_energy_net_monthly_firstyear_TOD3,	
	CF_revenue_monthly_firstyear_TOD4,	
	CF_energy_net_monthly_firstyear_TOD4,	
	CF_revenue_monthly_firstyear_TOD5,	
	CF_energy_net_monthly_firstyear_TOD5,	
	CF_revenue_monthly_firstyear_TOD6,	
	CF_energy_net_monthly_firstyear_TOD6,	
	CF_revenue_monthly_firstyear_TOD7,	
	CF_energy_net_monthly_firstyear_TOD7,	
	CF_revenue_monthly_firstyear_TOD8,	
	CF_energy_net_monthly_firstyear_TOD8,	
	CF_revenue_monthly_firstyear_TOD9,	
	CF_energy_net_monthly_firstyear_TOD9,	


	CF_Availability,
	CF_Degradation,

	CF_Recapitalization,
	CF_Recapitalization_boolean,

	CF_max };



class cm_singleowner : public compute_module
{
private:
	util::matrix_t<double> cf;

public:
	cm_singleowner()
	{
		add_var_info( vtab_standard_financial );
		add_var_info( vtab_oandm );
		add_var_info( vtab_tax_credits );
		add_var_info( vtab_payment_incentives );
				
		add_var_info( _cm_vtab_singleowner );
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

		// In conjunction with SAM - take installed costs and salestax costs (for deducting if necessary)
		double cost_salestax = as_double("cost_salestax");
		double cost_prefinancing = as_double("cost_prefinancing");

		// use DHF named range names for variables whenever possible
		double nameplate = as_double("system_capacity");
		double year1_fuel_use = as_double("annual_fuel_usage"); // kWht
	
		double assessed_frac = as_double("prop_tax_cost_assessed_percent")*0.01;
		double salvage_value_frac = as_double("salvage_percentage")*0.01;
		double salvage_value = salvage_value_frac * cost_prefinancing;

		double cost_debt_closing = as_double("cost_debt_closing");
		double cost_debt_fee_frac = as_double("cost_debt_fee")*0.01;
		double cost_other_financing = as_double("cost_other_financing");

		double constr_total_financing = as_double("constr_total_financing");

		int ppa_mode = as_integer("ppa_soln_mode");

		// general financial expenses and incentives - stdlib?
		// precompute expenses from annual schedules or value+escalation
		escal_or_annual( CF_om_fixed_expense, nyears, "om_fixed", inflation_rate, 1.0, false, as_double("om_fixed_escal")*0.01 );
		escal_or_annual( CF_om_production_expense, nyears, "om_production", inflation_rate, 0.001, false, as_double("om_production_escal")*0.01 );  
		escal_or_annual( CF_om_capacity_expense, nyears, "om_capacity", inflation_rate, 1.0, false, as_double("om_capacity_escal")*0.01 );  
		escal_or_annual( CF_om_fuel_expense, nyears, "om_fuel_cost", inflation_rate, as_double("system_heat_rate")*0.001, false, as_double("om_fuel_cost_escal")*0.01 );
		
		escal_or_annual( CF_om_opt_fuel_1_expense, nyears, "om_opt_fuel_1_cost", inflation_rate, 1.0, false, as_double("om_opt_fuel_1_cost_escal")*0.01 );  
		escal_or_annual( CF_om_opt_fuel_2_expense, nyears, "om_opt_fuel_2_cost", inflation_rate, 1.0, false, as_double("om_opt_fuel_2_cost_escal")*0.01 );  

		double om_opt_fuel_1_usage = as_double("om_opt_fuel_1_usage");
		double om_opt_fuel_2_usage = as_double("om_opt_fuel_2_usage");
		
		// initialize energy

		int i=0;
		//double first_year_energy = as_double("energy_net");
		double first_year_energy = 0.0;
		size_t count_avail = 0;
		ssc_number_t *avail = 0;
		avail = as_array("energy_availability", &count_avail);
		size_t count_degrad = 0;
		ssc_number_t *degrad = 0;
		degrad = as_array("energy_degradation", &count_degrad);

		// degradation and availability set in cmod_annualoutput
		for (i=0;i<nyears && i<(int)count_degrad;i++) cf.at(CF_Degradation,i+1) = degrad[i];
		for (i=0;i<nyears && i<(int)count_avail;i++) cf.at(CF_Availability,i+1) = avail[i];



		// dispatch
		if (as_integer("system_use_lifetime_output"))
		{
			compute_lifetime_dispatch_output(nyears);
		}
		else
		{
			size_t count_energy = 0;
			ssc_number_t *energy = 0;
			energy = as_array("energy_net_hourly", &count_energy);
			for (i=0;i<(int)count_energy;i++) first_year_energy += energy[i]; // sum up hourly kWh to get total annual kWh first year production

			for (i=1;i<=nyears;i++)
				cf.at(CF_energy_net,i) = first_year_energy * cf.at(CF_Degradation,i) * cf.at(CF_Availability,i);
			compute_dispatch_output(nyears);
		}

		first_year_energy = cf.at(CF_energy_net, 1);


		double dispatch_factor1 = as_double("dispatch_factor1");
		double dispatch_factor2 = as_double("dispatch_factor2");
		double dispatch_factor3 = as_double("dispatch_factor3");
		double dispatch_factor4 = as_double("dispatch_factor4");
		double dispatch_factor5 = as_double("dispatch_factor5");
		double dispatch_factor6 = as_double("dispatch_factor6");
		double dispatch_factor7 = as_double("dispatch_factor7");
		double dispatch_factor8 = as_double("dispatch_factor8");
		double dispatch_factor9 = as_double("dispatch_factor9");

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_om_production_expense,i) *= cf.at(CF_energy_net,i);
			cf.at(CF_om_capacity_expense,i) *= nameplate;
			cf.at(CF_om_fuel_expense,i) *= year1_fuel_use;

			cf.at(CF_om_opt_fuel_1_expense,i) *= om_opt_fuel_1_usage;
			cf.at(CF_om_opt_fuel_2_expense,i) *= om_opt_fuel_2_usage;
		}


		double ppa = as_double("ppa_price_input")*100.0; // either initial guess for ppa_mode=1 or final ppa for pp_mode=0

		double property_tax_assessed_value = cost_prefinancing * as_double("prop_tax_cost_assessed_percent") * 0.01;
		double property_tax_decline_percentage = as_double("prop_tax_assessed_decline");
		double property_tax_rate = as_double("property_tax_rate")*0.01;
		double insurance_rate = as_double("insurance_rate")*0.01;
		double months_working_reserve_frac = as_integer("months_working_reserve") / 12.0;
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
		depreciation_sched_custom(CF_custom_frac,nyears,"depr_custom_schedule");

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
				+ cf.at(CF_om_fixed_expense,i)
				+ cf.at(CF_om_production_expense,i)
				+ cf.at(CF_om_capacity_expense,i)
				+ cf.at(CF_om_fuel_expense,i)
				+ cf.at(CF_om_opt_fuel_1_expense,i)
				+ cf.at(CF_om_opt_fuel_2_expense,i)
				+ cf.at(CF_property_tax_expense,i)
				+ cf.at(CF_insurance_expense,i)
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
		double equity_investment;

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

		// itc fixed
		double itc_fed_amount = as_double("itc_fed_amount");
		double itc_sta_amount = as_double("itc_sta_amount");

		// itc percent - max value used for comparison to qualifying costs
		double itc_fed_frac = as_double("itc_fed_percent")*0.01;
		double itc_fed_per;
		double itc_sta_frac = as_double("itc_sta_percent")*0.01;
		double itc_sta_per;

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
		double itc_fed_total;
		double itc_sta_total;
		double itc_total;

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

		double itc_sta_percent_maxvalue = as_double("itc_sta_percent_maxvalue");

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

		double itc_fed_percent_maxvalue = as_double("itc_fed_percent_maxvalue");

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

/***************** begin iterative solution *********************************************************************/

	do
	{

		flip_year=-1;
		cash_for_debt_service=0;
		pv_cafds=0;
		size_of_debt=0;
		if (ppa_interval_found)	ppa = (w0*x1+w1*x0)/(w0 + w1);

		// debt pre calculation
		for (i=1; i<=nyears; i++)
		{			
		// Project partial income statement			
			// energy_value = DHF Total PPA Revenue
			cf.at(CF_ppa_price,i) = ppa * pow( 1 + ppa_escalation, i-1 ); // ppa_mode==1
//			cf.at(CF_energy_value,i) = cf.at(CF_energy_net,i) * cf.at(CF_ppa_price,i) /100.0;
			// dispatch
			cf.at(CF_energy_value,i) = cf.at(CF_ppa_price,i) /100.0 * (
				cf.at(CF_TOD1Energy,i) * dispatch_factor1 +
				cf.at(CF_TOD2Energy,i) * dispatch_factor2 +
				cf.at(CF_TOD3Energy,i) * dispatch_factor3 +
				cf.at(CF_TOD4Energy,i) * dispatch_factor4 +
				cf.at(CF_TOD5Energy,i) * dispatch_factor5 +
				cf.at(CF_TOD6Energy,i) * dispatch_factor6 +
				cf.at(CF_TOD7Energy,i) * dispatch_factor7 +
				cf.at(CF_TOD8Energy,i) * dispatch_factor8 +
				cf.at(CF_TOD9Energy,i) * dispatch_factor9			);
			// PBI
			// total revenue
			cf.at(CF_total_revenue,i) = cf.at(CF_energy_value,i) +
				pbi_fed_for_ds_frac * cf.at(CF_pbi_fed,i) +
				pbi_sta_for_ds_frac * cf.at(CF_pbi_sta,i) +
				pbi_uti_for_ds_frac * cf.at(CF_pbi_uti,i) +
				pbi_oth_for_ds_frac * cf.at(CF_pbi_oth,i) +
				cf.at(CF_net_salvage_value,i);

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
			cost_debt_closing + 
			cost_debt_fee_frac * size_of_debt +
			cost_other_financing +
			cf.at(CF_reserve_debtservice,0) +
			constr_total_financing +
			cf.at(CF_reserve_om,0);

		cost_installed = cost_prefinancing + cost_financing;

		depr_alloc_total = depr_alloc_total_frac * cost_installed;
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

		itc_sta_per = min(itc_sta_percent_maxvalue,itc_sta_frac*itc_sta_qual_total);

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

		itc_fed_per = min(itc_fed_percent_maxvalue,itc_fed_frac*itc_fed_qual_total);

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

		itc_fed_total = itc_fed_amount + itc_fed_per;
		itc_sta_total = itc_sta_amount + itc_sta_per;
		itc_total = itc_fed_total + itc_sta_total;

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

		purchase_of_property = -cost_installed + cf.at(CF_reserve_debtservice,0) + cf.at(CF_reserve_om,0);
		issuance_of_equity = cost_installed - (size_of_debt + ibi_total + cbi_total);	
		
		equity_investment = -issuance_of_equity;

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

			cf.at(CF_project_financing_activities,i) = -cf.at(CF_debt_payment_principal,i);
			if (i==0) cf.at(CF_project_financing_activities,i) += issuance_of_equity + size_of_debt + ibi_total + cbi_total;

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
			cf.at(CF_statax,i) = -state_tax_rate * cf.at(CF_statax_income_with_incentives,i); 

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
				cf.at(CF_ptc_sta,i);
			if (i==1) cf.at(CF_fedtax_income_prior_incentives,i) += itc_sta_total;


			// pbi in ebitda - so remove if non-taxable
			// 5/1/11
			cf.at(CF_fedtax_income_with_incentives,i) = cf.at(CF_fedtax_income_prior_incentives,i) + cf.at(CF_fedtax_taxable_incentives,i);
			cf.at(CF_fedtax,i) = -federal_tax_rate * cf.at(CF_fedtax_income_with_incentives,i); 

			cf.at(CF_project_return_aftertax,i) = 
				cf.at(CF_project_return_aftertax_cash,i) +
				cf.at(CF_ptc_fed,i) + cf.at(CF_ptc_sta,i) +
				cf.at(CF_statax,i) + cf.at(CF_fedtax,i);
			if (i==1) cf.at(CF_project_return_aftertax,i) += itc_total;

			cf.at(CF_project_return_aftertax_irr,i) = irr(CF_project_return_aftertax,i)*100.0;
			cf.at(CF_project_return_aftertax_max_irr,i) = max(cf.at(CF_project_return_aftertax_max_irr,i-1),cf.at(CF_project_return_aftertax_irr,i));
			cf.at(CF_project_return_aftertax_npv,i) = npv(CF_project_return_aftertax,i,nom_discount_rate) +  cf.at(CF_project_return_aftertax,0) ;

			if (flip_year <=0) 
			{
				double residual = cf.at(CF_project_return_aftertax_irr, i) - flip_target_percent;
				if ( ( cf.at(CF_project_return_aftertax_max_irr,i-1) < flip_target_percent ) &&  (  fabs( residual ) < ppa_soln_tolerance ) ) 
				{
					flip_year=i;
					cf.at(CF_project_return_aftertax_max_irr,i)=flip_target_percent; //within tolerance so pre-flip and post-flip percentages applied correctly
				}
				else if ( ( cf.at(CF_project_return_aftertax_max_irr,i-1) < flip_target_percent ) &&  ( cf.at(CF_project_return_aftertax_max_irr,i) >= flip_target_percent ) ) flip_year=i;
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
			solved = (( fabs( residual )/resid_denom < ppa_soln_tolerance ) || ( fabs(x0-x1)/ppa_denom < ppa_soln_tolerance) );
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
				irr_weighting_factor = fabs(itnpv_target);
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
					if (fabs(x0-x1)<ppa_soln_tolerance) x0 = x1-2*ppa_soln_tolerance;
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

	assign("sv_flip_target_year", var_data((ssc_number_t) flip_target_year ));
	assign("sv_flip_target_irr", var_data((ssc_number_t)  flip_target_percent ));
	assign("sv_flip_actual_year", var_data((ssc_number_t) flip_year));
	double actual_flip_irr = 0;
	if (flip_year > -1) actual_flip_irr = cf.at(CF_project_return_aftertax_irr, flip_target_year);
	assign("sv_flip_actual_irr", var_data((ssc_number_t) actual_flip_irr ));

	// LCOE
	double npv_ppa_revenue = npv(CF_energy_value,nyears,nom_discount_rate);
	double npv_energy_nom = npv(CF_energy_net,nyears,nom_discount_rate);
	double lcoe_nom = 0;
	if (npv_energy_nom != 0) lcoe_nom = npv_ppa_revenue / npv_energy_nom * 100.0;
	double lcoe_real = 0;
	double npv_energy_real = npv(CF_energy_net,nyears,disc_real);
	if (npv_energy_real != 0) lcoe_real = npv_ppa_revenue / npv_energy_real * 100.0;


	double npv_fed_ptc = npv(CF_ptc_fed,nyears,nom_discount_rate);
	double npv_sta_ptc = npv(CF_ptc_sta,nyears,nom_discount_rate);

	double effective_tax_rate = state_tax_rate + (1.0-state_tax_rate)*federal_tax_rate;
	npv_fed_ptc /= (1.0 - effective_tax_rate);
	npv_sta_ptc /= (1.0 - effective_tax_rate);


	double lcoptc_fed_nom=0.0;
	if (npv_energy_nom != 0) lcoptc_fed_nom = npv_fed_ptc / npv_energy_nom * 100.0;
	double lcoptc_fed_real=0.0;
	if (npv_energy_real != 0) lcoptc_fed_real = npv_fed_ptc / npv_energy_real * 100.0;

	double lcoptc_sta_nom=0.0;
	if (npv_energy_nom != 0) lcoptc_sta_nom = npv_sta_ptc / npv_energy_nom * 100.0;
	double lcoptc_sta_real=0.0;
	if (npv_energy_real != 0) lcoptc_sta_real = npv_sta_ptc / npv_energy_real * 100.0;

	assign("sv_lcoptc_fed_nom", var_data((ssc_number_t) lcoptc_fed_nom));
	assign("sv_lcoptc_fed_real", var_data((ssc_number_t) lcoptc_fed_real));
	assign("sv_lcoptc_sta_nom", var_data((ssc_number_t) lcoptc_sta_nom));
	assign("sv_lcoptc_sta_real", var_data((ssc_number_t) lcoptc_sta_real));

	double analysis_period_irr = 0.0;
	analysis_period_irr = cf.at(CF_project_return_aftertax_irr, nyears)/100.0; //fraction for calculations

	double debt_fraction =0.0;
	if (cost_installed > 0) debt_fraction = size_of_debt / cost_installed;


	double wacc = 0.0;
	wacc = (1.0-debt_fraction)*analysis_period_irr + debt_fraction*term_int_rate*(1.0-effective_tax_rate);

	// percentages
	debt_fraction *= 100.0;
	wacc *= 100.0;
	effective_tax_rate *= 100.0;
	analysis_period_irr *= 100.0;


	assign("sv_debt_fraction", var_data((ssc_number_t) debt_fraction ));
	assign("sv_wacc", var_data( (ssc_number_t) wacc));
	assign("sv_effective_tax_rate", var_data( (ssc_number_t) effective_tax_rate));
	assign("sv_analysis_period_irr", var_data( (ssc_number_t) analysis_period_irr));



	assign("sv_npv_ppa_revenue", var_data( (ssc_number_t) npv_ppa_revenue));
	assign("sv_npv_energy_nom", var_data( (ssc_number_t) npv_energy_nom));
	assign("sv_npv_energy_real", var_data( (ssc_number_t) npv_energy_real));

		assign( "cf_length", var_data( (ssc_number_t) nyears+1 ));

		assign( "salvage_value", var_data((ssc_number_t)salvage_value));

		assign( "prop_tax_assessed_value", var_data((ssc_number_t)( assessed_frac * cost_prefinancing )));

		assign( "cost_prefinancing", var_data((ssc_number_t) cost_prefinancing ) );
		assign( "cost_prefinancingperwatt", var_data((ssc_number_t)( cost_prefinancing / nameplate / 1000.0 ) ));

		assign( "cost_salestax", var_data((ssc_number_t)cost_salestax ) );
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

		assign( "cost_installed", var_data((ssc_number_t) cost_installed ) );
		assign( "size_of_equity", var_data((ssc_number_t) (cost_installed - ibi_total - cbi_total - size_of_debt)) );
		assign( "cost_installedperwatt", var_data((ssc_number_t)( cost_installed / nameplate / 1000.0 ) ));

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
		
		assign("ppa_price", var_data((ssc_number_t) ppa));
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
		assign("itc_total_fed", var_data((ssc_number_t) itc_fed_total));
		assign("itc_total_sta", var_data((ssc_number_t) itc_sta_total));
		assign("itc_total", var_data((ssc_number_t) itc_total));

		assign("sv_first_year_energy_net", var_data((ssc_number_t) cf.at(CF_energy_net,1)));
		double kWhperkW = 0.0;
		// add to address geothermal capacity factor issue 4/13/13
		if (as_integer("system_use_lifetime_output"))
		{
			if ((nameplate > 0) && (nyears>0))
			{
				for (int i=0;i<=nyears;i++)
					kWhperkW += cf.at(CF_energy_net,i);
				kWhperkW /= (nyears * nameplate);
			}
		}
		else
		{
			if (nameplate > 0) kWhperkW = cf.at(CF_energy_net,1) / nameplate;
		}
		assign( "sv_capacity_factor", var_data((ssc_number_t) (kWhperkW / 87.6)) );
		assign( "sv_kwh_per_kw", var_data((ssc_number_t) kWhperkW) );

		assign("sv_lcoe_nom", var_data((ssc_number_t) lcoe_nom));
		assign("sv_lcoe_real", var_data((ssc_number_t) lcoe_real));
		assign("ppa_price", var_data((ssc_number_t) ppa));
		assign("sv_ppa_escalation", var_data((ssc_number_t) (ppa_escalation *100.0) ));
		assign("sv_first_year_ppa", var_data((ssc_number_t) ppa));


		assign("issuance_of_equity", var_data((ssc_number_t) issuance_of_equity));
		

		assign("sv_project_return_aftertax_irr", var_data((ssc_number_t)  (irr(CF_project_return_aftertax,nyears)*100.0)));
		assign("sv_project_return_aftertax_npv", var_data((ssc_number_t)  (npv(CF_project_return_aftertax,nyears,nom_discount_rate) +  cf.at(CF_project_return_aftertax,0)) ));


		// cash flow line items

		
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
		save_cf( CF_om_opt_fuel_1_expense, nyears, "cf_om_opt_fuel_1_expense" );
		save_cf( CF_om_opt_fuel_2_expense, nyears, "cf_om_opt_fuel_2_expense" );
		save_cf( CF_property_tax_assessed_value, nyears, "cf_property_tax_assessed_value" );
		save_cf( CF_property_tax_expense, nyears, "cf_property_tax_expense" );
		save_cf( CF_insurance_expense, nyears, "cf_insurance_expense" );
		save_cf( CF_operating_expenses, nyears, "cf_operating_expenses" );
		save_cf( CF_ebitda, nyears, "cf_ebitda" );
		save_cf( CF_net_salvage_value, nyears, "cf_net_salvage_value" );
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

		// dispatch
		if (as_integer("system_use_lifetime_output"))
			process_lifetime_dispatch_output(nyears);
		else
			process_dispatch_output(nyears);

		save_cf( CF_Recapitalization, nyears, "cf_recapitalization" );
		
		// dispatch energy
		save_cf( CF_TODJanEnergy, nyears, "cf_energy_net_jan");
		save_cf( CF_TODFebEnergy, nyears, "cf_energy_net_feb");
		save_cf( CF_TODMarEnergy, nyears, "cf_energy_net_mar");
		save_cf( CF_TODAprEnergy, nyears, "cf_energy_net_apr");
		save_cf( CF_TODMayEnergy, nyears, "cf_energy_net_may");
		save_cf( CF_TODJunEnergy, nyears, "cf_energy_net_jun");
		save_cf( CF_TODJulEnergy, nyears, "cf_energy_net_jul");
		save_cf( CF_TODAugEnergy, nyears, "cf_energy_net_aug");
		save_cf( CF_TODSepEnergy, nyears, "cf_energy_net_sep");
		save_cf( CF_TODOctEnergy, nyears, "cf_energy_net_oct");
		save_cf( CF_TODNovEnergy, nyears, "cf_energy_net_nov");
		save_cf( CF_TODDecEnergy, nyears, "cf_energy_net_dec");

		save_cf( CF_TOD1Energy, nyears, "cf_energy_net_dispatch1");
		save_cf( CF_TOD2Energy, nyears, "cf_energy_net_dispatch2");
		save_cf( CF_TOD3Energy, nyears, "cf_energy_net_dispatch3");
		save_cf( CF_TOD4Energy, nyears, "cf_energy_net_dispatch4");
		save_cf( CF_TOD5Energy, nyears, "cf_energy_net_dispatch5");
		save_cf( CF_TOD6Energy, nyears, "cf_energy_net_dispatch6");
		save_cf( CF_TOD7Energy, nyears, "cf_energy_net_dispatch7");
		save_cf( CF_TOD8Energy, nyears, "cf_energy_net_dispatch8");
		save_cf( CF_TOD9Energy, nyears, "cf_energy_net_dispatch9");

		// dispatch revenue
		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_TOD1Revenue,i) = cf.at(CF_ppa_price,i)/100.0 * dispatch_factor1 * cf.at(CF_TOD1Energy,i);
			cf.at(CF_TOD2Revenue,i) = cf.at(CF_ppa_price,i)/100.0 * dispatch_factor2 * cf.at(CF_TOD2Energy,i);
			cf.at(CF_TOD3Revenue,i) = cf.at(CF_ppa_price,i)/100.0 * dispatch_factor3 * cf.at(CF_TOD3Energy,i);
			cf.at(CF_TOD4Revenue,i) = cf.at(CF_ppa_price,i)/100.0 * dispatch_factor4 * cf.at(CF_TOD4Energy,i);
			cf.at(CF_TOD5Revenue,i) = cf.at(CF_ppa_price,i)/100.0 * dispatch_factor5 * cf.at(CF_TOD5Energy,i);
			cf.at(CF_TOD6Revenue,i) = cf.at(CF_ppa_price,i)/100.0 * dispatch_factor6 * cf.at(CF_TOD6Energy,i);
			cf.at(CF_TOD7Revenue,i) = cf.at(CF_ppa_price,i)/100.0 * dispatch_factor7 * cf.at(CF_TOD7Energy,i);
			cf.at(CF_TOD8Revenue,i) = cf.at(CF_ppa_price,i)/100.0 * dispatch_factor8 * cf.at(CF_TOD8Energy,i);
			cf.at(CF_TOD9Revenue,i) = cf.at(CF_ppa_price,i)/100.0 * dispatch_factor9 * cf.at(CF_TOD9Energy,i);
		}

		save_cf( CF_TOD1Revenue, nyears, "cf_revenue_dispatch1");
		save_cf( CF_TOD2Revenue, nyears, "cf_revenue_dispatch2");
		save_cf( CF_TOD3Revenue, nyears, "cf_revenue_dispatch3");
		save_cf( CF_TOD4Revenue, nyears, "cf_revenue_dispatch4");
		save_cf( CF_TOD5Revenue, nyears, "cf_revenue_dispatch5");
		save_cf( CF_TOD6Revenue, nyears, "cf_revenue_dispatch6");
		save_cf( CF_TOD7Revenue, nyears, "cf_revenue_dispatch7");
		save_cf( CF_TOD8Revenue, nyears, "cf_revenue_dispatch8");
		save_cf( CF_TOD9Revenue, nyears, "cf_revenue_dispatch9");

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_TODJanRevenue,i) = cf.at(CF_ppa_price,i)/100.0 * (
				dispatch_factor1 * cf.at(CF_TOD1JanEnergy,i) +
				dispatch_factor2 * cf.at(CF_TOD2JanEnergy,i) +
				dispatch_factor3 * cf.at(CF_TOD3JanEnergy,i) +
				dispatch_factor4 * cf.at(CF_TOD4JanEnergy,i) +
				dispatch_factor5 * cf.at(CF_TOD5JanEnergy,i) +
				dispatch_factor6 * cf.at(CF_TOD6JanEnergy,i) +
				dispatch_factor7 * cf.at(CF_TOD7JanEnergy,i) +
				dispatch_factor8 * cf.at(CF_TOD8JanEnergy,i) +
				dispatch_factor9 * cf.at(CF_TOD9JanEnergy,i) 			);
		}
		save_cf( CF_TODJanRevenue, nyears, "cf_revenue_jan");
		
		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_TODFebRevenue,i) = cf.at(CF_ppa_price,i)/100.0 * (
				dispatch_factor1 * cf.at(CF_TOD1FebEnergy,i) +
				dispatch_factor2 * cf.at(CF_TOD2FebEnergy,i) +
				dispatch_factor3 * cf.at(CF_TOD3FebEnergy,i) +
				dispatch_factor4 * cf.at(CF_TOD4FebEnergy,i) +
				dispatch_factor5 * cf.at(CF_TOD5FebEnergy,i) +
				dispatch_factor6 * cf.at(CF_TOD6FebEnergy,i) +
				dispatch_factor7 * cf.at(CF_TOD7FebEnergy,i) +
				dispatch_factor8 * cf.at(CF_TOD8FebEnergy,i) +
				dispatch_factor9 * cf.at(CF_TOD9FebEnergy,i) 			);
		}
		save_cf( CF_TODFebRevenue, nyears, "cf_revenue_feb");

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_TODMarRevenue,i) = cf.at(CF_ppa_price,i)/100.0 * (
				dispatch_factor1 * cf.at(CF_TOD1MarEnergy,i) +
				dispatch_factor2 * cf.at(CF_TOD2MarEnergy,i) +
				dispatch_factor3 * cf.at(CF_TOD3MarEnergy,i) +
				dispatch_factor4 * cf.at(CF_TOD4MarEnergy,i) +
				dispatch_factor5 * cf.at(CF_TOD5MarEnergy,i) +
				dispatch_factor6 * cf.at(CF_TOD6MarEnergy,i) +
				dispatch_factor7 * cf.at(CF_TOD7MarEnergy,i) +
				dispatch_factor8 * cf.at(CF_TOD8MarEnergy,i) +
				dispatch_factor9 * cf.at(CF_TOD9MarEnergy,i) 			);
		}
		save_cf( CF_TODMarRevenue, nyears, "cf_revenue_mar");

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_TODAprRevenue,i) = cf.at(CF_ppa_price,i)/100.0 * (
				dispatch_factor1 * cf.at(CF_TOD1AprEnergy,i) +
				dispatch_factor2 * cf.at(CF_TOD2AprEnergy,i) +
				dispatch_factor3 * cf.at(CF_TOD3AprEnergy,i) +
				dispatch_factor4 * cf.at(CF_TOD4AprEnergy,i) +
				dispatch_factor5 * cf.at(CF_TOD5AprEnergy,i) +
				dispatch_factor6 * cf.at(CF_TOD6AprEnergy,i) +
				dispatch_factor7 * cf.at(CF_TOD7AprEnergy,i) +
				dispatch_factor8 * cf.at(CF_TOD8AprEnergy,i) +
				dispatch_factor9 * cf.at(CF_TOD9AprEnergy,i) 			);
		}
		save_cf( CF_TODAprRevenue, nyears, "cf_revenue_apr");

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_TODMayRevenue,i) = cf.at(CF_ppa_price,i)/100.0 * (
				dispatch_factor1 * cf.at(CF_TOD1MayEnergy,i) +
				dispatch_factor2 * cf.at(CF_TOD2MayEnergy,i) +
				dispatch_factor3 * cf.at(CF_TOD3MayEnergy,i) +
				dispatch_factor4 * cf.at(CF_TOD4MayEnergy,i) +
				dispatch_factor5 * cf.at(CF_TOD5MayEnergy,i) +
				dispatch_factor6 * cf.at(CF_TOD6MayEnergy,i) +
				dispatch_factor7 * cf.at(CF_TOD7MayEnergy,i) +
				dispatch_factor8 * cf.at(CF_TOD8MayEnergy,i) +
				dispatch_factor9 * cf.at(CF_TOD9MayEnergy,i) 			);
		}
		save_cf( CF_TODMayRevenue, nyears, "cf_revenue_may");

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_TODJunRevenue,i) = cf.at(CF_ppa_price,i)/100.0 * (
				dispatch_factor1 * cf.at(CF_TOD1JunEnergy,i) +
				dispatch_factor2 * cf.at(CF_TOD2JunEnergy,i) +
				dispatch_factor3 * cf.at(CF_TOD3JunEnergy,i) +
				dispatch_factor4 * cf.at(CF_TOD4JunEnergy,i) +
				dispatch_factor5 * cf.at(CF_TOD5JunEnergy,i) +
				dispatch_factor6 * cf.at(CF_TOD6JunEnergy,i) +
				dispatch_factor7 * cf.at(CF_TOD7JunEnergy,i) +
				dispatch_factor8 * cf.at(CF_TOD8JunEnergy,i) +
				dispatch_factor9 * cf.at(CF_TOD9JunEnergy,i) 			);
		}
		save_cf( CF_TODJunRevenue, nyears, "cf_revenue_jun");

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_TODJulRevenue,i) = cf.at(CF_ppa_price,i)/100.0 * (
				dispatch_factor1 * cf.at(CF_TOD1JulEnergy,i) +
				dispatch_factor2 * cf.at(CF_TOD2JulEnergy,i) +
				dispatch_factor3 * cf.at(CF_TOD3JulEnergy,i) +
				dispatch_factor4 * cf.at(CF_TOD4JulEnergy,i) +
				dispatch_factor5 * cf.at(CF_TOD5JulEnergy,i) +
				dispatch_factor6 * cf.at(CF_TOD6JulEnergy,i) +
				dispatch_factor7 * cf.at(CF_TOD7JulEnergy,i) +
				dispatch_factor8 * cf.at(CF_TOD8JulEnergy,i) +
				dispatch_factor9 * cf.at(CF_TOD9JulEnergy,i) 			);
		}
		save_cf( CF_TODJulRevenue, nyears, "cf_revenue_jul");

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_TODAugRevenue,i) = cf.at(CF_ppa_price,i)/100.0 * (
				dispatch_factor1 * cf.at(CF_TOD1AugEnergy,i) +
				dispatch_factor2 * cf.at(CF_TOD2AugEnergy,i) +
				dispatch_factor3 * cf.at(CF_TOD3AugEnergy,i) +
				dispatch_factor4 * cf.at(CF_TOD4AugEnergy,i) +
				dispatch_factor5 * cf.at(CF_TOD5AugEnergy,i) +
				dispatch_factor6 * cf.at(CF_TOD6AugEnergy,i) +
				dispatch_factor7 * cf.at(CF_TOD7AugEnergy,i) +
				dispatch_factor8 * cf.at(CF_TOD8AugEnergy,i) +
				dispatch_factor9 * cf.at(CF_TOD9AugEnergy,i) 			);
		}
		save_cf( CF_TODAugRevenue, nyears, "cf_revenue_aug");

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_TODSepRevenue,i) = cf.at(CF_ppa_price,i)/100.0 * (
				dispatch_factor1 * cf.at(CF_TOD1SepEnergy,i) +
				dispatch_factor2 * cf.at(CF_TOD2SepEnergy,i) +
				dispatch_factor3 * cf.at(CF_TOD3SepEnergy,i) +
				dispatch_factor4 * cf.at(CF_TOD4SepEnergy,i) +
				dispatch_factor5 * cf.at(CF_TOD5SepEnergy,i) +
				dispatch_factor6 * cf.at(CF_TOD6SepEnergy,i) +
				dispatch_factor7 * cf.at(CF_TOD7SepEnergy,i) +
				dispatch_factor8 * cf.at(CF_TOD8SepEnergy,i) +
				dispatch_factor9 * cf.at(CF_TOD9SepEnergy,i) 			);
		}
		save_cf( CF_TODSepRevenue, nyears, "cf_revenue_sep");

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_TODOctRevenue,i) = cf.at(CF_ppa_price,i)/100.0 * (
				dispatch_factor1 * cf.at(CF_TOD1OctEnergy,i) +
				dispatch_factor2 * cf.at(CF_TOD2OctEnergy,i) +
				dispatch_factor3 * cf.at(CF_TOD3OctEnergy,i) +
				dispatch_factor4 * cf.at(CF_TOD4OctEnergy,i) +
				dispatch_factor5 * cf.at(CF_TOD5OctEnergy,i) +
				dispatch_factor6 * cf.at(CF_TOD6OctEnergy,i) +
				dispatch_factor7 * cf.at(CF_TOD7OctEnergy,i) +
				dispatch_factor8 * cf.at(CF_TOD8OctEnergy,i) +
				dispatch_factor9 * cf.at(CF_TOD9OctEnergy,i) 			);
		}
		save_cf( CF_TODOctRevenue, nyears, "cf_revenue_oct");

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_TODNovRevenue,i) = cf.at(CF_ppa_price,i)/100.0 * (
				dispatch_factor1 * cf.at(CF_TOD1NovEnergy,i) +
				dispatch_factor2 * cf.at(CF_TOD2NovEnergy,i) +
				dispatch_factor3 * cf.at(CF_TOD3NovEnergy,i) +
				dispatch_factor4 * cf.at(CF_TOD4NovEnergy,i) +
				dispatch_factor5 * cf.at(CF_TOD5NovEnergy,i) +
				dispatch_factor6 * cf.at(CF_TOD6NovEnergy,i) +
				dispatch_factor7 * cf.at(CF_TOD7NovEnergy,i) +
				dispatch_factor8 * cf.at(CF_TOD8NovEnergy,i) +
				dispatch_factor9 * cf.at(CF_TOD9NovEnergy,i) 			);
		}
		save_cf( CF_TODNovRevenue, nyears, "cf_revenue_nov");

		for (i=1;i<=nyears;i++)
		{
			cf.at(CF_TODDecRevenue,i) = cf.at(CF_ppa_price,i)/100.0 * (
				dispatch_factor1 * cf.at(CF_TOD1DecEnergy,i) +
				dispatch_factor2 * cf.at(CF_TOD2DecEnergy,i) +
				dispatch_factor3 * cf.at(CF_TOD3DecEnergy,i) +
				dispatch_factor4 * cf.at(CF_TOD4DecEnergy,i) +
				dispatch_factor5 * cf.at(CF_TOD5DecEnergy,i) +
				dispatch_factor6 * cf.at(CF_TOD6DecEnergy,i) +
				dispatch_factor7 * cf.at(CF_TOD7DecEnergy,i) +
				dispatch_factor8 * cf.at(CF_TOD8DecEnergy,i) +
				dispatch_factor9 * cf.at(CF_TOD9DecEnergy,i) 			);
		}
		save_cf( CF_TODDecRevenue, nyears, "cf_revenue_Dec");


		cf.at( CF_revenue_monthly_firstyear, 0) = cf.at(CF_TODJanRevenue, 1);
		cf.at( CF_revenue_monthly_firstyear, 1) = cf.at(CF_TODFebRevenue, 1);
		cf.at( CF_revenue_monthly_firstyear, 2) = cf.at(CF_TODMarRevenue, 1);
		cf.at( CF_revenue_monthly_firstyear, 3) = cf.at(CF_TODAprRevenue, 1);
		cf.at( CF_revenue_monthly_firstyear, 4) = cf.at(CF_TODMayRevenue, 1);
		cf.at( CF_revenue_monthly_firstyear, 5) = cf.at(CF_TODJunRevenue, 1);
		cf.at( CF_revenue_monthly_firstyear, 6) = cf.at(CF_TODJulRevenue, 1);
		cf.at( CF_revenue_monthly_firstyear, 7) = cf.at(CF_TODAugRevenue, 1);
		cf.at( CF_revenue_monthly_firstyear, 8) = cf.at(CF_TODSepRevenue, 1);
		cf.at( CF_revenue_monthly_firstyear, 9) = cf.at(CF_TODOctRevenue, 1);
		cf.at( CF_revenue_monthly_firstyear, 10) = cf.at(CF_TODNovRevenue, 1);
		cf.at( CF_revenue_monthly_firstyear, 11) = cf.at(CF_TODDecRevenue, 1);

		cf.at( CF_energy_net_monthly_firstyear, 0) = cf.at(CF_TODJanEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear, 1) = cf.at(CF_TODFebEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear, 2) = cf.at(CF_TODMarEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear, 3) = cf.at(CF_TODAprEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear, 4) = cf.at(CF_TODMayEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear, 5) = cf.at(CF_TODJunEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear, 6) = cf.at(CF_TODJulEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear, 7) = cf.at(CF_TODAugEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear, 8) = cf.at(CF_TODSepEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear, 9) = cf.at(CF_TODOctEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear, 10) = cf.at(CF_TODNovEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear, 11) = cf.at(CF_TODDecEnergy, 1);

		save_cf( CF_revenue_monthly_firstyear, 11, "cf_revenue_monthly_firstyear");
		save_cf( CF_energy_net_monthly_firstyear, 11, "cf_energy_net_monthly_firstyear");
		


		cf.at( CF_revenue_monthly_firstyear_TOD1, 0) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor1 * cf.at(CF_TOD1JanEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD1, 1) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor1 * cf.at(CF_TOD1FebEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD1, 2) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor1 * cf.at(CF_TOD1MarEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD1, 3) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor1 * cf.at(CF_TOD1AprEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD1, 4) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor1 * cf.at(CF_TOD1MayEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD1, 5) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor1 * cf.at(CF_TOD1JunEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD1, 6) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor1 * cf.at(CF_TOD1JulEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD1, 7) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor1 * cf.at(CF_TOD1AugEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD1, 8) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor1 * cf.at(CF_TOD1SepEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD1, 9) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor1 * cf.at(CF_TOD1OctEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD1, 10) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor1 * cf.at(CF_TOD1NovEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD1, 11) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor1 * cf.at(CF_TOD1DecEnergy,1);

		cf.at( CF_energy_net_monthly_firstyear_TOD1, 0) = cf.at(CF_TOD1JanEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD1, 1) = cf.at(CF_TOD1FebEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD1, 2) = cf.at(CF_TOD1MarEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD1, 3) = cf.at(CF_TOD1AprEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD1, 4) = cf.at(CF_TOD1MayEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD1, 5) = cf.at(CF_TOD1JunEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD1, 6) = cf.at(CF_TOD1JulEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD1, 7) = cf.at(CF_TOD1AugEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD1, 8) = cf.at(CF_TOD1SepEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD1, 9) = cf.at(CF_TOD1OctEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD1, 10) = cf.at(CF_TOD1NovEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD1, 11) = cf.at(CF_TOD1DecEnergy, 1);

		save_cf( CF_revenue_monthly_firstyear_TOD1, 11, "cf_revenue_monthly_firstyear_TOD1");
		save_cf( CF_energy_net_monthly_firstyear_TOD1, 11, "cf_energy_net_monthly_firstyear_TOD1");


		cf.at( CF_revenue_monthly_firstyear_TOD2, 0) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor2 * cf.at(CF_TOD2JanEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD2, 1) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor2 * cf.at(CF_TOD2FebEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD2, 2) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor2 * cf.at(CF_TOD2MarEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD2, 3) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor2 * cf.at(CF_TOD2AprEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD2, 4) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor2 * cf.at(CF_TOD2MayEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD2, 5) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor2 * cf.at(CF_TOD2JunEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD2, 6) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor2 * cf.at(CF_TOD2JulEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD2, 7) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor2 * cf.at(CF_TOD2AugEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD2, 8) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor2 * cf.at(CF_TOD2SepEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD2, 9) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor2 * cf.at(CF_TOD2OctEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD2, 10) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor2 * cf.at(CF_TOD2NovEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD2, 11) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor2 * cf.at(CF_TOD2DecEnergy,1);

		cf.at( CF_energy_net_monthly_firstyear_TOD2, 0) = cf.at(CF_TOD2JanEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD2, 1) = cf.at(CF_TOD2FebEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD2, 2) = cf.at(CF_TOD2MarEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD2, 3) = cf.at(CF_TOD2AprEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD2, 4) = cf.at(CF_TOD2MayEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD2, 5) = cf.at(CF_TOD2JunEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD2, 6) = cf.at(CF_TOD2JulEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD2, 7) = cf.at(CF_TOD2AugEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD2, 8) = cf.at(CF_TOD2SepEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD2, 9) = cf.at(CF_TOD2OctEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD2, 10) = cf.at(CF_TOD2NovEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD2, 11) = cf.at(CF_TOD2DecEnergy, 1);

		save_cf( CF_revenue_monthly_firstyear_TOD2, 11, "cf_revenue_monthly_firstyear_TOD2");
		save_cf( CF_energy_net_monthly_firstyear_TOD2, 11, "cf_energy_net_monthly_firstyear_TOD2");


		cf.at( CF_revenue_monthly_firstyear_TOD3, 0) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor3 * cf.at(CF_TOD3JanEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD3, 1) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor3 * cf.at(CF_TOD3FebEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD3, 2) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor3 * cf.at(CF_TOD3MarEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD3, 3) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor3 * cf.at(CF_TOD3AprEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD3, 4) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor3 * cf.at(CF_TOD3MayEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD3, 5) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor3 * cf.at(CF_TOD3JunEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD3, 6) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor3 * cf.at(CF_TOD3JulEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD3, 7) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor3 * cf.at(CF_TOD3AugEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD3, 8) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor3 * cf.at(CF_TOD3SepEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD3, 9) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor3 * cf.at(CF_TOD3OctEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD3, 10) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor3 * cf.at(CF_TOD3NovEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD3, 11) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor3 * cf.at(CF_TOD3DecEnergy,1);

		cf.at( CF_energy_net_monthly_firstyear_TOD3, 0) = cf.at(CF_TOD3JanEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD3, 1) = cf.at(CF_TOD3FebEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD3, 2) = cf.at(CF_TOD3MarEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD3, 3) = cf.at(CF_TOD3AprEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD3, 4) = cf.at(CF_TOD3MayEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD3, 5) = cf.at(CF_TOD3JunEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD3, 6) = cf.at(CF_TOD3JulEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD3, 7) = cf.at(CF_TOD3AugEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD3, 8) = cf.at(CF_TOD3SepEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD3, 9) = cf.at(CF_TOD3OctEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD3, 10) = cf.at(CF_TOD3NovEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD3, 11) = cf.at(CF_TOD3DecEnergy, 1);

		save_cf( CF_revenue_monthly_firstyear_TOD3, 11, "cf_revenue_monthly_firstyear_TOD3");
		save_cf( CF_energy_net_monthly_firstyear_TOD3, 11, "cf_energy_net_monthly_firstyear_TOD3");


		cf.at( CF_revenue_monthly_firstyear_TOD4, 0) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor4 * cf.at(CF_TOD4JanEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD4, 1) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor4 * cf.at(CF_TOD4FebEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD4, 2) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor4 * cf.at(CF_TOD4MarEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD4, 3) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor4 * cf.at(CF_TOD4AprEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD4, 4) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor4 * cf.at(CF_TOD4MayEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD4, 5) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor4 * cf.at(CF_TOD4JunEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD4, 6) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor4 * cf.at(CF_TOD4JulEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD4, 7) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor4 * cf.at(CF_TOD4AugEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD4, 8) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor4 * cf.at(CF_TOD4SepEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD4, 9) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor4 * cf.at(CF_TOD4OctEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD4, 10) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor4 * cf.at(CF_TOD4NovEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD4, 11) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor4 * cf.at(CF_TOD4DecEnergy,1);

		cf.at( CF_energy_net_monthly_firstyear_TOD4, 0) = cf.at(CF_TOD4JanEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD4, 1) = cf.at(CF_TOD4FebEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD4, 2) = cf.at(CF_TOD4MarEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD4, 3) = cf.at(CF_TOD4AprEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD4, 4) = cf.at(CF_TOD4MayEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD4, 5) = cf.at(CF_TOD4JunEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD4, 6) = cf.at(CF_TOD4JulEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD4, 7) = cf.at(CF_TOD4AugEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD4, 8) = cf.at(CF_TOD4SepEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD4, 9) = cf.at(CF_TOD4OctEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD4, 10) = cf.at(CF_TOD4NovEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD4, 11) = cf.at(CF_TOD4DecEnergy, 1);

		save_cf( CF_revenue_monthly_firstyear_TOD4, 11, "cf_revenue_monthly_firstyear_TOD4");
		save_cf( CF_energy_net_monthly_firstyear_TOD4, 11, "cf_energy_net_monthly_firstyear_TOD4");


		cf.at( CF_revenue_monthly_firstyear_TOD5, 0) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor5 * cf.at(CF_TOD5JanEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD5, 1) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor5 * cf.at(CF_TOD5FebEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD5, 2) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor5 * cf.at(CF_TOD5MarEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD5, 3) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor5 * cf.at(CF_TOD5AprEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD5, 4) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor5 * cf.at(CF_TOD5MayEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD5, 5) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor5 * cf.at(CF_TOD5JunEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD5, 6) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor5 * cf.at(CF_TOD5JulEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD5, 7) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor5 * cf.at(CF_TOD5AugEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD5, 8) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor5 * cf.at(CF_TOD5SepEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD5, 9) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor5 * cf.at(CF_TOD5OctEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD5, 10) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor5 * cf.at(CF_TOD5NovEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD5, 11) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor5 * cf.at(CF_TOD5DecEnergy,1);

		cf.at( CF_energy_net_monthly_firstyear_TOD5, 0) = cf.at(CF_TOD5JanEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD5, 1) = cf.at(CF_TOD5FebEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD5, 2) = cf.at(CF_TOD5MarEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD5, 3) = cf.at(CF_TOD5AprEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD5, 4) = cf.at(CF_TOD5MayEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD5, 5) = cf.at(CF_TOD5JunEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD5, 6) = cf.at(CF_TOD5JulEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD5, 7) = cf.at(CF_TOD5AugEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD5, 8) = cf.at(CF_TOD5SepEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD5, 9) = cf.at(CF_TOD5OctEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD5, 10) = cf.at(CF_TOD5NovEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD5, 11) = cf.at(CF_TOD5DecEnergy, 1);

		save_cf( CF_revenue_monthly_firstyear_TOD5, 11, "cf_revenue_monthly_firstyear_TOD5");
		save_cf( CF_energy_net_monthly_firstyear_TOD5, 11, "cf_energy_net_monthly_firstyear_TOD5");


		cf.at( CF_revenue_monthly_firstyear_TOD6, 0) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor6 * cf.at(CF_TOD6JanEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD6, 1) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor6 * cf.at(CF_TOD6FebEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD6, 2) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor6 * cf.at(CF_TOD6MarEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD6, 3) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor6 * cf.at(CF_TOD6AprEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD6, 4) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor6 * cf.at(CF_TOD6MayEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD6, 5) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor6 * cf.at(CF_TOD6JunEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD6, 6) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor6 * cf.at(CF_TOD6JulEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD6, 7) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor6 * cf.at(CF_TOD6AugEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD6, 8) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor6 * cf.at(CF_TOD6SepEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD6, 9) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor6 * cf.at(CF_TOD6OctEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD6, 10) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor6 * cf.at(CF_TOD6NovEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD6, 11) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor6 * cf.at(CF_TOD6DecEnergy,1);

		cf.at( CF_energy_net_monthly_firstyear_TOD6, 0) = cf.at(CF_TOD6JanEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD6, 1) = cf.at(CF_TOD6FebEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD6, 2) = cf.at(CF_TOD6MarEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD6, 3) = cf.at(CF_TOD6AprEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD6, 4) = cf.at(CF_TOD6MayEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD6, 5) = cf.at(CF_TOD6JunEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD6, 6) = cf.at(CF_TOD6JulEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD6, 7) = cf.at(CF_TOD6AugEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD6, 8) = cf.at(CF_TOD6SepEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD6, 9) = cf.at(CF_TOD6OctEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD6, 10) = cf.at(CF_TOD6NovEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD6, 11) = cf.at(CF_TOD6DecEnergy, 1);

		save_cf( CF_revenue_monthly_firstyear_TOD6, 11, "cf_revenue_monthly_firstyear_TOD6");
		save_cf( CF_energy_net_monthly_firstyear_TOD6, 11, "cf_energy_net_monthly_firstyear_TOD6");


		cf.at( CF_revenue_monthly_firstyear_TOD7, 0) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor7 * cf.at(CF_TOD7JanEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD7, 1) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor7 * cf.at(CF_TOD7FebEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD7, 2) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor7 * cf.at(CF_TOD7MarEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD7, 3) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor7 * cf.at(CF_TOD7AprEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD7, 4) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor7 * cf.at(CF_TOD7MayEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD7, 5) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor7 * cf.at(CF_TOD7JunEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD7, 6) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor7 * cf.at(CF_TOD7JulEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD7, 7) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor7 * cf.at(CF_TOD7AugEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD7, 8) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor7 * cf.at(CF_TOD7SepEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD7, 9) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor7 * cf.at(CF_TOD7OctEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD7, 10) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor7 * cf.at(CF_TOD7NovEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD7, 11) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor7 * cf.at(CF_TOD7DecEnergy,1);

		cf.at( CF_energy_net_monthly_firstyear_TOD7, 0) = cf.at(CF_TOD7JanEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD7, 1) = cf.at(CF_TOD7FebEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD7, 2) = cf.at(CF_TOD7MarEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD7, 3) = cf.at(CF_TOD7AprEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD7, 4) = cf.at(CF_TOD7MayEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD7, 5) = cf.at(CF_TOD7JunEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD7, 6) = cf.at(CF_TOD7JulEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD7, 7) = cf.at(CF_TOD7AugEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD7, 8) = cf.at(CF_TOD7SepEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD7, 9) = cf.at(CF_TOD7OctEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD7, 10) = cf.at(CF_TOD7NovEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD7, 11) = cf.at(CF_TOD7DecEnergy, 1);

		save_cf( CF_revenue_monthly_firstyear_TOD7, 11, "cf_revenue_monthly_firstyear_TOD7");
		save_cf( CF_energy_net_monthly_firstyear_TOD7, 11, "cf_energy_net_monthly_firstyear_TOD7");


		cf.at( CF_revenue_monthly_firstyear_TOD8, 0) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor8 * cf.at(CF_TOD8JanEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD8, 1) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor8 * cf.at(CF_TOD8FebEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD8, 2) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor8 * cf.at(CF_TOD8MarEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD8, 3) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor8 * cf.at(CF_TOD8AprEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD8, 4) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor8 * cf.at(CF_TOD8MayEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD8, 5) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor8 * cf.at(CF_TOD8JunEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD8, 6) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor8 * cf.at(CF_TOD8JulEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD8, 7) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor8 * cf.at(CF_TOD8AugEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD8, 8) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor8 * cf.at(CF_TOD8SepEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD8, 9) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor8 * cf.at(CF_TOD8OctEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD8, 10) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor8 * cf.at(CF_TOD8NovEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD8, 11) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor8 * cf.at(CF_TOD8DecEnergy,1);

		cf.at( CF_energy_net_monthly_firstyear_TOD8, 0) = cf.at(CF_TOD8JanEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD8, 1) = cf.at(CF_TOD8FebEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD8, 2) = cf.at(CF_TOD8MarEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD8, 3) = cf.at(CF_TOD8AprEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD8, 4) = cf.at(CF_TOD8MayEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD8, 5) = cf.at(CF_TOD8JunEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD8, 6) = cf.at(CF_TOD8JulEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD8, 7) = cf.at(CF_TOD8AugEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD8, 8) = cf.at(CF_TOD8SepEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD8, 9) = cf.at(CF_TOD8OctEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD8, 10) = cf.at(CF_TOD8NovEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD8, 11) = cf.at(CF_TOD8DecEnergy, 1);

		save_cf( CF_revenue_monthly_firstyear_TOD8, 11, "cf_revenue_monthly_firstyear_TOD8");
		save_cf( CF_energy_net_monthly_firstyear_TOD8, 11, "cf_energy_net_monthly_firstyear_TOD8");


		cf.at( CF_revenue_monthly_firstyear_TOD9, 0) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor9 * cf.at(CF_TOD9JanEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD9, 1) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor9 * cf.at(CF_TOD9FebEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD9, 2) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor9 * cf.at(CF_TOD9MarEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD9, 3) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor9 * cf.at(CF_TOD9AprEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD9, 4) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor9 * cf.at(CF_TOD9MayEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD9, 5) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor9 * cf.at(CF_TOD9JunEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD9, 6) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor9 * cf.at(CF_TOD9JulEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD9, 7) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor9 * cf.at(CF_TOD9AugEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD9, 8) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor9 * cf.at(CF_TOD9SepEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD9, 9) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor9 * cf.at(CF_TOD9OctEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD9, 10) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor9 * cf.at(CF_TOD9NovEnergy,1);
		cf.at( CF_revenue_monthly_firstyear_TOD9, 11) = cf.at(CF_ppa_price,1)/100.0 * 
				dispatch_factor9 * cf.at(CF_TOD9DecEnergy,1);

		cf.at( CF_energy_net_monthly_firstyear_TOD9, 0) = cf.at(CF_TOD9JanEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD9, 1) = cf.at(CF_TOD9FebEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD9, 2) = cf.at(CF_TOD9MarEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD9, 3) = cf.at(CF_TOD9AprEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD9, 4) = cf.at(CF_TOD9MayEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD9, 5) = cf.at(CF_TOD9JunEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD9, 6) = cf.at(CF_TOD9JulEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD9, 7) = cf.at(CF_TOD9AugEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD9, 8) = cf.at(CF_TOD9SepEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD9, 9) = cf.at(CF_TOD9OctEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD9, 10) = cf.at(CF_TOD9NovEnergy, 1);
		cf.at( CF_energy_net_monthly_firstyear_TOD9, 11) = cf.at(CF_TOD9DecEnergy, 1);

		save_cf( CF_revenue_monthly_firstyear_TOD9, 11, "cf_revenue_monthly_firstyear_TOD9");
		save_cf( CF_energy_net_monthly_firstyear_TOD9, 11, "cf_energy_net_monthly_firstyear_TOD9");


		assign( "firstyear_revenue_dispatch1", var_data((ssc_number_t) cf.at(CF_TOD1Revenue,1) ));  
		assign( "firstyear_revenue_dispatch2", var_data((ssc_number_t) cf.at(CF_TOD2Revenue,1) ));  
		assign( "firstyear_revenue_dispatch3", var_data((ssc_number_t) cf.at(CF_TOD3Revenue,1) ));  
		assign( "firstyear_revenue_dispatch4", var_data((ssc_number_t) cf.at(CF_TOD4Revenue,1) ));  
		assign( "firstyear_revenue_dispatch5", var_data((ssc_number_t) cf.at(CF_TOD5Revenue,1) ));  
		assign( "firstyear_revenue_dispatch6", var_data((ssc_number_t) cf.at(CF_TOD6Revenue,1) ));  
		assign( "firstyear_revenue_dispatch7", var_data((ssc_number_t) cf.at(CF_TOD7Revenue,1) ));  
		assign( "firstyear_revenue_dispatch8", var_data((ssc_number_t) cf.at(CF_TOD8Revenue,1) ));  
		assign( "firstyear_revenue_dispatch9", var_data((ssc_number_t) cf.at(CF_TOD9Revenue,1) ));  
		
		assign( "firstyear_energy_dispatch1", var_data((ssc_number_t) cf.at(CF_TOD1Energy,1) ));  
		assign( "firstyear_energy_dispatch2", var_data((ssc_number_t) cf.at(CF_TOD2Energy,1) ));  
		assign( "firstyear_energy_dispatch3", var_data((ssc_number_t) cf.at(CF_TOD3Energy,1) ));  
		assign( "firstyear_energy_dispatch4", var_data((ssc_number_t) cf.at(CF_TOD4Energy,1) ));  
		assign( "firstyear_energy_dispatch5", var_data((ssc_number_t) cf.at(CF_TOD5Energy,1) ));  
		assign( "firstyear_energy_dispatch6", var_data((ssc_number_t) cf.at(CF_TOD6Energy,1) ));  
		assign( "firstyear_energy_dispatch7", var_data((ssc_number_t) cf.at(CF_TOD7Energy,1) ));  
		assign( "firstyear_energy_dispatch8", var_data((ssc_number_t) cf.at(CF_TOD8Energy,1) ));  
		assign( "firstyear_energy_dispatch9", var_data((ssc_number_t) cf.at(CF_TOD9Energy,1) ));  

		assign( "firstyear_energy_price1", var_data((ssc_number_t) ( (cf.at(CF_TOD1Energy,1)==0) ? 0 : (cf.at(CF_TOD1Revenue,1)/cf.at(CF_TOD1Energy,1) )/100.0 ) ) ) ;
		assign( "firstyear_energy_price2", var_data((ssc_number_t) ( (cf.at(CF_TOD2Energy,1)==0) ? 0 : (cf.at(CF_TOD2Revenue,1)/cf.at(CF_TOD2Energy,1) )/100.0 ) ) ) ;
		assign( "firstyear_energy_price3", var_data((ssc_number_t) ( (cf.at(CF_TOD3Energy,1)==0) ? 0 : (cf.at(CF_TOD3Revenue,1)/cf.at(CF_TOD3Energy,1) )/100.0 ) ) ) ;
		assign( "firstyear_energy_price4", var_data((ssc_number_t) ( (cf.at(CF_TOD4Energy,1)==0) ? 0 : (cf.at(CF_TOD4Revenue,1)/cf.at(CF_TOD4Energy,1) )/100.0 ) ) ) ;
		assign( "firstyear_energy_price5", var_data((ssc_number_t) ( (cf.at(CF_TOD5Energy,1)==0) ? 0 : (cf.at(CF_TOD5Revenue,1)/cf.at(CF_TOD5Energy,1) )/100.0 ) ) ) ;
		assign( "firstyear_energy_price6", var_data((ssc_number_t) ( (cf.at(CF_TOD6Energy,1)==0) ? 0 : (cf.at(CF_TOD6Revenue,1)/cf.at(CF_TOD6Energy,1) )/100.0 ) ) ) ;
		assign( "firstyear_energy_price7", var_data((ssc_number_t) ( (cf.at(CF_TOD7Energy,1)==0) ? 0 : (cf.at(CF_TOD7Revenue,1)/cf.at(CF_TOD7Energy,1) )/100.0 ) ) ) ;
		assign( "firstyear_energy_price8", var_data((ssc_number_t) ( (cf.at(CF_TOD8Energy,1)==0) ? 0 : (cf.at(CF_TOD8Revenue,1)/cf.at(CF_TOD8Energy,1) )/100.0 ) ) ) ;
		assign( "firstyear_energy_price9", var_data((ssc_number_t) ( (cf.at(CF_TOD9Energy,1)==0) ? 0 : (cf.at(CF_TOD9Revenue,1)/cf.at(CF_TOD9Energy,1) )/100.0 ) ) ) ;





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
		assign( "depr_fedbas_ibi_reduc_total", var_data((ssc_number_t) depr_sta_reduction_ibi ) );
		assign( "depr_fedbas_cbi_reduc_total", var_data((ssc_number_t) depr_sta_reduction_cbi ) );
 		assign( "depr_fedbas_prior_itc_total", var_data((ssc_number_t) ( depr_alloc_total - depr_sta_reduction_ibi - depr_sta_reduction_cbi)) );
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


	void depreciation_sched_custom(int cf_line, int nyears, const std::string &custom)
	{
		// computes custom percentage schedule 100%
		// if customValue is array then annual schedule of percent
		// if customValue is a single value then that value is used up to 100%
		int i;
		size_t count = 0;
		ssc_number_t *parr = as_array(custom, &count);
		for (i = 1; i<nyears; i++)
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
			for (i = 1; i<scheduleDuration; i++)
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
				cf.at(cf_line, i) = (i <= term) ? parr[0] * cf.at(CF_energy_net,i) * pow(1 + escal, i-1) : 0.0;
		}
		else
		{
			for (int i=1;i<=nyears && i <= (int)len;i++)
				cf.at(cf_line, i) = parr[i-1]*cf.at(CF_energy_net,i);
		}
	}

		void compute_production_incentive_IRS_2010_37( int cf_line, int nyears, const std::string &s_val, const std::string &s_term, const std::string &s_escal )
	{
		// rounding based on IRS document and emails from John and Matt from DHF Financials 2/24/2011 and DHF model v4.4
		size_t len = 0;
		ssc_number_t *parr = as_array(s_val, &len);
		int term = as_integer(s_term);
		double escal = as_double(s_escal)/100.0;

		if (len == 1)
		{
			for (int i=1;i<=nyears;i++)
				cf.at(cf_line, i) = (i <= term) ? cf.at(CF_energy_net,i) / 1000.0 * round_dhf(1000.0 * parr[0] * pow(1 + escal, i-1)) : 0.0;
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

	double npv( int cf_line, int nyears, double rate ) throw ( general_error )
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
		double max=fabs(cf.at(cf_unscaled,0));
		for (i=0;i<=count;i++) 
			if (fabs(cf.at(cf_unscaled,i))> max) max =fabs(cf.at(cf_unscaled,i));
		return (max>0 ? max:1);
	}

	bool is_valid_irr( int cf_line, int count, double residual, double tolerance, int number_of_iterations, int max_iterations, double calculated_irr, double scale_factor )
	{
		double npv_of_irr = npv(cf_line,count,calculated_irr)+cf.at(cf_line,0);
		double npv_of_irr_plus_delta = npv(cf_line,count,calculated_irr+0.001)+cf.at(cf_line,0);
		bool is_valid = ( (number_of_iterations<max_iterations) && (fabs(residual)<tolerance) && (npv_of_irr>npv_of_irr_plus_delta) && (fabs(npv_of_irr/scale_factor)<tolerance) );
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
		double calculated_irr=0;


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
				calculated_irr = 0.0; // did not converge
			}

		}
		return calculated_irr;
	}


	double irr_calc( int cf_line, int count, double initial_guess, double tolerance, int max_iterations, double scale_factor, int &number_of_iterations, double &residual )
	{
		double calculated_irr=0;
		double deriv_sum = irr_derivative_sum(initial_guess,cf_line,count);
		if (deriv_sum != 0.0)
			calculated_irr = initial_guess - irr_poly_sum(initial_guess,cf_line,count)/deriv_sum;
		else
			return initial_guess;

		number_of_iterations++;


		residual = irr_poly_sum(calculated_irr,cf_line,count) / scale_factor;

		while (!(fabs(residual) <= tolerance) && (number_of_iterations < max_iterations))
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


	double min( double a, double b )
	{
		return (a < b) ? a : b;
	}

	double max( double a, double b )
	{
		return (a > b) ? a : b;
	}

	bool compute_dispatch_output(int nyears)
	{
	//Calculate energy dispatched in each dispatch period 
		ssc_number_t *hourly_dispatch; // tou period 
		ssc_number_t *hourly_enet; // hourly energy output


		int h;
		size_t count;

	// hourly energy
		hourly_enet = as_array("energy_net_hourly", &count );
		if ( count != 8760)
		{
			std::stringstream outm;
			outm <<  "Bad hourly energy output length (" << count << "), should be 8760 value";
			log( outm.str() );
			return false;
		}

	// hourly dispatch
		hourly_dispatch = as_array("dispatch_hourly", &count );
		if ( count != 8760)
		{
			std::stringstream outm;
			outm <<  "Bad hourly dispatch output length (" << count << "), should be 8760 value";
			log( outm.str() );
			return false;
		}
	
		cf.at(CF_TOD1Energy,1) = 0;
		cf.at(CF_TOD2Energy,1) = 0;
		cf.at(CF_TOD3Energy,1) = 0;
		cf.at(CF_TOD4Energy,1) = 0;
		cf.at(CF_TOD5Energy,1) = 0;
		cf.at(CF_TOD6Energy,1) = 0;
		cf.at(CF_TOD7Energy,1) = 0;
		cf.at(CF_TOD8Energy,1) = 0;
		cf.at(CF_TOD9Energy,1) = 0;
		for (h=0;h<8760;h++)
		{
			switch ((int)hourly_dispatch[h])
			{
				case 0:
					cf.at(CF_TOD1Energy,1) += hourly_enet[h];
					break;
				case 1:
					cf.at(CF_TOD2Energy,1) += hourly_enet[h];
					break;
				case 2:
					cf.at(CF_TOD3Energy,1) += hourly_enet[h];
					break;
				case 3:
					cf.at(CF_TOD4Energy,1) += hourly_enet[h];
					break;
				case 4:
					cf.at(CF_TOD5Energy,1) += hourly_enet[h];
					break;
				case 5:
					cf.at(CF_TOD6Energy,1) += hourly_enet[h];
					break;
				case 6:
					cf.at(CF_TOD7Energy,1) += hourly_enet[h];
					break;
				case 7:
					cf.at(CF_TOD8Energy,1) += hourly_enet[h];
					break;
				case 8:
					cf.at(CF_TOD9Energy,1) += hourly_enet[h];
					break;
			}
		}

		for (int y=2;y<=nyears;y++)
		{
	// compute energy dispatched
			cf.at(CF_TOD1Energy,y) = cf.at(CF_TOD1Energy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD2Energy,y) = cf.at(CF_TOD2Energy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD3Energy,y) = cf.at(CF_TOD3Energy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD4Energy,y) = cf.at(CF_TOD4Energy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD5Energy,y) = cf.at(CF_TOD5Energy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD6Energy,y) = cf.at(CF_TOD6Energy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD7Energy,y) = cf.at(CF_TOD7Energy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD8Energy,y) = cf.at(CF_TOD8Energy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD9Energy,y) = cf.at(CF_TOD9Energy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);

		}
		cf.at(CF_TOD1Energy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD2Energy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD3Energy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD4Energy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD5Energy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD6Energy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD7Energy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD8Energy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD9Energy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);

		return true;
	}

	bool process_dispatch_output(int nyears)
	{
	//Calculate energy dispatched in each dispatch period 
		ssc_number_t *hourly_dispatch; // tou period 
		ssc_number_t *hourly_enet; // hourly energy output

		size_t count;

	// hourly energy
		hourly_enet = as_array("energy_net_hourly", &count );
		if ( count != 8760)
		{
			std::stringstream outm;
			outm <<  "Bad hourly energy output length (" << count << "), should be 8760 value";
			log( outm.str() );
			return false;
		}

	// hourly dispatch
		hourly_dispatch = as_array("dispatch_hourly", &count );
		if ( count != 8760)
		{
			std::stringstream outm;
			outm <<  "Bad hourly dispatch output length (" << count << "), should be 8760 value";
			log( outm.str() );
			return false;
		}


		cf.at(CF_TODJanEnergy,1) = 0;
		cf.at(CF_TODFebEnergy,1) = 0;
		cf.at(CF_TODMarEnergy,1) = 0;
		cf.at(CF_TODAprEnergy,1) = 0;
		cf.at(CF_TODMayEnergy,1) = 0;
		cf.at(CF_TODJunEnergy,1) = 0;
		cf.at(CF_TODJulEnergy,1) = 0;
		cf.at(CF_TODAugEnergy,1) = 0;
		cf.at(CF_TODSepEnergy,1) = 0;
		cf.at(CF_TODOctEnergy,1) = 0;
		cf.at(CF_TODNovEnergy,1) = 0;
		cf.at(CF_TODDecEnergy,1) = 0;

		cf.at(CF_TOD1JanEnergy,1) = 0;
		cf.at(CF_TOD1FebEnergy,1) = 0;
		cf.at(CF_TOD1MarEnergy,1) = 0;
		cf.at(CF_TOD1AprEnergy,1) = 0;
		cf.at(CF_TOD1MayEnergy,1) = 0;
		cf.at(CF_TOD1JunEnergy,1) = 0;
		cf.at(CF_TOD1JulEnergy,1) = 0;
		cf.at(CF_TOD1AugEnergy,1) = 0;
		cf.at(CF_TOD1SepEnergy,1) = 0;
		cf.at(CF_TOD1OctEnergy,1) = 0;
		cf.at(CF_TOD1NovEnergy,1) = 0;
		cf.at(CF_TOD1DecEnergy,1) = 0;
	
		cf.at(CF_TOD2JanEnergy,1) = 0;
		cf.at(CF_TOD2FebEnergy,1) = 0;
		cf.at(CF_TOD2MarEnergy,1) = 0;
		cf.at(CF_TOD2AprEnergy,1) = 0;
		cf.at(CF_TOD2MayEnergy,1) = 0;
		cf.at(CF_TOD2JunEnergy,1) = 0;
		cf.at(CF_TOD2JulEnergy,1) = 0;
		cf.at(CF_TOD2AugEnergy,1) = 0;
		cf.at(CF_TOD2SepEnergy,1) = 0;
		cf.at(CF_TOD2OctEnergy,1) = 0;
		cf.at(CF_TOD2NovEnergy,1) = 0;
		cf.at(CF_TOD2DecEnergy,1) = 0;
	
		cf.at(CF_TOD3JanEnergy,1) = 0;
		cf.at(CF_TOD3FebEnergy,1) = 0;
		cf.at(CF_TOD3MarEnergy,1) = 0;
		cf.at(CF_TOD3AprEnergy,1) = 0;
		cf.at(CF_TOD3MayEnergy,1) = 0;
		cf.at(CF_TOD3JunEnergy,1) = 0;
		cf.at(CF_TOD3JulEnergy,1) = 0;
		cf.at(CF_TOD3AugEnergy,1) = 0;
		cf.at(CF_TOD3SepEnergy,1) = 0;
		cf.at(CF_TOD3OctEnergy,1) = 0;
		cf.at(CF_TOD3NovEnergy,1) = 0;
		cf.at(CF_TOD3DecEnergy,1) = 0;
	
		cf.at(CF_TOD4JanEnergy,1) = 0;
		cf.at(CF_TOD4FebEnergy,1) = 0;
		cf.at(CF_TOD4MarEnergy,1) = 0;
		cf.at(CF_TOD4AprEnergy,1) = 0;
		cf.at(CF_TOD4MayEnergy,1) = 0;
		cf.at(CF_TOD4JunEnergy,1) = 0;
		cf.at(CF_TOD4JulEnergy,1) = 0;
		cf.at(CF_TOD4AugEnergy,1) = 0;
		cf.at(CF_TOD4SepEnergy,1) = 0;
		cf.at(CF_TOD4OctEnergy,1) = 0;
		cf.at(CF_TOD4NovEnergy,1) = 0;
		cf.at(CF_TOD4DecEnergy,1) = 0;
	
		cf.at(CF_TOD5JanEnergy,1) = 0;
		cf.at(CF_TOD5FebEnergy,1) = 0;
		cf.at(CF_TOD5MarEnergy,1) = 0;
		cf.at(CF_TOD5AprEnergy,1) = 0;
		cf.at(CF_TOD5MayEnergy,1) = 0;
		cf.at(CF_TOD5JunEnergy,1) = 0;
		cf.at(CF_TOD5JulEnergy,1) = 0;
		cf.at(CF_TOD5AugEnergy,1) = 0;
		cf.at(CF_TOD5SepEnergy,1) = 0;
		cf.at(CF_TOD5OctEnergy,1) = 0;
		cf.at(CF_TOD5NovEnergy,1) = 0;
		cf.at(CF_TOD5DecEnergy,1) = 0;
	
		cf.at(CF_TOD6JanEnergy,1) = 0;
		cf.at(CF_TOD6FebEnergy,1) = 0;
		cf.at(CF_TOD6MarEnergy,1) = 0;
		cf.at(CF_TOD6AprEnergy,1) = 0;
		cf.at(CF_TOD6MayEnergy,1) = 0;
		cf.at(CF_TOD6JunEnergy,1) = 0;
		cf.at(CF_TOD6JulEnergy,1) = 0;
		cf.at(CF_TOD6AugEnergy,1) = 0;
		cf.at(CF_TOD6SepEnergy,1) = 0;
		cf.at(CF_TOD6OctEnergy,1) = 0;
		cf.at(CF_TOD6NovEnergy,1) = 0;
		cf.at(CF_TOD6DecEnergy,1) = 0;
	
		cf.at(CF_TOD7JanEnergy,1) = 0;
		cf.at(CF_TOD7FebEnergy,1) = 0;
		cf.at(CF_TOD7MarEnergy,1) = 0;
		cf.at(CF_TOD7AprEnergy,1) = 0;
		cf.at(CF_TOD7MayEnergy,1) = 0;
		cf.at(CF_TOD7JunEnergy,1) = 0;
		cf.at(CF_TOD7JulEnergy,1) = 0;
		cf.at(CF_TOD7AugEnergy,1) = 0;
		cf.at(CF_TOD7SepEnergy,1) = 0;
		cf.at(CF_TOD7OctEnergy,1) = 0;
		cf.at(CF_TOD7NovEnergy,1) = 0;
		cf.at(CF_TOD7DecEnergy,1) = 0;
	
		cf.at(CF_TOD8JanEnergy,1) = 0;
		cf.at(CF_TOD8FebEnergy,1) = 0;
		cf.at(CF_TOD8MarEnergy,1) = 0;
		cf.at(CF_TOD8AprEnergy,1) = 0;
		cf.at(CF_TOD8MayEnergy,1) = 0;
		cf.at(CF_TOD8JunEnergy,1) = 0;
		cf.at(CF_TOD8JulEnergy,1) = 0;
		cf.at(CF_TOD8AugEnergy,1) = 0;
		cf.at(CF_TOD8SepEnergy,1) = 0;
		cf.at(CF_TOD8OctEnergy,1) = 0;
		cf.at(CF_TOD8NovEnergy,1) = 0;
		cf.at(CF_TOD8DecEnergy,1) = 0;
	
		cf.at(CF_TOD9JanEnergy,1) = 0;
		cf.at(CF_TOD9FebEnergy,1) = 0;
		cf.at(CF_TOD9MarEnergy,1) = 0;
		cf.at(CF_TOD9AprEnergy,1) = 0;
		cf.at(CF_TOD9MayEnergy,1) = 0;
		cf.at(CF_TOD9JunEnergy,1) = 0;
		cf.at(CF_TOD9JulEnergy,1) = 0;
		cf.at(CF_TOD9AugEnergy,1) = 0;
		cf.at(CF_TOD9SepEnergy,1) = 0;
		cf.at(CF_TOD9OctEnergy,1) = 0;
		cf.at(CF_TOD9NovEnergy,1) = 0;
		cf.at(CF_TOD9DecEnergy,1) = 0;
	
		int i=0;
		for (int m=0;m<12;m++)
		{
			for (int d=0;d<util::nday[m];d++)
			{
				for (int h=0;h<24&&i<8760 && m*24+h<288;h++)
				{
					switch (m)
					{
						case 0:
							cf.at(CF_TODJanEnergy,1) +=  hourly_enet[i];
							switch ((int)hourly_dispatch[i])
							{
								case 0:
									cf.at(CF_TOD1JanEnergy,1) +=  hourly_enet[i];
									break;
								case 1:
									cf.at(CF_TOD2JanEnergy,1) +=  hourly_enet[i];
									break;
								case 2:
									cf.at(CF_TOD3JanEnergy,1) +=  hourly_enet[i];
									break;
								case 3:
									cf.at(CF_TOD4JanEnergy,1) +=  hourly_enet[i];
									break;
								case 4:
									cf.at(CF_TOD5JanEnergy,1) +=  hourly_enet[i];
									break;
								case 5:
									cf.at(CF_TOD6JanEnergy,1) +=  hourly_enet[i];
									break;
								case 6:
									cf.at(CF_TOD7JanEnergy,1) +=  hourly_enet[i];
									break;
								case 7:
									cf.at(CF_TOD8JanEnergy,1) +=  hourly_enet[i];
									break;
								case 8:
									cf.at(CF_TOD9JanEnergy,1) +=  hourly_enet[i];
									break;
							}
							break;
						case 1:
							cf.at(CF_TODFebEnergy,1) +=  hourly_enet[i];
							switch ((int)hourly_dispatch[i])
							{
								case 0:
									cf.at(CF_TOD1FebEnergy,1) +=  hourly_enet[i];
									break;
								case 1:
									cf.at(CF_TOD2FebEnergy,1) +=  hourly_enet[i];
									break;
								case 2:
									cf.at(CF_TOD3FebEnergy,1) +=  hourly_enet[i];
									break;
								case 3:
									cf.at(CF_TOD4FebEnergy,1) +=  hourly_enet[i];
									break;
								case 4:
									cf.at(CF_TOD5FebEnergy,1) +=  hourly_enet[i];
									break;
								case 5:
									cf.at(CF_TOD6FebEnergy,1) +=  hourly_enet[i];
									break;
								case 6:
									cf.at(CF_TOD7FebEnergy,1) +=  hourly_enet[i];
									break;
								case 7:
									cf.at(CF_TOD8FebEnergy,1) +=  hourly_enet[i];
									break;
								case 8:
									cf.at(CF_TOD9FebEnergy,1) +=  hourly_enet[i];
									break;
							}
							break;
						case 2:
							cf.at(CF_TODMarEnergy,1) +=  hourly_enet[i];
							switch ((int)hourly_dispatch[i])
							{
								case 0:
									cf.at(CF_TOD1MarEnergy,1) +=  hourly_enet[i];
									break;
								case 1:
									cf.at(CF_TOD2MarEnergy,1) +=  hourly_enet[i];
									break;
								case 2:
									cf.at(CF_TOD3MarEnergy,1) +=  hourly_enet[i];
									break;
								case 3:
									cf.at(CF_TOD4MarEnergy,1) +=  hourly_enet[i];
									break;
								case 4:
									cf.at(CF_TOD5MarEnergy,1) +=  hourly_enet[i];
									break;
								case 5:
									cf.at(CF_TOD6MarEnergy,1) +=  hourly_enet[i];
									break;
								case 6:
									cf.at(CF_TOD7MarEnergy,1) +=  hourly_enet[i];
									break;
								case 7:
									cf.at(CF_TOD8MarEnergy,1) +=  hourly_enet[i];
									break;
								case 8:
									cf.at(CF_TOD9MarEnergy,1) +=  hourly_enet[i];
									break;
							}
							break;
						case 3:
							cf.at(CF_TODAprEnergy,1) +=  hourly_enet[i];
							switch ((int)hourly_dispatch[i])
							{
								case 0:
									cf.at(CF_TOD1AprEnergy,1) +=  hourly_enet[i];
									break;
								case 1:
									cf.at(CF_TOD2AprEnergy,1) +=  hourly_enet[i];
									break;
								case 2:
									cf.at(CF_TOD3AprEnergy,1) +=  hourly_enet[i];
									break;
								case 3:
									cf.at(CF_TOD4AprEnergy,1) +=  hourly_enet[i];
									break;
								case 4:
									cf.at(CF_TOD5AprEnergy,1) +=  hourly_enet[i];
									break;
								case 5:
									cf.at(CF_TOD6AprEnergy,1) +=  hourly_enet[i];
									break;
								case 6:
									cf.at(CF_TOD7AprEnergy,1) +=  hourly_enet[i];
									break;
								case 7:
									cf.at(CF_TOD8AprEnergy,1) +=  hourly_enet[i];
									break;
								case 8:
									cf.at(CF_TOD9AprEnergy,1) +=  hourly_enet[i];
									break;
							}
							break;
						case 4:
							cf.at(CF_TODMayEnergy,1) +=  hourly_enet[i];
							switch ((int)hourly_dispatch[i])
							{
								case 0:
									cf.at(CF_TOD1MayEnergy,1) +=  hourly_enet[i];
									break;
								case 1:
									cf.at(CF_TOD2MayEnergy,1) +=  hourly_enet[i];
									break;
								case 2:
									cf.at(CF_TOD3MayEnergy,1) +=  hourly_enet[i];
									break;
								case 3:
									cf.at(CF_TOD4MayEnergy,1) +=  hourly_enet[i];
									break;
								case 4:
									cf.at(CF_TOD5MayEnergy,1) +=  hourly_enet[i];
									break;
								case 5:
									cf.at(CF_TOD6MayEnergy,1) +=  hourly_enet[i];
									break;
								case 6:
									cf.at(CF_TOD7MayEnergy,1) +=  hourly_enet[i];
									break;
								case 7:
									cf.at(CF_TOD8MayEnergy,1) +=  hourly_enet[i];
									break;
								case 8:
									cf.at(CF_TOD9MayEnergy,1) +=  hourly_enet[i];
									break;
							}
							break;
						case 5:
							cf.at(CF_TODJunEnergy,1) +=  hourly_enet[i];
							switch ((int)hourly_dispatch[i])
							{
								case 0:
									cf.at(CF_TOD1JunEnergy,1) +=  hourly_enet[i];
									break;
								case 1:
									cf.at(CF_TOD2JunEnergy,1) +=  hourly_enet[i];
									break;
								case 2:
									cf.at(CF_TOD3JunEnergy,1) +=  hourly_enet[i];
									break;
								case 3:
									cf.at(CF_TOD4JunEnergy,1) +=  hourly_enet[i];
									break;
								case 4:
									cf.at(CF_TOD5JunEnergy,1) +=  hourly_enet[i];
									break;
								case 5:
									cf.at(CF_TOD6JunEnergy,1) +=  hourly_enet[i];
									break;
								case 6:
									cf.at(CF_TOD7JunEnergy,1) +=  hourly_enet[i];
									break;
								case 7:
									cf.at(CF_TOD8JunEnergy,1) +=  hourly_enet[i];
									break;
								case 8:
									cf.at(CF_TOD9JunEnergy,1) +=  hourly_enet[i];
									break;
							}
							break;
						case 6:
							cf.at(CF_TODJulEnergy,1) +=  hourly_enet[i];
							switch ((int)hourly_dispatch[i])
							{
								case 0:
									cf.at(CF_TOD1JulEnergy,1) +=  hourly_enet[i];
									break;
								case 1:
									cf.at(CF_TOD2JulEnergy,1) +=  hourly_enet[i];
									break;
								case 2:
									cf.at(CF_TOD3JulEnergy,1) +=  hourly_enet[i];
									break;
								case 3:
									cf.at(CF_TOD4JulEnergy,1) +=  hourly_enet[i];
									break;
								case 4:
									cf.at(CF_TOD5JulEnergy,1) +=  hourly_enet[i];
									break;
								case 5:
									cf.at(CF_TOD6JulEnergy,1) +=  hourly_enet[i];
									break;
								case 6:
									cf.at(CF_TOD7JulEnergy,1) +=  hourly_enet[i];
									break;
								case 7:
									cf.at(CF_TOD8JulEnergy,1) +=  hourly_enet[i];
									break;
								case 8:
									cf.at(CF_TOD9JulEnergy,1) +=  hourly_enet[i];
									break;
							}
							break;
						case 7:
							cf.at(CF_TODAugEnergy,1) +=  hourly_enet[i];
							switch ((int)hourly_dispatch[i])
							{
								case 0:
									cf.at(CF_TOD1AugEnergy,1) +=  hourly_enet[i];
									break;
								case 1:
									cf.at(CF_TOD2AugEnergy,1) +=  hourly_enet[i];
									break;
								case 2:
									cf.at(CF_TOD3AugEnergy,1) +=  hourly_enet[i];
									break;
								case 3:
									cf.at(CF_TOD4AugEnergy,1) +=  hourly_enet[i];
									break;
								case 4:
									cf.at(CF_TOD5AugEnergy,1) +=  hourly_enet[i];
									break;
								case 5:
									cf.at(CF_TOD6AugEnergy,1) +=  hourly_enet[i];
									break;
								case 6:
									cf.at(CF_TOD7AugEnergy,1) +=  hourly_enet[i];
									break;
								case 7:
									cf.at(CF_TOD8AugEnergy,1) +=  hourly_enet[i];
									break;
								case 8:
									cf.at(CF_TOD9AugEnergy,1) +=  hourly_enet[i];
									break;
							}
							break;
						case 8:
							cf.at(CF_TODSepEnergy,1) +=  hourly_enet[i];
							switch ((int)hourly_dispatch[i])
							{
								case 0:
									cf.at(CF_TOD1SepEnergy,1) +=  hourly_enet[i];
									break;
								case 1:
									cf.at(CF_TOD2SepEnergy,1) +=  hourly_enet[i];
									break;
								case 2:
									cf.at(CF_TOD3SepEnergy,1) +=  hourly_enet[i];
									break;
								case 3:
									cf.at(CF_TOD4SepEnergy,1) +=  hourly_enet[i];
									break;
								case 4:
									cf.at(CF_TOD5SepEnergy,1) +=  hourly_enet[i];
									break;
								case 5:
									cf.at(CF_TOD6SepEnergy,1) +=  hourly_enet[i];
									break;
								case 6:
									cf.at(CF_TOD7SepEnergy,1) +=  hourly_enet[i];
									break;
								case 7:
									cf.at(CF_TOD8SepEnergy,1) +=  hourly_enet[i];
									break;
								case 8:
									cf.at(CF_TOD9SepEnergy,1) +=  hourly_enet[i];
									break;
							}
							break;
						case 9:
							cf.at(CF_TODOctEnergy,1) +=  hourly_enet[i];
							switch ((int)hourly_dispatch[i])
							{
								case 0:
									cf.at(CF_TOD1OctEnergy,1) +=  hourly_enet[i];
									break;
								case 1:
									cf.at(CF_TOD2OctEnergy,1) +=  hourly_enet[i];
									break;
								case 2:
									cf.at(CF_TOD3OctEnergy,1) +=  hourly_enet[i];
									break;
								case 3:
									cf.at(CF_TOD4OctEnergy,1) +=  hourly_enet[i];
									break;
								case 4:
									cf.at(CF_TOD5OctEnergy,1) +=  hourly_enet[i];
									break;
								case 5:
									cf.at(CF_TOD6OctEnergy,1) +=  hourly_enet[i];
									break;
								case 6:
									cf.at(CF_TOD7OctEnergy,1) +=  hourly_enet[i];
									break;
								case 7:
									cf.at(CF_TOD8OctEnergy,1) +=  hourly_enet[i];
									break;
								case 8:
									cf.at(CF_TOD9OctEnergy,1) +=  hourly_enet[i];
									break;
							}
							break;
						case 10:
							cf.at(CF_TODNovEnergy,1) +=  hourly_enet[i];
							switch ((int)hourly_dispatch[i])
							{
								case 0:
									cf.at(CF_TOD1NovEnergy,1) +=  hourly_enet[i];
									break;
								case 1:
									cf.at(CF_TOD2NovEnergy,1) +=  hourly_enet[i];
									break;
								case 2:
									cf.at(CF_TOD3NovEnergy,1) +=  hourly_enet[i];
									break;
								case 3:
									cf.at(CF_TOD4NovEnergy,1) +=  hourly_enet[i];
									break;
								case 4:
									cf.at(CF_TOD5NovEnergy,1) +=  hourly_enet[i];
									break;
								case 5:
									cf.at(CF_TOD6NovEnergy,1) +=  hourly_enet[i];
									break;
								case 6:
									cf.at(CF_TOD7NovEnergy,1) +=  hourly_enet[i];
									break;
								case 7:
									cf.at(CF_TOD8NovEnergy,1) +=  hourly_enet[i];
									break;
								case 8:
									cf.at(CF_TOD9NovEnergy,1) +=  hourly_enet[i];
									break;
							}
							break;
						case 11:
							cf.at(CF_TODDecEnergy,1) +=  hourly_enet[i];
							switch ((int)hourly_dispatch[i])
							{
								case 0:
									cf.at(CF_TOD1DecEnergy,1) +=  hourly_enet[i];
									break;
								case 1:
									cf.at(CF_TOD2DecEnergy,1) +=  hourly_enet[i];
									break;
								case 2:
									cf.at(CF_TOD3DecEnergy,1) +=  hourly_enet[i];
									break;
								case 3:
									cf.at(CF_TOD4DecEnergy,1) +=  hourly_enet[i];
									break;
								case 4:
									cf.at(CF_TOD5DecEnergy,1) +=  hourly_enet[i];
									break;
								case 5:
									cf.at(CF_TOD6DecEnergy,1) +=  hourly_enet[i];
									break;
								case 6:
									cf.at(CF_TOD7DecEnergy,1) +=  hourly_enet[i];
									break;
								case 7:
									cf.at(CF_TOD8DecEnergy,1) +=  hourly_enet[i];
									break;
								case 8:
									cf.at(CF_TOD9DecEnergy,1) +=  hourly_enet[i];
									break;
							}
							break;
					}
					i++;					
				}
			}
		}

		for (int y=2;y<=nyears;y++)
		{
	// compute energy dispatched
			cf.at(CF_TODJanEnergy,y) = cf.at(CF_TODJanEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TODFebEnergy,y) = cf.at(CF_TODFebEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TODMarEnergy,y) = cf.at(CF_TODMarEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TODAprEnergy,y) = cf.at(CF_TODAprEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TODMayEnergy,y) = cf.at(CF_TODMayEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TODJunEnergy,y) = cf.at(CF_TODJunEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TODJulEnergy,y) = cf.at(CF_TODJulEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TODAugEnergy,y) = cf.at(CF_TODAugEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TODSepEnergy,y) = cf.at(CF_TODSepEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TODOctEnergy,y) = cf.at(CF_TODOctEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TODNovEnergy,y) = cf.at(CF_TODNovEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TODDecEnergy,y) = cf.at(CF_TODDecEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);

			cf.at(CF_TOD1JanEnergy,y) = cf.at(CF_TOD1JanEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD1FebEnergy,y) = cf.at(CF_TOD1FebEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD1MarEnergy,y) = cf.at(CF_TOD1MarEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD1AprEnergy,y) = cf.at(CF_TOD1AprEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD1MayEnergy,y) = cf.at(CF_TOD1MayEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD1JunEnergy,y) = cf.at(CF_TOD1JunEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD1JulEnergy,y) = cf.at(CF_TOD1JulEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD1AugEnergy,y) = cf.at(CF_TOD1AugEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD1SepEnergy,y) = cf.at(CF_TOD1SepEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD1OctEnergy,y) = cf.at(CF_TOD1OctEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD1NovEnergy,y) = cf.at(CF_TOD1NovEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD1DecEnergy,y) = cf.at(CF_TOD1DecEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);

			cf.at(CF_TOD2JanEnergy,y) = cf.at(CF_TOD2JanEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD2FebEnergy,y) = cf.at(CF_TOD2FebEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD2MarEnergy,y) = cf.at(CF_TOD2MarEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD2AprEnergy,y) = cf.at(CF_TOD2AprEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD2MayEnergy,y) = cf.at(CF_TOD2MayEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD2JunEnergy,y) = cf.at(CF_TOD2JunEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD2JulEnergy,y) = cf.at(CF_TOD2JulEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD2AugEnergy,y) = cf.at(CF_TOD2AugEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD2SepEnergy,y) = cf.at(CF_TOD2SepEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD2OctEnergy,y) = cf.at(CF_TOD2OctEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD2NovEnergy,y) = cf.at(CF_TOD2NovEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD2DecEnergy,y) = cf.at(CF_TOD2DecEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);

			cf.at(CF_TOD3JanEnergy,y) = cf.at(CF_TOD3JanEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD3FebEnergy,y) = cf.at(CF_TOD3FebEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD3MarEnergy,y) = cf.at(CF_TOD3MarEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD3AprEnergy,y) = cf.at(CF_TOD3AprEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD3MayEnergy,y) = cf.at(CF_TOD3MayEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD3JunEnergy,y) = cf.at(CF_TOD3JunEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD3JulEnergy,y) = cf.at(CF_TOD3JulEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD3AugEnergy,y) = cf.at(CF_TOD3AugEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD3SepEnergy,y) = cf.at(CF_TOD3SepEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD3OctEnergy,y) = cf.at(CF_TOD3OctEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD3NovEnergy,y) = cf.at(CF_TOD3NovEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD3DecEnergy,y) = cf.at(CF_TOD3DecEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);

			cf.at(CF_TOD4JanEnergy,y) = cf.at(CF_TOD4JanEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD4FebEnergy,y) = cf.at(CF_TOD4FebEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD4MarEnergy,y) = cf.at(CF_TOD4MarEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD4AprEnergy,y) = cf.at(CF_TOD4AprEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD4MayEnergy,y) = cf.at(CF_TOD4MayEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD4JunEnergy,y) = cf.at(CF_TOD4JunEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD4JulEnergy,y) = cf.at(CF_TOD4JulEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD4AugEnergy,y) = cf.at(CF_TOD4AugEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD4SepEnergy,y) = cf.at(CF_TOD4SepEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD4OctEnergy,y) = cf.at(CF_TOD4OctEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD4NovEnergy,y) = cf.at(CF_TOD4NovEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD4DecEnergy,y) = cf.at(CF_TOD4DecEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);

			cf.at(CF_TOD5JanEnergy,y) = cf.at(CF_TOD5JanEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD5FebEnergy,y) = cf.at(CF_TOD5FebEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD5MarEnergy,y) = cf.at(CF_TOD5MarEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD5AprEnergy,y) = cf.at(CF_TOD5AprEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD5MayEnergy,y) = cf.at(CF_TOD5MayEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD5JunEnergy,y) = cf.at(CF_TOD5JunEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD5JulEnergy,y) = cf.at(CF_TOD5JulEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD5AugEnergy,y) = cf.at(CF_TOD5AugEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD5SepEnergy,y) = cf.at(CF_TOD5SepEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD5OctEnergy,y) = cf.at(CF_TOD5OctEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD5NovEnergy,y) = cf.at(CF_TOD5NovEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD5DecEnergy,y) = cf.at(CF_TOD5DecEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);

			cf.at(CF_TOD6JanEnergy,y) = cf.at(CF_TOD6JanEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD6FebEnergy,y) = cf.at(CF_TOD6FebEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD6MarEnergy,y) = cf.at(CF_TOD6MarEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD6AprEnergy,y) = cf.at(CF_TOD6AprEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD6MayEnergy,y) = cf.at(CF_TOD6MayEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD6JunEnergy,y) = cf.at(CF_TOD6JunEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD6JulEnergy,y) = cf.at(CF_TOD6JulEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD6AugEnergy,y) = cf.at(CF_TOD6AugEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD6SepEnergy,y) = cf.at(CF_TOD6SepEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD6OctEnergy,y) = cf.at(CF_TOD6OctEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD6NovEnergy,y) = cf.at(CF_TOD6NovEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD6DecEnergy,y) = cf.at(CF_TOD6DecEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);

			cf.at(CF_TOD7JanEnergy,y) = cf.at(CF_TOD7JanEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD7FebEnergy,y) = cf.at(CF_TOD7FebEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD7MarEnergy,y) = cf.at(CF_TOD7MarEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD7AprEnergy,y) = cf.at(CF_TOD7AprEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD7MayEnergy,y) = cf.at(CF_TOD7MayEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD7JunEnergy,y) = cf.at(CF_TOD7JunEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD7JulEnergy,y) = cf.at(CF_TOD7JulEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD7AugEnergy,y) = cf.at(CF_TOD7AugEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD7SepEnergy,y) = cf.at(CF_TOD7SepEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD7OctEnergy,y) = cf.at(CF_TOD7OctEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD7NovEnergy,y) = cf.at(CF_TOD7NovEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD7DecEnergy,y) = cf.at(CF_TOD7DecEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);

			cf.at(CF_TOD8JanEnergy,y) = cf.at(CF_TOD8JanEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD8FebEnergy,y) = cf.at(CF_TOD8FebEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD8MarEnergy,y) = cf.at(CF_TOD8MarEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD8AprEnergy,y) = cf.at(CF_TOD8AprEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD8MayEnergy,y) = cf.at(CF_TOD8MayEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD8JunEnergy,y) = cf.at(CF_TOD8JunEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD8JulEnergy,y) = cf.at(CF_TOD8JulEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD8AugEnergy,y) = cf.at(CF_TOD8AugEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD8SepEnergy,y) = cf.at(CF_TOD8SepEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD8OctEnergy,y) = cf.at(CF_TOD8OctEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD8NovEnergy,y) = cf.at(CF_TOD8NovEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD8DecEnergy,y) = cf.at(CF_TOD8DecEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);

			cf.at(CF_TOD9JanEnergy,y) = cf.at(CF_TOD9JanEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD9FebEnergy,y) = cf.at(CF_TOD9FebEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD9MarEnergy,y) = cf.at(CF_TOD9MarEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD9AprEnergy,y) = cf.at(CF_TOD9AprEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD9MayEnergy,y) = cf.at(CF_TOD9MayEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD9JunEnergy,y) = cf.at(CF_TOD9JunEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD9JulEnergy,y) = cf.at(CF_TOD9JulEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD9AugEnergy,y) = cf.at(CF_TOD9AugEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD9SepEnergy,y) = cf.at(CF_TOD9SepEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD9OctEnergy,y) = cf.at(CF_TOD9OctEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD9NovEnergy,y) = cf.at(CF_TOD9NovEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
			cf.at(CF_TOD9DecEnergy,y) = cf.at(CF_TOD9DecEnergy,1) * cf.at(CF_Degradation,y) * cf.at(CF_Availability,y);
		}


		cf.at(CF_TODJanEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TODFebEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TODMarEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TODAprEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TODMayEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TODJunEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TODJulEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TODAugEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TODSepEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TODOctEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TODNovEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TODDecEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);

		cf.at(CF_TOD1JanEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD1FebEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD1MarEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD1AprEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD1MayEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD1JunEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD1JulEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD1AugEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD1SepEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD1OctEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD1NovEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD1DecEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);

		cf.at(CF_TOD2JanEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD2FebEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD2MarEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD2AprEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD2MayEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD2JunEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD2JulEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD2AugEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD2SepEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD2OctEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD2NovEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD2DecEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);

		cf.at(CF_TOD3JanEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD3FebEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD3MarEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD3AprEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD3MayEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD3JunEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD3JulEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD3AugEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD3SepEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD3OctEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD3NovEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD3DecEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);

		cf.at(CF_TOD4JanEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD4FebEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD4MarEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD4AprEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD4MayEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD4JunEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD4JulEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD4AugEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD4SepEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD4OctEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD4NovEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD4DecEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);

		cf.at(CF_TOD5JanEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD5FebEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD5MarEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD5AprEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD5MayEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD5JunEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD5JulEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD5AugEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD5SepEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD5OctEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD5NovEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD5DecEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);

		cf.at(CF_TOD6JanEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD6FebEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD6MarEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD6AprEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD6MayEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD6JunEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD6JulEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD6AugEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD6SepEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD6OctEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD6NovEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD6DecEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);

		cf.at(CF_TOD7JanEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD7FebEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD7MarEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD7AprEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD7MayEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD7JunEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD7JulEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD7AugEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD7SepEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD7OctEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD7NovEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD7DecEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);

		cf.at(CF_TOD8JanEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD8FebEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD8MarEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD8AprEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD8MayEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD8JunEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD8JulEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD8AugEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD8SepEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD8OctEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD8NovEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD8DecEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);

		cf.at(CF_TOD9JanEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD9FebEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD9MarEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD9AprEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD9MayEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD9JunEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD9JulEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD9AugEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD9SepEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD9OctEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD9NovEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);
		cf.at(CF_TOD9DecEnergy,1) *= cf.at(CF_Degradation,1) * cf.at(CF_Availability,1);

		return true;
	}


	bool compute_lifetime_dispatch_output(int nyears)
	{
	//Calculate energy dispatched in each dispatch period 
		ssc_number_t *hourly_dispatch; // tou period 
		ssc_number_t *hourly_enet; // hourly energy output


		int h;
		size_t count;

	// hourly energy
		hourly_enet = as_array("energy_net_hourly", &count );
		if ( (int)count != (8760*nyears))
		{
			std::stringstream outm;
			outm <<  "Bad hourly dispatch output length (" << count << "), should be (analysis period-1) * 8760 value (" << 8760*nyears << ")";
			log( outm.str() );
			return false;
		}

	// hourly dispatch
		hourly_dispatch = as_array("dispatch_hourly", &count );
		if ( (int)count != 8760)
		{
			std::stringstream outm;
			outm <<  "Bad hourly dispatch output length (" << count << "), should be 8760";
			log( outm.str() );
			return false;
		}



		for (int y=1;y<=nyears;y++)
		{
			cf.at(CF_TOD1Energy,y) = 0;
			cf.at(CF_TOD2Energy,y) = 0;
			cf.at(CF_TOD3Energy,y) = 0;
			cf.at(CF_TOD4Energy,y) = 0;
			cf.at(CF_TOD5Energy,y) = 0;
			cf.at(CF_TOD6Energy,y) = 0;
			cf.at(CF_TOD7Energy,y) = 0;
			cf.at(CF_TOD8Energy,y) = 0;
			cf.at(CF_TOD9Energy,y) = 0;

			for (h=0;h<8760;h++)
			{
				switch ((int)hourly_dispatch[h])
				{
					case 0:
						cf.at(CF_TOD1Energy,y) += hourly_enet[(y-1)*8760+h] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
						break;
					case 1:
						cf.at(CF_TOD2Energy,y) += hourly_enet[(y-1)*8760+h] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
						break;
					case 2:
						cf.at(CF_TOD3Energy,y) += hourly_enet[(y-1)*8760+h] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
						break;
					case 3:
						cf.at(CF_TOD4Energy,y) += hourly_enet[(y-1)*8760+h] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
						break;
					case 4:
						cf.at(CF_TOD5Energy,y) += hourly_enet[(y-1)*8760+h] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
						break;
					case 5:
						cf.at(CF_TOD6Energy,y) += hourly_enet[(y-1)*8760+h] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
						break;
					case 6:
						cf.at(CF_TOD7Energy,y) += hourly_enet[(y-1)*8760+h] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
						break;
					case 7:
						cf.at(CF_TOD8Energy,y) += hourly_enet[(y-1)*8760+h] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
						break;
					case 8:
						cf.at(CF_TOD9Energy,y) += hourly_enet[(y-1)*8760+h] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
						break;
				}
			}
			cf.at(CF_energy_net,y) = 
				cf.at(CF_TOD1Energy,y) +
				cf.at(CF_TOD2Energy,y) +
				cf.at(CF_TOD3Energy,y) +
				cf.at(CF_TOD4Energy,y) +
				cf.at(CF_TOD5Energy,y) +
				cf.at(CF_TOD6Energy,y) +
				cf.at(CF_TOD7Energy,y) +
				cf.at(CF_TOD8Energy,y) +
				cf.at(CF_TOD9Energy,y) ;

		}


		return true;
	}

	bool process_lifetime_dispatch_output(int nyears)
	{
	//Calculate energy dispatched in each dispatch period 
		ssc_number_t *hourly_dispatch; // tou period 
		ssc_number_t *hourly_enet; // hourly energy output

		size_t count;

	// hourly energy
		hourly_enet = as_array("energy_net_hourly", &count );
		if ( (int)count != (8760*nyears))
		{
			std::stringstream outm;
			outm <<  "Bad hourly dispatch output length (" << count << "), should be (analysis period-1) * 8760 value (" << 8760*nyears << ")";
			log( outm.str() );
			return false;
		}

	// hourly dispatch
		hourly_dispatch = as_array("dispatch_hourly", &count );
		if ( count != 8760)
		{
			std::stringstream outm;
			outm <<  "Bad hourly dispatch output length (" << count << "), should be 8760";
			log( outm.str() );
			return false;
		}


		for (int y=1;y<=nyears;y++)
		{
			cf.at(CF_TODJanEnergy,y) = 0;
			cf.at(CF_TODFebEnergy,y) = 0;
			cf.at(CF_TODMarEnergy,y) = 0;
			cf.at(CF_TODAprEnergy,y) = 0;
			cf.at(CF_TODMayEnergy,y) = 0;
			cf.at(CF_TODJunEnergy,y) = 0;
			cf.at(CF_TODJulEnergy,y) = 0;
			cf.at(CF_TODAugEnergy,y) = 0;
			cf.at(CF_TODSepEnergy,y) = 0;
			cf.at(CF_TODOctEnergy,y) = 0;
			cf.at(CF_TODNovEnergy,y) = 0;
			cf.at(CF_TODDecEnergy,y) = 0;

			cf.at(CF_TOD1JanEnergy,y) = 0;
			cf.at(CF_TOD1FebEnergy,y) = 0;
			cf.at(CF_TOD1MarEnergy,y) = 0;
			cf.at(CF_TOD1AprEnergy,y) = 0;
			cf.at(CF_TOD1MayEnergy,y) = 0;
			cf.at(CF_TOD1JunEnergy,y) = 0;
			cf.at(CF_TOD1JulEnergy,y) = 0;
			cf.at(CF_TOD1AugEnergy,y) = 0;
			cf.at(CF_TOD1SepEnergy,y) = 0;
			cf.at(CF_TOD1OctEnergy,y) = 0;
			cf.at(CF_TOD1NovEnergy,y) = 0;
			cf.at(CF_TOD1DecEnergy,y) = 0;
		
			cf.at(CF_TOD2JanEnergy,y) = 0;
			cf.at(CF_TOD2FebEnergy,y) = 0;
			cf.at(CF_TOD2MarEnergy,y) = 0;
			cf.at(CF_TOD2AprEnergy,y) = 0;
			cf.at(CF_TOD2MayEnergy,y) = 0;
			cf.at(CF_TOD2JunEnergy,y) = 0;
			cf.at(CF_TOD2JulEnergy,y) = 0;
			cf.at(CF_TOD2AugEnergy,y) = 0;
			cf.at(CF_TOD2SepEnergy,y) = 0;
			cf.at(CF_TOD2OctEnergy,y) = 0;
			cf.at(CF_TOD2NovEnergy,y) = 0;
			cf.at(CF_TOD2DecEnergy,y) = 0;
		
			cf.at(CF_TOD3JanEnergy,y) = 0;
			cf.at(CF_TOD3FebEnergy,y) = 0;
			cf.at(CF_TOD3MarEnergy,y) = 0;
			cf.at(CF_TOD3AprEnergy,y) = 0;
			cf.at(CF_TOD3MayEnergy,y) = 0;
			cf.at(CF_TOD3JunEnergy,y) = 0;
			cf.at(CF_TOD3JulEnergy,y) = 0;
			cf.at(CF_TOD3AugEnergy,y) = 0;
			cf.at(CF_TOD3SepEnergy,y) = 0;
			cf.at(CF_TOD3OctEnergy,y) = 0;
			cf.at(CF_TOD3NovEnergy,y) = 0;
			cf.at(CF_TOD3DecEnergy,y) = 0;
		
			cf.at(CF_TOD4JanEnergy,y) = 0;
			cf.at(CF_TOD4FebEnergy,y) = 0;
			cf.at(CF_TOD4MarEnergy,y) = 0;
			cf.at(CF_TOD4AprEnergy,y) = 0;
			cf.at(CF_TOD4MayEnergy,y) = 0;
			cf.at(CF_TOD4JunEnergy,y) = 0;
			cf.at(CF_TOD4JulEnergy,y) = 0;
			cf.at(CF_TOD4AugEnergy,y) = 0;
			cf.at(CF_TOD4SepEnergy,y) = 0;
			cf.at(CF_TOD4OctEnergy,y) = 0;
			cf.at(CF_TOD4NovEnergy,y) = 0;
			cf.at(CF_TOD4DecEnergy,y) = 0;
		
			cf.at(CF_TOD5JanEnergy,y) = 0;
			cf.at(CF_TOD5FebEnergy,y) = 0;
			cf.at(CF_TOD5MarEnergy,y) = 0;
			cf.at(CF_TOD5AprEnergy,y) = 0;
			cf.at(CF_TOD5MayEnergy,y) = 0;
			cf.at(CF_TOD5JunEnergy,y) = 0;
			cf.at(CF_TOD5JulEnergy,y) = 0;
			cf.at(CF_TOD5AugEnergy,y) = 0;
			cf.at(CF_TOD5SepEnergy,y) = 0;
			cf.at(CF_TOD5OctEnergy,y) = 0;
			cf.at(CF_TOD5NovEnergy,y) = 0;
			cf.at(CF_TOD5DecEnergy,y) = 0;
		
			cf.at(CF_TOD6JanEnergy,y) = 0;
			cf.at(CF_TOD6FebEnergy,y) = 0;
			cf.at(CF_TOD6MarEnergy,y) = 0;
			cf.at(CF_TOD6AprEnergy,y) = 0;
			cf.at(CF_TOD6MayEnergy,y) = 0;
			cf.at(CF_TOD6JunEnergy,y) = 0;
			cf.at(CF_TOD6JulEnergy,y) = 0;
			cf.at(CF_TOD6AugEnergy,y) = 0;
			cf.at(CF_TOD6SepEnergy,y) = 0;
			cf.at(CF_TOD6OctEnergy,y) = 0;
			cf.at(CF_TOD6NovEnergy,y) = 0;
			cf.at(CF_TOD6DecEnergy,y) = 0;
		
			cf.at(CF_TOD7JanEnergy,y) = 0;
			cf.at(CF_TOD7FebEnergy,y) = 0;
			cf.at(CF_TOD7MarEnergy,y) = 0;
			cf.at(CF_TOD7AprEnergy,y) = 0;
			cf.at(CF_TOD7MayEnergy,y) = 0;
			cf.at(CF_TOD7JunEnergy,y) = 0;
			cf.at(CF_TOD7JulEnergy,y) = 0;
			cf.at(CF_TOD7AugEnergy,y) = 0;
			cf.at(CF_TOD7SepEnergy,y) = 0;
			cf.at(CF_TOD7OctEnergy,y) = 0;
			cf.at(CF_TOD7NovEnergy,y) = 0;
			cf.at(CF_TOD7DecEnergy,y) = 0;
		
			cf.at(CF_TOD8JanEnergy,y) = 0;
			cf.at(CF_TOD8FebEnergy,y) = 0;
			cf.at(CF_TOD8MarEnergy,y) = 0;
			cf.at(CF_TOD8AprEnergy,y) = 0;
			cf.at(CF_TOD8MayEnergy,y) = 0;
			cf.at(CF_TOD8JunEnergy,y) = 0;
			cf.at(CF_TOD8JulEnergy,y) = 0;
			cf.at(CF_TOD8AugEnergy,y) = 0;
			cf.at(CF_TOD8SepEnergy,y) = 0;
			cf.at(CF_TOD8OctEnergy,y) = 0;
			cf.at(CF_TOD8NovEnergy,y) = 0;
			cf.at(CF_TOD8DecEnergy,y) = 0;
		
			cf.at(CF_TOD9JanEnergy,y) = 0;
			cf.at(CF_TOD9FebEnergy,y) = 0;
			cf.at(CF_TOD9MarEnergy,y) = 0;
			cf.at(CF_TOD9AprEnergy,y) = 0;
			cf.at(CF_TOD9MayEnergy,y) = 0;
			cf.at(CF_TOD9JunEnergy,y) = 0;
			cf.at(CF_TOD9JulEnergy,y) = 0;
			cf.at(CF_TOD9AugEnergy,y) = 0;
			cf.at(CF_TOD9SepEnergy,y) = 0;
			cf.at(CF_TOD9OctEnergy,y) = 0;
			cf.at(CF_TOD9NovEnergy,y) = 0;
			cf.at(CF_TOD9DecEnergy,y) = 0;
		
			int i=0;
			for (int m=0;m<12;m++)
			{
				for (int d=0;d<util::nday[m];d++)
				{
					for (int h=0;h<24&&i<8760 && m*24+h<288;h++)
					{
						switch (m)
						{
							case 0:
								cf.at(CF_TODJanEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
								switch ((int)hourly_dispatch[i])
								{
									case 0:
										cf.at(CF_TOD1JanEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 1:
										cf.at(CF_TOD2JanEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 2:
										cf.at(CF_TOD3JanEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 3:
										cf.at(CF_TOD4JanEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 4:
										cf.at(CF_TOD5JanEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 5:
										cf.at(CF_TOD6JanEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 6:
										cf.at(CF_TOD7JanEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 7:
										cf.at(CF_TOD8JanEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 8:
										cf.at(CF_TOD9JanEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
								}
								break;
							case 1:
								cf.at(CF_TODFebEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
								switch ((int)hourly_dispatch[i])
								{
									case 0:
										cf.at(CF_TOD1FebEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 1:
										cf.at(CF_TOD2FebEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 2:
										cf.at(CF_TOD3FebEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 3:
										cf.at(CF_TOD4FebEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 4:
										cf.at(CF_TOD5FebEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 5:
										cf.at(CF_TOD6FebEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 6:
										cf.at(CF_TOD7FebEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 7:
										cf.at(CF_TOD8FebEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 8:
										cf.at(CF_TOD9FebEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
								}
								break;
							case 2:
								cf.at(CF_TODMarEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
								switch ((int)hourly_dispatch[i])
								{
									case 0:
										cf.at(CF_TOD1MarEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 1:
										cf.at(CF_TOD2MarEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 2:
										cf.at(CF_TOD3MarEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 3:
										cf.at(CF_TOD4MarEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 4:
										cf.at(CF_TOD5MarEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 5:
										cf.at(CF_TOD6MarEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 6:
										cf.at(CF_TOD7MarEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 7:
										cf.at(CF_TOD8MarEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 8:
										cf.at(CF_TOD9MarEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
								}
								break;
							case 3:
								cf.at(CF_TODAprEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
								switch ((int)hourly_dispatch[i])
								{
									case 0:
										cf.at(CF_TOD1AprEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 1:
										cf.at(CF_TOD2AprEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 2:
										cf.at(CF_TOD3AprEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 3:
										cf.at(CF_TOD4AprEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 4:
										cf.at(CF_TOD5AprEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 5:
										cf.at(CF_TOD6AprEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 6:
										cf.at(CF_TOD7AprEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 7:
										cf.at(CF_TOD8AprEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 8:
										cf.at(CF_TOD9AprEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
								}
								break;
							case 4:
								cf.at(CF_TODMayEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
								switch ((int)hourly_dispatch[i])
								{
									case 0:
										cf.at(CF_TOD1MayEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 1:
										cf.at(CF_TOD2MayEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 2:
										cf.at(CF_TOD3MayEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 3:
										cf.at(CF_TOD4MayEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 4:
										cf.at(CF_TOD5MayEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 5:
										cf.at(CF_TOD6MayEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 6:
										cf.at(CF_TOD7MayEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 7:
										cf.at(CF_TOD8MayEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 8:
										cf.at(CF_TOD9MayEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
								}
								break;
							case 5:
								cf.at(CF_TODJunEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
								switch ((int)hourly_dispatch[i])
								{
									case 0:
										cf.at(CF_TOD1JunEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 1:
										cf.at(CF_TOD2JunEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 2:
										cf.at(CF_TOD3JunEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 3:
										cf.at(CF_TOD4JunEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 4:
										cf.at(CF_TOD5JunEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 5:
										cf.at(CF_TOD6JunEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 6:
										cf.at(CF_TOD7JunEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 7:
										cf.at(CF_TOD8JunEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 8:
										cf.at(CF_TOD9JunEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
								}
								break;
							case 6:
								cf.at(CF_TODJulEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
								switch ((int)hourly_dispatch[i])
								{
									case 0:
										cf.at(CF_TOD1JulEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 1:
										cf.at(CF_TOD2JulEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 2:
										cf.at(CF_TOD3JulEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 3:
										cf.at(CF_TOD4JulEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 4:
										cf.at(CF_TOD5JulEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 5:
										cf.at(CF_TOD6JulEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 6:
										cf.at(CF_TOD7JulEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 7:
										cf.at(CF_TOD8JulEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 8:
										cf.at(CF_TOD9JulEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
								}
								break;
							case 7:
								cf.at(CF_TODAugEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
								switch ((int)hourly_dispatch[i])
								{
									case 0:
										cf.at(CF_TOD1AugEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 1:
										cf.at(CF_TOD2AugEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 2:
										cf.at(CF_TOD3AugEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 3:
										cf.at(CF_TOD4AugEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 4:
										cf.at(CF_TOD5AugEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 5:
										cf.at(CF_TOD6AugEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 6:
										cf.at(CF_TOD7AugEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 7:
										cf.at(CF_TOD8AugEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 8:
										cf.at(CF_TOD9AugEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
								}
								break;
							case 8:
								cf.at(CF_TODSepEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
								switch ((int)hourly_dispatch[i])
								{
									case 0:
										cf.at(CF_TOD1SepEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 1:
										cf.at(CF_TOD2SepEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 2:
										cf.at(CF_TOD3SepEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 3:
										cf.at(CF_TOD4SepEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 4:
										cf.at(CF_TOD5SepEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 5:
										cf.at(CF_TOD6SepEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 6:
										cf.at(CF_TOD7SepEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 7:
										cf.at(CF_TOD8SepEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 8:
										cf.at(CF_TOD9SepEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
								}
								break;
							case 9:
								cf.at(CF_TODOctEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
								switch ((int)hourly_dispatch[i])
								{
									case 0:
										cf.at(CF_TOD1OctEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 1:
										cf.at(CF_TOD2OctEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 2:
										cf.at(CF_TOD3OctEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 3:
										cf.at(CF_TOD4OctEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 4:
										cf.at(CF_TOD5OctEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 5:
										cf.at(CF_TOD6OctEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 6:
										cf.at(CF_TOD7OctEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 7:
										cf.at(CF_TOD8OctEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 8:
										cf.at(CF_TOD9OctEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
								}
								break;
							case 10:
								cf.at(CF_TODNovEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
								switch ((int)hourly_dispatch[i])
								{
									case 0:
										cf.at(CF_TOD1NovEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 1:
										cf.at(CF_TOD2NovEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 2:
										cf.at(CF_TOD3NovEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 3:
										cf.at(CF_TOD4NovEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 4:
										cf.at(CF_TOD5NovEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 5:
										cf.at(CF_TOD6NovEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 6:
										cf.at(CF_TOD7NovEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 7:
										cf.at(CF_TOD8NovEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 8:
										cf.at(CF_TOD9NovEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
								}
								break;
							case 11:
								cf.at(CF_TODDecEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
								switch ((int)hourly_dispatch[i])
								{
									case 0:
										cf.at(CF_TOD1DecEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 1:
										cf.at(CF_TOD2DecEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 2:
										cf.at(CF_TOD3DecEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 3:
										cf.at(CF_TOD4DecEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 4:
										cf.at(CF_TOD5DecEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 5:
										cf.at(CF_TOD6DecEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 6:
										cf.at(CF_TOD7DecEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 7:
										cf.at(CF_TOD8DecEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
									case 8:
										cf.at(CF_TOD9DecEnergy,y) += hourly_enet[(y-1)*8760+i] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
										break;
								}
								break;
						}
						i++;					
					}
				}
			}

		}

		return true;
	}


};




DEFINE_MODULE_ENTRY( singleowner, "DHF Single Owner Financial Model_", 1 );


