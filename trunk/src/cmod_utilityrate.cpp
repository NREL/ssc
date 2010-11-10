#include "core.h"

static var_info _cm_vtab_utilityrate[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_ARRAY,      "p_system",                 "Power produced by system",        "kW",     "",                      "",             "*",						   "LENGTH_MULTIPLE_OF=8760",       "" },
	{ SSC_INPUT,        SSC_ARRAY,      "p_load",                   "Power consumed by loads",         "kW",     "",                      "",             "?",                         "LENGTH_MULTIPLE_OF_8760",       "" },
	
	{ SSC_INPUT,        SSC_ARRAY,      "system_degradation",       "Annual system degradation",       "%",      "%/year or custom sched","",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,      "load_escalation",          "Annual load escalation",          "%",      "%/year or custom sched","",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,      "rate_escalation",          "Annual aggregate rate escalation_","%",     "%/year or custom sched","",             "?=0.0",                     "",                              "" },
	
	{ SSC_INPUT,        SSC_NUMBER,     "analysis_years",           "Number of years in analysis",     "years",  "",                      "",             "?=30",                      "INTEGER,MIN=1,MAX=50",          "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_buy_eq_sell",           "Force sell rate equal to buy",    "0/1",    "Enforce net metering",  "",             "?=1",                       "BOOLEAN",                       "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_monthly_fixed_charge",  "Monthly fixed charge",            "$",      "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_flat_buy_rate",         "Flat rate (buy)",                 "$/kWh",  "",                      "",             "*",                         "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_flat_sell_rate",        "Flat rate (sell)",                "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_enable",            "Enable time-of-use rates",        "0/1",    "",                      "",             "?=0",                       "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p1_buy_rate",       "TOU Period 1 Rate (buy)",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p1_sell_rate",      "TOU Period 1 Rate (sell)",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p2_buy_rate",       "TOU Period 2 Rate (buy)",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p2_sell_rate",      "TOU Period 2 Rate (sell)",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p3_buy_rate",       "TOU Period 3 Rate (buy)",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p3_sell_rate",      "TOU Period 3 Rate (sell)",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p4_buy_rate",       "TOU Period 4 Rate (buy)",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p4_sell_rate",      "TOU Period 4 Rate (sell)",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p5_buy_rate",       "TOU Period 5 Rate (buy)",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p5_sell_rate",      "TOU Period 5 Rate (sell)",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p6_buy_rate",       "TOU Period 6 Rate (buy)",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p6_sell_rate",      "TOU Period 6 Rate (sell)",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p7_buy_rate",       "TOU Period 7 Rate (buy)",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p7_sell_rate",      "TOU Period 7 Rate (sell)",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p8_buy_rate",       "TOU Period 8 Rate (buy)",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p8_sell_rate",      "TOU Period 8 Rate (sell)",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p9_buy_rate",       "TOU Period 9 Rate (buy)",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tou_p9_sell_rate",      "TOU Period 9 Rate (sell)",        "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_STRING,     "ur_tou_sched_weekday",     "TOU Weekday Schedule",            "",       "288 digits 0-9, 24x12", "",             "ur_tou_enable=1",           "TOUSCHED",                      "" },
	{ SSC_INPUT,        SSC_STRING,     "ur_tou_sched_weekend",     "TOU Weekend Schedule",            "",       "288 digits 0-9, 24x12", "",             "ur_tou_enable=1",           "TOUSCHED",                      "" },


	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_enable",             "Enable demand charges",           "0/1",    "",                      "",             "?=0",                       "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_fixed1",             "DC Fixed Rate January",           "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_fixed2",             "DC Fixed Rate February",          "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_fixed3",             "DC Fixed Rate March",             "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_fixed4",             "DC Fixed Rate April",             "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_fixed5",             "DC Fixed Rate May",               "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_fixed6",             "DC Fixed Rate June",              "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_fixed7",             "DC Fixed Rate July",              "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_fixed8",             "DC Fixed Rate August",            "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_fixed9",             "DC Fixed Rate September",         "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_fixed10",            "DC Fixed Rate October",           "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_fixed11",            "DC Fixed Rate November",          "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_fixed12",            "DC Fixed Rate December",          "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p1",                 "DC TOU Rate Period 1",            "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p2",                 "DC TOU Rate Period 2",            "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p3",                 "DC TOU Rate Period 3",            "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p4",                 "DC TOU Rate Period 4",            "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p5",                 "DC TOU Rate Period 5",            "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p6",                 "DC TOU Rate Period 6",            "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p7",                 "DC TOU Rate Period 7",            "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p8",                 "DC TOU Rate Period 8",            "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_dc_p9",                 "DC TOU Rate Period 9",            "$/kW,pk","",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_STRING,     "ur_dc_sched_weekday",      "DC TOU Weekday Schedule",         "",       "288 digits 0-9, 24x12", "",             "ur_dc_enable=1",            "TOUSCHED",                      "" },
	{ SSC_INPUT,        SSC_STRING,     "ur_dc_sched_weekend",      "DC TOU Weekend Schedule",         "",       "288 digits 0-9, 24x12", "",             "ur_dc_enable=1",            "TOUSCHED",                      "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_enable",             "Enable tiered rates",             "0/1",    "",                      "",             "?=0",                       "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_sell_type",          "Tiered rate sell mode",           "0,1,2",  "0=specified,1=tier1,2=lowest", "",      "?=0",                       "INTEGER,MIN=0,MAX=2",           "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_sell_rate",          "Specified tiered sell rate",      "$/kW",   "",                      "",             "ur_tr_sell_type=0",         "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p1_energy_ub1",      "Tiered Struct. 1 Energy UB 1",    "kWh",    "",                      "",             "?=1e99",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p1_energy_ub2",      "Tiered Struct. 1 Energy UB 2",    "kWh",    "",                      "",             "?=1e99",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p1_energy_ub3",      "Tiered Struct. 1 Energy UB 3",    "kWh",    "",                      "",             "?=1e99",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p1_energy_ub4",      "Tiered Struct. 1 Energy UB 4",    "kWh",    "",                      "",             "?=1e99",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p1_energy_ub5",      "Tiered Struct. 1 Energy UB 5",    "kWh",    "",                      "",             "?=1e99",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p1_energy_ub6",      "Tiered Struct. 1 Energy UB 6",    "kWh",    "",                      "",             "?=1e99",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p1_rate1",           "Tiered Struct. 1 Rate 1",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p1_rate2",           "Tiered Struct. 1 Rate 2",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p1_rate3",           "Tiered Struct. 1 Rate 3",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p1_rate4",           "Tiered Struct. 1 Rate 4",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p1_rate5",           "Tiered Struct. 1 Rate 5",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p1_rate6",           "Tiered Struct. 1 Rate 6",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p2_energy_ub1",      "Tiered Struct. 2 Energy UB 1",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p2_energy_ub2",      "Tiered Struct. 2 Energy UB 2",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p2_energy_ub3",      "Tiered Struct. 2 Energy UB 3",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p2_energy_ub4",      "Tiered Struct. 2 Energy UB 4",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p2_energy_ub5",      "Tiered Struct. 2 Energy UB 5",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p2_energy_ub6",      "Tiered Struct. 2 Energy UB 6",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p2_rate1",           "Tiered Struct. 2 Rate 1",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p2_rate2",           "Tiered Struct. 2 Rate 2",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p2_rate3",           "Tiered Struct. 2 Rate 3",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p2_rate4",           "Tiered Struct. 2 Rate 4",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p2_rate5",           "Tiered Struct. 2 Rate 5",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p2_rate6",           "Tiered Struct. 2 Rate 6",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p3_energy_ub1",      "Tiered Struct. 3 Energy UB 1",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p3_energy_ub2",      "Tiered Struct. 3 Energy UB 2",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p3_energy_ub3",      "Tiered Struct. 3 Energy UB 3",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p3_energy_ub4",      "Tiered Struct. 3 Energy UB 4",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p3_energy_ub5",      "Tiered Struct. 3 Energy UB 5",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p3_energy_ub6",      "Tiered Struct. 3 Energy UB 6",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p3_rate1",           "Tiered Struct. 3 Rate 1",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p3_rate2",           "Tiered Struct. 3 Rate 2",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p3_rate3",           "Tiered Struct. 3 Rate 3",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p3_rate4",           "Tiered Struct. 3 Rate 4",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p3_rate5",           "Tiered Struct. 3 Rate 5",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p3_rate6",           "Tiered Struct. 3 Rate 6",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p4_energy_ub1",      "Tiered Struct. 4 Energy UB 1",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p4_energy_ub2",      "Tiered Struct. 4 Energy UB 2",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p4_energy_ub3",      "Tiered Struct. 4 Energy UB 3",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p4_energy_ub4",      "Tiered Struct. 4 Energy UB 4",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p4_energy_ub5",      "Tiered Struct. 4 Energy UB 5",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p4_energy_ub6",      "Tiered Struct. 4 Energy UB 6",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p4_rate1",           "Tiered Struct. 4 Rate 1",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p4_rate2",           "Tiered Struct. 4 Rate 2",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p4_rate3",           "Tiered Struct. 4 Rate 3",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p4_rate4",           "Tiered Struct. 4 Rate 4",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p4_rate5",           "Tiered Struct. 4 Rate 5",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p4_rate6",           "Tiered Struct. 4 Rate 6",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p5_energy_ub1",      "Tiered Struct. 5 Energy UB 1",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p5_energy_ub2",      "Tiered Struct. 5 Energy UB 2",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p5_energy_ub3",      "Tiered Struct. 5 Energy UB 3",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p5_energy_ub4",      "Tiered Struct. 5 Energy UB 4",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p5_energy_ub5",      "Tiered Struct. 5 Energy UB 5",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p5_energy_ub6",      "Tiered Struct. 5 Energy UB 6",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p5_rate1",           "Tiered Struct. 5 Rate 1",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p5_rate2",           "Tiered Struct. 5 Rate 2",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p5_rate3",           "Tiered Struct. 5 Rate 3",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p5_rate4",           "Tiered Struct. 5 Rate 4",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p5_rate5",           "Tiered Struct. 5 Rate 5",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p5_rate6",           "Tiered Struct. 5 Rate 6",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p6_energy_ub1",      "Tiered Struct. 6 Energy UB 1",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p6_energy_ub2",      "Tiered Struct. 6 Energy UB 2",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p6_energy_ub3",      "Tiered Struct. 6 Energy UB 3",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p6_energy_ub4",      "Tiered Struct. 6 Energy UB 4",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p6_energy_ub5",      "Tiered Struct. 6 Energy UB 5",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p6_energy_ub6",      "Tiered Struct. 6 Energy UB 6",    "kWh",    "",                      "",             "?=1e99",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p6_rate1",           "Tiered Struct. 6 Rate 1",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p6_rate2",           "Tiered Struct. 6 Rate 2",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p6_rate3",           "Tiered Struct. 6 Rate 3",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p6_rate4",           "Tiered Struct. 6 Rate 4",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p6_rate5",           "Tiered Struct. 6 Rate 5",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_p6_rate6",           "Tiered Struct. 6 Rate 6",         "$/kWh",  "",                      "",             "?=0.0",                     "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_sched_m1",           "Tiered Structure for January",    "0-5",    "tiered structure #",    "",             "?=0",                       "INTEGER,MIN=0,MAX=5",           "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_sched_m2",           "Tiered Structure for February"    "0-5",    "tiered structure #",    "",             "?=0",                       "INTEGER,MIN=0,MAX=5",           "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_sched_m3",           "Tiered Structure for March",      "0-5",    "tiered structure #",    "",             "?=0",                       "INTEGER,MIN=0,MAX=5",           "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_sched_m4",           "Tiered Structure for April",      "0-5",    "tiered structure #",    "",             "?=0",                       "INTEGER,MIN=0,MAX=5",           "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_sched_m5",           "Tiered Structure for May",        "0-5",    "tiered structure #",    "",             "?=0",                       "INTEGER,MIN=0,MAX=5",           "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_sched_m6",           "Tiered Structure for June",       "0-5",    "tiered structure #",    "",             "?=0",                       "INTEGER,MIN=0,MAX=5",           "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_sched_m7",           "Tiered Structure for July",       "0-5",    "tiered structure #",    "",             "?=0",                       "INTEGER,MIN=0,MAX=5",           "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_sched_m8",           "Tiered Structure for August",     "0-5",    "tiered structure #",    "",             "?=0",                       "INTEGER,MIN=0,MAX=5",           "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_sched_m9",           "Tiered Structure for September",  "0-5",    "tiered structure #",    "",             "?=0",                       "INTEGER,MIN=0,MAX=5",           "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_sched_m10",          "Tiered Structure for October",    "0-5",    "tiered structure #",    "",             "?=0",                       "INTEGER,MIN=0,MAX=5",           "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_sched_m11",          "Tiered Structure for November",   "0-5",    "tiered structure #",    "",             "?=0",                       "INTEGER,MIN=0,MAX=5",           "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ur_tr_sched_m12",          "Tiered Structure for December",   "0-5",    "tiered structure #",    "",             "?=0",                       "INTEGER,MIN=0,MAX=5",           "" },


var_info_invalid };



class cm_utilityrate : public compute_module
{
private:
public:
	cm_utilityrate()
	{
		add_var_info( _cm_vtab_utilityrate );
	}

	bool exec( ) throw( general_error )
	{
		return false;
	}
};

DEFINE_MODULE_ENTRY( utilityrate, "Complex utility rate structure calculator_", 1 );


