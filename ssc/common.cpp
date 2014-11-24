#include "common.h"

var_info vtab_standard_financial[] = {

/*   VARTYPE           DATATYPE         NAME                                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "analysis_period",                           "Analyis period",                                  "years",  "",                      "Financials",      "?=30",                   "INTEGER,MIN=0,MAX=50",          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "federal_tax_rate",                         "Federal tax rate",                                "%",      "",                      "Financials",      "*",                      "MIN=0,MAX=100",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "state_tax_rate",                           "State tax rate",                                  "%",      "",                      "Financials",      "*",                      "MIN=0,MAX=100",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "property_tax_rate",                        "Property tax rate",                               "%",      "",                      "Financials",      "?=0.0",                  "MIN=0,MAX=100",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,     "prop_tax_cost_assessed_percent",            "Percent of pre-financing costs assessed","%","",			  "Financials",			 "?=95",                     "MIN=0,MAX=100",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "prop_tax_assessed_decline",                 "Assessed value annual decline",	"%",	 "",					  "Financials",             "?=5",                     "MIN=0,MAX=100",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,      "sales_tax_rate",                           "Sales tax rate",                                  "%",      "",                      "Financials",      "?=0.0",                  "MIN=0,MAX=100",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "real_discount_rate",                       "Real discount rate",                              "%",      "",                      "Financials",      "*",                      "MIN=0,MAX=100",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inflation_rate",                           "Inflation rate",                                  "%",      "",                      "Financials",      "*",                      "MIN=0,MAX=100",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "insurance_rate",                           "Insurance rate",                                  "%",      "",                      "Financials",      "?=0.0",                  "MIN=0,MAX=100",                 "" },

	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",                          "System nameplate capacity",                       "kW",     "",                      "System",          "*",                      "POSITIVE",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_heat_rate",                         "System heat rate",                                "MMBTus/MWh", "",                  "System",          "?=0.0",                  "MIN=0",                                         "" },

var_info_invalid };

var_info vtab_standard_loan[] = {

/*   VARTYPE           DATATYPE         NAME                            LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "loan_term",					"Loan term",					  "years",  "",                      "Loan",            "?=0",                    "INTEGER,MIN=0,MAX=50",          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "loan_rate",					"Loan rate",					  "%",      "",                      "Loan",            "?=0",                    "MIN=0,MAX=100",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "debt_fraction",                   "Debt percentage",                "%",      "",                      "Loan",			"?=0",                    "MIN=0,MAX=100",                 "" },

var_info_invalid };

var_info vtab_oandm[] = {
/*   VARTYPE           DATATYPE         NAME                             LABEL                                UNITS      META                 GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	
	{ SSC_INPUT,        SSC_ARRAY,       "om_fixed",                     "Fixed O&M annual amount",           "$/year",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_fixed_escal",               "Fixed O&M escalation",              "%/year",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "om_production",                "Production-based O&M amount",       "$/MWh",   "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_production_escal",          "Production-based O&M escalation",   "%/year",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "om_capacity",                  "Capacity-based O&M amount",         "$/kWcap", "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_capacity_escal",            "Capacity-based O&M escalation",     "%/year",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,		 "om_fuel_cost",                 "Fuel cost",                         "$/MMBtu", "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_fuel_cost_escal",           "Fuel cost escalation",              "%/year",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "annual_fuel_usage",        "Fuel usage",                         "kWht",         "",                      "O&M",      "?=0",                     "MIN=0",                                         "" },

	// optional fuel o and m for Biopower - usage can be in any unit and cost is in $ per usage unit
	{ SSC_INPUT,        SSC_NUMBER,      "om_opt_fuel_1_usage",           "Biomass feedstock usage",              "unit",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,		 "om_opt_fuel_1_cost",                 "Biomass feedstock cost",          "$/unit", "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_opt_fuel_1_cost_escal",           "Biomass feedstock cost escalation","%/year",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_opt_fuel_2_usage",           "Coal feedstock usage",              "unit",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,		 "om_opt_fuel_2_cost",                 "Coal feedstock cost",          "$/unit", "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_opt_fuel_2_cost_escal",           "Coal feedstock cost escalation","%/year",  "",                  "O&M",            "?=0.0",                 "",                                         "" },


var_info_invalid };

var_info vtab_depreciation[] = {
/*   VARTYPE           DATATYPE         NAME                              LABEL                                 UNITS     META                                      GROUP             REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,        SSC_NUMBER,      "depr_fed_type",                "Federal depreciation type",           "",       "0=none,1=macrs_half_year,2=sl,3=custom",  "Depreciation",      "?=0",                     "INTEGER,MIN=0,MAX=3",        "" },
	{ SSC_INPUT,        SSC_NUMBER,      "depr_fed_sl_years",            "Federal depreciation straight-line Years",       "years",  "",                                        "Depreciation",      "depr_fed_type=2",                     "INTEGER,POSITIVE",           "" },
	{ SSC_INPUT,        SSC_ARRAY,       "depr_fed_custom",              "Federal custom depreciation",         "%/year", "",                                        "Depreciation",      "depr_fed_type=3",         "",                           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "depr_sta_type",                "State depreciation type",             "",       "0=none,1=macrs_half_year,2=sl,3=custom",  "Depreciation",      "?=0",                     "INTEGER,MIN=0,MAX=3",        "" },
	{ SSC_INPUT,        SSC_NUMBER,      "depr_sta_sl_years",            "State depreciation straight-line years",         "years",  "",                                        "Depreciation",      "depr_sta_type=2",                     "INTEGER,POSITIVE",           "" },
	{ SSC_INPUT,        SSC_ARRAY,       "depr_sta_custom",              "State custom depreciation",           "%/year", "",                                        "Depreciation",      "depr_sta_type=3",         "",                           "" },

var_info_invalid };
	
var_info vtab_tax_credits[] = {
/*   VARTYPE           DATATYPE         NAME                               LABEL                                                UNITS     META                      GROUP                 REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,        SSC_NUMBER,       "itc_fed_amount",                 "Federal amount-based ITC amount",                         "$",      "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_fed_amount_deprbas_fed",     "Federal amount-based ITC reduces federal depreciation basis",       "0/1",    "",          "Tax Credit Incentives",      "?=1",                       "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_fed_amount_deprbas_sta",     "Federal amount-based ITC reduces state depreciation basis",       "0/1",    "",          "Tax Credit Incentives",      "?=1",                       "BOOLEAN",                     "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "itc_sta_amount",                 "State amount-based ITC amount",                           "$",      "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_sta_amount_deprbas_fed",     "State amount-based ITC reduces federal depreciation basis",         "0/1",    "",          "Tax Credit Incentives",      "?=0",                       "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_sta_amount_deprbas_sta",     "State amount-based ITC reduces state depreciation basis",         "0/1",    "",          "Tax Credit Incentives",      "?=0",                       "BOOLEAN",                     "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "itc_fed_percent",                "Federal percentage-based ITC percent",                    "%",      "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_fed_percent_maxvalue",       "Federal percentage-based ITC maximum value",                 "$",      "",          "Tax Credit Incentives",      "?=1e99",                    "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_fed_percent_deprbas_fed",    "Federal percentage-based ITC reduces federal depreciation basis",   "0/1",    "",          "Tax Credit Incentives",      "?=1",                       "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_fed_percent_deprbas_sta",    "Federal percentage-based ITC reduces state depreciation basis",   "0/1",    "",          "Tax Credit Incentives",      "?=1",                       "BOOLEAN",                     "" },

	{ SSC_INPUT,        SSC_NUMBER,       "itc_sta_percent",               "State percentage-based ITC percent",                      "%",      "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_sta_percent_maxvalue",       "State percentage-based ITC maximum Value",                   "$",      "",          "Tax Credit Incentives",      "?=1e99",                    "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_sta_percent_deprbas_fed",    "State percentage-based ITC reduces federal depreciation basis",     "0/1",    "",          "Tax Credit Incentives",      "?=0",                       "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_sta_percent_deprbas_sta",    "State percentage-based ITC reduces state depreciation basis",     "0/1",    "",          "Tax Credit Incentives",      "?=0",                       "BOOLEAN",                     "" },

	{ SSC_INPUT,        SSC_ARRAY,       "ptc_fed_amount",                 "Federal PTC amount",                                      "$/kWh",  "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ptc_fed_term",                   "Federal PTC term",                                        "years",  "",          "Tax Credit Incentives",      "?=10",                      "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ptc_fed_escal",                  "Federal PTC escalation",                                  "%/year", "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "ptc_sta_amount",                 "State PTC amount",                                        "$/kWh",  "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ptc_sta_term",                   "State PTC term",                                          "years",  "",          "Tax Credit Incentives",      "?=10",                      "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ptc_sta_escal",                  "State PTC escalation",                                    "%/year", "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	
var_info_invalid };


var_info vtab_payment_incentives[] = {
	/*   VARTYPE           DATATYPE         NAME                          LABEL                                                  UNITS     META                      GROUP                   REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,        SSC_NUMBER,       "ibi_fed_amount",                "Federal amount-based IBI amount",                    "$",      "",                      "Payment Incentives",      "?=0",                 "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_amount_tax_fed",        "Federal amount-based IBI federal taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_amount_tax_sta",        "Federal amount-based IBI state taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_amount_deprbas_fed",    "Federal amount-based IBI reduces federal depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_amount_deprbas_sta",    "Federal amount-based IBI reduces state depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                 "BOOLEAN",                       "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "ibi_sta_amount",                "State amount-based IBI amount",                    "$",      "",                      "Payment Incentives",      "?=0",                   "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_amount_tax_fed",        "State amount-based IBI federal taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                   "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_amount_tax_sta",        "State amount-based IBI state taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                   "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_amount_deprbas_fed",    "State amount-based IBI reduces federal depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                   "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_amount_deprbas_sta",    "State amount-based IBI reduces state depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                   "BOOLEAN",                       "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "ibi_uti_amount",                "Utility amount-based IBI amount",                    "$",      "",                      "Payment Incentives",      "?=0",                 "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_amount_tax_fed",        "Utility amount-based IBI federal taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_amount_tax_sta",        "Utility amount-based IBI state taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_amount_deprbas_fed",    "Utility amount-based IBI reduces federal depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_amount_deprbas_sta",    "Utility amount-based IBI reduces state depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                 "BOOLEAN",                       "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "ibi_oth_amount",                "Other amount-based IBI amount",                    "$",      "",                      "Payment Incentives",      "?=0",                   "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_amount_tax_fed",        "Other amount-based IBI federal taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                   "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_amount_tax_sta",        "Other amount-based IBI state taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                   "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_amount_deprbas_fed",    "Other amount-based IBI reduces federal depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                   "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_amount_deprbas_sta",    "Other amount-based IBI reduces state depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                   "BOOLEAN",                       "" },

	{ SSC_INPUT,        SSC_NUMBER,       "ibi_fed_percent",               "Federal percentage-based IBI percent",                   "%",      "",                      "Payment Incentives",      "?=0.0",           "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_percent_maxvalue",      "Federal percentage-based IBI maximum value",             "$",      "",                      "Payment Incentives",      "?=1e99",          "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_percent_tax_fed",       "Federal percentage-based IBI federal taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_percent_tax_sta",       "Federal percentage-based IBI state taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_percent_deprbas_fed",   "Federal percentage-based IBI reduces federal depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_percent_deprbas_sta",   "Federal percentage-based IBI reduces state depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },

	{ SSC_INPUT,        SSC_NUMBER,       "ibi_sta_percent",               "State percentage-based IBI percent",                   "%",      "",                      "Payment Incentives",      "?=0.0",           "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_percent_maxvalue",      "State percentage-based IBI maximum value",             "$",      "",                      "Payment Incentives",      "?=1e99",          "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_percent_tax_fed",       "State percentage-based IBI federal taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_percent_tax_sta",       "State percentage-based IBI state taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_percent_deprbas_fed",   "State percentage-based IBI reduces federal depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_percent_deprbas_sta",   "State percentage-based IBI reduces state depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },

	{ SSC_INPUT,        SSC_NUMBER,       "ibi_uti_percent",               "Utility percentage-based IBI percent",                   "%",      "",                      "Payment Incentives",      "?=0.0",           "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_percent_maxvalue",      "Utility percentage-based IBI maximum value",             "$",      "",                      "Payment Incentives",      "?=1e99",          "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_percent_tax_fed",       "Utility percentage-based IBI federal taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_percent_tax_sta",       "Utility percentage-based IBI state taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_percent_deprbas_fed",   "Utility percentage-based IBI reduces federal depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_percent_deprbas_sta",   "Utility percentage-based IBI reduces state depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },

	{ SSC_INPUT,        SSC_NUMBER,       "ibi_oth_percent",               "Other percentage-based IBI percent",                   "%",      "",                      "Payment Incentives",      "?=0.0",           "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_percent_maxvalue",      "Other percentage-based IBI maximum value",             "$",      "",                      "Payment Incentives",      "?=1e99",          "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_percent_tax_fed",       "Other percentage-based IBI federal taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_percent_tax_sta",       "Other percentage-based IBI state taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_percent_deprbas_fed",   "Other percentage-based IBI reduces federal depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_percent_deprbas_sta",   "Other percentage-based IBI reduces state depreciation basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },


	{ SSC_INPUT,        SSC_NUMBER,       "cbi_fed_amount",         "Federal CBI amount",                   "$/Watt", "",                      "Payment Incentives",      "?=0.0",                     "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_fed_maxvalue",       "Federal CBI maximum",                  "$",      "",                      "Payment Incentives",      "?=1e99",                    "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_fed_tax_fed",        "Federal CBI federal taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_fed_tax_sta",        "Federal CBI state taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_fed_deprbas_fed",    "Federal CBI reduces federal depreciation basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_fed_deprbas_sta",    "Federal CBI reduces state depreciation basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "cbi_sta_amount",         "State CBI amount",                   "$/Watt", "",                      "Payment Incentives",      "?=0.0",                     "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_sta_maxvalue",       "State CBI maximum",                  "$",      "",                      "Payment Incentives",      "?=1e99",                    "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_sta_tax_fed",        "State CBI federal taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_sta_tax_sta",        "State CBI state taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_sta_deprbas_fed",    "State CBI reduces federal depreciation basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_sta_deprbas_sta",    "State CBI reduces state depreciation basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "cbi_uti_amount",         "Utility CBI amount",                   "$/Watt", "",                      "Payment Incentives",      "?=0.0",                     "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_uti_maxvalue",       "Utility CBI maximum",                  "$",      "",                      "Payment Incentives",      "?=1e99",                    "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_uti_tax_fed",        "Utility CBI federal taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_uti_tax_sta",        "Utility CBI state taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_uti_deprbas_fed",    "Utility CBI reduces federal depreciation basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_uti_deprbas_sta",    "Utility CBI reduces state depreciation basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "cbi_oth_amount",         "Other CBI amount",                   "$/Watt", "",                      "Payment Incentives",      "?=0.0",                     "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_oth_maxvalue",       "Other CBI maximum",                  "$",      "",                      "Payment Incentives",      "?=1e99",                    "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_oth_tax_fed",        "Other CBI federal taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_oth_tax_sta",        "Other CBI state taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_oth_deprbas_fed",    "Other CBI reduces federal depreciation basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_oth_deprbas_sta",    "Other CBI reduces state depreciation basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },


	{ SSC_INPUT,        SSC_ARRAY,       "pbi_fed_amount",                "Federal PBI amount",           "$/kWh",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_fed_term",                  "Federal PBI term",             "years",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_fed_escal",                 "Federal PBI escalation",       "%",        "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_fed_tax_fed",               "Federal PBI federal taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_fed_tax_sta",               "Federal PBI state taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "pbi_sta_amount",                "State PBI amount",           "$/kWh",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_sta_term",                  "State PBI term",             "years",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_sta_escal",                 "State PBI escalation",       "%",        "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_sta_tax_fed",               "State PBI federal taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_sta_tax_sta",               "State PBI state taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "pbi_uti_amount",                "Utility PBI amount",           "$/kWh",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_uti_term",                  "Utility PBI term",             "years",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_uti_escal",                 "Utility PBI escalation",       "%",        "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_uti_tax_fed",               "Utility PBI federal taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_uti_tax_sta",               "Utility PBI state taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "pbi_oth_amount",                "Other PBI amount",           "$/kWh",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_oth_term",                  "Other PBI term",             "years",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_oth_escal",                 "Other PBI escalation",       "%",        "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_oth_tax_fed",               "Other PBI federal taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_oth_tax_sta",               "Other PBI state taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },

var_info_invalid };


var_info vtab_adjustment_factors[] = {
/*   VARTYPE           DATATYPE         NAME                               LABEL                                       UNITS     META                                     GROUP                 REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,        SSC_NUMBER,      "adjust:factor",                 "Constant adjustment factor",                "0..1",    "",                                     "Adjustment factors",      "*",                     "POSITIVE",                     "" },
	{ SSC_INPUT,        SSC_ARRAY,       "adjust:hourly",                 "Hourly adjustment factors",                 "0..1",    "",                                     "Adjustment factors",      "?",                     "LENGTH=8760",                "" },
	{ SSC_INPUT,        SSC_MATRIX,      "adjust:periods",                "Period-based adjustment factors",           "0..1",    "n x 3 matrix [ start, end, factor ]",  "Adjustment factors",      "?",                     "COLS=3",                     "" },
	
var_info_invalid };


adjustment_factors::adjustment_factors( compute_module *cm )
	: m_cm(cm)
{	
}

bool adjustment_factors::setup()
{
	float f = (float)m_cm->as_number( "adjust:factor" );
	m_factors.resize( 8760, f );

	if ( m_cm->is_assigned("adjust:hourly") )
	{
		size_t n;
		ssc_number_t *p = m_cm->as_array( "adjust:hourly", &n );
		if ( p != 0 && n == 8760 )
		{
			for( size_t i=0;i<8760;i++ )
				m_factors[i] *= p[i];
		}
	}

	if ( m_cm->is_assigned("adjust:periods") )
	{
		size_t nr, nc;
		ssc_number_t *mat = m_cm->as_matrix( "adjust:periods", &nr, &nc );
		if ( mat != 0 && nc == 3 )
		{
			for( size_t r=0;r<nr;r++ )
			{
				int start = (int) mat[ nc*r ];
				int end = (int) mat[ nc*r + 1 ];
				float factor = (float) mat[ nc*r + 2 ];
				
				if ( start < 0 || start >= 8760 || end < start )
				{
					m_error = util::format( "period %d is invalid ( start: %d, end %d )", (int)r, start, end );
					continue;
				}

				if ( end >= 8760 ) end = 8759;

				for( int i=start;i<=end;i++ )
					m_factors[i] *= factor;
			}
		}
	}

	return m_error.length() == 0;
}

float adjustment_factors::operator()( size_t time )
{
	if ( time < m_factors.size() ) return m_factors[time];
	else return 0.0;
}


shading_factor_calculator::shading_factor_calculator()
{
	m_enAzAlt = false;
	m_diffFactor = 1.0;
}

bool shading_factor_calculator::setup( compute_module *cm, const std::string &prefix )
{
	bool ok = true;
	m_diffFactor = 1.0;
	m_beamFactors.resize( 8760, 1.0 );

	
	for ( size_t j=0;j<8760;j++)
		m_beamFactors[j] = 1.0;

	if ( cm->is_assigned( prefix+"shading:hourly" ) )
	{
		size_t len = 0;
		ssc_number_t *vals = cm->as_array( prefix+"shading:hourly", &len );
		if ( len == 8760 )
		{
			for ( size_t j=0;j<8760;j++)
				m_beamFactors[j] = 1-vals[j]/100;
		}
		else
		{
			ok = false;
			m_errors.push_back("hourly shading beam losses must have 8760 values");
		}
	}


	if ( cm->is_assigned( prefix+"shading:mxh" ) )
	{
		size_t nrows, ncols;
		ssc_number_t *mat = cm->as_matrix( prefix+"shading:mxh", &nrows, &ncols );
		if ( nrows != 12 || ncols != 24 )
		{
			ok = false;
			m_errors.push_back("month x hour shading losses must have 12 rows and 24 columns");
		}
		else
		{
			int c=0;
			for (int m=0;m<12;m++)
				for (int d=0;d<util::nday[m];d++)
					for (int h=0;h<24;h++)
						m_beamFactors[c++] *= 1-mat[ m*ncols + h ]/100;
		}
	}

	m_enAzAlt = false;
	if ( cm->is_assigned( prefix+"shading:azal" ) )
	{
		size_t nrows, ncols;
		ssc_number_t *mat = cm->as_matrix( prefix+"shading:azal", &nrows, &ncols );
		if ( nrows < 3 || ncols < 3 )
		{
			ok = false;
			m_errors.push_back("azimuth x altitude shading losses must have at least 3 rows and 3 columns");
		}

		m_azaltvals.resize_fill( nrows, ncols, 1.0 );
		for ( size_t r=0;r<nrows;r++ )
			for ( size_t c=0;c<ncols;c++ )
				m_azaltvals.at(r,c) = 1-mat[r*ncols+c]/100;

		m_enAzAlt = true;
	}

	if ( cm->is_assigned( prefix+"shading:diff" ) )
		m_diffFactor = 1-cm->as_double( prefix+"shading:diff" )/100;


	return ok;
}

std::string shading_factor_calculator::get_error(size_t i)
{
	if( i < m_errors.size() ) return m_errors[i];
	else return std::string("");
}

double shading_factor_calculator::fbeam( size_t hour, double solalt, double solazi )
{
	if ( hour >= 0 && hour < m_beamFactors.size() )
	{
		double factor = m_beamFactors[hour];
		if ( m_enAzAlt )
			factor *= util::bilinear( solalt, solazi, m_azaltvals );
		return factor;
	}
	else
		return 1.0;
}

double shading_factor_calculator::fdiff()
{
	return m_diffFactor;
}