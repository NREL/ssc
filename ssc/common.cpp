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
	
	{ SSC_INPUT,        SSC_ARRAY,       "om_fixed",                     "Fixed O&M Annual Amount",           "$/year",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_fixed_escal",               "Fixed O&M Escalation",              "%/year",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "om_production",                "Production-based O&M Amount",       "$/MWh",   "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_production_escal",          "Production-based O&M Escalation",   "%/year",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "om_capacity",                  "Capacity-based O&M Amount",         "$/kWcap", "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_capacity_escal",            "Capacity-based O&M Escalation",     "%/year",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,		 "om_fuel_cost",                 "Fuel Cost",                         "$/MMBtu", "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_fuel_cost_escal",           "Fuel Cost Escalation",              "%/year",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "annual_fuel_usage",        "Fuel usage",                         "kWht",         "",                      "O&M",      "?=0",                     "MIN=0",                                         "" },

	// optional fuel o and m for Biopower - usage can be in any unit and cost is in $ per usage unit
	{ SSC_INPUT,        SSC_NUMBER,      "om_opt_fuel_1_usage",           "Optional Fuel 1 Usage",              "unit",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,		 "om_opt_fuel_1_cost",                 "Optional Fuel 1  Cost",          "$/unit", "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_opt_fuel_1_cost_escal",           "Optional Fuel 1  Cost Escalation","%/year",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_opt_fuel_2_usage",           "Optional Fuel 2 Usage",              "unit",  "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,		 "om_opt_fuel_2_cost",                 "Optional Fuel 2  Cost",          "$/unit", "",                  "O&M",            "?=0.0",                 "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_opt_fuel_2_cost_escal",           "Optional Fuel 2  Cost Escalation","%/year",  "",                  "O&M",            "?=0.0",                 "",                                         "" },


var_info_invalid };

var_info vtab_depreciation[] = {
/*   VARTYPE           DATATYPE         NAME                              LABEL                                 UNITS     META                                      GROUP             REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,        SSC_NUMBER,      "depr_fed_type",                "Federal Depreciation Type",           "",       "0=none,1=macrs_half_year,2=sl,3=custom",  "Depreciation",      "?=0",                     "INTEGER,MIN=0,MAX=3",        "" },
	{ SSC_INPUT,        SSC_NUMBER,      "depr_fed_sl_years",            "Federal Depreciation SL Years",       "years",  "",                                        "Depreciation",      "depr_fed_type=2",                     "INTEGER,POSITIVE",           "" },
	{ SSC_INPUT,        SSC_ARRAY,       "depr_fed_custom",              "Federal Custom Depreciation",         "%/year", "",                                        "Depreciation",      "depr_fed_type=3",         "",                           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "depr_sta_type",                "State Depreciation Type",             "",       "0=none,1=macrs_half_year,2=sl,3=custom",  "Depreciation",      "?=0",                     "INTEGER,MIN=0,MAX=3",        "" },
	{ SSC_INPUT,        SSC_NUMBER,      "depr_sta_sl_years",            "State Depreciation SL Years",         "years",  "",                                        "Depreciation",      "depr_sta_type=2",                     "INTEGER,POSITIVE",           "" },
	{ SSC_INPUT,        SSC_ARRAY,       "depr_sta_custom",              "State Custom Depreciation",           "%/year", "",                                        "Depreciation",      "depr_sta_type=3",         "",                           "" },

var_info_invalid };
	
var_info vtab_tax_credits[] = {
/*   VARTYPE           DATATYPE         NAME                               LABEL                                                UNITS     META                      GROUP                 REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,        SSC_NUMBER,       "itc_fed_amount",                 "Federal Amount-based ITC Amount",                         "$",      "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_fed_amount_deprbas_fed",     "Federal Amount-based ITC Reduces Fed. Depr. Basis",       "0/1",    "",          "Tax Credit Incentives",      "?=1",                       "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_fed_amount_deprbas_sta",     "Federal Amount-based ITC Reduces Sta. Depr. Basis",       "0/1",    "",          "Tax Credit Incentives",      "?=1",                       "BOOLEAN",                     "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "itc_sta_amount",                 "State Amount-based ITC Amount",                           "$",      "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_sta_amount_deprbas_fed",     "State Amount-based ITC Reduces Fed. Depr. Basis",         "0/1",    "",          "Tax Credit Incentives",      "?=0",                       "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_sta_amount_deprbas_sta",     "State Amount-based ITC Reduces Sta. Depr. Basis",         "0/1",    "",          "Tax Credit Incentives",      "?=0",                       "BOOLEAN",                     "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "itc_fed_percent",                "Federal Percentage-based ITC Percent",                    "%",      "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_fed_percent_maxvalue",       "Federal Percentage-based ITC Max. Value",                 "$",      "",          "Tax Credit Incentives",      "?=1e99",                    "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_fed_percent_deprbas_fed",    "Federal Percentage-based ITC Reduces Fed. Depr. Basis",   "0/1",    "",          "Tax Credit Incentives",      "?=1",                       "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_fed_percent_deprbas_sta",    "Federal Percentage-based ITC Reduces Sta. Depr. Basis",   "0/1",    "",          "Tax Credit Incentives",      "?=1",                       "BOOLEAN",                     "" },

	{ SSC_INPUT,        SSC_NUMBER,       "itc_sta_percent",                "State Percentage-based ITC Percent",                      "%",      "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_sta_percent_maxvalue",       "State Percentage-based ITC Max. Value",                   "$",      "",          "Tax Credit Incentives",      "?=1e99",                    "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_sta_percent_deprbas_fed",    "State Percentage-based ITC Reduces Fed. Depr. Basis",     "0/1",    "",          "Tax Credit Incentives",      "?=0",                       "BOOLEAN",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itc_sta_percent_deprbas_sta",    "State Percentage-based ITC Reduces Sta. Depr. Basis",     "0/1",    "",          "Tax Credit Incentives",      "?=0",                       "BOOLEAN",                     "" },

	{ SSC_INPUT,        SSC_ARRAY,       "ptc_fed_amount",                 "Federal PTC Amount",                                      "$/kWh",  "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ptc_fed_term",                   "Federal PTC Term",                                        "years",  "",          "Tax Credit Incentives",      "?=10",                      "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ptc_fed_escal",                  "Federal PTC Escalation",                                  "%/year", "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "ptc_sta_amount",                 "State PTC Amount",                                        "$/kWh",  "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ptc_sta_term",                   "State PTC Term",                                          "years",  "",          "Tax Credit Incentives",      "?=10",                      "",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ptc_sta_escal",                  "State PTC Escalation",                                    "%/year", "",          "Tax Credit Incentives",      "?=0",                       "",                            "" },
	
var_info_invalid };


var_info vtab_payment_incentives[] = {
	/*   VARTYPE           DATATYPE         NAME                          LABEL                                                  UNITS     META                      GROUP                   REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,        SSC_NUMBER,       "ibi_fed_amount",                "Federal Amount-based IBI Amount",                    "$",      "",                      "Payment Incentives",      "?=0",                 "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_amount_tax_fed",        "Federal Amount-based IBI Fed. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_amount_tax_sta",        "Federal Amount-based IBI Sta. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_amount_deprbas_fed",    "Federal Amount-based IBI Reduces Fed. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_amount_deprbas_sta",    "Federal Amount-based IBI Reduces Sta. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                 "BOOLEAN",                       "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "ibi_sta_amount",                "State Amount-based IBI Amount",                    "$",      "",                      "Payment Incentives",      "?=0",                   "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_amount_tax_fed",        "State Amount-based IBI Fed. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                   "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_amount_tax_sta",        "State Amount-based IBI Sta. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                   "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_amount_deprbas_fed",    "State Amount-based IBI Reduces Fed. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                   "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_amount_deprbas_sta",    "State Amount-based IBI Reduces Sta. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                   "BOOLEAN",                       "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "ibi_uti_amount",                "Utility Amount-based IBI Amount",                    "$",      "",                      "Payment Incentives",      "?=0",                 "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_amount_tax_fed",        "Utility Amount-based IBI Fed. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_amount_tax_sta",        "Utility Amount-based IBI Sta. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_amount_deprbas_fed",    "Utility Amount-based IBI Reduces Fed. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                 "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_amount_deprbas_sta",    "Utility Amount-based IBI Reduces Sta. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                 "BOOLEAN",                       "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "ibi_oth_amount",                "Other Amount-based IBI Amount",                    "$",      "",                      "Payment Incentives",      "?=0",                   "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_amount_tax_fed",        "Other Amount-based IBI Fed. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                   "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_amount_tax_sta",        "Other Amount-based IBI Sta. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",                   "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_amount_deprbas_fed",    "Other Amount-based IBI Reduces Fed. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                   "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_amount_deprbas_sta",    "Other Amount-based IBI Reduces Sta. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",                   "BOOLEAN",                       "" },

	{ SSC_INPUT,        SSC_NUMBER,       "ibi_fed_percent",               "Federal Percentage-based IBI Percent",                   "%",      "",                      "Payment Incentives",      "?=0.0",           "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_percent_maxvalue",      "Federal Percentage-based IBI Maximum Value",             "$",      "",                      "Payment Incentives",      "?=1e99",          "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_percent_tax_fed",       "Federal Percentage-based IBI Fed. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_percent_tax_sta",       "Federal Percentage-based IBI Sta. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_percent_deprbas_fed",   "Federal Percentage-based IBI Reduces Fed. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_fed_percent_deprbas_sta",   "Federal Percentage-based IBI Reduces Sta. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },

	{ SSC_INPUT,        SSC_NUMBER,       "ibi_sta_percent",               "State Percentage-based IBI Percent",                   "%",      "",                      "Payment Incentives",      "?=0.0",           "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_percent_maxvalue",      "State Percentage-based IBI Maximum Value",             "$",      "",                      "Payment Incentives",      "?=1e99",          "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_percent_tax_fed",       "State Percentage-based IBI Fed. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_percent_tax_sta",       "State Percentage-based IBI Sta. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_percent_deprbas_fed",   "State Percentage-based IBI Reduces Fed. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_sta_percent_deprbas_sta",   "State Percentage-based IBI Reduces Sta. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },

	{ SSC_INPUT,        SSC_NUMBER,       "ibi_uti_percent",               "Utility Percentage-based IBI Percent",                   "%",      "",                      "Payment Incentives",      "?=0.0",           "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_percent_maxvalue",      "Utility Percentage-based IBI Maximum Value",             "$",      "",                      "Payment Incentives",      "?=1e99",          "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_percent_tax_fed",       "Utility Percentage-based IBI Fed. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_percent_tax_sta",       "Utility Percentage-based IBI Sta. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_percent_deprbas_fed",   "Utility Percentage-based IBI Reduces Fed. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_uti_percent_deprbas_sta",   "Utility Percentage-based IBI Reduces Sta. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },

	{ SSC_INPUT,        SSC_NUMBER,       "ibi_oth_percent",               "Other Percentage-based IBI Percent",                   "%",      "",                      "Payment Incentives",      "?=0.0",           "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_percent_maxvalue",      "Other Percentage-based IBI Maximum Value",             "$",      "",                      "Payment Incentives",      "?=1e99",          "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_percent_tax_fed",       "Other Percentage-based IBI Fed. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_percent_tax_sta",       "Other Percentage-based IBI Sta. Taxable",              "0/1",    "",                      "Payment Incentives",      "?=1",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_percent_deprbas_fed",   "Other Percentage-based IBI Reduces Fed. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibi_oth_percent_deprbas_sta",   "Other Percentage-based IBI Reduces Sta. Depr. Basis",  "0/1",    "",                      "Payment Incentives",      "?=0",             "BOOLEAN",                                         "" },


	{ SSC_INPUT,        SSC_NUMBER,       "cbi_fed_amount",         "Federal CBI Amount",                   "$/Watt", "",                      "Payment Incentives",      "?=0.0",                     "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_fed_maxvalue",       "Federal CBI Maximum",                  "$",      "",                      "Payment Incentives",      "?=1e99",                    "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_fed_tax_fed",        "Federal CBI Fed. Taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_fed_tax_sta",        "Federal CBI Sta. Taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_fed_deprbas_fed",    "Federal CBI Reduces Fed. Depr. Basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_fed_deprbas_sta",    "Federal CBI Reduces Sta. Depr. Basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "cbi_sta_amount",         "State CBI Amount",                   "$/Watt", "",                      "Payment Incentives",      "?=0.0",                     "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_sta_maxvalue",       "State CBI Maximum",                  "$",      "",                      "Payment Incentives",      "?=1e99",                    "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_sta_tax_fed",        "State CBI Fed. Taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_sta_tax_sta",        "State CBI Sta. Taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_sta_deprbas_fed",    "State CBI Reduces Fed. Depr. Basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_sta_deprbas_sta",    "State CBI Reduces Sta. Depr. Basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "cbi_uti_amount",         "Utility CBI Amount",                   "$/Watt", "",                      "Payment Incentives",      "?=0.0",                     "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_uti_maxvalue",       "Utility CBI Maximum",                  "$",      "",                      "Payment Incentives",      "?=1e99",                    "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_uti_tax_fed",        "Utility CBI Fed. Taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_uti_tax_sta",        "Utility CBI Sta. Taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_uti_deprbas_fed",    "Utility CBI Reduces Fed. Depr. Basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_uti_deprbas_sta",    "Utility CBI Reduces Sta. Depr. Basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "cbi_oth_amount",         "Other CBI Amount",                   "$/Watt", "",                      "Payment Incentives",      "?=0.0",                     "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_oth_maxvalue",       "Other CBI Maximum",                  "$",      "",                      "Payment Incentives",      "?=1e99",                    "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_oth_tax_fed",        "Other CBI Fed. Taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_oth_tax_sta",        "Other CBI Sta. Taxable",             "0/1",    "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_oth_deprbas_fed",    "Other CBI Reduces Fed. Depr. Basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cbi_oth_deprbas_sta",    "Other CBI Reduces Sta. Depr. Basis", "0/1",    "",                      "Payment Incentives",      "?=0",                       "BOOLEAN",                                         "" },


	{ SSC_INPUT,        SSC_ARRAY,       "pbi_fed_amount",                "Federal PBI Amount",           "$/kWh",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_fed_term",                  "Federal PBI Term",             "years",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_fed_escal",                 "Federal PBI Escalation",       "%",        "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_fed_tax_fed",               "Federal PBI Fed. Taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_fed_tax_sta",               "Federal PBI Sta. Taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "pbi_sta_amount",                "State PBI Amount",           "$/kWh",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_sta_term",                  "State PBI Term",             "years",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_sta_escal",                 "State PBI Escalation",       "%",        "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_sta_tax_fed",               "State PBI Fed. Taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_sta_tax_sta",               "State PBI Sta. Taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "pbi_uti_amount",                "Utility PBI Amount",           "$/kWh",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_uti_term",                  "Utility PBI Term",             "years",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_uti_escal",                 "Utility PBI Escalation",       "%",        "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_uti_tax_fed",               "Utility PBI Fed. Taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_uti_tax_sta",               "Utility PBI Sta. Taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "pbi_oth_amount",                "Other PBI Amount",           "$/kWh",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_oth_term",                  "Other PBI Term",             "years",    "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_oth_escal",                 "Other PBI Escalation",       "%",        "",                      "Payment Incentives",      "?=0",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_oth_tax_fed",               "Other PBI Fed. Taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pbi_oth_tax_sta",               "Other PBI Sta. Taxable",     "0/1",      "",                      "Payment Incentives",      "?=1",                       "BOOLEAN",                                         "" },

var_info_invalid };


var_info vtab_adjustment_factors[] = {
/*   VARTYPE           DATATYPE         NAME                               LABEL                                       UNITS     META                                     GROUP                 REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,        SSC_NUMBER,      "adjust:factor",                 "Constant adjustment factor",                "0..1",    "",                                     "Adjustment factors",      "*",                     "POSITIVE",                     "" },
	{ SSC_INPUT,        SSC_ARRAY,       "adjust:hourly",                 "Hourly adjustment factors",                 "0..1",    "",                                     "Adjustment factors",      "?",                     "LENGTH=8760",                "" },
	{ SSC_INPUT,        SSC_MATRIX,      "adjust:periods",                "Period based adjustment factors",           "0..1",    "n x 3 matrix [ start, end, factor ]",  "Adjustment factors",      "?",                     "COLS=3",                     "" },
	
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



