#include "lib_financial.h"
#include "core.h"

static var_info _cm_vtab_levpartflip[] = {

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
	{ SSC_INPUT,        SSC_NUMBER,     "equip_reserve1_cost",      "Major equipment reserve1 cost",	"$/Wdc",	 "",				  "DHF",             "?=0.25",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "equip_reserve1_freq",      "Major equipment reserve1 frequency",	"years",	 "",			  "DHF",             "?=12",               "INTEGER,MIN=0",                         "" },

	{ SSC_INPUT,        SSC_NUMBER,     "equip_reserve2_cost",      "Major equipment reserve2 cost",	"$/Wdc",	 "",				  "DHF",             "?=0",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "equip_reserve2_freq",      "Major equipment reserve2 frequency",	"years",	 "",			  "DHF",             "?=15",               "INTEGER,MIN=0",                         "" },

	{ SSC_INPUT,        SSC_NUMBER,     "equip_reserve3_cost",      "Major equipment reserve3 cost",	"$/Wdc",	 "",				  "DHF",             "?=0",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "equip_reserve3_freq",      "Major equipment reserve3 frequency",	"years",	 "",			  "DHF",             "?=20",               "INTEGER,MIN=0",                         "" },

// TODO - add to depreciation static table */
	{ SSC_INPUT,        SSC_NUMBER,     "equip_reserve_depr_sta",   "Major equipment reserve state depreciation",	"",	 "0=5yr MACRS,1=15yr MACRS,2=5yr SL,3=15yr SL, 4=20yr SL,5=39yr SL",  "DHF", "?=0",   "INTEGER,MIN=0,MAX=5",  "" },
	{ SSC_INPUT,        SSC_NUMBER,     "equip_reserve_depr_fed",   "Major equipment reserve federal depreciation",	"",	 "0=5yr MACRS,1=15yr MACRS,2=5yr SL,3=15yr SL, 4=20yr SL,5=39yr SL",  "DHF", "?=0",   "INTEGER,MIN=0,MAX=5",  "" },

/* DHF salvage value */	
	{ SSC_INPUT,        SSC_NUMBER,     "salvage_percentage",          "Net pre-tax cash salvage value",	"%",	 "",					  "DHF",             "?=10",                     "MIN=0,MAX=100",      			"" },
/* DHF market specific inputs - leveraged partnership flip */
	{ SSC_INPUT,        SSC_NUMBER,		"ppa_soln_mode",            "PPA solution mode",                "0/1",   "0=specify ppa,1=solve ppa", "DHF",         "?=0",                     "INTEGER,MIN=0,MAX=1",            "" },
	{ SSC_INPUT,        SSC_NUMBER,     "ppa_price_input",			"Initial year PPA price",			"cents/kWh",	 "",			  "DHF",			 "ppa_soln_mode=0",         "",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "ppa_escalation",           "PPA escalation",					"%",	 "",					  "DHF",             "?=0",                     "MIN=0,MAX=100",      			"" },
/* DHF construction period */
	{ SSC_INPUT,        SSC_NUMBER,     "constr_period",            "Construction period",				"months", "",				      "DHF",             "?=10",					"INTEGER,MIN=0",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "constr_int_rate",          "Construction interest rate",		"%",	 "",					  "DHF",             "?=4",                     "MIN=0,MAX=100",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "ppa_escalation",           "Construction up-front fee",    	"%",	 "",					  "DHF",             "?=1",                     "MIN=0,MAX=100",      			"" },
/* DHF term financing */
	{ SSC_INPUT,        SSC_NUMBER,     "term_tenor",               "Term financing tenor",				"years", "",				      "DHF",             "?=10",					"INTEGER,MIN=0",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "term_int_rate",            "Term financing interest rate",		"%",	 "",					  "DHF",             "?=8.5",                   "MIN=0,MAX=100",      			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "dscr",						"Debt service coverage ratio",		"",	     "",				      "DHF",             "?=1.5",					"MIN=0",      			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "dscr_reserve",				"Debt service reserve account",		"months P&I","",			      "DHF",             "?=6",					    "INTEGER,MIN=0",      			        "" },
/* DHF Capital Cost */
	{ SSC_INPUT,        SSC_NUMBER,     "cost_debt_closing",		"Debt closing cost",				"$",	 "",					  "DHF",             "?=250000",					    "MIN=0",      			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "cost_equity_closing",		"Equity closing cost",				"$",	 "",					  "DHF",             "?=100000",					    "MIN=0",      			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "cost_working_reserve",		"Q&M/Working capital reserve",		"$",	 "",					  "DHF",             "?=150000",					    "MIN=0",      			        "" },
/* DHF Equity Structure */
	{ SSC_INPUT,        SSC_NUMBER,     "equity_tax_investor",		"Tax investor equity",				"%",	 "",					  "DHF",             "?=98",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "preflip_sharing_tax_investor",		"Tax investor pre-flip cash sharing ratio",		"%",	 "",  "DHF",             "?=98",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "postflip_sharing_tax_investor",	"Tax investor post-flip cash sharing ratio",	"%",	 "",  "DHF",             "?=15",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "return_target",			"After-tax flip/return target",		"%",	 "",					  "DHF",             "?=11",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "return_target_year",		"Return target year",				"",		 "",					  "DHF",             "?=11",					  "MIN=1",     			        "" },
/* DHF depreciation allocation */
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_macrs_5",		"5-yr MACRS depreciation federal and state allocation",	"%", "",	  "DHF",             "?=89",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_macrs_15",		"15-yr MACRS depreciation federal and state allocation",	"%", "",  "DHF",             "?=1.5",					  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_5",		"5-yr straight line depreciation federal and state allocation",	"%", "",  "DHF",             "?=0",						  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_15",		"15-yr straight line depreciation federal and state allocation","%", "",  "DHF",             "?=3",						  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_20",		"20-yr straight line depreciation federal and state allocation","%", "",  "DHF",             "?=3",						  "MIN=0,MAX=100",     			        "" },
	{ SSC_INPUT,        SSC_NUMBER,     "depr_alloc_sl_39",		"39-yr straight line depreciation federal and state allocation","%", "",  "DHF",             "?=0.5",					  "MIN=0,MAX=100",     			        "" },
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

/* intermediate outputs */
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_contingency",        "Contingency cost",                 "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_hard",               "Hard cost",                        "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_soft",               "Soft cost",                        "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_salestax",           "Sales tax",                        "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_installed",          "Installed cost",                   "$",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_installedperwatt",   "Installed cost per watt",          "$/W",   "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "nominal_discount_rate",   "Nominal discount rate",            "%",     "",					  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "prop_tax_assessed_value", "Assessed value of property for tax purposes","$", "",				  "DHF",			 "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "salvage_value",			"Net pre-tax cash salvage value",	"$",	 "",					  "DHF",			 "*",                         "",                             "" },

/* model outputs */
	{ SSC_OUTPUT,        SSC_NUMBER,     "cf_length",                "Number of periods in cashflow",      "",             "",                      "DHF",      "*",                       "INTEGER",                                  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "ppa_price",			    "Initial year PPA price",			"cents/kWh",	"",				   "DHF",			  "*",                         "",      					   "" },
/* Production - input as energy_net above */

/* Partial Income Statement: Project */	
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

/*
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_deductible_expenses",   "Deductible expenses",                "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
		
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_debt_balance",          "Debt balance",                       "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_debt_payment_interest", "Interest payment",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_debt_payment_principal","Principal payment",                  "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_debt_payment_total",    "Total P&I debt payment",             "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_ibi_total",             "Total IBI incentive income",         "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_cbi_total",             "Total CBI incentive income",         "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_pbi_total",             "Total PBI incentive income",         "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_ptc_fed",               "Federal PTC income",                 "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_ptc_sta",               "State PTC income",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_itc_fed_total",         "Federal ITC income",                 "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_itc_sta_total",         "State ITC income",                   "$",            "",                      "DHF",      "*",                     "LENGTH_EQUAL=cf_length",                "" },
*/

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

	CF_deductible_expenses,

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



class cm_levpartflip : public compute_module
{
private:
	util::matrix_t<double> cf;

public:
	cm_levpartflip()
	{
		add_var_info( vtab_standard_financial );
		add_var_info( vtab_standard_loan );
		add_var_info( vtab_oandm );
		add_var_info( vtab_depreciation );
		add_var_info( vtab_tax_credits );
		add_var_info( vtab_payment_incentives );
				
		add_var_info( _cm_vtab_levpartflip );
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
		double nom_discount_rate = (1+inflation_rate)*(1+disc_real)-1;
		assign( "nominal_discount_rate", var_data((ssc_number_t)nom_discount_rate ) );

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
		assign( "cost_contingency", var_data((ssc_number_t) cost_cont ) );
		assign( "cost_hard", var_data( (ssc_number_t)(cost_hard + cost_cont)) );
		assign( "cost_salestax", var_data((ssc_number_t)cost_salestax ) );
		assign( "cost_soft", var_data((ssc_number_t) (cost_soft + cost_salestax) ) );

		// use DHF named range names for variables whenever possible
		double pre_financing_installed_cost_total = cost_soft + cost_salestax + cost_hard + cost_cont;
		double total_cost = pre_financing_installed_cost_total; // translation to values used in cmod_DHF for incentive calculations
		assign( "cost_installed", var_data((ssc_number_t) pre_financing_installed_cost_total ) );

		double nameplate = as_double("system_capacity");
		assign( "cost_installedperwatt", var_data((ssc_number_t)( pre_financing_installed_cost_total / nameplate / 1000.0 ) ));

		double assessed_frac = as_double("prop_tax_cost_assessed_percent")*0.01;
		assign( "prop_tax_assessed_value", var_data((ssc_number_t)( assessed_frac * pre_financing_installed_cost_total )));

		double salvage_value_frac = as_double("salvage_percentage")*0.01;
		double salvage_value = salvage_value_frac * pre_financing_installed_cost_total;
		assign( "salvage_value", var_data((ssc_number_t)salvage_value));


		// initialize energy
		size_t count = 0;
		ssc_number_t *arrp = 0;
		
		arrp = as_array("energy_net", &count);
		int i=0;
		while ( i < nyears && i < (int)count )
		{
			cf.at(CF_energy_net, i+1) = (double) arrp[i];
			i++;
		}

		int ppa_mode = as_integer("ppa_soln_mode");


		// general financial expenses and incentives - stdlib?
		// precompute expenses from annual schedules or value+escalation
		escal_or_annual( CF_om_fixed_expense, nyears, "om_fixed", inflation_rate, 1.0, false, as_double("om_fixed_escal")*0.01 );
		escal_or_annual( CF_om_production_expense, nyears, "om_production", inflation_rate, 0.001, false, as_double("om_production_escal")*0.01 );  
		escal_or_annual( CF_om_capacity_expense, nyears, "om_capacity", inflation_rate, 1.0, false, as_double("om_capacity_escal")*0.01 );  
		escal_or_annual( CF_om_fuel_expense, nyears, "om_fuel_cost", inflation_rate, as_double("system_heat_rate")*0.001, false, as_double("om_fuel_cost_escal")*0.01 );
		
		// precompute ibi
		single_or_schedule( CF_ibi_fed_amt, nyears, 1.0, "ibi_fed_amount" );
		single_or_schedule( CF_ibi_sta_amt, nyears, 1.0, "ibi_sta_amount" );
		single_or_schedule( CF_ibi_uti_amt, nyears, 1.0, "ibi_uti_amount" );
		single_or_schedule( CF_ibi_oth_amt, nyears, 1.0, "ibi_oth_amount" );

		single_or_schedule_check_max( CF_ibi_fed_per, nyears, 0.01*total_cost, "ibi_fed_percent", "ibi_fed_percent_maxvalue" );
		single_or_schedule_check_max( CF_ibi_sta_per, nyears, 0.01*total_cost, "ibi_sta_percent", "ibi_sta_percent_maxvalue" );
		single_or_schedule_check_max( CF_ibi_uti_per, nyears, 0.01*total_cost, "ibi_uti_percent", "ibi_uti_percent_maxvalue" );
		single_or_schedule_check_max( CF_ibi_oth_per, nyears, 0.01*total_cost, "ibi_oth_percent", "ibi_oth_percent_maxvalue" );
		
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
		
		// precompute credit basis
		double federal_credit_basis = total_cost
			- ( as_boolean("ibi_fed_amount_itcbas_fed")  ? npv( CF_ibi_fed_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_sta_amount_itcbas_fed")  ? npv( CF_ibi_sta_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_uti_amount_itcbas_fed")  ? npv( CF_ibi_uti_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_oth_amount_itcbas_fed")  ? npv( CF_ibi_oth_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_fed_percent_itcbas_fed") ? npv( CF_ibi_fed_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_sta_percent_itcbas_fed") ? npv( CF_ibi_sta_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_uti_percent_itcbas_fed") ? npv( CF_ibi_uti_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_oth_percent_itcbas_fed") ? npv( CF_ibi_oth_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_fed_itcbas_fed")  ? npv( CF_cbi_fed, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_sta_itcbas_fed")  ? npv( CF_cbi_sta, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_uti_itcbas_fed")  ? npv( CF_cbi_uti, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_oth_itcbas_fed")  ? npv( CF_cbi_oth, nyears, nom_discount_rate ) : 0 );
				
		double state_credit_basis = total_cost
			- ( as_boolean("ibi_fed_amount_itcbas_sta")  ? npv( CF_ibi_fed_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_sta_amount_itcbas_sta")  ? npv( CF_ibi_sta_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_uti_amount_itcbas_sta")  ? npv( CF_ibi_uti_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_oth_amount_itcbas_sta")  ? npv( CF_ibi_oth_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_fed_percent_itcbas_sta") ? npv( CF_ibi_fed_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_sta_percent_itcbas_sta") ? npv( CF_ibi_sta_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_uti_percent_itcbas_sta") ? npv( CF_ibi_uti_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_oth_percent_itcbas_sta") ? npv( CF_ibi_oth_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_fed_itcbas_sta")  ? npv( CF_cbi_fed, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_sta_itcbas_sta")  ? npv( CF_cbi_sta, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_uti_itcbas_sta")  ? npv( CF_cbi_uti, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_oth_itcbas_sta")  ? npv( CF_cbi_oth, nyears, nom_discount_rate ) : 0 );
		
		// precompute itc
		single_or_schedule( CF_itc_fed_amt, nyears, 1.0, "itc_fed_amount" );
		single_or_schedule( CF_itc_sta_amt, nyears, 1.0, "itc_sta_amount" );
		single_or_schedule_check_max( CF_itc_fed_per, nyears, 0.01*federal_credit_basis, "itc_fed_percent", "itc_fed_percent_maxvalue");
		single_or_schedule_check_max( CF_itc_sta_per, nyears, 0.01*state_credit_basis, "itc_sta_percent", "itc_sta_percent_maxvalue");

		// precompute depreciation schedules
		double federal_depr_basis = total_cost
			- ( as_boolean("ibi_fed_amount_deprbas_fed")  ? npv( CF_ibi_fed_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_sta_amount_deprbas_fed")  ? npv( CF_ibi_sta_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_uti_amount_deprbas_fed")  ? npv( CF_ibi_uti_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_oth_amount_deprbas_fed")  ? npv( CF_ibi_oth_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_fed_percent_deprbas_fed") ? npv( CF_ibi_fed_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_sta_percent_deprbas_fed") ? npv( CF_ibi_sta_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_uti_percent_deprbas_fed") ? npv( CF_ibi_uti_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_oth_percent_deprbas_fed") ? npv( CF_ibi_oth_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_fed_deprbas_fed")  ? npv( CF_cbi_fed, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_sta_deprbas_fed")  ? npv( CF_cbi_sta, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_uti_deprbas_fed")  ? npv( CF_cbi_uti, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_oth_deprbas_fed")  ? npv( CF_cbi_oth, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("itc_fed_amount_deprbas_fed")   ? 0.5*(1+nom_discount_rate)*npv( CF_itc_fed_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("itc_fed_percent_deprbas_fed")  ? 0.5*(1+nom_discount_rate)*npv( CF_itc_fed_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("itc_sta_amount_deprbas_fed")   ? 0.5*(1+nom_discount_rate)*npv( CF_itc_sta_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("itc_sta_percent_deprbas_fed")  ? 0.5*(1+nom_discount_rate)*npv( CF_itc_sta_per, nyears, nom_discount_rate ) : 0 );

		double state_depr_basis = total_cost
			- ( as_boolean("ibi_fed_amount_deprbas_sta")  ? npv( CF_ibi_fed_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_sta_amount_deprbas_sta")  ? npv( CF_ibi_sta_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_uti_amount_deprbas_sta")  ? npv( CF_ibi_uti_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_oth_amount_deprbas_sta")  ? npv( CF_ibi_oth_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_fed_percent_deprbas_sta") ? npv( CF_ibi_fed_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_sta_percent_deprbas_sta") ? npv( CF_ibi_sta_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_uti_percent_deprbas_sta") ? npv( CF_ibi_uti_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("ibi_oth_percent_deprbas_sta") ? npv( CF_ibi_oth_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_fed_deprbas_sta")  ? npv( CF_cbi_fed, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_sta_deprbas_sta")  ? npv( CF_cbi_sta, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_uti_deprbas_sta")  ? npv( CF_cbi_uti, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("cbi_oth_deprbas_sta")  ? npv( CF_cbi_oth, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("itc_fed_amount_deprbas_sta")   ? 0.5*(1+nom_discount_rate)*npv( CF_itc_fed_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("itc_fed_percent_deprbas_sta")  ? 0.5*(1+nom_discount_rate)*npv( CF_itc_fed_per, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("itc_sta_amount_deprbas_sta")   ? 0.5*(1+nom_discount_rate)*npv( CF_itc_sta_amt, nyears, nom_discount_rate ) : 0 )
			- ( as_boolean("itc_sta_percent_deprbas_sta")  ? 0.5*(1+nom_discount_rate)*npv( CF_itc_sta_per, nyears, nom_discount_rate ) : 0 );



		double property_tax_assessed_value = pre_financing_installed_cost_total * as_double("prop_tax_cost_assessed_percent") * 0.01;
		double property_tax_decline_percentage = as_double("prop_tax_assessed_decline");
		double property_tax_rate = as_double("property_tax_rate")*0.01;
		double insurance_rate = as_double("insurance_rate")*0.01;
		for (i=1; i<=nyears; i++)
		{			
			double decline_percent = 100 - (i-1)*property_tax_decline_percentage;
			cf.at(CF_property_tax_assesed_value,i) = (decline_percent > 0) ? property_tax_assessed_value * decline_percent * 0.01:0.0;
			cf.at(CF_property_tax_expense,i) = cf.at(CF_property_tax_assesed_value,i) * property_tax_rate;
			cf.at(CF_insurance_expense,i) = total_cost * insurance_rate * pow( 1 + inflation_rate, i-1 );

			cf.at(CF_operating_expenses,i) = 
				+ cf.at(CF_om_fixed_expense,i)
				+ cf.at(CF_om_production_expense,i)
				+ cf.at(CF_om_capacity_expense,i)
				+ cf.at(CF_om_fuel_expense,i)
				+ cf.at(CF_property_tax_expense,i)
				+ cf.at(CF_insurance_expense,i);
		}


		// outputs
		assign( "cf_length", var_data( (ssc_number_t) nyears+1 ));
		double ppa = 0;
		if (ppa_mode == 0)
			ppa = as_double("ppa_price_input");

		// energy_value = DHF Total PPA Revenue
		for (i=1; i<=nyears; i++)
		{			
			cf.at(CF_ppa_price,i) = ppa * pow( 1 + ppa_escalation, i-1 ); // ppa_mode==0
			cf.at(CF_energy_value,i) = cf.at(CF_ppa_price,i) /100.0;
		}
		// salvage value
		cf.at(CF_energy_value,nyears) += salvage_value;


		assign("ppa_price", var_data((ssc_number_t) ppa));

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
			switch(i)
			{
			case 1: factor = 0.0128; break;
			case 2: factor = 0.0256; break;
			case 3: factor = 0.0256; break;
			case 4: factor = 0.0256; break;
			case 5: factor = 0.0256; break;
			case 6: factor = 0.0256; break;
			case 7: factor = 0.0256; break;
			case 8: factor = 0.0256; break;
			case 9: factor = 0.0256; break;
			case 10: factor = 0.0256; break;
			case 11: factor = 0.0256; break;
			case 12: factor = 0.0256; break;
			case 13: factor = 0.0256; break;
			case 14: factor = 0.0256; break;
			case 15: factor = 0.0256; break;
			case 16: factor = 0.0256; break;
			case 17: factor = 0.0256; break;
			case 18: factor = 0.0256; break;
			case 19: factor = 0.0256; break;
			case 20: factor = 0.0256; break;
			case 21: factor = 0.0256; break;
			case 22: factor = 0.0256; break;
			case 23: factor = 0.0256; break;
			case 24: factor = 0.0256; break;
			case 25: factor = 0.0256; break;
			case 26: factor = 0.0256; break;
			case 27: factor = 0.0256; break;
			case 28: factor = 0.0256; break;
			case 29: factor = 0.0256; break;
			case 30: factor = 0.0256; break;
			case 31: factor = 0.0256; break;
			case 32: factor = 0.0256; break;
			case 33: factor = 0.0256; break;
			case 34: factor = 0.0256; break;
			case 35: factor = 0.0256; break;
			case 36: factor = 0.0256; break;
			case 37: factor = 0.0256; break;
			case 38: factor = 0.0256; break;
			case 39: factor = 0.0256; break;
			case 40: factor = 0.0128; break;
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


	double min( double a, double b )
	{
		return (a < b) ? a : b;
	}

};




DEFINE_MODULE_ENTRY( levpartflip, "DHF Leveraged Partnership Flip Financial Model_", 1 );


