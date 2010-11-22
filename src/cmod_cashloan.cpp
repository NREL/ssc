#include "core.h"
#include "lib_financial.h"

static var_info vtab_cashloan[] = {
/*   VARTYPE           DATATYPE          NAME                        LABEL                                  UNITS         META                      GROUP            REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,        SSC_NUMBER,		 "market",                   "Residential or Commercial Market",   "0/1",          "0=residential,1=comm.", "Cashloan",      "?=1",                     "INTEGER,MIN=0,MAX=1",            "" },
	{ SSC_INPUT,        SSC_NUMBER,		 "mortgage",		         "Use mortgage style loan (res. only)","0/1",          "",                      "Cashloan",      "?=0",                     "BOOLEAN",                        "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "annual_fuel_usage",        "Fuel usage",                         "kWht",         "",                      "Cashloan",      "?=0",                     "MIN=0",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "energy_value",             "Energy value",                       "$",            "",                      "Cashloan",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "energy_net",               "Net energy",                         "kWh",          "",                      "Cashloan",      "*",                       "",                                         "" },

	/* financial outputs */
	{ SSC_OUTPUT,        SSC_NUMBER,     "lcoe_real",                "Real LCOE",                          "cents/kWh",    "",                      "Cashloan",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "lcoe_nom",                 "Nominal LCOE",                       "cents/kWh",    "",                      "Cashloan",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "payback",                  "Payback",                            "years",        "",                      "Cashloan",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "npv",                      "Net present value",				   "$",            "",                      "Cashloan",      "*",                       "",                                         "" },
			
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_om_fixed_expense",      "O&M Fixed expense",                  "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_om_production_expense", "O&M Production-based expense",       "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_om_capacity_expense",   "O&M Capacity-based expense",         "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_om_fuel_expense",       "O&M Fuel expense",                   "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_property_tax_expense",  "Property tax expense",               "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_insurance_expense",     "Insurance expense",                  "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_operating_expenses",    "Total operating expense",            "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_deductible_expenses",   "Deductible expenses",                "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
		
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_debt_balance",          "Debt balance",                       "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_debt_payment_interest", "Interest payment",                   "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_debt_payment_principal","Principal payment",                  "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_debt_payment_total",    "Total P&I debt payment",             "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_ibi_total",             "Total IBI incentive income",         "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_cbi_total",             "Total CBI incentive income",         "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_pbi_total",             "Total PBI incentive income",         "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_ptc_fed",               "Federal PTC income",                 "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_ptc_sta",               "State PTC income",                   "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_itc_fed_total",         "Federal ITC income",                 "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_itc_sta_total",         "State ITC income",                   "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sta_depr_sched",                        "State depreciation schedule",              "%",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sta_depreciation",                      "State depreciation",                       "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sta_incentive_income_less_deductions",  "State incentive income less deductions",   "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sta_taxable_income_less_deductions",    "State taxable income less deductions",     "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sta_tax_savings",                       "State tax savings",                        "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_fed_depr_sched",                        "Federal depreciation schedule",            "%",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_fed_depreciation",                      "Federal depreciation",                     "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_fed_incentive_income_less_deductions",  "Federal incentive income less deductions", "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_fed_taxable_income_less_deductions",    "Federal taxable income less deductions",   "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_fed_tax_savings",                       "Federal tax savings",                      "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_sta_and_fed_tax_savings",               "Total tax savings (Federal & State)",      "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_after_tax_net_equity_cost_flow",        "After tax net equity cost flow",           "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_after_tax_cash_flow",                   "After tax cash flow",                      "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },

	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_payback_with_expenses",                 "Payback with expenses",                    "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_cumulative_payback_with_expenses",      "Cumulative payback with expenses",         "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_payback_without_expenses",              "Payback without expenses",                 "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "cf_cumulative_payback_without_expenses",   "Cumulative payback without expenses",      "$",            "",                      "Cashloan",      "*",                     "LENGTH=analysis_years",                "" },
	
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

	CF_om_fixed_expense,
	CF_om_production_expense,
	CF_om_capacity_expense,
	CF_om_fuel_expense,
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




class cm_cashloan : public compute_module
{
private:
	util::matrix_t<double> cf;

public:
	cm_cashloan()
	{
		add_var_info( vtab_standard_financial );
		add_var_info( vtab_standard_loan );
		add_var_info( vtab_oandm );
		add_var_info( vtab_depreciation );
		add_var_info( vtab_tax_credits );
		add_var_info( vtab_payment_incentives );
				
		add_var_info( vtab_cashloan );
	}

	void exec( ) throw( general_error )
	{
		int i;

		bool is_commercial = (as_integer("market")==1);
		bool is_mortgage = as_boolean("mortgage");

		if (is_commercial) log("commercial market"); else log("residential market");
		if (is_mortgage) log("mortgage loan"); else log("standard loan");

		int nyears = as_integer("analysis_years");

		// initialize cashflow matrix
		cf.resize_fill( CF_max, nyears+1, 0.0 );

		// initialize energy and revenue
		size_t count = 0;
		ssc_number_t *arrp = 0;
		
		arrp = as_array("energy_net", &count);
		i=0;
		while ( i < nyears && i < (int)count )
		{
			cf.at(CF_energy_net, i+1) = (double) arrp[i];
			i++;
		}

		arrp = as_array("energy_value", &count);
		i=0;
		while ( i < nyears && i < (int)count )
		{
			cf.at(CF_energy_value, i+1) = (double) arrp[i];
			i++;
		}
		
		double year1_fuel_use = as_double("annual_fuel_usage"); // kWht
    	double nameplate = as_double("system_capacity"); // kW
		
		double inflation_rate = as_double("inflation_rate")*0.01;
		double property_tax = as_double("property_tax_rate")*0.01;
		double insurance_rate = as_double("insurance_rate")*0.01;
		double salvage_frac = as_double("salvage_percentage")*0.01;
		double federal_tax_rate = as_double("federal_tax_rate")*0.01;
		double state_tax_rate = as_double("state_tax_rate")*0.01;
		double effective_tax_rate = federal_tax_rate + (1-federal_tax_rate)*state_tax_rate;
		
		double real_discount_rate = as_double("real_discount_rate")*0.01;
		double nom_discount_rate = (1.0 + real_discount_rate) * (1.0 + inflation_rate) - 1.0;


		double hard_cost = as_double("total_hard_cost");
		double total_sales_tax = as_double("percent_of_cost_subject_sales_tax")*0.01*hard_cost*as_double("sales_tax_rate")*0.01;
		double soft_cost = as_double("total_soft_cost") + total_sales_tax;
		double total_cost = hard_cost + soft_cost;

		int loan_term = as_integer("loan_term");
		double loan_rate = as_double("loan_rate")*0.01;
		double debt_frac = as_double("loan_debt")*0.01;
				
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

		if (is_commercial)
		{
			// only compute depreciation for commercial market

			switch( as_integer("depr_sta_type") )
			{
			case 1: depreciation_sched_macrs_half_year( CF_sta_depr_sched, nyears ); break;
			case 2: depreciation_sched_straight_line( CF_sta_depr_sched, nyears, as_integer("depr_sta_sl_years") ); break;
			case 3: 
				{
					size_t arr_len;
					ssc_number_t *arr_cust = as_array( "depr_sta_custom", &arr_len );
					depreciation_sched_custom( CF_sta_depr_sched, nyears, arr_cust, arr_len );
					break;
				}
			}

			switch( as_integer("depr_fed_type") )
			{
			case 1: depreciation_sched_macrs_half_year( CF_fed_depr_sched, nyears ); break;
			case 2: depreciation_sched_straight_line( CF_fed_depr_sched, nyears, as_integer("depr_fed_sl_years") ); break;
			case 3: 
				{
					size_t arr_len;
					ssc_number_t *arr_cust = as_array( "depr_fed_custom", &arr_len );
					depreciation_sched_custom( CF_fed_depr_sched, nyears, arr_cust, arr_len );
					break;
				}
			}
		}
		
		double state_tax_savings = 0.0;
		double federal_tax_savings = 0.0;

		double adjusted_installed_cost = total_cost
			- npv( CF_ibi_fed_amt, nyears, nom_discount_rate )
			- npv( CF_ibi_sta_amt, nyears, nom_discount_rate )
			- npv( CF_ibi_uti_amt, nyears, nom_discount_rate )
			- npv( CF_ibi_oth_amt, nyears, nom_discount_rate )
			- npv( CF_ibi_fed_per, nyears, nom_discount_rate )
			- npv( CF_ibi_sta_per, nyears, nom_discount_rate )
			- npv( CF_ibi_uti_per, nyears, nom_discount_rate )
			- npv( CF_ibi_oth_per, nyears, nom_discount_rate )
			- npv( CF_cbi_fed, nyears, nom_discount_rate )
			- npv( CF_cbi_sta, nyears, nom_discount_rate )
			- npv( CF_cbi_uti, nyears, nom_discount_rate )
			- npv( CF_cbi_oth, nyears, nom_discount_rate );

		double loan_amount = debt_frac * adjusted_installed_cost;
		double first_cost = adjusted_installed_cost - loan_amount;
		double capital_investment = loan_amount + first_cost;
		
		cf.at(CF_after_tax_net_equity_cost_flow,0) = -first_cost + state_tax_savings + federal_tax_savings;
		cf.at(CF_after_tax_cash_flow,0) = cf.at(CF_after_tax_net_equity_cost_flow,0);

		cf.at(CF_payback_with_expenses,0) = -capital_investment;
		cf.at(CF_cumulative_payback_with_expenses,0) = -capital_investment;
		
		cf.at(CF_payback_without_expenses,0) = -capital_investment;
		cf.at(CF_cumulative_payback_without_expenses,0) = -capital_investment;

		for (i=1; i<=nyears; i++)
		{			
			// compute expenses
			cf.at(CF_om_production_expense,i) *= cf.at(CF_energy_net,i);
			cf.at(CF_om_capacity_expense,i) *= nameplate;
			cf.at(CF_om_fuel_expense,i) *= year1_fuel_use;
			cf.at(CF_property_tax_expense,i) =  total_cost * property_tax * pow( 1 + inflation_rate, i-1 );
			
			cf.at(CF_insurance_expense,i) = total_cost * insurance_rate * pow( 1 + inflation_rate, i-1 );

			cf.at(CF_operating_expenses,i) = 
				+ cf.at(CF_om_fixed_expense,i)
				+ cf.at(CF_om_production_expense,i)
				+ cf.at(CF_om_capacity_expense,i)
				+ cf.at(CF_om_fuel_expense,i)
				+ cf.at(CF_property_tax_expense,i)
				+ cf.at(CF_insurance_expense,i);

			if (i == nyears) /* salvage value handled as negative operating expense in last year */
				cf.at(CF_operating_expenses,i) -= total_cost * salvage_frac * pow( 1+inflation_rate, i-1 );
			
			if (is_commercial)
				cf.at(CF_deductible_expenses,i) = -cf.at(CF_operating_expenses,i);  // commercial
			else
				cf.at(CF_deductible_expenses,i) = -cf.at(CF_property_tax_expense,i); // residential

			if (i == 1)
			{
				cf.at(CF_debt_balance,i) = -loan_amount;
				cf.at(CF_debt_payment_interest,i) = loan_amount * loan_rate;
				cf.at(CF_debt_payment_principal,i) = -ppmt( loan_rate,       // Rate
																i,           // Period
																loan_term,   // Number periods
																loan_amount, // Present Value
																0,           // future Value
																0 );         // cash flow at end of period
			}
			else
			{
				if (i <= loan_term) 
				{
					cf.at(CF_debt_balance,i) = cf.at(CF_debt_balance,i-1) + cf.at(CF_debt_payment_principal,i-1);
					cf.at(CF_debt_payment_interest,i) = -loan_rate * cf.at(CF_debt_balance,i);

					if (loan_rate != 0.0)
					{
						cf.at(CF_debt_payment_principal,i) = loan_rate * loan_amount/(1 - pow((1 + loan_rate),-loan_term))
							- cf.at(CF_debt_payment_interest,i);
					}
					else
					{
						cf.at(CF_debt_payment_principal,i) = loan_amount / loan_term - cf.at(CF_debt_payment_interest,i);
					}
				}
			}

			cf.at(CF_debt_payment_total,i) = cf.at(CF_debt_payment_principal,i) + cf.at(CF_debt_payment_interest,i);
			
			// compute ibi total
			cf.at(CF_ibi_total, i) =
				cf.at(CF_ibi_fed_amt,i) + cf.at(CF_ibi_sta_amt,i) + cf.at(CF_ibi_uti_amt,i) + cf.at(CF_ibi_oth_amt,i) +
				cf.at(CF_ibi_fed_per,i) + cf.at(CF_ibi_sta_per,i) + cf.at(CF_ibi_uti_per,i) + cf.at(CF_ibi_oth_per,i);

			// compute cbi total
			cf.at(CF_cbi_total, i) = cf.at(CF_cbi_fed, i) + cf.at(CF_cbi_sta, i) + cf.at(CF_cbi_uti, i) + cf.at(CF_cbi_oth, i);

			// compute pbi total		
			cf.at(CF_pbi_total, i) = cf.at(CF_pbi_fed, i) + cf.at(CF_pbi_sta, i) + cf.at(CF_pbi_uti, i) + cf.at(CF_pbi_oth, i);
			
			// compute itc total, fed&sta
			cf.at(CF_itc_fed_total, i) = cf.at(CF_itc_fed_amt,i) + cf.at(CF_itc_fed_per,i);
			cf.at(CF_itc_sta_total, i) = cf.at(CF_itc_sta_amt,i) + cf.at(CF_itc_sta_per,i);

			// compute depreciation from basis and precalculated schedule
			cf.at(CF_sta_depreciation,i) = cf.at(CF_sta_depr_sched,i)*state_depr_basis;
			cf.at(CF_fed_depreciation,i) = cf.at(CF_fed_depr_sched,i)*federal_depr_basis;

			
			// ************************************************
			// tax effect on equity (state)

			cf.at(CF_sta_incentive_income_less_deductions, i) =
				+ cf.at(CF_deductible_expenses, i) 
				+ cf.at(CF_ibi_total,i)
				+ cf.at(CF_cbi_total,i)
				+ cf.at(CF_pbi_total,i)
				- cf.at(CF_sta_depreciation,i)
				- ( is_commercial ? cf.at(CF_debt_payment_interest,i) : 0.0 );
			
			if (is_commercial && i == 1) cf.at(CF_sta_incentive_income_less_deductions, i) -= total_sales_tax;


			if (!is_commercial && is_mortgage) // interest only deductible if residential mortgage
				cf.at(CF_sta_incentive_income_less_deductions, i) -= cf.at(CF_debt_payment_interest,i);

			cf.at(CF_sta_taxable_income_less_deductions, i) = taxable_incentive_income( i, "sta" )
				+ cf.at(CF_deductible_expenses,i)
				- cf.at(CF_sta_depreciation,i)
				- ( is_commercial ? cf.at(CF_debt_payment_interest,i) : 0.0 );
			
			if (is_commercial && i == 1) cf.at(CF_sta_taxable_income_less_deductions,i) -= total_sales_tax;

			if (!is_commercial && is_mortgage) // interest only deductible if residential mortgage
				cf.at(CF_sta_taxable_income_less_deductions, i) -= cf.at(CF_debt_payment_interest,i);

			cf.at(CF_sta_tax_savings, i) = cf.at(CF_itc_sta_amt,i) + cf.at(CF_itc_sta_per,i) + cf.at(CF_ptc_sta,i)
				- state_tax_rate*cf.at(CF_sta_taxable_income_less_deductions,i);
			
			// ************************************************
			//	tax effect on equity (federal)

			cf.at(CF_fed_incentive_income_less_deductions, i) =
				+ cf.at(CF_deductible_expenses, i)
				+ cf.at(CF_ibi_total,i)
				+ cf.at(CF_cbi_total,i)
				+ cf.at(CF_pbi_total,i)
				- cf.at(CF_fed_depreciation,i)
				- ( is_commercial ? cf.at(CF_debt_payment_interest,i) : 0.0 )
				+ cf.at(CF_sta_tax_savings, i);
			
			if (is_commercial && i == 1) cf.at(CF_fed_incentive_income_less_deductions, i) -= total_sales_tax;
			
			if (!is_commercial && is_mortgage) // interest only deductible if residential mortgage
				cf.at(CF_fed_incentive_income_less_deductions, i) -= cf.at(CF_debt_payment_interest,i);

			cf.at(CF_fed_taxable_income_less_deductions, i) = taxable_incentive_income( i, "fed" )
				+ cf.at(CF_deductible_expenses,i)
				- cf.at(CF_fed_depreciation,i)
				- ( is_commercial ? cf.at(CF_debt_payment_interest,i) : 0.0 )
				+ cf.at(CF_sta_tax_savings, i);

			if (is_commercial && i == 1) cf.at(CF_fed_taxable_income_less_deductions, i) -= total_sales_tax;

			if (!is_commercial && is_mortgage) // interest only deductible if residential mortgage
				cf.at(CF_fed_taxable_income_less_deductions, i) -= cf.at(CF_debt_payment_interest,i);
			
			cf.at(CF_fed_tax_savings, i) = cf.at(CF_itc_fed_amt,i) + cf.at(CF_itc_fed_per,i) + cf.at(CF_ptc_fed,i)
				- federal_tax_rate*cf.at(CF_fed_taxable_income_less_deductions,i);

			
			// ************************************************
			// combined tax savings and cost/cash flows
				
			cf.at(CF_sta_and_fed_tax_savings,i) = cf.at(CF_sta_tax_savings, i)+cf.at(CF_fed_tax_savings, i);

			cf.at(CF_after_tax_net_equity_cost_flow, i) =
				+ (is_commercial ? cf.at(CF_deductible_expenses, i) : -cf.at(CF_operating_expenses,i) )
				- cf.at(CF_debt_payment_total, i)
				+ cf.at(CF_pbi_total, i)
				+ cf.at(CF_sta_and_fed_tax_savings,i);

			cf.at(CF_after_tax_cash_flow,i) = 
				cf.at(CF_after_tax_net_equity_cost_flow, i)
				+ (is_commercial?(1.0 - effective_tax_rate):1.0)*cf.at(CF_energy_value, i);
	
			cf.at(CF_payback_with_expenses,i) =
				cf.at(CF_after_tax_cash_flow,i)
				+ cf.at(CF_debt_payment_interest,i) * (1-effective_tax_rate)
				+ cf.at(CF_debt_payment_principal,i);

			cf.at(CF_cumulative_payback_with_expenses,i) = 
				cf.at(CF_cumulative_payback_with_expenses,i-1)
				+cf.at(CF_payback_with_expenses,i);
	
			cf.at(CF_payback_without_expenses,i) =
				+ cf.at(CF_after_tax_cash_flow,i)
				+ cf.at(CF_debt_payment_interest,i) * (1.0 - effective_tax_rate)
				+ cf.at(CF_debt_payment_principal,i)
				- cf.at(CF_deductible_expenses,i)
				+ cf.at(CF_deductible_expenses,i) * effective_tax_rate;

			cf.at(CF_cumulative_payback_without_expenses,i) =
				+ cf.at(CF_cumulative_payback_without_expenses,i-1)
				+ cf.at(CF_payback_without_expenses,i);	
		}
		
		double x = npv( CF_energy_net, nyears, real_discount_rate );
		if (x == 0.0) throw general_error("lcoe real failed because energy npv is zero");
		double lcoe_real = -( cf.at(CF_after_tax_net_equity_cost_flow,0) + npv(CF_after_tax_net_equity_cost_flow, nyears, nom_discount_rate) ) * 100 / x;

		x = npv( CF_energy_net, nyears, nom_discount_rate );
		if (x == 0.0) throw general_error("lcoe nom failed because energy npv is zero");
		double lcoe_nom = -( cf.at(CF_after_tax_net_equity_cost_flow,0) + npv(CF_after_tax_net_equity_cost_flow, nyears, nom_discount_rate) ) * 100 / x;

		double net_present_value = cf.at(CF_after_tax_cash_flow, 0) + npv(CF_after_tax_cash_flow, nyears, nom_discount_rate );

		double payback = compute_payback( CF_cumulative_payback_with_expenses, CF_payback_with_expenses, nyears );

		// save outputs
		assign( "payback", var_data((ssc_number_t)payback) );
		assign( "lcoe_real", var_data((ssc_number_t)lcoe_real) );
		assign( "lcoe_nom", var_data((ssc_number_t)lcoe_nom) );
		assign( "npv",  var_data((ssc_number_t)net_present_value) );

		/*
		assign( "credit_basis_fed", var_data((ssc_number_t)federal_credit_basis ));
		assign( "credit_basis_sta", var_data((ssc_number_t)state_credit_basis ));
		assign( "depr_basis_fed", var_data((ssc_number_t)federal_depr_basis ));
		assign( "depr_basis_sta", var_data((ssc_number_t)state_depr_basis ));
		assign( "discount_nominal", var_data((ssc_number_t)(nom_discount_rate*100.0) ));		
		assign( "sales_tax_deduction", var_data((ssc_number_t)total_sales_tax ));		
		assign( "adj_installed_cost", var_data((ssc_number_t)adjusted_installed_cost ));		
		
		save_cf( CF_energy_net, nyears, "cf_energy_net" );
		save_cf( CF_energy_value, nyears, "cf_energy_value" );
		*/
		
		save_cf( CF_om_fixed_expense, nyears, "cf_om_fixed_expense" );
		save_cf( CF_om_production_expense, nyears, "cf_om_production_expense" );
		save_cf( CF_om_capacity_expense, nyears, "cf_om_capacity_expense" );
		save_cf( CF_om_fuel_expense, nyears, "cf_om_fuel_expense" );
		save_cf( CF_property_tax_expense, nyears, "cf_property_tax_expense" );
		save_cf( CF_insurance_expense, nyears, "cf_insurance_expense" );
		save_cf( CF_operating_expenses, nyears, "cf_operating_expenses" );

		save_cf( CF_deductible_expenses, nyears, "cf_deductible_expenses");
		
		save_cf( CF_debt_balance, nyears, "cf_debt_balance" );
		save_cf( CF_debt_payment_interest, nyears, "cf_debt_payment_interest" );
		save_cf( CF_debt_payment_principal, nyears, "cf_debt_payment_principal" );
		save_cf( CF_debt_payment_total, nyears, "cf_debt_payment_total" );
	
		save_cf( CF_ibi_total, nyears, "cf_ibi_total" );
		save_cf( CF_cbi_total, nyears, "cf_cbi_total" );
		save_cf( CF_pbi_total, nyears, "cf_pbi_total" );
	
		save_cf( CF_ptc_fed, nyears, "cf_ptc_fed" );
		save_cf( CF_ptc_sta, nyears, "cf_ptc_sta" );

		save_cf( CF_itc_fed_total, nyears, "cf_itc_fed_total" );
		save_cf( CF_itc_sta_total, nyears, "cf_itc_sta_total" );
	
		save_cf( CF_sta_depr_sched, nyears, "cf_sta_depr_sched" );
		save_cf( CF_sta_depreciation, nyears, "cf_sta_depreciation" );
		save_cf( CF_sta_incentive_income_less_deductions, nyears, "cf_sta_incentive_income_less_deductions" );
		save_cf( CF_sta_taxable_income_less_deductions, nyears, "cf_sta_taxable_income_less_deductions" );
		save_cf( CF_sta_tax_savings, nyears, "cf_sta_tax_savings" );
	
		save_cf( CF_fed_depr_sched, nyears, "cf_fed_depr_sched" );
		save_cf( CF_fed_depreciation, nyears, "cf_fed_depreciation" );
		save_cf( CF_fed_incentive_income_less_deductions, nyears, "cf_fed_incentive_income_less_deductions" );
		save_cf( CF_fed_taxable_income_less_deductions, nyears, "cf_fed_taxable_income_less_deductions" );
		save_cf( CF_fed_tax_savings, nyears, "cf_fed_tax_savings" );

		save_cf( CF_sta_and_fed_tax_savings, nyears, "cf_sta_and_fed_tax_savings" );
		save_cf( CF_after_tax_net_equity_cost_flow, nyears, "cf_after_tax_net_equity_cost_flow" );
		save_cf( CF_after_tax_cash_flow, nyears, "cf_after_tax_cash_flow" );

		save_cf( CF_payback_with_expenses, nyears, "cf_payback_with_expenses" );
		save_cf( CF_cumulative_payback_with_expenses, nyears, "cf_cumulative_payback_with_expenses" );
	
		save_cf( CF_payback_without_expenses, nyears, "cf_payback_without_expenses" );
		save_cf( CF_cumulative_payback_without_expenses, nyears, "cf_cumulative_payback_without_expenses" );

	}

/* These functions can be placed in common financial library with matrix and constants passed? */

	void save_cf(int cf_line, int nyears, const std::string &name)
	{
		ssc_number_t *arrp = allocate( name, nyears+1 );
		for (int i=0;i<=nyears;i++)
			arrp[i] = (ssc_number_t)cf.at(cf_line, i);
	}

	double compute_payback( int cf_cpb, int cf_pb, int nyears )
	{	
		double dPayback = 1e99; // report as > analysis period
		bool bolPayback = false;
		int iPayback = 0;
		int i = 1; 
		while ((i<=nyears) && (!bolPayback))
		{
			if (cf.at(cf_cpb,i) > 0)
			{
				bolPayback = true;
				iPayback = i;
			}
			i++;
		}

		if (bolPayback)
		{
			dPayback = iPayback;
			if (cf.at(cf_pb, iPayback) != 0.0)
				dPayback -= cf.at(cf_cpb,iPayback) / cf.at(cf_pb,iPayback);
		}

		return dPayback;
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
	
	void depreciation_sched_macrs_half_year( int cf_line, int nyears )
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

	void depreciation_sched_straight_line( int cf_line, int nyears, int slyears )
	{
		double depr_per_year = (slyears!=0) ? 1.0 / ((double)slyears) : 0;
		for (int i=1; i<=nyears; i++)
			cf.at(cf_line, i) = (i<=slyears) ? depr_per_year : 0.0;
	}
	
	void depreciation_sched_custom( int cf_line, int nyears, ssc_number_t *custp, int custp_len )
	{
		if (custp_len < 2)
		{
			if (custp[0] > 100.0)
			{
				cf.at(cf_line, 1) = 1.0;
			}
			else
			{
				double d = custp[0];
				if ( nyears * d < 100.0 )
					d = 100.0 / nyears;

				double totpercent = 0.0;
				for (int i=1;i<=nyears;i++)
				{
					totpercent += d;
					if (totpercent > 100.0)
					{
						cf.at(cf_line,i) = (100.0 - totpercent - d)/100.0;
						break;
					}
					cf.at(cf_line, i) = d / 100.0;
				}
			}
		}
		else
		{
			double totpercent = 0;
			double scalef = 1.0;

			for (int i=0; i<custp_len; i++)
				totpercent += custp[i];

			if (totpercent < 100.0 && totpercent > 0.0)
				scalef = 100.0 / totpercent;

			totpercent = 0;
			for (int i=1;i<=nyears && i-1 < custp_len;i++)
			{
				totpercent += scalef * custp[i-1];
				if (totpercent > 100.0)
				{
					cf.at(cf_line, i) = (100.0 - totpercent - scalef*custp[i-1])/100.0;
					break;
				}
				cf.at(cf_line, i) = scalef*custp[i-1]/100.0;
			}
		}
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

	double min( double a, double b )
	{
		return (a < b) ? a : b;
	}

};

DEFINE_MODULE_ENTRY( cashloan, "Residential/Commerical Finance model.", 1 );
