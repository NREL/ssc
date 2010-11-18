#include "core.h"
#include "lib_financial.h"

static var_info vtab_cashloan[] = {
/*   VARTYPE           DATATYPE          NAME                        LABEL                                  UNITS         META                      GROUP            REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,        SSC_NUMBER,		 "market",                   "Financial mode",                     "0/1",          "0=residential,1=comm.", "Cashloan",      "?=1",                     "INTEGER,MIN=0,MAX=1",            "" },
	{ SSC_INPUT,        SSC_NUMBER,		 "deduct_interest",          "Deduct loan interest payments",      "0/1",          "",                      "Cashloan",      "?=0",                     "BOOLEAN",                        "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "annual_fuel_usage",        "Fuel usage",                         "kWht",         "",                      "Cashloan",      "?=0",                     "MIN=0",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "energy_value",             "Energy value",                       "$",            "",                      "Cashloan",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "energy_net",               "Net energy",                         "kWh",          "",                      "Cashloan",      "*",                       "",                                         "" },

	/* standard financial outputs */
	{ SSC_OUTPUT,        SSC_NUMBER,     "lcoe_real",                "Real LCOE",                          "cents/kWh",    "",                      "Cashloan",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "lcoe_nominal",             "Nominal LCOE",                       "cents/kWh",    "",                      "Cashloan",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "payback",                  "Payback",                            "years",        "",                      "Cashloan",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,     "npv",                      "Net present value",                  "$",            "",                      "Cashloan",      "*",                       "",                                         "" },
	
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
	CF_operating_expenses_total,

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
	CF_itc_sta_amt,
	CF_itc_sta_per,
	
	CF_state_depr_sched,
	CF_state_depreciation,
	CF_state_total_income,
	CF_state_total_taxable_incentive_income_less_deductions,
	CF_state_tax_savings,
	
	CF_federal_depr_sched,
	CF_federal_depreciation,
	CF_federal_total_income,
	CF_federal_total_taxable_incentive_income_less_deductions,
	CF_federal_tax_savings,

	CF_state_and_federal_tax_savings,
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
		add_var_info( vtab_utility_rate );
		add_var_info( vtab_tax_credits );
		add_var_info( vtab_payment_incentives );
				
		add_var_info( vtab_cashloan );
	}

	void exec( ) throw( general_error )
	{
		int i;

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
		double property_tax = as_double("property_tax")*0.01;
		double insurance_rate = as_double("insurance_rate")*0.01;
		double salvage_frac = as_double("salvage_percentage")*0.01;
		double federal_tax_rate = as_double("federal_tax_rate")*0.01;
		double state_tax_rate = as_double("state_tax_rate")*0.01;
		double effective_tax_rate = federal_tax_rate + (1-federal_tax_rate)*state_tax_rate;
		
		double real_discount_rate = as_double("real_discount_rate");
		double nom_discount_rate = (1.0 + real_discount_rate) * (1.0 + inflation_rate) - 1.0;

		double direct_cost = as_double("total_direct_cost");
		double indirect_cost = as_double("total_indirect_cost");
		double total_cost = direct_cost + indirect_cost;
		double total_sales_tax = as_double("total_sales_tax");

		int loan_term = as_integer("loan_term");
		double loan_rate = as_double("loan_rate")*0.01;
		double debt_frac = as_double("debt_percentage")*0.01;
		double loan_amount = debt_frac * total_cost;

		
		// precompute expenses from annual schedules or value+escalation
		escal_or_annual( CF_om_fixed_expense, nyears, "om_fixed", inflation_rate, 1.0, false, as_double("om_fixed_escal") );
		escal_or_annual( CF_om_production_expense, nyears, "om_production", inflation_rate, 0.001, false, as_double("om_production_escal") );  
		escal_or_annual( CF_om_capacity_expense, nyears, "om_capacity", inflation_rate, 1.0, false, as_double("om_capacity_escal") );  
		escal_or_annual( CF_om_fuel_expense, nyears, "om_fuel_cost", inflation_rate, as_double("system_heat_rate")*0.001, false, as_double("om_fuel_cost_escal") );
		
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


		switch( as_integer("depr_state_basis") )
		{
		case 1: depreciation_sched_macrs_half_year( CF_state_depr_sched, nyears ); break;
		case 2: depreciation_sched_straight_line( CF_state_depr_sched, nyears, as_integer("depr_sta_sl_years") ); break;
		case 3: 
			{
				size_t arr_len;
				ssc_number_t *arr_cust = as_array( "depr_sta_custom", &arr_len );
				depreciation_sched_custom( CF_state_depr_sched, nyears, arr_cust, arr_len );
				break;
			}
		}

		switch( as_integer("depr_federal_basis") )
		{
		case 1: depreciation_sched_macrs_half_year( CF_federal_depr_sched, nyears ); break;
		case 2: depreciation_sched_straight_line( CF_federal_depr_sched, nyears, as_integer("depr_fed_sl_years") ); break;
		case 3: 
			{
				size_t arr_len;
				ssc_number_t *arr_cust = as_array( "depr_fed_custom", &arr_len );
				depreciation_sched_custom( CF_federal_depr_sched, nyears, arr_cust, arr_len );
				break;
			}
		}



		for (i=1; i<=nyears; i++)
		{			
			// compute expenses
			cf.at(CF_om_production_expense,i) *= cf.at(CF_energy_net,i);
			cf.at(CF_om_capacity_expense,i) *= nameplate;
			cf.at(CF_om_fuel_expense,i) *= year1_fuel_use;
			cf.at(CF_property_tax_expense,i) =  total_cost * property_tax * pow( 1 + inflation_rate, i-1 );
			cf.at(CF_insurance_expense,i) = total_cost * insurance_rate * pow( 1 + inflation_rate, i-1 );

			cf.at(CF_operating_expenses_total,i) = cf.at(CF_om_production_expense,i)
				+ cf.at(CF_om_capacity_expense,i)
				+ cf.at(CF_om_fuel_expense,i)
				+ cf.at(CF_property_tax_expense,i)
				+ cf.at(CF_insurance_expense,i);

			if (i == nyears) /* salvage value handled as negative operating expense in last year */
				cf.at(CF_operating_expenses_total,i) -= total_cost * salvage_frac * pow( 1+inflation_rate, i-1 );

			if (i == 1)
			{
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
					cf.at(CF_debt_payment_interest,i) = -loan_rate * 
						(cf.at(CF_debt_payment_total,i-1) + cf.at(CF_debt_payment_principal,i-1));

					if (loan_rate != 0.0)
					{
						cf.at(CF_debt_payment_principal,i) = loan_rate * loan_amount/(1 - pow((1 + loan_rate),-loan_term))
							- cf.at(CF_debt_payment_interest,i);
					}
					else
					{
						cf.at(CF_debt_payment_principal,i) = loan_amount / loan_term
						-  cf.at(CF_debt_payment_interest,i);
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
			
			// compute depreciation from basis and precalculated schedule
			cf.at(CF_state_depreciation,i) = cf.at(CF_state_depr_sched,i)*state_depr_basis;
			cf.at(CF_federal_depreciation,i) = cf.at(CF_federal_depr_sched,i)*federal_depr_basis;


			// tax effect on equity (state)
			cf.at(CF_state_total_income, i) =
				- cf.at(CF_operating_expenses_total, i) 
				+ cf.at(CF_ibi_total,i)
				+ cf.at(CF_cbi_total,i)
				+ cf.at(CF_pbi_total,i)
				- cf.at(CF_state_depreciation,i)
				- cf.at(CF_debt_payment_interest,i);
			
			if (i == 1) cf.at(CF_state_total_income, i) -= total_sales_tax;

			cf.at(CF_state_total_taxable_incentive_income_less_deductions, i) = taxable_incentive_income( i, "sta" )
				- cf.at(CF_operating_expenses_total,i)
				- cf.at(CF_state_depreciation,i)
				- cf.at(CF_debt_payment_interest,i);

			if (i == 1) cf.at(CF_state_total_taxable_incentive_income_less_deductions,i) -= total_sales_tax;

			if (as_boolean("deduct_interest") == 0) cf.at(CF_state_total_taxable_incentive_income_less_deductions,i) -= cf.at(CF_debt_payment_interest,i);

			cf.at(CF_state_tax_savings, i) = cf.at(CF_itc_sta_amt,i) + cf.at(CF_itc_sta_per,i) + cf.at(CF_ptc_sta,i)
				- state_tax_rate*cf.at(CF_state_total_taxable_incentive_income_less_deductions,i);
			
			// tax effect on equity (federal)

			cf.at(CF_federal_total_income, i) =
				- cf.at(CF_operating_expenses_total, i)
				+ cf.at(CF_ibi_total,i)
				+ cf.at(CF_cbi_total,i)
				+ cf.at(CF_pbi_total,i)
				- cf.at(CF_federal_depreciation,i)
				- cf.at(CF_debt_payment_interest,i)
				+ cf.at(CF_state_tax_savings, i);
			
			if (i == 1) cf.at(CF_federal_total_income, i) -= total_sales_tax;

			cf.at(CF_federal_total_taxable_incentive_income_less_deductions, i) = taxable_incentive_income( i, "fed" )
				- cf.at(CF_operating_expenses_total,i)
				- cf.at(CF_federal_depreciation,i)
				- cf.at(CF_debt_payment_interest,i)
				+ cf.at(CF_state_tax_savings, i);

			if (i == 1) cf.at(CF_federal_total_taxable_incentive_income_less_deductions, i) -= total_sales_tax;
			
			cf.at(CF_federal_tax_savings, i) = cf.at(CF_itc_fed_amt,i) + cf.at(CF_itc_fed_per,i) + cf.at(CF_ptc_fed,i)
				- federal_tax_rate*cf.at(CF_federal_total_taxable_incentive_income_less_deductions,i);

				
			cf.at(CF_state_and_federal_tax_savings,i) = cf.at(CF_state_tax_savings, i)+cf.at(CF_federal_tax_savings, i);

			cf.at(CF_after_tax_net_equity_cost_flow, i) =
				- cf.at(CF_operating_expenses_total, i)
				- cf.at(CF_debt_payment_total, i)
				+ cf.at(CF_pbi_total, i)
				+ cf.at(CF_state_and_federal_tax_savings,i);

			cf.at(CF_after_tax_cash_flow,i) = 
				cf.at(CF_after_tax_net_equity_cost_flow, i)
				+ (1.0 - effective_tax_rate)*cf.at(CF_energy_value, i);
	
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
				+ cf.at(CF_operating_expenses_total,i)
				- cf.at(CF_operating_expenses_total,i) * effective_tax_rate;

			cf.at(CF_cumulative_payback_without_expenses,i) =
				+ cf.at(CF_cumulative_payback_without_expenses,i-1)
				+ cf.at(CF_payback_without_expenses,i);	
		}

		double net_present_value = npv( CF_after_tax_net_equity_cost_flow, nyears, nom_discount_rate );

		double x = npv( CF_energy_net, nyears, real_discount_rate );
		if (x == 0.0) throw general_error("lcoe real failed because energy npv is zero");
		double lcoe_real = -( cf.at(CF_after_tax_net_equity_cost_flow,0) + net_present_value ) * 100 / x;

		x = npv( CF_energy_net, nyears, nom_discount_rate );
		if (x == 0.0) throw general_error("lcoe nom failed because energy npv is zero");
		double lcoe_nom = -( cf.at(CF_after_tax_net_equity_cost_flow,0) + net_present_value ) * 100 / x;

		net_present_value = cf.at(CF_after_tax_cash_flow, 0) + npv(CF_after_tax_cash_flow, nyears, nom_discount_rate );

		double payback = compute_payback( CF_cumulative_payback_with_expenses, CF_payback_with_expenses, nyears );
	}

/* These functions can be placed in common financial library with matrix and constants passed? */
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
					cf.at(cf_line, i+1) = arrp[0]*scale*pow( 1+escal, i );
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
