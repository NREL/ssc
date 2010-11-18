#include "core.h"
#include "lib_financial.h"

static var_info vtab_cashloan[] = {

	{ SSC_INPUT,        SSC_ARRAY,		 "financial_mode",                         "Financial mode",             "",    "",                      "Cashloan",      "*",                       "",                                         "" },
	
	{ SSC_INPUT,        SSC_NUMBER,       "annual_fuel_usage",                         "Fuel usage",             "kWht",    "",                      "Cashloan",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "energy_value",                         "Energy value",             "$",    "",                      "Cashloan",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "energy_net",                         "Net energy",             "kWh",    "",                      "Cashloan",      "*",                       "",                                         "" },

	/* standard financial outputs */
	{ SSC_OUTPUT,        SSC_NUMBER,       "lcoe_real",                         "Real LCOE",             "cents/kWh",    "",                      "Cashloan",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,       "lcoe_nominal",                         "Nominal LCOE",             "cents/kWh",    "",                      "Cashloan",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,       "payback",                         "Payback",             "years",    "",                      "Cashloan",      "*",                       "",                                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,       "npv",                         "Net present value",             "$",    "",                      "Cashloan",      "*",                       "",                                         "" },


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
	CF_deductable_expenses_total,

	CF_debt_payment_interest,
	CF_debt_payment_principal,
	CF_debt_payment_total,

	CF_ibi_federal,
	CF_ibi_state,
	CF_ibi_utility,
	CF_ibi_other,
	CF_ibi_total,
	
	CF_cbi_federal,
	CF_cbi_state,
	CF_cbi_utility,
	CF_cbi_other,
	CF_cbi_total,

	CF_pbi_federal,
	CF_pbi_state,
	CF_pbi_utility,
	CF_pbi_other,
	CF_pbi_total,

	CF_state_depr_sched,
	CF_state_depreciation,
	CF_state_total_income,
	CF_state_incentive_income_less_deductions,
	CF_state_total_taxable_incentive_income_less_deductions,
	CF_state_ptc,
	CF_state_itc,
	CF_state_tax_savings,
	
	CF_federal_depr_sched,
	CF_federal_depreciation,
	CF_federal_state_tax_liability,
	CF_federal_incentive_income_less_deductions,
	CF_federal_total_taxable_incentive_income_less_deductions,
	CF_federal_income_taxes,
	CF_federal_ptc,
	CF_federal_itc,
	CF_federal_tax_savings,

	CF_state_and_federal_tax_savings,
	CF_after_tax_net_equity_cost_flow,
	CF_after_tax_cash_flow,


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

	bool exec( ) throw( general_error )
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
		double discount_real = as_double("real_discount_rate")*0.01;
		double property_tax = as_double("property_tax")*0.01;
		double insurance_rate = as_double("insurance_rate")*0.01;
		double salvage_frac = as_double("salvage_percentage")*0.01;

		double direct_cost = as_double("total_direct_cost");
		double indirect_cost = as_double("total_indirect_cost");
		double total_cost = direct_cost + indirect_cost;

		int loan_term = as_integer("loan_term");
		double loan_rate = as_double("loan_rate")*0.01;
		double debt_frac = as_double("debt_percentage")*0.01;
		double loan_amount = debt_frac * total_cost;
		
		// precompute expenses from annual schedules or value+escalation
		escal_or_annual( CF_om_fixed_expense, nyears, "om_fixed", inflation_rate, 1.0, false, as_double("om_fixed_escal") );
		escal_or_annual( CF_om_production_expense, nyears, "om_production", inflation_rate, 0.001, false, as_double("om_production_escal") );  
		escal_or_annual( CF_om_capacity_expense, nyears, "om_capacity", inflation_rate, 1.0, false, as_double("om_capacity_escal") );  
		escal_or_annual( CF_om_fuel_expense, nyears, "om_fuel_cost", inflation_rate, as_double("system_heat_rate")*0.001, false, as_double("om_fuel_cost_escal") );
		
		for (i=1; i<=nyears; i++)
		{
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

			if ( i == nyears ) /* salvage value handled as negative operating expense in last year */
				cf.at(CF_operating_expenses_total,i) -= total_cost * salvage_frac * pow( 1+inflation_rate, i-1 );

			// Total Deductable Expenses
			cf.at(CF_deductable_expenses_total,i) = 0 - cf.at(CF_operating_expenses_total,i);


		// Debt Interest Payment   = InterestPayment
	

		// Debt Repayment  = PrincipalPayment
			  if (i == 1)
			  {
				cf.at(CF_debt_payment_total,i) = 0 - loan_amount;
				cf.at(CF_debt_payment_interest,i) = - cf.at(CF_debt_payment_total,i) * loan_rate;
				cf.at(CF_debt_payment_principal,i) =
						(-ppmt(loan_rate, //Rate
						i, // Period
						loan_term, // Number periods
						loan_amount, // Present Value
						0, // future Value
						0)); // cash flow at end of period
			  }
			  else
			  {
				if (i <= loan_term) 
				{
				  cf.at(CF_debt_payment_total,i) = cf.at(CF_debt_payment_total,i-1) + cf.at(CF_debt_payment_principal,i-1);
				  cf.at(CF_debt_payment_interest,i) = -cf.at(CF_debt_payment_total,i)* loan_rate;
				  if (loan_rate != 0)
				  {
					  cf.at(CF_debt_payment_principal,i) = loan_rate * loan_amount	
						  /(1 - pow((1 + loan_rate),-loan_term)) -
						  cf.at(CF_debt_payment_interest,i);
				  }
				  else  // L"Hopital applied to above equation or common sense!!
				  {
					cf.at(CF_debt_payment_principal,i) = loan_amount / loan_term
					  -  cf.at(CF_debt_payment_interest,i);
				  }
				}
			  }
			// Total Debt Payment
			  cf.at(CF_debt_payment_total,i) = cf.at(CF_debt_payment_principal,i) +
				cf.at(CF_debt_payment_interest,i);



/*-------------------------------------------TODO----------------------------------------------*/
			  // initialize incentives
			  // initialize depreciation
			  // npv and payback calls - need std::vector



			// Tax Effect on Equity (State)
			// Deductable Expenses  = Total Deductible Expenses
			// Investment Based Incentives (IBI) - in initialization
			// Capacity Based Incentives (CBI) - in initialization
			// Performance Based Incentives (PBI)
			  cf.at(CF_pbi_federal,i) = cf.at(cfFederalPBI_it0,i) * cf.at(CF_energy_net,i);
			  cf.at(CF_pbi_state,i) = cf.at(cfStatePBI_it0,i) * cf.at(CF_energy_net,i);
			  cf.at(CF_pbi_utility,i) = cf.at(cfUtilityPBI_it0,i) * cf.at(CF_energy_net,i);
			  cf.at(CF_pbi_other,i) = cf.at(cfOtherPBI_it0,i) * cf.at(CF_energy_net,i);
			  cf.at(CF_pbi_total,i) = cf.at(CF_pbi_federal,i) + cf.at(CF_pbi_state,i) + cf.at(CF_pbi_utility,i) + cf.at(CF_pbi_other,i);

			  // Sales Tax in initialize

			  // State Total Incentive Income - Deductions
			  cf.at(CF_state_total_income,i) =
				  cf.at(CF_deductable_expenses_total,i)
				+ cf.at(CF_ibi_total,i)
				+ cf.at(CF_cbi_total,i)
				+ cf.at(CF_pbi_total,i)
				- cf.at(cfStateDepreciation,i)
				- cf.at(CF_debt_payment_interest,i);

			  if (i == 1) cf.at(CF_state_total_income,i) -= sv[svTotalSalesTax];

			  // State Total Taxable Incentive Income - Deductions
			  cf.at(cfStateTotalTaxableIncentiveIncomeMinusDeductions,i) = StateTaxableIncentiveIncome(intYear) 
				  + cf.at(CF_deductable_expenses_total,i)
				  - cf.at(cfStateDepreciation,i)
				  - cf.at(cfDebtInterestPayment,i);

			  if (intYear == 1) cf.at(cfStateTotalTaxableIncentiveIncomeMinusDeductions,i) -= sv[svTotalSalesTax];

			// 4/24/07 - interest only deductible if residential mortgage
			  if (sv[svMortgage] > 0) cf.at(cfStateTotalTaxableIncentiveIncomeMinusDeductions,i) -= cf.at(cfDebtInterestPayment,i);


			  // State Income Tax
			  cf.at(cfStateIncomeTaxes,i) = sv[svStateTax] * cf.at(cfStateTotalTaxableIncentiveIncomeMinusDeductions,i);

			  // PTC
			  cf.at(cfStatePTC,i) = cf.at(cfStatePTC_it0,i) * cf.at(CF_energy_net,i);

			  // ITC - in initialization
			  // State Tax Savings (Liability)
			  cf.at(cfStateTaxSavings,i) = 0;
			  if ( (ia[iaChkIncITCStateAmt] || ia[iaChkIncITCStateMax]) ) cf.at(cfStateTaxSavings,i) += cf.at(cfStateITCAmount,i);
			  if (ia[iaChkIncPTCState])  cf.at(cfStateTaxSavings,i) += cf.at(cfStatePTC,i);

			  cf.at(cfStateTaxSavings,i) -=  cf.at(cfStateIncomeTaxes,i);

			// Tax Effect on Equity (Federal)
			  // Sales Tax in initialize

			  // Federal Total Incentive Income - Deductions
			  cf.at(cfFederalTotalIncentiveIncomeMinusDeductions,i) =
					cf.at(CF_deductable_expenses_total,i)
					+ cf.at(CF_ibi_total,i)
					+ cf.at(CF_cbi_total,i)
					+ cf.at(CF_pbi_total,i)
				  - cf.at(CF_federal_depreciation,i)
				  - cf.at(cfDebtInterestPayment,i)
				  + cf.at(cfStateTaxSavings,i);

			  if (i == 1) cf.at(cfFederalTotalIncentiveIncomeMinusDeductions,i) -= sv[svTotalSalesTax];



			  // Federal Total Taxable Incentive Income - Deductions
			  cf.at(cfFederalTotalTaxableIncentiveIncomeMinusDeductions,i) = FederalTaxableIncentiveIncome(intYear)
				  + cf.at(CF_deductable_expenses_total,i)
				  - cf.at(cfFederalDepreciation,i)
				  - cf.at(cfDebtInterestPayment,i)
				  + cf.at(cfStateTaxSavings,i);

			  if (i == 1) cf.at(cfFederalTotalTaxableIncentiveIncomeMinusDeductions,i) -= sv[svTotalSalesTax];




			  // Federal Income Tax
			  cf.at(cfFederalIncomeTaxes,i) = sv[svFederalTax] *   cf.at(cfFederalTotalTaxableIncentiveIncomeMinusDeductions,i);


			  // Federal PTC
			  cf.at(cfFederalPTC,i) = cf.at(cfFederalPTC_it0,i) * cf.at(CF_energy_net,i);

			  // ITC - in initialization
			  // Federal Tax Savings (Liability)
			  cf.at(cfFederalTaxSavings,i) = 0;
			  if ( (ia[iaChkIncITCFederalAmt] || ia[iaChkIncITCFederalMax]))  cf.at(cfFederalTaxSavings,i) += cf.at(cfFederalITCAmount,i);
			  if (ia[iaChkIncPTCFederal]) cf.at(cfFederalTaxSavings,i) += cf.at(cfFederalPTC,i);

			  cf.at(cfFederalTaxSavings,i) -= cf.at(cfFederalIncomeTaxes,i);

			//State and Federal Tax Savings (Liability)
			  cf.at(cfStateAndFederalTaxSavings,i) = cf.at(cfStateTaxSavings,i)
				+ cf.at(cfFederalTaxSavings,i);

			//After Tax Net Equity Cost Flow
			  cf.at(cfAfterTaxNetEquityCostFlow,i) = 0
				- cf.at(cfTotalOperatingCost,i)
				- cf.at(cfTotalDebtPayment,i)
				+ cf.at(CF_pbi_federal,i)
				+ cf.at(CF_pbi_state,i)
				+ cf.at(CF_pbi_utility,i)
				+ cf.at(CF_pbi_other,i)
				+ cf.at(cfStateAndFederalTaxSavings,i);
			//After Tax Cash Flow
			  cf.at(cfAfterTaxCashFlow,i) = 0
				+ cf.at(cfAfterTaxNetEquityCostFlow,i)
				+ (1.0 - sv[svEffectiveTaxRate]) * cf.at(cfOffsetPayments,i);

			// Payback including expenses
			  cf.at(cfPaybackCashFlowWExpenses,i) = 0
				+ cf.at(cfAfterTaxCashFlow,i)
				+ cf.at(cfDebtInterestPayment,i)
				* (1.0 - sv[svEffectiveTaxRate])
				+ cf.at(cfDebtRepayment,i);

			  cf.at(cfCumulativePaybackCashFlowWExpenses,i) = 0
				+ cf.at(cfCumulativePaybackCashFlowWExpenses,i-1)
				+ cf.at(cfPaybackCashFlowWExpenses,i);

			// Payback excluding expenses
			  cf.at(cfPaybackCashFlowWOExpenses,i) = 0
				+ cf.at(cfAfterTaxCashFlow,i)
				+ cf.at(CF_debt_payment_interest,i)
				* (1.0 - sv[svEffectiveTaxRate])
				+ cf.at(cfDebtRepayment,i)
				+ cf.at(cfTotalOperatingCost,i)
				+ cf.at(cfTotalDeductableExpenses,i)
				* sv[svEffectiveTaxRate];

			  cf.at(cfCumulativePaybackCashFlowWOExpenses,i) = 0
				+ cf.at(cfCumulativePaybackCashFlowWOExpenses,i-1)
				+ cf.at(cfPaybackCashFlowWOExpenses,i);




		}




		/* will be used for all fin models */
		double discount_nominal = (1+discount_real)*(1+inflation_rate)-1;
		compute_cashflow(nyears, discount_nominal);



		return false;
	}



/* These functions can be placed in common financial library with matrix and constants passed? */
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


	void compute_cashflow(int nyears, double nominal_discount)
	{
	  double lcoe=0;
	  // need to pass std::vector cf.at(CF_after_tax_net_equity_cost_flow,0) to cf.at(CF_after_tax_net_equity_cost_flow,nyears)
	  double npv_cost = npv(nominal_discount, cf.at(CF_after_tax_net_equity_cost_flow,0), nyears);


	  double x = 0;

	  x = npv(nominal_discount, cf.at(CF_energy_net], nyears);
	  if (x != 0)
		lcoe = 0 - (cf.at(CF_after_tax_net_equity_cost_flow,0) + npv_cost) * 100 / x;
	  else
	  {
//		  Messages.Add("LCOE real failed");
		  lcoe = 0;
	  }

	  assign( "lcoe_real", var_data( lcoe ) );


	  x = npv(nominal_discount, cf.at(CF_energy_value), nyears);
	  if (x != 0)
		lcoe = 0 - (cf.at(CF_after_tax_net_equity_cost_flow][0] + npv_cost) * 100 / x;
	  else
	  {
//		  Messages.Add("LCOE nom failed");
		  lcoe = 0;
	  }

	  assign( "lcoe_nominal", var_data( lcoe ) );

	  double npv_cash = cf.at(CF_after_tax_cash_flow,0) + npv(nominal_discount, cf.at(CF_after_tax_cash_flow], nyears);
	  assign( "npv", var_data( npv_cash ) );

	// Payback - 2/5/07
	  double payback_expenses = payback(cf.at(cfCumulativePaybackCashFlowWExpenses], cf.at(cfPaybackCashFlowWExpenses], nyears);
	  assign( "payback", var_data( payback_expenses ) );

	}



};



DEFINE_MODULE_ENTRY( cashloan, "Residential/Commerical Cash or Loan Finance model.", 1 );


