#include "core.h"

static var_info vtab_cashloan[] = {

	{ SSC_INPUT,        SSC_ARRAY,		 "financial_mode",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },
	
	{ SSC_INPUT,        SSC_ARRAY,		 "loan_rate",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "loan_term",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "loan_debt",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },

	
	{ SSC_INPUT,        SSC_NUMBER,       "annual_fuel_usage",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "energy_value",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "energy_net",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },

var_info_invalid };

extern var_info
	vtab_standard_financial[],
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
		
		double nameplate = as_double("system_capacity"); // kW
		double year1_fuel_use = as_double("annual_fuel_usage"); // kWht
		
		double inflation_rate = as_double("inflation_rate")*0.01;
		double property_tax = as_double("property_tax")*0.01;
		double insurance_rate = as_double("insurance_rate")*0.01;
		double salvage_frac = as_double("salvage_percentage")*0.01;

		double direct_cost = as_double("total_direct_cost");
		double indirect_cost = as_double("total_indirect_cost");
		double total_cost = direct_cost + indirect_cost;
		
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

		}




		return false;
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
};



DEFINE_MODULE_ENTRY( cashloan, "Residential/Commerical Cash or Loan Finance model.", 1 );


