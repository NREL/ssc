#include "core.h"

static var_info vtab_cashloan[] = {

	{ SSC_INPUT,        SSC_ARRAY,		 "financial_mode",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },
	
	{ SSC_INPUT,        SSC_ARRAY,		 "loan_rate",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "loan_term",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "loan_debt",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },

	{ SSC_INPUT,        SSC_ARRAY,       "annual_revenue",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "annual_expense",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },

var_info_invalid };

extern var_info
	vtab_standard_financial[],
	vtab_oandm[],
	vtab_depreciation[],
	vtab_utility_rate[],
	vtab_tax_credits[],
	vtab_payment_incentives[];

class cm_cashloan : public compute_module
{
private:
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
		return false;
	}
};

DEFINE_MODULE_ENTRY( cashloan, "Residential/Commerical Cash or Loan Finance model.", 1 );


