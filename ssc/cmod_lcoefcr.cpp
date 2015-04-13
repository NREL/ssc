#include "core.h"
#include "lib_financial.h"

static var_info vtab_lcoefcr[] = 
{	
/*   VARTYPE            DATATYPE         NAME                        LABEL                       UNITS     META      GROUP          REQUIRED_IF    CONSTRAINTS UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "capital_cost",             "Capital cost",             "$",      "",       "Simple LCOE", "*",           "",         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "operating_cost",           "Annual operating cost",    "$/yr",   "",       "Simple LCOE", "*",           "",         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fixed_charge_rate",        "Fixed charge rate",        "",       "",       "Simple LCOE", "*",           "",         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "annual_energy",            "Annual energy production", "kWh/yr", "",       "Simple LCOE", "*",           "",         "" },
	
	{ SSC_OUTPUT,       SSC_NUMBER,       "lcoe",                     "Levelized cost of energy", "$/kWh", "",       "Simple LCOE", "*",           "",         "" },

var_info_invalid };

// Implementation of "shortcut LCOE calculation" from Short, W. et al. (1995) 
// "A Manual for the Economic Evaluation of Energy Efficiency and Renewable Energy 
// Technologies" NREL/TP-462-4173, p. 49.
//
// This approach requires:
// 1. Constant output (no degradation)
// 2. Constant O&M (no O&M escalation)
// 3. No financing (no debt)
//
class cm_lcoefcr : public compute_module
{
private:
public:
	cm_lcoefcr()
	{
		add_var_info( vtab_lcoefcr );
	}

	void exec( ) throw( general_error )
	{
		double aep = 0; // annual output, get from performance model
		double aoe = 0; // annual operating costs
		double fcr = 1; // fixed charge rate, before tax revenues required
		double icc = 0; // initial investment, or capital cost

		aep = as_double("annual_energy");     // kWh
		aoe = as_double("operating_cost");    // $
		fcr = as_double("fixed_charge_rate"); // unitless fraction
		icc = as_double("capital_cost");      // $

		// Short (1995) Equation 4-11, p. 49
		double lcoe;
		lcoe = (fcr*icc + aoe) / aep; //$/kWh
		assign("lcoe", var_data((ssc_number_t)lcoe));

	}


};

DEFINE_MODULE_ENTRY( lcoefcr, "Calculate levelized cost of energy using fixed charge rate method.", 1 )
