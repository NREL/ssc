#include "core.h"

static var_info _cm_vtab_levpartflip[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_ARRAY,      "system_enet",				"Annual energy produced by system",	"kWh",   "",                      "",             "*",						   "",                              "" },
/* constraint is > 0 */
	{ SSC_INPUT,        SSC_NUMBER,     "system_nameplate",			"System nameplate capacity",		"kW",    "",                      "",             "*",						   "MIN=1",                         "" },
/* want analysis years determined by enet length - system degradation, availablity compute module output
	{ SSC_INPUT,        SSC_NUMBER,     "analysis_years",           "Number of years in analysis",     "years",  "",                      "",             "?=30",                      "INTEGER,MIN=1,MAX=40",          "" }, */
	{ SSC_INPUT,        SSC_NUMBER,     "inflation",			    "Inflation rate",					"%",     "",                      "",             "*",						   "MIN=0,MAX=100",					"" },
	{ SSC_INPUT,        SSC_NUMBER,     "discount_real",			"Real discount rate",				"%",     "",                      "",             "*",						   "MIN=0,MAX=100",					"" },
	{ SSC_INPUT,        SSC_NUMBER,     "sales_tax_rate",		    "Sales tax rate",	    			"%",     "",                      "",             "*",						   "MIN=0,MAX=100",					"" },

/* costs - to be updated based on meetings with DHF */
	{ SSC_INPUT,        SSC_NUMBER,     "cost_gen_equip",           "Generation equiptment cost",		"$",	 "",					  "",             "?=24000000",              "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "cost_bop",					"Balance of plant cost",			"$",	 "",					  "",             "?=8000000",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "cost_network",             "Network upgrade cost",				"$",	 "",					  "",             "?=3500000",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "percent_contingency",      "Contingency percent",				"%",	 "",					  "",             "?=1",                       "MIN=0,MAX=100",		        	"" },

	{ SSC_INPUT,        SSC_NUMBER,     "cost_developer",           "Developer cost & fees",		    "$",	 "",					  "",             "?=2000000",               "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "cost_land_improve",        "Land improvements cost",		    "$",	 "",					  "",             "?=200000",                 "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "cost_other",               "Other cost",						"$",	 "",					  "",             "?=75000",                  "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,     "percent_taxable",          "Taxable cost",				        "%",	 "",					  "",             "?=100",                     "MIN=0,MAX=100",      			"" },


/* o and m */

/* market specific inputs */

/* intermediate outputs */
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_contingency",        "Contingency cost",                 "$",     "",					   "",			   "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_hard",               "Hard cost",                        "$",     "",					   "",			   "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_soft",               "Soft cost",                        "$",     "",					   "",			   "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_salestax",           "Sales tax",                        "$",     "",					   "",			   "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_installed",          "Installed cost",                   "$",     "",					   "",			   "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "cost_installedperwatt",   "Installed cost",                   "$/W",   "",					   "",			   "*",                         "",                             "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "discount_nominal",        "Nominal discount rate",            "%",     "",					   "",			   "*",                         "",                             "" },

var_info_invalid };



class cm_levpartflip : public compute_module
{
private:
public:
	cm_levpartflip()
	{
		add_var_info( _cm_vtab_levpartflip );
	}

	void exec( ) throw( general_error )
	{
		double inf = as_double("inflation") / 100.0;
		double disc_real = as_double("discount_real") / 100.0;
		assign( "discount_nominal", var_data( (1+inf)*(1+disc_real)-1 ) );

		double gen = as_double("cost_gen_equip");
		double bop = as_double("cost_bop");
		double net = as_double("cost_network");
		double cont = as_double("percent_contingency") / 100.0;

		double cost_hard = gen + bop + net;
		double cost_cont = cost_hard * cont;

		double dev = as_double("cost_developer");
		double land = as_double("cost_land_improve");
		double other = as_double("cost_other");
		double cost_taxable = as_double("percent_taxable") / 100.0;
		double sales_tax_rate = as_double("sales_tax_rate") / 100.0;

		double cost_soft = dev + land + other;
		double cost_salestax = (cost_hard + cost_cont + cost_soft) * cost_taxable * sales_tax_rate;
		assign( "cost_contingency", var_data( cost_cont ) );
		assign( "cost_hard", var_data( cost_hard + cost_cont) );
		assign( "cost_salestax", var_data( cost_salestax ) );
		assign( "cost_soft", var_data( cost_soft + cost_salestax ) );

		double cost_installed = cost_soft + cost_salestax + cost_hard + cost_cont;
		assign( "cost_installed", var_data( cost_installed ) );

		double nameplate = as_double("system_nameplate");
		assign( "cost_installedperwatt", var_data( cost_installed / nameplate / 1000.0 ) );
	}
};

DEFINE_MODULE_ENTRY( levpartflip, "DHF Leveraged Partnership Flip Financial Model_", 1 );


