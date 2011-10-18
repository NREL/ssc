#include "core.h"

#include "6par_jacobian.h"
#include "6par_lu.h"
#include "6par_search.h"
#include "6par_newton.h"
#include "6par_gamma.h"
#include "6par_solve.h"

static double to_double( double x ) { return x; }

static var_info _cm_vtab_6parsolve[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                                UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,         SSC_STRING,      "celltype",               "Cell technology type",           "monoSi,multiSi/polySi,cis,cigs,cdte,amorphous","","6 Parameter Solver",      "*",        "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "Vmp",                    "Maximum power point voltage",    "V",       "",                      "6 Parameter Solver",      "*",                       "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "Imp",                    "Maximum power point current",    "A",       "",                      "6 Parameter Solver",      "*",                       "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "Voc",                    "Open circuit voltage",           "V",       "",                      "6 Parameter Solver",      "*",                       "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "Isc",                    "Short circuit current",          "A",       "",                      "6 Parameter Solver",      "*",                       "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "alpha",                  "Temp coeff of current at SC",    "A/'C",    "",                      "6 Parameter Solver",      "*",                       "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "beta",                   "Temp coeff of voltage at OC",    "V/'C",    "",                      "6 Parameter Solver",      "*",                       "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "gamma",                  "Temp coeff of power at MP",      "%/'C",    "",                      "6 Parameter Solver",      "*",                       "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "Nser",                   "Number of cells in series",      "",        "",                      "6 Parameter Solver",      "*",                       "INTEGER,POSITIVE",      "" },
	
// outputs
	{ SSC_OUTPUT,        SSC_NUMBER,      "a",                      "Modified nonideality factor",    "1/V",    "",                      "6 Parameter Solver",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "Il",                     "Light current",                  "A",      "",                      "6 Parameter Solver",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "Io",                     "Saturation current",             "A",      "",                      "6 Parameter Solver",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "Rs",                     "Series resistance",              "ohm",    "",                      "6 Parameter Solver",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "Rsh",                    "Shunt resistance",               "ohm",    "",                      "6 Parameter Solver",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "Adj",                    "OC SC temp coeff adjustment",    "%",      "",                      "6 Parameter Solver",      "*",                        "",                      "" },
	
var_info_invalid };

class cm_6parsolve : public compute_module
{
public:

	cm_6parsolve()
	{
		add_var_info( _cm_vtab_6parsolve );
	}
	
	void exec( ) throw( general_error )
	{
		int tech_id = module6par::monoSi;
		std::string stype = as_string("celltype");

		if (stype.find("mono") != std::string::npos) tech_id = module6par::monoSi;
		else if (stype.find("multi") != std::string::npos || stype.find("poly") != std::string::npos) tech_id = module6par::multiSi;
		else if (stype.find("cis") != std::string::npos) tech_id = module6par::CIS;
		else if (stype.find("cigs") != std::string::npos) tech_id = module6par::CIGS;
		else if (stype.find("cdte") != std::string::npos) tech_id = module6par::CdTe;
		else if (stype.find("amor") != std::string::npos) tech_id = module6par::Amorphous;
		else
			throw general_error("could not determine cell type (mono,multi,cis,cigs,cdte,amorphous)");

		double Vmp = as_double("Vmp");
		double Imp = as_double("Imp");
		double Voc = as_double("Voc");
		double Isc = as_double("Isc");
		double bVoc = as_double("beta");
		double aIsc = as_double("alpha");
		double gPmp = as_double("gamma");
		int nser = as_integer("Nser");

		module6par m( tech_id, Vmp, Imp, Voc, Isc, bVoc, aIsc, gPmp, nser );
		int err = m.solve_with_sanity_and_heuristics<double>(300,1e-7);
		if (err < 0)
			throw general_error("could not solve, check inputs");

		assign("a", var_data( (ssc_number_t) m.a));
		assign("Il", var_data( (ssc_number_t) m.Il));
		assign("Io", var_data( (ssc_number_t) m.Io));
		assign("Rs", var_data( (ssc_number_t) m.Rs));
		assign("Rsh", var_data( (ssc_number_t) m.Rsh));
		assign("Adj", var_data( (ssc_number_t) m.Adj));
	}
};

DEFINE_MODULE_ENTRY( 6parsolve, "Solver for CEC/6 parameter PV module coefficients", 1 )
