/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#include "core.h"

#include <limits>
#include <cmath>

#include "6par_jacobian.h"
#include "6par_lu.h"
#include "6par_search.h"
#include "6par_newton.h"
#include "6par_gamma.h"
#include "6par_solve.h"



static var_info _cm_vtab_6parsolve[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                                UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,         SSC_STRING,      "celltype",               "Cell technology type",           "monoSi,multiSi/polySi,cis,cigs,cdte,amorphous","","6 Parameter Solver",      "*",        "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "Vmp",                    "Maximum power point voltage",    "V",       "",                      "6 Parameter Solver",      "*",                       "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "Imp",                    "Maximum power point current",    "A",       "",                      "6 Parameter Solver",      "*",                       "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "Voc",                    "Open circuit voltage",           "V",       "",                      "6 Parameter Solver",      "*",                       "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "Isc",                    "Short circuit current",          "A",       "",                      "6 Parameter Solver",      "*",                       "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "alpha_isc",              "Temp coeff of current at SC",    "A/'C",    "",                      "6 Parameter Solver",      "*",                       "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "beta_voc",               "Temp coeff of voltage at OC",    "V/'C",    "",                      "6 Parameter Solver",      "*",                       "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "gamma_pmp",              "Temp coeff of power at MP",      "%/'C",    "",                      "6 Parameter Solver",      "*",                       "",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "Nser",                   "Number of cells in series",      "",        "",                      "6 Parameter Solver",      "*",                       "INTEGER,POSITIVE",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "Tref",                   "Reference cell temperature",     "'C",      "",                      "6 Parameter Solver",      "?",                       "",      "" },
	
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
	
	void exec( )
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
		double bVoc = as_double("beta_voc");
		double aIsc = as_double("alpha_isc");
		double gPmp = as_double("gamma_pmp");
		int nser = as_integer("Nser");

		double Tref = 25;
		if ( is_assigned("Tref") )
			Tref = as_double("Tref");

		module6par m( tech_id, Vmp, Imp, Voc, Isc, bVoc, aIsc, gPmp, nser, Tref+273.15 );
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
