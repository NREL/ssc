/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
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
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
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

static var_info _cm_vtab_geothermal_costs[] = {
/*   VARTYPE           DATATYPE         NAME                              LABEL                                                      UNITS     META                      GROUP                   REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
																	      								                             
	// Inputs		
	//example input types, names, labels, units, required_if, constraints- delete or change
	{ SSC_INPUT,        SSC_NUMBER,      "machine_rating",                "Machine Rating",                                          "kW",     "",                      "geothermal_costs",      "*",                       "INTEGER,MIN=0,MAX=1",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rotor_diameter",                "Rotor Diameter",                                          "m",      "",                      "geothermal_costs",      "?=20",                    "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_STRING,      "solar_resource_file",           "Weather file in TMY2, TMY3, EPW, or SAM CSV.",            "",       "",                      "geothermal_costs",      "?",                       "POSITIVE",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dc_degradation",                "Annual module degradation",                               "%/yr",   "",                      "geothermal_costs",      "rotor_diameter=1",        "LENGTH=12",                     "" },

	// Outputs	
	//example output, delete or change
	{ SSC_OUTPUT,       SSC_NUMBER,      "project_total_budgeted_cost",   "Project Total Budgeted Cost",                             "$s",     "",                      "geothermal_costs",      "*",                       "",                              "" },

var_info_invalid };

class cm_geothermal_costs : public compute_module
{
public:
	cm_geothermal_costs()
	{
		add_var_info(_cm_vtab_geothermal_costs);
	}

	//example typedef- delete or change
	typedef enum { FLAT_TO_ROLLING, RIDGE_TOP, MOUNTAINOUS } SiteTerrain;

	//example function within compute module- delete or change
	double farmSize(double rating, int nTurb){
		return rating * nTurb / 1000.0;
	}


	void exec( ) throw( general_error )
	{
		//first get values from input variable table
		//examples- delete or change
		double rating = (double) as_number("machine_rating");
		int nTurb = as_integer("number_of_turbines");
		const char *file = as_string("solar_resource_file");

		//reading in an array- create a pointer
		size_t count_dc_degrad = 0;
		ssc_number_t *dc_degrad = 0;
		dc_degrad = as_array("dc_degradation", &count_dc_degrad);


		// run model calculations here- delete or change
		//can call out to functions defined above
		double cost = 566;

		// assign outputs
		//example- delete or change
		assign( "project_total_budgeted_cost", var_data(cost) );
	}
};

DEFINE_MODULE_ENTRY( geothermal_costs, "Computes geothermal plant cost", 1 )
