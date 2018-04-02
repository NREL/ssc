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

static var_info _cm_vtab_windcsm[] = {
/*   VARTYPE           DATATYPE         NAME                              LABEL                                                      UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/			

	// Rotor														      								                             
	{ SSC_INPUT,        SSC_NUMBER,      "blades",							"Blade mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hub",								"Hub mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pitch",							"Pitch mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "spinner",							"Spinner mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },

	// Drivetrain
	{ SSC_INPUT,        SSC_NUMBER,      "lowspeedshaft",					"Low speed shaft mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bearings",						"Bearings mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "gearbox",							"Gearbox mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "brake",							"Brake mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "generator",						"Low speed shaft mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "variablespeed",					"Variable speed electronics mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "yaw",								"Yaw mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bedplate",						"Bedplate mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "connections",						"Electrical connections mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hydraulic",						"Hydraulics mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "nacelle",							"Nacelle mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "controls",						"Controls mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "other",							"Other rivetrain mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },

	// Tower
	{ SSC_INPUT,        SSC_NUMBER,      "tower",							"Tower mass",                                          "kg",     "",                      "wind_csm",      "*",                       "",                              "" },

	// Outputs intermediate percentages and cost breakdown and total cost
	{ SSC_OUTPUT,       SSC_NUMBER,      "rotor_cost",             "Rotor Cost",                                 "$",     "",                      "wind_csm",      "*",                       "",                              "" },

var_info_invalid };

// $/kg costs from WISDEM https://github.com/WISDEM/Turbine_CostsSE


class cm_windcsm : public compute_module
{
public:
	cm_windcsm()
	{
		add_var_info(_cm_vtab_windcsm);
	}



	void exec( ) throw( general_error )
	{
		// get values
		double blades = (double) as_number("blades");

		// assign outputs
//		assign( "rotor_cost", var_data(output) );
	}
};

DEFINE_MODULE_ENTRY( windcsm, "WISDEM turbine cost model", 1 )
