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

#include <math.h>

#include "common.h"
#include "core.h"


var_info vtab_fuelcell[] = {
	/*   VARTYPE           DATATYPE         NAME                               LABEL                                    UNITS      META                   GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_available_units",          "Fuel cell available units",             "0/1/2",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_degradation",              "Fuel cell degradation per hour",        "kW/h",       "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_degradation_restart",      "Fuel cell degradation at restart",      "kW",         "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_dispatch_choice",          "Fuel cell dispatch choice",             "0/1/2",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_fixed_pct",				  "Fuel cell fixed operation percent",     "%",          "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_dynamic_response",         "Fuel cell response after startup",      "kW/min",     "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "fuelcell_efficiency",               "Fuel cell efficiency table ",           "",           "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_fuel_available",           "Fuel cell available quantity",          "",           "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_fuel_price",				  "Fuel cell price",                       "",           "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_fuel_type",				  "Fuel cell type",                        "0/1",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,		SSC_NUMBER,	     "fuelcell_lhv_units",			      "Fuel cell lower heating value units",   "",	         "",		         "Fuel Cell",                  "",	                      "",	                           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_lower_heating_value",      "Fuel cell lower heating value",         "",           "",                 "Fuel Cell",				   "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_number_of_units",          "Fuel cell number of units",             "",           "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_operation_options",        "Fuel cell turn off options",            "0/1",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_price_units",			  "Fuel cell price units",                 "0/1/2",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_replacement_percent",      "Fuel cell replace at percentage",       "",           "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_startup_time",             "Fuel cell startup hours",               "hours",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_type",                     "Fuel cell type",						   "0/1/2",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_unit_max_power",           "Fuel cell max power per unit",          "kW",         "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_unit_min_power",           "Fuel cell min power per unit",          "kW",         "",                 "Fuel Cell",                  "",                        "",                              "" },


var_info_invalid };

class cm_battwatts : public compute_module
{
public:

	cm_battwatts()
	{
		add_var_info(vtab_technology_outputs);
	}

	void exec() throw(general_error)
	{
	}
};

DEFINE_MODULE_ENTRY(battwatts, "simple battery model", 1)
