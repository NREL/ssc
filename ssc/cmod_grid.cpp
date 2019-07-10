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

#include <cmath>
#include <memory>
#include <numeric>

#include "common.h"
#include "core.h"

#include "cmod_grid.h"

var_info vtab_grid_input[] = {
	/*   VARTYPE           DATATYPE         NAME                               LABEL                                    UNITS      META                   GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	// simulation inputs
	{ SSC_INPUT,        SSC_NUMBER,      "system_use_lifetime_output",        "Lifetime simulation",                   "0/1",     "0=SingleYearRepeated,1=RunEveryYear",   "Lifetime",        "?=0",                   "BOOLEAN",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "analysis_period",                   "Lifetime analysis period",              "years",   "The number of years in the simulation", "Lifetime",        "system_use_lifetime_output=1","",                           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "enable_interconnection_limit",      "Enable grid interconnection limit",     "0/1",     "Enable a grid interconnection limit",   "",        "","",                           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "grid_interconnection_limit_kwac",   "Grid interconnection limit",            "kWac",    "The number of years in the simulation", "",        "","",                           "" },

	// external compute module inputs
	{ SSC_INOUT,        SSC_ARRAY,       "gen",								  "System power generated",                "kW",        "Lifetime system generation", "",                  "",                        "",                              "" },
	{ SSC_INPUT,		SSC_ARRAY,	     "load",			                  "Electricity load (year 1)",             "kW",	    "",                  "",	                       "",	                      "",	                           "" },

var_info_invalid };

var_info vtab_grid_output[] = {

	{ SSC_OUTPUT,        SSC_ARRAY,       "system_pre_interconnect_kwac",     "System power before grid interconnect",  "kW",       "Lifetime system generation" "",                 "",                        "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "capacity_factor_interconnect_ac",  "Capacity factor of the interconnection (year 1)",  "%",          "",                "",                           "?=0",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_energy_pre_interconnect_ac", "Annual Energy AC pre-interconnection (year 1)",   "kWh",        "",                "",                           "?=0",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_energy",                    "Annual Energy AC (year 1)",                        "kWh",        "",                "",                           "?=0",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_ac_interconnect_loss_percent","Annual Energy loss from interconnection limit (year 1)", "%", "",                "",                           "?=0",                     "",                              "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_ac_interconnect_loss_kwh",   "Annual Energy loss from interconnection limit (year 1)", "kWh", "",                "",                           "?=0",                     "",                              "" },


var_info_invalid };


cm_grid::cm_grid()
{
	add_var_info(vtab_grid_input);
	add_var_info(vtab_grid_output);
	add_var_info(vtab_technology_outputs);
}

// Have to add this since compute module isn't actually fully constructed until compute is called with
// a vartable.
void cm_grid::construct()
{
	std::unique_ptr<gridVariables> tmp(new gridVariables(*this));
	gridVars = std::move(tmp);
	allocateOutputs();
}

void cm_grid::exec()
{
	construct();

	// interconnection  calculations
	double capacity_factor_interconnect, annual_energy_pre_interconnect, annual_energy_interconnect;
	capacity_factor_interconnect = annual_energy_pre_interconnect = annual_energy_interconnect = 0;

	// compute grid export, apply interconnection limit
	if (gridVars->enable_interconnection_limit)
	{
		for (size_t i = 0; i < gridVars->numberOfLifetimeRecords; i++) {
			double gridNet = gridVars->systemGenerationLifetime_kW[i] - gridVars->loadLifetime_kW[i];
			double gridNetCurtailed = fmin(gridNet, gridVars->grid_interconnection_limit_kW);
			double curtailed = gridNet - gridNetCurtailed;
			gridVars->systemGenerationLifetime_kW[i] -= curtailed;

			p_genPreInterconnect_kW[i] = static_cast<ssc_number_t>(gridVars->systemGenerationPreInterconnect_kW[i]);
			p_genInterconnect_kW[i] = static_cast<ssc_number_t>(gridVars->systemGenerationLifetime_kW[i]);
		}

		annual_energy_interconnect = std::accumulate(gridVars->systemGenerationLifetime_kW.begin(), gridVars->systemGenerationLifetime_kW.begin() + gridVars->numberOfSingleYearRecords, (double)0.0)*gridVars->dt_hour_gen;
		annual_energy_pre_interconnect = std::accumulate(gridVars->systemGenerationPreInterconnect_kW.begin(), gridVars->systemGenerationPreInterconnect_kW.begin() + gridVars->numberOfSingleYearRecords, (double)0.0)*gridVars->dt_hour_gen;
		capacity_factor_interconnect = annual_energy_interconnect * util::fraction_to_percent / (gridVars->grid_interconnection_limit_kW * 8760.);

		assign("capacity_factor_interconnect_ac", var_data(capacity_factor_interconnect));
		assign("annual_energy_pre_interconnect_ac", var_data(annual_energy_pre_interconnect));
		assign("annual_energy", var_data(annual_energy_interconnect));
		assign("annual_ac_interconnect_loss_kwh", var_data(std::roundf(annual_energy_pre_interconnect - annual_energy_interconnect)));
		assign("annual_ac_interconnect_loss_percent", var_data(100.0*(annual_energy_pre_interconnect - annual_energy_interconnect)/ annual_energy_pre_interconnect));
	}

}

void cm_grid::allocateOutputs()
{
	if (gridVars->enable_interconnection_limit) {
		p_genInterconnect_kW = allocate("gen", gridVars->systemGenerationLifetime_kW.size());
		p_genPreInterconnect_kW = allocate("system_pre_interconnect_kwac", gridVars->systemGenerationLifetime_kW.size());
	}
}

DEFINE_MODULE_ENTRY(grid, "Grid model", 1)
