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
#include <memory>

#include "common.h"
#include "core.h"

#include "cmod_fuelcell.h"

var_info vtab_fuelcell_input[] = {
	/*   VARTYPE           DATATYPE         NAME                               LABEL                                    UNITS      META                   GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	// simulation inputs
	{ SSC_INPUT,        SSC_NUMBER,      "system_use_lifetime_output",        "Lifetime simulation",                   "0/1",     "0=SingleYearRepeated,1=RunEveryYear",   "",        "?=0",                   "BOOLEAN",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "analysis_period",                   "Lifetime analysis period",              "years",   "The number of years in the simulation", "",        "system_use_lifetime_output=1","",                           "" },

	// external compute module inputs
	{ SSC_INOUT,        SSC_ARRAY,       "gen",								  "System power generated",                "kW",        "Lifetime system generation", "",                  "",                        "",                              "" },
	{ SSC_INPUT,		SSC_ARRAY,	     "load",			                  "Electricity load (year 1)",             "kW",	    "",                  "",	                       "",	                      "",	                           "" },

	// fuel cell
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_degradation",              "Fuel cell degradation per hour",        "kW/h",       "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_degradation_restart",      "Fuel cell degradation at restart",      "kW",         "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_fixed_pct",				  "Fuel cell fixed operation percent",     "%",          "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_dynamic_response",         "Fuel cell response after startup",      "kW/h",       "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "fuelcell_efficiency",               "Fuel cell efficiency table ",           "",           "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_fuel_available",           "Fuel cell available fuel quantity",     "MCf",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_fuel_price",				  "Fuel cell price",                       "$/MCf",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_fuel_type",				  "Fuel cell type",                        "0/1",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_lhv",                      "Fuel cell lower heating value",         "Btu/ft3",    "",                 "Fuel Cell",				   "",                        "",                              "" },
	//{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_hhv",                      "Fuel cell higher heating value",        "Btu/ft3",    "",                 "Fuel Cell",				   "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_number_of_units",          "Fuel cell number of units",             "",           "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_operation_options",         "Fuel cell turn off options",            "0/1",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_replacement_option",       "Fuel cell replacement option",          "0/1/2",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_replacement_percent",      "Fuel cell replace at percentage",       "",           "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "fuelcell_replacement_schedule",     "Fuel cell replace on schedule",         "",           "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_startup_time",             "Fuel cell startup hours",               "hours",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_type",                     "Fuel cell type",						   "0/1/2",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_unit_max_power",           "Fuel cell max power per unit",          "kW",         "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_unit_min_power",           "Fuel cell min power per unit",          "kW",         "",                 "Fuel Cell",                  "",                        "",                              "" },

	//  Dispatch
	{ SSC_INPUT,        SSC_ARRAY,       "fuelcell_dispatch",                 "Fuel cell dispatch input per unit",     "kW",         "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fuelcell_dispatch_choice",          "Fuel cell dispatch choice",             "0/1/2",      "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dispatch_manual_fuelcellcharge",    "Periods 1-6 charging allowed?",          "",          "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dispatch_manual_fuelcelldischarge", "Periods 1-6 discharging allowed?",       "",          "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dispatch_manual_percent_fc_discharge","Periods 1-6 discharging allowed?",     "",          "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "dispatch_manual_sched",             "Dispatch schedule for weekday",          "",          "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "dispatch_manual_sched_weekend",     "Dispatch schedule for weekend",          "",          "",                 "Fuel Cell",                  "",                        "",                              "" },

var_info_invalid };

var_info vtab_fuelcell_output[] = {

	//{ SSC_OUTPUT,       SSC_ARRAY,       "gen",                                "System power generated",                "kW",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "fuelcell_power",                     "Electricity from fuel cell",            "kW",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "fuelcell_power_thermal",             "Heat from fuel cell",                   "kWt",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "fuelcell_fuel_consumption_mcf",      "Fuel consumption of fuel cell",         "MCf",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "fuelcell_to_load",                   "Electricity to load from fuel cell",    "kW",        "",                 "Fuel Cell",                  "",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "fuelcell_to_grid",                   "Electricity to grid from fuel cell",    "kW",        "",                 "Fuel Cell",                  "",                        "",                              "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "system_heat_rate",                     "Heat rate conversion factor (MMBTUs/MWhe)",  "MMBTUs/MWhe",   "",      "Fuel Cell",           "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_fuel_usage",             "Annual Fuel Usage",                          "kWht",          "",      "Fuel Cell",           "*",               "",                    "" },


var_info_invalid };

cm_fuelcell::cm_fuelcell()
{
	add_var_info(vtab_fuelcell_input);
	add_var_info(vtab_fuelcell_output);
	add_var_info(vtab_technology_outputs);
}

// Have to add this since compute module isn't actually fully constructed until compute is called with
// a vartable.
void cm_fuelcell::construct()
{
	std::unique_ptr<fuelCellVariables> tmp(new fuelCellVariables(*this));
	fcVars = std::move(tmp);

	std::unique_ptr<FuelCell> tmp2(new FuelCell(fcVars->unitPowerMax_kW, fcVars->unitPowerMin_kW,
		fcVars->startup_hours, fcVars->dynamicResponse_kWperHour, fcVars->degradation_kWperHour,
		fcVars->degradationRestart_kW, 
		fcVars->replacementOption, fcVars->replacement_percent, fcVars->replacementSchedule,
		fcVars->efficiencyTable,
		fcVars->lowerHeatingValue_BtuPerFt3, fcVars->higherHeatingValue_BtuPerFt3, fcVars->availableFuel_MCf,
		fcVars->shutdownOption, fcVars->dt_hour));
	fuelCell = std::move(tmp2);

	std::unique_ptr<FuelCellDispatch> tmp3(new FuelCellDispatch(fuelCell.get(), fcVars->numberOfUnits,
		fcVars->dispatchOption, fcVars->shutdownOption, fcVars->dt_hour, fcVars->fixed_percent, fcVars->dispatch_kW,
		fcVars->canCharge, fcVars->canDischarge, fcVars->discharge_percentByPeriod, fcVars->scheduleWeekday,
		fcVars->scheduleWeekend));
	fuelCellDispatch = std::move(tmp3);

	allocateOutputs();
}

void cm_fuelcell::exec() throw (general_error)
{
	construct();
	size_t idx = 0;
 	for (size_t y = 0; y < fcVars->numberOfYears; y++) {
		size_t idx_year = 0;
		for (size_t h = 0; h < 8760; h++){
			for (size_t s = 0; s < fcVars->stepsPerHour; s++) {
				fuelCellDispatch->runSingleTimeStep(h, idx_year, fcVars->systemGeneration_kW[idx], fcVars->electricLoad_kW[idx]);
				p_fuelCellPower_kW[idx] = (ssc_number_t)fuelCellDispatch->getPower();
				p_fuelCellPowerThermal_kW[idx] = (ssc_number_t)fuelCellDispatch->getPowerThermal();
				p_fuelCellConsumption_MCf[idx] = (ssc_number_t)fuelCellDispatch->getFuelConsumption();
				p_fuelCellToGrid_kW[idx] = (ssc_number_t)(fuelCellDispatch->getBatteryPower()->powerFuelCellToGrid);
				p_fuelCellToLoad_kW[idx] = (ssc_number_t)(fuelCellDispatch->getBatteryPower()->powerFuelCellToLoad);
				p_gen_kW[idx] = (ssc_number_t)(fcVars->systemGeneration_kW[idx]) + p_fuelCellPower_kW[idx];
				idx++;
				idx_year++;
			}
		}
	}
	// post calculations for financial models
	ssc_number_t annual_fuel_usage = 0.0;
	for (idx = 0; idx < fcVars->numberOfLifetimeRecords; idx++) {
		annual_fuel_usage += p_fuelCellConsumption_MCf[idx];
	}
	// modify heat rate to use here (MMBTU/MWhe)
	assign("system_heat_rate", var_data((ssc_number_t)3.4123)); 
	annual_fuel_usage *= (ssc_number_t)0.29; // 1cu ft = 0.29 kWh
	assign("annual_fuel_usage", annual_fuel_usage);


}

void cm_fuelcell::allocateOutputs()
{
	p_fuelCellPower_kW = allocate("fuelcell_power", fcVars->numberOfLifetimeRecords);
	p_fuelCellPowerThermal_kW = allocate("fuelcell_power_thermal", fcVars->numberOfLifetimeRecords);
	p_fuelCellConsumption_MCf = allocate("fuelcell_fuel_consumption_mcf", fcVars->numberOfLifetimeRecords);
	p_fuelCellToGrid_kW = allocate("fuelcell_to_grid", fcVars->numberOfLifetimeRecords);
	p_fuelCellToLoad_kW = allocate("fuelcell_to_load", fcVars->numberOfLifetimeRecords);

	p_gen_kW = allocate("gen", fcVars->numberOfLifetimeRecords);

}

cm_fuelcell::~cm_fuelcell(){/* nothing to do */ }




DEFINE_MODULE_ENTRY(fuelcell, "Fuel cell model", 1)
