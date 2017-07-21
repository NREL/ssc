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
#include "lib_windfile.h"
#include "lib_windwatts.h"
// for adjustment factors
#include "common.h"

static var_info _cm_vtab_generic_system[] = {
//	  VARTYPE           DATATYPE         NAME                           LABEL                                 UNITS           META     GROUP                REQUIRED_IF        CONSTRAINTS           UI_HINTS
	{ SSC_INPUT,        SSC_NUMBER,      "spec_mode",                  "Spec mode: 0=constant CF,1=profile",  "",             "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                     "Derate",                              "%",            "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",         "Nameplace Capcity",                   "kW",           "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "user_capacity_factor",            "Capacity Factor",                     "%",            "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heat_rate",                  "Heat Rate",                           "MMBTUs/MWhe",  "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "conv_eff",                   "Conversion Efficiency",               "%",            "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "energy_output_array",        "Array of Energy Output Profile",      "kWh",          "",      "generic_system",      "*",               "",                    "" }, 
																														      														   
//    OUTPUTS ----------------------------------------------------------------------------								      														   
//	  VARTYPE           DATATYPE         NAME                          LABEL                                   UNITS           META     GROUP                 REQUIRED_IF        CONSTRAINTS           UI_HINTS
//	{ SSC_OUTPUT,       SSC_ARRAY,       "hourly_energy",              "Hourly Energy",                        "kWh",           "",      "Time Series",      "*",               "LENGTH=8760",         "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",             "Monthly Energy",                       "kWh",          "",      "Monthly",      "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",              "Annual Energy",                        "kWh",          "",      "Annual",      "*",               "",                    "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_fuel_usage",                 "Annual Fuel Usage",                    "kWht",         "",      "Annual",      "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "water_usage",                "Annual Water Usage",                   "",             "",      "Annual",      "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "system_heat_rate",           "Heat Rate Conversion Factor",          "MMBTUs/MWhe",  "",      "Annual",      "*",               "",                    "" },

	{ SSC_OUTPUT, SSC_NUMBER, "capacity_factor", "Capacity factor", "%", "", "Annual", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "kwh_per_kw", "First year kWh/kW", "kWh/kW", "", "Annual", "*", "", "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "system_use_lifetime_output",     "Use lifetime output",                         "0/1", "",                        "Miscellaneous", "*",                       "INTEGER",                   "" },

var_info_invalid };

class cm_generic_system : public compute_module
{
private:
public:
	
	cm_generic_system()
	{
		add_var_info( _cm_vtab_generic_system );
		// performance adjustment factors
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);
	}

	void exec( ) throw( general_error )
	{
		int spec_mode = as_integer("spec_mode");
		ssc_number_t *enet = allocate("gen", 8760);
//		ssc_number_t *p_gen = allocate("gen", 8760);

		double derate = (1 - (double)as_number("derate") / 100);
		double annual_output = 0;

		adjustment_factors haf(this, "adjust");
		if (!haf.setup())
			throw exec_error("pvwattsv5", "failed to setup adjustment factors: " + haf.error());

		if (spec_mode == 0)
		{
			double output = (double)as_number("system_capacity")
				* (double)as_number("user_capacity_factor") / 100
				* derate;

			annual_output = 8760 * output;

			for (int i = 0; i < 8760; i++)
			{
				enet[i] = output*haf(i);
//				p_gen[i] = enet[i];
			}
		}
		else
		{
			size_t count = 0;
			ssc_number_t *data = as_array("energy_output_array", &count);

			if (!data)
				throw exec_error("generic", util::format("energy_output_array variable had no values."));

			int nmult = count / 8760;
			if (nmult * 8760 != count)
				throw exec_error("generic", util::format("energy_output_array not a multiple of 8760: len=%d.", count));

			int c = 0;
			int i = 0;
			while (c<8760 && i<count)
			{
				double integ = 0;
				for (int j = 0; j<nmult; j++)
				{
					integ += data[i];
					i++;
				}

				enet[c] = integ*derate*haf(c);
//				p_gen[c] = enet[c];

				annual_output += enet[c];
				c++;
			}
		}

		accumulate_monthly("gen", "monthly_energy");
		accumulate_annual("gen", "annual_energy");

		// if conversion efficiency is zero then set fuel usage to zero per email from Paul 5/17/12
		double fuel_usage = 0.0;
		if (as_double("conv_eff") != 0.0)
			fuel_usage = annual_output * 100.0 / as_double("conv_eff");
		assign("annual_fuel_usage", fuel_usage);

		assign("water_usage", 0.0);
		assign("system_heat_rate", as_double("heat_rate") *  as_double("conv_eff") / 100.0);

		// metric outputs moved to technology
		double kWhperkW = 0.0;
		double nameplate = as_double("system_capacity");
		double annual_energy = 0.0;
		for (int i = 0; i < 8760; i++)
			annual_energy += enet[i];
		if (nameplate > 0) kWhperkW = annual_energy / nameplate;
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));

		assign("system_use_lifetime_output", 0);

	} // exec
};

DEFINE_MODULE_ENTRY( generic_system, "Generic System", 1 );

