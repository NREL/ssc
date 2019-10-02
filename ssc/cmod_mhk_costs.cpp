/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "core.h"
#include "common.h"

enum MHK_DEVICE_TYPES { GENERIC, RM3, RM5, RM6 };
enum MHK_TECHNOLOGY_TYPE { WAVE, TIDAL };



static var_info _cm_vtab_mhk_costs[] = {
	/*   VARTYPE			DATATYPE			NAME									   LABEL                                                   UNITS			META            GROUP              REQUIRED_IF            CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,			SSC_NUMBER,			"device_rated_power",						"Rated capacity of device",								"kW",			"",								"MHKCosts",			"*",					"MIN=0",					"" },
	{ SSC_INPUT,			SSC_NUMBER,			"system_capacity",							"System Nameplate Capacity",							"kW",			"",								"MHKCosts",			"*",					"MIN=0",					"" },
	{ SSC_INPUT,			SSC_NUMBER,			"devices_per_row",							"Number of wave devices per row in array",				"",				"",								"MHKCosts",         "*",                    "INTEGER",			    	"" },
	{ SSC_INPUT,			SSC_NUMBER,			"device_type",								"Device Type",											"0/1/2/3",		"0=Generic,1=RM3,2=RM5,3=RM6",	"MHKCosts",			"?=0",					"MIN=0,MAX=3",				"" },
	{ SSC_INPUT,			SSC_NUMBER,			"marine_energy_tech",						"Marine energy technology",								"0/1",			"0=Wave,1=Tidal",				"MHKCosts",			"*",					"MIN=0,MAX=1",				"" },
	
	{ SSC_INPUT,			SSC_NUMBER,			"inter_array_cable_length",					"Inter-array cable length",								"m",			"",								"MHKCosts",			"*",					"MIN=0",					"" },
	{ SSC_INPUT,			SSC_NUMBER,			"riser_cable_length",						"Riser cable length",									"m",			"",								"MHKCosts",			"*",					"MIN=0",					"" },
	{ SSC_INPUT,			SSC_NUMBER,			"export_cable_length",						"Export cable length",									"m",			"",								"MHKCosts",			"*",					"MIN=0",					"" },

	//CapEx costs
	{ SSC_OUTPUT,			SSC_NUMBER,			"structural_assembly_cost_modeled",			"Modeled structural assembly cost",						"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"power_takeoff_system_cost_modeled",		"Modeled power take-off cost",							"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"mooring_found_substruc_cost_modeled",		"Modeled mooring, foundation, and substructure cost",	"$",			"",								"MHKCosts",			"",						"",							"" },
	
	//Balance of system costs
	{ SSC_OUTPUT,			SSC_NUMBER,			"development_cost_modeled",					"Modeled development cost",								"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"eng_and_mgmt_cost_modeled",				"Modeled engineering and management cost",				"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"plant_commissioning_cost_modeled",			"Modeled plant commissioning cost",						"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"site_access_port_staging_cost_modeled",	"Modeled site access, port, and staging cost",			"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"assembly_and_install_cost_modeled",		"Modeled assembly and installation cost",				"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"other_infrastructure_cost_modeled",		"Modeled other infrastructure cost",					"$",			"",								"MHKCosts",			"",						"",							"" },
	
	//Electrical infrastructure costs
	{ SSC_OUTPUT,			SSC_NUMBER,			"array_cable_system_cost_modeled",			"Modeled array cable system cost",						"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"export_cable_system_cost_modeled",			"Modeled export cable system cost",						"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"onshore_substation_cost_modeled",			"Modeled onshore substation cost",						"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"offshore_substation_cost_modeled",			"Modeled offshore substation cost",						"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"other_elec_infra_cost_modeled",			"Modeled other electrical infrastructure cost",			"$",			"",								"MHKCosts",			"",						"",							"" },

	//Financial costs
	{ SSC_OUTPUT,			SSC_NUMBER,			"project_contingency",						"Modeled project contingency cost",						"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"insurance_during_construction",			"Modeled cost of insurance during construction",		"$",			"",								"MHKCosts",			"",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"reserve_accounts",							"Modeled reserve account costs",						"$",			"",								"MHKCosts",			"",						"",							"" },

		var_info_invalid };



class cm_mhk_costs : public compute_module
{
public:
	cm_mhk_costs()
	{
		add_var_info(_cm_vtab_mhk_costs);
	}

	void exec()
	{
		//get inputs to compute module
		double device_rating = as_double("device_rated_power");
		double system_capacity = as_double("system_capacity");
		int device_type = as_integer("device_type");
		int technology = as_integer("marine_energy_tech");
		int devices_per_row = as_integer("devices_per_row");
		double interarray_length = as_double("inter_array_cable_length");
		double riser_length = as_double("riser_cable_length");
		double export_length = as_double("export_cable_length");

		//define intermediate variables to store outputs
		double structural_assembly, power_takeoff, mooring_found_substruc;
		double development, eng_and_mgmt, plant_commissioning, site_access_port_staging, assembly_and_install, other_infrastructure;
		double array_cable_system, export_cable_system, onshore_substation, offshore_substation, other_elec_infra;
		double project_contingency, insurance_during_construction, reserve_accounts;

		//CapEx costs depend on technology
		if (device_type == RM3)
		{
			structural_assembly = 6854912 * system_capacity + 2629191;
			power_takeoff = 2081129 * pow(system_capacity, 0.91);
			mooring_found_substruc = 1836365 * system_capacity + 29672;
		}

		else if (device_type == RM5)
		{
			structural_assembly = 6848402 * system_capacity + 3315338;
			power_takeoff = 1600927 * pow(system_capacity, 0.78);
			mooring_found_substruc = 2030816 * system_capacity + 478400;
		}

		else if (device_type == RM6)
		{
			structural_assembly = 13320092 * system_capacity + 6681164;
			power_takeoff = 3796551 * pow(system_capacity, 0.91);
			mooring_found_substruc = 2158462 * system_capacity + 1048932;
		}
		
		else //generic model applies to everything else
		{
			structural_assembly = 6854912 * system_capacity + 2629191;
			if (technology == WAVE) power_takeoff = 1179579 * system_capacity + 2495107;
			else power_takeoff = 2906035 * system_capacity;
			mooring_found_substruc = 2158462 * system_capacity + 1048932;
		}

		//BOS costs are the same regardless of device technology
		development = 3197591 * pow(system_capacity, 0.49);
		eng_and_mgmt = 850744 * pow(system_capacity, 0.5649);
		double capex = structural_assembly + power_takeoff + mooring_found_substruc;
		plant_commissioning = 0.016 * capex;
		site_access_port_staging = 0.011 * capex;
		assembly_and_install = 2805302 * pow(system_capacity, 0.66);
		other_infrastructure = 0;

		//electrical infrastructure costs
		array_cable_system = 4.4 * (device_rating * devices_per_row / 1000.0) + 162.81 * interarray_length + 4.4 * (device_rating / 1000.0) + 162.81 * riser_length;
		export_cable_system = 4.4 * (system_capacity / 1000.0) + 162.81 * export_length;
		onshore_substation = 75000 * system_capacity / 1000.0;
		offshore_substation = 100000 * system_capacity / 1000.0;
		other_elec_infra = 47966.16 * system_capacity / 1000.0 + 665841;

		//financial costs are the same regardless of technology
		project_contingency = 0.05 * capex;
		insurance_during_construction = 0.01 * capex;
		reserve_accounts = 0.03 * capex;

		//assign all outputs
		assign("structural_assembly_cost_modeled", var_data(static_cast<ssc_number_t>(structural_assembly)));
		assign("power_takeoff_system_cost_modeled", var_data(static_cast<ssc_number_t>(power_takeoff)));
		assign("mooring_found_substruc_cost_modeled", var_data(static_cast<ssc_number_t>(mooring_found_substruc)));

		assign("development_cost_modeled", var_data(static_cast<ssc_number_t>(development)));
		assign("eng_and_mgmt_cost_modeled", var_data(static_cast<ssc_number_t>(eng_and_mgmt)));
		assign("plant_commissioning_cost_modeled", var_data(static_cast<ssc_number_t>(plant_commissioning)));
		assign("site_access_port_staging_cost_modeled", var_data(static_cast<ssc_number_t>(site_access_port_staging)));
		assign("assembly_and_install_cost_modeled", var_data(static_cast<ssc_number_t>(assembly_and_install)));
		assign("other_infrastructure_cost_modeled", var_data(static_cast<ssc_number_t>(other_infrastructure)));

		assign("array_cable_system_cost_modeled", var_data(static_cast<ssc_number_t>(array_cable_system)));
		assign("export_cable_system_cost_modeled", var_data(static_cast<ssc_number_t>(export_cable_system)));
		assign("onshore_substation_cost_modeled", var_data(static_cast<ssc_number_t>(onshore_substation)));
		assign("offshore_substation_cost_modeled", var_data(static_cast<ssc_number_t>(offshore_substation)));
		assign("other_elec_infra_cost_modeled", var_data(static_cast<ssc_number_t>(other_elec_infra)));

		assign("project_contingency", var_data(static_cast<ssc_number_t>(project_contingency)));
		assign("insurance_during_construction", var_data(static_cast<ssc_number_t>(insurance_during_construction)));
		assign("reserve_accounts", var_data(static_cast<ssc_number_t>(reserve_accounts)));

	}

};

DEFINE_MODULE_ENTRY(mhk_costs, "Calculates various cost categories for Marine Energy arrays for different device types.", 3);