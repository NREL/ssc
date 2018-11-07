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
#include "lib_util.h"
#include "lib_battery.h"
#include "cmod_battery.h"
#include "lib_power_electronics.h"




var_info vtab_battwatts[] = {
	/*   VARTYPE           DATATYPE         NAME                               LABEL                                    UNITS      META                   GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "batt_simple_enable",                "Enable Battery",                         "0/1",     "",                 "battwatts",                  "?=0",                        "BOOLEAN",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_simple_kwh",                   "Battery Capacity",                       "kWh",     "",                 "battwatts",                  "?=0",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_simple_kw",                    "Battery Power",                          "kW",      "",                 "battwatts",                  "?=0",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_simple_chemistry",             "Battery Chemistry",                      "0=lead acid/1=Li-ion/2",   "",                 "battwatts",                  "?=0",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_simple_dispatch",              "Battery Dispatch",                       "0=peak shaving look ahead/1=peak shaving look behind",     "",                 "battwatts",                  "?=0",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "batt_simple_meter_position",        "Battery Meter Position",                 "0=behind meter/1=front of meter",     "",                 "battwatts",                  "?=0",                        "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dc",								  "DC array power",                         "W",       "",                 "",                           "",                           "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "ac",								  "AC inverter power",                      "W",       "",                 "",                           "",                           "",                              "" },
	{ SSC_INPUT,		SSC_ARRAY,	     "load",			                  "Electricity load (year 1)",              "kW",	   "",		           "",                           "",	                         "",	                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inverter_model",                    "Inverter model specifier",                 "",      "0=cec,1=datasheet,2=partload,3=coefficientgenerator,4=generic", "",     "",                           "INTEGER,MIN=0,MAX=4",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inverter_efficiency",               "Inverter Efficiency",                     "%",      "",                  "",                          "",                           "",                               "" },

var_info_invalid  };

class cm_battwatts : public compute_module
{
public:

	cm_battwatts()
	{
		add_var_info(vtab_battwatts);
		add_var_info(vtab_battery_outputs);
		add_var_info(vtab_technology_outputs);
	}

	batt_variables * setup_variables(size_t n_recs)
	{
		batt_variables * batt_vars = new batt_variables();

		// allocate vectors
		std::vector<double> * lifetime_matrix = new std::vector < double >;
		std::vector<double> * capacity_vs_temperature = new std::vector<double>;
		double batt_specific_energy_per_mass = 0;
		double batt_specific_energy_per_volume = 0;

		// lithium ion NMC
		if (batt_vars->batt_chem == battery_t::LITHIUM_ION)
		{
			// Voltage properties
			batt_vars->batt_Vnom_default = 3.6;
			batt_vars->batt_Vfull = 4.1;
			batt_vars->batt_Vexp = 4.05;
			batt_vars->batt_Vnom = 3.4;
			batt_vars->batt_Qfull = 2.25;
			batt_vars->batt_Qfull_flow = 0;
			batt_vars->batt_Qexp = 1.78;
			batt_vars->batt_Qnom = 88.9;
			batt_vars->batt_C_rate = 0.2;
			batt_vars->batt_resistance = 0.1;

			// Battery lifetime
			lifetime_matrix->push_back(20); lifetime_matrix->push_back(0); lifetime_matrix->push_back(100);
			lifetime_matrix->push_back(20); lifetime_matrix->push_back(5000); lifetime_matrix->push_back(80);
			lifetime_matrix->push_back(80); lifetime_matrix->push_back(0); lifetime_matrix->push_back(100);
			lifetime_matrix->push_back(80); lifetime_matrix->push_back(1000); lifetime_matrix->push_back(80);
			util::matrix_t<double> batt_lifetime_matrix(6, 3, lifetime_matrix);
			batt_vars->batt_lifetime_matrix = batt_lifetime_matrix;

			batt_vars->batt_calendar_q0 = 1.02;
			batt_vars->batt_calendar_a = 2.66e-3;
			batt_vars->batt_calendar_b = -7280;
			batt_vars->batt_calendar_c = 930;

			// Thermal behavior
			capacity_vs_temperature->push_back(-15); capacity_vs_temperature->push_back(65);
			capacity_vs_temperature->push_back(0);  capacity_vs_temperature->push_back(85);
			capacity_vs_temperature->push_back(25); capacity_vs_temperature->push_back(100);
			capacity_vs_temperature->push_back(40); capacity_vs_temperature->push_back(104);
			util::matrix_t<double> batt_capacity_vs_temperature(4, 2, capacity_vs_temperature);
			batt_vars->cap_vs_temp = batt_capacity_vs_temperature;
			batt_vars->batt_Cp = 1004;
			batt_vars->batt_h_to_ambient = 500;
			batt_vars->T_room = 20;
			batt_specific_energy_per_mass = 197.33;  // Wh/kg
			batt_specific_energy_per_volume = 501.25; // Wh/L
		}
		// Lead acid AGM defaults
		else if (batt_vars->batt_chem == battery_t::LEAD_ACID)
		{
			// Capacity properties
			double LeadAcid_q20 = 100;
			double LeadAcid_q10 = 93.2;
			double LeadAcid_qn = 58.12;
			double LeadAcid_tn = 1;

			batt_vars->LeadAcid_q10_computed = batt_vars->batt_computed_strings * LeadAcid_q10 * batt_vars->batt_Qfull / 100;
			batt_vars->LeadAcid_q20_computed = batt_vars->batt_computed_strings * LeadAcid_q20 * batt_vars->batt_Qfull / 100;
			batt_vars->LeadAcid_qn_computed = batt_vars->batt_computed_strings * LeadAcid_qn * batt_vars->batt_Qfull / 100;
			batt_vars->LeadAcid_tn = LeadAcid_tn;

			// Voltage properties
			batt_vars->batt_Vnom_default = 2;
			batt_vars->batt_Vfull = 2.2;
			batt_vars->batt_Vexp = 2.06;
			batt_vars->batt_Vnom = 2.03;
			batt_vars->batt_Qfull = 20;
			batt_vars->batt_Qexp = 0.25;
			batt_vars->batt_Qnom = 90;
			batt_vars->batt_C_rate = 0.05;
			batt_vars->batt_resistance = 0.1;

			// Battery lifetime
			lifetime_matrix->push_back(30); lifetime_matrix->push_back(0); lifetime_matrix->push_back(100);
			lifetime_matrix->push_back(30); lifetime_matrix->push_back(1100); lifetime_matrix->push_back(90);
			lifetime_matrix->push_back(30); lifetime_matrix->push_back(1200); lifetime_matrix->push_back(50);
			lifetime_matrix->push_back(50); lifetime_matrix->push_back(0); lifetime_matrix->push_back(100);
			lifetime_matrix->push_back(50); lifetime_matrix->push_back(400); lifetime_matrix->push_back(90);
			lifetime_matrix->push_back(50); lifetime_matrix->push_back(500); lifetime_matrix->push_back(50);
			lifetime_matrix->push_back(100); lifetime_matrix->push_back(0); lifetime_matrix->push_back(100);
			lifetime_matrix->push_back(100); lifetime_matrix->push_back(100); lifetime_matrix->push_back(90);
			lifetime_matrix->push_back(100); lifetime_matrix->push_back(150); lifetime_matrix->push_back(50);
			util::matrix_t<double> batt_lifetime_matrix(9, 3, lifetime_matrix);
			batt_vars->batt_lifetime_matrix = batt_lifetime_matrix;

			// Thermal behavior
			capacity_vs_temperature->push_back(-15); capacity_vs_temperature->push_back(65);
			capacity_vs_temperature->push_back(0);  capacity_vs_temperature->push_back(85);
			capacity_vs_temperature->push_back(25); capacity_vs_temperature->push_back(100);
			capacity_vs_temperature->push_back(40); capacity_vs_temperature->push_back(104);
			util::matrix_t<double> batt_capacity_vs_temperature(4, 2, capacity_vs_temperature);
			batt_vars->cap_vs_temp = batt_capacity_vs_temperature;

			batt_vars->batt_Cp = 600;
			batt_vars->batt_h_to_ambient = 500;
			batt_vars->T_room = 20;

			batt_specific_energy_per_mass = 30;  // Wh/kg
			batt_specific_energy_per_volume = 30; // Wh/L
		}


		// Financial Parameters
		batt_vars->analysis_period = 25;
		batt_vars->batt_meter_position = as_integer("batt_simple_meter_position");

		// Lifetime simulation
		batt_vars->system_use_lifetime_output = false;

		// Chemistry
		batt_vars->batt_chem = as_integer("batt_simple_chemistry");

		// Battery bank sizing (assuming max current = 15A)
		batt_vars->batt_kwh = as_double("batt_simple_kwh");
		batt_vars->batt_kw = as_double("batt_simple_kw");
		double current_max = 15;
		double batt_bank_voltage = batt_vars->batt_kw * 1000. / current_max;
		batt_vars->batt_computed_series = (int)std::ceil(batt_bank_voltage / batt_vars->batt_Vnom_default);
		batt_vars->batt_computed_strings = (int)std::ceil((batt_vars->batt_kwh * 1000.) / (batt_vars->batt_Qfull * batt_vars->batt_computed_series * batt_vars->batt_Vnom_default)) - 1;

		// Common Voltage properties
		batt_vars->batt_voltage_choice = voltage_t::VOLTAGE_MODEL;
		batt_vars->batt_voltage_matrix = util::matrix_t<double>();

		// Current and Capacity
		double batt_time_hour = batt_vars->batt_kwh / batt_vars->batt_kw;
		double batt_C_rate_discharge = 1. / batt_time_hour;
		batt_vars->batt_current_choice = dispatch_t::RESTRICT_CURRENT;
		batt_vars->batt_current_charge_max = 1000 * batt_C_rate_discharge * batt_vars->batt_kwh / batt_bank_voltage;
		batt_vars->batt_current_discharge_max = 1000 * batt_C_rate_discharge * batt_vars->batt_kwh / batt_bank_voltage;
		batt_vars->batt_power_charge_max = batt_vars->batt_kw;
		batt_vars->batt_power_discharge_max = batt_vars->batt_kw;

		// Power converters and topology
		batt_vars->batt_topology = ChargeController::AC_CONNECTED;
		batt_vars->batt_ac_dc_efficiency = 96;
		batt_vars->batt_dc_ac_efficiency = 96;
		batt_vars->batt_dc_dc_bms_efficiency = 99;
		batt_vars->pv_dc_dc_mppt_efficiency = 99;

		// Ancillary equipment losses
		double_vec batt_losses;
		double_vec batt_losses_monthly = { 0 };
		for (int i = 0; i != (int)n_recs; i++)
			batt_losses.push_back(0.);
		for (int m = 0; m != 12; m++)
			batt_losses_monthly.push_back(0.);

		batt_vars->batt_loss_choice = losses_t::MONTHLY;
		batt_vars->batt_losses = batt_losses;
		batt_vars->batt_losses_charging = batt_losses_monthly;
		batt_vars->batt_losses_discharging = batt_losses_monthly;
		batt_vars->batt_losses_idle = batt_losses_monthly;

		// Charge limits and priority
		batt_vars->batt_initial_SOC = 50.;
		batt_vars->batt_maximum_SOC = 95.;
		batt_vars->batt_minimum_SOC = 15.;
		batt_vars->batt_minimum_modetime = 10;

		// Storage dispatch controllers
		int dispatch = as_integer("batt_simple_dispatch");
		batt_vars->batt_dispatch = (dispatch == 0 ? dispatch_t::LOOK_AHEAD : dispatch_t::LOOK_BEHIND);
		batt_vars->batt_dispatch_auto_can_charge = true;
		batt_vars->batt_dispatch_auto_can_gridcharge = true;

		// Battery bank replacement
		batt_vars->batt_replacement_capacity = 0.;

		// Battery lifetime
		batt_vars->batt_calendar_choice = lifetime_calendar_t::NONE;
		batt_vars->batt_calendar_lifetime_matrix = util::matrix_t<double>();
		batt_vars->batt_calendar_q0 = 1.0;

		// Common Thermal behavior
		batt_vars->batt_mass = batt_vars->batt_kwh * 1000 / batt_specific_energy_per_mass;
		double batt_volume = batt_vars->batt_kwh / batt_specific_energy_per_volume;
		batt_vars->batt_length = std::pow(batt_volume, 1. / 3.);
		batt_vars->batt_width = std::pow(batt_volume, 1. / 3.);
		batt_vars->batt_height = std::pow(batt_volume, 1. / 3.);
		
	
		// Inverter model
		batt_vars->inverter_model = as_integer("inverter_model");
		if (batt_vars->inverter_model > 3)
			batt_vars->inverter_efficiency = as_double("inverter_efficiency");

		// Clean up
		delete lifetime_matrix;
		delete capacity_vs_temperature;

		return batt_vars;
	}
	
	void clean_up(batt_variables * batt_vars)
	{
		if (batt_vars) 
			delete batt_vars;
	}

	void exec() throw(general_error)
	{
		if (as_boolean("batt_simple_enable"))
		{

			/* *********************************************************************************************
			Setup problem
			*********************************************************************************************** */
			std::vector<ssc_number_t> p_ac;
			std::vector<ssc_number_t> p_load;

			const double voltage = 500;

			p_ac = as_vector_ssc_number_t("ac");
			util::vector_multiply_scalar<ssc_number_t>(p_ac, static_cast<ssc_number_t>(util::watt_to_kilowatt));
			p_load = as_vector_ssc_number_t("load");

			
			batt_variables * batt_vars = setup_variables(p_ac.size());
			battstor batt(*this, true, p_ac.size(), static_cast<double>(8760 / p_ac.size()), batt_vars);
			batt.initialize_automated_dispatch(p_ac, p_load);
			
			/* *********************************************************************************************
			Run Simulation
			*********************************************************************************************** */
			ssc_number_t *p_gen = allocate("gen", p_ac.size());
			size_t hour = 0;
			int count = 0;

			for (hour = 0; hour < 8760; hour++)
			{
				for (int jj = 0; jj < batt.step_per_hour; jj++)
				{
					batt.initialize_time(0, hour, jj);
					batt.advance(*this, p_ac[count], voltage, p_load[count]);
					p_gen[count] = batt.outGenPower[count];
					count++;
				}
			}
			batt.calculate_monthly_and_annual_outputs(*this);

			clean_up(batt_vars);
		}
		else
			assign("average_battery_roundtrip_efficiency", var_data((ssc_number_t)0.));
	}
};

DEFINE_MODULE_ENTRY(battwatts, "simple battery model", 1)
