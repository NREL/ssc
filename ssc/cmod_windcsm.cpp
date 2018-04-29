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
	{ SSC_INPUT,        SSC_NUMBER,      "turbine_class",					"Turbine class",                                          "",     "",                      "wind_csm",    "?=0", "INTEGER,MIN=0,MAX=3", "" },
	{ SSC_INPUT,        SSC_NUMBER,      "turbine_user_exponent",					"Turbine user exponent",                                          "",     "",                      "wind_csm",    "?=2.5", "", "" },
	{ SSC_INPUT,		SSC_NUMBER, "turbine_carbon_blades", "Turbine carbon blades", "0/1", "", "wind_csm", "?=0", "INTEGER,MIN=0,MAX=1", "" },
	{ SSC_INPUT,		SSC_NUMBER, "turbine_rotor_diameter", "Turbine rotor diameter", "m", "", "wind_csm", "*", "", "" },

	{ SSC_INPUT,		SSC_NUMBER, "machine_rating", "Machine rating", "kW", "", "wind_csm", "*", "", "" },

	{ SSC_INPUT,		SSC_NUMBER, "rotor_torque", "Rotor torque", "Nm", "", "wind_csm", "*", "", "" },

	{ SSC_INPUT,		SSC_NUMBER, "onbard_crane", "Onboard crane", "0/1", "", "wind_csm", "?=0", "INTEGER,MIN=0,MAX=1", "" },

	{ SSC_INPUT,		SSC_NUMBER, "hub_height", "Hub height", "m", "", "wind_csm", "*", "", "" },

	{ SSC_INPUT,		SSC_NUMBER, "num_blades", "Number of blades", "", "", "wind_csm", "?=3", "INTEGER,MIN=1", "" },

	// Outputs intermediate percentages and cost breakdown and total cost
	{ SSC_OUTPUT,       SSC_NUMBER,      "rotor_mass",             "Rotor mass",                                 "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "rotor_cost",             "Rotor cost",                                 "$",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "drivetrain_mass",        "Drivetrain mass",                            "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "drivetrain_cost",             "Drivetrain cost",                                 "$",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "tower_mass",             "Tower mass",                                 "kg",     "",                      "wind_csm",      "*",                       "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "tower_cost",             "Tower cost",                                 "$",     "",                      "wind_csm",      "*",                       "",                              "" },

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
		// mass values from nrel_csm_tcc_2015.py
		// Cost values from turbine_costsse_2015.py


		// get values
		double blades = (double)as_number("blades");
		double turbine_user_exponent = (double)as_number("turbine_user_exponent");
		double turbine_rotor_diameter = (double)as_number("turbine_rotor_diameter");
		bool turbine_carbon_blades = as_integer("turbine_carbon_blades")==1;
		int turbine_class = as_integer("turbine_class");
		double exponent = 0;
		switch (turbine_class)
		{
		case 0:
			exponent = turbine_user_exponent;
			break;
		case 1:
		{
			if (turbine_carbon_blades)
				exponent = 2.47;
			else
				exponent = 2.54;
		}
			break;
		case 2:
		case 3:
		{
			if (turbine_carbon_blades)
				exponent = 2.44;
			else
				exponent = 2.50;
		}
		break;
		default:
			exponent = 2.5;
		}
		double blade_mass = 0.5 * pow((0.5*turbine_rotor_diameter), exponent);
		double blade_mass_cost_coeff = 14.6; // line 27
		double blade_mass_cost_coeff_2015 = 13.08; // line 152
		double blade_cost_2015 = blade_mass_cost_coeff * blade_mass;

		// hub mass
		double hub_mass = 2.3 * blade_mass + 1320.0;
		double hub_mass_cost_coeff = 3.9; // line 45
		double hub_mass_cost_coeff_2015 = 3.8; // line 154
		double hub_cost_2015 = hub_mass_cost_coeff * hub_mass;

		// pitch mass
		double pitch_bearing_mass = 0.1295 * blade_mass * blades + 491.31;
		double pitch_mass = pitch_bearing_mass * (1.0 + 0.3280) + 555.0;
		double pitch_mass_cost_coeff = 22.1; // line 63
		double pitch_mass_cost_coeff_2015 = 22.91; // line 156
		double pitch_cost_2015 = pitch_mass_cost_coeff * pitch_mass;

		// spinner mass
		double spinner_mass = 15.5 * turbine_rotor_diameter - 980.0;
		double spinner_mass_cost_coeff = 11.1; // line 81
		double spinner_mass_cost_coeff_2015 = 15.59; // line 158
		double spinner_cost_2015 = spinner_mass_cost_coeff * spinner_mass;


		double rotor_mass = blade_mass + hub_mass + pitch_mass + spinner_mass;
		assign( "rotor_mass", var_data(ssc_number_t(rotor_mass)) );

		// cost adders
		double hub_assembly_cost_multiplier = 0.0;
		double hub_overhead_cost_multiplier = 0.0;
		double hub_profit_multiplier = 0.0;
		double hub_transport_multiplier = 0.0;

		double parts_cost = hub_cost_2015 + pitch_cost_2015 + spinner_cost_2015;
		double hub_system_cost_adder_2015 = (1.0 + hub_transport_multiplier + hub_profit_multiplier)
			* ((1.0 + hub_overhead_cost_multiplier + hub_assembly_cost_multiplier) * parts_cost);

		int num_blades = as_integer("num_blades");
		double rotor_cost_added_2015 = blade_cost_2015 * (double)num_blades + hub_system_cost_adder_2015;



		///////////////////////////////////////////////////////////////////////////////////////
		// Drivetrain - 13 subcomponents

		double machine_rating = as_double("machine_rating"); //kW

		double low_speed_shaft_mass = 13.0 * pow((blade_mass *  machine_rating / 1000.0), 0.65) + 775.0;
		double low_speed_shaft_mass_cost_coeff = 11.9;; // line 211
		double low_speed_shaft_cost = low_speed_shaft_mass_cost_coeff * low_speed_shaft_mass;

		double bearing_mass = 0.0001 * pow(turbine_rotor_diameter, 3.5);

		double rotor_torque = as_double("rotor_torque");

		double gearbox_mass = 113.0 * pow(rotor_torque / 1000.0, 0.71);

		double high_speed_side_mass = 0.19894 * machine_rating;

		double generator_mass = 2300.0 * machine_rating / 1000.0 + 3400.0;

		double bedplate_mass = pow(turbine_rotor_diameter, 2.2);

		double yaw_system_mass = 1.5 * (0.0009 * pow(turbine_rotor_diameter, 3.314));

		double hydraulic_cooling_mass = 0.08 * machine_rating;

		double nacelle_cover_mass = 1.2817 * machine_rating + 428.19;

		bool onboard_crane = as_integer("onboard_crane") == 1;

		double crane_mass = 0.0;
		if (onboard_crane) crane_mass = 3000.0; //kg

		double nacelle_platforms_mass = 0.125 * bedplate_mass;

		double other_mass = nacelle_platforms_mass + crane_mass;

		double transformer_mass = 1915.0 * machine_rating / 1000.0 + 1910.0;

		double drivetrain_mass = low_speed_shaft_mass + bearing_mass + gearbox_mass + high_speed_side_mass
			+ generator_mass + bedplate_mass + yaw_system_mass + hydraulic_cooling_mass
			+ nacelle_cover_mass + other_mass + transformer_mass;

		assign("drivetrain_mass", var_data(ssc_number_t(drivetrain_mass)));
		// cost


		//////////////////////////////////////////////////////////////////////////////////////
		// tower mass
		double hub_height = as_double("hub_height");

		double tower_mass = 19.828 * pow(hub_height, 2.0282);

		assign("tower_mass", var_data(ssc_number_t(tower_mass)));
	

		// assign outputs
//		assign( "rotor_cost", var_data(output) );
	}
};

DEFINE_MODULE_ENTRY( windcsm, "WISDEM turbine cost model", 1 )
