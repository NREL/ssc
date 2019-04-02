/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
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
#include "common.h"

static var_info _cm_vtab_mhk_tidal[] = {
	//   VARTYPE			DATATYPE			NAME									LABEL																		UNITS           META            GROUP              REQUIRED_IF					CONSTRAINTS				UI_HINTS	
	{ SSC_INPUT,			SSC_MATRIX,			"tidal_resource_definition",            "Frequency distribution of resource as a function of stream speeds",		"",				"",             "MHKTidal",			"*",						"",						"" },	
	{ SSC_INPUT,			SSC_MATRIX,			"tidal_power_curve",					"Power curve of tidal energy conversion system",							"",				"",             "MHKTidal",			"*",						"",						"" },	
	{ SSC_INPUT,			SSC_NUMBER,			"annual_energy_loss",					"Total energy losses",														"%",			"",             "MHKTidal",			"*",						"",						"" },	
	{ SSC_INPUT,			SSC_NUMBER,			"calculate_capacity",					"Calculate capacity outside UI?",											"0/1",			"",             "MHKTidal",         "*",                      "INTEGER,MIN=0,MAX=1",	"" },

	{ SSC_INOUT,			SSC_NUMBER,			"rated_capacity",						"Rated Capacity of System",													"kW",			"",				"MHKTidal",			"*",						"",						"" },

	{ SSC_OUTPUT,			SSC_NUMBER,			"average_power",						"Average power production",													"kW",			"",				"MHKTidal",			"*",						"",						"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"annual_energy",						"Annual energy production",													"kWh",			"",				"MHKTidal",			"*",						"",						"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"capacity_factor",						"Capacity Factor",															"%",			"",				"MHKTidal",			"*",						"",						"" },
	{ SSC_OUTPUT,			SSC_ARRAY,			"annual_energy_distribution",			"Annual energy production as function of speed",							"kWh",			"",				"MHKTidal",			"*",						"",						"" },
	
	var_info_invalid
};


class cm_mhk_tidal : public compute_module
{
private:
public: 
	cm_mhk_tidal() {
		add_var_info(_cm_vtab_mhk_tidal);
	}
	
	void exec() throw(general_error) {

	//Read and store tidal resource and power curve:
		util::matrix_t<double>  tidal_resource_matrix = as_matrix("tidal_resource_definition");
		util::matrix_t<double>  tidal_power_curve = as_matrix("tidal_power_curve");
		
		//Check to ensure size of _power_vect == _speed_vect : 
		if ( (tidal_power_curve.ncols() * tidal_power_curve.nrows()) != (tidal_resource_matrix.ncols() * tidal_resource_matrix.nrows()) )
			throw compute_module::exec_error("mhk_tidal", "Size of Power Curve is not equal to Tidal Resource");

		//Create vectors to store individual columns from the user input matrix "tidal_resource_definition":
		std::vector<double> _speed_vect;	//Stream speed (u [m/s])
		std::vector<double> _sheer_vect;	// f(z/D = x)
		
		//Vector to store power curve of tidal energy conversion system from "tidal_power_curve":
		std::vector<double> _power_vect;	//Tidal power curve (P [kW])
		
	//Initialize variables to store calculated values:
		//Vector to store annual energy production as function of speed (annual energy production at each stream speed).
		std::vector<double> _annual_energy_distribution;	
		double annual_energy = 0, average_power = 0, sheer_vect_checker = 0, capacity_factor = 0;
		
		//User either sets rated_capacity in the UI, or allows cmod to determine from power curve:
		double rated_capacity = as_double("rated_capacity");
	

		//Storing each column of the tidal_resource_matrix and tidal_power_curve as vectors:
		for (int i = 0; i < (int)tidal_resource_matrix.nrows(); i++) {
			
			_speed_vect.push_back(tidal_resource_matrix.at(i, 0));	
			_sheer_vect.push_back(tidal_resource_matrix.at(i, 1));
			_power_vect.push_back(tidal_power_curve.at(i, 1));

			
			//Store max power if not set in UI:
			if (as_integer("calculate_capacity"))
				if (_power_vect[i] > rated_capacity)
					rated_capacity = _power_vect[i];
			
			//Checker to ensure frequency distribution adds to >= 99.5%:
			sheer_vect_checker += _sheer_vect[i];
		
			//Calculate annual energy production at each stream speed bin:
			_annual_energy_distribution.push_back(_speed_vect[i] * _power_vect[i] * _sheer_vect[i] * 8760);	
			
			//Average Power: 
			average_power = average_power + ( _power_vect[i] * _sheer_vect[i] / 100 );
		}
				
		//Throw exception if sheer vector is < 99.5%
		if (sheer_vect_checker < 99.5)
			throw compute_module::exec_error("mhk_tidal", "Sheer vector does not add up to 100%.");

		//assign _annual_energy_distribution values to ssc variable -> "annual_energy_distribution": 
		ssc_number_t * _aep_distribution_ptr = cm_mhk_tidal::allocate("annual_energy_distribution", _annual_energy_distribution.size());

		for (size_t i = 0; i != _annual_energy_distribution.size(); i++) {
			_aep_distribution_ptr[i] = _annual_energy_distribution[i];	
			annual_energy = annual_energy + _annual_energy_distribution[i];	//Calculate total annual energy.
		}

		//Factoring in losses in total annual energy production:
		annual_energy *= (1 - (as_double("annual_energy_loss") / 100 ));

		//Calculating capacity factor:
		capacity_factor = annual_energy / (rated_capacity * 8760);


		//Assigning values to outputs:
		assign("annual_energy", var_data((ssc_number_t)annual_energy));
		assign("average_power", var_data((ssc_number_t)average_power));
		assign("rated_capacity", var_data((ssc_number_t)rated_capacity));
		assign("capacity_factor", var_data((ssc_number_t)capacity_factor));
	}
};

DEFINE_MODULE_ENTRY( mhk_tidal , "MHK Tidal power calculation model using power distribution.", 3);



