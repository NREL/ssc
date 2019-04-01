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
static var_info _cm_vtab_mhk_wave[] = {
	//   VARTYPE			DATATYPE			NAME									LABEL																UNITS           META            GROUP              REQUIRED_IF					CONSTRAINTS			UI_HINTS	
	{ SSC_INPUT,			SSC_MATRIX,			"wave_resource_definition",				"Frequency distribution of resource as a function of Hs and Te",	"",				"",             "MHKWave",			"*",						"",					"" },
	{ SSC_INPUT,			SSC_MATRIX,			"wave_power_curve",						"Wave Power Matrix",												"",				"",             "MHKWave",			"*",						"",					"" },
	
	{ SSC_OUTPUT,			SSC_NUMBER,			"average_power",						"Average power production",											"",				"",				"MHKWave",			"?",						"",					"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"annual_energy",						"Annual energy production",											"",				"",				"MHKWave",			"?",						"",					"" },
	{ SSC_OUTPUT,			SSC_MATRIX,			"annual_energy_distribution",			"Annual energy production as function of Hs and Te",					"",				"",				"MHKWave",			"?",						"",					"" },
};


class cm_mhk_wave : public compute_module
{
private:
public:
	cm_mhk_wave() {
		add_var_info(_cm_vtab_mhk_wave);
	}

	void exec() throw(general_error) {

		//Read and store wave resource as a 2D matrix of vectors:
		util::matrix_t<double>  wave_resource_matrix = as_matrix("wave_resource_definition");
		std::vector<std::vector<double> > _resource_vect;	//Initialize wave power curve of size specified by user.
		_resource_vect.resize(wave_resource_matrix.nrows() * wave_resource_matrix.ncols());
		
		//Read and store power curve as a 2D matrix of vectors:
		util::matrix_t<double>  wave_power_matrix = as_matrix("wave_power_curve");
		std::vector<std::vector<double> > _power_vect;	//Initialize wave power curve of size specified by user.
		_power_vect.resize(wave_power_matrix.nrows() * wave_power_matrix.ncols());

		//2D vector matrix to store energy production
		util::matrix_t<double>  wave_energy_matrix(wave_resource_matrix.nrows() , wave_resource_matrix.ncols());
		
		for (size_t i = 0; i < (size_t)wave_power_matrix.nrows(); i++) {
			for (size_t j = 0; j < (size_t)wave_power_matrix.ncols(); j++) {
				_resource_vect[i].push_back(wave_resource_matrix.at(i, j));
				_power_vect[i].push_back(wave_power_matrix.at(i , j));
			}
		}		

		ssc_number_t * _aep_distribution_ptr = cm_mhk_wave::allocate("annual_energy_distribution", wave_resource_matrix.nrows() , wave_resource_matrix.ncols() );
		for (size_t i = 0; i < (size_t)wave_power_matrix.nrows(); i++) {
			for (size_t j = 0; j < (size_t)wave_power_matrix.ncols(); j++) {
				_aep_distribution_ptr = _resource_vect[i][j] * _power_vect[i][j];
				//_aep_distribution_ptr[i][j] = wave_energy_matrix.at(i, j);
			}
		}
	}
};

DEFINE_MODULE_ENTRY(mhk_wave, "MHK Wave power calculation model using power distribution.", 3);