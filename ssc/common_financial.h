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

#ifndef __common_financial_h
#define __common_financial_h

#include <vector>
#include "core.h"


void save_cf(compute_module *cm, util::matrix_t<double>& mat, int cf_line, int nyears, const std::string &name);



class dispatch_calculations
{
private:
	compute_module *m_cm;
	std::vector<int> m_periods;
	std::string m_error;
	util::matrix_t<double> m_cf;
	std::vector<double> m_degradation;
	std::vector<double> m_hourly_energy;
	int m_nyears;
	bool m_timestep;
	ssc_number_t *m_gen;
	ssc_number_t *m_multipliers;
	size_t m_ngen;
	size_t m_nmultipliers;

public:
	dispatch_calculations() {};
	dispatch_calculations(compute_module *cm, std::vector<double>& degradation, std::vector<double>& hourly_energy);
	bool init(compute_module *cm, std::vector<double>& degradation, std::vector<double>& hourly_energy);
	bool setup();
	bool setup_ts();
	bool compute_outputs(std::vector<double>& ppa);
	bool compute_outputs_ts(std::vector<double>& ppa);
	int operator()(size_t time);
	std::string error() { return m_error; }
	bool process_dispatch_output();
	bool compute_dispatch_output();
	bool process_lifetime_dispatch_output();
	bool compute_lifetime_dispatch_output();
	bool compute_dispatch_output_ts();
	bool compute_lifetime_dispatch_output_ts();
	util::matrix_t<double>& dispatch_output();
	double tod_energy(int period, int year);
	double tod_energy_value(int period, int year);
	double tod_energy_value(int year);
};


class hourly_energy_calculation
{
private:
	compute_module *m_cm;
	std::vector<double> m_hourly_energy;
	std::string m_error;
	size_t m_nyears;

public:
	bool calculate(compute_module *cm);
	std::vector<double>& hourly_energy() {
		return m_hourly_energy;
	}
	std::string error() { return m_error; }
};




/*
extern var_info vtab_advanced_financing_cost[];


class advanced_financing_cost
{
private:
	compute_module *m_cm;

public:
	advanced_financing_cost(compute_module *cm);
	bool compute_cost(double cost_installed, double equity, double debt, double cbi, double ibi);
};
*/


#endif

