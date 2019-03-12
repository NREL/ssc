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

#ifndef __common_h
#define __common_h

#include <memory>
#include <vector>
#include "core.h"

#include "../shared/lib_util.h"
#include "../shared/lib_weatherfile.h"
#include "../shared/lib_pv_shade_loss_mpp.h"

extern var_info vtab_standard_financial[];
extern var_info vtab_standard_loan[];
extern var_info vtab_oandm[];
extern var_info vtab_depreciation[];
extern var_info vtab_tax_credits[];
extern var_info vtab_payment_incentives[];

extern var_info vtab_adjustment_factors[];
extern var_info vtab_dc_adjustment_factors[];
extern var_info vtab_sf_adjustment_factors[];
extern var_info vtab_technology_outputs[];

class adjustment_factors
{
	compute_module *m_cm;
	std::vector<float> m_factors;
	std::string m_error;
	std::string m_prefix;
public:
	adjustment_factors(compute_module *cm, const std::string &prefix);
	bool setup(int nsteps=8760);
	float operator()(size_t time);
	std::string error() { return m_error; }
};

class sf_adjustment_factors
{
	compute_module *m_cm;
	std::vector<float> m_factors;
	std::string m_error;
public:
	sf_adjustment_factors(compute_module *cm);
	bool setup(int nsteps=8760);
    int size();
	float operator()(size_t time);
	std::string error() { return m_error; }
};


class shading_factor_calculator
{
	std::vector<std::string> m_errors;
	util::matrix_t<double> m_azaltvals;
	bool m_enAzAlt;
	double m_diffFactor;

	// shading database mods
	int m_string_option;// 0=shading db, 1=average, 2=max, 3=min
	//ShadeDB8_mpp *m_db8;
	//std::unique_ptr<ShadeDB8_mpp> m_db8;
	double m_beam_shade_factor;
	double m_dc_shade_factor;

	// subhourly modifications
	int m_steps_per_hour;
	bool m_enTimestep;
	util::matrix_t<double> m_beamFactors;
	bool m_enMxH;
	util::matrix_t<double> m_mxhFactors;

public:
	shading_factor_calculator();
	bool setup(compute_module *cm, const std::string &prefix = "");
	std::string get_error(size_t i = 0);

	size_t get_row_index_for_input(size_t hour, size_t hour_step, size_t steps_per_hour);
	bool use_shade_db();

	// beam and diffuse loss factors (0: full loss, 1: no loss )
	bool fbeam(size_t hour, double solalt, double solazi, size_t hour_step = 0, size_t steps_per_hour = 1);
	// shading database instantiated once outside of shading factor calculator
	bool fbeam_shade_db(ShadeDB8_mpp * p_shadedb, size_t hour, double solalt, double solazi, size_t hour_step = 0, size_t steps_per_hour = 1, double gpoa = 0.0, double dpoa = 0.0, double pv_cell_temp = 0.0, int mods_per_str = 0, double str_vmp_stc = 0.0, double mppt_lo = 0.0, double mppt_hi = 0.0);

	double fdiff();

	double beam_shade_factor();
	double dc_shade_factor();
};

class weatherdata : public weather_data_provider
{
	std::vector< weather_record* > m_data;
	std::vector<size_t> m_columns;

	struct vec {
		ssc_number_t *p;
		size_t len;
	};

	vec get_vector(var_data *v, const char *name, size_t *len = nullptr);
	ssc_number_t get_number(var_data *v, const char *name);

	int name_to_id(const char *name);

public:
	/* Detects file format, read header information, detects which data columns are available and at what index
	and read weather record information.
	If wet-bulb temperature or dew point are missing, calculate using tdry, pres & rhum or tdry & rhum, respectively.
	Interpolates meteorological data if requested.*/
	weatherdata(var_data *data_table);
	virtual ~weatherdata();

	void set_counter_to(size_t cur_index);
	bool read(weather_record *r); // reads one more record	
	bool read_average(weather_record *r, std::vector<int> &cols, size_t &num_timesteps); // reads one more record	
	bool has_data_column(size_t id);
};

bool ssc_cmod_update(std::string &log_msg, std::string &progress_msg, void *data, double progress, int out_type);

#endif

