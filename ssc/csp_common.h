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

#ifndef _CSP_COMMON_
#define _CSP_COMMON_ 1
#include <memory>

#include "core.h"
#include "AutoPilot_API.h"
#include "lib_weatherfile.h"

#include "sco2_pc_csp_int.h"

class solarpilot_invoke : public var_map
{
    compute_module *m_cmod;
    AutoPilot_S *m_sapi;
	std::vector<std::vector<double> > _optimization_sim_points;
	std::vector<double>
		_optimization_objectives,
		_optimization_fluxes;

public:

	void getOptimizationSimulationHistory(std::vector<std::vector<double> > &sim_points, std::vector<double> &obj_values, std::vector<double> &flux_values);
	void setOptimizationSimulationHistory(std::vector<std::vector<double> > &sim_points, std::vector<double> &obj_values, std::vector<double> &flux_values);

    //sp_optimize opt;
    /*sp_ambient amb;
    sp_cost cost;
    sp_heliostats helios;
    sp_receivers recs;*/
    sp_layout layout;
    sp_flux_table fluxtab;
    sp_layout_table heliotab;

    solarpilot_invoke( compute_module *cm );
    ~solarpilot_invoke();
    AutoPilot_S *GetSAPI();
    bool run(std::shared_ptr<weather_data_provider> wdata = nullptr);
    bool postsim_calcs( compute_module *cm );
};

bool ssc_cmod_solarpilot_callback(simulation_info *siminfo, void *data);

extern var_info vtab_sco2_design[];

int sco2_design_cmod_common(compute_module *cm, C_sco2_recomp_csp & c_sco2_cycle);





#endif