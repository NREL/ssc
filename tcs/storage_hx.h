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

// HTF_props.h -- function prototypes for HTF property routines
#ifndef __STORAGE_HX_
#define __STORAGE_HX_

#include "htf_props.h"
//#include <shared/lib_util.h>
#include "lib_util.h"
#include "sam_csp_util.h"

class Storage_HX
{
public:
	Storage_HX();
	
	enum {
		Counter_flow = 2,
		Parallel_flow,
		Cross_flow_unmixed,
		Shell_and_tube};

	bool define_storage( HTFProperties &fluid_field, HTFProperties &fluid_store, bool is_direct, 
		int config, double duty_des, double vol_des, double h_des, 
		double u_des, double tank_pairs_des, double hot_htr_set_point_des, double cold_htr_set_point_des,
		double max_q_htr_cold, double max_q_htr_hot, double dt_hot_des, double dt_cold_des, double T_h_in_des, double T_h_out_des );

	//bool hx_size( HTFProperties &fluid_field, HTFProperties &fluid_store, 
	//	int config, double duty_des, double vol_des, double h_des, 
	//	double u_des, double tank_pairs_des, double hot_htr_set_point_des, double cold_htr_set_point_des, 
	//	double max_q_htr, double dt_hot_des, double dt_cold_des, double T_h_in_des, double T_h_out_des );

	bool mixed_tank( bool is_hot_tank, double dt, double m_prev, double T_prev, double m_dot_in, double m_dot_out, 
						double T_in, double T_amb, double &T_ave, double &vol_ave, 
						double &q_loss, double &T_fin, double &vol_fin, double &m_fin, double &q_heater);

	bool hx_performance( bool is_hot_side_mdot, bool is_storage_side, double T_hot_in, double m_dot_known, double T_cold_in, 
							double &eff, double &T_hot_out, double &T_cold_out, double &q_trans, double &m_dot_solved );

	bool hx_perf_q_transfer(bool is_hot_side_mdot, bool is_storage_side, double T_hot_in, double m_dot_known, double T_cold_in, double &q_trans);

private:
	HTFProperties m_field_htfProps;
	HTFProperties m_store_htfProps;
	int m_config;
	double m_dt_cold_des;
	double m_dt_hot_des;
	double m_vol_des;
	double m_h_des;
	double m_u_des;
	double m_tank_pairs_des;
	double m_Thtr_hot_des;
	double m_Thtr_cold_des;
	double m_a_cs;
	double m_dia;
	double m_ua;
	double m_dot_des;			//[kg/s]  7/9/14 twn: added
	double m_max_q_htr_cold;
	double m_max_q_htr_hot;

	// HX properties
	double m_eff_des;
	double m_UA_des;
										
};

#endif
