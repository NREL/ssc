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

#ifndef __csp_solver_mspt_receiver_222_
#define __csp_solver_mspt_receiver_222_

#include "csp_solver_util.h"

#include "htf_props.h"
#include "ngcc_powerblock.h"
#include "csp_solver_core.h"
#include "csp_solver_pt_receiver.h"

class C_mspt_receiver_222 : public C_pt_receiver
{
// The steady-state receiver (as opposed to the transient, for example)

private:
	ngcc_power_cycle cycle_calcs;

	double m_id_tube;
	double m_A_tube;
	int m_n_t;
	double m_A_rec_proj;
	double m_A_node;
	
	double m_Q_dot_piping_loss;		//[Wt] = Constant thermal losses from piping to env. = (THT*length_mult + length_add) * piping_loss_coef


	int m_itermode;
	double m_od_control;
	double m_eta_field_iter_prev;	//[-] Efficiency from heliostat on last iteration. Maybe change if CR gets defocus signal from controller
	double m_tol_od;

	/* declare storage variables here */
	double m_E_su;
	double m_E_su_prev;
	double m_t_su;
	double m_t_su_prev;

	util::matrix_t<int> m_flow_pattern;
	int m_n_lines;

	util::matrix_t<double> m_flux_in;

	util::matrix_t<double> m_q_dot_inc;

	util::matrix_t<double> m_T_s_guess;
	util::matrix_t<double> m_T_s;
	util::matrix_t<double> m_T_panel_out_guess;
	util::matrix_t<double> m_T_panel_out;
	util::matrix_t<double> m_T_panel_in_guess;
	util::matrix_t<double> m_T_panel_in;
	util::matrix_t<double> m_T_panel_ave;
	util::matrix_t<double> m_T_panel_ave_guess;
	util::matrix_t<double> m_T_film;
	util::matrix_t<double> m_q_dot_conv;
	util::matrix_t<double> m_q_dot_rad;
	util::matrix_t<double> m_q_dot_loss;
	util::matrix_t<double> m_q_dot_abs;

	double m_m_mixed;
	double m_LoverD;
	double m_RelRough;

	// ISCC-specific
	double m_T_amb_low;
	double m_T_amb_high;
	double m_P_amb_low;
	double m_P_amb_high;
	double m_q_iscc_max;

	// track number of calls per timestep, reset = -1 in converged() call
	int m_ncall;

public:
	// Class to save messages for up stream classes
	C_csp_messages csp_messages;

	// Data
	int m_n_panels;					//[-]
	double m_d_rec;					//[m]
	double m_h_rec;					//[m]
	double m_od_tube;				//[mm], convert to [m] in init()
	double m_th_tube;				//[mm], convert to [m] in init()
	double m_hl_ffact;				//[-]
	double m_A_sf;					//[m2]

	// 8.10.2015 twn: add tower piping thermal losses to receiver performance
	double m_pipe_loss_per_m;		//[Wt/m]
	double m_pipe_length_add;		//[m]
	double m_pipe_length_mult;		//[-]

	// 7.13.17 twn: keep this public for now so iscc can calculate
	double m_m_dot_htf_max;			//[kg/s]


	int m_n_flux_x;
	int m_n_flux_y;

		// 4.17.15 twn: former TCS inputs, moved to member data because are constant throughout simulation
	double m_T_salt_hot_target;			//[C], convert to K in init() call
	double m_hel_stow_deploy;			//[-]

		// Added for csp_solver/tcs wrappers:
	int m_field_fl;
	util::matrix_t<double> m_field_fl_props;	
	int m_mat_tube;
	int m_flow_type;
    int m_crossover_shift;

		// ISCC specific
	bool m_is_iscc;
	int m_cycle_config;
	
	S_outputs outputs;

	// Methods
	C_mspt_receiver_222();

	~C_mspt_receiver_222(){};

	virtual void init();

	virtual void call(const C_csp_weatherreader::S_outputs &weather, 
		const C_csp_solver_htf_1state &htf_state_in, 
		const C_pt_receiver::S_inputs &inputs,
		const C_csp_solver_sim_info &sim_info);

	virtual void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

    void calc_pump_performance(double rho_f, double mdot, double ffact, double &PresDrop_calc, double &WdotPump_calc);

    virtual double get_pumping_parasitic_coef();

    virtual double area_proj();

};

#endif // __csp_solver_mspt_receiver_222_