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

class C_mspt_receiver_222
{
private:
	HTFProperties field_htfProps;		// Instance of HTFProperties class for field HTF
	HTFProperties tube_material;		// Instance of HTFProperties class for receiver tube material
	HTFProperties ambient_air;			// Instance of HTFProperties class for ambient air

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
	double m_m_dot_htf_des;

	/* declare storage variables here */
	int m_mode;
	int m_mode_prev;
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

	// member string for exception messages
	std::string error_msg;

	// track number of calls per timestep, reset = -1 in converged() call
	int m_ncall;

public:
	// Class to save messages for up stream classes
	C_csp_messages csp_messages;

	// Data
	int m_n_panels;					//[-]
	double m_d_rec;					//[m]
	double m_h_rec;					//[m]
	double m_h_tower;				//[m]
	double m_od_tube;				//[mm], convert to [m] in init()
	double m_th_tube;				//[mm], convert to [m] in init()
	double m_epsilon;				//[-]
	double m_hl_ffact;				//[-]
	double m_T_htf_hot_des;			//[C], convert to [K] in init()
	double m_T_htf_cold_des;		//[C], convert to [K] in init()
	double m_f_rec_min;				//[-]
	double m_q_rec_des;				//[MW], convert to [W] in init()
	double m_rec_su_delay;			//[-]
	double m_rec_qf_delay;			//[-]
	double m_m_dot_htf_max_frac;	//[-]
	double m_A_sf;					//[m2]

	int m_mode_initial;
	double m_E_su_accum_init;    //Initial accumulated startup energy [MWht]

	// 8.10.2015 twn: add tower piping thermal losses to receiver performance
	double m_pipe_loss_per_m;		//[Wt/m]
	double m_pipe_length_add;		//[m]
	double m_pipe_length_mult;		//[-]

	// 7.13.17 twn: keep this public for now so iscc can calculate
	double m_m_dot_htf_max;			//[kg/s]


	int m_n_flux_x;
	int m_n_flux_y;

	// Calculate in init()
	double m_q_dot_inc_min;			//[Wt]
	// double m_q_rec_min;				//[W]

		// 4.17.15 twn: former TCS inputs, moved to member data because are constant throughout simulation
	double m_T_salt_hot_target;			//[C], convert to K in init() call
	double m_eta_pump;					//[-]
	int m_night_recirc;					//[-]
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
	
	struct S_inputs
	{
		double m_field_eff;					//[-] 
		int m_input_operation_mode;			//[-]

		const util::matrix_t<double> *m_flux_map_input;		//[-]

		S_inputs()
		{
			m_field_eff = std::numeric_limits<double>::quiet_NaN();

			m_input_operation_mode = -1;
		}
	};

		// Let's put outputs in a structure...
	struct S_outputs
	{
		
		double m_m_dot_salt_tot;		//[kg/hr] 
		double m_eta_therm;				//[-] RECEIVER thermal efficiency
		double m_W_dot_pump;			//[MW] 
		double m_q_conv_sum;			//[MW] 
		double m_q_rad_sum;				//[MW] 
		double m_Q_thermal;				//[MW] Thermal power delivered to TES/PC: subtracts piping losses (q_dot_rec - q_dot_piping_losses)
		double m_T_salt_hot;			//[C]
		double m_field_eff_adj;			//[-] Heliostat field efficiency including component defocus
		double m_component_defocus;		//[-] Defocus applied by component model to stay within mass flow or other constraints
		double m_q_dot_rec_inc;			//[MWt] Receiver incident thermal power (after reflection losses)
		double m_q_startup;				//[MWt-hr]
		double m_dP_receiver;			//[bar] receiver pressure drop
		double m_dP_total;				//[bar] total pressure drop
		double m_vel_htf;				//[m/s] HTF flow velocity through receiver tubes
		double m_T_salt_cold;			//[C] 
		double m_m_dot_ss;				//[kg/hr] 
		double m_q_dot_ss;				//[MW] 
		double m_f_timestep;			//[-]
		double m_time_required_su;		//[s]
		double m_q_dot_piping_loss;		//[MWt] Thermal power lost from piping to surroundings 
	
		S_outputs()
		{
			m_m_dot_salt_tot = m_eta_therm = m_W_dot_pump = m_q_conv_sum = m_q_rad_sum = m_Q_thermal =
				m_T_salt_hot = m_field_eff_adj = m_component_defocus = m_q_dot_rec_inc = m_q_startup = m_dP_receiver = m_dP_total =
				m_vel_htf = m_T_salt_cold = m_m_dot_ss = m_q_dot_ss = m_f_timestep = 
				m_time_required_su = m_q_dot_piping_loss = std::numeric_limits<double>::quiet_NaN();
		}
	};

	S_outputs ms_outputs;

	void clear_outputs();
	
	// Methods
	C_mspt_receiver_222();

	~C_mspt_receiver_222(){};

	void init();

	int get_operating_state();

	void call(const C_csp_weatherreader::S_outputs &weather, 
		const C_csp_solver_htf_1state &htf_state_in, 
		const C_mspt_receiver_222::S_inputs &inputs,
		const C_csp_solver_sim_info &sim_info);

	void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_solver_sim_info &sim_info);

	void converged();

    void calc_pump_performance(double rho_f, double mdot, double ffact, double &PresDrop_calc, double &WdotPump_calc);

    HTFProperties *get_htf_property_object();

	double get_remaining_startup_energy();

};
















#endif // __csp_solver_mspt_receiver_222_