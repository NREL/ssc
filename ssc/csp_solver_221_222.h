#ifndef __csp_solver_221_222_
#define __csp_solver_221_222_

#include "csp_solver_core.h"

#include "lib_util.h"
#include "lib_weatherfile.h"
#include <algorithm>

#include "interpolation_routines.h"
#include "AutoPilot_API.h"
#include "IOUtil.h"
#include "sort_method.h"

#include "ngcc_powerblock.h"

class C_csp_mspt_221_222 : public C_csp_collector_receiver
{
private:
	// private member data from 'sam_mw_pt_heliostat'

	// Class Instances
	GaussMarkov *field_efficiency_table;
	// Flux table
	sp_flux_table fluxtab;

	//Parameters
	string weather_file;
	int run_type;
	double helio_width;
	double helio_height;
	double helio_optical_error;
	double helio_active_fraction;
	double dens_mirror;
	double helio_reflectance;
	double rec_absorptance;
	double rec_height;
	double rec_aspect;
	double rec_hl_perm2;
	double q_design;
	double h_tower;
	int land_bound_type;
	double land_max;
	double land_min;
	double* land_bound_table;
	double* land_bound_list;
	double p_start;
	double p_track;
	double hel_stow_deploy;
	double v_wind_max;
	double interp_nug;
	double interp_beta;
	double* helio_positions;
	double* helio_aim_points;
	int N_hel, pos_dim;
	double* eta_map;
	int n_flux_x;
	int n_flux_y;
	double* flux_positions;
	MatDoub m_flux_positions;
	double* flux_maps;
	double* flux_map;
	double c_atm_0, c_atm_1, c_atm_2, c_atm_3;
	int n_facet_x, n_facet_y;
	int cant_type, focus_type;
	int n_flux_days, delta_flux_hrs;
	double dni_des;

	//Stored Variables
	double eta_prev;
	double v_wind_prev;

	// ******************************************************
	// ******************************************************
	// ******************************************************
		// Type 222 MSPT Member Data
	HTFProperties field_htfProps;		// Instance of HTFProperties class for field HTF
	HTFProperties tube_material;		// Instance of HTFProperties class for receiver tube material
	HTFProperties ambient_air;			// Instance of HTFProperties class for ambient air

	ngcc_power_cycle cycle_calcs;

	int m_n_panels;
	double m_d_rec;
	double m_h_rec;
	double m_h_tower;
	double m_od_tube;
	double m_th_tube;
	double m_epsilon;
	double m_hl_ffact;
	double m_T_htf_hot_des;
	double m_T_htf_cold_des;
	double m_f_rec_min;
	double m_q_rec_des;
	double m_rec_su_delay;
	double m_rec_qf_delay;
	double m_m_dot_htf_max;
	double m_A_sf;

	double m_id_tube;
	double m_A_tube;
	int m_n_t;
	int m_n_flux_x;
	int m_n_flux_y;

	double m_A_rec_proj;
	double m_A_node;

	int m_itermode;
	double m_od_control;
	double m_tol_od;
	double m_m_dot_htf_des;
	double m_q_rec_min;

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

	//T_sX(N_panels),T_panel_outX(N_panels),T_panel_inX(N_panels)
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
	bool m_is_iscc;
	int m_cycle_config;
	double m_T_amb_low;
	double m_T_amb_high;
	double m_P_amb_low;
	double m_P_amb_high;
	double m_q_iscc_max;
	double *m_i_flux_map;

	virtual void exec() throw(general_error)
	{

	}

	void init_rec(double time_step);

public:
	struct RUN_TYPE {
		enum A {
			AUTO, USER_FIELD, USER_DATA
		};
	};
	
	C_csp_mspt_221_222();

	virtual ~C_csp_mspt_221_222();

	virtual void init();

	int relay_message(string &msg, double percent);

};








#endif	// __csp_solver_221_222_