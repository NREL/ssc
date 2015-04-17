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

	const double *m_i_flux_map;

	// member string for exception messages
	std::string error_msg;

public:
	// Class to save messages for up stream classes
	C_csp_messages csp_messages;

	// Data
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

	int m_n_flux_x;
	int m_n_flux_y;

		// Added for csp_solver/tcs wrappers:
	int m_field_fl;
	util::matrix_t<double> m_field_fl_props;	
	int m_mat_tube;
	int m_flow_type;		

		// ISCC specific
	bool m_is_iscc;
	int m_cycle_config;
	
	
		// Let's put outputs in a structure...
	struct S_outputs
	{
		double m_m_dot_salt_tot;
		double m_eta_therm;
		double m_W_dot_pump;
		double m_q_conv_sum;
		double m_q_rad_sum;
		double m_Q_thermal;
		double m_T_salt_hot;
		double m_field_eff_adj;
		double m_Q_solar_total;
		double m_q_startup;
		double m_dP_receiver;
		double m_dP_total;
		double m_vel_htf;
		double m_T_salt_cold;
		double m_m_dot_ss;
		double m_q_dot_ss;
		double m_f_timestep;
	
		S_outputs()
		{
			m_m_dot_salt_tot = m_eta_therm = m_W_dot_pump = m_q_conv_sum = m_q_rad_sum = m_Q_thermal =
				m_T_salt_hot = m_field_eff_adj = m_Q_solar_total = m_q_startup = m_dP_receiver = m_dP_total =
				m_vel_htf = m_T_salt_cold = m_m_dot_ss = m_q_dot_ss = m_f_timestep = std::numeric_limits<double>::quiet_NaN();
		}
	};

	S_outputs outputs;

	void clear_outputs();
	
	// Methods
	C_mspt_receiver_222();

	~C_mspt_receiver_222(){};

	void init();

	void call(const C_csp_weatherreader::S_outputs *p_weather, double T_salt_hot_target, double T_salt_cold_in, double eta_pump, double field_eff, int night_recirc,
		double hel_stow_deploy, const double * i_flux_map, int n_flux_y, int n_flux_x, const C_csp_solver_sim_info *p_sim_info);

	//void call(double azimuth, double zenith, double T_salt_hot_target, double T_salt_cold_in, double v_wind_10, double P_amb,
	//	double eta_pump, double T_dp, double I_bn, double field_eff, double T_amb, int night_recirc,
	//	double hel_stow_deploy, const double * i_flux_map, int n_flux_y, int n_flux_x, double time, int ncall, double step);

	void converged();

};
















#endif // __csp_solver_mspt_receiver_222_