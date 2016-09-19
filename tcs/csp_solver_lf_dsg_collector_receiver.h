#ifndef __csp_solver_lf_dsg_collector_receiver_
#define __csp_solver_lf_dsg_collector_receiver_

#include "csp_solver_core.h"
#include "htf_props.h"
#include "tcstype.h"

#include "lib_weatherfile.h"
#include <cmath>
#include "sam_csp_util.h"
#include "water_properties.h"

#include "numeric_solvers.h"

#include <iostream>
#include <fstream>

class C_csp_lf_dsg_collector_receiver : public C_csp_collector_receiver
{
private:

	util::matrix_t<AbsorberProps*> m_AbsorberMaterial;
	util::matrix_t<HTFProperties*> m_AnnulusGas;


	int m_operating_mode_converged;
	int m_operating_mode;

public:

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	//parameters and inputs
	// Class Instances
	emit_table eps_abs;
	OpticalDataTable b_optical_table;
	OpticalDataTable sh_optical_table;
	TwoOptTables optical_tables;
	P_max_check check_pressure;
	enth_lim check_h;
	water_state wp;
	Evacuated_Receiver evac_tube_model;
	HTFProperties htfProps;

	// Parameters
	double m_ncall;
	double m_dt;
	int m_touperiod;
	double m_tes_hours;
	double m_q_max_aux;
	double m_LHV_eff;
	double m_T_set_aux;
	double m_T_field_in_des;
	double m_T_field_out_des;
	double m_x_b_des;
	double m_P_turb_des;
	double m_fP_hdr_c;
	double m_fP_sf_boil;
	double m_fP_boil_to_sh;
	double m_fP_sf_sh;
	double m_fP_hdr_h;
	double m_q_pb_des;
	double m_W_pb_des;
	double m_cycle_max_fraction;
	double m_cycle_cutoff_frac;
	double m_t_sby_des;
	double m_q_sby_frac;
	double m_solarm;
	double m_PB_pump_coef;
	double m_PB_fixed_par;
	//double * m_bop_array;
	//int m_l_bop_array;
	std::vector<double> m_bop_array;
	//double * m_aux_array;
	//int m_l_aux_array;
	std::vector<double> m_aux_array;
	double m_T_startup;

	int m_fossil_mode;
	double m_I_bn_des;
	bool m_is_sh;
	double m_is_oncethru;
	double m_is_multgeom;
	int m_nModBoil;
	int m_nModSH;
	int m_nLoops;
	double m_eta_pump;
	double m_latitude;
	double m_theta_stow;
	double m_theta_dep;
	double m_m_dot_min;
	double m_T_field_ini;
	double m_T_fp;
	double m_Pipe_hl_coef;
	double m_SCA_drives_elec;
	double m_ColAz;
	double m_e_startup;
	double m_T_amb_des_sf;
	double m_V_wind_max;
	//double * m_ffrac;
	//int    m_l_ffrac;
	std::vector<double> m_ffrac;

	util::matrix_t<double> m_A_aperture;
	util::matrix_t<double> m_L_col;
	util::matrix_t<double> m_OptCharType;
	util::matrix_t<double> m_IAM_T;
	util::matrix_t<double> m_IAM_L;
	util::matrix_t<double> m_TrackingError;
	util::matrix_t<double> m_GeomEffects;
	util::matrix_t<double> m_rho_mirror_clean;
	util::matrix_t<double> m_dirt_mirror;
	util::matrix_t<double> m_error;
	util::matrix_t<double> m_HLCharType;
	util::matrix_t<double> m_HL_dT;
	util::matrix_t<double> m_HL_W;
	util::matrix_t<double> m_D_2;
	util::matrix_t<double> m_D_3;
	util::matrix_t<double> m_D_4;
	util::matrix_t<double> m_D_5;
	util::matrix_t<double> m_D_p;
	util::matrix_t<double> m_Rough;
	util::matrix_t<double> m_Flow_type;
	util::matrix_t<double> m_AbsorberMaterial_in;
	util::matrix_t<double> m_b_eps_HCE1;
	util::matrix_t<double> m_b_eps_HCE2;
	util::matrix_t<double> m_b_eps_HCE3;
	util::matrix_t<double> m_b_eps_HCE4;
	util::matrix_t<double> m_sh_eps_HCE1;
	util::matrix_t<double> m_sh_eps_HCE2;
	util::matrix_t<double> m_sh_eps_HCE3;
	util::matrix_t<double> m_sh_eps_HCE4;
	util::matrix_t<double> m_HCE_FieldFrac;
	util::matrix_t<double> m_alpha_abs;
	util::matrix_t<double> m_alpha_env;
	util::matrix_t<double> m_EPSILON_4;
	util::matrix_t<double> m_Tau_envelope;
	util::matrix_t<bool> m_GlazingIntactIn;
	util::matrix_t<double> m_AnnulusGas_in;
	util::matrix_t<double> m_P_a;
	util::matrix_t<double> m_Design_loss;
	util::matrix_t<double> m_Shadowing;
	util::matrix_t<double> m_Dirt_HCE;
	util::matrix_t<double> m_b_OpticalTable;
	util::matrix_t<double> m_sh_OpticalTable;

	////Type 261 (solar field collector) inputs
	//double m_dnifc;					//[W/m2] Forecast DNI
	//double m_I_bn;						//[W/m2] Current DNI
	//double m_T_db;				//[K] Dry bulb temp, convert from C
	//double m_T_dp;				//[K] Dewpoint temp, convert from C
	//double m_P_amb;				//[Pa] Ambient pressure, convert from mbar
	//double m_V_wind;					//[m/s] Ambient windspeed
	//double m_m_dot_htf_ref;	//[kg/s] Reference HTF flow rate at design conditions, convert from kg/hr
	//double m_m_pb_demand;		//[kg/s] Demand HTF flow from the power block, convert from kg/hr
	//double m_shift;			//[deg] Shift in longitude from local standard meridian
	//double m_SolarAz;				//[deg] Solar azimuth angle
	//double m_SolarZen;		//Solar zenith angle [deg]
	//double m_T_pb_out;
	
	// Stored Variables
	util::matrix_t<double> m_T_ave, m_T_ave0, m_h_ave, m_h_ave0;

	// Constants
	double m_P_max;
	double m_fP_turb_min;
	double m_d2r;
	double m_r2d;
	double m_mtoinch;
	double m_T_htf_prop_min;
	//int	m_accept_loc;

	// Calculated
	util::matrix_t<double> m_D_h;
	util::matrix_t<double> m_A_cs;
	util::matrix_t<double> m_EPSILON_5;
	util::matrix_t<double> m_q_inc, m_q_loss, m_q_abs, m_h_in, m_h_out, m_x, m_q_rec;
	util::matrix_t<double> m_eta_opt_fixed, m_opteff_des;
	double m_fP_sf_tot;
	int m_n_rows_matrix;
	int m_nModTot;
	double m_Ap_tot;
	double m_m_dot_des;
	double m_q_rec_tot_des;
	double m_m_dot_max;
	double m_m_dot_pb_des;
	double m_e_trans;
	double m_m_dot_b_max;
	double m_m_dot_b_des;
	double m_m_dot_htf_tot;
	double m_P_out_des;

	double m_eta_opt_ave;
	double m_q_rec_loop;
	double m_q_inc_loop;

	// Stored
	vector<double> m_T_ave_prev;
	double m_defocus_prev;
	double m_t_sby_prev;
	double m_t_sby;
	bool   m_is_pb_on_prev;
	bool   m_is_pb_on;
	double m_T_sys_prev;
	double m_T_field_in;
	double m_T_field_out;
	double m_h_field_in;
	double m_h_field_out;
	double m_xb_field_out;

	// Calculated - ncall = 0
	util::matrix_t<double> m_eta_optical;
	double m_defocus;
	double m_defocus_new;
	double m_defocus_old;
	bool m_is_def;
	double m_err_def;
	double m_tol_def;
	double m_rc;
	double phi_t;
	double theta_L;
	double m_ftrack;

	// *********************************************
	// *********************************************

	// *********************************************
	// CSP Solver Temperature Tracking
	// Temperatures from the most recent converged() operation
	double m_T_sys_c_t_end_converged;
	std::vector<double> m_T_htf_out_t_end_converged;
	double m_T_sys_h_t_end_converged;
	// ** Check for these in other methods developed for CSP Solver **

	// Temperatures from the most recent timstep (in the event that a method solves multiple, shorter timesteps
	double m_T_sys_c_t_end_last;			//[K] Temperature (bulk) of cold runners & headers at end of previous timestep
	std::vector<double> m_T_htf_out_t_end_last;			//[K] Temperature of HTF temperature & material at end of previous timestep
	double m_T_sys_h_t_end_last;			//[K] Temperature (bulk) of hot runners & headers at end of previous timestep

	// ** Check for these in other methods developed for CSP Solver **

	// Latest temperature solved during present call to this class
	double m_T_sys_c_t_end;				//[K] Temperature (bulk) of cold runners & headers at end of current timestep
	//double m_T_sys_c_t_int;				//[K] Temperature (bulk) of cold runners & headers at time-INTegrated-average	
	//std::vector<double> m_T_htf_in_t_int;	//[K] time-integrated-average inlet HTF temperature to each SCA
	//std::vector<double> m_T_htf_out_t_end;	//[K] end-of-timestep outlet HTF temperature of each SCA
	//std::vector<double> m_T_htf_out_t_int;	//[K] time-integrated-average outlet HTF temp of each SCA
	double m_T_sys_h_t_end;				//[K] Temperature (bulk) of hot runners & headers at end of current timestep
	//double m_T_sys_h_t_int;				//[K] Temperature (bulk) of hot runners & headers at timestep-integrated-average

	double m_Q_field_losses_total;		//[MJ] scas + xover + hot_HR + cold_HR
	double m_c_htf_ave_ts_ave_temp;		//[J/kg-K] integrated-averaged cp over T_htf_cold_in, m_T_sys_h_t_in
	
	int m_nSCA;
	double m_m_dot_htfmin;
	double m_m_dot_htfmax;

	// DSG system-specific -start
	std::vector<double> m_h_htf_out_t_end;	// end-of-timestep outlet HTF enthalpy of each SCA
	std::vector<double> m_h_htf_t_ave;	// end-of-timestep average HTF enthapy of each SCA
	std::vector<double> m_T_htf_out_t_end;	// end-of-timestep outlet HTF temperature of each SCA
	std::vector<double> m_T_htf_t_ave;	// end-of-timestep average HTF temperature of each SCA
	std::vector<double> m_P_htf_out_t_end;	// end-of-timestep outlet HTF pressure of each SCA
	std::vector<double> m_P_htf_t_ave;	// end-of-timestep average HTF pressure of each SCA
	std::vector<double> m_xb_htf_out_t_end;	// end-of-timestep outlet HTF steam quality of each SCA
	std::vector<double> m_xb_htf_t_ave;	// end-of-timestep average HTF steam quality of each SCA

	// enthalpy from the most recent converged() operation
	double m_h_sys_c_t_end_converged;
	double m_h_sys_c_t_end;
	std::vector<double> m_h_htf_out_t_end_converged;
	std::vector<double> m_T_htf_t_ave_converged;
	double m_h_sys_h_t_end_converged;
	double m_h_sys_h_t_end;
	// ** Check for these in other methods developed for CSP Solver **

	// entalpy from the most recent timstep (in the event that a method solves multiple, shorter timesteps
	double m_h_sys_c_t_end_last;			//[K] Temperature (bulk) of cold runners & headers at end of previous timestep
	std::vector<double> m_h_htf_out_t_end_last;			//[K] Temperature of HTF temperature & material at end of previous timestep
	std::vector<double> m_T_htf_t_ave_last;			//[K] Temperature of HTF temperature & material at end of previous timestep
	double m_h_sys_h_t_end_last;			//[K] Temperature (bulk) of hot runners & headers at end of previous timestep

	// DSG system-specific -end


	// ***** T  E  M  P ******
	double m_step_recirc;
	// *********************************************
	// *********************************************

	// **************************************************************************
	// **************************************************************************
	// **************************************************************************

	C_csp_lf_dsg_collector_receiver();

	~C_csp_lf_dsg_collector_receiver(){};

		// GD version
	//virtual void init();
		// Trunk version
	virtual void init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs,
		C_csp_collector_receiver::S_csp_cr_solved_params & solved_params);

	virtual bool init_fieldgeom();

	virtual int get_operating_state();

	virtual double get_startup_time();
	virtual double get_startup_energy(double step /*sec*/); //MWh
	virtual double get_pumping_parasitic_coef();  //MWe/MWt
	virtual double get_min_power_delivery();    //MWt

	virtual void get_design_parameters(C_csp_collector_receiver::S_csp_cr_solved_params & solved_params);

	virtual void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);

	virtual void startup(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);

	virtual void on(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		double field_control,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);

	virtual void estimates(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_est_out &est_out,
		const C_csp_solver_sim_info &sim_info);
	
	virtual void converged();

	virtual void write_output_intervals(double report_time_start,
		const std::vector<double> & v_temp_ts_time_end, double report_time_end);

	virtual double calculate_optical_efficiency(const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim);

	virtual double calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs &weather, double q_incident /*MW*/);

	virtual double get_collector_area();

	// ------------------------------------------ supplemental methods -----------------------------------------------------------
	double turb_pres_frac(double m_dot_nd, int fmode, double ffrac, double fP_min);

	
	//void call(const C_csp_weatherreader::S_outputs &weather,
	//	C_csp_solver_htf_state &htf_state,
	//	const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
	//	C_csp_collector_receiver::S_csp_cr_outputs &cr_outputs,
	//	const C_csp_solver_sim_info &sim_info);

	/*void call(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);*/

	// ------------------------------------------ supplemental methods -----------------------------------------------------------
	class E_piping_config
	{
	public:
		enum
		{
			FIELD = 1,
			LOOP
		};
	};
	class E_loop_energy_balance_exit
	{
	public:
		enum
		{
			SOLVED,
			NaN
		};
	};

	struct S_loop_energy_balance_inputs
	{
		const C_csp_weatherreader::S_outputs *ms_weather;
		double m_T_htf_cold_in;		//[K]
		double m_m_dot_htf_loop;		//[kg/s]
		const C_csp_solver_sim_info *ms_sim_info;

		S_loop_energy_balance_inputs()
		{
			m_T_htf_cold_in = m_m_dot_htf_loop = std::numeric_limits<double>::quiet_NaN();
			ms_weather = 0;
			ms_sim_info = 0;
		}
	};

	void reset_S_loop_energy_balance_inputs();

	S_loop_energy_balance_inputs ms_loop_energy_balance_inputs;

	// Input weather, and inlet HTF conditions
	// Calculates energy balances for headers (if applicable) and receivers
	// This method uses TCS convention where the End Of Timestep HTF temperature
	//    is passed to the next receiver
	int loop_energy_balance_T_t_end(const C_csp_weatherreader::S_outputs &weather,
		double T_htf_cold_in /*K*/, double m_dot_htf_loop /*kg/s*/,
		const C_csp_solver_sim_info &sim_info);

	// This method is designed to pass the timestep integrated HTF temperature to successive energy balance nodes
	int loop_energy_balance_T_t_int(const C_csp_weatherreader::S_outputs &weather,
		double T_htf_cold_in /*K*/, double m_dot_htf_loop /*kg/s*/,
		const C_csp_solver_sim_info &sim_info);

	int loop_energy_balance_T_t_int_OT(const C_csp_weatherreader::S_outputs &weather,
		double T_htf_cold_in /*K*/, double m_dot_htf_loop /*kg/s*/,
		const C_csp_solver_sim_info &sim_info);

	int loop_energy_balance_T_t_int_RC(const C_csp_weatherreader::S_outputs &weather,
		double T_htf_cold_in /*K*/, double m_dot_htf_loop /*kg/s*/,
		const C_csp_solver_sim_info &sim_info);

	int loop_energy_balance_T_t_int();
	
	void loop_optical_eta(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_sim_info &sim_info);

	void loop_optical_eta_off();

	void update_last_temps();

	void reset_last_temps();

	void call(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);

	void call_bk(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);

	class C_mono_eq_xb_loop_out : public C_monotonic_equation
	{
	private:
		C_csp_lf_dsg_collector_receiver *mpc_csp;

	public:
		C_mono_eq_xb_loop_out(C_csp_lf_dsg_collector_receiver *pc_csp)
		{
			mpc_csp = pc_csp;
		}

		virtual int operator()(double m_dot_htf_loop /*kg/s*/, double *xb_loop_out /*-*/);
	};

	void apply_control_defocus(double defocus /*-*/);
	void apply_component_defocus(double defocus /*-*/);

	class C_mono_eq_defocus : public C_monotonic_equation
	{	// The solver chooses a defocus and sends it to the operator. The operator 
		//    calculates a new m_q_SCA and then solves the loop_energy_balance *at max HTF mass flow rate* 
		//    and returns T_htf_SCA_out. The solver finds the defocus resulting in the target HTF outlet temp
	private:
		C_csp_lf_dsg_collector_receiver *mpc_csp;

	public:
		C_mono_eq_defocus(C_csp_lf_dsg_collector_receiver *pc_csp)
		{
			mpc_csp = pc_csp;
		}

		virtual int operator()(double defocus /*-*/, double *T_htf_loop_out /*K*/);
	};

	class C_mono_eq_freeze_prot_E_bal : public C_monotonic_equation
	{	// The solver chooses a cold inlet temperature and sends it to the operator. The operator
		//		call the loop energy balance at the recirculation mass flow rate
		//		and returns the total field heat loss. The solver finds the T_cold_in such that E_fp_htf = E_losses
	private:
		C_csp_lf_dsg_collector_receiver *mpc_csp;

	public:

		double Q_htf_fp;	//[MJ]

		C_mono_eq_freeze_prot_E_bal(C_csp_lf_dsg_collector_receiver *pc_csp)
		{
			mpc_csp = pc_csp;
			Q_htf_fp = std::numeric_limits<double>::quiet_NaN();
		}


		virtual int operator()(double T_htf_cold_in /*K*/, double *E_loss_balance /*-*/);
	};



};


#endif