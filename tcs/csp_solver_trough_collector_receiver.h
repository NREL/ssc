#ifndef __csp_solver_trough_collector_receiver_
#define __csp_solver_trough_collector_receiver_

#include "csp_solver_core.h"
#include "htf_props.h"
#include "tcstype.h"

#include "lib_weatherfile.h"
#include <cmath>
#include "sam_csp_util.h"

class C_csp_trough_collector_receiver : public C_csp_collector_receiver
{

	HTFProperties m_htfProps, m_airProps;
	double m_pi, m_Pi, m_d2r, m_r2d, m_g, m_mtoinch;

	//int m_n_c_iam_matrix = 0;
	//int m_n_r_iam_matrix = 0;

	//Declare variables that require storage from step to step
	double
		m_Ap_tot, //Total aperture area m2
		m_L_tot,	//Total length of a collector row [m]
		m_opteff_des,	//Solar field optical efficiency at design
		m_m_dot_design,	//Solar field mass flow rate at design [kg/s]
		m_q_design;	//Design thermal power from the solar field [Wt]

	int
		m_nfsec,	//Number of field sections
		m_nhdrsec,	//Number of header sections
		m_nrunsec;	//Number of unique runner diameters
	//Other matrices
	util::matrix_t<HTFProperties*> m_AnnulusGasMat;
	util::matrix_t<AbsorberProps*> m_AbsorberPropMat;
	util::matrix_t<double> m_L_actSCA, m_A_cs, m_D_h, m_ColOptEff /*m_nColt, m_nSCA*/;
	emit_table m_epsilon_3;
	util::matrix_t<double> m_D_runner, m_L_runner, m_D_hdr;

	util::matrix_t<double>
		m_T_htf_in, m_T_htf_out, m_T_htf_ave, m_q_loss, m_q_abs, m_c_htf, m_rho_htf, m_DP_tube, m_E_abs_field,
		m_E_int_loop, m_E_accum, m_E_avail, m_E_abs_max, m_v_1, m_q_loss_SCAtot, m_q_abs_SCAtot, m_q_SCA, m_T_htf_in0, m_T_htf_out0,
		m_T_htf_ave0, m_E_fp, m_q_1abs_tot, m_q_1abs, m_q_i, m_IAM, m_EndGain, m_EndLoss, m_RowShadow;
	double m_T_sys_c_last, m_T_sys_h_last; //stored values for header thermal inertia calculations
	double m_N_run_mult;
	double m_v_hot, m_v_cold;	//Header HTF volume
	double m_defocus_new, m_defocus_old, m_ftrack;
	bool
		m_no_fp,	//Freeze protection flag
		m_is_fieldgeom_init;	//Flag to indicate whether the field geometry has been initialized
	double m_T_cold_in_1, m_c_hdr_cold, m_start_time, m_current_time, m_dt, m_SolarAlt, m_costh, m_theta, m_shift,
		m_q_SCA_tot, m_m_dot_htfX, m_Header_hl_cold, m_Runner_hl_cold, m_Pipe_hl_cold, m_T_loop_in,
		m_T_loop_outX, m_Runner_hl_hot, m_Header_hl_hot, m_Pipe_hl_hot, m_c_hdr_hot, m_time_hr, m_dt_hr;
	int m_day_of_year, m_SolveMode, m_dfcount;

	double m_ncall_track;

	double m_T_save[5];
	double m_reguess_args[3];

	double m_hour, m_T_sky;

	double m_m_htf_prop_min;

	bool m_ss_init_complete;

	// member string for exception messages
	std::string m_error_msg;

	// track number of calls per timestep, reset = -1 in converged() call
	int m_ncall;

public:

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;
	
	// Design Parameters: Must be defined before member functions can be used
	// **************************************************************************


	//parameters and inputs
	int m_nSCA;		//Number of SCA's in a loop
	int m_nHCEt;		//Number of HCE types
	int m_nColt;		//Number of collector types
	int m_nHCEVar;		//Number of HCE variants per type
	int m_nLoops;		//Number of loops in the field
	double m_eta_pump;		//HTF pump efficiency
	double m_HDR_rough;		//Header pipe roughness
	double m_theta_stow;		//stow angle
	double m_theta_dep;		//deploy angle
	double m_Row_Distance;		//Spacing between rows (centerline to centerline)
	int m_FieldConfig;		//Number of subfield headers
	double m_T_startup;		//The required temperature of the system before the power block can be switched on
	double m_pb_rated_cap;		//Rated plant capacity
	double m_m_dot_htfmin;		//Minimum loop HTF flow rate
	double m_m_dot_htfmax;		//Maximum loop HTF flow rate
	double m_T_loop_in_des;		//Design loop inlet temperature
	double m_T_loop_out;		//Target loop outlet temperature
	int m_Fluid;		//Field HTF fluid number
	double m_T_field_ini;		//Initial field temperature
	//double* HTF_data_in;		//m_Fluid property data
	int m_nrow_HTF_data, m_ncol_HTF_data;
	double m_T_fp;		//Freeze protection temperature (heat trace activation temperature)
	double m_I_bn_des;		//Solar irradiation at design
	double m_V_hdr_max;		//Maximum HTF velocity in the header at design
	double m_V_hdr_min;		//Minimum HTF velocity in the header at design
	double m_Pipe_hl_coef;		//Loss coefficient from the header, runner pipe, and non-HCE piping
	double m_SCA_drives_elec;		//Tracking power, in Watts per SCA drive
	int m_fthrok;		//Flag to allow partial defocusing of the collectors
	int m_fthrctrl;		//Defocusing strategy
	double m_ColTilt;		//Collector tilt angle (0 is horizontal, 90deg is vertical)
	double m_ColAz;		//Collector azimuth angle

	int m_accept_mode;		//Acceptance testing mode? (1=yes, 0=no)
	bool m_accept_init;		//In acceptance testing mode - require steady-state startup
	int m_accept_loc;			//In acceptance testing mode - temperature sensor location (1=hx,2=loop)
	bool m_is_using_input_gen;

	double m_solar_mult;		//Solar multiple
	double m_mc_bal_hot;		//The heat capacity of the balance of plant on the hot side
	double m_mc_bal_cold;		//The heat capacity of the balance of plant on the cold side
	double m_mc_bal_sca;		//Non-HTF heat capacity associated with each SCA - per meter basis

	//double* m_OptCharType;		 
	//int m_nval_OptCharType;
	std::vector<int> m_OptCharType; //The optical characterization method
	
	//double* m_CollectorType;		
	//int m_nval_CollectorType;
	std::vector<int> m_CollectorType;	//{1=user defined, 2=LS-2, 3=LS-3, 4=IST} 

	//double* m_W_aperture;		
	//int m_nval_W_aperture;
	std::vector<double> m_W_aperture; //The collector aperture width (Total structural area.. used for shadowing)
	
	//double* m_A_aperture;		//Reflective aperture area of the collector
	//int m_nval_A_aperture;
	std::vector<double> m_A_aperture; //Reflective aperture area of the collector

	//double* m_reflectivity;		//Base solar-weighted mirror m_reflectivity value 
	//int m_nval_reflectivity;
	std::vector<double> m_reflectivity;

	//double* m_TrackingError;		//User-defined tracking error derate
	//int m_nval_TrackingError;
	std::vector<double> m_TrackingError;

	//double* m_GeomEffects;		//User-defined geometry effects derate
	//int m_nval_GeomEffects;
	std::vector<double> m_GeomEffects;

	//double* m_Rho_mirror_clean;		//User-defined clean mirror m_reflectivity
	//int m_nval_Rho_mirror_clean;
	std::vector<double> m_Rho_mirror_clean;

	//double* m_Dirt_mirror;		//User-defined dirt on mirror derate
	//int m_nval_Dirt_mirror;
	std::vector<double> m_Dirt_mirror; //User-defined dirt on mirror derate

	//double* m_Error;		//User-defined general optical error derate 
	//int m_nval_Error;
	std::vector<double> m_Error; //User-defined general optical error derate

	//double* m_Ave_Focal_Length;		//The average focal length of the collector 
	//int m_nval_Ave_Focal_Length;
	std::vector<double> m_Ave_Focal_Length; //The average focal length of the collector 

	//double* m_L_SCA;		//The length of the SCA 
	//int m_nval_L_SCA;
	std::vector<double> m_L_SCA; //The length of the SCA 

	//double* m_L_aperture;		//The length of a single mirror/HCE unit
	//int m_nval_L_aperture;
	std::vector<double> m_L_aperture;//The length of a single mirror/HCE unit

	//double* m_ColperSCA;		//The number of individual collector sections in an SCA 
	//int m_nval_ColperSCA;
	std::vector<double> m_ColperSCA; //The number of individual collector sections in an SCA

	//double* m_Distance_SCA;		// piping distance between SCA's in the field
	//int m_nval_Distance_SCA;
	std::vector<double> m_Distance_SCA; // piping distance between SCA's in the field
	
	//double* m_HCE_FieldFrac_in;		//The fraction of the field occupied by this HCE type 
	//int m_nrow_HCE_FieldFrac, m_ncol_HCE_FieldFrac;
	//double* m_D_2_in;		//The inner absorber tube diameter
	//int m_nrow_D_2, m_ncol_D_2;
	//double* m_D_3_in;		//The outer absorber tube diameter
	//int m_nrow_D_3, m_ncol_D_3;
	//double* m_D_4_in;		//The inner glass envelope diameter 
	//int m_nrow_D_4, m_ncol_D_4;
	//double* m_D_5_in;		//The outer glass envelope diameter 
	//int m_nrow_D_5, m_ncol_D_5;
	//double* m_D_p_in;		//The diameter of the absorber flow plug (optional) 
	//int m_nrow_D_p, m_ncol_D_p;
	//double* m_Flow_type_in;		//The flow type through the absorber
	//int m_nrow_Flow_type, m_ncol_Flow_type;
	//double* m_Rough_in;		//Roughness of the internal surface 
	//int m_nrow_Rough, m_ncol_Rough;
	//double* m_alpha_env_in;		//Envelope absorptance 
	//int m_nrow_alpha_env, m_ncol_alpha_env;
	//double* m_epsilon_3_11_in;		//Absorber emittance - HCE type 1 - HCE variation 1
	//int m_nrow_epsilon_3_11, m_ncol_epsilon_3_11;
	//double* m_epsilon_3_12_in;		//Absorber emittance - HCE type 1 - HCE variation 2
	//int m_nrow_epsilon_3_12, m_ncol_epsilon_3_12;
	//double* m_epsilon_3_13_in;		//Absorber emittance - HCE type 1 - HCE variation 3
	//int m_nrow_epsilon_3_13, m_ncol_epsilon_3_13;
	//double* m_epsilon_3_14_in;		//Absorber emittance - HCE type 1 - HCE variation 4
	//int m_nrow_epsilon_3_14, m_ncol_epsilon_3_14;
	//double* m_epsilon_3_21_in;		//Absorber emittance - HCE type 2 - HCE variation 1
	//int m_nrow_epsilon_3_21, m_ncol_epsilon_3_21;
	//double* m_epsilon_3_22_in;		//Absorber emittance - HCE type 2 - HCE variation 2
	//int m_nrow_epsilon_3_22, m_ncol_epsilon_3_22;
	//double* m_epsilon_3_23_in;		//Absorber emittance - HCE type 2 - HCE variation 3
	//int m_nrow_epsilon_3_23, m_ncol_epsilon_3_23;
	//double* m_epsilon_3_24_in;		//Absorber emittance - HCE type 2 - HCE variation 4
	//int m_nrow_epsilon_3_24, m_ncol_epsilon_3_24;
	//double* m_epsilon_3_31_in;		//Absorber emittance - HCE type 3 - HCE variation 1
	//int m_nrow_epsilon_3_31, m_ncol_epsilon_3_31;
	//double* m_epsilon_3_32_in;		//Absorber emittance - HCE type 3 - HCE variation 2
	//int m_nrow_epsilon_3_32, m_ncol_epsilon_3_32;
	//double* m_epsilon_3_33_in;		//Absorber emittance - HCE type 3 - HCE variation 3
	//int m_nrow_epsilon_3_33, m_ncol_epsilon_3_33;
	//double* m_epsilon_3_34_in;		//Absorber emittance - HCE type 3 - HCE variation 4
	//int m_nrow_epsilon_3_34, m_ncol_epsilon_3_34;
	//double* m_epsilon_3_41_in;		//Absorber emittance - HCE type 4 - HCE variation 1
	//int m_nrow_epsilon_3_41, m_ncol_epsilon_3_41;
	//double* m_epsilon_3_42_in;		//Absorber emittance - HCE type 4 - HCE variation 2
	//int m_nrow_epsilon_3_42, m_ncol_epsilon_3_42;
	//double* m_epsilon_3_43_in;		//Absorber emittance - HCE type 4 - HCE variation 3
	//int m_nrow_epsilon_3_43, m_ncol_epsilon_3_43;
	//double* m_epsilon_3_44_in;		//Absorber emittance - HCE type 4 - HCE variation 4
	//int m_nrow_epsilon_3_44, m_ncol_epsilon_3_44;
	//double* m_alpha_abs_in;		//Absorber absorptance 
	//int m_nrow_alpha_abs, m_ncol_alpha_abs;
	//double* m_Tau_envelope_in;		//Envelope transmittance
	//int m_nrow_Tau_envelope, m_ncol_Tau_envelope;
	//double* m_EPSILON_4_in;		//Inner glass envelope emissivities (Pyrex) 
	//int m_nrow_EPSILON_4, m_ncol_EPSILON_4;
	//double* m_EPSILON_5_in;		//Outer glass envelope emissivities (Pyrex) 
	//int m_nrow_EPSILON_5, m_ncol_EPSILON_5;
	//double* m_GlazingIntactIn_in;		//The glazing intact flag {1=true, else=false}
	//int m_nrow_GlazingIntactIn, m_ncol_GlazingIntactIn;
	//double* m_P_a_in;		//Annulus gas pressure
	//int m_nrow_P_a, m_ncol_P_a;
	//double* m_AnnulusGas_in;		//Annulus gas type (1=air, 26=Ar, 27=H2)
	//int m_nrow_AnnulusGas, m_ncol_AnnulusGas;
	//double* m_AbsorberMaterial_in;		//Absorber material type
	//int m_nrow_AbsorberMaterial, m_ncol_AbsorberMaterial;
	//double* m_Shadowing_in;		//Receiver bellows shadowing loss factor
	//int m_nrow_Shadowing, m_ncol_Shadowing;
	//double* m_Dirt_HCE_in;		//Loss due to dirt on the receiver envelope
	//int m_nrow_Dirt_HCE, m_ncol_Dirt_HCE;
	//double* m_Design_loss_in;		//Receiver heat loss at design
	//int m_nrow_Design_loss, m_ncol_Design_loss;
	//double* m_SCAInfoArray_in;		//(:,1) = HCE type, (:,2)= Collector type for each SCA in the loop 
	//int m_nrow_SCAInfoArray, m_ncol_SCAInfoArray;

	//double* m_SCADefocusArray;		//Order in which the SCA's should be defocused
	//int m_nval_SCADefocusArray;
	std::vector<int> m_SCADefocusArray; //Order in which the SCA's should be defocused


	double m_I_b;		//Direct normal incident solar irradiation
	double m_T_db;		//Dry bulb air temperature
	double m_V_wind;		//Ambient windspeed 
	double m_P_amb;		//Ambient pressure
	double m_T_dp;		//The dewpoint temperature
	double m_T_cold_in;		//HTF return temperature
	double m_m_dot_in;		//HTF mass flow rate at the inlet 
	double m_defocus;		//Defocus control 
	double m_SolarAz;		//Solar azimuth angle reported by the Type15 weather file
	double m_latitude;		//Site m_latitude read from weather file
	double m_longitude;		//Site m_longitude read from weather file
	//double timezone;		//Time zone

	double m_T_sys_h;		//Solar field HTF outlet temperature
	double m_m_dot_avail;		//HTF mass flow rate from the field
	double m_q_avail;		//Thermal power produced by the field
	double m_DP_tot;		//Total HTF pressure drop
	double m_W_dot_pump;		//Required solar field pumping power
	double m_E_fp_tot;		//Freeze protection energy
	int m_qq;		//Number of iterations required to solve
	double m_T_sys_c;		//Collector inlet temperature
	double m_EqOpteff;		//Collector equivalent optical efficiency
	double m_SCAs_def;		//The fraction of focused SCA's
	double m_m_dot_htf_tot;		//The actual flow rate through the field..
	double m_E_bal_startup;		//Startup energy consumed
	double m_q_inc_sf_tot;		//Total power incident on the field
	double m_q_abs_tot;		//Total absorbed energy
	double m_q_loss_tot;		//Total receiver thermal and optical losses
	double m_m_dot_htf;		//Flow rate in a single loop
	double m_q_loss_spec_tot;		//Field-average receiver thermal losses (convection and radiation)
	double m_SCA_par_tot;		//Parasitic electric power consumed by the SC
	double m_Pipe_hl;		//Pipe heat loss in the hot header and the hot runner
	double m_q_dump;		//Dumped thermal energy
	double m_Theta_ave;		//Field average m_theta value
	double m_CosTh_ave;		//Field average costheta value
	double m_IAM_ave;		//Field average incidence angle modifier
	double m_RowShadow_ave;		//Field average row shadowing loss
	double m_EndLoss_ave;		//Field average end loss
	double m_dni_costh;		//DNI_x_CosTh
	double m_qinc_costh;		//Q_inc_x_CosTh
	double m_t_loop_outlet;		//HTF temperature immediately subsequent to the loop outlet
	double m_c_htf_ave;		//Average solar field specific heat
	double m_q_field_delivered;		//Total solar field thermal power delivered
	double m_eta_thermal;		//Solar field thermal efficiency (power out/ANI)
	double m_E_loop_accum;		//Accumulated internal energy change rate in the loops ONLY
	double m_E_hdr_accum;		//Accumulated internal energy change rate in the headers/SGS
	double m_E_tot_accum;		//Total accumulated internal energy change rate
	double m_E_field;		//Accumulated internal energy in the entire solar field
	double m_tilt; // 
	double m_azimuth; //
	double m_P_ref; // the plant nameplate capacity

	util::matrix_t<double> m_field_fl_props;

	util::matrix_t<double> m_HCE_FieldFrac, m_D_2, m_D_3, m_D_4, m_D_5, m_D_p, m_Flow_type, m_Rough, m_alpha_env, m_epsilon_3_11, m_epsilon_3_12,
		m_epsilon_3_13, m_epsilon_3_14, m_epsilon_3_21, m_epsilon_3_22, m_epsilon_3_23, m_epsilon_3_24, m_epsilon_3_31, m_epsilon_3_32, m_epsilon_3_33,
		m_epsilon_3_34, m_epsilon_3_41, m_epsilon_3_42, m_epsilon_3_43, m_epsilon_3_44, m_alpha_abs, m_Tau_envelope, m_EPSILON_4, m_EPSILON_5,
		m_P_a, m_AnnulusGas, m_AbsorberMaterial, m_Shadowing, m_Dirt_HCE, m_Design_loss, m_SCAInfoArray; // m_GlazingIntactIn,

	util::matrix_t<double> m_IAM_matrix;

	util::matrix_t<double> m_GlazingIntact;

	int m_n_c_iam_matrix;
	int m_n_r_iam_matrix;

	// **************************************************************************
	// **************************************************************************
	// **************************************************************************

	C_csp_trough_collector_receiver();

	~C_csp_trough_collector_receiver(){};

	virtual void init();
	virtual bool init_fieldgeom();	

	virtual int get_operating_state();

	virtual double get_startup_time();
	virtual double get_startup_energy(double step /*sec*/); //MWh
	virtual double get_pumping_parasitic_coef();  //MWe/MWt
	virtual double get_min_power_delivery();    //MWt

	virtual void get_design_parameters(C_csp_collector_receiver::S_csp_cr_solved_params & solved_params);

	virtual void call(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

	virtual double calculate_optical_efficiency(const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim);

	virtual double calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs &weather, double q_incident /*MW*/);

	virtual double get_collector_area();

	// ------------------------------------------ supplemental methods -----------------------------------------------------------
	void EvacReceiver(double T_1_in, double m_dot, double T_amb, double m_T_sky, double v_6, double P_6, double m_q_i,
		int hn /*HCE number [0..3] */, int hv /* HCE variant [0..3] */, int ct /*Collector type*/, int sca_num, bool single_point, int ncall, double time,
		//outputs
		double &q_heatloss, double &q_12conv, double &q_34tot, double &c_1ave, double &rho_1ave);
	double fT_2(double q_12conv, double T_1, double T_2g, double m_v_1, int hn, int hv);
	void FQ_34CONV(double T_3, double T_4, double P_6, double v_6, double T_6, int hn, int hv, double &q_34conv, double &h_34);
	void FQ_56CONV(double T_5, double T_6, double P_6, double v_6, int hn, int hv, double &q_56conv, double &h_6);
	double FQ_COND_BRACKET(double T_3, double T_6, double P_6, double v_6, int hn, int hv);
	void FQ_34RAD(double T_3, double T_4, double T_7, double epsilon_3_v, int hn, int hv, double &q_34rad, double &h_34);
	//void FQ_56CONV(double T_5, double T_6, double P_6, double v_6, int hn, int hv, double &q_56conv, double &h_6);
	//double FQ_COND_BRACKET(double T_3, double T_6, double P_6, double v_6, int hn, int hv);
	double FK_23(double T_2, double T_3, int hn, int hv);
	double PressureDrop(double m_dot, double T, double P, double D, double m_Rough, double L_pipe,
		double Nexp, double Ncon, double Nels, double Nelm, double Nell, double Ngav, double Nglv,
		double Nchv, double Nlw, double Nlcv, double Nbja);
	double FricFactor(double m_Rough, double Reynold);
	void header_design(int nhsec, int m_nfsec, int m_nrunsec, double rho, double V_max, double V_min, double m_dot,
		util::matrix_t<double> &m_D_hdr, util::matrix_t<double> &m_D_runner, std::string *summary = NULL);
	double pipe_sched(double De);
	double Pump_SGS(double rho, double m_dotsf, double sm);

	
};







#endif