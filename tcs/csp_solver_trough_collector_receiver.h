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
private:

	HTFProperties m_htfProps, m_airProps;
	double m_pi, m_Pi, m_d2r, m_r2d, m_g, m_mtoinch;
	
	// Variables moved to private
	double m_Pipe_hl;		//Pipe heat loss in the hot header and the hot runner
	double m_I_b;			//Direct normal incident solar irradiation
	double m_T_db;			//Dry bulb air temperature
	double m_V_wind;		//Ambient windspeed 
	double m_P_amb;			//Ambient pressure
	double m_T_dp;			//The dewpoint temperature
	double m_T_cold_in;		//HTF return temperature
	double m_m_dot_in;		//HTF mass flow rate at the inlet 
	double m_defocus;		//Defocus control 
	double m_SolarAz;		//Solar azimuth angle reported by the Type15 weather file
	double m_latitude;		//Site m_latitude read from weather file
	double m_longitude;		//Site m_longitude read from weather file

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

	int m_n_c_iam_matrix;
	int m_n_r_iam_matrix;

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

	//parameters and inputs
	int m_nSCA;				//[-] Number of SCA's in a loop
	int m_nHCEt;			//[-] Number of HCE types
	int m_nColt;			//[-] Number of collector types
	int m_nHCEVar;			//[-] Number of HCE variants per type
	int m_nLoops;			//[-] Number of loops in the field
	double m_eta_pump;		//[-] HTF pump efficiency
	double m_HDR_rough;		//[m] Header pipe roughness
	double m_theta_stow;	//[deg] stow angle
	double m_theta_dep;		//[deg] deploy angle
	double m_Row_Distance;	//[m] Spacing between rows (centerline to centerline)
	int m_FieldConfig;		//[-] Number of subfield headers
	double m_T_startup;		//[C] The required temperature (converted to K in init) of the system before the power block can be switched on
	double m_m_dot_htfmin;	//[kg/s] Minimum loop HTF flow rate
	double m_m_dot_htfmax;	//[kg/s] Maximum loop HTF flow rate
	double m_T_loop_in_des;	//[C] Design loop inlet temperature, converted to K in init
	double m_T_loop_out;	//[C] Target loop outlet temperature, converted to K in init
	int m_Fluid;			//[-] Field HTF fluid number
	
	double m_T_fp;			//[C] Freeze protection temperature (heat trace activation temperature), convert to K in init
	double m_I_bn_des;		//[W/m^2] Solar irradiation at design
	double m_V_hdr_max;		//[m/s] Maximum HTF velocity in the header at design
	double m_V_hdr_min;		//[m/s] Minimum HTF velocity in the header at design
	double m_Pipe_hl_coef;	//[W/m2-K] Loss coefficient from the header, runner pipe, and non-HCE piping
	double m_SCA_drives_elec;	//[W/SCA] Tracking power, in Watts per SCA drive
	int m_fthrok;			//[-] Flag to allow partial defocusing of the collectors
	int m_fthrctrl;			//[-] Defocusing strategy
	double m_ColTilt;		//[deg] Collector tilt angle (0 is horizontal, 90deg is vertical)
	double m_ColAz;			//[deg] Collector azimuth angle

	int m_accept_mode;		//[-] Acceptance testing mode? (1=yes, 0=no)
	bool m_accept_init;		//[-] In acceptance testing mode - require steady-state startup
	int m_accept_loc;		//[-] In acceptance testing mode - temperature sensor location (1=hx,2=loop)
	bool m_is_using_input_gen;

	double m_solar_mult;		//[-] Solar multiple 
	double m_mc_bal_hot;		//[kWht/K-MWt] The heat capacity of the balance of plant on the hot side
	double m_mc_bal_cold;		//[kWht/K-MWt] The heat capacity of the balance of plant on the cold side
	double m_mc_bal_sca;		//[Wht/K-m] Non-HTF heat capacity associated with each SCA - per meter basis

	std::vector<double> m_W_aperture;	//[m] The collector aperture width (Total structural area.. used for shadowing)
	std::vector<double> m_A_aperture;	//[m^2] Reflective aperture area of the collector
	std::vector<double> m_TrackingError;//[-] Tracking error derate
	std::vector<double> m_GeomEffects;	//[-] Geometry effects derate
	std::vector<double> m_Rho_mirror_clean;	//[-] Clean mirror reflectivity
	std::vector<double> m_Dirt_mirror;	//[-] Dirt on mirror derate
	std::vector<double> m_Error;		//[-] General optical error derate
	std::vector<double> m_Ave_Focal_Length; //[m] The average focal length of the collector 
	std::vector<double> m_L_SCA;		//[m] The length of the SCA 
	std::vector<double> m_L_aperture;	//[m] The length of a single mirror/HCE unit
	std::vector<double> m_ColperSCA;	//[-] The number of individual collector sections in an SCA
	std::vector<double> m_Distance_SCA; //[m] Piping distance between SCA's in the field
	
	std::vector<int> m_SCADefocusArray; //[-] Order in which the SCA's should be defocused

	util::matrix_t<double> m_field_fl_props;	//[-] User-defined field HTF properties

	util::matrix_t<double> m_HCE_FieldFrac,  //[-] Fraction of the field occupied by this HCE type
	m_D_2, 									 //[m] Inner absorber tube diameter
	m_D_3, 									 //[m] Outer absorber tube diameter
	m_D_4, 									 //[m] Inner glass envelope diameter
	m_D_5, 									 //[m] Outer glass envelope diameter
	m_D_p, 									 //[m] Diameter of the absorber flow plug (optional)
	m_Flow_type, 							 //[-] Flow type through the absorber
	m_Rough, 								 //[m] Roughness of the internal surface
	m_alpha_env, 							 //[-] Envelope absorptance
	
	//[-] Emittance vs. temperature profile for each receiver type and variation
	m_epsilon_3_11, m_epsilon_3_12,	m_epsilon_3_13, m_epsilon_3_14, 
	m_epsilon_3_21, m_epsilon_3_22, m_epsilon_3_23, m_epsilon_3_24, 
	m_epsilon_3_31, m_epsilon_3_32, m_epsilon_3_33,	m_epsilon_3_34,
	m_epsilon_3_41, m_epsilon_3_42, m_epsilon_3_43, m_epsilon_3_44, 
	
	m_alpha_abs,             //[-] Absorber absorptance
	m_Tau_envelope, 		 //[-] Envelope transmittance
	m_EPSILON_4, 			 //[-] Inner glass envelope emissivities
	m_EPSILON_5,			 //[-] Outer glass envelope emissivities
	m_GlazingIntact,         //[-] Glazing intact (broken glass) flag {1=true, else=false}
	m_P_a, 					 //[torr] Annulus gas pressure				 
	m_AnnulusGas, 			 //[-] Annulus gas type (1=air, 26=Ar, 27=H2)
	m_AbsorberMaterial, 	 //[-] Absorber material type
	m_Shadowing, 			 //[-] Receiver bellows shadowing loss factor
	m_Dirt_HCE, 			 //[-] Loss due to dirt on the receiver envelope
	m_Design_loss, 			 //[-] Receiver heat loss at design
	m_SCAInfoArray;          //[-] Receiver (,1) and collector (,2) type for each assembly in loop 	 

	util::matrix_t<double> m_IAM_matrix;		//[-] IAM coefficients, matrix for 4 collectors
	
	// **************************************************************************
	// **************************************************************************
	// **************************************************************************

	C_csp_trough_collector_receiver();

	~C_csp_trough_collector_receiver(){};

	virtual void init(C_csp_collector_receiver::S_csp_cr_solved_params & solved_params);
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