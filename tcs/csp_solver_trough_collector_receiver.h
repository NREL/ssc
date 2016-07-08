#ifndef __csp_solver_trough_collector_receiver_
#define __csp_solver_trough_collector_receiver_

#include "csp_solver_core.h"
#include "htf_props.h"
#include "tcstype.h"

#include "lib_weatherfile.h"
#include <cmath>
#include "sam_csp_util.h"

#include "numeric_solvers.h"

#include <iostream>
#include <fstream>

class C_csp_trough_collector_receiver : public C_csp_collector_receiver
{
private:

	// Member classes
	HTFProperties m_htfProps, m_airProps;
	
	// Hardcoded constants
	double m_d2r, m_r2d, m_mtoinch;
	double m_T_htf_prop_min;	//[K] Minimum temperature allowed in props call to minimize errors

	// Init() inputs
	double m_latitude;		//[deg] convert to [rad] in init()
	double m_longitude;		//[deg] convert to [rad] in init()
	double m_shift;			//[deg] convert to [rad] in init()

	// Parameters calculated in init()
	int m_n_c_iam_matrix;	//[-] Number of columns in the IAM matrix
	int m_n_r_iam_matrix;	//[-] Number of rows in the IAM matrix
	double m_N_run_mult;	//[-] Multiplier for runner heat loss - see comments in init()
	double m_v_hot;			//[m^3] Hot piping volume
	double m_v_cold;		//[m^3] Cold piping volume
	double m_Ap_tot;		//[m^2] Total field aperture area
	int	m_nfsec;			//[-] Number of field sections
	int	m_nhdrsec;			//[-] Number of header sections
	int	m_nrunsec;			//[-] Number of unique runner diameters
	double m_L_tot;			//[m] Total length of collectors in a loop
	double m_opteff_des;	//[-] Design-point optical efficieny (theta = 0) from the solar field
	double m_m_dot_design;	//[kg/s] Total solar field mass flow rate at design
	double m_q_design;		//[Wt] Design-point thermal power from the solar field

	std::vector<double> m_D_runner;	//[m] Diameters of runner sections
	std::vector<double> m_L_runner;	//[m] Lengths of runner sections
	std::vector<double> m_D_hdr;	//[m] Diameters of header sections
	std::vector<double> m_L_actSCA;	//[m] Lengths of each SCA

	emit_table m_epsilon_3;			// Table of emissivity vs temperature for each variant of each receiver type
	util::matrix_t<HTFProperties*> m_AnnulusGasMat;		// HTF Property class for each variant of each receiver type
	util::matrix_t<AbsorberProps*> m_AbsorberPropMat;	// Absorber Property class for each variant of each receiver type

	util::matrix_t<double> m_A_cs;	//[m^2] Cross-sectional area for HTF flow for each receiver and variant (why variant?)
	util::matrix_t<double> m_D_h;	//[m^2] Hydraulic diameters for HTF flow for each receiver and variant (why variant?)	

	// Variables that we need to track between calls during one timestep
	double m_T_cold_in_1;	//[K] Calculated HTF inlet temperature
	double m_defocus;		//[-] Defocus during present call = m_defocus_new / m_defocus_old
	double m_defocus_new;	//[-] Defocus signal from controller during present timestep
	double m_defocus_old;	//[-] Defocus during previous call (= 1 at first call)
	bool m_no_fp;			//[-] Did previous call require freeze protection (T = no, F = yes) 
	double m_m_dot_htfX;	//[kg/s] Loop mass flow rate solved in previous timestep
	int m_ncall;			//[-] Track number of calls per timestep, reset = -1 in converged() call
	
	// Variables that are passed between methods, but not necessary to carry over timesteps
	double m_EqOpteff;		//[-] Collector equivalent (weighted over variants AND all SCAs) optical efficiency
	double m_m_dot_htf_tot;	//[kg/s] The total flow rate through the entire field (m_dot_loop * N_loops)
	double m_c_htf_ave;		//[J/kg-K] Average solar field specific heat

	std::vector<double> m_E_int_loop;	//[J] Energy relative to ambient for each receiver
	std::vector<double> m_E_accum;		//[J] Internal energy change in timestep for each receiver
	std::vector<double> m_E_avail;		//[J] Energy absorbed less internal energy change for each receiver
	std::vector<double> m_q_abs_SCAtot;	//[W] Heat absorption into HTF in each SCA, weighted variants
	std::vector<double> m_q_loss_SCAtot;//[W] Total heat losses from each SCA, weighted variants
	std::vector<double> m_q_1abs_tot;	//[W/m] Thermal losses from each SCA, weighted variants

	std::vector<double> m_q_loss;		//[W/m] Total losses (thermal + optical) per length in each SCA, one variant
	std::vector<double> m_q_abs;		//[W/m] Total heat absorption per length into HTF in each SCA, one variant
	std::vector<double> m_q_1abs;		//[W/m] Total *thermal* losses per length in each SCA, one variant
	std::vector<double> m_q_i;			//[W/m] DNI * A_aper / L_sca
		// This value (m_q_SCA) is passed to the Evacuated Tube model, where the other optical losses are applied
	std::vector<double> m_q_SCA;		//[W/m] Total incident irradiation on the receiver (q"*A_aper/L_sca*cos(theta)*all_defocus)
	std::vector<double> m_q_SCA_control_df;	//[W/m] Total incident irradiation less CONTROL defocus (m_q_sca * control_defocus)

	std::vector<double> m_IAM;			//[-] Incidence angle modifiers
	std::vector<double> m_RowShadow;	//[-] Row-to-row shadowing losses

	/*m_nColt, m_nSCA*/
	util::matrix_t<double> m_ColOptEff;	//[-] tracking * geom * rho * dirt * error * IAM * row shadow * end loss * ftrack
	util::matrix_t<double> m_EndGain;	//[-] Light from different collector hitting receiver
	util::matrix_t<double> m_EndLoss;	//[-] Light missing receiver due to length

	double m_Theta_ave;					//[rad] Field average m_theta value (but... nothing in our model allows for this to different over SCAs)
	double m_CosTh_ave;					//[-] Field average costheta value
	double m_IAM_ave;					//[-] Field average incidence angle modifier
	double m_RowShadow_ave;				//[-] Field average row shadowing loss
	double m_EndLoss_ave;				//[-] Field average end loss
	
	double m_ftrack;			//[-] Fraction of timestep that solar field is deployed
	double m_costh;				//[-] Cosine of the incidence angle between sun and trough aperture
	
	double m_Header_hl_cold;	//[W] Total heat loss from the cold headers *in one field section*
	double m_Runner_hl_cold;	//[W] Total heat loss from the cold runners *in one field section*

	double m_Header_hl_hot;		//[W] Total heat loss from the hot headers *in one field section*
	double m_Runner_hl_hot;		//[W] Total heat loss from the hot runners *in one field section*

	double m_c_hdr_cold;		//[J/kg-K] Specific heat of fluid at m_T_sys_c
	double m_c_hdr_hot;			//[J/kg-K] Specific heat of fluid at outlet temperature of last SCA (not necessarily return temperature if modeling runners and headers)

	double m_mc_bal_hot;		//[J/K] The heat capacity of the balance of plant on the hot side
	double m_mc_bal_cold;		//[J/K] The heat capacity of the balance of plant on the cold side

	// Classes that are defined as member data so are re-declared each time performance function is called
	std::vector<double> m_DP_tube;	//[Pa] Pressure drops in each receiver

	// *********************************************
	// TCS Temperature Tracking
		// Temperatures from the most recent converged() operation
	double m_TCS_T_sys_c_converged;					//[K] Temperature (bulk) of cold runners & headers in previous timestep
	std::vector<double> m_TCS_T_htf_ave_converged;	//[K] Average HTF temperature in each SCA
	double m_TCS_T_sys_h_converged;					//[K] Temperature (bulk) of hot runners & headers in previous timestep		
	
		// Temperatures from the most recent timestep (in the event that a method solves multiple, shorter timesteps)
	double m_TCS_T_sys_c_last;					//[K] Temperature (bulk) of cold runners & headers in previous timestep
	std::vector<double> m_TCS_T_htf_ave_last;	//[K] Average HTF temperature in each SCA
	double m_TCS_T_sys_h_last;					//[K] Temperature (bulk) of hot runners & headers in previous timestep		

		// Latest temperatures solved during present call to this class
	double m_TCS_T_sys_c;					//[K] Temperature (bulk) of cold runners & headers
	std::vector<double> m_TCS_T_htf_in;		//[K] Inlet HTF temperature to each SCA
	std::vector<double> m_TCS_T_htf_ave;	//[K] Average HTF temperature in each SCA
	std::vector<double> m_TCS_T_htf_out;	//[K] Outlet HTF temperature to each SCA
	double m_TCS_T_sys_h;					//[K] Solar field HTF outlet temperature
	// *********************************************
	// *********************************************


	bool m_ss_init_complete;	//[-] For TCS-based model in acceptance testing, has model achieved steady state at first timestep?

	// Member variables that are used to store information for the EvacReceiver method
	double m_T_save[5];			//[K] Saved temperatures from previous call to EvacReceiver single SCA energy balance model
	double m_reguess_args[3];	//[-] Logic to determine whether to use previous guess values or start iteration fresh
	
	// member string for exception messages
	std::string m_error_msg;

	int m_operating_mode_converged;
	int m_operating_mode;

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
	double m_T_loop_out_des;//[C] Target loop outlet temperature, converted to K in init
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
	double m_mc_bal_hot_per_MW;		//[kWht/K-MWt] The heat capacity per MWt design of the balance of plant on the hot side
	double m_mc_bal_cold_per_MW;	//[kWht/K-MWt] The heat capacity per MWt design of the balance of plant on the cold side
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


	// ***********************
	// ***** T  E  M  P ******
	double m_step_recirc;

	std::ofstream m_outfile;	//("example.txt", std::ofstream); //("C:/Users/tneises/Documents/2015 SuNLaMP Projects/SAM/FY16/Q2 reporting/debug.csv");

	// ***********************
	// ***********************

	C_csp_trough_collector_receiver();

	~C_csp_trough_collector_receiver(){};

	virtual void init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs, 
				C_csp_collector_receiver::S_csp_cr_solved_params & solved_params);
	virtual bool init_fieldgeom();	

	virtual double get_startup_time();
	virtual double get_startup_energy(double step /*sec*/); //MWh
	virtual double get_pumping_parasitic_coef();  //MWe/MWt
	virtual double get_min_power_delivery();    //MWt

	virtual int get_operating_state();
	
	virtual void get_design_parameters(C_csp_collector_receiver::S_csp_cr_solved_params & solved_params);

	virtual void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void startup(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void on(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		double field_control,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void estimates(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_est_out &est_out,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

	virtual double calculate_optical_efficiency(const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim);

	virtual double calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs &weather, double q_incident /*MW*/);

	virtual double get_collector_area();

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
		C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	class C_mono_eq_T_htf_loop_out : public C_monotonic_equation
	{
	private:
		C_csp_trough_collector_receiver *mpc_trough;

	public:
		C_mono_eq_T_htf_loop_out(C_csp_trough_collector_receiver *pc_trough)
		{
			mpc_trough = pc_trough;
		}
	
		virtual int operator()(double m_dot_htf_loop /*kg/s*/, double *T_htf_loop_out /*K*/);
	};

	void apply_control_defocus(double defocus /*-*/);
	void apply_component_defocus(double defocus /*-*/);

	class C_mono_eq_defocus : public C_monotonic_equation
	{	// The solver chooses a defocus and sends it to the operator. The operator 
		//    calculates a new m_q_SCA and then solves the loop_energy_balance *at max HTF mass flow rate* 
		//    and returns T_htf_SCA_out. The solver finds the defocus resulting in the target HTF outlet temp
	private:
		C_csp_trough_collector_receiver *mpc_trough;

	public:
		C_mono_eq_defocus(C_csp_trough_collector_receiver *pc_trough)
		{
			mpc_trough = pc_trough;
		}
	
		virtual int operator()(double defocus /*-*/, double *T_htf_loop_out /*K*/);
	};

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
		std::vector<double> &m_D_hdr, std::vector<double> &m_D_runner, std::string *summary = NULL);
	double pipe_sched(double De);
	double Pump_SGS(double rho, double m_dotsf, double sm);

	
};







#endif