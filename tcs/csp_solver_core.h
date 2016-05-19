#ifndef __csp_solver_core_
#define __csp_solver_core_

#include <numeric>
#include <limits>

#include "lib_weatherfile.h"
#include "csp_solver_util.h"

class C_csp_solver_htf_1state
{
public:
	double m_temp;	//[C]
	double m_pres;	//[kPa]
	double m_qual;	//[-]
	double m_m_dot;	//[kg/s]

	C_csp_solver_htf_1state()
	{
		m_temp = m_pres = m_qual = m_m_dot = std::numeric_limits<double>::quiet_NaN();
	}
};

class C_csp_solver_htf_state
{
public:
	double m_m_dot;		//[kg/hr]
	double m_temp_in;	//[C]
	double m_pres_in;	//[kPa]
	double m_qual_in;	//[-]
	double m_temp_out;	//[C]
	double m_pres_out;	//[kPa]
	double m_qual_out;	//[-]

	C_csp_solver_htf_state()
	{
		m_m_dot = 
			m_temp_in = m_pres_in = m_qual_in =
			m_temp_out = m_pres_out = m_qual_out = std::numeric_limits<double>::quiet_NaN();
	}
};

class C_csp_solver_sim_info
{
public:
	double m_time;		//[s] Time at end of timestep
	double m_step;		//[s] Duration of timestep

	int m_tou;		//[-] Time-Of-Use Period

	C_csp_solver_sim_info()
	{
		m_time = m_step = std::numeric_limits<double>::quiet_NaN();

		m_tou = -1;
	}
};

class C_csp_weatherreader
{
private:
	weatherfile m_wfile;
	weather_header m_hdr;
	weather_record m_rec;
	bool m_first;		// flag to indicate whether this is the first call

	// member string for exception messages
	std::string m_error_msg;

	int m_ncall;

	int day_prev;

public:
	C_csp_weatherreader();

	~C_csp_weatherreader(){};

	void init();

	void timestep_call(const C_csp_solver_sim_info &p_sim_info);

	void converged();

    bool read_time_step(int time_step, C_csp_solver_sim_info &p_sim_info);

    int get_current_step();

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	struct S_csp_weatherreader_solved_params
	{
		double m_lat;			//[deg]
		double m_lon;			//[deg]
		double m_tz;			//[deg]
		double m_shift;			//[deg]
		double m_elev;			//[m]

		S_csp_weatherreader_solved_params()
		{
			m_lat = m_lon = m_tz = m_shift = m_elev = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_outputs
	{
		int m_year;				//[yr]
		int m_month;			//[mn]
		int m_day;				//[day]
		int m_hour;				//[hr]
		double m_minute;		//[min]

		double m_global;		//[W/m2]
		double m_beam;			//[W/m2]
		double m_hor_beam;		//[W/m2]
		double m_diffuse;		//[W/m2]
		double m_tdry;			//[C]
		double m_twet;			//[C]
		double m_tdew;			//[C]
		double m_wspd;			//[m/s]
		double m_wdir;			//[deg]
		double m_rhum;			//[%]
		double m_pres;			//[mbar]
		double m_snow;			//[cm]
		double m_albedo;		//[-] (0..1)

		double m_poa;			//[W/m2]
		double m_solazi;		//[deg]
		double m_solzen;		//[deg]
		double m_lat;			//[deg]
		double m_lon;			//[deg]
		double m_tz;			//[deg]
		double m_shift;			//[deg]
		double m_elev;			//[m]

		double m_time_rise;		//[hr]
		double m_time_set;		//[hr]

		S_outputs()
		{
			m_year = m_month = m_day = m_hour = -1;

			m_global = m_beam = m_hor_beam = m_diffuse = m_tdry = m_twet = m_tdew = m_wspd = 
				m_wdir = m_rhum = m_pres = m_snow = m_albedo =
				m_poa = m_solazi = m_solzen = m_lat = m_lon = m_tz = m_shift = m_elev =
				m_time_rise = m_time_set = std::numeric_limits<double>::quiet_NaN();
		}
	};

	// Member data - public so can be set from up stream code
	std::string m_filename;
	int m_trackmode;
	double m_tilt;
	double m_azimuth;

	S_outputs ms_outputs;
	S_csp_weatherreader_solved_params ms_solved_params;
};

class C_csp_tou
{

public:

    struct S_csp_tou_params
    {
		bool m_dispatch_optimize;
        int m_optimize_frequency;
        int m_optimize_horizon;
        double m_solver_timeout;
        double m_mip_gap;
        int m_presolve_type;
        int m_bb_type;
        int m_scaling_type;
        int m_max_iterations;

		bool m_is_write_ampl_dat;
        bool m_is_ampl_engine;
        std::string m_ampl_data_dir;
        std::string m_ampl_exec_call;
		
		bool m_is_block_dispatch;

		bool m_use_rule_1;
		double m_standby_off_buffer;

		bool m_use_rule_2;
		double m_q_dot_rec_des_mult;
		double m_f_q_dot_pc_overwrite;


        S_csp_tou_params()
        {
            m_dispatch_optimize = false;        //Do dispatch optimization
            m_optimize_frequency = 24;          //[hr] Optimization occurs every X hours
            m_optimize_horizon = 48;            //[hr] Optimization time horizon
            m_solver_timeout = 5.;
            m_mip_gap = 0.055;
            m_max_iterations = 10000;
            m_bb_type = -1;
            m_presolve_type = -1;
            m_scaling_type = -1;

			m_is_write_ampl_dat = false;        //write ampl data files?
            m_is_ampl_engine = false;           //run dispatch with external AMPL engine?
            m_ampl_data_dir = "";               //directory where files should be written 
            m_ampl_exec_call = "";
			
			m_is_block_dispatch = true;			// Either this or m_dispatch_optimize must be true
			
			// Rule 1: if the sun sets (or does not rise) in m_standby_off_buffer [hours], then do not allow power cycle standby
			m_use_rule_1 = false;				
			m_standby_off_buffer = -1.23;		//[hr]


			// Rule 2: If both:
			//   1) Block Dispatch calls for PC OFF
			//   2) Thermal storage charge capacity is less than the product of 'm_q_dot_rec_des_mult' and the receiver design output
			//
			//   THEN: Run power cycle at 'm_f_q_dot_pc_overwrite' until either:
			//   1) the Block Dispatch target fraction calls for PC ON
			//   2) the PC shuts off due to lack of thermal resource
			//   
			m_use_rule_2 = false;
			m_q_dot_rec_des_mult = -1.23;
			m_f_q_dot_pc_overwrite = 1.23;

        };

    } mc_dispatch_params;

	struct S_csp_tou_outputs
	{
        size_t m_csp_op_tou;
		size_t m_pricing_tou;
		double m_f_turbine;
		double m_price_mult;

		S_csp_tou_outputs()
		{
            m_csp_op_tou = m_pricing_tou = -1;

			m_f_turbine = m_price_mult = std::numeric_limits<double>::quiet_NaN();
		}
	};

	C_csp_tou(){};

	~C_csp_tou(){};

	void init_parent();

	virtual void init() = 0;

	virtual void call(double time_s, C_csp_tou::S_csp_tou_outputs & tou_outputs) = 0;
};

class C_csp_collector_receiver
{

public:
	C_csp_collector_receiver(){};

	~C_csp_collector_receiver(){};

	enum E_csp_cr_modes
	{
		OFF = 0,
		STARTUP,
		ON,
		STEADY_STATE
	};

	struct S_csp_cr_init_inputs
	{
		double m_latitude;		//[deg]
		double m_longitude;		//[deg]
		double m_shift;			//[deg]

		S_csp_cr_init_inputs()
		{
			m_latitude = m_longitude = m_shift = std::numeric_limits<double>::quiet_NaN();	
		}
	};
	
	struct S_csp_cr_solved_params
	{
		double m_T_htf_cold_des;		//[K]
		double m_q_dot_rec_on_min;		//[MW]
		double m_q_dot_rec_des;			//[MW]
		double m_A_aper_total;			//[m^2] Total solar field aperture area

		S_csp_cr_solved_params()
		{
			m_T_htf_cold_des = m_q_dot_rec_on_min = m_q_dot_rec_des = m_A_aper_total = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_csp_cr_inputs
	{	
		double m_field_control;			//[-] Defocus signal from controller (can PC and TES accept all receiver output?)
		int m_input_operation_mode;		//[-]

		S_csp_cr_inputs()
		{
			m_field_control = std::numeric_limits<double>::quiet_NaN();

			m_input_operation_mode = -1;
		}
	};
	
	struct S_csp_cr_out_solver
	{	
		// Collector receiver outputs that must be defined in the CR call() for the solver to succeed.
			// The controller only checks whether this value is > 0. 
			// If it is <= 0.0, then the controller assumes that startup failed
			// This value is also reported as a modeled timestep output
		double m_q_startup;				//[MWt-hr] 
		double m_time_required_su;		//[s] Time required for receiver to startup
		double m_m_dot_salt_tot;		//[kg/hr] Molten salt mass flow rate
		double m_q_thermal;				//[MWt] 'Available' receiver thermal output
		double m_T_salt_hot;			//[C] Hot HTF from receiver
			
		// These are used for the parasitic class call(), so could be zero...
		double m_E_fp_total;			//[MW] Solar field freeze protection power
		double m_W_dot_col_tracking;	//[MWe] Collector tracking power
		double m_W_dot_htf_pump;		//[MWe] HTF pumping power
			
		S_csp_cr_out_solver()
		{
			m_q_thermal = m_q_startup = m_m_dot_salt_tot = m_T_salt_hot = m_W_dot_htf_pump = 
				m_W_dot_col_tracking = m_time_required_su = m_E_fp_total = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_csp_cr_out_report
	{
		double m_q_dot_field_inc;		//[MWt] Field incident thermal power (from the sun!)
		double m_eta_field;				//[-] Field optical efficiency
		
		double m_q_dot_rec_inc;         //[MWt] Receiver incident thermal power (after reflection losses)
		double m_eta_thermal;			//[-] Receiver thermal efficiency
		double m_q_dot_piping_loss;		//[MWt] Thermal power lost from piping to surroundings

		S_csp_cr_out_report()
		{
			m_q_dot_field_inc = m_eta_field = 
				m_q_dot_rec_inc = m_eta_thermal = m_q_dot_piping_loss = std::numeric_limits<double>::quiet_NaN();
		}
	};

	virtual void init( const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs,
		C_csp_collector_receiver::S_csp_cr_solved_params & solved_params) = 0;

	virtual int get_operating_state() = 0;

    virtual double get_startup_time() = 0;
    virtual double get_startup_energy(double step /*sec*/) = 0; //MWh
    virtual double get_pumping_parasitic_coef() = 0;  //MWe/MWt
    virtual double get_min_power_delivery() = 0;    //MWt

	virtual void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info) = 0;

	virtual void startup(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info) = 0;

	virtual void on(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		double field_control,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info) = 0;

	struct S_csp_cr_est_out
	{
		double m_q_startup_avail;	//[MWt] Estimate startup thermal power. Only > 0 if solar avail AND cr is OFF or Starting Up
		double m_q_dot_avail;		//[MWt] Estimated output if cr is ON and producing useful thermal power

		S_csp_cr_est_out()
		{
			m_q_startup_avail = m_q_dot_avail = std::numeric_limits<double>::quiet_NaN();
		}
	};

	virtual void estimates(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_est_out &est_out,
		const C_csp_solver_sim_info &sim_info) = 0;

	virtual void converged() = 0;

    virtual double calculate_optical_efficiency( const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim ) = 0;

    virtual double calculate_thermal_efficiency_approx( const C_csp_weatherreader::S_outputs &weather, double q_incident ) = 0; //very approximate thermal efficiency for optimization projections

    virtual double get_collector_area() = 0;
};


class C_csp_power_cycle
{

public:
	C_csp_power_cycle(){};

	~C_csp_power_cycle(){};

	enum E_csp_power_cycle_modes
	{
		STARTUP = 0,
		ON,
		STANDBY,
		OFF,
		STARTUP_CONTROLLED
	};

	struct S_control_inputs
	{
		int m_standby_control;		//[-] Control signal indicating standby mode
		double m_m_dot;				//[kg/hr] HTF mass flow rate to power cycle
		//int m_tou;					//[-] Time-of-use period: ONE BASED, converted to 0-based in code

		S_control_inputs()
		{
			m_standby_control = /*m_tou =*/ -1;
		}
	};

	struct S_solved_params
	{
		double m_W_dot_des;			//[MW]
		double m_eta_des;			//[-]
		double m_q_dot_des;			//[MW]
		double m_q_startup;			//[MWt-hr]
		double m_max_frac;			//[-]
		double m_cutoff_frac;		//[-]
		double m_sb_frac;			//[-]
		double m_T_htf_hot_ref;		//[C]
		double m_m_dot_design;		//[kg/hr]

		S_solved_params()
		{
			m_W_dot_des = m_eta_des = m_q_dot_des = m_q_startup = m_max_frac = m_cutoff_frac = 
				m_sb_frac = m_T_htf_hot_ref = m_m_dot_design = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_csp_pc_out_solver
	{
		double m_time_required_su;	//[s] Time required for receiver to startup MIN(controller timestep, calculated time to startup during call)
		double m_P_cycle;			//[MWe] Cycle power output
		double m_T_htf_cold;		//[C] Heat transfer fluid outlet temperature
		double m_q_dot_htf;			//[MWt] Thermal power from HTF (= thermal power into cycle)
		double m_m_dot_htf;			//[kg/hr] Actual HTF flow rate passing through the power cycle

			// Parasitics, plant net power equation
		double m_W_dot_htf_pump;	//[MWe] HTF pumping power
		double m_W_cool_par;		//[MWe] Cooling system parasitic load

		S_csp_pc_out_solver()
		{
			m_time_required_su = m_P_cycle = m_T_htf_cold = m_q_dot_htf = m_m_dot_htf =
				m_W_dot_htf_pump = m_W_cool_par = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_csp_pc_out_report
	{
		double m_eta;				//[-] Cycle thermal efficiency
		double m_m_dot_makeup;		//[kg/hr] Cooling water makeup flow rate
		double m_m_dot_demand;		//[kg/hr] HTF required flow rate to meet power load
		double m_m_dot_htf_ref;		//[kg/hr] Calculated reference HTF flow rate at design
		double m_P_ref;				//[MWe] Reference power level output at design
		double m_f_hrsys;			//[-] Fraction of operating heat rejection system
		double m_P_cond;			//[Pa] Condenser pressure		
		
		double m_q_startup;			//[MWt-hr] Startup energy required

		S_csp_pc_out_report()
		{
			m_eta = m_m_dot_makeup = m_m_dot_demand = m_m_dot_htf_ref =
				m_P_ref = m_f_hrsys = m_P_cond = std::numeric_limits<double>::quiet_NaN();
			
			m_q_startup = std::numeric_limits<double>::quiet_NaN();
		}
	};
	
	virtual void init(C_csp_power_cycle::S_solved_params &solved_params) = 0;

	virtual int get_operating_state() = 0;

    //required gets
    virtual double get_cold_startup_time() = 0;
    virtual double get_warm_startup_time() = 0;
    virtual double get_hot_startup_time() = 0;
    virtual double get_standby_energy_requirement() = 0; //[MW]
    virtual double get_cold_startup_energy(double step /*sec*/) = 0;    //[MWh]
    virtual double get_warm_startup_energy(double step /*sec*/) = 0;    //[MWh]
    virtual double get_hot_startup_energy(double step /*sec*/) = 0;    //[MWh]
    virtual double get_max_thermal_power() = 0;     //MW
    virtual double get_min_thermal_power() = 0;     //MW
    virtual double get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct) = 0; //-
    virtual double get_efficiency_at_load(double load_frac) = 0;
	
	// This can vary between timesteps for Type224, depending on remaining startup energy and time
	virtual double get_max_q_pc_startup() = 0;		//[MWt]

	virtual void call(const C_csp_weatherreader::S_outputs &weather,
		C_csp_solver_htf_1state &htf_state_in,
		const C_csp_power_cycle::S_control_inputs &inputs,
		C_csp_power_cycle::S_csp_pc_out_solver &out_solver,
		C_csp_power_cycle::S_csp_pc_out_report &out_report,
		const C_csp_solver_sim_info &sim_info) = 0;

	virtual void converged() = 0;

};

class C_csp_tes
{

public:
	C_csp_tes(){};

	~C_csp_tes(){};

	struct S_csp_tes_outputs
	{
		double m_q_heater;			//[MWe] Heating power required to keep tanks at a minimum temperature
		double m_W_dot_rhtf_pump;	//[MWe] Pumping power for Receiver HTF thru storage
		double m_q_dot_loss;		//[MWt] Storage thermal losses
		double m_q_dot_dc_to_htf;	//[MWt] Thermal power to the HTF from storage
		double m_q_dot_ch_from_htf;	//[MWt] Thermal power from the HTF to storage
		double m_T_hot_ave;		//[K] Average hot tank temperature over timestep
		double m_T_cold_ave;	//[K] Average cold tank temperature over timestep
		double m_T_hot_final;	//[K] Hot temperature at end of timestep
		double m_T_cold_final;	//[K] Cold temperature at end of timestep
	
		S_csp_tes_outputs()
		{
			m_q_heater = m_W_dot_rhtf_pump = m_q_dot_loss = m_q_dot_dc_to_htf = m_q_dot_ch_from_htf = m_T_hot_ave = 
			m_T_cold_ave = m_T_hot_final = m_T_cold_final = std::numeric_limits<double>::quiet_NaN();
		}
	};

	virtual void init() = 0;

	virtual bool does_tes_exist() = 0;

	virtual double get_hot_temp() = 0;

	virtual double get_cold_temp() = 0;

    virtual double get_initial_charge_energy() = 0; //MWh

    virtual double get_min_charge_energy() = 0; //MWh

    virtual double get_max_charge_energy() = 0; //MWh

    virtual double get_degradation_rate() = 0;  // s^-1

    virtual void discharge_avail_est(double T_cold_K, double step_s, double &q_dot_dc_est, double &m_dot_field_est, double &T_hot_field_est) = 0;
	
	virtual void charge_avail_est(double T_hot_K, double step_s, double &q_dot_ch_est, double &m_dot_field_est, double &T_cold_field_est) = 0;

	virtual bool discharge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/, double T_htf_cold_in, double & T_htf_hot_out /*K*/, C_csp_tes::S_csp_tes_outputs &outputs) = 0;
	
	virtual void discharge_full(double timestep /*s*/, double T_amb /*K*/, double T_htf_cold_in, double & T_htf_hot_out /*K*/, double & m_dot_htf_out /*kg/s*/, C_csp_tes::S_csp_tes_outputs &outputs) = 0;

	virtual bool charge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/, double T_htf_hot_in, double & T_htf_cold_out /*K*/, C_csp_tes::S_csp_tes_outputs &outputs) = 0;

	virtual void charge_full(double timestep /*s*/, double T_amb /*K*/, double T_htf_hot_in /*K*/, double & T_htf_cold_out /*K*/, double & m_dot_htf_out /*kg/s*/, C_csp_tes::S_csp_tes_outputs &outputs) = 0;
	
	virtual void idle(double timestep, double T_amb, C_csp_tes::S_csp_tes_outputs &outputs) = 0;
	
	virtual void converged() = 0;
};

class C_csp_solver
{

public:

	enum E_post_proc_outputs
	{
		PC_Q_STARTUP,				//[MWt-hr] Receiver startup thermal energy consumed
	
		N_END_POST_PROC
	};
	
	enum E_reported_outputs
	{
		// Ouputs that are NOT reported as weighted averages
		TIME_FINAL,       //[hr]		
		N_OP_MODES,       //[-]
		ERR_M_DOT,        //[-] Relative mass conservation error
		ERR_Q_DOT,        //[-] Relative energy conservation error
		OP_MODE_1,        //[-] First operating mode in reporting timestep - always should be valid
		OP_MODE_2,        //[-] 2nd operating mode in reporting timestep - not always populated
		OP_MODE_3,        //[-] 3rd operating mode in reporting timestep - usually NOT populated
		

		// **************************************************************
		//      ONLY instantaneous outputs that are reported as the first value
		//        if multiple csp-timesteps for one reporting timestep
		// **************************************************************
		TOU_PERIOD,       //[-] CSP operating TOU period
		PRICING_MULT,     //[-] PPA price multiplier
		PC_Q_DOT_SB,      //[MWt] PC required standby thermal power
		PC_Q_DOT_MIN,     //[MWt] PC required min thermal power
		PC_Q_DOT_TARGET,  //[MWt] PC target thermal power
		PC_Q_DOT_MAX,     //[MWt] PC allowable max thermal power
		CTRL_IS_REC_SU,   //[-] Control decision: is receiver startup allowed?
		CTRL_IS_PC_SU,    //[-] Control decision: is power cycle startup allowed?
		CTRL_IS_PC_SB,    //[-] Control decision: is power cycle standby allowed?
		EST_Q_DOT_CR_SU,  //[MWt] Estimate receiver startup thermal power
		EST_Q_DOT_CR_ON,  //[MWt] Estimate receiver thermal power to HTF
		EST_Q_DOT_DC,     //[MWt] Estimate max TES dc thermal power
		EST_Q_DOT_CH,     //[MWt] Estimate max TES ch thermal power
		CTRL_OP_MODE_SEQ_A,   //[-] First 3 operating modes tried
		CTRL_OP_MODE_SEQ_B,   //[-] Next 3 operating modes tried
		CTRL_OP_MODE_SEQ_C,   //[-] Final 3 operating modes tried
        DISPATCH_SOLVE_STATE,   //[-] The status of the dispatch optimization solver
        DISPATCH_SOLVE_ITER,    //[-] Number of iterations before completing dispatch optimization
        DISPATCH_SOLVE_OBJ,     //[?] Objective function value achieved by the dispatch optimization solver
        DISPATCH_SOLVE_OBJ_RELAX,   //[?] Objective function value for the relaxed continuous problem 
        DISPATCH_QSF_EXPECT,        //MWt Expected total solar field energy generation in dispatch model
        DISPATCH_QSFPROD_EXPECT,    //MWt Expected useful solar field energy generation in dispatch model
        DISPATCH_QSFSU_EXPECT,      //MWt Solar field startup energy in dispatch model
        DISPATCH_TES_EXPECT,        //MWht Thermal energy storage charge state in dispatch model
        DISPATCH_PCEFF_EXPECT,      //-  Expected power cycle efficiency adjustment in dispatch model
        DISPATCH_SFEFF_EXPECT,      //-  Expected solar field thermal efficiency adjustment in dispatch model
        DISPATCH_QPBSU_EXPECT,      //MWt   Power cycle startup energy consumption in dispatch model
        DISPATCH_WPB_EXPECT,        //MWe   Power cycle electricity production in dispatch model
        DISPATCH_REV_EXPECT,        //MWe*fact Power cycle electricity production times revenue factor in dispatch model
        DISPATCH_PRES_NCONSTR,      //- Number of constraint relationships in dispatch model formulation
        DISPATCH_PRES_NVAR,         //- Number of variables in dispatch model formulation
        DISPATCH_SOLVE_TIME,        //sec   Time required to solve the dispatch model at each instance

		// **************************************************************
		//      ONLY Outputs that are reported as weighted averages if 
		//       multiple csp-timesteps for one reporting timestep
		// **************************************************************
		SOLZEN,			      //[deg] Solar zenith angle
		BEAM,			      //[W/m^2] Resource beam normal irradiance
		CR_Q_INC,             //[MWt] Field incident thermal power
		CR_OPT_ETA,		      //[-] Collector-receiver optical efficiency
		CR_DEFOCUS,           //[-] Field optical focus fraction
		REC_Q_DOT_INC,        //[MWt] Receiver incident thermal power
		REC_ETA_THERMAL,      //[-] Receiver thermal efficiency
		REC_Q_DOT,            //[MWt] Receiver thermal power to HTF including piping losses
		REC_M_DOT,            //[kg/hr] Receiver mass flow rate
		REC_Q_DOT_STARTUP,    //[MWt] Receiver startup thermal power consumed
		REC_T_IN,             //[C] Receiver HTF inlet temperature
		REC_T_OUT,            //[C] Receiver HTF outlet temperature
		CR_Q_DOT_PIPING_LOSS, //[MWt] Tower piping losses		   
		PC_ETA_THERMAL,       //[-] Cycle thermal efficiency (gross)
		PC_Q_DOT,             //[-] Cycle input thermal power
		PC_M_DOT,             //[kg/hr] Cycle HTF mass flow rate
		PC_Q_DOT_STARTUP,     //[MWt] Cycle startup thermal power
		PC_W_DOT,             //[MWe] Cycle electric power output (gross)
		PC_T_IN,              //[C] Cycle HTF inlet temperature
		PC_T_OUT,             //[C] Cycle HTF outlet temperature
		PC_M_DOT_WATER,       //[kg/hr] Cycle water consumption: makeup + cooling
		TES_Q_DOT_LOSS,       //[MWt] TES thermal losses
		TES_W_DOT_HEATER,     //[MWe] TES freeze protection power
		TES_T_HOT,            //[C] TES final hot tank temperature
		TES_T_COLD,           //[C] TES final cold tank temperature
		TES_Q_DOT_DC,         //[MWt] TES discharge thermal power
		TES_Q_DOT_CH,         //[MWt] TES charge thermal power
        TES_E_CH_STATE,       //[MWht] TES charge state at the end of the time step
		TES_M_DOT_DC,         //[MWt] TES discharge mass flow rate
		TES_M_DOT_CH,         //[MWt] TES charge mass flow rate
		COL_W_DOT_TRACK,      //[MWe] Parasitic collector tracking, startup, stow power consumption
		CR_W_DOT_PUMP,        //[MWe] Parasitic tower HTF pump power
		SYS_W_DOT_PUMP,       //[MWe] Parasitic PC and TES HTF pump power
		PC_W_DOT_COOLING,     //[MWe] Parasitic condenser operation power
		SYS_W_DOT_FIXED,      //[MWe] Parasitic fixed power consumption
		SYS_W_DOT_BOP,        //[MWe] Parasitic BOP power consumption
		W_DOT_NET,            //[MWe] System total electric power to grid

		N_END	
	};

	struct S_sim_setup
	{
		double m_sim_time_start;
		double m_sim_time_end;
		double m_report_step;

		S_sim_setup()
		{
			m_sim_time_start = m_sim_time_end = m_report_step = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_csp_system_params
	{
		double m_pb_fixed_par;		//[MWe/MWcap]
		
		double m_bop_par;			//[MWe/MWcap]
		double m_bop_par_f;			//[-]
		double m_bop_par_0;			//[-]
		double m_bop_par_1;			//[-]
		double m_bop_par_2;			//[-]

		S_csp_system_params()
		{
			m_pb_fixed_par =

			m_bop_par = m_bop_par_f = m_bop_par_0 = m_bop_par_1 = m_bop_par_2 = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:
	C_csp_weatherreader &mc_weather;
	C_csp_collector_receiver &mc_collector_receiver;
	C_csp_power_cycle &mc_power_cycle;
	C_csp_tes &mc_tes;
	C_csp_tou &mc_tou;

	S_csp_system_params & ms_system_params;

	C_csp_solver_htf_1state mc_cr_htf_state_in;
	C_csp_collector_receiver::S_csp_cr_out_solver mc_cr_out_solver;
	C_csp_collector_receiver::S_csp_cr_out_report mc_cr_out_report;

	C_csp_solver_htf_1state mc_pc_htf_state_in;
	C_csp_power_cycle::S_control_inputs mc_pc_inputs;
	C_csp_power_cycle::S_csp_pc_out_solver mc_pc_out_solver;
	C_csp_power_cycle::S_csp_pc_out_report mc_pc_out_report;

	C_csp_solver_htf_state mc_tes_ch_htf_state;
	C_csp_solver_htf_state mc_tes_dc_htf_state;
	C_csp_tes::S_csp_tes_outputs mc_tes_outputs;

    C_csp_tou::S_csp_tou_outputs mc_tou_outputs;

	C_csp_solver_sim_info mc_sim_info;

	// Hierarchy logic
	bool m_is_CR_SU__PC_OFF__TES_OFF__AUX_OFF_avail;
	bool m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail;
	bool m_is_CR_ON__PC_SU__TES_OFF__AUX_OFF_avail;
	bool m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail;
	bool m_is_CR_OFF__PC_SU__TES_DC__AUX_OFF_avail;
	bool m_is_CR_DF__PC_MAX__TES_OFF__AUX_OFF_avail;
	
	bool m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_HI_SIDE;
	bool m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_LO_SIDE;

	bool m_is_CR_ON__PC_RM_LO__TES_OFF__AUX_OFF_avail;

	bool m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_HI_SIDE;
	bool m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_LO_SIDE;

	bool m_is_CR_ON__PC_TARGET__TES_DC__AUX_OFF_avail;
	bool m_is_CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF_avail;

	bool m_is_CR_DF__PC_OFF__TES_FULL__AUX_OFF_avail;

	bool m_is_CR_OFF__PC_SB__TES_DC__AUX_OFF_avail;
	bool m_is_CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF_avail;
	bool m_is_CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF_avail;

	bool m_is_CR_ON__PC_SB__TES_CH__AUX_OFF_avail;
	bool m_is_CR_SU__PC_MIN__TES_EMPTY__AUX_OFF_avail;
	bool m_is_CR_SU__PC_SB__TES_DC__AUX_OFF_avail;
	bool m_is_CR_ON__PC_SB__TES_DC__AUX_OFF_avail;

	bool m_is_CR_OFF__PC_TARGET__TES_DC__AUX_OFF_avail;
	bool m_is_CR_SU__PC_TARGET__TES_DC__AUX_OFF_avail;
	bool m_is_CR_ON__PC_RM_HI__TES_FULL__AUX_OFF_avail;

	bool m_is_CR_ON__PC_MIN__TES_EMPTY__AUX_OFF_avail;

	bool m_is_CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF_avail;

	bool m_is_CR_DF__PC_MAX__TES_FULL__AUX_OFF_avail;

	bool m_is_CR_ON__PC_SB__TES_FULL__AUX_OFF_avail;

	bool m_is_CR_SU__PC_SU__TES_DC__AUX_OFF_avail;

	bool m_is_CR_ON__PC_SU__TES_CH__AUX_OFF_avail;

	bool m_is_CR_DF__PC_SU__TES_FULL__AUX_OFF_avail;

	bool m_is_CR_DF__PC_SU__TES_OFF__AUX_OFF_avail;

	// member string for exception messages
	std::string error_msg;

		// Collector receiver design parameters
	double m_T_htf_cold_des;			//[K]
	double m_q_dot_rec_on_min;			//[MW]
	double m_q_dot_rec_des;				//[MW]

		// Power cycle design parameters
	double m_cycle_W_dot_des;			//[MW]
	double m_cycle_eta_des;				//[-]
	double m_cycle_q_dot_des;			//[MW]
	double m_cycle_max_frac;			//[-]
	double m_cycle_cutoff_frac;			//[-]
	double m_cycle_sb_frac_des;			//[-]
	double m_cycle_T_htf_hot_des;		//[K]
	double m_m_dot_pc_des;				//[kg/hr]

		// Storage logic
	bool m_is_tes;			//[-] True: plant has storage

		// Reporting and Output Tracking
	int m_i_reporting;					//[-]
	double m_sim_time_start;			//[s]
	double m_sim_time_end;				//[s]
	double m_sim_step_size_baseline;	//[s]
	double m_report_time_start;			//[s]
	double m_report_time_end;			//[s]
	double m_report_step;				//[s]
	 

		// Reset hierarchy logic
	void reset_hierarchy_logic();

	void solver_cr_to_pc_to_cr(double field_control_in, double tol, int &exit_mode, double &exit_tolerance);
	 
	void solver_pc_su_controlled__tes_dc(double step_tol /*s*/,
		double &time_pc_su /*s*/, 
		int & exit_mode, double &T_pc_in_exit_tolerance);
	
	void solver_cr_on__pc_fixed__tes_ch(double q_dot_pc_fixed /*MWt*/, int power_cycle_mode, 
		double field_control_in, 
		double tol, 
		int &T_rec_in_exit_mode, double &T_rec_in_exit_tolerance,
		int &q_pc_exit_mode, double &q_pc_exit_tolerance);

	void solver_cr_on__pc_fixed__tes_dc(double q_dot_pc_fixed /*MWt*/, int power_cycle_mode,
		double field_control_in,
		double tol,
		int &T_rec_in_exit_mode, double &T_rec_in_exit_tolerance,
		int &q_pc_exit_mode, double &q_pc_exit_tolerance);

	void solver_pc_fixed__tes_empty(double q_dot_pc_fixed /*MWt*/,
		double tol,
		double & time_tes_dc,
		int &T_tes_in_exit_mode, double &T_tes_in_exit_tolerance,
		int &q_pc_exit_mode, double &q_pc_exit_tolerance);

	void solver_pc_fixed__tes_dc(double q_dot_pc_fixed /*MWt*/, int power_cycle_mode,
		double tol,
		int &T_cold_exit_mode, double &T_cold_exit_tolerance,
		int &q_pc_exit_mode, double &q_pc_exit_tolerance);

	void solver_cr_on__pc_float__tes_full(int power_cycle_mode,
		double field_control_in,
		double tol,
		int &T_rec_in_exit_mode, double &T_rec_in_exit_tolerance);

	enum E_solver_outcomes
	{
		NO_SOLUTION,		// Models did not provide enough information with which to iterate on T_rec_in
		POOR_CONVERGENCE,	// Models solved, but convergence on T_rec_in was not within specified tolerance
		CONVERGED,			// Models solved; convergence within specified tolerance
		KNOW_NEXT_MODE,		// Models did not solve, but failure mode allowed next mode to be determined
		UNDER_TARGET_PC,	// Models solved, but could not converge because the operating mode did not allow enough thermal power to go to power cycle
		OVER_TARGET_PC,		// Models solved, but could not converge because the operating mode could not reduce the mass flow rate enough to the power cycle
		REC_IS_OFF			// Collector-receiver model did not produce power
	};

	// Solved Controller Variables
	double m_defocus;		//[-] (1..0) Should only be less than 1 if receiver is on, but defocused
	

public:

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	// Vector to track operating modes
	std::vector<int> m_op_mode_tracking;

	enum tech_operating_modes
	{
		ENTRY_MODE = 0,
		
		CR_OFF__PC_OFF__TES_OFF__AUX_OFF,
		CR_SU__PC_OFF__TES_OFF__AUX_OFF,
		CR_ON__PC_SU__TES_OFF__AUX_OFF,
		CR_ON__PC_SB__TES_OFF__AUX_OFF,
		
		CR_ON__PC_RM_HI__TES_OFF__AUX_OFF,
		CR_ON__PC_RM_LO__TES_OFF__AUX_OFF,
		
		CR_DF__PC_MAX__TES_OFF__AUX_OFF,

		CR_OFF__PC_SU__TES_DC__AUX_OFF,
		CR_ON__PC_OFF__TES_CH__AUX_OFF,

		SKIP_10,

		CR_ON__PC_TARGET__TES_CH__AUX_OFF,
		CR_ON__PC_TARGET__TES_DC__AUX_OFF,

		CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF,

		CR_DF__PC_OFF__TES_FULL__AUX_OFF,
		
		CR_OFF__PC_SB__TES_DC__AUX_OFF,
		CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF,
		CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF,

		CR_ON__PC_SB__TES_CH__AUX_OFF,
		CR_SU__PC_MIN__TES_EMPTY__AUX_OFF,

		SKIP_20,

		CR_SU__PC_SB__TES_DC__AUX_OFF,
		CR_ON__PC_SB__TES_DC__AUX_OFF,
		CR_OFF__PC_TARGET__TES_DC__AUX_OFF,
		CR_SU__PC_TARGET__TES_DC__AUX_OFF,
		CR_ON__PC_RM_HI__TES_FULL__AUX_OFF,

		CR_ON__PC_MIN__TES_EMPTY__AUX_OFF,

		CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF,

		CR_DF__PC_MAX__TES_FULL__AUX_OFF,

		CR_ON__PC_SB__TES_FULL__AUX_OFF,

		SKIP_30,

		CR_SU__PC_SU__TES_DC__AUX_OFF,

		CR_ON__PC_SU__TES_CH__AUX_OFF,

		CR_DF__PC_SU__TES_FULL__AUX_OFF,

		CR_DF__PC_SU__TES_OFF__AUX_OFF
	};

	C_csp_solver(C_csp_weatherreader &weather,
		C_csp_collector_receiver &collector_receiver,
		C_csp_power_cycle &power_cycle,
		C_csp_tes &tes,
		C_csp_tou &tou,
		S_csp_system_params &system);

	~C_csp_solver(){};

	void init();

	void Ssimulate(C_csp_solver::S_sim_setup & sim_setup, 
					bool(*mf_callback)(void *data, double percent, C_csp_messages *csp_messages, float time_sec), void *m_cdata,
					float **ptr_array,
					float **post_proc_array);

	float **mp_reporting_array;
	float **mp_post_proc_array;

	void set_outputs_at_reporting_interval();

	// Output vectors
	// Need to be sure these are always up-to-date as multiple operating modes are tested during one timestep
	std::vector< std::vector< double > > mvv_outputs_temp;

};


#endif //__csp_solver_core_
