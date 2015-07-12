#ifndef __csp_solver_core_
#define __csp_solver_core_

#include "lib_weatherfile.h"
#include "csp_solver_util.h"

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
	double m_time;		//[s]
	double m_step;		//[s]

	int m_tou;		//[-]

	C_csp_solver_sim_info()
	{
		m_time = m_step = std::numeric_limits<double>::quiet_NaN();

		m_tou = -1;
	}
};

class C_csp_weatherreader
{
private:
	weatherfile m_wf;
	bool m_first;		// flag to indicate whether this is the first call

	// member string for exception messages
	std::string m_error_msg;

	int m_ncall;

public:
	C_csp_weatherreader();

	~C_csp_weatherreader(){};

	void init();

	void timestep_call(const C_csp_solver_sim_info &p_sim_info);

	void converged();

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	struct S_outputs
	{
		int m_year;				//[yr]
		int m_month;			//[mn]
		int m_day;				//[day]
		int m_hour;				//[hr]
		double m_minute;		//[min]

		double m_global;		//[W/m2]
		double m_beam;			//[W/m2]
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

		S_outputs()
		{
			m_year = m_month = m_day = m_hour = -1;

			m_global = m_beam = m_diffuse = m_tdry = m_twet = m_tdew = m_wspd = m_wdir = m_rhum = m_pres = m_snow = m_albedo =
				m_poa = m_solazi = m_solzen = m_lat = m_lon = m_tz = m_shift = m_elev = std::numeric_limits<double>::quiet_NaN();
		}
	};

	// Member data - public so can be set from up stream code
	std::string m_filename;
	int m_trackmode;
	double m_tilt;
	double m_azimuth;

	S_outputs ms_outputs;
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

	struct S_csp_cr_solved_params
	{
		double m_T_htf_cold_des;
		double m_q_dot_rec_on_min;		//[MW]

		S_csp_cr_solved_params()
		{
			m_T_htf_cold_des = m_q_dot_rec_on_min = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_csp_cr_outputs
	{
		double m_eta_field;				//[-] Field optical efficiency
		
		double m_eta_thermal;			//[-] Receiver thermal efficiency
		double m_q_thermal;				//[MW] 'Available' receiver thermal output
		double m_q_startup;				//[MWt-hr] Receiver thermal output
		double m_m_dot_salt_tot;		//[kg/hr] Molten salt mass flow rate
		double m_T_salt_hot;			//[C] Hot HTF from receiver

		double m_time_required_su;		//[s] Time required for receiver to startup
		//int m_mode_calculated;			//[-] Receiver operation mode after last performance CALL - could still change this timestep

		S_csp_cr_outputs()
		{
			m_eta_field =  

			m_eta_thermal = m_q_thermal = m_m_dot_salt_tot = m_T_salt_hot =
				m_time_required_su = std::numeric_limits<double>::quiet_NaN();

			//m_mode_calculated = -1;
		}
	};

	virtual void init() = 0;

	virtual int get_operating_state() = 0;

	virtual void call(const C_csp_weatherreader::S_outputs &weather,
		C_csp_solver_htf_state &htf_state,
		const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
		C_csp_collector_receiver::S_csp_cr_outputs &cr_outputs,
		const C_csp_solver_sim_info &sim_info) = 0;

	virtual void get_design_parameters(C_csp_collector_receiver::S_csp_cr_solved_params & solved_params) = 0;

	virtual void converged() = 0;
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
		//int m_tou;					//[-] Time-of-use period: ONE BASED, converted to 0-based in code

		S_control_inputs()
		{
			m_standby_control = /*m_tou =*/ -1;
		}
	};

	struct S_solved_params
	{
		double m_W_dot_des;			//[MW]
		double m_eta_des;			//[MW]
		double m_q_dot_des;			//[MW]
		double m_max_frac;			//[-]
		double m_cutoff_frac;		//[-]
		double m_sb_frac;			//[-]
		double m_T_htf_hot_ref;		//[C]
		double m_m_dot_design;		//[kg/hr]

		S_solved_params()
		{
			m_W_dot_des = m_eta_des = m_q_dot_des = m_max_frac = m_cutoff_frac = 
				m_sb_frac = m_T_htf_hot_ref = m_m_dot_design = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_csp_pc_outputs
	{
		double m_P_cycle;			//[MWe] Cycle power output
		double m_eta;				//[-] Cycle thermal efficiency
		double m_T_htf_cold;		//[C] Heat transfer fluid outlet temperature
		double m_m_dot_makeup;		//[kg/hr] Cooling water makeup flow rate
		double m_m_dot_demand;		//[kg/hr] HTF required flow rate to meet power load
		double m_m_dot_htf;			//[kg/hr] Actual HTF flow rate passing through the power cycle
		double m_m_dot_htf_ref;		//[kg/hr] Calculated reference HTF flow rate at design
		double m_W_cool_par;		//[MWe] Cooling system parasitic load
		double m_P_ref;				//[MWe] Reference power level output at design
		double m_f_hrsys;			//[-] Fraction of operating heat rejection system
		double m_P_cond;			//[Pa] Condenser pressure
		
		double m_time_required_su;		//[s] Time required for receiver to startup MIN(controller timestep, calculated time to startup during call)
		double m_q_startup;			//[MWt-hr] Startup energy required
		double m_q_dot_htf;			//[MWt] Thermal power from HTF (= thermal power into cycle)

		S_csp_pc_outputs()
		{
			m_P_cycle = m_eta = m_T_htf_cold = m_m_dot_makeup = m_m_dot_demand = m_m_dot_htf = m_m_dot_htf_ref =
				m_W_cool_par = m_P_ref = m_f_hrsys = m_P_cond = std::numeric_limits<double>::quiet_NaN();
			
			m_time_required_su = m_q_startup = m_q_dot_htf = std::numeric_limits<double>::quiet_NaN();
		}
	};
	
	virtual void init() = 0;

	virtual int get_operating_state() = 0;

	virtual void get_design_parameters(C_csp_power_cycle::S_solved_params &solved_params) = 0;

	virtual void call(const C_csp_weatherreader::S_outputs &weather,
		C_csp_solver_htf_state &htf_state,
		const C_csp_power_cycle::S_control_inputs &inputs,
		C_csp_power_cycle::S_csp_pc_outputs &outputs,
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
		double m_q_heater;		//[MW] Heating power required to keep tanks at a minimum temperature
		double m_q_dot_loss;	//[MW] Storage thermal losses
		double m_q_dot_dc_to_htf;	//[MW] Thermal power to the HTF from storage
		double m_q_dot_ch_from_htf;	//[MW] Thermal power from the HTF to storage
		double m_T_hot_ave;		//[K] Average hot tank temperature over timestep
		double m_T_cold_ave;	//[K] Average cold tank temperature over timestep
		double m_T_hot_final;	//[K] Hot temperature at end of timestep
		double m_T_cold_final;	//[K] Cold temperature at end of timestep
	
		S_csp_tes_outputs()
		{
			m_q_heater = m_q_dot_loss = m_q_dot_dc_to_htf = m_q_dot_ch_from_htf = m_T_hot_ave = 
			m_T_cold_ave = m_T_hot_final = m_T_cold_final = std::numeric_limits<double>::quiet_NaN();
		}
	};

	virtual void init() = 0;

	virtual bool does_tes_exist() = 0;

	virtual double get_hot_temp() = 0;

	virtual double get_cold_temp() = 0;
	
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

private:
	C_csp_weatherreader &mc_weather;
	C_csp_collector_receiver &mc_collector_receiver;
	C_csp_power_cycle &mc_power_cycle;
	C_csp_tes &mc_tes;

	C_csp_solver_htf_state mc_cr_htf_state;
	C_csp_collector_receiver::S_csp_cr_inputs mc_cr_inputs;
	C_csp_collector_receiver::S_csp_cr_outputs mc_cr_outputs;

	C_csp_solver_htf_state mc_pc_htf_state;
	C_csp_power_cycle::S_control_inputs mc_pc_inputs;
	C_csp_power_cycle::S_csp_pc_outputs mc_pc_outputs;

	C_csp_solver_htf_state mc_tes_ch_htf_state;
	C_csp_solver_htf_state mc_tes_dc_htf_state;
	C_csp_tes::S_csp_tes_outputs mc_tes_outputs;

	C_csp_solver_sim_info mc_sim_info;

	// Hierarchy logic
	bool m_is_CR_SU__PC_OFF__TES_OFF__AUX_OFF_avail;
	bool m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail;
	bool m_is_CR_ON__PC_SU__TES_OFF__AUX_OFF_avail;
	bool m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail;
	bool m_is_CR_OFF__PC_SU__TES_DC__AUX_OFF_avail;
	bool m_is_CR_DF__PC_FULL__TES_OFF__AUX_OFF_avail;
	
	bool m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_HI_SIDE;
	bool m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_LO_SIDE;

	bool m_is_CR_ON__PC_RM_LO__TES_OFF__AUX_OFF_avail;

	bool m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_HI_SIDE;
	bool m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_LO_SIDE;

	bool m_is_CR_ON__PC_TARGET__TES_DC__AUX_OFF_avail;
	bool m_is_CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF_avail;

	bool m_is_CR_DF__PC_OFF__TES_FULL__AUX_OFF_avail;

	bool m_is_CR_OFF__PC_SB__TES_DC__AUX_OFF_avail;

	// member string for exception messages
	std::string error_msg;

		// Collector receiver design parameters
	double m_T_htf_cold_des;			//[K]
	double m_q_dot_rec_on_min;			//[MW]

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

		// Reset hierarchy logic
	void reset_hierarchy_logic();

	void init_independent();

	void solver_cr_to_pc_to_cr(double field_control_in, double tol, int &exit_mode, double &exit_tolerance);

	enum E_solver_outcomes
	{
		NO_SOLUTION,		// Models did not provide enough information with which to iterate on T_rec_in
		POOR_CONVERGENCE,	// Models solved, but convergence on T_rec_in was not within specified tolerance
		CONVERGED,			// Models solved; convergence within specified tolerance
		KNOW_NEXT_MODE,		// Models did not solve, but failure mode allowed next mode to be determined
		UNDER_TARGET_PC,	// Models solved, but could not converge because the operating mode did not allow enough thermal power to go to power cycle
		OVER_TARGET_PC		// Models solved, but could not converge because the operating mode could not reduce the mass flow rate enough to the power cycle
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
		
		//CR_ON__PC_RM__TES_OFF__AUX_OFF,			// PC_RM = Resource Match
		CR_ON__PC_RM_HI__TES_OFF__AUX_OFF,
		CR_ON__PC_RM_LO__TES_OFF__AUX_OFF,
		
		CR_DF__PC_FULL__TES_OFF__AUX_OFF,

		CR_OFF__PC_SU__TES_DC__AUX_OFF,
		CR_ON__PC_OFF__TES_CH__AUX_OFF,

		SKIP_10,

		CR_ON__PC_TARGET__TES_CH__AUX_OFF,
		CR_ON__PC_TARGET__TES_DC__AUX_OFF,

		CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF,

		CR_DF__PC_OFF__TES_FULL__AUX_OFF,
		
		CR_OFF__PC_SB__TES_DC__AUX_OFF
	};

	C_csp_solver(C_csp_weatherreader &weather,
		C_csp_collector_receiver &collector_receiver,
		C_csp_power_cycle &power_cycle,
		C_csp_tes &tes);

	~C_csp_solver(){};

	void init();

	void simulate();

	// Output vectors
	// Need to be sure these are always up-to-date as multiple operating modes are tested during one timestep
	std::vector<double> mv_time_mid;		//[hr]
	std::vector<double> mv_solzen;			//[deg]
	std::vector<double> mv_beam;			//[W/m2]
	std::vector<double> mv_eta_field;		//[-]
	std::vector<double> mv_defocus;			//[-] = m_defocus 
	std::vector<double> mv_rec_eta_thermal;	//[-] Receiver thermal efficiency
	std::vector<double> mv_rec_q_thermal;	//[MWt-hr] Receiver thermal energy output over (perhaps varying length) timestep 
	std::vector<double> mv_rec_q_startup;	//[MWt-hr] Receiver startup thermal energy
	std::vector<double> mv_pc_eta;			//[-] Power cycle efficiency (gross - no parasitics outside of power block)
	std::vector<double> mv_pc_W_gross;		//[MWe-hr] Power cycle electric gross energy (only parasitics baked into regression) over (perhaps varying length) timestep
	std::vector<double> mv_pc_q_startup;	//[MWt-hr] Power cycle startup thermal energy
	std::vector<double> mv_pc_q_thermal;	//[MWt-hr] Power cycle input thermal energy
	std::vector<double> mv_tes_q_losses;	//[MWt-hr] TES thermal losses to environment
	std::vector<double> mv_tes_q_heater;	//[MWt-hr] Energy into TES from heaters (hot+cold) to maintain tank temperatures
	std::vector<double> mv_tes_T_hot;		//[C] TES hot temperature at end of timestep
	std::vector<double> mv_tes_T_cold;		//[C] TES cold temperature at end of timestep
	std::vector<double> mv_tes_dc_q_thermal;	//[MWt-hr] TES discharge thermal energy
	std::vector<double> mv_tes_ch_q_thermal;	//[MWt-hr] TES charge thermal energy

	std::vector<double> mv_rec_m_dot;		//[kg/hr] Mass flow rate from receiver
	std::vector<double> mv_pc_m_dot;		//[kg/hr] Mass flow rate to power cycle
	std::vector<double> mv_tes_dc_m_dot;	//[kg/hr] Mass flow rate (HTF) discharged from TES
	std::vector<double> mv_tes_ch_m_dot;	//[kg/hr] Mass flow rate (HTF) charging TES
	std::vector<double> mv_m_dot_balance;	//[-] Are the sums of mass flow (close to) zero?

	std::vector<double> mv_q_balance;		//[-] Is the 1st law satisfied?

	std::vector<double> mv_operating_modes;	//[-] List of operating modes tried each timestep

	// Controller logic info	
	std::vector<double> mv_q_dot_pc_sb;		//[MW]
	std::vector<double> mv_q_dot_pc_min;	//[MW]
	std::vector<double> mv_q_dot_pc_max;	//[MW]
	std::vector<double> mv_q_dot_pc_target;	//[MW]

	std::vector<int> mv_is_rec_su_allowed;	//[-]
	std::vector<int> mv_is_pc_su_allowed;	//[-]
	std::vector<int> mv_is_pc_sb_allowed;	//[-]

	std::vector<double> mv_q_dot_est_cr_su;	 //[MW]
	std::vector<double> mv_q_dot_est_cr_on;	 //[MW]
	std::vector<double> mv_q_dot_est_tes_dc; //[MW]
	std::vector<double> mv_q_dot_est_tes_ch; //[MW]
	
	std::vector<double> mv_rec_q_dot_thermal;	//[MWt] Receiver thermal power output over (perhaps varying length) timestep 
	std::vector<double> mv_pc_q_dot_thermal;	//[MWt] Power cycle input thermal power
	std::vector<double> mv_tes_dc_q_dot_thermal;	//[MWt] TES discharge thermal power
	std::vector<double> mv_tes_ch_q_dot_thermal;	//[MWt] TES charge thermal power

};






#endif //__csp_solver_core_