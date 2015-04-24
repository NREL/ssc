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

	C_csp_solver_sim_info()
	{
		m_time = m_step = std::numeric_limits<double>::quiet_NaN();
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

		S_csp_cr_solved_params()
		{
			m_T_htf_cold_des = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_csp_cr_outputs
	{
		double m_q_thermal;				//[MW] 'Available' receiver thermal output
		double m_m_dot_salt_tot;		//[kg/hr] Molten salt mass flow rate
		double m_T_salt_hot;			//[C] Hot HTF from receiver

		double m_time_required_su;		//[s] Time required for receiver to startup
		//int m_mode_calculated;			//[-] Receiver operation mode after last performance CALL - could still change this timestep

		S_csp_cr_outputs()
		{
			m_q_thermal = m_m_dot_salt_tot = m_T_salt_hot =
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
		OFF	
	};

	struct S_control_inputs
	{
		int m_standby_control;		//[-] Control signal indicating standby mode
		int m_tou;					//[-] Time-of-use period: ONE BASED, converted to 0-based in code

		S_control_inputs()
		{
			m_standby_control = m_tou = -1;
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

		S_solved_params()
		{
			m_W_dot_des = m_eta_des = m_q_dot_des = m_max_frac = m_cutoff_frac = 
				m_sb_frac = m_T_htf_hot_ref = std::numeric_limits<double>::quiet_NaN();
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

		S_csp_pc_outputs()
		{
			m_P_cycle = m_eta = m_T_htf_cold = m_m_dot_makeup = m_m_dot_demand = m_m_dot_htf = m_m_dot_htf_ref =
				m_W_cool_par = m_P_ref = m_f_hrsys = m_P_cond = std::numeric_limits<double>::quiet_NaN();
			
			m_time_required_su = std::numeric_limits<double>::quiet_NaN();
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

class C_csp_solver
{

private:
	C_csp_weatherreader &mc_weather;
	C_csp_collector_receiver &mc_collector_receiver;
	C_csp_power_cycle &mc_power_cycle;

	C_csp_solver_sim_info mc_sim_info;

	// member string for exception messages
	std::string error_msg;

		// Collector receiver design parameters
	double m_T_htf_cold_des;			//[K]

		// Power cycle design parameters
	double m_cycle_W_dot_des;			//[MW]
	double m_cycle_eta_des;				//[-]
	double m_cycle_q_dot_des;			//[MW]
	double m_cycle_max_frac;			//[-]
	double m_cycle_cutoff_frac;			//[-]
	double m_cycle_sb_frac_des;			//[-]
	double m_cycle_T_htf_hot_des;		//[K]

	void init_independent();

	

public:

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	enum tech_operating_modes
	{
		CR_OFF__PC_OFF__TES_OFF__AUX_OFF = 1,
		CR_SU__PC_OFF__TES_OFF__AUX_OFF,
		CR_ON__PC_SU__TES_OFF__AUX_OFF,
		CR_ON__PC_SB__TES_OFF__AUX_OFF,
		CR_ON__PC_RM__TES_OFF__AUX_OFF,			// PC_RM = Resource Match
		CR_DF__PC_FULL__TES_OFF__AUX_OFF
	};

	C_csp_solver(C_csp_weatherreader &weather,
		C_csp_collector_receiver &collector_receiver,
		C_csp_power_cycle &power_cycle);

	~C_csp_solver(){};

	void init();

	void simulate();
};






#endif //__csp_solver_core_