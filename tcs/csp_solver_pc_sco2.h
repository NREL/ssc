#ifndef __csp_solver_pc_sco2_
#define __csp_solver_pc_sco2_

#include "csp_solver_util.h"
#include "csp_solver_core.h"

#include "sco2_pc_csp_int.h"

class C_pc_sco2 : public C_csp_power_cycle
{

private:

	C_sco2_recomp_csp mc_sco2_recomp;

	HTFProperties mc_pc_htfProps;

	double m_q_dot_design;					//[MWt]
	double m_q_dot_standby;					//[MWt]
	double m_q_dot_max;						//[MWt]
	double m_q_dot_min;						//[MWt]
	double m_startup_energy_required;		//[kW-hr]

	double m_W_dot_des;						//[MWe]
	double m_T_htf_cold_des;				//[K]

	int m_standby_control_prev;				//[-]
	double m_startup_time_remain_prev;		//[hr]
	double m_startup_energy_remain_prev;	//[kW-hr]

	int m_standby_control_calc;				//[-]
	double m_startup_time_remain_calc;		//[hr]
	double m_startup_energy_remain_calc;	//[kW-hr]

public:

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	struct S_des_par
	{
		C_sco2_recomp_csp::S_des_par ms_mc_sco2_recomp_params;

		double m_cycle_max_frac;	//[-] Maximum turbine over-design operation fraction
		double m_cycle_cutoff_frac;	//[-] Minimum turbine operation fraction
		double m_q_sby_frac;		//[-] fraction of thermal power required for standby mode
		double m_startup_time;		//[hr] time needed for power block startup
		double m_startup_frac;		//[-] fraction of design thermal power needed for startup
		double m_htf_pump_coef;		//[kW/kg/s] Pumping power to move 1 kg/s of HTF through power cycle

		S_des_par()
		{
			m_cycle_max_frac = m_cycle_cutoff_frac = m_q_sby_frac = m_startup_time = m_startup_frac = m_htf_pump_coef = std::numeric_limits<double>::quiet_NaN();
		}
	};

	S_des_par ms_params;

	C_pc_sco2();

	~C_pc_sco2(){};

	virtual void init(C_csp_power_cycle::S_solved_params &solved_params);

	virtual int get_operating_state();

	virtual double get_cold_startup_time();
	virtual double get_warm_startup_time();
	virtual double get_hot_startup_time();
	virtual double get_standby_energy_requirement();    //[MW]
	virtual double get_cold_startup_energy();    //[MWh]
	virtual double get_warm_startup_energy();    //[MWh]
	virtual double get_hot_startup_energy();    //[MWh]
	virtual double get_max_thermal_power();     //MW
	virtual double get_min_thermal_power();     //MW
	virtual double get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct);
	virtual double get_efficiency_at_load(double load_frac);

	// This can vary between timesteps for Type224, depending on remaining startup energy and time
	virtual double get_max_q_pc_startup();		//[MWt]

	virtual void call(const C_csp_weatherreader::S_outputs &weather,
		C_csp_solver_htf_1state &htf_state_in,
		const C_csp_power_cycle::S_control_inputs &inputs,
		C_csp_power_cycle::S_csp_pc_out_solver &out_solver,
		C_csp_power_cycle::S_csp_pc_out_report &out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

};


#endif	//__csp_solver_pc_sco2_