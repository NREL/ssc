#ifndef __csp_solver_pc_sco2_
#define __csp_solver_pc_sco2_

#include "csp_solver_util.h"
#include "csp_solver_core.h"

class C_pc_sco2 : public C_csp_power_cycle
{

private:

	double abc;

public:

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	C_pc_sco2();

	~C_pc_sco2(){};

	virtual void init(C_csp_power_cycle::S_solved_params &solved_params);

	virtual int get_operating_state();

	virtual double get_cold_startup_time();
	virtual double get_warm_startup_time();
	virtual double get_hot_startup_time();
	virtual double get_standby_energy_requirement();    //[MW]
	virtual double get_cold_startup_energy(double step /*sec*/);    //[MWh]
	virtual double get_warm_startup_energy(double step /*sec*/);    //[MWh]
	virtual double get_hot_startup_energy(double step /*sec*/);    //[MWh]
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