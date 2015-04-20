#include "csp_solver_core.h"
#include "csp_solver_util.h"

C_csp_solver::C_csp_solver(C_csp_weatherreader *p_weather,
	C_csp_collector_receiver *p_collector_receiver,
	C_csp_power_cycle *p_power_cycle)
{
	mpc_weather = p_weather;
	mpc_collector_receiver = p_collector_receiver;
	mpc_power_cycle = p_power_cycle;

	init_independent();

	// Get controller values from component models
		// Collector/Receiver
	mpc_collector_receiver->get_design_parameters(&m_T_htf_cold_des);

		// Power Cycle
	C_csp_power_cycle::S_solved_params solved_params;
	mpc_power_cycle->get_design_parameters(solved_params);
	m_cycle_W_dot_des = solved_params.m_W_dot_des;
	m_cycle_eta_des = solved_params.m_eta_des;
	m_cycle_q_dot_des = solved_params.m_q_dot_des;
	m_cycle_max_frac = solved_params.m_cycle_max_frac;
	m_cycle_cutoff_frac = solved_params.m_cycle_cutoff_frac;
	m_cycle_sb_frac = solved_params.m_cycle_sb_frac;


		// Run annual simulation
}

void C_csp_solver::init_independent()
{
	mpc_weather->init();
	mpc_collector_receiver->init();
	mpc_power_cycle->init();

	return;
}

void C_csp_solver::simulate()
{
	size_t hour = 0;					//[hr] hardcode simulation to start at first of year, for now
	size_t hour_end = 8760;				//[hr] hardcode simulation to run through entire year, for now
	mc_sim_info.m_step = 3600.0;		//[hr] hardcode steps = 1 hr, for now

	C_csp_solver_htf_state cr_htf_state;
	C_csp_collector_receiver::S_csp_cr_inputs cr_inputs;
	C_csp_collector_receiver::S_csp_cr_outputs cr_outputs;

	while( hour < 8760 )
	{
		mc_sim_info.m_time = mc_sim_info.m_step*(hour + 1);
	
		// Get weather at this timestep. Should only be called once per timestep. (Except converged() function)
		mpc_weather->timestep_call(mc_sim_info);
		
		// Solve collector/receiver with design inputs and weather to estimate output
			// May replace this call with a simple proxy model later...
		cr_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
		cr_inputs.m_field_control = 1.0;						//[-] no defocusing for initial simulation
		mpc_collector_receiver->call((*mpc_weather).ms_outputs,
			&cr_htf_state,
			&cr_inputs,
			cr_outputs,
			&mc_sim_info);


		double q_dot_cr_output = cr_outputs.m_q_thermal;		//[MW]





		// Timestep solved: run post-processing, converged()
	
	}

}