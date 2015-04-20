#include "csp_solver_mspt_collector_receiver.h"

C_csp_mspt_collector_receiver::C_csp_mspt_collector_receiver(C_pt_heliostatfield & pt_heliostatfield,
	C_mspt_receiver_222 & mspt_receiver_222)
{
	mc_pt_heliostatfield = pt_heliostatfield;
	mc_mspt_receiver_222 = mspt_receiver_222;
}

C_csp_mspt_collector_receiver::~C_csp_mspt_collector_receiver()
{}

void C_csp_mspt_collector_receiver::init()
{
	mc_pt_heliostatfield.init();
	mc_mspt_receiver_222.init();
	return;
}

void C_csp_mspt_collector_receiver::get_design_parameters(double *p_T_htf_cold_des)
{
	*p_T_htf_cold_des = mc_mspt_receiver_222.m_T_htf_cold_des;
}

void C_csp_mspt_collector_receiver::call(const C_csp_weatherreader::S_outputs *p_weather,
	C_csp_solver_htf_state *p_htf_state,
	const C_csp_collector_receiver::S_csp_cr_inputs *p_inputs,
	C_csp_collector_receiver::S_csp_cr_outputs &cr_outputs,
	const C_csp_solver_sim_info *p_sim_info)
{
	// What about catching errors here?
	
	// First call heliostat field class: 'csp_solver_pt_heliostat'
	// Then use its outputs as inputs to receiver class: 'csp_solver_mspt_receiver_222'

	// Set heliostat field call() parameters and solve
	double heliostat_field_control = (*p_inputs).m_field_control;
	mc_pt_heliostatfield.call(p_weather, heliostat_field_control, p_sim_info);

	// Get heliostat field outputs and set corresponding receiver inputs
	C_mspt_receiver_222::S_inputs receiver_inputs;
	receiver_inputs.m_field_eff = mc_pt_heliostatfield.ms_outputs.m_eta_field;
	receiver_inputs.m_flux_map_input = &mc_pt_heliostatfield.ms_outputs.m_flux_map_out;
	mc_mspt_receiver_222.call(p_weather, p_htf_state, &receiver_inputs, p_sim_info);
		
	// Set collector/receiver parent class outputs and return
	cr_outputs.m_q_thermal = mc_mspt_receiver_222.ms_outputs.m_Q_thermal;		//[MW]
}

void C_csp_mspt_collector_receiver::converged()
{
	mc_pt_heliostatfield.converged();
	mc_mspt_receiver_222.converged();
}