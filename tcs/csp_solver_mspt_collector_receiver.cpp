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

int C_csp_mspt_collector_receiver::get_operating_state()
{
	return mc_mspt_receiver_222.get_operating_state();
}

void C_csp_mspt_collector_receiver::get_design_parameters(C_csp_collector_receiver::S_csp_cr_solved_params & solved_params)
{
	solved_params.m_T_htf_cold_des = mc_mspt_receiver_222.m_T_htf_cold_des;
}

void C_csp_mspt_collector_receiver::call(const C_csp_weatherreader::S_outputs &weather,
	C_csp_solver_htf_state &htf_state,
	const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
	C_csp_collector_receiver::S_csp_cr_outputs &cr_outputs,
	const C_csp_solver_sim_info &sim_info)
{
	// What about catching errors here?
	
	// First call heliostat field class: 'csp_solver_pt_heliostat'
	// Then use its outputs as inputs to receiver class: 'csp_solver_mspt_receiver_222'

	// Set heliostat field call() parameters and solve
	double heliostat_field_control = inputs.m_field_control;
	mc_pt_heliostatfield.call(weather, heliostat_field_control, sim_info);

	// Get heliostat field outputs and set corresponding receiver inputs
	C_mspt_receiver_222::S_inputs receiver_inputs;
	receiver_inputs.m_field_eff = mc_pt_heliostatfield.ms_outputs.m_eta_field;
	receiver_inputs.m_input_operation_mode = inputs.m_input_operation_mode;
	receiver_inputs.m_flux_map_input = &mc_pt_heliostatfield.ms_outputs.m_flux_map_out;
	mc_mspt_receiver_222.call(weather, htf_state, receiver_inputs, sim_info);
		
	// Set collector/receiver parent class outputs and return
	cr_outputs.m_eta_field = mc_pt_heliostatfield.ms_outputs.m_eta_field;				//[-]

	cr_outputs.m_eta_thermal = mc_mspt_receiver_222.ms_outputs.m_eta_therm;				//[-]
	cr_outputs.m_q_thermal = mc_mspt_receiver_222.ms_outputs.m_Q_thermal;				//[MW]
	cr_outputs.m_m_dot_salt_tot = mc_mspt_receiver_222.ms_outputs.m_m_dot_salt_tot;		//[kg/hr]
	cr_outputs.m_T_salt_hot = mc_mspt_receiver_222.ms_outputs.m_T_salt_hot;				//[C]

	cr_outputs.m_time_required_su = mc_mspt_receiver_222.ms_outputs.m_time_required_su;	//[s]
}

void C_csp_mspt_collector_receiver::converged()
{
	mc_pt_heliostatfield.converged();
	mc_mspt_receiver_222.converged();
}