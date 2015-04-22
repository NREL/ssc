#include "csp_solver_core.h"
#include "csp_solver_util.h"

C_csp_solver::C_csp_solver(C_csp_weatherreader &weather,
	C_csp_collector_receiver &collector_receiver,
	C_csp_power_cycle &power_cycle) : 
	mc_weather(weather), 
	mc_collector_receiver(collector_receiver), 
	mc_power_cycle(power_cycle)
{
	// Inititalize non-reference member data
	m_T_htf_cold_des = m_cycle_W_dot_des = m_cycle_eta_des = m_cycle_q_dot_des = m_cycle_max_frac = m_cycle_cutoff_frac =
		m_cycle_sb_frac_des = m_cycle_T_htf_hot_des = std::numeric_limits<double>::quiet_NaN();

}

void C_csp_solver::init_independent()
{
	mc_weather.init();
	mc_collector_receiver.init();
	mc_power_cycle.init();

	return;
}

void C_csp_solver::init()
{
	init_independent();

	// Get controller values from component models
		// Collector/Receiver
	C_csp_collector_receiver::S_csp_cr_solved_params cr_solved_params;
	mc_collector_receiver.get_design_parameters(cr_solved_params);
	m_T_htf_cold_des = cr_solved_params.m_T_htf_cold_des;	//[K]

		// Power Cycle
	C_csp_power_cycle::S_solved_params solved_params;
	mc_power_cycle.get_design_parameters(solved_params);		
	m_cycle_W_dot_des = solved_params.m_W_dot_des;					//[MW]
	m_cycle_eta_des = solved_params.m_eta_des;						//[-]
	m_cycle_q_dot_des = solved_params.m_q_dot_des;					//[MW]
	m_cycle_max_frac = solved_params.m_cycle_max_frac;				//[-]
	m_cycle_cutoff_frac = solved_params.m_cycle_cutoff_frac;		//[-]
	m_cycle_sb_frac_des = solved_params.m_cycle_sb_frac;			//[-]
	m_cycle_T_htf_hot_des = solved_params.m_T_htf_hot_ref+273.15;	//[K] convert from C

}


void C_csp_solver::simulate()
{
	size_t hour = 0;					//[hr] hardcode simulation to start at first of year, for now
	size_t hour_end = 8760;				//[hr] hardcode simulation to run through entire year, for now
	mc_sim_info.m_step = 3600.0;		//[hr] hardcode steps = 1 hr, for now

	C_csp_solver_htf_state cr_htf_state;
	C_csp_collector_receiver::S_csp_cr_inputs cr_inputs;
	C_csp_collector_receiver::S_csp_cr_outputs cr_outputs;

	C_csp_solver_htf_state pc_htf_state;
	C_csp_power_cycle::S_control_inputs pc_inputs;

	bool is_rec_su_allowed = true;
	bool is_pc_su_allowed = true;
	bool is_pc_sb_allowed = true;

	int cr_operating_state = C_csp_collector_receiver::E_csp_cr_modes::OFF;
	int pc_operating_state = C_csp_power_cycle::E_csp_power_cycle_modes::OFF;

	bool is_est_rec_output_useful = false;

	double tol_mode_switching = 0.05;		// Give buffer to account for uncertainty in estimates

	while( hour < 8760 )
	{
		mc_sim_info.m_time = mc_sim_info.m_step*(hour + 1);
		
		// Get collector/receiver & power cycle operating states
		cr_operating_state = mc_collector_receiver.get_operating_state();
		pc_operating_state = mc_power_cycle.get_operating_state();

		// Get weather at this timestep. Should only be called once per timestep. (Except converged() function)
		mc_weather.timestep_call(mc_sim_info);

		// Get or set decision variables
		bool is_rec_su_allowed = true;
		bool is_pc_su_allowed = true;
		bool is_pc_sb_allowed = true;
		int tou_timestep = 1;			//[base 1] used by power cycle model for hybrid cooling - may also want to move this to controller

		// Get standby fraction and min operating fraction
			// Could eventually be a method in PC class...
		double cycle_sb_frac = m_cycle_sb_frac_des;						//[-]
		double q_pc_sb_frac = cycle_sb_frac * m_cycle_q_dot_des;		//[MW]
		double q_pc_min_frac = m_cycle_cutoff_frac * m_cycle_q_dot_des;	//[MW]

		// Solve collector/receiver with design inputs and weather to estimate output
			// May replace this call with a simple proxy model later...
		cr_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
		cr_inputs.m_field_control = 1.0;						//[-] no defocusing for initial simulation
		cr_inputs.m_input_operation_mode = C_csp_collector_receiver::E_csp_cr_modes::ON;
		mc_collector_receiver.call(mc_weather.ms_outputs,
			cr_htf_state,
			cr_inputs,
			cr_outputs,
			mc_sim_info);

		double q_dot_cr_output = cr_outputs.m_q_thermal;		//[MW]

		// Can receiver output be used?
			// No TES
		is_est_rec_output_useful = q_dot_cr_output*(1.0+tol_mode_switching) > q_pc_sb_frac;

		
		int operating_mode = -1;
		bool are_models_converged = false;
		
		// Determine which operating mode to try first
		if( is_est_rec_output_useful )		// Can receiver produce power and can it be used somewhere (power cycle, in this case)
		{
			if(cr_operating_state==C_csp_collector_receiver::OFF && pc_operating_state==C_csp_power_cycle::OFF)
			{	// At start of this timestep, are power cycle AND collector/receiver off?
				
				if(is_rec_su_allowed && is_pc_su_allowed)
				{	// Are receiver and power cycle startup allowed?
					
					if( q_dot_cr_output*(1.0 + tol_mode_switching) > q_pc_min_frac )
					{	// Do we estimate that there is enough thermal power to operate cycle at min fraction?

						operating_mode = CR_SU__PC_OFF__TES_OFF__AUX_OFF;
					}
					else if( is_pc_sb_allowed && q_dot_cr_output*(1.0 + tol_mode_switching) > q_pc_sb_frac )
					{	// If not, is there enough to operate at standby, AND is standby allowed

						operating_mode = CR_SU__PC_OFF__TES_OFF__AUX_OFF;
					}
					else
					{	// If we can't use the receiver output, then don't start up

						operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					}				
				}
				else
				{	// If startup isn't allowed, then don't try

					operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
				}
			}
			else if(cr_operating_state==C_csp_collector_receiver::ON && pc_operating_state==C_csp_power_cycle::OFF)
			{	// At start of this timestep, is collector/receiver on, but the power cycle is off?
				
			
			}

		}
		else
		{
			operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
		}


		while(!are_models_converged)		// Solve for correct operating mode and performance in following loop:
		{
			switch(operating_mode)
			{
			case CR_SU__PC_OFF__TES_OFF__AUX_OFF:
				// Run the collector/receiver under startup mode
					// **************
				// This will probably result in a local timestep shorter than the baseline simulation timestep (governed by weather file)

				cr_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
				cr_inputs.m_field_control = 1.0;						//[-] no defocusing for initial simulation
				cr_inputs.m_input_operation_mode = C_csp_collector_receiver::STARTUP;

				mc_collector_receiver.call(mc_weather.ms_outputs,
					cr_htf_state,
					cr_inputs,
					cr_outputs,
					mc_sim_info);

				// GET NEW TIME!!!!!

				break;

			case tech_operating_modes::CR_OFF__PC_OFF__TES_OFF__AUX_OFF:
				// Solve all models as 'off' or 'idle'
					// Collector/receiver
				cr_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
				cr_inputs.m_field_control = 1.0;						//[-] no defocusing for initial simulation
				cr_inputs.m_input_operation_mode = C_csp_collector_receiver::E_csp_cr_modes::OFF;
				mc_collector_receiver.call(mc_weather.ms_outputs,
					cr_htf_state,
					cr_inputs,
					cr_outputs,
					mc_sim_info);
					
					// Power Cycle
						// HTF State
				pc_htf_state.m_temp_in = m_cycle_T_htf_hot_des-273.15;	//[C]
				pc_htf_state.m_m_dot = 0.0;		//[kg/hr] no mass flow rate to power cycle
						// Inputs
				pc_inputs.m_standby_control = C_csp_power_cycle::E_csp_power_cycle_modes::OFF;
				pc_inputs.m_tou = tou_timestep;
						// Performance Call
				mc_power_cycle.call(mc_weather.ms_outputs,
					pc_htf_state,
					pc_inputs,
					mc_sim_info);

				are_models_converged = true;
			
				break;		// exit switch() after CR_OFF__PC_OFF__TES_OFF__AUX_OFF:

			default: 
				double catch_here_for_now = 1.23;

			}
		
		}	// End loop to find correct operating mode and system performance


		// Timestep solved: run post-processing, converged()		
		mc_collector_receiver.converged();
		mc_power_cycle.converged();

			// Don't converge weather file if working with partial timesteps
		mc_weather.converged();

		hour++;
	}	// End timestep loop

}	// End simulate() method