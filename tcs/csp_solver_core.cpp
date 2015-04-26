#include "csp_solver_core.h"
#include "csp_solver_util.h"

#include "lib_util.h"

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

	m_op_mode_tracking.resize(0);

	error_msg = "";

	// Output vectors
	mv_time_mid.resize(0);
	mv_solzen.resize(0);
	mv_beam.resize(0);
	mv_defocus.resize(0);
	mv_eta_field.resize(0);

	// Solved Controller Variables
	m_defocus = std::numeric_limits<double>::quiet_NaN();
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
	m_cycle_max_frac = solved_params.m_max_frac;				//[-]
	m_cycle_cutoff_frac = solved_params.m_cutoff_frac;		//[-]
	m_cycle_sb_frac_des = solved_params.m_sb_frac;			//[-]
	m_cycle_T_htf_hot_des = solved_params.m_T_htf_hot_ref+273.15;	//[K] convert from C
}


void C_csp_solver::simulate()
{
	
	double sim_time_start = 0.0;			//[s] hardcode simulation to start at first of year, for now
	double sim_time_end = 8760.0*3600;		//[s] hardcode simulation to run through entire year, for now
	double sim_step_size_baseline = 3600.0;			//[s]
	mc_sim_info.m_step = sim_step_size_baseline;		//[s] hardcode steps = 1 hr, for now

	bool is_rec_su_allowed = true;
	bool is_pc_su_allowed = true;
	bool is_pc_sb_allowed = true;

	int cr_operating_state = C_csp_collector_receiver::E_csp_cr_modes::OFF;
	int pc_operating_state = C_csp_power_cycle::E_csp_power_cycle_modes::OFF;

	bool is_est_rec_output_useful = false;

	double tol_mode_switching = 0.05;		// Give buffer to account for uncertainty in estimates

	double step_local = mc_sim_info.m_step;		//[hr] Step size might adjust during receiver and/or power cycle startup
	bool is_sim_timestep_complete = true;		//[-] Are we running serial simulations at partial timesteps inside of one typical timestep?

	double time_previous = sim_time_start;		//[s]

	double time_sim_step_next = sim_time_start + sim_step_size_baseline;	//[s]

	mc_sim_info.m_step = step_local;						//[s]
	mc_sim_info.m_time = time_previous + step_local;		//[s]

	// Reset vector that tracks operating modes
	m_op_mode_tracking.resize(0);

	// Reset Controller Variables to Defaults
	m_defocus = 1.0;		//[-]  

	while( mc_sim_info.m_time <= sim_time_end )
	{
		// Get collector/receiver & power cycle operating states
		cr_operating_state = mc_collector_receiver.get_operating_state();
		pc_operating_state = mc_power_cycle.get_operating_state();

		// Get weather at this timestep. Should only be called once per timestep. (Except converged() function)
		mc_weather.timestep_call(mc_sim_info);

		// Get or set decision variables
		bool is_rec_su_allowed = true;
		bool is_pc_su_allowed = true;
		bool is_pc_sb_allowed = true;
		mc_sim_info.m_tou = 1;			//[base 1] used by power cycle model for hybrid cooling - may also want to move this to controller

		// Get standby fraction and min operating fraction
			// Could eventually be a method in PC class...
		double cycle_sb_frac = m_cycle_sb_frac_des;				//[MW]
			
			// *** If standby not allowed, then reset q_pc_sb = q_pc_min ?? *** or is this too confusing and not helpful enough?
		double q_pc_sb = cycle_sb_frac * m_cycle_q_dot_des;		//[MW]
		double q_pc_min = m_cycle_cutoff_frac * m_cycle_q_dot_des;	//[MW]
		double q_pc_max = m_cycle_max_frac * m_cycle_q_dot_des;		//[MW]

		// Solve collector/receiver with design inputs and weather to estimate output
			// May replace this call with a simple proxy model later...
		mc_cr_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
		mc_cr_inputs.m_field_control = 1.0;						//[-] no defocusing for initial simulation
		mc_cr_inputs.m_input_operation_mode = C_csp_collector_receiver::E_csp_cr_modes::STEADY_STATE;
		mc_collector_receiver.call(mc_weather.ms_outputs,
			mc_cr_htf_state,
			mc_cr_inputs,
			mc_cr_outputs,
			mc_sim_info);

		double q_dot_cr_output = mc_cr_outputs.m_q_thermal;		//[MW]

		// Can receiver output be used?
			// No TES
		is_est_rec_output_useful = q_dot_cr_output*(1.0+tol_mode_switching) > q_pc_sb;

		
		int operating_mode = -1;
		bool are_models_converged = false;
		
		// Determine which operating mode to try first
		if( is_est_rec_output_useful )		// Can receiver produce power and can it be used somewhere (power cycle, in this case)
		{
			if( (cr_operating_state==C_csp_collector_receiver::OFF || cr_operating_state==C_csp_collector_receiver::STARTUP) 
				&& pc_operating_state==C_csp_power_cycle::OFF)
			{	// At start of this timestep, are power cycle AND collector/receiver off?
				
				if(is_rec_su_allowed && is_pc_su_allowed)
				{	// Are receiver and power cycle startup allowed?
					
					if( q_dot_cr_output*(1.0 + tol_mode_switching) > q_pc_min )
					{	// Do we estimate that there is enough thermal power to operate cycle at min fraction?

						operating_mode = CR_SU__PC_OFF__TES_OFF__AUX_OFF;
					}
					else if( is_pc_sb_allowed && q_dot_cr_output*(1.0 + tol_mode_switching) > q_pc_sb )
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
			else if(cr_operating_state==C_csp_collector_receiver::ON && 
					(pc_operating_state==C_csp_power_cycle::OFF || pc_operating_state==C_csp_power_cycle::STARTUP) )
			{	// At start of this timestep, is collector/receiver on, but the power cycle is off?
				
				if( is_pc_su_allowed )
				{	// Is power cycle startup allowed?
					
					if( q_dot_cr_output*(1.0 + tol_mode_switching) > q_pc_min )
					{
						operating_mode = CR_ON__PC_SU__TES_OFF__AUX_OFF;
					}
					else if( is_pc_sb_allowed && q_dot_cr_output*(1.0 + tol_mode_switching) > q_pc_sb )
					{
						operating_mode = CR_ON__PC_SU__TES_OFF__AUX_OFF;
					}
					else
					{
						operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					}					
				}
				else
				{	// If startup isn't allowed, then don't try

					operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
				}				
			}
			else if(cr_operating_state==C_csp_collector_receiver::ON &&
					pc_operating_state==C_csp_power_cycle::ON)
			{	// Are the collector/receiver AND power cylce ON?

				if( q_dot_cr_output*(1.0-tol_mode_switching) > q_pc_max )
				{	// Is it likely that the receiver will defocus?
					// If this mode is entered as the initial mode, then controller can't go back to CR_ON__PC_RM__TES_OFF__AUX_OFF
					
					operating_mode = CR_DF__PC_FULL__TES_OFF__AUX_OFF;
				}
				else if( q_dot_cr_output*(1.0 + tol_mode_switching) > q_pc_min )
				{	// Receiver can likely operate at full available power

					operating_mode = CR_ON__PC_RM__TES_OFF__AUX_OFF;
				}
				else if(is_pc_sb_allowed && q_dot_cr_output*(1.0 + tol_mode_switching) > q_pc_sb ) // this is the entry logic to the outer nest 
				{	// Receiver can likely operate in standby
				
					operating_mode = CR_ON__PC_SB__TES_OFF__AUX_OFF;
				}
				else
				{	// Can't use receiver output, so shutdown CR and PC
					// Shouldn't end up here because of outer-nest entry logic, but include as a safety check 

					operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
				}
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
			case CR_DF__PC_FULL__TES_OFF__AUX_OFF:
			{
				// Running CR at full power results in too much thermal power to power cycle
				// Therefore, must defocus CR and operating PC at FULL POWER

				// Assuming here that partial defocus is allowed, so should always be able to reach full power to PC
				// If CR and PC for some reason don't solve or produce power, will shut down CR and PC

				// Store operating mode
				m_op_mode_tracking.push_back(operating_mode);
				
				// Should have CR thermal output results from either steady state call at beginning of timestep or previouso mode
				// Use this to estimate required defocus as a starting point for iteration
					// But.. check anyway
				double defocus_guess_ini = std::numeric_limits<double>::quiet_NaN();
				if(mc_cr_outputs.m_q_thermal > 0.0)
				{
					// Controller hierarchy doesn't allow to go back to No Defocus and PC_RM, so check that defocus is <= 1
					defocus_guess_ini = fmin(1.0, q_pc_max / mc_cr_outputs.m_q_thermal);
				}
				else
				{
					mc_cr_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
					mc_cr_inputs.m_field_control = 1.0;						//[-] no defocusing for initial simulation
					mc_cr_inputs.m_input_operation_mode = C_csp_collector_receiver::E_csp_cr_modes::ON;
					mc_collector_receiver.call(mc_weather.ms_outputs,
						mc_cr_htf_state,
						mc_cr_inputs,
						mc_cr_outputs,
						mc_sim_info);

					if(mc_cr_outputs.m_q_thermal > 0.0)
					{
						// Controller hierarchy doesn't allow to go back to No Defocus and PC_RM, so check that defocus is <= 1
						defocus_guess_ini = fmin(1.0, q_pc_max / mc_cr_outputs.m_q_thermal);
					}
					else
					{	// CR not producing power at design inlet temperature

						// Weird that controller chose Defocus operating mode, so report message and shut down CR and PC
						error_msg = util::format("At time = %lg the controller chose Defocus operating mode, but the collector/receiver"
							"did not produce power with the design inlet temperature. Controller will shut-down CR and PC",
							mc_sim_info.m_time / 3600.0);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
						are_models_converged = false;

						break;
					}
				}
				
				// Solve for inner loop: cr-pc convergence tolerances

				double tol_cr_pc_C = 2.0;
				double tol_cr_pc = tol_cr_pc_C / m_T_htf_cold_des;

				double relaxed_tol_cr_pc_mult = 5.0;
				double relaxed_tol_cr_pc = relaxed_tol_cr_pc_mult*tol_cr_pc;

				double defocus_guess = defocus_guess_ini;

				// Consider upper and lower bounds on defocus
				// Know that upper bound on defocus = 1, so can set that
				// Some combination of CR & PC methods (existing or otherwise) could *possibly* be used to guess lower, but let's assume we don't know it
					// Upper bound, error, and booleans
				double defocus_upper = 1.0;
				double y_defocus_uppper = std::numeric_limits<double>::quiet_NaN();
				bool is_upper_bound = true;
				bool is_upper_error = false;
					// Lower bound, error, and booleans
				double defocus_lower = std::numeric_limits<double>::quiet_NaN();
				double y_defocus_lower = std::numeric_limits<double>::quiet_NaN();
				bool is_lower_bound = false;
				bool is_lower_error = false;

				
				// Iterating on defocus until q_rec_thermal = q_pc_max AND CR_to_PC iteration solves successfully
				// Tolerance, in this case, probably doesn't need to be larger than inner nest tolerance
				double tol = 0.001;
				double relax_tol_mult = 5.0;
				double relax_tol = relax_tol_mult*tol;
				double bounds_tol = tol / 2.0;			// (upper - lower)/upper if no solution, when upper and lower get this close, make decision and get out

				// Defocus: 1 = full power, 0 = no power
				double diff_q_dot = 999.9*tol;			// (Rec - q_pc_max)/q_pc_max: (+) q_dot too large, decrease defocus, (-) q_dot too small, increase defocus fraction

				// CR-PC solver outputs are needed to determine whether defocus iteration solved within convergence
				double cr_pc_exit_tol = std::numeric_limits<double>::quiet_NaN();
				int cr_pc_exit_mode = -1;		

				// Will be reset in while() loop under other outcomes
				int defocus_exit_mode = CONVERGED;		// Need this because have to use 'break' to exit the while() iteration loop

				int iter_defocus = 0;

				// Start iteration loop
				while( abs(diff_q_dot) > tol )
				{
					iter_defocus++;			// First iteration = 1
				
					// Check if distance between bounds is "too small" (using 'bounds_tol' defined above)
					double diff_defocus_bounds = defocus_upper - defocus_lower;
					if( diff_defocus_bounds / defocus_upper < bounds_tol )
					{
						if( diff_q_dot != diff_q_dot )
						{	// CR-PC aren't converging, so need to shut them down

							diff_q_dot = std::numeric_limits<double>::quiet_NaN();
							defocus_exit_mode = NO_SOLUTION;
							break;		// Get out of while()					
						}
						else
						{	// Poor convergence between power delivered to PC and power requested

							defocus_exit_mode = POOR_CONVERGENCE;
							break;		// Get out of while()
						}
					}

					// Subsequent iterations need to re-calculate defocus
					if(iter_defocus > 1)
					{
						if(diff_q_dot != diff_q_dot)		// Check if solution was found
						{	// CR-PC model did not converge, so we don't know anything about this defocus
							// However, we know that we should now have an upper or lower bound (else code would have exited from logic below)
							// But, check that bounds exist, just to be careful
							if(!is_lower_bound || !is_upper_bound)
							{

								diff_q_dot = std::numeric_limits<double>::quiet_NaN();
								defocus_exit_mode = NO_SOLUTION;
								break;		// Get out of while()	
							}
							defocus_guess = 0.5*(defocus_lower + defocus_upper);
						}
						else if( diff_q_dot > 0.0 )		// q_dot was too high, decrease defocus
						{
							is_upper_bound = true;
							is_upper_error = true;
							defocus_upper = defocus_guess;		// Set upper bound
							y_defocus_uppper = diff_q_dot;		// Set upper convergence error

							if(is_lower_bound && is_lower_error)	// False-position method
							{
								defocus_guess = y_defocus_uppper/(y_defocus_uppper-y_defocus_lower)*(defocus_lower-defocus_upper) + defocus_upper;
							}
							else if(is_lower_bound)
							{
								defocus_guess = 0.5*(defocus_upper + defocus_lower);
							}
							else
							{
								defocus_guess = fmax(0.01, defocus_guess - 0.05);			// Could perhaps use last solution to make a smarter guess...
							}

						}
						else							// q_dot was too low, increase defocus 
						{
							is_lower_bound = true;
							is_lower_error = true;
							defocus_lower = defocus_guess;	// Set lower bound
							y_defocus_lower = diff_q_dot;	// Set lower convergence error

							if(is_upper_bound && is_upper_error)
							{
								defocus_guess = y_defocus_uppper/(y_defocus_uppper-y_defocus_lower)*(defocus_lower-defocus_upper) + defocus_upper;
							}
							else if(is_upper_bound)
							{	// should always have upper bound, but keep this framework for consistency...
								defocus_guess = 0.5*(defocus_upper + defocus_lower);
							}
							else
							{
								defocus_guess = fmin(1.0, defocus_guess + 0.05);
							}
						}
					}

					// Use defocus_guess and call method to solve CR-PC iteration
					cr_pc_exit_tol = std::numeric_limits<double>::quiet_NaN();
					solver_cr_to_pc_to_cr(defocus_guess, tol_cr_pc, cr_pc_exit_mode, cr_pc_exit_tol);

					// Process results from CR-PC iteration:
					if(cr_pc_exit_mode == NO_SOLUTION)
					{	// CR and PC did not produce power or did not solve
						
						if(iter_defocus == 1)
						{	// If this happened on first iteration, assume guess is lower bound and try again?
						
							defocus_lower = defocus_guess;
							is_lower_bound = true;
							is_lower_error = false;			
							diff_q_dot = std::numeric_limits<double>::quiet_NaN();
						}
						else
						{	// CR_PC has solved successfully at least once
							// And assume upper bound always solves
							// So assume that NO_SOLUTION corresponds to lower bound
							// So if a lower bound is already known, then nowhere to go
							if(is_lower_bound)
							{
								diff_q_dot = std::numeric_limits<double>::quiet_NaN();
								defocus_exit_mode = NO_SOLUTION;
								break;		// Get out of while()	
							}
							else
							{
								defocus_lower = defocus_guess;
								is_lower_bound = true;
								is_lower_error = false;
								diff_q_dot = std::numeric_limits<double>::quiet_NaN();
							}
						}	// end logic on iteration count for NO SOLUTION					
					}	// end code for NO SOLUTION

					// CR-PC iteration found a solution (though perhaps at POOR CONVERGENCE)
					// Calculate the difference between thermal power delivered to PC and thermal power requested
					// (Rec - q_pc_max)/q_pc_max: (+) q_dot too large, decrease defocus, (-) q_dot too small, increase defocus fraction
					diff_q_dot = (mc_cr_outputs.m_q_thermal - q_pc_max) / q_pc_max;

				}	// end iteration on CR defocus

				// Set Member Defocus Here
				m_defocus = defocus_guess;

				// Reached convergence on defocus, but it is *possibly* that the CR-PC iteration only solved at POOR CONVERGENCE
				// Check here...?
				if(cr_pc_exit_mode == POOR_CONVERGENCE)
				{
					if( abs(cr_pc_exit_tol) > relaxed_tol_cr_pc )
					{	// Did not converge within Relaxed Tolerance, shut off CR and PC

						// update 'exit_mode'
						cr_pc_exit_mode = NO_SOLUTION;
					}
					else
					{	// Convergence within Relaxed Tolerance, *Report message* but assume timestep solved in this mode

						error_msg = util::format("At time = %lg the collector/receiver and power cycle solution only reached a convergence"
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_sim_info.m_time / 3600.0, cr_pc_exit_mode);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						// update 'exit_mode' for following logic branches
						cr_pc_exit_mode = CONVERGED;
					}
				}
				if(defocus_exit_mode == POOR_CONVERGENCE)
				{
					if( abs(diff_q_dot) > relax_tol )
					{	// Defocus did not converge within Relaxed Tolerance, shut off CR & PC
					
						// update defocus Exit Mode
						defocus_exit_mode = NO_SOLUTION;
					}
					else
					{	// Convergence within Relaxed Tolerance, *Report message* but assume timestep solved in this mode

						error_msg = util::format("At time = %lg the defocus iteration only reached a convergence"
							" = &lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_sim_info.m_time / 3600.0, cr_pc_exit_mode);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						// update Exit Mode
						defocus_exit_mode = CONVERGED;
					}
				}

				if(defocus_exit_mode == NO_SOLUTION || cr_pc_exit_mode == NO_SOLUTION)
				{
					error_msg = util::format("At time = %lg the controller chose Defocus operating mode, but the solver failed to reach convergence "
						"Controller will shut-down CR and PC",
						mc_sim_info.m_time / 3600.0);
					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					// Shut down CR and PC
					operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					are_models_converged = false;

					break;
				}
				else if(defocus_exit_mode == CONVERGED && cr_pc_exit_mode == CONVERGED)
				{
					// If defocus solution has converged, then q_pc = q_pc_max, and shouldn't need to double-check anything...

					are_models_converged = true;
				}
				else
				{
					throw(C_csp_exception("Solver tried mode 'CR_DF__PC_FULL__TES_OFF__AUX_OFF' and did not receive useful exit instructions", "CSP Solver"));
				}




				break;		// Get out of switch()
			}

			case CR_ON__PC_RM__TES_OFF__AUX_OFF:
			{
				// Collector/Receiver in ON, and only place for HTF to go is power cycle.
				// Therefore, power cycle must operate at Resource Match and use w/e is provided
					// (in cases with storage or field defocus, power cycle will try to hit an exact thermal input)
				// 'Failure Modes'
				// 1) Receiver provides too much power
				//		* Go to defocus
				// 2) Receiver cannot maintain minimum operation fraction
				//		* Go to power cycle standby or shutdown

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				// Store operating mode
				m_op_mode_tracking.push_back(operating_mode);

				double tol_C = 2.0;
				double tol = tol_C / m_T_htf_cold_des;

				double relaxed_tol_multiplier = 5.0;
				double relaxed_tol = relaxed_tol_multiplier*tol;
				
				// Call CR-PC_CR Solver
				int exit_mode = -1;
				double exit_tolerance = std::numeric_limits<double>::quiet_NaN();
				double field_control = 1.0;
				solver_cr_to_pc_to_cr(field_control, tol, exit_mode, exit_tolerance);

				// If CR and PC models solved and produced power, but did not converge within tolerance,
				// check whether achieved convergence is "good enough" to report and continue
				if(exit_mode == POOR_CONVERGENCE)
				{
					if(abs(exit_tolerance) > relaxed_tol)
					{	// Did not converge within Relaxed Tolerance, shut off CR and PC

						// update 'exit_mode'
						exit_mode = NO_SOLUTION;
					}
					else
					{	// Convergence within Relaxed Tolerance, *Report message* but assume timestep solved in this mode

						error_msg = util::format("At time = %lg the collector/receiver and power cycle solution only reached a convergence"
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_sim_info.m_time / 3600.0, exit_tolerance);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						// update 'exit_mode' for following logic branches
						exit_mode = CONVERGED;
					}
				}

				if( exit_mode == NO_SOLUTION )
				{	// Either CR & PC did not solve/produce power, or did not solve within Relaxed Tolerance: shut off CR and PC

					operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					are_models_converged = false;

					break;		// exits switch(operating mode)
				}

				else if( exit_mode == CONVERGED )
				{
					// Now, check whether we need to defocus the receiver
					if( mc_cr_outputs.m_q_thermal > q_pc_max )
					{	// Too much power to PC, try defocusing
						operating_mode = CR_DF__PC_FULL__TES_OFF__AUX_OFF;

						are_models_converged = false;
					}
					else if( mc_cr_outputs.m_q_thermal < q_pc_min )
					{	// Not enough thermal power to run power cycle at Min Cutoff fraction: check if we can try standby

						if( is_pc_sb_allowed )
						{	// If controller *was* trying to generate power, then assume that there is enough power to at least try standby

							operating_mode = CR_ON__PC_SB__TES_OFF__AUX_OFF;
						}
						else
						{	// PC standby not allowed - shut down CR and PC

							operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
						}

						are_models_converged = false;
					}
					else
					{	// Solved successfully within bounds of this operation mode: move on

						are_models_converged = true;
					}

					break;
				}
				else
				{
					throw(C_csp_exception("Solver tried mode 'CR_ON__PC_RM__TES_OFF__AUX_OFF' and did not receive exit instructions", "CSP Solver"));
				}


			}	// end case{} to allow compilation with local (w/r/t case) variables

				break;
			
			
			case CR_ON__PC_SB__TES_OFF__AUX_OFF:
				// Collector/receiver is ON
				// Power cycle is running in standby
				// During standby, assume power cycle HTF return temperature is constant and = m_T_htf_cold_des
					// so shouldn't need to iterate between CR and PC
				// Assume power cycle can remain in standby the entirety of the timestep

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;
				
				// Store operating mode
				m_op_mode_tracking.push_back(operating_mode);

				// First, solve the CR. Again, we're assuming HTF inlet temperature is always = m_T_htf_cold_des
				mc_cr_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
				mc_cr_inputs.m_field_control = 1.0;						//[-] no defocusing for initial simulation
				mc_cr_inputs.m_input_operation_mode = C_csp_collector_receiver::ON;

				mc_collector_receiver.call(mc_weather.ms_outputs,
					mc_cr_htf_state,
					mc_cr_inputs,
					mc_cr_outputs,
					mc_sim_info);

				if( mc_cr_outputs.m_q_thermal == 0.0 )
				{	// Collector/receiver can't produce useful energy
					operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					are_models_converged = false;
				}

				// If receiver is indeed producing power, then try power cycle at standby
					// Power cycle: STANDBY


				break;


			case CR_ON__PC_SU__TES_OFF__AUX_OFF:
				// Collector/receiver is ON
				// Startup power cycle
				// During startup, assume power cycle HTF return temperature is constant and = m_T_htf_cold_des
					// so shouldn't need to iterate between collector/receiver and power cycle
				// This will probably result in a local timestep shorter than the baseline simulation timestep (governed by weather file)

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				// Store operating mode
				m_op_mode_tracking.push_back(operating_mode);

				// CR: ON
				mc_cr_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
				mc_cr_inputs.m_field_control = 1.0;						//[-] no defocusing for initial simulation
				mc_cr_inputs.m_input_operation_mode = C_csp_collector_receiver::ON;

				mc_collector_receiver.call(mc_weather.ms_outputs,
					mc_cr_htf_state,
					mc_cr_inputs,
					mc_cr_outputs,
					mc_sim_info);

				if( mc_cr_outputs.m_q_thermal == 0.0 )
				{	// Collector/receiver can't produce useful energy
					operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					are_models_converged = false;
				}

				// If receiver IS producing energy, try starting up power cycle
					// Power Cycle: STARTUP
				mc_pc_htf_state.m_temp_in = mc_cr_outputs.m_T_salt_hot;		//[C]
				mc_pc_htf_state.m_m_dot = mc_cr_outputs.m_m_dot_salt_tot;		//[kg/hr] no mass flow rate to power cycle
				// Inputs
				mc_pc_inputs.m_standby_control = C_csp_power_cycle::E_csp_power_cycle_modes::STARTUP;
					//mc_pc_inputs.m_tou = tou_timestep;
				// Performance Call
				mc_power_cycle.call(mc_weather.ms_outputs,
					mc_pc_htf_state,
					mc_pc_inputs,
					mc_pc_outputs,
					mc_sim_info);

				// Would be nice to have some check to know whether startup solved appropriately...


				// Check for new timestep
				step_local = mc_pc_outputs.m_time_required_su;		//[s] power cycle model returns MIN(time required to completely startup, full timestep duration)
				if( step_local < mc_sim_info.m_step )
				{
					is_sim_timestep_complete = false;
				}

				// Reset sim_info values
				if( !is_sim_timestep_complete )
				{
					mc_sim_info.m_step = step_local;						//[s]
					mc_sim_info.m_time = time_previous + step_local;		//[s]
				}


				are_models_converged = true;

				break;
			
			case CR_SU__PC_OFF__TES_OFF__AUX_OFF:
				// Run the collector/receiver under startup mode
					// **************
				// This will probably result in a local timestep shorter than the baseline simulation timestep (governed by weather file)

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				// Store operating mode
				m_op_mode_tracking.push_back(operating_mode);

				mc_cr_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
				mc_cr_inputs.m_field_control = 1.0;						//[-] no defocusing for initial simulation
				mc_cr_inputs.m_input_operation_mode = C_csp_collector_receiver::STARTUP;

				mc_collector_receiver.call(mc_weather.ms_outputs,
					mc_cr_htf_state,
					mc_cr_inputs,
					mc_cr_outputs,
					mc_sim_info);

				// Check for new timestep
				step_local = mc_cr_outputs.m_time_required_su;		//[s] Receiver model returns MIN(time required to completely startup, full timestep duration)
				if(step_local < mc_sim_info.m_step)
				{
					is_sim_timestep_complete = false;
				}

				// Reset sim_info values
				if( !is_sim_timestep_complete )
				{
					mc_sim_info.m_step = step_local;						//[s]
					mc_sim_info.m_time = time_previous + step_local;		//[s]
				}

				// Power Cycle: OFF
				mc_pc_htf_state.m_temp_in = m_cycle_T_htf_hot_des - 273.15;	//[C]
				mc_pc_htf_state.m_m_dot = 0.0;		//[kg/hr] no mass flow rate to power cycle
				// Inputs
				mc_pc_inputs.m_standby_control = C_csp_power_cycle::E_csp_power_cycle_modes::OFF;
					//mc_pc_inputs.m_tou = tou_timestep;
				// Performance Call
				mc_power_cycle.call(mc_weather.ms_outputs,
					mc_pc_htf_state,
					mc_pc_inputs,
					mc_pc_outputs,
					mc_sim_info);

				are_models_converged = true;

				break;

			case tech_operating_modes::CR_OFF__PC_OFF__TES_OFF__AUX_OFF:
				// Solve all models as 'off' or 'idle'
					// Collector/receiver

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				// Store operating mode
				m_op_mode_tracking.push_back(operating_mode);

				mc_cr_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
				mc_cr_inputs.m_field_control = 1.0;						//[-] no defocusing for initial simulation
				mc_cr_inputs.m_input_operation_mode = C_csp_collector_receiver::E_csp_cr_modes::OFF;
				mc_collector_receiver.call(mc_weather.ms_outputs,
					mc_cr_htf_state,
					mc_cr_inputs,
					mc_cr_outputs,
					mc_sim_info);
					
					// Power Cycle: OFF
						// HTF State
				mc_pc_htf_state.m_temp_in = m_cycle_T_htf_hot_des - 273.15;	//[C]
				mc_pc_htf_state.m_m_dot = 0.0;		//[kg/hr] no mass flow rate to power cycle
						// Inputs
				mc_pc_inputs.m_standby_control = C_csp_power_cycle::E_csp_power_cycle_modes::OFF;
					//mc_pc_inputs.m_tou = tou_timestep;
						// Performance Call
				mc_power_cycle.call(mc_weather.ms_outputs,
					mc_pc_htf_state,
					mc_pc_inputs,
					mc_pc_outputs,
					mc_sim_info);

				are_models_converged = true;
			
				break;		// exit switch() after CR_OFF__PC_OFF__TES_OFF__AUX_OFF:

			default: 
				double catch_here_for_now = 1.23;

			}	// End switch() on receiver operating modes
		
		}	// End loop to find correct operating mode and system performance


		// Timestep solved: run post-processing, converged()		
		mc_collector_receiver.converged();
		mc_power_cycle.converged();

				
		// Don't converge weather file if working with partial timesteps
		if( !is_sim_timestep_complete )
		{
			// Calculate new timestep
			step_local = time_sim_step_next - mc_sim_info.m_time;
		}
		else
		{
			// If partial timestep, use constant weather data for all partial timesteps
			mc_weather.converged();

			step_local = sim_step_size_baseline;

			time_sim_step_next += sim_step_size_baseline;
		}

		// Save timestep outputs
		// This is after timestep convergence, so be sure convergence() methods don't unexpectedly change outputs
		mv_time_mid.push_back((time_previous+mc_sim_info.m_time)/2.0/3600.0);		//[hr] Time at end of timestep
		mv_solzen.push_back(mc_weather.ms_outputs.m_solzen);		//[deg] Solar zenith
		mv_beam.push_back(mc_weather.ms_outputs.m_beam);			//[W/m2] DNI
		mv_eta_field.push_back(mc_cr_outputs.m_eta_field);			//[-] Field efficiency (= eta_field_full * defocus)
		mv_defocus.push_back(m_defocus);							//[-] Defocus
		

		// Track time and step forward
		is_sim_timestep_complete = true;
		time_previous = mc_sim_info.m_time;						//[s]
		mc_sim_info.m_step = step_local;						//[s]
		mc_sim_info.m_time = time_previous + step_local;		//[s]
					
		// Reset operating mode tracker, so get "save" or write or pass results somewhere
		m_op_mode_tracking.resize(0);

	}	// End timestep loop

}	// End simulate() method

void C_csp_solver::solver_cr_to_pc_to_cr(double field_control_in, double tol, int &exit_mode, double &exit_tolerance)
{
	// Method to solve scenario where the CR is on (under some fixed operating conditions, i.e. defocus)
	// and the PC is on. No TES or AUX, so the output of the CR connects directly to the PC

	// Ouputs:
	// int exit_mode: E_solver_outcomes 
	
	// Solution procedure
	// 1) Guess the receiver inlet temperature
	// Use design temperature for now, but this is an area where "smart" guesses could be applied
	double T_rec_in_guess_ini = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
	double T_rec_in_guess = T_rec_in_guess_ini;
	// Set lower and upper bounds, or find through iteration?
	// Lower bound could be freeze protection temperature...
	double T_rec_in_lower = std::numeric_limits<double>::quiet_NaN();
	double T_rec_in_upper = std::numeric_limits<double>::quiet_NaN();
	double y_rec_in_lower = std::numeric_limits<double>::quiet_NaN();
	double y_rec_in_upper = std::numeric_limits<double>::quiet_NaN();
	// Booleans for bounds and convergence error
	bool is_upper_bound = false;
	bool is_lower_bound = false;
	bool is_upper_error = false;
	bool is_lower_error = false;

	double diff_T_in = 999.9*tol;		// (Calc - Guess)/Guess: (+) Guess was too low, (-) Guess was too high

	int iter_T_in = 0;

	// Start iteration loop
	while( abs(diff_T_in) > tol )
	{
		iter_T_in++;			// First iteration = 1

		// Check if distance between bounds is "too small"
		double diff_T_bounds = T_rec_in_upper - T_rec_in_lower;
		if( diff_T_bounds / T_rec_in_upper < tol / 2.0 )
		{
			if( diff_T_in != diff_T_in )
			{	// Models aren't producing power or are returning errors, and it appears we've tried the solution space for T_rec_in
				
				exit_mode = NO_SOLUTION;
				exit_tolerance = diff_T_in;
				return;
			}
			else
			{	// Models are producing power, but convergence errors are not within Tolerance

				exit_mode = POOR_CONVERGENCE;
				exit_tolerance = diff_T_in;
				return;
			}
		}


		// Subsequent iterations need to re-calcualte T_in
		if( iter_T_in > 1 )
		{
			if( diff_T_in != diff_T_in )
			{	// Models did not solve such that a convergence error could be generated
				// However, we know that upper and lower bounds are set, so we can calculate a new guess via bisection method
				// but check that bounds exist, to be careful
				if( !is_lower_bound || !is_upper_bound )
				{
					exit_mode = NO_SOLUTION;
					exit_tolerance = diff_T_in;
					return;
				}
				T_rec_in_guess = 0.5*(T_rec_in_lower + T_rec_in_upper);		//[C]
			}
			else if( diff_T_in > 0.0 )		// Guess receiver inlet temperature was too low
			{
				is_lower_bound = true;
				is_lower_error = true;
				T_rec_in_lower = T_rec_in_guess;		// Set lower bound
				y_rec_in_lower = diff_T_in;				// Set lower convergence error

				if( is_upper_bound && is_upper_error )		// False-position method
				{
					T_rec_in_guess = y_rec_in_upper / (y_rec_in_upper - y_rec_in_lower)*(T_rec_in_lower - T_rec_in_upper) + T_rec_in_upper;	//[C]
				}
				else if( is_upper_bound )						// Bisection method
				{
					T_rec_in_guess = 0.5*(T_rec_in_lower + T_rec_in_upper);		//[C]
				}
				else				// Constant adjustment
				{
					T_rec_in_guess += 15.0;			//[C]
				}
			}
			else							// Guess receiver inlet temperature was too high
			{
				is_upper_bound = true;
				is_upper_error = true;
				T_rec_in_upper = T_rec_in_guess;		// Set upper bound
				y_rec_in_upper = diff_T_in;				// Set upper convergence error

				if( is_lower_bound && is_lower_error )		// False-position method
				{
					T_rec_in_guess = y_rec_in_upper / (y_rec_in_upper - y_rec_in_lower)*(T_rec_in_lower - T_rec_in_upper) + T_rec_in_upper;	//[C]
				}
				else if( is_lower_bound )
				{
					T_rec_in_guess = 0.5*(T_rec_in_lower + T_rec_in_upper);		//[C]
				}
				else
				{
					T_rec_in_guess -= 15.0;			//[C] 
				}
			}
		}

		// 2) Solve the receiver model

		// CR: ON
		mc_cr_htf_state.m_temp_in = T_rec_in_guess;			//[C], convert from [K]
		mc_cr_inputs.m_field_control = field_control_in;	//[-] no defocusing for initial simulation
		mc_cr_inputs.m_input_operation_mode = C_csp_collector_receiver::ON;

		mc_collector_receiver.call(mc_weather.ms_outputs,
			mc_cr_htf_state,
			mc_cr_inputs,
			mc_cr_outputs,
			mc_sim_info);

		// Check if receiver is OFF or model didn't solve
		if( mc_cr_outputs.m_m_dot_salt_tot == 0.0 || mc_cr_outputs.m_q_thermal == 0.0 )
		{
			// If first iteration, don't know enough about why collector/receiver is not producing power to advance iteration
			if( iter_T_in == 1 )
			{	
				exit_mode = NO_SOLUTION;
				exit_tolerance = diff_T_in;
				return;
			}
			else
			{	// Set this T_rec_in_guess as either upper or lower bound, depending on which end of DESIGN temp it falls
				// Assumption here is that receiver solved at first guess temperature
				// and that the failure wouldn't occur between established bounds
				if( T_rec_in_guess < T_rec_in_guess_ini )
				{
					if( is_lower_bound && !is_upper_bound )
					{
						exit_mode = NO_SOLUTION;
						exit_tolerance = diff_T_in;
						return;
					}
					T_rec_in_lower = T_rec_in_guess;
					is_lower_bound = true;
					is_lower_error = false;
					// At this point, both and upper and lower bound should exist, so can generate new guess
					// And communicate this to Guess-Generator by setting diff_T_in to NaN
					diff_T_in = std::numeric_limits<double>::quiet_NaN();
				}
				else
				{
					if( is_upper_bound && !is_lower_bound )
					{
						exit_mode = NO_SOLUTION;
						exit_tolerance = diff_T_in;
						return;
					}
					T_rec_in_upper = T_rec_in_guess;
					is_upper_bound = true;
					is_upper_error = false;
					// At this point, both and upper and lower bound should exist, so can generate new guess
					// And communicate this to Guess-Generator by setting diff_T_in to NaN
					diff_T_in = std::numeric_limits<double>::quiet_NaN();
				}
			}
		}	// End Collector/Receiver OFF decisions

		// 3) Solve the power cycle model using receiver outputs
		// Power Cycle: ON
		mc_pc_htf_state.m_temp_in = mc_cr_outputs.m_T_salt_hot;		//[C]
		mc_pc_htf_state.m_m_dot = mc_cr_outputs.m_m_dot_salt_tot;	//[kg/hr] no mass flow rate to power cycle
		// Inputs
		mc_pc_inputs.m_standby_control = C_csp_power_cycle::E_csp_power_cycle_modes::ON;
			//mc_pc_inputs.m_tou = tou_timestep;
		// Performance Call
		mc_power_cycle.call(mc_weather.ms_outputs,
			mc_pc_htf_state,
			mc_pc_inputs,
			mc_pc_outputs,
			mc_sim_info);

		// Check that power cycle is producing power or model didn't solve
		if( mc_pc_outputs.m_P_cycle == 0.0 )
		{
			// If first iteration, don't know enough about why power cycle is not producing power to advance iteration
			// Go to Receiver OFF power cycle OFF
			if( iter_T_in == 1 )
			{
				exit_mode = NO_SOLUTION;
				exit_tolerance = diff_T_in;
				return;
			}
			else
			{	// Set this T_rec_in_guess as either upper or lower bound, depending on which end of DESIGN temp it falls
				// Assumption here is that receiver solved at first guess temperature
				// and that the failure wouldn't occur between established bounds
				if( T_rec_in_guess < T_rec_in_guess_ini )
				{
					if( is_lower_bound && !is_upper_bound )
					{
						exit_mode = NO_SOLUTION;
						exit_tolerance = diff_T_in;
						return;
					}
					T_rec_in_lower = T_rec_in_guess;
					is_lower_bound = true;
					is_lower_error = false;
					// At this point, both and upper and lower bound should exist, so can generate new guess
					// And communicate this to Guess-Generator by setting diff_T_in to NaN
					diff_T_in = std::numeric_limits<double>::quiet_NaN();
				}
				else
				{
					if( is_upper_bound && !is_lower_bound )
					{
						exit_mode = NO_SOLUTION;
						exit_tolerance = diff_T_in;
						return;
					}
					T_rec_in_upper = T_rec_in_guess;
					is_upper_bound = true;
					is_upper_error = false;
					// At this point, both and upper and lower bound should exist, so can generate new guess
					// And communicate this to Guess-Generator by setting diff_T_in to NaN
					diff_T_in = std::numeric_limits<double>::quiet_NaN();
				}
			}
		}	// end Power Cycle OFF decisions

		diff_T_in = (mc_pc_outputs.m_T_htf_cold - T_rec_in_guess) / T_rec_in_guess;

	}	// end iteration on T_rec_in

	exit_mode = CONVERGED;
	exit_tolerance = diff_T_in;

	return;
}