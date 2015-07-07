#include "csp_solver_core.h"
#include "csp_solver_util.h"

#include "lib_util.h"

C_csp_solver::C_csp_solver(C_csp_weatherreader &weather,
	C_csp_collector_receiver &collector_receiver,
	C_csp_power_cycle &power_cycle,
	C_csp_tes &tes) : 
	mc_weather(weather), 
	mc_collector_receiver(collector_receiver), 
	mc_power_cycle(power_cycle),
	mc_tes(tes)
{
	// Hierarchy logic
	reset_hierarchy_logic();

	// Inititalize non-reference member data
	m_T_htf_cold_des = m_q_dot_rec_on_min = 
		
		m_cycle_W_dot_des = m_cycle_eta_des = m_cycle_q_dot_des = m_cycle_max_frac = m_cycle_cutoff_frac =
		m_cycle_sb_frac_des = m_cycle_T_htf_hot_des = m_m_dot_pc_des = std::numeric_limits<double>::quiet_NaN();

	m_op_mode_tracking.resize(0);

	error_msg = "";

	// Output vectors
	mv_time_mid.resize(0);
	mv_solzen.resize(0);
	mv_beam.resize(0);
	mv_eta_field.resize(0);
	mv_defocus.resize(0);
	mv_rec_eta_thermal.resize(0);
	mv_rec_q_thermal.resize(0);
	mv_rec_q_startup.resize(0);
	mv_pc_eta.resize(0);
	mv_pc_W_gross.resize(0);
	mv_pc_q_startup.resize(0);
	mv_tes_q_losses.resize(0);
	mv_tes_q_heater.resize(0);
	mv_tes_T_hot.resize(0);
	mv_tes_T_cold.resize(0);	
	mv_operating_modes.resize(0);

	// Solved Controller Variables
	m_defocus = std::numeric_limits<double>::quiet_NaN();
}

void C_csp_solver::reset_hierarchy_logic()
{
	m_is_CR_SU__PC_OFF__TES_OFF__AUX_OFF_avail = true;
	m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail = true;
	m_is_CR_ON__PC_SU__TES_OFF__AUX_OFF_avail = true;
	m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail = true;
	m_is_CR_OFF__PC_SU__TES_DC__AUX_OFF_avail = true;
	m_is_CR_DF__PC_FULL__TES_OFF__AUX_OFF_avail = true;

	m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_HI_SIDE = true;
	m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_LO_SIDE = true;

	m_is_CR_ON__PC_RM_LO__TES_OFF__AUX_OFF_avail = true;

	m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_HI_SIDE = true;
	m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_LO_SIDE = true;
}

void C_csp_solver::init_independent()
{
	mc_weather.init();
	mc_collector_receiver.init();
	mc_power_cycle.init();
	mc_tes.init();

	return;
}

void C_csp_solver::init()
{
	init_independent();

	// Get controller values from component models
		// Collector/Receiver
	C_csp_collector_receiver::S_csp_cr_solved_params cr_solved_params;
	mc_collector_receiver.get_design_parameters(cr_solved_params);
	m_T_htf_cold_des = cr_solved_params.m_T_htf_cold_des;		//[K]
	m_q_dot_rec_on_min = cr_solved_params.m_q_dot_rec_on_min;	//[MW]

		// Power Cycle
	C_csp_power_cycle::S_solved_params solved_params;
	mc_power_cycle.get_design_parameters(solved_params);		
	m_cycle_W_dot_des = solved_params.m_W_dot_des;					//[MW]
	m_cycle_eta_des = solved_params.m_eta_des;						//[-]
	m_cycle_q_dot_des = solved_params.m_q_dot_des;					//[MW]
	m_cycle_max_frac = solved_params.m_max_frac;					//[-]
	m_cycle_cutoff_frac = solved_params.m_cutoff_frac;				//[-]
	m_cycle_sb_frac_des = solved_params.m_sb_frac;					//[-]
	m_cycle_T_htf_hot_des = solved_params.m_T_htf_hot_ref+273.15;	//[K] convert from C
	m_m_dot_pc_des = solved_params.m_m_dot_design;					//[kg/hr]
		
		// Thermal Storage
	m_is_tes = mc_tes.does_tes_exist();
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

	//bool is_est_rec_output_useful = false;

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

		// Get TES operating state info
		double q_dot_tes_dc, q_dot_tes_ch;
		q_dot_tes_dc = q_dot_tes_ch = std::numeric_limits<double>::quiet_NaN();
		if( m_is_tes )
		{
			double m_dot_field_dc_est, T_hot_field_dc_est;	//[MW, kg/s, K]
			m_dot_field_dc_est = T_hot_field_dc_est = std::numeric_limits<double>::quiet_NaN();
			mc_tes.discharge_avail_est(m_T_htf_cold_des, mc_sim_info.m_step, q_dot_tes_dc, m_dot_field_dc_est, T_hot_field_dc_est);

			double m_dot_field_ch_est, T_cold_field_ch_est;	//[MW, kg/s, K]
			m_dot_field_ch_est = T_cold_field_ch_est = std::numeric_limits<double>::quiet_NaN();
			mc_tes.charge_avail_est(m_cycle_T_htf_hot_des, mc_sim_info.m_step, q_dot_tes_ch, m_dot_field_ch_est, T_cold_field_ch_est);
		}
		else
		{
			q_dot_tes_dc = q_dot_tes_ch = 0.0;
		}


		
		
		// Can add the following code to simulate with no storage charge/discharge, but IDLE calcs
		//q_dot_tes_dc = q_dot_tes_ch = 0.0;





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
		double q_pc_target = q_pc_max;							//[MW]


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


		// Need a way to consider min power (as calculated in receiver model)
		// Steady State call is suggesting receiver can produce power, and when ON is tried, it is nowhere close to meeting min power requirements
		// Think the idea with initially not checking output was for startup considerations...
		double q_dot_cr_startup = mc_cr_outputs.m_q_thermal;		//[MW]



		double q_dot_cr_on = std::numeric_limits<double>::quiet_NaN();
		if(q_dot_cr_startup < m_q_dot_rec_on_min*(1.0-0.05))
			q_dot_cr_on = 0.0;
		else
			q_dot_cr_on = q_dot_cr_startup;



		int operating_mode = ENTRY_MODE;
		bool are_models_converged = false;
		reset_hierarchy_logic();
		// Reset operating mode tracker		
		m_op_mode_tracking.resize(0);
					

		while(!are_models_converged)		// Solve for correct operating mode and performance in following loop:
		{

			if( (cr_operating_state == C_csp_collector_receiver::OFF || cr_operating_state == C_csp_collector_receiver::STARTUP)
				&& (pc_operating_state == C_csp_power_cycle::OFF || pc_operating_state == C_csp_power_cycle::STARTUP) )
			{	// At start of this timestep, are power cycle AND collector/receiver off?

				if( q_dot_cr_startup > 0.0 && is_rec_su_allowed &&
					m_is_CR_SU__PC_OFF__TES_OFF__AUX_OFF_avail )
				{	// Receiver startup is allowed and possible (will generate net energy)

					operating_mode = CR_SU__PC_OFF__TES_OFF__AUX_OFF;
				}
				else if( q_dot_tes_dc > 0.0 && is_pc_su_allowed &&
					m_is_CR_OFF__PC_SU__TES_DC__AUX_OFF_avail )		// Can power cycle startup using TES?
				{
					operating_mode = CR_OFF__PC_SU__TES_DC__AUX_OFF;
				}
				else
				{
					operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
				}
			}	// End logic for CR_state == OFF or STARTUP    AND     PC_state == OFF or STARTUP

			else if( cr_operating_state == C_csp_collector_receiver::ON &&
				(pc_operating_state == C_csp_power_cycle::OFF || pc_operating_state == C_csp_power_cycle::STARTUP) )
			{
				if( q_dot_cr_on > 0.0 && is_rec_su_allowed )
				{	// Receiver is allowed to remain on, and it can produce useful energy. Now, need to find a home for it

					if( is_pc_su_allowed &&
						m_is_CR_ON__PC_SU__TES_OFF__AUX_OFF_avail )	// Can receiver output go to power cycle?
					{
						operating_mode = CR_ON__PC_SU__TES_OFF__AUX_OFF;
					}
					else if( q_dot_tes_ch > 0.0 )
					{
						if( q_dot_cr_on*(1.0 - tol_mode_switching) < q_dot_tes_ch &&
							m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail )
						{
							operating_mode = CR_ON__PC_OFF__TES_CH__AUX_OFF;
						}
						else
						{
							throw(C_csp_exception("operating_mode = CR_DF__PC_OFF__TES_FULL__AUX_OFF", "CSP Solver"));
						}
					}
					else
					{
						operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					}
				}
				else if( q_dot_tes_dc && is_pc_su_allowed &&
					m_is_CR_OFF__PC_SU__TES_DC__AUX_OFF_avail )
				{	// Can power cycle startup using TES?

					operating_mode = CR_OFF__PC_SU__TES_DC__AUX_OFF;
				}
				else
				{
					operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
				}
			}

			else if( (cr_operating_state == C_csp_collector_receiver::OFF || cr_operating_state == C_csp_collector_receiver::STARTUP) &&
				pc_operating_state == C_csp_power_cycle::ON )
			{
				if( q_dot_cr_startup > 0.0 && is_rec_su_allowed )
				{	// Receiver startup is allowed and possible (will generate net energy) - determine if power cycle can remain on

					if( is_pc_su_allowed && q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_target )
					{	// Tolerance is applied so that if TES is *close* to matching target, the controller tries that mode

						throw(C_csp_exception("operating_mode = CR_SU__PC_TARGET__TES_DC__AUX_OFF; not yet available", "CSP Solver"));
					}
					else if( is_pc_su_allowed && q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_min )
					{	// Tolerance is applied so that if TES is *close* to reaching min fraction, the controller tries that mode

						throw(C_csp_exception("operating_mode = CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF; not yet available", "CSP Solver"));
					}
					else if( is_pc_sb_allowed && q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_sb )
					{	// Tolerance is applied so that if TES is *close* to reaching min fraction, the controller tries that mode

						throw(C_csp_exception("operating_mode = CR_SU__PC_SB__TES_DC__AUX_OFF; not yet available", "CSP Solver"));
					}
					else if( is_pc_su_allowed && q_dot_tes_dc > 0.0 )
					{
						throw(C_csp_exception("operating_mode = CR_SU__PC_RM_LO__TES_EMPTY__AUX_OFF; not yet available", "CSP Solver"));
						// In this mode, need to be able to operate PC at >= MIN level until CR is running
					}
					else if( m_is_CR_SU__PC_OFF__TES_OFF__AUX_OFF_avail )
					{
						operating_mode = CR_SU__PC_OFF__TES_OFF__AUX_OFF;
					}
					else
					{
						operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					}
				}
				else	// Receiver remains OFF - determine if power cycle can remain on
				{
					if( is_pc_su_allowed && q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_target )
					{	// Tolerance is applied so that if TES is *close* to matching target, the controller tries that mode

						throw(C_csp_exception("operating_mode = CR_OFF__PC_TARGET__TES_DC__AUX_OFF; not yet available", "CSP Solver"));
					}
					else if( is_pc_su_allowed && q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_min )
					{	// Tolerance is applied so that if TES is *close* to reaching min fraction, the controller tries that mode

						throw(C_csp_exception("operating_mode = CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF; not yet available", "CSP Solver"));
					}
					else if( is_pc_sb_allowed && q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_sb )
					{	// Tolerance is applied so that if TES is *close* to reaching min fraction, the controller tries that mode

						throw(C_csp_exception("operating_mode = CR_OFF__PC_SB__TES_DC__AUX_OFF; not yet available", "CSP Solver"));
					}
					else if( is_pc_sb_allowed && q_dot_tes_dc > 0.0 )
					{
						throw(C_csp_exception("operating_mode = CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF; not yet available", "CSP Solver"));
					}
					else
					{
						operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					}
				}
			}

			else if( cr_operating_state == C_csp_collector_receiver::ON &&
				(pc_operating_state == C_csp_power_cycle::ON || pc_operating_state == C_csp_power_cycle::STANDBY) )
			{
				if( q_dot_cr_on > 0.0 && is_rec_su_allowed )
				{	// Receiver operation is allowed and possible - find a home for output

					if( is_pc_su_allowed )
					{
						if( q_dot_cr_on*(1.0 + tol_mode_switching) > q_pc_target &&
							m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_LO_SIDE && m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_LO_SIDE )
						{	// The power cycle cannot accept the entire receiver output
							// Tolerance is applied so that if CR is *close* to reaching the PC target, the controller tries modes that fill TES

							// Is storage available to discharge to power cycle?
							if( q_dot_tes_ch > 0.0 )
							{
								// 1) Try to fill storage while hitting power cycle target
								if( (q_dot_cr_on - q_dot_tes_ch)*(1.0 - tol_mode_switching) < q_pc_target &&
									m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_HI_SIDE )
								{	// Storage can accept the remaining receiver output
									// Tolerance is applied so that if CR + TES is *close* to reaching PC target, the controller tries that mode

									operating_mode = CR_ON__PC_TARGET__TES_CH__AUX_OFF;
								}

								// 2) Try operating power cycle at maximum capacity
								// Assume we want to completely fill storage, so the power cycle operation should float to meet that condition
								else if( (q_dot_cr_on - q_dot_tes_ch)*(1.0 - tol_mode_switching) < q_pc_max )
								{	// Storage and the power cycle operating between target and max can accept the remaining receiver output
									// Tolerance is applied so that if CR + TES is *close* to reaching PC  max, the controller tries that mode

									throw(C_csp_exception("operating_mode = CR_ON__PC_RM_HI__TES_FULL__AUX_OFF", "CSP_Solver"));
								}

								// 3) Try defocusing the CR and operating the power cycle at maximum capacity
								else
								{
									throw(C_csp_exception("operating_mode = CR_DF__PC_FULL__TES_FULL__AUX_OFF", "CSP_Solver"));
								}
							}	// End if(q_dot_tes_ch > 0.0) logic

							else
							{	// No storage available for dispatch

								// 1) Try operating power cycle at maximum capacity
								if( q_dot_cr_on*(1.0 - tol_mode_switching) < q_pc_max &&
									m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_HI_SIDE )
								{	// Tolerance is applied so that if CR + TES is *close* to reaching PC  max, the controller tries that mode

									operating_mode = CR_ON__PC_RM_HI__TES_OFF__AUX_OFF;
								}
								else if( m_is_CR_DF__PC_FULL__TES_OFF__AUX_OFF_avail )
								{
									operating_mode = CR_DF__PC_FULL__TES_OFF__AUX_OFF;
								}
								else
								{
									operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
								}
							}	// End else 'no storage available for dispatch'
						}
						else
						{	// Power cycle is asking for more output than the receiver can supply

							if( q_dot_tes_dc > 0.0 )
							{	// Storage dispatch is available

								if( (q_dot_cr_on + q_dot_tes_dc)*(1.0 + tol_mode_switching) > q_pc_target )
								{	// Storage can provide enough dispatch to reach power cycle target
									// Tolerance is applied so that if CR + TES is *close* to reaching PC target, the controller tries that mode

									throw(C_csp_exception("operating_mode = CR_ON__PC_TARGET__TES_DC__AUX_OFF", "CSP Solver"));
								}
								else if( (q_dot_cr_on + q_dot_tes_dc)*(1.0 + tol_mode_switching) > q_pc_min )
								{	// Storage can provide enough dispatch to at least meet power cycle minimum operation fraction
									// Run at highest possible PC fraction by dispatch all remaining storage
									// Tolerance is applied so that if CR + TES is *close* to reaching PC min, the controller tries that mode

									throw(C_csp_exception("operating_mode = CR_ON__PC_RM_LO__TES_EMPTY__AUX_OFF", "CSP Solver"));
								}
								else if( is_pc_sb_allowed )
								{	// If standby is allowed

									if( q_dot_cr_on*(1.0 + tol_mode_switching) > q_pc_sb &&
										m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail )
									{	// Tolerance is applied so that if CR output is *close* to reaching standby, the controller tries that mode

										if( q_dot_tes_ch > 0.0 )
										{
											throw(C_csp_exception("operating_mode = CR_ON__PC_SB__TES_CH__AUX_OFF", "CSP Solver"));
										}
										else
										{
											// This could *technically* use defocus, but can argue the energy is just being thrown away in power cycle anyway
											operating_mode = CR_ON__PC_SB__TES_OFF__AUX_OFF;
										}
									}
									else if( (q_dot_cr_on + q_dot_tes_dc)*(1.0 + tol_mode_switching) > q_pc_sb )
									{	// Tolerance is applied so that if CR + TES is *close* to reaching standby, the controller tries that mode

										throw(C_csp_exception("operating_mode = CR_ON__PC_SB__TES_DC__AUX_OFF", "CSP Solver"));
									}
									else
									{	// If not enough thermal power to stay in standby, then run at min PC load until TES is fully discharged

										throw(C_csp_exception("operating_mode = CR_ON__PC_MIN__TES_EMPTY__AUX_OFF", "CSP Solver"));
									}
								}
								else
								{	// If not enough thermal power to stay in standby, then run at min PC load until TES is fully discharged

									throw(C_csp_exception("operating_mode = CR_ON__PC_MIN__TES_EMPTY__AUX_OFF", "CSP Solver"));
								}
							}
							else
							{	// Storage dispatch is not available

								// Can the power cycle operate at or above the minimum operation fraction?
								if( q_dot_cr_on*(1.0 + tol_mode_switching) > q_pc_min &&
									m_is_CR_ON__PC_RM_LO__TES_OFF__AUX_OFF_avail )
								{	// Tolerance is applied so that if CR is *close* to reaching PC min, the controller tries that mode

									operating_mode = CR_ON__PC_RM_LO__TES_OFF__AUX_OFF;
								}
								else if( is_pc_sb_allowed && q_dot_cr_on*(1.0 + tol_mode_switching) > q_pc_sb &&
									m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail )
								{	// Receiver can likely operate in standby
									// Tolerance is applied so that if CR is *close* to reaching PC standby, the controller tries that mode

									operating_mode = CR_ON__PC_SB__TES_OFF__AUX_OFF;
								}
								else if( q_dot_tes_ch > 0.0 )
								{	// Charge storage with receiver output

									if( q_dot_cr_on*(1.0 - tol_mode_switching) < q_dot_tes_ch &&
										m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail )
									{	// Tolerance is applied so that if CR is *close* to being less than a full TES charge, the controller tries normal operation (no defocus)


										operating_mode = CR_ON__PC_OFF__TES_CH__AUX_OFF;
									}
									else
									{	// The CR output will overcharge storage, so it needs to defocus.
										// However, because the CR output is already part-load, it may be close to shutting down before defocus...

										throw(C_csp_exception("operating_mode = CR_DF__PC_OFF__TES_FULL__AUX_OFF", "CSP Solver"));
									}

								}
								else
								{	// No home for receiver output, and not enough thermal power for power cycle

									operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
								}
							}	// End logic else 'storage dispatch not available'
						}	// End logic else 'power cycle requires more q_dot than receiver can supply'				
					}	// End logic if(is_rec_su_allowed)
					else
					{	// Power cycle startup is not allowed - see if receiver output can go to storage

						if( q_dot_tes_ch > 0.0 )
						{
							if( q_dot_cr_on*(1.0 - tol_mode_switching) < q_dot_tes_ch &&
								m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail )
							{
								operating_mode = CR_ON__PC_OFF__TES_CH__AUX_OFF;
							}
							else
							{
								throw(C_csp_exception("operating_mode = CR_DF__PC_OFF__TES_FULL__AUX_OFF", "CSP Solver"));
							}
						}
						else
						{
							operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
						}

					}	// End logic else 'pc su is NOT allowed'		
				}	// End logic if(q_dot_cr_output > 0.0 && is_rec_su_allowed)

				else	// Receiver is off - determine if power cycle can remain on
				{
					if( is_pc_su_allowed )
					{
						if( q_dot_tes_dc > 0.0 )
						{	// Storage dispatch is available

							if( q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_target )
							{	// Storage can provide enough dispatch to reach power cycle target
								// Tolerance is applied so that if TES is *close* to reaching PC target, the controller tries that mode

								throw(C_csp_exception("operating_mode = CR_OFF__PC_TARGET__TES_DC__AUX_OFF", "CSP Solver"));
							}
							else if( q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_min )
							{	// Storage can provide enough dispatch to at least meet power cycle minimum operation fraction
								// Run at highest possible PC fraction by dispatching all remaining storage
								// Tolerance is applied so that if CR + TES is *close* to reaching PC min, the controller tries that mode

								throw(C_csp_exception("operating_mode = CR_OFF__PC_RM_LO__TES_EMPTY__AUX_OFF", "CSP Solver"));
							}
							else if( q_dot_tes_dc*(1.0 + tol_mode_switching) > q_pc_sb )
							{	// Tolerance is applied so that if CR + TES is *close* to reaching standby, the controller tries that mode

								throw(C_csp_exception("operating_mode = CR_OFF__PC_SB__TES_DC__AUX_OFF", "CSP Solver"));
							}
							else
							{	// If not enough thermal power to stay in standby, then run at min PC load until TES is fully discharged

								throw(C_csp_exception("operating_mode = CR_OFF__PC_MIN__TES_EMPTY__AUX_OFF", "CSP Solver"));
							}
						}	// End logic for if( q_dot_tes_dc > 0.0 )
						else
						{	// Storage dispatch is not available

							// No thermal power available to power cycle
							operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
						}
					}	// End logic if( is_pc_su_allowed )
					else
					{	// If neither receiver nor power cycle operation is allowed, then shut everything off

						operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					}
				}	// End logic for else 'receiver not on'

			}
			// End operating state mode for CR ON, PC ON/STANDBY


			// Store operating mode
			m_op_mode_tracking.push_back(operating_mode);


			switch( operating_mode )
			{
			case CR_DF__PC_FULL__TES_OFF__AUX_OFF:
			{
				// Running CR at full power results in too much thermal power to power cycle
				// Therefore, must defocus CR and operating PC at FULL POWER

				// Assuming here that partial defocus is allowed, so should always be able to reach full power to PC
				// If CR and PC for some reason don't solve or produce power, will shut down CR and PC

				// Should have CR thermal output results from either steady state call at beginning of timestep or previouso mode
				// Use this to estimate required defocus as a starting point for iteration
				// But.. check anyway
				double defocus_guess_ini = std::numeric_limits<double>::quiet_NaN();
				if( mc_cr_outputs.m_q_thermal > 0.0 )
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

					if( mc_cr_outputs.m_q_thermal > 0.0 )
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

						m_is_CR_DF__PC_FULL__TES_OFF__AUX_OFF_avail = false;

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
				while( abs(diff_q_dot) > tol || diff_q_dot != diff_q_dot )
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
					if( iter_defocus > 1 )
					{
						if( diff_q_dot != diff_q_dot )		// Check if solution was found
						{	// CR-PC model did not converge, so we don't know anything about this defocus
							// However, we know that we should now have an upper or lower bound (else code would have exited from logic below)
							// But, check that bounds exist, just to be careful
							if( !is_lower_bound || !is_upper_bound )
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

							if( is_lower_bound && is_lower_error )	// False-position method
							{
								defocus_guess = y_defocus_uppper / (y_defocus_uppper - y_defocus_lower)*(defocus_lower - defocus_upper) + defocus_upper;
							}
							else if( is_lower_bound )
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

							if( is_upper_bound && is_upper_error )
							{
								defocus_guess = y_defocus_uppper / (y_defocus_uppper - y_defocus_lower)*(defocus_lower - defocus_upper) + defocus_upper;
							}
							else if( is_upper_bound )
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
					if( cr_pc_exit_mode == NO_SOLUTION )
					{	// CR and PC did not produce power or did not solve

						if( iter_defocus == 1 )
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
							if( is_lower_bound )
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
					else
					{
						// CR-PC iteration found a solution (though perhaps at POOR CONVERGENCE)
						// Calculate the difference between thermal power delivered to PC and thermal power requested
						// (Rec - q_pc_max)/q_pc_max: (+) q_dot too large, decrease defocus, (-) q_dot too small, increase defocus fraction

						diff_q_dot = (mc_cr_outputs.m_q_thermal - q_pc_max) / q_pc_max;
					}

				}	// end iteration on CR defocus

				// Set Member Defocus Here
				m_defocus = defocus_guess;

				// Reached convergence on defocus, but it is *possible* that the CR-PC iteration only solved at POOR CONVERGENCE
				// Check here...?
				if( cr_pc_exit_mode == POOR_CONVERGENCE )
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
				if( defocus_exit_mode == POOR_CONVERGENCE )
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

				if( defocus_exit_mode == NO_SOLUTION || cr_pc_exit_mode == NO_SOLUTION )
				{
					error_msg = util::format("At time = %lg the controller chose Defocus operating mode, but the solver failed to reach convergence "
						"Controller will shut-down CR and PC",
						mc_sim_info.m_time / 3600.0);
					mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

					// Shut down CR and PC
					//operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					are_models_converged = false;

					m_is_CR_DF__PC_FULL__TES_OFF__AUX_OFF_avail = false;

					break;
				}
				else if( defocus_exit_mode == CONVERGED && cr_pc_exit_mode == CONVERGED )
				{
					// If defocus solution has converged, then q_pc = q_pc_max, and shouldn't need to double-check anything...

					// Solve for idle storage
					if( m_is_tes )
					{
						mc_tes.idle(mc_sim_info.m_step, mc_weather.ms_outputs.m_tdry + 273.15, mc_tes_outputs);
					}

					are_models_converged = true;
				}
				else
				{
					throw(C_csp_exception("Solver tried mode 'CR_DF__PC_FULL__TES_OFF__AUX_OFF' and did not receive useful exit instructions", "CSP Solver"));
				}




				break;		// Get out of switch()
			}

			//case CR_ON__PC_RM__TES_OFF__AUX_OFF:
			case CR_ON__PC_RM_LO__TES_OFF__AUX_OFF:
			case CR_ON__PC_RM_HI__TES_OFF__AUX_OFF:
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
				if( exit_mode == POOR_CONVERGENCE )
				{
					if( abs(exit_tolerance) > relaxed_tol )
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

					if( operating_mode == CR_ON__PC_RM_LO__TES_OFF__AUX_OFF )
					{
						m_is_CR_ON__PC_RM_LO__TES_OFF__AUX_OFF_avail = false;
						are_models_converged = false;
					}
					else if( operating_mode == CR_ON__PC_RM_HI__TES_OFF__AUX_OFF )
					{
						m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_LO_SIDE = false;
						are_models_converged = false;
					}
					else
					{
						throw(C_csp_exception("Operating mode not recognized", "CSP Solver"));
					}

					operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					are_models_converged = false;

					break;		// exits switch(operating mode)
				}

				else if( exit_mode == CONVERGED )
				{
					// If the CR and PC models converged, check whether the power cycle thermal input is within bounds

					if( operating_mode == CR_ON__PC_RM_LO__TES_OFF__AUX_OFF )
					{	// In this mode, the power cycle thermal input needs to be greater than the minimum power cycle fraction

						if( mc_cr_outputs.m_q_thermal < q_pc_min )
						{
							m_is_CR_ON__PC_RM_LO__TES_OFF__AUX_OFF_avail = false;
							are_models_converged = false;
							break;						
						}

					}
					else if( operating_mode == CR_ON__PC_RM_HI__TES_OFF__AUX_OFF )
					{	// In this mode, the power cycle thermal input needs to be greater than the target cycle fraction
						// ... and less than the maximum cycle fraction

						if( mc_cr_outputs.m_q_thermal > q_pc_max )
						{
							m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_HI_SIDE = false;
							are_models_converged = false;
							break;
						}
						else if( mc_cr_outputs.m_q_thermal < q_pc_target )
						{
							m_is_CR_ON__PC_RM_HI__TES_OFF__AUX_OFF_avail_LO_SIDE = false;
							are_models_converged = false;
							break;
						}

					}
					else
					{
						throw(C_csp_exception("Operating mode not recognized", "CSP Solver"));
					}



					mc_tes.idle(mc_sim_info.m_step, mc_weather.ms_outputs.m_tdry + 273.15, mc_tes_outputs);
					are_models_converged = true;
					break;


					//// Now, check whether we need to defocus the receiver
					//if( mc_cr_outputs.m_q_thermal > q_pc_max )
					//{	// Too much power to PC, try defocusing
					//	operating_mode = CR_DF__PC_FULL__TES_OFF__AUX_OFF;

					//	are_models_converged = false;
					//}
					//else if( mc_cr_outputs.m_q_thermal < q_pc_min )
					//{	// Not enough thermal power to run power cycle at Min Cutoff fraction: check if we can try standby

					//	// Controller initially entered PC_RM mode, so assume that we should try standby if allowed
					//	if( is_pc_sb_allowed )
					//	{	// If controller *was* trying to generate power, then assume that there is enough power to at least try standby

					//		operating_mode = CR_ON__PC_SB__TES_OFF__AUX_OFF;
					//	}
					//	else
					//	{	// PC standby not allowed - shut down CR and PC

					//		operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;
					//	}

					//	are_models_converged = false;
					//}
					//else
					//{	// Solved successfully within bounds of this operation mode: move on
					//	if( m_is_tes )
					//	{
					//		mc_tes.idle(mc_sim_info.m_step, mc_weather.ms_outputs.m_tdry + 273.15, mc_tes_outputs);
					//	}

					//	are_models_converged = true;
					//}

					//break;
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

				// First, solve the CR. Again, we're assuming HTF inlet temperature is always = m_T_htf_cold_des
				mc_cr_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
				mc_cr_inputs.m_field_control = 1.0;						//[-] no defocusing for initial simulation
				mc_cr_inputs.m_input_operation_mode = C_csp_collector_receiver::ON;

				mc_collector_receiver.call(mc_weather.ms_outputs,
					mc_cr_htf_state,
					mc_cr_inputs,
					mc_cr_outputs,
					mc_sim_info);

				if( mc_cr_outputs.m_q_thermal < q_pc_sb )
				{	// Collector/receiver can't produce useful energy
					//operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;

					m_is_CR_ON__PC_SB__TES_OFF__AUX_OFF_avail = false;

					are_models_converged = false;
					break;
				}

				// If receiver is indeed producing power, then try power cycle at standby
				// Power cycle: STANDBY
				mc_pc_htf_state.m_temp_in = mc_cr_outputs.m_T_salt_hot;		//[C]
				mc_pc_htf_state.m_m_dot = mc_cr_outputs.m_m_dot_salt_tot;		//[kg/hr] no mass flow rate to power cycle
				// Inputs
				mc_pc_inputs.m_standby_control = C_csp_power_cycle::E_csp_power_cycle_modes::STANDBY;
				//mc_pc_inputs.m_tou = tou_timestep;
				// Performance Call
				mc_power_cycle.call(mc_weather.ms_outputs,
					mc_pc_htf_state,
					mc_pc_inputs,
					mc_pc_outputs,
					mc_sim_info);

				if( m_is_tes )
				{
					mc_tes.idle(mc_sim_info.m_step, mc_weather.ms_outputs.m_tdry + 273.15, mc_tes_outputs);
				}

				are_models_converged = true;

				break;


			case CR_ON__PC_SU__TES_OFF__AUX_OFF:
				// Collector/receiver is ON
				// Startup power cycle
				// During startup, assume power cycle HTF return temperature is constant and = m_T_htf_cold_des
				// so shouldn't need to iterate between collector/receiver and power cycle
				// This will probably result in a local timestep shorter than the baseline simulation timestep (governed by weather file)

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

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
					//operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;

					m_is_CR_ON__PC_SU__TES_OFF__AUX_OFF_avail = false;

					are_models_converged = false;
					break;
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

				if( m_is_tes )
				{
					mc_tes.idle(mc_sim_info.m_step, mc_weather.ms_outputs.m_tdry + 273.15, mc_tes_outputs);
				}

				are_models_converged = true;

				break;

			case CR_SU__PC_OFF__TES_OFF__AUX_OFF:
				// Run the collector/receiver under startup mode
				// **************
				// This will probably result in a local timestep shorter than the baseline simulation timestep (governed by weather file)

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				mc_cr_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
				mc_cr_inputs.m_field_control = 1.0;						//[-] no defocusing for initial simulation
				mc_cr_inputs.m_input_operation_mode = C_csp_collector_receiver::STARTUP;

				mc_collector_receiver.call(mc_weather.ms_outputs,
					mc_cr_htf_state,
					mc_cr_inputs,
					mc_cr_outputs,
					mc_sim_info);

				// Check that startup happened
				if( mc_cr_outputs.m_q_startup == 0.0 )
				{	// Collector/receiver can't produce useful energy
					//operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;

					m_is_CR_SU__PC_OFF__TES_OFF__AUX_OFF_avail = false;

					are_models_converged = false;
					break;
				}

				// Check for new timestep
				step_local = mc_cr_outputs.m_time_required_su;		//[s] Receiver model returns MIN(time required to completely startup, full timestep duration)
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

				if( m_is_tes )
				{
					mc_tes.idle(mc_sim_info.m_step, mc_weather.ms_outputs.m_tdry + 273.15, mc_tes_outputs);
				}

				are_models_converged = true;

				break;

			case tech_operating_modes::CR_OFF__PC_OFF__TES_OFF__AUX_OFF:
				// Solve all models as 'off' or 'idle'
				// Collector/receiver

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				mc_cr_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
				mc_cr_inputs.m_field_control = 0.0;						//[-] Field OFF when receiver is OFF!
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

				if( m_is_tes )
				{
					mc_tes.idle(mc_sim_info.m_step, mc_weather.ms_outputs.m_tdry + 273.15, mc_tes_outputs);
				}

				are_models_converged = true;

				break;		// exit switch() after CR_OFF__PC_OFF__TES_OFF__AUX_OFF:

			case tech_operating_modes::CR_OFF__PC_SU__TES_DC__AUX_OFF:
			{
				// Use thermal storage to startup power cycle
				// This solver iterates to find the thermal storage outlet temperature to the power cycle
				//    and the power cycle demand mass flow rate that reach system equilibrium

				// Set Solved Controller Variables Here (that won't be reset in this operating mode)
				m_defocus = 1.0;

				double T_pc_in_guess = mc_tes.get_hot_temp();

				double T_pc_in_upper = std::numeric_limits<double>::quiet_NaN();	//[K]
				double T_pc_in_lower = std::numeric_limits<double>::quiet_NaN();	//[K]

				double y_T_pc_in_upper = std::numeric_limits<double>::quiet_NaN();	//[-]
				double y_T_pc_in_lower = std::numeric_limits<double>::quiet_NaN();	//[-]

				bool is_upper_bound = false;
				bool is_lower_bound = false;
				bool is_upper_error = false;
				bool is_lower_error = false;

				double tol_C = 1.0;								//[K]
				double tol = tol_C / m_cycle_T_htf_hot_des;		//[-]

				double diff_T_pc_in = 999.9*tol;

				int iter_T_pc_in = 0;

				int exit_mode = NO_SOLUTION;
				double exit_tolerance = std::numeric_limits<double>::quiet_NaN();

				double T_pc_in_calc = std::numeric_limits<double>::quiet_NaN();

				while( abs(diff_T_pc_in) > tol || diff_T_pc_in != diff_T_pc_in )
				{
					iter_T_pc_in++;			// First iteration = 1

					// Check if distance between bounds is "too small"
					double diff_T_bounds = T_pc_in_upper - T_pc_in_lower;
					if( diff_T_bounds / m_cycle_T_htf_hot_des < tol / 2.0 )
					{
						exit_mode = NO_SOLUTION;
						exit_tolerance = diff_T_pc_in;
						break;
					}

					// Subsequent iterations need to re-calculate T_pc_in_guess
					if( iter_T_pc_in > 1 )
					{
						if( diff_T_pc_in != diff_T_pc_in )
						{	// Models did not solve such that a convergence error could be generated
							// If upper bound exists, then assume this is lower bound and try iterating
							// If upper bound does not exist, then we don't have information to move forward
							// ... if can't move forward, then assume not enough storage to meet *optimal* PC startup requirements
							// ... but, we can still attempt to begin PC startup during this timestep
							if( !is_upper_bound )
							{
								exit_mode = NO_SOLUTION;
								exit_tolerance = diff_T_pc_in;
								break;
							}
							else
							{
								is_lower_bound = true;
								is_lower_error = false;
								T_pc_in_lower = T_pc_in_guess;
								T_pc_in_guess = 0.5*(T_pc_in_lower + T_pc_in_upper);
							}
						}
						else if( diff_T_pc_in > 0.0 )	// T_pc_in_guess was too low
						{
							is_lower_bound = true;
							is_lower_error = true;
							T_pc_in_lower = T_pc_in_guess;		// Set lower bound
							y_T_pc_in_lower = diff_T_pc_in;		// Set lower convergence error

							if( is_upper_bound && is_upper_error )
							{
								T_pc_in_guess = y_T_pc_in_upper / (y_T_pc_in_upper - y_T_pc_in_lower)*(T_pc_in_lower - T_pc_in_upper) + T_pc_in_upper;
							}
							else if( is_upper_bound )
							{
								T_pc_in_guess = 0.5*(T_pc_in_lower + T_pc_in_upper);
							}
							else
							{	// Initial guess is the hot side temperature at the beginning of the timestep
								// Assume that the storage won't get hotter throughout the timestep,
								// ... so if results from 1st guess suggests that a higher temperature is required, get out

								exit_mode = NO_SOLUTION;
								exit_tolerance = diff_T_pc_in;
								break;
							}
						}
						else							// T_pc_in_guess was too high
						{
							is_upper_bound = true;
							is_upper_error = true;
							T_pc_in_upper = T_pc_in_guess;		// Set upper bound
							y_T_pc_in_upper = diff_T_pc_in;		// Set upper convergence error

							if( is_lower_bound && is_lower_error )
							{
								T_pc_in_guess = y_T_pc_in_upper / (y_T_pc_in_upper - y_T_pc_in_lower)*(T_pc_in_lower - T_pc_in_upper) + T_pc_in_upper;
							}
							else if( is_lower_bound )
							{
								T_pc_in_guess = 0.5*(T_pc_in_lower + T_pc_in_upper);
							}
							else
							{
								T_pc_in_guess = T_pc_in_calc - 5.0;
							}

						}
					}


					// Call the power cycle in STARTUP_CONTROLLED mode
					mc_pc_htf_state.m_temp_in = T_pc_in_guess - 273.15;		//[C] convert from K
					mc_pc_inputs.m_standby_control = C_csp_power_cycle::E_csp_power_cycle_modes::STARTUP_CONTROLLED;

					mc_power_cycle.call(mc_weather.ms_outputs,
						mc_pc_htf_state,
						mc_pc_inputs,
						mc_pc_outputs,
						mc_sim_info);

					// Use 'm_m_dot_demand' as an input to TES model
					// 'm_m_dot_htf' and 'm_m_dot_htf_ref' will be NaN, but that should be ok...

					double m_dot_pc = mc_pc_outputs.m_m_dot_demand / 3600.0;		//[kg/s]

					bool dc_solved = mc_tes.discharge(mc_sim_info.m_step, mc_weather.ms_outputs.m_tdry + 273.15, m_dot_pc, m_T_htf_cold_des, T_pc_in_calc, mc_tes_outputs);

					if( dc_solved )
					{
						diff_T_pc_in = (T_pc_in_calc - T_pc_in_guess) / T_pc_in_guess;
					}
					else
					{
						diff_T_pc_in = std::numeric_limits<double>::quiet_NaN();	// Provided discharge mass flow rate is too large for amount of storage remaining
						continue;
					}

					exit_mode = CONVERGED;
				}

				if( exit_mode == NO_SOLUTION )
				{	// Try fully discharging TES and beginning PC startup
					// Check that power cycle hasn't completely started up, as that suggests an error above (in this mode)

					// Get mass flow rate and temperature at a full discharge
					double m_dot_pc = std::numeric_limits<double>::quiet_NaN();
					mc_tes.discharge_full(mc_sim_info.m_step, mc_weather.ms_outputs.m_tdry + 273.15, m_T_htf_cold_des, T_pc_in_calc, m_dot_pc, mc_tes_outputs);


					// If receiver IS producing energy, try starting up power cycle
					// Power Cycle: STARTUP
					mc_pc_htf_state.m_temp_in = T_pc_in_calc - 273.15;				//[C]
					mc_pc_htf_state.m_m_dot = m_dot_pc*3600.0;								//[kg/hr] no mass flow rate to power cycle
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
					double step_local_su = mc_pc_outputs.m_time_required_su;		//[s] power cycle model returns MIN(time required to completely startup, full timestep duration)
					if( step_local_su < mc_sim_info.m_step )
					{
						are_models_converged = false;
						m_is_CR_OFF__PC_SU__TES_DC__AUX_OFF_avail = false;
						break;
						
						//throw(C_csp_exception("PC startup using TES failed...", ""));
					}

					// Should probably just write a message above and then move to OFF
				}
				else if( exit_mode == CONVERGED )
				{

				}
				else
				{
					throw(C_csp_exception("PC startup using TES failed to converge in a recognized mode", ""));
				}

				// Now run CR at 'OFF'
				mc_cr_htf_state.m_temp_in = m_T_htf_cold_des - 273.15;		//[C], convert from [K]
				mc_cr_inputs.m_field_control = 0.0;							//[-] Field OFF when receiver is OFF!
				mc_cr_inputs.m_input_operation_mode = C_csp_collector_receiver::E_csp_cr_modes::OFF;
				mc_collector_receiver.call(mc_weather.ms_outputs,
					mc_cr_htf_state,
					mc_cr_inputs,
					mc_cr_outputs,
					mc_sim_info);

				are_models_converged = true;
			}

				break;

			case tech_operating_modes::CR_ON__PC_OFF__TES_CH__AUX_OFF:
			{
				// Method to solve operating mode where the CR is on (under some fixed operating conditions, i.e. defocus)
				// and charging TES. No PC operating or AUX, so the output of the CR connects directly to TES

				// (the following is modeled after 'solver_cr_to_pc_to_cr'... perhaps this could be generalized in the future)

				// *****************************
				// *****************************

				// Need to step through and validate this operating mode!!!!

				// *****************************
				// *****************************

				// Guess the receiver inlet temperature = cold storage tank temperature
				double T_rec_in_guess_ini = mc_tes.get_cold_temp();	//[K]
				double T_rec_in_guess = T_rec_in_guess_ini;			//[K]

				// Initialize upper and lower bounds and booleans
				double T_rec_in_lower = std::numeric_limits<double>::quiet_NaN();
				double T_rec_in_upper = std::numeric_limits<double>::quiet_NaN();
				double y_rec_in_lower = std::numeric_limits<double>::quiet_NaN();
				double y_rec_in_upper = std::numeric_limits<double>::quiet_NaN();
				// Booleans for bounds and convergence error
				bool is_upper_bound = false;
				bool is_lower_bound = false;
				bool is_upper_error = false;
				bool is_lower_error = false;

				double tol_C = 1.0;								//[K]
				double tol = tol_C / m_cycle_T_htf_hot_des;		//[-]

				double relaxed_tol_mult = 5.0;
				double relaxed_tol = relaxed_tol_mult*tol;

				double diff_T_rec_in = 999.9*tol;

				int iter_T_rec_in = 0;

				int exit_mode = CONVERGED;
				double exit_tolerance = std::numeric_limits<double>::quiet_NaN();

				// Start iteration loop
				while( abs(diff_T_rec_in) > tol || diff_T_rec_in != diff_T_rec_in )
				{
					iter_T_rec_in++;		// First iteration = 1

					// Check if distance between bounds is "too small"
					//***********************
					// Check if distance between bounds is "too small"
					double diff_T_bounds = T_rec_in_upper - T_rec_in_lower;
					if( diff_T_bounds / T_rec_in_upper < tol / 2.0 )
					{
						if( diff_T_rec_in != diff_T_rec_in )
						{	// Models aren't producing power or are returning errors, and it appears we've tried the solution space for T_rec_in
				
							exit_mode = NO_SOLUTION;
							exit_tolerance = diff_T_rec_in;
							return;
						}
						else
						{	// Models are producing power, but convergence errors are not within Tolerance

							exit_mode = POOR_CONVERGENCE;
							exit_tolerance = diff_T_rec_in;
							return;
						}
					}


					if(iter_T_rec_in > 1)
					{
						// Subsequent iterations need to re-calculate T_rec_in_guess
						//***********************
						if( diff_T_rec_in != diff_T_rec_in )
						{	// Models did not solve such that a convergence error could be generated
							// However, we know that upper and lower bounds are set, so we can calculate a new guess via bisection method
							// but check that bounds exist, to be careful
							if( !is_lower_bound || !is_upper_bound )
							{
								exit_mode = NO_SOLUTION;
								exit_tolerance = diff_T_rec_in;
								return;
							}
							T_rec_in_guess = 0.5*(T_rec_in_lower + T_rec_in_upper);		//[C]
						}
						else if( diff_T_rec_in > 0.0 )		// Guess receiver inlet temperature was too low
						{
							is_lower_bound = true;
							is_lower_error = true;
							T_rec_in_lower = T_rec_in_guess;		// Set lower bound
							y_rec_in_lower = diff_T_rec_in;				// Set lower convergence error

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
							y_rec_in_upper = diff_T_rec_in;				// Set upper convergence error

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
					}	// End iter > 1 loop to reset guess values or get out

					// Solve the collector-receiver model
					// CR ON
					mc_cr_htf_state.m_temp_in = T_rec_in_guess - 273.15;		//[C], convert from K
					mc_cr_inputs.m_field_control = 1.0;							//[-] assuming no defocus
					mc_cr_inputs.m_input_operation_mode = C_csp_collector_receiver::ON;		// Receiver is operating and producing useful output

					mc_collector_receiver.call(mc_weather.ms_outputs,
						mc_cr_htf_state,
						mc_cr_inputs,
						mc_cr_outputs,
						mc_sim_info);

					// Check if receiver is OFF or model didn't solve
					// ... if that is the case, then can't send useful information to TES and need to branch off here
					if( mc_cr_outputs.m_m_dot_salt_tot == 0.0 || mc_cr_outputs.m_q_thermal == 0.0 )
					{
						
						// If first iteration, don't know enough about why collector/receiver is not producing power to advance iteration
						if( iter_T_rec_in == 1 )
						{	
							exit_mode = NO_SOLUTION;
							exit_tolerance = diff_T_rec_in;
							break;
						}
						else
						{	// Set this T_rec_in_guess as either upper or lower bound, depending on which end of DESIGN temp it falls
							// Assumption here is that receiver solved at first guess temperature
							// and that the failure wouldn't occur between established bounds
							if( T_rec_in_guess < T_rec_in_guess_ini )
							{
								if( is_lower_bound || !is_upper_bound )
								{
									exit_mode = NO_SOLUTION;
									exit_tolerance = diff_T_rec_in;
									break;
								}
								T_rec_in_lower = T_rec_in_guess;
								is_lower_bound = true;
								is_lower_error = false;
								// At this point, both and upper and lower bound should exist, so can generate new guess
								// And communicate this to Guess-Generator by setting diff_T_in to NaN
								diff_T_rec_in = std::numeric_limits<double>::quiet_NaN();
								continue;
							}
							else
							{
								if( is_upper_bound || !is_lower_bound )
								{
									exit_mode = NO_SOLUTION;
									exit_tolerance = diff_T_rec_in;
									break;
								}
								T_rec_in_upper = T_rec_in_guess;
								is_upper_bound = true;
								is_upper_error = false;
								// At this point, both and upper and lower bound should exist, so can generate new guess
								// And communicate this to Guess-Generator by setting diff_T_in to NaN
								diff_T_rec_in = std::numeric_limits<double>::quiet_NaN();
								continue;
							}
						}															
					}	// End logic to handle CR off or failure

					// Now, solved TES charge with CR outputs
					double T_htf_tes_cold_out = std::numeric_limits<double>::quiet_NaN();
					bool tes_charge_success = mc_tes.charge(mc_sim_info.m_step, mc_weather.ms_outputs.m_tdry+273.15, mc_cr_outputs.m_m_dot_salt_tot/3600.0, mc_cr_outputs.m_T_salt_hot+273.15,
						T_htf_tes_cold_out, mc_tes_outputs);
					T_htf_tes_cold_out -= 273.15;		//[C] convert back from K

					if( !tes_charge_success )
					{	// If receiver output overcharges storage during iteration, then assume we need some defocus and break loop
						// Receiver thermal output is *roughly* constant for varying receiver inlet temperatures,
						// ... and we don't want to try to throttle thermal power output by controlling this value

						exit_mode = KNOW_NEXT_MODE;
						exit_tolerance = std::numeric_limits<double>::quiet_NaN();						
						break;											
					}

					diff_T_rec_in = (T_htf_tes_cold_out - T_rec_in_guess) / T_rec_in_guess;
				}	// while () iteration on diff_T_rec_in

				if(exit_mode = KNOW_NEXT_MODE)
				{
					m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail = false;

					are_models_converged = false;
					break;

					//are_models_converged = false;
					//// operating_mode = .....
					//throw(C_csp_exception("Need operating_mode = CR_DF__PC_OFF__TES_FULL__AUX_OFF", "CSP Solver"));
					//break;
				}

				// Check exit_mode to determine how while loop exited
				// Reached convergence on defocus, but it is *possible* that the CR-PC iteration only solved at POOR CONVERGENCE
				if( exit_mode == POOR_CONVERGENCE )
				{
					if( abs(exit_tolerance) > relaxed_tol )
					{	// Did not converge within Relaxed Tolerance, shut off CR and PC

						// update 'exit_mode'
						exit_mode = NO_SOLUTION;
					}
					else
					{	// Convergence within Relaxed Tolerance, *Report message* but assume timestep solved in this mode

						error_msg = util::format("At time = %lg the collector/receiver and thermal storage charging method only reached a convergence"
							"= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
							mc_sim_info.m_time / 3600.0, exit_tolerance);
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);

						// update 'exit_mode' for following logic branches
						exit_mode = CONVERGED;
					}
				}
			
				if( exit_mode == NO_SOLUTION )
				{	// This mode did not solve, and did not provide enough information to try other operating mode. Shut plant off
				
					//operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;

					m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail = false;

					are_models_converged = false;
					break;								
				}
			
				if( exit_mode != CONVERGED )
				{	// All other options should be exhausted, so if not CONVERGED, something is wrong. Shut down plant
				
					//operating_mode = CR_OFF__PC_OFF__TES_OFF__AUX_OFF;

					m_is_CR_ON__PC_OFF__TES_CH__AUX_OFF_avail = false;

					are_models_converged = false;
					break;			
				}

				// If CR ON, TES CH solved, then solve powerblock OFF and get out
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
			
			}	// End brace after code for this operating mode - brace required to avoid compiler error for local variables

				break;

			case tech_operating_modes::CR_ON__PC_TARGET__TES_CH__AUX_OFF:
			{
				// CR is on (no defocus)
				// PC is on and hitting specified target
				// TES is charging


				
				double T_rec_in_guess_ini = m_T_htf_cold_des - 273.15;		//[C], convert from K
				double T_rec_in_guess = T_rec_in_guess_ini;					//[C]

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

				double tol_C = 1.0;								//[K]
				double tol = tol_C / m_cycle_T_htf_hot_des;		//[-]

				double relaxed_tol_mult = 5.0;
				double relaxed_tol = relaxed_tol_mult*tol;

				double diff_T_rec_in = 999.9*tol;

				int iter_T_rec_in = 0;

				int exit_mode = CONVERGED;
				double exit_tolerance = std::numeric_limits<double>::quiet_NaN();

				// Exit mode for inner iteration nest. Needs to be available at outer scope to help guide next controller decisions
				int q_pc_exit_mode = CONVERGED;

				// Start iteration loop
				while( abs(diff_T_rec_in) > tol || diff_T_rec_in != diff_T_rec_in )
				{
					iter_T_rec_in++;		// First iteration = 1

					// Check if distance between bounds is "too small"
					double diff_T_bounds = T_rec_in_upper - T_rec_in_lower;
					if(diff_T_bounds / T_rec_in_upper < tol / 2.0)
					{
						if( diff_T_rec_in != diff_T_rec_in )
						{	// Models aren't producing power or are returning errors, and it appears we've tried the solution space for T_rec_in
						
							exit_mode = NO_SOLUTION;
							exit_tolerance = std::numeric_limits<double>::quiet_NaN();
							break;
						}
						else
						{
							exit_mode = POOR_CONVERGENCE;
							exit_tolerance = diff_T_rec_in;
							break;
						}					
					}

					// Subsequent iterations need to re-calculate T_in
					if(iter_T_rec_in > 1)
					{			// diff_T_rec_in = (T_rec_in_calc - T_rec_in_guess)/T_rec_in_guess;
						if( diff_T_rec_in != diff_T_rec_in )
						{	// Models did not solve such that a convergence error could be calculated
							// However, we can check whether upper and lower bounds are set, and may be able to calculate a new guess via bisection method
							// But, check that bounds exist
							if( !is_lower_bound || !is_upper_bound )
							{
								exit_mode = NO_SOLUTION;
								exit_tolerance = std::numeric_limits<double>::quiet_NaN();
								break;
							}
							T_rec_in_guess = 0.5*(T_rec_in_lower + T_rec_in_upper);		//[C]
						}
						else if( diff_T_rec_in > 0.0 )		// Guess receiver inlet temperature was too low
						{
							is_lower_bound = true;
							is_lower_error = true;
							T_rec_in_lower = T_rec_in_guess;		//[C]
							y_rec_in_lower = diff_T_rec_in;			//[-]
							
							if( is_upper_bound && is_upper_error )
							{
								T_rec_in_guess = y_rec_in_upper / (y_rec_in_upper - y_rec_in_lower)*(T_rec_in_lower - T_rec_in_upper) + T_rec_in_upper;		//[C]
							}
							else if( is_upper_bound )
							{
								T_rec_in_guess = 0.5*(T_rec_in_lower + T_rec_in_upper);		//[C]	
							}
							else
							{
								T_rec_in_guess += 10.0;			//[C]
							}									
						}
						else
						{
							is_upper_bound = true;
							is_upper_error = true;
							T_rec_in_upper = T_rec_in_guess;		//[C] Set upper bound
							y_rec_in_upper = diff_T_rec_in;			//[-]

							if( is_lower_bound && is_upper_bound )
							{
								T_rec_in_guess = y_rec_in_upper / (y_rec_in_upper - y_rec_in_lower)*(T_rec_in_lower - T_rec_in_upper) + T_rec_in_upper;		//[C]
							}
							else if( is_lower_bound )
							{
								T_rec_in_guess = 0.5*(T_rec_in_lower + T_rec_in_upper);		//[C]
							}
							else
							{
								T_rec_in_guess -= 10.0;		//[C]
							}
						}										
					}

					// Solve the receiver model
					// CR ON
					mc_cr_htf_state.m_temp_in = T_rec_in_guess;			//[C], convert from [K]
					mc_cr_inputs.m_field_control = 1.0;					//[-] no defocusing for initial simulation
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
						if(iter_T_rec_in == 1)
						{
							exit_mode = NO_SOLUTION;
							exit_tolerance = std::numeric_limits<double>::quiet_NaN();
							break;  // exit while() on diff_T_rec_in
						}
						else
						{	// If collector-receiver model has solved with results previously, then try to find another guess value
							// Assumption here is that the receiver solved at the first guess temperature: 'T_rec_in_guess_ini'
							// Also, assume that if both upper and lower bounds exist, then can't generate a new guess
							if( T_rec_in_guess < T_rec_in_guess_ini )
							{	// If current guess value is less than initial value, then:

								// If lower bound is already set OR upper bound is not set, can't generate new guess
								if( is_lower_bound || !is_upper_bound )
								{
									exit_mode = NO_SOLUTION;
									exit_tolerance = std::numeric_limits<double>::quiet_NaN();
									break;	// exit while() on diff_T_rec_in
								}
								
								T_rec_in_lower = T_rec_in_guess;
								is_lower_bound = true;
								is_lower_error = false;
								// At this point, both upper and lower bound should exist, so we can generate new guess
								// And communicate this to Guess-Generator by setting diff_T_rec_in to NaN
								diff_T_rec_in = std::numeric_limits<double>::quiet_NaN();
								continue;							
							}
							else
							{	// If current guess value is greater than initial value, then:

								// If upper bound is already set OR lower bound is not set, can't generate new guess
								if( is_upper_bound || !is_lower_bound )
								{
									exit_mode = NO_SOLUTION;
									exit_tolerance = std::numeric_limits<double>::quiet_NaN();
									break;	// exit while() on diff_T_rec_in
								}

								T_rec_in_upper = T_rec_in_guess;
								is_upper_bound = true;
								is_upper_error = false;
								// At this point, both upper and lower bound should exist, so we can generate new guess
								// And communicate this to Guess-Generator by setting diff_T_rec_in to NaN
								diff_T_rec_in = std::numeric_limits<double>::quiet_NaN();
								continue;
							}
						}	// end else on if(iter_T_rec_in == 1)					
					}	// end logic to determine path if receiver is off or did not solve

					// Now need to iterate on receiver mass flow rate output to send to PC
					double m_dot_receiver = mc_cr_outputs.m_m_dot_salt_tot;		//[kg/hr]

					
					// Set up iteration variables
					// Calculate the max and min possible mass flow rates to the power cycle
					// If there is no solution space between them, then need to guess new receiver inlet temperature or get out of iteration


					// Knowing the receiver outlet temperature, can calculate the maximum mass flow rate available for charging
					double q_dot_tes_ch_local, m_dot_tes_ch_max, T_tes_cold_return;
					q_dot_tes_ch_local = m_dot_tes_ch_max = T_tes_cold_return = std::numeric_limits<double>::quiet_NaN();
					mc_tes.charge_avail_est(mc_cr_outputs.m_T_salt_hot+273.15, mc_sim_info.m_step, q_dot_tes_ch_local, m_dot_tes_ch_max, T_tes_cold_return);
					m_dot_tes_ch_max *= 3600.0;		//[kg/hr] convert from kg/s
					
					double m_dot_pc_min = fmax(0.0, m_dot_receiver - m_dot_tes_ch_max);
					double m_dot_pc_max = fmin(m_dot_receiver, m_m_dot_pc_des*1.2*m_cycle_max_frac);
							

					double m_dot_pc_lower   = m_dot_pc_min;
						// Goal in setting m_dot_pc_upper is to not send the power cycle a ridiculously large mass flow rate
						//     which could be possible in large solar multiple cases
					double m_dot_pc_upper	= m_dot_pc_max;


					double m_dot_pc_guess = fmin(m_dot_pc_max, fmin(m_dot_receiver, m_m_dot_pc_des*q_pc_target / m_cycle_q_dot_des));
					double m_dot_pc_guess_ini = m_dot_pc_guess;


					double y_m_dot_pc_lower	= std::numeric_limits<double>::quiet_NaN();
					double y_m_dot_pc_upper	= std::numeric_limits<double>::quiet_NaN();

					bool is_m_dot_upper_bound = true;
					bool is_m_dot_lower_bound = false;
					bool is_m_dot_upper_error = false;
					bool is_m_dot_lower_error = false;

					// Iteration assumption: increasing mass flow rate to power cycle at a constant inlet temperature will increase
					// the thermal power delivered to the cycle

					double tol_q_pc = 0.9*tol;		//[-] Set inner nest tolerance smaller than outer nest
					double diff_q_pc = 999.9*tol;	//[-] (Calc - Target)/Target: (+) Mass flow rate guess too high, (-) Mass flow rate guess too low
					int iter_q_pc = 0;				//[-]

					q_pc_exit_mode = CONVERGED;
					double q_pc_exit_tolerance = std::numeric_limits<double>::quiet_NaN();

					// Start iteration loop
					while( abs(diff_q_pc) > tol_q_pc || diff_q_pc != diff_q_pc )
					{
						iter_q_pc++;		// First iteration = 1

						// Check if distance between bounds is "too small"
						double diff_q_pc_bounds = m_dot_pc_upper - m_dot_pc_lower;
						if( diff_q_pc_bounds / m_dot_pc_upper < tol_q_pc/2.0 )
						{
							if(diff_q_pc != diff_q_pc)
							{	// Models aren't producing power or are returning errors, and it appears we've tried the solution space for m_dot_pc
								
								q_pc_exit_mode = NO_SOLUTION;
								q_pc_exit_tolerance = std::numeric_limits<double>::quiet_NaN();
								break;	// exits while() on diff_q_pc and sends control to while() on diff_T_rec_in 
							}
							else if(  (m_dot_pc_max - m_dot_pc_guess)/m_dot_receiver < tol_q_pc  )
							{	// Have tried maximum mass flow rate and can't achieve target power

								q_pc_exit_mode = UNDER_TARGET_PC;
								q_pc_exit_tolerance = std::numeric_limits<double>::quiet_NaN();
								break;	// exits while() on diff_q_pc and sends control to while() on diff_T_rec_in
							}
							else if(  (m_dot_pc_guess - m_dot_pc_min)/m_dot_receiver < tol_q_pc )
							{	// At minimum mass flow rate, we're still overshooting target power
							
								q_pc_exit_mode = OVER_TARGET_PC;
								q_pc_exit_tolerance = std::numeric_limits<double>::quiet_NaN();
								break;	// exits while() on diff_q_pc and sends control to while() on diff_T_rec_in
							}
							else
							{	// Models are producing power, but convergence errors are not within Tolerance

								q_pc_exit_mode = POOR_CONVERGENCE;
								q_pc_exit_tolerance = diff_q_pc;
								break;	// exits while() on diff_q_pc and sends control to while() on diff_T_rec_in
							}
						}


						// Subsequent iterations need to re-calculate T_in
						if(iter_q_pc > 1)
						{
							if( diff_q_pc != diff_q_pc )
							{	// Models did not solve such that a convergence error could be calculated
								// However, if upper and lower bounds are set, then we can calculate a new guess via bisection method
								// First, need to check that bounds exist
								if( !is_m_dot_lower_bound || !is_m_dot_upper_bound )
								{
									q_pc_exit_mode = NO_SOLUTION;
									q_pc_exit_tolerance = std::numeric_limits<double>::quiet_NaN();
									break;	// exits while() on diff_q_pc and sends control to while() on diff_T_rec_in
								}
								m_dot_pc_guess = 0.5*(m_dot_pc_lower + m_dot_pc_upper);		//[kg/hr]							
							}
							else if( diff_q_pc < 0.0 )		// Mass flow rate guess was too low
							{
								is_m_dot_lower_bound = true;
								is_m_dot_lower_error = true;
								m_dot_pc_lower = m_dot_pc_guess;	// Set lower bound
								y_m_dot_pc_lower = diff_q_pc;		// Set lower convergence error

								if( is_m_dot_upper_bound && is_m_dot_upper_error )	// False-position method
								{
									m_dot_pc_guess = y_m_dot_pc_upper / (y_m_dot_pc_upper - y_m_dot_pc_lower)*(m_dot_pc_lower - m_dot_pc_upper) + m_dot_pc_upper;	//[kg/hr]
								}
								else if( is_m_dot_upper_bound )
								{
									m_dot_pc_guess = 0.5*(m_dot_pc_lower + m_dot_pc_upper);		//[kg/hr]
								}
								else
								{	
									m_dot_pc_guess = fmin( 1.35*m_dot_pc_guess, m_dot_pc_max );	//[kg/hr]
								}
							}
							else							// Mass flow rate guess was too high
							{
								is_m_dot_upper_bound = true;
								is_m_dot_upper_error = true;
								m_dot_pc_upper = m_dot_pc_guess;	// Set upper bound
								y_m_dot_pc_upper = diff_q_pc;		// Set lower convergence error

								if( is_m_dot_lower_bound && is_m_dot_lower_error )	// False-position method
								{
									m_dot_pc_guess = y_m_dot_pc_upper / (y_m_dot_pc_upper - y_m_dot_pc_lower)*(m_dot_pc_lower - m_dot_pc_upper) + m_dot_pc_upper;	//[kg/hr]
								}
								else if( is_m_dot_lower_bound )
								{
									m_dot_pc_guess = 0.5*(m_dot_pc_lower + m_dot_pc_upper);		//[kg/hr]
								}
								else
								{
									m_dot_pc_guess *= 0.75;
								}
							}						
						}	// End calculation of m_dot_pc_guess
					
						// Set inputs to power cycle model
						// Power Cycle: ON
						mc_pc_htf_state.m_temp_in = mc_cr_outputs.m_T_salt_hot;		//[C]
						mc_pc_htf_state.m_m_dot = m_dot_pc_guess;					//[kg/hr] no mass flow rate to power cycle

						// Inputs
						mc_pc_inputs.m_standby_control = C_csp_power_cycle::E_csp_power_cycle_modes::ON;

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
							if( iter_q_pc == 1 )
							{
								q_pc_exit_mode = NO_SOLUTION;
								q_pc_exit_tolerance = diff_q_pc;
								break;		// exits while() on diff_q_pc and sends control to while() on diff_T_rec_in
							}
							else
							{	// If power cycle model has solved with results previously, then try to find another guess value
								// Assumption here is that power cycle at the first guess mass flow rate
								// Also, assume that if both upper and lower bounds exist, then can't generate a new guess
							
								if( m_dot_pc_guess < m_dot_pc_guess_ini )
								{	// If current guess value is less than initial value, then:	

									// If lower bound is already set OR upper bound is not set, can't generate new guess
									if( is_m_dot_lower_bound || !is_m_dot_upper_bound )
									{
										q_pc_exit_mode = NO_SOLUTION;
										q_pc_exit_tolerance = diff_q_pc;
										break;	// exits while() on diff_q_pc and sends control to while() on diff_T_rec_in
									}

									m_dot_pc_lower = m_dot_pc_guess;
									is_m_dot_lower_bound = true;
									is_m_dot_lower_error = false;
									
									// At this point, both upper and lower bound should exist, so we can generate new guess
									// And communicate this to Guess-Generator by setting diff_T_rec_in to NaN
									diff_q_pc = std::numeric_limits<double>::quiet_NaN();								
								}
								else
								{	// If current guess value is greater than initial guess, then:

									// If upper bound is already set OR lower bound is not set, can't generate new guess
									if( is_m_dot_upper_bound || !is_m_dot_lower_bound )
									{
										q_pc_exit_mode = NO_SOLUTION;
										q_pc_exit_tolerance = diff_q_pc;
										break;	// exits while() on diff_q_pc and sends control to while() on diff_T_rec_in
									}

									m_dot_pc_upper = m_dot_pc_guess;
									is_m_dot_upper_bound = true;
									is_m_dot_upper_error = false;

									// At this point, both upper and lower bound should exist, so we can generate new guess
									// And communicate this to Guess-Generator by setting diff_T_rec_in to NaN
									diff_q_pc = std::numeric_limits<double>::quiet_NaN();
								}
							}													
						}	// end logic to handle power cycle not solving 

						// Calculate thermal power delivered to power cycle
						// Calculate difference between calculated thermal power to cycle and target: diff_q_pc
						diff_q_pc = (mc_pc_outputs.m_q_dot_htf - q_pc_target)/q_pc_target; 	//[-] (Calc - Target)/Target: (+) Mass flow rate guess too high, (-) Mass flow rate guess too low

					}	// end while() on diff_q_pc
				
					// Check exit modes
					if(q_pc_exit_mode != CONVERGED && q_pc_exit_mode != POOR_CONVERGENCE)
					{
						break;		// exits while() on diff_T_rec_in
					}

					// Get power cycle HTF return temperature...
					double T_pc_out = mc_pc_outputs.m_T_htf_cold + 273.15;	//[K]

					// Charge storage
					double m_dot_tes = m_dot_receiver - m_dot_pc_guess;					//[kg/hr]
					double T_tes_cold_out = std::numeric_limits<double>::quiet_NaN();	
					bool tes_success = mc_tes.charge(mc_sim_info.m_step, mc_weather.ms_outputs.m_tdry + 273.15, m_dot_tes / 3600.0, mc_cr_outputs.m_T_salt_hot + 273.15, 
						T_tes_cold_out, mc_tes_outputs);

					if(!tes_success)
					{
						exit_mode = OVER_TARGET_PC;
						break;
					}

					// Enthalpy balancer (mixer)
					double T_rec_in_calc = (m_dot_tes*T_tes_cold_out + m_dot_pc_guess*T_pc_out)/m_dot_receiver - 273.15;		//[C]

					// Calculate diff_T_rec_in
					diff_T_rec_in = (T_rec_in_calc - T_rec_in_guess)/T_rec_in_guess;		//[-]

				}	// while () on diff_T_rec_in

				// Handle exit modes from outer and inner loops
					// If inner nest (power cycle thermal power iteration) causes exit, then we know CR solved with *some* inputs
				if(q_pc_exit_mode == UNDER_TARGET_PC)
				{
					m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_LO_SIDE = false;
					are_models_converged = false;
					break;
					
					//if( q_dot_tes_dc > 0.0 )
					//{	// Can we dispatch thermal storage to hit target cycle output?
					//
					//	throw(C_csp_exception("operating_mode = CR_OFF__PC_TARGET__TES_DC__AUX_OFF", "CSP Solver"));
					//	break;
					//}
					//else
					//{	// If not, try running power cycle below target power level with idle TES

					//	operating_mode = CR_ON__PC_RM_LO__TES_OFF__AUX_OFF;
					//	break;
					//}
				}
				else if( q_pc_exit_mode == OVER_TARGET_PC )
				{
					m_is_CR_ON__PC_TARGET__TES_CH__AUX_OFF_avail_HI_SIDE = false;
					are_models_converged = false;
					break;

					//// 2) Try operating power cycle at maximum capacity
					//// Assume we want to completely fill storage, so the power cycle operation should float to meet that condition
					//if( (q_dot_cr_on - q_dot_tes_ch)*(1.0 - tol_mode_switching) < q_pc_max )
					//{	// Storage and the power cycle operating between target and max can accept the remaining receiver output
					//	// Tolerance is applied so that if CR + TES is *close* to reaching PC  max, the controller tries that mode

					//	throw(C_csp_exception("operating_mode = CR_ON__PC_RM__TES_FULL__AUX_OFF", "CSP_Solver"));
					//}

					//// 3) Try defocusing the CR and operating the power cycle at maximum capacity
					//else
					//{
					//	throw(C_csp_exception("operating_mode = CR_DF__PC_FULL__TES_FULL__AUX_OFF", "CSP_Solver"));
					//}
				}

				// If convergence was successful, finalize this timestep and get out
					// Have solved CR, TES, and PC in this operating mode, so only need to set flag to get out of Mode Iteration
				are_models_converged = true;
			
			}
				break;

			default: 
				throw(C_csp_exception("Operation mode not recognized",""));

			}	// End switch() on receiver operating modes
		
		}	// End loop to find correct operating mode and system performance


		// Timestep solved: run post-processing, converged()		
		mc_collector_receiver.converged();
		mc_power_cycle.converged();
		mc_tes.converged();
				
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


		double step_hr = mc_sim_info.m_step/3600.0;
		// Save timestep outputs
		// This is after timestep convergence, so be sure convergence() methods don't unexpectedly change outputs
		mv_time_mid.push_back((time_previous+mc_sim_info.m_time)/2.0/3600.0);		//[hr] Time at end of timestep
		mv_solzen.push_back(mc_weather.ms_outputs.m_solzen);		//[deg] Solar zenith
		mv_beam.push_back(mc_weather.ms_outputs.m_beam);			//[W/m2] DNI
		mv_eta_field.push_back(mc_cr_outputs.m_eta_field);			//[-] Field efficiency (= eta_field_full * defocus)
		mv_defocus.push_back(m_defocus);							//[-] Defocus
		mv_rec_eta_thermal.push_back(mc_cr_outputs.m_eta_thermal);	//[-] Receiver thermal efficiency
		mv_rec_q_thermal.push_back(mc_cr_outputs.m_q_thermal*step_hr);	//[MWt-hr] Receiver thermal output
		mv_rec_q_startup.push_back(mc_cr_outputs.m_q_startup);		//[MWt-hr] Receiver startup thermal energy
		mv_pc_eta.push_back(mc_pc_outputs.m_eta);					//[-] Power cycle efficiency (gross - no parasitics outside of power block)
		mv_pc_W_gross.push_back(mc_pc_outputs.m_P_cycle*step_hr);	//[MWe-hr] Power cycle electric gross energy (only parasitics baked into regression) over (perhaps varying length) timestep
		mv_pc_q_startup.push_back(mc_pc_outputs.m_q_startup);		//[MWt-hr] Power cycle startup thermal energy
		mv_tes_q_losses.push_back(mc_tes_outputs.m_q_dot_loss*step_hr);	//[MWt-hr] TES thermal losses to environment
		mv_tes_q_heater.push_back(mc_tes_outputs.m_q_heater*step_hr);	//[MWt-hr] Energy into TES from heaters (hot+cold) to maintain tank temperatures
		mv_tes_T_hot.push_back(mc_tes_outputs.m_T_hot_final-273.15);	//[C] TES hot temperature at end of timestep
		mv_tes_T_cold.push_back(mc_tes_outputs.m_T_cold_final-273.15);	//[C] TES cold temperature at end of timestep

		// Track time and step forward
		is_sim_timestep_complete = true;
		time_previous = mc_sim_info.m_time;						//[s]
		mc_sim_info.m_step = step_local;						//[s]
		mc_sim_info.m_time = time_previous + step_local;		//[s]
					
		

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
	while( abs(diff_T_in) > tol || diff_T_in != diff_T_in )
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


		// Subsequent iterations need to re-calculate T_in
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
					if( is_lower_bound || !is_upper_bound )
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
					continue;
				}
				else
				{
					if( is_upper_bound || !is_lower_bound )
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
					continue;
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
					if( is_lower_bound || !is_upper_bound )
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
					if( is_upper_bound || !is_lower_bound )
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
		else
		{
			diff_T_in = (mc_pc_outputs.m_T_htf_cold - T_rec_in_guess) / T_rec_in_guess;
		}

	}	// end iteration on T_rec_in

	exit_mode = CONVERGED;
	exit_tolerance = diff_T_in;

	return;
}
