    { SSC_INPUT,        SSC_ARRAY,       "aux_array",                 "Coefficients for auxiliary heater parasitics calcs",             "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fossil_mode",               "Fossil backup mode 1=Normal 2=Topping",                          "-",            "",             "controller",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",                "Fraction of thermal power required for standby",                 "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_standby_reset",           "Maximum allowable time for PB standby operation",                "hr",           "",             "controller",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sf_type",                   "Solar field type, 1 = trough, 2 = tower",                        "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_type",                  "1=2-tank, 2=thermocline",                                        "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_a",                 "Dispatch logic without solar",                                   "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_b",                 "Dispatch logic with solar",                                      "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_c",                 "Dispatch logic for turbine load fraction",                       "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "ffrac",                     "Fossil dispatch logic",                                          "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tc_fill",                   "Thermocline fill material",                                      "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tc_void",                   "Thermocline void fraction",                                      "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_dis_out_min",             "Min allowable hot side outlet temp during discharge",            "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_ch_out_max",              "Max allowable cold side outlet temp during charge",              "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nodes",                     "Nodes modeled in the flow path",                                 "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_tc_cold",                 "0=entire tank is hot, 1=entire tank is cold",                    "-",            "",             "controller",     "*",                       "",                      "" },

    // Time of use schedules for thermal storage
    { SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",          "Dispatch 12mx24h schedule for week days",                         "",             "",             "tou_translator", "*",                       "",                      "" }, 
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",          "Dispatch 12mx24h schedule for weekends",                          "",             "",             "tou_translator", "*",                       "",                      "" }, 
															          																	                  
						          
//   VARTYPE            DATATYPE          NAME                LABEL                                                                        UNITS           META                            GROUP             REQUIRED_IF                CONSTRAINTS              UI_HINTS
	// Power Cycle Inputs
	{ SSC_INPUT,        SSC_NUMBER,      "pc_config",         "0: Steam Rankine (224), 1: user defined",                                   "-",            "",                             "powerblock",     "?=0",                     "INTEGER",               "" },        
	{ SSC_INPUT,        SSC_NUMBER,      "eta_ref",           "Reference conversion efficiency at design condition",                       "none",         "",                             "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "startup_time",      "Time needed for power block startup",                                       "hr",           "",                             "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",      "Fraction of design thermal power needed for startup",                       "none",         "",                             "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",        "Fraction of thermal power required for standby mode",                       "none",         "",                             "powerblock",     "*",                       "",                      "" },
    

	// Steam Rankine cycle
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",         "Reference condenser cooling water inlet/outlet T diff",                     "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",         "Reference ambient temperature at design point",                             "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil",            "Boiler operating pressure",                                                 "bar",          "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                "Flag for using dry cooling or wet cooling system",                          "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",        "Cooling tower approach temperature",                                        "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",         "ITD at design for dry system",                                              "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",      "Condenser pressure ratio",                                                  "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",        "Power block blowdown steam fraction ",                                      "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",        "Minimum condenser pressure",                                                "inHg",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",          "Number of part-load increments for the heat rejection system",              "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",              "Fraction indicating wet cooling use for hybrid system",                     "none",         "constant=[0,0,0,0,0,0,0,0,0]", "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tech_type",         "Turbine inlet pressure control flag (sliding=user, fixed=trough)",          "1/2/3",         "tower/trough/user",           "powerblock",     "pc_config=0",             "",                      "" },
	
		// User Defined cycle
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_amb_des",         "Ambient temperature at user-defined power cycle design point",                   "C",	    "",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_f_W_dot_cool_des",  "Percent of user-defined power cycle design gross output consumed by cooling",    "%",	    "",                            "user_defined_PC", "pc_config=1",            "",                      "" },

		// ********************************
		// ********************************
		// Now add the TOU class
		// ********************************
		// ********************************
		C_csp_tou_block_schedules tou;
		tou.setup_block_uniform_tod();
		//C_csp_tou_block_schedules::S_params *tou_params = &tou.ms_params;
		//tou_params->mc_csp_ops.mc_weekdays = as_matrix("weekday_schedule");
		//tou_params->mc_csp_ops.mc_weekends = as_matrix("weekend_schedule");
		//tou_params->mc_pricing.mc_weekdays = as_matrix("weekday_schedule");
		//tou_params->mc_pricing.mc_weekends = as_matrix("weekend_schedule");
		//tou.mc_dispatch_params.m_dispatch_optimize = false;
		//tou.mc_dispatch_params.m_is_write_ampl_dat = false;
		//tou.mc_dispatch_params.m_is_ampl_engine = false;
		//tou.mc_dispatch_params.m_ampl_data_dir = "";
		//tou.mc_dispatch_params.m_ampl_exec_call = "";
		//if( tou.mc_dispatch_params.m_dispatch_optimize )
		//{
		//	tou.mc_dispatch_params.m_optimize_frequency = as_integer("disp_frequency");
		//	tou.mc_dispatch_params.m_optimize_horizon = as_integer("disp_horizon");
		//	tou.mc_dispatch_params.m_max_iterations = as_integer("disp_max_iter");
		//	tou.mc_dispatch_params.m_solver_timeout = as_double("disp_timeout");
		//	tou.mc_dispatch_params.m_mip_gap = as_double("disp_mip_gap");
		//	tou.mc_dispatch_params.m_presolve_type = as_integer("disp_spec_presolve");
		//	tou.mc_dispatch_params.m_bb_type = as_integer("disp_spec_bb");
		//	tou.mc_dispatch_params.m_scaling_type = as_integer("disp_spec_scaling");
		//}
		//tou.mc_dispatch_params.m_is_block_dispatch = !tou.mc_dispatch_params.m_dispatch_optimize;      //mw
		//tou.mc_dispatch_params.m_use_rule_1 = true;
		//tou.mc_dispatch_params.m_standby_off_buffer = 2.0;
		//tou.mc_dispatch_params.m_use_rule_2 = false;
		//tou.mc_dispatch_params.m_q_dot_rec_des_mult = -1.23;
		//tou.mc_dispatch_params.m_f_q_dot_pc_overwrite = -1.23;

		//size_t n_f_turbine = -1;
		//ssc_number_t *p_f_turbine = as_array("tslogic_c", &n_f_turbine);
		//tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC].resize(n_f_turbine, 0.0);
		////tou_params->mv_t_frac.resize(n_f_turbine, 0.0);
		//for( int i = 0; i < n_f_turbine; i++ )
		//	tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC][i] = (double)p_f_turbine[i];

		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(9, 0.0);
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][0] = 1.0;  //as_double("dispatch_factor1");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][1] = 1.0;  //as_double("dispatch_factor2");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][2] = 1.0;  //as_double("dispatch_factor3");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][3] = 1.0;  //as_double("dispatch_factor4");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][4] = 1.0;  //as_double("dispatch_factor5");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][5] = 1.0;  //as_double("dispatch_factor6");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][6] = 1.0;  //as_double("dispatch_factor7");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][7] = 1.0;  //as_double("dispatch_factor8");
		//tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][8] = 1.0;  //as_double("dispatch_factor9");

		// System parameters
		C_csp_solver::S_csp_system_params system;
		system.m_pb_fixed_par = as_double("pb_fixed_par");
		system.m_bop_par = 0.0;
		system.m_bop_par_f = 0.0;
		system.m_bop_par_0 = 0.0;
		system.m_bop_par_1 = 0.0;
		system.m_bop_par_2 = 0.0;

		// Instantiate Solver
		C_csp_solver csp_solver(weather_reader, c_trough, heat_sink, storage, tou, system);

		int out_type = -1;
		std::string out_msg = "";
		try
		{
			// Initialize Solver
			csp_solver.init();
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( csp_solver.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				log(out_msg, out_type);
			}

			log(csp_exception.m_error_message, SSC_ERROR, -1.0);

			return;
		}
		
		// Set up ssc output arrays
		// Set steps per hour
		double nhourssim = 8760.0;				//[hr] Number of hours to simulate
		C_csp_solver::S_sim_setup sim_setup;
		sim_setup.m_sim_time_start = 0.0;				//[s] starting first hour of year
		sim_setup.m_sim_time_end = nhourssim*3600.0;	//[s] full year simulation

		int steps_per_hour = 1;			//[-]
		int n_steps_fixed = steps_per_hour*8760.0;	//[-]
		sim_setup.m_report_step = 3600.0 / (double) steps_per_hour;	//[s]

		float **ptr_array = new float*[C_csp_solver::N_END];
		float **post_proc_array = new float*[C_csp_solver::N_END_POST_PROC];

		for( int i = 0; i < C_csp_solver::N_END_POST_PROC; i++ )
		{
			post_proc_array[i] = 0;
		}

		post_proc_array[C_csp_solver::PC_Q_STARTUP] = allocate("q_pc_startup", n_steps_fixed);

		for( int i = 0; i < C_csp_solver::N_END; i++ )
		{
			ptr_array[i] = 0;
		}

			// Simulation outputs
		ptr_array[C_csp_solver::TIME_FINAL] = allocate("time_hr", n_steps_fixed);
		ptr_array[C_csp_solver::SOLZEN] = allocate("solzen", n_steps_fixed);
		ptr_array[C_csp_solver::SOLAZ] = allocate("solaz", n_steps_fixed);
		ptr_array[C_csp_solver::BEAM] = allocate("beam", n_steps_fixed);
		ptr_array[C_csp_solver::TDRY] = allocate("tdry", n_steps_fixed);
		ptr_array[C_csp_solver::TWET] = allocate("twet", n_steps_fixed);
		ptr_array[C_csp_solver::RH] = allocate("rh", n_steps_fixed);


		// Collector-receiver outputs
		ptr_array[C_csp_solver::CR_Q_INC] = allocate("q_sf_inc", n_steps_fixed);
		ptr_array[C_csp_solver::CR_OPT_ETA] = allocate("eta_field", n_steps_fixed);
		ptr_array[C_csp_solver::CR_DEFOCUS] = allocate("defocus", n_steps_fixed);
		ptr_array[C_csp_solver::CR_ADJUST] = allocate("sf_adjust_out", n_steps_fixed);
		ptr_array[C_csp_solver::REC_Q_DOT_INC] = allocate("q_dot_rec_inc", n_steps_fixed);
		ptr_array[C_csp_solver::REC_ETA_THERMAL] = allocate("eta_therm", n_steps_fixed);
		ptr_array[C_csp_solver::REC_Q_DOT] = allocate("Q_thermal", n_steps_fixed);
		ptr_array[C_csp_solver::REC_M_DOT] = allocate("m_dot_rec", n_steps_fixed);
		ptr_array[C_csp_solver::REC_Q_DOT_STARTUP] = allocate("q_startup", n_steps_fixed);
		ptr_array[C_csp_solver::REC_T_IN] = allocate("T_rec_in", n_steps_fixed);
		ptr_array[C_csp_solver::REC_T_OUT] = allocate("T_rec_out", n_steps_fixed);
		ptr_array[C_csp_solver::CR_Q_DOT_PIPING_LOSS] = allocate("q_piping_losses", n_steps_fixed);

		// Power cycle outputs
		ptr_array[C_csp_solver::PC_ETA_THERMAL] = allocate("eta", n_steps_fixed);
		ptr_array[C_csp_solver::PC_Q_DOT] = allocate("q_pb", n_steps_fixed);
		ptr_array[C_csp_solver::PC_M_DOT] = allocate("m_dot_pc", n_steps_fixed);
		ptr_array[C_csp_solver::PC_Q_DOT_STARTUP] = allocate("q_dot_pc_startup", n_steps_fixed);
		ptr_array[C_csp_solver::PC_W_DOT] = allocate("P_cycle", n_steps_fixed);
		ptr_array[C_csp_solver::PC_T_IN] = allocate("T_pc_in", n_steps_fixed);
		ptr_array[C_csp_solver::PC_T_OUT] = allocate("T_pc_out", n_steps_fixed);
		ptr_array[C_csp_solver::PC_M_DOT_WATER] = allocate("m_dot_water_pc", n_steps_fixed);

		// Thermal energy storage outputs
		ptr_array[C_csp_solver::TES_Q_DOT_LOSS] = allocate("tank_losses", n_steps_fixed);
		ptr_array[C_csp_solver::TES_W_DOT_HEATER] = allocate("q_heater", n_steps_fixed);
		ptr_array[C_csp_solver::TES_T_HOT] = allocate("T_tes_hot", n_steps_fixed);
		ptr_array[C_csp_solver::TES_T_COLD] = allocate("T_tes_cold", n_steps_fixed);
		ptr_array[C_csp_solver::TES_Q_DOT_DC] = allocate("q_dc_tes", n_steps_fixed);
		ptr_array[C_csp_solver::TES_Q_DOT_CH] = allocate("q_ch_tes", n_steps_fixed);
		ptr_array[C_csp_solver::TES_E_CH_STATE] = allocate("e_ch_tes", n_steps_fixed);
		ptr_array[C_csp_solver::TES_M_DOT_DC] = allocate("m_dot_tes_dc", n_steps_fixed);
		ptr_array[C_csp_solver::TES_M_DOT_CH] = allocate("m_dot_tes_ch", n_steps_fixed);

		// Parasitics outputs
		ptr_array[C_csp_solver::COL_W_DOT_TRACK] = allocate("pparasi", n_steps_fixed);
		ptr_array[C_csp_solver::CR_W_DOT_PUMP] = allocate("P_tower_pump", n_steps_fixed);
		ptr_array[C_csp_solver::SYS_W_DOT_PUMP] = allocate("htf_pump_power", n_steps_fixed);
		ptr_array[C_csp_solver::PC_W_DOT_COOLING] = allocate("P_cooling_tower_tot", n_steps_fixed);
		ptr_array[C_csp_solver::SYS_W_DOT_FIXED] = allocate("P_fixed", n_steps_fixed);
		ptr_array[C_csp_solver::SYS_W_DOT_BOP] = allocate("P_plant_balance_tot", n_steps_fixed);

		// System outputs
		ptr_array[C_csp_solver::W_DOT_NET] = allocate("P_out_net", n_steps_fixed);

		// Controller outputs
		ptr_array[C_csp_solver::TOU_PERIOD] = allocate("tou_value", n_steps_fixed);
		ptr_array[C_csp_solver::PRICING_MULT] = allocate("pricing_mult", n_steps_fixed);
		ptr_array[C_csp_solver::N_OP_MODES] = allocate("n_op_modes", n_steps_fixed);
		ptr_array[C_csp_solver::OP_MODE_1] = allocate("op_mode_1", n_steps_fixed);
		ptr_array[C_csp_solver::OP_MODE_2] = allocate("op_mode_2", n_steps_fixed);
		ptr_array[C_csp_solver::OP_MODE_3] = allocate("op_mode_3", n_steps_fixed);
		ptr_array[C_csp_solver::ERR_M_DOT] = allocate("m_dot_balance", n_steps_fixed);
		ptr_array[C_csp_solver::ERR_Q_DOT] = allocate("q_balance", n_steps_fixed);


		ptr_array[C_csp_solver::PC_Q_DOT_SB] = allocate("q_dot_pc_sb", n_steps_fixed);
		ptr_array[C_csp_solver::PC_Q_DOT_MIN] = allocate("q_dot_pc_min", n_steps_fixed);
		ptr_array[C_csp_solver::PC_Q_DOT_MAX] = allocate("q_dot_pc_max", n_steps_fixed);
		ptr_array[C_csp_solver::PC_Q_DOT_TARGET] = allocate("q_dot_pc_target", n_steps_fixed);

		ptr_array[C_csp_solver::CTRL_IS_REC_SU] = allocate("is_rec_su_allowed", n_steps_fixed);
		ptr_array[C_csp_solver::CTRL_IS_PC_SU] = allocate("is_pc_su_allowed", n_steps_fixed);
		ptr_array[C_csp_solver::CTRL_IS_PC_SB] = allocate("is_pc_sb_allowed", n_steps_fixed);
		ptr_array[C_csp_solver::EST_Q_DOT_CR_SU] = allocate("q_dot_est_cr_su", n_steps_fixed);
		ptr_array[C_csp_solver::EST_Q_DOT_CR_ON] = allocate("q_dot_est_cr_on", n_steps_fixed);
		ptr_array[C_csp_solver::EST_Q_DOT_DC] = allocate("q_dot_est_tes_dc", n_steps_fixed);
		ptr_array[C_csp_solver::EST_Q_DOT_CH] = allocate("q_dot_est_tes_ch", n_steps_fixed);

		ptr_array[C_csp_solver::CTRL_OP_MODE_SEQ_A] = allocate("operating_modes_a", n_steps_fixed);
		ptr_array[C_csp_solver::CTRL_OP_MODE_SEQ_B] = allocate("operating_modes_b", n_steps_fixed);
		ptr_array[C_csp_solver::CTRL_OP_MODE_SEQ_C] = allocate("operating_modes_c", n_steps_fixed);

		ptr_array[C_csp_solver::DISPATCH_SOLVE_STATE] = allocate("disp_solve_state", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_SOLVE_ITER] = allocate("disp_solve_iter", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_SOLVE_OBJ] = allocate("disp_objective", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_SOLVE_OBJ_RELAX] = allocate("disp_obj_relax", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_QSF_EXPECT] = allocate("disp_qsf_expected", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_QSFPROD_EXPECT] = allocate("disp_qsfprod_expected", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_QSFSU_EXPECT] = allocate("disp_qsfsu_expected", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_TES_EXPECT] = allocate("disp_tes_expected", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_PCEFF_EXPECT] = allocate("disp_pceff_expected", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_SFEFF_EXPECT] = allocate("disp_thermeff_expected", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_QPBSU_EXPECT] = allocate("disp_qpbsu_expected", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_WPB_EXPECT] = allocate("disp_wpb_expected", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_REV_EXPECT] = allocate("disp_rev_expected", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_PRES_NCONSTR] = allocate("disp_presolve_nconstr", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_PRES_NVAR] = allocate("disp_presolve_nvar", n_steps_fixed);
		ptr_array[C_csp_solver::DISPATCH_SOLVE_TIME] = allocate("disp_solve_time", n_steps_fixed);

		try
		{
			// Simulate !
			csp_solver.Ssimulate(sim_setup,
				ssc_trough_physical_process_heat_sim_progress, (void*)this,
				ptr_array,
				post_proc_array);
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( csp_solver.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				log(out_msg);
			}

			log(csp_exception.m_error_message, SSC_WARNING);
			delete[] ptr_array;
			delete[] post_proc_array;

			return;
		}


		// ************************************
		// ************************************
		delete[] ptr_array;
		delete[] post_proc_array;
		// ************************************
		// ************************************


	}

