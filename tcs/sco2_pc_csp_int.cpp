#include "sco2_pc_csp_int.h"
#include "sco2_pc_core.h"
#include "csp_solver_util.h"

#include "nlopt.hpp"

void C_sco2_recomp_csp::design(C_sco2_recomp_csp::S_des_par des_par)
{
	ms_des_par = des_par;

	design_core();
}

void C_sco2_recomp_csp::design_core()
{
	// Design the recompression cycle
		// Define sCO2 cycle design parameter structure
	ms_rc_cycle_des_par.m_W_dot_net = ms_des_par.m_W_dot_net;		//[kWe]
	ms_rc_cycle_des_par.m_eta_thermal = ms_des_par.m_eta_thermal;	//[-]
	ms_rc_cycle_des_par.m_T_mc_in = ms_des_par.m_T_amb_des+ms_des_par.m_dt_mc_approach;	//[K]
	ms_rc_cycle_des_par.m_T_t_in = ms_des_par.m_T_htf_hot_in-ms_des_par.m_phx_dt_hot_approach;	//[K]
	ms_rc_cycle_des_par.m_DP_LT = ms_des_par.m_DP_LT;
	ms_rc_cycle_des_par.m_DP_HT = ms_des_par.m_DP_HT;
	ms_rc_cycle_des_par.m_DP_PC = ms_des_par.m_DP_PC;
	ms_rc_cycle_des_par.m_DP_PHX = ms_des_par.m_DP_PHX;
	ms_rc_cycle_des_par.m_eta_mc = ms_des_par.m_eta_mc;
	ms_rc_cycle_des_par.m_eta_rc = ms_des_par.m_eta_rc;
	ms_rc_cycle_des_par.m_eta_t = ms_des_par.m_eta_t;
	ms_rc_cycle_des_par.m_N_sub_hxrs = ms_des_par.m_N_sub_hxrs;
	ms_rc_cycle_des_par.m_P_high_limit = ms_des_par.m_P_high_limit;
	ms_rc_cycle_des_par.m_tol = ms_des_par.m_tol;
	ms_rc_cycle_des_par.m_opt_tol = ms_des_par.m_opt_tol;
	ms_rc_cycle_des_par.m_N_turbine = ms_des_par.m_N_turbine;

	// using -> C_RecompCycle::S_auto_opt_design_hit_eta_parameters
	std::string error_msg;
	error_msg[0] = NULL;
	int auto_err_code = 0;

	mc_rc_cycle.auto_opt_design_hit_eta(ms_rc_cycle_des_par, auto_err_code, error_msg);
	ms_des_solved.ms_rc_cycle_solved = *mc_rc_cycle.get_design_solved();

	if(auto_err_code != 0)
	{
		throw("sCO2 recompression cycle and CSP integration design method failed: ", error_msg);
	}

	if( error_msg[0] == NULL )
	{
		mc_messages.add_notice("The recompression cycle design optimization was successful");
	}
	else
	{
		string out_msg = "The sCO2 cycle design optimization solved with the following warning(s):\n" + error_msg;
		mc_messages.add_notice(out_msg);
	}
	//*************************************************************************************
	//*************************************************************************************

	// Set air cooler design parameters that are dependent on the cycle design solution
	ms_air_cooler_des_par_dep.m_T_hot_in_des = ms_des_solved.ms_rc_cycle_solved.m_temp[9-1];
	ms_air_cooler_des_par_dep.m_P_hot_in_des = ms_des_solved.ms_rc_cycle_solved.m_pres[9-1];
	ms_air_cooler_des_par_dep.m_m_dot_total = ms_des_solved.ms_rc_cycle_solved.m_m_dot_t;
		// This pressure drop is currently uncoupled from the cycle design
	ms_air_cooler_des_par_dep.m_delta_P_des = ms_des_par.m_deltaP_cooler_frac*ms_des_solved.ms_rc_cycle_solved.m_pres[2-1];
	ms_air_cooler_des_par_dep.m_T_hot_out_des = ms_des_solved.ms_rc_cycle_solved.m_temp[1-1];
	ms_air_cooler_des_par_dep.m_W_dot_fan_des = ms_des_par.m_frac_fan_power*ms_des_par.m_W_dot_net/1000.0;		//[MWe]

	// Initialize the PHX
	mc_phx.initialize(ms_des_par.m_hot_fl_code, ms_des_par.mc_hot_fl_props);

	// Design the PHX
	ms_phx_des_par.m_Q_dot_design = ms_des_solved.ms_rc_cycle_solved.m_W_dot_net / ms_des_solved.ms_rc_cycle_solved.m_eta_thermal;		//[kWt]
	ms_phx_des_par.m_T_h_in = ms_des_par.m_T_htf_hot_in;	//[K] HTF hot inlet temperature 
		// Okay, but CO2-HTF HX is assumed here. How does "structure inheritance" work?
	ms_phx_des_par.m_P_h_in = 1.0;							// Assuming HTF is incompressible...
	ms_phx_des_par.m_P_h_out = 1.0;						// Assuming HTF is incompressible...
		// .................................................................................
	ms_phx_des_par.m_T_c_in = ms_des_solved.ms_rc_cycle_solved.m_temp[5 - 1];		//[K]
	ms_phx_des_par.m_P_c_in = ms_des_solved.ms_rc_cycle_solved.m_pres[5 - 1];		//[K]
	ms_phx_des_par.m_P_c_out = ms_des_solved.ms_rc_cycle_solved.m_pres[6 - 1];		//[K]
	ms_phx_des_par.m_m_dot_cold_des = ms_des_solved.ms_rc_cycle_solved.m_m_dot_t;	//[kg/s]
		// Calculating the HTF mass flow rate in 'design_and_calc_m_dot_htf'
	ms_phx_des_par.m_m_dot_hot_des = std::numeric_limits<double>::quiet_NaN();
	
	mc_phx.design_and_calc_m_dot_htf(ms_phx_des_par, ms_des_par.m_phx_dt_cold_approach, ms_des_solved.ms_phx_des_solved);

	// Design the air cooler
		// Define Independent Air Cooler Design Parameters
	ms_air_cooler_des_par_ind.m_T_amb_des = ms_des_par.m_T_amb_des;		//[K]
	ms_air_cooler_des_par_ind.m_elev = ms_des_par.m_elevation;			//[m]
		// Add checks from Type 424 to the air cooler design code?
	mc_air_cooler.design_hx(ms_air_cooler_des_par_ind, ms_air_cooler_des_par_dep);

	return;
}

void C_sco2_recomp_csp::off_design(S_od_par od_par, int off_design_strategy)
{
	ms_od_par = od_par;

	if(off_design_strategy == FIX_T_MC_APPROACH__FLOAT_PHX_DT)
	{
		// Define ms_rc_cycle_od_par
			// Defined now
		ms_rc_cycle_od_par.m_T_mc_in = ms_od_par.m_T_amb + ms_des_par.m_dt_mc_approach;		//[K]
		ms_rc_cycle_od_par.m_N_sub_hxrs = ms_des_par.m_N_sub_hxrs;			//[-]
		ms_rc_cycle_od_par.m_tol = ms_des_par.m_tol;						//[-]
		ms_rc_cycle_od_par.m_N_t = ms_des_solved.ms_rc_cycle_solved.m_N_t;	//[rpm]
			// Defined downstream
		ms_rc_cycle_od_par.m_T_t_in = std::numeric_limits<double>::quiet_NaN();			//[K]
		ms_rc_cycle_od_par.m_P_mc_in = std::numeric_limits<double>::quiet_NaN();		//[kPa]
		ms_rc_cycle_od_par.m_recomp_frac = std::numeric_limits<double>::quiet_NaN();	//[-]
		ms_rc_cycle_od_par.m_N_mc = std::numeric_limits<double>::quiet_NaN();			//[rpm]

		// Define ms_phx_od_par
			// Defined now
		ms_phx_od_par.m_T_h_in = ms_od_par.m_T_htf_hot;			//[K]
		ms_phx_od_par.m_P_h_in = ms_phx_des_par.m_P_h_in;		//[kPa] Assuming fluid is incompressible in that pressure doesn't affect its properties
		ms_phx_od_par.m_m_dot_h = ms_od_par.m_m_dot_htf;		//[kg/s]
			// Defined downstream
		ms_phx_od_par.m_T_c_in = std::numeric_limits<double>::quiet_NaN();		//[K]
		ms_phx_od_par.m_P_c_in = std::numeric_limits<double>::quiet_NaN();		//[kPa]
		ms_phx_od_par.m_m_dot_c = std::numeric_limits<double>::quiet_NaN();		//[kg/s]
		
		int opt_eta_code = od_fix_T_mc__float_phx_dt__opt_eta();

		return;
	}
	else
	{
		std::string msg = util::format("Off design strategy, %d, is not an available option", off_design_strategy);
		throw(C_csp_exception("C_sco2_recomp_csp::off_design", msg));
	}

}

int C_sco2_recomp_csp::od_fix_T_mc__float_phx_dt__opt_eta()
{
	// Set up 3 variable optimization in NLOPT
	std::vector<double> x(0);
	std::vector<double> lb(0);
	std::vector<double> ub(0);
	std::vector<double> scale(0);
	int index = 0;

	// Inlet pressure
	// Might think of ways to generate better values here...
	x.push_back(ms_des_solved.ms_rc_cycle_solved.m_pres[1-1]);
	lb.push_back(1000.0);	
	ub.push_back(17000.0);		
	scale.push_back(-0.1*ms_des_solved.ms_rc_cycle_solved.m_pres[1-1]);	// Solution is probably less than design pressure
	index++;

	// Recompression Fraction
	if( ms_des_solved.ms_rc_cycle_solved.m_is_rc )
	{
		x.push_back(ms_des_solved.ms_rc_cycle_solved.m_recomp_frac);
		lb.push_back(0.0);
		ub.push_back(1.0);
		scale.push_back(-.02);
		index++;
	}

	// Reset optimization tracking structure
	ms_od_opt_eta_tracking.m_is_opt_found = false;
	ms_od_opt_eta_tracking.m_eta_max = 0.0;
	ms_od_opt_eta_tracking.m_over_T_t_in_at_eta_max = std::numeric_limits<double>::quiet_NaN();
	ms_od_opt_eta_tracking.m_over_P_high_at_eta_max = std::numeric_limits<double>::quiet_NaN();

	// Compressor Speed
	x.push_back(ms_des_solved.ms_rc_cycle_solved.m_N_mc);
	lb.push_back(ms_des_solved.ms_rc_cycle_solved.m_N_mc*0.1);
	ub.push_back(ms_des_solved.ms_rc_cycle_solved.m_N_mc*1.5);
	scale.push_back(ms_des_solved.ms_rc_cycle_solved.m_N_mc*0.1);
	index++;

	// Save initial vectors
	std::vector<double> x_base = x;
	std::vector<double> lb_base = lb;
	std::vector<double> ub_base = ub;
	std::vector<double> sc_base = scale;

	// Set up instance of nlopt class and set optimization parameters
	nlopt::opt          opt_od_eta(nlopt::LN_SBPLX, index);
	opt_od_eta.set_lower_bounds(lb);
	opt_od_eta.set_upper_bounds(ub);
	opt_od_eta.set_initial_step(scale);
	opt_od_eta.set_xtol_rel(ms_rc_cycle_des_par.m_tol);

	// Set max objective function
	opt_od_eta.set_max_objective(nlopt_cb_opt_od_eta__float_phx_dt, this);
	double max_f = std::numeric_limits<double>::quiet_NaN();
	nlopt::result          result_od_cycle = opt_od_eta.optimize(x, max_f);

	if( !ms_od_opt_eta_tracking.m_is_opt_found )
	{
		// For now, let's just catch this...

		throw(C_csp_exception("C_sco2_recomp_csp::off_design_fix_T_mc__float_phx_dt__opt_eta","Cycle optimization did not succeed"));
	
		// But, eventually, can probably trying modifying guesses and calling NLOPT again, like in C_RecompCycle::opt_od_eta_for_hx
	}

	// Call a final time with optimized parameters
	double eta_opt = od_fix_T_mc_approach__float_phx_dt(x);

	if(eta_opt > 0.0)
	{
		return 0;
	}
	else
	{
		return -1;
	}

}

double C_sco2_recomp_csp::od_fix_T_mc_approach__float_phx_dt(const std::vector<double> &x)
{
	// This method solves for the off-design performance of the cycle integrated with the PHX and HTF stream
	// x includes main compressor inlet pressure, recompression fraction, and main compressor speed
	// Other off-design parameters should be already stored in ms_od_par:
	//		m_T_htf_hot
	//      m_m_dot_htf
	// ... or in ms_rc_cycle_od_par
	//	    m_T_mc_in
	//	    m_N_sub_hxrs
	//	    m_tol
	//	    m_N_t

	int index = 0;

	ms_rc_cycle_od_par.m_P_mc_in = x[index];		//[kPa]
	index++;

	ms_rc_cycle_od_par.m_recomp_frac = 0.0;
	if( ms_des_solved.ms_rc_cycle_solved.m_is_rc )
	{
		ms_rc_cycle_od_par.m_recomp_frac = x[index];
		index++;
	}

	ms_rc_cycle_od_par.m_N_mc = x[index];
	index++;

	// Apply 1 var solver to find the turbine inlet temperature that results in a "converged" PHX
	C_mono_eq_T_t_in c_phx_cycle(this);
	C_monotonic_eq_solver c_phx_cycle_solver(c_phx_cycle);

	// Set upper and lower bounds
	double T_t_upper = ms_phx_od_par.m_T_h_in;		//[K] Upper CO2 limit is HTF hot temperature
	double T_t_lower = 373.15;						//[K] Lower CO2 limit is something fairly low, I guess

	// Generate guess values
	double T_t_guess_upper = ms_phx_od_par.m_T_h_in - ms_des_par.m_phx_dt_hot_approach;	//[K] One reasonable guess might be to apply the design approach
	double T_t_guess_lower = T_t_guess_upper - 20.0;		//[K] This might be another reasonable guess...

	// Set solver settings
	// Because this application of solver is trying to get outlet to match guess, need to calculate error in function
	// So its already relative, and solver is looking at an absolute value
	c_phx_cycle_solver.settings(ms_des_par.m_tol, 50, T_t_lower, T_t_upper, false);

	// Now, solve for the turbine inlet temperature
	double T_t_solved, tol_solved;
	T_t_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int phx_cycle_code = c_phx_cycle_solver.solve(T_t_guess_lower, T_t_guess_upper, 0.0, T_t_solved, tol_solved, iter_solved);
	
	if( phx_cycle_code != C_monotonic_eq_solver::CONVERGED )
	{
		return 0.0;
	}

	// Now, need to filter results that exceed temperature/pressure/other limitations
	// 1) Don't let the turbine inlet temperature exceed the design inlet temperature
	double over_T_t_in = max(0.0, T_t_solved - mc_rc_cycle.get_design_solved()->m_temp[6-1]);

	// 2) Don't let the upper pressure in the system exceed the specified max (typically also = design point P_high)
	double over_P_high = max(0.0, mc_rc_cycle.get_od_solved()->m_pres[2-1] - ms_des_par.m_P_high_limit);

	// Want thermal efficiency gradient, not step change, as turbine inlet temperature exceeds design
	// ... to help the solver
	double eta_T_t_in_scale = exp(-over_T_t_in);
	double eta_P_high_scale = exp(-over_P_high);
	double eta_solved = mc_rc_cycle.get_od_solved()->m_eta_thermal*eta_T_t_in_scale*eta_P_high_scale;

	// BUT, need to inform upstream code that a solution in this gradient is not acceptable
	if( over_T_t_in == 0.0 && over_P_high == 0.0 )
	{
		ms_od_opt_eta_tracking.m_is_opt_found = true;
	}

	if( eta_solved > ms_od_opt_eta_tracking.m_eta_max )
	{
		if( over_T_t_in > 0.0 || over_P_high > 0.0 )
		{	// Scaled eta could still be the current max that NLOPT sees, so need to communicate upstream that it's breaking limits
			ms_od_opt_eta_tracking.m_is_opt_found = false;
		}
		ms_od_opt_eta_tracking.m_eta_max = eta_solved;
		ms_od_opt_eta_tracking.m_over_T_t_in_at_eta_max = over_T_t_in;
		ms_od_opt_eta_tracking.m_over_P_high_at_eta_max = over_P_high;
	}

	return eta_solved;
}

int C_sco2_recomp_csp::C_mono_eq_T_t_in::operator()(double T_t_in /*K*/, double *diff_T_t_in /*-*/)
{
	// Using:
	//	-mc_rc_cycle
	//	-ms_rc_cycle_od_par
	//	-ms_phx_od_par

	// 1) Update Turbine Inlet Temperature in sco2 cycle off design parameter structure
	mpc_sco2_rc->ms_rc_cycle_od_par.m_T_t_in = T_t_in;

	// 2) Solve the off-design cycle model with off design parameter structure
	int rc_od_error_code = 0;
	mpc_sco2_rc->mc_rc_cycle.off_design(mpc_sco2_rc->ms_rc_cycle_od_par, rc_od_error_code);

	// If off-design cycle model did not solve, return to solver
	if( rc_od_error_code != 0 )
		return -1;

	// Solve PHX heat exchanger performance using CO2 and HTF *inlet* conditions
	mpc_sco2_rc->ms_phx_od_par.m_T_c_in = mpc_sco2_rc->mc_rc_cycle.get_od_solved()->m_temp[5-1];	//[K]
	mpc_sco2_rc->ms_phx_od_par.m_P_c_in = mpc_sco2_rc->mc_rc_cycle.get_od_solved()->m_pres[5-1];	//[kPa]
	mpc_sco2_rc->ms_phx_od_par.m_m_dot_c = mpc_sco2_rc->mc_rc_cycle.get_od_solved()->m_m_dot_t;		//[kg/s]
	double q_dot, T_co2_phx_out, T_htf_cold;
	q_dot = T_co2_phx_out = T_htf_cold = std::numeric_limits<double>::quiet_NaN();
	
	// Solves HX performance. 
	// If successful, this call updates 'ms_od_solved'
	try
	{
		mpc_sco2_rc->mc_phx.od_performance(mpc_sco2_rc->ms_phx_od_par.m_T_c_in, mpc_sco2_rc->ms_phx_od_par.m_P_c_in, mpc_sco2_rc->ms_phx_od_par.m_m_dot_c,
			mpc_sco2_rc->ms_phx_od_par.m_T_h_in, mpc_sco2_rc->ms_phx_od_par.m_P_h_in, mpc_sco2_rc->ms_phx_od_par.m_m_dot_h,
			q_dot, T_co2_phx_out, T_htf_cold);
	}						
	catch( C_csp_exception )
	{
		// reset 'diff_T_t_in' to NaN
		*diff_T_t_in = std::numeric_limits<double>::quiet_NaN();
		
		return -1;
	}
	
	*diff_T_t_in = (T_co2_phx_out - T_t_in) / T_t_in;
	return 0;
}

double nlopt_cb_opt_od_eta__float_phx_dt(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_sco2_recomp_csp *frame = static_cast<C_sco2_recomp_csp*>(data);
	if( frame != NULL ) return frame->od_fix_T_mc_approach__float_phx_dt(x);
}