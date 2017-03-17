#include "csp_solver_core.h"
#include "numeric_solvers.h"
#include <math.h>

#include "lib_util.h"

int C_csp_solver::C_mono_eq_cr_df__pc_max__tes_off::operator()(double defocus /*-*/, double *y_constrain)
{
	// Use defocus_guess and call method to solve CR-PC iteration
	double cr_pc_exit_tol = std::numeric_limits<double>::quiet_NaN();
	int cr_pc_exit_mode = -1;
	mpc_csp_solver->solver_cr_to_pc_to_cr(m_pc_mode, defocus, 1.E-3, cr_pc_exit_mode, cr_pc_exit_tol);

	if (cr_pc_exit_mode != C_csp_solver::CSP_CONVERGED)
	{
		*y_constrain = std::numeric_limits<double>::quiet_NaN();
		return -1;
	}

	if (m_is_df_q_dot)
	{
		*y_constrain = (mpc_csp_solver->mc_cr_out_solver.m_q_thermal - m_q_dot_max) / m_q_dot_max;
	}
	else
	{
		if (m_pc_mode == C_csp_power_cycle::ON)
		{
			*y_constrain = (mpc_csp_solver->mc_cr_out_solver.m_m_dot_salt_tot - mpc_csp_solver->m_m_dot_pc_max) / mpc_csp_solver->m_m_dot_pc_max;
		}
		else if (m_pc_mode == C_csp_power_cycle::STARTUP_CONTROLLED)
		{
			*y_constrain = (mpc_csp_solver->mc_cr_out_solver.m_m_dot_salt_tot - mpc_csp_solver->mc_pc_out_solver.m_m_dot_htf) / mpc_csp_solver->mc_pc_out_solver.m_m_dot_htf;
		}
	}
	return 0;
}

int C_csp_solver::C_mono_eq_cr_to_pc_to_cr::operator()(double T_htf_cold /*C*/, double *diff_T_htf_cold /*-*/)
{
	// Solve the receiver model
	mpc_csp_solver->mc_cr_htf_state_in.m_temp = T_htf_cold;		//[C]
	mpc_csp_solver->mc_cr_htf_state_in.m_pres = m_P_field_in;	//[kPa]
	mpc_csp_solver->mc_cr_htf_state_in.m_qual = m_x_field_in;	//[-]
	
	mpc_csp_solver->mc_collector_receiver.on(mpc_csp_solver->mc_weather.ms_outputs,
						mpc_csp_solver->mc_cr_htf_state_in,
						m_field_control_in,
						mpc_csp_solver->mc_cr_out_solver,
						mpc_csp_solver->mc_kernel.mc_sim_info);

	if (mpc_csp_solver->mc_cr_out_solver.m_m_dot_salt_tot == 0.0 || mpc_csp_solver->mc_cr_out_solver.m_q_thermal == 0.0)
	{
		*diff_T_htf_cold = std::numeric_limits<double>::quiet_NaN();
		return -1;
	}

	// Solve the power cycle model using receiver outputs
		// Inlet State
	mpc_csp_solver->mc_pc_htf_state_in.m_temp = mpc_csp_solver->mc_cr_out_solver.m_T_salt_hot;	//[C]
	mpc_csp_solver->mc_pc_htf_state_in.m_pres = mpc_csp_solver->mc_cr_out_solver.m_P_htf_hot;	//[kPa]
	mpc_csp_solver->mc_pc_htf_state_in.m_qual = mpc_csp_solver->mc_cr_out_solver.m_xb_htf_hot;	//[-]

	// For now, check the CR return pressure against the assumed constant system interface pressure
	if (fabs((mpc_csp_solver->mc_cr_out_solver.m_P_htf_hot - m_P_field_in) / m_P_field_in) > 0.001 && !mpc_csp_solver->mc_collector_receiver.m_is_sensible_htf)
	{
		std::string msg = util::format("C_csp_solver::solver_cr_to_pc_to_cr(...) The pressure returned from the CR model, %lg [bar],"
			" is different than the assumed constant pressure, %lg [bar]",
			mpc_csp_solver->mc_cr_out_solver.m_P_htf_hot / 100.0, m_P_field_in / 100.0);
		mpc_csp_solver->mc_csp_messages.add_message(C_csp_messages::NOTICE, msg);
	}

	mpc_csp_solver->mc_pc_inputs.m_m_dot = mpc_csp_solver->mc_cr_out_solver.m_m_dot_salt_tot;	//[kg/hr]
	mpc_csp_solver->mc_pc_inputs.m_standby_control = m_pc_mode;		//[-]

	mpc_csp_solver->mc_power_cycle.call(mpc_csp_solver->mc_weather.ms_outputs,
						mpc_csp_solver->mc_pc_htf_state_in,
						mpc_csp_solver->mc_pc_inputs,
						mpc_csp_solver->mc_pc_out_solver,
						mpc_csp_solver->mc_kernel.mc_sim_info);

	if (!mpc_csp_solver->mc_pc_out_solver.m_was_method_successful)
	{
		*diff_T_htf_cold = std::numeric_limits<double>::quiet_NaN();
		return -2;
	}

	*diff_T_htf_cold = (mpc_csp_solver->mc_pc_out_solver.m_T_htf_cold - T_htf_cold) / T_htf_cold;

	return 0;
}
