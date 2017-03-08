#include "csp_solver_core.h"
#include "numeric_solvers.h"


int C_csp_solver::C_mono_eq_T_cr_in__pc_su_controlled::operator()(double T_cr_in /*C*/, double *diff_T_cr_in /*-*/)
{
	// Solve the receiver model with T_rec_in_guess and current defocus guess
	mpc_csp_solver->mc_cr_htf_state_in.m_temp = T_cr_in;	//[C]



	return -1;
}

int C_csp_solver::C_mono_eq_cr_df__pc_max__tes_off::operator()(double defocus /*-*/, double *y_constrain)
{
	// Use defocus_guess and call method to solve CR-PC iteration
	double cr_pc_exit_tol = std::numeric_limits<double>::quiet_NaN();
	int cr_pc_exit_mode = -1;
	mpc_csp_solver->solver_cr_to_pc_to_cr(defocus, 1.E-3, cr_pc_exit_mode, cr_pc_exit_tol);

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
		*y_constrain = (mpc_csp_solver->mc_cr_out_solver.m_m_dot_salt_tot - mpc_csp_solver->m_m_dot_pc_max) / mpc_csp_solver->m_m_dot_pc_max;
	}
	return 0;
}