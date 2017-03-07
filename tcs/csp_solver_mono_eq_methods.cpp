#include "csp_solver_core.h"
#include "numeric_solvers.h"


int C_csp_solver::C_mono_eq_T_cr_in__pc_su_controlled::operator()(double T_cr_in /*C*/, double *diff_T_cr_in /*-*/)
{
	// Solve the receiver model with T_rec_in_guess and current defocus guess
	mpc_csp_solver->mc_cr_htf_state_in.m_temp = T_cr_in;	//[C]



	return -1;
}