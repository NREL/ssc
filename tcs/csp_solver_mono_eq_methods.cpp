#include "csp_solver_core.h"
#include "numeric_solvers.h"


int C_csp_solver::C_mono_eq_T_cr_in_from_pc::operator()(double T_cr_in /*K*/, double *diff_T_cr_in /*-*/)
{
	return -1;
}