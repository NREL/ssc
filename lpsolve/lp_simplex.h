/********************************************************************************************************************************************

Note
-------------
The version of lp_solve included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The lp_solve specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/lpsolve

The original version of lp_solve can be found at https://sourceforge.net/projects/lpsolve/

********************************************************************************************************************************************/
#ifndef HEADER_lp_simplex
#define HEADER_lp_simplex

#include "lp_types.h"

#define ForceDualSimplexInBB               /* Force use/switch of dual simplex in B&B */
#define AssumeHighAccuracyInBB    /* No iteration of simplex solves at infeasibility */
/*#define UseLongStepPruning*/
/*#define UseLongStepDualPhase1*/
#define primal_UseRejectionList
#define dual_UseRejectionList
#define dual_RemoveBasicFixedVars
/*#define dual_Phase1PriceEqualities */   /* Force elimination of equality slacks */
#define AcceptMarginalAccuracy

#ifdef __cplusplus
extern "C" {
#endif

/* Put function headers here */
STATIC int primloop(lprec *lp, MYBOOL primalfeasible, REAL primaloffset);
STATIC int dualloop(lprec *lp, MYBOOL dualfeasible, int dualinfeasibles[], REAL dualoffset);
STATIC int spx_run(lprec *lp, MYBOOL validInvB);
STATIC int spx_solve(lprec *lp);
STATIC int lag_solve(lprec *lp, REAL start_bound, int num_iter);
STATIC int heuristics(lprec *lp, int mode);
STATIC int lin_solve(lprec *lp);

#ifdef __cplusplus
 }
#endif

#endif /* HEADER_lp_simplex */

