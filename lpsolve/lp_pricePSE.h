/********************************************************************************************************************************************

Note
-------------
The version of lp_solve included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The lp_solve specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/lpsolve

The original version of lp_solve can be found at https://sourceforge.net/projects/lpsolve/

********************************************************************************************************************************************/
#ifndef HEADER_lp_pricePSE
#define HEADER_lp_pricePSE

#include "lp_types.h"

#define ApplySteepestEdgeMinimum

#ifdef __cplusplus
extern "C" {
#endif

/* Price norm management routines */
STATIC MYBOOL initPricer(lprec *lp);
INLINE MYBOOL applyPricer(lprec *lp);
STATIC void simplexPricer(lprec *lp, MYBOOL isdual);
STATIC void freePricer(lprec *lp);
STATIC MYBOOL resizePricer(lprec *lp);
STATIC REAL getPricer(lprec *lp, int item, MYBOOL isdual);
STATIC MYBOOL restartPricer(lprec *lp, MYBOOL isdual);
STATIC MYBOOL updatePricer(lprec *lp, int rownr, int colnr, REAL *pcol, REAL *prow, int *nzprow);
STATIC MYBOOL verifyPricer(lprec *lp);

#ifdef __cplusplus
 }
#endif

#endif /* HEADER_lp_pricePSE */

