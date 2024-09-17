/********************************************************************************************************************************************

Note
-------------
The version of lp_solve included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The lp_solve specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/lpsolve

The original version of lp_solve can be found at https://sourceforge.net/projects/lpsolve/

********************************************************************************************************************************************/

#ifndef HEADER_lp_crash
#define HEADER_lp_crash


#include "lp_types.h"

#define CRASH_SIMPLESCALE       /* Specify if we should use a simple absolute scaling threshold */

#define CRASH_THRESHOLD  0.167
#define CRASH_SPACER        10
#define CRASH_WEIGHT     0.500



#ifdef __cplusplus
__EXTERN_C {
#endif

STATIC MYBOOL crash_basis(lprec *lp);

#ifdef __cplusplus
}
#endif

#endif /* HEADER_lp_crash */

