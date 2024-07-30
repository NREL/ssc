/********************************************************************************************************************************************

Note
-------------
The version of lp_solve included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The lp_solve specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/lpsolve

The original version of lp_solve can be found at https://sourceforge.net/projects/lpsolve/

********************************************************************************************************************************************/
#ifndef HEADER_lp_lp
#define HEADER_lp_lp

#include "lp_types.h"


#ifdef __cplusplus
extern "C" {
#endif

/* Put function headers here */
MYBOOL LP_writefile(lprec *lp, char *filename);
MYBOOL LP_writehandle(lprec *lp, FILE *output);


#ifdef __cplusplus
 }
#endif

#endif /* HEADER_lp_lp */

