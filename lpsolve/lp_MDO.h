/********************************************************************************************************************************************

Note
-------------
The version of lp_solve included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The lp_solve specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/lpsolve

The original version of lp_solve can be found at https://sourceforge.net/projects/lpsolve/

********************************************************************************************************************************************/
#ifndef HEADER_MDO
#define HEADER_MDO

#include "lp_types.h"


#ifdef __cplusplus
extern "C" {
#endif

int __WINAPI getMDO(lprec *lp, MYBOOL *usedpos, int *colorder, int *size, MYBOOL symmetric);

#ifdef __cplusplus
 }
#endif

#endif /* HEADER_MDO */

