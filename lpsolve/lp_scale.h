/********************************************************************************************************************************************

Note
-------------
The version of lp_solve included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The lp_solve specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/lpsolve

The original version of lp_solve can be found at https://sourceforge.net/projects/lpsolve/

********************************************************************************************************************************************/
#ifndef HEADER_lp_scale
#define HEADER_lp_scale

#include "lp_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Put function headers here */
STATIC MYBOOL scale_updatecolumns(lprec *lp, REAL *scalechange, MYBOOL updateonly);
STATIC MYBOOL scale_updaterows(lprec *lp, REAL *scalechange, MYBOOL updateonly);
STATIC MYBOOL scale_rows(lprec *lp, REAL *scaledelta);
STATIC MYBOOL scale_columns(lprec *lp, REAL *scaledelta);
STATIC void unscale_columns(lprec *lp);
STATIC REAL scale(lprec *lp, REAL *scaledelta);
STATIC REAL scaled_mat(lprec *lp, REAL value, int rownr, int colnr);
STATIC REAL unscaled_mat(lprec *lp, REAL value, int rownr, int colnr);
STATIC REAL scaled_value(lprec *lp, REAL value, int index);
STATIC REAL unscaled_value(lprec *lp, REAL value, int index);
STATIC MYBOOL scaleCR(lprec *lp, REAL *scaledelta);
STATIC MYBOOL finalize_scaling(lprec *lp, REAL *scaledelta);
STATIC REAL auto_scale(lprec *lp);
void undoscale(lprec *lp);

#ifdef __cplusplus
 }
#endif

#endif /* HEADER_lp_scale */

