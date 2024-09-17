/********************************************************************************************************************************************

Note
-------------
The version of nlopt included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The nlopt specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/nlopt

The original version of nlopt can be found at https://github.com/stevengj/nlopt

********************************************************************************************************************************************/
#ifndef SLSQP_H
#define SLSQP_H

#include "nlopt.h"
#include "nlopt-util.h"

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

nlopt_result nlopt_slsqp(unsigned n, nlopt_func f, void *f_data,
			 unsigned m, nlopt_constraint *fc,
			 unsigned p, nlopt_constraint *h,
			 const double *lb, const double *ub,
			 double *x, double *minf,
			 nlopt_stopping *stop);
#ifdef __cplusplus
}  /* extern "C" */
#endif /* __cplusplus */

#endif
