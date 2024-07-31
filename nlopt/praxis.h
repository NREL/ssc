/********************************************************************************************************************************************

Note
-------------
The version of nlopt included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The nlopt specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/nlopt

The original version of nlopt can be found at https://github.com/stevengj/nlopt

********************************************************************************************************************************************/
#ifndef PRAXIS_H
#define PRAXIS_H

#include "nlopt-util.h"
#include "nlopt.h"

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

typedef double (*praxis_func)(int n, const double *x, void *f_data);

nlopt_result praxis_(double t0, double machep, double h0,
		     int n, double *x, praxis_func f, void *f_data, 
		     nlopt_stopping *stop, double *minf);

#ifdef __cplusplus
}  /* extern "C" */
#endif /* __cplusplus */

#endif /* PRAXIS_H */
