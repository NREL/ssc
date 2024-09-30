/********************************************************************************************************************************************

Note
-------------
The version of nlopt included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The nlopt specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/nlopt

The original version of nlopt can be found at https://github.com/stevengj/nlopt

********************************************************************************************************************************************/
#ifndef NEWUOA_H
#define NEWUOA_H 1

#include "nlopt-util.h"
#include "nlopt.h"

typedef double (*newuoa_func)(int n, const double *x, void *func_data);

extern nlopt_result newuoa(int n, int npt, double *x, 
			   const double *lb, const double *ub,
			   double rhobeg, nlopt_stopping *stop, double *minf,
			   newuoa_func calfun, void *calfun_data);

#endif /* NEWUOA_H */
