/********************************************************************************************************************************************

Note
-------------
The version of nlopt included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The nlopt specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/nlopt

The original version of nlopt can be found at https://github.com/stevengj/nlopt

********************************************************************************************************************************************/
#ifndef BOBYQA_H
#define BOBYQA_H 1

#include "nlopt-util.h"
#include "nlopt.h"

extern nlopt_result bobyqa(int n, int npt, double *x, 
			   const double *lb, const double *ub,
			   const double *dx, 
			   nlopt_stopping *stop, double *minf,
			   nlopt_func f, void *f_data);

#endif /* BOBYQA_H */
