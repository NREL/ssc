/********************************************************************************************************************************************

Note
-------------
The version of nlopt included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The nlopt specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/nlopt

The original version of nlopt can be found at https://github.com/stevengj/nlopt

********************************************************************************************************************************************/
/* Copyright (c) 2007-2012 Massachusetts Institute of Technology
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
 */

#ifndef CRS_H
#define CRS_H

#include "nlopt.h"
#include "nlopt-util.h"

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

nlopt_result crs_minimize(int n, nlopt_func f, void *f_data,
			  const double *lb, const double *ub, /* bounds */
			  double *x, /* in: initial guess, out: minimizer */
			  double *minf,
			  nlopt_stopping *stop,
			  int population, /* initial population (0=default) */
			  int random); /* random or low-discrepancy seq. */

#ifdef __cplusplus
}  /* extern "C" */
#endif /* __cplusplus */

#endif

