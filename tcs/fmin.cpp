/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/
/*
************************************************************************
*	    		    C math library
* function FMINBR - one-dimensional search for a function minimum
*			  over the given range
*
* Input
*	double fminbr(a,b,f,tol)
*	double a; 			Minimum will be seeked for over
*	double b;  			a range [a,b], a being < b.
*	double (*f)(double x);		Name of the function whose minimum
*					will be seeked for
*	double tol;			Acceptable tolerance for the minimum
*					location. It have to be positive
*					(e.g. may be specified as EPSILON)
*
* Output
*	Fminbr returns an estimate for the minimum location with accuracy
*	3*SQRT_EPSILON*abs(x) + tol.
*	The function always obtains a local minimum which coincides with
*	the global one only if a function under investigation being
*	unimodular.
*	If a function being examined possesses no local minimum within
*	the given range, Fminbr returns 'a' (if f(a) < f(b)), otherwise
*	it returns the right range boundary value b.
*
* Algorithm
*	G.Forsythe, M.Malcolm, C.Moler, Computer methods for mathematical
*	computations. M., Mir, 1980, p.202 of the Russian edition
*
*	The function makes use of the "gold section" procedure combined with
*	the parabolic interpolation.
*	At every step program operates three abscissae - x,v, and w.
*	x - the last and the best approximation to the minimum location,
*	    i.e. f(x) <= f(a) or/and f(x) <= f(b)
*	    (if the function f has a local minimum in (a,b), then the both
*	    conditions are fulfiled after one or two steps).
*	v,w are previous approximations to the minimum location. They may
*	coincide with a, b, or x (although the algorithm tries to make all
*	u, v, and w distinct). Points x, v, and w are used to construct
*	interpolating parabola whose minimum will be treated as a new
*	approximation to the minimum location if the former falls within
*	[a,b] and reduces the range enveloping minimum more efficient than
*	the gold section procedure.
*	When f(x) has a second derivative positive at the minimum location
*	(not coinciding with a or b) the procedure converges superlinearly
*	at a rate order about 1.324
*
************************************************************************
*/

#include "assert.h"
#include "math.h"

#include "fmin.h"

#define EPSILON       2.22045e-16
#define SQRT_EPSILON  1.49012e-08

double fminbr(double a, double b, double(*f)(double x, void *data), void *data_in, double tol)
{
	//	return (*f)(y);
	//}
	//
	//double fminbr(a, b, f, tol)     /* An estimate to the min location */
	//double a;                       /* Left border | of the range   */
	//double b;                       /* Right border| the min is seeked */
	//double (*f) (double x);         /* Function under investigation */
	//double tol;                     /* Acceptable tolerance         */
	//{
	double x, v, w;              /* Abscissae, descr. see above  */
	double fx;                   /* f(x)                         */
	double fv;                   /* f(v)                         */
	double fw;                   /* f(w)                         */
	const double r = (3. - sqrt(5.0)) / 2;       /* Gold section ratio           */

	assert(tol > 0 && b > a);

	v = a + r * (b - a);
	fv = (*f) (v, data_in);               /* First step - always gold section */
	x = v;
	w = v;
	fx = fv;
	fw = fv;

	for( ;; ) {                   /* Main iteration loop    */
		double range = b - a;     /* Range over which the minimum */
		/* is seeked for                */
		double middle_range = (a + b) / 2;
		double tol_act =          /* Actual tolerance             */
			SQRT_EPSILON * fabs(x) + tol / 3;
		double new_step;          /* Step at this iteration       */



		if( fabs(x - middle_range) + range / 2 <= 2 * tol_act )
			return x;              /* Acceptable approx. is found  */

		/* Obtain the gold section step */
		new_step = r * (x < middle_range ? b - x : a - x);


		/* Decide if the interpolation can be tried     */
		if( fabs(x - w) >= tol_act ) {     /* If x and w are distinct      *//* interpolatiom may be tried   */
			register double p;     /* Interpolation step is calcula- */
			register double q;     /* ted as p/q; division operation */
			/* is delayed until last moment */
			register double t;

			t = (x - w) * (fx - fv);
			q = (x - v) * (fx - fw);
			p = (x - v) * q - (x - w) * t;
			q = 2 * (q - t);

			if( q > (double)0 )    /* q was calculated with the op- */
				p = -p;             /* posite sign; make q positive */
			else                   /* and assign possible minus to     */
				q = -q;             /* p                            */

			if( fabs(p) < fabs(new_step * q) &&    /* If x+p/q falls in [a,b] */
				p > q * (a - x + 2 * tol_act) &&   /* not too close to a and */
				p < q * (b - x - 2 * tol_act) )     /* b, and isn't too large */
				new_step = p / q;   /* it is accepted         */
			/* If p/q is too large then the */
			/* gold section procedure can   */
			/* reduce [a,b] range to more   */
			/* extent                       */
		}

		if( fabs(new_step) < tol_act ) {   /* Adjust the step to be not less */
			if( new_step > (double)0 ) {   /* than tolerance               */
				new_step = tol_act;
			}
			else {
				new_step = -tol_act;
			}
		}                         /* Obtain the next approximation to min     */
		{                         /* and reduce the enveloping range      */
			register double t = x + new_step;      /* Tentative point for the min  */
			register double ft = (*f) (t,data_in);
			if( ft <= fx ) {        /* t is a better approximation  */
				if( t < x )          /* Reduce the range so that */
					b = x;           /* t would fall within it       */
				else
					a = x;

				v = w;
				w = x;
				x = t;              /* Assign the best approx to x  */
				fv = fw;
				fw = fx;
				fx = ft;
			}
			else {               /* x remains the better approx  */

				if( t < x )          /* Reduce the range enclosing x     */
					a = t;
				else
					b = t;

				if( ft <= fw || w == x ) {
					v = w;
					w = t;
					fv = fw;
					fw = ft;
				}
				else if( ft <= fv || v == x || v == w ) {
					v = t;
					fv = ft;
				}
			}

		}                         /* ----- end-of-block ----- */
	}                            /* ===== End of loop ===== */

}
