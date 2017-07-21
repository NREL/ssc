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
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <vector>
#include "nlopt.hpp"

using namespace std;

class testoptclass
{
	int call_count;
public:
	testoptclass();

	void reset_counter();

	void random_start(vector<double> &x, vector<vector<double> > &range);

	double memfunc(unsigned n, const double *x, double *grad, void *my_func_data);
	
	double styb_tang_test(unsigned n, const double *x, double *grad, void *data);

	double rosenbrock_test(unsigned n, const double *x, double *grad, void *data);

	double matyas_test(unsigned n, const double *x, double *grad, void *data);

	int get_call_count();
};
//
//double nlopt_callback(unsigned n, const double *x, double *grad, void *data){
//	testoptclass *frame = static_cast<testoptclass*>( data );
//	if(frame != NULL) return frame->styb_tang_test(n, x, grad, data);
//}
//
//
//void test_optimization(){
//	int ndim = 4;
//
//	/* 
//	GN_ORIG_DIRECT 
//	GN_ORIG_DIRECT_L  1
//	GN_DIRECT
//	GN_DIRECT_L
//	GN_CRS2_LM
//	GN_ISRES
//	GN_ESCH
//	LN_COBYLA
//	LN_BOBYQA
//	LN_PRAXIS
//	LN_NELDERMEAD 1
//	LN_SBPLX
//	*/
//
//	nlopt::opt opt(nlopt::GN_ORIG_DIRECT_L, ndim); /* Algorithm and dimension */
//	
//
//	vector<double> 
//		ub(ndim),
//		lb(ndim);
//	for(int i=0; i<ndim; i++){
//		ub.at(i) = 10.;
//		lb.at(i) = -10.;
//	}
//	opt.set_lower_bounds(lb);
//	opt.set_upper_bounds(ub);
//
//	testoptclass P;
//		
//	opt.set_min_objective(nlopt_callback, &P);// NULL);
//	//opt.set_min_objective(myfunc, NULL);
//
//	/*my_constraint_data data[2] = { {2,0}, {-1,1} };
//	opt.add_inequality_constraint(myconstraint, &data[0], 1e-8);
//	opt.add_inequality_constraint(myconstraint, &data[1], 1e-8);*/
//
//	opt.set_xtol_rel(.001);
//	opt.set_xtol_abs(.001);
//	
//	std::vector<double> x(ndim);
//	vector<vector<double> > xrange;
//	for(int i=0; i<ndim; i++){
//		vector<double> trange(2);
//		trange.at(0) = lb.at(i);
//		trange.at(1) = ub.at(i);
//		xrange.push_back(trange);
//	}
//	double minf;
//
//	int nstart = 20;
//
//	vector<vector<double> > xopts;
//	vector<double> fopts;
//	vector<int> ncalls;
//
//	double fminall=9e9;
//	int iminall=0;
//	int countall=0;
//
//	for(int i=0; i<nstart; i++){
//		P.reset_counter();
//
//		P.random_start(x, xrange);
//		
//		nlopt::result result = opt.optimize(x, minf);
//
//		int ncall = P.get_call_count();
//
//		xopts.push_back(x);
//		fopts.push_back(minf);
//		ncalls.push_back(ncall);
//
//		countall += ncall;
//
//		//keep track of overall min
//		if(minf < fminall){
//			fminall=minf;
//			iminall = i;
//		}
//	}
//
//	fminall *= 1./(double)ndim;
//	double *xstar = new double[ndim];
//	for(int i=0; i<ndim; i++){
//		xstar[i] = xopts.at(iminall).at(i);
//	}
//}


//typedef struct {
//    double a, b;
//} my_constraint_data;
//
//double myconstraint(unsigned n, const double *x, double *grad, void *data)
//{
//    my_constraint_data *d = (my_constraint_data *) data;
//    double a = d->a, b = d->b;
//    if (grad) {
//        grad[0] = 3 * a * (a*x[0] + b) * (a*x[0] + b);
//        grad[1] = -1.0;
//    }
//    return ((a*x[0] + b) * (a*x[0] + b) * (a*x[0] + b) - x[1]);
// }

