#include <math.h>

#include "lib_battery.h"

/* 
Define Capacity Model 
*/

capacity_t::capacity_t(double q20, double I20, double V)
{
	_q20 = q20;
	_I20 = I20;
	_V = V;

	// Initialize SOC to 1, DOD to 0
	_SOC = 1;
	_DOD = 0;
}

capacity_kibam_t::capacity_kibam_t(double q20, double I20, double V, double t1, double t2, double q1, double q2) : 
capacity_t(q20, I20, V)
{
	// parameters for c, k calculation
	_q1 = q1;
	_q2 = q2;
	_t1 = t1;
	_t2 = t2;
	_F1 = q1 / q20; // use t1, 20
	_F2 = q1 / q2;  // use t1, t2

	// compute the parameters
	parameter_compute();

	// Initialize charge quantities.  
	// Assumes battery is initially fully charged
	_q0 = q20;
	_q1_0 = q20*_c;
	_q2_0 = _q0 - _q1_0;

	// Initialize other variables
	// Assumes initial current is 20 hour discharge current
	double T = _q0 / _I20;
	_qmaxI = qmax_of_i_compute(T);

	// Setup output structure
	_output = new output[TOTAL_CAPACITY_OUT];
	_output[TOTAL_CHARGE].name = "q0"; 
	_output[AVAILABLE_CHARGE].name = "q1"; 
	_output[BOUND_CHARGE].name = "q2"; 
	_output[POWER_DURING_STEP].name = "P";
	_output[STATE_OF_CHARGE].name = "SOC";
	_output[DEPTH_OF_DISCHARGE].name = "DOD"; 
	_output[MAX_CHARGE_AT_CURRENT].name = "qmaxI"; 
	_output[CURRENT].name = "I"; 

}

capacity_kibam_t::~capacity_kibam_t()
{
	delete[] _output;
}

double capacity_kibam_t::c_compute(double F, double t1, double t2, double k_guess)
{
	double num = F*(1 - exp(-k_guess*t1))*t2 - (1 - exp(-k_guess*t2))*t1;
	double denom = F*(1 - exp(-k_guess*t1))*t2 - (1 - exp(-k_guess*t2))*t1 - k_guess*F*t1*t2 + k_guess*t1*t2;
	return (num / denom);
}

double capacity_kibam_t::q1_compute(double q10, double q0, double dt, double I)
{
	double A = q10*exp(-_k*dt);
	double B = (q0*_k*_c - I)*(1 - exp(-_k*dt)) / _k;
	double C = I*_c*(_k*dt - 1 + exp(-_k*dt)) / _k;
	return (A + B - C);
}

double capacity_kibam_t::q2_compute(double q20, double q0, double dt, double I)
{
	double A = q20*exp(-_k*dt);
	double B = q0*(1 - _c)*(1 - exp(-_k*dt));
	double C = I*(1 - _c)*(_k*dt - 1 + exp(-_k*dt)) / _k;
	return (A + B - C);
}

double capacity_kibam_t::Icmax_compute(double q10, double q0, double dt)
{
	double num = -_k*_c*_qmax + _k*q10*exp(-_k*dt) + q0*_k*_c*(1 - exp(-_k*dt));
	double denom = 1 - exp(-_k*dt) + _c*(_k*dt - 1 + exp(-_k*dt));
	return (num / denom);
}

double capacity_kibam_t::Idmax_compute(double q10, double q0, double dt)
{
	double num = _k*q10*exp(-_k*dt) + q0*_k*_c*(1 - exp(-_k*dt));
	double denom = 1 - exp(-_k*dt) + _c*(_k*dt - 1 + exp(-_k*dt));
	return (num / denom);
}

double capacity_kibam_t::qmax_compute()
{
	double num = _q20*((1 - exp(-_k * 20)) * (1 - _c) + _k*_c * 20);
	double denom = _k*_c * 20;
	return (num / denom);
}

double capacity_kibam_t::qmax_of_i_compute(double T)
{
	return ((_qmax*_k*_c*T) / (1 -exp(-_k*T) + _c*(_k*T - 1 + exp(-_k*T))));
}
void capacity_kibam_t::parameter_compute()
{
	double k_guess = 0.;
	double c1 = 0.;
	double c2 = 0.;
	double minRes = 10000.;

	for (int i = 0; i < 1000; i++)
	{
		k_guess = i*0.01;
		c1 = c_compute(_F1, _t1, 20, k_guess);
		c2 = c_compute(_F2, _t1, _t2, k_guess);

		if (fabs(c1 - c2) < minRes)
		{
			minRes = fabs(c1 - c2);
			_k = k_guess;
			_c = 0.5*(c1 + c2);
		}
	}
	_qmax = qmax_compute();
}

output* capacity_kibam_t::updateCapacity(double P, double V, double dt)
{
	double I = P / V;
	double Idmax = 0.;
	double Icmax = 0.;
	double Id = 0.;
	double Ic = 0.;
	double q1 = 0.;
	double q2 = 0.;

	if (I > 0)
	{
		Idmax = Idmax_compute(_q1_0, _q0, dt);
		Id = fmin(I, Idmax);
		I = Id;
	}
	else
	{
		Icmax = Icmax_compute(_q1_0, _q0, dt);
		Ic = -fmin(fabs(I), fabs(Icmax));
		I = Ic;
	}

	// new charge levels
	q1 = q1_compute(_q1_0, _q0, dt, I);
	q2 = q2_compute(_q2_0, _q0, dt, I);

	// update max charge at this current
	if (fabs(I) > 0)
		_qmaxI = qmax_of_i_compute(fabs(_qmaxI / I));
	else
		_qmaxI = _qmax;

	// update the SOC
	_SOC = (q1 + q2) / _qmaxI;
	_DOD = 1 - _SOC;

	// update internal variables for next time
	_q1_0 = q1;
	_q2_0 = q2;
	_q0 = q1 + q2;
	
	// return variables
	_output[TOTAL_CHARGE].value = _q0;
	_output[AVAILABLE_CHARGE].value = _q1_0;
	_output[BOUND_CHARGE].value = _q2_0;
	_output[POWER_DURING_STEP].value = P;
	_output[STATE_OF_CHARGE].value = _SOC;
	_output[DEPTH_OF_DISCHARGE].value = _DOD;
	_output[MAX_CHARGE_AT_CURRENT].value = _qmaxI;
	_output[CURRENT].value = I;

	return _output;
}

/*
Define Lifetime Model
*/
/*
lifetime_t::lifetime_t( double * DOD_vect, double * cycle_vect )
{
	_DOD_vect = DOD_vect;
	_cycle_vect = cycle_vect;
}

*/

/* 
Define Battery 
*/
battery_t::battery_t(capacity_t *capacity, double dt)
{
	_capacity = capacity;
	_dt = dt;
}

output* battery_t::runCapacityModel(double P, double V)
{
	return _capacity->updateCapacity(P, V, _dt);
}