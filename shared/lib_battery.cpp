#include <math.h>

#include "lib_battery.h"
/* 
Define Capacity Model 
*/

capacity_t::capacity_t(double q, double V)
{
	_q0 = q;
	_I = 0.;
	_V = V;
	_P = 0.;

	// Initialize SOC to 1, DOD to 0
	_SOC = 1;
	_DOD = 0;
}

bool capacity_t::chargeChanged()
{
	return _chargeChange;
}

double capacity_t::getDOD()
{
	return _DOD;
}

double capacity_t::getTotalCapacity()
{
	return _q0;
}
double capacity_t::getCurrent()
{
	return _I;
}
/*
Define KiBam Capacity Model
*/
capacity_kibam_t::capacity_kibam_t(double q10, double q20, double I20, double V, double t1, double t2, double q1, double q2) :
capacity_t(q20, V)
{
	_q10 = q10;
	_q20 = q20;
	_I20 = I20;

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
	_q1_0 = q20*_c;
	_q2_0 = _q0 - _q1_0;
	_chargeChange = false;
	_prev_charging = false;

	// Initialize other variables
	// Assumes initial current is 20 hour discharge current
	double T = _q0 / _I20;
	_qmaxI = qmax_of_i_compute(T);

	// output structure
	_output["q0"]= q20;
	_output["q1"] = _q1_0;
	_output["q2"] = _q2_0;
	_output["P"] = _P;
	_output["SOC"] = _SOC;
	_output["DOD"]= _DOD;
	_output["qmaxI"] = _qmaxI;
	_output["I"]= _I20;
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

output_map capacity_kibam_t::updateCapacity(double P, double V, double dt)
{
	double I = P / V;
	double Idmax = 0.;
	double Icmax = 0.;
	double Id = 0.;
	double Ic = 0.;
	double q1 = 0.;
	double q2 = 0.;
	bool charging = false;
	bool no_charge = false;

	if (I > 0)
	{
		Idmax = Idmax_compute(_q1_0, _q0, dt);
		Id = fmin(I, Idmax);
		I = Id;
	}
	else if (I < 0 )
	{
		Icmax = Icmax_compute(_q1_0, _q0, dt);
		Ic = -fmin(fabs(I), fabs(Icmax));
		I = Ic;
		charging = true;
	}
	else
	{
		no_charge = true;
	}

	// Check if charge changed
	if (charging != _prev_charging && !no_charge)
		_chargeChange = true;
	else
		_chargeChange = false;

	// new charge levels
	q1 = q1_compute(_q1_0, _q0, dt, I);
	q2 = q2_compute(_q2_0, _q0, dt, I);

	// update max charge at this current
	if (fabs(I) > 0)
		_qmaxI = qmax_of_i_compute(fabs(_qmaxI / I));
	else
	{
		//  just leave alone for timestep?
		_qmaxI = _qmax;
	}

	// update the SOC
	_SOC = (q1 + q2) / _qmaxI;
	_DOD = 1 - _SOC;

	// update internal variables 
	_q1_0 = q1;
	_q2_0 = q2;
	_q0 = q1 + q2;
	_I = I;
	_V = V;
	_P = P;
	
	// return variables
	_output["q0"] = _q0;
	_output["q1"] = _q1_0;
	_output["q2"] = _q2_0;
	_output["P"] = _P;
	_output["SOC"] = _SOC;
	_output["DOD"] = _DOD;
	_output["qmaxI"] = _qmaxI;
	_output["I"] = _I;

	return _output;
}
double capacity_kibam_t::getAvailableCapacity()
{
	return _q1_0;
}
double capacity_kibam_t::getMaxCapacityAtCurrent()
{
	return _qmaxI;
}
double capacity_kibam_t::get10HourCapacity()
{
	return _q10;
}
double capacity_kibam_t::get20HourCapacity()
{
	return _q20;
}

/*
Define Lithium Ion capacity model
*/
capacity_lithium_ion_t::capacity_lithium_ion_t(double q, double V) :capacity_t(q, V)
{
	_qmax = q;

	_output["q0"] = q;
	_output["P"] = _P;
	_output["SOC"] = _SOC;
	_output["DOD"] = _DOD;
	_output["I"] = _I;
};

output_map capacity_lithium_ion_t::updateCapacity(double P, double V, double dt)
{
	// currently just a tank of coloumbs
	_I = P / V;
	P = _P;

	// update charge ( I > 0 discharging, I < 0 charging)
	_q0 -= _I*dt;

	// check if overcharged
	if (_q0 > _qmax)
		_q0 = _qmax;

	// check if undercharged (implement minimum charge limit)
	if (_q0 < 0)
		_q0 = 0;

	// update SOC, DOD
	_SOC = _q0 / _qmax;
	_DOD = 1 - _SOC;


	// outputs
	_output["q0"] = _q0;
	_output["P"] = _P;
	_output["SOC"] = _SOC;
	_output["DOD"] = _DOD;
	_output["I"] = _I;

	return _output;
}


double capacity_lithium_ion_t::getAvailableCapacity()
{
	return _q0;
}
double capacity_lithium_ion_t::getMaxCapacityAtCurrent()
{
	return _qmax;
}
double capacity_lithium_ion_t::get10HourCapacity()
{
	return _qmax;
}



/*
Define Voltage Model
*/
voltage_t::voltage_t(int num_cells, double voltage )
{
	_num_cells = num_cells;
	_cell_voltage = voltage;
}

double voltage_t::getVoltage()
{
	return _num_cells*_cell_voltage;
}

double voltage_t::getCellVoltage()
{
	return _cell_voltage;
}

voltage_copetti_t::voltage_copetti_t(int num_cells, double voltage) :
voltage_t(num_cells, voltage)
{
	_output["voltage_cell"] = voltage;
	_output["voltage_battery"] = voltage*num_cells;
}

output_map voltage_copetti_t::updateVoltage(capacity_t* capacity, double dT)
{
	double I = capacity->getCurrent();
	double DOD = capacity->getDOD();
	double q10 = capacity->get10HourCapacity();

	// discharge
	if (I > 0)
		voltage_discharge(DOD, q10, I, dT);
	// charge
	else if (I < 0)
		voltage_charge(DOD, q10, fabs(I), dT);
	// or nothing

	_output["voltage_cell"] = _cell_voltage;
	_output["voltage_battery"] = _cell_voltage*_num_cells;

	return _output;
}

double voltage_copetti_t::voltage_charge(double DOD, double q10, double I, double dT)
{
	if (DOD < 0)
		DOD = 0.;

	double term1 = 2 + 0.16*DOD;
	double term2 = (I / q10)*(6 / (1 + std::pow(I,0.6)) + 0.48 / std::pow((1 - DOD),1.2) + 0.036);
	double term3 = (1 - 0.025 * dT);
	_cell_voltage = term1 - term2*term3;
	return (_cell_voltage);
}

double voltage_copetti_t::voltage_discharge(double DOD, double q10, double I, double dT)
{
	if (DOD < 0)
		DOD = 0.;

	double term1 = 2.085 - 0.12*(DOD);
	double term2 = (1. / q10)*( (4. / (1 + std::pow(I,1.3))) + (0.27 / (1 - std::pow(DOD,1.5))) + 0.02);
	double term3 = (1. - 0.007 * dT);
	_cell_voltage = term1 - term2*term3;
	return (_cell_voltage);
}

// Basic voltage model
voltage_basic_t::voltage_basic_t(int num_cells, double voltage) :
voltage_t(num_cells, voltage)
{
	_output["voltage_cell"] = voltage;
	_output["voltage_battery"] = voltage*num_cells;

}

output_map voltage_basic_t::updateVoltage(capacity_t * capacity, double dT)
{
	// do nothing;
	_output["voltage_cell"] = _cell_voltage;
	_output["voltage_battery"] = _cell_voltage*_num_cells;

	return _output;
}


/*
Define Lifetime Model
*/

lifetime_t::lifetime_t( std::vector<double> DOD_vect, std::vector<double> cycle_vect, int n )
{
	_DOD_vect = new double[n];
	_cycle_vect = new double[n]; 

	for (int i = 0; i != n; i++)
	{
		_DOD_vect[i] = DOD_vect[i];
		_cycle_vect[i] = cycle_vect[i];
	}

	// Perform Curve fit
	_a = new double[n];
	for (int i = 0; i != n; i++)
		_a[i] = 0;
	int info = lsqfit(life_vs_DOD, 0, _a, n, _DOD_vect, _cycle_vect, n);
	int j;

	// initialize other member variables
	_nCycles = 0;
	_Dlt = 0;
	_jlt = 0;
	_klt = 0; 
	_Xlt = 0;
	_Ylt = 0;
	_Slt = 0;
	_Range = 0;

	// initialize output
	_output["Damage"] = 0.;
	_output["Cycles"] = 0.;
}

lifetime_t::~lifetime_t()
{
	delete[] _DOD_vect;
	delete[] _cycle_vect;
	delete[] _a;
}

output_map lifetime_t::rainflow(double DOD)
{
	// initialize return code
	int retCode = LT_GET_DATA;

	// Begin algorithm
	_Peaks.push_back(DOD);
	bool atStepTwo = true;

	// Assign S, which is the starting peak or valley
	if (_jlt == 0)
	{
		_Slt = DOD;
		_klt = _jlt;
	}

	// Loop until break
	while (atStepTwo)
	{
		// Rainflow: Step 2: Form ranges X,Y
		if (_jlt >= 2)
			rainflow_ranges();
		else
		{
			// Get more data (Step 1)
			retCode = LT_GET_DATA;
			break;
		}

		// Rainflow: Step 3: Compare ranges
		retCode = rainflow_compareRanges();

		// We break to get more data, or if we are done with step 5
		if (retCode == LT_GET_DATA)
			break;
	}

	if (retCode == LT_GET_DATA)
		_jlt++;

	// Return output
	_output["Damage"] = _Dlt;
	_output["Cycles"] = _nCycles;

	return _output;
}

void lifetime_t::rainflow_ranges()
{
	_Ylt = fabs(_Peaks[_jlt - 1] - _Peaks[_jlt - 2]);
	_Xlt = fabs(_Peaks[_jlt] - _Peaks[_jlt - 1]);
}
void lifetime_t::rainflow_ranges_circular(int index)
{
	int end = _Peaks.size() - 1;
	if (index == 0)
	{
		_Xlt = fabs(_Peaks[0] - _Peaks[end]);
		_Ylt = fabs(_Peaks[end] - _Peaks[end - 1]);
	}
	else if (index == 1)
	{
		_Xlt = fabs(_Peaks[1] - _Peaks[0]);
		_Ylt = fabs(_Peaks[0] - _Peaks[end]);
	}
	else
		rainflow_ranges();
}

int lifetime_t::rainflow_compareRanges()
{
	int retCode = LT_SUCCESS;
	bool contained = true;

	if (_Xlt < _Ylt)
		retCode = LT_GET_DATA;
	else if (_Xlt == _Ylt)
	{
		if ((_Slt == _Peaks[_jlt - 1]) || (_Slt == _Peaks[_jlt - 2]))
			retCode = LT_GET_DATA;
		else
			contained = false;
	}
	else if (_Xlt >= _Ylt)
	{
		if (_Xlt > _Ylt)
		{
			if ((_Slt == _Peaks[_jlt - 1]) || (_Slt == _Peaks[_jlt - 2]))
			{
				// Step 4: Move S to next point in vector, then go to step 1
				_klt++;
				_Slt = _Peaks[_klt];
				retCode = LT_GET_DATA;
			}
			else
				contained = false;
		}
		else if (_Xlt == _Ylt)
		{
			if ((_Slt != _Peaks[_jlt - 1]) && (_Slt != _Peaks[_jlt - 2]))
				contained = false;
		}

	}

	// Step 5: Count range Y, discard peak & valley of Y, go to Step 2
	if (!contained)
	{
		_Range = _Ylt;
		double Cf = life_vs_DOD(_Range, _a, 0);
		_Dlt += 1. / Cf;
		_nCycles++;
		// discard peak & valley of Y
		double save = _Peaks[_jlt];
		_Peaks.pop_back(); 
		_Peaks.pop_back();
		_Peaks.pop_back();
		_Peaks.push_back(save);
		_jlt -= 2;
		// stay in while loop
		retCode = LT_RERANGE;
	}

	return retCode;
}

output_map lifetime_t::rainflow_finish()
{
	// starting indices, must decrement _jlt by 1
	int ii = 0;
	_jlt--;
	double P = 0.;
	int rereadCount = 0;


	while ( rereadCount <= 1 )
	{
		if (ii < _Peaks.size())
			P = _Peaks[ii];
		else
			break;

		// Step 6
		if (P == _Slt)
			rereadCount++;

		bool atStepSeven = true;

		// Step 7: Form ranges X,Y
		while (atStepSeven)
		{
			if (_jlt >= 2)
				rainflow_ranges_circular(ii);
			else
			{
				atStepSeven = false;
				if (_jlt == 1)
				{
					_Peaks.push_back(P);
					_jlt++;
					// move to end point
					ii = _jlt;
					rainflow_ranges_circular(ii);
				}
				else
					break;
			}

			// Step 8: compare X,Y
			if (_Xlt < _Ylt)
			{
				atStepSeven = false;
				// move to next point (Step 6)
				ii++;
			}
			else
			{
				_Range = _Ylt;
				double Cf = life_vs_DOD(_Range, _a, 0);
				_Dlt += 1. / Cf;
				_nCycles++;
				// Discard peak and vally of Y
				double save = _Peaks[_jlt];
				_Peaks.pop_back();
				_Peaks.pop_back();
				_Peaks.pop_back();
				_Peaks.push_back(save);
				_jlt -= 2;
			}
		}
	}
	// Return output
	_output["Damage"] = _Dlt;
	_output["Cycles"] = _nCycles;

	return _output;
}

double life_vs_DOD(double R, double * a, void * user_data)
{
	// may need to figure out how to make more robust (if user enters only three pairs)
	return (a[0] + a[1] * exp(a[2] * R) + a[3] * exp(a[4] * R));
}

/* 
Define Battery 
*/
battery_t::battery_t(){};
battery_t::battery_t(capacity_t *capacity, voltage_t * voltage, lifetime_t * lifetime, double dt)
{
	initialize(capacity, voltage, lifetime, dt);
}
void battery_t::initialize(capacity_t *capacity, voltage_t * voltage, lifetime_t * lifetime, double dt)
{
	_capacity = capacity;
	_lifetime = lifetime;
	_voltage = voltage;
	_dt = dt;
	_firstStep = true;
}

void battery_t::run(double P, double dT)
{
	double lastDOD = _capacity->getDOD();

	if (lastDOD <= 1. && lastDOD >= 0.)
	{
		if (_capacity->chargeChanged() || _firstStep)
		{
			_LifetimeOutput = runLifetimeModel(lastDOD);
			_firstStep = false;
		}
	}

	_CapacityOutput = runCapacityModel(P, _voltage->getVoltage() );
	_VoltageOutput = runVoltageModel(dT);

}

void battery_t::finish()
{
	_LifetimeOutput = _lifetime->rainflow_finish();
}

output_map battery_t::runCapacityModel(double P, double V)
{
	return _capacity->updateCapacity(P, V, _dt);
}

output_map battery_t::runVoltageModel(double dT)
{
	return _voltage->updateVoltage(_capacity, dT);
}

output_map battery_t::runLifetimeModel(double DOD)
{
	return _lifetime->rainflow(DOD);
}

output_map battery_t::getCapacityOutput()
{
	return _CapacityOutput;
}

output_map battery_t::getVoltageOutput()
{
	return _VoltageOutput;
}

output_map battery_t::getLifetimeOutput()
{
	return _LifetimeOutput;
}

double battery_t::chargeNeededToFill()
{
	double charge_needed =_capacity->getMaxCapacityAtCurrent() - _capacity->getTotalCapacity();
	if (charge_needed > 0)
		return charge_needed;
	else
		return 0.;
}

double battery_t::getCurrentCharge()
{
	return _capacity->getAvailableCapacity();
}

double battery_t::cellVoltage()
{
	return _voltage->getCellVoltage();
}
double battery_t::batteryVoltage()
{
	return _voltage->getVoltage();
}
/*
Non-class function
*/
void getMonthHour(int hourOfYear, int * out_month, int * out_hour)
{
	int tmpSum = 0;
	int hour = 0;
	int month;

	for ( month = 1; month <= 12; month++)
	{
		int hoursInMonth = util::hours_in_month(month);
		tmpSum += hoursInMonth;

		// found the month
		if (hourOfYear + 1 <= tmpSum)
		{
			// get the day of the month
			int tmp = floor((float)(hourOfYear) / 24);
			hour = (hourOfYear + 1) - (tmp * 24);
			break;
		}
	}

	*out_month = month;
	*out_hour = hour;

}
bool compare(int i, int j)
{
	return i == j;
}