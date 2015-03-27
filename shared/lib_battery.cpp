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

	// Initialize charging states
	_prev_charging = false;
	_chargeChange = false;
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

	// Assume initial current is 20 hour discharge current
	// Assume initial charge is 20 capacity
	double T = _q0 / _I20;
	_qmaxI = qmax_of_i_compute(T);
	_q0 = _q20;

	// Initialize charge quantities.  
	// Assumes battery is initially fully charged
	_q1_0 = _q0*_c;
	_q2_0 = _q0 - _q1_0;

	// output structure
	_output["q0"]= _q0;
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

output_map capacity_kibam_t::updateCapacity(double P, double V, double dt, int cycles)
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

	// update the SOC
	_SOC = (q1 + q2) / _qmax;
	
	// due to dynamics, it's possible SOC could be slightly above 1 or below 0
	if (_SOC > 1.)
		_SOC = 1.;
	else if (_SOC < 0.)
		_SOC = 0.;

	_DOD = 1 - _SOC;

	// update internal variables 
	_q1_0 = q1;
	_q2_0 = q2;
	_q0 = q1 + q2;
	_I = I;
	_V = V;
	_P = P;
	if (I > 0)
		_prev_charging = false;
	else
		_prev_charging = true;

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
output_map capacity_kibam_t::updateCapacityForThermal(thermal_t * thermal)
{
	double capacity_percent = thermal->getCapacityPercent();
	_q0 *= capacity_percent;
	_q1_0 *= capacity_percent;
	_q2_0 *= capacity_percent;

	_output["q0"] = _q0;
	_output["q1"] = _q1_0;
	_output["q2"] = _q2_0;


	return _output;
}
double capacity_kibam_t::getAvailableCapacity()
{
	return _q1_0;
}
double capacity_kibam_t::getMaxCapacity()
{
	return _qmax;
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
capacity_lithium_ion_t::capacity_lithium_ion_t(double q, double V, std::vector<double> capacities, std::vector<double> cycles) :capacity_t(q, V)
{
	_qmax = q;
	_qmax0 = q;

	// fit polynomial to cycles vs capacity
	_n = capacities.size();
	_cycle_vect = new double[_n];
	_capacities_vect = new double[_n];
	 _a = new double[_n];

	for (int i = 0; i < _n; i++)
	{
		_capacities_vect[i] = (capacities[i]);
		_cycle_vect[i] = (cycles[i]);
		_a[i] = 0;
	}

	// Perform Curve fit
	int info = lsqfit(third_order_polynomial, 0, _a, _n, _cycle_vect, _capacities_vect, _n);

	_output["q0"] = q;
	_output["qmax"] = _qmax;
	_output["P"] = _P;
	_output["SOC"] = _SOC;
	_output["DOD"] = _DOD;
	_output["I"] = _I;
};
capacity_lithium_ion_t::~capacity_lithium_ion_t()
{
	delete[] _cycle_vect;
	delete[] _capacities_vect;
	delete[] _a;
}
output_map capacity_lithium_ion_t::updateCapacity(double P, double V, double dt, int cycles)
{
	double q0_old = _q0;

	// update maximum capacity based on number of cycles
	double capacity_modifier = third_order_polynomial(cycles, _a, 0);
	 _qmax = _qmax0 * capacity_modifier / 100;

	// currently just a tank of coloumbs
	_I = P / V;
	P = _P;
	bool charging = false;
	bool no_charge = false;

	// charge state 
	if (_I < 0)
		charging = true;
	else if (_I == 0)
		no_charge = true;

	// Check if charge changed
	if (charging != _prev_charging && !no_charge)
		_chargeChange = true;
	else
		_chargeChange = false;

	// Update for next time
	if (_I < 0)
		_prev_charging = true;
	else
		_prev_charging = false;

	// update charge ( I > 0 discharging, I < 0 charging)
	_q0 -= _I*dt;

	// check if overcharged
	if (_q0 > _qmax)
	{
		_I = -(_qmax - q0_old)/dt;
		_P = _I*V;
		_q0 = _qmax;
	}

	// check if undercharged (implement minimum charge limit)
	if (_q0 < 0)
	{
		_I = (q0_old) / dt;
		_P = _I*V;
		_q0 = 0;
	}

	// update SOC, DOD
	_SOC = _q0 / _qmax;
	_DOD = 1 - _SOC;

	// outputs
	_output["q0"] = _q0;
	_output["qmax"] = _qmax;
	_output["P"] = _P;
	_output["SOC"] = _SOC;
	_output["DOD"] = _DOD;
	_output["I"] = _I;

	return _output;
}
output_map capacity_lithium_ion_t::updateCapacityForThermal(thermal_t * thermal)
{
	double capacity_percent = thermal->getCapacityPercent();
	_q0 *= capacity_percent;
	
	_output["q0"] = _q0;
	return _output;
}

double capacity_lithium_ion_t::getAvailableCapacity()
{
	return _q0;
}
double capacity_lithium_ion_t::getMaxCapacity()
{
	return _qmax;
}
double capacity_lithium_ion_t::getMaxCapacityAtCurrent()
{
	return _qmax;
}
double capacity_lithium_ion_t::get10HourCapacity()
{
	return _qmax;
}

double third_order_polynomial(double cycles, double * a, void * user_data)
{
	return (a[0] + a[1] * cycles + a[2] * pow(cycles, 2) + a[3] * pow(cycles, 3));
}

/*
Define Voltage Model
*/
voltage_t::voltage_t(int num_cells, double voltage, double *other)
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

// Dynamic voltage model
voltage_dynamic_t::voltage_dynamic_t(int num_cells, double voltage, double *other) : 
voltage_t(num_cells, voltage, other)
{
	_Vfull = other[0];
	_Vexp = other[1];
	_Vnom = other[2];
	_Qfull = other[3];
	_Qexp = other[4];
	_Qnom = other[5];
	_C_rate = other[6];

	// assume fully charged, not the nominal value
	_cell_voltage = _Vfull;

	parameter_compute();
};

void voltage_dynamic_t::parameter_compute()
{
	double eta = 0.995;
	double I = _Qfull*_C_rate;
	_R = _Vnom*(1. - eta) / (_C_rate*_Qnom);
	_A = _Vfull - _Vexp; // [V]
	_B = 3. / _Qexp;     // [1/Ah]
	_K = ((_Vfull - _Vnom + _A*(std::exp(-_B*_Qnom) - 1))*(_Qfull - _Qnom)) / (_Qnom);
	_E0 = _Vfull + _K + _R*I - _A;
}

output_map voltage_dynamic_t::updateVoltage(capacity_t * capacity,  double dt)
{

	double Q = capacity->getMaxCapacityAtCurrent();
	double I = capacity->getCurrent();
	double q0 = capacity->getTotalCapacity();

	_cell_voltage = voltage_model(Q/_num_cells,I/_num_cells,q0/_num_cells);

	_output["voltage_cell"] = _cell_voltage;
	_output["voltage_battery"] = _cell_voltage*_num_cells;

	return _output;
}
double voltage_dynamic_t::voltage_model(double Q, double I, double q0)
{
	// Should increase when charge increases, decrease when charge decreases
	// everything in here is on a per-cell basis
	// Unnewehr Universal Model

	double term1 = _R*I;
	double term2 = _K*(1 - q0/Q);
	double V = _E0 - term1 - term2; 
	return V;
}



// Basic voltage model
voltage_basic_t::voltage_basic_t(int num_cells, double voltage) :
voltage_t(num_cells, voltage)
{
	_output["voltage_cell"] = voltage;
	_output["voltage_battery"] = voltage*num_cells;

}

output_map voltage_basic_t::updateVoltage(capacity_t * capacity,  double dt)
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
	_a = new double[n];

	for (int i = 0; i != n; i++)
	{
		_DOD_vect[i] = DOD_vect[i];
		_cycle_vect[i] = cycle_vect[i];
		_a[i] = 0;
	}

	// Perform Curve fit
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

		if (fabs(Cf) > 0)
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
				// _jlt == 0
				else
				{
					// force out of while
					rereadCount++;
					break;
				}
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
				if (fabs(Cf) > 0)
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
int lifetime_t::getNumberOfCycles()
{
	return _nCycles;
}

double life_vs_DOD(double R, double * a, void * user_data)
{
	// may need to figure out how to make more robust (if user enters only three pairs)
	return (a[0] + a[1] * exp(a[2] * R) + a[3] * exp(a[4] * R));
}

/*
Define Thermal Model
*/
thermal_t::thermal_t(double mass, double length, double width, double height, double thickness,
	double Cp, double k, double h, double T_room, double shade_factor, int storage_configuration, double R,
	std::vector<double> temperature_vect, std::vector<double> capacity_vect)
{
	_mass = mass;
	_length = length;
	_width = width;
	_height = height;
	_thickness = thickness;
	_Cp = Cp;
	_k = k;
	_h = h;
	_T_room = T_room;
	_shade_factor = shade_factor;
	_storage_configuration = storage_configuration;
	_R = R;

	// assume all surfaces are exposed
	_A = 2 * (length*width + length*height + width*height);

	// initialize to room temperature
	_T_battery = T_room;

	// curve fit
	int n = capacity_vect.size();
	_temperature_vect = new double[n];
	_capacity_vect = new double[n];
	_a = new double[n];

	for (int ii = 0; ii != n; ii++)
	{
		// user inputs F, modify to K
		_temperature_vect[ii] = (temperature_vect[ii]-32.)*(5./9.)+273.15;
		_capacity_vect[ii] = capacity_vect[ii];
		_a[ii] = 0.;
	}
	int info = lsqfit(third_order_polynomial, 0, _a, n, _temperature_vect, _capacity_vect, n);


	_output["T_battery"] = _T_battery;
	_output["Capacity_thermal_percent"] = 1.;

}
thermal_t::~thermal_t()
{
	delete[] _temperature_vect;
	delete[] _capacity_vect;
	delete[] _a;
}
output_map thermal_t::updateTemperature(double I, double dt)
{
	// double T_new = _T_battery + dt * 3600 * simpleModel(I);
	double T_new = rk4(I, dt*_hours_to_seconds);
	_T_battery = T_new;
	_output["T_battery"] = _T_battery;
	_output["Capacity_thermal_percent"] = getCapacityPercent();

	return _output;
}
double thermal_t::getCapacityPercent()
{
	return third_order_polynomial(_T_battery, _a, 0);
}

double thermal_t::f(double T_battery, double I)
{
	return (1 / (_mass*_Cp)) * ((_h*(_T_room - T_battery)*_A) + pow(I, 2)*_R);
}
double thermal_t::rk4( double I, double dt)
{
	double k1 = dt*f(_T_battery, I);
	double k2 = dt*f(_T_battery + k1 / 2, I);
	double k3 = dt*f(_T_battery + k2 / 2, I);
	double k4 = dt*f(_T_battery + k3, I);
	return (_T_battery + (1. / 6)*(k1 + k4) + (1. / 3.)*(k2 + k3));
}


/* 
Define Battery 
*/
battery_t::battery_t(){};
battery_t::battery_t(int num_batteries, double power_conversion_efficiency, double dt)
{
	_num_batteries = num_batteries;
	_power_conversion_efficiency = power_conversion_efficiency;
	_dt = dt;
}
void battery_t::initialize(capacity_t *capacity, voltage_t * voltage, lifetime_t * lifetime, thermal_t * thermal)
{
	_capacity = capacity;
	_lifetime = lifetime;
	_voltage = voltage;
	_thermal = thermal;
	_firstStep = true;
}

void battery_t::run(double P)
{
	double lastDOD = _capacity->getDOD();

	if (_capacity->chargeChanged() || _firstStep)
	{
		_LifetimeOutput = runLifetimeModel(lastDOD);
		_firstStep = false;
	}
	
	// Compute temperature at end of timestep
	_ThermalOutput = runThermalModel(P / _voltage->getVoltage());
	_CapacityOutput = runCapacityModel(P, _voltage->getVoltage());
	_VoltageOutput = runVoltageModel();
}

void battery_t::finish()
{
	_LifetimeOutput = _lifetime->rainflow_finish();
}
output_map battery_t::runThermalModel(double I)
{
	return _thermal->updateTemperature(I, _dt);
}

output_map battery_t::runCapacityModel(double P, double V)
{
	_capacity->updateCapacity(P, V, _dt,_lifetime->getNumberOfCycles() );
	return _capacity->updateCapacityForThermal(_thermal);
}

output_map battery_t::runVoltageModel()
{
	return _voltage->updateVoltage(_capacity, _dt);
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
output_map battery_t::getThermalOutput()
{
	return _ThermalOutput;
}
double battery_t::chargeNeededToFill()
{
	// Leads to minor discrepency, since gets max capacity from the old time step, which is based on the previous current level
	// Since the new time step will have a different power requirement, and a different current level, this leads to charge_needed not truly equaling the charge needed at the new current.
	// I don't know if there is simple way to correct this, or if it is necessary to correct
	// double charge_needed =_capacity->getMaxCapacityAtCurrent() - _capacity->getTotalCapacity();
	double charge_needed = _capacity->getMaxCapacity() - _capacity->getTotalCapacity();
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
Define Battery Bank
*/
battery_bank_t::battery_bank_t(battery_t * battery, int num_batteries, int battery_chemistry, double power_conversion_efficiency)
{
	_battery = battery;
	_num_batteries = num_batteries;
	_battery_chemistry = battery_chemistry;
	_power_conversion_efficiency = power_conversion_efficiency; // currently unused

	adjustOutputs();
}
output_map battery_bank_t::run(double P)
{
	_battery->run(P / _num_batteries);
	adjustOutputs();
	return _output;
}
output_map battery_bank_t::finish()
{
	_battery->finish();
	
	// lifetime output update
	output_map LifetimeOutput = _battery->getLifetimeOutput();
	_output["Damage"] = LifetimeOutput["Damage"];
	_output["Cycles"] = LifetimeOutput["Cycles"];
	return _output;
}
double battery_bank_t::chargeNeededToFill()
{
	return ( _num_batteries*_battery->chargeNeededToFill() );
}
double battery_bank_t::getCurrentCharge()
{
	return ( _num_batteries*_battery->getCurrentCharge() );
}
double battery_bank_t::getBankVoltage()
{
	return _num_batteries*_battery->batteryVoltage();
}
output_map battery_bank_t::getOutputs()
{
	adjustOutputs();
	return _output;
}
void battery_bank_t::adjustOutputs()
{
	// outputs are on a single battery basis
	// needs to be adjusted for battery bank
	output_map CapacityOutput = _battery->getCapacityOutput();
	output_map VoltageOutput = _battery->getVoltageOutput();

	// lifetime & thermal outputs do not need adjustment
	output_map LifetimeOutput = _battery->getLifetimeOutput();
	output_map ThermalOutput = _battery->getThermalOutput();

	// capacity output adjustment
	if (_battery_chemistry == 0)
	{
		_output["q1"] = _num_batteries*CapacityOutput["q1"];
		_output["q2"] = _num_batteries*CapacityOutput["q2"];
		_output["qmaxI"] = _num_batteries*CapacityOutput["qmaxI"];
	}
	else
		_output["qmax"] = _num_batteries*CapacityOutput["qmax"];


	_output["q0"] = _num_batteries*CapacityOutput["q0"];
	_output["P"] = 0;
	_output["SOC"] = CapacityOutput["SOC"];
	_output["DOD"] = CapacityOutput["DOD"];
	_output["I"] = CapacityOutput["I"];

	// voltage output 
	_output["voltage_bank"] = _num_batteries*VoltageOutput["voltage_battery"];
	_output["voltage_battery"] = VoltageOutput["voltage_battery"];
	_output["voltage_cell"] = VoltageOutput["voltage_cell"];

	// lifetime output
	_output["Damage"] = LifetimeOutput["Damage"];
	_output["Cycles"] = LifetimeOutput["Cycles"];

	// thermal output
	_output["T_battery"] = ThermalOutput["T_battery"];
	_output["Capacity_thermal_percent"] = ThermalOutput["Capacity_thermal_percent"];

}


/*
Non-class functions
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