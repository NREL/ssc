#include <math.h>
#include <cmath>
#include <cfloat>

#include "lib_battery.h"
/* 
Define Capacity Model 
*/

capacity_t::capacity_t(double q)
{
	_q0 = q;
	_qmax = q;
	_qmax0 = q;
	_I = 0.;

	// Initialize SOC, DOD
	_SOC = 100;
	_DOD = 0;
	_DOD_prev = 0;

	// Initialize charging states
	_prev_charge = DISCHARGE;
	_chargeChange = false;
}
void capacity_t::check_charge_change()
{
	int charging = NO_CHARGE;

	// charge state 
	if (_I < 0)
		charging = CHARGE;
	else if (_I > 0)
		charging = DISCHARGE;

	// Check if charge changed 
	_chargeChange = false;
	if ((charging != _prev_charge) && (charging != NO_CHARGE) && (_prev_charge != NO_CHARGE) && (fabs(_I) > 1) )
	{
		_chargeChange = true;
		_prev_charge = charging;
	}
}
void capacity_t::update_SOC()
{
	if (_qmax > 0)
		_SOC = 100.*(_q0 / _qmax);
	else
		_SOC = 0.;

	// due to dynamics, it's possible SOC could be slightly above 1 or below 0
	if (_SOC > 100.)
		_SOC = 100.;
	else if (_SOC < 0.)
		_SOC = 0.;

	_DOD = 100. - _SOC;
}
bool capacity_t::chargeChanged(){return _chargeChange;}
double capacity_t::SOC(){ return _SOC; }
double capacity_t::DOD(){ return _DOD; }
double capacity_t::prev_DOD(){ return _DOD_prev; }
double capacity_t::q0(){ return _q0;}
double capacity_t::qmax(){ return _qmax; }
double capacity_t::I(){ return _I; }


/*
Define KiBam Capacity Model
*/
capacity_kibam_t::capacity_kibam_t(double q20, double t1, double q1, double q10) :
capacity_t(q20)
{
	_q10 = q10;
	_q20 = q20;
	_I20 = q20/20.;

	// parameters for c, k calculation
	_q1 = q1;
	_q2 = q10;
	_t1 = t1;
	_t2 = 10.;
	_F1 = q1 / q20; // use t1, 20
	_F2 = q1 / q10;  // use t1, 10

	// compute the parameters
	parameter_compute();
	_qmax0 = _qmax;

	// initializes to full battery
	replace_battery();
}
void capacity_kibam_t::replace_battery()
{
	// Assume initial charge is 20 capacity
	_q0 = _q20;
	_q1_0 = _q0*_c;
	_q2_0 = _q0 - _q1_0;
	_qmax = _qmax0;
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

	for (int i = 0; i < 5000; i++)
	{
		k_guess = i*0.001;
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

void capacity_kibam_t::updateCapacity(double I, double dt_hour)
{
	_DOD_prev = _DOD;							 
	_I = I;
	double Idmax = 0.;
	double Icmax = 0.;
	double Id = 0.;
	double Ic = 0.;
	double q1 = 0.;
	double q2 = 0.;

	if (_I > 0)
	{
		Idmax = Idmax_compute(_q1_0, _q0, dt_hour);
		Id = fmin(_I, Idmax);
		_I = Id;
	}
	else if (_I < 0 )
	{
		Icmax = Icmax_compute(_q1_0, _q0, dt_hour);
		Ic = -fmin(fabs(_I), fabs(Icmax));
		_I = Ic;
	}

	// new charge levels
	q1 = q1_compute(_q1_0, _q0, dt_hour, _I);
	q2 = q2_compute(_q2_0, _q0, dt_hour, _I);

	// update internal variables 
	_q1_0 = q1;
	_q2_0 = q2;
	_q0 = q1 + q2;

	update_SOC();
	check_charge_change(); 
}
void capacity_kibam_t::updateCapacityForThermal(double capacity_percent)
{

	_I *= capacity_percent*0.01;
	update_SOC();
}
void capacity_kibam_t::updateCapacityForLifetime(double capacity_percent, bool update_max_capacity)
{

	if (update_max_capacity)
	{
		if (_qmax0* capacity_percent*0.01 <= _qmax)
			_qmax = _qmax0* capacity_percent*0.01;
	}
	// scale to q0 = qmax if q0 > qmax
	if (_q0 > _qmax)
	{
		double p = _qmax / _q0;
		_q0 *= p;
		_q1 *= p;
		_q2 *= p;
	}
	update_SOC();
}

double capacity_kibam_t::q1(){ return _q1_0; }
double capacity_kibam_t::q2(){ return _q2_0; }
double capacity_kibam_t::q10(){ return _q10; }
double capacity_kibam_t::q20(){return _q20;}


/*
Define Lithium Ion capacity model
*/
capacity_lithium_ion_t::capacity_lithium_ion_t(double q) :capacity_t(q){};
capacity_lithium_ion_t::~capacity_lithium_ion_t(){}
void capacity_lithium_ion_t::replace_battery()
{
	_q0 = _qmax0;
	_qmax = _qmax0;
}
void capacity_lithium_ion_t::updateCapacity(double I, double dt)
{
	_DOD_prev = _DOD;
	double q0_old = _q0;
	_I = I;

	// update charge ( I > 0 discharging, I < 0 charging)
	_q0 -= _I*dt;

	// check if overcharged
	if (_q0 > _qmax)
	{
		_I = -(_qmax - q0_old) / dt;
		_q0 = _qmax;
	}

	// check if undercharged 
	if (_q0 < 0)
	{
		_I = (q0_old) / dt;
		_q0 = 0;
	}

	// update SOC, DOD
	update_SOC();
	check_charge_change();
}
void capacity_lithium_ion_t::updateCapacityForThermal(double capacity_percent)
{
	_I *= capacity_percent*0.01;
	update_SOC();
}
void capacity_lithium_ion_t::updateCapacityForLifetime(double capacity_percent, bool update_max_capacity)
{
	if (update_max_capacity)
	{
		if (_qmax0* capacity_percent*0.01 <= _qmax)
			_qmax = _qmax0* capacity_percent*0.01;
	}
	if (_q0 > _qmax)
		_q0 = _qmax;

	update_SOC();
}
double capacity_lithium_ion_t::q1(){return _q0;}
double capacity_lithium_ion_t::q10(){return _qmax;}


/*
Define Voltage Model
*/
voltage_t::voltage_t(int num_cells_series, int num_cells_parallel, double voltage)
{
	_num_cells_series = num_cells_series;
	_num_cells_parallel = num_cells_parallel;
	_cell_voltage = voltage;
	_R = 0.004; // just a default, will get recalculated upon construction
}

double voltage_t::battery_voltage(){ return _num_cells_series*_cell_voltage; }
double voltage_t::cell_voltage(){ return _cell_voltage; }
double voltage_t::R(){ return _R; }


// Dynamic voltage model
voltage_dynamic_t::voltage_dynamic_t(int num_cells_series, int num_cells_parallel, double voltage, double Vfull, double Vexp, double Vnom, double Qfull, double Qexp, double Qnom, double C_rate):
voltage_t(num_cells_series, num_cells_parallel, voltage)
{
	_Vfull = Vfull;
	_Vexp = Vexp;
	_Vnom = Vnom;
	_Qfull = Qfull;
	_Qexp = Qexp;
	_Qnom = Qnom;
	_C_rate = C_rate;

	// assume fully charged, not the nominal value
	_cell_voltage = _Vfull;

	parameter_compute();
};

void voltage_dynamic_t::parameter_compute()
{
	// Determines parameters according to page 2 of:
	// Tremblay 2009 "A Generic Bettery Model for the Dynamic Simulation of Hybrid Electric Vehicles"
	double eta = 0.995;
	double I = _Qfull*_C_rate; // [A]
	_R = _Vnom*(1. - eta) / (_C_rate*_Qnom); // [Ohm]
	_A = _Vfull - _Vexp; // [V]
	_B = 3. / _Qexp;     // [1/Ah]
	_K = ((_Vfull - _Vnom + _A*(std::exp(-_B*_Qnom) - 1))*(_Qfull - _Qnom)) / (_Qnom); // [V] - polarization voltage
	_E0 = _Vfull + _K + _R*I - _A;
}

void voltage_dynamic_t::updateVoltage(capacity_t * capacity,  double dt)
{

	double Q = capacity->qmax();
	double I = capacity->I();
	double q0 = capacity->q0();
	int num_cells = _num_cells_parallel + _num_cells_series;
	int num_strings = num_cells / _num_cells_series;
	
	// is on a per-cell basis
	_cell_voltage = voltage_model_tremblay_hybrid(Q / num_cells, I/num_strings , q0 / num_cells);
}
double voltage_dynamic_t::voltage_model(double Q, double I, double q0)
{
	// Should increase when charge increases, decrease when charge decreases
	// everything in here is on a per-cell basis
	// Unnewehr Universal Model

	double term1 = _E0 - _R*I;
	double term2 = _K*(1 - q0/Q);
	double V = term1 - term2; 
	return V;
}
double voltage_dynamic_t::voltage_model_tremblay_hybrid(double Q, double I, double q0)
{
	// everything in here is on a per-cell basis
	// Tremblay Dynamic Model
	double it = Q - q0;
	double E = _E0 - _K*(Q / (Q - it)) + _A*exp(-_B*it);
	double V = E - _R*I;

	// Discharged lower than model can handle ( < 1% SOC)
	if (V < 0 || !std::isfinite(V))
		V = 0.5*_Vnom; 
	else if (V > _Vfull*1.25)
		V = _Vfull;
	return V;
}

// Basic voltage model
voltage_basic_t::voltage_basic_t(int num_cells_series, int num_cells_parallel, double voltage) :
voltage_t(num_cells_series, num_cells_parallel, voltage){}

void voltage_basic_t::updateVoltage(capacity_t * capacity, double dt){}

/*
Define Lifetime Model
*/

lifetime_t::lifetime_t(const util::matrix_t<double> &batt_lifetime_matrix, const bool enable_replacement, const double replacement_capacity)
{
	_batt_lifetime_matrix = batt_lifetime_matrix;
	_enable_replacement = enable_replacement;
	_replacement_capacity = replacement_capacity;
	_replacements = 0;

	for (int i = 0; i < _batt_lifetime_matrix.nrows(); i++)
	{
		_DOD_vect.push_back(batt_lifetime_matrix.at(i,0));
		_cycles_vect.push_back(batt_lifetime_matrix.at(i,1));
		_capacities_vect.push_back(batt_lifetime_matrix.at(i, 2));
	}
	// initialize other member variables
	_nCycles = 0;
	_Dlt = 0;
	_Clt = bilinear(0.,0);
	_jlt = 0;
	_Xlt = 0;
	_Ylt = 0;
	_Range = 0;
	_average_range = 0;
	_fortyPercent = 0;
	_hundredPercent = 0;
}

lifetime_t::~lifetime_t(){}

void lifetime_t::rainflow(double DOD)
{
	// initialize return code
	int retCode = LT_GET_DATA;

	// Begin algorithm
	_Peaks.push_back(DOD);
	bool atStepTwo = true;

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

	// modified to disregard some of algorithm which doesn't work well
	if (_Xlt < _Ylt)
		retCode = LT_GET_DATA;
	else if (_Xlt >= _Ylt)
		contained = false;

	// Step 5: Count range Y, discard peak & valley of Y, go to Step 2
	if (!contained)
	{
		_Range = _Ylt;
		_average_range = (_average_range*_nCycles + _Range) / (_nCycles + 1);
		_nCycles++;

		// the capacity percent cannot increase
		if (bilinear(_average_range, _nCycles) <= _Clt)
			_Clt = bilinear(_average_range, _nCycles);

		if (_Clt < 0)
			_Clt = 0.;

		// check DOD, increment counters
		if (_Range > 40.)
			_fortyPercent++;
		if (_Range >= 99.5)
			_hundredPercent++;

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
bool lifetime_t::check_replaced()
{
	bool replaced = false;
	if (_Clt <= _replacement_capacity)
	{
		_replacements++;
		_Clt = bilinear(0.,0);
		_Dlt = 0.;
		_nCycles = 0;
		_fortyPercent = 0;
		_hundredPercent = 0;
		_jlt = 0;
		_Xlt = 0;
		_Ylt = 0;
		_Range = 0;
		_Peaks.clear();
		replaced = true;
	}
	return replaced;
}
int lifetime_t::replacements(){ return _replacements; }
int lifetime_t::cycles_elapsed(){return _nCycles;}
double lifetime_t::capacity_percent(){ return _Clt; }
int lifetime_t::forty_percent_cycles(){ return _fortyPercent; }
int lifetime_t::hundred_percent_cycles(){ return _hundredPercent; }
double lifetime_t::cycle_range(){ return _Range; }


double lifetime_t::bilinear(double DOD, int cycle_number)
{
	/*
	Work could be done to make this simpler
	Current idea is to interpolate first along the C = f(n) curves for each DOD to get C_DOD_, C_DOD_+ 
	Then interpolate C_, C+ to get C at the DOD of interest
	*/

	std::vector<double> D_unique_vect;
	std::vector<double> C_n_low_vect;
	std::vector<double> D_high_vect;
	std::vector<double> C_n_high_vect;
	std::vector<int> low_indices;
	std::vector<int> high_indices;
	double D = 0.;
	int n = 0;
	double C = 100;

	// get unique values of D
	D_unique_vect.push_back(_DOD_vect[0]);
	for (int i = 0; i < _DOD_vect.size(); i++){
		bool contained = false;
		for (int j = 0; j < D_unique_vect.size(); j++){
			if (_DOD_vect[i] == D_unique_vect[j]){
				contained = true;
				break;
			}
		}
		if (!contained){
			D_unique_vect.push_back(_DOD_vect[i]);
		}
	}
	n = D_unique_vect.size();

	if (n > 1)
	{
		// get where DOD is bracketed [D_lo, DOD, D_hi]
		double D_lo = 0;
		double D_hi = 100;

		for (int i = 0; i < _DOD_vect.size(); i++)
		{
			D = _DOD_vect[i];
			if (D < DOD && D > D_lo)
				D_lo = D;
			else if (D > DOD && D < D_hi)
				D_hi = D;
		}

		// Seperate table into bins
		double D_min = 100.;
		double D_max = 0.;

		for (int i = 0; i < _DOD_vect.size(); i++)
		{
			D = _DOD_vect[i];
			if (D == D_lo)
				low_indices.push_back(i);
			else if (D == D_hi)
				high_indices.push_back(i);

			if (D < D_min){ D_min = D; }
			else if (D > D_max){ D_max = D; }
		}
		size_t n_rows_lo = low_indices.size();
		size_t n_rows_hi = high_indices.size();
		size_t n_cols = 2;

		// If we aren't bounded, fill in values
		if (n_rows_lo == 0)
		{
			// Assumes 0% DOD
			for (int i = 0; i < n_rows_hi; i++)
			{
				C_n_low_vect.push_back(0. + i * 500); // cycles
				C_n_low_vect.push_back(100.); // 100 % capacity
			}
		}
		else if (n_rows_hi == 0)
		{
			// Assume 100% DOD
			for (int i = 0; i < n_rows_lo; i++)
			{
				C_n_high_vect.push_back(100. + i * 500); // cycles
				C_n_high_vect.push_back(0.); // 100 % capacity
			}
		}

		if (n_rows_lo != 0)
		{
			for (int i = 0; i < n_rows_lo; i++)
			{
				C_n_low_vect.push_back(_cycles_vect[low_indices[i]]);
				C_n_low_vect.push_back(_capacities_vect[low_indices[i]]);
			}
		}
		if (n_rows_hi != 0)
		{
			for (int i = 0; i < n_rows_hi; i++)
			{
				C_n_high_vect.push_back(_cycles_vect[high_indices[i]]);
				C_n_high_vect.push_back(_capacities_vect[high_indices[i]]);
			}
		}
		n_rows_lo = C_n_low_vect.size() / n_cols;
		n_rows_hi = C_n_high_vect.size() / n_cols;

		if (n_rows_lo == 0 || n_rows_hi == 0)
		{
			// need a safeguard here
		}

		util::matrix_t<double> C_n_low(n_rows_lo, n_cols, &C_n_low_vect);
		util::matrix_t<double> C_n_high(n_rows_lo, n_cols, &C_n_high_vect);

		// Compute C(D_lo, n), C(D_hi, n)
		double C_Dlo = util::linterp_col(C_n_low, 0, cycle_number, 1);
		double C_Dhi = util::linterp_col(C_n_high, 0, cycle_number, 1);

		if (C_Dlo < 0.)
			C_Dlo = 0.;
		if (C_Dhi > 100.)
			C_Dhi = 100.;

		// Interpolate to get C(D, n)
		C = util::interpolate(D_lo, C_Dlo, D_hi, C_Dhi, DOD);
	}
	// just have one row, single level interpolation
	else
	{
		C = util::linterp_col(_batt_lifetime_matrix, 1, cycle_number, 2);
	}

	return C;
}


/*
Define Thermal Model
*/
thermal_t::thermal_t(double mass, double length, double width, double height, 
	double Cp,  double h, double T_room, 
	const util::matrix_t<double> &c_vs_t )
{
	_cap_vs_temp = c_vs_t;
	_mass = mass;
	_length = length;
	_width = width;
	_height = height;
	_Cp = Cp;
	_h = h;
	_T_room = T_room;
	_R = 0.004;
	_capacity_percent = 100;

	// assume all surfaces are exposed
	_A = 2 * (length*width + length*height + width*height);

	// initialize to room temperature
	_T_battery = T_room;

	// curve fit
	int n = _cap_vs_temp.nrows();
	for (int i = 0; i < n; i++)
	{
		_cap_vs_temp(i,0) += 273.15; // convert C to K
	}
}
void thermal_t::replace_battery()
{ 
	_T_battery = _T_room; 
	_capacity_percent = 100.;
}

#define HR2SEC 3600.0


void thermal_t::updateTemperature(double I, double R, double dt)
{
	_R = R;
	double T_new = _T_battery;

	// use RK4 iff timestep is 5 minutes or less
	if (dt <= 5./60)
		T_new = rk4(I, dt*HR2SEC);
	else
		T_new = trapezoidal(I, dt*HR2SEC);

	_T_battery = T_new;
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
double thermal_t::trapezoidal(double I, double dt)
{
	double B = 1 / (_mass*_Cp); // [K/J]
	double C = _h*_A;			// [W/K]
	double D = pow(I, 2)*_R;	// [Ohm A*A]
	double T_prime = f(_T_battery, I);	// [K]

	return (_T_battery + 0.5*dt*(T_prime + B*(C*_T_room + D))) / (1 + 0.5*dt*B*C);
}
double thermal_t::T_battery(){ return _T_battery; }
double thermal_t::capacity_percent()
{ 
	return util::linterp_col(_cap_vs_temp, 0, _T_battery, 1); 
}
/*
Define Losses
*/
losses_t::losses_t(lifetime_t * lifetime, thermal_t * thermal, capacity_t* capacity)
{
	_lifetime = lifetime;
	_thermal = thermal;
	_capacity = capacity;
	_nCycle = 0;
}
void losses_t::replace_battery(){ _nCycle = 0; }
void losses_t::run_losses(double dt_hour)
{
	bool update_max_capacity = false;
	
	// if cycle number has changed, update max capacity
	if (_lifetime->cycles_elapsed() > _nCycle)
	{
		_nCycle++;
		update_max_capacity = true;
	}
	
	// apply losses only on discharge
	if (_capacity->I() > 0)
	{
		_capacity->updateCapacityForThermal(_thermal->capacity_percent());
		_capacity->updateCapacityForLifetime(_lifetime->capacity_percent(), update_max_capacity);
	}
}
/* 
Define Battery 
*/
battery_t::battery_t(){};
battery_t::battery_t(double dt_hour, int battery_chemistry)
{
	_dt_hour = dt_hour;
	_dt_min = dt_hour * 60;
	_battery_chemistry = battery_chemistry;
}
void battery_t::initialize(capacity_t *capacity, voltage_t * voltage, lifetime_t * lifetime, thermal_t * thermal, losses_t * losses)
{
	_capacity = capacity;
	_lifetime = lifetime;
	_voltage = voltage;
	_thermal = thermal;
	_losses = losses;
	_firstStep = true;
}

void battery_t::run(double I)
{	
	// Compute temperature at end of timestep
	runThermalModel(I);
	runCapacityModel(I);
	runVoltageModel();

	if (_capacity->chargeChanged())
		runLifetimeModel(_capacity->prev_DOD());
	else if (_firstStep)
	{
		runLifetimeModel(_capacity->DOD());
		_firstStep = false;
	}

	runLossesModel();
}
void battery_t::runThermalModel(double I)
{
	_thermal->updateTemperature(I, _voltage->R(), _dt_hour);
}

void battery_t::runCapacityModel(double I)
{
	_capacity->updateCapacity(I, _dt_hour );
}

void battery_t::runVoltageModel()
{
	_voltage->updateVoltage(_capacity, _dt_hour);
}

void battery_t::runLifetimeModel(double DOD)
{
	_lifetime->rainflow(DOD);
	if (_lifetime->check_replaced())
	{
		_capacity->replace_battery();
		_thermal->replace_battery();
		_losses->replace_battery();
	}
}
void battery_t::runLossesModel()
{
	_losses->run_losses(_dt_hour);
}
capacity_t * battery_t::capacity_model()
{
	return _capacity;
}
voltage_t * battery_t::voltage_model()
{
	return _voltage;
}
lifetime_t * battery_t::lifetime_model()
{
	return _lifetime;
}
double battery_t::battery_charge_needed()
{
	double charge_needed = _capacity->qmax() - _capacity->q0();
	if (charge_needed > 0)
		return charge_needed;
	else
		return 0.;
}

double battery_t::battery_charge_total(){return _capacity->q0();}
double battery_t::battery_charge_maximum(){ return _capacity->qmax(); }
double battery_t::cell_voltage(){ return _voltage->cell_voltage();}
double battery_t::battery_voltage(){ return _voltage->battery_voltage();}

/*
Dispatch base class
*/
dispatch_t::dispatch_t(battery_t * Battery, double dt_hour, double SOC_min, double Ic_max, double Id_max, double t_min, bool ac_or_dc, double dc_dc, double ac_dc, double dc_ac)
{
	_Battery = Battery;
	_dt_hour = dt_hour;
	_SOC_min = SOC_min;
	_Ic_max = Ic_max;
	_Id_max = Id_max;
	_t_min = t_min;
	_ac_or_dc = ac_or_dc;
	_dc_dc = dc_ac;
	_ac_dc = ac_dc;
	_dc_ac = dc_ac;

	// positive quantities describing how much went to load
	_pv_to_load = 0.;
	_battery_to_load = 0.;
	_grid_to_load = 0.;

	// positive or negative quantities describing net power flows
	// note, do not include pv, since we don't modify from pv module
	_e_tofrom_batt = 0.;
	_e_grid = 0.;
	_e_gen = 0.;

	// limit the switch from charging to discharge so that doesn't flip-flop subhourly
	_t_at_mode = 1000; 
	_prev_charging = false;
	_charging = false;
	_e_max_discharge = 0.;

	// efficiency
	_charge_accumulated = _Battery->battery_charge_total()*_Battery->battery_voltage()*watt_to_kilowatt;
	_discharge_accumulated = 0.;
	_average_efficiency = 100.;
}
double dispatch_t::energy_tofrom_battery(){ return _e_tofrom_batt; };
double dispatch_t::energy_tofrom_grid(){ return _e_grid; };
double dispatch_t::pv_to_load(){ return _pv_to_load; };
double dispatch_t::battery_to_load(){ return _battery_to_load; };
double dispatch_t::grid_to_load(){ return _grid_to_load; };
double dispatch_t::gen(){ return _e_gen; }
double dispatch_t::average_efficiency(){ return _average_efficiency; }



void dispatch_t::SOC_controller(double battery_voltage, double charge_total, double charge_max, double percent_discharge)
{
	// Implement minimum SOC cut-off
	if (_e_tofrom_batt > 0.0001)
	{
		_charging = false;
		double e_max_discharge = battery_voltage *(charge_total - charge_max*_SOC_min*0.01)*watt_to_kilowatt;
		if (fabs(_e_tofrom_batt) > e_max_discharge)
			_e_tofrom_batt = e_max_discharge;

		if (_charging != _prev_charging)
			_e_max_discharge = e_max_discharge;

		// implement discharge percent
		double e_percent = _e_max_discharge*percent_discharge*0.01;

		if (_e_tofrom_batt > e_percent)
			_e_tofrom_batt = e_percent;
	}
	else if (_e_tofrom_batt < -0.0001)
		_charging = true;
	else
		_charging = _prev_charging;
}
void dispatch_t::switch_controller()
{
	// Implement rapid switching check
	if (_charging != _prev_charging)
	{
		if (_t_at_mode <= _t_min)
		{
			_e_tofrom_batt = 0.;
			_charging = _prev_charging;
			_t_at_mode += round(_dt_hour * hour_to_min);
		}
		else
			_t_at_mode = 0.;
	}
	_t_at_mode += round(_dt_hour * hour_to_min);

}
double dispatch_t::current_controller(double battery_voltage)
{
	// Implement current limits
	double P, I = 0.; // [W],[V]
	P = kilowatt_to_watt*_e_tofrom_batt / _dt_hour;
	I = P / battery_voltage;
	if (_charging)
	{
		if (fabs(I) > _Ic_max)
			I = -_Ic_max;
	}
	else
	{
		if (I > _Id_max)
			I = _Id_max;
	}
	return I;
}
void dispatch_t::compute_efficiency()
{
	// average cycle efficiency
	if (_e_tofrom_batt > 0.)
		_discharge_accumulated += _e_tofrom_batt;
	else if (_e_tofrom_batt < 0.)
		_charge_accumulated += (-_e_tofrom_batt);

	_average_efficiency = 100.*(_discharge_accumulated / _charge_accumulated);

	// update for next step
	_prev_charging = _charging;
}
double dispatch_t::conversion_loss_in(double I)
{
	if (_ac_or_dc == 0)
		I*=_dc_dc*0.01;
	else
		I*=_ac_dc*0.01;
	return I;
}
double dispatch_t::conversion_loss_out(double I)
{
	if (_ac_or_dc == 0)
		I*=_dc_dc*0.01;
	else
		I*=_dc_ac*0.01;
	return I;
}
/*
Manual Dispatch
*/
dispatch_manual_t::dispatch_manual_t(battery_t * Battery, double dt, double SOC_min, double Ic_max, double Id_max, double t_min, 
	bool ac_or_dc, double dc_dc, double ac_dc, double dc_ac,
	util::matrix_static_t<float, 12, 24> dm_sched, bool * dm_charge, bool *dm_discharge, bool * dm_gridcharge, std::map<int,double>  dm_percent_discharge)
	: dispatch_t(Battery, dt, SOC_min, Ic_max, Id_max, t_min, ac_or_dc, dc_dc, ac_dc, dc_ac)
{
	_sched = dm_sched;
	_charge_array = dm_charge;
	_discharge_array = dm_discharge;
	_gridcharge_array = dm_gridcharge;
	_percent_discharge_array = dm_percent_discharge;
}
void dispatch_manual_t::dispatch(size_t hour_of_year, double e_pv, double e_load)
{
	int m, h;
	int iprofile = -1;
	getMonthHour(hour_of_year, &m, &h);
	iprofile = _sched(m - 1, h - 1) - 1;
	//if (iprofile < 0 || iprofile > 3) throw compute_module::exec_error("battery", "invalid battery dispatch schedule profile [0..3] ok");

	_can_charge = _charge_array[iprofile];
	_can_discharge = _discharge_array[iprofile];
	_can_grid_charge = _gridcharge_array[iprofile];
	_percent_discharge = 0.;
	if (_can_discharge){ _percent_discharge = _percent_discharge_array[iprofile]; }

	// current charge state of battery from last time step.  
	double battery_voltage = _Battery->battery_voltage();								// [V] 
	double chargeNeededToFill = _Battery->battery_charge_needed();						// [Ah] - qmax - q0
	double energyNeededToFill = (chargeNeededToFill * battery_voltage)*watt_to_kilowatt;// [kWh]
	double charge_total = _Battery->battery_charge_total();								// [Ah]
	double charge_max = _Battery->battery_charge_maximum();								// [Ah]

	_e_grid = 0.;																		// [KWh] energy needed from grid to charge battery.  Positive indicates sending to grid.  Negative pulling from grid.
	_e_tofrom_batt = 0.;																// [KWh] energy transferred to/from the battery.     Positive indicates discharging, Negative indicates charging
	_pv_to_load = 0.;
	_battery_to_load = 0.;
	_grid_to_load = 0.;
	_charging = true;

	// Is there extra energy from array
	if (e_pv > e_load)
	{
		if (_can_charge)
		{
			// use all energy available, it will only use what it can handle
			_e_tofrom_batt = -(e_pv - e_load);

			if ( (e_pv - e_load < energyNeededToFill) && _can_grid_charge)
				_e_tofrom_batt = -energyNeededToFill;
		}
		// if we want to charge from grid without charging from array
		else if (_can_grid_charge)
			_e_tofrom_batt = -energyNeededToFill;
	}
	// Or, is the demand greater than or equal to what the array provides
	else if (e_load >= e_pv)
	{
		// try to discharge full amount.  Will only use what battery can provide
		if (_can_discharge)
			_e_tofrom_batt = e_load - e_pv;
		// if we want to charge from grid
		// this scenario doesn't really make sense
		else if (_can_grid_charge)
			_e_tofrom_batt = -energyNeededToFill;
	}

	// Controllers
	SOC_controller(battery_voltage, charge_total, charge_max, _percent_discharge);
	switch_controller();
	double I = current_controller(battery_voltage);

	// Apply conversion loss on AC or DC side
	I = conversion_loss_in(I);

	// Run Battery Model to update charge based on charge/discharge
	_Battery->run(I);

	// Update how much power was actually used to/from battery
	I = _Battery->capacity_model()->I();
	I = conversion_loss_out(I);
	double battery_voltage_new = _Battery->voltage_model()->battery_voltage();
	_e_tofrom_batt = I * 0.5*(battery_voltage + battery_voltage_new)* _dt_hour * watt_to_kilowatt;// [kWh]

	// compute internal round-trip efficiency
	compute_efficiency();
	
	// Update net grid energy
	// e_tofrom_batt > 0 -> more energy available to send to grid or meet load (discharge)
	// e_grid > 0 (sending to grid) e_grid < 0 (pulling from grid)
	_e_gen = e_pv + _e_tofrom_batt;
	_e_grid = _e_gen - e_load;

	// Next, get how much of each component will meet the load.  
	// PV always meets load before battery
	if (e_pv > e_load)
		_pv_to_load = e_load;
	else
	{
		_pv_to_load = e_pv;
		
		if (_e_tofrom_batt > 0)
			_battery_to_load = _e_tofrom_batt;

		_grid_to_load = e_load - (_pv_to_load + _battery_to_load);
	}
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
