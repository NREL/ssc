#include <math.h>
#include <cmath>
#include <cfloat>
#include <sstream>

#include "lib_battery.h"

/*
Message class
*/
void message::add(std::string message)
{
	std::vector<std::string>::iterator it;
	it = std::find(messages.begin(), messages.end(), message);
	if (it == messages.end())
	{
		messages.push_back(message);
		count.push_back(1);
	}
	else
		count[it - messages.begin()]++;

}
int message::total_message_count(){ return messages.size(); }
size_t message::message_count(int index)
{
	if (index < messages.size())
		return count[index];
	else
		return 0;
}
std::string message::get_message(int index)
{
	if (index < messages.size())
		return messages[index];
	else
		return NULL;
}
std::string message::construct_log_count_string(int index)
{
    std::string message_count = static_cast<std::ostringstream*>(&(std::ostringstream() << count[index]))->str();
//    std::string message_count = static_cast<std::ostringstream>((std::ostringstream() << count[index])).str();
    std::string log = messages[index] + " - warning occurred: " + message_count + " times";
	return log;
}

/* 
Define Capacity Model 
*/

capacity_t::capacity_t(double q, double SOC_max)
{
	_q0 = 0.01*SOC_max*q;
	_qmax = q;
	_qmax0 = q;
	_I = 0.;
	_I_loss = 0.;
	_dt_hour = 0.;

	// Initialize SOC, DOD
	_SOC = SOC_max;
	_SOC_max = SOC_max;
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
	if ((charging != _prev_charge) && (charging != NO_CHARGE) && (_prev_charge != NO_CHARGE)  )
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
	if (_SOC > _SOC_max)
		_SOC = _SOC_max;
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
double capacity_t::I_loss() { return _I_loss; }

/*
Define KiBam Capacity Model
*/
capacity_kibam_t::capacity_kibam_t(double q20, double t1, double q1, double q10, double SOC_max) :
capacity_t(q20, SOC_max)
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
	// Assume initial charge is max capacity
	_q0 = _qmax*_SOC_max*0.01;
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
	_I_loss = 0.;
	_I = I;
	_dt_hour = dt_hour;

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

	// potentially a bug that needs to be fixed, for now hack
	if (q1 + q2 > _qmax)
	{
		double q0 = q1 + q2;
		double p1 = q1 / q0;
		double p2 = q2 / q0;
		_q0 = _qmax;
		q1 = _q0*p1;
		q2 = _q0*p2;
	}

	// update internal variables 
	_q1_0 = q1;
	_q2_0 = q2;
	_q0 = q1 + q2;

	update_SOC();
	check_charge_change(); 
}
void capacity_kibam_t::updateCapacityForThermal(double capacity_percent)
{
	double qmax_tmp = _qmax*capacity_percent*0.01;
	if (_q0 > qmax_tmp)
	{
		double q0_orig = _q0;
		double p = qmax_tmp / _q0;
		_q0 *= p;
		_q1 *= p;
		_q2 *= p;
		_I_loss += (q0_orig - _q0) / _dt_hour;
		_I += (_q0 - qmax_tmp) / _dt_hour;
	}
	update_SOC();
}
void capacity_kibam_t::updateCapacityForLifetime(double capacity_percent)
{

	if (_qmax0* capacity_percent*0.01 <= _qmax)
		_qmax = _qmax0* capacity_percent*0.01;

	// scale to q0 = qmax if q0 > qmax
	if (_q0 > _qmax)
	{
		double q0_orig = _q0;
		double p = _qmax / _q0;
		_q0 *= p;
		_q1 *= p;
		_q2 *= p;
		_I_loss += (q0_orig - _q0) / _dt_hour;
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
capacity_lithium_ion_t::capacity_lithium_ion_t(double q, double SOC_max) :capacity_t(q, SOC_max){};
capacity_lithium_ion_t::~capacity_lithium_ion_t(){}
void capacity_lithium_ion_t::replace_battery()
{
	_q0 = _qmax0;
	_qmax = _qmax0;
}
void capacity_lithium_ion_t::updateCapacity(double I, double dt)
{
	_DOD_prev = _DOD;
	_I_loss = 0.;
	_dt_hour = dt;
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
	double qmax_tmp = _qmax*capacity_percent*0.01;
	if (_q0 > qmax_tmp)
	{
		_I_loss += (_q0 - qmax_tmp) / _dt_hour;
		_I += (_q0 - qmax_tmp) / _dt_hour;
		_q0 = qmax_tmp;
	}
	update_SOC();

}
void capacity_lithium_ion_t::updateCapacityForLifetime(double capacity_percent)
{

	if (_qmax0* capacity_percent*0.01 <= _qmax)
		_qmax = _qmax0* capacity_percent*0.01;
	
	if (_q0 > _qmax)
	{
		_I_loss += (_q0 - _qmax) / _dt_hour;
		_q0 = _qmax;
	}

	update_SOC();
}
double capacity_lithium_ion_t::q1(){return _q0;}
double capacity_lithium_ion_t::q10(){return _qmax;}


/*
Define Voltage Model
*/
voltage_t::voltage_t(int num_cells_series, int num_strings, double voltage)
{
	_num_cells_series = num_cells_series;
	_num_strings = num_strings;
	_cell_voltage = voltage;
	_R = 0.004; // just a default, will get recalculated upon construction
}

double voltage_t::battery_voltage(){ return _num_cells_series*_cell_voltage; }
double voltage_t::cell_voltage(){ return _cell_voltage; }
double voltage_t::R(){ return _R; }


// Dynamic voltage model
voltage_dynamic_t::voltage_dynamic_t(int num_cells_series, int num_strings, double voltage, double Vfull, double Vexp, double Vnom, double Qfull, double Qexp, double Qnom, double C_rate, double R):
voltage_t(num_cells_series, num_strings, voltage)
{
	_Vfull = Vfull;
	_Vexp = Vexp;
	_Vnom = Vnom;
	_Qfull = Qfull;
	_Qexp = Qexp;
	_Qnom = Qnom;
	_C_rate = C_rate;
	_R = R;

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
	//_R = _Vnom*(1. - eta) / (_C_rate*_Qnom); // [Ohm]
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
	
	// is on a per-cell basis.
	// I, Q, q0 are on a per-string basis since adding cells in series does not change current or charge
	double cell_voltage = voltage_model_tremblay_hybrid(Q / _num_strings, I/_num_strings , q0 / _num_strings);

	// the cell voltage should not increase when the battery is discharging
	if (I <= 0 || (I > 0 && cell_voltage <= _cell_voltage) )
		_cell_voltage = cell_voltage;
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

lifetime_t::lifetime_t(const util::matrix_t<double> &batt_lifetime_matrix, const int replacement_option, const double replacement_capacity)
{
	_batt_lifetime_matrix = batt_lifetime_matrix;
	_replacement_option = replacement_option;
	_replacement_capacity = replacement_capacity;
	// issues as capacity approaches 0%
	if (replacement_capacity == 0.) { _replacement_capacity = 2.; }
	_replacements = 0;
	_replacement_scheduled = false;

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
	if ( (_replacement_option == 1 && _Clt <= _replacement_capacity) || _replacement_scheduled)
	{
		_replacements++;
		_Clt = bilinear(0.,0);
		_Dlt = 0.;
		_nCycles = 0;
		_jlt = 0;
		_Xlt = 0;
		_Ylt = 0;
		_Range = 0;
		_Peaks.clear();
		replaced = true;
		_replacement_scheduled = false;
	}
	return replaced;
}
void lifetime_t::force_replacement()
{
	_replacement_scheduled = true;
}

void lifetime_t::reset_replacements(){ _replacements = 0; }
int lifetime_t::replacements(){ return _replacements; }
int lifetime_t::cycles_elapsed(){return _nCycles;}
double lifetime_t::capacity_percent(){ return _Clt; }
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
				C_n_high_vect.push_back(80 - i*10); // % capacity
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

	//initialize maximum temperature
	_T_max = 400.;

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
	if (trapezoidal(I, dt*HR2SEC) < _T_max && trapezoidal(I, dt*HR2SEC) > 0)
		_T_battery = trapezoidal(I, dt*HR2SEC);
	else if (rk4(I, dt*HR2SEC) < _T_max && rk4(I, dt*HR2SEC) > 0)
		_T_battery = rk4(I, dt*HR2SEC);
	else if (implicit_euler(I, dt*HR2SEC) < _T_max && implicit_euler(I, dt*HR2SEC) > 0)
		_T_battery = implicit_euler(I, dt*HR2SEC);
	else
		_message.add("Computed battery temperature below zero or greater than max allowed, consider reducing C-rate");
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
double thermal_t::implicit_euler(double I, double dt)
{
	double B = 1 / (_mass*_Cp); // [K/J]
	double C = _h*_A;			// [W/K]
	double D = pow(I, 2)*_R;	// [Ohm A*A]
	double T_prime = f(_T_battery, I);	// [K]

	return (_T_battery + dt*(B*C*_T_room + D)) / (1 + dt*B*C);
}
double thermal_t::T_battery(){ return _T_battery; }
double thermal_t::capacity_percent()
{ 
	double percent = util::linterp_col(_cap_vs_temp, 0, _T_battery, 1); 

	if (percent < 0 || percent > 100)
	{
		percent = 100;
		_message.add("Unable to determine capacity adjustment for temperature, ignoring");
	}

	return percent;
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
		_capacity->updateCapacityForLifetime(_lifetime->capacity_percent());
	}
	
	// modify max capacity based on temperature
	_capacity->updateCapacityForThermal(_thermal->capacity_percent());
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
dispatch_t::dispatch_t(battery_t * Battery, double dt_hour, double SOC_min, double SOC_max, double Ic_max, double Id_max, 
	double t_min, bool ac_or_dc, double dc_dc, double ac_dc, double dc_ac, int mode, bool pv_dispatch)
{
	_Battery = Battery;
	_dt_hour = dt_hour;
	_SOC_min = SOC_min;
	_SOC_max = SOC_max;
	_Ic_max = Ic_max;
	_Id_max = Id_max;
	_I_loss = 0;
	_t_min = t_min;
	_ac_or_dc = ac_or_dc;
	_dc_dc = dc_dc;
	_ac_dc = ac_dc;
	_dc_ac = dc_ac;
	_mode = mode; 
	_pv_dispatch_to_battery_first = pv_dispatch;

	// positive quantities describing how much went to load
	_pv_to_load = 0.;
	_battery_to_load = 0.;
	_grid_to_load = 0.;

	// positive or negative quantities describing net power flows
	// note, do not include pv, since we don't modify from pv module
	_e_tofrom_batt = 0.;
	_e_grid = 0.;
	_e_gen = 0.;
	_battery_fraction = 0.;
	_pv_fraction = 0.;
	_pv_to_batt = 0.;
	_grid_to_batt = 0.;
	_batt_charge_loss = 0.;
	_batt_discharge_loss = 0.;
	_pv_loss = 0.;

	// limit the switch from charging to discharge so that doesn't flip-flop subhourly
	_t_at_mode = 1000; 
	_prev_charging = false;
	_charging = false;
	_e_max_discharge = Battery->battery_voltage()*(Battery->battery_charge_total() - Battery->battery_charge_maximum()*SOC_min*0.01)*watt_to_kilowatt;
	_e_max_charge = Battery->battery_voltage()*(Battery->battery_charge_total() - Battery->battery_charge_maximum()*SOC_max*0.01)*watt_to_kilowatt;
	_grid_recharge = false;

	// single value metrics
	_charge_accumulated = _Battery->battery_charge_total()*_Battery->battery_voltage()*watt_to_kilowatt;
	_charge_from_pv = 0.;
	_charge_from_grid = _charge_accumulated; // assumes initial charge from grid
	_discharge_accumulated = 0.;
	_average_efficiency = 100.;
	_pv_charge_percent = 0.;

	// annual metrics
	_charge_from_pv_annual = 0.;
	_charge_from_grid_annual =_charge_from_grid;
	_charge_annual = _charge_accumulated;
	_discharge_annual = 0.;
	_grid_import_annual = 0.;
	_grid_export_annual = 0.;
	_e_loss_annual = 0.;
}
double dispatch_t::energy_tofrom_battery(){ return _e_tofrom_batt; };
double dispatch_t::energy_tofrom_grid(){ return _e_grid; };
double dispatch_t::pv_to_load(){ return _pv_to_load; };
double dispatch_t::battery_to_load(){ return _battery_to_load; };
double dispatch_t::grid_to_load(){ return _grid_to_load; };
double dispatch_t::pv_to_batt(){ return _pv_to_batt; };
double dispatch_t::grid_to_batt(){ return _grid_to_batt; };
double dispatch_t::gen(){ return _e_gen; }
double dispatch_t::average_efficiency(){ return _average_efficiency; }
double dispatch_t::pv_charge_percent(){ return _pv_charge_percent; }
double dispatch_t::pv_charge_annual(){ return _charge_from_pv_annual; }
double dispatch_t::grid_charge_annual(){ return _charge_from_grid_annual; }
double dispatch_t::charge_annual(){ return _charge_annual; }
double dispatch_t::discharge_annual(){ return _discharge_annual; }
double dispatch_t::grid_import_annual(){ return _grid_import_annual; }
double dispatch_t::grid_export_annual(){ return _grid_export_annual; }
double dispatch_t::energy_loss_annual(){ return _e_loss_annual; };
message dispatch_t::get_messages(){ return _message; };

void dispatch_t::new_year()
{
	_charge_from_pv_annual = 0.;
	_charge_from_grid_annual = 0;
	_charge_annual = 0.;
	_discharge_annual = 0.;
	_grid_import_annual = 0.;
	_grid_export_annual = 0.;
	_e_loss_annual = 0.;
}


void dispatch_t::SOC_controller(double battery_voltage, double charge_total, double charge_max)
{
	// Implement minimum SOC cut-off
	if (_e_tofrom_batt > 0)
	{
		_charging = false;
		double e_max_discharge = battery_voltage *(charge_total - charge_max*_SOC_min*0.01)*watt_to_kilowatt;
		
		if (e_max_discharge < 0)
			e_max_discharge = 0;

		if (_e_tofrom_batt > e_max_discharge)
			_e_tofrom_batt = e_max_discharge;

		if (_charging != _prev_charging)
			_e_max_discharge = e_max_discharge;

		//  discharge percent
		double e_percent = _e_max_discharge*_percent_discharge*0.01;

		if (_e_tofrom_batt > e_percent)
			_e_tofrom_batt = e_percent;
	}
	// Maximum SOC cut-off
	else if (_e_tofrom_batt < 0)
	{
		_charging = true;
		double e_max_charge = battery_voltage*(charge_total - charge_max*_SOC_max*0.01)*watt_to_kilowatt;
		if (e_max_charge > 0)
			e_max_charge = 0;

		if (_e_tofrom_batt < e_max_charge)
			_e_tofrom_batt = e_max_charge;

		if (_charging != _prev_charging)
			_e_max_charge = e_max_charge;

		//  charge percent for automated grid charging
		double e_percent = _e_max_charge*_percent_charge*0.01;

		if (fabs(_e_tofrom_batt) > fabs(e_percent))
			_e_tofrom_batt = e_percent;
	}
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
void dispatch_t::accumulate_charge()
{
	if (_e_tofrom_batt < 0.)
	{
		_charge_accumulated += (-_e_tofrom_batt);
		_charge_annual += (-_e_tofrom_batt);
	}
}
void dispatch_t::accumulate_discharge()
{
	if (_e_tofrom_batt > 0.)
	{
		_discharge_accumulated += _e_tofrom_batt;
		_discharge_annual += _e_tofrom_batt;
	}
}
void dispatch_t::compute_metrics()
{	
	if (_e_tofrom_batt < 0.)
	{
		_charge_from_pv += _pv_to_batt;
		_charge_from_pv_annual += _pv_to_batt;
		_charge_from_grid += _grid_to_batt;
		_charge_from_grid_annual += _grid_to_batt;
	}
	_average_efficiency = 100.*(_discharge_accumulated / _charge_accumulated);
	_pv_charge_percent = 100.*(_charge_from_pv / _charge_accumulated);

	// update for next step
	_prev_charging = _charging;
}
void dispatch_t::conversion_loss_in(double &I, double multiplier)
{
	// currently set for AC/DC conversion loss
	// in DC connected system this could be charge controller conversion loss
	double I_in = I; 
	if (_ac_or_dc == 0)
		I*=_dc_dc*0.01;
	else
		I*=_ac_dc*0.01;
	_I_loss += fabs(I_in - I);
	_batt_charge_loss = _I_loss*multiplier;
}
void dispatch_t::conversion_loss_out(double &I, double multiplier)
{
	double I_in = I;
	if (_ac_or_dc == 0)
		I*=_dc_dc*0.01;
	else
		I*=_dc_ac*0.01;
	_I_loss += fabs(I_in - I);
	_batt_discharge_loss = _I_loss*multiplier;
}
void dispatch_t::compute_loss(double I, double battery_voltage, double battery_voltage_new)
{
	double multiplier = 0.5*(battery_voltage + battery_voltage_new)* _dt_hour * watt_to_kilowatt;

	// energy to battery already includes internal losses, add in conversion losses
	if (_charging) 
		conversion_loss_in(I, multiplier);
	else
		conversion_loss_out(I, multiplier);
	
	// post-loss energy
	_e_tofrom_batt = I*multiplier;

	// Add internal capacity losses due to lifetime and thermal effects onto conversion losses
	 // This would be a way to isolate voltage losses, conversion losses, and internal losses
	// _I_loss += _Battery->capacity_model()->I_loss();
	// _e_loss_annual += fabs(_I_loss*multiplier); // [kWh]

	_e_loss_annual = _charge_annual - _discharge_annual;
}
void dispatch_t::accumulate_grid_annual()
{
	// e_grid > 0 (export to grid) 
	// e_grid < 0 (import from grid)

	if (_e_grid > 0)
		_grid_export_annual += _e_grid;
	else
		_grid_import_annual += (-_e_grid);
}
void dispatch_t::compute_to_batt(double e_pv)
{
	// potential energy to battery after conversion loss
	double e_pv_dc = e_pv * _ac_dc * 0.01;

	// Compute how much power went to battery from each component
	if (_e_tofrom_batt < 0)
	{
		if (_pv_to_batt > 0)
		{
			// in event less energy dispatched than requested
			if (_pv_to_batt > fabs(_e_tofrom_batt))
				_pv_to_batt = fabs(_e_tofrom_batt);

			// in event more energy dispatched than requested
			if (_pv_dispatch_to_battery_first)
			{
				if (_pv_to_batt < fabs(_e_tofrom_batt))
				{
					// accounts for conversion loss
					if (_pv_to_batt < e_pv_dc)
					{
						_pv_to_batt = e_pv_dc;

						if (_pv_to_batt > fabs(_e_tofrom_batt))
							_pv_to_batt = fabs(_e_tofrom_batt);
					}
				}
			}
		}
		if (_pv_to_batt < 0)
			_pv_to_batt = 0;

		_grid_to_batt = fabs(_e_tofrom_batt) - _pv_to_batt;

		if (_pv_to_batt == e_pv_dc)
			_pv_loss = e_pv - e_pv_dc;
		else
			_pv_loss = _pv_to_batt * (1 - _ac_dc * 0.01); // if AC-connected
	}
	else
	{
		_pv_to_batt = 0;
		_grid_to_batt = 0;
	}
}
void dispatch_t::compute_to_load(double e_pv, double e_load, double e_tofrom_battery)
{
	// Compute how much of each component will meet the load.  
	if (!_pv_dispatch_to_battery_first)
	{
		// PV meets load before battery
		if (e_pv > e_load)
			_pv_to_load = e_load;
		else
			_pv_to_load = e_pv;
	}
	else
	{
		// derate the PV energy available
		_pv_to_load = e_pv - (_pv_to_batt + _pv_loss);
	}

	if (_pv_to_load > e_load)
		_pv_to_load = e_load;

	if (_pv_to_load < 0)
		_pv_to_load = 0;

	if (_e_tofrom_batt > 0)
		_battery_to_load = e_tofrom_battery;

	// could have slightly more dispatched than needed
	if (_battery_to_load > e_load || (_battery_to_load + _pv_to_load > e_load))
		_battery_to_load = e_load - _pv_to_load;

	_grid_to_load = e_load - (_pv_to_load + _battery_to_load);
}
void dispatch_t::compute_generation(double e_pv)
{
	_e_gen = e_pv + _e_tofrom_batt;

	if (_e_gen > 0)
	{
		_battery_fraction = _e_tofrom_batt / _e_gen;
		_pv_fraction = e_pv / _e_gen;
	}
	else
	{
		_battery_fraction = 0.;
		_pv_fraction = 0.;
	}
}
void dispatch_t::compute_grid_net(double e_gen, double e_load)
{
	double e_pv = e_gen;
	double e_tofrom_battery = _e_tofrom_batt;
	_e_grid = e_pv + _e_tofrom_batt - e_load;

	// dc connected - need to revisit
	if (_ac_or_dc == 0)
	{
		e_pv = e_gen*_pv_fraction;
		e_tofrom_battery = e_gen*_battery_fraction;
	}

	compute_to_batt(e_pv);
	compute_to_load(e_pv, e_load, e_tofrom_battery);

	accumulate_grid_annual();
}
/*
Manual Dispatch
*/
dispatch_manual_t::dispatch_manual_t(battery_t * Battery, double dt, double SOC_min, double SOC_max, double Ic_max, double Id_max, 
	double t_min, bool ac_or_dc, double dc_dc, double ac_dc, double dc_ac,
	int mode, bool pv_dispatch,
	util::matrix_t<float> dm_dynamic_sched,  util::matrix_t<float> dm_dynamic_sched_weekend,
	bool * dm_charge, bool *dm_discharge, bool * dm_gridcharge, std::map<int, double>  dm_percent_discharge, std::map<int, double>  dm_percent_gridcharge)
	: dispatch_t(Battery, dt, SOC_min, SOC_max, Ic_max, Id_max, 
				t_min, ac_or_dc, dc_dc, ac_dc, dc_ac, mode, pv_dispatch)
{
	_sched = dm_dynamic_sched;
	_sched_weekend = dm_dynamic_sched_weekend;
	for (int i = 0; i != 6; i++)
	{
		_charge_array.push_back(dm_charge[i]);
		_discharge_array.push_back(dm_discharge[i]);
		_gridcharge_array.push_back(dm_gridcharge[i]);
	}
	_percent_discharge_array = dm_percent_discharge; 
	_percent_charge_array = dm_percent_gridcharge;
}
void dispatch_manual_t::initialize_dispatch(size_t hour_of_year, size_t step)
{
	int m, h, column;
	int iprofile = -1;
	util::month_hour(hour_of_year, m, h);
	bool is_weekday = util::weekday(hour_of_year);
	_mode == MANUAL ? column = h - 1 : column = (h - 1) / _dt_hour + step;

	if (!is_weekday && _mode == MANUAL)
		iprofile = _sched_weekend(m - 1, column);
	else
		iprofile = _sched(m - 1, column);  // 1-based

	_can_charge = _charge_array[iprofile - 1];
	_can_discharge = _discharge_array[iprofile - 1];
	_can_grid_charge = _gridcharge_array[iprofile - 1];
	_percent_discharge = 0.;
	_percent_charge = 0.;

	if (_can_discharge){ _percent_discharge = _percent_discharge_array[iprofile]; }
	if (_can_charge){ _percent_charge = 100.; }
	if (_can_grid_charge){ _percent_charge = _percent_charge_array[iprofile]; }

	_I_loss = 0.;		      // [A] - The current reduction due to AC/DC conversion
	_e_grid = 0.;		      // [KWh] energy needed from grid to charge battery.  Positive indicates sending to grid.  Negative pulling from grid.
	_e_tofrom_batt = 0.;      // [KWh] energy transferred to/from the battery.     Positive indicates discharging, Negative indicates charging
	_pv_to_load = 0.;
	_battery_to_load = 0.;
	_grid_to_load = 0.;
	_pv_to_batt = 0.;
	_grid_to_batt = 0.;
	_batt_charge_loss = 0.;
	_batt_discharge_loss = 0.;
	_pv_loss = 0.;
	_charging = true;
}
void dispatch_manual_t::dispatch(size_t year, size_t hour_of_year, size_t step, double e_pv, double e_load)
{
	initialize_dispatch(hour_of_year, step);

	// current charge state of battery from last time step.  
	double battery_voltage = _Battery->battery_voltage();								         // [V] 
	double charge_needed_to_fill = _Battery->battery_charge_needed();						     // [Ah] - qmax - q0
	double energy_needed_to_fill = (charge_needed_to_fill * battery_voltage)*watt_to_kilowatt;   // [kWh]
	double charge_total = _Battery->battery_charge_total();								         // [Ah]
	double charge_max = _Battery->battery_charge_maximum();								         // [Ah]
	double I = 0.;															                     // [A] - The  current input/draw from battery after losses
	
	// Options for how to use PV
	if (!_pv_dispatch_to_battery_first)
		compute_energy_load_priority(e_pv, e_load, energy_needed_to_fill);
	else
		compute_energy_battery_priority(e_pv, e_load, energy_needed_to_fill);

	// Controllers
	SOC_controller(battery_voltage, charge_total, charge_max );
	switch_controller();
	I = current_controller(battery_voltage);

	// Run Battery Model to update charge based on charge/discharge
	_Battery->run(I);

	// Update how much power was actually used to/from battery
	I = _Battery->capacity_model()->I();
	double battery_voltage_new = _Battery->voltage_model()->battery_voltage();
	_e_tofrom_batt = I * 0.5*(battery_voltage + battery_voltage_new)* _dt_hour * watt_to_kilowatt;// [kWh]
	
	// Count charging before losses applied
	accumulate_charge();

	// Compute total losses
	compute_loss(I,battery_voltage, battery_voltage_new);
	compute_generation(e_pv);

	// if ac-connected, compute metrics for net grid, pv to load, batt to load, grid to load
	if (_ac_or_dc == 1)
		compute_grid_net(e_pv , e_load);
	// else dc connected, must compute post inverter

	// Count discharging after losses applied 
	accumulate_discharge();
	compute_metrics();

}
void dispatch_manual_t::compute_energy_load_priority(double e_pv, double e_load, double energy_needed)
{
	double diff = 0.; // [%]

	// potential energy to battery after conversion loss
	double e_pv_dc = e_pv * _ac_dc * 0.01;

	// Is there extra energy from array
	if (e_pv > e_load)
	{
		if (_can_charge)
		{
			// use all energy available, it will only use what it can handle
			_pv_to_batt = e_pv_dc - e_load;
			_e_tofrom_batt = -_pv_to_batt;

			if ((e_pv - e_load < energy_needed) && _can_grid_charge)
				_e_tofrom_batt = -energy_needed;
		}
		// if we want to charge from grid without charging from array
		else if (_can_grid_charge)
			_e_tofrom_batt = -energy_needed;
	}
	// Or, is the demand greater than or equal to what the array provides
	else if (e_load >= e_pv)
	{
		// try to discharge full amount.  Will only use what battery can provide
		if (_can_discharge)
		{
			_e_tofrom_batt = e_load - e_pv;
			diff = fabs(_Battery->capacity_model()->SOC() - _SOC_min);
			if ((diff < tolerance) || _grid_recharge)
			{
				if (_can_grid_charge)
				{
					_grid_recharge = true;
					_e_tofrom_batt = -energy_needed;
					diff = fabs(_Battery->capacity_model()->SOC() - _SOC_max);
					if (diff < tolerance)
						_grid_recharge = false;
				}
			}

		}
		// if we want to charge from grid
		else if (_can_grid_charge)
			_e_tofrom_batt = -energy_needed;
		else if (!_can_grid_charge)
			_grid_recharge = false;
	}
}
void dispatch_manual_t::compute_energy_battery_priority(double e_pv, double e_load, double energy_needed)
{
	double e_pv_dc = e_pv * _ac_dc * 0.01;

	double SOC = _Battery->capacity_model()->SOC();
	bool charged = (round(SOC) == _SOC_max);

	if (_can_charge && !charged > 0 && e_pv > 0)
	{
		if (e_pv_dc > energy_needed)
			_pv_to_batt = energy_needed;
		else
			_pv_to_batt = e_pv_dc;

		_e_tofrom_batt = -_pv_to_batt;

		if (_can_grid_charge)
			_e_tofrom_batt = -energy_needed;

	}
	else if (_can_grid_charge && !charged > 0)
		_e_tofrom_batt = -energy_needed;
	else if (_can_discharge && e_load > 0)
		_e_tofrom_batt = e_load - e_pv;
	
}
automate_dispatch_t::automate_dispatch_t(
	battery_t * Battery,
	double dt_hour,
	double SOC_min,
	double SOC_max,
	double Ic_max,
	double Id_max,
	double t_min,
	bool ac_or_dc,
	double dc_dc,
	double ac_dc,
	double dc_ac,
	int mode,
	bool pv_dispatch,
	util::matrix_t<float> dm_dynamic_sched,
	util::matrix_t<float> dm_dynamic_sched_weekend,
	bool * dm_charge,
	bool *dm_discharge,
	bool * dm_gridcharge,
	std::map<int, double> dm_percent_discharge,
	std::map<int, double> dm_percent_gridcharge,
	int nyears
	) : dispatch_manual_t(Battery, dt_hour, SOC_min, SOC_max, Ic_max, Id_max, t_min, ac_or_dc, dc_dc, ac_dc, dc_ac, mode, pv_dispatch,
	dm_dynamic_sched, dm_dynamic_sched_weekend, dm_charge, dm_discharge, dm_gridcharge, dm_percent_discharge, dm_percent_gridcharge)
{
	_hour_last_updated = -999;
	_dt_hour = dt_hour;
	_steps_per_hour = 1. / dt_hour;
	_nyears = nyears;
	_mode = mode;
	_num_steps = 24 * _steps_per_hour; // change if do look ahead of more than 24 hours
	_target_power_month = -1e16;
	_month = 1;
	grid.reserve(_num_steps);
	for (int ii = 0; ii != _num_steps; ii++)
		grid.push_back(grid_point(0.,0,0));
}
void automate_dispatch_t::dispatch(size_t year, size_t hour_of_year, size_t step, double e_pv, double e_load)
{
	int step_per_hour = 1 / _dt_hour; 
	int idx = 0;

	// look ahead
	if (_mode == LOOK_AHEAD || _mode == MAINTAIN_TARGET)
		idx = (year * 8760 + hour_of_year)*step_per_hour + step;
	// look behind
	else if (_mode == LOOK_BEHIND)
	{
		// start on day 2 
		bool first_day = (year == 0 && hour_of_year == 0);
		if ((hour_of_year) % 24 == 0 && (!first_day))
		{
			_pv.clear();
			_load.clear();
		}
		else
		{
			_pv.push_back(e_pv / _dt_hour);
			_load.push_back(e_load / _dt_hour);
		}
	}
	update_dispatch(hour_of_year, step, idx);
	dispatch_manual_t::dispatch(year, hour_of_year, step, e_pv, e_load);
}
void automate_dispatch_t::update_pv_load_data(std::vector<double> pv, std::vector<double> load)
{
	_pv = pv;
	_load = load;
}
int automate_dispatch_t::get_mode(){return _mode;}
void automate_dispatch_t::set_target_power(std::vector<double> target_power){ _target_power = target_power; }
void automate_dispatch_t::update_dispatch(int hour_of_year, int step, int idx)
{
	bool debug = false;
	FILE *p;
	check_debug(p,debug,hour_of_year, idx);
	
	if ( (hour_of_year) % 24 == 0 && hour_of_year != _hour_last_updated)
	{
		double E_useful;  // [kWh] - the cyclable energy available in the battery
		double E_max;     // [kWh] - the maximum energy that can be cycled
		double P_target;  // [kW] - the target power

		check_new_month(hour_of_year, step);

		// setup vectors
		initialize(hour_of_year);

		// compute grid power, sort highest to lowest
		sort_grid(p, debug, idx);

		// set period 1 as only PV charging
		int profile = 1;
		set_charge(profile);
		
		// Peak shaving scheme
		compute_energy(p, debug, E_max);
		target_power(p, debug, P_target, E_max, idx);

		// Set discharge, gridcharge profiles
		profile = set_discharge(p, debug, hour_of_year, P_target, E_max);
		set_gridcharge(p, debug, hour_of_year, profile, P_target, E_max);
	}
	
	if (debug)
		fclose(p);
}
void automate_dispatch_t::initialize(int hour_of_year)
{
	_hour_last_updated = hour_of_year;
	_charge_array.clear();
	_discharge_array.clear();
	_gridcharge_array.clear();	
	
	// clean up vectors
	for (int ii = 0; ii != _num_steps; ii++)
		grid[ii] = grid_point(0.,0,0);
}
void automate_dispatch_t::check_new_month(int hour_of_year, int step)
{
	int hours = 0;
	for (int month = 1; month <= _month; month++)
		hours += util::hours_in_month(month);

	if (hours == 8760)
		hours = 0;

	if ((hour_of_year == hours) && step == 0)
	{
		_target_power_month = -1e16;
		_month < 12 ? _month++ : _month = 1;
	}
}
void automate_dispatch_t::check_debug(FILE *&p, bool & debug, int hour_of_year, int idx)
{
	// for now, don't enable
	debug = false;

	if (hour_of_year == 4584 && hour_of_year != _hour_last_updated)
	{
		// debug = true;
		if (debug)
		{
			p = fopen("dispatch.txt", "w");
			fprintf(p, "Hour of Year: %d\t Hour Last Updated: %d \t Steps per Hour: %d\n", hour_of_year, _hour_last_updated, _steps_per_hour);
		}
		// failed for some reason
		if (p == NULL)
			debug = false;
	}
}

void automate_dispatch_t::sort_grid(FILE *p, bool debug, int idx )
{
	// compute grid net from pv and load (no battery)
	int count = 0;
	for (int hour = 0; hour != 24; hour++)
	{
		for (int step = 0; step != _steps_per_hour; step++)
		{
			grid[count] = grid_point(_load[idx] - _pv[idx], hour, step);
			idx++;
			count++;
		}
	}
	std::sort(grid.begin(), grid.end(), byGrid());
}

void automate_dispatch_t::compute_energy(FILE *p, bool debug, double & E_max )
{
	if (capacity_kibam_t * capacity = dynamic_cast<capacity_kibam_t *>(_Battery->capacity_model()))
	{
		E_max = _Battery->battery_voltage() *_Battery->capacity_model()->q1()*watt_to_kilowatt;
		if (E_max < 0)
			E_max = 0;
	}
	else
		E_max = _Battery->battery_voltage() *_Battery->battery_charge_maximum()*(_SOC_max-_SOC_min) *0.01 *watt_to_kilowatt;

	if (debug)
	{
		fprintf(p, "Energy Max: %.3f\t", E_max);
		fprintf(p, "Battery Voltage: %.3f\n", _Battery->battery_voltage());
	}
}

void automate_dispatch_t::target_power(FILE*p, bool debug, double & P_target, double E_useful, int idx)
{
	// if target power set, use that
	if (_target_power.size() > idx && _target_power[idx] > 0)
	{
		P_target = _target_power[idx];
		return;
	}

	// don't calculate if peak grid demand is less than a previous target in the month
	if (grid[0].Grid() < _target_power_month)
	{
		P_target = _target_power_month;
		return;
	}

	// First compute target power which will allow battery to charge up to E_useful over 24 hour period
	if (debug)
		fprintf(p, "Index\tRecharge_target\t charge_energy\n");

	double P_target_min = 1e16;
	double E_charge = 0.;
	int index = _num_steps - 1; 
	std::vector<double> E_charge_vec;
	for (int jj = _num_steps - 1; jj >= 0; jj--)
	{
		E_charge = 0.;
		P_target_min = grid[index].Grid();
		for (int ii = _num_steps - 1; ii >= 0; ii--)
		{
			if (grid[ii].Grid() > P_target_min)
				break;

			E_charge += (P_target_min - grid[ii].Grid())*_dt_hour;
		}
		E_charge_vec.push_back(E_charge);
		if (debug)
			fprintf(p, "%d: index\t%.3f\t %.3f\n",index,P_target_min, E_charge);
		index--;

		if (index < 0)
			break;
	}
	std::reverse(E_charge_vec.begin(), E_charge_vec.end());

	// Calculate target power 
	std::vector<double> sorted_grid_diff;
	sorted_grid_diff.reserve(_num_steps - 1);

	for (int ii = 0; ii != _num_steps - 1; ii++)
		sorted_grid_diff.push_back(grid[ii].Grid() - grid[ii + 1].Grid());

	P_target = grid[0].Grid(); // target power to shave to [kW]
	double sum = 0;			   // energy [kWh];
	if (debug)
		fprintf(p, "Step\tTarget_Power\tEnergy_Sum\tEnergy_charged\n");

	for (int ii = 0; ii != _num_steps - 1; ii++)
	{
		// don't look at negative grid power
		if (grid[ii + 1].Grid() < 0)
			break;
		// Update power target
		else
			P_target = grid[ii + 1].Grid();

		if (debug)
			fprintf(p, "%d\t %.3f\t", ii, P_target);

		// implies a repeated power
		if (sorted_grid_diff[ii] == 0)
		{
			if (debug)
				fprintf(p, "\n");
			continue;
		}
		// add to energy we are trimming
		else
			sum += sorted_grid_diff[ii] * (ii + 1)*_dt_hour;

		if (debug)
			fprintf(p, "%.3f\t%.3f\n", sum, E_charge_vec[ii+1]);

		if (sum < E_charge_vec[ii+1] && sum < E_useful)
			continue;
		// we have limited power, we'll shave what more we can
		else if (sum > E_charge_vec[ii+1])
		{
			P_target += (sum - E_charge_vec[ii]) / ((ii+1)*_dt_hour);
			sum = E_charge_vec[ii];
			if (debug)
				fprintf(p, "%d\t %.3f\t%.3f\t%.3f\n", ii, P_target, sum, E_charge_vec[ii] );
			break;
		}
		// only allow one cycle per day
		else if (sum > E_useful)
		{
			P_target += (sum - E_useful) / ((ii + 1)*_dt_hour);
			sum = E_useful;
			if (debug)
				fprintf(p, "%d\t %.3f\t%.3f\t%.3f\n", ii, P_target, sum, E_charge_vec[ii]);
			break;
		}
	}
	// don't set target lower than previous high in month
	if (P_target < _target_power_month)
	{
		P_target = _target_power_month;
		if (debug)
			fprintf(p, "P_target exceeds monthly target, move to  %.3f\n", P_target);
	}
	else
		_target_power_month = P_target;
}
void automate_dispatch_t::set_charge(int profile)
{
	// set period 1 as only PV charging
	_charge_array.push_back(true);
	_discharge_array.push_back(false);
	_gridcharge_array.push_back(false);
	_sched.fill(profile);
}
int automate_dispatch_t::set_discharge(FILE *p, bool debug, int hour_of_year, double P_target, double E_max)
{
	// Assign profiles within dispatch controller
	int profile = 1;
	int m, h;
	double discharge_energy = 0;
	if (debug)
		fprintf(p, "Profile\t Hour\t Step\t Discharge_Percent\t Discharge_Energy\t E_required\t Grid\n");

	for (int ii = 0; ii != _num_steps; ii++)
	{
		double discharge_percent = 0;
		double energy_required = (grid[ii].Grid() - P_target)*_dt_hour;
		if (energy_required > 0)
		{
			discharge_percent = 100 * (energy_required / E_max);
			discharge_energy += energy_required;
			profile++;
			if (debug)
				fprintf(p, "%d\t %d\t %d\t %.3f\t %.3f\t %.3f\t %.3f\n", profile, grid[ii].Hour(), grid[ii].Step(), discharge_percent, discharge_energy, energy_required, grid[ii].Grid());

			if (discharge_percent > 100 && _mode == MAINTAIN_TARGET)
				_message.add("Unable to discharge enough to meet power target.  Increase power target.");
		}
		else
			break;

		util::month_hour(hour_of_year + grid[ii].Hour(), m, h);
		int min = grid[ii].Step();
		int column = (h - 1)*_steps_per_hour + min;

		// have set profile 0 as charge from solar only as default, start from 1
		_sched.set_value(profile, m - 1, column); // in hourly case, column is hour-1, sched is 1-based
		_charge_array.push_back(true);
		_discharge_array.push_back(true);
		_gridcharge_array.push_back(false);
		_percent_discharge_array[profile] = discharge_percent;
		_percent_charge_array[profile] = 100.; 
	}
	return profile;
}
void automate_dispatch_t::set_gridcharge(FILE *p, bool debug, int hour_of_year, int profile, double P_target, double E_max)
{
	// grid charging scheme
	profile++;
	int m, h;
	std::vector<int> grid_profiles;
	int peak_hour =grid[0].Hour();
	double charge_energy = 0;
	int steps_grid_charged = 0;
	double charge_percent = 0;

	// Count charge all day
	for (int ii = 0; ii != _num_steps; ii++)
	{
		if (grid[ii].Grid() < 0)
			charge_energy += (-grid[ii].Grid()) * _dt_hour;
	}

	if (charge_energy < E_max)
	{
		if (debug)
			fprintf(p, "hour\t step\t grid\t charge_percent\t charge_energy\n");

		for (int ii = _num_steps - 1; ii >= 0; ii--)
		{
			if (grid[ii].Grid() > P_target)
				break;

			int hour = grid[ii].Hour();
			int step = grid[ii].Step();
			charge_percent = 100 * ((P_target - grid[ii].Grid())*_dt_hour) / E_max;
			charge_energy += (P_target - grid[ii].Grid())*_dt_hour;

			if (debug)
				fprintf(p, "%d\t %d\t %.3f\t %.3f\t %.3f\n", hour, step, grid[ii].Grid(), charge_percent, charge_energy);

			if (charge_percent < 0)
				break;

			util::month_hour(hour_of_year + hour, m, h);
			int column = (h - 1)*_steps_per_hour + step;
			_sched.set_value(profile, m - 1, column); // hourly, column is h-1
			_charge_array.push_back(true);
			_discharge_array.push_back(false);
			_gridcharge_array.push_back(true);
			_percent_charge_array[profile] = charge_percent;
			profile++;
		}
	}
	if (charge_energy == 0 && _mode == MAINTAIN_TARGET )
		_message.add("Power target too low, unable to charge battery.  Increase target power.");
}

