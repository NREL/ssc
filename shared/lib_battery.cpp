/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <cmath>
#include <cfloat>
#include <sstream>
#include <algorithm>

#include <functional>

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
size_t message::total_message_count(){ return messages.size(); }
size_t message::message_count(int index)
{
	if (index < (int)messages.size())
		return count[index];
	else
		return 0;
}
std::string message::get_message(int index)
{
	if (index < (int)messages.size())
		return messages[index];
	else
		return NULL;
}
std::string message::construct_log_count_string(int index)
{
	std::ostringstream oss;
	oss << count[index];

	std::string message_count = oss.str();
	std::string log = messages[index] + " - warning occurred: " + message_count + " times";
	return log;
}



/*
Define Thermal Model
*/
thermal_t::thermal_t() { /* nothing to do */ }
thermal_t::thermal_t(double dt_hour, double mass, double length, double width, double height, double R, double Cp,
                     double h, std::vector<double> T_room_K, const util::matrix_t<double> &c_vs_t)
        : dt_sec(dt_hour * 3600), _mass(mass), _length(length), _width(width), _height(height),
          _Cp(Cp), _h(h), T_room_K(T_room_K), _cap_vs_temp(c_vs_t)
{
    _R = R;
    _capacity_percent = 100;

    // assume all surfaces are exposed
    _A = 2 * (length*width + length*height + width*height);

    T_room_init = T_room_K[0];
    T_batt_init = T_room_init;
    T_batt_avg = T_room_init;

    //initialize maximum temperature
    _T_max = 400.;

    next_time_at_current_T_room = dt_sec;

    // exp(-A*h*t/m/Cp) < tol
    t_threshold = -_mass * _Cp / _A / _h * log(tolerance) + dt_sec;

    // curve fit
    size_t n = _cap_vs_temp.nrows();
    for (int i = 0; i < (int)n; i++)
    {
        _cap_vs_temp(i,0) += 273.15; // convert C to K
    }
}

thermal_t::thermal_t(const thermal_t& thermal){
    dt_sec = thermal.dt_sec;
    next_time_at_current_T_room = thermal.next_time_at_current_T_room;
    t_threshold = thermal.t_threshold;
    _mass = thermal._mass;
    _length = thermal._length;
    _width = thermal._width;
    _height = thermal._height;
    _Cp = thermal._Cp;
    _h = thermal._h;
    // _T_room = thermal._T_room;  // don't copy, super slow in subhourly simulations
    _R = thermal._R;
    _A = thermal._A;
    T_room_init = thermal.T_room_init;
    T_batt_init = thermal.T_batt_init;
    T_batt_avg = thermal.T_batt_avg;
    T_room_K = thermal.T_room_K;
    _capacity_percent = thermal._capacity_percent;
    _T_max = thermal._T_max;
    _cap_vs_temp = thermal._cap_vs_temp;
}
thermal_t * thermal_t::clone(){ return new thermal_t(*this); }
void thermal_t::copy(thermal_t * thermal)
{
    dt_sec = thermal->dt_sec;
    next_time_at_current_T_room = thermal->next_time_at_current_T_room;
    t_threshold = thermal->t_threshold;
    _mass = thermal->_mass;
    _length = thermal->_length;
    _width = thermal->_width;
    _height = thermal->_height;
    _Cp = thermal->_Cp;
    _h = thermal->_h;
    // _T_room = thermal->_T_room;  // don't copy, super slow in subhourly simulations
    _R = thermal->_R;
    _A = thermal->_A;
    T_room_init = thermal->T_room_init;
    T_batt_init = thermal->T_batt_init;
    T_batt_avg = thermal->T_batt_avg;
    T_room_K = thermal->T_room_K;
    _capacity_percent = thermal->_capacity_percent;
    _T_max = thermal->_T_max;
    _cap_vs_temp = thermal->_cap_vs_temp;
}
void thermal_t::replace_battery(size_t lifetimeIndex)
{
    T_room_init = T_room_K[T_room_K[lifetimeIndex % T_room_K.size()]];
    T_batt_init = T_room_init;
    T_batt_avg = T_room_init;
    _capacity_percent = 100.;
}

void thermal_t::calcCapacity() {
    double percent = util::linterp_col(_cap_vs_temp, 0, T_batt_avg, 1);

    if (std::isnan(percent) || percent < 0 || percent > 100)
    {
        percent = 100;
        _message.add("Unable to determine capacity adjustment for temperature, ignoring");
    }
    _capacity_percent = percent;
}

void thermal_t::calcTemperature(double I, size_t lifetimeIndex)
{
    double T_room = T_room_K[lifetimeIndex % T_room_K.size()];
    double source = pow(I,2) * _R/_A/_h + T_room;

    // if initial temp of batt will change, old initial conditions are invalid and need new T(t) piece
    if (abs(T_room - T_room_init) > tolerance){
        T_room_init = T_room;
        T_batt_init = T_batt_avg;

        // then the battery temp is the average temp over that step
        double diffusion = exp(-_A * _h * next_time_at_current_T_room / _mass / _Cp);
        double coeff_avg = _mass * _Cp / _A / _h / next_time_at_current_T_room;
        T_batt_avg = (T_batt_init - T_room_init) * coeff_avg * (1 - diffusion) + source;
    }
        // if initial temp of batt will not be changed, then continue with previous T(t) piece
    else  {
        // otherwise, given enough time, the diffusion term is negligible so the temp comes from source only
        if (next_time_at_current_T_room > t_threshold){
            T_batt_avg = source;
        }
            // battery temp is still adjusting to room
        else{
            double diffusion = exp(-_A * _h * next_time_at_current_T_room / _mass / _Cp);
            T_batt_avg = (T_batt_init - T_room_init) * diffusion + source;
        }
    }

    calcCapacity();
}

// battery temperature is the average temp during the time step
void thermal_t::updateTemperature(double I, size_t lifetimeIndex)
{
    if (lifetimeIndex >= 60808)
        int x = 0;

    double T_room = T_room_K[lifetimeIndex % T_room_K.size()];

    if (abs(T_room - T_room_init) > tolerance)
        next_time_at_current_T_room = dt_sec;

    calcTemperature(I, lifetimeIndex);

    next_time_at_current_T_room += dt_sec;

    calcCapacity();
}

double thermal_t::capacity_percent(){ return _capacity_percent; }
double thermal_t::T_battery(){ return T_batt_avg; }

/*
Define Losses
*/
losses_t::losses_t(double dtHour, lifetime_t * lifetime, thermal_t * thermal, capacity_t* capacity, int loss_choice, double_vec charge_loss, double_vec discharge_loss, double_vec idle_loss, double_vec losses):
_loss_mode(loss_choice)
{
	_dtHour = dtHour;
	_lifetime = lifetime;
	_thermal = thermal;
	_capacity = capacity;

	// User can input vectors of size 1 or size 12
	if (loss_choice == losses_t::MONTHLY)
	{
		if (charge_loss.size() == 1) {
			for (size_t m = 0; m < 12; m++) {
				_charge_loss.push_back(charge_loss[0]);
			}
		}
		else if (charge_loss.size() == 0) {
			for (size_t m = 0; m < 12; m++) {
				_charge_loss.push_back(0);
			}
		}
		else {
			_charge_loss = charge_loss;
		}
		if (discharge_loss.size() == 1) {

			for (size_t m = 0; m < 12; m++) {
				_discharge_loss.push_back(discharge_loss[0]);
			}
		}
		else if (discharge_loss.size() == 0) {

			for (size_t m = 0; m < 12; m++) {
				_discharge_loss.push_back(0);
			}
		}
		else {
			_discharge_loss = discharge_loss;
		}
		if (idle_loss.size() == 1) {
			for (size_t m = 0; m < 12; m++) {
				_idle_loss.push_back(idle_loss[0]);
			}
		}
		else if (idle_loss.size() == 0) {
			for (size_t m = 0; m < 12; m++) {
				_idle_loss.push_back(0);
			}
		}
		else {
			_idle_loss = idle_loss;
		}
		for (size_t i = 0; i < (size_t)(8760 / dtHour); i++) {
			_full_loss.push_back(0);
		}
	}
	// User can input vectors of size 1 or size nrec (first year)
	else {
		if (losses.size() == 1) {
			for (size_t i = 0; i < (size_t)(8760 / dtHour); i++) {
				_full_loss.push_back(losses[0]);
			}
		}
		else {
			_full_loss = losses;
		}

	}
}
void losses_t::set_models(lifetime_t * l, thermal_t * t, capacity_t* c){
    _lifetime = l;
    _thermal = t;
    _capacity = c;
}
void losses_t::copy(losses_t * losses)
{
	_loss_mode = losses->_loss_mode;
	_charge_loss = losses->_charge_loss;
	_discharge_loss = losses->_discharge_loss;
	_idle_loss = losses->_idle_loss;
	_full_loss = losses->_full_loss;
}

double losses_t::getLoss(size_t indexFirstYear) { return _full_loss[indexFirstYear]; }
void losses_t::run_losses(size_t lifetimeIndex)
{
	_capacity->updateCapacityForLifetime(_lifetime->capacity_percent());

	size_t indexYearOne = util::yearOneIndex(_dtHour, lifetimeIndex);
	size_t hourOfYear = (size_t)std::floor(indexYearOne * _dtHour);
	size_t monthIndex = util::month_of((double)(hourOfYear)) - 1;

	// update system losses depending on user input
	if (_loss_mode == losses_t::MONTHLY) {
		if (_capacity->charge_operation() == capacity_t::CHARGE)
			_full_loss[indexYearOne] = _charge_loss[monthIndex];
		if (_capacity->charge_operation() == capacity_t::DISCHARGE)
			_full_loss[indexYearOne] = _discharge_loss[monthIndex];
		if (_capacity->charge_operation() == capacity_t::NO_CHARGE)
			_full_loss[indexYearOne] = _idle_loss[monthIndex];
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
	_last_idx = 0;

	if (battery_chemistry != battery_t::LEAD_ACID) {
		_capacity_initial = new capacity_lithium_ion_t();
	}
	else {
		_capacity_initial = new capacity_kibam_t();
	}
	_thermal_initial = new thermal_t();
}

battery_t::battery_t(const battery_t& battery)
{
	_battery_chemistry = battery._battery_chemistry;
	_dt_hour = battery._dt_hour;
	_dt_min = battery._dt_min;
	_last_idx = battery._last_idx;
	_capacity = battery.capacity_model()->clone();
	_capacity_initial = battery.capacity_initial_model()->clone();
	_voltage = battery.voltage_model()->clone();
	_thermal = battery.thermal_model()->clone();
	_thermal_initial = battery.thermal_initial_model()->clone();
	_lifetime = battery.lifetime_model()->clone();
	_losses = new losses_t(_dt_hour, _lifetime, _thermal, _capacity, 0);
	_losses->copy(battery.losses_model());
}

battery_t::~battery_t()
{
	if (_capacity_initial)
		delete _capacity_initial;
	if (_thermal_initial)
		delete _thermal_initial;
}

// copy from battery to this
void battery_t::copy(const battery_t * battery)
{
	*_capacity = *battery->capacity_model();
	*_capacity_initial = *battery->capacity_initial_model();
	_thermal->copy(battery->thermal_model());
	_thermal_initial->copy(battery->thermal_initial_model());
	_lifetime->copy(battery->lifetime_model());
	*_voltage = *battery->voltage_model();
	_losses->set_models(_lifetime, _thermal, _capacity);

	_battery_chemistry = battery->_battery_chemistry;
	_dt_hour = battery->_dt_hour;
	_dt_min = battery->_dt_min;
	_last_idx = battery->_last_idx;
}

void battery_t::delete_clone()
{
	if (_capacity) delete _capacity;
	if (_voltage) delete _voltage;
	if (_thermal) delete _thermal;
	if (_lifetime)
	{
		_lifetime->delete_clone();
		delete _lifetime;
	}
	if (_losses) delete _losses;
}
void battery_t::initialize(capacity_t *capacity, voltage_t * voltage, lifetime_t * lifetime, thermal_t * thermal, losses_t * losses)
{
	_capacity = capacity;
	_lifetime = lifetime;
	_voltage = voltage;
	_thermal = thermal;
	_losses = losses;

    *_capacity_initial = *_capacity;
	_thermal_initial->copy(_thermal);
}

double battery_t::calculate_current_for_power_kw(double &P_kw){
    if (P_kw == 0.)
        return 0.;
    double current;
    if (P_kw < 0){
        double max_P = calculate_max_charge_kw(&current);
        if (max_P > P_kw){
            P_kw = max_P;
            return current;
        }
    }
    else{
        double max_P = calculate_max_discharge_kw(&current);
        if (max_P < P_kw){
            P_kw = max_P;
            return current;
        }
    }
    return _voltage->calculate_current_for_target_w(P_kw * 1000., _capacity->q0(), fmin(_capacity->qmax(), _capacity->qmax_thermal()), _thermal->T_battery());
}

double battery_t::calculate_voltage_for_current(double I) {
    // TODO: add looping when this function will actually be used... doesn't work that well atm
    double qmax = fmin(_capacity->qmax(), _capacity->qmax_thermal());
    return voltage_model()->calculate_voltage_for_current(I, battery_charge_total(), qmax, _thermal->T_battery());
}

double battery_t::calculate_max_charge_kw(double *max_current_A) {
    double q = _capacity->q0();
    double qmax = battery_charge_maximum();
    double power_W = 0;
    double current = 0;
    size_t its = 0;
    while (fabs(power_W - _voltage->calculate_max_charge_w(q, qmax, _thermal->T_battery(), &current)) > tolerance
           && its++ < 10){
        power_W = _voltage->calculate_max_charge_w(q, qmax, _thermal->T_battery(), &current);
        _thermal->calcTemperature(current, _last_idx + 1);
        qmax = _capacity->qmax() * _thermal->capacity_percent() / 100.;
    }
    return _voltage->calculate_max_charge_w(q, qmax, _thermal->T_battery(), max_current_A) / 1000.;
}

double battery_t::calculate_max_discharge_kw(double *max_current_A) {
    double q = _capacity->q0();
    double qmax = battery_charge_maximum();
    double power_W = 0;
    double current = 0;
    size_t its = 0;
    while (fabs(power_W - _voltage->calculate_max_discharge_w(q, qmax, _thermal->T_battery(), &current)) > tolerance
        && its++ < 10){
        power_W = _voltage->calculate_max_discharge_w(q, qmax, _thermal->T_battery(), &current);
        _thermal->calcTemperature(current, _last_idx + 1);
        qmax = _capacity->qmax() * _thermal->capacity_percent() / 100.;
    }
    return _voltage->calculate_max_discharge_w(q, qmax, _thermal->T_battery(), max_current_A) / 1000.;
}

double battery_t::run(size_t lifetimeIndex, double &I)
{
	// Temperature affects capacity, but capacity model can reduce current, which reduces temperature, need to iterate
	double I_initial = I;
	size_t iterate_count = 0;
	*_capacity_initial = *_capacity;
	_thermal_initial->copy(_thermal);

	while (iterate_count < 5)
	{
		runThermalModel(I, lifetimeIndex);
		runCapacityModel(I);

		if (fabs(I - I_initial)/fabs(I_initial) > tolerance)
		{
			_thermal->copy(_thermal_initial);
			*_capacity = *_capacity_initial;
			I_initial = I;
			iterate_count++;
		}
		else {
			break;
		}

	}
	runVoltageModel();
	runLifetimeModel(lifetimeIndex);
	runLossesModel(lifetimeIndex);

	return I * voltage_model()->battery_voltage() * util::watt_to_kilowatt;
}
void battery_t::runThermalModel(double I, size_t lifetimeIndex)
{
	_thermal->updateTemperature(I, lifetimeIndex);
}

void battery_t::runCapacityModel(double &I)
{
	// Don't update max capacity if the battery is idle
	if (fabs(I) > tolerance) {
		// Need to first update capacity model to ensure temperature accounted for
		_capacity->updateCapacityForThermal(_thermal->capacity_percent());
	}
	_capacity->updateCapacity(I, _dt_hour );
}

void battery_t::runVoltageModel()
{
	_voltage->updateVoltage(_capacity, _thermal->T_battery(), _dt_hour);
}

void battery_t::runLifetimeModel(size_t lifetimeIndex)
{
    _lifetime->runLifetimeModels(lifetimeIndex,
            _capacity->chargeChanged(), _capacity->prev_DOD(), _capacity->DOD(), thermal_model()->T_battery());
	if (_lifetime->check_replaced())
	{
		_capacity->replace_battery(_lifetime->get_replacement_percent());
		_thermal->replace_battery(lifetimeIndex);
	}
}
void battery_t::runLossesModel(size_t idx)
{
	if (idx > _last_idx || idx == 0)
	{
		_losses->run_losses(idx);
		_last_idx = idx;
	}
}
capacity_t * battery_t::capacity_model() const { return _capacity; }
capacity_t * battery_t::capacity_initial_model() const { return _capacity_initial; }
voltage_t * battery_t::voltage_model() const { return _voltage; }
lifetime_t * battery_t::lifetime_model() const { return _lifetime; }
thermal_t * battery_t::thermal_model() const { return _thermal; }
thermal_t * battery_t::thermal_initial_model() const { return _thermal_initial; }
losses_t * battery_t::losses_model() const { return _losses; }

double battery_t::battery_charge_needed(double SOC_max)
{
	double charge_needed = _capacity->qmax_thermal() * SOC_max * 0.01 - _capacity->q0();
	if (charge_needed > 0)
		return charge_needed;
	else
		return 0.;
}
double battery_t::battery_energy_to_fill(double SOC_max)
{
	double battery_voltage = this->battery_voltage_nominal(); // [V]
	double charge_needed_to_fill = this->battery_charge_needed(SOC_max); // [Ah] - qmax - q0
	return (charge_needed_to_fill * battery_voltage)*util::watt_to_kilowatt;  // [kWh]
}
double battery_t::battery_energy_nominal()
{
	return battery_voltage_nominal() * _capacity->qmax() * util::watt_to_kilowatt;
}
double battery_t::battery_power_to_fill(double SOC_max)
{
	// in one time step
	return (this->battery_energy_to_fill(SOC_max) / _dt_hour);
}

double battery_t::battery_charge_total(){return _capacity->q0();}
double battery_t::battery_charge_maximum() { return fmin(_capacity->qmax(), _capacity->qmax_thermal()); }
double battery_t::battery_charge_maximum_lifetime(){ return _capacity->qmax(); }
double battery_t::battery_charge_maximum_thermal() { return _capacity->qmax_thermal(); }
double battery_t::cell_voltage(){ return _voltage->cell_voltage();}
double battery_t::battery_voltage(){ return _voltage->battery_voltage();}
double battery_t::battery_voltage_nominal(){ return _voltage->battery_voltage_nominal(); }
double battery_t::battery_soc(){ return _capacity->SOC(); }
