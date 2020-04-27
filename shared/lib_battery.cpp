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
Define Thermal Model
*/

void thermal_t::initialize() {
    size_t n = params->cap_vs_temp.nrows();
    for (int i = 0; i < (int) n; i++) {
        params->cap_vs_temp(i, 0);
    }
    state.T_batt_avg = state.T_room;
    state.T_batt_prev = state.T_room;
    state.q_relative_thermal = 100;
    dt_sec = params->dt_hour * 3600;
}

thermal_t::thermal_t(double dt_hour, double mass, double surface_area, double R, double Cp, double h,
                     const util::matrix_t<double> &c_vs_t, std::vector<double> T_room_C) {
    params = std::shared_ptr<thermal_params>(new thermal_params({dt_hour, mass, surface_area, Cp, h, R, c_vs_t}));
    params->option = thermal_params::SCHEDULE;
    params->T_room_schedule = std::move(T_room_C);
    state.T_room = params->T_room_schedule[0];
    initialize();
}

thermal_t::thermal_t(double dt_hour, double mass, double surface_area, double R, double Cp, double h,
                     const util::matrix_t<double> &c_vs_t, double T_room_C) {
    params = std::shared_ptr<thermal_params>(new thermal_params({dt_hour, mass, surface_area, Cp, h, R, c_vs_t}));
    params->option = thermal_params::VALUE;
    state.T_room = T_room_C;
    initialize();
}

thermal_t::thermal_t(const thermal_t &rhs) :
        state(rhs.state) {
    params = std::make_shared<thermal_params>();
    operator=(rhs);
}

thermal_t &thermal_t::operator=(const thermal_t &rhs) {
    if (this != &rhs) {
        *params = *rhs.params;
        dt_sec = rhs.dt_sec;
        state = rhs.state;
    }
    return *this;
}

thermal_t *thermal_t::clone() { return new thermal_t(*this); }

void thermal_t::replace_battery(size_t lifetimeIndex) {
    if (params->option == thermal_params::VALUE)
        state.T_batt_avg = params->T_room_schedule[lifetimeIndex % params->T_room_schedule.size()];
    else
        state.T_batt_avg = state.T_room;
    state.T_batt_prev = state.T_room;
    state.q_relative_thermal = 100.;
}

void thermal_t::calc_capacity() {
    double percent = util::linterp_col(params->cap_vs_temp, 0, state.T_batt_avg, 1);

    if (std::isnan(percent) || percent < 0 || percent > 100) {
        percent = 100;
//        log.add("Unable to determine capacity adjustment for temperature, ignoring");
    }
    state.q_relative_thermal = percent;
}

// battery temperature is the average temp during the time step
void thermal_t::updateTemperature(double I, size_t lifetimeIndex) {
    if (params->option == thermal_params::SCHEDULE) {
        state.T_room = params->T_room_schedule[lifetimeIndex % params->T_room_schedule.size()];
    }

    // the battery temp is the average temp over that step, starting with temp from end of last timestep
    double source = I * I * params->R / (params->surface_area * params->h) + state.T_room;
    double diffusion = exp(-params->surface_area * params->h * dt_sec / params->mass / params->Cp);
    double coeff_avg = params->mass * params->Cp / params->surface_area / params->h / dt_sec;
    state.T_batt_avg = (state.T_batt_prev - state.T_room) * coeff_avg * (1 - diffusion) + source;

    // update temp for use in next timestep
    state.T_batt_prev = (state.T_batt_prev - state.T_room) * diffusion + source;

    calc_capacity();
}

double thermal_t::capacity_percent() { return state.q_relative_thermal; }

double thermal_t::T_battery() { return state.T_batt_avg; }

thermal_state thermal_t::get_state() { return state; }

thermal_params thermal_t::get_params() { return *params; }

/*
Define Losses
*/
losses_t::losses_t(const std::vector<double>& charge_loss, const std::vector<double>& monthly_discharge, const std::vector<double>& monthly_idle) {
    params = std::make_shared<losses_params>();
    params->option = losses_params::MONTHLY;
    state.loss_percent = 0;

    if (charge_loss.size() == 1) {
        params->charge_loss = std::vector<double>(12, charge_loss[0]);
    }
    else {
        params->charge_loss = charge_loss;
    }
    if (monthly_discharge.size() == 1) {
        params->discharge_loss = std::vector<double>(12, monthly_discharge[0]);
    }
    else {
        params->discharge_loss = monthly_discharge;
    }
    if (monthly_idle.size() == 1) {
        params->idle_loss = std::vector<double>(12, monthly_idle[0]);
    }
    else {
        params->idle_loss = monthly_idle;
    }
}

losses_t::losses_t(const std::vector<double>& schedule_loss) {
    params = std::make_shared<losses_params>();
    params->option = losses_params::SCHEDULE;
    params->full_loss = schedule_loss;
    state.loss_percent = 0;
}

losses_t::losses_t(const losses_t& rhs) {
    params = std::make_shared<losses_params>();
    operator=(rhs);
}

losses_t &losses_t::operator=(const losses_t& rhs) {
    if (this != &rhs) {
        *params = *rhs.params;
        state = rhs.state;
    }
    return *this;
}

void losses_t::run_losses(size_t lifetimeIndex, double dtHour, double charge_operation) {
    size_t indexYearOne = util::yearOneIndex(dtHour, lifetimeIndex);
    auto hourOfYear = (size_t) std::floor(indexYearOne * dtHour);
    size_t monthIndex = util::month_of((double) (hourOfYear)) - 1;

    // update system losses depending on user input
    if (params->option == losses_params::MONTHLY) {
        if (charge_operation == capacity_state::CHARGE)
            state.loss_percent = params->charge_loss[monthIndex];
        if (charge_operation == capacity_state::DISCHARGE)
            state.loss_percent = params->discharge_loss[monthIndex];
        if (charge_operation == capacity_state::NO_CHARGE)
            state.loss_percent = params->idle_loss[monthIndex];
    }
    else if (params->option == losses_params::SCHEDULE)  {
        state.loss_percent = params->full_loss[lifetimeIndex % params->full_loss.size()];
    }
}

double losses_t::getLoss() { return state.loss_percent; }

losses_state losses_t::get_state() { return state; }

losses_params losses_t::get_params() {return *params; }

/*
Define Battery
*/
battery_t::battery_t() {};

battery_t::battery_t(double dt_hour, int battery_chemistry) {
    _dt_hour = dt_hour;
    _dt_min = dt_hour * 60;
    _battery_chemistry = battery_chemistry;
    _last_idx = 0;

    if (battery_chemistry != battery_t::LEAD_ACID) {
        _capacity_initial = new capacity_lithium_ion_t();
    } else {
        _capacity_initial = new capacity_kibam_t();
    }
}

battery_t::battery_t(const battery_t &battery) {
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
    _losses = new losses_t(*battery.losses_model());
}

battery_t::~battery_t() {
    delete _capacity_initial;
    delete _thermal_initial;
}

// copy from battery to this
void battery_t::copy(const battery_t *battery) {
    *_capacity = *battery->capacity_model();
    *_capacity_initial = *battery->capacity_initial_model();
    *_thermal = *battery->thermal_model();
    *_thermal_initial = *battery->thermal_initial_model();
    *_lifetime = *battery->lifetime_model();
    *_voltage = *battery->voltage_model();

    _battery_chemistry = battery->_battery_chemistry;
    _dt_hour = battery->_dt_hour;
    _dt_min = battery->_dt_min;
    _last_idx = battery->_last_idx;
}

void battery_t::delete_clone() {
    delete _capacity;
    delete _voltage;
    delete _thermal;
    delete _lifetime;
    delete _losses;
}

void battery_t::initialize(capacity_t *capacity, voltage_t *voltage, lifetime_t *lifetime, thermal_t *thermal,
                           losses_t *losses) {
    _capacity = capacity;
    _lifetime = lifetime;
    _voltage = voltage;
    _thermal = thermal;
    _losses = losses;

    *_capacity_initial = *_capacity;
    _thermal_initial = new thermal_t(*thermal);

    params = std::make_shared<replacement_params>();
    params->option = replacement_params::NONE;
}

void battery_t::setupReplacements(double capacity) {
    params = std::make_shared<replacement_params>();
    params->option = replacement_params::CAPACITY_PERCENT;
    params->capacity_percent = capacity;
}

void battery_t::setupReplacements(std::vector<int> schedule, std::vector<double> replacement_percents) {
    params = std::make_shared<replacement_params>();
    params->option = replacement_params::SCHEDULE;
    params->schedule = std::move(schedule);
    params->schedule_percent_to_replace = std::move(replacement_percents);
}

double battery_t::calculate_current_for_power_kw(double &P_kw) {
    if (P_kw == 0.)
        return 0.;
    double current;
    if (P_kw < 0) {
        double max_P = calculate_max_charge_kw(&current);
        if (max_P > P_kw) {
            P_kw = max_P;
            return current;
        }
    } else {
        double max_P = calculate_max_discharge_kw(&current);
        if (max_P < P_kw) {
            P_kw = max_P;
            return current;
        }
    }
    return _voltage->calculate_current_for_target_w(P_kw * 1000., _capacity->q0(),
                                                    fmin(_capacity->qmax(), _capacity->qmax_thermal()),
                                                    _thermal->T_battery());
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
           && its++ < 10) {
        power_W = _voltage->calculate_max_charge_w(q, qmax, _thermal->T_battery(), &current);
        _thermal->updateTemperature(current, _last_idx + 1);
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
           && its++ < 10) {
        power_W = _voltage->calculate_max_discharge_w(q, qmax, _thermal->T_battery(), &current);
        _thermal->updateTemperature(current, _last_idx + 1);
        qmax = _capacity->qmax() * _thermal->capacity_percent() / 100.;
    }
    return _voltage->calculate_max_discharge_w(q, qmax, _thermal->T_battery(), max_current_A) / 1000.;
}

double battery_t::run(size_t lifetimeIndex, double &I) {
    // Temperature affects capacity, but capacity model can reduce current, which reduces temperature, need to iterate
    double I_initial = I;
    size_t iterate_count = 0;
    *_capacity_initial = *_capacity;
    *_thermal_initial = *_thermal;

    while (iterate_count < 5) {
        runThermalModel(I, lifetimeIndex);
        runCapacityModel(I);

        if (fabs(I - I_initial) / fabs(I_initial) > tolerance) {
            *_thermal = *_thermal_initial;
            *_capacity = *_capacity_initial;
            I_initial = I;
            iterate_count++;
        } else {
            break;
        }

    }
    runVoltageModel();
    runLifetimeModel(lifetimeIndex);
    runLossesModel(lifetimeIndex);

    return I * voltage_model()->battery_voltage() * util::watt_to_kilowatt;
}

void battery_t::runThermalModel(double I, size_t lifetimeIndex) {
    _thermal->updateTemperature(I, lifetimeIndex);
}

void battery_t::runCapacityModel(double &I) {
    // Don't update max capacity if the battery is idle
    if (fabs(I) > tolerance) {
        // Need to first update capacity model to ensure temperature accounted for
        _capacity->updateCapacityForThermal(_thermal->capacity_percent());
    }
    _capacity->updateCapacity(I, _dt_hour);
}

void battery_t::runVoltageModel() {
    _voltage->updateVoltage(_capacity, _thermal->T_battery(), _dt_hour);
}

void battery_t::runLifetimeModel(size_t lifetimeIndex) {
    _lifetime->runLifetimeModels(lifetimeIndex,
                                 _capacity->chargeChanged(), _capacity->prev_DOD(), _capacity->DOD(),
                                 thermal_model()->T_battery());
    _capacity->updateCapacityForLifetime(_lifetime->capacity_percent());
}

void battery_t::runLossesModel(size_t idx) {
    if (idx > _last_idx || idx == 0) {
        _losses->run_losses(idx, _dt_hour, _capacity->charge_operation());
        _last_idx = idx;
    }
}

void battery_t::runReplacement(size_t year, size_t hour, size_t step) {
    if (year == 0 && hour == 0)
        return;

    if (params->option == replacement_params::OPTIONS::NONE)
        return;

    bool replace = false;
    double percent = 0;
    if (params->option == replacement_params::OPTIONS::SCHEDULE) {
        if (year < params->schedule.size()) {
            auto num_repl = (size_t) params->schedule[year];
            for (size_t j_repl = 0; j_repl < num_repl; j_repl++) {
                if ((hour == (j_repl * 8760 / num_repl)) && step == 0) {
                    replace = true;
                    break;
                }
            }
        }
        if (replace) {
            percent = params->schedule_percent_to_replace[year];
        }
    } else if (params->option == replacement_params::OPTIONS::CAPACITY_PERCENT) {
        if ((lifetime_model()->capacity_percent() - tolerance) <= params->capacity_percent) {
            replace = true;
            percent = 100.;
        }
    }

    if (replace) {
        replacement_state.n_replacements++;
        replacement_state.indices_replaced.push_back(util::lifetimeIndex(year, hour, step, (size_t) (1 / _dt_hour)));
        _lifetime->replaceBattery(percent);
        _capacity->replace_battery(percent);
        _thermal->replace_battery(year);
    }
}

void battery_t::resetReplacement() {
    replacement_state.n_replacements = 0;
}

double battery_t::getNumReplacementYear() {
    return replacement_state.n_replacements;
}

capacity_t *battery_t::capacity_model() const { return _capacity; }

capacity_t *battery_t::capacity_initial_model() const { return _capacity_initial; }

voltage_t *battery_t::voltage_model() const { return _voltage; }

lifetime_t *battery_t::lifetime_model() const { return _lifetime; }

thermal_t *battery_t::thermal_model() const { return _thermal; }

thermal_t *battery_t::thermal_initial_model() const { return _thermal_initial; }

losses_t *battery_t::losses_model() const { return _losses; }

double battery_t::battery_charge_needed(double SOC_max) {
    double charge_needed = _capacity->qmax_thermal() * SOC_max * 0.01 - _capacity->q0();
    if (charge_needed > 0)
        return charge_needed;
    else
        return 0.;
}

double battery_t::battery_energy_to_fill(double SOC_max) {
    double battery_voltage = this->battery_voltage_nominal(); // [V]
    double charge_needed_to_fill = this->battery_charge_needed(SOC_max); // [Ah] - qmax - q0
    return (charge_needed_to_fill * battery_voltage) * util::watt_to_kilowatt;  // [kWh]
}

double battery_t::battery_energy_nominal() {
    return battery_voltage_nominal() * _capacity->qmax() * util::watt_to_kilowatt;
}

double battery_t::battery_power_to_fill(double SOC_max) {
    // in one time step
    return (this->battery_energy_to_fill(SOC_max) / _dt_hour);
}

double battery_t::battery_charge_total() { return _capacity->q0(); }

double battery_t::battery_charge_maximum() { return fmin(_capacity->qmax(), _capacity->qmax_thermal()); }

double battery_t::battery_charge_maximum_lifetime() { return _capacity->qmax(); }

double battery_t::battery_charge_maximum_thermal() { return _capacity->qmax_thermal(); }

double battery_t::cell_voltage() { return _voltage->cell_voltage(); }

double battery_t::battery_voltage() { return _voltage->battery_voltage(); }

double battery_t::battery_voltage_nominal() { return _voltage->battery_voltage_nominal(); }

double battery_t::battery_soc() { return _capacity->SOC(); }
