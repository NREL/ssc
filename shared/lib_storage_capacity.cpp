#include <math.h>

#include <cmath>
#include <cfloat>
#include <sstream>
#include <algorithm>

#include "lib_storage_capacity.h"

/*
Define Capacity Model
*/
capacity_t::capacity_t() { /* nothing to do */ }
capacity_t::capacity_t(double q, double SOC_init, double SOC_max, double SOC_min)
{
    _qmax0 = q;
    _dt_hour = 0.;
    _SOC_init = SOC_init;
    _SOC_max = SOC_max;
    _SOC_min = SOC_min;

    _state.q0 = 0.01*SOC_init*q;
    _state.qmax = q;
    _state.qmax_thermal = q;
    _state.I = 0.;
    _state.I_loss = 0.;
    _state.DOD = 0;

    // Initialize SOC, DOD
    _state.SOC = SOC_init;
    _state.DOD_prev = 0;

    // Initialize charging states
    _state.prev_charge = DISCHARGE;
    _state.charge = DISCHARGE;
    _state.chargeChange = false;
}
void capacity_t::copy(capacity_t * capacity)
{
    _state = capacity_state_params(capacity->_state);

    _qmax0 = capacity->_qmax0;
        _SOC_init = capacity->_SOC_init;
    _SOC_min = capacity->_SOC_min;
    _SOC_max = capacity->_SOC_max;
    _dt_hour = capacity->_dt_hour;
}
void capacity_t::check_charge_change()
{
    _state.charge = NO_CHARGE;

    // charge state
    if (_state.I < 0)
        _state.charge = CHARGE;
    else if (_state.I > 0)
        _state.charge = DISCHARGE;

    // Check if charge changed
    _state.chargeChange = false;
    if ((_state.charge != _state.prev_charge) && (_state.charge != NO_CHARGE) && (_state.prev_charge != NO_CHARGE))
    {
        _state.chargeChange = true;
        _state.prev_charge = _state.charge;
    }
}
int capacity_t::charge_operation(){ return _state.charge; }
void capacity_t::check_SOC()
{
    double q_upper = _state.qmax * _SOC_max * 0.01;
    double q_lower = _state.qmax * _SOC_min * 0.01;
    double I_orig = _state.I;

    // set capacity to upper thermal limit
    if (q_upper > _state.qmax_thermal * _SOC_max * 0.01)
        q_upper = _state.qmax_thermal * _SOC_max * 0.01;

    // check if overcharged
    if (_state.q0 > q_upper )
    {
        if (fabs(_state.I) > tolerance)
        {
            _state.I += (_state.q0 - q_upper) / _dt_hour;
            if (_state.I / I_orig < 0)
                _state.I = 0;
        }
        _state.q0 = q_upper;
    }
        // check if undercharged
    else if (_state.q0 < q_lower)
    {
        if (fabs(_state.I) > tolerance)
        {
            _state.I += (_state.q0 - q_lower) / _dt_hour;
            if (_state.I / I_orig < 0)
                _state.I = 0;
        }
        _state.q0 = q_lower;
    }
}

void capacity_t::update_SOC()
{
    if (_state.qmax > 0)
        _state.SOC = 100.*(_state.q0 / _state.qmax_thermal);
    else
        _state.SOC = 0.;

    // due to dynamics, it's possible SOC could be slightly above 1 or below 0
    if (_state.SOC > 100.0)
        _state.SOC = 100.0;
    else if (_state.SOC < 0.)
        _state.SOC = 0.;

    _state.DOD = 100. - _state.SOC;
}
bool capacity_t::chargeChanged(){return _state.chargeChange;}
double capacity_t::SOC(){ return _state.SOC; }
double capacity_t::DOD(){ return _state.DOD; }
double capacity_t::DOD_max(){ return _SOC_max - _SOC_min; }
double capacity_t::prev_DOD(){ return _state.DOD_prev; }
double capacity_t::q0(){ return _state.q0;}
double capacity_t::qmax(){ return _state.qmax; }
double capacity_t::qmax_thermal(){ return _state.qmax_thermal; }
double capacity_t::I(){ return _state.I; }
double capacity_t::I_loss() { return _state.I_loss; }

/*
Define KiBam Capacity Model
*/
capacity_kibam_t::capacity_kibam_t(){ /* nothing to do */}
capacity_kibam_t::capacity_kibam_t(double q20, double t1, double q1, double q10, double SOC_init, double SOC_max, double SOC_min) :
        capacity_t(q20, SOC_init, SOC_max, SOC_min)
{
    _kibam_state.q10 = q10;
    _kibam_state.q20 = q20;
    _kibam_state.I20 = q20/20.;

    // parameters for c, k calculation
    _q1 = q1;
    _q2 = q10;
    _t1 = t1;
    _t2 = 10.;
    _F1 = q1 / q20; // use t1, 20
    _F2 = q1 / q10;  // use t1, 10

    // compute the parameters
    parameter_compute();
    _qmax0 = _state.qmax;

    // initializes to full battery
    replace_battery();
}
capacity_kibam_t * capacity_kibam_t::clone(){ return new capacity_kibam_t(*this); }
void capacity_kibam_t::copy(capacity_t * capacity)
{
    capacity_t::copy(capacity);
    capacity_kibam_t * tmp = dynamic_cast<capacity_kibam_t*>(capacity);

    _t1 = tmp->_t1;
    _t2 = tmp->_t2;
    _q1 = tmp->_q1;
    _q2 = tmp->_q2;
    _F1 = tmp->_F1;
    _F2 = tmp->_F2;
    _c = tmp->_c;
    _k = tmp->_k;
    _kibam_state = kibam_struct_params(tmp->_kibam_state);
}

void capacity_kibam_t::replace_battery()
{
    // Assume initial charge is max capacity
    _state.q0 = _qmax0*_SOC_init*0.01;
    _kibam_state.q1_0 = _state.q0*_c;
    _kibam_state.q2_0 = _state.q0 - _kibam_state.q1_0;
    _state.qmax = _qmax0;
    _state.SOC = _SOC_init;
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
    double num = -_k*_c*_state.qmax + _k*q10*exp(-_k*dt) + q0*_k*_c*(1 - exp(-_k*dt));
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
    double num = _kibam_state.q20*((1 - exp(-_k * 20)) * (1 - _c) + _k*_c * 20);
    double denom = _k*_c * 20;
    return (num / denom);
}

double capacity_kibam_t::qmax_of_i_compute(double T)
{
    return ((_state.qmax*_k*_c*T) / (1 -exp(-_k*T) + _c*(_k*T - 1 + exp(-_k*T))));
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
    _state.qmax = qmax_compute();
}

void capacity_kibam_t::updateCapacity(double &I, double dt_hour)
{
    if (fabs(I) < low_tolerance)
        I = 0;

    _state.DOD_prev = _state.DOD;
    _state.I_loss = 0.;
    _state.I = I;
    _dt_hour = dt_hour;

    double Idmax = 0.;
    double Icmax = 0.;
    double Id = 0.;
    double Ic = 0.;
    double q1 = 0.;
    double q2 = 0.;

    if (_state.I > 0)
    {
        Idmax = Idmax_compute(_kibam_state.q1_0, _state.q0, dt_hour);
        Id = fmin(_state.I, Idmax);
        _state.I = Id;
    }
    else if (_state.I < 0)
    {
        Icmax = Icmax_compute(_kibam_state.q1_0, _state.q0, dt_hour);
        Ic = -fmin(fabs(_state.I), fabs(Icmax));
        _state.I = Ic;
    }

    // new charge levels
    q1 = q1_compute(_kibam_state.q1_0, _state.q0, dt_hour, _state.I);
    q2 = q2_compute(_kibam_state.q2_0, _state.q0, dt_hour, _state.I);

    // Check for thermal effects
    if (q1 + q2 > _state.qmax_thermal)
    {
        double q0 = q1 + q2;
        double p1 = q1 / q0;
        double p2 = q2 / q0;
        _state.q0 = _state.qmax_thermal;
        q1 = _state.q0*p1;
        q2 = _state.q0*p2;
    }

    // update internal variables
    _kibam_state.q1_0 = q1;
    _kibam_state.q2_0 = q2;
    _state.q0 = q1 + q2;

    update_SOC();
    check_charge_change();

    // Pass current out
    I = _state.I;
}
void capacity_kibam_t::updateCapacityForThermal(double capacity_percent)
{
    // Modify the lifetime degraded capacity by the thermal effect
    _state.qmax_thermal = _state.qmax*capacity_percent*0.01;
}
void capacity_kibam_t::updateCapacityForLifetime(double capacity_percent)
{

    if (_qmax0* capacity_percent*0.01 <= _state.qmax)
        _state.qmax = _qmax0* capacity_percent*0.01;

    // scale to q0 = qmax if q0 > qmax
    if (_state.q0 > _state.qmax)
    {
        double q0_orig = _state.q0;
        double p = _state.qmax / _state.q0;
        _state.q0 *= p;
        _q1 *= p;
        _q2 *= p;
        _state.I_loss += (q0_orig - _state.q0) / _dt_hour;
    }
    update_SOC();
}

double capacity_kibam_t::q1(){ return _kibam_state.q1_0; }
double capacity_kibam_t::q2(){ return _kibam_state.q2_0; }
double capacity_kibam_t::q10(){ return _kibam_state.q10; }
double capacity_kibam_t::q20(){return _kibam_state.q20;}


/*
Define Lithium Ion capacity model
*/
capacity_lithium_ion_t::capacity_lithium_ion_t() { /* nothing to do */ }
capacity_lithium_ion_t::capacity_lithium_ion_t(double q, double SOC_init, double SOC_max, double SOC_min) :capacity_t(q, SOC_init, SOC_max, SOC_min){};
capacity_lithium_ion_t * capacity_lithium_ion_t::clone(){ return new capacity_lithium_ion_t(*this); }
void capacity_lithium_ion_t::copy(capacity_t * capacity){ capacity_t::copy(capacity);}

void capacity_lithium_ion_t::replace_battery()
{
    _state.q0 = _qmax0 * _SOC_init * 0.01;
    _state.qmax = _qmax0;
    _state.qmax_thermal = _qmax0;
    _state.SOC = _SOC_init;
}
void capacity_lithium_ion_t::updateCapacity(double &I, double dt)
{
    _state.DOD_prev = _state.DOD;
    _state.I_loss = 0.;
    _dt_hour = dt;
    _state.I = I;

    // compute charge change ( I > 0 discharging, I < 0 charging)
    _state.q0 -= _state.I*dt;

    // check if SOC constraints violated, update q0, I if so
    check_SOC();

    // update SOC, DOD
    update_SOC();
    check_charge_change();

    // Pass current out
    I = _state.I;
}
void capacity_lithium_ion_t::updateCapacityForThermal(double capacity_percent)
{
    // Modify the lifetime degraded capacity by the thermal effect
    _state.qmax_thermal = _state.qmax*capacity_percent*0.01;
}
void capacity_lithium_ion_t::updateCapacityForLifetime(double capacity_percent)
{

    if (_qmax0* capacity_percent*0.01 <= _state.qmax)
        _state.qmax = _qmax0* capacity_percent*0.01;

    if (_state.q0 > _state.qmax)
    {
        _state.I_loss += (_state.q0 - _state.qmax) / _dt_hour;
        _state.q0 = _state.qmax;
    }

    update_SOC();
}
double capacity_lithium_ion_t::q1(){return _state.q0;}
double capacity_lithium_ion_t::q10(){return _state.qmax;}

