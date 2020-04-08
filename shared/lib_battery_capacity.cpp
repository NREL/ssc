#include <cmath>

#include "lib_battery_capacity.h"

double low_tolerance = 0.01;
double tolerance = 0.001;

/*
Define Capacity Model
*/
capacity_t::capacity_t(){
    params = std::make_shared<capacity_params>();
    state = capacity_state();
}

capacity_t::capacity_t(double q, double SOC_init, double SOC_max, double SOC_min, double dt_hour):
capacity_t()
{
    state.q0 = 0.01*SOC_init*q;
    state.qmax_lifetime = q;
    state.qmax_thermal = q;
    params->qmax_init = q;
    state.I = 0.;
    state.I_loss = 0.;
    params->dt_hour = dt_hour;

    // Initialize SOC, DOD
    state.SOC = SOC_init;
    params->SOC_init = SOC_init;
    params->SOC_max = SOC_max;
    params->SOC_min = SOC_min;
    state.DOD = 100. - state.SOC;
    state.DOD_prev = 0;

    // Initialize charging states
    state.prev_charge = DISCHARGE;
    state.charge_mode = DISCHARGE;
    state.chargeChange = false;
}

capacity_t::capacity_t(const capacity_t& rhs):
        params(rhs.params),
        state(rhs.state){
}

capacity_t& capacity_t::operator= (const capacity_t& rhs){
    params = rhs.params;
    state = rhs.state;
    return *this;
}

void capacity_t::check_charge_change()
{
    state.charge_mode = NO_CHARGE;

    // charge state
    if (state.I < 0)
        state.charge_mode = CHARGE;
    else if (state.I > 0)
        state.charge_mode = DISCHARGE;

    // Check if charge changed
    state.chargeChange = false;
    if ((state.charge_mode != state.prev_charge) && (state.charge_mode != NO_CHARGE) && (state.prev_charge != NO_CHARGE))
    {
        state.chargeChange = true;
        state.prev_charge = state.charge_mode;
    }
}
int capacity_t::charge_operation(){ return state.charge_mode; }
void capacity_t::check_SOC()
{
    double q_upper = state.qmax_lifetime * params->SOC_max * 0.01;
    double q_lower = state.qmax_lifetime * params->SOC_min * 0.01;
    double I_orig = state.I;

    // set capacity to upper thermal limit
    if (q_upper > state.qmax_thermal * params->SOC_max * 0.01) {
        q_upper = state.qmax_thermal * params->SOC_max * 0.01;
    }
    // do this so battery can cycle full depth and we calculate correct SOC min
    if (q_lower > state.qmax_thermal * params->SOC_min * 0.01) {
        q_lower = state.qmax_thermal * params->SOC_min * 0.01;
    }

    // check if overcharged
    if (state.q0 > q_upper )
    {
        if (fabs(state.I) > tolerance)
        {
            state.I += (state.q0 - q_upper) / params->dt_hour;
            if (state.I / I_orig < 0)
                state.I = 0;
        }
        state.q0 = q_upper;
    }
        // check if undercharged
    else if (state.q0 < q_lower)
    {
        if (fabs(state.I) > tolerance)
        {
            state.I += (state.q0 - q_lower) / params->dt_hour;
            if (state.I / I_orig < 0)
                state.I = 0;
        }
        state.q0 = q_lower;
    }
}

void capacity_t::update_SOC()
{
    double max = fmin(state.qmax_lifetime, state.qmax_thermal);
    if (max == 0){
        state.q0 = 0;
        state.SOC = 0;
        state.DOD = 100;
        return;
    }
    if (state.q0 > max)
        state.q0 = max;
    if (state.qmax_lifetime > 0)
        state.SOC = 100.*(state.q0 / max);
    else
        state.SOC = 0.;

    // due to dynamics, it's possible SOC could be slightly above 1 or below 0
    if (state.SOC > 100.0)
        state.SOC = 100.0;
    else if (state.SOC < 0.)
        state.SOC = 0.;

    state.DOD = 100. - state.SOC;
}
bool capacity_t::chargeChanged(){return state.chargeChange;}
double capacity_t::SOC_max(){ return params->SOC_max; }
double capacity_t::SOC_min(){ return params->SOC_min; }
double capacity_t::SOC(){ return state.SOC; }
double capacity_t::DOD(){ return state.DOD; }
double capacity_t::DOD_max(){ return params->SOC_max - params->SOC_min; }
double capacity_t::prev_DOD(){ return state.DOD_prev; }
double capacity_t::q0(){ return state.q0;}
double capacity_t::qmax(){ return state.qmax_lifetime; }
double capacity_t::qmax_thermal(){ return state.qmax_thermal; }
double capacity_t::I(){ return state.I; }
double capacity_t::I_loss() { return state.I_loss; }

/*
Define KiBam Capacity Model
*/
capacity_kibam_t::capacity_kibam_t():
        capacity_t(){}

capacity_kibam_t::capacity_kibam_t(double q20, double t1, double q1, double q10, double SOC_init, double SOC_max, double SOC_min, double dt_hr) :
        capacity_t(q20, SOC_init, SOC_max, SOC_min, dt_hr)
{
    params->leadacid.q10 = q10;
    params->leadacid.q20 = q20;
    params->leadacid.I20 = q20/20.;

    // parameters for c, k calculation
    state.leadacid.q1 = q1;
    state.leadacid.q2 = q10;
    params->leadacid.t1 = t1;
    params->leadacid.t2 = 10.;
    params->leadacid.F1 = q1 / q20; // use t1, 20
    params->leadacid.F2 = q1 / q10;  // use t1, 10

    // compute the parameters
    parameter_compute();
    state.qmax_thermal = state.qmax_lifetime;
    params->qmax_init = state.qmax_lifetime;
    state.q0 = params->qmax_init * SOC_init * 0.01;

    // initializes to full battery
    capacity_kibam_t::replace_battery(100);
}

capacity_kibam_t& capacity_kibam_t::operator= (const capacity_t& rhs){
    capacity_t::operator=(rhs);
    auto rhs_p = dynamic_cast<capacity_kibam_t*>(const_cast<capacity_t*>(&rhs));
    c = rhs_p->c;
    k = rhs_p->k;
    return *this;
}

capacity_kibam_t::capacity_kibam_t(const capacity_kibam_t& rhs):
capacity_t(rhs) {
    operator=(rhs);
}

capacity_t* capacity_kibam_t::clone(){
    return new capacity_kibam_t(*this);
}

void capacity_kibam_t::replace_battery(double replacement_percent)
{
    replacement_percent = fmax(0, replacement_percent);
    double qmax_old = state.qmax_lifetime;
    state.qmax_lifetime += replacement_percent * 0.01* params->qmax_init;
    state.qmax_lifetime = fmin(state.qmax_lifetime, params->qmax_init);
    state.qmax_thermal = state.qmax_lifetime;
    state.q0 += (state.qmax_lifetime-qmax_old)*params->SOC_init*0.01;
    state.leadacid.q1_0 = state.q0*c;
    state.leadacid.q2_0 = state.q0 - state.leadacid.q1_0;
    state.SOC = params->SOC_init;
    update_SOC();
}

double capacity_kibam_t::c_compute(double F, double t1, double t2, double k_guess)
{
    double num = F*(1 - exp(-k_guess*t1))*t2 - (1 - exp(-k_guess*t2))*t1;
    double denom = F*(1 - exp(-k_guess*t1))*t2 - (1 - exp(-k_guess*t2))*t1 - k_guess*F*t1*t2 + k_guess*t1*t2;
    return (num / denom);
}

double capacity_kibam_t::q1_compute(double q10, double q0, double dt, double I)
{
    double A = q10*exp(-k*dt);
    double B = (q0*k*c - I)*(1 - exp(-k*dt)) / k;
    double C = I*c*(k*dt - 1 + exp(-k*dt)) / k;
    return (A + B - C);
}

double capacity_kibam_t::q2_compute(double q20, double q0, double dt, double I)
{
    double A = q20*exp(-k*dt);
    double B = q0*(1 - c)*(1 - exp(-k*dt));
    double C = I*(1 - c)*(k*dt - 1 + exp(-k*dt)) / k;
    return (A + B - C);
}

double capacity_kibam_t::Icmax_compute(double q10, double q0, double dt)
{
    double num = -k*c*state.qmax_lifetime + k*q10*exp(-k*dt) + q0*k*c*(1 - exp(-k*dt));
    double denom = 1 - exp(-k*dt) + c*(k*dt - 1 + exp(-k*dt));
    return (num / denom);
}

double capacity_kibam_t::Idmax_compute(double q10, double q0, double dt)
{
    double num = k*q10*exp(-k*dt) + q0*k*c*(1 - exp(-k*dt));
    double denom = 1 - exp(-k*dt) + c*(k*dt - 1 + exp(-k*dt));
    return (num / denom);
}

double capacity_kibam_t::qmax_compute()
{
    double num = params->leadacid.q20*((1 - exp(-k * 20)) * (1 - c) + k*c * 20);
    double denom = k*c * 20;
    return (num / denom);
}

double capacity_kibam_t::qmax_of_i_compute(double T)
{
    return ((state.qmax_lifetime*k*c*T) / (1 -exp(-k*T) + c*(k*T - 1 + exp(-k*T))));
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
        c1 = c_compute(params->leadacid.F1, params->leadacid.t1, 20, k_guess);
        c2 = c_compute(params->leadacid.F2, params->leadacid.t1, params->leadacid.t2, k_guess);

        if (fabs(c1 - c2) < minRes)
        {
            minRes = fabs(c1 - c2);
            k = k_guess;
            c = 0.5*(c1 + c2);
        }
    }
    state.qmax_lifetime = qmax_compute();
}

void capacity_kibam_t::updateCapacity(double &I, double dt_hour)
{
    if (fabs(I) < low_tolerance)
        I = 0;

    state.DOD_prev = state.DOD;
    state.I_loss = 0.;
    state.I = I;
    params->dt_hour = dt_hour;

    double Idmax = 0.;
    double Icmax = 0.;
    double Id = 0.;
    double Ic = 0.;
    double q1 = 0.;
    double q2 = 0.;

    if (state.I > 0)
    {
        Idmax = Idmax_compute(state.leadacid.q1_0, state.q0, dt_hour);
        Id = fmin(state.I, Idmax);
        state.I = Id;
    }
    else if (state.I < 0)
    {
        Icmax = Icmax_compute(state.leadacid.q1_0, state.q0, dt_hour);
        Ic = -fmin(fabs(state.I), fabs(Icmax));
        state.I = Ic;
    }

    // new charge levels
    q1 = q1_compute(state.leadacid.q1_0, state.q0, dt_hour, state.I);
    q2 = q2_compute(state.leadacid.q2_0, state.q0, dt_hour, state.I);

    // Check for thermal effects
    if (q1 + q2 > state.qmax_thermal)
    {
        double q0 = q1 + q2;
        double p1 = q1 / q0;
        double p2 = q2 / q0;
        state.q0 = state.qmax_thermal;
        q1 = state.q0*p1;
        q2 = state.q0*p2;
    }

    // update internal variables
    state.leadacid.q1_0 = q1;
    state.leadacid.q2_0 = q2;
    state.q0 = q1 + q2;

    update_SOC();
    check_charge_change();

    // Pass current out
    I = state.I;
}
void capacity_kibam_t::updateCapacityForThermal(double capacity_percent)
{
    if (capacity_percent < 0)
        capacity_percent = 0;
    // Modify the lifetime degraded capacity by the thermal effect
    state.qmax_thermal = state.qmax_lifetime*capacity_percent*0.01;

    // scale to q0 = qmax if q0 > qmax
    if (state.q0 > state.qmax_thermal)
    {
        double q0_orig = state.q0;
        double p = state.qmax_thermal / state.q0;
        state.q0 *= p;
        state.leadacid.q1 *= p;
        state.leadacid.q2 *= p;
        state.I_loss += (q0_orig - state.q0) / params->dt_hour;
    }
    update_SOC();
}
void capacity_kibam_t::updateCapacityForLifetime(double capacity_percent)
{
    if (capacity_percent < 0)
        capacity_percent = 0;
    if (params->qmax_init* capacity_percent*0.01 <= state.qmax_lifetime)
        state.qmax_lifetime = params->qmax_init* capacity_percent*0.01;

    // scale to q0 = qmax if q0 > qmax
    if (state.q0 > state.qmax_lifetime)
    {
        double q0_orig = state.q0;
        double p = state.qmax_lifetime / state.q0;
        state.q0 *= p;
        state.leadacid.q1 *= p;
        state.leadacid.q2 *= p;
        state.I_loss += (q0_orig - state.q0) / params->dt_hour;
    }
    update_SOC();
}

double capacity_kibam_t::q1(){ return state.leadacid.q1_0; }
double capacity_kibam_t::q2(){ return state.leadacid.q2_0; }
double capacity_kibam_t::q10(){ return params->leadacid.q10; }
double capacity_kibam_t::q20(){return params->leadacid.q20;}


/*
Define Lithium Ion capacity model
*/
capacity_lithium_ion_t::capacity_lithium_ion_t():
capacity_t(){}

capacity_lithium_ion_t::capacity_lithium_ion_t(double q, double SOC_init, double SOC_max, double SOC_min, double dt_hr):
        capacity_t(q, SOC_init, SOC_max, SOC_min, dt_hr) {
}

capacity_lithium_ion_t::capacity_lithium_ion_t(const capacity_lithium_ion_t& rhs):
capacity_t(rhs){}

capacity_lithium_ion_t& capacity_lithium_ion_t::operator= (const capacity_t& rhs){
    capacity_t::operator=(rhs);
    return *this;
}

capacity_t* capacity_lithium_ion_t::clone(){
    return new capacity_lithium_ion_t(*this);
}

void capacity_lithium_ion_t::replace_battery(double replacement_percent)
{
    replacement_percent = fmax(0, replacement_percent);
    double qmax_old = state.qmax_lifetime;
    state.qmax_lifetime += params->qmax_init * replacement_percent * 0.01;
    state.qmax_lifetime = fmin(params->qmax_init, state.qmax_lifetime);
    state.qmax_thermal = state.qmax_lifetime;
    state.q0 += (state.qmax_lifetime-qmax_old) * params->SOC_init * 0.01;
    state.SOC = params->SOC_init;
    update_SOC();
}
void capacity_lithium_ion_t::updateCapacity(double &I, double dt)
{
    state.DOD_prev = state.DOD;
    state.I_loss = 0.;
    params->dt_hour = dt;
    state.I = I;

    // compute charge change ( I > 0 discharging, I < 0 charging)
    state.q0 -= state.I*dt;

    // check if SOC constraints violated, update q0, I if so
    check_SOC();

    // update SOC, DOD
    update_SOC();
    check_charge_change();

    // Pass current out
    I = state.I;
}
void capacity_lithium_ion_t::updateCapacityForThermal(double capacity_percent)
{
    if (capacity_percent < 0)
        capacity_percent = 0;
    // Modify the lifetime degraded capacity by the thermal effect
    state.qmax_thermal = state.qmax_lifetime*capacity_percent*0.01;
    if (state.q0 > state.qmax_thermal)
    {
        state.I_loss += (state.q0 - state.qmax_thermal) / params->dt_hour;
        state.q0 = state.qmax_thermal;
    }
    update_SOC();
}
void capacity_lithium_ion_t::updateCapacityForLifetime(double capacity_percent)
{
    if (capacity_percent < 0)
        capacity_percent = 0;
    if (params->qmax_init* capacity_percent*0.01 <= state.qmax_lifetime)
        state.qmax_lifetime = params->qmax_init* capacity_percent*0.01;

    if (state.q0 > state.qmax_lifetime)
    {
        state.I_loss += (state.q0 - state.qmax_lifetime) / params->dt_hour;
        state.q0 = state.qmax_lifetime;
    }

    update_SOC();
}
double capacity_lithium_ion_t::q1(){return state.q0;}
double capacity_lithium_ion_t::q10(){return state.qmax_lifetime;}
