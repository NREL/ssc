#include "lib_battery_lifetime_calendar_cycle.h"
#include "lib_battery_lifetime_nmc.h"
#include "lib_battery_lifetime.h"

void lifetime_nmc_t::initialize() {
    
    // cycle model for counting cycles only, no cycle-only degradation
    cycle_model = std::unique_ptr<lifetime_cycle_t>(new lifetime_cycle_t(params, state));
    // do any state initialization here
    state->nmc_state->q_relative_li = 100;
    state->nmc_state->q_relative_neg = 100;
    state->nmc_state->b1_dt.clear();
    state->nmc_state->b2_dt.clear();
    state->nmc_state->b3_dt.clear();

}

lifetime_nmc_t::lifetime_nmc_t(double dt_hr) {
    params = std::make_shared<lifetime_params>();
    params->model_choice = lifetime_params::NMCNREL;
    params->dt_hr = dt_hr;
    state = std::make_shared<lifetime_state>();
    initialize();
}

lifetime_nmc_t::lifetime_nmc_t(std::shared_ptr<lifetime_params> params_pt) {
    params = std::move(params_pt);
    initialize();
}

lifetime_nmc_t::lifetime_nmc_t(const lifetime_nmc_t &rhs) :
        lifetime_t(rhs){
    operator=(rhs);
}

lifetime_nmc_t& lifetime_nmc_t::operator=(const lifetime_nmc_t& rhs) {
    if (this != &rhs) {
        *params = *rhs.params;
        *state = *rhs.state;
    }
    return *this;
}

lifetime_t * lifetime_nmc_t::clone() {
    return new lifetime_nmc_t(*this);
}

// Rohit - Define negative electrode voltage function
double lifetime_nmc_t::Uneg_computation(double SOC) {
    double Uneg = 0.1;
    if (SOC <= 0.1)
        Uneg = ((0.2420 - 1.2868) / 0.1) * SOC + 1.2868;
    else
        Uneg = ((0.0859 - 0.2420) / 0.9) * (SOC - 0.1) + 0.2420;
    return Uneg;
}

// Rohit - Define open circuit voltage function
double lifetime_nmc_t::Voc_computation(double SOC) {
    double Voc = 0.1;
    if (SOC <= 0.1)
        Voc = ((0.4679) / 0.1) * SOC + 3;
    else if (SOC <= 0.6)
        Voc = ((3.747 - 3.4679) / 0.5) * (SOC - 0.1) + 3.4679;
    else
        Voc = ((4.1934 - 3.7469) / 0.4) * (SOC - 0.6) + 3.7469;
    return Voc;
}

void lifetime_nmc_t::runLifetimeModels(size_t lifetimeIndex, bool charge_changed, double prev_DOD, double DOD,
                                       double T_battery) {
    if (charge_changed)
        cycle_model->rainflow(prev_DOD);

    state->day_age_of_battery = (int)(lifetimeIndex / (util::hours_per_day / params->dt_hr));

}

double lifetime_nmc_t::estimateCycleDamage() {
    return 0.1;
}

void lifetime_nmc_t::replaceBattery(double percent_to_replace) {

}
