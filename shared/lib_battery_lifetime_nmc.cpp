#include "lib_battery_lifetime_nmc.h"

void lifetime_nmc_t::initialize() {

}

lifetime_nmc_t::lifetime_nmc_t(double dt_hr) {
    params = std::make_shared<lifetime_params>();
    params->model_choice == lifetime_params::NMCNREL;
//    params->;
    initialize();
}

lifetime_nmc_t::lifetime_nmc_t(std::shared_ptr<lifetime_params> params_pt) {
    params = std::move(params_pt);
    initialize();
}

lifetime_t * lifetime_nmc_t::clone() {

}

void lifetime_nmc_t::runLifetimeModels(size_t lifetimeIndex, bool charge_changed, double prev_DOD, double DOD,
                                       double T_battery) {

}

double lifetime_nmc_t::estimateCycleDamage() {

}

void lifetime_nmc_t::replaceBattery(double percent_to_replace) {

}
