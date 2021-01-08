#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_NMC_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_NMC_H

#include <memory>

#include "lib_util.h"
#include "lib_battery_lifetime_calendar_cycle.h"

class lifetime_nmc_t : public lifetime_t {
public:
    lifetime_nmc_t(double dt_hr);

    lifetime_nmc_t(std::shared_ptr<lifetime_params> params_pt);

    lifetime_nmc_t(const lifetime_nmc_t& rhs);

    lifetime_nmc_t &operator=(const lifetime_nmc_t& rhs);

    lifetime_t *clone() override;

    /// Execute the lifetime models given the current lifetime run index, capacity model, and temperature
    void runLifetimeModels(size_t lifetimeIndex, bool charge_changed, double prev_DOD, double DOD, double T_battery) override;

    double estimateCycleDamage() override;

    void replaceBattery(double percent_to_replace) override;

protected:
    std::unique_ptr<lifetime_cycle_t> cycle_model;

    void initialize();

    // add constants / coefficients / data here
};


#endif //SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_NMC_H
