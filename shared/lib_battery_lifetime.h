#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_H

#include <memory>

struct calendar_cycle_params;

struct lifetime_params {
    double dt_hour;

    enum MODEL_CHOICE {
        CALCYC,
        NMCNREL         // K. Smith: Life Prediction model coefficients
    } model_choice;

    std::shared_ptr<calendar_cycle_params> cal_cyc;

    lifetime_params();

    lifetime_params &operator=(const lifetime_params &rhs);

    friend std::ostream &operator<<(std::ostream &os, const lifetime_params &p);
};

struct cycle_state;
struct calendar_state;

struct lifetime_state {
    double q_relative;                      // total lifetime relative capacity %
    int n_cycles;
    double range;
    double average_range;
    int day_age_of_battery;

    // CALCYC model state
    std::shared_ptr<calendar_state> calendar;
    std::shared_ptr<cycle_state> cycle;

    lifetime_state();

    lifetime_state(const std::shared_ptr<cycle_state>& cyc, const std::shared_ptr<calendar_state>& cal);

    lifetime_state &operator=(const lifetime_state &rhs);

    friend std::ostream &operator<<(std::ostream &os, const lifetime_state &p);
};

class lifetime_t {
public:
    lifetime_t() = default;

    lifetime_t(const lifetime_t &rhs);

    virtual lifetime_t &operator=(const lifetime_t &rhs);

    virtual lifetime_t *clone() = 0;

    virtual ~lifetime_t() = default;

    /// Execute the lifetime models given the current lifetime run index, capacity model, and temperature
    virtual void runLifetimeModels(size_t lifetimeIndex, bool charge_changed, double prev_DOD, double DOD, double T_battery) = 0;

    /// Return the relative capacity percentage of nominal (%)
    double capacity_percent();

    virtual double estimateCycleDamage() = 0;

    virtual void replaceBattery(double percent_to_replace) = 0;

    lifetime_params get_params();

    lifetime_state get_state();

protected:

    std::shared_ptr<lifetime_state> state;
    std::shared_ptr<lifetime_params> params;

    friend class battery_t;
};


#endif //SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_H
