#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_H

#include "lib_util.h"

/*
Lifetime cycling class.
*/

struct lifetime_params {
    // cycling
    util::matrix_t<double> cycling_matrix;
    enum CYCLING_COLUMNS {
        DOD, CYCLE, CAPACITY_CYCLE
    };

    // calendar
    enum CALENDAR_CHOICE {
        NONE, MODEL, TABLE
    };
    int calendar_choice;
    double dt_hour;

    // K. Smith: Life Prediction model coefficients
    double calendar_model_q0; // unitless
    double calendar_model_a;  // 1/sqrt(day)
    double calendar_model_b;  // K
    double calendar_model_c;  // K

    // table entries
    util::matrix_t<double> calendar_matrix;
    enum CALENDAR_COLUMNS {
        DAYS, CAPACITY_CAL
    };

    friend std::ostream &operator<<(std::ostream &os, const lifetime_params &p);
};

struct cycle_state {
    double q_relative_cycle;                // %
    int n_cycles;
    double range;
    double average_range;
    enum RAINFLOW_CODES {
        LT_SUCCESS, LT_GET_DATA, LT_RERANGE
    };
    double rainflow_Xlt;
    double rainflow_Ylt;
    int rainflow_jlt;                // last index in Peaks, i.e, if Peaks = [0,1], then jlt = 1
    std::vector<double> peaks;

    friend std::ostream &operator<<(std::ostream &os, const cycle_state &p);
};

class lifetime_cycle_t {

public:
    /// Constructor for independent model, owning its state and params
    lifetime_cycle_t(const util::matrix_t<double> &batt_lifetime_matrix);

    /// Constructor as lifetime_t component
    lifetime_cycle_t(std::shared_ptr<lifetime_params> params_ptr);

    lifetime_cycle_t(const lifetime_cycle_t &rhs);

    lifetime_cycle_t &operator=(const lifetime_cycle_t &rhs);

    lifetime_cycle_t *clone();

    /// return q, the effective capacity percent
    double runCycleLifetime(double DOD);

    /// return hypothetical dq the average cycle
    double estimateCycleDamage();

    /// Return the relative capacity percentage of nominal (%)
    double capacity_percent();

    /// Run the rainflow counting algorithm at the current depth-of-discharge to determine cycle
    void rainflow(double DOD);

    /// Replace or partially replace a batteyr
    void replaceBattery(double replacement_percent);

    /// Return the total cycles elapse
    int cycles_elapsed();

    /// Return the range of the last cycle
    double cycle_range();

    /// Return the average cycle range
    double average_range();

    cycle_state get_state();

protected:

    void rainflow_ranges();

    void rainflow_ranges_circular(int index);

    int rainflow_compareRanges();

    /// Bilinear interpolation, given the depth-of-discharge and cycle number, return the capacity percent
    double bilinear(double DOD, int cycle_number);

    std::shared_ptr<cycle_state> state;
    std::shared_ptr<lifetime_params> params;

private:
    void initialize();

    friend class lifetime_t;
};

/*
Lifetime calendar model
*/

struct calendar_state {
    double q_relative_calendar;            // %
    int day_age_of_battery;
    size_t last_idx;
    double dq_relative_calendar_old;       // (0 - 1)

    friend std::ostream &operator<<(std::ostream &os, const calendar_state &p);
};

class lifetime_calendar_t {
public:
    /// Constructors for independent models, owning its state and params
    lifetime_calendar_t(double dt_hour, const util::matrix_t<double>& calendar_matrix);
    lifetime_calendar_t(double dt_hour, double q0= 1.02, double a= 2.66e-3, double b= -7280, double c= 930);

    /// Constructor as lifetime_t component
    lifetime_calendar_t(std::shared_ptr<lifetime_params> params_ptr);

    lifetime_calendar_t(const lifetime_calendar_t &rhs);

    lifetime_calendar_t &operator=(const lifetime_calendar_t &rhs);

    lifetime_calendar_t *clone();

    /// Given the index of the simulation, the tempertature and SOC, return the effective capacity percent
    double runLifetimeCalendarModel(size_t idx, double T, double SOC);

    /// Reset or augment the capacity
    void replaceBattery(double replacement_percent);

    /// Return the relative capacity percentage of nominal (%)
    double capacity_percent();

    calendar_state get_state();

protected:
    void runLithiumIonModel(double T, double SOC);

    void runTableModel();

    double dt_day;

    std::shared_ptr<calendar_state> state;
    std::shared_ptr<lifetime_params> params;

private:
    void initialize();

    friend class lifetime_t;
};

/*
Class to encapsulate multiple lifetime models, and linearly combined the associated degradation and handle replacements
*/

struct lifetime_state {
    double q_relative;                      // total lifetime relative capacity %

    std::shared_ptr<cycle_state> cycle;
    std::shared_ptr<calendar_state> calendar;

    lifetime_state();

    lifetime_state &operator=(const lifetime_state &rhs);

    friend std::ostream &operator<<(std::ostream &os, const lifetime_state &p);
};

class lifetime_t {
public:
    /// Cycle with Calendar table
    lifetime_t(const util::matrix_t<double> &batt_lifetime_matrix,
               double dt_hour, const util::matrix_t<double>& calendar_matrix);

    /// Cycle with Calendar model
    lifetime_t(const util::matrix_t<double> &batt_lifetime_matrix,
               double dt_hour, double q0, double a, double b, double c);

    /// Cycle with no Calendar
    lifetime_t(const util::matrix_t<double> &batt_lifetime_matrix, double dt_hour);

    lifetime_t(std::shared_ptr<lifetime_params> params_ptr);

    lifetime_t(const lifetime_t& rhs);

    lifetime_t &operator=(const lifetime_t& rhs);

    virtual ~lifetime_t() {};

    lifetime_t *clone();

    /// Execute the lifetime models given the current lifetime run index, capacity model, and temperature
    void runLifetimeModels(size_t idx, bool charge_changed, double prev_DOD, double DOD, double T_battery);

    double estimateCycleDamage();

    void replaceBattery(double percent_to_replace);

    /// Return the relative capacity percentage of nominal (%)
    double capacity_percent();

    /// Return the relative capacity percentage of nominal caused by cycle damage (%)
    double capacity_percent_cycle();

    /// Return the relative capacity percentage of nominal caused by calendar fade (%)
    double capacity_percent_calendar();

    lifetime_params get_params();

    lifetime_state get_state();

protected:

    std::shared_ptr<lifetime_state> state;
    std::shared_ptr<lifetime_params> params;

    std::unique_ptr<lifetime_calendar_t> calendar_model;
    std::unique_ptr<lifetime_cycle_t> cycle_model;

private:
    void initialize();
};


#endif //SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_H
