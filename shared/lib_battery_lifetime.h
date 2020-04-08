#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_H

#include "lib_util.h"

/*
Lifetime cycling class.
*/

class lifetime_cycle_t
{

public:
    lifetime_cycle_t(const util::matrix_t<double> &cyles_vs_DOD);
    virtual ~lifetime_cycle_t();

    /// deep copy
    lifetime_cycle_t * clone();

    /// copy from lifetime_cycle to this
    void copy(lifetime_cycle_t *);

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

protected:

    void rainflow_ranges();
    void rainflow_ranges_circular(int index);
    int rainflow_compareRanges();

    /// Bilinear interpolation, given the depth-of-discharge and cycle number, return the capacity percent
    double bilinear(double DOD, int cycle_number);

    util::matrix_t<double> _cycles_vs_DOD;
    util::matrix_t<double> _batt_lifetime_matrix;
    std::vector<double> _DOD_vect;
    std::vector<double> _cycles_vect;
    std::vector<double> _capacities_vect;


    int _nCycles;
    double _q;				// relative capacity %
    double _Dlt;			// % damage according to rainflow
    int _jlt;			    // last index in Peaks, i.e, if Peaks = [0,1], then _jlt = 1
    double _Xlt;
    double _Ylt;
    std::vector<double> _Peaks;
    double _Range;
    double _average_range;

    enum RETURN_CODES
    {
        LT_SUCCESS,
        LT_GET_DATA,
        LT_RERANGE
    };
};
/*
Lifetime calendar model
*/
class lifetime_calendar_t
{
public:
    lifetime_calendar_t(int calendar_choice, util::matrix_t<double> calendar_matrix, double dt_hour,
                        float q0=1.02, float a=2.66e-3, float b=-7280, float c=930);
    virtual ~lifetime_calendar_t(){/* Nothing to do */};

    // deep copy
    lifetime_calendar_t * clone();

    // copy from lifetime_calendar to this
    void copy(lifetime_calendar_t *);

    /// Given the index of the simulation, the tempertature and SOC, return the effective capacity percent
    double runLifetimeCalendarModel(size_t idx, double T, double SOC);

    /// Reset or augment the capacity
    void replaceBattery(double replacement_percent);

    /// Return the relative capacity percentage of nominal (%)
    double capacity_percent();

    enum CALENDAR_LOSS_OPTIONS {NONE, LITHIUM_ION_CALENDAR_MODEL, CALENDAR_LOSS_TABLE};

protected:
    void runLithiumIonModel(double T, double SOC);
    void runTableModel();

private:

    int _calendar_choice;
    std::vector<int> _calendar_days;
    std::vector<double> _calendar_capacity;

    int _day_age_of_battery;

    double _dt_hour; // timestep in hours
    double _dt_day; // timestep in terms of days


    // the last index of the simulation
    size_t _last_idx;

    // relative capacity (0 - 1)
    double _q;
    double _dq_old;
    double _dq_new;

    // K. Smith: Life Prediction model coeffiecients
    float _q0; // unitless
    float _a;  // 1/sqrt(day)
    float _b;  // K
    float _c;  // K
};

/*
Class to encapsulate multiple lifetime models, and linearly combined the associated degradation and handle replacements
*/
class lifetime_t
{
public:
    lifetime_t(lifetime_cycle_t *, lifetime_calendar_t *, const int replacement_option, const double replacement_capacity);
    virtual ~lifetime_t(){};

    /// Deep copy
    lifetime_t * clone();

    /// Delete deep copy
    void delete_clone();

    /// Copy lifetime to this
    void copy(lifetime_t *);

    /// Execute the lifetime models given the current lifetime run index, capacity model, and temperature
    void runLifetimeModels(size_t idx, bool charge_changed, double prev_DOD, double DOD, double T_battery);

    /// Return the relative capacity percentage of nominal (%)
    double capacity_percent();

    /// Return the relative capacity percentage of nominal caused by cycle damage (%)
    double capacity_percent_cycle();

    /// Return the relative capacity percentage of nominal caused by calendar fade (%)
    double capacity_percent_calendar();

    /// Return pointer to underlying lifetime cycle model
    lifetime_cycle_t * cycleModel() { return _lifetime_cycle; }

    /// Return pointer to underlying lifetime capacity model
    lifetime_calendar_t * calendarModel() { return _lifetime_calendar; }

    /// Check if the battery should be replaced based upon the replacement criteria
    bool check_replaced();

    /// Reset the number of replacements at the year end
    void reset_replacements();

    /// Return the number of total replacements in the year
    int get_replacements();

    /// Return the replacement percent
    double get_replacement_percent();

    /// Set the replacement option
    void set_replacement_option(int option);

    /// Replace the battery and reset the lifetime degradation
    void force_replacement(double replacement_percent);

protected:

    /// Underlying lifetime cycle model
    lifetime_cycle_t * _lifetime_cycle;

    /// Underlying lifetime calendar model
    lifetime_calendar_t * _lifetime_calendar;

    /// Replacement option, 0 = none, 1 = replace at capacity 2 = replace by schedule
    int _replacement_option;

    /// Maximum capacity relative to nameplate at which to replace battery
    double _replacement_capacity;

    /// Number of replacements this year
    int _replacements;

    /// Boolean describing if replacement has been scheduled
    bool _replacement_scheduled;

    /// Percentage of how much capacity to replace (0 - 100%)
    double _replacement_percent;

    /// battery relative capacity (0 - 100%)
    double _q;
};



#endif //SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_H
