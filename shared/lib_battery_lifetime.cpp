
#include "lib_battery_lifetime.h"

extern double tolerance;
extern double low_tolerance;

/*
Define Lifetime Model
*/
lifetime_t::lifetime_t(lifetime_cycle_t * lifetime_cycle, lifetime_calendar_t * lifetime_calendar, const int replacement_option, const double replacement_capacity)
{
    _lifetime_cycle = lifetime_cycle;
    _lifetime_calendar = lifetime_calendar;

    _replacement_option = replacement_option;
    _replacement_capacity = replacement_capacity;
    _replacement_percent = 100;

    // issues as capacity approaches 0%
    if (replacement_capacity == 0.) { _replacement_capacity = 2.; }
    _replacements = 0;
    _replacement_scheduled = false;

    // relative capacity
    _q = 100;
}
lifetime_t * lifetime_t::clone()
{
    lifetime_t * tmp = new lifetime_t(*this);
    tmp->_lifetime_calendar = _lifetime_calendar->clone();
    tmp->_lifetime_cycle = _lifetime_cycle->clone();
    return tmp;
}
void lifetime_t::delete_clone()
{
    if (_lifetime_calendar) delete _lifetime_calendar;
    if (_lifetime_cycle) delete _lifetime_cycle;
}
void lifetime_t::copy(lifetime_t * lifetime)
{
    _lifetime_cycle->copy(lifetime->_lifetime_cycle);
    _lifetime_calendar->copy(lifetime->_lifetime_calendar);

    _replacement_option = lifetime->_replacement_option;
    _replacement_capacity = lifetime->_replacement_capacity;
    _replacements = lifetime->_replacements;
    _replacement_scheduled = lifetime->_replacement_scheduled;
    _q = lifetime->_q;
}
double lifetime_t::capacity_percent(){ return _q; }
double lifetime_t::capacity_percent_cycle() { return _lifetime_cycle->capacity_percent(); }
double lifetime_t::capacity_percent_calendar() { return _lifetime_calendar->capacity_percent(); }

void lifetime_t::runLifetimeModels(size_t idx, bool charge_changed, double prev_DOD, double DOD, double T_battery)
{
    double q_last = _q;
    double q_cycle = _q;
    double q_calendar = _q;

    if (_q > 0)
    {
        if (charge_changed)
            q_cycle = _lifetime_cycle->runCycleLifetime(prev_DOD);
        else if (idx==0)
            q_cycle = _lifetime_cycle->runCycleLifetime(DOD);

        q_calendar = _lifetime_calendar->runLifetimeCalendarModel(idx, T_battery, 100. - DOD);

        // total capacity is min of cycle (Q_neg) and calendar (Q_li) capacity
        _q = fmin(q_cycle, q_calendar);
    }
    if (_q < 0)
        _q = 0;

    // capacity cannot increase
    if (_q > q_last)
        _q = q_last;
}

bool lifetime_t::check_replaced()
{
    bool replaced = false;
    if ((_replacement_option == 1 && (_q - tolerance) <= _replacement_capacity) || _replacement_scheduled)
    {
        _replacements++;

        _q += _replacement_percent;

        // for now, only allow augmenting up to original installed capacity
        _q = fmin(100., _q);

        replaced = true;
        _replacement_scheduled = false;

        _lifetime_cycle->replaceBattery(_replacement_percent);
        _lifetime_calendar->replaceBattery(_replacement_percent);
    }
    return replaced;
}
void lifetime_t::reset_replacements(){ _replacements = 0; }
int lifetime_t::get_replacements(){ return _replacements; }
double lifetime_t::get_replacement_percent() {
    return _replacement_percent;
};

void lifetime_t::set_replacement_option(int option) { _replacement_option = option; }
void lifetime_t::force_replacement(double replacement_percent){
    _replacement_scheduled = true;
    _replacement_percent = replacement_percent;
}

lifetime_cycle_t::lifetime_cycle_t(const util::matrix_t<double> &batt_lifetime_matrix)
{

    _batt_lifetime_matrix = batt_lifetime_matrix;
    for (int i = 0; i <(int)_batt_lifetime_matrix.nrows(); i++)
    {
        _DOD_vect.push_back(batt_lifetime_matrix.at(i,0));
        _cycles_vect.push_back(batt_lifetime_matrix.at(i,1));
        _capacities_vect.push_back(batt_lifetime_matrix.at(i, 2));
    }
    // initialize other member variables
    _nCycles = 0;
    _Dlt = 0;
    _q = bilinear(0.,0);
    _jlt = 0;
    _Xlt = 0;
    _Ylt = 0;
    _Range = 0;
    _average_range = 0;
}

lifetime_cycle_t::~lifetime_cycle_t(){}
lifetime_cycle_t * lifetime_cycle_t::clone(){ return new lifetime_cycle_t(*this); }
void lifetime_cycle_t::copy(lifetime_cycle_t * lifetime_cycle)
{
    // doesn't change (and potentially slow)
    /*
    _cycles_vs_DOD = lifetime_cycle->_cycles_vs_DOD;
    _batt_lifetime_matrix = lifetime_cycle->_batt_lifetime_matrix;
    _DOD_vect = lifetime_cycle->_DOD_vect;
    _cycles_vect = lifetime_cycle->_cycles_vect;
    _capacities_vect = lifetime_cycle->_capacities_vect;
    */

    _nCycles = lifetime_cycle->_nCycles;
    _q = lifetime_cycle->_q;
    _Dlt = lifetime_cycle->_Dlt;
    _jlt = lifetime_cycle->_jlt;
    _Xlt = lifetime_cycle->_Xlt;
    _Ylt = lifetime_cycle->_Ylt;
    _Peaks = lifetime_cycle->_Peaks;
    _Range = lifetime_cycle->_Range;
    _average_range = lifetime_cycle->_average_range;
}
double lifetime_cycle_t::estimateCycleDamage()
{
    // Initialize assuming 50% DOD
    double DOD = 50;
    if (_average_range > 0){
        DOD = _average_range;
    }
    return(bilinear(DOD, _nCycles+1) - bilinear(DOD, _nCycles + 2));
}
double lifetime_cycle_t::runCycleLifetime(double DOD)
{
    rainflow(DOD);

    // return the effective capacity (Q_neg)
    return _q;
}

void lifetime_cycle_t::rainflow(double DOD)
{
    // initialize return code
    int retCode = LT_GET_DATA;

    // Begin algorithm
    _Peaks.push_back(DOD);
    bool atStepTwo = true;

    // Loop until break
    while (atStepTwo)
    {
        // Rainflow: Step 2: Form ranges X,Y
        if (_jlt >= 2)
            rainflow_ranges();
        else
        {
            // Get more data (Step 1)
            retCode = LT_GET_DATA;
            break;
        }

        // Rainflow: Step 3: Compare ranges
        retCode = rainflow_compareRanges();

        // We break to get more data, or if we are done with step 5
        if (retCode == LT_GET_DATA)
            break;
    }

    if (retCode == LT_GET_DATA)
        _jlt++;
}

void lifetime_cycle_t::rainflow_ranges()
{
    _Ylt = fabs(_Peaks[_jlt - 1] - _Peaks[_jlt - 2]);
    _Xlt = fabs(_Peaks[_jlt] - _Peaks[_jlt - 1]);
}
void lifetime_cycle_t::rainflow_ranges_circular(int index)
{
    size_t end = _Peaks.size() - 1;
    if (index == 0)
    {
        _Xlt = fabs(_Peaks[0] - _Peaks[end]);
        _Ylt = fabs(_Peaks[end] - _Peaks[end - 1]);
    }
    else if (index == 1)
    {
        _Xlt = fabs(_Peaks[1] - _Peaks[0]);
        _Ylt = fabs(_Peaks[0] - _Peaks[end]);
    }
    else
        rainflow_ranges();
}

int lifetime_cycle_t::rainflow_compareRanges()
{
    int retCode = LT_SUCCESS;
    bool contained = true;

    // modified to disregard some of algorithm which doesn't work well
    if (_Xlt < _Ylt)
        retCode = LT_GET_DATA;
    else if (_Xlt >= _Ylt)
        contained = false;

    // Step 5: Count range Y, discard peak & valley of Y, go to Step 2
    if (!contained)
    {
        _Range = _Ylt;
        _average_range = (_average_range*_nCycles + _Range) / (_nCycles + 1);
        _nCycles++;

        // the capacity percent cannot increase
        double dq = bilinear(_average_range, _nCycles) - bilinear(_average_range, _nCycles + 1);
        if (dq > 0)
            _q -= dq;

        if (_q < 0)
            _q = 0.;

        // discard peak & valley of Y
        double save = _Peaks[_jlt];
        _Peaks.pop_back();
        _Peaks.pop_back();
        _Peaks.pop_back();
        _Peaks.push_back(save);
        _jlt -= 2;
        // stay in while loop
        retCode = LT_RERANGE;
    }

    return retCode;
}
void lifetime_cycle_t::replaceBattery(double replacement_percent)
{
    _q += replacement_percent;
    _q = fmin(bilinear(0., 0), _q);
    _Dlt = 0.; // seems unused

    // More work to figure out degradation of multiple-aged battery units
    if (replacement_percent == 100) {
        _nCycles = 0;
    }

    _jlt = 0;
    _Xlt = 0;
    _Ylt = 0;
    _Range = 0;
    _Peaks.clear();
}

int lifetime_cycle_t::cycles_elapsed(){ return _nCycles; }
double lifetime_cycle_t::cycle_range(){ return _Range; }
double lifetime_cycle_t::average_range() { return _average_range; }
double lifetime_cycle_t::capacity_percent() { return _q; }

double lifetime_cycle_t::bilinear(double DOD, int cycle_number)
{
    /*
    Work could be done to make this simpler
    Current idea is to interpolate first along the C = f(n) curves for each DOD to get C_DOD_, C_DOD_+
    Then interpolate C_, C+ to get C at the DOD of interest
    */

    std::vector<double> D_unique_vect;
    std::vector<double> C_n_low_vect;
    std::vector<double> D_high_vect;
    std::vector<double> C_n_high_vect;
    std::vector<int> low_indices;
    std::vector<int> high_indices;
    double D = 0.;
    size_t n = 0;
    double C = 100;

    // get unique values of D
    D_unique_vect.push_back(_DOD_vect[0]);
    for (int i = 0; i < (int)_DOD_vect.size(); i++){
        bool contained = false;
        for (int j = 0; j < (int)D_unique_vect.size(); j++){
            if (_DOD_vect[i] == D_unique_vect[j]){
                contained = true;
                break;
            }
        }
        if (!contained){
            D_unique_vect.push_back(_DOD_vect[i]);
        }
    }
    n = D_unique_vect.size();

    if (n > 1)
    {
        // get where DOD is bracketed [D_lo, DOD, D_hi]
        double D_lo = 0;
        double D_hi = 100;

        for (int i = 0; i < (int)_DOD_vect.size(); i++)
        {
            D = _DOD_vect[i];
            if (D < DOD && D > D_lo)
                D_lo = D;
            else if (D >= DOD && D < D_hi)
                D_hi = D;
        }

        // Seperate table into bins
        double D_min = 100.;
        double D_max = 0.;

        for (int i = 0; i < (int)_DOD_vect.size(); i++)
        {
            D = _DOD_vect[i];
            if (D == D_lo)
                low_indices.push_back(i);
            else if (D == D_hi)
                high_indices.push_back(i);

            if (D < D_min){ D_min = D; }
            else if (D > D_max){ D_max = D; }
        }

        // if we're out of the bounds, just make the upper bound equal to the highest input
        if (high_indices.size() == 0)
        {
            for (int i = 0; i != (int)_DOD_vect.size(); i++)
            {
                if (_DOD_vect[i] == D_max)
                    high_indices.push_back(i);
            }
        }

        size_t n_rows_lo = low_indices.size();
        size_t n_rows_hi = high_indices.size();
        size_t n_cols = 2;

        // If we aren't bounded, fill in values
        if (n_rows_lo == 0)
        {
            // Assumes 0% DOD
            for (int i = 0; i < (int)n_rows_hi; i++)
            {
                C_n_low_vect.push_back(0. + i * 500); // cycles
                C_n_low_vect.push_back(100.); // 100 % capacity
            }
        }

        if (n_rows_lo != 0)
        {
            for (int i = 0; i < (int)n_rows_lo; i++)
            {
                C_n_low_vect.push_back(_cycles_vect[low_indices[i]]);
                C_n_low_vect.push_back(_capacities_vect[low_indices[i]]);
            }
        }
        if (n_rows_hi != 0)
        {
            for (int i = 0; i < (int)n_rows_hi; i++)
            {
                C_n_high_vect.push_back(_cycles_vect[high_indices[i]]);
                C_n_high_vect.push_back(_capacities_vect[high_indices[i]]);
            }
        }
        n_rows_lo = C_n_low_vect.size() / n_cols;
        n_rows_hi = C_n_high_vect.size() / n_cols;

        if (n_rows_lo == 0 || n_rows_hi == 0)
        {
            // need a safeguard here
        }

        util::matrix_t<double> C_n_low(n_rows_lo, n_cols, &C_n_low_vect);
        util::matrix_t<double> C_n_high(n_rows_lo, n_cols, &C_n_high_vect);

        // Compute C(D_lo, n), C(D_hi, n)
        double C_Dlo = util::linterp_col(C_n_low, 0, cycle_number, 1);
        double C_Dhi = util::linterp_col(C_n_high, 0, cycle_number, 1);

        if (C_Dlo < 0.)
            C_Dlo = 0.;
        if (C_Dhi > 100.)
            C_Dhi = 100.;

        // Interpolate to get C(D, n)
        C = util::interpolate(D_lo, C_Dlo, D_hi, C_Dhi, DOD);
    }
        // just have one row, single level interpolation
    else
    {
        C = util::linterp_col(_batt_lifetime_matrix, 1, cycle_number, 2);
    }

    return C;
}

/*
Lifetime Calendar Model
*/
lifetime_calendar_t::lifetime_calendar_t(int calendar_choice, util::matrix_t<double> calendar_matrix, double dt_hour,
                                         float q0, float a, float b, float c)
{
    _calendar_choice = calendar_choice;

    _day_age_of_battery = 0;
    _last_idx = 0;

    // coefficients based on fractional capacity (0 - 1)
    _dq_old = 0;
    _dq_new = 0;

    _q0 = q0;
    _a = a;
    _b = b;
    _c = c;

    // output based on percentage capacity (0 - 100%)
    _q = _q0 * 100;

    // timestep
    _dt_hour = dt_hour;
    _dt_day = dt_hour / util::hours_per_day;

    // extract and sort calendar life info from table
    if (_calendar_choice == CALENDAR_LOSS_TABLE)
    {
        for (size_t i = 0; i != calendar_matrix.nrows(); i++)
        {
            _calendar_days.push_back((int)calendar_matrix.at(i, 0));
            _calendar_capacity.push_back(calendar_matrix.at(i, 1));
        }
    }
        // Ensure don't accidently initialize to 0 if not using model
    else if (_calendar_choice == NONE) {
        _q0 = 1.0;
    }
}
lifetime_calendar_t * lifetime_calendar_t::clone(){ return new lifetime_calendar_t(*this); }
void lifetime_calendar_t::copy(lifetime_calendar_t * lifetime_calendar)
{
    _calendar_choice = lifetime_calendar->_calendar_choice;
    _calendar_days = lifetime_calendar->_calendar_days;
    _calendar_capacity = lifetime_calendar->_calendar_capacity;
    _day_age_of_battery = lifetime_calendar->_day_age_of_battery;
    _dt_hour = lifetime_calendar->_dt_hour;
    _dt_day = lifetime_calendar->_dt_day;
    _last_idx = lifetime_calendar->_last_idx;
    _q = lifetime_calendar->_q;
    _dq_old = lifetime_calendar->_dq_old;
    _dq_new = lifetime_calendar->_dq_new;
    _q0 = lifetime_calendar->_q0;
    _a = lifetime_calendar->_a;
    _b = lifetime_calendar->_b;
    _c = lifetime_calendar->_c;
}
double lifetime_calendar_t::capacity_percent() { return _q; }
double lifetime_calendar_t::runLifetimeCalendarModel(size_t idx, double T, double SOC)
{
    if (_calendar_choice != lifetime_calendar_t::NONE)
    {
        // only run once per iteration (need to make the last iteration)
        if (idx > _last_idx)
        {

            if (idx % util::hours_per_day / _dt_hour == 0)
                _day_age_of_battery++;

            if (_calendar_choice == lifetime_calendar_t::LITHIUM_ION_CALENDAR_MODEL)
                runLithiumIonModel(T, SOC * 0.01);
            else if (_calendar_choice == lifetime_calendar_t::CALENDAR_LOSS_TABLE)
                runTableModel();

            _last_idx = idx;
        }
    }
    return _q;
}
void lifetime_calendar_t::runLithiumIonModel(double T, double SOC)
{
    double k_cal = _a * exp(_b * (1. / T - 1. / 296))*exp(_c*(SOC / T - 1. / 296));
    if (_dq_old == 0)
        _dq_new = k_cal * sqrt(_dt_day);
    else
        _dq_new = (0.5 * pow(k_cal, 2) / _dq_old) * _dt_day + _dq_old;
    _dq_old = _dq_new;
    _q = (_q0 - (_dq_new)) * 100;

}
void lifetime_calendar_t::runTableModel()
{
    size_t n = _calendar_days.size() - 1;
    int day_lo = 0;
    int day_hi = _calendar_days[n];
    double capacity_lo = 100;
    double capacity_hi = 0;

    // interpolation mode
    for (int i = 0; i != (int)_calendar_days.size(); i++)
    {
        int day = _calendar_days[i];
        double capacity = _calendar_capacity[i];
        if (day <= _day_age_of_battery)
        {
            day_lo = day;
            capacity_lo = capacity;
        }
        if (day > _day_age_of_battery)
        {
            day_hi = day;
            capacity_hi = capacity;
            break;
        }
    }
    if (day_lo == day_hi)
    {
        day_lo = _calendar_days[n - 1];
        day_hi = _calendar_days[n];
        capacity_lo = _calendar_capacity[n - 1];
        capacity_hi = _calendar_capacity[n];
    }

    _q = util::interpolate(day_lo, capacity_lo, day_hi, capacity_hi, _day_age_of_battery);
}

void lifetime_calendar_t::replaceBattery(double replacement_percent)
{
    _day_age_of_battery = 0;
    _q += replacement_percent;
    _q = fmin(_q0 * 100, _q);
    _dq_new = 0;
    _dq_old = 0;
}
