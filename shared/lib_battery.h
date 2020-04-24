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

#ifndef battery_h
#define battery_h

#include <vector>
#include <map>
#include <string>
#include <cstdio>
#include <algorithm>

#include "lib_util.h"
#include "lib_battery_capacity.h"
#include "lib_battery_voltage.h"
#include "lib_battery_lifetime.h"

struct thermal_state {
    double q_relative_thermal;      //[%]
    double T_batt_avg;              // C
    double T_room;
    double T_batt_prev;

    friend std::ostream &operator<<(std::ostream &os, const thermal_state &p);
};

struct thermal_params {
    double dt_hour;
    double mass;		        // [kg]
    double surface_area;		// [m2] - exposed surface area
    double Cp;			        // [J/KgK] - battery specific heat capacity
    double h;			        // [Wm2K] - general heat transfer coefficient
    double R;			        // [Ohm] - internal resistance
    util::matrix_t<double> cap_vs_temp;

    enum OPTIONS {
        VALUE, SCHEDULE
    };
    int option;
    std::vector<double> T_room_schedule;   // can be year one hourly data or a single value constant throughout year

    friend std::ostream &operator<<(std::ostream &os, const thermal_params &p);
};

/*
Thermal classes
*/
class thermal_t
{
public:
    thermal_t(double dt_hour, double mass, double surface_area, double R, double Cp, double h,
              const util::matrix_t<double> &c_vs_t, std::vector<double> T_room_C);

    thermal_t(double dt_hour, double mass, double surface_area, double R, double Cp, double h,
              const util::matrix_t<double> &c_vs_t, double T_room_C);

    thermal_t(const thermal_t& rhs);

    thermal_t &operator=(const thermal_t &rhs);

    thermal_t * clone();

    void updateTemperature(double I, size_t lifetimeIndex);
    void replace_battery(size_t lifetimeIndex);

    // outputs
    double T_battery();
    double capacity_percent();

    thermal_state get_state();
    thermal_params get_params();

protected:
    double dt_sec;              // [sec] - timestep
    void calc_capacity();

    std::shared_ptr<thermal_params> params;
    thermal_state state;

private:
    void initialize();
};
/**
* \class losses_t
*
* \brief
*
*  The Battery losses class takes generic losses which occur during charging, discharge, or idle operation modes:
*  The model also accepts a time-series vector of losses defined for every time step of the first year of simulation
*  which may be used in lieu of the losses for operational mode.
*/
class losses_t
{
public:

	/**
	* \function losses_t
	*
	* Construct the losses object
	*
	* \param[in] lifetime_t * pointer to lifetime class
	* \param[in] thermal_t * pointer to thermal class (currently unused)
	* \param[in] capacity_t * pointer to capacity class
	* \param[in] loss_mode 0 for monthy input, 1 for input time series
	* \param[in] batt_loss_charge_kw vector (size 1 for annual or 12 for monthly) containing battery system losses when charging (kW)
	* \param[in] batt_loss_discharge_kw vector (size 1 for annual or 12 for monthly) containing battery system losses when discharge (kW)
	* \param[in] batt_loss_idle_kw vector (size 1 for annual or 12 for monthly) containing battery system losses when idle (kW)
	* \param[in] batt_loss_kw vector (size 1 for annual or 12 for monthly) containing battery system losses when idle (kW)
	*/
	losses_t(double dtHour,
			lifetime_t *,
			thermal_t *,
			capacity_t*,
			const int loss_mode,
			const double_vec batt_loss_charge_kw = std::vector<double>(0),
			const double_vec batt_loss_discharge_kw = std::vector<double>(0),
			const double_vec batt_loss_idle_kw = std::vector<double>(0),
			const double_vec batt_loss_kw=std::vector<double>(0));

	void set_models(lifetime_t *, thermal_t *, capacity_t*);

	/// Copy input losses to this object
	void copy(losses_t *);

	/// Run the losses model at the present simulation index (for year 1 only)
	void run_losses(size_t lifetimeIndex);

	/// Get the loss at the specified simulation index (year 1)
	double getLoss(size_t indexFirstYear);

	/// Options for the loss inputs to use
	enum { MONTHLY, TIMESERIES};

protected:

    int _loss_mode;
	double _dtHour;

	lifetime_t * _lifetime;
	thermal_t * _thermal;
	capacity_t * _capacity;
	double_vec  _charge_loss;
	double_vec  _discharge_loss;
	double_vec  _idle_loss;
	double_vec  _full_loss;
};

/*
Class which encapsulates a battery and all its models
*/

struct replacement_state {
    int n_replacements;                                 // number of replacements this year
    std::vector<size_t> indices_replaced;               // lifetime indices at which replacements occurred

    friend std::ostream &operator<<(std::ostream &os, const replacement_state &p);
};

struct replacement_params {
    enum OPTIONS {
        NONE, CAPACITY_PERCENT, SCHEDULE
    };
    int option;

    /// Maximum capacity relative to nameplate at which to replace battery back to 100%
    double capacity_percent;

    std::vector<int> schedule;
    std::vector<double> schedule_percent_to_replace;    // (0 - 100%)

    friend std::ostream &operator<<(std::ostream &os, const replacement_params &p);
};


class battery_t
{
public:
	battery_t();
	battery_t(double dt, int battery_chemistry);

	// deep copy constructor (new memory), from battery to this
	battery_t(const battery_t& battery);

	// copy members from battery to this
	void copy(const battery_t * battery);

	// virtual destructor, does nothing as no memory allocated in constructor
	virtual ~battery_t();

	// delete the new submodels that have been allocated
	void delete_clone();

	void initialize(capacity_t *, voltage_t *, lifetime_t *, thermal_t *, losses_t *);

	// replace by capacity
	void setupReplacements(double capacity);

	// replace by schedule
    void setupReplacements(std::vector<int> schedule, std::vector<double> replacement_percents);

	void runReplacement(size_t year, size_t hour, size_t step);
	void resetReplacement();
	double getNumReplacementYear();

    // Run all for single time step, updating all component model states and return the dispatched power [kW]
	double run(size_t lifetimeIndex, double &I);

	double calculate_voltage_for_current(double I);

	// Return the max charge or discharge power achievable in the next time step, and the required current [A]
    double calculate_max_charge_kw(double *max_current_A = nullptr);
    double calculate_max_discharge_kw(double *max_current_A = nullptr);

	// Returns current [A] required to dispatch input power [kW], or the max power (to which P_kw is set)
	double calculate_current_for_power_kw(double &P_kw);

    // Run a component level model
	void runCapacityModel(double &I);
	void runVoltageModel();
	void runThermalModel(double I, size_t lifetimeIndex);
	void runLifetimeModel(size_t lifetimeIndex);
	void runLossesModel(size_t lifetimeIndex);


	capacity_t * capacity_model() const;
	capacity_t * capacity_initial_model() const;
	voltage_t * voltage_model() const;
	lifetime_t * lifetime_model() const;
	thermal_t * thermal_model() const;
	thermal_t * thermal_initial_model() const;
	losses_t * losses_model() const;

	// Get capacity quantities
	double battery_charge_needed(double SOC_max);
	double battery_charge_total();
	double battery_charge_maximum();
	double battery_charge_maximum_lifetime();
	double battery_charge_maximum_thermal();
	double battery_energy_nominal();
	double battery_energy_to_fill(double SOC_max);
	double battery_power_to_fill(double SOC_max);
	double battery_soc();

	// Get Voltage
	double cell_voltage();
	double battery_voltage(); // the actual battery voltage
	double battery_voltage_nominal(); // the nominal battery voltage

	enum CHEMS{ LEAD_ACID, LITHIUM_ION, VANADIUM_REDOX, IRON_FLOW};
	enum REPLACE{ NO_REPLACEMENTS, REPLACE_BY_CAPACITY, REPLACE_BY_SCHEDULE};


private:
	capacity_t * _capacity;
	capacity_t * _capacity_initial;
	thermal_t * _thermal;
	thermal_t * _thermal_initial;
	lifetime_t * _lifetime;
	voltage_t * _voltage;
	losses_t * _losses;
	int _battery_chemistry;
	double _dt_hour;			// [hr] - timestep
	double _dt_min;				// [min] - timestep
	size_t _last_idx;

    replacement_state replacement_state;
    std::shared_ptr<replacement_params> params;

};

#endif
