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
#ifndef __LIB_BATTERY_DISPATCH_AUTOMATIC_BTM_H__
#define __LIB_BATTERY_DISPATCH_AUTOMATIC_BTM_H__

#include "lib_battery_dispatch.h"
#include "lib_utility_rate.h"

/*! Automated dispatch class for behind-the-meter connections */
class dispatch_automatic_behind_the_meter_t : public dispatch_automatic_t
{
	/**
	Class contains methods and data required to automate dispatch for a behind-the-meter battery targeting peak-shaving applications.
	This includes:
		1. Methods to set or compute the grid power target (desired grid power at every step over the next 24 hours)
		2. Methods to program the dispatch to acheive the target
		3. Method to update the electric load forecast
	*/
public:
	dispatch_automatic_behind_the_meter_t(
		battery_t * Battery,
		double dt,
		double SOC_min,
		double SOC_max,
		int current_choice,
		double Ic_max,
		double Id_max,
		double Pc_max_kwdc,
		double Pd_max_kwdc,
		double Pc_max_kwac,
		double Pd_max_kwac,
		double t_min,
		int dispatch_mode,
		int pv_dispatch,
		size_t nyears,
		size_t look_ahead_hours,
		double dispatch_update_frequency_hours,
		bool can_charge,
		bool can_clipcharge,
		bool can_grid_charge,
		bool can_fuelcell_charge,
        rate_data* util_rate
		);

	~dispatch_automatic_behind_the_meter_t() override
    {
        // TODO: who owns rate?
        delete rate_forecast;
    };

	// deep copy constructor (new memory), from dispatch to this
	dispatch_automatic_behind_the_meter_t(const dispatch_t& dispatch);

	// copy members from dispatch to this
	virtual void copy(const dispatch_t * dispatch);

	/// Public API to run the battery dispatch model for the current timestep, given the system power, and optionally the electric load, amount of system clipping, or specified battery power
	virtual void dispatch(size_t year,
		size_t hour_of_year,
		size_t step);

	/*! Compute the updated power to send to the battery over the next N hours */
	void update_dispatch(size_t hour_of_year, size_t step, size_t idx);

	/*! Pass in the load forecast */
	void update_load_data(std::vector<double> P_load_dc);

	/*! Pass in the grid power target vector */
	void set_target_power(std::vector<double> P_target);

    /* Call after pv, load, and cliploss forecasts have been set*/
    void setup_rate_forecast();

	/*! Grid target power */
	double power_grid_target();

	enum BTM_TARGET_MODES {TARGET_SINGLE_MONTHLY, TARGET_TIME_SERIES};

protected:

	/*! Initialize with a pointer*/
	void init_with_pointer(const dispatch_automatic_behind_the_meter_t * tmp);

	void initialize(size_t hour_of_year);
	void check_debug(FILE *&p, bool & debug, size_t hour_of_year, size_t idx);
	void sort_grid(FILE *p, bool debug, size_t idx);
	void compute_energy(FILE *p, bool debug, double & E_max);
	void target_power(FILE*p, bool debug, double E_max, size_t idx);
	void set_battery_power(FILE *p, bool debug);
	void check_new_month(size_t hour_of_year, size_t step);

	/*! Full time-series of loads [kW] */
	double_vec _P_load_dc;

	/*! Full time-series of target power [kW] */
	double_vec _P_target_input;

	/*! Time series of length (24 hours * steps_per_hour) of target powers [kW] */
	double_vec _P_target_use;

	/*! The target grid power for the month [kW] */
	double _P_target_month;

	/*! The grid power target at the current time [kW] */
	double _P_target_current;

	/* Vector of length (24 hours * steps_per_hour) containing grid calculation [P_grid, hour, step] */
	grid_vec grid;

	/* Vector of length (24 hours * steps_per_hour) containing sorted grid calculation [P_grid, hour, step] */
	grid_vec sorted_grid;

    /* Utility rate data structure for cost aware dispatch algorithms */
    rate_data* rate;

    /* Forecasting class cost aware dispatch algorithms. Dispatch will make many copies of this. */
    UtilityRateForecast* rate_forecast;

};

#endif // __LIB_BATTERY_DISPATCH_AUTOMATIC_BTM_H__
