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

#include "lib_battery_dispatch_automatic_btm.h"
#include "lib_battery_powerflow.h"

dispatch_automatic_behind_the_meter_t::dispatch_automatic_behind_the_meter_t(
	battery_t * Battery,
	double dt_hour,
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
	bool can_clip_charge,
	bool can_grid_charge,
	bool can_fuelcell_charge
	) : dispatch_automatic_t(Battery, dt_hour, SOC_min, SOC_max, current_choice, Ic_max, Id_max, Pc_max_kwdc, Pd_max_kwdc, Pc_max_kwac, Pd_max_kwac,
		t_min, dispatch_mode, pv_dispatch, nyears, look_ahead_hours, dispatch_update_frequency_hours, can_charge, can_clip_charge, can_grid_charge, can_fuelcell_charge)
{
	_P_target_month = -1e16;
	_P_target_current = -1e16;
	_P_target_use.reserve(_num_steps);
	_P_battery_use.reserve(_num_steps);

	grid.reserve(_num_steps);
	sorted_grid.reserve(_num_steps);

	for (size_t ii = 0; ii != _num_steps; ii++)
	{
		grid.push_back(grid_point(0., 0, 0));
		sorted_grid.push_back(grid[ii]);
	}
}

void dispatch_automatic_behind_the_meter_t::init_with_pointer(const dispatch_automatic_behind_the_meter_t* tmp)
{
	_P_target_input = tmp->_P_target_input;
	_P_target_month = tmp->_P_target_month;
	_P_target_current = tmp->_P_target_current;
	grid = tmp->grid;

	// time series data which could be slow to copy. Since this doesn't change, should probably make const and have copy point to common memory
	_P_load_dc = tmp->_P_load_dc;
	_P_target_use = tmp->_P_target_use;
	sorted_grid = tmp->sorted_grid;
}

// deep copy from dispatch to this
dispatch_automatic_behind_the_meter_t::dispatch_automatic_behind_the_meter_t(const dispatch_t & dispatch) :
dispatch_automatic_t(dispatch)
{
	const dispatch_automatic_behind_the_meter_t * tmp = dynamic_cast<const dispatch_automatic_behind_the_meter_t *>(&dispatch);
	init_with_pointer(tmp);
}

// shallow copy from dispatch to this
void dispatch_automatic_behind_the_meter_t::copy(const dispatch_t * dispatch)
{
	dispatch_automatic_t::copy(dispatch);
	const dispatch_automatic_behind_the_meter_t * tmp = dynamic_cast<const dispatch_automatic_behind_the_meter_t *>(dispatch);
	init_with_pointer(tmp);
}

void dispatch_automatic_behind_the_meter_t::dispatch(size_t year,
	size_t hour_of_year,
	size_t step)
{
	size_t step_per_hour = (size_t)(1 / _dt_hour);
	size_t lifetimeIndex = util::lifetimeIndex(year, hour_of_year, step, step_per_hour);

	update_dispatch(hour_of_year, step, lifetimeIndex);
	dispatch_automatic_t::dispatch(year, hour_of_year, step);
}

void dispatch_automatic_behind_the_meter_t::update_load_data(std::vector<double> P_load_dc){ _P_load_dc = P_load_dc; }
void dispatch_automatic_behind_the_meter_t::set_target_power(std::vector<double> P_target){ _P_target_input = P_target; }
double dispatch_automatic_behind_the_meter_t::power_grid_target() { return _P_target_current; };
void dispatch_automatic_behind_the_meter_t::update_dispatch(size_t hour_of_year, size_t step, size_t idx)
{
	bool debug = false;
	FILE *p;
	check_debug(p, debug, hour_of_year, idx);
	size_t hour_of_day = util::hour_of_day(hour_of_year);
	_day_index = (hour_of_day * _steps_per_hour + step);

	if (_mode != dispatch_t::CUSTOM_DISPATCH)
	{
		// Currently hardcoded to have 24 hour look ahead and 24 dispatch_update
		if (hour_of_day == 0 && hour_of_year != _hour_last_updated)
		{

			// [kWh] - the maximum energy that can be cycled
			double E_max = 0;

			check_new_month(hour_of_year, step);

			// setup vectors
			initialize(hour_of_year);

			// compute grid power, sort highest to lowest
			sort_grid(p, debug, idx);

			// Peak shaving scheme
			compute_energy(p, debug, E_max);
			target_power(p, debug, E_max, idx);

			// Set battery power profile
			set_battery_power(p, debug);
		}
		// save for extraction
		_P_target_current = _P_target_use[_day_index];
		m_batteryPower->powerBatteryTarget = _P_battery_use[_day_index];
	}
	else
	{
		m_batteryPower->powerBatteryTarget = _P_battery_use[idx % (8760 *_steps_per_hour)];
        if (m_batteryPower->connectionMode == AC_CONNECTED){
            if (m_batteryPower->powerBatteryTarget < 0)
                m_batteryPower->powerBatteryTarget *= m_batteryPower->singlePointEfficiencyDCToAC;
            else
                m_batteryPower->powerBatteryTarget /= m_batteryPower->singlePointEfficiencyDCToAC;
        }
	}

	m_batteryPower->powerBatteryDC = m_batteryPower->powerBatteryTarget;

	if (debug)
		fclose(p);
}
void dispatch_automatic_behind_the_meter_t::initialize(size_t hour_of_year)
{
	_hour_last_updated = hour_of_year;
	_P_target_use.clear();
	_P_battery_use.clear();
	m_batteryPower->powerBatteryDC = 0;
	m_batteryPower->powerBatteryAC = 0;
	m_batteryPower->powerBatteryTarget = 0;

	// clean up vectors
	for (size_t ii = 0; ii != _num_steps; ii++)
	{
		grid[ii] = grid_point(0., 0, 0);
		sorted_grid[ii] = grid[ii];
		_P_target_use.push_back(0.);
		_P_battery_use.push_back(0.);
	}
}
void dispatch_automatic_behind_the_meter_t::check_new_month(size_t hour_of_year, size_t step)
{
	size_t hours = 0;
	for (size_t month = 1; month <= _month; month++)
		hours += util::hours_in_month(month);

	if (hours == 8760)
		hours = 0;

	if ((hour_of_year == hours) && step == 0)
	{
		_P_target_month = -1e16;
		_month < 12 ? _month++ : _month = 1;
	}
}
void dispatch_automatic_behind_the_meter_t::check_debug(FILE *&p, bool & debug, size_t hour_of_year, size_t)
{
	// for now, don't enable
	// debug = true;

	if (hour_of_year == 0 && hour_of_year != _hour_last_updated)
	{
		// debug = true;
		if (debug)
		{
			p = fopen("dispatch.txt", "w");
			fprintf(p, "Hour of Year: %zu\t Hour Last Updated: %zu \t Steps per Hour: %zu\n", hour_of_year, _hour_last_updated, _steps_per_hour);
		}
		// failed for some reason
		if (p == NULL)
			debug = false;
	}
}

void dispatch_automatic_behind_the_meter_t::sort_grid(FILE *p, bool debug, size_t idx)
{

	if (debug)
		fprintf(p, "Index\t P_load (kW)\t P_pv (kW)\t P_grid (kW)\n");

	// compute grid net from pv and load (no battery)
	size_t count = 0;
	for (size_t hour = 0; hour != 24; hour++)
	{
		for (size_t step = 0; step != _steps_per_hour; step++)
		{
			grid[count] = grid_point(_P_load_dc[idx] - _P_pv_dc[idx], hour, step);
			sorted_grid[count] = grid[count];

			if (debug)
				fprintf(p, "%zu\t %.1f\t %.1f\t %.1f\n", count, _P_load_dc[idx], _P_pv_dc[idx], _P_load_dc[idx] - _P_pv_dc[idx]);

			idx++;
			count++;
		}
	}
	std::sort(sorted_grid.begin(), sorted_grid.end(), byGrid());
}

void dispatch_automatic_behind_the_meter_t::compute_energy(FILE *p, bool debug, double & E_max)
{

	E_max = _Battery->V() * _Battery->charge_maximum_lifetime() * (m_batteryPower->stateOfChargeMax - m_batteryPower->stateOfChargeMin) * 0.01 * util::watt_to_kilowatt;

	if (debug)
	{
		fprintf(p, "Energy Max: %.3f\t", E_max);
		fprintf(p, "Battery Voltage: %.3f\n", _Battery->V());
	}
}

void dispatch_automatic_behind_the_meter_t::target_power(FILE*p, bool debug, double E_useful, size_t idx)
{
	// if target power set, use that
	if (_P_target_input.size() > idx && _P_target_input[idx] >= 0)
	{
		double_vec::const_iterator first = _P_target_input.begin() + idx;
		double_vec::const_iterator last = _P_target_input.begin() + idx + _num_steps;
		double_vec tmp(first, last);
		_P_target_use = tmp;
		return;
	}
	// don't calculate if peak grid demand is less than a previous target in the month
	else if (sorted_grid[0].Grid() < _P_target_month)
	{
		for (size_t i = 0; i != _num_steps; i++)
			_P_target_use[i] = _P_target_month;
		return;
	}
	// otherwise, compute one target for the next 24 hours.
	else
	{
		// First compute target power which will allow battery to charge up to E_useful over 24 hour period
		if (debug)
			fprintf(p, "Index\tRecharge_target\t charge_energy\n");

		double P_target = sorted_grid[0].Grid();
		double P_target_min = 1e16;
		double E_charge = 0.;
		int index = (int)_num_steps - 1;
		std::vector<double> E_charge_vec;
		for (int jj = (int)_num_steps - 1; jj >= 0; jj--)
		{
			E_charge = 0.;
			P_target_min = sorted_grid[index].Grid();

			for (int ii = (int)_num_steps - 1; ii >= 0; ii--)
			{
				if (sorted_grid[ii].Grid() > P_target_min)
					break;

				E_charge += (P_target_min - sorted_grid[ii].Grid())*_dt_hour;
			}
			E_charge_vec.push_back(E_charge);
			if (debug)
				fprintf(p, "%u: index\t%.3f\t %.3f\n", index, P_target_min, E_charge);
			index--;

			if (index < 0)
				break;
		}
		std::reverse(E_charge_vec.begin(), E_charge_vec.end());

		// Calculate target power
		std::vector<double> sorted_grid_diff;
		sorted_grid_diff.reserve(_num_steps - 1);

		for (size_t ii = 0; ii != _num_steps - 1; ii++)
			sorted_grid_diff.push_back(sorted_grid[ii].Grid() - sorted_grid[ii + 1].Grid());

		P_target = sorted_grid[0].Grid(); // target power to shave to [kW]
		double sum = 0;			   // energy [kWh];
		if (debug)
			fprintf(p, "Step\tTarget_Power\tEnergy_Sum\tEnergy_charged\n");

		// Iterate over sorted load to determine target power
		for (size_t ii = 0; ii != _num_steps - 1; ii++)
		{
			// don't look at negative grid power
			if (sorted_grid[ii + 1].Grid() < 0)
				break;
			// Update power target
			else
				P_target = sorted_grid[ii + 1].Grid();

			if (debug)
				fprintf(p, "%zu\t %.3f\t", ii, P_target);

			// implies a repeated power
			if (sorted_grid_diff[ii] == 0)
			{
				if (debug)
					fprintf(p, "\n");
				continue;
			}
			// add to energy we are trimming
			else
				sum += sorted_grid_diff[ii] * (ii + 1)*_dt_hour;

			if (debug)
				fprintf(p, "%.3f\t%.3f\n", sum, E_charge_vec[ii + 1]);

			if (sum < E_charge_vec[ii + 1] && sum < E_useful)
				continue;
			// we have limited power, we'll shave what more we can
			else if (sum > E_charge_vec[ii + 1])
			{
				P_target += (sum - E_charge_vec[ii]) / ((ii + 1)*_dt_hour);
				sum = E_charge_vec[ii];
				if (debug)
					fprintf(p, "%zu\t %.3f\t%.3f\t%.3f\n", ii, P_target, sum, E_charge_vec[ii]);
				break;
			}
			// only allow one cycle per day
			else if (sum > E_useful)
			{
				P_target += (sum - E_useful) / ((ii + 1)*_dt_hour);
				sum = E_useful;
				if (debug)
					fprintf(p, "%zu\t %.3f\t%.3f\t%.3f\n", ii, P_target, sum, E_charge_vec[ii]);
				break;
			}
		}
		// set safety factor in case voltage differences make it impossible to achieve target without violated minimum SOC
		P_target *= (1 + _safety_factor);

		// don't set target lower than previous high in month
		if (P_target < _P_target_month)
		{
			P_target = _P_target_month;
			if (debug)
				fprintf(p, "P_target exceeds monthly target, move to  %.3f\n", P_target);
		}
		else
			_P_target_month = P_target;

		// write vector of targets
		for (size_t i = 0; i != _num_steps; i++)
			_P_target_use[i] = P_target;
	}
}

void dispatch_automatic_behind_the_meter_t::set_battery_power(FILE *p, bool debug)
{
	for (size_t i = 0; i != _P_target_use.size(); i++) {
		_P_battery_use[i] = grid[i].Grid() - _P_target_use[i];

		// At this point the target power is expressed in AC, must convert to DC for battery
		if (m_batteryPower->connectionMode == m_batteryPower->AC_CONNECTED) {
			if (_P_battery_use[i] > 0) {
				_P_battery_use[i] /= m_batteryPower->singlePointEfficiencyDCToAC;
			}
			else {
				_P_battery_use[i] *= m_batteryPower->singlePointEfficiencyACToDC;
			}
		}
		// DC-connected is harder to convert to AC, must make assumptions about inverter efficiency and charge shource
		else {
			if (_P_battery_use[i] > 0) {
				_P_battery_use[i] /= (m_batteryPower->singlePointEfficiencyDCToDC * m_batteryPower->singlePointEfficiencyACToDC);
			}
			// Assuming just charging from PV not grid
			else {
				_P_battery_use[i] *= m_batteryPower->singlePointEfficiencyDCToDC;
			}
		}
	}

	if (debug)
	{
		for (size_t i = 0; i != _P_target_use.size(); i++)
			fprintf(p, "i=%zu  P_battery: %.2f\n", i, _P_battery_use[i]);
	}
}