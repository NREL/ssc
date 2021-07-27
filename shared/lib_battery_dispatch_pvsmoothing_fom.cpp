#include "lib_battery_dispatch_pvsmoothing_fom.h"
#include "lib_battery_powerflow.h"
#include "lib_utility_rate.h"

#include <numeric>

dispatch_pvsmoothing_front_of_meter_t::dispatch_pvsmoothing_front_of_meter_t(
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
	bool can_fuelcell_charge,
	double inverter_paco,
    std::vector<double> battReplacementCostPerkWh,
	int battCycleCostChoice,
    std::vector<double> battCycleCost,
	double etaPVCharge,
	double etaGridCharge,
	double etaDischarge,
    double batt_dispatch_pvs_nameplate_ac,
    double batt_dispatch_pvs_ac_lb,
    bool batt_dispatch_pvs_ac_lb_enable,
    double batt_dispatch_pvs_ac_ub,
    bool batt_dispatch_pvs_ac_ub_enable,
    bool batt_dispatch_pvs_curtail_as_control,
    bool batt_dispatch_pvs_curtail_if_violation,
    size_t batt_dispatch_pvs_forecast_shift_periods, 
    double batt_dispatch_pvs_kf,
    double batt_dispatch_pvs_ki,
    double batt_dispatch_pvs_kp,
    double batt_dispatch_pvs_max_ramp,
    bool batt_dispatch_pvs_short_forecast_enable,
    double batt_dispatch_pvs_soc_rest,
    size_t batt_dispatch_pvs_timestep_multiplier,
    double batt_dispatch_pvs_initial_SOC

) : dispatch_automatic_t(Battery, dt_hour, SOC_min, SOC_max, current_choice, Ic_max, Id_max, Pc_max_kwdc, Pd_max_kwdc, Pc_max_kwac, Pd_max_kwac,
		t_min, dispatch_mode, pv_dispatch, nyears, look_ahead_hours, dispatch_update_frequency_hours, can_charge, can_clip_charge, can_grid_charge, can_fuelcell_charge,
        battReplacementCostPerkWh, battCycleCostChoice, battCycleCost),
    m_batt_dispatch_pvs_nameplate_ac(batt_dispatch_pvs_nameplate_ac),
    m_batt_dispatch_pvs_ac_lb(batt_dispatch_pvs_ac_lb),
    m_batt_dispatch_pvs_ac_lb_enable(batt_dispatch_pvs_ac_lb_enable),
    m_batt_dispatch_pvs_ac_ub(batt_dispatch_pvs_ac_ub),
    m_batt_dispatch_pvs_ac_ub_enable(batt_dispatch_pvs_ac_ub_enable),
    m_batt_dispatch_pvs_curtail_as_control(batt_dispatch_pvs_curtail_as_control),
    m_batt_dispatch_pvs_curtail_if_violation(batt_dispatch_pvs_curtail_if_violation),
    m_batt_dispatch_pvs_forecast_shift_periods(batt_dispatch_pvs_forecast_shift_periods),
    m_batt_dispatch_pvs_kf(batt_dispatch_pvs_kf),
    m_batt_dispatch_pvs_ki(batt_dispatch_pvs_ki),
    m_batt_dispatch_pvs_kp(batt_dispatch_pvs_kp),
    m_batt_dispatch_pvs_max_ramp(batt_dispatch_pvs_max_ramp),
    m_batt_dispatch_pvs_short_forecast_enable(batt_dispatch_pvs_short_forecast_enable),
    m_batt_dispatch_pvs_soc_rest(batt_dispatch_pvs_soc_rest),
    m_batt_dispatch_pvs_timestep_multiplier(batt_dispatch_pvs_timestep_multiplier),
    m_batt_dispatch_pvs_initial_SOC(batt_dispatch_pvs_initial_SOC)
{

	_inverter_paco = inverter_paco;

	m_etaPVCharge = etaPVCharge * 0.01;
	m_etaGridCharge = etaGridCharge * 0.01;
	m_etaDischarge = etaDischarge * 0.01;

    m_batt_dispatch_pvs_outpower = m_batt_dispatch_pvs_battpower = m_batt_dispatch_pvs_curtail = m_batt_dispatch_pvs_violation_list = 0.0;

    costToCycle();
    setup_pvsmoothing_ramp_interval_vectors();
}
dispatch_pvsmoothing_front_of_meter_t::~dispatch_pvsmoothing_front_of_meter_t(){ /* NOTHING TO DO */}

void dispatch_pvsmoothing_front_of_meter_t::init_with_pointer(const dispatch_pvsmoothing_front_of_meter_t* tmp)
{
	_forecast_hours = tmp->_forecast_hours;
	_inverter_paco = tmp->_inverter_paco;

	m_etaPVCharge = tmp->m_etaPVCharge;
	m_etaGridCharge = tmp->m_etaGridCharge;
	m_etaDischarge = tmp->m_etaDischarge;

    m_batt_dispatch_pvs_nameplate_ac = tmp->m_batt_dispatch_pvs_nameplate_ac;
    m_batt_dispatch_pvs_ac_lb = tmp->m_batt_dispatch_pvs_ac_lb;
    m_batt_dispatch_pvs_ac_lb_enable = tmp->m_batt_dispatch_pvs_ac_lb_enable;
    m_batt_dispatch_pvs_ac_ub = tmp->m_batt_dispatch_pvs_ac_ub;
    m_batt_dispatch_pvs_ac_ub_enable = tmp->m_batt_dispatch_pvs_ac_ub_enable;
    m_batt_dispatch_pvs_curtail_as_control = tmp->m_batt_dispatch_pvs_curtail_as_control;
    m_batt_dispatch_pvs_curtail_if_violation = tmp->m_batt_dispatch_pvs_curtail_if_violation;
    m_batt_dispatch_pvs_forecast_shift_periods = tmp->m_batt_dispatch_pvs_forecast_shift_periods;
    m_batt_dispatch_pvs_kf = tmp->m_batt_dispatch_pvs_kf;
    m_batt_dispatch_pvs_ki = tmp->m_batt_dispatch_pvs_ki;
    m_batt_dispatch_pvs_kp = tmp->m_batt_dispatch_pvs_kp;
    m_batt_dispatch_pvs_max_ramp = tmp->m_batt_dispatch_pvs_max_ramp;
    m_batt_dispatch_pvs_short_forecast_enable = tmp->m_batt_dispatch_pvs_short_forecast_enable;
    m_batt_dispatch_pvs_soc_rest = tmp->m_batt_dispatch_pvs_soc_rest;
    m_batt_dispatch_pvs_timestep_multiplier = tmp->m_batt_dispatch_pvs_timestep_multiplier;

}


// deep copy from dispatch to this
dispatch_pvsmoothing_front_of_meter_t::dispatch_pvsmoothing_front_of_meter_t(const dispatch_t & dispatch) :
dispatch_automatic_t(dispatch)
{
	const dispatch_pvsmoothing_front_of_meter_t * tmp = dynamic_cast<const dispatch_pvsmoothing_front_of_meter_t *>(&dispatch);
	init_with_pointer(tmp);
}

// shallow copy from dispatch to this
void dispatch_pvsmoothing_front_of_meter_t::copy(const dispatch_t * dispatch)
{
	dispatch_automatic_t::copy(dispatch);
	const dispatch_pvsmoothing_front_of_meter_t * tmp = dynamic_cast<const dispatch_pvsmoothing_front_of_meter_t *>(dispatch);
	init_with_pointer(tmp);
}

void dispatch_pvsmoothing_front_of_meter_t::dispatch(size_t year,
	size_t hour_of_year,
	size_t step)
{
    curr_year = year;
	size_t step_per_hour = (size_t)(1 / _dt_hour);
	size_t lifetimeIndex = util::lifetimeIndex(year, hour_of_year, step, step_per_hour);

	update_dispatch(year, hour_of_year, step, lifetimeIndex);
	dispatch_automatic_t::dispatch(year, hour_of_year, step);
}

void dispatch_pvsmoothing_front_of_meter_t::update_dispatch(size_t year, size_t hour_of_year, size_t , size_t lifetimeIndex)
{
	// Initialize
	m_batteryPower->powerBatteryDC = 0;
	m_batteryPower->powerBatteryAC = 0;
	m_batteryPower->powerBatteryTarget = 0;

    size_t ndx_sampled = lifetimeIndex / m_batt_dispatch_pvs_timestep_multiplier;
      
    size_t xval = (lifetimeIndex % m_batt_dispatch_pvs_timestep_multiplier);

    // based on EPRI email that value is constant over smoothing interval
    m_batt_dispatch_pvs_battpower = m_batt_dispatch_pvs_battpower_vec[ndx_sampled];
    m_batt_dispatch_pvs_outpower = m_batt_dispatch_pvs_outpower_vec[ndx_sampled];
    m_batt_dispatch_pvs_battsoc = m_batt_dispatch_pvs_battsoc_vec[ndx_sampled];
    m_batt_dispatch_pvs_curtail = m_batt_dispatch_pvs_curtail_vec[ndx_sampled];
    m_batt_dispatch_pvs_PV_ramp_interval = m_pv_power_input_sampled_vec[ndx_sampled];
    m_batt_dispatch_pvs_forecast_pv_energy = m_forecast_pv_energy_vec[ndx_sampled];

    // record violations at end of ramp interval timestep, e.g. 10 minute = 0 to 9 time steps with violation at index 9
    if (xval == m_batt_dispatch_pvs_timestep_multiplier - 1)
        m_batt_dispatch_pvs_violation_list = m_batt_dispatch_pvs_violation_list_vec[ndx_sampled];
    else
        m_batt_dispatch_pvs_violation_list = 0;

    m_batt_dispatch_pvs_P_pv_ac = _P_pv_ac[lifetimeIndex]; // unsmoothed pv system output.

    // adjust for loss and efficiencies
    m_batteryPower->powerBatteryTarget = m_batt_dispatch_pvs_nameplate_ac > 0 ? m_batt_dispatch_pvs_nameplate_ac * m_batt_dispatch_pvs_battpower : m_batt_dispatch_pvs_battpower;
    double loss_kw = _Battery->calculate_loss(m_batteryPower->powerBatteryTarget, lifetimeIndex); // Battery is responsible for covering discharge losses
    if (m_batteryPower->connectionMode == AC_CONNECTED) {
        m_batteryPower->powerBatteryTarget = m_batteryPower->adjustForACEfficiencies(m_batteryPower->powerBatteryTarget, loss_kw);
    }
    else if (m_batteryPower->powerBatteryTarget > 0) {
        // Adjust for DC discharge losses
        m_batteryPower->powerBatteryTarget += loss_kw;
    }

    m_batteryPower->powerBatteryDC = m_batteryPower->powerBatteryTarget;
}

void dispatch_pvsmoothing_front_of_meter_t::update_pv_data(double_vec P_pv_ac)
{
	_P_pv_ac = P_pv_ac;

	// append to end to allow for look-ahead
	for (size_t i = 0; i != _forecast_hours * _steps_per_hour; i++)
		_P_pv_ac.push_back(P_pv_ac[i]);

    setup_pvsmoothing_ramp_interval_vectors();
}

void dispatch_pvsmoothing_front_of_meter_t::costToCycle()
{
    // Calculate assuming maximum depth of discharge (most conservative assumption)
    if (m_battCycleCostChoice == dispatch_t::MODEL_CYCLE_COST)
    {
        double capacityPercentDamagePerCycle = _Battery->estimateCycleDamage();
        m_cycleCost = 0.01 * capacityPercentDamagePerCycle * m_battReplacementCostPerKWH[curr_year];
    }
    else if(m_battCycleCostChoice == dispatch_t::INPUT_CYCLE_COST)
    {
        m_cycleCost = cycle_costs_by_year[curr_year];
    }
}

void dispatch_pvsmoothing_front_of_meter_t::setup_pvsmoothing_ramp_interval_vectors()
{
    size_t timestep_multiplier = m_batt_dispatch_pvs_timestep_multiplier;
    size_t nRecords = _P_pv_ac.size();
    size_t nRecordsSampled = nRecords / timestep_multiplier;
    m_pv_power_input_sampled_vec.clear();
    m_pv_power_input_sampled_vec.reserve(nRecordsSampled);

    size_t ndx = 0;
    for (size_t ndx_sampled = 0; ndx_sampled < nRecordsSampled; ndx_sampled++) {
        ssc_number_t sum = 0;
        for (size_t i_sampled = 0; i_sampled < timestep_multiplier && ndx < nRecords; i_sampled++) {
            sum += _P_pv_ac[ndx] / m_batt_dispatch_pvs_nameplate_ac;  // scaled values
            ndx++;
        }
        m_pv_power_input_sampled_vec.push_back(sum / timestep_multiplier); 
    }

    bool en_forecast = m_batt_dispatch_pvs_short_forecast_enable;
    m_forecast_pv_energy_vec.clear();
    m_forecast_pv_energy_vec.reserve(nRecordsSampled);
    ssc_number_t forecast_shift_periods = m_batt_dispatch_pvs_forecast_shift_periods;
    for (size_t ndx_sampled = 0; ndx_sampled < nRecordsSampled; ndx_sampled++) {
        ndx = 0;
        ssc_number_t sum = 0;
        while ((ndx < forecast_shift_periods) && (ndx_sampled + ndx < nRecordsSampled)) {
            sum += m_pv_power_input_sampled_vec[ndx_sampled + ndx];
            ndx++;
        }
        m_forecast_pv_energy_vec.push_back(sum * m_batt_dispatch_pvs_timestep_multiplier * _dt_hour);
    }
 
    // main loop from ramp_rate_control.py
    // conversion factors
    ssc_number_t  power_to_energy_conversion_factor = m_batt_dispatch_pvs_timestep_multiplier * _dt_hour;
    ssc_number_t  batt_half_round_trip_eff = sqrt(m_etaDischarge * m_etaPVCharge);


    ssc_number_t kp = m_batt_dispatch_pvs_kp;
    ssc_number_t ki = m_batt_dispatch_pvs_ki;
    ssc_number_t kf = m_batt_dispatch_pvs_kf;
    ssc_number_t soc_rest = m_batt_dispatch_pvs_soc_rest;
    ssc_number_t max_ramp = m_batt_dispatch_pvs_max_ramp;

    bool AC_upper_bound_on = m_batt_dispatch_pvs_ac_ub_enable;
    ssc_number_t AC_upper_bound = m_batt_dispatch_pvs_ac_ub;
    bool AC_lower_bound_on = m_batt_dispatch_pvs_ac_lb_enable;
    ssc_number_t AC_lower_bound = m_batt_dispatch_pvs_ac_lb;
    bool curtail_as_control = m_batt_dispatch_pvs_curtail_as_control;
    bool curtail_if_violation = m_batt_dispatch_pvs_curtail_if_violation;

    ssc_number_t battery_energy = _Battery->energy_nominal(); // check units in equations below 
    ssc_number_t battery_power = m_batteryPower->powerBatteryChargeMaxAC;
    // scale by nameplate per ERPI code
    battery_energy = m_batt_dispatch_pvs_nameplate_ac > 0 ? battery_energy / m_batt_dispatch_pvs_nameplate_ac : battery_energy;
    battery_power = m_batt_dispatch_pvs_nameplate_ac > 0 ? battery_power / m_batt_dispatch_pvs_nameplate_ac : battery_power;


    //#local variables
    ssc_number_t previous_power = 0;
    ssc_number_t battery_soc = m_batt_dispatch_pvs_initial_SOC/ 100.0; 
    m_batt_dispatch_pvs_outpower_vec.clear();
    m_batt_dispatch_pvs_battpower_vec.clear();
    m_batt_dispatch_pvs_battsoc_vec.clear();
    m_batt_dispatch_pvs_curtail_vec.clear();
    m_batt_dispatch_pvs_violation_list_vec.clear();

    m_batt_dispatch_pvs_outpower_vec.reserve(nRecordsSampled);
    m_batt_dispatch_pvs_battpower_vec.reserve(nRecordsSampled);
    m_batt_dispatch_pvs_battsoc_vec.reserve(nRecordsSampled);
    m_batt_dispatch_pvs_curtail_vec.reserve(nRecordsSampled);
    m_batt_dispatch_pvs_violation_list_vec.reserve(nRecordsSampled);


    // output accumulators
    size_t violation_count = 0;
    ssc_number_t total_energy = 0;

    if (!en_forecast)
        kf = 0;

    // iterate through time - series
    for (size_t ndx_sampled = 0; ndx_sampled < nRecordsSampled; ndx_sampled++) {
        ssc_number_t pv_power = m_pv_power_input_sampled_vec[ndx_sampled];
        ssc_number_t out_power = 0;
        ssc_number_t battery_power_terminal = 0;
        ssc_number_t forecast_power = 0;
        forecast_power = m_forecast_pv_energy_vec[ndx_sampled];
        // calculate controller error
        ssc_number_t delta_power = pv_power - previous_power; //#proportional error
        ssc_number_t soc_increment = battery_soc + (pv_power - previous_power) * power_to_energy_conversion_factor;// #integral error
        ssc_number_t future_error = previous_power * forecast_shift_periods * power_to_energy_conversion_factor - forecast_power; //#derivitive error
        ssc_number_t error = kp * delta_power + ki * (soc_increment - soc_rest * battery_energy) - kf * future_error;

        // calculate the desired output power, enforce ramp rate limit
        if (error > 0)
            out_power = previous_power + std::min(max_ramp, std::abs(error));
        else
            out_power = previous_power - std::min(max_ramp, std::abs(error));

        // enforce grid power limits
        if (AC_upper_bound_on) {
            if (out_power > AC_upper_bound)
                out_power = AC_upper_bound;
        }
        if (AC_lower_bound_on) {
            if (out_power < AC_lower_bound)
                out_power = AC_lower_bound;
        }

        // calculate desired(unconstrained) battery power
        battery_power_terminal = out_power - pv_power; //# positive is power leaving battery(discharging)

        // adjust battery power to factor in battery constraints
        // check SOC limit - reduce battery power if either soc exceeds either 0 or 100 %
        // check full
        if ((battery_soc - battery_power_terminal * batt_half_round_trip_eff * power_to_energy_conversion_factor) > battery_energy)
            battery_power_terminal = -1.0 * (battery_energy - battery_soc) / power_to_energy_conversion_factor / batt_half_round_trip_eff; 
        //            #check empty
        else if ((battery_soc - battery_power_terminal * power_to_energy_conversion_factor) < 0)
            battery_power_terminal = battery_soc / power_to_energy_conversion_factor / batt_half_round_trip_eff;

        // enforce battery power limits
        // discharging too fast
        if (battery_power_terminal > battery_power)
            battery_power_terminal = battery_power;
        // charging too fast
        else if (battery_power_terminal < -1.0 * battery_power)
            battery_power_terminal = -1.0 * battery_power;

        // update output power after battery constraints are applied
        out_power = pv_power + battery_power_terminal;

        // flag if a ramp rate violation has occurred - up or down - because limits of battery prevented smoothing
        int violation = 0;
        if (std::abs(out_power - previous_power) > (max_ramp + 0.00001))
            violation = 1;


        // curtailment
        ssc_number_t curtail_power = 0;

        // if curtailment is considered part of the control - don't count up-ramp violations
        if (curtail_as_control) {
            if ((out_power - previous_power) > (max_ramp - 0.00001)) {
                out_power = previous_power + max_ramp;// reduce output to a non - violation
                curtail_power = pv_power + battery_power_terminal - out_power;// curtail the remainder
                violation = 0;
            }
        }

        // with this setting, curtail output power upon an upramp violation - rather than sending excess power to the grid
        // curtailment still counts as a violation
        // sum total of energy output is reduced
        if (curtail_if_violation) {
            if ((out_power - previous_power) > (max_ramp - 0.00001)) {
                out_power = previous_power + max_ramp; // reduce output to a non - violation
                curtail_power = pv_power + battery_power_terminal - out_power;// curtail the remainder
            }
        }


        // update memory variables
        if (battery_power_terminal > 0)//discharging - efficiency loss increases the amount of energy drawn from the battery
            battery_soc = battery_soc - battery_power_terminal * power_to_energy_conversion_factor / batt_half_round_trip_eff;
        else if (battery_power_terminal < 0)//charging - efficiency loss decreases the amount of energy put into the battery
            battery_soc = battery_soc - battery_power_terminal * batt_half_round_trip_eff * power_to_energy_conversion_factor;
        previous_power = out_power;
        // check limits

        // update output variables
        total_energy += out_power;
        violation_count += violation;

        m_batt_dispatch_pvs_outpower_vec.push_back( out_power);
        m_batt_dispatch_pvs_battpower_vec.push_back(battery_power_terminal);
        m_batt_dispatch_pvs_battsoc_vec.push_back(battery_soc);
        m_batt_dispatch_pvs_violation_list_vec.push_back(violation);
        m_batt_dispatch_pvs_curtail_vec.push_back(curtail_power);
    }

}
