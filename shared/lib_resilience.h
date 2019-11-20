#ifndef SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_H
#define SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_H

#include <numeric>
#include <algorithm>
#include <functional>

#include "lib_shared_inverter.h"
#include "lib_battery_dispatch.h"
#include "lib_battery_powerflow.h"
#include "../ssc/cmod_battery.h"

/*! Dispatches the battery in the case where the grid is unavailable */
class dispatch_resiliency : public dispatch_t {
public:

    dispatch_resiliency(const dispatch_t& orig, size_t index, batt_variables* vars):
            dispatch_t(orig),
            connection(static_cast<CONNECTION>(m_batteryPower->connectionMode)),
            start_outage_index(index){
        inverter = nullptr;
        if (connection == CONNECTION::DC_CONNECTED)
            inverter = std::unique_ptr<SharedInverter>(new SharedInverter(*m_batteryPower->sharedInverter));
        batt_vars = vars;
        current_outage_index = start_outage_index;
        met_loads_kw = 0;

        m_batteryPower->canClipCharge = true;
        m_batteryPower->canPVCharge = true;
        m_batteryPower->canGridCharge = false;
        m_batteryPower->canDischarge = true;

        // change SOC limits
        _Battery->capacity_model()->change_SOC_limits(0., 100.);
    }

    const CONNECTION connection;

    // Runs a timestep of an outage for an AC-connected battery
    bool run_outage_step_ac(double crit_load_kwac, double pv_kwac){
        if (connection != CONNECTION::AC_CONNECTED)
            throw std::runtime_error("Error in resilience::run_outage_step_ac: called for battery with DC connection.");

        double battery_dispatched_kwac;
        double max_discharge_kwdc = _Battery->calculate_max_discharge_kw();
        double max_charge_kwdc = _Battery->calculate_max_charge_kw();

        double met_load;

        if (pv_kwac > crit_load_kwac){
            double remaining_kwdc = -(pv_kwac - crit_load_kwac) * m_batteryPower->singlePointEfficiencyACToDC;
            remaining_kwdc = fmax(remaining_kwdc, max_charge_kwdc);
            dispatch_kw(remaining_kwdc) / m_batteryPower->singlePointEfficiencyACToDC;
            met_load = crit_load_kwac;
        }
        else{
            double max_to_load_kwac = max_discharge_kwdc * m_batteryPower->singlePointEfficiencyDCToAC + pv_kwac;

            if (max_to_load_kwac > crit_load_kwac){
                double discharge_kwdc = (crit_load_kwac - pv_kwac) / m_batteryPower->singlePointEfficiencyDCToAC;
                discharge_kwdc = fmin(discharge_kwdc, max_discharge_kwdc);
                battery_dispatched_kwac = dispatch_kw(discharge_kwdc) * m_batteryPower->singlePointEfficiencyDCToAC;
            }
            else
                battery_dispatched_kwac = dispatch_kw(max_discharge_kwdc) * m_batteryPower->singlePointEfficiencyDCToAC;
            met_load = battery_dispatched_kwac + pv_kwac;
        }

        double unmet_load = crit_load_kwac - met_load;
        met_loads_kw += met_load;
        bool survived = abs(unmet_load) < tolerance;
        if (survived)
            current_outage_index += 1;
        return survived;
    }

    bool run_outage_step_dc(double crit_load_kwac, double pv_kwdc, double V_pv, double pv_clipped, double tdry) {
        if (connection != CONNECTION::DC_CONNECTED)
            throw std::runtime_error("Error in resilience::run_outage_step_dc: called for battery with AC connection.");

        double dc_dc_eff = batt_vars->batt_dc_dc_bms_efficiency * 0.01;

        inverter->calculateACPower(pv_kwdc, V_pv, tdry);
        double dc_ac_eff = inverter->efficiencyAC * 0.01;
        double pv_kwac = inverter->powerAC_kW;

        double battery_dispatched_kwdc;
        double battery_dispatched_kwac;
        double max_discharge_kwdc = _Battery->calculate_max_discharge_kw();
        double max_charge_kwdc = _Battery->calculate_max_charge_kw();

        double met_load;
        if (pv_kwac > crit_load_kwac){
            double remaining_kwdc = -(pv_kwac - crit_load_kwac) / dc_ac_eff + pv_clipped;
            remaining_kwdc = fmax(remaining_kwdc / dc_dc_eff, max_charge_kwdc);
            dispatch_kw(remaining_kwdc);
            met_load = crit_load_kwac;
        }
        else{
            // find dc power required from pv + battery discharge to meet load
            do {
                dc_ac_eff = inverter->efficiencyAC * 0.01;
                inverter->calculateACPower(crit_load_kwac / dc_ac_eff, V_pv, tdry);
            } while (abs(dc_ac_eff * 100 - inverter->efficiencyAC) > tolerance);
            double required_kwdc = crit_load_kwac / dc_ac_eff;

            battery_dispatched_kwdc = dispatch_kw(fmin(required_kwdc / dc_dc_eff, max_discharge_kwdc));
            inverter->calculateACPower(battery_dispatched_kwdc * dc_dc_eff, V_pv, tdry);
            battery_dispatched_kwac = inverter->powerAC_kW;
            met_load = battery_dispatched_kwac + pv_kwac;
        }

        double unmet_load = crit_load_kwac - met_load;
        met_loads_kw += met_load;
        bool survived = abs(unmet_load) < tolerance;
        if (survived)
            current_outage_index += 1;
        return survived;
    }

    size_t get_indices_survived() {
        return current_outage_index - start_outage_index;
    }

    double get_met_loads(){
        return met_loads_kw;
    }

protected:
    size_t start_outage_index;
    size_t current_outage_index;
    double met_loads_kw;

    batt_variables* batt_vars;
    std::unique_ptr<SharedInverter> inverter;

    double dispatch_kw(double kw){
        double charging_current = _Battery->calculate_current_for_power_kw(kw);
        return _Battery->run(current_outage_index, charging_current);
    }

    void dispatch(size_t year, size_t hour_of_year, size_t step) override {}

};

class resiliency_runner {
private:
    std::shared_ptr<battstor> batt;

    std::map<size_t, std::shared_ptr<dispatch_resiliency>> battery_per_outage_start;

    std::vector<size_t> indices_survived;
    std::vector<double> total_load_met;

    std::vector<double> outage_durations;
    std::vector<double> probs_of_surviving;

    std::vector<std::string> logs;

public:
    explicit resiliency_runner(battstor* battery){
        batt = std::make_shared<battstor>(*battery);
        size_t steps_lifetime = batt->step_per_hour * batt->nyears * 8760;
        indices_survived.resize(steps_lifetime);
        total_load_met.resize(steps_lifetime);
    }

    std::vector<std::string> get_logs() {return logs;}

    void add_battery_at_outage_timestep(const dispatch_t& orig, size_t index){
        if (battery_per_outage_start.find(index) != battery_per_outage_start.end())
            logs.emplace_back(
                    "Replacing battery which already existed at index " + to_string(index) + ".");
        battery_per_outage_start.insert({index, std::make_shared<dispatch_resiliency>(orig, index, batt->batt_vars)});
    }

    void run_surviving_batteries(double load_ac, double pv_ac_kw, double pv_dc_kw = 0., double V = 0.,
                                 double pv_clipped = 0.,
                                 double tdry = 0.) {
        if (batt->batt_vars->batt_topology == dispatch_resiliency::DC_CONNECTED) {
            if (batt->batt_vars->inverter_paco * batt->batt_vars->inverter_count < load_ac)
                logs.emplace_back(
                        "For DC-connected battery, maximum inverter AC Power less than max load will lead to dropped load.");
        }

        std::vector<size_t> depleted_battery_keys;
        for (auto& i : battery_per_outage_start){
            size_t start_index = i.first;
            auto batt_system = i.second;
            bool survived;
            if (batt_system->connection == dispatch_resiliency::DC_CONNECTED)
                survived = batt_system->run_outage_step_dc(load_ac, pv_dc_kw, V, pv_clipped, tdry);
            else
                survived = batt_system->run_outage_step_ac(load_ac, pv_ac_kw);
            if (!survived){
                depleted_battery_keys.emplace_back(start_index);
                indices_survived[start_index] = batt_system->get_indices_survived();
            }
        }
        for (auto& i : depleted_battery_keys){
            auto b = battery_per_outage_start[i];
            indices_survived[i] = b->get_indices_survived();
            total_load_met[i] = b->get_met_loads();
            battery_per_outage_start.erase(i);
        }
    }

    // crit loads and tdry are single year; pv, V, clipped are lifetime arrays
    void run_surviving_batteries_by_looping(double* crit_loads_kwac, double* pv_kwac, double* pv_kwdc = nullptr,
                                            double* V = nullptr, double* pv_clipped = nullptr, double* tdry = nullptr){
        size_t nrec = batt->step_per_year;
        size_t steps_lifetime = nrec * batt->nyears;
        size_t i = 0;
        while (get_n_surviving_batteries() > 0 && i < steps_lifetime){
            if (pv_kwdc && V && pv_clipped && tdry)
                run_surviving_batteries(crit_loads_kwac[i % nrec], pv_kwac[i], pv_kwdc[i], V[i], pv_clipped[i], tdry[i % nrec]);
            else
                run_surviving_batteries(crit_loads_kwac[i % nrec], pv_kwac[i]);
            i++;
        }

        if (battery_per_outage_start.empty())
            return;

        double total_load = std::accumulate(crit_loads_kwac, crit_loads_kwac + nrec, 0.0) * batt->nyears;
        for (auto& b : battery_per_outage_start){
            indices_survived[b.first] = steps_lifetime;
            total_load_met[b.first] = total_load;
        }
        battery_per_outage_start.clear();
    }

    // return average hours survived
    double compute_metrics(size_t steps_per_hour){
        outage_durations.clear();
        probs_of_surviving.clear();

        double hrs_per_yr = (double)batt->step_per_hour * 8760.;
        outage_durations = std::vector<double>(indices_survived.begin(), indices_survived.end());;
        std::sort(outage_durations.begin(), outage_durations.end());
        outage_durations.erase(unique(outage_durations.begin(), outage_durations.end()), outage_durations.end());
        for (auto& i : outage_durations){
            double prob = std::count(indices_survived.begin(), indices_survived.end(), i) / hrs_per_yr;
            i /= batt->step_per_hour;       // convert to hours
            probs_of_surviving.emplace_back(prob);
        }

        return std::accumulate(indices_survived.begin(), indices_survived.end(), 0.0)/batt->step_per_hour/(double)indices_survived.size();
    }

    size_t get_n_surviving_batteries() {
        return battery_per_outage_start.size();
    }

    std::vector<double> get_hours_survived() {
        double hours_per_step = 1. / batt->step_per_hour;
        std::vector<double> hours_survived;
        for (const auto& i : indices_survived)
            hours_survived.push_back(i * hours_per_step);
        return hours_survived;
    }

    double get_avg_critical_load(){
        return std::accumulate(total_load_met.begin(), total_load_met.end(), 0.0) / (double)total_load_met.size();
    }

    std::vector<double> get_outage_duration_hrs() {
        return outage_durations;
    }

    std::vector<double> get_probs_of_surviving(){
        return probs_of_surviving;
    }
};


#endif //SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_H
