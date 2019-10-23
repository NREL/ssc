#ifndef SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_H
#define SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_H

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
        inverter = std::unique_ptr<SharedInverter>(new SharedInverter(*m_batteryPower->sharedInverter));
        batt_vars = vars;
        current_outage_index = start_outage_index;

        m_batteryPower->canClipCharge = true;
        m_batteryPower->canPVCharge = true;
        m_batteryPower->canGridCharge = false;
        m_batteryPower->canDischarge = true;
    }

    const CONNECTION connection;

    bool run_offgrid_step(size_t index, double load_ac, double pv_ac_kw, double pv_dc_kw, double V_pv, double pv_clipped, double tdry){
        current_outage_index = index;
        double dc_dc_eff = batt_vars->batt_dc_dc_bms_efficiency * 0.01;
        if (pv_ac_kw > load_ac){
            meet_load_charge_battery(load_ac, pv_ac_kw, pv_dc_kw, V_pv, pv_clipped, tdry);
        }
        else{
            double max_power_to_load;
            double max_discharge_power = _Battery->calculate_max_discharge_kw();
            if (connection == DC_CONNECTED) {
                double inv_eff_approx;
                do {
                    inv_eff_approx = inverter->efficiencyAC;
                    inverter->calculateACPower(load_ac / (inv_eff_approx * 0.01), V_pv, tdry);
                } while (abs(inv_eff_approx - inverter->efficiencyAC) > 0.1);
                max_power_to_load = (max_discharge_power * dc_dc_eff + pv_dc_kw) * inv_eff_approx;
            }
            else {
                max_power_to_load = max_discharge_power + pv_ac_kw;
            }

            if (max_power_to_load > load_ac)
                meet_load_discharge_battery(pv_dc_kw, load_ac, max_power_to_load, max_discharge_power);
            else
                drop_load_discharge_battery(max_discharge_power);
        }

        return _Battery->battery_soc() > tolerance;
    }

    size_t get_indices_survived() {
        return current_outage_index - start_outage_index;
    }

protected:
    size_t start_outage_index;

    size_t current_outage_index;

    batt_variables* batt_vars;

    std::unique_ptr<SharedInverter> inverter;

    void dispatch(size_t year, size_t hour_of_year, size_t step) override {

    }

    void meet_load_charge_battery(double load_ac, double pv_ac_kw, double pv_dc_kw, double V_pv, double pv_clipped, double tdry){
        double max_charge_power = _Battery->calculate_max_charge_kw();
        double dc_dc_eff = batt_vars->batt_dc_dc_bms_efficiency * 0.01;

        if (connection == DC_CONNECTED){
            inverter->calculateACPower(load_ac / (batt_vars->inverter_efficiency * 0.01), V_pv, tdry);
            double inv_eff_approx = inverter->efficiencyAC * 0.01;
            double remaining_power_dc = pv_dc_kw - load_ac / inv_eff_approx + pv_clipped;

            // change the dc voltage from pv's to battery's required voltage
            double charge_power_dc = fmin(remaining_power_dc * dc_dc_eff, max_charge_power );
            double charging_current = _Battery->calculate_current_for_power(charge_power_dc);
            _Battery->run(current_outage_index, charging_current);
        }
        else{
            double remaining_power_ac = pv_ac_kw - load_ac;

            // change the ac voltage
            double charge_power_ac = fmin(remaining_power_ac, max_charge_power);
            double charging_current = _Battery->calculate_current_for_power(charge_power_ac);
            _Battery->run(current_outage_index, charging_current);
        }
    }

    void meet_load_discharge_battery(double pv_dc_kw, double load_ac, double max_power_to_load, double max_discharge_power){
        if (max_power_to_load > load_ac){
            double discharge_power = load_ac / inverter->efficiencyAC - pv_dc_kw;
            double discharge_current = _Battery->calculate_current_for_power(discharge_power);
            _Battery->run(current_outage_index, discharge_current);
        }
        else{
            double discharge_current = _Battery->calculate_current_for_power(max_discharge_power);
            _Battery->run(current_outage_index, discharge_current);
        }
    }

    void drop_load_discharge_battery(double max_discharge_power){
        double discharge_current = _Battery->calculate_current_for_power(max_discharge_power);
        _Battery->run(current_outage_index, discharge_current);
    }


};

class resiliency_runner {
private:
    std::shared_ptr<battstor> batt;

    std::map<size_t, std::shared_ptr<dispatch_resiliency>> battery_per_outage_start;

    std::map<size_t, size_t> hours_survived;

    std::vector<std::string> logs;

public:
    explicit resiliency_runner(battstor* battery, const std::vector<double>& load_kw){
        batt = std::make_shared<battstor>(*battery);

        if (batt->batt_vars->batt_topology == dispatch_resiliency::DC_CONNECTED){
            double max_load = *max_element(load_kw.begin(), load_kw.end());
            if (batt->batt_vars->inverter_paco * batt->batt_vars->inverter_count < max_load)
                logs.emplace_back("For DC-connected battery, maximum inverter AC Power less than max load will lead to dropped load.");
        }
    }

    std::vector<std::string> get_logs() {return logs;}

    void add_battery_at_outage_index(const dispatch_t& orig, size_t index){
        battery_per_outage_start.insert({index, std::make_shared<dispatch_resiliency>(orig, index, batt->batt_vars)});
    }

    void run_surviving_batteries(size_t index, double load_ac, double pv_ac_kw, double pv_dc_kw = 0., double V = 0., double pv_clipped = 0., double tdry = 0.){

        std::vector<size_t> depleted_battery_keys;
        for (auto& i : battery_per_outage_start){
            size_t start_hour = i.first;
            auto batt_system = i.second;
            bool survived = batt_system->run_offgrid_step(index, load_ac, pv_ac_kw, pv_dc_kw, V, pv_clipped, tdry);
            if (!survived){
                depleted_battery_keys.emplace_back(start_hour);
                hours_survived.insert({start_hour, batt_system->get_indices_survived()});
            }
        }
        for (auto& i : depleted_battery_keys){
            battery_per_outage_start.erase(i);
        }
    }

    void compute_metrics(){

    }

    std::map<size_t, size_t> get_outage_hours_survived() {
        return hours_survived;
    }
};


#endif //SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_H
