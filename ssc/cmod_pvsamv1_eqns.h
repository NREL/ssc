#ifndef SYSTEM_ADVISOR_MODEL_CMOD_PVSAMV1_EQNS_H
#define SYSTEM_ADVISOR_MODEL_CMOD_PVSAMV1_EQNS_H

#include "vartab.h"
#include "sscapi.h"

#ifdef __cplusplus
extern "C" {
#endif

static const char *Reopt_size_battery_params_doc =
        "Given a PV system, get the optimal battery size. Wind and additional PV are disabled."
        "Maps SAM compute module inputs to those of the ReOpt Lite API\\n"
        "Residential, Commercial, Third Party and Host Developer financial models.\\n"
        "Input: var_table with key-value pairs:  \\n"
        "     ++ Site inputs ++\\n"
        "     'lon' - double, longitude\\n"
        "     'lat' - double, latitude\\n"
        "     ++ PV inputs ++\\n"
        "     'subarray1_track_mode' - double, 0=fixed,1=1axis,2=2axis,3=azi,4=monthly\\n"
        "     'subarray1_backtrack' - double, 0=false, 1=true\\n"
        "     'subarray1_azimuth' - double\\n"
        "     'subarray1_tilt' - double\\n"
        "     'dc_degradation' - double [%/year], Annual module degradation, 0-100\\n"
        "     'subarray1_gcr' - double, ground coverage ratio, between 0 and 1\\n"
        "     'inverter_model' - double, 0=cec,1=datasheet,2=partload,3=coefficientgenerator\\n"
        "     one of 'inv_snl_eff_cec', 'inv_ds_eff', 'inv_pd_eff', 'inv_cec_cg_eff' - double [%], inverter efficiency for selected inverter model, 0-100\\n"
        "     'inverter_count' - double\\n"
        "     'system_capacity' - double [kW], PV AC system size\\n"
        "     'dc_ac_ratio' - double\\n"
        "     'subarray1_dcloss' - double [%], PV array power loss, 0-100\\n"
        "     'dc_adjust' - optional, double [%], DC power loss additional, optional, 0-100\\n"
        "     'dcoptimizer_loss' - optional, double [%], DC optimizer power loss, 0-100\\n"
        "     'itc_fed_percent' - double [%], Percentage of capital costs that are credited towards federal taxes, 0-100\\n"
        "     'pbi_fed_amount' - double [$/kWh], Federal PBI amount\\n"
        "     'pbi_fed_term' - double [years], Federal PBI term\\n"
        "     'ibi_sta_percent' - double [%], Federal percentage-based IBI percent, 0-100\\n"
        "     'ibi_sta_percent_maxvalue' - double [$], Federal percentage-based IBI maximum value\\n"
        "     'ibi_uti_percent' - double [%], Utility percentage-based IBI percent, 0-100\\n"
        "     'ibi_uti_percent_maxvalue' - double [$], Utility percentage-based IBI maximum value\\n"
        "     'om_fixed' - double [$/year], Fixed O&M annual amount\\n"
        "     'om_production' - double [$/MWh], Production-based O&M amount\\n"
        "     'total_installed_cost' - double [$], Total system installation cost\\n"
        "     'depr_bonus_fed' - optional, double [%], Percent of upfront project costs to depreciate in year one in addition to scheduled depreciation, 0-100\\n"
        "     'depr_bonus_fed_macrs_5' - optional, double [0/1], Federal bonus depreciation follows 5-yr MACRS\\n"
        "     ++ Battery inputs ++\\n"
        "     'batt_dc_ac_efficiency' - double [%], Battery DC to AC efficiency, 0-100\\n"
        "     'batt_ac_dc_efficiency' - double [%], Inverter AC to battery DC efficiency, 0-100\\n"
        "     'battery_per_kW' - double [$/kW], Battery cost per kW\\n"
        "     'battery_per_kWh' - double [$/kWh], Battery cost per kWh\\n"
        "     'om_replacement_cost1' - double [$/kWh], Cost to replace battery per kWh\\n"
        "     'batt_initial_SOC' - double [%], Initial State-of-Charge, 0-100\\n"
        "     'batt_minimum_SOC' - double [%], Minimum State-of-Charge, 0-100\\n"
        "     'batt_replacement_schedule' - array [year], Number of years from start of analysis period to replace battery\\n"
        "     'batt_replacement_cost' - optional, double [$/kWh], Battery capacity replacement cost\\n"
        "     ++ Utility Rate inputs ++\\n"
        "     'ur_monthly_fixed_charge' - double [$], Monthly fixed charge\\n"
        "     'ur_dc_sched_weekday' - matrix [tiers], Demand charge weekday schedule, count starts at 1, 12mx24hr\\n"
        "     'ur_dc_sched_weekend' - matrix [tiers], Demand charge weekend schedule, count starts at 1, 12mx24hr\\n"
        "     'ur_dc_tou_mat' - matrix [[period, tier, kWh, 'kWh', $/kWh], Energy rates (TOU), each row provides period, tier, max usage, 'kWh' units, and charge\\n"
        "     'ur_dc_flat_mat' -matrix [[month, tier, kW, $]] - Demand rates (flat), each row provides month, tier, peak demand and charge \\n"
        "     'ur_ec_sched_weekday' - matrix [tiers], Energy charge weekday schedule, count starts at 1, 12mx24hr\\n"
        "     'ur_ec_sched_weekend' - matrix [tiers], Energy charge weekend schedule, count starts at 1, 12mx24hr\\n"
        "     'ur_ec_tou_mat' - matrix [[period, tier, kw, $], Demand rates (TOU), each row provides period, tier, peak power, and charge\\n"
        "     'load_user_data' - array [kW], Electricity load (year 1)\\n"
        "     'crit_load_user_data' - array [kW], Critical electricity load (year 1)\\n"
        "     ++ Financial inputs ++\\n"
        "     'analysis_period' - double [years]\\n"
        "     'federal_tax_rate' - double [%], 0-100\\n"
        "     'state_tax_rate' - double [%], 0-100\\n"
        "     'rate_escalation' - double [%/year], Annual electricity rate escalation, 0-100\\n"
        "     'inflation_rate' - double [%], 0-100\\n"
        "     'real_discount_rate' - double [%], 0-100\\n"
        "     'om_fixed_escal' - double [%/year], Fixed O&M escalation\\n"
        "     'om_production_escal' - double [%/year], Production-based O&M escalation\\n"
        "     'total_installed_cost' - double [$]\\n"
        "     'value_of_lost_load' - double [$/kWh], Value placed on unmet site load during grid outages\\n"
        "Output: key-value pairs added to var_table\\n"
        "     'reopt_scenario' - table, Scenario inputs to Reopt Lite API\\n"
        "     'log' - string";

SSCEXPORT void Reopt_size_battery_params(ssc_data_t data);


}

#endif //SYSTEM_ADVISOR_MODEL_CMOD_PVSAMV1_EQNS_H