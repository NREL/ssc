#ifndef SYSTEM_ADVISOR_MODEL_LIB_STORAGE_PARAMS_H
#define SYSTEM_ADVISOR_MODEL_LIB_STORAGE_PARAMS_H

#include "core.h"

struct batvariables
{
    bool en_batt;
    bool en_fuelcell;
    int chem;
    int dispatch;
    int meter_position;
    int loss_choice;
};


struct storage_common_params
{
    bool system_use_lifetime_output;
    int analysis_period;

    /*! Battery lifetime costs */
    int calendar_choice;
    util::matrix_t<double>  lifetime_matrix;
    util::matrix_t<double> calendar_lifetime_matrix;
    double batt_calendar_q0;
    double batt_calendar_a;
    double batt_calendar_b;
    double batt_calendar_c;

    /* Battery replacement options */
    double cost_per_kwh;
    int replacement_option;
    std::vector<int> replacement_schedule;
    double replacement_capacity;
};

struct storage_front_of_meter_params
{
    /* Automated or Custom dispatch */
    int dispatch_mode;
    std::vector<double> custom_dispatch_kw;

    /* Automated Dispatch */
    size_t look_ahead_hours;
    double dispatch_update_frequency_hours;
    std::vector<double> pv_clipping_forecast;
    std::vector<double> pv_dc_power_forecast;
    std::vector<double> ppa_price_series_dollar_per_kwh;

    /* Energy rates */
    bool ec_rate_defined;
    util::matrix_t<size_t> ec_weekday_schedule;
    util::matrix_t<size_t> ec_weekend_schedule;
    util::matrix_t<double> ec_tou_matrix;

    /* Battery cycle costs */
    int cycle_cost_choice;
    double cycle_cost;
};

struct storage_behind_of_meter_params
{
    /* Automated or Custom dispatch */
    int dispatch_mode;
    std::vector<double> custom_dispatch_kw;

    std::vector<double> target_power;
};

struct storage_automated_dispatch_params
{
    /* Determines if the battery is allowed to charge from the grid using automated control*/
    bool dispatch_auto_can_gridcharge;

    /* Determines if the battery is allowed to charge from the RE using automated control*/
    bool dispatch_auto_can_charge;

    /* Determines if the battery is allowed to charge from PV clipping using automated control*/
    bool dispatch_auto_can_clipcharge;

    /* Determines if the battery is allowed to charge from fuel cell using automated control*/
    bool dispatch_auto_can_fuelcellcharge;

};

struct storage_manual_dispatch_params
{
    /* Vector of periods and if battery can charge from PV*/
    std::vector<bool> can_charge;

    /* Vector of periods if battery can charge from Fuel Cell*/
    std::vector<bool> can_fuelcellcharge;

    /* Vector of periods and if battery can discharge*/
    std::vector<bool> can_discharge;

    /* Vector of periods and if battery can charge from the grid*/
    std::vector<bool> can_gridcharge;

    /* Vector of percentages that battery is allowed to charge for periods*/
    std::vector<double> discharge_percent;

    /* Vector of percentages that battery is allowed to gridcharge for periods*/
    std::vector<double> gridcharge_percent;

    /* Schedule of manual discharge for weekday*/
    util::matrix_t<size_t> discharge_schedule_weekday;

    /* Schedule of manual discharge for weekend*/
    util::matrix_t<size_t> discharge_schedule_weekend;
};

struct storage_state_params
{
    ssc_number_t *pcharge = 0;
    ssc_number_t *pdischarge = 0;
    ssc_number_t *pdischarge_percent = 0;
    ssc_number_t *pgridcharge_percent = 0;
    ssc_number_t *pgridcharge = 0;
    ssc_number_t *psched = 0;
    ssc_number_t *psched_weekend = 0;
};

struct storage_losses_params
{
    int loss_monthly_or_timeseries;
    std::vector<double> losses_charging;
    std::vector<double> losses_discharging;
    std::vector<double> losses_idle;
    std::vector<double> losses;
};

struct battery_voltage_params
{
    double Vnom_default;
    double Vfull;
    double Vexp;
    double Vnom;
    double Qfull;
    double Qfull_flow;
    double Qexp;
    double Qnom;
    double C_rate;
    double resistance;
};

struct battery_thermal_params
{
    double mass;
    double length;
    double width;
    double height;
    double Cp;
    util::matrix_t<double> cap_vs_temp;
    double h_to_ambient;
    std::vector<double> T_room;
};

struct battery_inverter_params
{
    size_t inverter_model;
    size_t inverter_count;
    double inverter_efficiency;
    double inverter_paco;
};

struct battery_properties_params
{
    battery_thermal_params thermal;

    // Battery bank sizing
    int computed_series;
    int computed_strings;
    double kw;
    double kwh;

    // voltage properties using either voltage model or table
    int voltage_choice;
    battery_voltage_params voltage_vars;
    util::matrix_t<double> voltage_matrix;

    // restrict operations by current, power or both
    int restriction_choice;

    double current_charge_max;
    double current_discharge_max;
    double power_charge_max;
    double power_discharge_max;

    // charge limits
    double initial_SOC;
    double maximum_SOC;
    double minimum_SOC;
    double minimum_modetime;


    // power converters and topology
    int topology;
    double ac_dc_efficiency;
    double dc_ac_efficiency;
    double dc_dc_bms_efficiency;
    double pv_dc_dc_mppt_efficiency;

    battery_inverter_params inverter;
};

struct LeadAcid_properties_params
{
    double LeadAcid_q20_computed;
    double LeadAcid_tn;
    double LeadAcid_qn_computed;
    double LeadAcid_q10_computed;
};

struct battery_state_params
{

};


/*! Fuelcell Storage Design Parameters */

struct fuelcell_automated_FOM_params
{
    storage_common_params common;
    storage_state_params state;

    storage_front_of_meter_params FOM;
    storage_automated_dispatch_params dispatch;
};

struct fuelcell_manual_FOM_params
{
    storage_common_params common;
    storage_state_params state;

    storage_front_of_meter_params FOM;
    storage_manual_dispatch_params dispatch;
};

struct fuelcell_automated_BOM_params
{
    storage_common_params common;
    storage_state_params state;

    storage_behind_of_meter_params BOM;
    storage_automated_dispatch_params dispatch;
};

struct fuelcell_manual_BOM_params
{
    storage_common_params common;
    storage_state_params state;

    storage_behind_of_meter_params BOM;
    storage_manual_dispatch_params dispatch;
};

/*! Lithium Ion, Vanadium Redox and Iron Flow Battery Design Parameters */

struct battery_automated_FOM_params
{
    storage_common_params common;
    storage_state_params state;
    battery_properties_params properties;

    storage_front_of_meter_params FOM;
    storage_automated_dispatch_params dispatch;
};

struct battery_manual_FOM_params
{
    storage_common_params common;
    storage_state_params state;
    battery_properties_params properties;

    storage_front_of_meter_params FOM;
    storage_manual_dispatch_params dispatch;
};

struct battery_automated_BOM_params
{
    storage_common_params common;
    storage_state_params state;
    battery_properties_params properties;

    storage_behind_of_meter_params FOM;
    storage_automated_dispatch_params dispatch;
};

struct battery_manual_BOM_params
{
    storage_common_params common;
    storage_state_params state;
    battery_properties_params properties;

    storage_behind_of_meter_params FOM;
    storage_manual_dispatch_params dispatch;
};

/*! Lead Acid Battery Design Parameters */

struct LeadAcid_automated_FOM_params
{
    storage_common_params common;
    storage_state_params state;
    battery_properties_params properties;

    LeadAcid_properties_params leadAcid;

    storage_front_of_meter_params FOM;
    storage_automated_dispatch_params dispatch;
};

struct LeadAcid_manual_FOM_params
{
    storage_common_params common;
    storage_state_params state;
    battery_properties_params properties;

    LeadAcid_properties_params leadAcid;

    storage_front_of_meter_params FOM;
    storage_manual_dispatch_params dispatch;
};

struct LeadAcid_automated_BOM_params
{
    storage_common_params common;
    storage_state_params state;
    battery_properties_params properties;

    LeadAcid_properties_params leadAcid;

    storage_behind_of_meter_params FOM;
    storage_automated_dispatch_params dispatch;
};

struct LeadAcid_manual_BOM_params
{
    storage_common_params common;
    storage_state_params state;
    battery_properties_params properties;

    LeadAcid_properties_params leadAcid;

    storage_behind_of_meter_params FOM;
    storage_manual_dispatch_params dispatch;
};

#endif //SYSTEM_ADVISOR_MODEL_LIB_STORAGE_PARAMS_H
