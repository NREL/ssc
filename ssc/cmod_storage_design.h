#ifndef SYSTEM_ADVISOR_MODEL_CMOD_STORAGE_DESIGN_H
#define SYSTEM_ADVISOR_MODEL_CMOD_STORAGE_DESIGN_H

#include "core.h"

struct batvariables
{
    bool en_batt;
    bool en_fuelcell;
    int chem;
    int dispatch;
    int voltage_choice;
    int meter_position;
    int target_choice;
    int loss_choice;
    int calendar_choice;












    std::vector<double> target_power_monthly;
    std::vector<double> target_power;








    double batt_replacement_capacity;
    util::matrix_t<double> cap_vs_temp;

    double batt_h_to_ambient;
    std::vector<double> T_room;







    double batt_calendar_q0;
    double batt_calendar_a;
    double batt_calendar_b;
    double batt_calendar_c;

};


struct storage_common
{
    bool system_use_lifetime_output;
    int analysis_period;

    util::matrix_t<double>  lifetime_matrix;
    util::matrix_t<double> calendar_lifetime_matrix;

    /*! Battery costs */
    double cost_per_kwh;


    /* Battery replacement options */
    int replacement_option;
    std::vector<int> replacement_schedule;
};

struct storage_FOM
{
    std::vector<double> pv_clipping_forecast;
    std::vector<double> pv_dc_power_forecast;
    std::vector<double> ppa_price_series_dollar_per_kwh;

    /*! Energy rates */
    bool ec_rate_defined;
    util::matrix_t<size_t> ec_weekday_schedule;
    util::matrix_t<size_t> ec_weekend_schedule;
    util::matrix_t<double> ec_tou_matrix;

    /* Battery cycle costs */
    int cycle_cost_choice;
    double cycle_cost;

};

struct storage_state
{
    ssc_number_t *pcharge = 0;
    ssc_number_t *pdischarge = 0;
    ssc_number_t *pdischarge_percent = 0;
    ssc_number_t *pgridcharge_percent = 0;
    ssc_number_t *pgridcharge = 0;
    ssc_number_t *psched = 0;
    ssc_number_t *psched_weekend = 0;
};

struct storage_losses
{
    int loss_monthly_or_timeseries;
    std::vector<double> losses_charging;
    std::vector<double> losses_discharging;
    std::vector<double> losses_idle;
    std::vector<double> losses;
};

struct battery_properties
{
    double mass;
    double length;
    double width;
    double height;
    double Cp;

    // Battery bank sizing
    int computed_series;
    int computed_strings;
    double kw;
    double kwh;

    // voltage properties using either voltage model or table
    int voltage_choice;
    struct voltage_model
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



    size_t inverter_model;
    double inverter_efficiency;
    double inverter_paco;
    size_t inverter_count;


};

struct battery_state
{

};

struct storage_automated_dispatch
{
    /*! Determines if the battery is allowed to charge from the grid using automated control*/
    bool dispatch_auto_can_gridcharge;

    /*! Determines if the battery is allowed to charge from the RE using automated control*/
    bool dispatch_auto_can_charge;

    /*! Determines if the battery is allowed to charge from PV clipping using automated control*/
    bool dispatch_auto_can_clipcharge;

    /*! Determines if the battery is allowed to charge from fuel cell using automated control*/
    bool dispatch_auto_can_fuelcellcharge;

    /*! The number of hours to look-ahead in automated dispatch */
    size_t look_ahead_hours;

    /*! The frequency to update the look-ahead automated dispatch */
    double dispatch_update_frequency_hours;
};

struct storage_manual_dispatch
{
    /*! Vector of periods and if battery can charge from PV*/
    std::vector<bool> can_charge;

    /*! Vector of periods if battery can charge from Fuel Cell*/
    std::vector<bool> can_fuelcellcharge;

    /*! Vector of periods and if battery can discharge*/
    std::vector<bool> can_discharge;

    /*! Vector of periods and if battery can charge from the grid*/
    std::vector<bool> can_gridcharge;

    /*! Vector of percentages that battery is allowed to charge for periods*/
    std::vector<double> discharge_percent;

    /*! Vector of percentages that battery is allowed to gridcharge for periods*/
    std::vector<double> gridcharge_percent;

    /*! Schedule of manual discharge for weekday*/
    util::matrix_t<size_t> discharge_schedule_weekday;

    /*! Schedule of manual discharge for weekend*/
    util::matrix_t<size_t> discharge_schedule_weekend;
};

struct storage_custom_dispatch
{
    /*! The custom dispatch power input by user (<0 = charging, >0 = discharging) in kW */
    std::vector<double> custom_dispatch_kw;
};

struct fuelcell_automated_dispatch_design
{
    storage_common common;
    storage_state state;
    storage_automated_dispatch dispatch;
};

struct fuelcell_manual_dispatch_design
{
    storage_common common;
    storage_state state;
    storage_automated_dispatch dispatch;
};

struct LeadAcid_automated_BTM_design
{
    storage_common common;
    storage_state state;
    storage_automated_dispatch dispatch;

    battery_properties properties;

    double LeadAcid_q20_computed;
    double LeadAcid_tn;
    double LeadAcid_qn_computed;
    double LeadAcid_q10_computed;
};

#endif //SYSTEM_ADVISOR_MODEL_CMOD_STORAGE_DESIGN_H
