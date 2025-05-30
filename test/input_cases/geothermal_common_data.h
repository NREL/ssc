/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/



#ifndef _GEOTHERMAL_COMMON_DATA_H_
#define _GEOTHERMAL_COMMON_DATA_H_

#include <stdio.h>
#include "code_generator_utilities.h"

static char geothermal_weather_path[256];
static char geothermal_dispatch_path[256];
static char geothermal_curtailment_path[256];

static int n1 = sprintf(geothermal_weather_path, "%s/test/input_cases/general_data/daggett_ca_34.865371_-116.783023_psmv3_60_tmy.csv", SSCDIR);
static int n2 = sprintf(geothermal_dispatch_path, "%s/test/input_cases/geothermal_data/dispatch_factors_ts.csv", SSCDIR);
static int n3 = sprintf(geothermal_curtailment_path, "%s/test/input_cases/general_data/grid_curtailment_default_MW.csv", SSCDIR);

/**
*  Default data for no-financial pvsamv1 run that can be further modified
*/
static void geothermal_singleowner_default(ssc_data_t &data)
{
	ssc_data_set_number(data, "ui_calculations_only", 0);
	ssc_data_set_string(data, "file_name", geothermal_weather_path);
	ssc_data_set_number(data, "resource_potential", 210);
	ssc_data_set_number(data, "resource_type", 0);
	ssc_data_set_number(data, "resource_temp", 200);
	ssc_data_set_number(data, "resource_depth", 2000);
    ssc_data_set_number(data, "geotherm.cost.inj_prod_well_ratio", 50);
	ssc_data_set_number(data, "geothermal_analysis_period", 2);
	ssc_data_set_number(data, "model_choice", 0);
	ssc_data_set_number(data, "specified_pump_work_amount", 0);
	ssc_data_set_number(data, "nameplate", 30000);
	ssc_data_set_number(data, "analysis_type", 0);
	ssc_data_set_number(data, "num_wells", 3);
	ssc_data_set_number(data, "num_wells_getem", 4.3197474479675293);
	ssc_data_set_number(data, "conversion_type", 0);
	ssc_data_set_number(data, "plant_efficiency_input", 80);
	ssc_data_set_number(data, "conversion_subtype", 0);
	ssc_data_set_number(data, "decline_type", 0);
	ssc_data_set_number(data, "temp_decline_rate", 0.5);
	ssc_data_set_number(data, "temp_decline_max", 30);
    ssc_data_set_number(data, "dt_prod_well", 0.0);
    ssc_data_set_number(data, "prod_well_choice", 0);
	ssc_data_set_number(data, "wet_bulb_temp", 15);
    ssc_data_set_number(data, "use_weather_file_conditions", 0);
	ssc_data_set_number(data, "ambient_pressure", 14.699999809265137);
	ssc_data_set_number(data, "well_flow_rate", 110);
	ssc_data_set_number(data, "pump_efficiency", 67.5);
	ssc_data_set_number(data, "delta_pressure_equip", 40);
	ssc_data_set_number(data, "excess_pressure_pump", 50);
	ssc_data_set_number(data, "well_diameter", 12.25);
	ssc_data_set_number(data, "casing_size", 9.625);
	ssc_data_set_number(data, "inj_well_diam", 12.25);
    ssc_data_set_number(data, "inj_casing_size", 11.5);
    ssc_data_set_number(data, "geotherm.cost.inj_cost_curve_welltype", 0);
    ssc_data_set_number(data, "geotherm.cost.prod_cost_curve_welltype", 0);
    ssc_data_set_number(data, "geotherm.cost.inj_cost_curve_welldiam", 0);
    ssc_data_set_number(data, "geotherm.cost.prod_cost_curve_welldiam", 0);
	ssc_data_set_number(data, "design_temp", 200);
	ssc_data_set_number(data, "specify_pump_work", 0);
	ssc_data_set_number(data, "rock_thermal_conductivity", 259200);
	ssc_data_set_number(data, "rock_specific_heat", 950);
	ssc_data_set_number(data, "rock_density", 2600);
	ssc_data_set_number(data, "reservoir_pressure_change_type", 0);
	ssc_data_set_number(data, "reservoir_pressure_change", 1000.0 / 0.40000000596046448);
    ssc_data_set_number(data, "injectivity_index", 3000);
    ssc_data_set_number(data, "exploration_wells_production", 0);
    ssc_data_set_number(data, "drilling_success_rate", 75);
    ssc_data_set_number(data, "stim_success_rate", 0);
    ssc_data_set_number(data, "failed_prod_flow_ratio", 0.3);
	ssc_data_set_number(data, "reservoir_width", 500);
	ssc_data_set_number(data, "reservoir_height", 100);
	ssc_data_set_number(data, "reservoir_permeability", 0.05000000074505806);
	ssc_data_set_number(data, "inj_prod_well_distance", 1500);
	ssc_data_set_number(data, "subsurface_water_loss", 2);
	ssc_data_set_number(data, "fracture_aperature", 0.00039999998989515007);
	ssc_data_set_number(data, "fracture_width", 175);
	ssc_data_set_number(data, "num_fractures", 6);
	ssc_data_set_number(data, "fracture_angle", 15);
    ssc_data_set_number(data, "fracture_length", 1000.0);
    ssc_data_set_number(data, "fracture_spacing", 50.0);
	ssc_data_set_number(data, "T_htf_cold_ref", 90);
	ssc_data_set_number(data, "T_htf_hot_ref", 200);
	ssc_data_set_number(data, "HTF", 3);
	ssc_data_set_number(data, "P_boil", 2);
	ssc_data_set_number(data, "eta_ref", 0.17000000178813934);
	ssc_data_set_number(data, "q_sby_frac", 0.20000000298023224);
	ssc_data_set_number(data, "startup_frac", 0.20000000298023224);
	ssc_data_set_number(data, "startup_time", 1);
	ssc_data_set_number(data, "pb_bd_frac", 0.013000000268220901);
	ssc_data_set_number(data, "T_amb_des", 15);
	ssc_data_set_number(data, "CT", 3);
	ssc_data_set_number(data, "dT_cw_ref", 10);
	ssc_data_set_number(data, "T_approach", 5);
	ssc_data_set_number(data, "T_ITD_des", 16);
	ssc_data_set_number(data, "P_cond_ratio", 1.0027999877929688);
	ssc_data_set_number(data, "P_cond_min", 1.25);
	ssc_data_set_number(data, "hr_pl_nlev", 8);
	ssc_data_set_number(data, "hc_ctl1", 0);
	ssc_data_set_number(data, "hc_ctl2", 0);
	ssc_data_set_number(data, "hc_ctl3", 0);
	ssc_data_set_number(data, "hc_ctl4", 0);
	ssc_data_set_number(data, "hc_ctl5", 0);
	ssc_data_set_number(data, "hc_ctl6", 0);
	ssc_data_set_number(data, "hc_ctl7", 0);
	ssc_data_set_number(data, "hc_ctl8", 0);
	ssc_data_set_number(data, "hc_ctl9", 0);
	ssc_data_set_string(data, "hybrid_dispatch_schedule", "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111");

    ssc_data_set_number(data, "adjust_constant", 0.0);

	ssc_data_set_number(data, "enable_interconnection_limit", 0);
	set_array(data, "grid_curtailment", geothermal_curtailment_path, 8760);
	ssc_data_set_number(data, "grid_interconnection_limit_kwac", 100000);
	
	ssc_data_set_number(data, "analysis_period", 2);

	ssc_number_t p_federal_tax_rate[1] = { 21 };
	ssc_data_set_array(data, "federal_tax_rate", p_federal_tax_rate, 1);
	ssc_number_t p_state_tax_rate[1] = { 7 };
	ssc_data_set_array(data, "state_tax_rate", p_state_tax_rate, 1);
	ssc_data_set_number(data, "property_tax_rate", 1);
	ssc_data_set_number(data, "prop_tax_cost_assessed_percent", 100);
	ssc_data_set_number(data, "prop_tax_assessed_decline", 0);
	ssc_data_set_number(data, "real_discount_rate", 6.4000000953674316);
	ssc_data_set_number(data, "inflation_rate", 2.5);
	ssc_data_set_number(data, "insurance_rate", 0.5);
	ssc_data_set_number(data, "system_capacity", 34788.67578125);
	ssc_number_t p_om_fixed[1] = { 0 };
	ssc_data_set_array(data, "om_fixed", p_om_fixed, 1);
	ssc_data_set_number(data, "om_fixed_escal", 0);
	ssc_number_t p_om_production[1] = { 0 };
	ssc_data_set_array(data, "om_production", p_om_production, 1);
	ssc_data_set_number(data, "om_production_escal", 0);
	ssc_number_t p_om_capacity[1] = { 175 };
	ssc_data_set_array(data, "om_capacity", p_om_capacity, 1);
	ssc_data_set_number(data, "om_capacity_escal", 0);
	ssc_number_t p_om_fuel_cost[1] = { 0 };
	ssc_data_set_array(data, "om_fuel_cost", p_om_fuel_cost, 1);
	ssc_data_set_number(data, "om_fuel_cost_escal", 0);
    ssc_number_t itc_amount[1] = { 0 };
    ssc_number_t itc_fed_percent[1] = { 30 };
    ssc_number_t itc_sta_percent[1] = { 0 };
    ssc_number_t itc_amount_max[1] = { 1e+38 };
    ssc_data_set_array(data, "itc_fed_amount", itc_amount, 1);
    ssc_data_set_array(data, "itc_sta_amount", itc_amount, 1);
    ssc_data_set_array(data, "itc_fed_percent", itc_fed_percent, 1);
    ssc_data_set_array(data, "itc_sta_percent", itc_sta_percent, 1);
    ssc_data_set_array(data, "itc_fed_percent_maxvalue", itc_amount_max, 1);
    ssc_data_set_array(data, "itc_sta_percent_maxvalue", itc_amount_max, 1);
	ssc_data_set_number(data, "itc_fed_amount_deprbas_fed", 1);
	ssc_data_set_number(data, "itc_fed_amount_deprbas_sta", 1);
	ssc_data_set_number(data, "itc_sta_amount_deprbas_fed", 0);
	ssc_data_set_number(data, "itc_sta_amount_deprbas_sta", 0);
	ssc_data_set_number(data, "itc_fed_percent_deprbas_fed", 1);
	ssc_data_set_number(data, "itc_fed_percent_deprbas_sta", 1);
	ssc_data_set_number(data, "itc_sta_percent_deprbas_fed", 0);
	ssc_data_set_number(data, "itc_sta_percent_deprbas_sta", 0);
	ssc_number_t p_ptc_fed_amount[1] = { 0 };
	ssc_data_set_array(data, "ptc_fed_amount", p_ptc_fed_amount, 1);
	ssc_data_set_number(data, "ptc_fed_term", 10);
	ssc_data_set_number(data, "ptc_fed_escal", 0);
	ssc_number_t p_ptc_sta_amount[1] = { 0 };
	ssc_data_set_array(data, "ptc_sta_amount", p_ptc_sta_amount, 1);
	ssc_data_set_number(data, "ptc_sta_term", 10);
	ssc_data_set_number(data, "ptc_sta_escal", 0);
	ssc_data_set_number(data, "ibi_fed_amount", 0);
	ssc_data_set_number(data, "ibi_fed_amount_tax_fed", 1);
	ssc_data_set_number(data, "ibi_fed_amount_tax_sta", 1);
	ssc_data_set_number(data, "ibi_fed_amount_deprbas_fed", 0);
	ssc_data_set_number(data, "ibi_fed_amount_deprbas_sta", 0);
	ssc_data_set_number(data, "ibi_sta_amount", 0);
	ssc_data_set_number(data, "ibi_sta_amount_tax_fed", 1);
	ssc_data_set_number(data, "ibi_sta_amount_tax_sta", 1);
	ssc_data_set_number(data, "ibi_sta_amount_deprbas_fed", 0);
	ssc_data_set_number(data, "ibi_sta_amount_deprbas_sta", 0);
	ssc_data_set_number(data, "ibi_uti_amount", 0);
	ssc_data_set_number(data, "ibi_uti_amount_tax_fed", 1);
	ssc_data_set_number(data, "ibi_uti_amount_tax_sta", 1);
	ssc_data_set_number(data, "ibi_uti_amount_deprbas_fed", 0);
	ssc_data_set_number(data, "ibi_uti_amount_deprbas_sta", 0);
	ssc_data_set_number(data, "ibi_oth_amount", 0);
	ssc_data_set_number(data, "ibi_oth_amount_tax_fed", 1);
	ssc_data_set_number(data, "ibi_oth_amount_tax_sta", 1);
	ssc_data_set_number(data, "ibi_oth_amount_deprbas_fed", 0);
	ssc_data_set_number(data, "ibi_oth_amount_deprbas_sta", 0);
	ssc_data_set_number(data, "ibi_fed_percent", 0);
	ssc_data_set_number(data, "ibi_fed_percent_maxvalue", 9.9999996802856925e+37);
	ssc_data_set_number(data, "ibi_fed_percent_tax_fed", 1);
	ssc_data_set_number(data, "ibi_fed_percent_tax_sta", 1);
	ssc_data_set_number(data, "ibi_fed_percent_deprbas_fed", 0);
	ssc_data_set_number(data, "ibi_fed_percent_deprbas_sta", 0);
	ssc_data_set_number(data, "ibi_sta_percent", 0);
	ssc_data_set_number(data, "ibi_sta_percent_maxvalue", 9.9999996802856925e+37);
	ssc_data_set_number(data, "ibi_sta_percent_tax_fed", 1);
	ssc_data_set_number(data, "ibi_sta_percent_tax_sta", 1);
	ssc_data_set_number(data, "ibi_sta_percent_deprbas_fed", 0);
	ssc_data_set_number(data, "ibi_sta_percent_deprbas_sta", 0);
	ssc_data_set_number(data, "ibi_uti_percent", 0);
	ssc_data_set_number(data, "ibi_uti_percent_maxvalue", 9.9999996802856925e+37);
	ssc_data_set_number(data, "ibi_uti_percent_tax_fed", 1);
	ssc_data_set_number(data, "ibi_uti_percent_tax_sta", 1);
	ssc_data_set_number(data, "ibi_uti_percent_deprbas_fed", 0);
	ssc_data_set_number(data, "ibi_uti_percent_deprbas_sta", 0);
	ssc_data_set_number(data, "ibi_oth_percent", 0);
	ssc_data_set_number(data, "ibi_oth_percent_maxvalue", 9.9999996802856925e+37);
	ssc_data_set_number(data, "ibi_oth_percent_tax_fed", 1);
	ssc_data_set_number(data, "ibi_oth_percent_tax_sta", 1);
	ssc_data_set_number(data, "ibi_oth_percent_deprbas_fed", 0);
	ssc_data_set_number(data, "ibi_oth_percent_deprbas_sta", 0);
	ssc_data_set_number(data, "cbi_fed_amount", 0);
	ssc_data_set_number(data, "cbi_fed_maxvalue", 9.9999996802856925e+37);
	ssc_data_set_number(data, "cbi_fed_tax_fed", 1);
	ssc_data_set_number(data, "cbi_fed_tax_sta", 1);
	ssc_data_set_number(data, "cbi_fed_deprbas_fed", 0);
	ssc_data_set_number(data, "cbi_fed_deprbas_sta", 0);
	ssc_data_set_number(data, "cbi_sta_amount", 0);
	ssc_data_set_number(data, "cbi_sta_maxvalue", 9.9999996802856925e+37);
	ssc_data_set_number(data, "cbi_sta_tax_fed", 1);
	ssc_data_set_number(data, "cbi_sta_tax_sta", 1);
	ssc_data_set_number(data, "cbi_sta_deprbas_fed", 0);
	ssc_data_set_number(data, "cbi_sta_deprbas_sta", 0);
	ssc_data_set_number(data, "cbi_uti_amount", 0);
	ssc_data_set_number(data, "cbi_uti_maxvalue", 9.9999996802856925e+37);
	ssc_data_set_number(data, "cbi_uti_tax_fed", 1);
	ssc_data_set_number(data, "cbi_uti_tax_sta", 1);
	ssc_data_set_number(data, "cbi_uti_deprbas_fed", 0);
	ssc_data_set_number(data, "cbi_uti_deprbas_sta", 0);
	ssc_data_set_number(data, "cbi_oth_amount", 0);
	ssc_data_set_number(data, "cbi_oth_maxvalue", 9.9999996802856925e+37);
	ssc_data_set_number(data, "cbi_oth_tax_fed", 1);
	ssc_data_set_number(data, "cbi_oth_tax_sta", 1);
	ssc_data_set_number(data, "cbi_oth_deprbas_fed", 0);
	ssc_data_set_number(data, "cbi_oth_deprbas_sta", 0);
	ssc_number_t p_pbi_fed_amount[1] = { 0 };
	ssc_data_set_array(data, "pbi_fed_amount", p_pbi_fed_amount, 1);
	ssc_data_set_number(data, "pbi_fed_term", 0);
	ssc_data_set_number(data, "pbi_fed_escal", 0);
	ssc_data_set_number(data, "pbi_fed_tax_fed", 1);
	ssc_data_set_number(data, "pbi_fed_tax_sta", 1);
	ssc_number_t p_pbi_sta_amount[1] = { 0 };
	ssc_data_set_array(data, "pbi_sta_amount", p_pbi_sta_amount, 1);
	ssc_data_set_number(data, "pbi_sta_term", 0);
	ssc_data_set_number(data, "pbi_sta_escal", 0);
	ssc_data_set_number(data, "pbi_sta_tax_fed", 1);
	ssc_data_set_number(data, "pbi_sta_tax_sta", 1);
	ssc_number_t p_pbi_uti_amount[1] = { 0 };
	ssc_data_set_array(data, "pbi_uti_amount", p_pbi_uti_amount, 1);
	ssc_data_set_number(data, "pbi_uti_term", 0);
	ssc_data_set_number(data, "pbi_uti_escal", 0);
	ssc_data_set_number(data, "pbi_uti_tax_fed", 1);
	ssc_data_set_number(data, "pbi_uti_tax_sta", 1);
	ssc_number_t p_pbi_oth_amount[1] = { 0 };
	ssc_data_set_array(data, "pbi_oth_amount", p_pbi_oth_amount, 1);
	ssc_data_set_number(data, "pbi_oth_term", 0);
	ssc_data_set_number(data, "pbi_oth_escal", 0);
	ssc_data_set_number(data, "pbi_oth_tax_fed", 1);
	ssc_data_set_number(data, "pbi_oth_tax_sta", 1);
	ssc_number_t p_gen[1] = { 0 };
	ssc_data_set_array(data, "gen", p_gen, 1);
	ssc_number_t p_degradation[1] = { 0 };
	ssc_data_set_array(data, "degradation", p_degradation, 1);
	ssc_data_set_number(data, "loan_moratorium", 0);
	ssc_data_set_number(data, "system_use_recapitalization", 1);
	ssc_data_set_number(data, "system_recapitalization_cost", 18771578);
	ssc_data_set_number(data, "system_use_lifetime_output", 1);
	ssc_data_set_number(data, "ppa_multiplier_model", 0);
	set_array(data, "dispatch_factors_ts", geothermal_dispatch_path, 8760);
    ssc_number_t p_dispatch_tod_factors[9] = { 1, 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_array(data, "dispatch_tod_factors", p_dispatch_tod_factors, 9);
	ssc_number_t p_dispatch_sched_weekday[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_data_set_matrix(data, "dispatch_sched_weekday", p_dispatch_sched_weekday, 12, 24);
	ssc_number_t p_dispatch_sched_weekend[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_data_set_matrix(data, "dispatch_sched_weekend", p_dispatch_sched_weekend, 12, 24);
	ssc_data_set_number(data, "total_installed_cost", 115551088);
	ssc_data_set_number(data, "reserves_interest", 1.75);
	ssc_data_set_number(data, "equip1_reserve_cost", 0.25);
	ssc_data_set_number(data, "equip1_reserve_freq", 12);
	ssc_data_set_number(data, "equip2_reserve_cost", 0);
	ssc_data_set_number(data, "equip2_reserve_freq", 15);
	ssc_data_set_number(data, "equip3_reserve_cost", 0);
	ssc_data_set_number(data, "equip3_reserve_freq", 3);
	ssc_data_set_number(data, "equip_reserve_depr_sta", 0);
	ssc_data_set_number(data, "equip_reserve_depr_fed", 0);
	ssc_data_set_number(data, "salvage_percentage", 0);
	ssc_data_set_number(data, "ppa_soln_mode", 1);
	ssc_number_t p_ppa_price_input[1] = { 0 };
	ssc_data_set_array(data, "ppa_price_input", p_ppa_price_input, 1);
	ssc_data_set_number(data, "cp_capacity_payment_esc", 0);
	ssc_data_set_number(data, "cp_capacity_payment_type", 0);
	ssc_data_set_number(data, "cp_system_nameplate", 0);
	ssc_data_set_number(data, "cp_battery_nameplate", 0);
	ssc_data_set_array(data, "cp_capacity_credit_percent", p_ppa_price_input, 1);
	ssc_data_set_array(data, "cp_capacity_payment_amount", p_ppa_price_input, 1);
	ssc_data_set_number(data, "ppa_escalation", 1);
	ssc_data_set_number(data, "construction_financing_cost", 6701963.5);
	ssc_data_set_number(data, "term_tenor", 18);
	ssc_data_set_number(data, "term_int_rate", 7);
	ssc_data_set_number(data, "dscr", 1.2999999523162842);
	ssc_data_set_number(data, "dscr_reserve_months", 6);
	ssc_data_set_number(data, "debt_percent", 50);
	ssc_data_set_number(data, "debt_option", 1);
	ssc_data_set_number(data, "payment_option", 0);
	ssc_data_set_number(data, "cost_debt_closing", 450000);
	ssc_data_set_number(data, "cost_debt_fee", 2.75);
	ssc_data_set_number(data, "months_working_reserve", 6);
	ssc_data_set_number(data, "months_receivables_reserve", 0);
	ssc_data_set_number(data, "cost_other_financing", 0);
	ssc_data_set_number(data, "flip_target_percent", 15);
	ssc_data_set_number(data, "flip_target_year", 20);
	ssc_data_set_number(data, "depr_alloc_macrs_5_percent", 90);
	ssc_data_set_number(data, "depr_alloc_macrs_15_percent", 1.5);
	ssc_data_set_number(data, "depr_alloc_sl_5_percent", 0);
	ssc_data_set_number(data, "depr_alloc_sl_15_percent", 2.5);
	ssc_data_set_number(data, "depr_alloc_sl_20_percent", 3);
	ssc_data_set_number(data, "depr_alloc_sl_39_percent", 0);
	ssc_data_set_number(data, "depr_alloc_custom_percent", 0);
	ssc_number_t p_depr_custom_schedule[1] = { 0 };
	ssc_data_set_array(data, "depr_custom_schedule", p_depr_custom_schedule, 1);
	ssc_data_set_number(data, "depr_bonus_sta", 0);
	ssc_data_set_number(data, "depr_bonus_sta_macrs_5", 1);
	ssc_data_set_number(data, "depr_bonus_sta_macrs_15", 1);
	ssc_data_set_number(data, "depr_bonus_sta_sl_5", 0);
	ssc_data_set_number(data, "depr_bonus_sta_sl_15", 0);
	ssc_data_set_number(data, "depr_bonus_sta_sl_20", 0);
	ssc_data_set_number(data, "depr_bonus_sta_sl_39", 0);
	ssc_data_set_number(data, "depr_bonus_sta_custom", 0);
	ssc_data_set_number(data, "depr_bonus_fed", 0);
	ssc_data_set_number(data, "depr_bonus_fed_macrs_5", 1);
	ssc_data_set_number(data, "depr_bonus_fed_macrs_15", 1);
	ssc_data_set_number(data, "depr_bonus_fed_sl_5", 0);
	ssc_data_set_number(data, "depr_bonus_fed_sl_15", 0);
	ssc_data_set_number(data, "depr_bonus_fed_sl_20", 0);
	ssc_data_set_number(data, "depr_bonus_fed_sl_39", 0);
	ssc_data_set_number(data, "depr_bonus_fed_custom", 0);
	ssc_data_set_number(data, "depr_itc_sta_macrs_5", 1);
	ssc_data_set_number(data, "depr_itc_sta_macrs_15", 0);
	ssc_data_set_number(data, "depr_itc_sta_sl_5", 0);
	ssc_data_set_number(data, "depr_itc_sta_sl_15", 0);
	ssc_data_set_number(data, "depr_itc_sta_sl_20", 0);
	ssc_data_set_number(data, "depr_itc_sta_sl_39", 0);
	ssc_data_set_number(data, "depr_itc_sta_custom", 0);
	ssc_data_set_number(data, "depr_itc_fed_macrs_5", 1);
	ssc_data_set_number(data, "depr_itc_fed_macrs_15", 0);
	ssc_data_set_number(data, "depr_itc_fed_sl_5", 0);
	ssc_data_set_number(data, "depr_itc_fed_sl_15", 0);
	ssc_data_set_number(data, "depr_itc_fed_sl_20", 0);
	ssc_data_set_number(data, "depr_itc_fed_sl_39", 0);
	ssc_data_set_number(data, "depr_itc_fed_custom", 0);
	ssc_data_set_number(data, "pbi_fed_for_ds", 0);
	ssc_data_set_number(data, "pbi_sta_for_ds", 0);
	ssc_data_set_number(data, "pbi_uti_for_ds", 0);
	ssc_data_set_number(data, "pbi_oth_for_ds", 0);
	ssc_data_set_number(data, "depr_stabas_method", 1);
	ssc_data_set_number(data, "depr_fedbas_method", 1);
}

#endif
