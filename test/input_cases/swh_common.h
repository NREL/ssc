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


#ifndef _SWH_COMMON_DATA_H_
#define	_SWH_COMMON_DATA_H_

#include <stdio.h>
#include "code_generator_utilities.h"

namespace swhtest {
	char solar_resource_file[256];
	char scaled_draw[256];
	char custom_mains[256];
	char custom_set[256];
	char load[256];
	char ur_ts_sell_rate[256];

	int n1 = sprintf(solar_resource_file, "%s/test/input_cases/swh_residential_data/fargo_nd_46.9_-96.8_mts1_60_tmy.csv", SSCDIR);
	int n2 = sprintf(scaled_draw, "%s/test/input_cases/swh_residential_data/scaled_draw.csv", SSCDIR);
	int n3 = sprintf(custom_mains, "%s/test/input_cases/swh_residential_data/custom_mains.csv", SSCDIR);
	int n4 = sprintf(custom_set, "%s/test/input_cases/swh_residential_data/custom_set.csv", SSCDIR);
	int n5 = sprintf(load, "%s/test/input_cases/swh_residential_data/load.csv", SSCDIR);
	int n6 = sprintf(ur_ts_sell_rate, "%s/test/input_cases/swh_residential_data/ur_ts_sell_rate.csv", SSCDIR);

}


void swh_common(ssc_data_t &data) {

	ssc_data_set_string(data, "solar_resource_file", swhtest::solar_resource_file);
	set_array(data, "scaled_draw", swhtest::scaled_draw, 8760);
	ssc_data_set_number(data, "system_capacity", 3.4180600643157959);
	ssc_data_set_number(data, "tilt", 30);
	ssc_data_set_number(data, "azimuth", 180);
	ssc_data_set_number(data, "albedo", 0.20000000298023224);
	ssc_data_set_number(data, "irrad_mode", 0);
	ssc_data_set_number(data, "sky_model", 0);
	ssc_data_set_number(data, "mdot", 0.091055996716022491);
	ssc_data_set_number(data, "ncoll", 2);
	ssc_data_set_number(data, "fluid", 1);
	ssc_data_set_number(data, "area_coll", 2.9800000190734863);
	ssc_data_set_number(data, "FRta", 0.68900001049041748);
	ssc_data_set_number(data, "FRUL", 3.8499999046325684);
	ssc_data_set_number(data, "iam", 0.20000000298023224);
	ssc_data_set_number(data, "test_fluid", 1);
	ssc_data_set_number(data, "test_flow", 0.045527998358011246);
	ssc_data_set_number(data, "pipe_length", 10);
	ssc_data_set_number(data, "pipe_diam", 0.018999999389052391);
	ssc_data_set_number(data, "pipe_k", 0.029999999329447746);
	ssc_data_set_number(data, "pipe_insul", 0.0060000000521540642);
	ssc_data_set_number(data, "tank_h2d_ratio", 2);
	ssc_data_set_number(data, "U_tank", 1);
	ssc_data_set_number(data, "V_tank", 0.30000001192092896);
	ssc_data_set_number(data, "hx_eff", 0.75);
	ssc_data_set_number(data, "T_room", 20);
	ssc_data_set_number(data, "T_tank_max", 99);
	ssc_data_set_number(data, "T_set", 55);
	ssc_data_set_number(data, "pump_power", 45);
	ssc_data_set_number(data, "pump_eff", 0.85000002384185791);
	ssc_data_set_number(data, "use_custom_mains", 0);
	set_array(data, "custom_mains", swhtest::custom_mains, 8760);
	ssc_data_set_number(data, "use_custom_set", 0);
	set_array(data, "custom_set", swhtest::custom_set , 8760);
	
    ssc_data_set_number(data, "adjust_constant", 0.0);

    ssc_data_set_number(data, "en_belpe", 0);
	set_array(data, "load", swhtest::load, 8760);
	ssc_data_set_number(data, "floor_area", 2000);
	ssc_data_set_number(data, "Stories", 2);
	ssc_data_set_number(data, "YrBuilt", 1980);
	ssc_data_set_number(data, "Retrofits", 0);
	ssc_data_set_number(data, "Occupants", 4);
	ssc_number_t p_Occ_Schedule[24] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_data_set_array(data, "Occ_Schedule", p_Occ_Schedule, 24);
	ssc_data_set_number(data, "THeat", 68);
	ssc_data_set_number(data, "TCool", 76);
	ssc_data_set_number(data, "THeatSB", 68);
	ssc_data_set_number(data, "TCoolSB", 76);
	ssc_number_t p_T_Sched[24] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_data_set_array(data, "T_Sched", p_T_Sched, 24);
	ssc_data_set_number(data, "en_heat", 1);
	ssc_data_set_number(data, "en_cool", 1);
	ssc_data_set_number(data, "en_fridge", 1);
	ssc_data_set_number(data, "en_range", 1);
	ssc_data_set_number(data, "en_dish", 1);
	ssc_data_set_number(data, "en_wash", 1);
	ssc_data_set_number(data, "en_dry", 1);
	ssc_data_set_number(data, "en_mels", 1);
	ssc_number_t p_Monthly_util[12] = { 725, 630, 665, 795, 1040, 1590, 1925, 1730, 1380, 1080, 635, 715 };
	ssc_data_set_array(data, "Monthly_util", p_Monthly_util, 12);
	
	ssc_data_set_number(data, "analysis_period", 30);
	ssc_data_set_number(data, "system_use_lifetime_output", 0);
	ssc_data_set_number(data, "inflation_rate", 2.5);
	ssc_number_t p_degradation[1] = { 0 };
	ssc_data_set_array(data, "degradation", p_degradation, 1);
	ssc_number_t p_load_escalation[1] = { 0 };
	ssc_data_set_array(data, "load_escalation", p_load_escalation, 1);
	ssc_number_t p_rate_escalation[1] = { 0 };
	ssc_data_set_array(data, "rate_escalation", p_rate_escalation, 1);
	ssc_data_set_number(data, "ur_metering_option", 0);
	ssc_data_set_number(data, "ur_nm_yearend_sell_rate", 0.019999999552965164);
	ssc_data_set_number(data, "ur_monthly_fixed_charge", 0);
	ssc_data_set_number(data, "ur_monthly_min_charge", 0);
	ssc_data_set_number(data, "ur_annual_min_charge", 0);
	ssc_data_set_number(data, "ur_en_ts_sell_rate", 0);
	set_array(data, "ur_ts_sell_rate", swhtest::ur_ts_sell_rate, 8760);
	ssc_number_t p_ur_ec_sched_weekday[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_data_set_matrix(data, "ur_ec_sched_weekday", p_ur_ec_sched_weekday, 12, 24);
	ssc_number_t p_ur_ec_sched_weekend[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_data_set_matrix(data, "ur_ec_sched_weekend", p_ur_ec_sched_weekend, 12, 24);
	ssc_number_t p_ur_ec_tou_mat[6] = { 1, 1, 9.9999996802856925e+37, 0, 0.11999999731779099, 0.11999999731779099 };
	ssc_data_set_matrix(data, "ur_ec_tou_mat", p_ur_ec_tou_mat, 1, 6);
	ssc_data_set_number(data, "ur_dc_enable", 0);
	ssc_number_t p_ur_dc_sched_weekday[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_data_set_matrix(data, "ur_dc_sched_weekday", p_ur_dc_sched_weekday, 12, 24);
	ssc_number_t p_ur_dc_sched_weekend[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_data_set_matrix(data, "ur_dc_sched_weekend", p_ur_dc_sched_weekend, 12, 24);
	ssc_number_t p_ur_dc_tou_mat[4] = { 1, 1, 0, 0 };
	ssc_data_set_matrix(data, "ur_dc_tou_mat", p_ur_dc_tou_mat, 1, 4);
	ssc_number_t p_ur_dc_flat_mat[48] = { 0, 1, 0, 0, 1, 1, 0, 0, 2, 1, 0, 0, 3, 1, 0, 0, 4, 1, 0, 0, 5, 1, 0, 0, 6, 1, 0, 0, 7, 1, 0, 0, 8, 1, 0, 0, 9, 1, 0, 0, 10, 1, 0, 0, 11, 1, 0, 0 };
	ssc_data_set_matrix(data, "ur_dc_flat_mat", p_ur_dc_flat_mat, 12, 4);
	
	ssc_number_t p_federal_tax_rate[1] = { 15 };
	ssc_data_set_array(data, "federal_tax_rate", p_federal_tax_rate, 1);
	ssc_number_t p_state_tax_rate[1] = { 7 };
	ssc_data_set_array(data, "state_tax_rate", p_state_tax_rate, 1);
	ssc_data_set_number(data, "property_tax_rate", 1);
	ssc_data_set_number(data, "prop_tax_cost_assessed_percent", 100);
	ssc_data_set_number(data, "prop_tax_assessed_decline", 0);
	ssc_data_set_number(data, "real_discount_rate", 6.4000000953674316);
	ssc_data_set_number(data, "insurance_rate", 1);
	ssc_data_set_number(data, "loan_term", 20);
	ssc_data_set_number(data, "loan_rate", 7);
	ssc_data_set_number(data, "debt_fraction", 80);
	ssc_number_t p_om_fixed[1] = { 0 };
	ssc_data_set_array(data, "om_fixed", p_om_fixed, 1);
	ssc_data_set_number(data, "om_fixed_escal", 0);
	ssc_number_t p_om_production[1] = { 0 };
	ssc_data_set_array(data, "om_production", p_om_production, 1);
	ssc_data_set_number(data, "om_production_escal", 0);
	ssc_number_t p_om_capacity[1] = { 50 };
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
	ssc_data_set_number(data, "market", 0);
	ssc_data_set_number(data, "mortgage", 0);
	ssc_data_set_number(data, "total_installed_cost", 8059.7998046875);
	ssc_data_set_number(data, "salvage_percentage", 5);
	
	/*
	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	printf("%s = %.17g\n", "Annual energy saved (year 1)", (double)annual_energy);
	ssc_number_t solar_fraction;
	ssc_data_get_number(data, "solar_fraction", &solar_fraction);
	printf("%s = %.17g\n", "Solar fraction (year 1)", (double)solar_fraction);
	ssc_number_t annual_Q_aux;
	ssc_data_get_number(data, "annual_Q_aux", &annual_Q_aux);
	printf("%s = %.17g\n", "Aux with solar (year 1)", (double)annual_Q_aux);
	ssc_number_t annual_Q_auxonly;
	ssc_data_get_number(data, "annual_Q_auxonly", &annual_Q_auxonly);
	printf("%s = %.17g\n", "Aux without solar (year 1)", (double)annual_Q_auxonly);
	ssc_number_t capacity_factor;
	ssc_data_get_number(data, "capacity_factor", &capacity_factor);
	printf("%s = %.17g\n", "Capacity factor (year 1)", (double)capacity_factor);
	ssc_number_t lcoe_nom;
	ssc_data_get_number(data, "lcoe_nom", &lcoe_nom);
	printf("%s = %.17g\n", "Levelized COE (nominal)", (double)lcoe_nom);
	ssc_number_t lcoe_real;
	ssc_data_get_number(data, "lcoe_real", &lcoe_real);
	printf("%s = %.17g\n", "Levelized COE (real)", (double)lcoe_real);
	ssc_number_t utility_bill_wo_sys_year1;
	ssc_data_get_number(data, "utility_bill_wo_sys_year1", &utility_bill_wo_sys_year1);
	printf("%s = %.17g\n", "Electricity bill without system (year 1)", (double)utility_bill_wo_sys_year1);
	ssc_number_t utility_bill_w_sys_year1;
	ssc_data_get_number(data, "utility_bill_w_sys_year1", &utility_bill_w_sys_year1);
	printf("%s = %.17g\n", "Electricity bill with system (year 1)", (double)utility_bill_w_sys_year1);
	ssc_number_t savings_year1;
	ssc_data_get_number(data, "savings_year1", &savings_year1);
	printf("%s = %.17g\n", "Net savings with system (year 1)", (double)savings_year1);
	ssc_number_t npv;
	ssc_data_get_number(data, "npv", &npv);
	printf("%s = %.17g\n", "Net present value", (double)npv);
	ssc_number_t payback;
	ssc_data_get_number(data, "payback", &payback);
	printf("%s = %.17g\n", "Simple payback period", (double)payback);
	ssc_number_t discounted_payback;
	ssc_data_get_number(data, "discounted_payback", &discounted_payback);
	printf("%s = %.17g\n", "Discounted payback period", (double)discounted_payback);
	ssc_number_t adjusted_installed_cost;
	ssc_data_get_number(data, "adjusted_installed_cost", &adjusted_installed_cost);
	printf("%s = %.17g\n", "Net capital cost", (double)adjusted_installed_cost);
	ssc_number_t first_cost;
	ssc_data_get_number(data, "first_cost", &first_cost);
	printf("%s = %.17g\n", "Equity", (double)first_cost);
	ssc_number_t loan_amount;
	ssc_data_get_number(data, "loan_amount", &loan_amount);
	printf("%s = %.17g\n", "Debt", (double)loan_amount);
	*/
}

#endif
