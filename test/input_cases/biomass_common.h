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



#ifndef _BIOMASS_COMMON_DATA_H_
#define _BIOMASS_COMMON_DATA_H_

#include <stdio.h>
#include "code_generator_utilities.h"

namespace biomass_test {
	char file_name[256];
	int n1 = sprintf(file_name, "%s/test/input_cases/swh_residential_data/fargo_nd_46.9_-96.8_mts1_60_tmy.csv", SSCDIR);
	char dispatch_factors[256];
    int n2 = sprintf(dispatch_factors, "%s/test/input_cases/custom_generation_data/dispatch_factors_ts.csv", SSCDIR);

}

void biomass_commondata(ssc_data_t &data) {
	ssc_data_set_string(data, "file_name", biomass_test::file_name);
	ssc_data_set_number(data, "system_capacity", 48535.58984375);
	ssc_data_set_number(data, "biopwr.feedstock.total", 320112.59375);
	ssc_data_set_number(data, "biopwr.feedstock.total_biomass", 320112.59375);
	ssc_data_set_number(data, "biopwr.feedstock.total_moisture", 0.36564341187477112);
	ssc_data_set_number(data, "biopwr.feedstock.total_coal", 0);
	ssc_data_set_number(data, "biopwr.feedstock.total_lhv", 7194.0302734375);
	ssc_data_set_number(data, "biopwr.feedstock.total_hhv", 7668.6982421875);
	ssc_data_set_number(data, "biopwr.feedstock.total_c", 0.42365738749504089);
	ssc_data_set_number(data, "biopwr.feedstock.total_biomass_c", 0.42365738749504089);
	ssc_data_set_number(data, "biopwr.feedstock.total_h", 0.053688719868659973);
	ssc_data_set_number(data, "biopwr.feedstock.bagasse_frac", 0);
	ssc_data_set_number(data, "biopwr.feedstock.barley_frac", 0.032862812280654907);
	ssc_data_set_number(data, "biopwr.feedstock.stover_frac", 0.59452515840530396);
	ssc_data_set_number(data, "biopwr.feedstock.rice_frac", 0);
	ssc_data_set_number(data, "biopwr.feedstock.wheat_frac", 0.2752515971660614);
	ssc_data_set_number(data, "biopwr.feedstock.forest_frac", 0.076395928859710693);
	ssc_data_set_number(data, "biopwr.feedstock.mill_frac", 0.0052990731783211231);
	ssc_data_set_number(data, "biopwr.feedstock.mill_c", 0.48759999871253967);
	ssc_data_set_number(data, "biopwr.feedstock.urban_frac", 0.015665424987673759);
	ssc_data_set_number(data, "biopwr.feedstock.urban_c", 0.49050000309944153);
	ssc_data_set_number(data, "biopwr.feedstock.woody_frac", 0);
	ssc_data_set_number(data, "biopwr.feedstock.woody_c", 0.49930000305175781);
	ssc_data_set_number(data, "biopwr.feedstock.herb_frac", 0);
	ssc_data_set_number(data, "biopwr.feedstock.herb_c", 0.47760000824928284);
	ssc_data_set_number(data, "biopwr.feedstock.additional_opt", 0);
	ssc_data_set_number(data, "biopwr.feedstock.feedstock1_resource", 0);
	ssc_data_set_number(data, "biopwr.feedstock.feedstock2_resource", 0);
	ssc_data_set_number(data, "biopwr.feedstock.feedstock1_c", 0);
	ssc_data_set_number(data, "biopwr.feedstock.feedstock2_c", 0);
	ssc_data_set_number(data, "biopwr.feedstock.feedstock1_h", 0);
	ssc_data_set_number(data, "biopwr.feedstock.feedstock2_h", 0);
	ssc_data_set_number(data, "biopwr.feedstock.feedstock1_hhv", 0);
	ssc_data_set_number(data, "biopwr.feedstock.feedstock2_hhv", 0);
	ssc_data_set_number(data, "biopwr.feedstock.feedstock1_frac", 0);
	ssc_data_set_number(data, "biopwr.feedstock.feedstock2_frac", 0);
	ssc_data_set_number(data, "biopwr.feedstock.bit_frac", 0);
	ssc_data_set_number(data, "biopwr.feedstock.subbit_frac", 0);
	ssc_data_set_number(data, "biopwr.feedstock.lig_frac", 0);
	ssc_data_set_number(data, "biopwr.feedstock.bagasse_moisture", 100);
	ssc_data_set_number(data, "biopwr.feedstock.barley_moisture", 19.047618865966797);
	ssc_data_set_number(data, "biopwr.feedstock.stover_moisture", 42.857143402099609);
	ssc_data_set_number(data, "biopwr.feedstock.rice_moisture", 203.03030395507813);
	ssc_data_set_number(data, "biopwr.feedstock.wheat_moisture", 13.636363983154297);
	ssc_data_set_number(data, "biopwr.feedstock.forest_moisture", 78.571426391601563);
	ssc_data_set_number(data, "biopwr.feedstock.mill_moisture", 92.307693481445313);
	ssc_data_set_number(data, "biopwr.feedstock.urban_moisture", 13.636363983154297);
	ssc_data_set_number(data, "biopwr.feedstock.woody_moisture", 33.333332061767578);
	ssc_data_set_number(data, "biopwr.feedstock.herb_moisture", 19.047618865966797);
	ssc_data_set_number(data, "biopwr.feedstock.feedstock1_moisture", 0);
	ssc_data_set_number(data, "biopwr.feedstock.feedstock2_moisture", 0);
	ssc_data_set_number(data, "biopwr.feedstock.bit_moisture", 11.111110687255859);
	ssc_data_set_number(data, "biopwr.feedstock.subbit_moisture", 33.333332061767578);
	ssc_data_set_number(data, "biopwr.feedstock.lig_moisture", 63.934425354003906);
	ssc_data_set_number(data, "biopwr.feedstock.collection_radius", 75);
	ssc_data_set_number(data, "biopwr.emissions.avoided_cred", 1);
	ssc_data_set_number(data, "biopwr.emissions.collection_fuel", 0);
	ssc_data_set_number(data, "biopwr.emissions.transport_fuel", 0);
	ssc_data_set_number(data, "biopwr.emissions.transport_legs", 0);
	ssc_data_set_number(data, "biopwr.emissions.transport_predist", 0);
	ssc_data_set_number(data, "biopwr.emissions.transport_long", 0);
	ssc_data_set_number(data, "biopwr.emissions.transport_longmiles", 0);
	ssc_data_set_number(data, "biopwr.emissions.transport_longopt", 0);
	ssc_data_set_number(data, "biopwr.emissions.pre_chipopt", 1);
	ssc_data_set_number(data, "biopwr.emissions.pre_grindopt", 1);
	ssc_data_set_number(data, "biopwr.emissions.pre_pelletopt", 0);
	ssc_data_set_number(data, "biopwr.emissions.grid_intensity", 643);
	ssc_data_set_number(data, "biopwr.plant.drying_method", 0);
	ssc_data_set_number(data, "biopwr.plant.drying_spec", 11.111110687255859);
	ssc_data_set_number(data, "biopwr.plant.combustor_type", 0);
	ssc_data_set_number(data, "biopwr.plant.boiler.air_feed", 11.196078300476074);
	ssc_data_set_number(data, "biopwr.plant.boiler.flue_temp", 390);
	ssc_data_set_number(data, "biopwr.plant.boiler.steam_enthalpy", 1452.199951171875);
	ssc_data_set_number(data, "biopwr.plant.boiler.num", 2);
	ssc_data_set_number(data, "biopwr.plant.boiler.cap_per_boiler", 162915.6875);
	ssc_data_set_number(data, "biopwr.plant.nameplate", 48535.58984375);
	ssc_data_set_number(data, "biopwr.plant.rated_eff", 0.34999999403953552);
	ssc_data_set_number(data, "biopwr.plant.min_load", 0.25);
	ssc_data_set_number(data, "biopwr.plant.max_over_design", 1.1000000238418579);
	ssc_data_set_number(data, "biopwr.plant.boiler.over_design", 10);
	ssc_data_set_number(data, "biopwr.plant.cycle_design_temp", 77);
	ssc_data_set_number(data, "biopwr.plant.pl_eff_f0", 0.89999997615814209);
	ssc_data_set_number(data, "biopwr.plant.pl_eff_f1", 0.10000000149011612);
	ssc_data_set_number(data, "biopwr.plant.pl_eff_f2", 0);
	ssc_data_set_number(data, "biopwr.plant.pl_eff_f3", 0);
	ssc_data_set_number(data, "biopwr.plant.pl_eff_f4", 0);
	ssc_data_set_number(data, "biopwr.plant.temp_eff_f0", 1);
	ssc_data_set_number(data, "biopwr.plant.temp_eff_f1", -0.0020000000949949026);
	ssc_data_set_number(data, "biopwr.plant.temp_eff_f2", 0);
	ssc_data_set_number(data, "biopwr.plant.temp_eff_f3", 0);
	ssc_data_set_number(data, "biopwr.plant.temp_eff_f4", 0);
	ssc_data_set_number(data, "biopwr.plant.temp_corr_mode", 0);
	ssc_data_set_number(data, "biopwr.plant.par_percent", 6);
	ssc_data_set_number(data, "biopwr.plant.tou_option", 0);
	ssc_number_t p_biopwr_plant_disp_power[9] = { 0.69999998807907104, 1.1000000238418579, 0, 0, 0, 0, 0, 0, 0 };
	ssc_data_set_array(data, "biopwr.plant.disp.power", p_biopwr_plant_disp_power, 9);
	ssc_data_set_number(data, "biopwr.plant.ramp_rate", 0);
	ssc_data_set_string(data, "biopwr.plant.tou_grid", "111111111112222222222222111111111112222222222222111111111112222222222222111111111112222222222222111111111112222222222222111111111112222222222222111111111112222222222222111111111112222222222222111111111112222222222222111111111112222222222222111111111112222222222222111111111112222222222222");
	ssc_data_set_number(data, "biopwr.plant.boiler.steam_pressure", 900);

    ssc_data_set_number(data, "adjust_constant", 0.0);

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
	ssc_number_t p_om_fixed[1] = { 0 };
	ssc_data_set_array(data, "om_fixed", p_om_fixed, 1);
	ssc_data_set_number(data, "om_fixed_escal", 0);
	ssc_number_t p_om_production[1] = { 4 };
	ssc_data_set_array(data, "om_production", p_om_production, 1);
	ssc_data_set_number(data, "om_production_escal", 0);
	ssc_number_t p_om_capacity[1] = { 110 };
	ssc_data_set_array(data, "om_capacity", p_om_capacity, 1);
	ssc_data_set_number(data, "om_capacity_escal", 0);
	ssc_number_t p_om_fuel_cost[1] = { 0 };
	ssc_data_set_array(data, "om_fuel_cost", p_om_fuel_cost, 1);
	ssc_data_set_number(data, "om_fuel_cost_escal", 0);
	ssc_data_set_number(data, "om_opt_fuel_1_usage", 320112.59375);
	ssc_number_t p_om_opt_fuel_1_cost[1] = { 54.028213500976563 };
	ssc_data_set_array(data, "om_opt_fuel_1_cost", p_om_opt_fuel_1_cost, 1);
	ssc_data_set_number(data, "om_opt_fuel_1_cost_escal", 0);
	ssc_data_set_number(data, "om_opt_fuel_2_usage", 0);
	ssc_number_t p_om_opt_fuel_2_cost[1] = { 0 };
	ssc_data_set_array(data, "om_opt_fuel_2_cost", p_om_opt_fuel_2_cost, 1);
	ssc_data_set_number(data, "om_opt_fuel_2_cost_escal", 0);
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
	ssc_number_t p_degradation[1] = { 0 };
	ssc_data_set_array(data, "degradation", p_degradation, 1);
	ssc_data_set_number(data, "loan_moratorium", 0);
	ssc_data_set_number(data, "system_use_recapitalization", 0);
	ssc_data_set_number(data, "system_use_lifetime_output", 0);
	ssc_data_set_number(data, "ppa_multiplier_model", 0);
	set_array(data, "dispatch_factors_ts", biomass_test::dispatch_factors, 8760);
    ssc_number_t p_dispatch_tod_factors[9] = { 1, 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_array(data, "dispatch_tod_factors", p_dispatch_tod_factors, 9);
	ssc_number_t p_dispatch_sched_weekday[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_data_set_matrix(data, "dispatch_sched_weekday", p_dispatch_sched_weekday, 12, 24);
	ssc_number_t p_dispatch_sched_weekend[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_data_set_matrix(data, "dispatch_sched_weekend", p_dispatch_sched_weekend, 12, 24);
	ssc_data_set_number(data, "total_installed_cost", 182285120);
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
	ssc_data_set_number(data, "ppa_soln_mode", 0);
	ssc_number_t p_ppa_price_input[1] = { 0 };
	ssc_data_set_array(data, "ppa_price_input", p_ppa_price_input, 1);
	ssc_data_set_number(data, "cp_capacity_payment_esc", 0);
	ssc_data_set_number(data, "cp_capacity_payment_type", 0);
	ssc_data_set_number(data, "cp_system_nameplate", 0);
	ssc_data_set_number(data, "cp_battery_nameplate", 0);
	ssc_data_set_array(data, "cp_capacity_credit_percent", p_ppa_price_input, 1);
	ssc_data_set_array(data, "cp_capacity_payment_amount", p_ppa_price_input, 1);
	ssc_data_set_number(data, "ppa_escalation", 1);
	ssc_data_set_number(data, "construction_financing_cost", 3645702.5);
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
	ssc_data_set_number(data, "flip_target_percent", 11);
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
