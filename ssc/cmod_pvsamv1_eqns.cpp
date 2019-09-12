#include "cmod_pvsamv1_eqns.h"

#include "vartab.h"

void map_input(var_table* vt, const std::string& sam_name, var_table* reopt_table, const std::string& reopt_name,
        bool sum = false, bool to_ratio = false){
    double sam_input;
    vt_get_double(vt, sam_name, &sam_input);
    if (var_data* vd = reopt_table->lookup(reopt_name)){
        if (sum){
            if (to_ratio)
                sam_input /= 100.;
            vd->num = vd->num + sam_input;
        }
        else
            throw std::runtime_error(reopt_name + " variable already exists in 'reopt_table'.");
    }
    else{
        if (to_ratio)
            reopt_table->assign(reopt_name, sam_input/100.);
        else
            reopt_table->assign(reopt_name, sam_input);
    }
}

SSCEXPORT void Reopt_size_battery_params(ssc_data_t data) {
    auto vt = static_cast<var_table*>(data);
    if (!vt){
        throw std::runtime_error("ssc_data_t data invalid");
    }
    std::string log;
    auto reopt_params = var_data();
    reopt_params.type = SSC_TABLE;
    var_table* reopt_table = &reopt_params.table;
    var_table reopt_scenario, reopt_site, reopt_electric, reopt_utility, reopt_load, reopt_fin, reopt_pv, reopt_batt,
        reopt_wind;
    reopt_wind.assign("max_kw", 0);

    // site lat and lon
    map_input(vt, "lat", &reopt_site, "latitude");
    map_input(vt, "lon", &reopt_site, "longitude");

    //
    // convert required pvsamv1 + battery inputs
    //
    int opt1, opt2;
    vt_get_int(vt, "subarray1_track_mode", &opt1);
    vt_get_int(vt, "subarray1_backtrack", &opt2);
    if (opt1 == 2 && opt2 == 1)
        opt1 = 3;
    std::vector<int> opt_map = {0, 0, 2, 3, 4};
    reopt_pv.assign("array_type", opt_map[opt1]);

    reopt_pv.assign("module_type", 1);
    map_input(vt, "subarray1_azimuth", &reopt_pv, "azimuth");
    map_input(vt, "subarray1_tilt", &reopt_pv, "tilt");
    map_input(vt, "dc_degradation", &reopt_pv, "degradation_pct", false, true);
    map_input(vt, "subarray1_gcr", &reopt_pv, "gcr");

    // use existing pv system from SAM, not allowing additional PV
    map_input(vt, "system_capacity", &reopt_pv, "existing_kw");
    map_input(vt, "system_capacity", &reopt_pv, "max_kw");

    // Get appropriate inverter efficiency input and transform to ratio from percent
    std::vector<std::string> inv_eff_names = {"inv_snl_eff_cec", "inv_ds_eff", "inv_pd_eff", "inv_cec_cg_eff"};
    double eff;
    vt_get_int(vt, "inverter_model", &opt1);
    if (opt1 == 4)
        throw std::runtime_error("Inverter Mermoud Lejeune Model not supported.");
    vt_get_double(vt, inv_eff_names[opt1], &eff);
    eff /= 100.;
    reopt_pv.assign("inv_eff", eff);
    reopt_batt.assign("inverter_efficiency_pct", eff);

    // calculate the dc ac ratio
    double val1, val2, system_cap;
    std::vector<std::string> inv_power_names = { "inv_snl_paco", "inv_ds_paco", "inv_pd_paco", "inv_cec_cg_paco"};
    vt_get_double(vt, inv_power_names[opt1], &val1);
    vt_get_double(vt, "inverter_count", &val2);
    vt_get_double(vt, "system_capacity", &system_cap);
    reopt_pv.assign("dc_ac_ratio", system_cap * 1000. / (val2 * val1) );

    map_input(vt, "losses", &reopt_pv, "losses", false, true);

    // financial inputs
    map_input(vt, "itc_fed_percent", &reopt_pv, "federal_itc_pct", false, true);
    map_input(vt, "pbi_fed_amount", &reopt_pv, "pbi_us_dollars_per_kwh");
    map_input(vt, "pbi_fed_term", &reopt_pv, "pbi_years");
    auto vd = reopt_pv.lookup("pbi_years");
    if (vd->num[0] < 1) vd->num[0] = 1;    // minimum is 1, set pbi_fed_amount to 0 to disable

    map_input(vt, "ibi_sta_percent", &reopt_pv, "state_ibi_pct", false, true);
    map_input(vt, "ibi_sta_percent_maxvalue", &reopt_pv, "state_ibi_max_us_dollars");
    vd = reopt_pv.lookup("state_ibi_max_us_dollars");
    if (vd->num[0] > 10000000000) vd->num[0] = 10000000000;

    map_input(vt, "ibi_uti_percent", &reopt_pv, "utility_ibi_pct", false, true);
    map_input(vt, "ibi_uti_percent_maxvalue", &reopt_pv, "utility_ibi_max_us_dollars");
    vd = reopt_pv.lookup("utility_ibi_max_us_dollars");
    if (vd->num[0] > 10000000000) vd->num[0] = 10000000000;

    vt_get_double(vt, "om_fixed", &val1);
    vt_get_double(vt, "om_production", &val2);
    reopt_pv.assign("om_cost_us_dollars_per_kw", (val1 / system_cap) + val2);

    map_input(vt, "total_installed_cost", &reopt_pv, "installed_cost_us_dollars_per_kw");
    reopt_pv.lookup("installed_cost_us_dollars_per_kw")->num[0] /= system_cap;

    vd = vt->lookup("depr_bonus_fed");
    if (vd){
        map_input(vt, "depr_bonus_fed", &reopt_pv, "macrs_bonus_pct", false, true);
        map_input(vt, "depr_bonus_fed", &reopt_batt, "macrs_bonus_pct", false, true);
    }
    vd = vt->lookup("depr_bonus_fed_macrs_5");
    if (vd && vd->num[0] == 1){
        reopt_pv.assign("macrs_option_years", 5);
        reopt_batt.assign("macrs_option_years", 5);
    }

    // ReOpt's internal_efficient_pct = SAM's (batt_dc_ac_efficiency + batt_ac_dc_efficiency)/2
    map_input(vt, "batt_dc_ac_efficiency", &reopt_batt, "internal_efficiency_pct");
    map_input(vt, "batt_ac_dc_efficiency", &reopt_batt, "internal_efficiency_pct", true);
    reopt_batt.lookup("internal_efficiency_pct")->num[0] /= 200.;

    map_input(vt, "battery_per_kW", &reopt_batt, "installed_cost_us_dollars_per_kw");
    map_input(vt, "battery_per_kWh", &reopt_batt, "installed_cost_us_dollars_per_kwh");
    vd = vt->lookup("batt_replacement_cost");
    if (vd) reopt_batt.assign("replace_cost_us_dollars_per_kwh", vd->num);
    map_input(vt, "om_replacement_cost1", &reopt_batt, "replace_cost_us_dollars_per_kwh");
    map_input(vt, "batt_initial_SOC", &reopt_batt, "soc_init_pct", false, true);
    map_input(vt, "batt_minimum_SOC", &reopt_batt, "soc_min_pct", false, true);

    // ReOpt's battery replacement single year versus SAM's array schedule
    std::vector<double> vec;
    VT_GET_ARRAY_VEC(vt, "batt_replacement_schedule", vec);
    if (vec.size() > 1)
        log += "Warning: only first value of 'batt_replacement_schedule' array is used for the ReOpt input 'battery_replacement_year'.\n";
    reopt_batt.assign("battery_replacement_year", vec[0]);

    //
    // convert required utilityrate5 inputs
    //
    map_input(vt, "ur_monthly_fixed_charge", &reopt_utility, "fixedmonthlycharge");

    // schedule matrices are numbered starting with 1 for sam but 0 for reopt
    std::vector<std::string> sam_sched_names = {"ur_dc_sched_weekday", "ur_dc_sched_weekend", "ur_ec_sched_weekday",
                                                "ur_ec_sched_weekend"};
    std::vector<std::string> reopt_sched_names = {"demandweekdayschedule", "demandweekendschedule",
                                                  "energyweekdayschedule", "energyweekendschedule"};
    util::matrix_t<double>* mat;
    int demand_n_tiers = 0, energy_n_tiers = 0;
    for (size_t n = 0; n < sam_sched_names.size(); n++){
        VT_GET_MATRIX(vt, sam_sched_names[n], mat);
        for (size_t i = 0; i < mat->nrows(); i++){
            for (size_t j = 0; j < mat->ncols(); j++ ) {
                mat->at(i, j) -= 1;
                if (n < 2)
                    demand_n_tiers = mat->at(i, j) > demand_n_tiers ? (int)mat->at(i, j) : demand_n_tiers;
                else
                    energy_n_tiers = mat->at(i, j) > energy_n_tiers ? (int)mat->at(i, j) : energy_n_tiers;
            }
        }
        reopt_utility.assign(reopt_sched_names[n], var_data(*mat));
    }

    // rate structures in sam are 2d arrays but are list of list of tables in reopt
    std::vector<std::vector<var_data>> vd_mat;
    VT_GET_MATRIX(vt, "ur_dc_tou_mat", mat);
    if (mat->nrows() < (size_t)demand_n_tiers){
        throw std::runtime_error("Demand rate structure should have " + std::to_string(demand_n_tiers) + " tiers to match the provided schedule.");
    }
    for (size_t i = 0; i < mat->nrows(); i++){
        std::vector<var_data> vd_vec;
        double rate = mat->row(i)[3];
        var_data rate_data;
        rate_data.type = SSC_TABLE;
        rate_data.table.assign("rate", rate);
        vd_vec.push_back(rate_data);
        vd_mat.push_back(vd_vec);
    }
    reopt_utility.assign("demandratestructure", vd_mat);
    vd_mat.clear();

    VT_GET_MATRIX(vt, "ur_ec_tou_mat", mat);
    if (mat->nrows() < (size_t)energy_n_tiers){
        throw std::runtime_error("Energy rate structure should have " + std::to_string(demand_n_tiers) + " tiers to match the provided schedule.");
    }
    for (size_t i = 0; i < mat->nrows(); i++) {
        std::vector<var_data> vd_vec;
        double rate = mat->row(i)[4];
        var_data rate_data;
        rate_data.type = SSC_TABLE;
        rate_data.table.assign("rate", rate);
        rate_data.table.assign("unit", var_data("kWh"));
        vd_vec.push_back(rate_data);
        vd_mat.push_back(vd_vec);
    }
    reopt_utility.assign("energyratestructure", vd_mat);
    vd_mat.clear();

    ssc_number_t flatdemandmonths[12] = {0};
    reopt_utility.assign("flatdemandmonths", var_data(flatdemandmonths, 12));

    VT_GET_MATRIX(vt, "ur_dc_flat_mat", mat);
    for (size_t i = 0; i < mat->nrows(); i++){
        std::vector<var_data> vd_vec;
        double rate = mat->row(i)[3];
        var_data rate_data;
        rate_data.type = SSC_TABLE;
        rate_data.table.assign("rate", rate);
        vd_vec.push_back(rate_data);
        vd_mat.push_back(vd_vec);
    }
    reopt_utility.assign("flatdemandstructure", vd_mat);
    vd_mat.clear();

    //
    // convert financial inputs and set variables not modeled by SAM to 0
    //
    map_input(vt, "analysis_period", &reopt_fin, "analysis_years");
    map_input(vt, "federal_tax_rate", &reopt_fin, "offtaker_tax_pct", false, true);
    map_input(vt, "state_tax_rate", &reopt_fin, "offtaker_tax_pct", true, true);
    map_input(vt, "rate_escalation", &reopt_fin, "escalation_pct");
	map_input(vt, "value_of_lost_load", &reopt_fin, "value_of_lost_load_us_dollars_per_kwh");
	reopt_fin.assign("microgrid_upgrade_cost_pct", 0);

    vt_get_double(vt, "inflation_rate", &val1);
    vt_get_double(vt, "real_discount_rate", &val2);
    reopt_fin.assign("offtaker_discount_pct", (1 + val1/100.)*(1 + val2/100.) - 1);

    vt_get_double(vt, "om_fixed_escal", &val1);
    vt_get_double(vt, "om_production_escal", &val2);
    reopt_fin.assign("om_cost_escalation_pct", (val1 / system_cap) + val2/100.);

    // convert load profile inputs, which are not net loads
    VT_GET_ARRAY_VEC(vt, "load_user_data", vec);
    if (vec.size() != 8760){
        throw std::runtime_error("Load profile must have 8760 entries.");
    }
    reopt_load.assign("loads_kw", var_data(&vec[0], 8760));
    reopt_load.assign("loads_kw_is_net", false);

    VT_GET_ARRAY_VEC(vt, "crit_load_user_data", vec);
    if (vec.size() != 8760){
        throw std::runtime_error("Critical load profile must have 8760 entries.");
    }
    else{
        reopt_load.assign("critical_load_pct", 0.0);
    }
    reopt_load.assign("critical_loads_kw", var_data(&vec[0], 8760));

    // assign the reopt parameter table and log messages
    reopt_electric.assign_match_case("urdb_response", reopt_utility);
    reopt_site.assign_match_case("ElectricTariff", reopt_electric);
    reopt_site.assign_match_case("LoadProfile", reopt_load);
    reopt_site.assign_match_case("Financial", reopt_fin);
    reopt_site.assign_match_case("Storage", reopt_batt);
    reopt_site.assign_match_case("Wind", reopt_wind);
    reopt_site.assign_match_case("PV", reopt_pv);
    reopt_scenario.assign_match_case("Site", reopt_site);
    reopt_table->assign_match_case("Scenario", reopt_scenario);
    vt->assign_match_case("reopt_scenario", reopt_params);
    vt->assign_match_case("log", log);
}