#include "cmod_csp_tower_eqns.h"
#include "cmod_csp_common_eqns.h"
#include "vartab.h"

#pragma warning(disable: 4297)  // ignore warning: 'function assumed not to throw an exception but does'

//var_data* var_table::assign(const std::string& name, const var_data& val)
//{
//    var_data* v = lookup(name);
//    if (!v)
//    {
//        v = new var_data;
//        m_hash[util::lower_case(name)] = v;
//    }
//
//    v->copy(val);
//    return v;
//}

//SSCEXPORT ssc_bool_t ssc_data_get_number(ssc_data_t p_data, const char* name, ssc_number_t* value)
//{
//    if (!value) return 0;
//    var_table* vt = static_cast<var_table*>(p_data);
//    if (!vt) return 0;
//    var_data* dat = vt->lookup(name);
//    if (!dat || dat->type != SSC_NUMBER) return 0;
//    *value = dat->num;
//    return 1;
//}

//void vt_get_matrix(var_table* vt, std::string name, util::matrix_t<double>& matrix) {
//    if (var_data* vd = vt->lookup(name)) {
//        if (vd->type == SSC_ARRAY)
//        {
//            std::vector<double> vec_double = vd->arr_vector();
//            matrix.resize(vec_double.size());
//            for (size_t i = 0; i < vec_double.size(); i++)
//                matrix.at(i) = vec_double[i];
//        }
//        else if (vd->type != SSC_MATRIX)
//            throw std::runtime_error(std::string(name) + std::string(" must be matrix type."));
//        matrix = vd->num;
//    }
//    else throw std::runtime_error(std::string(name) + std::string(" must be assigned."));
//}


SSCEXPORT ssc_bool_t ssc_data_t_get_number(ssc_data_t p_data, const char* name, ssc_number_t* value)
{
    bool success = ssc_data_get_number(p_data, name, value);
    if (!success) {
        // replace any periods in the name with underscores in order to read variables set by the UI
        std::string str_name(name);
        size_t n_replaced = util::replace(str_name, ".", "_");
        if (n_replaced > 0) {
            success = ssc_data_get_number(p_data, str_name.c_str(), value);
        }
    }

    return success;
}

void ssc_data_t_get_matrix(var_table* vt, std::string name, util::matrix_t<double>& matrix) {
    try
    {
        vt_get_matrix(vt, name, matrix);
    }
    catch (std::exception& e) {
    }

    // replace any periods in the name with underscores in order to read variables set by the UI
    std::string str_name(name);
    size_t n_replaced = util::replace(str_name, ".", "_");
    if (n_replaced > 0) {
        vt_get_matrix(vt, name, matrix);        // allow exceptions to be uncaught
    }
}

SSCEXPORT void ssc_data_t_set_number(ssc_data_t p_data, const char* name, ssc_number_t value)
{
    ssc_data_set_number(p_data, name, value);

    // replace any periods in the name with underscores so UI equations can read value
    std::string str_name(name);
    size_t n_replaced = util::replace(str_name, ".", "_");
    if (n_replaced > 0) {
        ssc_data_set_number(p_data, str_name.c_str(), value);
    }
}

void MSPT_System_Design_Equations(ssc_data_t data)
{
    //auto vt = static_cast<var_table*>(data);
    //if (!vt) {
    //    throw std::runtime_error("ssc_data_t data invalid");
    //}
    double P_ref, gross_net_conversion_factor, nameplate, design_eff, solarm, q_pb_design, q_rec_des, tshours, tshours_sf;

    // nameplate
    ssc_data_t_get_number(data, "P_ref", &P_ref);
    ssc_data_t_get_number(data, "gross_net_conversion_factor", &gross_net_conversion_factor);
    nameplate = Nameplate(P_ref, gross_net_conversion_factor);
    ssc_data_t_set_number(data, "nameplate", nameplate);

    // q_pb_design
    ssc_data_t_get_number(data, "P_ref", &P_ref);                         // repeat assessors because the values may have changed
    ssc_data_t_get_number(data, "design_eff", &design_eff);
    q_pb_design = Q_pb_design(P_ref, design_eff);
    ssc_data_t_set_number(data, "q_pb_design", q_pb_design);

    // q_rec_des
    ssc_data_t_get_number(data, "solarm", &solarm);
    ssc_data_t_get_number(data, "q_pb_design", &q_pb_design);
    q_rec_des = Q_rec_des(solarm, q_pb_design);
    ssc_data_t_set_number(data, "q_rec_des", q_rec_des);

    // tshours_sf
    ssc_data_t_get_number(data, "tshours", &tshours);
    ssc_data_t_get_number(data, "solarm", &solarm);
    tshours_sf = Tshours_sf(tshours, solarm);
    ssc_data_t_set_number(data, "tshours_sf", tshours_sf);
}

void Tower_SolarPilot_Solar_Field_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }
    double land_max, h_tower, land_max_calc, helio_height, helio_width, dens_mirror, csp_pt_sf_heliostat_area,
        land_min, land_min_calc, csp_pt_sf_fixed_land_area, land_area_base,
        csp_pt_sf_land_overhead_factor, csp_pt_sf_total_land_area, a_sf_ui, helio_area_tot, csp_pt_sf_tower_height,
        c_atm_0, c_atm_1, c_atm_2, c_atm_3, c_atm_info, helio_optical_error_mrad, error_equiv, field_model_type,
        q_rec_des, q_design, dni_des, dni_des_calc, opt_flux_penalty,
        n_hel, override_opt, is_optimize, override_layout, opt_algorithm;

    util::matrix_t<double> helio_positions;

    // land_max_calc
    ssc_data_t_get_number(data, "land_max", &land_max);
    ssc_data_t_get_number(data, "h_tower", &h_tower);
    land_max_calc = Land_max_calc(land_max, h_tower);
    ssc_data_t_set_number(data, "land_max_calc", land_max_calc);

    // n_hel
    ssc_data_t_get_matrix(vt, "helio_positions", helio_positions);
    n_hel = N_hel(helio_positions);
    vt->assign("n_hel", n_hel);

    // csp_pt_sf_heliostat_area
    ssc_data_t_get_number(data, "helio_height", &helio_height);
    ssc_data_t_get_number(data, "helio_width", &helio_width);
    ssc_data_t_get_number(data, "dens_mirror", &dens_mirror);
    csp_pt_sf_heliostat_area = Csp_pt_sf_heliostat_area(helio_height, helio_width, dens_mirror);
    ssc_data_t_set_number(data, "csp.pt.sf.heliostat_area", csp_pt_sf_heliostat_area);
    
    //  This one is not being read in the UI
    //// csp_pt_sf_total_reflective_area
    //double csp_pt_sf_total_reflective_area;
    //ssc_data_t_get_number(data, "n_hel", &n_hel);
    //ssc_data_t_get_number(data, "csp_pt_sf_heliostat_area", &csp_pt_sf_heliostat_area);
    //csp_pt_sf_total_reflective_area = Csp_pt_sf_total_reflective_area(n_hel, csp_pt_sf_heliostat_area);
    //ssc_data_t_set_number(data, "csp_pt_sf_total_reflective_area", csp_pt_sf_total_reflective_area);

    // land_min_calc
    ssc_data_t_get_number(data, "land_min", &land_min);
    ssc_data_t_get_number(data, "h_tower", &h_tower);
    land_min_calc = Land_min_calc(land_min, h_tower);
    ssc_data_t_set_number(data, "land_min_calc", land_min_calc);

    // csp_pt_sf_total_land_area
    ssc_data_t_get_number(data, "csp.pt.sf.fixed_land_area", &csp_pt_sf_fixed_land_area);
    ssc_data_t_get_number(data, "land_area_base", &land_area_base);
    ssc_data_t_get_number(data, "csp.pt.sf.land_overhead_factor", &csp_pt_sf_land_overhead_factor);
    csp_pt_sf_total_land_area = Csp_pt_sf_total_land_area(csp_pt_sf_fixed_land_area, land_area_base, csp_pt_sf_land_overhead_factor);
    ssc_data_t_set_number(data, "csp.pt.sf.total_land_area", csp_pt_sf_total_land_area);

    // a_sf_ui
    ssc_data_t_get_number(data, "helio_width", &helio_width);
    ssc_data_t_get_number(data, "helio_height", &helio_height);
    ssc_data_t_get_number(data, "dens_mirror", &dens_mirror);
    ssc_data_t_get_number(data, "n_hel", &n_hel);
    a_sf_ui = A_sf_UI(helio_width, helio_height, dens_mirror, n_hel);
    ssc_data_t_set_number(data, "a_sf_ui", a_sf_ui);

    // helio_area_tot
    ssc_data_t_get_number(data, "a_sf_ui", &a_sf_ui);
    helio_area_tot = Helio_area_tot(a_sf_ui);
    ssc_data_t_set_number(data, "helio_area_tot", helio_area_tot);
    
    // csp_pt_sf_tower_height
    ssc_data_t_get_number(data, "h_tower", &h_tower);
    csp_pt_sf_tower_height = Csp_pt_sf_tower_height(h_tower);
    ssc_data_t_set_number(data, "csp.pt.sf.tower_height", csp_pt_sf_tower_height);

    // c_atm_info
    //ssc_data_t_get_matrix(vt, "helio_positions", helio_positions);        // THIS IS A PROBLEM: (getting the same ssc_data_t matrix data must also be changing it)
    ssc_data_t_get_number(data, "c_atm_0", &c_atm_0);
    ssc_data_t_get_number(data, "c_atm_1", &c_atm_1);
    ssc_data_t_get_number(data, "c_atm_2", &c_atm_2);
    ssc_data_t_get_number(data, "c_atm_3", &c_atm_3);
    ssc_data_t_get_number(data, "h_tower", &h_tower);
    c_atm_info = C_atm_info(helio_positions, c_atm_0, c_atm_1, c_atm_2, c_atm_3, h_tower);
    ssc_data_t_set_number(data, "c_atm_info", c_atm_info);

    // error_equiv
    ssc_data_t_get_number(data, "helio_optical_error_mrad", &helio_optical_error_mrad);
    error_equiv = Error_equiv(helio_optical_error_mrad);
    ssc_data_t_set_number(data, "error_equiv", error_equiv);

    // is_optimize
    ssc_data_t_get_number(data, "override_opt", &override_opt);
    is_optimize = Is_optimize(override_opt);
    ssc_data_t_set_number(data, "is_optimize", is_optimize);

    // field_model_type
    ssc_data_t_get_number(data, "is_optimize", &is_optimize);
    ssc_data_t_get_number(data, "override_layout", &override_layout);
    field_model_type = Field_model_type(is_optimize, override_layout);
    ssc_data_t_set_number(data, "field_model_type", field_model_type);

    // q_design
    ssc_data_t_get_number(data, "q_rec_des", &q_rec_des);
    q_design = Q_design(q_rec_des);
    ssc_data_t_set_number(data, "q_design", q_design);

    // dni_des_calc
    ssc_data_t_get_number(data, "dni_des", &dni_des);
    dni_des_calc = Dni_des_calc(dni_des);
    ssc_data_t_set_number(data, "dni_des_calc", dni_des_calc);

    // opt_algorithm
    opt_algorithm = Opt_algorithm();
    ssc_data_t_set_number(data, "opt_algorithm", opt_algorithm);

    // opt_flux_penalty
    opt_flux_penalty = Opt_flux_penalty();
    ssc_data_t_set_number(data, "opt_flux_penalty", opt_flux_penalty);
}

void MSPT_Receiver_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }
    double csp_pt_rec_max_oper_frac, q_rec_des, csp_pt_rec_htf_c_avg, t_htf_hot_des, t_htf_cold_des,
        csp_pt_rec_max_flow_to_rec, csp_pt_rec_htf_t_avg, rec_d_spec, csp_pt_rec_cav_ap_hw_ratio, csp_pt_rec_cav_ap_height, d_rec, rec_height, rec_aspect,
        h_tower, piping_length_mult, piping_length_const, piping_length, piping_loss, piping_loss_tot;

    int rec_htf;

    util::matrix_t<double> field_fl_props;

    //  This one is not being read in the UI
    // Csp_pt_rec_cav_lip_height
    //double csp_pt_rec_cav_lip_height;
    //csp_pt_rec_cav_lip_height = Csp_pt_rec_cav_lip_height();
    //vt->assign("csp_pt_rec_cav_lip_height", csp_pt_rec_cav_lip_height);

    //  This one is not being read in the UI
    // csp_pt_rec_cav_panel_height
    //double csp_pt_rec_cav_panel_height;
    //csp_pt_rec_cav_panel_height = Csp_pt_rec_cav_panel_height();
    //vt->assign("csp_pt_rec_cav_panel_height", csp_pt_rec_cav_panel_height);

    // csp_pt_rec_htf_t_avg
    vt_get_number(vt, "t_htf_cold_des", &t_htf_cold_des);
    vt_get_number(vt, "t_htf_hot_des", &t_htf_hot_des);
    csp_pt_rec_htf_t_avg = Csp_pt_rec_htf_t_avg(t_htf_cold_des, t_htf_hot_des);
    vt->assign("csp_pt_rec_htf_t_avg", csp_pt_rec_htf_t_avg);

    // csp_pt_rec_htf_c_avg
    vt_get_number(vt, "csp_pt_rec_htf_t_avg", &csp_pt_rec_htf_t_avg);
    vt_get_int(vt, "rec_htf", &rec_htf);
    vt_get_matrix(vt, "field_fl_props", field_fl_props);
    csp_pt_rec_htf_c_avg = Csp_pt_rec_htf_c_avg(csp_pt_rec_htf_t_avg, rec_htf, field_fl_props);
    vt->assign("csp_pt_rec_htf_c_avg", csp_pt_rec_htf_c_avg);

    // csp_pt_rec_max_flow_to_rec
    vt_get_number(vt, "csp_pt_rec_max_oper_frac", &csp_pt_rec_max_oper_frac);
    vt_get_number(vt, "q_rec_des", &q_rec_des);
    vt_get_number(vt, "csp_pt_rec_htf_c_avg", &csp_pt_rec_htf_c_avg);
    vt_get_number(vt, "t_htf_hot_des", &t_htf_hot_des);
    vt_get_number(vt, "t_htf_cold_des", &t_htf_cold_des);
    csp_pt_rec_max_flow_to_rec = Csp_pt_rec_max_flow_to_rec(csp_pt_rec_max_oper_frac, q_rec_des, csp_pt_rec_htf_c_avg, t_htf_hot_des, t_htf_cold_des);
    vt->assign("csp_pt_rec_max_flow_to_rec", csp_pt_rec_max_flow_to_rec);

    // csp_pt_rec_cav_ap_height
    vt_get_number(vt, "rec_d_spec", &rec_d_spec);
    vt_get_number(vt, "csp_pt_rec_cav_ap_hw_ratio", &csp_pt_rec_cav_ap_hw_ratio);
    csp_pt_rec_cav_ap_height = Csp_pt_rec_cav_ap_height(rec_d_spec, csp_pt_rec_cav_ap_hw_ratio);
    vt->assign("csp_pt_rec_cav_ap_height", csp_pt_rec_cav_ap_height);

    // rec_aspect
    vt_get_number(vt, "d_rec", &d_rec);
    vt_get_number(vt, "rec_height", &rec_height);
    rec_aspect = Rec_aspect(d_rec, rec_height);
    vt->assign("rec_aspect", rec_aspect);

    // piping_length
    vt_get_number(vt, "h_tower", &h_tower);
    vt_get_number(vt, "piping_length_mult", &piping_length_mult);
    vt_get_number(vt, "piping_length_const", &piping_length_const);
    piping_length = Piping_length(h_tower, piping_length_mult, piping_length_const);
    vt->assign("piping_length", piping_length);

    // piping_loss_tot
    vt_get_number(vt, "piping_length", &piping_length);
    vt_get_number(vt, "piping_loss", &piping_loss);
    piping_loss_tot = Piping_loss_tot(piping_length, piping_loss);
    vt->assign("piping_loss_tot", piping_loss_tot);
}

void MSPT_System_Control_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }
    double bop_par, bop_par_f, bop_par_0, bop_par_1, bop_par_2, p_ref, csp_pt_par_calc_bop,
        aux_par, aux_par_f, aux_par_0, aux_par_1, aux_par_2, csp_pt_par_calc_aux,
        disp_wlim_maxspec, constant, disp_wlim_max;

    //double* wlim_series;
    util::matrix_t<double> wlim_series;

    // csp_pt_par_calc_bop
    vt_get_number(vt, "bop_par", &bop_par);
    vt_get_number(vt, "bop_par_f", &bop_par_f);
    vt_get_number(vt, "bop_par_0", &bop_par_0);
    vt_get_number(vt, "bop_par_1", &bop_par_1);
    vt_get_number(vt, "bop_par_2", &bop_par_2);
    vt_get_number(vt, "p_ref", &p_ref);
    csp_pt_par_calc_bop = Csp_pt_par_calc_bop(bop_par, bop_par_f, bop_par_0, bop_par_1, bop_par_2, p_ref);
    vt->assign("csp_pt_par_calc_bop", csp_pt_par_calc_bop);

    // csp_pt_par_calc_aux
    vt_get_number(vt, "aux_par", &aux_par);
    vt_get_number(vt, "aux_par_f", &aux_par_f);
    vt_get_number(vt, "aux_par_0", &aux_par_0);
    vt_get_number(vt, "aux_par_1", &aux_par_1);
    vt_get_number(vt, "aux_par_2", &aux_par_2);
    vt_get_number(vt, "p_ref", &p_ref);
    csp_pt_par_calc_aux = Csp_pt_par_calc_aux(aux_par, aux_par_f, aux_par_0, aux_par_1, aux_par_2, p_ref);
    vt->assign("csp_pt_par_calc_aux", csp_pt_par_calc_aux);

    // disp_wlim_max
    vt_get_number(vt, "disp_wlim_maxspec", &disp_wlim_maxspec);
    vt_get_number(vt, "constant", &constant);
    disp_wlim_max = Disp_wlim_max(disp_wlim_maxspec, constant);
    vt->assign("disp_wlim_max", disp_wlim_max);

    // wlim_series
    vt_get_number(vt, "disp_wlim_max", &disp_wlim_max);
    vt_get_number(vt, "constant", &constant);
    wlim_series = Wlim_series(disp_wlim_max);
    vt->assign("wlim_series", wlim_series);
}

void Tower_SolarPilot_Capital_Costs_MSPT_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    double d_rec, rec_height, receiver_type, rec_d_spec, csp_pt_rec_cav_ap_height, csp_pt_cost_receiver_area,
        p_ref, design_eff, tshours, csp_pt_cost_storage_mwht,
        demand_var, csp_pt_cost_power_block_mwe;

    TowerTypes tower_type = TowerTypes::kMoltenSalt;

    ssc_data_get_number(data, "d_rec", &d_rec);
    ssc_data_get_number(data, "rec_height", &rec_height);
    ssc_data_get_number(data, "receiver_type", &receiver_type);
    ssc_data_get_number(data, "rec_d_spec", &rec_d_spec);
    ssc_data_get_number(data, "csp_pt_rec_cav_ap_height", &csp_pt_rec_cav_ap_height);
    csp_pt_cost_receiver_area = Csp_pt_cost_receiver_area(tower_type, d_rec, rec_height,
        static_cast<int>(receiver_type), rec_d_spec, csp_pt_rec_cav_ap_height);
    ssc_data_set_number(data, "csp_pt_cost_receiver_area", csp_pt_cost_receiver_area);

    ssc_data_get_number(data, "p_ref", &p_ref);
    ssc_data_get_number(data, "design_eff", &design_eff);
    ssc_data_get_number(data, "tshours", &tshours);
    csp_pt_cost_storage_mwht = Csp_pt_cost_storage_mwht(tower_type, p_ref, design_eff, tshours);
    ssc_data_set_number(data, "csp_pt_cost_storage_mwht", csp_pt_cost_storage_mwht);

    ssc_data_get_number(data, "p_ref", &p_ref);
    demand_var = NULL;
    csp_pt_cost_power_block_mwe = Csp_pt_cost_power_block_mwe(tower_type, p_ref, demand_var);
    ssc_data_set_number(data, "csp_pt_cost_power_block_mwe", csp_pt_cost_power_block_mwe);

    Tower_SolarPilot_Capital_Costs_Equations(data);
}

void Tower_SolarPilot_Capital_Costs_DSPT_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    double d_rec, rec_height, receiver_type, rec_d_spec, csp_pt_rec_cav_ap_height, csp_pt_cost_receiver_area,
        p_ref, design_eff, tshours, csp_pt_cost_storage_mwht,
        demand_var, csp_pt_cost_power_block_mwe;

    TowerTypes tower_type = TowerTypes::kDirectSteam;

    ssc_data_get_number(data, "d_rec", &d_rec);
    ssc_data_get_number(data, "rec_height", &rec_height);
    receiver_type = NULL;
    rec_d_spec = NULL;
    csp_pt_rec_cav_ap_height = NULL;
    csp_pt_cost_receiver_area = Csp_pt_cost_receiver_area(tower_type, d_rec, rec_height,
        static_cast<int>(receiver_type), rec_d_spec, csp_pt_rec_cav_ap_height);
    ssc_data_set_number(data, "csp_pt_cost_receiver_area", csp_pt_cost_receiver_area);

    p_ref = NULL;
    design_eff = NULL;
    tshours = NULL;
    csp_pt_cost_storage_mwht = Csp_pt_cost_storage_mwht(tower_type, p_ref, design_eff, tshours);
    ssc_data_set_number(data, "csp_pt_cost_storage_mwht", csp_pt_cost_storage_mwht);

    p_ref = NULL;
    ssc_data_get_number(data, "demand_var", &demand_var);
    csp_pt_cost_power_block_mwe = Csp_pt_cost_power_block_mwe(tower_type, p_ref, demand_var);
    ssc_data_set_number(data, "csp_pt_cost_power_block_mwe", csp_pt_cost_power_block_mwe);

    Tower_SolarPilot_Capital_Costs_Equations(data);
}

void Tower_SolarPilot_Capital_Costs_ISCC_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    double d_rec, rec_height, receiver_type, rec_d_spec, csp_pt_rec_cav_ap_height, csp_pt_cost_receiver_area,
        p_ref, design_eff, tshours, csp_pt_cost_storage_mwht,
        demand_var, csp_pt_cost_power_block_mwe;

    TowerTypes tower_type = TowerTypes::kMoltenSalt;

    ssc_data_get_number(data, "d_rec", &d_rec);
    ssc_data_get_number(data, "rec_height", &rec_height);
    ssc_data_get_number(data, "receiver_type", &receiver_type);
    ssc_data_get_number(data, "rec_d_spec", &rec_d_spec);
    ssc_data_get_number(data, "csp_pt_rec_cav_ap_height", &csp_pt_rec_cav_ap_height);
    csp_pt_cost_receiver_area = Csp_pt_cost_receiver_area(tower_type, d_rec, rec_height,
        static_cast<int>(receiver_type), rec_d_spec, csp_pt_rec_cav_ap_height);
    ssc_data_set_number(data, "csp_pt_cost_receiver_area", csp_pt_cost_receiver_area);

    p_ref = NULL;
    design_eff = NULL;
    tshours = NULL;
    csp_pt_cost_storage_mwht = Csp_pt_cost_storage_mwht(tower_type, p_ref, design_eff, tshours);
    ssc_data_set_number(data, "csp_pt_cost_storage_mwht", csp_pt_cost_storage_mwht);

    p_ref = NULL;
    demand_var = NULL;
    csp_pt_cost_power_block_mwe = Csp_pt_cost_power_block_mwe(tower_type, p_ref, demand_var);
    ssc_data_set_number(data, "csp_pt_cost_power_block_mwe", csp_pt_cost_power_block_mwe);

    Tower_SolarPilot_Capital_Costs_Equations(data);
}
