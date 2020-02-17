#include "cmod_csp_tower_eqns.h"
#include "cmod_csp_common_eqns.h"
#include "vartab.h"

#pragma warning(disable: 4297)  // ignore warning: 'function assumed not to throw an exception but does'

void MSPT_System_Design_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }
    double P_ref, gross_net_conversion_factor, nameplate, design_eff, solarm, q_pb_design, q_rec_des, tshours, tshours_sf;

    // nameplate
    vt_get_number(vt, "P_ref", &P_ref);
    vt_get_number(vt, "gross_net_conversion_factor", &gross_net_conversion_factor);
    nameplate = Nameplate(P_ref, gross_net_conversion_factor);
    vt->assign("nameplate", nameplate);

    // q_pb_design
    vt_get_number(vt, "P_ref", &P_ref);                         // repeat assessors because the values may have changed
    vt_get_number(vt, "design_eff", &design_eff);
    q_pb_design = Q_pb_design(P_ref, design_eff);
    vt->assign("q_pb_design", q_pb_design);

    // q_rec_des
    vt_get_number(vt, "solarm", &solarm);
    vt_get_number(vt, "q_pb_design", &q_pb_design);
    q_rec_des = Q_rec_des(solarm, q_pb_design);
    vt->assign("q_rec_des", q_rec_des);

    // tshours_sf
    vt_get_number(vt, "tshours", &tshours);
    vt_get_number(vt, "solarm", &solarm);
    tshours_sf = Tshours_sf(tshours, solarm);
    vt->assign("tshours_sf", tshours_sf);
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
        q_rec_des, q_design, dni_des, dni_des_calc, opt_flux_penalty;
    int n_hel, override_opt, is_optimize, override_layout, opt_algorithm;

    util::matrix_t<double> helio_positions;

    // land_max_calc
    vt_get_number(vt, "land_max", &land_max);
    vt_get_number(vt, "h_tower", &h_tower);
    land_max_calc = Land_max_calc(land_max, h_tower);
    vt->assign("land_max_calc", land_max_calc);

    // n_hel
    vt_get_matrix(vt, "helio_positions", helio_positions);
    n_hel = N_hel(helio_positions);
    vt->assign("n_hel", n_hel);

    // csp_pt_sf_heliostat_area
    vt_get_number(vt, "helio_height", &helio_height);
    vt_get_number(vt, "helio_width", &helio_width);
    vt_get_number(vt, "dens_mirror", &dens_mirror);
    csp_pt_sf_heliostat_area = Csp_pt_sf_heliostat_area(helio_height, helio_width, dens_mirror);
    vt->assign("csp_pt_sf_heliostat_area", csp_pt_sf_heliostat_area);
    
    //  This one is not being read in the UI
    //// csp_pt_sf_total_reflective_area
    //double csp_pt_sf_total_reflective_area;
    //vt_get_int(vt, "n_hel", &n_hel);
    //vt_get_number(vt, "csp_pt_sf_heliostat_area", &csp_pt_sf_heliostat_area);
    //csp_pt_sf_total_reflective_area = Csp_pt_sf_total_reflective_area(n_hel, csp_pt_sf_heliostat_area);
    //vt->assign("csp_pt_sf_total_reflective_area", csp_pt_sf_total_reflective_area);

    // land_min_calc
    vt_get_number(vt, "land_min", &land_min);
    vt_get_number(vt, "h_tower", &h_tower);
    land_min_calc = Land_min_calc(land_min, h_tower);
    vt->assign("land_min_calc", land_min_calc);

    // csp_pt_sf_total_land_area
    vt_get_number(vt, "csp_pt_sf_fixed_land_area", &csp_pt_sf_fixed_land_area);
    vt_get_number(vt, "land_area_base", &land_area_base);
    vt_get_number(vt, "csp_pt_sf_land_overhead_factor", &csp_pt_sf_land_overhead_factor);
    csp_pt_sf_total_land_area = Csp_pt_sf_total_land_area(csp_pt_sf_fixed_land_area, land_area_base, csp_pt_sf_land_overhead_factor);
    vt->assign("csp_pt_sf_total_land_area", csp_pt_sf_total_land_area);

    // a_sf_ui
    vt_get_number(vt, "helio_width", &helio_width);
    vt_get_number(vt, "helio_height", &helio_height);
    vt_get_number(vt, "dens_mirror", &dens_mirror);
    vt_get_int(vt, "n_hel", &n_hel);
    a_sf_ui = A_sf_UI(helio_width, helio_height, dens_mirror, n_hel);
    vt->assign("a_sf_ui", a_sf_ui);

    // helio_area_tot
    vt_get_number(vt, "a_sf_ui", &a_sf_ui);
    helio_area_tot = Helio_area_tot(a_sf_ui);
    vt->assign("helio_area_tot", helio_area_tot);
    
    // csp_pt_sf_tower_height
    vt_get_number(vt, "h_tower", &h_tower);
    csp_pt_sf_tower_height = Csp_pt_sf_tower_height(h_tower);
    vt->assign("csp_pt_sf_tower_height", csp_pt_sf_tower_height);

    // c_atm_info
    //vt_get_matrix(vt, "helio_positions", helio_positions);        // THIS IS A PROBLEM: (getting the same ssc_data_t matrix data must also be changing it)
    vt_get_number(vt, "c_atm_0", &c_atm_0);
    vt_get_number(vt, "c_atm_1", &c_atm_1);
    vt_get_number(vt, "c_atm_2", &c_atm_2);
    vt_get_number(vt, "c_atm_3", &c_atm_3);
    vt_get_number(vt, "h_tower", &h_tower);
    c_atm_info = C_atm_info(helio_positions, c_atm_0, c_atm_1, c_atm_2, c_atm_3, h_tower);
    vt->assign("c_atm_info", c_atm_info);

    // error_equiv
    vt_get_number(vt, "helio_optical_error_mrad", &helio_optical_error_mrad);
    error_equiv = Error_equiv(helio_optical_error_mrad);
    vt->assign("error_equiv", error_equiv);

    // is_optimize
    vt_get_int(vt, "override_opt", &override_opt);
    is_optimize = Is_optimize(override_opt);
    vt->assign("is_optimize", is_optimize);

    // field_model_type
    vt_get_int(vt, "is_optimize", &is_optimize);
    vt_get_int(vt, "override_layout", &override_layout);
    field_model_type = Field_model_type(is_optimize, override_layout);
    vt->assign("field_model_type", field_model_type);

    // q_design
    vt_get_number(vt, "q_rec_des", &q_rec_des);
    q_design = Q_design(q_rec_des);
    vt->assign("q_design", q_design);

    // dni_des_calc
    vt_get_number(vt, "dni_des", &dni_des);
    dni_des_calc = Dni_des_calc(dni_des);
    vt->assign("dni_des_calc", dni_des_calc);

    // opt_algorithm
    opt_algorithm = Opt_algorithm();
    vt->assign("opt_algorithm", opt_algorithm);

    // opt_flux_penalty
    opt_flux_penalty = Opt_flux_penalty();
    vt->assign("opt_flux_penalty", opt_flux_penalty);
}
