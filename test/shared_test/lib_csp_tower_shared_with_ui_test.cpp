#include <gtest/gtest.h>
#include "cmod_csp_tower_eqns.h"
#include "cmod_financial_eqns.h"
#include "csp_common_test.h"
#include "vs_google_test_explorer_namespace.h"

namespace csp_common {}
using namespace csp_common;

//=======Testing Molten Salt Power Tower UI Equations=============================================
NAMESPACE_TEST(csp_common, TowerSharedWithUi, SystemDesign) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("design_eff", 0.412);
    vd->assign("gross_net_conversion_factor", 0.9);
    vd->assign("P_ref", 115.);
    vd->assign("solarm", 2.4);
    vd->assign("tshours", 10.);

    MSPT_System_Design_Equations(vd);

    double nameplate = vd->lookup("nameplate")->num;
    double q_pb_design = vd->lookup("q_pb_design")->num;
    double q_rec_des = vd->lookup("q_rec_des")->num;
    double tshours_sf = vd->lookup("tshours_sf")->num;
    ASSERT_NEAR(nameplate, 103.5, 103.5 * error_tolerance);
    ASSERT_NEAR(q_pb_design, 279., 279. * error_tolerance);
    ASSERT_NEAR(q_rec_des, 670., 670. * error_tolerance);
    ASSERT_NEAR(tshours_sf, 4.16667, 4.16667 * error_tolerance);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, SolarPilotField) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("c_atm_0", 0.006789);
    vd->assign("c_atm_1", 0.1046);
    vd->assign("c_atm_2", -0.017);
    vd->assign("c_atm_3", 0.002845);
    vd->assign("csp_pt_sf_fixed_land_area", 45.);
    vd->assign("csp_pt_sf_land_overhead_factor", 1.);
    vd->assign("dens_mirror", 0.97);
    vd->assign("dni_des", 950.);
    vd->assign("h_tower", 193.458);
    vd->assign("helio_height", 12.2);
    vd->assign("helio_optical_error_mrad", 1.53);
    util::matrix_t<double> helio_positions(8790, 2, 1.e3);
    vd->assign("helio_positions", helio_positions);
    vd->assign("helio_width", 12.2);
    vd->assign("land_area_base", 1847.04);
    vd->assign("land_max", 9.5);
    vd->assign("land_min", 0.75);
    vd->assign("override_layout", 0);
    vd->assign("override_opt", 0);
    vd->assign("q_rec_des", 670.);

    Tower_SolarPilot_Solar_Field_Equations(vd);

    double a_sf_ui = vd->lookup("a_sf_ui")->num;
    double c_atm_info = vd->lookup("c_atm_info")->num;
    double csp_pt_sf_heliostat_area = vd->lookup("csp_pt_sf_heliostat_area")->num;
    double csp_pt_sf_total_land_area = vd->lookup("csp_pt_sf_total_land_area")->num;
    //double csp_pt_sf_total_reflective_area = vd->lookup("csp_pt_sf_total_reflective_area")->num;	//  This one is not being read in the UI
    double csp_pt_sf_tower_height = vd->lookup("csp_pt_sf_tower_height")->num;
    double dni_des_calc = vd->lookup("dni_des_calc")->num;
    double error_equiv = vd->lookup("error_equiv")->num;
    double field_model_type = vd->lookup("field_model_type")->num;
    double helio_area_tot = vd->lookup("helio_area_tot")->num;
    double is_optimize = vd->lookup("is_optimize")->num;
    double land_max_calc = vd->lookup("land_max_calc")->num;
    double land_min_calc = vd->lookup("land_min_calc")->num;
    double n_hel = vd->lookup("n_hel")->num;
    double opt_algorithm = vd->lookup("opt_algorithm")->num;
    double opt_flux_penalty = vd->lookup("opt_flux_penalty")->num;
    double q_design = vd->lookup("q_design")->num;
    ASSERT_NEAR(a_sf_ui, 1269055., 1269055. * error_tolerance);
    ASSERT_NEAR(c_atm_info, 12.97, 12.97 * error_tolerance);
    ASSERT_NEAR(csp_pt_sf_heliostat_area, 144.375, 144.375 * error_tolerance);
    ASSERT_NEAR(csp_pt_sf_total_land_area, 1892., 1892. * error_tolerance);
    //ASSERT_NEAR(csp_pt_sf_total_reflective_area, 1269056.25, 1269056.25 * error_tolerance);			//  This one is not being read in the UI
    ASSERT_NEAR(csp_pt_sf_tower_height, 193.458, 193.458 * error_tolerance);
    ASSERT_NEAR(dni_des_calc, 950., 950. * error_tolerance);
    ASSERT_NEAR(error_equiv, 4.32749, 4.32749 * error_tolerance);
    ASSERT_NEAR(field_model_type, 2., 2. * error_tolerance);
    ASSERT_NEAR(helio_area_tot, 1269055., 1269055. * error_tolerance);
    ASSERT_NEAR(is_optimize, 0., 0. * error_tolerance);
    ASSERT_NEAR(land_max_calc, 1837.85, 1837.85 * error_tolerance);
    ASSERT_NEAR(land_min_calc, 145.094, 145.094 * error_tolerance);
    ASSERT_NEAR(n_hel, 8790., 8790. * error_tolerance);
    ASSERT_NEAR(opt_algorithm, 1., 1. * error_tolerance);
    ASSERT_NEAR(opt_flux_penalty, 0.25, 0.25 * error_tolerance);
    ASSERT_NEAR(q_design, 670., 670. * error_tolerance);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, SolarPilotFieldWithPeriodUse) {
    // Testing period use in variable names
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("c_atm_0", 0.006789);
    vd->assign("c_atm_1", 0.1046);
    vd->assign("c_atm_2", -0.017);
    vd->assign("c_atm_3", 0.002845);
    vd->assign("csp.pt.sf.fixed_land_area", 45.);
    vd->assign("csp.pt.sf.land_overhead_factor", 1.);
    vd->assign("dens_mirror", 0.97);
    vd->assign("dni_des", 950.);
    vd->assign("h_tower", 193.458);
    vd->assign("helio_height", 12.2);
    vd->assign("helio_optical_error_mrad", 1.53);
    util::matrix_t<double> helio_positions(8790, 2, 1.e3);
    vd->assign("helio_positions", helio_positions);
    vd->assign("helio_width", 12.2);
    vd->assign("land_area_base", 1847.04);
    vd->assign("land_max", 9.5);
    vd->assign("land_min", 0.75);
    vd->assign("override_layout", 0);
    vd->assign("override_opt", 0);
    vd->assign("q_rec_des", 670.);

    Tower_SolarPilot_Solar_Field_Equations(vd);

    double a_sf_ui = vd->lookup("a_sf_ui")->num;
    double c_atm_info = vd->lookup("c_atm_info")->num;
    double csp_pt_sf_heliostat_area = vd->lookup("csp.pt.sf.heliostat_area")->num;
    double csp_pt_sf_total_land_area = vd->lookup("csp.pt.sf.total_land_area")->num;
    //double csp_pt_sf_total_reflective_area = vd->lookup("csp.pt.sf.total_reflective_area")->num;	//  This one is not being read in the UI
    double csp_pt_sf_tower_height = vd->lookup("csp.pt.sf.tower_height")->num;
    double dni_des_calc = vd->lookup("dni_des_calc")->num;
    double error_equiv = vd->lookup("error_equiv")->num;
    double field_model_type = vd->lookup("field_model_type")->num;
    double helio_area_tot = vd->lookup("helio_area_tot")->num;
    double is_optimize = vd->lookup("is_optimize")->num;
    double land_max_calc = vd->lookup("land_max_calc")->num;
    double land_min_calc = vd->lookup("land_min_calc")->num;
    double n_hel = vd->lookup("n_hel")->num;
    double opt_algorithm = vd->lookup("opt_algorithm")->num;
    double opt_flux_penalty = vd->lookup("opt_flux_penalty")->num;
    double q_design = vd->lookup("q_design")->num;
    ASSERT_NEAR(a_sf_ui, 1269055., 1269055. * error_tolerance);
    ASSERT_NEAR(c_atm_info, 12.97, 12.97 * error_tolerance);
    ASSERT_NEAR(csp_pt_sf_heliostat_area, 144.375, 144.375 * error_tolerance);
    ASSERT_NEAR(csp_pt_sf_total_land_area, 1892., 1892. * error_tolerance);
    //ASSERT_NEAR(csp_pt_sf_total_reflective_area, 1269056.25, 1269056.25 * error_tolerance);			//  This one is not being read in the UI
    ASSERT_NEAR(csp_pt_sf_tower_height, 193.458, 193.458 * error_tolerance);
    ASSERT_NEAR(dni_des_calc, 950., 950. * error_tolerance);
    ASSERT_NEAR(error_equiv, 4.32749, 4.32749 * error_tolerance);
    ASSERT_NEAR(field_model_type, 2., 2. * error_tolerance);
    ASSERT_NEAR(helio_area_tot, 1269055., 1269055. * error_tolerance);
    ASSERT_NEAR(is_optimize, 0., 0. * error_tolerance);
    ASSERT_NEAR(land_max_calc, 1837.85, 1837.85 * error_tolerance);
    ASSERT_NEAR(land_min_calc, 145.094, 145.094 * error_tolerance);
    ASSERT_NEAR(n_hel, 8790., 8790. * error_tolerance);
    ASSERT_NEAR(opt_algorithm, 1., 1. * error_tolerance);
    ASSERT_NEAR(opt_flux_penalty, 0.25, 0.25 * error_tolerance);
    ASSERT_NEAR(q_design, 670., 670. * error_tolerance);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, Receiver) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("t_htf_cold_des", 290.);
    vd->assign("t_htf_hot_des", 574.);
    vd->assign("rec_htf", 17);
    vd->assign("csp_pt_rec_max_oper_frac", 1.2);
    vd->assign("q_rec_des", 660.9);
    vd->assign("rec_d_spec", 15.);
    vd->assign("csp_pt_rec_cav_ap_hw_ratio", 1.2);
    vd->assign("d_rec", 17.65);
    vd->assign("rec_height", 23.8084);
    vd->assign("h_tower", 193.458);
    vd->assign("piping_length_mult", 2.6);
    vd->assign("piping_length_const", 0.);
    vd->assign("piping_loss", 10200.);
    std::vector<double> field_fluid_properties{ 1, 7, 0, 0, 0, 0, 0, 0, 0 };
    util::matrix_t<double> field_fl_props(1, 9, &field_fluid_properties);
    vd->assign("field_fl_props", field_fl_props);

    MSPT_Receiver_Equations(vd);

    double csp_pt_rec_htf_t_avg = vd->lookup("csp_pt_rec_htf_t_avg")->num;
    double csp_pt_rec_htf_c_avg = vd->lookup("csp_pt_rec_htf_c_avg")->num;
    double csp_pt_rec_max_flow_to_rec = vd->lookup("csp_pt_rec_max_flow_to_rec")->num;
    double csp_pt_rec_cav_ap_height = vd->lookup("csp_pt_rec_cav_ap_height")->num;
    double rec_aspect = vd->lookup("rec_aspect")->num;
    double piping_length = vd->lookup("piping_length")->num;
    double piping_loss_tot = vd->lookup("piping_loss_tot")->num;
    ASSERT_NEAR(csp_pt_rec_htf_t_avg, 432., 432. * error_tolerance);
    ASSERT_NEAR(csp_pt_rec_htf_c_avg, 1.5066, 1.5066 * error_tolerance);
    ASSERT_NEAR(csp_pt_rec_max_flow_to_rec, 1853.5, 1853.5 * error_tolerance);
    ASSERT_NEAR(csp_pt_rec_cav_ap_height, 18., 18. * error_tolerance);
    ASSERT_NEAR(rec_aspect, 1.349, 1.349 * error_tolerance);
    ASSERT_NEAR(piping_length, 502.991, 502.991 * error_tolerance);
    ASSERT_NEAR(piping_loss_tot, 5130.51, 5130.51 * error_tolerance);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, ReceiverWithPeriodUse) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("t_htf_cold_des", 290.);
    vd->assign("t_htf_hot_des", 574.);
    vd->assign("rec_htf", 17);
    vd->assign("csp.pt.rec.max_oper_frac", 1.2);
    vd->assign("q_rec_des", 660.9);
    vd->assign("rec_d_spec", 15.);
    vd->assign("csp.pt.rec.cav_ap_hw_ratio", 1.2);
    vd->assign("d_rec", 17.65);
    vd->assign("rec_height", 23.8084);
    vd->assign("h_tower", 193.458);
    vd->assign("piping_length_mult", 2.6);
    vd->assign("piping_length_const", 0.);
    vd->assign("piping_loss", 10200.);
    std::vector<double> field_fluid_properties{ 1, 7, 0, 0, 0, 0, 0, 0, 0 };
    util::matrix_t<double> field_fl_props(1, 9, &field_fluid_properties);
    vd->assign("field_fl_props", field_fl_props);

    MSPT_Receiver_Equations(vd);

    double csp_pt_rec_htf_t_avg = vd->lookup("csp.pt.rec.htf_t_avg")->num;
    double csp_pt_rec_htf_c_avg = vd->lookup("csp.pt.rec.htf_c_avg")->num;
    double csp_pt_rec_max_flow_to_rec = vd->lookup("csp.pt.rec.max_flow_to_rec")->num;
    double csp_pt_rec_cav_ap_height = vd->lookup("csp.pt.rec.cav_ap_height")->num;
    double rec_aspect = vd->lookup("rec_aspect")->num;
    double piping_length = vd->lookup("piping_length")->num;
    double piping_loss_tot = vd->lookup("piping_loss_tot")->num;
    ASSERT_NEAR(csp_pt_rec_htf_t_avg, 432., 432. * error_tolerance);
    ASSERT_NEAR(csp_pt_rec_htf_c_avg, 1.5066, 1.5066 * error_tolerance);
    ASSERT_NEAR(csp_pt_rec_max_flow_to_rec, 1853.5, 1853.5 * error_tolerance);
    ASSERT_NEAR(csp_pt_rec_cav_ap_height, 18., 18. * error_tolerance);
    ASSERT_NEAR(rec_aspect, 1.349, 1.349 * error_tolerance);
    ASSERT_NEAR(piping_length, 502.991, 502.991 * error_tolerance);
    ASSERT_NEAR(piping_loss_tot, 5130.51, 5130.51 * error_tolerance);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, Tes) {
    double error_tolerance = 0.01;
    ssc_data_t data = ssc_data_create();
    auto data_vtab = static_cast<var_table*>(data);

    data_vtab->assign("P_ref", 115.);
    data_vtab->assign("design_eff", 0.412);
    data_vtab->assign("tshours", 10.);
    data_vtab->assign("T_htf_hot_des", 574.);
    data_vtab->assign("T_htf_cold_des", 290.);
    data_vtab->assign("rec_htf", 17);
    std::vector<double> field_fluid_properties{ 1, 7, 0, 0, 0, 0, 0, 0, 0 };
    util::matrix_t<double> field_fl_props(1, 9, &field_fluid_properties);
    data_vtab->assign("field_fl_props", field_fl_props);
    data_vtab->assign("h_tank_min", 1.);
    data_vtab->assign("h_tank", 12.);
    data_vtab->assign("tank_pairs", 1.);
    data_vtab->assign("u_tank", 0.4);

    int errors = run_module(data, "ui_tes_calcs");
    EXPECT_FALSE(errors);

    double q_tes = data_vtab->as_number("q_tes");
    double tes_avail_vol = data_vtab->as_number("tes_avail_vol");
    double vol_tank = data_vtab->as_number("vol_tank");
    double csp_pt_tes_tank_diameter = data_vtab->as_number("csp_pt_tes_tank_diameter");
    double q_dot_tes_est = data_vtab->as_number("q_dot_tes_est");
    double csp_pt_tes_htf_density = data_vtab->as_number("csp_pt_tes_htf_density");
    ASSERT_NEAR(q_tes, 2791.3, 2791.3 * error_tolerance);
    ASSERT_NEAR(tes_avail_vol, 12986., 12986. * error_tolerance);
    ASSERT_NEAR(vol_tank, 14166., 14166. * error_tolerance);
    ASSERT_NEAR(csp_pt_tes_tank_diameter, 38.8, 38.8 * error_tolerance);
    ASSERT_NEAR(q_dot_tes_est, 0.73, 0.73 * error_tolerance);
    ASSERT_NEAR(csp_pt_tes_htf_density, 1808.48, 1808.48 * error_tolerance);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, TesWithPeriodUse) {
    double error_tolerance = 0.01;
    ssc_data_t data = ssc_data_create();
    auto data_vtab = static_cast<var_table*>(data);

    data_vtab->assign("P_ref", 115.);
    data_vtab->assign("design_eff", 0.412);
    data_vtab->assign("tshours", 10.);
    data_vtab->assign("T_htf_hot_des", 574.);
    data_vtab->assign("T_htf_cold_des", 290.);
    data_vtab->assign("rec_htf", 17);
    std::vector<double> field_fluid_properties{ 1, 7, 0, 0, 0, 0, 0, 0, 0 };
    util::matrix_t<double> field_fl_props(1, 9, &field_fluid_properties);
    data_vtab->assign("field_fl_props", field_fl_props);
    data_vtab->assign("h_tank_min", 1.);
    data_vtab->assign("h_tank", 12.);
    data_vtab->assign("tank_pairs", 1.);
    data_vtab->assign("u_tank", 0.4);

    int errors = run_module(data, "ui_tes_calcs");
    EXPECT_FALSE(errors);

    double q_tes = data_vtab->as_number("q_tes");
    double tes_avail_vol = data_vtab->as_number("tes_avail_vol");
    double vol_tank = data_vtab->as_number("vol_tank");
    double csp_pt_tes_tank_diameter = data_vtab->as_number("csp.pt.tes.tank_diameter");
    double q_dot_tes_est = data_vtab->as_number("q_dot_tes_est");
    double csp_pt_tes_htf_density = data_vtab->as_number("csp.pt.tes.htf_density");
    ASSERT_NEAR(q_tes, 2791.3, 2791.3 * error_tolerance);
    ASSERT_NEAR(tes_avail_vol, 12986., 12986. * error_tolerance);
    ASSERT_NEAR(vol_tank, 14166., 14166. * error_tolerance);
    ASSERT_NEAR(csp_pt_tes_tank_diameter, 38.8, 38.8 * error_tolerance);
    ASSERT_NEAR(q_dot_tes_est, 0.73, 0.73 * error_tolerance);
    ASSERT_NEAR(csp_pt_tes_htf_density, 1808.48, 1808.48 * error_tolerance);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, SystemControl) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("bop_par", 0.);
    vd->assign("bop_par_f", 1.);
    vd->assign("bop_par_0", 0.);
    vd->assign("bop_par_1", 0.483);
    vd->assign("bop_par_2", 0.);
    vd->assign("p_ref", 115.);
    vd->assign("aux_par", 0.023);
    vd->assign("aux_par_f", 1.);
    vd->assign("aux_par_0", 0.483);
    vd->assign("aux_par_1", 0.571);
    vd->assign("aux_par_2", 0.);
    vd->assign("disp_wlim_maxspec", 1.);
    vd->assign("constant", 4.);

    MSPT_System_Control_Equations(vd);

    double csp_pt_par_calc_bop = vd->lookup("csp_pt_par_calc_bop")->num;
    double csp_pt_par_calc_aux = vd->lookup("csp_pt_par_calc_aux")->num;
    double disp_wlim_max = vd->lookup("disp_wlim_max")->num;
    util::matrix_t<ssc_number_t> wlim_series = vd->lookup("wlim_series")->num;
    ASSERT_NEAR(csp_pt_par_calc_bop, 0., 0. * error_tolerance);
    ASSERT_NEAR(csp_pt_par_calc_aux, 2.78783, 2.78783 * error_tolerance);
    ASSERT_NEAR(disp_wlim_max, 0.96, 0.96 * error_tolerance);
    ASSERT_NEAR(wlim_series.ncells(), 8760, 0.);
    ASSERT_NEAR(wlim_series.at(0, 0), 960., 960. * error_tolerance);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, SystemControlWithPeriods) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("bop_par", 0.);
    vd->assign("bop_par_f", 1.);
    vd->assign("bop_par_0", 0.);
    vd->assign("bop_par_1", 0.483);
    vd->assign("bop_par_2", 0.);
    vd->assign("p_ref", 115.);
    vd->assign("aux_par", 0.023);
    vd->assign("aux_par_f", 1.);
    vd->assign("aux_par_0", 0.483);
    vd->assign("aux_par_1", 0.571);
    vd->assign("aux_par_2", 0.);
    vd->assign("disp_wlim_maxspec", 1.);
    vd->assign("constant", 4.);

    MSPT_System_Control_Equations(vd);

    double csp_pt_par_calc_bop = vd->lookup("csp.pt.par.calc.bop")->num;
    double csp_pt_par_calc_aux = vd->lookup("csp.pt.par.calc.aux")->num;
    double disp_wlim_max = vd->lookup("disp_wlim_max")->num;
    util::matrix_t<ssc_number_t> wlim_series = vd->lookup("wlim_series")->num;
    ASSERT_NEAR(csp_pt_par_calc_bop, 0., 0. * error_tolerance);
    ASSERT_NEAR(csp_pt_par_calc_aux, 2.78783, 2.78783 * error_tolerance);
    ASSERT_NEAR(disp_wlim_max, 0.96, 0.96 * error_tolerance);
    ASSERT_NEAR(wlim_series.ncells(), 8760, 0.);
    ASSERT_NEAR(wlim_series.at(0, 0), 960., 960. * error_tolerance);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, CapitalCosts) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("d_rec", 17.65);
    vd->assign("rec_height", 21.60);
    vd->assign("receiver_type", 0);
    vd->assign("rec_d_spec", 15.);
    vd->assign("csp_pt_rec_cav_ap_height", 18.);
    vd->assign("p_ref", 115.);
    vd->assign("design_eff", 0.412);
    vd->assign("tshours", 10.);
    vd->assign("demand_var", 0);
    vd->assign("a_sf_ui", 1269055.);
    vd->assign("site_spec_cost", 16.);
    vd->assign("heliostat_spec_cost", 140.);
    vd->assign("cost_sf_fixed", 0.);
    vd->assign("h_tower", 193.458);
    vd->assign("rec_height", 21.6029);
    vd->assign("helio_height", 12.2);
    vd->assign("tower_fixed_cost", 3000000.);
    vd->assign("tower_exp", 0.0113);
    vd->assign("csp_pt_cost_receiver_area", 1269055.);
    vd->assign("rec_ref_cost", 103000000.);
    vd->assign("rec_ref_area", 1571.);
    vd->assign("rec_cost_exp", 0.7);
    vd->assign("csp_pt_cost_storage_mwht", 2791.26);
    vd->assign("tes_spec_cost", 22.);
    vd->assign("csp_pt_cost_power_block_mwe", 115.);
    vd->assign("plant_spec_cost", 1040.);
    vd->assign("bop_spec_cost", 290.);
    vd->assign("fossil_spec_cost", 0.);
    vd->assign("contingency_rate", 7.);
    vd->assign("csp_pt_sf_total_land_area", 1892.);
    vd->assign("nameplate", 104.);
    vd->assign("csp_pt_cost_epc_per_acre", 0.);
    vd->assign("csp_pt_cost_epc_percent", 13.);
    vd->assign("csp_pt_cost_epc_per_watt", 0.);
    vd->assign("csp_pt_cost_epc_fixed", 0.);
    vd->assign("land_spec_cost", 10000.);
    vd->assign("csp_pt_cost_plm_percent", 0.);
    vd->assign("csp_pt_cost_plm_per_watt", 0.);
    vd->assign("csp_pt_cost_plm_fixed", 0.);
    vd->assign("sales_tax_frac", 80.);
    vd->assign("sales_tax_rate", 5.);

    Tower_SolarPilot_Capital_Costs_MSPT_Equations(vd);

    double csp_pt_cost_receiver_area = vd->lookup("csp_pt_cost_receiver_area")->num;
    double csp_pt_cost_storage_mwht = vd->lookup("csp_pt_cost_storage_mwht")->num;
    double csp_pt_cost_power_block_mwe = vd->lookup("csp_pt_cost_power_block_mwe")->num;
    double csp_pt_cost_site_improvements = vd->lookup("csp_pt_cost_site_improvements")->num;
    double csp_pt_cost_heliostats = vd->lookup("csp_pt_cost_heliostats")->num;
    double csp_pt_cost_tower = vd->lookup("csp_pt_cost_tower")->num;
    double csp_pt_cost_receiver = vd->lookup("csp_pt_cost_receiver")->num;
    double csp_pt_cost_storage = vd->lookup("csp_pt_cost_storage")->num;
    double csp_pt_cost_power_block = vd->lookup("csp_pt_cost_power_block")->num;
    double csp_pt_cost_bop = vd->lookup("csp_pt_cost_bop")->num;
    double csp_pt_cost_fossil = vd->lookup("csp_pt_cost_fossil")->num;
    double ui_direct_subtotal = vd->lookup("ui_direct_subtotal")->num;
    double csp_pt_cost_contingency = vd->lookup("csp_pt_cost_contingency")->num;
    double total_direct_cost = vd->lookup("total_direct_cost")->num;
    double csp_pt_cost_epc_total = vd->lookup("csp_pt_cost_epc_total")->num;
    double csp_pt_cost_plm_total = vd->lookup("csp_pt_cost_plm_total")->num;
    double csp_pt_cost_sales_tax_total = vd->lookup("csp_pt_cost_sales_tax_total")->num;
    double total_indirect_cost = vd->lookup("total_indirect_cost")->num;
    double total_installed_cost = vd->lookup("total_installed_cost")->num;
    double csp_pt_cost_installed_per_capacity = vd->lookup("csp_pt_cost_installed_per_capacity")->num;
    ASSERT_NEAR(csp_pt_cost_receiver_area, 1197.86, 1197.86 * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_storage_mwht, 2791.26, 2791.26 * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_power_block_mwe, 115., 115. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_site_improvements, 20304872., 20304872. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_heliostats, 177667632., 177667632. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_tower, 25319156., 25319156. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_receiver, 85191944., 85191944. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_storage, 61407768., 61407768. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_power_block, 119600000., 119600000. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_bop, 33350000., 33350000. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_fossil, 0., 0. * error_tolerance);
    ASSERT_NEAR(ui_direct_subtotal, 522841376., 522841376. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_contingency, 36598896., 36598896. * error_tolerance);
    ASSERT_NEAR(total_direct_cost, 559440256., 559440256. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_epc_total, 72727232., 72727232. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_plm_total, 18920378., 18920378. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_sales_tax_total, 22377610., 22377610. * error_tolerance);
    ASSERT_NEAR(total_indirect_cost, 114025224., 114025224. * error_tolerance);
    ASSERT_NEAR(total_installed_cost, 673465472., 673465472. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_installed_per_capacity, 6506.91, 6506.91 * error_tolerance);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, CapitalCostsWithPeriods) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("d_rec", 17.65);
    vd->assign("rec_height", 21.60);
    vd->assign("receiver_type", 0);
    vd->assign("rec_d_spec", 15.);
    vd->assign("csp.pt.rec.cav_ap_height", 18.);
    vd->assign("p_ref", 115.);
    vd->assign("design_eff", 0.412);
    vd->assign("tshours", 10.);
    vd->assign("demand_var", 0);
    vd->assign("a_sf_ui", 1269055.);
    vd->assign("site_spec_cost", 16.);
    vd->assign("heliostat_spec_cost", 140.);
    vd->assign("cost_sf_fixed", 0.);
    vd->assign("h_tower", 193.458);
    vd->assign("rec_height", 21.6029);
    vd->assign("helio_height", 12.2);
    vd->assign("tower_fixed_cost", 3000000.);
    vd->assign("tower_exp", 0.0113);
    vd->assign("csp.pt.cost.receiver.area", 1269055.);
    vd->assign("rec_ref_cost", 103000000.);
    vd->assign("rec_ref_area", 1571.);
    vd->assign("rec_cost_exp", 0.7);
    vd->assign("csp.pt.cost.storage_mwht", 2791.26);
    vd->assign("tes_spec_cost", 22.);
    vd->assign("csp.pt.cost.power_block_mwe", 115.);
    vd->assign("plant_spec_cost", 1040.);
    vd->assign("bop_spec_cost", 290.);
    vd->assign("fossil_spec_cost", 0.);
    vd->assign("contingency_rate", 7.);
    vd->assign("csp.pt.sf.total_land_area", 1892.);
    vd->assign("nameplate", 104.);
    vd->assign("csp.pt.cost.epc.per_acre", 0.);
    vd->assign("csp.pt.cost.epc.percent", 13.);
    vd->assign("csp.pt.cost.epc.per_watt", 0.);
    vd->assign("csp.pt.cost.epc.fixed", 0.);
    vd->assign("land_spec_cost", 10000.);
    vd->assign("csp.pt.cost.plm.percent", 0.);
    vd->assign("csp.pt.cost.plm.per_watt", 0.);
    vd->assign("csp.pt.cost.plm.fixed", 0.);
    vd->assign("sales_tax_frac", 80.);
    vd->assign("sales_tax_rate", 5.);

    Tower_SolarPilot_Capital_Costs_MSPT_Equations(vd);

    double csp_pt_cost_receiver_area = vd->lookup("csp.pt.cost.receiver.area")->num;
    double csp_pt_cost_storage_mwht = vd->lookup("csp.pt.cost.storage_mwht")->num;
    double csp_pt_cost_power_block_mwe = vd->lookup("csp.pt.cost.power_block_mwe")->num;
    double csp_pt_cost_site_improvements = vd->lookup("csp.pt.cost.site_improvements")->num;
    double csp_pt_cost_heliostats = vd->lookup("csp.pt.cost.heliostats")->num;
    double csp_pt_cost_tower = vd->lookup("csp.pt.cost.tower")->num;
    double csp_pt_cost_receiver = vd->lookup("csp.pt.cost.receiver")->num;
    double csp_pt_cost_storage = vd->lookup("csp.pt.cost.storage")->num;
    double csp_pt_cost_power_block = vd->lookup("csp.pt.cost.power_block")->num;
    double csp_pt_cost_bop = vd->lookup("csp.pt.cost.bop")->num;
    double csp_pt_cost_fossil = vd->lookup("csp.pt.cost.fossil")->num;
    double ui_direct_subtotal = vd->lookup("ui_direct_subtotal")->num;
    double csp_pt_cost_contingency = vd->lookup("csp.pt.cost.contingency")->num;
    double total_direct_cost = vd->lookup("total_direct_cost")->num;
    double csp_pt_cost_epc_total = vd->lookup("csp.pt.cost.epc.total")->num;
    double csp_pt_cost_plm_total = vd->lookup("csp.pt.cost.plm.total")->num;
    double csp_pt_cost_sales_tax_total = vd->lookup("csp.pt.cost.sales_tax.total")->num;
    double total_indirect_cost = vd->lookup("total_indirect_cost")->num;
    double total_installed_cost = vd->lookup("total_installed_cost")->num;
    double csp_pt_cost_installed_per_capacity = vd->lookup("csp.pt.cost.installed_per_capacity")->num;
    ASSERT_NEAR(csp_pt_cost_receiver_area, 1197.86, 1197.86 * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_storage_mwht, 2791.26, 2791.26 * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_power_block_mwe, 115., 115. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_site_improvements, 20304872., 20304872. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_heliostats, 177667632., 177667632. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_tower, 25319156., 25319156. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_receiver, 85191944., 85191944. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_storage, 61407768., 61407768. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_power_block, 119600000., 119600000. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_bop, 33350000., 33350000. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_fossil, 0., 0. * error_tolerance);
    ASSERT_NEAR(ui_direct_subtotal, 522841376., 522841376. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_contingency, 36598896., 36598896. * error_tolerance);
    ASSERT_NEAR(total_direct_cost, 559440256., 559440256. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_epc_total, 72727232., 72727232. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_plm_total, 18920378., 18920378. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_sales_tax_total, 22377610., 22377610. * error_tolerance);
    ASSERT_NEAR(total_indirect_cost, 114025224., 114025224. * error_tolerance);
    ASSERT_NEAR(total_installed_cost, 673465472., 673465472. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_installed_per_capacity, 6506.91, 6506.91 * error_tolerance);
}

//======Financial Equations=======================================================================
NAMESPACE_TEST(csp_common, TowerSharedWithUi, FinancialCase1) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("total_installed_cost", 673465536.);

    vd->assign("const_per_percent1", 100.);
    vd->assign("const_per_upfront_rate1", 1.);
    vd->assign("const_per_months1", 24.);
    vd->assign("const_per_interest_rate1", 4.);

    vd->assign("const_per_percent2", 0.);
    vd->assign("const_per_upfront_rate2", 0.);
    vd->assign("const_per_months2", 0.);
    vd->assign("const_per_interest_rate2", 0.);

    vd->assign("const_per_percent3", 0.);
    vd->assign("const_per_upfront_rate3", 0.);
    vd->assign("const_per_months3", 0.);
    vd->assign("const_per_interest_rate3", 0.);

    vd->assign("const_per_percent4", 0.);
    vd->assign("const_per_upfront_rate4", 0.);
    vd->assign("const_per_months4", 0.);
    vd->assign("const_per_interest_rate4", 0.);

    vd->assign("const_per_percent5", 0.);
    vd->assign("const_per_upfront_rate5", 0.);
    vd->assign("const_per_months5", 0.);
    vd->assign("const_per_interest_rate5", 0.);

    Financial_Construction_Financing_Equations(vd);

    double const_per_principal1 = vd->lookup("const_per_principal1")->num;
    double const_per_interest1 = vd->lookup("const_per_interest1")->num;
    double const_per_total1 = vd->lookup("const_per_total1")->num;

    double const_per_principal2 = vd->lookup("const_per_principal2")->num;
    double const_per_interest2 = vd->lookup("const_per_interest2")->num;
    double const_per_total2 = vd->lookup("const_per_total2")->num;

    double const_per_principal3 = vd->lookup("const_per_principal3")->num;
    double const_per_interest3 = vd->lookup("const_per_interest3")->num;
    double const_per_total3 = vd->lookup("const_per_total3")->num;

    double const_per_principal4 = vd->lookup("const_per_principal4")->num;
    double const_per_interest4 = vd->lookup("const_per_interest4")->num;
    double const_per_total4 = vd->lookup("const_per_total4")->num;

    double const_per_principal5 = vd->lookup("const_per_principal5")->num;
    double const_per_interest5 = vd->lookup("const_per_interest5")->num;
    double const_per_total5 = vd->lookup("const_per_total5")->num;

    double const_per_principal_total = vd->lookup("const_per_principal_total")->num;
    double const_per_percent_total = vd->lookup("const_per_percent_total")->num;
    double construction_financing_cost = vd->lookup("construction_financing_cost")->num;
    double const_per_interest_total = vd->lookup("const_per_interest_total")->num;

    ASSERT_NEAR(const_per_principal1, 673465472., 673465472. * error_tolerance);
    ASSERT_NEAR(const_per_interest1, 26938618., 26938618. * error_tolerance);
    ASSERT_NEAR(const_per_total1, 33673272., 33673272. * error_tolerance);

    ASSERT_NEAR(const_per_principal2, 0., 0. * error_tolerance);
    ASSERT_NEAR(const_per_interest2, 0., 0. * error_tolerance);
    ASSERT_NEAR(const_per_total2, 0., 0. * error_tolerance);

    ASSERT_NEAR(const_per_principal3, 0., 0. * error_tolerance);
    ASSERT_NEAR(const_per_interest3, 0., 0. * error_tolerance);
    ASSERT_NEAR(const_per_total3, 0., 0. * error_tolerance);

    ASSERT_NEAR(const_per_principal4, 0., 0. * error_tolerance);
    ASSERT_NEAR(const_per_interest4, 0., 0. * error_tolerance);
    ASSERT_NEAR(const_per_total4, 0., 0. * error_tolerance);

    ASSERT_NEAR(const_per_principal5, 0., 0. * error_tolerance);
    ASSERT_NEAR(const_per_interest5, 0., 0. * error_tolerance);
    ASSERT_NEAR(const_per_total5, 0., 0. * error_tolerance);

    ASSERT_NEAR(const_per_percent_total, 100., 100. * error_tolerance);
    ASSERT_NEAR(const_per_principal_total, 673465472., 673465472. * error_tolerance);
    ASSERT_NEAR(const_per_interest_total, 26938618., 26938618. * error_tolerance);
    ASSERT_NEAR(construction_financing_cost, 33673272., 33673272. * error_tolerance);
}

NAMESPACE_TEST(csp_common, TowerSharedWithUi, FinancialCase2) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("system_capacity", 103500.);

    Financial_Capacity_Payments_Equations(vd);

    double cp_system_nameplate = vd->lookup("cp_system_nameplate")->num;
    ASSERT_NEAR(cp_system_nameplate, 103.5, 103.5 * error_tolerance);
}

//======/Testing Molten Salt Power Tower UI Equations=============================================


//TEST(Mspt_cmod_csp_tower_eqns, NoData) {
//	ASSERT_THROW(MSPT_System_Design_Equations(nullptr), std::runtime_error);
//	ASSERT_THROW(Tower_SolarPilot_Solar_Field_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(MSPT_Receiver_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(MSPT_System_Control_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_MSPT_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_DSPT_Equations(nullptr), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_ISCC_Equations(nullptr), std::runtime_error);
//}

//TEST(Mspt_cmod_csp_tower_eqns, MissingVariables) {
//	var_table* vd = new var_table;
//	ASSERT_THROW(MSPT_System_Design_Equations(vd), std::runtime_error);
//	ASSERT_THROW(Tower_SolarPilot_Solar_Field_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(MSPT_Receiver_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(MSPT_System_Control_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_MSPT_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_DSPT_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_ISCC_Equations(vd), std::runtime_error);
//}
