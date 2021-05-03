#include <gtest/gtest.h>
#include "cmod_csp_trough_eqns.h"
#include "cmod_financial_eqns.h"
#include "csp_common_test.h"
#include "vs_google_test_explorer_namespace.h"

namespace csp_common {}
using namespace csp_common;

double GetNum(var_table* vd, std::string name);

double GetNum(ssc_data_t data, std::string name);

//=======Testing Physical Power Trough UI Equations=============================================
NAMESPACE_TEST(csp_common, TroughSharedWithUi, SystemDesign) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("P_ref", 111);
    vd->assign("gross_net_conversion_factor", 0.9);
    vd->assign("eta_ref", 0.356);

    Physical_Trough_System_Design_Equations(vd);

    ASSERT_NEAR_FRAC(GetNum(vd, "csp_dtr_pwrb_nameplate"), 99.9, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "q_pb_design"), 311.8, kErrorToleranceHi);
}

NAMESPACE_TEST(csp_common, TroughSharedWithUi, SolarField) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("T_loop_in_des", 293);
    vd->assign("T_loop_out", 391);
    vd->assign("Fluid", 21);
    vd->assign("I_bn_des", 950);
    vd->assign("m_dot_htfmax", 12);
    vd->assign("fluid_dens_outlet_temp", 706.38);
    vd->assign("m_dot_htfmin", 1);
    vd->assign("fluid_dens_inlet_temp", 820.81);
    vd->assign("radio_sm_or_area", 0);
    vd->assign("specified_solar_multiple", 2);
    vd->assign("specified_total_aperture", 877000);
    vd->assign("tshours", 6);
    vd->assign("Row_Distance", 15);
    vd->assign("non_solar_field_land_area_multiplier", 1.4);
    vd->assign("nSCA", 8);
    vd->assign("SCA_drives_elec", 125);
    vd->assign("q_pb_design", 311.8);
    util::matrix_t<double> field_fl_props(1, 1, 0.);
    vd->assign("field_fl_props", field_fl_props);
    std::vector<double> trough_loop_control_vec{ 8, 1, 1, 8, 1, 1, 7, 1, 1, 6, 1, 1, 5, 1, 1, 4, 1, 1, 3, 1, 1,
        2, 1, 1, 1 };
    util::matrix_t<double> trough_loop_control(1, 25, &trough_loop_control_vec);
    vd->assign("trough_loop_control", trough_loop_control);
    util::matrix_t<double> A_aperture(1, 4, 656.);
    vd->assign("A_aperture", A_aperture);
    util::matrix_t<double> D_2(4, 4, 0.076);
    vd->assign("D_2", D_2);
    std::vector<double> HCE_FieldFrac_vec{ 0.985, 0.01, 0.005, 0., 1., 0., 0., 0., 1., 0., 0., 0., 1., 0., 0., 0. };
    util::matrix_t<double> HCE_FieldFrac(4, 4, &HCE_FieldFrac_vec);
    vd->assign("HCE_FieldFrac", HCE_FieldFrac);
    std::vector<double> Design_loss_vec{ 190., 1270., 1500., 0., 190., 1270., 1500., 0., 190., 1270., 1500., 0.,
        190., 1270., 1500., 0. };
    util::matrix_t<double> Design_loss(4, 4, &Design_loss_vec);
    vd->assign("Design_loss", Design_loss);
    util::matrix_t<double> L_SCA(1, 4, 115);
    vd->assign("L_SCA", L_SCA);
    util::matrix_t<double> TrackingError(1, 4, 0.988);
    vd->assign("TrackingError", TrackingError);
    util::matrix_t<double> GeomEffects(1, 4, 0.952);
    vd->assign("GeomEffects", GeomEffects);
    util::matrix_t<double> Rho_mirror_clean(1, 4, 0.93);
    vd->assign("Rho_mirror_clean", Rho_mirror_clean);
    util::matrix_t<double> Dirt_mirror(1, 4, 0.97);
    vd->assign("Dirt_mirror", Dirt_mirror);
    util::matrix_t<double> Error(1, 4, 1.);
    vd->assign("Error", Error);
    std::vector<double> Shadowing_vec{ 0.935, 0.935, 0.935, 0.963, 0.935, 0.935, 0.935, 0.963, 0.935, 0.935, 0.935,
        0.963, 0.935, 0.935, 0.935, 0.963 };
    util::matrix_t<double> Shadowing(4, 4, &Shadowing_vec);
    vd->assign("Shadowing", Shadowing);
    std::vector<double> Dirt_HCE_vec{ 0.98, 0.98, 1., 0.98, 0.98, 0.98, 1., 0.98, 0.98, 0.98, 1., 0.98, 0.98, 0.98,
        1., 0.98 };
    util::matrix_t<double> Dirt_HCE(4, 4, &Dirt_HCE_vec);
    vd->assign("Dirt_HCE", Dirt_HCE);
    std::vector<double> alpha_abs_vec{ 0.963, 0.963, 0.8, 0., 0.963, 0.963, 0.8, 0., 0.963, 0.963, 0.8, 0., 0.963,
        0.963, 0.8, 0. };
    util::matrix_t<double> alpha_abs(4, 4, &alpha_abs_vec);
    vd->assign("alpha_abs", alpha_abs);
    std::vector<double> Tau_envelope_vec{ 0.964, 0.964, 1., 0., 0.964, 0.964, 1., 0., 0.964, 0.964, 1., 0., 0.964,
        0.964, 1., 0. };
    util::matrix_t<double> Tau_envelope(4, 4, &Tau_envelope_vec);
    vd->assign("Tau_envelope", Tau_envelope);
    util::matrix_t<double> W_aperture(1, 4, 6.);
    vd->assign("W_aperture", W_aperture);

    Physical_Trough_Solar_Field_Equations(vd);

    ASSERT_NEAR_FRAC(GetNum(vd, "field_htf_cp_avg"), 2.455, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "single_loop_aperature"), 5248, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "min_inner_diameter"), 0.076, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "cspdtr_loop_hce_heat_loss"), 0.9617, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "loop_optical_efficiency"), 0.7213, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "max_field_flow_velocity"), 3.745, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "min_field_flow_velocity"), 0.2686, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "required_number_of_loops_for_SM1"), 91, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "total_loop_conversion_efficiency"), 0.6937, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "total_required_aperture_for_SM1"), 473114, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "nLoops"), 181, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "total_aperture"), 949888, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "field_thermal_output"), 626, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "solar_mult"), 2, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "q_rec_des"), 623.6, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "tshours_sf"), 3, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "fixed_land_area"), 586.8, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "total_land_area"), 821.5, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "total_tracking_power"), 181000, kErrorToleranceHi);
    util::matrix_t<ssc_number_t> csp_dtr_hce_design_heat_losses = vd->lookup("csp_dtr_hce_design_heat_losses")->num;
    ASSERT_NEAR_FRAC(csp_dtr_hce_design_heat_losses.at(0,0), 207.35, kErrorToleranceHi);
    util::matrix_t<ssc_number_t> csp_dtr_sca_calc_sca_effs = vd->lookup("csp_dtr_sca_calc_sca_effs")->num;
    ASSERT_NEAR_FRAC(csp_dtr_sca_calc_sca_effs.at(0,0), 0.8485, kErrorToleranceHi);
    util::matrix_t<ssc_number_t> csp_dtr_hce_optical_effs = vd->lookup("csp_dtr_hce_optical_effs")->num;
    ASSERT_NEAR_FRAC(csp_dtr_hce_optical_effs.at(0,0), 0.8501, kErrorToleranceHi);
    util::matrix_t<ssc_number_t> SCAInfoArray = vd->lookup("SCAInfoArray")->num;
    ASSERT_NEAR_FRAC(SCAInfoArray.ncells(), 16, kErrorToleranceHi);
    util::matrix_t<ssc_number_t> SCADefocusArray = vd->lookup("SCADefocusArray")->num;
    ASSERT_NEAR_FRAC(SCADefocusArray.ncells(), 8, kErrorToleranceHi);
    util::matrix_t<ssc_number_t> K_cpnt = vd->lookup("K_cpnt")->num;
    ASSERT_NEAR_FRAC(K_cpnt.ncells(), 121, kErrorToleranceHi);
    util::matrix_t<ssc_number_t> D_cpnt = vd->lookup("D_cpnt")->num;
    ASSERT_NEAR_FRAC(D_cpnt.ncells(), 121, kErrorToleranceHi);
    util::matrix_t<ssc_number_t> L_cpnt = vd->lookup("L_cpnt")->num;
    ASSERT_NEAR_FRAC(L_cpnt.ncells(), 121, kErrorToleranceHi);
    util::matrix_t<ssc_number_t> Type_cpnt = vd->lookup("Type_cpnt")->num;
    ASSERT_NEAR_FRAC(Type_cpnt.ncells(), 121, kErrorToleranceHi);
}

NAMESPACE_TEST(csp_common, TroughSharedWithUi, CollectorType) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("lat", 32.13);
    vd->assign("tilt", 0.);
    vd->assign("azimuth", 0.);
    vd->assign("nSCA", 8);
    util::matrix_t<double> L_SCA(1, 4, 115.);
    vd->assign("L_SCA", L_SCA);
    util::matrix_t<double> ColperSCA(1, 4, 8);
    vd->assign("ColperSCA", ColperSCA);
    util::matrix_t<double> Ave_Focal_Length(1, 4, 2.15);
    vd->assign("Ave_Focal_Length", Ave_Focal_Length);
    util::matrix_t<double> Distance_SCA(1, 4, 1.);
    vd->assign("Distance_SCA", Distance_SCA);
    std::vector<double> IAM_matrix_vec{ 1., 0.0327, -0.1351, 1., 0.0327, -0.1351, 1., 0.0327, -0.1351, 1., 0.0327, -0.1351 };
    util::matrix_t<double> IAM_matrix(4, 3, &IAM_matrix_vec);
    vd->assign("IAM_matrix", IAM_matrix);

    Physical_Trough_Collector_Type_Equations(vd);

    ASSERT_NEAR_FRAC(GetNum(vd, "csp_dtr_sca_calc_zenith"), 0.1506, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_dtr_sca_calc_costh"), 0.9886, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_dtr_sca_calc_theta"), 0.1514, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "csp_dtr_sca_calc_latitude"), 32.13, kErrorToleranceHi);

    util::matrix_t<ssc_number_t> csp_dtr_sca_ap_lengths = vd->lookup("csp_dtr_sca_ap_lengths")->num;
    ASSERT_NEAR_FRAC(csp_dtr_sca_ap_lengths.at(0,0), 14.375, kErrorToleranceHi);
    util::matrix_t<ssc_number_t> csp_dtr_sca_calc_end_gains = vd->lookup("csp_dtr_sca_calc_end_gains")->num;
    ASSERT_NEAR_FRAC(csp_dtr_sca_calc_end_gains.at(0,0), 0., kErrorToleranceHi);
    util::matrix_t<ssc_number_t> csp_dtr_sca_calc_end_losses = vd->lookup("csp_dtr_sca_calc_end_losses")->num;
    ASSERT_NEAR_FRAC(csp_dtr_sca_calc_end_losses.at(0,0), 0.9996, kErrorToleranceHi);
    util::matrix_t<ssc_number_t> csp_dtr_sca_calc_iams = vd->lookup("csp_dtr_sca_calc_iams")->num;
    ASSERT_NEAR_FRAC(csp_dtr_sca_calc_iams.at(0,0), 1.0019, kErrorToleranceHi);
}

NAMESPACE_TEST(csp_common, TroughSharedWithUi, SystemControl) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("is_dispatch", 0);
    vd->assign("disp_wlim_maxspec", 1.e38);
    vd->assign("constant", 4);

    Physical_Trough_System_Control_Equations(vd);

    ASSERT_NEAR_FRAC(GetNum(vd, "is_wlim_series"), 0, kErrorToleranceHi);
    ASSERT_NEAR_FRAC(GetNum(vd, "disp_wlim_max"), 9.6e37, kErrorToleranceHi);
    util::matrix_t<ssc_number_t> wlim_series = vd->lookup("wlim_series")->num;
    ASSERT_NEAR_FRAC(wlim_series.ncells(), 8760, 0.);
    ASSERT_NEAR_FRAC(wlim_series.at(0, 0), 9.6e40, kErrorToleranceHi);
}
