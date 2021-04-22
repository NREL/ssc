#include "cmod_csp_trough_eqns.h"
#include "cmod_csp_common_eqns.h"
#include "vartab.h"
#include <cmath>

#pragma warning(disable: 4297)  // ignore warning: 'function assumed not to throw an exception but does'


void Physical_Trough_System_Design_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    double P_ref, gross_net_conversion_factor, csp_dtr_pwrb_nameplate,
        eta_ref, q_pb_design,
        radio_sm_or_area, specified_solar_multiple, total_aperture, total_required_aperture_for_SM1, solar_mult,
        q_rec_des,
        tshours, tshours_sf,
        specified_total_aperture, single_loop_aperture, nloops;
/*
    // csp_dtr_pwrb_nameplate
    ssc_data_t_get_number(data, "P_ref", &P_ref);
    ssc_data_t_get_number(data, "gross_net_conversion_factor", &gross_net_conversion_factor);
    csp_dtr_pwrb_nameplate = Nameplate(P_ref, gross_net_conversion_factor);
    ssc_data_t_set_number(data, "csp_dtr_pwrb_nameplate", csp_dtr_pwrb_nameplate);

    // q_pb_design
    //ssc_data_t_get_number(data, "P_ref", &P_ref);
    ssc_data_t_get_number(data, "eta_ref", &eta_ref);
    q_pb_design = Q_pb_design(P_ref, eta_ref);
    ssc_data_t_set_number(data, "q_pb_design", q_pb_design);

    // solar_mult
    ssc_data_t_get_number(data, "radio_sm_or_area", &radio_sm_or_area);
    ssc_data_t_get_number(data, "specified_solar_multiple", &specified_solar_multiple);
    ssc_data_t_get_number(data, "total_aperture", &total_aperture);
    ssc_data_t_get_number(data, "total_required_aperture_for_SM1", &total_required_aperture_for_SM1);
    solar_mult = Solar_mult(static_cast<int>(radio_sm_or_area), specified_solar_multiple, total_aperture, total_required_aperture_for_SM1);
    ssc_data_t_set_number(data, "solar_mult", solar_mult);

    // q_rec_des
    ssc_data_t_get_number(data, "solar_mult", &solar_mult);
    ssc_data_t_get_number(data, "q_pb_design", &q_pb_design);
    q_rec_des = Q_rec_des(solar_mult, q_pb_design);
    ssc_data_t_set_number(data, "q_rec_des", q_rec_des);

    // tshours_sf
    ssc_data_t_get_number(data, "tshours", &tshours);
    ssc_data_t_get_number(data, "solar_mult", &solar_mult);
    tshours_sf = Tshours_sf(tshours, solar_mult);
    ssc_data_t_set_number(data, "tshours_sf", tshours_sf);

    // nloops
    ssc_data_t_get_number(data, "radio_sm_or_area", &radio_sm_or_area);
    ssc_data_t_get_number(data, "specified_solar_multiple", &specified_solar_multiple);
    ssc_data_t_get_number(data, "total_required_aperture_for_SM1", &total_required_aperture_for_SM1);
    ssc_data_t_get_number(data, "specified_total_aperture", &specified_total_aperture);
    ssc_data_t_get_number(data, "single_loop_aperature", &single_loop_aperture);
    nloops = Nloops(static_cast<int>(radio_sm_or_area), specified_solar_multiple, total_required_aperture_for_SM1, specified_total_aperture, single_loop_aperture);
    ssc_data_t_set_number(data, "nloops", nloops);
*/
    double x = 1.;
}

void Physical_Trough_Solar_Field_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    // Inputs
    double P_ref, gross_net_conversion_factor,
        eta_ref,
        T_loop_in_des, T_loop_out, Fluid,
        csp_dtr_sca_aperture_1, csp_dtr_sca_aperture_2, csp_dtr_sca_aperture_3, csp_dtr_sca_aperture_4,
        csp_dtr_hce_diam_absorber_inner_1, csp_dtr_hce_diam_absorber_inner_2, csp_dtr_hce_diam_absorber_inner_3, csp_dtr_hce_diam_absorber_inner_4,
        I_bn_des, csp_dtr_hce_design_heat_loss_1, csp_dtr_hce_design_heat_loss_2, csp_dtr_hce_design_heat_loss_3, csp_dtr_hce_design_heat_loss_4,
        csp_dtr_sca_length_1, csp_dtr_sca_length_2, csp_dtr_sca_length_3, csp_dtr_sca_length_4,
        csp_dtr_sca_calc_sca_eff_1, csp_dtr_sca_calc_sca_eff_2, csp_dtr_sca_calc_sca_eff_3, csp_dtr_sca_calc_sca_eff_4,
        csp_dtr_hce_optical_eff_1, csp_dtr_hce_optical_eff_2, csp_dtr_hce_optical_eff_3, csp_dtr_hce_optical_eff_4,
        m_dot_htfmax, fluid_dens_outlet_temp,
        m_dot_htfmin, fluid_dens_inlet_temp,
        radio_sm_or_area, specified_solar_multiple, specified_total_aperture,
        tshours,
        Row_Distance, csp_dtr_sca_w_profile_1, csp_dtr_sca_w_profile_2, csp_dtr_sca_w_profile_3, csp_dtr_sca_w_profile_4,
        non_solar_field_land_area_multiplier,
        nSCA, SCA_drives_elec;


    // Outputs
    double csp_dtr_pwrb_nameplate,
        q_pb_design,
        field_htf_cp_avg,
        single_loop_aperature,
        min_inner_diameter,
        cspdtr_loop_hce_heat_loss,
        loop_optical_efficiency,
        max_field_flow_velocity,
        min_field_flow_velocity,
        required_number_of_loops_for_SM1,
        total_loop_conversion_efficiency,
        total_required_aperture_for_SM1,
        nLoops,
        total_aperture,
        field_thermal_output,
        solar_mult,
        q_rec_des,
        tshours_sf,
        fixed_land_area,
        total_land_area,
        total_tracking_power;

    util::matrix_t<ssc_number_t> field_fl_props, trough_loop_control, SCAInfoArray, SCADefocusArray,
        K_cpnt, D_cpnt, L_cpnt, Type_cpnt, L_SCA;

    // csp_dtr_pwrb_nameplate
    ssc_data_t_get_number(data, "P_ref", &P_ref);
    ssc_data_t_get_number(data, "gross_net_conversion_factor", &gross_net_conversion_factor);
    csp_dtr_pwrb_nameplate = Nameplate(P_ref, gross_net_conversion_factor);
    ssc_data_t_set_number(data, "csp_dtr_pwrb_nameplate", csp_dtr_pwrb_nameplate);

    // q_pb_design
    ssc_data_t_get_number(data, "eta_ref", &eta_ref);
    q_pb_design = Q_pb_design(P_ref, eta_ref);
    ssc_data_t_set_number(data, "q_pb_design", q_pb_design);

    // field_htf_cp_avg
    ssc_data_t_get_number(data, "T_loop_in_des", &T_loop_in_des);
    ssc_data_t_get_number(data, "T_loop_out", &T_loop_out);
    ssc_data_t_get_number(data, "Fluid", &Fluid);
    ssc_data_t_get_matrix(vt, "field_fl_props", field_fl_props);
    field_htf_cp_avg = Field_htf_cp_avg(T_loop_in_des, T_loop_out, Fluid, field_fl_props);      // [kJ/kg-K]
    ssc_data_t_set_number(data, "field_htf_cp_avg", field_htf_cp_avg);

    // single_loop_aperature
    ssc_data_t_get_matrix(vt, "trough_loop_control", trough_loop_control);
    util::matrix_t<ssc_number_t> A_aperture;
    ssc_data_t_get_matrix(vt, "A_aperture", A_aperture);
    csp_dtr_sca_aperture_1 = A_aperture.at(0);
    csp_dtr_sca_aperture_2 = A_aperture.at(1);
    csp_dtr_sca_aperture_3 = A_aperture.at(2);
    csp_dtr_sca_aperture_4 = A_aperture.at(3);
    single_loop_aperature = Single_loop_aperature(trough_loop_control, csp_dtr_sca_aperture_1,
        csp_dtr_sca_aperture_2, csp_dtr_sca_aperture_3, csp_dtr_sca_aperture_4);
    ssc_data_t_set_number(data, "single_loop_aperature", single_loop_aperature);

    // min_inner_diameter
    util::matrix_t<ssc_number_t> D_2;
    ssc_data_t_get_matrix(vt, "D_2", D_2);
    csp_dtr_hce_diam_absorber_inner_1 = D_2.at(0,0);
    csp_dtr_hce_diam_absorber_inner_2 = D_2.at(1,0);
    csp_dtr_hce_diam_absorber_inner_3 = D_2.at(2,0);
    csp_dtr_hce_diam_absorber_inner_4 = D_2.at(3,0);
    min_inner_diameter = Min_inner_diameter(trough_loop_control, csp_dtr_hce_diam_absorber_inner_1,
        csp_dtr_hce_diam_absorber_inner_2, csp_dtr_hce_diam_absorber_inner_3, csp_dtr_hce_diam_absorber_inner_4);
    ssc_data_t_set_number(data, "min_inner_diameter", min_inner_diameter);

    // cspdtr_loop_hce_heat_loss
    ssc_data_t_get_number(data, "I_bn_des", &I_bn_des);
    // **THESE ARE OUTPUTS FROM ANOTHER FORM**
    ssc_data_t_get_number(data, "csp_dtr_hce_design_heat_loss_1", &csp_dtr_hce_design_heat_loss_1);
    ssc_data_t_get_number(data, "csp_dtr_hce_design_heat_loss_2", &csp_dtr_hce_design_heat_loss_2);
    ssc_data_t_get_number(data, "csp_dtr_hce_design_heat_loss_3", &csp_dtr_hce_design_heat_loss_3);
    ssc_data_t_get_number(data, "csp_dtr_hce_design_heat_loss_4", &csp_dtr_hce_design_heat_loss_4);

    ssc_data_t_get_matrix(vt, "L_SCA", L_SCA);
    csp_dtr_sca_length_1 = L_SCA.at(0);
    csp_dtr_sca_length_2 = L_SCA.at(1);
    csp_dtr_sca_length_3 = L_SCA.at(2);
    csp_dtr_sca_length_4 = L_SCA.at(3);
    cspdtr_loop_hce_heat_loss = Cspdtr_loop_hce_heat_loss(trough_loop_control, I_bn_des,
        csp_dtr_hce_design_heat_loss_1, csp_dtr_hce_design_heat_loss_2,
        csp_dtr_hce_design_heat_loss_3, csp_dtr_hce_design_heat_loss_4,
        csp_dtr_sca_length_1, csp_dtr_sca_length_2, csp_dtr_sca_length_3, csp_dtr_sca_length_4,
        csp_dtr_sca_aperture_1, csp_dtr_sca_aperture_2, csp_dtr_sca_aperture_3, csp_dtr_sca_aperture_4);
    ssc_data_t_set_number(data, "cspdtr_loop_hce_heat_loss", cspdtr_loop_hce_heat_loss);

    // loop_optical_efficiency
    // **THESE ARE OUTPUTS FROM ANOTHER FORM**
    ssc_data_t_get_number(data, "csp_dtr_sca_calc_sca_eff_1", &csp_dtr_sca_calc_sca_eff_1);
    ssc_data_t_get_number(data, "csp_dtr_sca_calc_sca_eff_2", &csp_dtr_sca_calc_sca_eff_2);
    ssc_data_t_get_number(data, "csp_dtr_sca_calc_sca_eff_3", &csp_dtr_sca_calc_sca_eff_3);
    ssc_data_t_get_number(data, "csp_dtr_sca_calc_sca_eff_4", &csp_dtr_sca_calc_sca_eff_4);
    // **THESE ARE OUTPUTS FROM ANOTHER FORM**
    ssc_data_t_get_number(data, "csp_dtr_hce_optical_eff_1", &csp_dtr_hce_optical_eff_1);
    ssc_data_t_get_number(data, "csp_dtr_hce_optical_eff_2", &csp_dtr_hce_optical_eff_2);
    ssc_data_t_get_number(data, "csp_dtr_hce_optical_eff_3", &csp_dtr_hce_optical_eff_3);
    ssc_data_t_get_number(data, "csp_dtr_hce_optical_eff_4", &csp_dtr_hce_optical_eff_4);
    loop_optical_efficiency = Loop_optical_efficiency(trough_loop_control,
        csp_dtr_sca_calc_sca_eff_1, csp_dtr_sca_calc_sca_eff_2,
        csp_dtr_sca_calc_sca_eff_3, csp_dtr_sca_calc_sca_eff_4,
        csp_dtr_sca_length_1, csp_dtr_sca_length_2, csp_dtr_sca_length_3, csp_dtr_sca_length_4,
        csp_dtr_hce_optical_eff_1, csp_dtr_hce_optical_eff_2,
        csp_dtr_hce_optical_eff_3, csp_dtr_hce_optical_eff_4);
    ssc_data_t_set_number(data, "loop_optical_efficiency", loop_optical_efficiency);

    // sca_info_array
    SCAInfoArray = Sca_info_array(trough_loop_control);
    ssc_data_t_set_matrix(data, "scainfoarray", SCAInfoArray);
    
    // sca_defocus_array
    SCADefocusArray = Sca_defocus_array(trough_loop_control);
    ssc_data_t_set_array(data, "scadefocusarray", SCADefocusArray.data(), SCADefocusArray.ncells());

    //
    // End of no calculated dependencies
    //


    // max_field_flow_velocity
    ssc_data_t_get_number(data, "m_dot_htfmax", &m_dot_htfmax);
    ssc_data_t_get_number(data, "fluid_dens_outlet_temp", &fluid_dens_outlet_temp);
    max_field_flow_velocity = Max_field_flow_velocity(m_dot_htfmax, fluid_dens_outlet_temp, min_inner_diameter);
    ssc_data_t_set_number(data, "max_field_flow_velocity", max_field_flow_velocity);

    // min_field_flow_velocity
    ssc_data_t_get_number(data, "m_dot_htfmin", &m_dot_htfmin);
    ssc_data_t_get_number(data, "fluid_dens_inlet_temp", &fluid_dens_inlet_temp);
    min_field_flow_velocity = Min_field_flow_velocity(m_dot_htfmin, fluid_dens_inlet_temp, min_inner_diameter);
    ssc_data_t_set_number(data, "min_field_flow_velocity", min_field_flow_velocity);

    // total_loop_conversion_efficiency
    total_loop_conversion_efficiency = Total_loop_conversion_efficiency(loop_optical_efficiency, cspdtr_loop_hce_heat_loss);
    ssc_data_t_set_number(data, "total_loop_conversion_efficiency", total_loop_conversion_efficiency);

    // total_required_aperture_for_SM1
    total_required_aperture_for_SM1 = Total_required_aperture_for_sm1(q_pb_design, I_bn_des, total_loop_conversion_efficiency);
    ssc_data_t_set_number(data, "total_required_aperture_for_sm1", total_required_aperture_for_SM1);

    // required_number_of_loops_for_SM1
    required_number_of_loops_for_SM1 = Required_number_of_loops_for_SM1(total_required_aperture_for_SM1, single_loop_aperature);
    ssc_data_t_set_number(data, "required_number_of_loops_for_sm1", required_number_of_loops_for_SM1);

    // nloops
    ssc_data_t_get_number(data, "radio_sm_or_area", &radio_sm_or_area);
    ssc_data_t_get_number(data, "specified_solar_multiple", &specified_solar_multiple);
    ssc_data_t_get_number(data, "specified_total_aperture", &specified_total_aperture);
    nLoops = Nloops(static_cast<int>(radio_sm_or_area), specified_solar_multiple, total_required_aperture_for_SM1, specified_total_aperture, single_loop_aperature);
    ssc_data_t_set_number(data, "nloops", nLoops);

    // total_aperture
    total_aperture = Total_aperture(single_loop_aperature, nLoops);
    ssc_data_t_set_number(data, "total_aperture", total_aperture);

    // field_thermal_output
    field_thermal_output = Field_thermal_output(I_bn_des, total_loop_conversion_efficiency, total_aperture);
    ssc_data_t_set_number(data, "field_thermal_output", field_thermal_output);

    // solar_mult
    solar_mult = Solar_mult(static_cast<int>(radio_sm_or_area), specified_solar_multiple, total_aperture, total_required_aperture_for_SM1);
    ssc_data_t_set_number(data, "solar_mult", solar_mult);

    // Q_rec_des
    q_rec_des = Q_rec_des(solar_mult, q_pb_design);
    ssc_data_t_set_number(data, "q_rec_des", q_rec_des);

    // tshours_sf
    ssc_data_t_get_number(data, "tshours", &tshours);
    tshours_sf = Tshours_sf(tshours, solar_mult);
    ssc_data_t_set_number(data, "tshours_sf", tshours_sf);

    // fixed_land_area
    ssc_data_t_get_number(data, "Row_Distance", &Row_Distance);
    util::matrix_t<ssc_number_t> W_aperture;
    ssc_data_t_get_matrix(vt, "W_aperture", W_aperture);
    csp_dtr_sca_w_profile_1 = W_aperture.at(0);
    csp_dtr_sca_w_profile_2 = W_aperture.at(1);
    csp_dtr_sca_w_profile_3 = W_aperture.at(2);
    csp_dtr_sca_w_profile_4 = W_aperture.at(3);
    fixed_land_area = Fixed_land_area(total_aperture, Row_Distance, SCAInfoArray,
        csp_dtr_sca_w_profile_1, csp_dtr_sca_w_profile_2, csp_dtr_sca_w_profile_3, csp_dtr_sca_w_profile_4);
    ssc_data_t_set_number(data, "fixed_land_area", fixed_land_area);

    // total_land_area
    ssc_data_t_get_number(data, "non_solar_field_land_area_multiplier", &non_solar_field_land_area_multiplier);
    total_land_area = Total_land_area(fixed_land_area, non_solar_field_land_area_multiplier);
    ssc_data_t_set_number(data, "total_land_area", total_land_area);

    // total_tracking_power
    ssc_data_t_get_number(data, "nSCA", &nSCA);
    ssc_data_t_get_number(data, "SCA_drives_elec", &SCA_drives_elec);
    total_tracking_power = Total_tracking_power(static_cast<int>(nSCA), static_cast<int>(nLoops), SCA_drives_elec);
    ssc_data_t_set_number(data, "total_tracking_power", total_tracking_power);

    // K_cpnt
    K_cpnt = K_Cpnt(static_cast<int>(nSCA));
    ssc_data_t_set_matrix(data, "k_cpnt", K_cpnt);

    // D_cpnt
    D_cpnt = D_Cpnt(static_cast<int>(nSCA));
    ssc_data_t_set_matrix(data, "d_cpnt", D_cpnt);

    // L_cpnt
    L_cpnt = L_Cpnt(static_cast<int>(nSCA));
    ssc_data_t_set_matrix(data, "l_cpnt", L_cpnt);

    // Type_cpnt
    Type_cpnt = Type_Cpnt(static_cast<int>(nSCA));
    ssc_data_t_set_matrix(data, "type_cpnt", Type_cpnt);

    /*
    double x = 1.;
    */
}

void Physical_Trough_Collector_Type_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    // Inputs
    double csp_dtr_sca_length_1, csp_dtr_sca_ncol_per_sca_1,
        csp_dtr_sca_ave_focal_len_1, csp_dtr_sca_piping_dist_1,
        tilt, azimuth,
        nSCA,
        csp_dtr_sca_tracking_error_1, csp_dtr_sca_geometry_effects_1, csp_dtr_sca_clean_reflectivity_1, csp_dtr_sca_mirror_dirt_1, csp_dtr_sca_general_error_1,
        lat;

    // Outputs
    double csp_dtr_sca_ap_length_1,
        csp_dtr_sca_calc_theta,             // singular
        csp_dtr_sca_calc_end_gain_1,
        csp_dtr_sca_calc_zenith,            // singular
        csp_dtr_sca_calc_costh,             // singular
        csp_dtr_sca_calc_end_loss_1,
        csp_dtr_sca_calc_sca_eff_1,
        csp_dtr_sca_calc_latitude,          // singular
        csp_dtr_sca_calc_iam_1;

    util::matrix_t<ssc_number_t> IAMs_1,
        Ave_Focal_Length, Distance_SCA,
        TrackingError, GeomEffects, Rho_mirror_clean, Dirt_mirror, Error;

    // csp_dtr_sca_ap_lengths
    util::matrix_t<ssc_number_t> L_SCA, ColperSCA, csp_dtr_sca_ap_lengths;
    ssc_data_t_get_matrix(vt, "L_SCA", L_SCA);
    ssc_data_t_get_matrix(vt, "ColperSCA", ColperSCA);
    csp_dtr_sca_ap_lengths = Csp_dtr_sca_ap_lengths(L_SCA, ColperSCA);
    ssc_data_t_set_matrix(data, "csp_dtr_sca_ap_lengths", csp_dtr_sca_ap_lengths);

    // csp_dtr_sca_calc_zenith
    ssc_data_t_get_number(data, "lat", &lat);
    csp_dtr_sca_calc_zenith = Csp_dtr_sca_calc_zenith(lat);
    ssc_data_t_set_number(data, "csp_dtr_sca_calc_zenith", csp_dtr_sca_calc_zenith);

    // csp_dtr_sca_calc_costh
    ssc_data_t_get_number(data, "tilt", &tilt);
    ssc_data_t_get_number(data, "azimuth", &azimuth);
    csp_dtr_sca_calc_costh = Csp_dtr_sca_calc_costh(csp_dtr_sca_calc_zenith, tilt, azimuth);
    ssc_data_t_set_number(data, "csp_dtr_sca_calc_costh", csp_dtr_sca_calc_costh);

    // csp_dtr_sca_calc_theta
    csp_dtr_sca_calc_theta = Csp_dtr_sca_calc_theta(csp_dtr_sca_calc_costh);
    ssc_data_t_set_number(data, "csp_dtr_sca_calc_theta", csp_dtr_sca_calc_theta);

    // csp_dtr_sca_calc_end_gain
    util::matrix_t<ssc_number_t> csp_dtr_sca_calc_end_gains;
    ssc_data_t_get_matrix(vt, "Ave_Focal_Length", Ave_Focal_Length);
    ssc_data_t_get_matrix(vt, "Distance_SCA", Distance_SCA);
    csp_dtr_sca_calc_end_gains = Csp_dtr_sca_calc_end_gains(Ave_Focal_Length, csp_dtr_sca_calc_theta, Distance_SCA);
    ssc_data_t_set_matrix(data, "csp_dtr_sca_calc_end_gains", csp_dtr_sca_calc_end_gains);

    // csp_dtr_sca_calc_end_loss
    util::matrix_t<ssc_number_t> csp_dtr_sca_calc_end_losses;
    ssc_data_t_get_number(data, "nSCA", &nSCA);
    csp_dtr_sca_calc_end_losses = Csp_dtr_sca_calc_end_losses(Ave_Focal_Length, csp_dtr_sca_calc_theta, nSCA,
        csp_dtr_sca_calc_end_gains, L_SCA, ColperSCA);
    ssc_data_t_set_matrix(data, "csp_dtr_sca_calc_end_losses", csp_dtr_sca_calc_end_losses);

    // csp_dtr_sca_calc_sca_eff
    util::matrix_t<ssc_number_t> csp_dtr_sca_calc_sca_effs;
    ssc_data_t_get_matrix(vt, "TrackingError", TrackingError);
    ssc_data_t_get_matrix(vt, "GeomEffects", GeomEffects);
    ssc_data_t_get_matrix(vt, "Rho_mirror_clean", Rho_mirror_clean);
    ssc_data_t_get_matrix(vt, "Dirt_mirror", Dirt_mirror);
    ssc_data_t_get_matrix(vt, "Error", Error);
    csp_dtr_sca_calc_sca_effs = Csp_dtr_sca_calc_sca_effs(TrackingError, GeomEffects,
        Rho_mirror_clean, Dirt_mirror, Error);
    ssc_data_t_set_matrix(data, "csp_dtr_sca_calc_sca_effs", csp_dtr_sca_calc_sca_effs);

    // csp_dtr_sca_calc_latitude
    csp_dtr_sca_calc_latitude = Csp_dtr_sca_calc_latitude(lat);
    ssc_data_t_set_number(data, "csp_dtr_sca_calc_latitude", csp_dtr_sca_calc_latitude);

    // csp_dtr_sca_calc_iam
    util::matrix_t<ssc_number_t> IAM_matrix, csp_dtr_sca_calc_iams;
    ssc_data_t_get_matrix(vt, "IAM_matrix", IAM_matrix);
    csp_dtr_sca_calc_iams = Csp_dtr_sca_calc_iams(IAM_matrix, csp_dtr_sca_calc_theta, csp_dtr_sca_calc_costh);
    ssc_data_t_set_matrix(data, "csp_dtr_sca_calc_iams", csp_dtr_sca_calc_iams);
}


void Physical_Trough_Receiver_Type_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    // Inputs
    util::matrix_t<ssc_number_t>
        HCE_FieldFrac,
        Design_loss,
        csp_dtr_hce_var1_field_fractions(4),
        csp_dtr_hce_var1_rated_heat_losses,
        csp_dtr_hce_var2_field_fractions(4),
        csp_dtr_hce_var2_rated_heat_losses,
        csp_dtr_hce_var3_field_fractions(4),
        csp_dtr_hce_var3_rated_heat_losses,
        csp_dtr_hce_var4_field_fractions(4),
        csp_dtr_hce_var4_rated_heat_losses,
        csp_dtr_hce_var1_bellows_shadowings,
        csp_dtr_hce_var1_env_trans,
        csp_dtr_hce_var2_bellows_shadowings,
        csp_dtr_hce_var3_bellows_shadowings,
        csp_dtr_hce_var4_bellows_shadowings,
        csp_dtr_hce_var1_hce_dirts,
        csp_dtr_hce_var2_hce_dirts,
        csp_dtr_hce_var3_hce_dirts,
        csp_dtr_hce_var4_hce_dirts,
        csp_dtr_hce_var2_abs_abs,
        csp_dtr_hce_var2_env_trans,
        csp_dtr_hce_var3_abs_abs,
        csp_dtr_hce_var3_env_trans,
        csp_dtr_hce_var4_env_trans,
        csp_dtr_hce_var1_abs_abs,
        csp_dtr_hce_var4_abs_abs;

    // Outputs
    util::matrix_t<ssc_number_t> csp_dtr_hce_design_heat_losses, csp_dtr_hce_optical_effs;


    // csp_dtr_hce_design_heat_loss_1
    ssc_data_t_get_matrix(vt, "HCE_FieldFrac", HCE_FieldFrac);
    ssc_data_t_get_matrix(vt, "Design_loss", Design_loss);
    csp_dtr_hce_design_heat_losses = Csp_dtr_hce_design_heat_losses(HCE_FieldFrac, Design_loss);
    ssc_data_t_set_matrix(data, "csp_dtr_hce_design_heat_losses", csp_dtr_hce_design_heat_losses);


    // csp_dtr_hce_optical_eff_1
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var1_bellows_shadowings", csp_dtr_hce_var1_bellows_shadowings);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var1_hce_dirts", csp_dtr_hce_var1_hce_dirts);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var1_abs_abs", csp_dtr_hce_var1_abs_abs);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var1_env_trans", csp_dtr_hce_var1_env_trans);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var2_bellows_shadowings", csp_dtr_hce_var2_bellows_shadowings);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var2_hce_dirts", csp_dtr_hce_var2_hce_dirts);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var2_abs_abs", csp_dtr_hce_var2_abs_abs);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var2_env_trans", csp_dtr_hce_var2_env_trans);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var3_bellows_shadowings", csp_dtr_hce_var3_bellows_shadowings);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var3_hce_dirts", csp_dtr_hce_var3_hce_dirts);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var3_abs_abs", csp_dtr_hce_var3_abs_abs);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var3_env_trans", csp_dtr_hce_var3_env_trans);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var4_bellows_shadowings", csp_dtr_hce_var4_bellows_shadowings);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var4_hce_dirts", csp_dtr_hce_var4_hce_dirts);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var4_abs_abs", csp_dtr_hce_var4_abs_abs);
    ssc_data_t_get_matrix(vt, "csp_dtr_hce_var4_env_trans", csp_dtr_hce_var4_env_trans);

    csp_dtr_hce_var1_field_fractions.at(0) = HCE_FieldFrac.at(0, 0);
    csp_dtr_hce_var1_field_fractions.at(1) = HCE_FieldFrac.at(1, 0);
    csp_dtr_hce_var1_field_fractions.at(2) = HCE_FieldFrac.at(2, 0);
    csp_dtr_hce_var1_field_fractions.at(3) = HCE_FieldFrac.at(3, 0);

    csp_dtr_hce_var2_field_fractions.at(0) = HCE_FieldFrac.at(0, 1);
    csp_dtr_hce_var2_field_fractions.at(1) = HCE_FieldFrac.at(1, 1);
    csp_dtr_hce_var2_field_fractions.at(2) = HCE_FieldFrac.at(2, 1);
    csp_dtr_hce_var2_field_fractions.at(3) = HCE_FieldFrac.at(3, 1);

    csp_dtr_hce_var3_field_fractions.at(0) = HCE_FieldFrac.at(0, 2);
    csp_dtr_hce_var3_field_fractions.at(1) = HCE_FieldFrac.at(1, 2);
    csp_dtr_hce_var3_field_fractions.at(2) = HCE_FieldFrac.at(2, 2);
    csp_dtr_hce_var3_field_fractions.at(3) = HCE_FieldFrac.at(3, 2);

    csp_dtr_hce_var4_field_fractions.at(0) = HCE_FieldFrac.at(0, 3);
    csp_dtr_hce_var4_field_fractions.at(1) = HCE_FieldFrac.at(1, 3);
    csp_dtr_hce_var4_field_fractions.at(2) = HCE_FieldFrac.at(2, 3);
    csp_dtr_hce_var4_field_fractions.at(3) = HCE_FieldFrac.at(3, 3);

    csp_dtr_hce_optical_effs = Csp_dtr_hce_optical_effs(
        csp_dtr_hce_var1_field_fractions,
        csp_dtr_hce_var1_bellows_shadowings,
        csp_dtr_hce_var1_hce_dirts,
        csp_dtr_hce_var1_abs_abs,
        csp_dtr_hce_var1_env_trans,
        csp_dtr_hce_var2_field_fractions,
        csp_dtr_hce_var2_bellows_shadowings,
        csp_dtr_hce_var2_hce_dirts,
        csp_dtr_hce_var2_abs_abs,
        csp_dtr_hce_var2_env_trans,
        csp_dtr_hce_var3_field_fractions,
        csp_dtr_hce_var3_bellows_shadowings,
        csp_dtr_hce_var3_hce_dirts,
        csp_dtr_hce_var3_abs_abs,
        csp_dtr_hce_var3_env_trans,
        csp_dtr_hce_var4_field_fractions,
        csp_dtr_hce_var4_bellows_shadowings,
        csp_dtr_hce_var4_hce_dirts,
        csp_dtr_hce_var4_abs_abs,
        csp_dtr_hce_var4_env_trans);
    ssc_data_t_set_matrix(data, "csp_dtr_hce_optical_effs", csp_dtr_hce_optical_effs);
}


void Physical_Trough_System_Control_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    // Inputs
    double is_dispatch, disp_wlim_maxspec, constant;
    util::matrix_t<ssc_number_t> xxx;

    // Outputs
    double is_wlim_series, disp_wlim_max;
    util::matrix_t<ssc_number_t> wlim_series;

    // is_wlim_series
    ssc_data_t_get_number(data, "is_dispatch", &is_dispatch);
    is_wlim_series = Is_wlim_series(is_dispatch);
    ssc_data_t_set_number(data, "is_wlim_series", is_wlim_series);

    // disp_wlim_max
    disp_wlim_maxspec = constant = std::numeric_limits<double>::quiet_NaN();
    ssc_data_t_get_number(data, "disp_wlim_maxspec", &disp_wlim_maxspec);       // if coming from UI
    if (std::isnan(disp_wlim_maxspec)) {
        disp_wlim_maxspec = 1.;                                                 // not passed to LK script
    }
    ssc_data_t_get_number(data, "constant", &constant);                         // if coming from UI
    if (std::isnan(constant)) {
        ssc_data_t_get_number(data, "adjust:constant", &constant);              // if coming from LK script
    }
    disp_wlim_max = Disp_wlim_max(disp_wlim_maxspec, constant);
    ssc_data_t_set_number(data, "disp_wlim_max", disp_wlim_max);

    // wlim_series
    if (!vt->is_assigned("wlim_series")) {
        wlim_series = Wlim_series(disp_wlim_max);
        ssc_data_t_set_array(data, "wlim_series", wlim_series.data(), wlim_series.ncells());
    }

}
