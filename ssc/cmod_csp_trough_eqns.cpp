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
        T_loop_in_des, T_loop_out, fluid,
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
        row_distance, max_collector_width,
        non_solar_field_land_area_multiplier,
        nsca, sca_drives_elec;


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
        required_number_of_loops_for_sm1,
        total_loop_conversion_efficiency,
        total_required_aperture_for_sm1,
        nloops,
        total_aperture,
        field_thermal_output,
        solar_mult,
        q_rec_des,
        tshours_sf,
        fixed_land_area,
        total_land_area,
        total_tracking_power;

    util::matrix_t<ssc_number_t> field_fl_props, trough_loop_control, sca_info_array, sca_defocus_array;

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
    ssc_data_t_get_number(data, "fluid", &fluid);
    ssc_data_t_get_matrix(vt, "field_fl_props", field_fl_props);
    field_htf_cp_avg = Field_htf_cp_avg(T_loop_in_des, T_loop_out, fluid, field_fl_props);      // [kJ/kg-K]
    ssc_data_t_set_number(data, "field_htf_cp_avg", field_htf_cp_avg);

    // single_loop_aperature
    ssc_data_t_get_matrix(vt, "trough_loop_control", trough_loop_control);
    ssc_data_t_get_number(data, "csp_dtr_sca_aperture_1", &csp_dtr_sca_aperture_1);
    ssc_data_t_get_number(data, "csp_dtr_sca_aperture_2", &csp_dtr_sca_aperture_2);
    ssc_data_t_get_number(data, "csp_dtr_sca_aperture_3", &csp_dtr_sca_aperture_3);
    ssc_data_t_get_number(data, "csp_dtr_sca_aperture_4", &csp_dtr_sca_aperture_4);
    single_loop_aperature = Single_loop_aperature(trough_loop_control, csp_dtr_sca_aperture_1,
        csp_dtr_sca_aperture_2, csp_dtr_sca_aperture_3, csp_dtr_sca_aperture_4);
    ssc_data_t_set_number(data, "single_loop_aperature", single_loop_aperature);

    // min_inner_diameter
    ssc_data_t_get_number(data, "csp_dtr_hce_diam_absorber_inner_1", &csp_dtr_hce_diam_absorber_inner_1);
    ssc_data_t_get_number(data, "csp_dtr_hce_diam_absorber_inner_2", &csp_dtr_hce_diam_absorber_inner_2);
    ssc_data_t_get_number(data, "csp_dtr_hce_diam_absorber_inner_3", &csp_dtr_hce_diam_absorber_inner_3);
    ssc_data_t_get_number(data, "csp_dtr_hce_diam_absorber_inner_4", &csp_dtr_hce_diam_absorber_inner_4);
    min_inner_diameter = Min_inner_diameter(trough_loop_control, csp_dtr_hce_diam_absorber_inner_1,
        csp_dtr_hce_diam_absorber_inner_2, csp_dtr_hce_diam_absorber_inner_3, csp_dtr_hce_diam_absorber_inner_4);
    ssc_data_t_set_number(data, "min_inner_diameter", min_inner_diameter);

    // cspdtr_loop_hce_heat_loss
    ssc_data_t_get_number(data, "I_bn_des", &I_bn_des);
    ssc_data_t_get_number(data, "csp_dtr_hce_design_heat_loss_1", &csp_dtr_hce_design_heat_loss_1);
    ssc_data_t_get_number(data, "csp_dtr_hce_design_heat_loss_2", &csp_dtr_hce_design_heat_loss_2);
    ssc_data_t_get_number(data, "csp_dtr_hce_design_heat_loss_3", &csp_dtr_hce_design_heat_loss_3);
    ssc_data_t_get_number(data, "csp_dtr_hce_design_heat_loss_4", &csp_dtr_hce_design_heat_loss_4);
    ssc_data_t_get_number(data, "csp_dtr_sca_length_1", &csp_dtr_sca_length_1);
    ssc_data_t_get_number(data, "csp_dtr_sca_length_2", &csp_dtr_sca_length_2);
    ssc_data_t_get_number(data, "csp_dtr_sca_length_3", &csp_dtr_sca_length_3);
    ssc_data_t_get_number(data, "csp_dtr_sca_length_4", &csp_dtr_sca_length_4);
    cspdtr_loop_hce_heat_loss = Cspdtr_loop_hce_heat_loss(trough_loop_control, I_bn_des,
        csp_dtr_hce_design_heat_loss_1, csp_dtr_hce_design_heat_loss_2,
        csp_dtr_hce_design_heat_loss_3, csp_dtr_hce_design_heat_loss_4,
        csp_dtr_sca_length_1, csp_dtr_sca_length_2, csp_dtr_sca_length_3, csp_dtr_sca_length_4,
        csp_dtr_sca_aperture_1, csp_dtr_sca_aperture_2, csp_dtr_sca_aperture_3, csp_dtr_sca_aperture_4);
    ssc_data_t_set_number(data, "cspdtr_loop_hce_heat_loss", cspdtr_loop_hce_heat_loss);

    // loop_optical_efficiency
    ssc_data_t_get_number(data, "csp_dtr_sca_calc_sca_eff_1", &csp_dtr_sca_calc_sca_eff_1);
    ssc_data_t_get_number(data, "csp_dtr_sca_calc_sca_eff_2", &csp_dtr_sca_calc_sca_eff_2);
    ssc_data_t_get_number(data, "csp_dtr_sca_calc_sca_eff_3", &csp_dtr_sca_calc_sca_eff_3);
    ssc_data_t_get_number(data, "csp_dtr_sca_calc_sca_eff_4", &csp_dtr_sca_calc_sca_eff_4);
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
    ssc_data_t_get_matrix(vt, "trough_loop_control", trough_loop_control);
    sca_info_array = Sca_info_array(trough_loop_control);
    ssc_data_t_set_matrix(data, "sca_info_array", sca_info_array);
    
    // sca_defocus_array
    sca_defocus_array = Sca_defocus_array(trough_loop_control);
    ssc_data_t_set_array(data, "sca_defocus_array", sca_defocus_array.data(), sca_defocus_array.ncells());

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

    // total_required_aperture_for_sm1
    total_required_aperture_for_sm1 = Total_required_aperture_for_sm1(q_pb_design, I_bn_des, total_loop_conversion_efficiency);
    ssc_data_t_set_number(data, "total_required_aperture_for_sm1", total_required_aperture_for_sm1);

    // required_number_of_loops_for_SM1
    required_number_of_loops_for_sm1 = Required_number_of_loops_for_SM1(total_required_aperture_for_sm1, single_loop_aperature);
    ssc_data_t_set_number(data, "required_number_of_loops_for_SM1", required_number_of_loops_for_sm1);

    // nloops
    ssc_data_t_get_number(data, "radio_sm_or_area", &radio_sm_or_area);
    ssc_data_t_get_number(data, "specified_solar_multiple", &specified_solar_multiple);
    ssc_data_t_get_number(data, "specified_total_aperture", &specified_total_aperture);
    nloops = Nloops(static_cast<int>(radio_sm_or_area), specified_solar_multiple, total_required_aperture_for_sm1, specified_total_aperture, single_loop_aperature);
    ssc_data_t_set_number(data, "nloops", nloops);

    /*
    // total_aperture
    total_aperture = Total_aperture(single_loop_aperature, nloops);
    ssc_data_t_set_number(data, "total_aperture", total_aperture);

    // field_thermal_output
    field_thermal_output = Field_thermal_output(I_bn_des, total_loop_conversion_efficiency, total_aperture);
    ssc_data_t_set_number(data, "field_thermal_output", field_thermal_output);

    // solar_mult
    solar_mult = Solar_mult(static_cast<int>(radio_sm_or_area), specified_solar_multiple, total_aperture, total_required_aperture_for_sm1);
    ssc_data_t_set_number(data, "solar_mult", solar_mult);

    // q_rec_des
    q_rec_des = Q_rec_des(solar_mult, q_pb_design);
    ssc_data_t_set_number(data, "q_rec_des", q_rec_des);

    // tshours_sf
    ssc_data_t_get_number(data, "tshours", &tshours);
    tshours_sf = Tshours_sf(tshours, solar_mult);
    ssc_data_t_set_number(data, "tshours_sf", tshours_sf);

    // fixed_land_area
    ssc_data_t_get_number(data, "row_distance", &row_distance);
    ssc_data_t_get_number(data, "max_collector_width", &max_collector_width);
    fixed_land_area = Fixed_land_area(total_aperture, row_distance, max_collector_width);
    ssc_data_t_set_number(data, "fixed_land_area", fixed_land_area);

    // total_land_area
    ssc_data_t_get_number(data, "non_solar_field_land_area_multiplier", &non_solar_field_land_area_multiplier);
    total_land_area = Total_land_area(fixed_land_area, non_solar_field_land_area_multiplier);
    ssc_data_t_set_number(data, "total_land_area", total_land_area);

    // total_tracking_power
    ssc_data_t_get_number(data, "nsca", &nsca);
    ssc_data_t_get_number(data, "sca_drives_elec", &sca_drives_elec);
    total_tracking_power = Total_tracking_power(static_cast<int>(nsca), static_cast<int>(nloops), sca_drives_elec);
    ssc_data_t_set_number(data, "total_tracking_power", total_tracking_power);


    double x = 1.;
    */
}
