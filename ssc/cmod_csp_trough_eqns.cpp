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

/*
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

    double T_loop_in_des, T_loop_out, fluid, field_htf_cp_avg,
        m_dot_htfmax, fluid_dens_outlet_temp, min_inner_diameter, max_field_flow_velocity,
        m_dot_htfmin, fluid_dens_inlet_temp, min_field_flow_velocity;

    util::matrix_t<double> field_fl_props;

    // field_htf_cp_avg
    ssc_data_t_get_number(data, "T_loop_in_des", &T_loop_in_des);
    ssc_data_t_get_number(data, "T_loop_out", &T_loop_out);
    ssc_data_t_get_number(data, "fluid", &fluid);
    ssc_data_t_get_matrix(vt, "field_fl_props", field_fl_props);
    field_htf_cp_avg = Field_htf_cp_avg(T_loop_in_des, T_loop_out, fluid, field_fl_props);      // [kJ/kg-K]
    ssc_data_t_set_number(data, "field_htf_cp_avg", field_htf_cp_avg);

    // max_field_flow_velocity
    ssc_data_t_get_number(data, "m_dot_htfmax", &m_dot_htfmax);
    ssc_data_t_get_number(data, "fluid_dens_outlet_temp", &fluid_dens_outlet_temp);
    ssc_data_t_get_number(data, "min_inner_diameter", &min_inner_diameter);
    max_field_flow_velocity = Max_field_flow_velocity(m_dot_htfmax, fluid_dens_outlet_temp, min_inner_diameter);
    ssc_data_t_set_number(data, "max_field_flow_velocity", max_field_flow_velocity);

    // min_field_flow_velocity
    ssc_data_t_get_number(data, "m_dot_htfmin", &m_dot_htfmin);
    ssc_data_t_get_number(data, "fluid_dens_inlet_temp", &fluid_dens_inlet_temp);
    min_field_flow_velocity = Min_field_flow_velocity(m_dot_htfmin, fluid_dens_inlet_temp, min_inner_diameter);
    ssc_data_t_set_number(data, "min_field_flow_velocity", min_field_flow_velocity);


    double x = 1.;
}
