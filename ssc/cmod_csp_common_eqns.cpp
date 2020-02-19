#include <cmath>
#include <math.h>
#include "cmod_csp_common_eqns.h"
#include "vartab.h"

#pragma warning(disable: 4297)  // ignore warning: 'function assumed not to throw an exception but does'


HTFProperties GetHtfProperties(int fluid_number, const util::matrix_t<double> &specified_fluid_properties) {       // [-]

    HTFProperties htf_properties;

    if (fluid_number != HTFProperties::User_defined)
    {
        if (!htf_properties.SetFluid(fluid_number))
        {
            throw("Fluid number is not recognized");
        }
    }
    else if (fluid_number == HTFProperties::User_defined)
    {
        std::size_t n_rows = specified_fluid_properties.nrows();
        std::size_t n_cols = specified_fluid_properties.ncols();
        if (n_rows > 2 && n_cols == 7)
        {
            if (!htf_properties.SetUserDefinedFluid(specified_fluid_properties))
            {
                std::string error_msg = util::format(htf_properties.UserFluidErrMessage(), n_rows, n_cols);
                throw(error_msg);
            }
        }
        else
        {
            std::string error_msg = util::format("The user defined fluid properties table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
            throw(error_msg);
        }
    }
    else
    {
        throw("Fluid code is not recognized");
    }

    return htf_properties;
}



// Originally from 'MSPT System Design' UI form
double Nameplate(double P_ref /*MWe*/, double gross_net_conversion_factor /*-*/) {      // MWe
    return P_ref * gross_net_conversion_factor;
}

double Q_pb_design(double P_ref /*MWe*/, double design_eff /*-*/) {     // MWt
    return P_ref / design_eff;
}

double Q_rec_des(double solarm /*-*/, double q_pb_design /*MWt*/) {     // MWt
    return solarm * q_pb_design;
}

double Tshours_sf(double tshours /*hr*/, double solarm /*-*/) {         // hr
    return tshours / solarm;
}



// Originally from 'Tower SolarPilot Solar Field' UI Form
double Land_max_calc(double land_max /*-*/, double h_tower /*m*/) {      // [m]
    return land_max * h_tower;
}

double N_hel(const util::matrix_t<ssc_number_t> &helio_positions /*m*/) {      // [-]
    return helio_positions.nrows();
}

double Csp_pt_sf_heliostat_area(double helio_height /*m*/, double helio_width /*m*/, double dens_mirror /*-*/) {     // [m2]
    return helio_height * helio_width * dens_mirror;
}

double Csp_pt_sf_total_reflective_area(int n_hel /*-*/, double csp_pt_sf_heliostat_area /*m2*/) {     // [m2]
    return n_hel * csp_pt_sf_heliostat_area;
}

double Land_min_calc(double land_min /*-*/, double h_tower /*m*/) {      // [m]
    return land_min * h_tower;
}

double Csp_pt_sf_total_land_area(double csp_pt_sf_fixed_land_area /*acres*/, double land_area_base /*acres*/,
    double csp_pt_sf_land_overhead_factor /*-*/) {       // [acres]
    
    return csp_pt_sf_fixed_land_area + land_area_base * csp_pt_sf_land_overhead_factor;
}

double A_sf_UI(double helio_width /*m*/, double helio_height /*m*/, double dens_mirror /*-*/, int n_hel /*-*/) {  // [m2]
    return helio_width * helio_height * dens_mirror * n_hel;
}

double Helio_area_tot(double A_sf_UI /*m2*/) {     // [m2]
    return A_sf_UI;
}

double Csp_pt_sf_tower_height(double h_tower /*m*/) {        // [m]
    return h_tower;
}

double C_atm_info(const util::matrix_t<ssc_number_t> &helio_positions /*m*/,
    double c_atm_0 /*-*/, double c_atm_1 /*-*/, double c_atm_2 /*-*/, double c_atm_3 /*-*/, double h_tower /*m*/) {  // [%]
    
    double tht2 = h_tower * h_tower;
    std::size_t n_hel = helio_positions.nrows();

    double tot_att = 0.;
    for (std::size_t i = 0; i < n_hel; i++) {
        double x = helio_positions.at(i, 0);
        double y = helio_positions.at(i, 1);
        double r = std::sqrt(x*x + y*y);
        double r2 = r*r;

        double s = std::sqrt(tht2 + r2) * 0.001;    // [km]
        double s2 = s*s;
        double s3 = s2*s;

        tot_att += c_atm_0 + c_atm_1*s + c_atm_2*s2 + c_atm_3*s3;
    }

    return 100. * tot_att / n_hel;
}

double Error_equiv(double helio_optical_error_mrad /*mrad*/) {       // [mrad]
    return std::sqrt(2. * helio_optical_error_mrad * 2. * helio_optical_error_mrad * 2.);
}

int Is_optimize(int override_opt /*-*/) {      // [-]
    if (override_opt == 1) {
        return 1;
    }
    else {
        return 0;
    }
}

double Field_model_type(int is_optimize /*-*/, int override_layout /*-*/) {      // [-]
    if (is_optimize == 1) {
        return 0;
    }
    else if (override_layout) {
        return 1;
    }
    else {
        return 2;
    }
}

double Q_design(double Q_rec_des /*MWt*/) {      // [MWt]
    return Q_rec_des;
}

double Dni_des_calc(double dni_des /*W/m2*/) {       // [W/m2]
    return dni_des;
}

int Opt_algorithm() {        // [-]
    return 1;
}

double Opt_flux_penalty() {  // [-]
    return 0.25;
}



// Originally from 'MSPT Receiver' UI Form
double Csp_pt_rec_cav_lip_height() {     // [m]
    return 1.;
}

double Csp_pt_rec_cav_panel_height() {   // [m]
    return 1.1;
}

double Csp_pt_rec_htf_t_avg(double T_htf_cold_des /*C*/, double T_htf_hot_des /*C*/) {       // [C]
    return (T_htf_cold_des + T_htf_hot_des) / 2.;
}

double Csp_pt_rec_htf_c_avg(double csp_pt_rec_htf_t_avg /*C*/, int rec_htf /*-*/,
    const util::matrix_t<ssc_number_t> &field_fl_props /*-*/) {      // [kJ/kg-K]
    
    HTFProperties htf_properties = GetHtfProperties(rec_htf, field_fl_props);
    return htf_properties.Cp(csp_pt_rec_htf_t_avg + 273.15);
}

double Csp_pt_rec_max_flow_to_rec(double csp_pt_rec_max_oper_frac /*-*/, double Q_rec_des /*MWt*/,
    double csp_pt_rec_htf_c_avg /*kJ/kg-K*/, double T_htf_hot_des /*C*/, double T_htf_cold_des /*C*/) {      // [kg/s]

    return (csp_pt_rec_max_oper_frac * Q_rec_des * 1.e6) /
        (csp_pt_rec_htf_c_avg * 1.e3 * (T_htf_hot_des - T_htf_cold_des));
}

double Csp_pt_rec_cav_ap_height(double rec_d_spec /*m*/, double csp_pt_rec_cav_ap_hw_ratio /*-*/) {      // [m]
    return rec_d_spec * csp_pt_rec_cav_ap_hw_ratio;
}

double Rec_aspect(double D_rec /*m*/, double rec_height /*m*/) {     // [-]
    double aspect;
    if (D_rec != 0.) {
        aspect = rec_height / D_rec;
    }
    else {
        aspect = 1.;
    }

    return aspect;
}

double Piping_length(double h_tower /*m*/, double piping_length_mult /*-*/, double piping_length_const /*m*/) {      // [m]
    return h_tower * piping_length_mult + piping_length_const;
}

double Piping_loss_tot(double piping_length /*m*/, double piping_loss /*Wt/m*/) {        // [kWt]
    return piping_length * piping_loss / 1000.;
}



// Originally from 'MSPT System Control'
double Csp_pt_par_calc_bop(double bop_par /*MWe/MWcap*/, double bop_par_f /*-*/, double bop_par_0 /*-*/,
    double bop_par_1 /*-*/, double bop_par_2 /*-*/, double p_ref /*MWe*/) {      // [MWe]

    return bop_par * bop_par_f * ( bop_par_0 + bop_par_1 + bop_par_2 ) * p_ref;
}

double Csp_pt_par_calc_aux(double aux_par /*MWe/MWcap*/, double aux_par_f /*-*/, double aux_par_0 /*-*/,
    double aux_par_1 /*-*/, double aux_par_2 /*-*/, double p_ref /*MWe*/) {      // [MWe]

    return aux_par * aux_par_f * (aux_par_0 + aux_par_1 + aux_par_2) * p_ref;
}

double Disp_wlim_max(double disp_wlim_maxspec /**/, double constant /*%*/) {        // [MWe]
    return disp_wlim_maxspec * (1. - constant / 100.);
}

util::matrix_t<double> Wlim_series(double disp_wlim_max /*MWe*/) {    // [kWe]
    const int kHoursInYear = 8760;

    double disp_wlim_max_kW = disp_wlim_max * 1000.;
    util::matrix_t<double> wlim_series(1, kHoursInYear, disp_wlim_max_kW);

    return wlim_series;
}
