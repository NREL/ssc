#include <cmath>
#include <math.h>
#include <cmath>
#include <algorithm>
#include "cmod_csp_common_eqns.h"
#include "vartab.h"
#include "csp_system_costs.h"

#pragma warning(disable: 4297)  // ignore warning: 'function assumed not to throw an exception but does'


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

SSCEXPORT ssc_number_t *ssc_data_t_get_array(ssc_data_t p_data, const char* name, int* length)
{
    ssc_number_t* data;
    data = ssc_data_get_array(p_data, name, length);
    if (data == 0) {
        // replace any periods in the name with underscores in order to read variables set by the UI
        std::string str_name(name);
        size_t n_replaced = util::replace(str_name, ".", "_");
        if (n_replaced > 0) {
            data = ssc_data_get_array(p_data, str_name.c_str(), length);
        }
    }

    return data;
}

SSCEXPORT void ssc_data_t_set_array(ssc_data_t p_data, const char* name, ssc_number_t* pvalues, int length)
{
    ssc_data_set_array(p_data, name, pvalues, length);

    // replace any periods in the name with underscores so UI equations can read value
    std::string str_name(name);
    size_t n_replaced = util::replace(str_name, ".", "_");
    if (n_replaced > 0) {
        ssc_data_set_array(p_data, str_name.c_str(), pvalues, length);
    }
}

SSCEXPORT void ssc_data_t_get_matrix(var_table* vt, std::string name, util::matrix_t<double>& matrix)
{
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

SSCEXPORT void ssc_data_t_set_matrix(ssc_data_t data, const std::string& name, const var_data& val)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    vt->assign(name, val);

    // replace any periods in the name with underscores so UI equations can read value
    std::string str_name(name);
    size_t n_replaced = util::replace(str_name, ".", "_");
    if (n_replaced > 0) {
        vt->assign(str_name.c_str(), val);
    }
}


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




/////////////////////////////////////////////////////////////////////////////////////////////
// Power Tower //////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////

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

int N_hel(const util::matrix_t<ssc_number_t> &helio_positions /*m*/) {      // [-]
    return static_cast<int>(helio_positions.nrows());
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

bool Is_optimize(bool override_opt /*-*/) {      // [-]
    if (override_opt) {
        return true;
    }
    else {
        return false;
    }
}

int Field_model_type(bool is_optimize /*-*/, bool override_layout /*-*/, int assigned_field_model_type /*-*/) {      // [-]
    if (is_optimize) {
        return 0;
    }
    else if (override_layout) {
        return 1;
    }
    else if (assigned_field_model_type >= 0) {       // if valid
        return assigned_field_model_type;
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





// Originally from 'Tower SolarPilot Capital Costs'
//double Ui_tower_height(TowerTypes tower_type, double height) {
//
//}

double Csp_pt_cost_receiver_area(TowerTypes tower_type /*-*/, double d_rec /*m*/, double rec_height /*m*/,
    int receiver_type /*-*/, double rec_d_spec /*m*/, double csp_pt_rec_cav_ap_height /*m*/) {      // [m2]

    double area = std::numeric_limits<double>::quiet_NaN();

    if (tower_type == TowerTypes::kMoltenSalt || tower_type == TowerTypes::kIscc) {
        switch (receiver_type) {
        case 0:
            area = rec_height * d_rec * M_PI;
            break;
        case 1:
            area = rec_d_spec * csp_pt_rec_cav_ap_height;
            break;
        default:
            throw std::runtime_error("Receiver type not supported.");
        }
    }
    else if (tower_type == TowerTypes::kDirectSteam) {
        area = d_rec * rec_height * M_PI;
    }

    return area;
}

double Csp_pt_cost_storage_mwht(TowerTypes tower_type /*-*/, double p_ref /*MWe*/, double design_eff /*-*/,
    double tshours /*hr*/) {      // [MWht]

    double nameplate = std::numeric_limits<double>::quiet_NaN();

    if (tower_type == TowerTypes::kMoltenSalt) {
        nameplate = p_ref / design_eff * tshours;
    }
    else {
        nameplate = 0.;
    }

    return nameplate;
}

double Csp_pt_cost_power_block_mwe(TowerTypes tower_type /*-*/, double p_ref /*MWe*/, double demand_var /*MWe*/)       // [MWe]
{
    double pb = std::numeric_limits<double>::quiet_NaN();

    if (tower_type == TowerTypes::kMoltenSalt) {
        pb = p_ref;
    }
    else {
        pb = demand_var;
    }

    return pb;
}

void Tower_SolarPilot_Capital_Costs_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }

    C_mspt_system_costs sys_costs;

    //sys_costs.ms_par.A_sf_refl = as_double("A_sf");
    ssc_data_t_get_number(data, "a_sf_ui", &sys_costs.ms_par.A_sf_refl);
    ssc_data_t_get_number(data, "site_spec_cost", &sys_costs.ms_par.site_improv_spec_cost);
    ssc_data_t_get_number(data, "heliostat_spec_cost", &sys_costs.ms_par.heliostat_spec_cost);
    ssc_data_t_get_number(data, "cost_sf_fixed", &sys_costs.ms_par.heliostat_fixed_cost);
    ssc_data_t_get_number(data, "h_tower", &sys_costs.ms_par.h_tower);                            // set different for other techs
    ssc_data_t_get_number(data, "rec_height", &sys_costs.ms_par.h_rec);
    ssc_data_t_get_number(data, "helio_height", &sys_costs.ms_par.h_helio);
    ssc_data_t_get_number(data, "tower_fixed_cost", &sys_costs.ms_par.tower_fixed_cost);
    ssc_data_t_get_number(data, "tower_exp", &sys_costs.ms_par.tower_cost_scaling_exp);
    ssc_data_t_get_number(data, "csp.pt.cost.receiver.area", &sys_costs.ms_par.A_rec);            // calculation specific to each tech
    ssc_data_t_get_number(data, "rec_ref_cost", &sys_costs.ms_par.rec_ref_cost);
    ssc_data_t_get_number(data, "rec_ref_area", &sys_costs.ms_par.A_rec_ref);
    ssc_data_t_get_number(data, "rec_cost_exp", &sys_costs.ms_par.rec_cost_scaling_exp);
    ssc_data_t_get_number(data, "csp.pt.cost.storage_mwht", &sys_costs.ms_par.Q_storage);         // calculation specific to each tech
    ssc_data_t_get_number(data, "tes_spec_cost", &sys_costs.ms_par.tes_spec_cost);
    ssc_data_t_get_number(data, "csp.pt.cost.power_block_mwe", &sys_costs.ms_par.W_dot_design);   // calculation specific to each tech
    ssc_data_t_get_number(data, "plant_spec_cost", &sys_costs.ms_par.power_cycle_spec_cost);
    ssc_data_t_get_number(data, "bop_spec_cost", &sys_costs.ms_par.bop_spec_cost);
    ssc_data_t_get_number(data, "fossil_spec_cost", &sys_costs.ms_par.fossil_backup_spec_cost);
    ssc_data_t_get_number(data, "contingency_rate", &sys_costs.ms_par.contingency_rate);
    ssc_data_t_get_number(data, "csp.pt.sf.total_land_area", &sys_costs.ms_par.total_land_area);
    ssc_data_t_get_number(data, "nameplate", &sys_costs.ms_par.plant_net_capacity);
    ssc_data_t_get_number(data, "csp.pt.cost.epc.per_acre", &sys_costs.ms_par.EPC_land_spec_cost);
    ssc_data_t_get_number(data, "csp.pt.cost.epc.percent", &sys_costs.ms_par.EPC_land_perc_direct_cost);
    ssc_data_t_get_number(data, "csp.pt.cost.epc.per_watt", &sys_costs.ms_par.EPC_land_per_power_cost);
    ssc_data_t_get_number(data, "csp.pt.cost.epc.fixed", &sys_costs.ms_par.EPC_land_fixed_cost);
    ssc_data_t_get_number(data, "land_spec_cost", &sys_costs.ms_par.total_land_spec_cost);
    ssc_data_t_get_number(data, "csp.pt.cost.plm.percent", &sys_costs.ms_par.total_land_perc_direct_cost);
    ssc_data_t_get_number(data, "csp.pt.cost.plm.per_watt", &sys_costs.ms_par.total_land_per_power_cost);
    ssc_data_t_get_number(data, "csp.pt.cost.plm.fixed", &sys_costs.ms_par.total_land_fixed_cost);
    ssc_data_t_get_number(data, "sales_tax_frac", &sys_costs.ms_par.sales_tax_basis);
    ssc_data_t_get_number(data, "sales_tax_rate", &sys_costs.ms_par.sales_tax_rate);

    try
    {
        sys_costs.calculate_costs();
    }
    catch (...)
    {
        throw std::runtime_error("MSPT system costs. System cost calculations failed. Check that all inputs are properly defined");
    }

    ssc_data_t_set_number(data, "csp.pt.cost.site_improvements", (ssc_number_t)sys_costs.ms_out.site_improvement_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.heliostats", (ssc_number_t)sys_costs.ms_out.heliostat_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.tower", (ssc_number_t)sys_costs.ms_out.tower_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.receiver", (ssc_number_t)sys_costs.ms_out.receiver_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.storage", (ssc_number_t)sys_costs.ms_out.tes_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.power_block", (ssc_number_t)sys_costs.ms_out.power_cycle_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.bop", (ssc_number_t)sys_costs.ms_out.bop_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.fossil", (ssc_number_t)sys_costs.ms_out.fossil_backup_cost);
    ssc_data_t_set_number(data, "ui_direct_subtotal", (ssc_number_t)sys_costs.ms_out.direct_capital_precontingency_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.contingency", (ssc_number_t)sys_costs.ms_out.contingency_cost);
    ssc_data_t_set_number(data, "total_direct_cost", (ssc_number_t)sys_costs.ms_out.total_direct_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.epc.total", (ssc_number_t)sys_costs.ms_out.epc_and_owner_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.plm.total", (ssc_number_t)sys_costs.ms_out.total_land_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.sales_tax.total", (ssc_number_t)sys_costs.ms_out.sales_tax_cost);
    ssc_data_t_set_number(data, "total_indirect_cost", (ssc_number_t)sys_costs.ms_out.total_indirect_cost);
    ssc_data_t_set_number(data, "total_installed_cost", (ssc_number_t)sys_costs.ms_out.total_installed_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.installed_per_capacity", (ssc_number_t)sys_costs.ms_out.estimated_installed_cost_per_cap);
}



/////////////////////////////////////////////////////////////////////////////////////////////
// Physical Trough //////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////


double Solar_mult(int radio_sm_or_area, double specified_solar_multiple, double total_aperture, double total_required_aperture_for_SM1)
{
    double solar_mult = std::numeric_limits<double>::quiet_NaN();

    if (radio_sm_or_area == 0) {
        solar_mult = specified_solar_multiple;
    }
    else if (radio_sm_or_area == 1) {
        solar_mult = total_aperture / total_required_aperture_for_SM1;
    }
    else {
        throw std::runtime_error("Physical Trough. Solar multiple calculation failed, invalid option.");
    }

    return solar_mult;
}

double Max_field_flow_velocity(double m_dot_htfmax, double fluid_dens_outlet_temp, double min_inner_diameter)
{
    return m_dot_htfmax * 4 / (fluid_dens_outlet_temp * M_PI *
        min_inner_diameter * min_inner_diameter);
}

double Min_field_flow_velocity(double m_dot_htfmin, double fluid_dens_inlet_temp, double min_inner_diameter)
{
    return m_dot_htfmin * 4 / (fluid_dens_inlet_temp * M_PI *
        min_inner_diameter * min_inner_diameter);
}

double Field_htf_cp_avg(double T_in /*C*/, double T_out /*C*/, int rec_htf /*-*/,
    const util::matrix_t<ssc_number_t>& field_fl_props /*-*/)      // [kJ/kg-K]
{
    double T_avg = 0.5 * (T_in + T_out);
    HTFProperties htf_properties = GetHtfProperties(rec_htf, field_fl_props);
    return htf_properties.Cp(T_avg + 273.15);
}

double Min_inner_diameter(const util::matrix_t<ssc_number_t>& trough_loop_control,
    double csp_dtr_hce_diam_absorber_inner_1, double csp_dtr_hce_diam_absorber_inner_2,
    double csp_dtr_hce_diam_absorber_inner_3, double csp_dtr_hce_diam_absorber_inner_4)
{
    //std::vector<ssc_number_t> trough_loop_control{ 8, 1, 1, 8, 1, 1, 7, 1, 1, 6, 1, 1, 5, 1, 1, 4, 1, 1, 3, 1, 1, 2, 1, 1, 1 };
    std::vector<double> diam_inputs(4, std::numeric_limits<double>::quiet_NaN());
    diam_inputs[0] = csp_dtr_hce_diam_absorber_inner_1;
    diam_inputs[1] = csp_dtr_hce_diam_absorber_inner_2;
    diam_inputs[2] = csp_dtr_hce_diam_absorber_inner_3;
    diam_inputs[3] = csp_dtr_hce_diam_absorber_inner_4;

    double minval = diam_inputs[0];
    int hce_t = -1;
    for (int i = 0; i < static_cast<int>(trough_loop_control.at(0)); i++)
    {
        hce_t = std::min(std::max(static_cast<int>(trough_loop_control.at(i * 3 + 2)), 1), 4) - 1;
        if (diam_inputs[hce_t] < minval) {
            minval = diam_inputs[hce_t];
        }
    }

    return minval;
}

double Single_loop_aperature(const util::matrix_t<ssc_number_t>& trough_loop_control,
    double csp_dtr_sca_aperture_1, double csp_dtr_sca_aperture_2,
    double csp_dtr_sca_aperture_3, double csp_dtr_sca_aperture_4)
{
    std::vector<double> sca_ap(4, std::numeric_limits<double>::quiet_NaN());

    int nsca = static_cast<int>(trough_loop_control.at(0));
    sca_ap[0] = csp_dtr_sca_aperture_1;
    sca_ap[1] = csp_dtr_sca_aperture_2;
    sca_ap[2] = csp_dtr_sca_aperture_3;
    sca_ap[3] = csp_dtr_sca_aperture_4;

    double total_ap = 0.;
    int sca_t = -1;
    for (int i = 0; i < nsca; i++)
    {
        sca_t = std::min(std::max(static_cast<int>(trough_loop_control.at(1 + i * 3)), 1), 4) - 1;
        total_ap = total_ap + sca_ap[sca_t];
    }

    return total_ap;
}

double Cspdtr_loop_hce_heat_loss(const util::matrix_t<ssc_number_t>& trough_loop_control, double I_bn_des,
    double csp_dtr_hce_design_heat_loss_1, double csp_dtr_hce_design_heat_loss_2,
    double csp_dtr_hce_design_heat_loss_3, double csp_dtr_hce_design_heat_loss_4,
    double csp_dtr_sca_length_1, double csp_dtr_sca_length_2, double csp_dtr_sca_length_3, double csp_dtr_sca_length_4,
    double csp_dtr_sca_aperture_1, double csp_dtr_sca_aperture_2, double csp_dtr_sca_aperture_3, double csp_dtr_sca_aperture_4)
{
    std::vector<double> hce_hl(4, std::numeric_limits<double>::quiet_NaN());
    std::vector<double> sca_len(4, std::numeric_limits<double>::quiet_NaN());
    std::vector<double> sca_ap(4, std::numeric_limits<double>::quiet_NaN());
    int ncol = static_cast<int>(trough_loop_control.at(0));
    double total_len = 0.;

    hce_hl[0] = csp_dtr_hce_design_heat_loss_1;
    hce_hl[1] = csp_dtr_hce_design_heat_loss_2;
    hce_hl[2] = csp_dtr_hce_design_heat_loss_3;
    hce_hl[3] = csp_dtr_hce_design_heat_loss_4;

    sca_len[0] = csp_dtr_sca_length_1;
    sca_len[1] = csp_dtr_sca_length_2;
    sca_len[2] = csp_dtr_sca_length_3;
    sca_len[3] = csp_dtr_sca_length_4;

    sca_ap[0] = csp_dtr_sca_aperture_1;
    sca_ap[1] = csp_dtr_sca_aperture_2;
    sca_ap[2] = csp_dtr_sca_aperture_3;
    sca_ap[3] = csp_dtr_sca_aperture_4;

    double derate = 0.;
    for (int i = 0; i < ncol; i++)
    {
        int sca_t = std::min(std::max(static_cast<int>(trough_loop_control.at(1 + i * 3)), 1), 4) - 1;
        int hce_t = std::min(std::max(static_cast<int>(trough_loop_control.at(2 + i * 3)), 1), 4) - 1;
        total_len = total_len + sca_len[sca_t];
        derate = derate + sca_len[sca_t] * (1 - (hce_hl[hce_t] / (I_bn_des * sca_ap[sca_t] / sca_len[sca_t])));
    }

    if (total_len != 0.0) {
        derate = derate / total_len;
    }
    else {
        derate = -777.7;
    }

    return derate;
}

double Nloops(int radio_sm_or_area, double specified_solar_multiple, double total_required_aperture_for_SM1,
    double specified_total_aperture, double single_loop_aperture)
{
    double total_aperture = std::numeric_limits<double>::quiet_NaN();
    double n_loops = std::numeric_limits<double>::quiet_NaN();

    if (radio_sm_or_area == 0) {
        total_aperture = specified_solar_multiple * total_required_aperture_for_SM1;
    }
    else if (radio_sm_or_area == 1) {
        total_aperture = specified_total_aperture;
    }
    else {
        throw std::runtime_error("Physical Trough. Number of loops calculation failed, invalid option.");
    }

    n_loops = std::ceil(total_aperture / single_loop_aperture);
    return n_loops;
}

double Total_aperture(double single_loop_aperature, double nloops)
{
    return single_loop_aperature * nloops;
};

double Required_number_of_loops_for_SM1(double total_required_aperture_for_SM1, double single_loop_aperature)
{
    return std::ceil(total_required_aperture_for_SM1 / single_loop_aperature);
};

double Loop_optical_efficiency(const util::matrix_t<ssc_number_t>& trough_loop_control,
    double csp_dtr_sca_calc_sca_eff_1, double csp_dtr_sca_calc_sca_eff_2,
    double csp_dtr_sca_calc_sca_eff_3, double csp_dtr_sca_calc_sca_eff_4,
    double csp_dtr_sca_length_1, double csp_dtr_sca_length_2, double csp_dtr_sca_length_3, double csp_dtr_sca_length_4,
    double csp_dtr_hce_optical_eff_1, double csp_dtr_hce_optical_eff_2,
    double csp_dtr_hce_optical_eff_3, double csp_dtr_hce_optical_eff_4)
{
    std::vector<double> sca_eff(4, std::numeric_limits<double>::quiet_NaN());
    std::vector<double> sca_len(4, std::numeric_limits<double>::quiet_NaN());
    std::vector<double> hce_eff(4, std::numeric_limits<double>::quiet_NaN());
    int ncol = static_cast<int>(trough_loop_control.at(0));

    if (trough_loop_control.ncells() != ncol * 3 + 1) {
        return -888.8;
    }

    // sca efficiency
    sca_eff[0] = csp_dtr_sca_calc_sca_eff_1;
    sca_eff[1] = csp_dtr_sca_calc_sca_eff_2;
    sca_eff[2] = csp_dtr_sca_calc_sca_eff_3;
    sca_eff[3] = csp_dtr_sca_calc_sca_eff_4;

    sca_len[0] = csp_dtr_sca_length_1;
    sca_len[1] = csp_dtr_sca_length_2;
    sca_len[2] = csp_dtr_sca_length_3;
    sca_len[3] = csp_dtr_sca_length_4;

    double total_len = 0.;
    double weighted_sca_eff = 0.0;
    for (int i = 0; i < ncol; i++)
    {
        int sca_t = std::min(std::max(static_cast<int>(trough_loop_control.at(1 + i * 3)), 1), 4) - 1;
        total_len = total_len + sca_len[sca_t];
        weighted_sca_eff = weighted_sca_eff + sca_len[sca_t] * sca_eff[sca_t];
    }

    if (total_len != 0.0) {
        weighted_sca_eff = weighted_sca_eff / total_len;
    }
    else {
        weighted_sca_eff = -777.7;
    }

    // hce efficiency
    hce_eff[0] = csp_dtr_hce_optical_eff_1;
    hce_eff[1] = csp_dtr_hce_optical_eff_2;
    hce_eff[2] = csp_dtr_hce_optical_eff_3;
    hce_eff[3] = csp_dtr_hce_optical_eff_4;

    total_len = 0;
    double weighted_hce_eff = 0.0;
    for (int i = 0; i < ncol; i++)
    {
        int hce_t = std::min(std::max(static_cast<int>(trough_loop_control.at(2 + i * 3)), 1), 4) - 1;
        int sca_t = std::min(std::max(static_cast<int>(trough_loop_control.at(1 + i * 3)), 1), 4) - 1;
        total_len = total_len + sca_len[sca_t];
        weighted_hce_eff = weighted_hce_eff + sca_len[sca_t] * hce_eff[hce_t];
    }

    if (total_len != 0.0) {
        weighted_hce_eff = weighted_hce_eff / total_len;
    }
    else {
        weighted_hce_eff = -777.7;
    }

    return weighted_hce_eff * weighted_sca_eff;
}

double Total_loop_conversion_efficiency(double loop_optical_efficiency, double cspdtr_loop_hce_heat_loss)
{
    return loop_optical_efficiency * cspdtr_loop_hce_heat_loss;
}

double Field_thermal_output(double I_bn_des, double total_loop_conversion_efficiency, double total_aperture)
{
    return I_bn_des * total_loop_conversion_efficiency * total_aperture / 1.E6;  // default now = 626.008
    //return solar_mult * P_ref / eta_ref;   // default = 623.596
}

double Total_required_aperture_for_sm1(double q_pb_design, double I_bn_des, double total_loop_conversion_efficiency)
{
    return q_pb_design / (I_bn_des * total_loop_conversion_efficiency) * 1000000.0;
}

double Fixed_land_area(double total_aperture, double row_distance, util::matrix_t<ssc_number_t> sca_info_array,
    double csp_dtr_sca_w_profile_1, double csp_dtr_sca_w_profile_2, double csp_dtr_sca_w_profile_3, double csp_dtr_sca_w_profile_4)
{
    std::vector<double> collector_widths = {csp_dtr_sca_w_profile_1, csp_dtr_sca_w_profile_2, csp_dtr_sca_w_profile_3, csp_dtr_sca_w_profile_4};
    double max_collector_width = 0.;
    for (int i = 0; i < sca_info_array.nrows(); i++) {
        max_collector_width = std::max(max_collector_width, collector_widths.at(sca_info_array.at(i, 0) - 1));
    }

    return total_aperture * row_distance / max_collector_width * 0.0002471;
}

double Total_land_area(double fixed_land_area, double non_solar_field_land_area_multiplier)
{
    return fixed_land_area * non_solar_field_land_area_multiplier;
}

util::matrix_t<ssc_number_t> Sca_info_array(const util::matrix_t<ssc_number_t>& trough_loop_control)
{
    int assemblies = static_cast<int>(trough_loop_control.at(0));
    util::matrix_t<ssc_number_t> p1(assemblies, 2);
    for (int i = 0; i < assemblies; i++) {
        p1.at(i, 1) = static_cast<int>(trough_loop_control.at(1 + 3 * i));
        p1.at(i, 0) = static_cast<int>(trough_loop_control.at(2 + 3 * i));
    }

    return p1;
}

util::matrix_t<ssc_number_t> Sca_defocus_array(const util::matrix_t<ssc_number_t>& trough_loop_control)
{
    int assemblies = static_cast<int>(trough_loop_control.at(0));
    util::matrix_t<ssc_number_t> p1(assemblies);
    for (int i = 0; i < assemblies; i++) {
        p1.at(i) = static_cast<int>(trough_loop_control.at(3 + 3 * i));
    }

    return p1;
}

double Total_tracking_power(int nSCA, int nLoops, double SCA_drives_elec)
{
    return nSCA * nLoops * SCA_drives_elec;
}

util::matrix_t<ssc_number_t> K_Cpnt(int nSCA)
{
    std::vector<double> K_cpnt_0 = { 0.9, 0, 0.19, 0, 0.9, -1, -1, -1, -1, -1, -1 };
    std::vector<double> K_cpnt_1 = { 0, 0.6, 0.05, 0, 0.6, 0, 0.6, 0, 0.42, 0, 0.15 };
    std::vector<double> K_cpnt_i = { 0.05, 0, 0.42, 0, 0.6, 0, 0.6, 0, 0.42, 0, 0.15 };
    std::vector<double> K_cpnt_x_2 = { 0.05, 0, 0.42, 0, 0.6, 0, 0.6, 0, 0.15, 0.6, 0 };
    std::vector<double> K_cpnt_x_1 = { 0.9, 0, 0.19, 0, 0.9, -1, -1, -1, -1, -1, -1 };

    util::matrix_t<ssc_number_t> K(nSCA + 3, 11);

    // After cold header before SCAs
    for (int j = 0; j < K_cpnt_0.size(); j++) {
        K.at(0, j) = K_cpnt_0.at(j);
        K.at(1, j) = K_cpnt_1.at(j);
    }

    // Between SCAs
    for (int i = 0; i < nSCA - 1; i++) {
        for (int j = 0; j < K_cpnt_i.size(); j++) {
            K.at(i + 2, j) = K_cpnt_i.at(j);
        }
    }

    // After SCAs before hot header
    for (int j = 0; j < K_cpnt_x_2.size(); j++) {
        K.at(nSCA + 1, j) = K_cpnt_x_2.at(j);
        K.at(nSCA + 2, j) = K_cpnt_x_1.at(j);
    }

    return K;
}

util::matrix_t<ssc_number_t> D_Cpnt(int nSCA)
{
    std::vector<double> D_cpnt_0 = { 0.085, 0.0635, 0.085, 0.0635, 0.085, -1, -1, -1, -1, -1, -1 };
    std::vector<double> D_cpnt_1 = { 0.085, 0.085, 0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085 };
    std::vector<double> D_cpnt_i = { 0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085 };
    std::vector<double> D_cpnt_x_2 = { 0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085, 0.085, 0.085 };
    std::vector<double> D_cpnt_x_1 = { 0.085, 0.0635, 0.085, 0.0635, 0.085, -1, -1, -1, -1, -1, -1 };

    util::matrix_t<ssc_number_t> D(nSCA + 3, 11);

    // After cold header before SCAs
    for (int j = 0; j < D_cpnt_0.size(); j++) {
        D.at(0, j) = D_cpnt_0.at(j);
        D.at(1, j) = D_cpnt_1.at(j);
    }

    // Between SCAs
    for (int i = 0; i < nSCA - 1; i++) {
        for (int j = 0; j < D_cpnt_i.size(); j++) {
            D.at(i + 2, j) = D_cpnt_i.at(j);
        }
    }

    // After SCAs before hot header
    for (int j = 0; j < D_cpnt_x_2.size(); j++) {
        D.at(nSCA + 1, j) = D_cpnt_x_2.at(j);
        D.at(nSCA + 2, j) = D_cpnt_x_1.at(j);
    }

    return D;
}

util::matrix_t<ssc_number_t> L_Cpnt(int nSCA)
{
    std::vector<double> L_cpnt_0 = { 0, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1 };
    std::vector<double> L_cpnt_1 = { 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0 };
    std::vector<double> L_cpnt_i = { 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0 };
    std::vector<double> L_cpnt_x_2 = { 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0 };
    std::vector<double> L_cpnt_x_1 = { 0, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1 };

    util::matrix_t<ssc_number_t> L(nSCA + 3, 11);

    // After cold header before SCAs
    for (int j = 0; j < L_cpnt_0.size(); j++) {
        L.at(0, j) = L_cpnt_0.at(j);
        L.at(1, j) = L_cpnt_1.at(j);
    }

    // Between SCAs
    for (int i = 0; i < nSCA - 1; i++) {
        for (int j = 0; j < L_cpnt_i.size(); j++) {
            L.at(i + 2, j) = L_cpnt_i.at(j);
        }
    }

    // After SCAs before hot header
    for (int j = 0; j < L_cpnt_x_2.size(); j++) {
        L.at(nSCA + 1, j) = L_cpnt_x_2.at(j);
        L.at(nSCA + 2, j) = L_cpnt_x_1.at(j);
    }

    return L;
}

util::matrix_t<ssc_number_t> Type_Cpnt(int nSCA)
{
    std::vector<double> Type_cpnt_0 = { 0, 1, 0, 1, 0, -1, -1, -1, -1, -1, -1 };
    std::vector<double> Type_cpnt_1 = { 1, 0, 0, 2, 0, 1, 0, 2, 0, 2, 0 };
    std::vector<double> Type_cpnt_i = { 0, 2, 0, 2, 0, 1, 0, 2, 0, 2, 0 };
    std::vector<double> Type_cpnt_x_2 = { 0, 2, 0, 2, 0, 1, 0, 2, 0, 0, 1 };
    std::vector<double> Type_cpnt_x_1 = { 0, 1, 0, 1, 0, -1, -1, -1, -1, -1, -1 };

    util::matrix_t<ssc_number_t> Type(nSCA + 3, 11);

    // After cold header before SCAs
    for (int j = 0; j < Type_cpnt_0.size(); j++) {
        Type.at(0, j) = Type_cpnt_0.at(j);
        Type.at(1, j) = Type_cpnt_1.at(j);
    }

    // Between SCAs
    for (int i = 0; i < nSCA - 1; i++) {
        for (int j = 0; j < Type_cpnt_i.size(); j++) {
            Type.at(i + 2, j) = Type_cpnt_i.at(j);
        }
    }

    // After SCAs before hot header
    for (int j = 0; j < Type_cpnt_x_2.size(); j++) {
        Type.at(nSCA + 1, j) = Type_cpnt_x_2.at(j);
        Type.at(nSCA + 2, j) = Type_cpnt_x_1.at(j);
    }

    return Type;
}

// Originally from 'Physical Trough Collector Type 1' (and 2, 3, 4)
double Csp_dtr_sca_ap_length(double csp_dtr_sca_length, double csp_dtr_sca_ncol_per_sca) {
    return csp_dtr_sca_length / csp_dtr_sca_ncol_per_sca;
}

util::matrix_t<ssc_number_t> Csp_dtr_sca_ap_lengths(const util::matrix_t<ssc_number_t>& csp_dtr_sca_lengths, const util::matrix_t<ssc_number_t>& csp_dtr_sca_ncol_per_scas) {
    int n = csp_dtr_sca_lengths.ncells();

    util::matrix_t<ssc_number_t> result(n);
    for (int i = 0; i < n; i++) {
        result.at(i) = csp_dtr_sca_lengths.at(i) / csp_dtr_sca_ncol_per_scas.at(i);
    }
    return result;
}

double Csp_dtr_sca_calc_end_gain(double csp_dtr_sca_ave_focal_len, double csp_dtr_sca_calc_theta, double csp_dtr_sca_piping_dist) {
    return  std::max(csp_dtr_sca_ave_focal_len * tan(csp_dtr_sca_calc_theta) - csp_dtr_sca_piping_dist, 0.);
}

double Csp_dtr_sca_calc_costh(double csp_dtr_sca_calc_zenith, double tilt, double azimuth) {
    return  sqrt(1 - pow(cos(1.57 - csp_dtr_sca_calc_zenith - tilt)
        - cos(tilt)
        * cos(1.57 - csp_dtr_sca_calc_zenith)
        * (1. - cos(0. - azimuth)), 2)
    );
}

double Csp_dtr_sca_calc_end_loss(double csp_dtr_sca_ave_focal_len, double csp_dtr_sca_calc_theta, double nSCA, double csp_dtr_sca_calc_end_gain,
    double csp_dtr_sca_length, double csp_dtr_sca_ncol_per_sca) {
    return  1 - (csp_dtr_sca_ave_focal_len * tan(csp_dtr_sca_calc_theta)
        - (nSCA - 1) / nSCA * csp_dtr_sca_calc_end_gain)
        / (csp_dtr_sca_length * csp_dtr_sca_ncol_per_sca);
}

double Csp_dtr_sca_calc_sca_eff(double csp_dtr_sca_tracking_error, double csp_dtr_sca_geometry_effects,
    double csp_dtr_sca_clean_reflectivity, double csp_dtr_sca_mirror_dirt, double csp_dtr_sca_general_error) {
    return  csp_dtr_sca_tracking_error * csp_dtr_sca_geometry_effects *
        csp_dtr_sca_clean_reflectivity * csp_dtr_sca_mirror_dirt * csp_dtr_sca_general_error;
}

double Csp_dtr_sca_calc_latitude(double lat) {
    return lat;
}

double Csp_dtr_sca_calc_zenith(double lat) {
    return M_PI / 180. * (90. - (90. - (lat - 23.5)));
}

double Csp_dtr_sca_calc_iam(const util::matrix_t<ssc_number_t>& IAMs, double csp_dtr_sca_calc_theta, double csp_dtr_sca_calc_costh) {
    double IAM = IAMs.at(0);
    int l_IAM = IAMs.ncells();
    if (l_IAM < 2) {
        return IAM;
    }
    else {
        for (int i = 1; i < l_IAM; i++) {
            IAM = IAM + IAMs.at(i) * pow(csp_dtr_sca_calc_theta, i) / csp_dtr_sca_calc_costh;
        }
        return IAM;
    }
}

double Csp_dtr_sca_calc_theta(double csp_dtr_sca_calc_costh) {
    return acos(csp_dtr_sca_calc_costh);
}
