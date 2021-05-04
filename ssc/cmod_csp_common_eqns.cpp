#include <cmath>
#include <math.h>
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

    double A_sf_refl, site_improv_spec_cost, heliostat_spec_cost, heliostat_fixed_cost,
        h_tower, h_rec, h_helio, tower_fixed_cost, tower_cost_scaling_exp,
        A_rec, rec_ref_cost, A_rec_ref, rec_cost_scaling_exp,
        Q_storage, tes_spec_cost,
        W_dot_design, power_cycle_spec_cost,
        bop_spec_cost,
        fossil_backup_spec_cost,
        contingency_rate,
        total_land_area, plant_net_capacity, EPC_land_spec_cost, EPC_land_perc_direct_cost,
        EPC_land_per_power_cost, EPC_land_fixed_cost, total_land_spec_cost, total_land_perc_direct_cost,
        total_land_per_power_cost, total_land_fixed_cost, sales_tax_basis, sales_tax_rate;

    //sys_costs.ms_par.A_sf_refl = as_double("A_sf");
    ssc_data_t_get_number(data, "a_sf_ui", &A_sf_refl);
    ssc_data_t_get_number(data, "site_spec_cost", &site_improv_spec_cost);
    ssc_data_t_get_number(data, "heliostat_spec_cost", &heliostat_spec_cost);
    ssc_data_t_get_number(data, "cost_sf_fixed", &heliostat_fixed_cost);
    ssc_data_t_get_number(data, "h_tower", &h_tower);                            // set different for other techs
    ssc_data_t_get_number(data, "rec_height", &h_rec);
    ssc_data_t_get_number(data, "helio_height", &h_helio);
    ssc_data_t_get_number(data, "tower_fixed_cost", &tower_fixed_cost);
    ssc_data_t_get_number(data, "tower_exp", &tower_cost_scaling_exp);
    ssc_data_t_get_number(data, "csp.pt.cost.receiver.area", &A_rec);            // calculation specific to each tech
    ssc_data_t_get_number(data, "rec_ref_cost", &rec_ref_cost);
    ssc_data_t_get_number(data, "rec_ref_area", &A_rec_ref);
    ssc_data_t_get_number(data, "rec_cost_exp", &rec_cost_scaling_exp);
    ssc_data_t_get_number(data, "csp.pt.cost.storage_mwht", &Q_storage);         // calculation specific to each tech
    ssc_data_t_get_number(data, "tes_spec_cost", &tes_spec_cost);
    ssc_data_t_get_number(data, "csp.pt.cost.power_block_mwe", &W_dot_design);   // calculation specific to each tech
    ssc_data_t_get_number(data, "plant_spec_cost", &power_cycle_spec_cost);
    ssc_data_t_get_number(data, "bop_spec_cost", &bop_spec_cost);
    ssc_data_t_get_number(data, "fossil_spec_cost", &fossil_backup_spec_cost);
    ssc_data_t_get_number(data, "contingency_rate", &contingency_rate);
    ssc_data_t_get_number(data, "csp.pt.sf.total_land_area", &total_land_area);
    ssc_data_t_get_number(data, "nameplate", &plant_net_capacity);
    ssc_data_t_get_number(data, "csp.pt.cost.epc.per_acre", &EPC_land_spec_cost);
    ssc_data_t_get_number(data, "csp.pt.cost.epc.percent", &EPC_land_perc_direct_cost);
    ssc_data_t_get_number(data, "csp.pt.cost.epc.per_watt", &EPC_land_per_power_cost);
    ssc_data_t_get_number(data, "csp.pt.cost.epc.fixed", &EPC_land_fixed_cost);
    ssc_data_t_get_number(data, "land_spec_cost", &total_land_spec_cost);
    ssc_data_t_get_number(data, "csp.pt.cost.plm.percent", &total_land_perc_direct_cost);
    ssc_data_t_get_number(data, "csp.pt.cost.plm.per_watt", &total_land_per_power_cost);
    ssc_data_t_get_number(data, "csp.pt.cost.plm.fixed", &total_land_fixed_cost);
    ssc_data_t_get_number(data, "sales_tax_frac", &sales_tax_basis);
    ssc_data_t_get_number(data, "sales_tax_rate", &sales_tax_rate);


    double site_improvement_cost, heliostat_cost, tower_cost, receiver_cost, tes_cost, power_cycle_cost,
        bop_cost, fossil_backup_cost,
        direct_capital_precontingency_cost, contingency_cost, total_direct_cost, epc_and_owner_cost, total_land_cost,
        sales_tax_cost, total_indirect_cost, total_installed_cost, estimated_installed_cost_per_cap;

    site_improvement_cost = heliostat_cost = tower_cost = receiver_cost = tes_cost = power_cycle_cost =
        bop_cost = fossil_backup_cost =
        direct_capital_precontingency_cost = contingency_cost = total_direct_cost = epc_and_owner_cost = total_land_cost =
        sales_tax_cost = total_indirect_cost = total_installed_cost = estimated_installed_cost_per_cap = std::numeric_limits<double>::quiet_NaN();


    N_mspt::calculate_mspt__no_rad_cool__costs(
        A_sf_refl,
        site_improv_spec_cost,
        heliostat_spec_cost,
        heliostat_fixed_cost,

        h_tower,
        h_rec,
        h_helio,
        tower_fixed_cost,
        tower_cost_scaling_exp,

        A_rec,
        rec_ref_cost,
        A_rec_ref,
        rec_cost_scaling_exp,

        Q_storage,
        tes_spec_cost,

        W_dot_design,
        power_cycle_spec_cost,

        bop_spec_cost,

        fossil_backup_spec_cost,

        contingency_rate,

        total_land_area,
        plant_net_capacity,
        EPC_land_spec_cost,
        EPC_land_perc_direct_cost,
        EPC_land_per_power_cost,
        EPC_land_fixed_cost,
        total_land_spec_cost,
        total_land_perc_direct_cost,
        total_land_per_power_cost,
        total_land_fixed_cost,
        sales_tax_basis,
        sales_tax_rate,

        site_improvement_cost,
        heliostat_cost,
        tower_cost,
        receiver_cost,
        tes_cost,
        power_cycle_cost,
        bop_cost,
        fossil_backup_cost,
        direct_capital_precontingency_cost,
        contingency_cost,
        total_direct_cost,
        total_land_cost,
        epc_and_owner_cost,
        sales_tax_cost,
        total_indirect_cost,
        total_installed_cost,
        estimated_installed_cost_per_cap
    );

    ssc_data_t_set_number(data, "csp.pt.cost.site_improvements", (ssc_number_t)site_improvement_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.heliostats", (ssc_number_t)heliostat_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.tower", (ssc_number_t)tower_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.receiver", (ssc_number_t)receiver_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.storage", (ssc_number_t)tes_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.power_block", (ssc_number_t)power_cycle_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.bop", (ssc_number_t)bop_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.fossil", (ssc_number_t)fossil_backup_cost);
    ssc_data_t_set_number(data, "ui_direct_subtotal", (ssc_number_t)direct_capital_precontingency_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.contingency", (ssc_number_t)contingency_cost);
    ssc_data_t_set_number(data, "total_direct_cost", (ssc_number_t)total_direct_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.epc.total", (ssc_number_t)epc_and_owner_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.plm.total", (ssc_number_t)total_land_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.sales_tax.total", (ssc_number_t)sales_tax_cost);
    ssc_data_t_set_number(data, "total_indirect_cost", (ssc_number_t)total_indirect_cost);
    ssc_data_t_set_number(data, "total_installed_cost", (ssc_number_t)total_installed_cost);
    ssc_data_t_set_number(data, "csp.pt.cost.installed_per_capacity", (ssc_number_t)estimated_installed_cost_per_cap);
}
