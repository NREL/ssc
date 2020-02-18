#ifndef _CMOD_CSP_TOWER_EQNS_H_
#define _CMOD_CSP_TOWER_EQNS_H_

#include "sscapi.h"

#ifdef __cplusplus
extern "C" {
#endif

    static const char* MSPT_System_Design_Equations_doc =
        "Sizes the design point system parameters of a molten salt power tower plant, as used on the System Design UI form\\n"
        "Input: var_table with key-value pairs\\n"
        "     'design_eff' - double [-]\\n"
        "     'gross_net_conversion_factor' - double [-]\\n"
        "     'P_ref' - double [MWe]\\n"
        "     'solarm' - double [-]\\n"
        "     'tshours' - double [hr]\\n"
        "Output: key-value pairs added to var_table\\n"
        "     'nameplate' - double [MWe]\\n"
        "     'q_pb_design' - double [MWt]\\n"
        "     'Q_rec_des' - double [MWt]\\n"
        "     'tshours_sf' - double [hr]";

    SSCEXPORT void MSPT_System_Design_Equations(ssc_data_t data);


    static const char* Tower_SolarPilot_Solar_Field_Equations_doc =
        "Sizes and lays out the heliostat field for a molten salt power tower plant, as used on the Heliostat Field UI form\\n"
        "Input: var_table with key-value pairs\\n"
        "     'c_atm_0' - double [-]\\n"
        "     'c_atm_1' - double [-]\\n"
        "     'c_atm_2' - double [-]\\n"
        "     'c_atm_3' - double [-]\\n"
        "     'csp_pt_sf_fixed_land_area' - double [acres]\\n"
        "     'csp_pt_sf_land_overhead_factor' - double [-]\\n"
        "     'dens_mirror' - double [-]\\n"
        "     'dni_des' - double [W/m2]\\n"
        "     'h_tower' - double [m]\\n"
        "     'helio_height' - double [m]\\n"
        "     'helio_optical_error_mrad' - double [mrad]\\n"
        "     'helio_positions' - ssc_number_t [m]\\n"
        "     'helio_width' - double [m]\\n"
        "     'land_area_base' - double [acres]\\n"
        "     'land_max' - double [-]\\n"
        "     'land_min' - double [-]\\n"
        "     'override_layout' - int [-]\\n"
        "     'override_opt' - int [-]\\n"
        "     'q_rec_des' - double [MWt]\\n"
        "Output: key-value pairs added to var_table\\n"
        "     'A_sf_UI' - double [m2]\\n"
        "     'c_atm_info' - double [%]\\n"
        "     'csp_pt_sf_heliostat_area' - double [m2]\\n"
        "     'csp_pt_sf_total_land_area' - double [acres]\\n"
        "     'csp_pt_sf_total_reflective_area' - double [m2]\\n"
        "     'csp_pt_sf_tower_height' - double [m]\\n"
        "     'dni_des_calc' - double [W/m2]\\n"
        "     'error_equiv' - double [mrad]\\n"
        "     'field_model_type' - double [-]\\n"
        "     'helio_area_tot' - double [m2]\\n"
        "     'is_optimize' - int [-]\\n"
        "     'land_max_calc' - double [m]\\n"
        "     'land_min_calc' - double [m]\\n"
        "     'n_hel' - int [-]\\n"
        "     'opt_algorithm' - double [-]\\n"
        "     'opt_flux_penalty' - double [-]\\n"
        "     'q_design' - double [MWt]\\n";

    SSCEXPORT void Tower_SolarPilot_Solar_Field_Equations(ssc_data_t data);


    static const char* MSPT_Receiver_Equations_doc =
        "Sizes the receiver and tower piping, as used on the Tower and Receiver UI form\\n"
        "Input: var_table with key-value pairs\\n"
        "     'csp_pt_rec_cav_ap_hw_ratio' - double [-]\\n"
        "     'csp_pt_rec_max_oper_frac' - double [-]\\n"
        "     'd_rec' - double [m]\\n"
        "     'field_fl_props' - double [-]\\n"
        "     'h_tower' - double [m]\\n"
        "     'piping_length_const' - double [m]\\n"
        "     'piping_length_mult' - double [-]\\n"
        "     'piping_loss' - double [Wt/m]\\n"
        "     'q_rec_des' - double [MWt]\\n"
        "     'rec_d_spec' - double [m]\\n"
        "     'rec_height' - double [m]\\n"
        "     'rec_htf' - int [-]\\n"
        "     't_htf_cold_des' - double [C]\\n"
        "     't_htf_hot_des' - double [C]\\n"
        "Output: key-value pairs added to var_table\\n"
        "     'csp_pt_rec_cav_ap_height' - double [m]\\n"
        "     'csp_pt_rec_cav_lip_height' - double [m]\\n"
        "     'csp_pt_rec_cav_panel_height' - double [m]\\n"
        "     'csp_pt_rec_htf_c_avg' - double [kJ/kg-K]\\n"
        "     'csp_pt_rec_htf_t_avg' - double [C]\\n"
        "     'csp_pt_rec_max_flow_to_rec' - double [kg/s]\\n"
        "     'piping_length' - double [m]\\n"
        "     'piping_loss_tot' - double [kWt]\\n"
        "     'rec_aspect' - double [-]\\n";

    SSCEXPORT void MSPT_Receiver_Equations(ssc_data_t data);

#ifdef __cplusplus
}
#endif

#endif
