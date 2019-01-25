#ifndef _TROUGH_PHYSICAL_IPH_COMMON_DATA_H_
#define _TROUGH_PHYSICAL_IPH_COMMON_DATA_H_

#include <stdio.h>

#include "code_generator_utilities.h"


/**
*  Default data for trough_physical_process_heat run that can be further modified
*/
void trough_physical_iph_default(ssc_data_t &data)
{
	const char * SSCDIR = std::getenv("SSCDIR");

	char solar_resource_path[200];
	//char load_profile_path[100];


	int n1 = sprintf(solar_resource_path, "%s/test/input_cases/tcstrough_data/tucson_az_32.116521_-110.933042_psmv3_60_tmy.csv", SSCDIR);
	//int n2 = sprintf(load_profile_path, "%s/test/input_cases/pvsamv1_data/pvsamv1_residential_load.csv", SSCDIR);

    ssc_data_set_string(data, "file_name", solar_resource_path);
    ssc_data_set_number(data, "track_mode", 1);
    ssc_data_set_number(data, "tilt", 0);
    ssc_data_set_number(data, "azimuth", 0);
    ssc_data_set_number(data, "I_bn_des", 950);
    ssc_data_set_number(data, "solar_mult", 2.6771607398986816);
    ssc_data_set_number(data, "T_loop_in_des", 90);
    ssc_data_set_number(data, "T_loop_out", 150);
    ssc_data_set_number(data, "q_pb_design", 5.1879100799560547);
    ssc_data_set_number(data, "tshours", 6);
    ssc_data_set_number(data, "nSCA", 4);
    ssc_data_set_number(data, "nHCEt", 4);
    ssc_data_set_number(data, "nColt", 4);
    ssc_data_set_number(data, "nHCEVar", 4);
    ssc_data_set_number(data, "nLoops", 8);
    ssc_data_set_number(data, "eta_pump", 0.85000002384185791);
    ssc_data_set_number(data, "HDR_rough", 4.5699998736381531e-05);
    ssc_data_set_number(data, "theta_stow", 170);
    ssc_data_set_number(data, "theta_dep", 10);
    ssc_data_set_number(data, "Row_Distance", 15);
    ssc_data_set_number(data, "FieldConfig", 1);
    ssc_data_set_number(data, "is_model_power_block_piping", 0);
    ssc_data_set_number(data, "L_power_block_piping", 50);
    ssc_data_set_number(data, "m_dot_htfmin", 1);
    ssc_data_set_number(data, "m_dot_htfmax", 12);
    ssc_data_set_number(data, "Fluid", 31);
    ssc_data_set_number(data, "wind_stow_speed", 25);
    ssc_number_t p_field_fl_props[77] = { 20, 4.179999828338623, 999, 0.0010000000474974513, 9.9999999747524271e-07, 0.58700001239776611,
        85.300003051757813, 40, 4.179999828338623, 993, 0.000653000024612993, 6.5799997628346318e-07, 0.61799997091293335, 169, 60,
        4.179999828338623, 984, 0.00046700000530108809, 4.7500000732725312e-07, 0.64200001955032349, 252, 80, 4.190000057220459, 972,
        0.00035499999648891389, 3.6500000533123966e-07, 0.65700000524520874, 336, 100, 4.2100000381469727, 959, 0.00028199999360367656,
        2.9399998879853229e-07, 0.66600000858306885, 420, 120, 4.25, 944, 0.00023299999884329736, 2.4600001324870391e-07, 0.67000001668930054,
        505, 140, 4.2800002098083496, 927, 0.00019700000120792538, 2.1200000333010394e-07, 0.67000001668930054, 590, 160, 4.3400001525878906,
        908, 0.00017100000695791095, 1.8800000134433503e-07, 0.66699999570846558, 676, 180, 4.4000000953674316, 887, 0.0001500000071246177,
        1.6900000332498166e-07, 0.66100001335144043, 764, 200, 4.4899997711181641, 865, 0.0001340000017080456, 1.5499999506118911e-07,
        0.65100002288818359, 852, 220, 4.5799999237060547, 842, 0.00011800000356743112, 1.4100000100825127e-07, 0.64099997282028198, 941 };
    ssc_data_set_matrix(data, "field_fl_props", p_field_fl_props, 11, 7);
    ssc_data_set_number(data, "T_fp", 10);
    ssc_data_set_number(data, "V_hdr_max", 3);
    ssc_data_set_number(data, "V_hdr_min", 2);
    ssc_data_set_number(data, "Pipe_hl_coef", 0.44999998807907104);
    ssc_data_set_number(data, "SCA_drives_elec", 125);
    ssc_data_set_number(data, "water_usage_per_wash", 0.69999998807907104);
    ssc_data_set_number(data, "washing_frequency", 12);
    ssc_data_set_number(data, "accept_mode", 0);
    ssc_data_set_number(data, "accept_init", 0);
    ssc_data_set_number(data, "accept_loc", 1);
    ssc_data_set_number(data, "mc_bal_hot", 0.20000000298023224);
    ssc_data_set_number(data, "mc_bal_cold", 0.20000000298023224);
    ssc_data_set_number(data, "mc_bal_sca", 4.5);
    ssc_number_t p_W_aperture[4] = { 6, 6, 6, 6 };
    ssc_data_set_array(data, "W_aperture", p_W_aperture, 4);
    ssc_number_t p_A_aperture[4] = { 656, 656, 656, 656 };
    ssc_data_set_array(data, "A_aperture", p_A_aperture, 4);
    ssc_number_t p_TrackingError[4] = { 0.98799997568130493, 0.98799997568130493, 0.98799997568130493, 0.98799997568130493 };
    ssc_data_set_array(data, "TrackingError", p_TrackingError, 4);
    ssc_number_t p_GeomEffects[4] = { 0.95200002193450928, 0.95200002193450928, 0.95200002193450928, 0.95200002193450928 };
    ssc_data_set_array(data, "GeomEffects", p_GeomEffects, 4);
    ssc_number_t p_Rho_mirror_clean[4] = { 0.93000000715255737, 0.93000000715255737, 0.93000000715255737, 0.93000000715255737 };
    ssc_data_set_array(data, "Rho_mirror_clean", p_Rho_mirror_clean, 4);
    ssc_number_t p_Dirt_mirror[4] = { 0.97000002861022949, 0.97000002861022949, 0.97000002861022949, 0.97000002861022949 };
    ssc_data_set_array(data, "Dirt_mirror", p_Dirt_mirror, 4);
    ssc_number_t p_Error[4] = { 1, 1, 1, 1 };
    ssc_data_set_array(data, "Error", p_Error, 4);
    ssc_number_t p_Ave_Focal_Length[4] = { 2.1500000953674316, 2.1500000953674316, 2.1500000953674316, 2.1500000953674316 };
    ssc_data_set_array(data, "Ave_Focal_Length", p_Ave_Focal_Length, 4);
    ssc_number_t p_L_SCA[4] = { 115, 115, 115, 115 };
    ssc_data_set_array(data, "L_SCA", p_L_SCA, 4);
    ssc_number_t p_L_aperture[4] = { 14.375, 14.375, 14.375, 14.375 };
    ssc_data_set_array(data, "L_aperture", p_L_aperture, 4);
    ssc_number_t p_ColperSCA[4] = { 8, 8, 8, 8 };
    ssc_data_set_array(data, "ColperSCA", p_ColperSCA, 4);
    ssc_number_t p_Distance_SCA[4] = { 1, 1, 1, 1 };
    ssc_data_set_array(data, "Distance_SCA", p_Distance_SCA, 4);
    ssc_number_t p_IAM_matrix[12] = { 1, 0.032699998468160629, -0.13510000705718994, 1, 0.032699998468160629, -0.13510000705718994,
        1, 0.032699998468160629, -0.13510000705718994, 1, 0.032699998468160629, -0.13510000705718994 };
    ssc_data_set_matrix(data, "IAM_matrix", p_IAM_matrix, 4, 3);
    ssc_number_t p_HCE_FieldFrac[16] = { 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0 };
    ssc_data_set_matrix(data, "HCE_FieldFrac", p_HCE_FieldFrac, 4, 4);
    ssc_number_t p_D_2[16] = { 0.075999997556209564, 0.075999997556209564, 0.075999997556209564, 0.075999997556209564,
        0.075999997556209564, 0.075999997556209564, 0.075999997556209564, 0.075999997556209564, 0.075999997556209564,
        0.075999997556209564, 0.075999997556209564, 0.075999997556209564, 0.075999997556209564, 0.075999997556209564,
        0.075999997556209564, 0.075999997556209564 };
    ssc_data_set_matrix(data, "D_2", p_D_2, 4, 4);
    ssc_number_t p_D_3[16] = { 0.079999998211860657, 0.079999998211860657, 0.079999998211860657, 0.079999998211860657,
        0.079999998211860657, 0.079999998211860657, 0.079999998211860657, 0.079999998211860657, 0.079999998211860657,
        0.079999998211860657, 0.079999998211860657, 0.079999998211860657, 0.079999998211860657, 0.079999998211860657,
        0.079999998211860657, 0.079999998211860657 };
    ssc_data_set_matrix(data, "D_3", p_D_3, 4, 4);
    ssc_number_t p_D_4[16] = { 0.11500000208616257, 0.11500000208616257, 0.11500000208616257, 0.11500000208616257,
        0.11500000208616257, 0.11500000208616257, 0.11500000208616257, 0.11500000208616257, 0.11500000208616257,
        0.11500000208616257, 0.11500000208616257, 0.11500000208616257, 0.11500000208616257, 0.11500000208616257,
        0.11500000208616257, 0.11500000208616257 };
    ssc_data_set_matrix(data, "D_4", p_D_4, 4, 4);
    ssc_number_t p_D_5[16] = { 0.11999999731779099, 0.11999999731779099, 0.11999999731779099, 0.11999999731779099,
        0.11999999731779099, 0.11999999731779099, 0.11999999731779099, 0.11999999731779099, 0.11999999731779099,
        0.11999999731779099, 0.11999999731779099, 0.11999999731779099, 0.11999999731779099, 0.11999999731779099,
        0.11999999731779099, 0.11999999731779099 };
    ssc_data_set_matrix(data, "D_5", p_D_5, 4, 4);
    ssc_number_t p_D_p[16] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    ssc_data_set_matrix(data, "D_p", p_D_p, 4, 4);
    ssc_number_t p_Flow_type[16] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "Flow_type", p_Flow_type, 4, 4);
    ssc_number_t p_Rough[16] = { 4.5000000682193786e-05, 4.5000000682193786e-05, 4.5000000682193786e-05, 4.5000000682193786e-05,
        4.5000000682193786e-05, 4.5000000682193786e-05, 4.5000000682193786e-05, 4.5000000682193786e-05, 4.5000000682193786e-05,
        4.5000000682193786e-05, 4.5000000682193786e-05, 4.5000000682193786e-05, 4.5000000682193786e-05, 4.5000000682193786e-05,
        4.5000000682193786e-05, 4.5000000682193786e-05 };
    ssc_data_set_matrix(data, "Rough", p_Rough, 4, 4);
    ssc_number_t p_alpha_env[16] = { 0.019999999552965164, 0.019999999552965164, 0, 0, 0.019999999552965164,
        0.019999999552965164, 0, 0, 0.019999999552965164, 0.019999999552965164, 0, 0, 0.019999999552965164,
        0.019999999552965164, 0, 0 };
    ssc_data_set_matrix(data, "alpha_env", p_alpha_env, 4, 4);
    ssc_number_t p_epsilon_3_11[18] = { 100, 0.064000003039836884, 150, 0.066500000655651093, 200, 0.070000000298023224,
        250, 0.074500001966953278, 300, 0.079999998211860657, 350, 0.086499996483325958, 400, 0.093999996781349182, 450,
        0.10249999910593033, 500, 0.1120000034570694 };
    ssc_data_set_matrix(data, "epsilon_3_11", p_epsilon_3_11, 9, 2);
    ssc_number_t p_epsilon_3_12[1] = { 0.64999997615814209 };
    ssc_data_set_matrix(data, "epsilon_3_12", p_epsilon_3_12, 1, 1);
    ssc_number_t p_epsilon_3_13[1] = { 0.64999997615814209 };
    ssc_data_set_matrix(data, "epsilon_3_13", p_epsilon_3_13, 1, 1);
    ssc_number_t p_epsilon_3_14[1] = { 0 };
    ssc_data_set_matrix(data, "epsilon_3_14", p_epsilon_3_14, 1, 1);
    ssc_number_t p_epsilon_3_21[18] = { 100, 0.064000003039836884, 150, 0.066500000655651093, 200, 0.070000000298023224, 250,
        0.074500001966953278, 300, 0.079999998211860657, 350, 0.086499996483325958, 400, 0.093999996781349182, 450,
        0.10249999910593033, 500, 0.1120000034570694 };
    ssc_data_set_matrix(data, "epsilon_3_21", p_epsilon_3_21, 9, 2);
    ssc_number_t p_epsilon_3_22[1] = { 0.64999997615814209 };
    ssc_data_set_matrix(data, "epsilon_3_22", p_epsilon_3_22, 1, 1);
    ssc_number_t p_epsilon_3_23[1] = { 0.64999997615814209 };
    ssc_data_set_matrix(data, "epsilon_3_23", p_epsilon_3_23, 1, 1);
    ssc_number_t p_epsilon_3_24[1] = { 0 };
    ssc_data_set_matrix(data, "epsilon_3_24", p_epsilon_3_24, 1, 1);
    ssc_number_t p_epsilon_3_31[18] = { 100, 0.064000003039836884, 150, 0.066500000655651093, 200, 0.070000000298023224, 250,
        0.074500001966953278, 300, 0.079999998211860657, 350, 0.086499996483325958, 400, 0.093999996781349182, 450,
        0.10249999910593033, 500, 0.1120000034570694 };
    ssc_data_set_matrix(data, "epsilon_3_31", p_epsilon_3_31, 9, 2);
    ssc_number_t p_epsilon_3_32[1] = { 0.64999997615814209 };
    ssc_data_set_matrix(data, "epsilon_3_32", p_epsilon_3_32, 1, 1);
    ssc_number_t p_epsilon_3_33[1] = { 0.64999997615814209 };
    ssc_data_set_matrix(data, "epsilon_3_33", p_epsilon_3_33, 1, 1);
    ssc_number_t p_epsilon_3_34[1] = { 0 };
    ssc_data_set_matrix(data, "epsilon_3_34", p_epsilon_3_34, 1, 1);
    ssc_number_t p_epsilon_3_41[18] = { 100, 0.064000003039836884, 150, 0.066500000655651093, 200, 0.070000000298023224, 250,
        0.074500001966953278, 300, 0.079999998211860657, 350, 0.086499996483325958, 400, 0.093999996781349182, 450,
        0.10249999910593033, 500, 0.1120000034570694 };
    ssc_data_set_matrix(data, "epsilon_3_41", p_epsilon_3_41, 9, 2);
    ssc_number_t p_epsilon_3_42[1] = { 0.64999997615814209 };
    ssc_data_set_matrix(data, "epsilon_3_42", p_epsilon_3_42, 1, 1);
    ssc_number_t p_epsilon_3_43[1] = { 0.64999997615814209 };
    ssc_data_set_matrix(data, "epsilon_3_43", p_epsilon_3_43, 1, 1);
    ssc_number_t p_epsilon_3_44[1] = { 0 };
    ssc_data_set_matrix(data, "epsilon_3_44", p_epsilon_3_44, 1, 1);
    ssc_number_t p_alpha_abs[16] = { 0.96299999952316284, 0.96299999952316284, 0.80000001192092896, 0, 0.96299999952316284,
        0.96299999952316284, 0.80000001192092896, 0, 0.96299999952316284, 0.96299999952316284, 0.80000001192092896, 0,
        0.96299999952316284, 0.96299999952316284, 0.80000001192092896, 0 };
    ssc_data_set_matrix(data, "alpha_abs", p_alpha_abs, 4, 4);
    ssc_number_t p_Tau_envelope[16] = { 0.96399998664855957, 0.96399998664855957, 1, 0, 0.96399998664855957, 0.96399998664855957,
        1, 0, 0.96399998664855957, 0.96399998664855957, 1, 0, 0.96399998664855957, 0.96399998664855957, 1, 0 };
    ssc_data_set_matrix(data, "Tau_envelope", p_Tau_envelope, 4, 4);
    ssc_number_t p_EPSILON_4[16] = { 0.86000001430511475, 0.86000001430511475, 1, 0, 0.86000001430511475, 0.86000001430511475,
        1, 0, 0.86000001430511475, 0.86000001430511475, 1, 0, 0.86000001430511475, 0.86000001430511475, 1, 0 };
    ssc_data_set_matrix(data, "EPSILON_4", p_EPSILON_4, 4, 4);
    ssc_number_t p_EPSILON_5[16] = { 0.86000001430511475, 0.86000001430511475, 1, 0, 0.86000001430511475, 0.86000001430511475,
        1, 0, 0.86000001430511475, 0.86000001430511475, 1, 0, 0.86000001430511475, 0.86000001430511475, 1, 0 };
    ssc_data_set_matrix(data, "EPSILON_5", p_EPSILON_5, 4, 4);
    ssc_number_t p_GlazingIntactIn[16] = { 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1 };
    ssc_data_set_matrix(data, "GlazingIntactIn", p_GlazingIntactIn, 4, 4);
    ssc_number_t p_P_a[16] = { 9.9999997473787516e-05, 750, 750, 0, 9.9999997473787516e-05, 750, 750, 0, 9.9999997473787516e-05,
        750, 750, 0, 9.9999997473787516e-05, 750, 750, 0 };
    ssc_data_set_matrix(data, "P_a", p_P_a, 4, 4);
    ssc_number_t p_AnnulusGas[16] = { 27, 1, 1, 1, 27, 1, 1, 1, 27, 1, 1, 27, 27, 1, 1, 27 };
    ssc_data_set_matrix(data, "AnnulusGas", p_AnnulusGas, 4, 4);
    ssc_number_t p_AbsorberMaterial[16] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "AbsorberMaterial", p_AbsorberMaterial, 4, 4);
    ssc_number_t p_Shadowing[16] = { 0.93500000238418579, 0.93500000238418579, 0.93500000238418579, 0.96299999952316284,
        0.93500000238418579, 0.93500000238418579, 0.93500000238418579, 0.96299999952316284, 0.93500000238418579,
        0.93500000238418579, 0.93500000238418579, 0.96299999952316284, 0.93500000238418579, 0.93500000238418579,
        0.93500000238418579, 0.96299999952316284 };
    ssc_data_set_matrix(data, "Shadowing", p_Shadowing, 4, 4);
    ssc_number_t p_Dirt_HCE[16] = { 0.98000001907348633, 0.98000001907348633, 1, 0.98000001907348633, 0.98000001907348633,
        0.98000001907348633, 1, 0.98000001907348633, 0.98000001907348633, 0.98000001907348633, 1, 0.98000001907348633,
        0.98000001907348633, 0.98000001907348633, 1, 0.98000001907348633 };
    ssc_data_set_matrix(data, "Dirt_HCE", p_Dirt_HCE, 4, 4);
    ssc_number_t p_Design_loss[16] = { 190, 1270, 1500, 0, 190, 1270, 1500, 0, 190, 1270, 1500, 0, 190, 1270, 1500, 0 };
    ssc_data_set_matrix(data, "Design_loss", p_Design_loss, 4, 4);
    ssc_number_t p_SCAInfoArray[8] = { 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "SCAInfoArray", p_SCAInfoArray, 4, 2);
    ssc_number_t p_SCADefocusArray[4] = { 4, 3, 2, 1 };
    ssc_data_set_array(data, "SCADefocusArray", p_SCADefocusArray, 4);
    ssc_data_set_number(data, "pb_pump_coef", 0.55000001192092896);
    ssc_data_set_number(data, "init_hot_htf_percent", 30);
    ssc_data_set_number(data, "h_tank", 15);
    ssc_data_set_number(data, "cold_tank_max_heat", 0.5);
    ssc_data_set_number(data, "u_tank", 0.30000001192092896);
    ssc_data_set_number(data, "tank_pairs", 1);
    ssc_data_set_number(data, "cold_tank_Thtr", 60);
    ssc_data_set_number(data, "h_tank_min", 0.5);
    ssc_data_set_number(data, "hot_tank_Thtr", 110);
    ssc_data_set_number(data, "hot_tank_max_heat", 1);
    ssc_data_set_number(data, "adjust:constant", 4);
}

/**
*  Default data for iph_to_lcoefcr run that can be further modified
*/
void convert_and_adjust_fixed_charge(ssc_data_t &data)
{
    ssc_data_set_number(data, "electricity_rate", 0.059999998658895493);
    ssc_data_set_number(data, "fixed_operating_cost", 103758.203125);
}

/**
*  Default data for lcoefcr run that can be further modified
*/
void fixed_charge_rate_default(ssc_data_t &data)
{
    ssc_data_set_number(data, "capital_cost", 7263074);
    ssc_data_set_number(data, "variable_operating_cost", 0.0010000000474974513);
    ssc_data_set_number(data, "fixed_charge_rate", 0.10807877779006958);
}

#endif
