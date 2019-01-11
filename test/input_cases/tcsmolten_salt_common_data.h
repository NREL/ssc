#ifndef _TCSMOLTEN_SALT_COMMON_DATA_H_
#define _TCSMOLTEN_SALT_COMMON_DATA_H_

#include <stdio.h>

#include "code_generator_utilities.h"

//const char * SSCDIR = std::getenv("SSCDIR");
//const char * SAMNTDIR = std::getenv("SAMNTDIR");

char dispatch_factors_path[200];
char ud_ind_od_path[200];
char wlim_series_path[200];
char helio_positions_path[200];


int nmspt1 = sprintf(dispatch_factors_path, "%s/test/input_cases/moltensalt_data/dispatch_factors_ts.csv", std::getenv("SSCDIR"));
int nmspt2 = sprintf(ud_ind_od_path, "%s/test/input_cases/moltensalt_data/ud_ind_od.csv", std::getenv("SSCDIR"));
int nmspt3 = sprintf(wlim_series_path, "%s/test/input_cases/moltensalt_data/wlim_series.csv", std::getenv("SSCDIR"));
int nmspt4 = sprintf(helio_positions_path, "%s/test/input_cases/moltensalt_data/helio_positions.csv", std::getenv("SSCDIR"));

/**
*  Default data for tcsmolten_salt run that can be further modified
*/
void tcsmolten_salt_default(ssc_data_t &data)
{
	char solar_resource_path[200];
	int n1 = sprintf(solar_resource_path, "%s/test/input_cases/moltensalt_data/daggett_ca_34.865371_-116.783023_psmv3_60_tmy.csv", std::getenv("SSCDIR"));

    ssc_data_set_string(data, "solar_resource_file", solar_resource_path);
    ssc_data_set_number(data, "ppa_multiplier_model", 0);
    set_array(data, "dispatch_factors_ts", dispatch_factors_path, 8760);
    ssc_data_set_number(data, "field_model_type", 2);
    ssc_data_set_number(data, "gross_net_conversion_factor", 0.89999997615814209);
    ssc_data_set_number(data, "helio_width", 12.199999809265137);
    ssc_data_set_number(data, "helio_height", 12.199999809265137);
    ssc_data_set_number(data, "helio_optical_error_mrad", 1.5299999713897705);
    ssc_data_set_number(data, "helio_active_fraction", 0.99000000953674316);
    ssc_data_set_number(data, "dens_mirror", 0.97000002861022949);
    ssc_data_set_number(data, "helio_reflectance", 0.89999997615814209);
    ssc_data_set_number(data, "rec_absorptance", 0.93999999761581421);
    ssc_data_set_number(data, "rec_hl_perm2", 30);
    ssc_data_set_number(data, "land_max", 9.5);
    ssc_data_set_number(data, "land_min", 0.75);
    ssc_data_set_number(data, "dni_des", 950);
    ssc_data_set_number(data, "p_start", 0.02500000037252903);
    ssc_data_set_number(data, "p_track", 0.054999999701976776);
    ssc_data_set_number(data, "hel_stow_deploy", 8);
    ssc_data_set_number(data, "v_wind_max", 15);
    ssc_data_set_number(data, "c_atm_0", 0.0067889997735619545);
    ssc_data_set_number(data, "c_atm_1", 0.10459999740123749);
    ssc_data_set_number(data, "c_atm_2", -0.017000000923871994);
    ssc_data_set_number(data, "c_atm_3", 0.002845000009983778);
    ssc_data_set_number(data, "n_facet_x", 2);
    ssc_data_set_number(data, "n_facet_y", 8);
    ssc_data_set_number(data, "focus_type", 1);
    ssc_data_set_number(data, "cant_type", 1);
    ssc_data_set_number(data, "n_flux_days", 8);
    ssc_data_set_number(data, "delta_flux_hrs", 2);
    ssc_data_set_number(data, "water_usage_per_wash", 0.69999998807907104);
    ssc_data_set_number(data, "washing_frequency", 63);
    ssc_data_set_number(data, "check_max_flux", 0);
    ssc_data_set_number(data, "sf_excess", 1);
    ssc_data_set_number(data, "tower_fixed_cost", 3000000);
    ssc_data_set_number(data, "tower_exp", 0.011300000362098217);
    ssc_data_set_number(data, "rec_ref_cost", 103000000);
    ssc_data_set_number(data, "rec_ref_area", 1571);
    ssc_data_set_number(data, "rec_cost_exp", 0.69999998807907104);
    ssc_data_set_number(data, "site_spec_cost", 16);
    ssc_data_set_number(data, "heliostat_spec_cost", 140);
    ssc_data_set_number(data, "plant_spec_cost", 1040);
    ssc_data_set_number(data, "bop_spec_cost", 290);
    ssc_data_set_number(data, "tes_spec_cost", 22);
    ssc_data_set_number(data, "land_spec_cost", 10000);
    ssc_data_set_number(data, "contingency_rate", 7);
    ssc_data_set_number(data, "sales_tax_rate", 5);
    ssc_data_set_number(data, "sales_tax_frac", 80);
    ssc_data_set_number(data, "cost_sf_fixed", 0);
    ssc_data_set_number(data, "fossil_spec_cost", 0);
    ssc_data_set_number(data, "flux_max", 1000);
    ssc_data_set_number(data, "opt_init_step", 0.059999998658895493);
    ssc_data_set_number(data, "opt_max_iter", 200);
    ssc_data_set_number(data, "opt_conv_tol", 0.0010000000474974513);
    ssc_data_set_number(data, "opt_flux_penalty", 0.25);
    ssc_data_set_number(data, "opt_algorithm", 1);
    ssc_data_set_number(data, "csp.pt.cost.epc.per_acre", 0);
    ssc_data_set_number(data, "csp.pt.cost.epc.percent", 13);
    ssc_data_set_number(data, "csp.pt.cost.epc.per_watt", 0);
    ssc_data_set_number(data, "csp.pt.cost.epc.fixed", 0);
    ssc_data_set_number(data, "csp.pt.cost.plm.percent", 0);
    ssc_data_set_number(data, "csp.pt.cost.plm.per_watt", 0);
    ssc_data_set_number(data, "csp.pt.cost.plm.fixed", 0);
    ssc_data_set_number(data, "csp.pt.sf.fixed_land_area", 45);
    ssc_data_set_number(data, "csp.pt.sf.land_overhead_factor", 1);
    ssc_data_set_number(data, "T_htf_cold_des", 290);
    ssc_data_set_number(data, "T_htf_hot_des", 574);
    ssc_data_set_number(data, "P_ref", 115);
    ssc_data_set_number(data, "design_eff", 0.41200000047683716);
    ssc_data_set_number(data, "tshours", 10);
    ssc_data_set_number(data, "solarm", 2.4000000953674316);
    ssc_data_set_number(data, "N_panels", 20);
    ssc_data_set_number(data, "d_tube_out", 40);
    ssc_data_set_number(data, "th_tube", 1.25);
    ssc_data_set_number(data, "mat_tube", 2);
    ssc_data_set_number(data, "rec_htf", 17);
    ssc_number_t p_field_fl_props[9] = { 1, 7, 0, 0, 0, 0, 0, 0, 0 };
    ssc_data_set_matrix(data, "field_fl_props", p_field_fl_props, 1, 9);
    ssc_data_set_number(data, "Flow_type", 1);
    ssc_data_set_number(data, "epsilon", 0.87999999523162842);
    ssc_data_set_number(data, "hl_ffact", 1);
    ssc_data_set_number(data, "f_rec_min", 0.25);
    ssc_data_set_number(data, "rec_su_delay", 0.20000000298023224);
    ssc_data_set_number(data, "rec_qf_delay", 0.25);
    ssc_data_set_number(data, "csp.pt.rec.max_oper_frac", 1.2000000476837158);
    ssc_data_set_number(data, "eta_pump", 0.85000002384185791);
    ssc_data_set_number(data, "piping_loss", 10200);
    ssc_data_set_number(data, "piping_length_mult", 2.5999999046325684);
    ssc_data_set_number(data, "piping_length_const", 0);
    ssc_data_set_number(data, "csp.pt.tes.init_hot_htf_percent", 30);
    ssc_data_set_number(data, "h_tank", 12);
    ssc_data_set_number(data, "cold_tank_max_heat", 15);
    ssc_data_set_number(data, "u_tank", 0.40000000596046448);
    ssc_data_set_number(data, "tank_pairs", 1);
    ssc_data_set_number(data, "cold_tank_Thtr", 280);
    ssc_data_set_number(data, "h_tank_min", 1);
    ssc_data_set_number(data, "hot_tank_Thtr", 500);
    ssc_data_set_number(data, "hot_tank_max_heat", 30);
    ssc_data_set_number(data, "pc_config", 0);
    ssc_data_set_number(data, "pb_pump_coef", 0.55000001192092896);
    ssc_data_set_number(data, "startup_time", 0.5);
    ssc_data_set_number(data, "startup_frac", 0.5);
    ssc_data_set_number(data, "cycle_max_frac", 1.0499999523162842);
    ssc_data_set_number(data, "cycle_cutoff_frac", 0.20000000298023224);
    ssc_data_set_number(data, "q_sby_frac", 0.20000000298023224);
    ssc_data_set_number(data, "dT_cw_ref", 10);
    ssc_data_set_number(data, "T_amb_des", 42);
    ssc_data_set_number(data, "P_boil", 100);
    ssc_data_set_number(data, "CT", 2);
    ssc_data_set_number(data, "T_approach", 5);
    ssc_data_set_number(data, "T_ITD_des", 16);
    ssc_data_set_number(data, "P_cond_ratio", 1.0027999877929688);
    ssc_data_set_number(data, "pb_bd_frac", 0.019999999552965164);
    ssc_data_set_number(data, "P_cond_min", 2);
    ssc_data_set_number(data, "n_pl_inc", 8);
    ssc_number_t p_F_wc[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    ssc_data_set_array(data, "F_wc", p_F_wc, 9);
    ssc_data_set_number(data, "tech_type", 1);
    ssc_data_set_number(data, "ud_T_amb_des", 43);
    ssc_data_set_number(data, "ud_f_W_dot_cool_des", 0);
    ssc_data_set_number(data, "ud_m_dot_water_cool_des", 0);
    ssc_data_set_number(data, "ud_T_htf_low", 500);
    ssc_data_set_number(data, "ud_T_htf_high", 580);
    ssc_data_set_number(data, "ud_T_amb_low", 0);
    ssc_data_set_number(data, "ud_T_amb_high", 55);
    ssc_data_set_number(data, "ud_m_dot_htf_low", 0.30000001192092896);
    ssc_data_set_number(data, "ud_m_dot_htf_high", 1.2000000476837158);
    ssc_number_t p_ud_T_htf_ind_od[260] = { 500, 0.14323300123214722, 0.68514901399612427, 0.76894098520278931, 0.22183099389076233,
        0.73943698406219482, 0.88732397556304932, 1, 1, 1, 1, 1, 1, 504.21099853515625, 0.146807000041008, 0.7022479772567749,
        0.78813201189041138, 0.22627900540828705, 0.75426197052001953, 0.90511500835418701, 1, 1, 1, 1, 1, 1, 508.42098999023438,
        0.15040400624275208, 0.71945101022720337, 0.80743902921676636, 0.23072600364685059, 0.76908797025680542, 0.92290598154067993,
        1, 1, 1, 1, 1, 1, 512.63201904296875, 0.15402199327945709, 0.73675799369812012, 0.82686197757720947, 0.23517400026321411,
        0.78391402959823608, 0.94069701433181763, 1, 1, 1, 1, 1, 1, 516.84197998046875, 0.15766100585460663, 0.75416600704193115,
        0.84639900922775269, 0.23962199687957764, 0.79874002933502197, 0.95848798751831055, 1, 1, 1, 1, 1, 1, 521.052978515625,
        0.16132199764251709, 0.77167600393295288, 0.86605000495910645, 0.24406999349594116, 0.81356602907180786, 0.97627902030944824,
        1, 1, 1, 1, 1, 1, 525.26300048828125, 0.16500300168991089, 0.78928399085998535, 0.88581198453903198, 0.24851700663566589,
        0.82839101552963257, 0.99406999349594116, 1, 1, 1, 1, 1, 1, 529.4739990234375, 0.16870500147342682, 0.80699199438095093,
        0.90568500757217407, 0.25296500325202942, 0.84321701526641846, 1.0118600130081177, 1, 1, 1, 1, 1, 1, 533.68402099609375,
        0.17242699861526489, 0.82479697465896606, 0.92566800117492676, 0.25741299986839294, 0.85804301500320435, 1.0296499729156494,
        1, 1, 1, 1, 1, 1, 537.89501953125, 0.17616899311542511, 0.84269797801971436, 0.94575798511505127, 0.26186099648475647,
        0.87286901473999023, 1.0474400520324707, 1, 1, 1, 1, 1, 1, 542.10498046875, 0.17993099987506866, 0.86069399118423462,
        0.96595501899719238, 0.2663080096244812, 0.88769501447677612, 1.0652300119400024, 1, 1, 1, 1, 1, 1, 546.31597900390625,
        0.18371300399303436, 0.87878400087356567, 0.98625797033309937, 0.27075600624084473, 0.90252000093460083, 1.0830199718475342,
        1, 1, 1, 1, 1, 1, 550.5260009765625, 0.18751400709152222, 0.89696800708770752, 1.006659984588623, 0.27520400285720825,
        0.91734600067138672, 1.1008199453353882, 1, 1, 1, 1, 1, 1, 554.73699951171875, 0.19133499264717102, 0.91524398326873779,
        1.0271799564361572, 0.27965199947357178, 0.93217200040817261, 1.1186100244522095, 1, 1, 1, 1, 1, 1, 558.947021484375,
        0.19517500698566437, 0.93361002206802368, 1.0477900505065918, 0.28409901261329651, 0.9469980001449585, 1.1363999843597412,
        1, 1, 1, 1, 1, 1, 563.15802001953125, 0.19903300702571869, 0.95206701755523682, 1.0685000419616699, 0.28854700922966003,
        0.96182399988174438, 1.1541899442672729, 1, 1, 1, 1, 1, 1, 567.36798095703125, 0.20291000604629517, 0.97061198949813843,
        1.0893199443817139, 0.29299500584602356, 0.97664898633956909, 1.1719800233840942, 1, 1, 1, 1, 1, 1, 571.5789794921875,
        0.20680500566959381, 0.98924601078033447, 1.1102299690246582, 0.29744300246238708, 0.99147498607635498, 1.189769983291626,
        1, 1, 1, 1, 1, 1, 575.78900146484375, 0.21071900427341461, 1.0079699754714966, 1.1312400102615356, 0.30188998579978943,
        1.0062999725341797, 1.2075599431991577, 1, 1, 1, 1, 1, 1, 580, 0.21465100347995758, 1.0267699956893921, 1.1523499488830566,
        0.30633801221847534, 1.0211299657821655, 1.225350022315979, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "ud_T_htf_ind_od", p_ud_T_htf_ind_od, 20, 13);
    ssc_number_t p_ud_T_amb_ind_od[260] = { 0, 0.77080899477005005, 1.1106699705123901, 1.1393799781799316, 0.73943698406219482,
        1, 1.0211299657821655, 1, 1, 1, 1, 1, 1, 2.894740104675293, 0.76483899354934692, 1.1029599905014038, 1.1315399408340454,
        0.73943698406219482, 1, 1.0211299657821655, 1, 1, 1, 1, 1, 1, 5.7894701957702637, 0.75890100002288818, 1.095289945602417,
        1.123729944229126, 0.73943698406219482, 1, 1.0211299657821655, 1, 1, 1, 1, 1, 1, 8.6842098236083984, 0.75299400091171265,
        1.087649941444397, 1.1159600019454956, 0.73943698406219482, 1, 1.0211299657821655, 1, 1, 1, 1, 1, 1, 11.578900337219238,
        0.74711602926254272, 1.0800600051879883, 1.108240008354187, 0.73943698406219482, 1, 1.0211299657821655, 1, 1, 1, 1, 1, 1,
        14.473699569702148, 0.74126899242401123, 1.0724999904632568, 1.1005500555038452, 0.73943698406219482, 1, 1.0211299657821655,
        1, 1, 1, 1, 1, 1, 17.368400573730469, 0.73545098304748535, 1.0649900436401367, 1.0929000377655029, 0.73943698406219482, 1,
        1.0211299657821655, 1, 1, 1, 1, 1, 1, 20.263200759887695, 0.72966200113296509, 1.0575100183486938, 1.0852899551391602,
        0.73943698406219482, 1, 1.0211299657821655, 1, 1, 1, 1, 1, 1, 23.157899856567383, 0.72390097379684448, 1.05007004737854,
        1.077720046043396, 0.73943698406219482, 1, 1.0211299657821655, 1, 1, 1, 1, 1, 1, 26.052600860595703, 0.71816802024841309,
        1.0426599979400635, 1.0701800584793091, 0.73943698406219482, 1, 1.0211299657821655, 1, 1, 1, 1, 1, 1, 28.947399139404297,
        0.71246302127838135, 1.035290002822876, 1.0626800060272217, 0.73943698406219482, 1, 1.0211299657821655, 1, 1, 1, 1, 1, 1,
        31.842100143432617, 0.70678597688674927, 1.0279500484466553, 1.0552200078964233, 0.73943698406219482, 1, 1.0211299657821655,
        1, 1, 1, 1, 1, 1, 34.736801147460938, 0.70113497972488403, 1.0206500291824341, 1.0477900505065918, 0.73943698406219482, 1,
        1.0211299657821655, 1, 1, 1, 1, 1, 1, 37.631599426269531, 0.69551098346710205, 1.0133899450302124, 1.0404000282287598,
        0.73943698406219482, 1, 1.0211299657821655, 1, 1, 1, 1, 1, 1, 40.526298522949219, 0.68991202116012573, 1.0061500072479248,
        1.0330400466918945, 0.73943698406219482, 1, 1.0211299657821655, 1, 1, 1, 1, 1, 1, 43.421100616455078, 0.68434000015258789,
        0.99895501136779785, 1.0257099866867065, 0.73943698406219482, 1, 1.0211299657821655, 1, 1, 1, 1, 1, 1, 46.315799713134766,
        0.67879301309585571, 0.99178802967071533, 1.0184199810028076, 0.73943698406219482, 1, 1.0211299657821655, 1, 1, 1, 1, 1, 1,
        49.210498809814453, 0.67327100038528442, 0.9846540093421936, 1.0111600160598755, 0.73943698406219482, 1, 1.0211299657821655,
        1, 1, 1, 1, 1, 1, 52.105300903320313, 0.6677740216255188, 0.97755199670791626, 1.0039299726486206, 0.73943698406219482, 1,
        1.0211299657821655, 1, 1, 1, 1, 1, 1, 55, 0.66230100393295288, 0.9704819917678833, 0.9967380166053772, 0.73943698406219482,
        1, 1.0211299657821655, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "ud_T_amb_ind_od", p_ud_T_amb_ind_od, 20, 13);
    ssc_number_t p_ud_m_dot_htf_ind_od[260] = { 0.30000001192092896, 0.23218899965286255, 0.20905399322509766, 0.20288300514221191,
        0.30000001192092896, 0.30000001192092896, 0.30000001192092896, 1, 1, 1, 1, 1, 1, 0.34736800193786621, 0.2809390127658844,
        0.25294598937034607, 0.24547900259494781, 0.34736800193786621, 0.34736800193786621, 0.34736800193786621, 1, 1, 1, 1, 1, 1,
        0.39473700523376465, 0.33173000812530518, 0.2986760139465332, 0.28985899686813354, 0.39473700523376465, 0.39473700523376465,
        0.39473700523376465, 1, 1, 1, 1, 1, 1, 0.4421049952507019, 0.38438698649406433, 0.34608501195907593, 0.33586999773979187,
        0.4421049952507019, 0.4421049952507019, 0.4421049952507019, 1, 1, 1, 1, 1, 1, 0.48947399854660034, 0.4387660026550293,
        0.39504599571228027, 0.38338500261306763, 0.48947399854660034, 0.48947399854660034, 0.48947399854660034, 1, 1, 1, 1, 1, 1,
        0.5368419885635376, 0.49474900960922241, 0.4454520046710968, 0.43230301141738892, 0.5368419885635376, 0.5368419885635376,
        0.5368419885635376, 1, 1, 1, 1, 1, 1, 0.58421099185943604, 0.55223602056503296, 0.49720999598503113, 0.48253300786018372,
        0.58421099185943604, 0.58421099185943604, 0.58421099185943604, 1, 1, 1, 1, 1, 1, 0.63157898187637329, 0.61114001274108887,
        0.55024498701095581, 0.53400200605392456, 0.63157898187637329, 0.63157898187637329, 0.63157898187637329, 1, 1, 1, 1, 1, 1,
        0.67894697189331055, 0.67138499021530151, 0.60448700189590454, 0.58664298057556152, 0.67894697189331055, 0.67894697189331055,
        0.67894697189331055, 1, 1, 1, 1, 1, 1, 0.72631597518920898, 0.73290497064590454, 0.65987700223922729, 0.64039897918701172,
        0.72631597518920898, 0.72631597518920898, 0.72631597518920898, 1, 1, 1, 1, 1, 1, 0.77368402481079102, 0.79564201831817627,
        0.71636301279067993, 0.69521701335906982, 0.77368402481079102, 0.77368402481079102, 0.77368402481079102, 1, 1, 1, 1, 1, 1,
        0.82105302810668945, 0.85954201221466064, 0.7738950252532959, 0.75105100870132446, 0.82105302810668945, 0.82105302810668945,
        0.82105302810668945, 1, 1, 1, 1, 1, 1, 0.86842101812362671, 0.92455798387527466, 0.83243298530578613, 0.80786097049713135,
        0.86842101812362671, 0.86842101812362671, 0.86842101812362671, 1, 1, 1, 1, 1, 1, 0.91578900814056396, 0.99064797163009644,
        0.8919370174407959, 0.86560899019241333, 0.91578900814056396, 0.91578900814056396, 0.91578900814056396, 1, 1, 1, 1, 1, 1,
        0.9631580114364624, 1.0577700138092041, 0.95237201452255249, 0.92426002025604248, 0.9631580114364624, 0.9631580114364624,
        0.9631580114364624, 1, 1, 1, 1, 1, 1, 1.0105299949645996, 1.1188000440597534, 1.0073200464248657, 0.97758901119232178,
        1.0105299949645996, 1.0105299949645996, 1.0105299949645996, 1, 1, 1, 1, 1, 1, 1.0578900575637817, 1.1541399955749512,
        1.039139986038208, 1.0084600448608398, 1.0578900575637817, 1.0578900575637817, 1.0578900575637817, 1, 1, 1, 1, 1, 1,
        1.1052600145339966, 1.1872999668121338, 1.0689899921417236, 1.0374399423599243, 1.1052600145339966, 1.1052600145339966,
        1.1052600145339966, 1, 1, 1, 1, 1, 1, 1.1526299715042114, 1.2181400060653687, 1.0967600345611572, 1.0643899440765381,
        1.1526299715042114, 1.1526299715042114, 1.1526299715042114, 1, 1, 1, 1, 1, 1, 1.2000000476837158, 1.2465000152587891,
        1.1223000288009644, 1.089169979095459, 1.2000000476837158, 1.2000000476837158, 1.2000000476837158, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "ud_m_dot_htf_ind_od", p_ud_m_dot_htf_ind_od, 20, 13);
    set_matrix(data, "ud_ind_od", ud_ind_od_path, 180, 7);
    ssc_data_set_number(data, "sco2_cycle_config", 1);
    ssc_data_set_number(data, "eta_c", 0.88999998569488525);
    ssc_data_set_number(data, "eta_t", 0.89999997615814209);
    ssc_data_set_number(data, "recup_eff_max", 0.95999997854232788);
    ssc_data_set_number(data, "P_high_limit", 25);
    ssc_data_set_number(data, "deltaT_PHX", 20);
    ssc_data_set_number(data, "fan_power_perc_net", 1.5);
    ssc_data_set_number(data, "sco2_T_amb_des", 35);
    ssc_data_set_number(data, "sco2_T_approach", 10);
    ssc_data_set_number(data, "is_sco2_preprocess", 0);
    ssc_data_set_number(data, "sco2ud_T_htf_cold_calc", 9.9999999999999998e+37);
    ssc_data_set_number(data, "sco2ud_T_htf_low", 0);
    ssc_data_set_number(data, "sco2ud_T_htf_high", 0);
    ssc_data_set_number(data, "sco2ud_T_amb_low", 0);
    ssc_data_set_number(data, "sco2ud_T_amb_high", 0);
    ssc_data_set_number(data, "sco2ud_m_dot_htf_low", 0);
    ssc_data_set_number(data, "sco2ud_m_dot_htf_high", 0);
    ssc_number_t p_sco2ud_T_htf_ind_od[39] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "sco2ud_T_htf_ind_od", p_sco2ud_T_htf_ind_od, 3, 13);
    ssc_number_t p_sco2ud_T_amb_ind_od[39] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "sco2ud_T_amb_ind_od", p_sco2ud_T_amb_ind_od, 3, 13);
    ssc_number_t p_sco2ud_m_dot_htf_ind_od[39] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "sco2ud_m_dot_htf_ind_od", p_sco2ud_m_dot_htf_ind_od, 3, 13);
    ssc_data_set_number(data, "_sco2_P_high_limit", 9.9999999999999998e+37);
    ssc_data_set_number(data, "_sco2_P_ref", 9.9999999999999998e+37);
    ssc_data_set_number(data, "_sco2_T_amb_des", 9.9999999999999998e+37);
    ssc_data_set_number(data, "_sco2_T_approach", 9.9999999999999998e+37);
    ssc_data_set_number(data, "_sco2_T_htf_hot_des", 9.9999999999999998e+37);
    ssc_data_set_number(data, "_sco2_deltaT_PHX", 9.9999999999999998e+37);
    ssc_data_set_number(data, "_sco2_design_eff", 9.9999999999999998e+37);
    ssc_data_set_number(data, "_sco2_eta_c", 9.9999999999999998e+37);
    ssc_data_set_number(data, "_sco2_eta_t", 9.9999999999999998e+37);
    ssc_data_set_number(data, "_sco2_recup_eff_max", 9.9999999999999998e+37);
    ssc_data_set_number(data, "time_start", 0);
    ssc_data_set_number(data, "time_stop", 31536000);
    ssc_data_set_number(data, "pb_fixed_par", 0.0054999999701976776);
    ssc_data_set_number(data, "aux_par", 0.023000000044703484);
    ssc_data_set_number(data, "aux_par_f", 1);
    ssc_data_set_number(data, "aux_par_0", 0.4830000102519989);
    ssc_data_set_number(data, "aux_par_1", 0.57099997997283936);
    ssc_data_set_number(data, "aux_par_2", 0);
    ssc_data_set_number(data, "bop_par", 0);
    ssc_data_set_number(data, "bop_par_f", 1);
    ssc_data_set_number(data, "bop_par_0", 0);
    ssc_data_set_number(data, "bop_par_1", 0.4830000102519989);
    ssc_data_set_number(data, "bop_par_2", 0);
    ssc_number_t p_f_turb_tou_periods[9] = { 1.0499999523162842, 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_array(data, "f_turb_tou_periods", p_f_turb_tou_periods, 9);
    ssc_number_t p_weekday_schedule[288] = { 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6,
        5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5,
        6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
        4, 4, 4, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2,
        1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5,
        6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
        4, 4, 4, 5, 5, 5 };
    ssc_data_set_matrix(data, "weekday_schedule", p_weekday_schedule, 12, 24);
    ssc_number_t p_weekend_schedule[288] = { 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6,
        5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 5, 5, 5 };
    ssc_data_set_matrix(data, "weekend_schedule", p_weekend_schedule, 12, 24);
    ssc_data_set_number(data, "is_dispatch", 0);
    ssc_data_set_number(data, "disp_horizon", 48);
    ssc_data_set_number(data, "disp_frequency", 24);
    ssc_data_set_number(data, "disp_max_iter", 35000);
    ssc_data_set_number(data, "disp_timeout", 5);
    ssc_data_set_number(data, "disp_mip_gap", 0.0010000000474974513);
    ssc_data_set_number(data, "disp_time_weighting", 0.99000000953674316);
    ssc_data_set_number(data, "disp_rsu_cost", 950);
    ssc_data_set_number(data, "disp_csu_cost", 10000);
    ssc_data_set_number(data, "disp_pen_delta_w", 0.10000000149011612);
    ssc_data_set_number(data, "is_wlim_series", 0);
    set_array(data, "wlim_series", wlim_series_path, 8760);
    ssc_number_t p_dispatch_sched_weekday[288] = { 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6,
        6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5,
        5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4,
        4, 4, 4, 4, 4, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2,
        2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5,
        5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4,
        4, 4, 4, 4, 4, 5, 5, 5 };
    ssc_data_set_matrix(data, "dispatch_sched_weekday", p_dispatch_sched_weekday, 12, 24);
    ssc_number_t p_dispatch_sched_weekend[288] = { 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6,
        6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 5, 5, 5, 5, 5 };
    ssc_data_set_matrix(data, "dispatch_sched_weekend", p_dispatch_sched_weekend, 12, 24);
    ssc_data_set_number(data, "dispatch_factor1", 2.0639998912811279);
    ssc_data_set_number(data, "dispatch_factor2", 1.2000000476837158);
    ssc_data_set_number(data, "dispatch_factor3", 1);
    ssc_data_set_number(data, "dispatch_factor4", 1.1000000238418579);
    ssc_data_set_number(data, "dispatch_factor5", 0.80000001192092896);
    ssc_data_set_number(data, "dispatch_factor6", 0.69999998807907104);
    ssc_data_set_number(data, "dispatch_factor7", 1);
    ssc_data_set_number(data, "dispatch_factor8", 1);
    ssc_data_set_number(data, "dispatch_factor9", 1);
    ssc_data_set_number(data, "is_dispatch_series", 0);
    ssc_number_t p_dispatch_series[1] = { 0 };
    ssc_data_set_array(data, "dispatch_series", p_dispatch_series, 1);
    ssc_data_set_number(data, "rec_height", 21.602899551391602);
    ssc_data_set_number(data, "D_rec", 17.649999618530273);
    ssc_data_set_number(data, "h_tower", 193.45799255371094);
    set_matrix(data, "helio_positions", helio_positions_path, 8790, 2);
    ssc_data_set_number(data, "land_area_base", 1847.0400390625);
    ssc_data_set_number(data, "const_per_interest_rate1", 4);
    ssc_data_set_number(data, "const_per_interest_rate2", 0);
    ssc_data_set_number(data, "const_per_interest_rate3", 0);
    ssc_data_set_number(data, "const_per_interest_rate4", 0);
    ssc_data_set_number(data, "const_per_interest_rate5", 0);
    ssc_data_set_number(data, "const_per_months1", 24);
    ssc_data_set_number(data, "const_per_months2", 0);
    ssc_data_set_number(data, "const_per_months3", 0);
    ssc_data_set_number(data, "const_per_months4", 0);
    ssc_data_set_number(data, "const_per_months5", 0);
    ssc_data_set_number(data, "const_per_percent1", 100);
    ssc_data_set_number(data, "const_per_percent2", 0);
    ssc_data_set_number(data, "const_per_percent3", 0);
    ssc_data_set_number(data, "const_per_percent4", 0);
    ssc_data_set_number(data, "const_per_percent5", 0);
    ssc_data_set_number(data, "const_per_upfront_rate1", 1);
    ssc_data_set_number(data, "const_per_upfront_rate2", 0);
    ssc_data_set_number(data, "const_per_upfront_rate3", 0);
    ssc_data_set_number(data, "const_per_upfront_rate4", 0);
    ssc_data_set_number(data, "const_per_upfront_rate5", 0);
    ssc_data_set_number(data, "adjust:constant", 4);
    ssc_data_set_number(data, "sf_adjust:constant", 0);
}

/**
*  Default data for the PPA single owner (utility) run that can be further modified
*/
void single_owner_default(ssc_data_t &data)
{
    ssc_data_set_number(data, "analysis_period", 25);
    ssc_number_t p_federal_tax_rate[1] = { 21 };
    ssc_data_set_array(data, "federal_tax_rate", p_federal_tax_rate, 1);
    ssc_number_t p_state_tax_rate[1] = { 7 };
    ssc_data_set_array(data, "state_tax_rate", p_state_tax_rate, 1);
    ssc_data_set_number(data, "property_tax_rate", 0);
    ssc_data_set_number(data, "prop_tax_cost_assessed_percent", 100);
    ssc_data_set_number(data, "prop_tax_assessed_decline", 0);
    ssc_data_set_number(data, "real_discount_rate", 6.4000000953674316);
    ssc_data_set_number(data, "inflation_rate", 2.5);
    ssc_data_set_number(data, "insurance_rate", 0.5);
    ssc_data_set_number(data, "system_capacity", 103500);
    ssc_number_t p_om_fixed[1] = { 0 };
    ssc_data_set_array(data, "om_fixed", p_om_fixed, 1);
    ssc_data_set_number(data, "om_fixed_escal", 0);
    ssc_number_t p_om_production[1] = { 3.5 };
    ssc_data_set_array(data, "om_production", p_om_production, 1);
    ssc_data_set_number(data, "om_production_escal", 0);
    ssc_number_t p_om_capacity[1] = { 66 };
    ssc_data_set_array(data, "om_capacity", p_om_capacity, 1);
    ssc_data_set_number(data, "om_capacity_escal", 0);
    ssc_number_t p_om_fuel_cost[1] = { 0 };
    ssc_data_set_array(data, "om_fuel_cost", p_om_fuel_cost, 1);
    ssc_data_set_number(data, "om_fuel_cost_escal", 0);
    ssc_data_set_number(data, "itc_fed_amount", 0);
    ssc_data_set_number(data, "itc_fed_amount_deprbas_fed", 1);
    ssc_data_set_number(data, "itc_fed_amount_deprbas_sta", 1);
    ssc_data_set_number(data, "itc_sta_amount", 0);
    ssc_data_set_number(data, "itc_sta_amount_deprbas_fed", 0);
    ssc_data_set_number(data, "itc_sta_amount_deprbas_sta", 0);
    ssc_data_set_number(data, "itc_fed_percent", 30);
    ssc_data_set_number(data, "itc_fed_percent_maxvalue", 9.9999996802856925e+37);
    ssc_data_set_number(data, "itc_fed_percent_deprbas_fed", 1);
    ssc_data_set_number(data, "itc_fed_percent_deprbas_sta", 1);
    ssc_data_set_number(data, "itc_sta_percent", 0);
    ssc_data_set_number(data, "itc_sta_percent_maxvalue", 9.9999996802856925e+37);
    ssc_data_set_number(data, "itc_sta_percent_deprbas_fed", 0);
    ssc_data_set_number(data, "itc_sta_percent_deprbas_sta", 0);
    ssc_number_t p_ptc_fed_amount[1] = { 0 };
    ssc_data_set_array(data, "ptc_fed_amount", p_ptc_fed_amount, 1);
    ssc_data_set_number(data, "ptc_fed_term", 10);
    ssc_data_set_number(data, "ptc_fed_escal", 0);
    ssc_number_t p_ptc_sta_amount[1] = { 0 };
    ssc_data_set_array(data, "ptc_sta_amount", p_ptc_sta_amount, 1);
    ssc_data_set_number(data, "ptc_sta_term", 10);
    ssc_data_set_number(data, "ptc_sta_escal", 0);
    ssc_data_set_number(data, "ibi_fed_amount", 0);
    ssc_data_set_number(data, "ibi_fed_amount_tax_fed", 1);
    ssc_data_set_number(data, "ibi_fed_amount_tax_sta", 1);
    ssc_data_set_number(data, "ibi_fed_amount_deprbas_fed", 0);
    ssc_data_set_number(data, "ibi_fed_amount_deprbas_sta", 0);
    ssc_data_set_number(data, "ibi_sta_amount", 0);
    ssc_data_set_number(data, "ibi_sta_amount_tax_fed", 1);
    ssc_data_set_number(data, "ibi_sta_amount_tax_sta", 1);
    ssc_data_set_number(data, "ibi_sta_amount_deprbas_fed", 0);
    ssc_data_set_number(data, "ibi_sta_amount_deprbas_sta", 0);
    ssc_data_set_number(data, "ibi_uti_amount", 0);
    ssc_data_set_number(data, "ibi_uti_amount_tax_fed", 1);
    ssc_data_set_number(data, "ibi_uti_amount_tax_sta", 1);
    ssc_data_set_number(data, "ibi_uti_amount_deprbas_fed", 0);
    ssc_data_set_number(data, "ibi_uti_amount_deprbas_sta", 0);
    ssc_data_set_number(data, "ibi_oth_amount", 0);
    ssc_data_set_number(data, "ibi_oth_amount_tax_fed", 1);
    ssc_data_set_number(data, "ibi_oth_amount_tax_sta", 1);
    ssc_data_set_number(data, "ibi_oth_amount_deprbas_fed", 0);
    ssc_data_set_number(data, "ibi_oth_amount_deprbas_sta", 0);
    ssc_data_set_number(data, "ibi_fed_percent", 0);
    ssc_data_set_number(data, "ibi_fed_percent_maxvalue", 9.9999996802856925e+37);
    ssc_data_set_number(data, "ibi_fed_percent_tax_fed", 1);
    ssc_data_set_number(data, "ibi_fed_percent_tax_sta", 1);
    ssc_data_set_number(data, "ibi_fed_percent_deprbas_fed", 0);
    ssc_data_set_number(data, "ibi_fed_percent_deprbas_sta", 0);
    ssc_data_set_number(data, "ibi_sta_percent", 0);
    ssc_data_set_number(data, "ibi_sta_percent_maxvalue", 9.9999996802856925e+37);
    ssc_data_set_number(data, "ibi_sta_percent_tax_fed", 1);
    ssc_data_set_number(data, "ibi_sta_percent_tax_sta", 1);
    ssc_data_set_number(data, "ibi_sta_percent_deprbas_fed", 0);
    ssc_data_set_number(data, "ibi_sta_percent_deprbas_sta", 0);
    ssc_data_set_number(data, "ibi_uti_percent", 0);
    ssc_data_set_number(data, "ibi_uti_percent_maxvalue", 9.9999996802856925e+37);
    ssc_data_set_number(data, "ibi_uti_percent_tax_fed", 1);
    ssc_data_set_number(data, "ibi_uti_percent_tax_sta", 1);
    ssc_data_set_number(data, "ibi_uti_percent_deprbas_fed", 0);
    ssc_data_set_number(data, "ibi_uti_percent_deprbas_sta", 0);
    ssc_data_set_number(data, "ibi_oth_percent", 0);
    ssc_data_set_number(data, "ibi_oth_percent_maxvalue", 9.9999996802856925e+37);
    ssc_data_set_number(data, "ibi_oth_percent_tax_fed", 1);
    ssc_data_set_number(data, "ibi_oth_percent_tax_sta", 1);
    ssc_data_set_number(data, "ibi_oth_percent_deprbas_fed", 0);
    ssc_data_set_number(data, "ibi_oth_percent_deprbas_sta", 0);
    ssc_data_set_number(data, "cbi_fed_amount", 0);
    ssc_data_set_number(data, "cbi_fed_maxvalue", 9.9999996802856925e+37);
    ssc_data_set_number(data, "cbi_fed_tax_fed", 1);
    ssc_data_set_number(data, "cbi_fed_tax_sta", 1);
    ssc_data_set_number(data, "cbi_fed_deprbas_fed", 0);
    ssc_data_set_number(data, "cbi_fed_deprbas_sta", 0);
    ssc_data_set_number(data, "cbi_sta_amount", 0);
    ssc_data_set_number(data, "cbi_sta_maxvalue", 9.9999996802856925e+37);
    ssc_data_set_number(data, "cbi_sta_tax_fed", 1);
    ssc_data_set_number(data, "cbi_sta_tax_sta", 1);
    ssc_data_set_number(data, "cbi_sta_deprbas_fed", 0);
    ssc_data_set_number(data, "cbi_sta_deprbas_sta", 0);
    ssc_data_set_number(data, "cbi_uti_amount", 0);
    ssc_data_set_number(data, "cbi_uti_maxvalue", 9.9999996802856925e+37);
    ssc_data_set_number(data, "cbi_uti_tax_fed", 1);
    ssc_data_set_number(data, "cbi_uti_tax_sta", 1);
    ssc_data_set_number(data, "cbi_uti_deprbas_fed", 0);
    ssc_data_set_number(data, "cbi_uti_deprbas_sta", 0);
    ssc_data_set_number(data, "cbi_oth_amount", 0);
    ssc_data_set_number(data, "cbi_oth_maxvalue", 9.9999996802856925e+37);
    ssc_data_set_number(data, "cbi_oth_tax_fed", 1);
    ssc_data_set_number(data, "cbi_oth_tax_sta", 1);
    ssc_data_set_number(data, "cbi_oth_deprbas_fed", 0);
    ssc_data_set_number(data, "cbi_oth_deprbas_sta", 0);
    ssc_number_t p_pbi_fed_amount[1] = { 0 };
    ssc_data_set_array(data, "pbi_fed_amount", p_pbi_fed_amount, 1);
    ssc_data_set_number(data, "pbi_fed_term", 0);
    ssc_data_set_number(data, "pbi_fed_escal", 0);
    ssc_data_set_number(data, "pbi_fed_tax_fed", 1);
    ssc_data_set_number(data, "pbi_fed_tax_sta", 1);
    ssc_number_t p_pbi_sta_amount[1] = { 0 };
    ssc_data_set_array(data, "pbi_sta_amount", p_pbi_sta_amount, 1);
    ssc_data_set_number(data, "pbi_sta_term", 0);
    ssc_data_set_number(data, "pbi_sta_escal", 0);
    ssc_data_set_number(data, "pbi_sta_tax_fed", 1);
    ssc_data_set_number(data, "pbi_sta_tax_sta", 1);
    ssc_number_t p_pbi_uti_amount[1] = { 0 };
    ssc_data_set_array(data, "pbi_uti_amount", p_pbi_uti_amount, 1);
    ssc_data_set_number(data, "pbi_uti_term", 0);
    ssc_data_set_number(data, "pbi_uti_escal", 0);
    ssc_data_set_number(data, "pbi_uti_tax_fed", 1);
    ssc_data_set_number(data, "pbi_uti_tax_sta", 1);
    ssc_number_t p_pbi_oth_amount[1] = { 0 };
    ssc_data_set_array(data, "pbi_oth_amount", p_pbi_oth_amount, 1);
    ssc_data_set_number(data, "pbi_oth_term", 0);
    ssc_data_set_number(data, "pbi_oth_escal", 0);
    ssc_data_set_number(data, "pbi_oth_tax_fed", 1);
    ssc_data_set_number(data, "pbi_oth_tax_sta", 1);
    ssc_number_t p_degradation[1] = { 0 };
    ssc_data_set_array(data, "degradation", p_degradation, 1);
    ssc_number_t p_roe_input[1] = { 0 };
    ssc_data_set_array(data, "roe_input", p_roe_input, 1);
    ssc_data_set_number(data, "loan_moratorium", 0);
    ssc_data_set_number(data, "system_use_recapitalization", 0);
    ssc_data_set_number(data, "system_use_lifetime_output", 0);
    ssc_data_set_number(data, "total_installed_cost", 673465536);
    ssc_data_set_number(data, "reserves_interest", 1.75);
    ssc_data_set_number(data, "equip1_reserve_cost", 0);
    ssc_data_set_number(data, "equip1_reserve_freq", 12);
    ssc_data_set_number(data, "equip2_reserve_cost", 0);
    ssc_data_set_number(data, "equip2_reserve_freq", 15);
    ssc_data_set_number(data, "equip3_reserve_cost", 0);
    ssc_data_set_number(data, "equip3_reserve_freq", 3);
    ssc_data_set_number(data, "equip_reserve_depr_sta", 0);
    ssc_data_set_number(data, "equip_reserve_depr_fed", 0);
    ssc_data_set_number(data, "salvage_percentage", 0);
    ssc_data_set_number(data, "ppa_soln_mode", 0);
    ssc_data_set_number(data, "ppa_price_input", 0.12999999523162842);
    ssc_data_set_number(data, "ppa_escalation", 1);
    ssc_data_set_number(data, "construction_financing_cost", 33673276);
    ssc_data_set_number(data, "term_tenor", 18);
    ssc_data_set_number(data, "term_int_rate", 7);
    ssc_data_set_number(data, "dscr", 1.2999999523162842);
    ssc_data_set_number(data, "dscr_reserve_months", 6);
    ssc_data_set_number(data, "debt_percent", 50);
    ssc_data_set_number(data, "debt_option", 1);
    ssc_data_set_number(data, "payment_option", 0);
    ssc_data_set_number(data, "cost_debt_closing", 450000);
    ssc_data_set_number(data, "cost_debt_fee", 2.75);
    ssc_data_set_number(data, "months_working_reserve", 6);
    ssc_data_set_number(data, "months_receivables_reserve", 0);
    ssc_data_set_number(data, "cost_other_financing", 0);
    ssc_data_set_number(data, "flip_target_percent", 11);
    ssc_data_set_number(data, "flip_target_year", 20);
    ssc_data_set_number(data, "depr_alloc_macrs_5_percent", 90);
    ssc_data_set_number(data, "depr_alloc_macrs_15_percent", 1.5);
    ssc_data_set_number(data, "depr_alloc_sl_5_percent", 0);
    ssc_data_set_number(data, "depr_alloc_sl_15_percent", 2.5);
    ssc_data_set_number(data, "depr_alloc_sl_20_percent", 3);
    ssc_data_set_number(data, "depr_alloc_sl_39_percent", 0);
    ssc_data_set_number(data, "depr_alloc_custom_percent", 0);
    ssc_number_t p_depr_custom_schedule[1] = { 0 };
    ssc_data_set_array(data, "depr_custom_schedule", p_depr_custom_schedule, 1);
    ssc_data_set_number(data, "depr_bonus_sta", 0);
    ssc_data_set_number(data, "depr_bonus_sta_macrs_5", 1);
    ssc_data_set_number(data, "depr_bonus_sta_macrs_15", 1);
    ssc_data_set_number(data, "depr_bonus_sta_sl_5", 0);
    ssc_data_set_number(data, "depr_bonus_sta_sl_15", 0);
    ssc_data_set_number(data, "depr_bonus_sta_sl_20", 0);
    ssc_data_set_number(data, "depr_bonus_sta_sl_39", 0);
    ssc_data_set_number(data, "depr_bonus_sta_custom", 0);
    ssc_data_set_number(data, "depr_bonus_fed", 0);
    ssc_data_set_number(data, "depr_bonus_fed_macrs_5", 1);
    ssc_data_set_number(data, "depr_bonus_fed_macrs_15", 1);
    ssc_data_set_number(data, "depr_bonus_fed_sl_5", 0);
    ssc_data_set_number(data, "depr_bonus_fed_sl_15", 0);
    ssc_data_set_number(data, "depr_bonus_fed_sl_20", 0);
    ssc_data_set_number(data, "depr_bonus_fed_sl_39", 0);
    ssc_data_set_number(data, "depr_bonus_fed_custom", 0);
    ssc_data_set_number(data, "depr_itc_sta_macrs_5", 1);
    ssc_data_set_number(data, "depr_itc_sta_macrs_15", 0);
    ssc_data_set_number(data, "depr_itc_sta_sl_5", 0);
    ssc_data_set_number(data, "depr_itc_sta_sl_15", 0);
    ssc_data_set_number(data, "depr_itc_sta_sl_20", 0);
    ssc_data_set_number(data, "depr_itc_sta_sl_39", 0);
    ssc_data_set_number(data, "depr_itc_sta_custom", 0);
    ssc_data_set_number(data, "depr_itc_fed_macrs_5", 1);
    ssc_data_set_number(data, "depr_itc_fed_macrs_15", 0);
    ssc_data_set_number(data, "depr_itc_fed_sl_5", 0);
    ssc_data_set_number(data, "depr_itc_fed_sl_15", 0);
    ssc_data_set_number(data, "depr_itc_fed_sl_20", 0);
    ssc_data_set_number(data, "depr_itc_fed_sl_39", 0);
    ssc_data_set_number(data, "depr_itc_fed_custom", 0);
    ssc_data_set_number(data, "pbi_fed_for_ds", 0);
    ssc_data_set_number(data, "pbi_sta_for_ds", 0);
    ssc_data_set_number(data, "pbi_uti_for_ds", 0);
    ssc_data_set_number(data, "pbi_oth_for_ds", 0);
    ssc_data_set_number(data, "depr_stabas_method", 1);
    ssc_data_set_number(data, "depr_fedbas_method", 1);
}

#endif
