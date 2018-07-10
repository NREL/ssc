#include <gtest/gtest.h>
#include <memory>
#include <vector>
#include <unordered_map>
#include "simulation_test_info.h"
#include "computeModuleTest.h"

// first test must be contain all possible inputs
std::vector<SimulationTestTable*> pvSAMv1IntgTests;
std::unordered_map<std::string, size_t> pvSAMv1IntgVarMap;
computeModuleTestData pvSAMv1Testing(&pvSAMv1IntgTests, &pvSAMv1IntgVarMap, "pvsamv1");

// Set up input file paths
char solar_resource_path[100];
char solar_resource_path_15_min[100];
char load_profile_path[100];
char target_power_path[100];
char sell_rate_path[100];
char subarray1_shading[100];
char subarray2_shading[100];

int n1 = sprintf(solar_resource_path, "%s/test/input_cases/pvsamv1_data/USA AZ Phoenix (TMY2).csv", NUM, std::getenv("SSCDIR"));
int n5 = sprintf(solar_resource_path_15_min, "%s/test/input_cases/pvsamv1_data/LosAngeles_WeatherFile_15min.csv", NUM, std::getenv("SSCDIR"));
int n2 = sprintf(load_profile_path, "%s/test/input_cases/pvsamv1_data/pvsamv1_residential_load.csv", NUM, std::getenv("SSCDIR"));
int n3 = sprintf(target_power_path, "%s/test/input_cases/pvsamv1_data/pvsamv1_batt_target_power.csv", NUM, std::getenv("SSCDIR"));
int n4 = sprintf(sell_rate_path, "%s/test/input_cases/pvsamv1_data/pvsamv1_ur_ts_sell_rate.csv", NUM, std::getenv("SSCDIR"));
int n6 = sprintf(subarray1_shading, "%s/test/input_cases/pvsamv1_data/subarray1_shading_timestep.csv", NUM, std::getenv("SSCDIR"));
int n7 = sprintf(subarray2_shading, "%s/test/input_cases/pvsamv1_data/subarray2_shading_timestep.csv", NUM, std::getenv("SSCDIR"));

// Set up large arrays
const char* inv_pd_partload_str = "0, 0.40400001406669617, 0.80800002813339233, 1.2120000123977661, 1.6160000562667847, 2.0199999809265137, 2.4240000247955322, 2.8280000686645508, 3.2320001125335693, 3.6359999179840088, 4.0399999618530273, 4.4439997673034668, 4.8480000495910645, 5.2519998550415039, 5.6560001373291016, 6.059999942779541, 6.4640002250671387, 6.8680000305175781, 7.2719998359680176, 7.6760001182556152, 8.0799999237060547, 8.4840002059936523, 8.8879995346069336, 9.2919998168945313, 9.6960000991821289, 10.100000381469727, 10.503999710083008, 10.907999992370605, 11.312000274658203, 11.715999603271484, 12.119999885559082, 12.52400016784668, 12.928000450134277, 13.331999778747559, 13.736000061035156, 14.140000343322754, 14.543999671936035, 14.947999954223633, 15.35200023651123, 15.755999565124512, 16.159999847412109, 16.563999176025391, 16.968000411987305, 17.371999740600586, 17.775999069213867, 18.180000305175781, 18.583999633789063, 18.988000869750977, 19.392000198364258, 19.795999526977539, 20.200000762939453, 20.604000091552734, 21.007999420166016, 21.41200065612793, 21.815999984741211, 22.219999313354492, 22.624000549316406, 23.027999877929688, 23.431999206542969, 23.836000442504883, 24.239999771118164, 24.643999099731445, 25.048000335693359, 25.451999664306641, 25.856000900268555, 26.260000228881836, 26.663999557495117, 27.068000793457031, 27.472000122070313, 27.875999450683594, 28.280000686645508, 28.684000015258789, 29.08799934387207, 29.492000579833984, 29.895999908447266, 30.299999237060547, 30.704000473022461, 31.107999801635742, 31.511999130249023, 31.916000366210938, 32.319999694824219, 32.7239990234375, 33.127998352050781, 33.532001495361328, 33.936000823974609, 34.340000152587891, 34.743999481201172, 35.147998809814453, 35.551998138427734, 35.956001281738281, 36.360000610351563, 36.763999938964844, 37.167999267578125, 37.571998596191406, 37.976001739501953, 38.380001068115234, 38.784000396728516, 39.187999725341797, 39.591999053955078, 39.995998382568359, 40.400001525878906, 40.804000854492188, 41.208000183105469, 41.61199951171875, 42.015998840332031, 42.419998168945313, 42.824001312255859, 43.228000640869141, 43.631999969482422, 44.035999298095703, 44.439998626708984, 44.844001770019531, 45.248001098632813, 45.652000427246094, 46.055999755859375, 46.459999084472656, 46.863998413085938, 47.268001556396484, 47.672000885009766, 48.076000213623047, 48.479999542236328, 48.883998870849609, 49.287998199462891, 49.692001342773438, 50.096000671386719, 50.5, 50.903999328613281, 51.307998657226563, 51.712001800537109, 52.116001129150391, 52.520000457763672, 52.923999786376953, 53.327999114990234, 53.731998443603516, 54.136001586914063, 54.540000915527344, 54.944000244140625, 55.347999572753906, 55.751998901367188, 56.155998229980469, 56.560001373291016, 56.964000701904297, 57.368000030517578, 57.771999359130859, 58.175998687744141, 58.580001831054688, 58.984001159667969, 59.38800048828125, 59.791999816894531, 60.195999145507813, 60.599998474121094, 61.004001617431641, 61.408000946044922, 61.812000274658203, 62.215999603271484, 62.619998931884766, 63.023998260498047, 63.428001403808594, 63.832000732421875, 64.236000061035156, 64.639999389648438, 65.043998718261719, 65.447998046875, 65.851997375488281, 66.255996704101563, 66.660003662109375, 67.064002990722656, 67.468002319335938, 67.872001647949219, 68.2760009765625, 68.680000305175781, 69.083999633789063, 69.487998962402344, 69.891998291015625, 70.295997619628906, 70.699996948242188, 71.103996276855469, 71.508003234863281, 71.912002563476563, 72.316001892089844, 72.720001220703125, 73.124000549316406, 73.527999877929688, 73.931999206542969, 74.33599853515625, 74.739997863769531, 75.143997192382813, 75.547996520996094, 75.952003479003906, 76.356002807617188, 76.760002136230469, 77.16400146484375, 77.568000793457031, 77.972000122070313, 78.375999450683594, 78.779998779296875, 79.183998107910156, 79.587997436523438, 79.991996765136719, 80.396003723144531, 80.800003051757813, 81.204002380371094, 81.608001708984375, 82.012001037597656, 82.416000366210938, 82.819999694824219, 83.2239990234375, 83.627998352050781, 84.031997680664063, 84.435997009277344, 84.839996337890625, 85.244003295898438, 85.648002624511719, 86.052001953125, 86.456001281738281, 86.860000610351563, 87.263999938964844, 87.667999267578125, 88.071998596191406, 88.475997924804688, 88.879997253417969, 89.28399658203125, 89.688003540039063, 90.092002868652344, 90.496002197265625, 90.900001525878906, 91.304000854492188, 91.708000183105469, 92.11199951171875, 92.515998840332031, 92.919998168945313, 93.323997497558594, 93.727996826171875, 94.132003784179688, 94.536003112792969, 94.94000244140625, 95.344001770019531, 95.748001098632813, 96.152000427246094, 96.555999755859375, 96.959999084472656, 97.363998413085938, 97.767997741699219, 98.1719970703125, 98.575996398925781, 98.980003356933594, 99.384002685546875, 99.788002014160156, 100.19200134277344, 100.59600067138672, 101";
const char* inv_pd_efficiency_str = "0, 0, 34.419998168945313, 55.200000762939453, 65.589996337890625, 71.819999694824219, 75.970001220703125, 78.94000244140625, 81.169998168945313, 82.900001525878906, 84.279998779296875, 85.419998168945313, 86.360000610351563, 87.160003662109375, 87.839996337890625, 88.44000244140625, 88.949996948242188, 89.410003662109375, 89.819999694824219, 90.180000305175781, 90.510002136230469, 90.80999755859375, 91.080001831054688, 91.319999694824219, 91.550003051757813, 91.75, 91.949996948242188, 92.120002746582031, 92.290000915527344, 92.44000244140625, 92.580001831054688, 92.720001220703125, 92.839996337890625, 92.959999084472656, 93.069999694824219, 93.169998168945313, 93.269996643066406, 93.370002746582031, 93.449996948242188, 93.540000915527344, 93.620002746582031, 93.69000244140625, 93.760002136230469, 93.830001831054688, 93.900001525878906, 93.959999084472656, 94.019996643066406, 94.080001831054688, 94.129997253417969, 94.180000305175781, 94.230003356933594, 94.279998779296875, 94.330001831054688, 94.370002746582031, 94.419998168945313, 94.459999084472656, 94.5, 94.540000915527344, 94.569999694824219, 94.610000610351563, 94.639999389648438, 94.680000305175781, 94.709999084472656, 94.739997863769531, 94.769996643066406, 94.800003051757813, 94.830001831054688, 94.860000610351563, 94.889999389648438, 94.910003662109375, 94.94000244140625, 94.959999084472656, 94.980003356933594, 95.010002136230469, 95.029998779296875, 95.050003051757813, 95.069999694824219, 95.089996337890625, 95.110000610351563, 95.129997253417969, 95.150001525878906, 95.169998168945313, 95.19000244140625, 95.209999084472656, 95.230003356933594, 95.239997863769531, 95.260002136230469, 95.279998779296875, 95.290000915527344, 95.30999755859375, 95.319999694824219, 95.339996337890625, 95.349998474121094, 95.360000610351563, 95.379997253417969, 95.389999389648438, 95.400001525878906, 95.419998168945313, 95.430000305175781, 95.44000244140625, 95.449996948242188, 95.470001220703125, 95.480003356933594, 95.489997863769531, 95.5, 95.510002136230469, 95.519996643066406, 95.529998779296875, 95.540000915527344, 95.550003051757813, 95.55999755859375, 95.569999694824219, 95.580001831054688, 95.589996337890625, 95.599998474121094, 95.610000610351563, 95.620002746582031, 95.629997253417969, 95.639999389648438, 95.639999389648438, 95.650001525878906, 95.660003662109375, 95.669998168945313, 95.680000305175781, 95.680000305175781, 95.69000244140625, 95.699996948242188, 95.709999084472656, 95.709999084472656, 95.720001220703125, 95.730003356933594, 95.730003356933594, 95.739997863769531, 95.75, 95.75, 95.760002136230469, 95.769996643066406, 95.769996643066406, 95.779998779296875, 95.779998779296875, 95.790000915527344, 95.800003051757813, 95.800003051757813, 95.80999755859375, 95.80999755859375, 95.819999694824219, 95.819999694824219, 95.830001831054688, 95.830001831054688, 95.839996337890625, 95.839996337890625, 95.849998474121094, 95.849998474121094, 95.860000610351563, 95.860000610351563, 95.870002746582031, 95.870002746582031, 95.879997253417969, 95.879997253417969, 95.889999389648438, 95.889999389648438, 95.889999389648438, 95.900001525878906, 95.900001525878906, 95.910003662109375, 95.910003662109375, 95.910003662109375, 95.919998168945313, 95.919998168945313, 95.930000305175781, 95.930000305175781, 95.930000305175781, 95.94000244140625, 95.94000244140625, 95.94000244140625, 95.949996948242188, 95.949996948242188, 95.959999084472656, 95.959999084472656, 95.959999084472656, 95.970001220703125, 95.970001220703125, 95.970001220703125, 95.980003356933594, 95.980003356933594, 95.980003356933594, 95.980003356933594, 95.989997863769531, 95.989997863769531, 95.989997863769531, 96, 96, 96, 96.010002136230469, 96.010002136230469, 96.010002136230469, 96.010002136230469, 96.019996643066406, 96.019996643066406, 96.019996643066406, 96.019996643066406, 96.029998779296875, 96.029998779296875, 96.029998779296875, 96.029998779296875, 96.040000915527344, 96.040000915527344, 96.040000915527344, 96.040000915527344, 96.050003051757813, 96.050003051757813, 96.050003051757813, 96.050003051757813, 96.05999755859375, 96.05999755859375, 96.05999755859375, 96.05999755859375, 96.05999755859375, 96.069999694824219, 96.069999694824219, 96.069999694824219, 96.069999694824219, 96.069999694824219, 96.080001831054688, 96.080001831054688, 96.080001831054688, 96.080001831054688, 96.080001831054688, 96.089996337890625, 96.089996337890625, 96.089996337890625, 96.089996337890625, 96.089996337890625, 96.089996337890625, 96.099998474121094, 96.099998474121094, 96.099998474121094, 96.099998474121094, 96.099998474121094, 96.099998474121094, 96.110000610351563, 96.110000610351563, 96.110000610351563, 96.110000610351563, 96.110000610351563, 96.110000610351563, 96.120002746582031, 96.120002746582031, 96.120002746582031, 96.120002746582031, 96.120002746582031";

/*
* Test 1
* Default Test Info: using SAM GUI defaults
*/
TestInfo pvSAMv1DefaultInfo[] = {
/*	SSC Var Name							Data Type			Test Values				Length,Width */
    {"solar_resource_file",					STR,				solar_resource_path},
    {"transformer_no_load_loss",			NUM,				"0"},
    {"transformer_load_loss",				NUM,				"0"},
    {"en_snow_model",						NUM,				"0"},
    {"system_capacity",						NUM,				"4.6928696632385254"},
    {"use_wf_albedo",						NUM,				"0"},
    {"albedo",								ARR,				"0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2",	12},
    {"irrad_mode",							NUM,				"0"},
    {"sky_model",							NUM,				"2"},
    {"modules_per_string",					NUM,				"7"},
    {"strings_in_parallel",					NUM,				"2"},
    {"inverter_count",						NUM,				"1"},
    {"enable_mismatch_vmax_calc",			NUM,				"0"},
    {"subarray1_tilt",						NUM,				"20"},
    {"subarray1_tilt_eq_lat",				NUM,				"0"},
    {"subarray1_azimuth",					NUM,				"180"},
    {"subarray1_track_mode",				NUM,				"0"},
    {"subarray1_rotlim",					NUM,				"45"},
    {"subarray1_shade_mode",				NUM,				"0"},
    {"subarray1_gcr",						NUM,				"0.3"},
    {"subarray1_monthly_tilt",				ARR,				"40, 40, 40, 20, 20, 20, 20, 20, 20, 40, 40, 40",				12},
    {"subarray1_soiling",					ARR,				"5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5",							12},
    {"subarray1_mismatch_loss",				NUM,				"2"},
    {"subarray1_diodeconn_loss",			NUM,				"0.5"},
    {"subarray1_dcwiring_loss",				NUM,				"2"},
    {"subarray1_tracking_loss",				NUM,				"0"},
    {"subarray1_nameplate_loss",			NUM,				"0"},
    {"subarray2_mismatch_loss",				NUM,				"2"},
    {"subarray2_diodeconn_loss",			NUM,				"0.5"},
    {"subarray2_dcwiring_loss",				NUM,				"2"},
    {"subarray2_tracking_loss",				NUM,				"0"},
    {"subarray2_nameplate_loss",			NUM,				"0"},
    {"subarray3_mismatch_loss",				NUM,				"2"},
    {"subarray3_diodeconn_loss",			NUM,				"0.5"},
    {"subarray3_dcwiring_loss",				NUM,				"2"},
    {"subarray3_tracking_loss",				NUM,				"0"},
    {"subarray3_nameplate_loss",			NUM,				"0"},
    {"subarray4_mismatch_loss",				NUM,				"2"},
    {"subarray4_diodeconn_loss",			NUM,				"0.5"},
    {"subarray4_dcwiring_loss",				NUM,				"2"},
    {"subarray4_tracking_loss",				NUM,				"0"},
    {"subarray4_nameplate_loss",			NUM,				"0"},
    {"dcoptimizer_loss",					NUM,				"0"},
    {"acwiring_loss",						NUM,				"1"},
    {"transmission_loss",					NUM,				"0"},
    {"subarray1_mod_orient",				NUM,				"0"},
    {"subarray1_nmodx",						NUM,				"7"},
    {"subarray1_nmody",						NUM,				"2"},
    {"subarray1_backtrack",					NUM,				"0"},
    {"subarray2_enable",					NUM,				"0"},
    {"subarray2_nstrings",					NUM,				"0"},
    {"subarray2_tilt",						NUM,				"20"},
    {"subarray2_tilt_eq_lat",				NUM,				"0"},
    {"subarray2_azimuth",					NUM,				"180"},
    {"subarray2_track_mode",				NUM,				"0"},
    {"subarray2_rotlim",					NUM,				"45"},
    {"subarray2_shade_mode",				NUM,				"0"},
    {"subarray2_gcr",						NUM,				"0.3"},
    {"subarray2_monthly_tilt",				ARR,				"40, 40, 40, 20, 20, 20, 20, 20, 20, 40, 40, 40",				12},
    {"subarray2_soiling",					ARR,				"5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5",							12},
    {"subarray2_mod_orient",				NUM,				"0"},
    {"subarray2_nmodx",						NUM,				"9"},
    {"subarray2_nmody",						NUM,				"2"},
    {"subarray2_backtrack",					NUM,				"0"},
    {"subarray3_enable",					NUM,				"0"},
    {"subarray3_nstrings",					NUM,				"0"},
    {"subarray3_tilt",						NUM,				"20"},
    {"subarray3_tilt_eq_lat",				NUM,				"0"},
    {"subarray3_azimuth",					NUM,				"180"},
    {"subarray3_track_mode",				NUM,				"0"},
    {"subarray3_rotlim",					NUM,				"45"},
    {"subarray3_shade_mode",				NUM,				"0"},
    {"subarray3_gcr",						NUM,				"0.36"},
    {"subarray3_monthly_tilt",				ARR,				"40, 40, 40, 20, 20, 20, 20, 20, 20, 40, 40, 40",				12},
    {"subarray3_soiling",					ARR,				"5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5",							12},
    {"subarray3_mod_orient",				NUM,				"0"},
    {"subarray3_nmodx",						NUM,				"9"},
    {"subarray3_nmody",						NUM,				"2"},
    {"subarray3_backtrack",					NUM,				"0"},
    {"subarray4_enable",					NUM,				"0"},
    {"subarray4_nstrings",					NUM,				"0"},
    {"subarray4_tilt",						NUM,				"20"},
    {"subarray4_tilt_eq_lat",				NUM,				"0"},
    {"subarray4_azimuth",					NUM,				"180"},
    {"subarray4_track_mode",				NUM,				"0"},
    {"subarray4_rotlim",					NUM,				"45"},
    {"subarray4_shade_mode",				NUM,				"0"},
    {"subarray4_gcr",						NUM,				"0.3"},
    { "subarray4_monthly_tilt",				ARR,				"40, 40, 40, 20, 20, 20, 20, 20, 20, 40, 40, 40",				12 },
    { "subarray4_soiling",					ARR,				"5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5",							12 },
    {"subarray4_mod_orient",				NUM,				"0"},
    {"subarray4_nmodx",						NUM,				"9"},
    {"subarray4_nmody",						NUM,				"2"},
    {"subarray4_backtrack",					NUM,				"0"},
    {"module_model",						NUM,				"1"},
    {"module_aspect_ratio",					NUM,				"1.7"},
    {"spe_area",							NUM,				"0.74074000120162964"},
    {"spe_rad0",							NUM,				"200"},
    {"spe_rad1",							NUM,				"400"},
    {"spe_rad2",							NUM,				"600"},
    {"spe_rad3",							NUM,				"800"},
    {"spe_rad4",							NUM,				"1000"},
    {"spe_eff0",							NUM,				"13.5"},
    {"spe_eff1",							NUM,				"13.5"},
    {"spe_eff2",							NUM,				"13.5"},
    {"spe_eff3",							NUM,				"13.5"},
    {"spe_eff4",							NUM,				"13.5"},
    {"spe_reference",						NUM,				"4"},
    {"spe_module_structure",				NUM,				"0"},
    {"spe_a",								NUM,				"-3.56"},
    {"spe_b",								NUM,				"-0.075"},
    {"spe_dT",								NUM,				"3"},
    {"spe_temp_coeff",						NUM,				"-0.5"},
    {"spe_fd",								NUM,				"1"},
    {"spe_vmp",								NUM,				"30"},
    {"spe_voc",								NUM,				"36"},
    {"cec_area",							NUM,				"1.631"},
    {"cec_a_ref",							NUM,				"2.4201"},
    {"cec_adjust",							NUM,				"5.01"},
    {"cec_alpha_sc",						NUM,				"0.002492"},
    {"cec_beta_oc",							NUM,				"-0.16975"},
    {"cec_gamma_r",							NUM,				"-0.31"},
    {"cec_i_l_ref",							NUM,				"6.237"},
    {"cec_i_mp_ref",						NUM,				"5.85"},
    {"cec_i_o_ref",							NUM,				"3.98e-12"},
    {"cec_i_sc_ref",						NUM,				"6.230"},
    {"cec_n_s",								NUM,				"96"},
    {"cec_r_s",								NUM,				"0.499"},
    {"cec_r_sh_ref",						NUM,				"457.12"},
    {"cec_t_noct",							NUM,				"46.4"},
    {"cec_v_mp_ref",						NUM,				"57.3"},
    {"cec_v_oc_ref",						NUM,				"67.9"},
    {"cec_temp_corr_mode",					NUM,				"0"},
    {"cec_standoff",						NUM,				"6"},
    {"cec_height",							NUM,				"0"},
    {"cec_mounting_config",					NUM,				"0"},
    {"cec_heat_transfer",					NUM,				"0"},
    {"cec_mounting_orientation",			NUM,				"0"},
    {"cec_gap_spacing",						NUM,				"0.05"},
    {"cec_module_width",					NUM,				"1"},
    {"cec_module_length",					NUM,				"1.631"},
    {"cec_array_rows",						NUM,				"1"},
    {"cec_array_cols",						NUM,				"10"},
    {"cec_backside_temp",					NUM,				"20"},
    {"6par_celltech",						NUM,				"1"},
    {"6par_vmp",							NUM,				"30"},
    {"6par_imp",							NUM,				"6"},
    {"6par_voc",							NUM,				"37"},
    {"6par_isc",							NUM,				"7"},
    {"6par_bvoc",							NUM,				"-0.11"},
    {"6par_aisc",							NUM,				"0.004"},
    {"6par_gpmp",							NUM,				"-0.41"},
    {"6par_nser",							NUM,				"60"},
    {"6par_area",							NUM,				"1.3"},
    {"6par_tnoct",							NUM,				"46"},
    {"6par_standoff",						NUM,				"6"},
    {"6par_mounting",						NUM,				"0"},
    {"snl_module_structure",				NUM,				"0"},
    {"snl_a",								NUM,				"-3.62"},
    {"snl_b",								NUM,				"-0.075"},
    {"snl_dtc",								NUM,				"3"},
    {"snl_ref_a",							NUM,				"-3.62"},
    {"snl_ref_b",							NUM,				"-0.075"},
    {"snl_ref_dT",							NUM,				"3"},
    {"snl_fd",								NUM,				"1"},
    {"snl_a0",								NUM,				"0.94045"},
    {"snl_a1",								NUM,				"0.052641"},
    {"snl_a2",								NUM,				"-0.0093897"},
    {"snl_a3",								NUM,				"0.00072622997686266899"},
    {"snl_a4",								NUM,				"-1.9938e-05"},
    {"snl_aimp",							NUM,				"-0.00038"},
    {"snl_aisc",							NUM,				"0.00061"},
    {"snl_area",							NUM,				"1.244"},
    {"snl_b0",								NUM,				"1"},
    {"snl_b1",								NUM,				"-0.002438"},
    {"snl_b2",								NUM,				"0.0003103"},
    {"snl_b3",								NUM,				"-1.246e-05"},
    {"snl_b4",								NUM,				"2.11e-07"},
    {"snl_b5",								NUM,				"-1.36e-09"},
    {"snl_bvmpo",							NUM,				"-0.139"},
    {"snl_bvoco",							NUM,				"-0.136"},
    {"snl_c0",								NUM,				"1.0039"},
    {"snl_c1",								NUM,				"-0.0039"},
    {"snl_c2",								NUM,				"0.291066"},
    {"snl_c3",								NUM,				"-4.7354598045349121"},
    {"snl_c4",								NUM,				"0.9942"},
    {"snl_c5",								NUM,				"0.0058"},
    {"snl_c6",								NUM,				"1.0723"},
    {"snl_c7",								NUM,				"-0.0723"},
    {"snl_impo",							NUM,				"5.25"},
    {"snl_isco",							NUM,				"5.75"},
    {"snl_ixo",								NUM,				"5.65"},
    {"snl_ixxo",							NUM,				"3.85"},
    {"snl_mbvmp",							NUM,				"0"},
    {"snl_mbvoc",							NUM,				"0"},
    {"snl_n",								NUM,				"1.221"},
    {"snl_series_cells",					NUM,				"72"},
    {"snl_vmpo",							NUM,				"40"},
    {"snl_voco",							NUM,				"47.7"},
    {"sd11par_nser",						NUM,				"116"},
    {"sd11par_area",						NUM,				"0.72"},
    {"sd11par_AMa0",						NUM,				"0.9417"},
    {"sd11par_AMa1",						NUM,				"0.06516"},
    {"sd11par_AMa2",						NUM,				"-0.02022"},
    {"sd11par_AMa3",						NUM,				"0.00219"},
    {"sd11par_AMa4",						NUM,				"-9.1e-05"},
    {"sd11par_glass",						NUM,				"0"},
    {"sd11par_tnoct",						NUM,				"44.9"},
    {"sd11par_standoff",					NUM,				"6"},
    {"sd11par_mounting",					NUM,				"0"},
    {"sd11par_Vmp0",						NUM,				"64.6"},
    {"sd11par_Imp0",						NUM,				"1.05"},
    {"sd11par_Voc0",						NUM,				"87"},
    {"sd11par_Isc0",						NUM,				"1.18"},
    {"sd11par_alphaIsc",					NUM,				"0.000472"},
    {"sd11par_n",							NUM,				"1.45071"},
    {"sd11par_Il",							NUM,				"1.18951"},
    {"sd11par_Io",							NUM,				"2.08522e-09"},
    {"sd11par_Egref",						NUM,				"0.737668"},
    {"sd11par_d1",							NUM,				"13.5504"},
    {"sd11par_d2",							NUM,				"-0.0769735"},
    {"sd11par_d3",							NUM,				"0.237327"},
    {"sd11par_c1",							NUM,				"1930.15"},
    {"sd11par_c2",							NUM,				"474.64"},
    {"sd11par_c3",							NUM,				"1.48746"},
    {"inverter_model",						NUM,				"0"},
    {"mppt_low_inverter",					NUM,				"250"},
    {"mppt_hi_inverter",					NUM,				"480"},
    {"inv_snl_c0",							NUM,				"-3.18e-06"},
    {"inv_snl_c1",							NUM,				"-5.12e-05"},
    {"inv_snl_c2",							NUM,				"0.000984"},
    {"inv_snl_c3",							NUM,				"-0.00151"},
    {"inv_snl_paco",						NUM,				"3800"},
    {"inv_snl_pdco",						NUM,				"3928.1142578125"},
    {"inv_snl_pnt",							NUM,				"0.99"},
    {"inv_snl_pso",							NUM,				"19.451622"},
    {"inv_snl_vdco",						NUM,				"398.496673584"},
    {"inv_snl_vdcmax",						NUM,				"600"},
    {"inv_cec_cg_c0",						NUM,				"-3.1752e-06"},
    {"inv_cec_cg_c1",						NUM,				"-5.1231381803518161e-05"},
    {"inv_cec_cg_c2",						NUM,				"0.0009835963137447834"},
    {"inv_cec_cg_c3",						NUM,				"-0.0015077980933710933"},
    {"inv_cec_cg_paco",						NUM,				"3800"},
    {"inv_cec_cg_pdco",						NUM,				"3928.11376953125"},
    {"inv_cec_cg_pnt",						NUM,				"0.99"},
    {"inv_cec_cg_psco",						NUM,				"19.448383331298828"},
    {"inv_cec_cg_vdco",						NUM,				"398.49661254882813"},
    {"inv_cec_cg_vdcmax",					NUM,				"600"},
    {"inv_ds_paco",							NUM,				"4000"},
    {"inv_ds_eff",							NUM,				"96"},
    {"inv_ds_pnt",							NUM,				"1"},
    {"inv_ds_pso",							NUM,				"0"},
    {"inv_ds_vdco",							NUM,				"310"},
    {"inv_ds_vdcmax",						NUM,				"600"},
    {"inv_pd_paco",							NUM,				"4000"},
    {"inv_pd_pdco",							NUM,				"4210.5263671875"},
    {"inv_pd_partload",						ARR,				inv_pd_partload_str,												251 },
    {"inv_pd_efficiency",					ARR,				inv_pd_efficiency_str,												251 },
    {"inv_pd_pnt",							NUM,				"0"},
	{"inv_pd_vdco",							NUM,				"310"},
	{"inv_pd_vdcmax",						NUM,				"600"},
	{"adjust:constant",						NUM,				"0"},
	{"dc_adjust:constant",					NUM,				"0"},
	{"inv_snl_eff_cec",						NUM,				"96.636932373046875"},
	{"inv_pd_eff",							NUM,				"95"},
	{"inv_cec_cg_eff_cec",					NUM,				"96.636306762695313"}
};

TestResult pvSAMv1DefaultResult[] = {
/*	SSC Var Name							Test Type			Test Result				Error Bound Ratio */
	{"annual_energy",						NR,					8714,					0.1},
	{"capacity_factor",						NR,					21.2,					0.1},
	{"kwh_per_kw",							NR,					1857,					0.1},
	{"performance_ratio",					NR,					0.79,					0.1},
};

testDeclaration pvSAMv1DefaultTest(pvSAMv1Testing, "default", &pvSAMv1DefaultInfo[0], 266, &pvSAMv1DefaultResult[0], 4);


/*
* Test 2
* 15-minute weather file
*/
TestInfo pvSAMv1fifteenMinInfo[] = {
	{"solar_resource_file",					STR,				solar_resource_path_15_min }
};

TestResult pvSAMv1fifteenMinResult[] = {
/*	SSC Var Name							Test Type			Test Result				Error Bound Ratio */
	{"annual_energy",						NR,					7587,					0.1},
	{"capacity_factor",						NR,					18.5,					0.1},
	{"kwh_per_kw",							NR,					1617,					0.1},
	{"performance_ratio",					NR,					0.80,					0.1},
};

testDeclaration pvSAMv1fifteenMinTest(pvSAMv1Testing, "default", &pvSAMv1fifteenMinInfo[0], 1, &pvSAMv1fifteenMinResult[0], 4);


/*
* Test 3-12
* Sky diffuse model combined with irradiance models
*/


/// Test PVSAMv1 with default no-financial model and combinations of Sky Diffuse Model and Weather File Irradiance
	////std::vector<double> annual_energy_expected = { 8513, 8522, 8525, 8635, 8645, 8647, 8714, 8723, 8726, 7623, 7377};

	//// Sky diffuse models: isotropic, hdkr, perez
	//for (int sky_diffuse_model = 0; sky_diffuse_model < 3; sky_diffuse_model++)
	//// Weather file irradiance: DNI & DHI, DNI & GHI, GHI & DHI
	//for (int irrad_mode = 0; irrad_mode < 3; irrad_mode++)

	//// Perez with POA reference cell
	//pairs["sky_model"] = 2;
	//pairs["irrad_mode"] = 3;

	//// Perez with POA pyranometer
	//pairs["irrad_mode"] = 4;
	
/// Test PVSAMv1 with default no-financial model and combinations of module and inverter models
	////std::vector<double> annual_energy_expected = { 2518, 2548, 2476, 2518, 8714, 8694, 8661, 8714, 54, 57, 60, 54, 5405, 5400, 5347, 5404, 1767, 1807, 1736, 1767};

	//// Module models: Simple Efficiency, CEC Performance Database, CEC User Entered, Sandia, IEC61853
	//for (int module_model = 0; module_model < 5; module_model++)
	//// Inverter models: CEC, Datasheet, Partload Curve, Coefficient Generator
	//for (int inverter_model = 0; inverter_model < 4; inverter_model++)


/// Test PVSAMv1 with default no-financial model and sytem design page changes
TEST_F(CMPvsamv1PowerIntegration, NoFinancialModelSystemDesign)
{
	pvsamv_nofinancial_default(data);

	// Specify modules and inverters with tracking options
	// Tracking options: Fixed, 1-axis, 2-axis, Azimuth Axis, Seasonal Tilt
	std::map<std::string, double> pairs;
	pairs["modules_per_string"] = 6;
	pairs["strings_in_parallel"] = 49;
	pairs["inverter_count"] = 22;
	pairs["subarray1_track_mode"] = 0;

	std::vector<double> annual_energy_expected = { 183243, 242540, 258572, 216242, 192975 };

	for (int tracking_option = 0; tracking_option != 5; tracking_option++)
	{
		// update tracking option
		pairs["subarray1_track_mode"] = (double)tracking_option;
		int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", NUM, "pairs);
		EXPECT_FALSE(pvsam_errors);
		if (!pvsam_errors)
		{
			ssc_number_t annual_energy;
			ssc_data_get_number(data, "annual_energy", NUM, "&annual_energy);
			EXPECT_NEAR(annual_energy, annual_energy_expected[tracking_option], m_error_tolerance_hi) << "Annual energy.";
		}
	}

	// Test fixed-tilt with backtracking
	pairs["subarray1_track_mode"] = 1;
	pairs["subarray1_backtrack"] = 1;

	int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", NUM, "pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", NUM, "&annual_energy);
		EXPECT_NEAR(annual_energy, 237340, m_error_tolerance_hi) << "Annual energy.";
	}

	// Test multiple sub-arrays with different tracking, tilt, azimuth, gcr, tracker rotation limit
	
	pairs["subarray2_enable"] = 1;
	pairs["subarray2_nstrings"] = 15;
	pairs["subarray3_enable"] = 1;
	pairs["subarray3_nstrings"] = 10;
	pairs["subarray4_enable"] = 1;
	pairs["subarray4_nstrings"] = 10;

	annual_energy_expected.clear();
	std::vector<double> subarray1_azimuth = {0, 90, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180};
	std::vector<double> subarray2_azimuth = { 180, 180, 180, 0, 90, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180 };
	std::vector<double> subarray3_azimuth = { 180, 180, 180, 180, 180, 180, 0, 90, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180 };
	std::vector<double> subarray4_azimuth = { 180, 180, 180, 180, 180, 180, 180, 180, 180, 0, 90, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180 };
	std::vector<double> enable_mismatch = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	std::vector<double> subarray1_gcr = { 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.1, 0.5, 0.9, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3 };
	std::vector<double> subarray2_gcr = { 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.1, 0.5, 0.9, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3 };
	std::vector<double> subarray3_gcr = { 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.1, 0.5, 0.9, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3 };
	std::vector<double> subarray4_gcr = { 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.1, 0.5, 0.9, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3 };
	std::vector<double> subarray1_tilt = { 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 0, 45, 90, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20 };
	std::vector<double> subarray2_tilt = { 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 0, 45, 90, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20 };
	std::vector<double> subarray3_tilt = { 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 0, 45, 90, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20 };
	std::vector<double> subarray4_tilt = { 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 0, 45, 90, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20 };
	std::vector<double> subarray1_rotlim = { 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45 };
	std::vector<double> subarray2_rotlim = { 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45 };
	std::vector<double> subarray3_rotlim = { 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45 };
	std::vector<double> subarray4_rotlim = { 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45 };
	std::vector<double> subarray1_track_mode = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	std::vector<double> subarray2_track_mode = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	std::vector<double> subarray3_track_mode = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 0, 0, 0, 0, 0 };
	std::vector<double> subarray4_track_mode = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4 };
	annual_energy_expected = { 167392, 176331, 183243, 166251, 175833, 183243, 171952, 178321, 183243, 171952, 178321, 183243, 183243, 183227, 183243, 183243, 183243, 183243, 183243, 183243, 183243, 183243, 183243, 183243, 183243, 183243, 177310, 182927, 162456, 176883, 182902, 160961, 179014, 183024, 168431, 179014, 183024, 168431, 183243, 183243, 183243, 183243, 183243, 198796, 205187, 192695, 186088, 183243, 201501, 206750, 193370, 186290, 183243, 195419, 198925, 189995, 185277, 183243, 195419, 198925, 189995, 185277 };

	for (int i = 0; i != annual_energy_expected.size(); i++)
	{
		pairs["enable_mismatch_vmax_calc"] = enable_mismatch[i];
		pairs["subarray1_azimuth"] = subarray1_azimuth[i];
		pairs["subarray2_azimuth"] = subarray2_azimuth[i]; 
		pairs["subarray3_azimuth"] = subarray3_azimuth[i]; 
		pairs["subarray4_azimuth"] = subarray4_azimuth[i];
		pairs["subarray1_gcr"] = subarray1_gcr[i]; 
		pairs["subarray2_gcr"] = subarray2_gcr[i]; 
		pairs["subarray3_gcr"] = subarray3_gcr[i]; 
		pairs["subarray4_gcr"] = subarray4_gcr[i];
		pairs["subarray1_tilt"] = subarray1_tilt[i]; 
		pairs["subarray2_tilt"] = subarray2_tilt[i]; 
		pairs["subarray3_tilt"] = subarray3_tilt[i]; 
		pairs["subarray4_tilt"] = subarray4_tilt[i];
		pairs["subarray1_rotlim"] = subarray1_rotlim[i]; 
		pairs["subarray2_rotlim"] = subarray2_rotlim[i]; 
		pairs["subarray3_rotlim"] = subarray3_rotlim[i]; 
		pairs["subarray4_rotlim"] = subarray4_rotlim[i];
		pairs["subarray1_track_mode"] = subarray1_track_mode[i]; 
		pairs["subarray2_track_mode"] = subarray2_track_mode[i]; 
		pairs["subarray3_track_mode"] = subarray3_track_mode[i]; 
		pairs["subarray4_track_mode"] = subarray4_track_mode[i];

		int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", NUM, "pairs);
		EXPECT_FALSE(pvsam_errors);
		if (!pvsam_errors)
		{
			ssc_number_t annual_energy;
			ssc_data_get_number(data, "annual_energy", NUM, "&annual_energy);
			EXPECT_NEAR(annual_energy, annual_energy_expected[i], m_error_tolerance_hi) << "Index: " << i;
		}
	}
}

/// Test PVSAMv1 with default no-financial model and different shading options
TEST_F(CMPvsamv1PowerIntegration, NoFinancialModelShading)
{
	// 0: No Shading, 1: 3D Shading, 2: 3D shading with self shading (non-linear), 3: Snow
	std::vector<double> annual_energy_expected = { 12911, 10607, 10579, 10377};
	std::map<std::string, double> pairs;

	// 2 subarrays, one pointing east, one west
	pairs["modules_per_string"] = 6;
	pairs["strings_in_parallel"] = 4;
	pairs["inverter_count"] = 2;
	pairs["subarray1_azimuth"] = 90;
	pairs["subarray2_enable"] = 1;
	pairs["subarray2_nstrings"] = 2;
	pairs["subarray2_azimuth"] = 270;

	// 0. No Shading
	int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", NUM, "pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[0], m_error_tolerance_hi);
	}

	// 1. Add 3D Shading
	set_matrix(data, "subarray1_shading:timestep", NUM, "subarray1_shading, 8760, 2);
	set_matrix(data, "subarray2_shading:timestep", NUM, "subarray2_shading, 8760, 2);
	pairs["subarray1_shading:diff"] =  10.010875701904297;
	pairs["subarray2_shading:diff"] = 10.278481483459473;

	pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", NUM, "pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[1], m_error_tolerance_hi);
	}
	
	// 2. Add Self Shading to 3D shading
	pairs["subarray1_shade_mode"] = 1;
	pairs["subarray1_mod_orient"] = 1;
	pairs["subarray1_nmody"] = 1;
	pairs["subarray1_nmodx"] = 6;
	pairs["subarray2_shade_mode"] = 1;
	pairs["subarray2_mod_orient"] = 1;
	pairs["subarray2_nmody"] = 1;
	pairs["subarray2_nmodx"] = 6;

	pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", NUM, "pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[2], m_error_tolerance_hi);
	}

	// 3. Add Snow losses to all shading
	pairs["en_snow_model"] = 1;
	pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", NUM, "pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[3], m_error_tolerance_hi);
	}

}

/// Test PVSAMv1 with default no-financial model and different loss options
TEST_F(CMPvsamv1PowerIntegration, NoFinancialModelLosses)
{
	// 0: Default Losses, 1: Modify Point Losses, 2: Modify Availability
	std::vector<double> annual_energy_expected = { 8714, 7874, 7607 };
	std::map<std::string, double> pairs;

	// 0: Default losses
	int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", NUM, "pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[0], m_error_tolerance_hi);
	}

	// 1: Modify Point Losses
	ssc_number_t p_subarray1_soiling[12] = { 5, 5, 5, 5, 6, 6, 6, 6, 5, 5, 5, 5 };
	ssc_data_set_array(data, "subarray1_soiling", NUM, "p_subarray1_soiling, 12);

	pairs["subarray1_mismatch_loss"] = 3;
	pairs["subarray1_diodeconn_loss"] = 0.6;
	pairs["subarray1_dcwiring_loss"] = 2;
	pairs["subarray1_tracking_loss"] = 1;
	pairs["subarray1_nameplate_loss"] = 1;
	pairs["dcoptimizer_loss"] = 1;
	pairs["acwiring_loss"] = 2;
	pairs["transformer_no_load_loss"] = 1;
	pairs["transformer_load_loss"] = 1;

	pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", NUM, "pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[1], m_error_tolerance_hi);
	}

	// 2. Modify availability losses
	ssc_number_t p_adjust[3] = { 5268, 5436, 50 };
	ssc_data_set_matrix(data, "adjust:periods", NUM, "p_adjust, 1, 3);
	ssc_number_t p_dc_adjust[3] = { 5088, 5256, 100 };
	ssc_data_set_matrix(data, "dc_adjust:periods", NUM, "p_dc_adjust, 1, 3);

	pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", NUM, "pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[2], m_error_tolerance_hi);
	}
}