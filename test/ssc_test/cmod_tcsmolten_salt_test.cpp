#include <gtest/gtest.h>

#include "cmod_tcsmolten_salt_test.h"
#include "../tcs_test/tcsmolten_salt_cases.h"
#include "../input_cases/weather_inputs.h"

/// Test tcsmolten_salt with all defaults and the single owner financial model
TEST_F(CMTcsMoltenSalt, Rankine_Default_SingleOwner_cmod_tcsmolten_salt) {

    ssc_data_t data = ssc_data_create();
    int test_errors = tcsmolten_salt_daggett_default(data);

    EXPECT_FALSE(test_errors);
    if (!test_errors) {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, 571408807.373179, 571408807.373179 * m_error_tolerance_hi)
                            << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t land_area_base;
        ssc_data_get_number(data, "land_area_base", &land_area_base);
        EXPECT_NEAR(land_area_base, 1847.040000, 1847.040000 * m_error_tolerance_hi)
                            << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t capacity_factor;
        ssc_data_get_number(data, "capacity_factor", &capacity_factor);
        EXPECT_NEAR(capacity_factor, 63.023494, 63.023494 * m_error_tolerance_hi)
                            << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t annual_W_cycle_gross;
        ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
        EXPECT_NEAR(annual_W_cycle_gross, 642428580.492706, 642428580.492706 * m_error_tolerance_hi)
                            << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t kwh_per_kw;
        ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
        EXPECT_NEAR(kwh_per_kw, 5520.858042, 5520.858042 * m_error_tolerance_hi)
                            << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t conversion_factor;
        ssc_data_get_number(data, "conversion_factor", &conversion_factor);
        EXPECT_NEAR(conversion_factor, 88.945110, 88.945110 * m_error_tolerance_hi)
                            << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t N_hel;
        ssc_data_get_number(data, "N_hel", &N_hel);
        EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi)
                            << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t rec_height;
        ssc_data_get_number(data, "rec_height", &rec_height);
        EXPECT_NEAR(rec_height, 21.602900, 21.602900 * m_error_tolerance_hi)
                            << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t A_sf;
        ssc_data_get_number(data, "A_sf", &A_sf);
        EXPECT_NEAR(A_sf, 1269054.492000, 1269054.492000 * m_error_tolerance_hi)
                            << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t D_rec;
        ssc_data_get_number(data, "D_rec", &D_rec);
        EXPECT_NEAR(D_rec, 17.650000, 17.650000 * m_error_tolerance_hi)
                            << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t annual_total_water_use;
        ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
        EXPECT_NEAR(annual_total_water_use, 98221.126175, 98221.126175 * m_error_tolerance_hi)
                            << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t csp_pt_cost_total_land_area;
        ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
        EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.040000, 1892.040000 * m_error_tolerance_hi)
                            << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t h_tower;
        ssc_data_get_number(data, "h_tower", &h_tower);
        EXPECT_NEAR(h_tower, 193.458000, 193.458000 * m_error_tolerance_hi)
                            << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        //ssc_number_t VARIABLE;
        //ssc_data_get_number(data, "VARIABLE", &VARIABLE);
        //EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
    }
}

/// Test tcsmolten_salt with alternative turbine inlet pressure control: Sliding pressure
/// Rest default configurations with respect to the single owner financial model
TEST_F(CMTcsMoltenSalt, Rankine_Sliding_P_SingleOwner_cmod_tcsmolten_salt) {

    ssc_data_t data = ssc_data_create();
    int test_errors = tcsmolten_salt_daggett_sliding_pressure(data);

    EXPECT_FALSE(test_errors);
    if (!test_errors) {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, 576302445.677569, 576302445.677569 * m_error_tolerance_hi)
                            << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t land_area_base;
        ssc_data_get_number(data, "land_area_base", &land_area_base);
        EXPECT_NEAR(land_area_base, 1847.040000, 1847.040000 * m_error_tolerance_hi)
                            << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t capacity_factor;
        ssc_data_get_number(data, "capacity_factor", &capacity_factor);
        EXPECT_NEAR(capacity_factor, 63.563237, 63.563237 * m_error_tolerance_hi)
                            << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t annual_W_cycle_gross;
        ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
        EXPECT_NEAR(annual_W_cycle_gross, 647174668.052062, 647174668.052062 * m_error_tolerance_hi)
                            << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t kwh_per_kw;
        ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
        EXPECT_NEAR(kwh_per_kw, 5568.139572, 5568.139572 * m_error_tolerance_hi)
                            << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t conversion_factor;
        ssc_data_get_number(data, "conversion_factor", &conversion_factor);
        EXPECT_NEAR(conversion_factor, 89.048981, 89.048981 * m_error_tolerance_hi)
                            << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t N_hel;
        ssc_data_get_number(data, "N_hel", &N_hel);
        EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi)
                            << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t rec_height;
        ssc_data_get_number(data, "rec_height", &rec_height);
        EXPECT_NEAR(rec_height, 21.602900, 21.602900 * m_error_tolerance_hi)
                            << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t A_sf;
        ssc_data_get_number(data, "A_sf", &A_sf);
        EXPECT_NEAR(A_sf, 1269054.492000, 1269054.492000 * m_error_tolerance_hi)
                            << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t D_rec;
        ssc_data_get_number(data, "D_rec", &D_rec);
        EXPECT_NEAR(D_rec, 17.650000, 17.650000 * m_error_tolerance_hi)
                            << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t annual_total_water_use;
        ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
        EXPECT_NEAR(annual_total_water_use, 98238.031245, 98238.031245 * m_error_tolerance_hi)
                            << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t csp_pt_cost_total_land_area;
        ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
        EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.040000, 1892.040000 * m_error_tolerance_hi)
                            << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t h_tower;
        ssc_data_get_number(data, "h_tower", &h_tower);
        EXPECT_NEAR(h_tower, 193.458000, 193.458000 * m_error_tolerance_hi)
                            << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        //ssc_number_t VARIABLE;
        //ssc_data_get_number(data, "VARIABLE", &VARIABLE);
        //EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
    }
}

/// Test tcsmolten_salt with alternative condenser type: Evaporative
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, Rankine_Evap_Condenser_SingleOwner_cmod_tcsmolten_salt) {
//
//    ssc_data_t data = ssc_data_create();
//    int test_errors = tcsmolten_salt_daggett_evap_condenser(data);
//
//    EXPECT_FALSE(test_errors);
//    if (!test_errors)
//    {
//        ssc_number_t annual_energy;
//        ssc_data_get_number(data, "annual_energy", &annual_energy);
//        EXPECT_NEAR(annual_energy, 571408807.373179, 571408807.373179 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t land_area_base;
//        ssc_data_get_number(data, "land_area_base", &land_area_base);
//        EXPECT_NEAR(land_area_base, 1847.040000, 1847.040000 * m_error_tolerance_hi) << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t capacity_factor;
//        ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//        EXPECT_NEAR(capacity_factor, 63.023494, 63.023494 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_W_cycle_gross;
//        ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//        EXPECT_NEAR(annual_W_cycle_gross, 642428580.492706, 642428580.492706 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t kwh_per_kw;
//        ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//        EXPECT_NEAR(kwh_per_kw, 5520.858042, 5520.858042 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t conversion_factor;
//        ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//        EXPECT_NEAR(conversion_factor, 88.945110, 88.945110 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t N_hel;
//        ssc_data_get_number(data, "N_hel", &N_hel);
//        EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t rec_height;
//        ssc_data_get_number(data, "rec_height", &rec_height);
//        EXPECT_NEAR(rec_height, 21.602900, 21.602900 * m_error_tolerance_hi) << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t A_sf;
//        ssc_data_get_number(data, "A_sf", &A_sf);
//        EXPECT_NEAR(A_sf, 1269054.492000, 1269054.492000 * m_error_tolerance_hi) << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t D_rec;
//        ssc_data_get_number(data, "D_rec", &D_rec);
//        EXPECT_NEAR(D_rec, 17.650000, 17.650000 * m_error_tolerance_hi) << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_total_water_use;
//        ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//        EXPECT_NEAR(annual_total_water_use, 98221.126175, 98221.126175 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t csp_pt_cost_total_land_area;
//        ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//        EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.040000, 1892.040000 * m_error_tolerance_hi) << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t h_tower;
//        ssc_data_get_number(data, "h_tower", &h_tower);
//        EXPECT_NEAR(h_tower, 193.458000, 193.458000 * m_error_tolerance_hi) << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        //ssc_number_t VARIABLE;
//        //ssc_data_get_number(data, "VARIABLE", &VARIABLE);
//        //EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//    }
//}

/// Test tcsmolten_salt with alternative condenser type: Hybrid
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, Rankine_Hybrid_Condenser_SingleOwner_cmod_tcsmolten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsmolten_salt_daggett_hybrid_condenser(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 571408807.373179, 571408807.373179 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t land_area_base;
//		ssc_data_get_number(data, "land_area_base", &land_area_base);
//		EXPECT_NEAR(land_area_base, 1847.040000, 1847.040000 * m_error_tolerance_hi) << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 63.023494, 63.023494 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 642428580.492706, 642428580.492706 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 5520.858042, 5520.858042 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 88.945110, 88.945110 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t N_hel;
//		ssc_data_get_number(data, "N_hel", &N_hel);
//		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t rec_height;
//		ssc_data_get_number(data, "rec_height", &rec_height);
//		EXPECT_NEAR(rec_height, 21.602900, 21.602900 * m_error_tolerance_hi) << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t A_sf;
//		ssc_data_get_number(data, "A_sf", &A_sf);
//		EXPECT_NEAR(A_sf, 1269054.492000, 1269054.492000 * m_error_tolerance_hi) << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t D_rec;
//		ssc_data_get_number(data, "D_rec", &D_rec);
//		EXPECT_NEAR(D_rec, 17.650000, 17.650000 * m_error_tolerance_hi) << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 98221.126175, 98221.126175 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t csp_pt_cost_total_land_area;
//		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//		EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.040000, 1892.040000 * m_error_tolerance_hi) << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t h_tower;
//		ssc_data_get_number(data, "h_tower", &h_tower);
//		EXPECT_NEAR(h_tower, 193.458000, 193.458000 * m_error_tolerance_hi) << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		//ssc_number_t VARIABLE;
//		//ssc_data_get_number(data, "VARIABLE", &VARIABLE);
//		//EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}

/// Test tcsmolten_salt with alternative condenser type: Radiative
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, Rankine_Radiative_Condenser_SingleOwner_cmod_tcsmolten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsmolten_salt_daggett_radiative_condenser(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 6.11007e8, 6.11007e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t land_area_base;
//		ssc_data_get_number(data, "land_area_base", &land_area_base);
//		EXPECT_NEAR(land_area_base, 1847.04, 1847.04 * m_error_tolerance_hi) << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 67.391, 67.391 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 6.68005e8, 6.68005e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 5903.45, 5903.45 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 91.4676, 91.4676 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t N_hel;
//		ssc_data_get_number(data, "N_hel", &N_hel);
//		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t rec_height;
//		ssc_data_get_number(data, "rec_height", &rec_height);
//		EXPECT_NEAR(rec_height, 21.6029, 21.6029 * m_error_tolerance_hi) << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t A_sf;
//		ssc_data_get_number(data, "A_sf", &A_sf);
//		EXPECT_NEAR(A_sf, 1.26905e6, 1.26905e6 * m_error_tolerance_hi) << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t D_rec;
//		ssc_data_get_number(data, "D_rec", &D_rec);
//		EXPECT_NEAR(D_rec, 17.65, 17.65 * m_error_tolerance_hi) << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 97830.1, 97830.1 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t csp_pt_cost_total_land_area;
//		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//		EXPECT_NEAR(csp_pt_cost_total_land_area, 2362.53, 2362.53 * m_error_tolerance_hi) << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t h_tower;
//		ssc_data_get_number(data, "h_tower", &h_tower);
//		EXPECT_NEAR(h_tower, 193.458, 193.458 * m_error_tolerance_hi) << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		//ssc_number_t VARIABLE;
//		//ssc_data_get_number(data, "VARIABLE", &VARIABLE);
//		//EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}

/// Test tcsmolten_salt with alternative flow pattern: Flow pattern 8
/// Rest default configurations with respect to the single owner financial model
TEST_F(CMTcsMoltenSalt, Rankine_Flow_Pattern_SingleOwner_cmod_tcsmolten_salt) {

    ssc_data_t data = ssc_data_create();
    int test_errors = tcsmolten_salt_daggett_flow_pattern(data);

    EXPECT_FALSE(test_errors);
    if (!test_errors) {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, 518055493.136035, 518055493.136035 * m_error_tolerance_hi)
                            << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t land_area_base;
        ssc_data_get_number(data, "land_area_base", &land_area_base);
        EXPECT_NEAR(land_area_base, 1847.040000, 1847.040000 * m_error_tolerance_hi)
                            << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t capacity_factor;
        ssc_data_get_number(data, "capacity_factor", &capacity_factor);
        EXPECT_NEAR(capacity_factor, 57.138894, 57.138894 * m_error_tolerance_hi)
                            << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t annual_W_cycle_gross;
        ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
        EXPECT_NEAR(annual_W_cycle_gross, 646287965.853696, 646287965.853696 * m_error_tolerance_hi)
                            << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t kwh_per_kw;
        ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
        EXPECT_NEAR(kwh_per_kw, 5005.367083, 5005.367083 * m_error_tolerance_hi)
                            << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t conversion_factor;
        ssc_data_get_number(data, "conversion_factor", &conversion_factor);
        EXPECT_NEAR(conversion_factor, 80.158617, 80.158617 * m_error_tolerance_hi)
                            << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t N_hel;
        ssc_data_get_number(data, "N_hel", &N_hel);
        EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi)
                            << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t rec_height;
        ssc_data_get_number(data, "rec_height", &rec_height);
        EXPECT_NEAR(rec_height, 21.602900, 21.602900 * m_error_tolerance_hi)
                            << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t A_sf;
        ssc_data_get_number(data, "A_sf", &A_sf);
        EXPECT_NEAR(A_sf, 1269054.492000, 1269054.492000 * m_error_tolerance_hi)
                            << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t D_rec;
        ssc_data_get_number(data, "D_rec", &D_rec);
        EXPECT_NEAR(D_rec, 17.650000, 17.650000 * m_error_tolerance_hi)
                            << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t annual_total_water_use;
        ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
        EXPECT_NEAR(annual_total_water_use, 98470.230665, 98470.230665 * m_error_tolerance_hi)
                            << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t csp_pt_cost_total_land_area;
        ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
        EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.040000, 1892.040000 * m_error_tolerance_hi)
                            << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t h_tower;
        ssc_data_get_number(data, "h_tower", &h_tower);
        EXPECT_NEAR(h_tower, 193.458000, 193.458000 * m_error_tolerance_hi)
                            << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        //ssc_number_t VARIABLE;
        //ssc_data_get_number(data, "VARIABLE", &VARIABLE);
        //EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
    }
}

/// Test tcsmolten_salt with alternative Location: Tucson, Arizona
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, Rankine_Location_SingleOwner_cmod_tcsmolten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsmolten_salt_Tucson_AZ(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 5.60538e8, 5.60538e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t land_area_base;
//		ssc_data_get_number(data, "land_area_base", &land_area_base);
//		EXPECT_NEAR(land_area_base, 1847.04, 1847.04 * m_error_tolerance_hi) << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 61.8245, 61.8245 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 6.29388e8, 6.29388e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 5415.83, 5415.83 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 89.0609, 89.0609 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t N_hel;
//		ssc_data_get_number(data, "N_hel", &N_hel);
//		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t rec_height;
//		ssc_data_get_number(data, "rec_height", &rec_height);
//		EXPECT_NEAR(rec_height, 21.6029, 21.6029 * m_error_tolerance_hi) << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t A_sf;
//		ssc_data_get_number(data, "A_sf", &A_sf);
//		EXPECT_NEAR(A_sf, 1.26905e6, 1.26905e6 * m_error_tolerance_hi) << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t D_rec;
//		ssc_data_get_number(data, "D_rec", &D_rec);
//		EXPECT_NEAR(D_rec, 17.65, 17.65 * m_error_tolerance_hi) << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 96449.7, 96449.7 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t csp_pt_cost_total_land_area;
//		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//		EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.04, 1892.04 * m_error_tolerance_hi) << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t h_tower;
//		ssc_data_get_number(data, "h_tower", &h_tower);
//		EXPECT_NEAR(h_tower, 193.458, 193.458 * m_error_tolerance_hi) << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		//ssc_number_t VARIABLE;
//		//ssc_data_get_number(data, "VARIABLE", &VARIABLE);
//		//EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}

/// Test tcsmolten_salt with power cycle alternative: User Defined
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, User_Defined_SingleOwner_cmod_tcsmolten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsmolten_salt_daggett_UD_default(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 5.9082e8, 5.9082e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t land_area_base;
//		ssc_data_get_number(data, "land_area_base", &land_area_base);
//		EXPECT_NEAR(land_area_base, 1847.04, 1847.04 * m_error_tolerance_hi) << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 65.1644, 65.1644 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 6.4659e8, 6.4659e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 5708.4, 5708.4 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 91.3747, 91.3747 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t N_hel;
//		ssc_data_get_number(data, "N_hel", &N_hel);
//		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t rec_height;
//		ssc_data_get_number(data, "rec_height", &rec_height);
//		EXPECT_NEAR(rec_height, 21.6029, 21.6029 * m_error_tolerance_hi) << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t A_sf;
//		ssc_data_get_number(data, "A_sf", &A_sf);
//		EXPECT_NEAR(A_sf, 1.26905e6, 1.26905e6 * m_error_tolerance_hi) << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t D_rec;
//		ssc_data_get_number(data, "D_rec", &D_rec);
//		EXPECT_NEAR(D_rec, 17.65, 17.65 * m_error_tolerance_hi) << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 55965.3, 55965.3 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t csp_pt_cost_total_land_area;
//		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//		EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.04, 1892.04 * m_error_tolerance_hi) << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t h_tower;
//		ssc_data_get_number(data, "h_tower", &h_tower);
//		EXPECT_NEAR(h_tower, 193.458, 193.458 * m_error_tolerance_hi) << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		//ssc_number_t VARIABLE;
//		//ssc_data_get_number(data, "VARIABLE", &VARIABLE);
//		//EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}

/// Test tcsmolten_salt with alternative power cycle: Super Critical CO2
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, SCO2_Default_SingleOwner_cmod_tcsmolten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsmolten_salt_daggett_SCO2_default(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 5.14776e8, 5.14776e8 * m_error_tolerance_hi) << "Annual Energy";  // choose m_error_tolerance_hi
//
//		ssc_number_t land_area_base;
//		ssc_data_get_number(data, "land_area_base", &land_area_base);
//		EXPECT_NEAR(land_area_base, 1847.04, 1847.04 * m_error_tolerance_hi) << "Land Area Base";
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 56.7772, 56.7772 * m_error_tolerance_hi) << "Capacity Factor";  // choose m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 6.1858e8, 6.1858e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 4973.68, 4973.68 * m_error_tolerance_hi) << "kwh per kw";  // choose m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 83.219, 83.219 * m_error_tolerance_hi) << "Conversion Factor";  // choose m_error_tolerance_hi
//
//		ssc_number_t N_hel;
//		ssc_data_get_number(data, "N_hel", &N_hel);
//		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";
//
//		ssc_number_t rec_height;
//		ssc_data_get_number(data, "rec_height", &rec_height);
//		EXPECT_NEAR(rec_height, 21.6029, 21.6029 * m_error_tolerance_hi) << "Rec Height";
//
//		ssc_number_t A_sf;
//		ssc_data_get_number(data, "A_sf", &A_sf);
//		EXPECT_NEAR(A_sf, 1.26905e6, 1.26905e6 * m_error_tolerance_hi) << "Solar Field Area";
//
//		ssc_number_t D_rec;
//		ssc_data_get_number(data, "D_rec", &D_rec);
//		EXPECT_NEAR(D_rec, 17.65, 17.65 * m_error_tolerance_hi) << "Receiver Outer Diameter";
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 55965.3, 55965.3 * m_error_tolerance_hi) << "Annual Total Water Use";
//
//		ssc_number_t csp_pt_cost_total_land_area;
//		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//		EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.04, 1892.04 * m_error_tolerance_hi) << "Total Land Area";
//
//		ssc_number_t h_tower;
//		ssc_data_get_number(data, "h_tower", &h_tower);
//		EXPECT_NEAR(h_tower, 193.458, 193.458 * m_error_tolerance_hi) << "Tower Height";
//	}
//}

/// Test tcsmolten_salt with alternative power cycle: Super Critical CO2
/// Cycle Configuration alternative: Partial Cooling
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, SCO2_Partial_Cooling_SingleOwner_cmod_tcsmolten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsmolten_salt_daggett_SCO2_partial_cooling(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 5.43316e8, 5.43316e8 * m_error_tolerance_hi) << "Annual Energy";  // choose m_error_tolerance_hi
//
//		ssc_number_t land_area_base;
//		ssc_data_get_number(data, "land_area_base", &land_area_base);
//		EXPECT_NEAR(land_area_base, 1847.04, 1847.04 * m_error_tolerance_hi) << "Land Area Base";
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 59.925, 59.925 * m_error_tolerance_hi) << "Capacity Factor";  // choose m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 6.15469e8, 6.15469e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 5249.43, 5249.43 * m_error_tolerance_hi) << "kwh per kw";  // choose m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 88.2767, 88.2767 * m_error_tolerance_hi) << "Conversion Factor";  // choose m_error_tolerance_hi
//
//		ssc_number_t N_hel;
//		ssc_data_get_number(data, "N_hel", &N_hel);
//		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";
//
//		ssc_number_t rec_height;
//		ssc_data_get_number(data, "rec_height", &rec_height);
//		EXPECT_NEAR(rec_height, 21.6029, 21.6029 * m_error_tolerance_hi) << "Rec Height";
//
//		ssc_number_t A_sf;
//		ssc_data_get_number(data, "A_sf", &A_sf);
//		EXPECT_NEAR(A_sf, 1.26905e6, 1.26905e6 * m_error_tolerance_hi) << "Solar Field Area";
//
//		ssc_number_t D_rec;
//		ssc_data_get_number(data, "D_rec", &D_rec);
//		EXPECT_NEAR(D_rec, 17.65, 17.65 * m_error_tolerance_hi) << "Receiver Outer Diameter";
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 55965.3, 55965.3 * m_error_tolerance_hi) << "Annual Total Water Use";
//
//		ssc_number_t csp_pt_cost_total_land_area;
//		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//		EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.04, 1892.04 * m_error_tolerance_hi) << "Total Land Area";
//
//		ssc_number_t h_tower;
//		ssc_data_get_number(data, "h_tower", &h_tower);
//		EXPECT_NEAR(h_tower, 193.458, 193.458 * m_error_tolerance_hi) << "Tower Height";
//	}
//}

/// Test tcsmolten_salt with alternative power cycle: Super Critical CO2
/// Materials and Flow alternative: Flow pattern 2 instead of 1
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, SCO2_Flow_Pattern_Alternative_SingleOwner_cmod_tcsmolten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsmolten_salt_daggett_SCO2_flow_pattern_2(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 5.15291e8, 5.15291e8 * m_error_tolerance_hi) << "Annual Energy";  // choose m_error_tolerance_hi
//
//		ssc_number_t land_area_base;
//		ssc_data_get_number(data, "land_area_base", &land_area_base);
//		EXPECT_NEAR(land_area_base, 1847.04, 1847.04 * m_error_tolerance_hi) << "Land Area Base";
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 56.834, 56.834 * m_error_tolerance_hi) << "Capacity Factor";  // choose m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 6.19302e8, 6.19302e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 4978.66, 4978.66 * m_error_tolerance_hi) << "kwh per kw";  // choose m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 83.7374, 83.7374 * m_error_tolerance_hi) << "Conversion Factor";  // choose m_error_tolerance_hi
//
//		ssc_number_t N_hel;
//		ssc_data_get_number(data, "N_hel", &N_hel);
//		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";
//
//		ssc_number_t rec_height;
//		ssc_data_get_number(data, "rec_height", &rec_height);
//		EXPECT_NEAR(rec_height, 21.6029, 21.6029 * m_error_tolerance_hi) << "Rec Height";
//
//		ssc_number_t A_sf;
//		ssc_data_get_number(data, "A_sf", &A_sf);
//		EXPECT_NEAR(A_sf, 1.26905e6, 1.26905e6 * m_error_tolerance_hi) << "Solar Field Area";
//
//		ssc_number_t D_rec;
//		ssc_data_get_number(data, "D_rec", &D_rec);
//		EXPECT_NEAR(D_rec, 17.65, 17.65 * m_error_tolerance_hi) << "Receiver Outer Diameter";
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 55965.3, 55965.3 * m_error_tolerance_hi) << "Annual Total Water Use";
//
//		ssc_number_t csp_pt_cost_total_land_area;
//		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//		EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.04, 1892.04 * m_error_tolerance_hi) << "Total Land Area";
//
//		ssc_number_t h_tower;
//		ssc_data_get_number(data, "h_tower", &h_tower);
//		EXPECT_NEAR(h_tower, 193.458, 193.458 * m_error_tolerance_hi) << "Tower Height";
//	}
//}

/// Test series of Advanced Combinatorial Testing System (ACTS) runs
//TEST_F(CMTcsMoltenSalt, ACTS_sCO2_recompression) {
//
//	// Outputs of 6/15 total ACTS tests for the sCO2 model
//	// The other 9 test case scenarios were unable to simulate properly
//	// on the SAM UI.
//
//	//  // ACTS pass/fail summary				F          F          P         P          P          F
//	//  // sCO2 ACTS Test Cases                 2          4          5         7         12         15
//	//  std::vector<double> annual_energys{ 4.47253e8, 4.83719e8, 5.3984e8, 5.29801e8, 5.12115e8, 4.648e8 };
//	//  std::vector<double> land_area_bases{ 1847.04, 1847.04, 1847.04, 1847.04, 1847.04, 1847.04 };
//	//  std::vector<double> capacity_factors{ 49.3297, 53.3518, 59.5417, 58.4344, 56.4837, 51.2651 };
//	//  std::vector<double> annual_W_cycle_grosss{ 5.20554e8, 6.03866e8, 6.21568e8, 6.29351e8, 6.31792e8, 5.49548e8 };
//	//  std::vector<double> kwh_per_kws{ 4321.28, 4673.62, 5215.85, 5118.85, 4947.98, 4490.82 };
//	//  std::vector<double> conversion_factors{ 85.9187, 80.1037, 86.8513, 84.1821, 81.0576, 84.5787 };
//	//  std::vector<double> N_hels{ 8790, 8790, 8790, 8790, 8790, 8790 };
//	//  std::vector<double> rec_heights{ 21.6029, 21.6029, 21.6029, 21.6029, 21.6029, 21.6029 };
//	//  std::vector<double> A_sfs{ 1.26905e6, 1.26905e6, 1.26905e6, 1.26905e6, 1.26905e6, 1.26905e6 };
//	//  std::vector<double> D_recs{ 17.65, 17.65, 17.65, 17.65, 17.65, 17.65 };
//	//  std::vector<double> annual_total_water_uses{ 55965.3, 55965.3, 55965.3, 55965.3, 55965.3, 55965.3 };
//	//  std::vector<double> csp_pt_cost_total_land_areas{ 1892.04, 1892.04, 1892.04, 1892.04, 1892.04, 1892.04 };
//	//  std::vector<double> h_towers{ 193.458, 193.458, 193.458, 193.458, 193.458, 193.458 };
//
//	// Passing ACTS configurations
//	// sCO2 ACTS Test Cases                 5         7         12
//	std::vector<double> annual_energys{ 5.3984e8, 5.29801e8, 5.12115e8 };
//	std::vector<double> land_area_bases{ 1847.04, 1847.04, 1847.04 };
//	std::vector<double> capacity_factors{ 59.5417, 58.4344, 56.4837 };
//	std::vector<double> annual_W_cycle_grosss{ 6.21568e8, 6.29351e8, 6.31792e8 };
//	std::vector<double> kwh_per_kws{ 5215.85, 5118.85, 4947.98 };
//	std::vector<double> conversion_factors{ 86.8513, 84.1821, 81.0576 };
//	std::vector<double> N_hels{ 8790, 8790, 8790 };
//	std::vector<double> rec_heights{ 21.6029, 21.6029, 21.6029 };
//	std::vector<double> A_sfs{ 1.26905e6, 1.26905e6, 1.26905e6 };
//	std::vector<double> D_recs{ 17.65, 17.65, 17.65 };
//	std::vector<double> annual_total_water_uses{ 55965.3, 55965.3, 55965.3 };
//	std::vector<double> csp_pt_cost_total_land_areas{ 1892.04, 1892.04, 1892.04 };
//	std::vector<double> h_towers{ 193.458, 193.458, 193.458 };
//
//	ssc_data_t data = ssc_data_create();
//
//	for (std::vector<double>::size_type i = 0; i != annual_energys.size(); i++) {
//		int test_errors = ACTS_sCO2_testing(data, i);
//
//		EXPECT_FALSE(test_errors);
//		if (!test_errors)
//		{
//			ssc_number_t annual_energy;
//			ssc_data_get_number(data, "annual_energy", &annual_energy);
//			EXPECT_NEAR(annual_energy, annual_energys[i], annual_energys[i] * m_error_tolerance_hi) << "Annual Energy"; // choose m_error_tolerance_hi
//
//			ssc_number_t land_area_base;
//			ssc_data_get_number(data, "land_area_base", &land_area_base);
//			EXPECT_NEAR(land_area_base, land_area_bases[i], land_area_bases[i] * m_error_tolerance_hi) << "Land Area Base";
//
//			ssc_number_t capacity_factor;
//			ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//			EXPECT_NEAR(capacity_factor, capacity_factors[i], capacity_factors[i] * m_error_tolerance_hi) << "Capacity Factor"; // choose m_error_tolerance_hi
//
//			ssc_number_t annual_W_cycle_gross;
//			ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//			EXPECT_NEAR(annual_W_cycle_gross, annual_W_cycle_grosss[i], annual_W_cycle_grosss[i] * m_error_tolerance_hi) << "Annual W_cycle Gross"; // choose m_error_tolerance_hi
//
//			ssc_number_t kwh_per_kw;
//			ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//			EXPECT_NEAR(kwh_per_kw, kwh_per_kws[i], kwh_per_kws[i] * m_error_tolerance_hi) << "kwh per kw"; // choose m_error_tolerance_hi
//
//			ssc_number_t conversion_factor;
//			ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//			EXPECT_NEAR(conversion_factor, conversion_factors[i], conversion_factors[i] * m_error_tolerance_hi) << "Conversion Factor"; // choose m_error_tolerance_hi
//
//			ssc_number_t N_hel;
//			ssc_data_get_number(data, "N_hel", &N_hel);
//			EXPECT_NEAR(N_hel, N_hels[i], N_hels[i] * m_error_tolerance_hi) << "Number of Heliostats";
//
//			ssc_number_t rec_height;
//			ssc_data_get_number(data, "rec_height", &rec_height);
//			EXPECT_NEAR(rec_height, rec_heights[i], rec_heights[i] * m_error_tolerance_hi) << "Rec Height";
//
//			ssc_number_t A_sf;
//			ssc_data_get_number(data, "A_sf", &A_sf);
//			EXPECT_NEAR(A_sf, A_sfs[i], A_sfs[i] * m_error_tolerance_hi) << "Solar Field Area";
//
//			ssc_number_t D_rec;
//			ssc_data_get_number(data, "D_rec", &D_rec);
//			EXPECT_NEAR(D_rec, D_recs[i], D_recs[i] * m_error_tolerance_hi) << "Receiver Outer Diameter";
//
//			ssc_number_t annual_total_water_use;
//			ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//			EXPECT_NEAR(annual_total_water_use, annual_total_water_uses[i], annual_total_water_uses[i] * m_error_tolerance_hi) << "Annual Total Water Use";
//
//			ssc_number_t csp_pt_cost_total_land_area;
//			ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//			EXPECT_NEAR(csp_pt_cost_total_land_area, csp_pt_cost_total_land_areas[i], csp_pt_cost_total_land_areas[i] * m_error_tolerance_hi) << "Total Land Area";
//
//			ssc_number_t h_tower;
//			ssc_data_get_number(data, "h_tower", &h_tower);
//			EXPECT_NEAR(h_tower, h_towers[i], h_towers[i] * m_error_tolerance_hi) << "Tower Height";
//
//		}
//	}
//}
