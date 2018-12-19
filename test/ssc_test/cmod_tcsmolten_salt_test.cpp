#include <gtest/gtest.h>

#include "cmod_tcsmolten_salt_test.h"
#include "../input_cases/tcsmolten_salt_cases.h"
#include "../input_cases/weather_inputs.h"

/// Test tcsmolten_salt with all defaults and the single owner financial model
TEST_F(CMTcsMoltenSalt, DefaultSingleOwnerFinancialModel) {

    ssc_data_t data = ssc_data_create();
    int test_errors = tcsmolten_salt_daggett(data);

    EXPECT_FALSE(test_errors);
    if (!test_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, 5.77916e8, 5.77916e8 * m_error_tolerance_lo) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t land_area_base;
        ssc_data_get_number(data, "land_area_base", &land_area_base);
        EXPECT_NEAR(land_area_base, 1847.04, 1847.04 * m_error_tolerance_lo) << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t capacity_factor;
        ssc_data_get_number(data, "capacity_factor", &capacity_factor);
        EXPECT_NEAR(capacity_factor, 63.7413, 63.7413 * m_error_tolerance_lo) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t annual_W_cycle_gross;
        ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
        EXPECT_NEAR(annual_W_cycle_gross, 6.46098e8, 6.46098e8 * m_error_tolerance_lo) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t kwh_per_kw;
        ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
        EXPECT_NEAR(kwh_per_kw, 5583.73, 5583.73 * m_error_tolerance_lo) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t conversion_factor;
        ssc_data_get_number(data, "conversion_factor", &conversion_factor);
        EXPECT_NEAR(conversion_factor, 89.4472, 89.4472 * m_error_tolerance_lo) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t N_hel;
        ssc_data_get_number(data, "N_hel", &N_hel);
        EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_lo) << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t rec_height;
        ssc_data_get_number(data, "rec_height", &rec_height);
        EXPECT_NEAR(rec_height, 21.6029, 21.6029 * m_error_tolerance_lo) << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t A_sf;
        ssc_data_get_number(data, "A_sf", &A_sf);
        EXPECT_NEAR(A_sf, 1.26905e6, 1.26905e6 * m_error_tolerance_lo) << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t D_rec;
        ssc_data_get_number(data, "D_rec", &D_rec);
        EXPECT_NEAR(D_rec, 17.65, 17.65 * m_error_tolerance_lo) << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t annual_total_water_use;
        ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
        EXPECT_NEAR(annual_total_water_use, 97591.1, 97591.1 * m_error_tolerance_lo) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t csp_pt_cost_total_land_area;
        ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
        EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.04, 1892.04 * m_error_tolerance_lo) << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t h_tower;
        ssc_data_get_number(data, "h_tower", &h_tower);
        EXPECT_NEAR(h_tower, 193.458, 193.458 * m_error_tolerance_lo) << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        //ssc_number_t VARIABLE;
        //ssc_data_get_number(data, "VARIABLE", &VARIABLE);
        //EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_lo) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
    }
}

//TestResult tcsmoltenSaltSingleOwnerDefaultResult[] = {
//    /*  SSC Var Name                            Test Type           Test Result             Error Bound % */
//    { "annual_energy",                          NR,                 5.77916e8,              0.1 },  // Annual total electric power to grid
//    { "land_area_base",                         NR,                 1847.04,                0.1 },  // Base land area occupied by heliostats
//    { "capacity_factor",                        NR,                 63.7413,                0.1 },  // Capacity factor
//    { "annual_W_cycle_gross",                   NR,                 6.46098e8,              0.1 },  // Electrical source - Power cycle gross output
//    { "kwh_per_kw",                             NR,                 5583.73,                0.1 },  // First year kWh/kW
//    { "conversion_factor",                      NR,                 89.4472,                0.1 },  // Gross to Net Conversion Factor
//    { "N_hel",                                  NR,                 8790,                   0.1 },  // Number of heliostats
//    { "rec_height",                             NR,                 21.6029,                0.1 },  // Receiver height
//    { "A_sf",                                   NR,                 1.26905e6,              0.1 },  // Solar Field Area
//    { "D_rec",                                  NR,                 17.65,                  0.1 },  // The overall outer diameter of the receiver
//    { "annual_total_water_use",                 NR,                 97591.1,                0.1 },  // Total Annual Water Usage: cycle + mirror washing
//    { "csp.pt.cost.total_land_area",            NR,                 1892.04,                0.1 },  // Total land area
//    { "h_tower",                                NR,                 193.458,                0.1 },  // Tower height
//};
