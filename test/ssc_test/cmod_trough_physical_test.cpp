#include <gtest/gtest.h>

#include "cmod_trough_physical_test.h"
#include "../tcs_test/trough_physical_cases.h"
#include "../input_cases/weather_inputs.h"

/// Test trough_physical with all defaults and no-financial model
TEST_F(CMTroughPhysical, DefaultNoFinancialModel){
	
	int test_errors = run_module(data, "trough_physical");

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
        ssc_number_t annual_gross_energy;
        ssc_data_get_number(data, "annual_gross_energy", &annual_gross_energy);
        EXPECT_NEAR(annual_gross_energy, 3.703e8, 3.703e8 * m_error_tolerance_lo) << "Annual Gross Thermal Energy Production";

        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, 3.697e8, 3.697e8 * m_error_tolerance_lo) << "Annual Net Thermal Energy Production";

        ssc_number_t annual_thermal_consumption;
        ssc_data_get_number(data, "annual_thermal_consumption", &annual_thermal_consumption);
        EXPECT_NEAR(annual_thermal_consumption, 588152, 588152 * m_error_tolerance_lo) << "Annual Thermal Freeze Protection";

        ssc_number_t annual_tes_freeze_protection;
        ssc_data_get_number(data, "annual_tes_freeze_protection", &annual_tes_freeze_protection);
        EXPECT_NEAR(annual_tes_freeze_protection, 588152, 588152 * m_error_tolerance_lo) << "Annual TES Freeze Protection";

        ssc_number_t annual_field_freeze_protection;
        ssc_data_get_number(data, "annual_field_freeze_protection", &annual_field_freeze_protection);
        EXPECT_NEAR(annual_field_freeze_protection, 0., m_error_tolerance_lo) << "Annual Field Freeze Protection";

        ssc_number_t capacity_factor;
        ssc_data_get_number(data, "capacity_factor", &capacity_factor);
        EXPECT_NEAR(capacity_factor, 42.24, 42.24 * m_error_tolerance_lo) << "Capacity factor";

        ssc_number_t annual_W_cycle_gross;
        ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
        EXPECT_NEAR(annual_W_cycle_gross, 4.225e8, 4.225e8 * m_error_tolerance_lo) << "Power cycle gross electrical output";

        ssc_number_t kwh_per_kw;
        ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
        EXPECT_NEAR(kwh_per_kw, 3701., 3701. * m_error_tolerance_lo) << "First year kWh/kW";

        ssc_number_t conversion_factor;
        ssc_data_get_number(data, "conversion_factor", &conversion_factor);
        EXPECT_NEAR(conversion_factor, 87.51, 87.51 * m_error_tolerance_lo) << "Gross to Net Conversion Factor";

        ssc_number_t annual_total_water_use;
        ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
        EXPECT_NEAR(annual_total_water_use, 81031, 81031 * m_error_tolerance_lo) << "Annual Total Water Use";

		//ssc_number_t VARIABLE;
		//ssc_data_get_number(data, "VARIABLE", &VARIABLE);
		//EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_lo) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
	}
}

/// Test trough_physical with all defaults and the financial model in the LCOH Calculator
//TEST_F(CMTroughPhysical, DefaultLCOHFinancialModel) {
//
//    ssc_data_t data = ssc_data_create();
//    int test_errors = trough_physical_tucson(data);
//
//    EXPECT_FALSE(test_errors);
//    if (!test_errors)
//    {
//        ssc_number_t annual_gross_energy;
//        ssc_data_get_number(data, "annual_gross_energy", &annual_gross_energy);
//        EXPECT_NEAR(annual_gross_energy, 2.44933e7, 2.44933e7 * m_error_tolerance_lo) << "Annual Gross Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_energy;
//        ssc_data_get_number(data, "annual_energy", &annual_energy);
//        EXPECT_NEAR(annual_energy, 2.44931e7, 2.44931e7 * m_error_tolerance_lo) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_electricity_consumption;
//        ssc_data_get_number(data, "annual_electricity_consumption", &annual_electricity_consumption);
//        EXPECT_NEAR(annual_electricity_consumption, 122659, 122659 * m_error_tolerance_lo) << "Annual Electricity Consumption";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t fixed_operating_cost;
//        ssc_data_get_number(data, "fixed_operating_cost", &fixed_operating_cost);
//        EXPECT_NEAR(fixed_operating_cost, 111118, 111118 * m_error_tolerance_lo) << "Fixed Operating Cost";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_thermal_consumption;
//        ssc_data_get_number(data, "annual_thermal_consumption", &annual_thermal_consumption);
//        EXPECT_NEAR(annual_thermal_consumption, 236.609, 236.609 * m_error_tolerance_lo) << "Annual Thermal Consumption";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_tes_freeze_protection;
//        ssc_data_get_number(data, "annual_tes_freeze_protection", &annual_tes_freeze_protection);
//        EXPECT_NEAR(annual_tes_freeze_protection, 236.609, 236.609 * m_error_tolerance_lo) << "Annual TES Freeze Protection";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_field_freeze_protection;
//        ssc_data_get_number(data, "annual_field_freeze_protection", &annual_field_freeze_protection);
//        EXPECT_NEAR(annual_field_freeze_protection, 0., m_error_tolerance_lo) << "Annual Field Freeze Protection";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t lcoe_fcr;
//        ssc_data_get_number(data, "lcoe_fcr", &lcoe_fcr);
//        EXPECT_NEAR(lcoe_fcr, 0.0375859, 0.0375859 * m_error_tolerance_lo) << "LCOE FCR";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_total_water_use;
//        ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//        EXPECT_NEAR(annual_total_water_use, 176.333, 176.333 * m_error_tolerance_lo) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        //ssc_number_t VARIABLE;
//        //ssc_data_get_number(data, "VARIABLE", &VARIABLE);
//        //EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_lo) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//    }
//}
