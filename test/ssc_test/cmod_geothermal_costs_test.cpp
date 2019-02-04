#include <gtest/gtest.h>

#include "cmod_geothermal_costs_test.h"



TEST_F(CMGeothermalCosts, CostModuleTest)
{
	//Check whether module runs with any errors:
	int geothermal_errors = run_module(data, "geothermal_costs");
	ASSERT_EQ(geothermal_errors, 1);
		
	ssc_number_t baseline_cost, gross_output;
	ssc_data_get_number(data, "baseline_cost", &baseline_cost);
	ssc_data_get_number(data, "gross_output", &gross_output);
	EXPECT_GE(baseline_cost, 11100);
	//if (!geothermal_errors)	// (!=geothermal_errors) == True;
	//{
	//	EXPECT_GE(baseline_cost, 2100);
	//}
}
