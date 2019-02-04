#include <gtest/gtest.h>

#include "cmod_geothermal_costs_test.h"



TEST_F(CMGeothermalCosts, CostModuleTest)
{
	//Check whether module runs with any errors:
	int geothermal_errors = run_module(data, "geothermal_costs");
	ASSERT_EQ(geothermal_errors, 0);

	
	
	ssc_number_t baseline_cost;
	ssc_data_get_number(data, "baseline_cost", &baseline_cost);
	EXPECT_NEAR(baseline_cost, 1943.0, 100 );
}
