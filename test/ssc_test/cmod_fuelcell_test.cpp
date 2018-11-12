#include <gtest/gtest.h>

#include "cmod_fuelcell_test.h"

/// Test PVSAMv1 with all defaults and no-financial model
TEST_F(CMFuelCell, DefaultNoFinancialModel) {

	int errors = run_module(data, "fuelcell");
	EXPECT_FALSE(errors);
	if (!errors)
	{

	}
}