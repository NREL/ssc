#include "lib_fuel_cell_test.h"


TEST_F(FuelCellTest, Initialize)
{
	// Test if started up
	EXPECT_EQ(fuelCell->isRunning(), false);

	// Test if fuel consumption curve correctly generated
	EXPECT_EQ(fuelCell->getFuelConsumptionMCf(0), 0);
	EXPECT_NEAR(fuelCell->getFuelConsumptionMCf(1), 0.647674, 0.01);
	EXPECT_NEAR(fuelCell->getFuelConsumptionMCf(2), 0.647674, 0.01);
}

TEST_F(FuelCellTest, Startup)
{
	// Run for startup_hours - 1
	for (size_t h = 0; h < startup_hours - 1; h++) {
		fuelCell->runSingleTimeStep(20);
		EXPECT_EQ(fuelCell->getPower(), 0);
		EXPECT_FALSE(fuelCell->isRunning());
	}

	// After one more hour it will be started, but won't deliver power during that time step
	fuelCell->runSingleTimeStep(20);
	EXPECT_EQ(fuelCell->getPower(), 0);
	EXPECT_TRUE(fuelCell->isRunning());

	// Next hour, it's fully started up
	fuelCell->runSingleTimeStep(20);
	EXPECT_EQ(fuelCell->getPower(), 20);

	// Test Min Turndown
	fuelCell->runSingleTimeStep(unitPowerMin_kW - 10);
	EXPECT_EQ(fuelCell->getPower(), unitPowerMin_kW);

	// Test dynamic limit
	fuelCell->runSingleTimeStep(100);
	EXPECT_EQ(fuelCell->getPower(), unitPowerMin_kW + dynamicResponse_kWperHour);
	fuelCell->runSingleTimeStep(100);
	fuelCell->runSingleTimeStep(100);
	fuelCell->runSingleTimeStep(100);
	fuelCell->runSingleTimeStep(100);

	// Test Max Limit (is not unitPowerMax_kW due to degradation)
	fuelCell->runSingleTimeStep(unitPowerMax_kW + 10);
	EXPECT_EQ(fuelCell->getPower(), fuelCell->getMaxPower());
}