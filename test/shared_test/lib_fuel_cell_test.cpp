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

TEST_F(FuelCellTest, FixedOutput)
{
	fuelCell->runSingleTimeStep();


}