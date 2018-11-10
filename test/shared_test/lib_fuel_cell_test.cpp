#include "lib_fuel_cell_test.h"


TEST_F(FuelCellTest, Initialize)
{
	// Test if started up
	EXPECT_EQ(fuelCell->isRunning(), false);

	// Test if fuel consumption curve correctly generated
	EXPECT_EQ(fuelCell->calculateFuelConsumptionMCf(0), 0);
	EXPECT_NEAR(fuelCell->calculateFuelConsumptionMCf(1), 0.647674, 0.01);
	EXPECT_NEAR(fuelCell->calculateFuelConsumptionMCf(2), 0.647674, 0.01);
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

TEST_F(FuelCellTest, AvailableFuel) {

	
	// Run for startup_hours, assume no fuel consumed from available stock during startup
	for (size_t h = 0; h < (size_t)startup_hours; h++) {
		fuelCell->runSingleTimeStep(20);
		EXPECT_EQ(fuelCell->getAvailableFuel(), availableFuel_Mcf);
	}

	// Available fuel should start decreasing
	double availableFuelTrack = availableFuel_Mcf;
	for (size_t h = (size_t)startup_hours; h < (size_t)startup_hours + 10; h++) {
		fuelCell->runSingleTimeStep(20);
		EXPECT_EQ(fuelCell->getAvailableFuel(), availableFuelTrack - fuelCell->getFuelConsumption());
		availableFuelTrack -= fuelCell->getFuelConsumption();
	}

}

TEST_F(FuelCellTest, DispatchFixed) {

	// Allow fuel cell to startup
	for (size_t h = 0; h < (size_t)startup_hours; h++) {
		fuelCellDispatch->runSingleTimeStep(h, 0, 0);
		EXPECT_EQ(fuelCell->getPower(), 0);
	}

	// Unit will take two hours to fully ramp to 40 kW
	fuelCellDispatch->runSingleTimeStep(startup_hours, 0, 0);
	EXPECT_EQ(fuelCell->getPower(), 20);

	// Run at fixed output, which will go lower than min turndown
	for (size_t h = (size_t)startup_hours + 1; h < (size_t)startup_hours + 10; h++) {
		fuelCellDispatch->runSingleTimeStep(h, 20,10);
		EXPECT_EQ(fuelCell->getPower(), unitPowerMax_kW * fixed_percent * 0.01);
	}

}

TEST_F(FuelCellTest, DispatchLoadFollow) {

	fuelCellDispatch->setDispatchOption(FuelCellDispatch::FC_DISPATCH_OPTION::LOAD_FOLLOW);

	// Allow fuel cell to startup
	for (size_t h = 0; h < (size_t)startup_hours; h++) {
		fuelCellDispatch->runSingleTimeStep(h, 0, 20);
		EXPECT_EQ(fuelCell->getPower(), 0);
	}

	// Dispatch fuel cell for net load of 20 kW
	fuelCellDispatch->runSingleTimeStep(startup_hours, 20, 40);
	EXPECT_EQ(fuelCell->getPower(), 20);

	// Dispatch fuel cell for net load of 60 kW, dynamic response should limit to 40 kW
	fuelCellDispatch->runSingleTimeStep(startup_hours + 1, 20, 80);
	EXPECT_EQ(fuelCell->getPower(), 40);

	// Dispatch fuel cell for net load of 60 kW, should be fully ramped by now
	fuelCellDispatch->runSingleTimeStep(startup_hours + 2, 20, 80);
	EXPECT_EQ(fuelCell->getPower(), 60);
}

TEST_F(FuelCellTest, DispatchManual) {

	fuelCellDispatch->setDispatchOption(FuelCellDispatch::FC_DISPATCH_OPTION::MANUAL);

	// Allow fuel cell to startup
	for (size_t h = 0; h < (size_t)startup_hours; h++) {
		fuelCellDispatch->runSingleTimeStep(h, 0, 20);
		EXPECT_EQ(fuelCell->getPower(), 0);
	}

	// Dispatch fuel cell at 40% of max output (40 kW, limited by dynamic response)
	fuelCellDispatch->runSingleTimeStep(startup_hours, 20, 40);
	EXPECT_EQ(fuelCell->getPower(), 20);

	// Dispatch fuel cell at 40% of max output (40 kW)
	fuelCellDispatch->runSingleTimeStep(startup_hours + 1, 20, 80);
	EXPECT_EQ(fuelCell->getPower(), 40);
}