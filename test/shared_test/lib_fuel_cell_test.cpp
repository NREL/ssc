#include "lib_fuel_cell_test.h"

TEST_F(FuelCellTest, EfficiencyCurve)
{
	fuelCell->calculateEfficiencyCurve(.16);
	EXPECT_EQ(fuelCell->getElectricalEfficiency(), 0.21);
	EXPECT_EQ(fuelCell->getHeatRecoveryEfficiency(), .50);
}

TEST_F(FuelCellTest, Initialize)
{
	// Test if started up
	EXPECT_EQ(fuelCell->isRunning(), false);
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
// Also check multiple fuel cells
TEST_F(FuelCellTest, DispatchFixed) {

	size_t sh = (size_t)startup_hours;

	// Allow fuel cell to startup
	for (size_t h = 0; h < sh; h++) {
		fuelCellDispatchMultiple->runSingleTimeStep(h, h, 0, 0);
		EXPECT_EQ(fuelCell->getPower(), 0);
	}

	// Unit will take two hours to fully ramp to 40 kW
	fuelCellDispatchMultiple->runSingleTimeStep(sh, 0, 0);
	EXPECT_EQ(fuelCell->getPower(), 20);

	// Run at fixed output, which will go lower than min turndown
	for (size_t h = sh + 1; h < sh + 10; h++) {
		fuelCellDispatchMultiple->runSingleTimeStep(h, h, 20,10);
		EXPECT_EQ(fuelCell->getPower(), unitPowerMax_kW * fixed_percent * 0.01);
		EXPECT_EQ(fuelCellDispatchMultiple->getBatteryPower()->powerFuelCellToLoad, 0);
		EXPECT_EQ(fuelCellDispatchMultiple->getBatteryPower()->powerFuelCellToGrid, n_multipleFuelCells * 40);
		EXPECT_EQ(fuelCellDispatchMultiple->getBatteryPower()->powerPVToLoad,  10);
		EXPECT_EQ(fuelCellDispatchMultiple->getBatteryPower()->powerPVToGrid,  10);
	}
}

TEST_F(FuelCellTest, DispatchLoadFollow) {

	size_t sh = (size_t)startup_hours;

	fuelCellDispatch->setDispatchOption(FuelCellDispatch::FC_DISPATCH_OPTION::LOAD_FOLLOW);

	// Allow fuel cell to startup
	for (size_t h = 0; h < sh; h++) {
		fuelCellDispatch->runSingleTimeStep(h, h, 0, 20);
		EXPECT_EQ(fuelCell->getPower(), 0);
	}

	// Dispatch fuel cell for net load of 20 kW
	fuelCellDispatch->runSingleTimeStep(sh, sh, 20, 40);
	EXPECT_EQ(fuelCell->getPower(), 20);

	// Dispatch fuel cell for net load of 60 kW, dynamic response should limit to 40 kW
	fuelCellDispatch->runSingleTimeStep(sh + 1, sh + 1, 20, 80);
	EXPECT_EQ(fuelCell->getPower(), 40);

	// Dispatch fuel cell for net load of 60 kW, should be fully ramped by now
	fuelCellDispatch->runSingleTimeStep(sh + 2, sh + 2, 20, 80);
	EXPECT_EQ(fuelCell->getPower(), 60);
}

TEST_F(FuelCellTest, DispatchManual) {

	size_t sh = (size_t)startup_hours;
	size_t stepsPerHour = (size_t)(1 / dt_subHourly);

	fuelCellDispatchSubhourly->setDispatchOption(FuelCellDispatch::FC_DISPATCH_OPTION::MANUAL);

	// Allow fuel cell to startup
	size_t year_idx = 0;
	for (size_t h = 0; h < sh; h++) {
		for (size_t s = 0; s < stepsPerHour; s++) {
			fuelCellDispatchSubhourly->runSingleTimeStep(h, year_idx, 0, 20);
			year_idx++;
		}
		EXPECT_EQ(fuelCellSubHourly->getPower(), 0);
	}

	// Dispatch fuel cell at 40% of max output (40 kW, limited by dynamic response, and min turndown)
	for (size_t s = 0; s < stepsPerHour; s++) {
		fuelCellDispatchSubhourly->runSingleTimeStep(sh, year_idx, 20, 40);
		year_idx++;
	}
	EXPECT_EQ(fuelCellSubHourly->getPower(), 35);

	// Dispatch fuel cell at 40% of max output (40 kW)
	for (size_t s = 0; s < stepsPerHour; s++) {
		fuelCellDispatchSubhourly->runSingleTimeStep(sh + 1, year_idx, 20, 80);
		EXPECT_EQ(fuelCellSubHourly->getPower(), 40);
		year_idx++;
	}
}

TEST_F(FuelCellTest, DispatchInput) {

	size_t sh = (size_t)startup_hours;

	// Dispatch input is set to constant 50 kW
	fuelCellDispatch->setDispatchOption(FuelCellDispatch::FC_DISPATCH_OPTION::INPUT);

	// Allow fuel cell to startup
	for (size_t h = 0; h < sh; h++) {
		fuelCellDispatch->runSingleTimeStep(h, h, 0, 20);
		EXPECT_EQ(fuelCell->getPower(), 0);
	}

	// Dispatch fuel cell at 50% of max output (50 kW, limited by dynamic response)
	fuelCellDispatch->runSingleTimeStep(sh, sh, 0, 0);
	EXPECT_EQ(fuelCell->getPower(), 20);

	// Dispatch fuel cell at 50% of max output (50 kW, limited by dynamic response)
	fuelCellDispatch->runSingleTimeStep(sh + 1, sh + 1, 0, 0);
	EXPECT_EQ(fuelCell->getPower(), 40);

	// Dispatch fuel cell at 50% of max output (50 kW)
	for (size_t h = sh + 2; h < 50; h++) {
		fuelCellDispatch->runSingleTimeStep(h, h, 0, 0);
		EXPECT_EQ(fuelCell->getPower(), 50);
	}
}