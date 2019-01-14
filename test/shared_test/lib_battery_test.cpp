#include <gtest/gtest.h>

#include <lib_battery.h>

#include "lib_battery_test.h"

/// Test  lithium ion battery capacity response
TEST_F(BatteryTest, LithiumIonCapacityTest)
{
	q = 100;
	SOC_init = 100;
	SOC_min = 20;
	SOC_max = 100;

	if (capacityModel) {
		delete capacityModel;
	}
	capacityModel = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min);

	// Check that initial capacity is equal to max
	EXPECT_EQ(capacityModel->SOC(), SOC_max);

	// Check that discharge of battery results in correct capacity
	double I = 10;
	capacityModel->updateCapacity(I, 1);
	EXPECT_EQ(capacityModel->SOC(), 90);
	EXPECT_EQ(capacityModel->q0(), 90);

	// check that charge of battery results in correct capacity
	I = -10;
	capacityModel->updateCapacity(I, 1);
	EXPECT_EQ(capacityModel->SOC(), 100);
	EXPECT_EQ(capacityModel->q0(), 100);

	// check that updating thermal behavior changes capacity as expected
	capacityModel->updateCapacityForThermal(95);
	capacityModel->check_SOC();
	EXPECT_EQ(capacityModel->q0(), 95);
	EXPECT_EQ(capacityModel->qmax(), 100);
	capacityModel->updateCapacityForThermal(100);
	capacityModel->check_SOC();

	// check that updating lifetime degradation changes capacity
	capacityModel->updateCapacityForLifetime(95);
	EXPECT_EQ(capacityModel->q0(), 95);
	EXPECT_EQ(capacityModel->qmax(), 95);

	// check that battery replacement works
	capacityModel->replace_battery();
	EXPECT_EQ(capacityModel->SOC(), SOC_max);
	EXPECT_EQ(capacityModel->q0(), 100);
	EXPECT_EQ(capacityModel->qmax(), 100);

	// check that model correctly detects overcharge, undercharge
	capacityModel->updateCapacity(I, 1);
	EXPECT_EQ(capacityModel->q0(), 100);
	EXPECT_EQ(capacityModel->SOC(), SOC_max);

	I = 110;
	capacityModel->updateCapacity(I, 1);
	EXPECT_EQ(capacityModel->q0(), 20);
	EXPECT_EQ(capacityModel->SOC(), SOC_min);
}

TEST_F(BatteryTest, LossesModel)
{
	size_t idx = 1000;

	// Return loss for february
	lossModel->run_losses(idx);
	EXPECT_EQ(lossModel->getLoss(idx), 1);

}