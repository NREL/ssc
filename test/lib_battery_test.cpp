#include "gtest\gtest.h"
#include <shared/lib_battery.h>

class LithiumIonBatteryProperties : public ::testing::Test
{
protected:
	double q;
	double SOC_min;
	double SOC_max;

	void SetUp()
	{
		q = 100;
		SOC_min = 20;
		SOC_max = 100;
	}
};

class LithiumIonBattery : public LithiumIonBatteryProperties
{
protected:
	capacity_lithium_ion_t * capacity_model; 

	void SetUp()
	{
		LithiumIonBatteryProperties::SetUp();
		capacity_model = new capacity_lithium_ion_t(q, SOC_max, SOC_min);
	}
	void TearDown()
	{
		if (capacity_model)
			delete capacity_model;
	}
};

TEST_F(LithiumIonBattery, CapacityUnitTest)
{
	// Check that initial capacity is equal to max
	EXPECT_EQ(capacity_model->SOC(), SOC_max);

	// Check that discharge of battery results in correct capacity
	capacity_model->updateCapacity(10, 1);
	EXPECT_EQ(capacity_model->SOC(), 90);
	EXPECT_EQ(capacity_model->q0(), 90);

	// check that charge of battery results in correct capacity
	capacity_model->updateCapacity(-10, 1);
	EXPECT_EQ(capacity_model->SOC(), 100);
	EXPECT_EQ(capacity_model->q0(), 100);

	// check that updating thermal behavior changes capacity as expected
	capacity_model->updateCapacityForThermal(95);
	EXPECT_EQ(capacity_model->q0(), 95);
	EXPECT_EQ(capacity_model->qmax(), 100);
	capacity_model->updateCapacityForThermal(100);

	// check that updating lifetime degradation changes capacity
	capacity_model->updateCapacityForLifetime(95);
	EXPECT_EQ(capacity_model->q0(), 95);
	EXPECT_EQ(capacity_model->qmax(), 95);

	// check that battery replacement works
	capacity_model->replace_battery();
	EXPECT_EQ(capacity_model->SOC(), SOC_max);
	EXPECT_EQ(capacity_model->q0(), 100);
	EXPECT_EQ(capacity_model->qmax(), 100);
}
