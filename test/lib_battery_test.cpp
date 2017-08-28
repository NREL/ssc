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
		capacity_model = new capacity_lithium_ion_t(q, SOC_min, SOC_max);
	}
	void TearDown()
	{
		if (capacity_model)
			delete capacity_model;
	}
};

TEST_F(LithiumIonBattery, UnitTest)
{
	EXPECT_EQ(capacity_model->SOC(), SOC_max);
}
