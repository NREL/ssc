#include <gtest/gtest.h>
#include <lib_battery.h>

class BatteryProperties : public ::testing::Test
{
protected:
	
	// general capacity
	double q;
	double SOC_min;
	double SOC_max;
	double SOC_init;

	void SetUp()
	{
		q = 100;
		SOC_init = 100;
		SOC_min = 20;
		SOC_max = 100;
	}
};

class LithiumIonBattery : public BatteryProperties
{
protected:
	capacity_lithium_ion_t * capacity_model; 

	void SetUp()
	{
		BatteryProperties::SetUp();
		capacity_model = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min);
	}
	void TearDown()
	{
		if (capacity_model)
			delete capacity_model;
	}
};

TEST_F(LithiumIonBattery, LithiumIonCapacityUnitTest_lib_battery)
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
	capacity_model->check_SOC();
	EXPECT_EQ(capacity_model->q0(), 95);
	EXPECT_EQ(capacity_model->qmax(), 100);
	capacity_model->updateCapacityForThermal(100);
	capacity_model->check_SOC();

	// check that updating lifetime degradation changes capacity
	capacity_model->updateCapacityForLifetime(95);
	EXPECT_EQ(capacity_model->q0(), 95);
	EXPECT_EQ(capacity_model->qmax(), 95);

	// check that battery replacement works
	capacity_model->replace_battery();
	EXPECT_EQ(capacity_model->SOC(), SOC_max);
	EXPECT_EQ(capacity_model->q0(), 100);
	EXPECT_EQ(capacity_model->qmax(), 100);

	// check that model correctly detects overcharge, undercharge
	capacity_model->updateCapacity(-10, 1);
	EXPECT_EQ(capacity_model->q0(), 100);
	EXPECT_EQ(capacity_model->SOC(), SOC_max);
	capacity_model->updateCapacity(110, 1);
	EXPECT_EQ(capacity_model->q0(), 20);
	EXPECT_EQ(capacity_model->SOC(), SOC_min);
}

class LeadAcidDC4006 : public BatteryProperties
{
protected:
	double q20;
	double t1;
	double q1;
	double q10;

	void SetUp()
	{
		BatteryProperties::SetUp();

		q20 = 415;
		q10 = 374;
		q1 = 340;
		t1 = 5;
	}
};

class LeadAcidBattery : public LeadAcidDC4006
{
protected:
	capacity_kibam_t * capacity_model;

	void SetUp()
	{
		LeadAcidDC4006::SetUp();
		capacity_model = new capacity_kibam_t(q20, t1, q1, q10, SOC_init, SOC_max, SOC_min);
	}
	void TearDown()
	{
		if (capacity_model)
			delete capacity_model;
	}
};

TEST_F(LeadAcidBattery, LeadAcidCapacityUnitTest_lib_battery)
{
	// Check that initial capacity is equal to max
	EXPECT_EQ(capacity_model->SOC(), SOC_max);

	/*
	// Check that discharge of battery results in correct capacity
	capacity_model->updateCapacity(10, 1);
	EXPECT_DOUBLE_EQ(capacity_model->SOC(), 97.959525493854386);
	EXPECT_DOUBLE_EQ(capacity_model->q0(), 480.08208482298681);

	// check that charge of battery results in correct capacity
	capacity_model->updateCapacity(-10, 1);
	EXPECT_DOUBLE_EQ(capacity_model->SOC(), 99.872284504899923);
	EXPECT_DOUBLE_EQ(capacity_model->q0(), 489.45617406195834);

	// check that updating thermal behavior changes capacity as expected
	capacity_model->updateCapacityForThermal(95);
	EXPECT_DOUBLE_EQ(capacity_model->q0(), 465.57798058183749);
	EXPECT_DOUBLE_EQ(capacity_model->qmax(), 490.08208482298681);
	capacity_model->updateCapacityForThermal(100);

	// check that updating lifetime degradation changes capacity
	capacity_model->updateCapacityForLifetime(95);
	EXPECT_DOUBLE_EQ(capacity_model->q0(), 465.57798058183749);
	EXPECT_DOUBLE_EQ(capacity_model->qmax(), 465.57798058183749);

	// check that battery replacement works
	capacity_model->replace_battery();
	EXPECT_DOUBLE_EQ(capacity_model->SOC(), SOC_max); 

	/*
	EXPECT_DOUBLE_EQ(capacity_model->q0(), 465.57798058183749);
	EXPECT_DOUBLE_EQ(capacity_model->qmax(), 490.08208482298681);

	// check that model correctly detects overcharge, undercharge
	capacity_model->updateCapacity(-100, 1);
	EXPECT_EQ(capacity_model->q0(), 100);
	EXPECT_EQ(capacity_model->SOC(), SOC_max);
	capacity_model->updateCapacity(1000, 1);
	EXPECT_EQ(capacity_model->q0(), 20);
	EXPECT_EQ(capacity_model->SOC(), SOC_min);
	*/
	

}