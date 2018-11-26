#ifndef __LIB_BATTERY_POWERFLOW_TEST_H__
#define __LIB_BATTERY_POWERFLOW_TEST_H__

#include <gtest/gtest.h>
#include "../shared/lib_battery_powerflow.h"
#include <lib_power_electronics.h>

// forward declarations
class sandia_inverter_t;
class partload_inverter_t;
class ond_inverter;

class BatteryPowerFlowTest : public ::testing::Test
{
protected:
	BatteryPowerFlow * m_batteryPowerFlow;
	BatteryPower * m_batteryPower;
	SharedInverter * m_sharedInverter;
	sandia_inverter_t * sandia;
	partload_inverter_t * partload;
	ond_inverter *ond;
	double error;

public:

	void SetUp();
	
	void TearDown()
	{
		if (m_batteryPowerFlow)
			delete m_batteryPowerFlow;
		if (m_sharedInverter)
			delete m_sharedInverter;
		if (sandia)
			delete sandia;
		if (partload)
			delete partload;
		if (ond)
			delete ond;
	}

};



#endif