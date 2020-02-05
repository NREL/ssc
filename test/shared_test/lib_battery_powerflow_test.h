#ifndef __LIB_BATTERY_POWERFLOW_TEST_H__
#define __LIB_BATTERY_POWERFLOW_TEST_H__

#include <gtest/gtest.h>
#include "../shared/lib_battery_powerflow.h"

#include <lib_ondinv.h>
#include <lib_power_electronics.h>
#include <lib_pvinv.h>
#include <lib_sandia.h>
#include <lib_shared_inverter.h>

// forward declarations
class sandia_inverter_t;
class partload_inverter_t;
class ond_inverter;

class BatteryPowerFlowTest_lib_battery_powerflow : public ::testing::Test
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

	double calc_dc_gen(){
		return m_batteryPower->powerPVToLoad + m_batteryPower->powerPVToGrid + m_batteryPower->powerBatteryToLoad
			   + m_batteryPower->powerBatteryToGrid - m_batteryPower->powerGridToBattery * m_batteryPower->sharedInverter->efficiencyAC/100;
	}

	double calc_met_load(){
		return m_batteryPower->powerBatteryToLoad + m_batteryPower->powerGridToLoad + m_batteryPower->powerPVToLoad;
	}

	void check_net_flows(){
	    // increased error due to efficiencies
		double gen = calc_dc_gen();
		EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, 3);
		double met_load = calc_met_load();
		EXPECT_NEAR(met_load, m_batteryPower->powerLoad, 3);
	}

	void TearDown()
	{
		if (m_batteryPowerFlow) {
			delete m_batteryPowerFlow;
			m_batteryPowerFlow = nullptr;
		}
		if (m_sharedInverter) {
			delete m_sharedInverter;
			m_sharedInverter = nullptr;
		}
		if (sandia) {
			delete sandia;
			sandia = nullptr;
		}
		if (partload) {
			delete partload;
			partload = nullptr;
		}
		if (ond) {
			delete ond;
			ond = nullptr;
		}
	}

};



#endif