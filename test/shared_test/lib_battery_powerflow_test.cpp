#include <gtest/gtest.h>
#include "lib_battery_powerflow_test.h"

TEST_F(BatteryPowerFlowTest, TestInitialize)
{
	// PV Charging Scenario
	m_batteryPower->canPVCharge = true;
	m_batteryPower->powerPV = 100;
	m_batteryPower->powerLoad = 50;
	m_batteryPowerFlow->initialize();
	EXPECT_EQ(m_batteryPower->powerBattery, -50);

	// Grid charging Scenario
	m_batteryPower->canGridCharge = true;
	m_batteryPowerFlow->initialize();
	EXPECT_EQ(m_batteryPower->powerBattery, -m_batteryPower->powerBatteryChargeMax);

	// Discharging Scenario
	m_batteryPower->canDischarge = true;
	m_batteryPower->powerPV = 50;
	m_batteryPower->powerLoad = 100;
	m_batteryPowerFlow->initialize();
	EXPECT_EQ(m_batteryPower->powerBattery, 50);
}