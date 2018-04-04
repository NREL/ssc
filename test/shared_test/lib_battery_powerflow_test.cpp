#include <gtest/gtest.h>

#include "lib_battery_powerflow_test.h"
#include <lib_power_electronics.h>

TEST_F(BatteryPowerFlowTest, TestInitialize)
{
	// PV Charging Scenario
	m_batteryPower->canPVCharge = true;
	m_batteryPower->powerPV = 100;
	m_batteryPower->powerLoad = 50;
	m_batteryPowerFlow->initialize(50);
	EXPECT_EQ(m_batteryPower->powerBattery, -50);

	// Grid charging Scenario
	m_batteryPower->canGridCharge = true;
	m_batteryPowerFlow->initialize(50);
	EXPECT_EQ(m_batteryPower->powerBattery, -m_batteryPower->powerBatteryChargeMax);

	// Discharging Scenario
	m_batteryPower->canDischarge = true;
	m_batteryPower->powerPV = 50;
	m_batteryPower->powerLoad = 100;
	m_batteryPowerFlow->initialize(50);
	EXPECT_EQ(m_batteryPower->powerBattery, 50);
}

TEST_F(BatteryPowerFlowTest, TestACConnected)
{
	m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;

	// PV and Grid Charging Scenario
	m_batteryPower->canPVCharge = true;
	m_batteryPower->powerPV = 100;
	m_batteryPower->powerLoad = 50;
	m_batteryPowerFlow->initialize(50);
	m_batteryPowerFlow->calculate();

	EXPECT_NEAR(m_batteryPower->powerBattery, -52.08, error); // The extra 2.08 kW is due to conversion efficiency
	EXPECT_NEAR(m_batteryPower->powerPVToLoad, 50, error);
	EXPECT_NEAR(m_batteryPower->powerPVToBattery, 50, error);
	EXPECT_NEAR(m_batteryPower->powerGridToBattery, 2.08, error);  // Note, grid power charging is NOT allowed here, but this model does not enforce.  It is enforced elsewhere, where this would be iterated upon.
	EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.08, error);

	// Exclusive Grid Charging Scenario
	m_batteryPower->canGridCharge = true;
	m_batteryPower->canPVCharge = false;
	m_batteryPower->powerPV = 100;
	m_batteryPower->powerLoad = 50;
	m_batteryPowerFlow->initialize(50);
	m_batteryPowerFlow->calculate();

	EXPECT_NEAR(m_batteryPower->powerBattery, -104.166, error);
	EXPECT_NEAR(m_batteryPower->powerPVToLoad, 50, error);
	EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
	EXPECT_NEAR(m_batteryPower->powerPVToGrid, 50, error);
	EXPECT_NEAR(m_batteryPower->powerGridToBattery, 104.166, error);
	EXPECT_NEAR(m_batteryPower->powerConversionLoss, 4.166, error);
	
	// Discharging Scenario
	m_batteryPower->canDischarge = true;
	m_batteryPower->powerPV = 50;
	m_batteryPower->powerLoad = 100;
	m_batteryPowerFlow->initialize(50);
	m_batteryPowerFlow->calculate();

	EXPECT_NEAR(m_batteryPower->powerBattery, 48 , error);
	EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 48, error);
	EXPECT_NEAR(m_batteryPower->powerPVToLoad, 50, error);
	EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
	EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
	EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
	EXPECT_NEAR(m_batteryPower->powerGridToLoad, 2, error);
	EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2, error);

}