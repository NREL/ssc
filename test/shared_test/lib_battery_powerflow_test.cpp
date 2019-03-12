#include <gtest/gtest.h>

#include "lib_battery_powerflow_test.h"
#include "lib_ondinv.h"
#include "lib_pvinv.h"
#include "lib_sandia.h"
#include "lib_shared_inverter.h"


void BatteryPowerFlowTest::SetUp()
{
	error = 0.02;
	double dtHour = 1.0;
	m_batteryPowerFlow = new BatteryPowerFlow(dtHour);
	m_batteryPower = m_batteryPowerFlow->getBatteryPower();
	m_batteryPower->reset();
	m_batteryPower->canDischarge = false;
	m_batteryPower->canPVCharge = false;
	m_batteryPower->canGridCharge = false;
	m_batteryPower->singlePointEfficiencyACToDC = 0.96;
	m_batteryPower->singlePointEfficiencyDCToAC = 0.96;
	m_batteryPower->singlePointEfficiencyDCToDC = 0.98;
	m_batteryPower->powerBatteryChargeMax = 100;
	m_batteryPower->powerBatteryDischargeMax = 50;
	m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;

	// setup Sandia inverter using SMA America: SB3800TL-US-22 (240V) [CEC 2013]
	int numberOfInverters = 100;
	sandia = new sandia_inverter_t();
	partload = new partload_inverter_t();
	ond = new ond_inverter();
	sandia->C0 = -3.18e-6;
	sandia->C1 = -5.12e-5;
	sandia->C2 = 0.000984;
	sandia->C3 = -0.00151;
	sandia->Paco = 3800;
	sandia->Pdco = 3928.11;
	sandia->Vdco = 398.497;
	sandia->Pso = 19.4516;
	sandia->Pntare = 0.99;
	m_sharedInverter = new SharedInverter(SharedInverter::SANDIA_INVERTER, numberOfInverters, sandia, partload, ond);
	m_batteryPower->setSharedInverter(m_sharedInverter);
}

TEST_F(BatteryPowerFlowTest, TestInitialize)
{
	// PV Charging Scenario
	m_batteryPower->canPVCharge = true;
	m_batteryPower->powerPV = 100;
	m_batteryPower->powerLoad = 50;
	m_batteryPowerFlow->initialize(50);
	EXPECT_EQ(m_batteryPower->powerBatteryDC, -50);

	// Grid charging Scenario
	m_batteryPower->canGridCharge = true;
	m_batteryPowerFlow->initialize(50);
	EXPECT_EQ(m_batteryPower->powerBatteryDC, -m_batteryPower->powerBatteryChargeMax);

	// Discharging Scenario
	m_batteryPower->canDischarge = true;
	m_batteryPower->powerPV = 50;
	m_batteryPower->powerLoad = 100;
	m_batteryPowerFlow->initialize(50);
	EXPECT_EQ(m_batteryPower->powerBatteryDC, m_batteryPower->powerBatteryDischargeMax);
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

	EXPECT_NEAR(m_batteryPower->powerBatteryAC, -52.08, error); // The extra 2.08 kW is due to conversion efficiency
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

	EXPECT_NEAR(m_batteryPower->powerBatteryAC, -104.166, error);
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

	EXPECT_NEAR(m_batteryPower->powerBatteryAC, 48 , error);
	EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 48, error);
	EXPECT_NEAR(m_batteryPower->powerPVToLoad, 50, error);
	EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
	EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
	EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
	EXPECT_NEAR(m_batteryPower->powerGridToLoad, 2, error);
	EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2, error);
}


TEST_F(BatteryPowerFlowTest, TestDCConnected)
{
	m_batteryPower->connectionMode = ChargeController::DC_CONNECTED;

	// PV and Grid Charging Scenario
	m_batteryPower->canPVCharge = true;
	m_batteryPower->powerPV = 300;
	m_batteryPower->powerLoad = 200;
	m_batteryPowerFlow->initialize(50);
	m_batteryPowerFlow->calculate();

	EXPECT_NEAR(m_batteryPower->powerBatteryAC, -102.04, error); 
	EXPECT_NEAR(m_batteryPower->powerPVToLoad, 191.78, error);
	EXPECT_NEAR(m_batteryPower->powerPVToBattery, 102.04, error);
	EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0.00, error);  
	EXPECT_NEAR(m_batteryPower->powerConversionLoss, 8.22, error);

	// Exclusive Grid Charging Scenario
	m_batteryPower->canGridCharge = true;
	m_batteryPower->canPVCharge = false;
	m_batteryPower->powerPV = 300;
	m_batteryPower->powerLoad = 200;
	m_batteryPowerFlow->initialize(50);
	m_batteryPowerFlow->calculate();

	EXPECT_NEAR(m_batteryPower->powerBatteryAC, -105.33, error);
	EXPECT_NEAR(m_batteryPower->powerPVToLoad, 200, error);
	EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
	EXPECT_NEAR(m_batteryPower->powerPVToGrid, 90.63, error);
	EXPECT_NEAR(m_batteryPower->powerGridToBattery, 105.33, error);
	EXPECT_NEAR(m_batteryPower->powerConversionLoss, 8.22, error);

	// Discharging Scenario
	m_batteryPower->canDischarge = true;
	m_batteryPower->powerPV = 200;
	m_batteryPower->powerLoad = 300;
	m_batteryPowerFlow->initialize(50);
	m_batteryPowerFlow->calculate();

	EXPECT_NEAR(m_batteryPower->powerBatteryAC, 47.49, error);
	EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 47.49, error);
	EXPECT_NEAR(m_batteryPower->powerPVToLoad, 193.83, error);
	EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
	EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
	EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
	EXPECT_NEAR(m_batteryPower->powerGridToLoad, 58.68, error);
	EXPECT_NEAR(m_batteryPower->powerConversionLoss, 8.68, error);
}