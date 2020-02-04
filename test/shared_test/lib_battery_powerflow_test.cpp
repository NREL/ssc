#include <gtest/gtest.h>

#include "lib_battery_powerflow_test.h"
#include "lib_ondinv.h"
#include "lib_pvinv.h"
#include "lib_sandia.h"
#include "lib_shared_inverter.h"


void BatteryPowerFlowTest_lib_battery_powerflow::SetUp()
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
	m_batteryPower->powerBatteryChargeMaxDC = 100;
	m_batteryPower->powerBatteryDischargeMaxDC = 50;
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

TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, TestInitialize)
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
	EXPECT_EQ(m_batteryPower->powerBatteryDC, -m_batteryPower->powerBatteryChargeMaxDC);

	// Discharging Scenario
	m_batteryPower->canDischarge = true;
	m_batteryPower->powerPV = 50;
	m_batteryPower->powerLoad = 100;
	m_batteryPowerFlow->initialize(50);
	EXPECT_EQ(m_batteryPower->powerBatteryDC, m_batteryPower->powerBatteryDischargeMaxDC);
}

// Excess PV production
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, PVChargingAC_ExcessPV) {
    m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    m_batteryPower->canPVCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canGridCharge = false;
    m_batteryPower->powerPV = 100;
    m_batteryPower->powerLoad = 50;

    // Try to charge
    m_batteryPower->powerBatteryDC = -50 * m_batteryPower->singlePointEfficiencyACToDC;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -50, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.0, error);

    double gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // Try to charge more than is available from PV, disallowed
    m_batteryPower->powerBatteryDC = -100;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -50, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.0, error);

    gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // Try to discharge
    m_batteryPower->powerBatteryDC = 50;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 48, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.0, error);

    gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
}

// Not enough PV for load
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, PVChargingAC_ExcessLoad) {
    m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    m_batteryPower->canPVCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canGridCharge = false;
    m_batteryPower->powerPV = 25;
    m_batteryPower->powerLoad = 50;

    // do not discharge battery
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0, error);

    double gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge battery
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 5.8, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.8, error);

    gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // try to charge battery from grid, not allowed
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0, error);

    gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
}

// Excess PV production
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, GridChargingAC_ExcessPV) {
    m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    m_batteryPower->canGridCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canPVCharge = false;
    m_batteryPower->powerPV = 100;
    m_batteryPower->powerLoad = 50;

    // charging will be from grid
    m_batteryPower->powerBatteryDC = -50;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -52.08, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 52.08, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.08, error);

    double gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge to grid
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.2, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 19.2, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.80, error);

    gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
}

// Not enough PV, pull from grid
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, GridChargingAC_ExcessLoad)
{
    m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    m_batteryPower->canGridCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canPVCharge = false;
    m_batteryPower->powerPV = 10;
    m_batteryPower->powerLoad = 50;

    // don't dispatch
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 40, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0, error);

    double gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // charging will happen from grid
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -20.83, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 20.83, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 40, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.83, error);

    gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 20.8, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.8, error);

    gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
}


// Excess PV production
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, PVChargingDC_ExcessPV) {
    m_batteryPower->connectionMode = ChargeController::DC_CONNECTED;

    m_batteryPower->canPVCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canGridCharge = false;
    m_batteryPower->powerPV = 100;
    m_batteryPower->powerLoad = 50;

    // Try to charge
    m_batteryPower->powerBatteryDC = -50 * m_batteryPower->singlePointEfficiencyACToDC;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -48.97, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 48.24, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 48.97, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 1.75, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 3.76, error);

    double gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryDC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // Try to charge more than is available from PV, disallowed
    m_batteryPower->powerBatteryDC = -100;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -50, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.0, error);

    gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // Try to discharge
    m_batteryPower->powerBatteryDC = 50;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 48, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.0, error);

    gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
}

// Not enough PV for load
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, PVChargingDC_ExcessLoad) {
    m_batteryPower->connectionMode = ChargeController::DC_CONNECTED;

    m_batteryPower->canPVCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canGridCharge = false;
    m_batteryPower->powerPV = 25;
    m_batteryPower->powerLoad = 50;

    // do not discharge battery
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0, error);

    double gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge battery
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 5.8, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.8, error);

    gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // try to charge battery from grid, not allowed
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0, error);

    gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
}

// Excess PV production
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, GridChargingDC_ExcessPV) {
    m_batteryPower->connectionMode = ChargeController::DC_CONNECTED;

    m_batteryPower->canGridCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canPVCharge = false;
    m_batteryPower->powerPV = 100;
    m_batteryPower->powerLoad = 50;

    // charging will be from grid
    m_batteryPower->powerBatteryDC = -50;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -52.08, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 52.08, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.08, error);

    double gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge to grid
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.2, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 19.2, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.80, error);

    gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
}

// Not enough PV, pull from grid
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, GridChargingDC_ExcessLoad)
{
    m_batteryPower->connectionMode = ChargeController::DC_CONNECTED;

    m_batteryPower->canGridCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canPVCharge = false;
    m_batteryPower->powerPV = 10;
    m_batteryPower->powerLoad = 50;

    // don't dispatch
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 40, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0, error);

    double gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // charging will happen from grid
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -20.83, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 20.83, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 40, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.83, error);

    gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerPVToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerPVToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 20.8, error);
    EXPECT_NEAR(m_batteryPower->powerPVToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.8, error);

    gen = m_batteryPower->powerPV + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
}

TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, TestDCConnected)
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
