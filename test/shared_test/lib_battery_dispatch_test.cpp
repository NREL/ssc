#include <math.h>
#include <gtest/gtest.h>
#include <functional>   // std::greater
#include <algorithm>    // std::sort

#include "lib_battery_dispatch_test.h"

#include <lib_power_electronics.h>

// forward declarations
class sandia_inverter_t;

class partload_inverter_t;

class ond_inverter;

size_t year = 0;
size_t hour_of_year = 0;
size_t step_of_hour = 0;

TEST_F(ManualTest_lib_battery_dispatch, PowerLimitsDispatchManualAC) {
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax,
                                           currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax,
                                           powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    double powerToFill = dispatchManual->battery_power_to_fill();
    EXPECT_NEAR(dispatchManual->battery_soc(), 50, 0.1);

    // Test max charge power constraint
    batteryPower->powerPV = 1000;
    batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, -powerChargeMax, 2.0);
    EXPECT_LT(dispatchManual->battery_power_to_fill(),
              powerToFill); // Confirm battery power moves in the expected direction

    powerToFill = dispatchManual->battery_power_to_fill();
    // Test max discharge power constraint
    batteryPower->powerPV = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, powerDischargeMax, 2.0);
    EXPECT_GT(dispatchManual->battery_power_to_fill(), powerToFill);
}

TEST_F(ManualTest_lib_battery_dispatch, PowerLimitsDispatchManualDC) {
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax,
                                           currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax,
                                           powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->setSharedInverter(m_sharedInverter);

    // Test max charge power constraint
    batteryPower->powerPV = 1000;
    batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, -powerChargeMax, 2.0);

    // Test max discharge power constraint
    batteryPower->powerPV = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, powerDischargeMax, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, CurrentLimitsDispatchManualAC) {
    dispatch_t::CURRENT_CHOICE testChoice = dispatch_t::CURRENT_CHOICE::RESTRICT_CURRENT;
    double testChargeMax = 20;
    double testDischargeMax = 20;
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, testChoice, testChargeMax,
                                           testDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax,
                                           powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Test max charge current constraint
    batteryPower->powerPV = 1000;
    batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    double current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, -testChargeMax, 2.0);

    // Test max discharge current constraint
    batteryPower->powerPV = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, testDischargeMax, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, CurrentLimitsDispatchManualDC) {
    dispatch_t::CURRENT_CHOICE testChoice = dispatch_t::CURRENT_CHOICE::RESTRICT_CURRENT;
    double testChargeMax = 20;
    double testDischargeMax = 20;
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, testChoice, testChargeMax,
                                           testDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax,
                                           powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->setSharedInverter(m_sharedInverter);

    // Test max charge constraints
    batteryPower->powerPV = 1000;
    batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    double current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, -testChargeMax, 2.0);

    // Test max discharge current constraint
    batteryPower->powerPV = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, testDischargeMax, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, BothLimitsDispatchManualAC) {
    dispatch_t::CURRENT_CHOICE testChoice = dispatch_t::CURRENT_CHOICE::RESTRICT_BOTH;
    double testChargeMax = 20;
    double testDischargeMax = 20;
    double testDischargeMaxPower = 11; // kW
    double testChargeMaxPower = 12; // kW
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, testChoice, testChargeMax,
                                           testDischargeMax, testChargeMaxPower, testDischargeMaxPower,
                                           testChargeMaxPower, testDischargeMaxPower, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Test max charge current constraint
    batteryPower->powerPV = 1000;
    batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    double current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, -testChargeMax, 2.0);

    hour_of_year += 1;

    // Test max charge power constraint
    batteryPower->powerPV = 1000;
    batteryPower->voltageSystem = 1200;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryAC, -testChargeMaxPower, 2.0);

    hour_of_year += 1;

    // Test max discharge constraints
    batteryPower->powerPV = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, testDischargeMax, 2.0);

    hour_of_year += 1;

    // Test max discharge power constraint
    batteryPower->powerPV = 0;
    batteryPower->voltageSystem = 1200;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryAC, testDischargeMaxPower, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, BothLimitsDispatchManualDC) {
    dispatch_t::CURRENT_CHOICE testChoice = dispatch_t::CURRENT_CHOICE::RESTRICT_BOTH;
    double testChargeMax = 20; // Amps
    double testDischargeMax = 20; // Amps
    double testDischargeMaxPower = 11; // kW
    double testChargeMaxPower = 11; // kW
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, testChoice, testChargeMax,
                                           testDischargeMax, testChargeMaxPower, testDischargeMaxPower, powerChargeMax,
                                           powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->setSharedInverter(m_sharedInverter);

    // Test max charge current constraint
    batteryPower->powerPV = 1000;
    batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    double current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, -testChargeMax, 2.0);

    hour_of_year += 1;

    // Test max discharge current constraint
    batteryPower->powerPV = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, testDischargeMax, 2.0);

    hour_of_year += 1;

    // Test max charge power constraint
    batteryPower->powerPV = 1000;
    batteryPower->voltageSystem = 1200;
    batteryPower->powerLoad = 0;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, -testChargeMaxPower, 2.0);

    hour_of_year += 1;

    // Test max discharge power constraint
    batteryPower->powerPV = 0;
    batteryPower->voltageSystem = 1200;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, testDischargeMaxPower, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, DispatchChangeFrequency) {
    double testTimestep = 1.0 / 60.0; // Minute timesteps
    double testMinTime = 4.0; // Only allow dispatch to change every 3 mins
    dispatchManual = new dispatch_manual_t(batteryModel, testTimestep, SOC_min, SOC_max, currentChoice,
                                           currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax,
                                           powerChargeMax, powerDischargeMax, testMinTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Start by charging (0 minutes)
    batteryPower->powerPV = 1000;
    batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, -powerChargeMax, 2.0);

    // Abruptly cut off the PV and increase the load. Power should go to zero (1 minute)
    step_of_hour += 1;
    batteryPower->powerPV = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 0.1);

    // Same status (2nd minute)
    step_of_hour += 1;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 0.1);

    // Same status (3rd minute)
    step_of_hour += 1;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 0.1);

    // Should dispatch to load now (4th minute)
    step_of_hour += 1;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, powerChargeMax, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, SOCLimitsOnDispatch) {
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax,
                                           currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax,
                                           powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    double soc = dispatchManual->battery_soc();
    EXPECT_NEAR(dispatchManual->battery_soc(), 50, 0.1);

	// Test dispatch iterations to fully charge
	batteryPower->powerPV = 1000; batteryPower->voltageSystem = 600;
	while (soc < SOC_max && hour_of_year < 100) {
		dispatchManual->dispatch(year, hour_of_year, step_of_hour);
		hour_of_year += 1;
		soc = dispatchManual->battery_soc();
	}
	EXPECT_NEAR(SOC_max, dispatchManual->battery_soc(), 0.1);
	EXPECT_NEAR(11, hour_of_year, 0.1);

	// Attempt dispatch one more time, should not charge
	hour_of_year += 1;
	dispatchManual->dispatch(year, hour_of_year, step_of_hour);
	EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 0.1);

	// Cut off PV and provide load
	batteryPower->powerPV = 0; batteryPower->voltageSystem = 600; batteryPower->powerLoad = 1000;
	while (soc > SOC_min && hour_of_year < 100) {
		dispatchManual->dispatch(year, hour_of_year, step_of_hour);
		hour_of_year += 1;
		soc = dispatchManual->battery_soc();
	}
	EXPECT_NEAR(SOC_min, dispatchManual->battery_soc(), 0.1);
	EXPECT_NEAR(22, hour_of_year, 0.1);

    // Cut off PV and provide load
    batteryPower->powerPV = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    while (soc > SOC_min && hour_of_year < 100) {
        dispatchManual->dispatch(year, hour_of_year, step_of_hour);
        hour_of_year += 1;
        soc = dispatchManual->battery_soc();
    }
    EXPECT_NEAR(SOC_min, dispatchManual->battery_soc(), 0.1);
    EXPECT_NEAR(16, hour_of_year, 0.1);

    // Attempt dispatch one more time, should not discharge
    hour_of_year += 1;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 0.1);
}

TEST_F(ManualTest_lib_battery_dispatch, ManualGridChargingOffTest)
{
	// canGridCharge is false by default
	dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
		dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, canGridcharge, canGridcharge, percentDischarge, percentGridcharge);

	batteryPower = dispatchManual->getBatteryPower();
	batteryPower->connectionMode = ChargeController::DC_CONNECTED;
	batteryPower->setSharedInverter(m_sharedInverter);

	// Should not charge since grid charging is disallowed
	batteryPower->powerPV = 0; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 1000;
	dispatchManual->dispatch(year, hour_of_year, step_of_hour);
	EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 0.1);
	EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
}

TEST_F(ManualTest_lib_battery_dispatch, ManualGridChargingOnTest)
{
	std::vector<bool> testCanGridcharge;
	std::map<size_t, double> testPercentGridCharge;
	for (int p = 0; p < 6; p++) {
		testCanGridcharge.push_back(1);
		testPercentGridCharge[p] = 100;
	}
	dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
		dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, testCanGridcharge, canGridcharge, percentDischarge, testPercentGridCharge);

	batteryPower = dispatchManual->getBatteryPower();
	batteryPower->connectionMode = ChargeController::AC_CONNECTED;

	// Test grid charging. AC to DC losses come into play
	batteryPower->powerPV = 0; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 1000;
	dispatchManual->dispatch(year, hour_of_year, step_of_hour);
	EXPECT_NEAR(batteryPower->powerBatteryDC, -powerChargeMax * batteryPower->singlePointEfficiencyACToDC, 1.0);
	EXPECT_NEAR(batteryPower->powerGridToBattery, powerChargeMax, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, NoGridChargingWhilePVIsOnTest)
{
	std::vector<bool> testCanGridcharge;
	std::map<size_t, double> testPercentGridCharge;
	for (int p = 0; p < 6; p++) {
		testCanGridcharge.push_back(1);
		testPercentGridCharge[p] = 100;
	}
	dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
		dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, testCanGridcharge, canGridcharge, percentDischarge, testPercentGridCharge);

	batteryPower = dispatchManual->getBatteryPower();
	batteryPower->connectionMode = ChargeController::AC_CONNECTED;

	// Test no grid charging while PV is on
	batteryPower->powerPV = 1000; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 1000;
	dispatchManual->dispatch(year, hour_of_year, step_of_hour);
	EXPECT_NEAR(batteryPower->powerBatteryDC, -powerChargeMax, 1.0);
	EXPECT_NEAR(batteryPower->powerGridToBattery, 0.0, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, EfficiencyLimitsDispatchManualDC)
{
	dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
		dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, canGridcharge, canGridcharge, percentDischarge, percentGridcharge);

	batteryPower = dispatchManual->getBatteryPower();
	batteryPower->connectionMode = ChargeController::DC_CONNECTED;
	batteryPower->setSharedInverter(m_sharedInverter);

	// Test max charge power constraint
	batteryPower->powerPV = 1000; batteryPower->voltageSystem = 600;
	dispatchManual->dispatch(year, hour_of_year, step_of_hour);
	EXPECT_NEAR(batteryPower->powerBatteryDC, -powerChargeMax, 2.0);

	// Test max discharge power constraint
	batteryPower->powerPV = 0; batteryPower->voltageSystem = 600; batteryPower->powerLoad = 1000;
	dispatchManual->dispatch(year, hour_of_year, step_of_hour);
	EXPECT_NEAR(batteryPower->powerBatteryDC, powerDischargeMax, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, InverterEfficiencyCutoffDC)
{
	std::vector<bool> testCanGridcharge;
	std::map<size_t, double> testPercentGridCharge;
	for (int p = 0; p < 6; p++) {
		testCanGridcharge.push_back(1);
		testPercentGridCharge[p] = 1;
	}
	testPercentGridCharge[1] = 1;
	testPercentGridCharge[3] = 100;
	dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
		dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, testCanGridcharge, canGridcharge, testPercentGridCharge, testPercentGridCharge);

	batteryPower = dispatchManual->getBatteryPower();
	batteryPower->connectionMode = ChargeController::DC_CONNECTED;
	batteryPower->setSharedInverter(m_sharedInverter);
	batteryPower->inverterEfficiencyCutoff = 80;
	batteryPower->canGridCharge = true;

	// Test inverter efficiency cutoff on grid charging
	batteryPower->powerPV = 0; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 7;
	dispatchManual->dispatch(year, 0, step_of_hour);
	EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 0.0, 0.1); // Efficiency is 0 when system is not running
	EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 0.1);

	batteryPower->powerPV = 0; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 1000;
	dispatchManual->dispatch(year, 12, step_of_hour);
	EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 93.7, 0.1);
	EXPECT_NEAR(batteryPower->powerBatteryDC, -47.8, 0.1);

	// Test discharge constraints. First constraint does not hit backoff
	batteryPower->powerPV = 0; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 0; batteryPower->powerLoad = 7;
	dispatchManual->dispatch(year, 0, step_of_hour);
	EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 37.69, 0.1); // Not enforced becasue no PV
	EXPECT_NEAR(batteryPower->powerBatteryDC, 4.55, 0.1);

	batteryPower->powerPV = 770; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 0; batteryPower->powerLoad = 1000;
	dispatchManual->dispatch(year, 12, step_of_hour);
	EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 93.9, 0.1);
	EXPECT_NEAR(batteryPower->powerBatteryDC, 49.9, 0.1); // Dispatch both battery and PV

	batteryPower->powerPV = 1000; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 0; batteryPower->powerLoad = 1100;
	dispatchManual->dispatch(year, 12, step_of_hour);
	EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 77, 0.1);
	EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 2.0); // Overwhelm inverter with PV, back off battery
}

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTM) {
    // Setup pv and load signal for peak shaving algorithm
    for (size_t d = 0; d < 365; d++) {
        for (size_t h = 0; h < 24; h++) {
            if (h > 6 && h < 18) {
                pv_prediction.push_back(fabs(12 - h) * 100);
            }
            load_prediction.push_back(600);
        }
    }
    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // TEST 1: Verify no grid charging since disallowed
    dispatchAutoBTM->dispatch(0, 0, 0);
    EXPECT_EQ(batteryPower->powerGridToBattery, 0);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 0.02);

    // TEST 2: Now, allow grid charging, should charge
    batteryPower->canGridCharge = true;
    dispatchAutoBTM->dispatch(0, 1, 0);
    EXPECT_GT(batteryPower->powerGridToBattery, 0);
    EXPECT_LT(batteryPower->powerBatteryDC, 0);
}

TEST_F(AutoFOMTest_lib_battery_dispatch, DispatchFOMInput) {
    std::vector<double> P_batt;
    for (int i = 0; i < 8760 * 60; i++) {
        P_batt.push_back(-336.062);
    }

    batteryPower = dispatchAutoFOM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->powerPV = 750;
    batteryPower->powerFuelCell = 300;

    dispatchAutoFOM->set_custom_dispatch(P_batt);
    dispatchAutoFOM->dispatch(0, 0, 0);

    EXPECT_NEAR(batteryPower->powerBatteryDC, -216.8, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryAC, -225.8, 0.1);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
}

TEST_F(AutoFOMDC_lib_battery_dispatch, DispatchFOM_DCCustomCharge) {
    dispatchChoice = dispatch_t::FOM_CUSTOM_DISPATCH;
    dispatchAutoDC = new dispatch_automatic_front_of_meter_t(batteryModelDC, dtHourDC, 10, 100, 1, 49960, 49960, 25000,
                                                             25000, 25000, 25000, 1, dispatchChoice, 0, 1, 18, 1, true,
                                                             true, true, false, 77000, 0, 1, 0.005, ppaRate, ur, 98, 98,
                                                             98);

    std::vector<double> P_batt;
    for (int i = 0; i < 8760; i++) {
        P_batt.push_back(-2915); // Charge as fast as possible
    }

    // battery setup
    dispatchAutoDC->update_pv_data(pv);
    dispatchAutoDC->update_cliploss_data(clip);
    dispatchAutoDC->set_custom_dispatch(P_batt);
    batteryPower = dispatchAutoDC->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->canGridCharge = true;
    batteryPower->setSharedInverter(m_sharedInverter);

    batteryPower->powerGeneratedBySystem = 0;
    batteryPower->powerPV = 0;
    batteryPower->powerPVClipped = 0;
    batteryPower->powerGridToBattery = 30000;

    std::vector<double> p_batterykW;
    double remainingPower = dispatchAutoDC->battery_power_to_fill();
    for (size_t h = 0; h < 24; h++) {
        remainingPower = dispatchAutoDC->battery_power_to_fill();
        dispatchAutoDC->dispatch(0, h, 0);
        p_batterykW.push_back(batteryPower->powerBatteryDC);
        EXPECT_NEAR(batteryPower->powerBatteryDC, -2915, 1e-3) << "error in dispatched power at hour " << h;
        if (remainingPower < -29157) {
            EXPECT_LT(batteryPower->powerBatteryDC, -29157) << "hour " << h;
            EXPECT_LT(batteryPower->powerGridToBattery, -29157) << "hour " << h;
        }
    }
    std::sort(p_batterykW.begin(), p_batterykW.end(), std::greater<double>());

    EXPECT_NEAR(p_batterykW[0], 3046.1, 0.1);
    EXPECT_NEAR(p_batterykW[23], -29403.5, 0.1);
}

TEST_F(AutoFOMDC_lib_battery_dispatch, DispatchFOM_DCAuto) {
    dispatchAutoDC = new dispatch_automatic_front_of_meter_t(batteryModelDC, dtHourDC, 10, 100, 1, 49960, 49960, 25000,
                                                             25000, 25000, 25000, 1, 0, 0, 1, 18, 1, true, true, false,
                                                             false, 77000, 0, 1, 0.005, ppaRate, ur, 98, 98, 98);

    // battery setup
    dispatchAutoDC->update_pv_data(pv);
    dispatchAutoDC->update_cliploss_data(clip);
    batteryPower = dispatchAutoDC->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->setSharedInverter(m_sharedInverter);

    std::vector<double> p_batterykW;
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerGeneratedBySystem = pv[h];
        batteryPower->powerPV = pv[h];
        batteryPower->powerPVClipped = clip[h];
        dispatchAutoDC->dispatch(0, h, 0);
        p_batterykW.push_back(batteryPower->powerBatteryAC);
    }
    std::sort(p_batterykW.begin(), p_batterykW.end(), std::greater<double>());

    EXPECT_NEAR(p_batterykW[0], 3046.1, 0.1);
    EXPECT_NEAR(p_batterykW[23], -29403.5, 0.1);
}


TEST_F(AutoFOMTest_lib_battery_dispatch, DispatchFOM_ACCustomCharge) {
    dispatchChoice = dispatch_t::FOM_CUSTOM_DISPATCH;
    dispatchAutoFOM = new dispatch_automatic_front_of_meter_t(batteryModelFOM, dtHour, 10, 100, 1, 49960, 49960, 25000,
                                                              25000, 25000, 25000, 1, dispatchChoice, 0, 1, 18, 1, true,
                                                              true, true, false, 77000, 0, 1, 0.005, ppaRate, ur, 98,
                                                              98, 98);

    std::vector<double> P_batt;
    for (int i = 0; i < 8760; i++) {
        P_batt.push_back(-2915); // Charge as fast as possible
    }

    // battery setup
    dispatchAutoFOM->update_pv_data(pv);
    dispatchAutoFOM->set_custom_dispatch(P_batt);
    batteryPower = dispatchAutoFOM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->canGridCharge = true;

    batteryPower->powerGeneratedBySystem = 0;
    batteryPower->powerPV = 0;
    batteryPower->powerPVClipped = 0;
    batteryPower->powerGridToBattery = 30000;

    std::vector<double> p_batterykW;
    double remainingPower = dispatchAutoFOM->battery_power_to_fill();
    for (size_t h = 0; h < 24; h++) {
        remainingPower = dispatchAutoFOM->battery_power_to_fill();
        dispatchAutoFOM->dispatch(0, h, 0);
        p_batterykW.push_back(batteryPower->powerBatteryDC);
        EXPECT_NEAR(batteryPower->powerBatteryDC, -2915, 1e-3) << "error in dispatched power at hour " << h;
        if (remainingPower < -29157) {
            EXPECT_LT(batteryPower->powerBatteryDC, -29157) << "hour " << h;
            EXPECT_LT(batteryPower->powerGridToBattery, -29157) << "hour " << h;
        }
    }
    std::sort(p_batterykW.begin(), p_batterykW.end(), std::greater<double>());

    EXPECT_NEAR(p_batterykW[0], 3046.1, 0.1);
    EXPECT_NEAR(p_batterykW[23], -29403.5, 0.1);
}

TEST_F(AutoFOMTest_lib_battery_dispatch, DispatchFOM_ACAuto) {
    dispatchAutoFOM = new dispatch_automatic_front_of_meter_t(batteryModelFOM, dtHour, 10, 100, 1, 49960, 49960, 25000,
                                                              25000, 25000, 25000, 1, 0, 0, 1, 18, 1, true, true, false,
                                                              false, 77000, 0, 1, 0.005, ppaRate, ur, 98, 98, 98);

    // battery setup
    dispatchAutoFOM->update_pv_data(pv);
    batteryPower = dispatchAutoFOM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->voltageSystem = 600;

    std::vector<double> p_batterykW;
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerGeneratedBySystem = pv[h];
        batteryPower->powerPV = pv[h];
        dispatchAutoFOM->dispatch(0, h, 0);
        p_batterykW.push_back(batteryPower->powerBatteryAC);
    }
    std::sort(p_batterykW.begin(), p_batterykW.end(), std::greater<double>());

    EXPECT_NEAR(p_batterykW[0], 3046.1, 0.1);
    EXPECT_NEAR(p_batterykW[23], -29403.5, 0.1);
}

