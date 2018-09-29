#ifndef __LIB_BATTERY_POWERFLOW_TEST_H__
#define __LIB_BATTERY_POWERFLOW_TEST_H__

#include <gtest/gtest.h>
#include "../shared/lib_battery_powerflow.h"
#include <lib_power_electronics.h>


class BatteryPowerFlowTest : public ::testing::Test
{
protected:
	BatteryPowerFlow * m_batteryPowerFlow;
	BatteryPower * m_batteryPower;
	SharedInverter * m_sharedInverter;
	sandia_inverter_t * sandia;
	partload_inverter_t * partload;
	double error;

public:

	void SetUp()
	{
		error = 0.02;
		double dtHour = 1.0;
		m_batteryPowerFlow = new BatteryPowerFlow(dtHour);
		m_batteryPower = m_batteryPowerFlow->getBatteryPower();
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
		sandia->C0 = -3.18e-6;
		sandia->C1 = -5.12e-5;
		sandia->C2 = 0.000984;
		sandia->C3 = -0.00151;
		sandia->Paco = 3800;
		sandia->Pdco = 3928.11;
		sandia->Vdco = 398.497;
		sandia->Pso = 19.4516;
		sandia->Pntare = 0.99;
		m_sharedInverter = new SharedInverter(SharedInverter::SANDIA_INVERTER, numberOfInverters, sandia, partload);
		m_batteryPower->setSharedInverter(m_sharedInverter);
	}
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
	}

};



#endif